/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  oafd: OAF CORBA dameon.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 1999, 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors: Elliot Lee <sopwith@redhat.com>,
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <time.h>
#include <string.h>

#include "server.h"

#include "bonobo-activation/bonobo-activation-i18n.h"
#include "activation-context-query.h"
#include "activation-server-corba-extensions.h"

#undef LOCALE_DEBUG

#define Bonobo_LINK_TIME_TO_LIVE 256

typedef struct {
	Bonobo_ObjectDirectory obj;

	char *hostname, *username;

	Bonobo_ServerInfoList *list;
	Bonobo_CacheTime time_list_pulled;
	GHashTable *by_iid;

	GHashTable *active_servers;	/* It is assumed that accesses to this
					 * hash table are atomic - i.e. a CORBA 
                                         * call cannot come in while
					 * checking a value in this table */
	Bonobo_ServerStateCache *active_server_list;
	Bonobo_CacheTime time_active_pulled;

	guchar locked;
} ChildODInfo;

static ChildODInfo *
child_od_info_new (Bonobo_ObjectDirectory obj, CORBA_Environment * ev)
{
	ChildODInfo *retval;
	char *host, *user;

	host = Bonobo_ObjectDirectory__get_hostname (obj, ev);
	if (ev->_major != CORBA_NO_EXCEPTION)
		goto errhost;

	user = Bonobo_ObjectDirectory__get_username (obj, ev);
	if (ev->_major != CORBA_NO_EXCEPTION)
		goto erruser;

	retval = g_new0 (ChildODInfo, 1);

	retval->obj = CORBA_Object_duplicate (obj, ev);
	retval->hostname = host;
	retval->username = user;

	return retval;

      erruser:
	CORBA_free (host);
      errhost:
	return NULL;
}

static void
child_od_info_free (ChildODInfo *child, CORBA_Environment *ev)
{
	CORBA_Object_release (child->obj, ev);
	CORBA_free (child->hostname);
	CORBA_free (child->username);

        if (child->by_iid)
                g_hash_table_destroy (child->by_iid);

        if (child->list) {
                CORBA_sequence_set_release (child->list, CORBA_TRUE);
                CORBA_free (child->list);
        }

        if (child->active_servers)
                g_hash_table_destroy (child->active_servers);
        CORBA_free (child->active_server_list);

        memset (child, 0xaa, sizeof (ChildODInfo));
	g_free (child);
}

static void
child_od_exception (ChildODInfo * child, CORBA_Environment * ev)
{
	CORBA_Object_release (child->obj, ev);
	child->obj = CORBA_OBJECT_NIL;
}

static void
child_od_update_active (ChildODInfo *child, CORBA_Environment *ev)
{
	int i;
	Bonobo_ServerStateCache *cache;

	cache = Bonobo_ObjectDirectory_get_active_servers (
                child->obj, child->time_active_pulled, ev);

	if (ev->_major != CORBA_NO_EXCEPTION) {
		child_od_exception (child, ev);
		return;
	}

	if (cache->_d) {
		if (child->active_servers) {
			g_hash_table_destroy (child->active_servers);
			CORBA_free (child->active_server_list);
		}

		child->active_server_list = cache;

		child->time_active_pulled = time (NULL);
		child->active_servers =
			g_hash_table_new (g_str_hash, g_str_equal);
		for (i = 0; i < cache->_u.active_servers._length; i++)
			g_hash_table_insert (child->active_servers,
					     cache->_u.
					     active_servers._buffer[i],
					     GINT_TO_POINTER (1));
	} else
		CORBA_free (cache);
}

static char *
ac_CORBA_Context_get_value (CORBA_Context         ctx, 
                            const char           *propname,
                            CORBA_Environment    *ev)
{
        return activation_server_CORBA_Context_get_value (
                ctx, propname,
                ex_Bonobo_Activation_IncompleteContext, ev);
}

/*** App-specific servant structures ***/

typedef struct {
	POA_Bonobo_ActivationContext servant;

	GSList *dirs;

	int total_servers;

	gint refs;		/* This is a use count, so we don't accidentally go
				 * updating our server list and invalidating memory */
	Bonobo_ActivationContext self;
} impl_POA_Bonobo_ActivationContext;

/*** Stub implementations ***/

static void
child_od_update_list (ChildODInfo *child, CORBA_Environment *ev)
{
	int i;
	Bonobo_ServerInfoListCache *cache;

	cache = Bonobo_ObjectDirectory_get_servers (
                child->obj, child->time_list_pulled, ev);

	if (ev->_major != CORBA_NO_EXCEPTION) {
		child->list = NULL;
		child_od_exception (child, ev);
		return;
	}

	if (cache->_d) {
		if (child->by_iid)
			g_hash_table_destroy (child->by_iid);
		if (child->list) {
			CORBA_sequence_set_release (child->list, CORBA_TRUE);
			CORBA_free (child->list);
			child->list = NULL;
		}

		child->list = Bonobo_ServerInfoList__alloc ();
		*(child->list) = cache->_u.server_list;
		CORBA_sequence_set_release (child->list, CORBA_FALSE);
		CORBA_sequence_set_release (&(cache->_u.server_list),
					    CORBA_FALSE);

		child->time_list_pulled = time (NULL);
		child->by_iid = g_hash_table_new (g_str_hash, g_str_equal);
		for (i = 0; i < child->list->_length; i++)
			g_hash_table_insert (child->by_iid,
					     child->list->_buffer[i].iid,
					     &(child->list->_buffer[i]));
	}

	CORBA_free (cache);
}

static void 
free_child_dirs (impl_POA_Bonobo_ActivationContext *servant,
                 CORBA_Environment                 *ev)
{
	GSList *l;

	for (l = servant->dirs; l; l = l->next)
                child_od_info_free (l->data, ev);
        g_slist_free (servant->dirs);
        servant->dirs = NULL;
}

static void
ac_update_list (impl_POA_Bonobo_ActivationContext *servant,
		ChildODInfo                       *child,
                CORBA_Environment                 *ev)
{
	int prev, new;

	if (servant->refs > 0)
		return;

	if (child->list)
		prev = child->list->_length;
	else
		prev = 0;

	child_od_update_list (child, ev);

	if (child->list)
		new = child->list->_length;
	else
		new = 0;

	servant->total_servers += (new - prev);
}

static ChildODInfo *
ac_find_child_for_server (impl_POA_Bonobo_ActivationContext *servant,
			  Bonobo_ServerInfo                 *server,
                          CORBA_Environment                 *ev)
{
	GSList *l;

	for (l = servant->dirs; l; l = l->next) {
		ChildODInfo *child = l->data;

		if (CORBA_Object_is_nil (child->obj, ev) || !child->list)
			continue;

		if ((server >= child->list->_buffer)
		    && (server <
			(child->list->_buffer +
			 child->list->_length))) return child;
	}

	return NULL;
}

static QueryExprConst
ac_query_get_var (Bonobo_ServerInfo *si, const char *id, QueryContext *qctx)
{
	ChildODInfo *child;
	QueryExprConst retval;

	retval.value_known = FALSE;
	retval.needs_free = FALSE;

	child = ac_find_child_for_server (qctx->user_data, si, NULL);
	if (!child)
		goto out;

	if (!strcasecmp (id, "_active")) {
		CORBA_Environment ev;

		CORBA_exception_init (&ev);
		child_od_update_active (child, &ev);
		CORBA_exception_free (&ev);

		retval.value_known = TRUE;
		retval.type = CONST_BOOLEAN;
		retval.u.v_boolean =
			g_hash_table_lookup (child->active_servers,
					     si->iid) ? TRUE : FALSE;
	}

      out:

	return retval;
}

/* This function should only be called by
 * impl_Bonobo_ActivationContext_query and
 * impl_Bonobo_ActivationContext_activateMatching - hairy implicit preconditions
 * exist. */
static void
ac_query_run (impl_POA_Bonobo_ActivationContext *servant,
	      const CORBA_char                  *requirements,
	      const Bonobo_StringList           *selection_order,
	      CORBA_Context                      ctx,
	      Bonobo_ServerInfo                **items,
              CORBA_Environment                 *ev)
{
	int total, i;
	GSList *cur;
	QueryContext qctx;

	Bonobo_ServerInfo **orig_items;
	int item_count, orig_item_count;
	char *errstr;
	Bonobo_Activation_ParseFailed *ex;

	QueryExpr *qexp_requirements;
	QueryExpr **qexp_sort_items;

	/* First, parse the query */
	errstr = (char *) qexp_parse (requirements, &qexp_requirements);
	if (errstr) {
		puts (errstr);

		g_strstrip (errstr);
		ex = Bonobo_Activation_ParseFailed__alloc ();
		ex->description = CORBA_string_dup (errstr);
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Activation_ParseFailed,
				     ex);
		return;
	}

	qexp_sort_items =
		g_alloca (selection_order->_length * sizeof (QueryExpr *));
	for (i = 0; i < selection_order->_length; i++) {
		errstr =
			(char *) qexp_parse (selection_order->_buffer[i],
					     &qexp_sort_items[i]);

		if (errstr) {
			qexp_free (qexp_requirements);
			for (i--; i >= 0; i--)
				qexp_free (qexp_sort_items[i]);

			g_strstrip (errstr);
			ex = Bonobo_Activation_ParseFailed__alloc ();
			ex->description = CORBA_string_dup (errstr);

			CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
					     ex_Bonobo_Activation_ParseFailed,
					     ex);
			return;
		}
	}

	total = servant->total_servers;
	orig_items = g_alloca (total * sizeof (Bonobo_ServerInfo *));

	for (item_count = 0, cur = servant->dirs; cur; cur = cur->next) {
		ChildODInfo *child;
		int i;

		child = cur->data;

		if (child->obj == CORBA_OBJECT_NIL)
			continue;

		for (i = 0; i < child->list->_length; i++, item_count++)
			items[item_count] = &child->list->_buffer[i];
	}

	memcpy (orig_items, items, item_count * sizeof (Bonobo_ServerInfo *));
	orig_item_count = item_count;

	qctx.sil = orig_items;
	qctx.nservers = orig_item_count;
	qctx.cctx = ctx;
	qctx.id_evaluator = ac_query_get_var;
	qctx.user_data = servant;

	for (i = 0; i < item_count; i++) {
		if (!qexp_matches (items[i], qexp_requirements, &qctx))
			items[i] = NULL;
	}

	qexp_sort (items, item_count, qexp_sort_items,
		   selection_order->_length, &qctx);

        qexp_free (qexp_requirements);
        for (i = 0; i < selection_order->_length; i++)
                qexp_free (qexp_sort_items[i]);
}

static void
ac_update_lists (impl_POA_Bonobo_ActivationContext *servant,
		 CORBA_Environment                 *ev)
{
	GSList *cur;

	if (servant->refs > 0) {
                /* FIXME: what happens on re-enterency here ?
                 * looks like this could get seriously out of date */
		return;
        }

	for (cur = servant->dirs; cur; cur = cur->next)
		ac_update_list (servant, cur->data, ev);
}

#define GET_SERVANT(s) ((impl_POA_Bonobo_ActivationContext *)(s))

static GList *clients = NULL;

void
activation_clients_cache_notify (void)
{
        GList *l;
        GSList *notify = NULL, *l2;
        CORBA_Environment ev;

        CORBA_exception_init (&ev);

        for (l = clients; l; l = l->next)
                notify = g_slist_prepend (notify, CORBA_Object_duplicate (l->data, &ev));

        for (l2 = notify; l2; l2 = l2->next) {
                Bonobo_ActivationClient_resetCache (l2->data, &ev);
                if (ev._major != CORBA_NO_EXCEPTION)
                        clients = g_list_remove (clients, l2->data);

                CORBA_Object_release (l2->data, &ev);
                CORBA_exception_free (&ev);
        }
}

gboolean
activation_clients_is_empty_scan (void)
{
        GList *l, *next;

        for (l = clients; l; l = next) {
                next = l->next;
                if (ORBit_small_get_connection_status (l->data) ==
                    ORBIT_CONNECTION_DISCONNECTED) {
                        CORBA_Object_release (l->data, NULL);
                        clients = g_list_delete_link (clients, l);
                }
        }

        return clients == NULL;
}

static void
active_client_cnx_broken (ORBitConnection *cnx,
                          gpointer         dummy)
{
        if (activation_clients_is_empty_scan ()) {
#ifdef BONOBO_ACTIVATION_DEBUG
                g_warning ("All clients dead");
#endif
                check_quit ();
        }

}

static void
impl_Bonobo_ActivationContext_addClient (PortableServer_Servant        servant,
                                         const Bonobo_ActivationClient client,
                                         const CORBA_char             *locales,
                                         CORBA_Environment            *ev)
{
        GList *l;
        gboolean new_locale;
        ORBitConnection *cnx;

        new_locale = register_interest_in_locales (locales);

        cnx = ORBit_small_get_connection (client);
        for (l = clients; l; l = l->next)
                if (cnx == ORBit_small_get_connection (l->data))
                        break;
        
        clients = g_list_prepend (
                clients, CORBA_Object_duplicate (client, ev));

        if (!l) {
                g_signal_connect (
                        cnx, "broken",
                        G_CALLBACK (active_client_cnx_broken),
                        NULL);
                check_quit ();
        }

        if (new_locale)
                bonobo_object_directory_reload ();
}

static Bonobo_ObjectDirectoryList *
impl_Bonobo_ActivationContext__get_directories (PortableServer_Servant _servant,
                                                CORBA_Environment     *ev)
{
        impl_POA_Bonobo_ActivationContext *servant = GET_SERVANT (_servant);
	Bonobo_ObjectDirectoryList *retval;
	int i;
	GSList *cur;

	retval = Bonobo_ObjectDirectoryList__alloc ();
	retval->_length = g_slist_length (servant->dirs);
	retval->_buffer =
		CORBA_sequence_Bonobo_ObjectDirectory_allocbuf (retval->_length);

	for (i = 0, cur = servant->dirs; cur; cur = cur->next, i++) {
		ChildODInfo *child;
		child = cur->data;
		retval->_buffer[i] = CORBA_Object_duplicate (child->obj, ev);
	}

	CORBA_sequence_set_release (retval, CORBA_TRUE);

	return retval;
}

static void
impl_Bonobo_ActivationContext_addDirectory (PortableServer_Servant _servant,
                                            Bonobo_ObjectDirectory dir,
                                            CORBA_Environment     *ev)
{
        impl_POA_Bonobo_ActivationContext *servant = GET_SERVANT (_servant);
	GSList *cur;
	ChildODInfo *new_child;

	for (cur = servant->dirs; cur; cur = cur->next) {
		ChildODInfo *child;
		child = cur->data;
		if (CORBA_Object_is_equivalent (dir, child->obj, ev)) {
			CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
					     ex_Bonobo_Activation_AlreadyListed,
					     NULL);
			return;
		}
	}

	new_child = child_od_info_new (dir, ev);
	if (new_child)
		servant->dirs = g_slist_append (servant->dirs, new_child);
}

static void
impl_Bonobo_ActivationContext_removeDirectory (PortableServer_Servant _servant,
                                               Bonobo_ObjectDirectory dir,
                                               CORBA_Environment     *ev)
{
        impl_POA_Bonobo_ActivationContext *servant = GET_SERVANT (_servant);
	GSList *cur;

	for (cur = servant->dirs; cur; cur = cur->next) {
		ChildODInfo *child;
		child = cur->data;

		if (CORBA_Object_is_equivalent (dir, child->obj, ev)) {
			if (servant->refs) {
				CORBA_Object_release (child->obj, ev);
				child->obj = CORBA_OBJECT_NIL;
			} else {
				servant->dirs =
					g_slist_remove (servant->dirs, child);
				child_od_info_free (child, ev);
			}
			break;
		}
	}

	if (!cur)
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Activation_NotListed,
				     NULL);
}

static void
ac_do_activation (impl_POA_Bonobo_ActivationContext  *servant,
		  Bonobo_ServerInfo                  *server,
		  const Bonobo_ActivationEnvironment *environment,
		  Bonobo_ActivationResult            *out,
		  Bonobo_ActivationFlags              flags,
		  const char                         *hostname,
		  CORBA_Context                       ctx,
                  CORBA_Environment                  *ev)
{
	int num_layers;
	ChildODInfo *child;
	Bonobo_ServerInfo *activatable;

	/* When doing checks for shlib loadability, we 
         * have to find the info on the factory object in case
	 * a factory is inside a shlib 
         */
	child = ac_find_child_for_server (servant, server, ev);

	if (!child || !child->obj || ev->_major != CORBA_NO_EXCEPTION) {
		Bonobo_GeneralError *errval = Bonobo_GeneralError__alloc ();
		errval->description =
			CORBA_string_dup
			(_("Couldn't find which child the server was listed in"));
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_GeneralError, errval);
		return;
	}

	for (num_layers = 0, activatable = server;
             activatable && activatable->server_type &&
                     !strcmp (activatable->server_type, "factory") &&
             num_layers < Bonobo_LINK_TIME_TO_LIVE; num_layers++) {

		activatable = g_hash_table_lookup (child->by_iid, activatable->location_info);
	}

	if (activatable == NULL) {		
		Bonobo_GeneralError *errval = Bonobo_GeneralError__alloc ();
		errval->description = CORBA_string_dup ("Couldn't find the factory server");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION, 
				     ex_Bonobo_GeneralError, errval);
		return;
	} 
	else if (num_layers == Bonobo_LINK_TIME_TO_LIVE) {
		Bonobo_GeneralError *errval = Bonobo_GeneralError__alloc ();
		errval->description = CORBA_string_dup ("Location loop");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_GeneralError, errval);
		return;
        }

	/* A shared library must be on the same host as the activator in
	 * order for loading to work properly (no, we're not going to
	 * bother with loading a remote shlib into a process - it gets far too complicated
	 * far too quickly :-) */
	
	if (activatable && !strcmp (activatable->server_type, "shlib")
	    && !(flags & Bonobo_ACTIVATION_FLAG_NO_LOCAL)
	    && (hostname && !strcmp (activatable->hostname, hostname))) {
		int j;
		char tbuf[512];
		
		out->res._d = Bonobo_ACTIVATION_RESULT_SHLIB;		

		/* Here is an explanation as to why we add 2 to num_layers.
		 * At the end of the string list, after all the factory iids are added
		 * to the string list, we then add the iid of the shaed library and the 
		 * location info.  This data is later used in oaf_server_activate_shlib
		 * to activate the component
		 */		 
		out->res._u.res_shlib._length = num_layers + 2;
		out->res._u.res_shlib._buffer = CORBA_sequence_CORBA_string_allocbuf (num_layers + 2);

		/* Copy over factory info */
		for (j = 0, activatable = server; activatable
		     && !strcmp (activatable->server_type, "factory"); j++) {
			out->res._u.res_shlib._buffer[j] = CORBA_string_dup (activatable->iid);
			activatable = g_hash_table_lookup (child->by_iid,
						     	   activatable->location_info);
		}

		/* Copy shlib iid into buffer */
		out->res._u.res_shlib._buffer[j] = CORBA_string_dup (activatable->iid);

		/* Copy location into last buffer slot for use in later activation */
		out->res._u.res_shlib._buffer[j+1] = CORBA_string_dup (activatable->location_info);
		
		g_snprintf (tbuf, sizeof (tbuf), "OAFAID:[%s,%s,%s]",
			    activatable->iid,
			    activatable->username,
			    activatable->hostname);
		out->aid = CORBA_string_dup (tbuf);
	} else {
		CORBA_Object retval;

		retval = Bonobo_ObjectDirectory_activate (
                        child->obj, server->iid, servant->self, environment, flags, ctx, ev);

		if (ev->_major == CORBA_NO_EXCEPTION) {
			char tbuf[512];
			out->res._d = Bonobo_ACTIVATION_RESULT_OBJECT;
			out->res._u.res_object = retval;
			g_snprintf (tbuf, sizeof (tbuf),
				    "OAFAID:[%s,%s,%s]", activatable->iid,
				    activatable->username,
				    activatable->hostname);
			out->aid = CORBA_string_dup (tbuf);
		}
#ifdef BONOBO_ACTIVATION_DEBUG
                else
                        g_warning ("Activation of '%s' failed with exception '%s'",
                                   activatable->iid, ev->_id);
#endif
	}
}


static Bonobo_ActivationResult *
impl_Bonobo_ActivationContext_activateMatching (
        PortableServer_Servant              _servant,
        const CORBA_char                   *requirements,
        const Bonobo_StringList            *selection_order,
	const Bonobo_ActivationEnvironment *environment,
        const Bonobo_ActivationFlags        flags,
        CORBA_Context                       ctx,
        CORBA_Environment                  *ev)
{
        impl_POA_Bonobo_ActivationContext *servant = GET_SERVANT (_servant);
	Bonobo_ActivationResult *retval = NULL;
	Bonobo_ServerInfo **items, *curitem;
	int i;
	char *hostname;

	ac_update_lists (servant, ev);

	servant->refs++;

	items = g_alloca (servant->total_servers *
                          sizeof (Bonobo_ServerInfo *));
	ac_query_run (servant, requirements, selection_order, ctx, items, ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		goto out;

	hostname = ac_CORBA_Context_get_value (ctx, "hostname", ev);

	retval = Bonobo_ActivationResult__alloc ();
	retval->res._d = Bonobo_ACTIVATION_RESULT_NONE;

	for (i = 0; (retval->res._d == Bonobo_ACTIVATION_RESULT_NONE) && items[i]
	     && (i < servant->total_servers); i++) {
		curitem = items[i];

		ac_do_activation (servant, curitem, environment,
				  retval, flags, hostname, ctx, ev);
	}

	if (retval->res._d == Bonobo_ACTIVATION_RESULT_NONE)
		retval->aid = CORBA_string_dup ("");

	g_free (hostname);

 out:
	if (ev->_major != CORBA_NO_EXCEPTION) {
                CORBA_free (retval);
                retval = NULL;
        }

	servant->refs--;

	return retval;
}


static Bonobo_ServerInfoList *
impl_Bonobo_ActivationContext__get_servers (PortableServer_Servant _servant,
                                            CORBA_Environment     *ev)
{
        impl_POA_Bonobo_ActivationContext *servant = GET_SERVANT (_servant);
	Bonobo_ServerInfoList *retval;
	GSList *cur;
	int i;
	int total;

	ac_update_lists (servant, ev);

	total = servant->total_servers;

	retval = Bonobo_ServerInfoList__alloc ();
	retval->_length = total;
	retval->_buffer = CORBA_sequence_Bonobo_ServerInfo_allocbuf (total);
	CORBA_sequence_set_release (retval, CORBA_TRUE);

	for (i = 0; i < total;) {
		for (cur = servant->dirs; cur; cur = cur->next) {
			ChildODInfo *child;
			int j;

			child = cur->data;

			for (j = 0; j < child->list->_length; j++, i++)
				Bonobo_ServerInfo_copy (&retval->_buffer[i],
						     &child->
						     list->_buffer[j]);
		}
	}

	return retval;
}

static Bonobo_ServerInfoList *
impl_Bonobo_ActivationContext_query (PortableServer_Servant _servant,
                                     const CORBA_char * requirements,
                                     const Bonobo_StringList * selection_order,
                                     CORBA_Context ctx, CORBA_Environment * ev)
{
        impl_POA_Bonobo_ActivationContext *servant = GET_SERVANT (_servant);
	Bonobo_ServerInfoList *retval;
	Bonobo_ServerInfo **items;
	int item_count;
	int i, j, total;

	retval = Bonobo_ServerInfoList__alloc ();
	retval->_length = 0;
	retval->_buffer = NULL;
	CORBA_sequence_set_release (retval, CORBA_TRUE);

	/* Pull in new lists from OD servers */
	ac_update_lists (servant, ev);
	servant->refs++;

	items = g_alloca (servant->total_servers *
                          sizeof (Bonobo_ServerInfo *));
	item_count = servant->total_servers;

	ac_query_run (servant, requirements, selection_order, ctx, items, ev);

	if (ev->_major == CORBA_NO_EXCEPTION) {
		for (total = i = 0; i < item_count; i++) {
			if (items[i])
				total++;
		}

		retval->_length = total;
		retval->_buffer =
			CORBA_sequence_Bonobo_ServerInfo_allocbuf (total);

		for (i = j = 0; i < item_count; i++) {
			if (!items[i])
				continue;

			Bonobo_ServerInfo_copy (&retval->_buffer[j], items[i]);

			j++;
		}
	}

	servant->refs--;

	return retval;
}

static char *
ac_aid_to_query_string (const CORBA_char *aid)
{
        char *tmp_aid;
        char *requirements;
        char *iid_requirement;
        char *username_requirement;
        char *hostname_requirement;
	BonoboActivationInfo *ainfo;

	ainfo = bonobo_activation_id_parse (aid);
	if (!ainfo)
                return NULL;

        iid_requirement = g_strconcat ("iid == \'", ainfo->iid, "\' ", NULL);

        if (ainfo->user) {
                username_requirement = g_strconcat ("AND username == \'", ainfo->user, "\'", NULL);
        } else {
                username_requirement = g_strdup ("");
        }
        
        if (ainfo->host) {
                hostname_requirement = g_strconcat ("AND hostname == \'", ainfo->host, "\'", NULL);
        } else {
                hostname_requirement = g_strdup ("");
        }
        
        requirements = g_strconcat (iid_requirement, username_requirement, 
                                    hostname_requirement, NULL);

        g_free (iid_requirement);
        g_free (username_requirement);
        g_free (hostname_requirement);
        bonobo_activation_info_free (ainfo);

        return requirements;
}

static void
ac_context_to_string_array (CORBA_Context context, char **sort_criteria,
                            CORBA_Environment *ev)
{
	char *context_username;
	char *context_hostname;

        context_username = ac_CORBA_Context_get_value (context, "username", ev);
        context_hostname = ac_CORBA_Context_get_value (context, "hostname", ev);
	if (ev->_major != CORBA_NO_EXCEPTION)
                return;
        
        sort_criteria[0] = g_strconcat ("username == \'", context_username, "\'", NULL);
        sort_criteria[1] = g_strconcat ("hostname == \'", context_hostname, "\'", NULL);
        sort_criteria[2] = NULL;

        g_free (context_username);
        g_free (context_hostname);
}

#define PARSE_ERROR_NOT_AN_AID (_("Not a valid Activation ID"))

static Bonobo_ActivationResult *
impl_Bonobo_ActivationContext_activateFromAid (PortableServer_Servant  _servant,
					       const CORBA_char       *aid,
					       Bonobo_ActivationFlags  flags,
					       CORBA_Context           ctx,
					       CORBA_Environment      *ev)
{
        impl_POA_Bonobo_ActivationContext *servant = GET_SERVANT (_servant);
	Bonobo_ActivationResult *retval;
        char *requirements;
        char *sort_criteria[3];
        Bonobo_StringList selection_order;
	Bonobo_ActivationEnvironment environment;

	if (strncmp ("OAFAID:", aid, 7) != 0) {
		Bonobo_Activation_ParseFailed *ex;

		ex = Bonobo_Activation_ParseFailed__alloc ();
		ex->description = CORBA_string_dup (PARSE_ERROR_NOT_AN_AID);

		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Activation_ParseFailed,
				     ex);
		return NULL;
	}

	ac_update_lists (servant, ev);
        if (ev->_major != CORBA_NO_EXCEPTION)
                return NULL;

	servant->refs++;

        requirements = ac_aid_to_query_string (aid);
        if (requirements == NULL) {
                servant->refs--;
                return NULL;
        }

        ac_context_to_string_array (ctx, sort_criteria, ev);
        if (ev->_major != CORBA_NO_EXCEPTION) {
                servant->refs--;
                g_free (requirements);
                return NULL;
        }

        selection_order._length = 2;
        selection_order._buffer = sort_criteria;
        CORBA_sequence_set_release (&selection_order, CORBA_FALSE);

	memset (&environment, 0, sizeof (Bonobo_ActivationEnvironment));

        retval = impl_Bonobo_ActivationContext_activateMatching (
                servant, requirements, &selection_order, &environment, flags, ctx, ev);

        g_free (sort_criteria[0]);
        g_free (sort_criteria[1]);
        g_free (requirements);

        servant->refs--;

        return retval;
}

/*** epv structures ***/

static PortableServer_ServantBase__epv impl_Bonobo_ActivationContext_base_epv = {
	NULL,			/* _private data */
	NULL,			/* finalize routine */
	NULL			/* default_POA routine */
};

/* FIXME: fill me in / deal with me globaly */
static POA_Bonobo_Unknown__epv impl_Bonobo_Unknown_epv = {
	NULL,			/* _private data */
	NULL,
	NULL,
        NULL
};

static POA_Bonobo_ActivationContext__epv impl_Bonobo_ActivationContext_epv = {
	NULL,			/* _private */
	impl_Bonobo_ActivationContext__get_directories,
	impl_Bonobo_ActivationContext__get_servers,
	impl_Bonobo_ActivationContext_addClient,
	impl_Bonobo_ActivationContext_addDirectory,
	impl_Bonobo_ActivationContext_removeDirectory,
	impl_Bonobo_ActivationContext_query,
	impl_Bonobo_ActivationContext_activateMatching,
	impl_Bonobo_ActivationContext_activateFromAid
};

/*** vepv structures ***/

static POA_Bonobo_ActivationContext__vepv impl_Bonobo_ActivationContext_vepv = {
	&impl_Bonobo_ActivationContext_base_epv,
        &impl_Bonobo_Unknown_epv,
	&impl_Bonobo_ActivationContext_epv
};

static impl_POA_Bonobo_ActivationContext *main_ac = NULL;

void
activation_context_init (PortableServer_POA     poa,
                         Bonobo_ObjectDirectory dir,
                         CORBA_Environment     *ev)
{
	PortableServer_ObjectId *objid;
	impl_POA_Bonobo_ActivationContext *servant;

	servant = g_new0 (impl_POA_Bonobo_ActivationContext, 1);

        main_ac = servant;

	servant->servant.vepv = &impl_Bonobo_ActivationContext_vepv;
	POA_Bonobo_ActivationContext__init ((PortableServer_Servant) servant, ev);
	objid = PortableServer_POA_activate_object (poa, servant, ev);
	CORBA_free (objid);

	servant->self = PortableServer_POA_servant_to_reference (poa, servant, ev);

        impl_Bonobo_ActivationContext_addDirectory (servant, dir, ev);
}

void
activation_context_shutdown (PortableServer_POA poa,
                             CORBA_Environment *ev)
{
	PortableServer_ObjectId *oid;
	impl_POA_Bonobo_ActivationContext *servant = main_ac;

	oid = PortableServer_POA_servant_to_id (poa, servant, ev);
	PortableServer_POA_deactivate_object (poa, oid, ev);
	CORBA_free (oid);

        main_ac = NULL;
        
        free_child_dirs (servant, ev);
        CORBA_Object_release (servant->self, ev);

        g_free (servant);
}

Bonobo_ActivationContext
activation_context_get (void)
{
        if (!main_ac)
                return CORBA_OBJECT_NIL;
        else
                return main_ac->self;
}
