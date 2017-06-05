/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  bonobo-activation: A library for accessing bonobo-activation-server.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Author: Elliot Lee <sopwith@redhat.com>
 */
#include <config.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>

#include <bonobo-activation/bonobo-activation-activate.h>

#include <bonobo-activation/bonobo-activation-id.h>
#include <bonobo-activation/bonobo-activation-init.h>
#include <bonobo-activation/bonobo-activation-server-info.h>
#include <bonobo-activation/bonobo-activation-private.h>
#include <bonobo-activation/bonobo-activation-shlib.h>
#include <bonobo-activation/bonobo-activation-client.h>
#include <bonobo-activation/bonobo-activation-async.h>
#include <bonobo-activation/bonobo-activation-i18n.h>
#include <bonobo-activation/Bonobo_ActivationContext.h>

static Bonobo_ActivationEnvironment activation_environment;

/* FIXME: deprecated internal functions. Should we just remove?
 */
void
bonobo_activation_set_test_components_enabled (gboolean val)
{
}

gboolean
bonobo_activation_get_test_components_enabled (void)
{
	return FALSE;
}

static void 
copy_strv_to_sequence (char *const       *selection_order,
		       Bonobo_StringList *str_seq)
{
	int len;

	if (!selection_order) {
		memset (str_seq, 0, sizeof (Bonobo_StringList));
		return;
	}

	for (len = 0; selection_order [len]; len++);

	str_seq->_length  = str_seq->_maximum = len;
	str_seq->_buffer  = (char **) selection_order;
	str_seq->_release = FALSE;
}

/* Limit of the number of cached queries */
#define QUERY_CACHE_MAX 32
#undef QUERY_CACHE_DEBUG

static GHashTable *query_cache = NULL;

typedef struct {
	char  *query;
	char **sort_criteria;

	Bonobo_ServerInfoList *list;
} QueryCacheEntry;

static void
query_cache_entry_free (gpointer data)
{
        QueryCacheEntry *entry = data;

#ifdef QUERY_CACHE_DEBUG
        g_warning ("Blowing item %p", entry);
#endif /* QUERY_CACHE_DEBUG */

        g_free (entry->query);
        g_strfreev (entry->sort_criteria);
        CORBA_free (entry->list);
        g_free (entry);
}

static gboolean
cache_clean_half (gpointer  key,
                  gpointer  value,
                  gpointer  user_data)
{
        int *a = user_data;
        /* Blow half the elements */
        return (*a)++ % 2;
}

static gboolean
query_cache_equal (gconstpointer a, gconstpointer b)
{
	int i;
	char **strsa, **strsb;
	const QueryCacheEntry *entrya = a;
	const QueryCacheEntry *entryb = b;

	if (strcmp (entrya->query, entryb->query))
		return FALSE;

	strsa = entrya->sort_criteria;
	strsb = entryb->sort_criteria;

	if (!strsa && !strsb)
		return TRUE;

	if (!strsa || !strsb)
		return FALSE;

	for (i = 0; strsa [i] && strsb [i]; i++)
		if (strcmp (strsa [i], strsb [i]))
			return FALSE;

	if (strsa [i] || strsb [i])
		return FALSE;

	return TRUE;
}

static guint
query_cache_hash (gconstpointer a)
{
	guint hash, i;
	char **strs;
	const QueryCacheEntry *entry = a;
	
	hash = g_str_hash (entry->query);
	strs = entry->sort_criteria;

	for (i = 0; strs && strs [i]; i++)
		hash ^= g_str_hash (strs [i]);

	return hash;
}

static void
query_cache_reset (void)
{
        if (query_cache) {
                g_hash_table_destroy (query_cache);
                query_cache = NULL;
        }
}

static void
create_query_cache (void)
{
        query_cache = g_hash_table_new_full (
                query_cache_hash,
                query_cache_equal,
                query_cache_entry_free,
                NULL);
        bonobo_activation_add_reset_notify (query_cache_reset);
}

static Bonobo_ServerInfoList *
query_cache_lookup (const char   *query,
		    char * const *sort_criteria)
{
	QueryCacheEntry  fake;
	QueryCacheEntry *entry;

	if (!query_cache) {
                create_query_cache ();
		return NULL;
	}

	fake.query = (char *) query;
	fake.sort_criteria = (char **) sort_criteria;
	if ((entry = g_hash_table_lookup (query_cache, &fake))) {
#ifdef QUERY_CACHE_DEBUG
		g_warning ("\n\n ---  Hit (%p)  ---\n\n\n", entry->list);
#endif /* QUERY_CACHE_DEBUG */
		return Bonobo_ServerInfoList_duplicate (entry->list);
	} else {
#ifdef QUERY_CACHE_DEBUG
		g_warning ("Miss");
#endif /* QUERY_CACHE_DEBUG */
		return NULL;
	}
}

static void
query_cache_insert (const char   *query,
		    char * const *sort_criteria,
		    Bonobo_ServerInfoList *list)
{
        int idx = 0;
	QueryCacheEntry *entry = g_new (QueryCacheEntry, 1);

        if (!query_cache) {
                create_query_cache ();
        
        } else if (g_hash_table_size (query_cache) > QUERY_CACHE_MAX) {
                g_hash_table_foreach_remove (
                        query_cache, cache_clean_half, &idx);
        }

	entry->query = g_strdup (query);
	entry->sort_criteria = g_strdupv ((char **) sort_criteria);
	entry->list = Bonobo_ServerInfoList_duplicate (list);

	g_hash_table_replace (query_cache, entry, entry);

#ifdef QUERY_CACHE_DEBUG
	g_warning ("Query cache size now %d",
                g_hash_table_size (query_cache));
#endif /* QUERY_CACHE_DEBUG */
}

/**
 * bonobo_activation_query: 
 * @requirements: query string.
 * @selection_order: sort criterion for returned list.
 * @ev: a %CORBA_Environment structure which will contain 
 *      the CORBA exception status of the operation, or NULL
 *
 * Executes the @requirements query on the bonobo-activation-server.
 * The result is sorted according to @selection_order. 
 * @selection_order can safely be NULL as well as @ev.
 * The returned list has to be freed with CORBA_free.
 *
 * Return value: the list of servers matching the requirements.
 */
Bonobo_ServerInfoList *
bonobo_activation_query (const char        *requirements,
                         char * const      *selection_order,
                         CORBA_Environment *opt_ev)
{
	Bonobo_StringList         selorder;
	Bonobo_ServerInfoList    *retval;
	Bonobo_ActivationContext  ac;
	CORBA_Environment         tempenv, *ev;

	g_return_val_if_fail (requirements != NULL, CORBA_OBJECT_NIL);

	ac = bonobo_activation_activation_context_get ();
	g_return_val_if_fail (ac != NULL, CORBA_OBJECT_NIL);

	retval = query_cache_lookup (requirements, selection_order);
	if (retval)
		return retval;

	if (!opt_ev) {
		CORBA_exception_init (&tempenv);
		ev = &tempenv;
	} else
		ev = opt_ev;

	copy_strv_to_sequence (selection_order, &selorder);

	retval = Bonobo_ActivationContext_query (
                ac, requirements, &selorder,
                bonobo_activation_context_get (), ev);

        if (ev->_major == CORBA_NO_EXCEPTION)
                query_cache_insert (requirements, selection_order, retval);
        else
                retval = NULL;

	if (!opt_ev)
		CORBA_exception_free (&tempenv);

	return retval;
}

static CORBA_Object
handle_activation_result (Bonobo_ActivationResult *result,
			  Bonobo_ActivationID     *ret_aid,
			  CORBA_Environment       *ev)
{
	CORBA_Object retval = CORBA_OBJECT_NIL;

	switch (result->res._d) {
	case Bonobo_ACTIVATION_RESULT_SHLIB:
		retval = bonobo_activation_activate_shlib_server (result, ev);
		break;
	case Bonobo_ACTIVATION_RESULT_OBJECT:
		retval = CORBA_Object_duplicate (result->res._u.res_object, ev);
		break;
	case Bonobo_ACTIVATION_RESULT_NONE:
	default:
		break;
	}

	if (ret_aid) {
		if (result->aid && result->aid [0])
			*ret_aid = g_strdup (result->aid);
		else
			*ret_aid = NULL;
	}

	CORBA_free (result);

	return retval;
}

/**
 * bonobo_activation_activate:
 * @requirements: query string.
 * @selection_order: sort criterion for returned list.
 * @flags: how to activate the object.
 * @ret_aid: AID of the activated object.
 * @ev: %CORBA_Environment structure which will contain 
 *      the CORBA exception status of the operation. 
 *
 * Activates a given object. @ret_aid can be safely NULLed as well
 * as @ev and @selection_order. @flags can be set to zero if you do 
 * not what to use.
 *
 * Return value: the CORBA object reference of the activated object.
 *               This value can be CORBA_OBJECT_NIL: you are supposed 
 *               to check @ev for success.
 */
CORBA_Object
bonobo_activation_activate (const char             *requirements,
			    char *const            *selection_order,
			    Bonobo_ActivationFlags  flags,
			    Bonobo_ActivationID    *ret_aid,
			    CORBA_Environment      *opt_ev)
{
	Bonobo_ActivationContext  ac;
	Bonobo_ActivationResult  *result;
	CORBA_Environment         tempenv, *ev;
	Bonobo_StringList         selorder;
	CORBA_Object              retval = CORBA_OBJECT_NIL;

	g_return_val_if_fail (requirements != NULL, CORBA_OBJECT_NIL);

	ac = bonobo_activation_activation_context_get ();
	g_return_val_if_fail (ac != NULL, CORBA_OBJECT_NIL);

	if (!opt_ev) {
		CORBA_exception_init (&tempenv);
		ev = &tempenv;
	} else
		ev = opt_ev;

	copy_strv_to_sequence (selection_order, &selorder);

	result = Bonobo_ActivationContext_activateMatching (
			ac, requirements, &selorder, &activation_environment,
			flags, bonobo_activation_context_get (), ev);

	if (ev->_major == CORBA_NO_EXCEPTION)
		retval = handle_activation_result (result, ret_aid, ev);

	if (!opt_ev)
		CORBA_exception_free (&tempenv);

	return retval;
}

/**
 * bonobo_activation_activate_from_id
 * @aid: AID or IID of the object to activate.
 * @flags: activation flag.
 * @ret_aid: AID of the activated server.
 * @ev: %CORBA_Environment structure which will contain 
 *      the CORBA exception status of the operation. 
 *
 * Activates the server corresponding to @aid. @ret_aid can be safely 
 * NULLed as well as @ev. @flags can be zero if you do not know what 
 * to do.
 *
 * Return value: a CORBA object reference to the newly activated 
 *               server. Do not forget to check @ev for failure!!
 */
CORBA_Object
bonobo_activation_activate_from_id (const Bonobo_ActivationID aid, 
				    Bonobo_ActivationFlags    flags,
				    Bonobo_ActivationID      *ret_aid,
				    CORBA_Environment        *opt_ev)
{
	Bonobo_ActivationContext  ac;
	Bonobo_ActivationResult  *result;
	CORBA_Environment        *ev, tempenv;
	CORBA_Object              retval = CORBA_OBJECT_NIL;

	g_return_val_if_fail (aid != NULL, CORBA_OBJECT_NIL);

	if (!strncmp ("OAFIID:", aid, 7)) {
		char *requirements;

		requirements = g_alloca (strlen (aid) + sizeof ("iid == ''"));
		sprintf (requirements, "iid == '%s'", aid);

		return bonobo_activation_activate (
				requirements, NULL, flags, ret_aid, opt_ev);
	}
	
	if (!opt_ev) {
		CORBA_exception_init (&tempenv);
		ev = &tempenv;
	} else
		ev = opt_ev;

	ac = bonobo_activation_internal_activation_context_get_extended (
				(flags & Bonobo_ACTIVATION_FLAG_EXISTING_ONLY), ev);
	if (!ac) {
		if (!opt_ev)
			CORBA_exception_free (&tempenv);

		return CORBA_OBJECT_NIL;
	}

	result = Bonobo_ActivationContext_activateFromAid (
			ac, aid, flags, bonobo_activation_context_get (), ev);

	if (ev->_major == CORBA_NO_EXCEPTION)
		retval = handle_activation_result (result, ret_aid, ev);
        
	if (!opt_ev)
		CORBA_exception_free (&tempenv);

	return retval;
}

/* Async activation
 */

#define ASYNC_ERROR_NO_AID            (_("No ActivationID supplied"))
#define ASYNC_ERROR_NO_REQUIREMENTS   (_("No requirements supplied"))
#define ASYNC_ERROR_NO_CONTEXT        (_("Failed to initialise the AcitvationContext"))
#define ASYNC_ERROR_INV_FAILED        (_("Failed to invoke method on the AcitvationContext"))
#define ASYNC_ERROR_GENERAL_EXCEPTION (_("System exception: %s : %s"))
#define ASYNC_ERROR_EXCEPTION         (_("System exception: %s"))

static ORBit_IMethod *activate_matching_method = NULL;
static ORBit_IMethod *activate_from_aid_method = NULL;

typedef struct {
	BonoboActivationCallback user_cb;
	gpointer                 user_data;
} AsyncActivationData;

static void
setup_methods (void)
{
	activate_matching_method = &Bonobo_ActivationContext__iinterface.methods._buffer [6];
	activate_from_aid_method = &Bonobo_ActivationContext__iinterface.methods._buffer [7];

	/* If these blow the IDL changed order, and the above
	   indexes need updating */
	g_assert (!strcmp (activate_matching_method->name, "activateMatching"));
	g_assert (!strcmp (activate_from_aid_method->name, "activateFromAid"));
}

static void
activation_async_callback (CORBA_Object          object,
			   ORBit_IMethod        *m_data,
			   ORBitAsyncQueueEntry *aqe,
			   gpointer              user_data,
			   CORBA_Environment    *ev)
{
	Bonobo_ActivationResult *result = NULL;
	AsyncActivationData     *async_data = user_data;
	Bonobo_GeneralError     *err;
	CORBA_Object             retval;
	char                    *reason = NULL;

	g_return_if_fail (async_data != NULL);
	g_return_if_fail (async_data->user_cb != NULL);

	if (ev->_major != CORBA_NO_EXCEPTION)
		goto return_exception;

	ORBit_small_demarshal_async (aqe, &result, NULL, ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		goto return_exception;

	retval = handle_activation_result (result, NULL, ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		goto return_exception;

	async_data->user_cb (retval, NULL, async_data->user_data);

clean_out:
	g_free (async_data);
	return;

return_exception:
	if (!strcmp (ev->_id, "IDL:Bonobo/GeneralError:1.0")) {
		err = ev->_any._value;

		if (!err || !err->description)
			reason = g_strdup_printf (ASYNC_ERROR_GENERAL_EXCEPTION,
						  ev->_id, "(no description)");
		else
			reason = g_strdup_printf (ASYNC_ERROR_GENERAL_EXCEPTION,
						  ev->_id, err->description);
	} else
		reason = g_strdup_printf (ASYNC_ERROR_EXCEPTION, ev->_id);

	async_data->user_cb (CORBA_OBJECT_NIL, reason, async_data->user_data);
	g_free (reason);

	goto clean_out;
}

/**
 * bonobo_activation_activate_async:
 * @requirements: the bonobo-activation query string.
 * @selection_order: preference array.
 * @flags: activation flags.
 * @callback: callback function.
 * @user_data: data to be poassed to the callback function.
 * @ev: exception structure.
 *
 * This function will asynchronously try to activate a component
 * given the @requirements query string. When the component is
 * activated or when the activation fails, it will call @callback
 * with the given @user_data data as parameter.
 * callback will be called with a CORBA_OBJECT_NIL object if the
 * activation fails. If the activation fails, the callback will be
 * given a human-readable string containing a description of the
 * error. In case of sucess, the error string value is undefined.
 *
 * @selection_order can be safely NULLed as well as @ev and
 * @user_data. @flags can be set to 0 if you do not know what to
 * use.
 */
void
bonobo_activation_activate_async (const char               *requirements,
				  char *const              *selection_order,
				  Bonobo_ActivationFlags    flags,
				  BonoboActivationCallback  async_cb,
				  gpointer                  user_data,
				  CORBA_Environment        *opt_ev)
{
	Bonobo_ActivationContext  ac;
	AsyncActivationData      *async_data;
	CORBA_Environment        *ev, tempenv;
	Bonobo_StringList         selorder;
	gpointer                  args [4];

	if (!requirements) {
		async_cb (CORBA_OBJECT_NIL, ASYNC_ERROR_NO_REQUIREMENTS, user_data);
		return;
	}

	ac = bonobo_activation_activation_context_get ();
	if (!ac) {
		async_cb (CORBA_OBJECT_NIL, ASYNC_ERROR_NO_CONTEXT, user_data);
		return;
	}

	if (!opt_ev) {
		CORBA_exception_init (&tempenv);
		ev = &tempenv;
	} else
		ev = opt_ev;

	async_data = g_new (AsyncActivationData, 1);
	async_data->user_cb   = async_cb;
	async_data->user_data = user_data;

	copy_strv_to_sequence (selection_order, &selorder);

	args [0] = &requirements;
	args [1] = &selorder;
	args [2] = &activation_environment;
	args [3] = &flags;

	if (!activate_matching_method)
		setup_methods ();

	ORBit_small_invoke_async (ac, activate_matching_method,
				  activation_async_callback, async_data,
				  args, bonobo_activation_context_get (), ev);

	if (ev->_major != CORBA_NO_EXCEPTION) {
		async_cb (CORBA_OBJECT_NIL, ASYNC_ERROR_INV_FAILED, user_data);
		g_free (async_data);
	}

	if (!opt_ev)
		CORBA_exception_free (&tempenv);

	return;
}

/**
 * bonobo_activation_activate_from_id_async:
 * @aid: the AID or IID of the component to activate.
 * @flags: activation flags.
 * @callback: callback function.
 * @user_data: data to be poassed to the callback function.
 * @ev: exception structure.
 *
 * This function will asynchronously try to activate a component
 * with the given @aid. When the component is
 * activated or when the activation fails, it will call @callback
 * with the given @user_data data as parameter.
 * callback will be called with a CORBA_OBJECT_NIL object if the
 * activation fails. If the activation fails, the callback will be
 * given a human-readable string containing a description of the
 * error. In case of sucess, the error string value is undefined.
 *
 * @flags can be 0 if you do not know what to set it to and
 * @ev can be safely set to NULL.
 */
void
bonobo_activation_activate_from_id_async (const Bonobo_ActivationID  aid,
					  Bonobo_ActivationFlags     flags,
					  BonoboActivationCallback   async_cb,
					  gpointer                   user_data,
					  CORBA_Environment         *opt_ev)
{
	Bonobo_ActivationContext  ac;
	AsyncActivationData      *async_data;
	CORBA_Environment        *ev, tempenv;
	gpointer                  args [2];

	if (!aid) {
		async_cb (CORBA_OBJECT_NIL, ASYNC_ERROR_NO_AID, user_data);
		return;
	}

	if (!strncmp ("OAFIID:", aid, 7)) {
		char *requirements;

		requirements = g_alloca (strlen (aid) + sizeof ("iid == ''"));
		sprintf (requirements, "iid == '%s'", aid);

		return bonobo_activation_activate_async (
				requirements, NULL, flags, async_cb, user_data, opt_ev);
	}
	
	if (!opt_ev) {
		CORBA_exception_init (&tempenv);
		ev = &tempenv;
	} else
		ev = opt_ev;

	ac = bonobo_activation_internal_activation_context_get_extended (
				(flags & Bonobo_ACTIVATION_FLAG_EXISTING_ONLY), ev);
	if (!ac) {
		if (!opt_ev)
			CORBA_exception_free (&tempenv);

		async_cb (CORBA_OBJECT_NIL, ASYNC_ERROR_NO_CONTEXT, user_data);
		return;
	}

	async_data = g_new (AsyncActivationData, 1);
	async_data->user_cb   = async_cb;
	async_data->user_data = user_data;

	if (!activate_from_aid_method)
		setup_methods ();

	args [0] = (gpointer) &aid;
	args [1] = &flags;

	ORBit_small_invoke_async (ac, activate_from_aid_method,
				  activation_async_callback, async_data,
				  args, bonobo_activation_context_get (), ev);

	if (ev->_major != CORBA_NO_EXCEPTION) {
		async_cb (CORBA_OBJECT_NIL, ASYNC_ERROR_INV_FAILED, user_data);
		g_free (async_data);
	}

	if (!opt_ev)
		CORBA_exception_free (&tempenv);

	return;

}

void
bonobo_activation_init_activation_env (void)
{
	int i, j, num_items = 0;

	struct {
		const char *name;
		const char *value;
	} getenv_values[] = {
		{ "DISPLAY",         NULL }, /* X display */
		{ "SESSION_MANAGER", NULL }, /* XSMP session manager */
		{ "AUDIODEV",        NULL }, /* Audio device on Sun systems */
		{ "LANG",            NULL }, /* Fallback locale name */
		{ NULL,              NULL }
	};

	struct {
		int         category;
		const char *name;
		const char *value;
	} setlocale_values[] =  { /* locale information: see setlocale(3) */
		{ LC_ALL,      "LC_ALL",          NULL },
		{ LC_COLLATE,  "LC_COLLATE",      NULL }, 
		{ LC_MESSAGES, "LC_MESSAGES",     NULL },
		{ LC_MONETARY, "LC_MONETARY",     NULL },
		{ LC_NUMERIC,  "LC_NUMERIC",      NULL },
		{ LC_TIME,     "LC_TIME",         NULL },
		{ 0,           NULL,              NULL }
	};

	for (i = 0; getenv_values [i].name; i++) {
		getenv_values [i].value = getenv (getenv_values [i].name);

		if (getenv_values [i].value)
			num_items++;
	}

	for (i = 0; setlocale_values [i].name; i++) {
		setlocale_values [i].value = setlocale (setlocale_values [i].category, NULL);

		if (!setlocale_values [i].value)
			setlocale_values [i].value = getenv (setlocale_values [i].name);

		if (setlocale_values [i].value) {
			num_items++;
			if (setlocale_values [i].category == LC_ALL)
				break; /* LC_ALL overrides all others */
		}
	}

	if (!num_items)
		return;

	activation_environment._length  = activation_environment._maximum = num_items;
	activation_environment._buffer  = Bonobo_ActivationEnvironment_allocbuf (num_items);
	activation_environment._release = TRUE;

	j = 0;

	for (i = 0; getenv_values [i].name; i++) {
		if (!getenv_values [i].value)
			continue;

		Bonobo_ActivationEnvValue_set (
			&activation_environment._buffer [j++],
			getenv_values [i].name,
			getenv_values [i].value);
	}


	for (i = 0; setlocale_values [i].name; i++) {
		if (!setlocale_values [i].value)
			continue;

		Bonobo_ActivationEnvValue_set (
			&activation_environment._buffer [j++],
			setlocale_values [i].name,
			setlocale_values [i].value);
	}

	g_assert (j == num_items);
}

void
bonobo_activation_set_activation_env_value (const char *name,
					    const char *value)
{
	Bonobo_ActivationEnvValue *old_buffer;
	int                        i;

	g_return_if_fail (name != NULL);

	for (i = 0; i < activation_environment._length; i++)
		if (!strcmp (activation_environment._buffer [i].name, name)) {
			Bonobo_ActivationEnvValue_set (
				&activation_environment._buffer [i], name, value);
			break;
		}

	if (i > 0 && i != activation_environment._length)
		return; /* We've overwritten a value */

	old_buffer = activation_environment._buffer;

	activation_environment._length++;
	activation_environment._maximum++;
	activation_environment._buffer  = Bonobo_ActivationEnvironment_allocbuf (
							activation_environment._length);
	activation_environment._release = TRUE;

	for (i = 0; i < activation_environment._length - 1; i++)
		Bonobo_ActivationEnvValue_copy (
			&activation_environment._buffer [i], &old_buffer [i]);

	Bonobo_ActivationEnvValue_set (&activation_environment._buffer [i], name, value);

	if (old_buffer)
		CORBA_free (old_buffer);
}

/**
 * bonobo_activation_name_service_get:
 * @ev: %CORBA_Environment structure which will contain 
 *      the CORBA exception status of the operation. 
 *
 * Returns the name server of bonobo-activation. @ev can be NULL.
 *
 * Return value: the name server of bonobo-activation.
 */
CORBA_Object
bonobo_activation_name_service_get (CORBA_Environment * ev)
{
	return bonobo_activation_activate_from_id (
                "OAFIID:Bonobo_CosNaming_NamingContext", 0, NULL, ev);
}
