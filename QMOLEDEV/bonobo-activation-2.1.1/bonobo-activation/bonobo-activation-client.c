/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  bonobo-activation-client.c: A client client to enable caching
 *
 *  Copyright (C) 2002 Ximian Inc.
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
 *  Author: Michael Meeks (michael@ximian.com)
 */

#include <config.h>
#include <bonobo-activation/bonobo-activation.h>
#include <bonobo-activation/bonobo-activation-client.h>
#include <bonobo-activation/Bonobo_ActivationContext.h>

static GSList *reset_notify_callbacks = NULL;

static void
reset_caches (void)
{
        GSList   *l;
        GVoidFunc cb;

        for (l = reset_notify_callbacks; l; l = l->next) {
                cb = l->data;
                cb ();
        }
}

void
bonobo_activation_add_reset_notify (GVoidFunc fn)
{
        if (!g_slist_find (reset_notify_callbacks, fn))
                reset_notify_callbacks = g_slist_prepend (
                        reset_notify_callbacks, fn);
}

typedef struct {
        POA_Bonobo_ActivationClient servant;
} impl_POA_Bonobo_ActivationClient;

static void
impl_Bonobo_ActivationClient__finalize (PortableServer_Servant servant,
                                        CORBA_Environment     *ev)
{
        g_free (servant);
}

static void
impl_Bonobo_ActivationClient_resetCache (PortableServer_Servant servant,
                                         CORBA_Environment     *ev)
{
        /* Reset the cache ! */
#ifdef BONOBO_ACTIVATION_DEBUG
        g_warning ("Reset cache");
#endif
        reset_caches ();
}

static PortableServer_ServantBase__epv impl_Bonobo_ActivationClient_base_epv = {
        NULL, /* private data */
        impl_Bonobo_ActivationClient__finalize,
        NULL, /* default_POA routine */
};
static POA_Bonobo_ActivationClient__epv impl_Bonobo_ActivationClient_epv = {
        NULL, /* private */
        &impl_Bonobo_ActivationClient_resetCache
};

static POA_Bonobo_Unknown__epv impl_Bonobo_Unknown_epv = {
	NULL, /* private data */
	NULL,
	NULL,
        NULL
};

static POA_Bonobo_ActivationClient__vepv impl_Bonobo_ActivationClient_vepv = {
        &impl_Bonobo_ActivationClient_base_epv,
        &impl_Bonobo_Unknown_epv,
        &impl_Bonobo_ActivationClient_epv,
};

static CORBA_Object
bonobo_activation_corba_client_new (void)
{
        CORBA_ORB orb;
        CORBA_Object retval;
        CORBA_Environment *ev, real_ev;
        PortableServer_POA poa;
        PortableServer_POAManager manager;
        impl_POA_Bonobo_ActivationClient *newservant;

        ev = &real_ev;
        CORBA_exception_init (ev);

        orb = bonobo_activation_orb_get ();

        poa = (PortableServer_POA) CORBA_ORB_resolve_initial_references (orb, "RootPOA", ev);
        manager = PortableServer_POA__get_the_POAManager (poa, ev);
        PortableServer_POAManager_activate (manager, ev);

        newservant = g_new0 (impl_POA_Bonobo_ActivationClient, 1);
        newservant->servant.vepv = &impl_Bonobo_ActivationClient_vepv;

        POA_Bonobo_ActivationClient__init ((PortableServer_Servant) newservant, ev);
        retval = PortableServer_POA_servant_to_reference (poa, newservant, ev);

        CORBA_Object_release ((CORBA_Object) manager, ev);
        CORBA_Object_release ((CORBA_Object) poa, ev);

        CORBA_exception_free (ev);

        return retval;
}

static CORBA_Object client = CORBA_OBJECT_NIL;

void
bonobo_activation_release_corba_client (void)
{
        CORBA_Environment ev;

        CORBA_exception_init (&ev);

        CORBA_Object_release (client, &ev);
        reset_caches ();

        CORBA_exception_free (&ev);
        client = CORBA_OBJECT_NIL;
}


static char *
get_lang_list (void)
{
        static char *result = NULL;
        static gboolean result_set = FALSE;
        const char *tmp;
        char *tmp2, *lang, *lang_with_locale, *equal_char;
        GString *str;
        gboolean add_comma = FALSE;
        
        lang_with_locale = NULL;
        
        if (result_set)
                return result;
        
        tmp = g_getenv ("LANGUAGE");

        if (!tmp)
                tmp = g_getenv ("LANG");
        
        lang = g_strdup (tmp);
        tmp2 = lang;

        str = g_string_new (NULL);
        
        if (lang) {
                /* envs can be in NAME=VALUE form */
		equal_char = strchr (lang, '=');
		if (equal_char)
			lang = equal_char + 1;

                /* check if the locale has a _ */
                equal_char = strchr (lang, '_');
                if (equal_char != NULL) {
                        lang_with_locale = g_strdup (lang);
                        *equal_char = 0;
                }

                if (lang_with_locale && strcmp (lang_with_locale, "")) {
                        g_string_append (str, lang_with_locale);
                        add_comma = TRUE;
                }
                if (lang && strcmp (lang, "")) {
                        if (add_comma)
                                g_string_append (str, ",");
                        g_string_append (str, lang);
                }

        }
        result_set = TRUE;
        g_free (tmp2);
        g_free (lang_with_locale);
        
        result = str->str ? str->str : "";
        g_string_free (str, FALSE);
        
        return result;
}

void
bonobo_activation_register_client (Bonobo_ActivationContext context,
                                   CORBA_Environment       *ev)
{
        if (client == CORBA_OBJECT_NIL) {
                client = bonobo_activation_corba_client_new ();
        }

        Bonobo_ActivationContext_addClient (context, client, get_lang_list (), ev);
}
