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
#include <bonobo-activation/bonobo-activation-shlib.h>

#include <bonobo-activation/Bonobo_GenericFactory.h>
#include <bonobo-activation/bonobo-activation-i18n.h>
#include <bonobo-activation/bonobo-activation-init.h>
#include <bonobo-activation/bonobo-activation-private.h>
#include <gmodule.h>

/* ORBit-specific hack */
#include <orbit/poa/poa.h>

typedef struct
{
	GModule *loaded;
	int refcount;
	char filename[1];
}
ActivePluginInfo;

static GHashTable *living_by_filename = NULL;

static void
gnome_plugin_unload (gpointer data, gpointer user_data)
{
	ActivePluginInfo *api = user_data;
	g_module_close (api->loaded);
	g_hash_table_remove (living_by_filename, api->filename);
	g_free (api);
}


/**
 * bonobo_activation_activate_shlib_server:
 * @sh:
 * @ev:
 *
 * Private function.
 *
 * Return value: 
 */
CORBA_Object
bonobo_activation_activate_shlib_server (Bonobo_ActivationResult *sh, 
                                         CORBA_Environment *ev)
{
	CORBA_Object retval;
	const BonoboActivationPlugin *plugin;
	ActivePluginInfo *local_plugin_info = NULL;
	const BonoboActivationPluginObject *pobj;
	int i;
	PortableServer_POA poa;
	CORBA_ORB orb;
	char *filename;
	const char *iid;

	g_return_val_if_fail (sh->res._d == Bonobo_ACTIVATION_RESULT_SHLIB,
			      CORBA_OBJECT_NIL);
	g_return_val_if_fail (sh->res._u.res_shlib._length > 0,
			      CORBA_OBJECT_NIL);

	/* The location info is at the end to of the string list */
	filename = sh->res._u.res_shlib._buffer[sh->res._u.res_shlib._length - 1];
	if (living_by_filename)
		local_plugin_info =
			g_hash_table_lookup (living_by_filename, filename);

	if (!local_plugin_info) {
		/* We have to load the thing from scratch */
		GModule *gmod;
		gboolean success;

		gmod = g_module_open (filename, G_MODULE_BIND_LAZY);
		if (!gmod) {
                        char *error_string;
                        Bonobo_GeneralError *error = Bonobo_GeneralError__alloc ();

                        error_string = g_strdup_printf (
                                _("g_module_open of `%s' failed with `%s'"),
                                filename, g_module_error ());
                        error->description = CORBA_string_dup (error_string);
                        CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
                                             ex_Bonobo_GeneralError, error);
                        g_free (error_string);
			return CORBA_OBJECT_NIL; /* Couldn't load it */
		}
		
		success = g_module_symbol (gmod, "Bonobo_Plugin_info",
					   (gpointer *) &plugin);
		if (!success) {
                        char *error_string;
                        Bonobo_GeneralError *error = Bonobo_GeneralError__alloc ();

			g_module_close (gmod);

                        error_string = g_strdup_printf (
                                _("Can't find symbol Bonobo_Plugin_info in `%s'"),
                                filename);
                        error->description = CORBA_string_dup (error_string);
                        CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
                                             ex_Bonobo_GeneralError, error);
                        g_free (error_string);
			return CORBA_OBJECT_NIL;
		}

		local_plugin_info =
			g_malloc (sizeof (ActivePluginInfo) +
				  strlen (filename) + 1);

		local_plugin_info->refcount = 0;
		local_plugin_info->loaded = gmod;
		strcpy (local_plugin_info->filename, filename);

		if (!living_by_filename)
			living_by_filename =
				g_hash_table_new (g_str_hash, g_str_equal);

		g_hash_table_insert (living_by_filename,
				     local_plugin_info->filename,
				     local_plugin_info);
	} else {
		int success;

		success =
			g_module_symbol (local_plugin_info->loaded,
					 "Bonobo_Plugin_info",
					 (gpointer *) & plugin);
		if (!success) {
                        char *error_string;
                        Bonobo_GeneralError *error = Bonobo_GeneralError__alloc ();

                        error_string = g_strdup_printf (
                                _("Can't find symbol Bonobo_Plugin_info in `%s'"),
                                filename);
                        error->description = CORBA_string_dup (error_string);
                        CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
                                             ex_Bonobo_GeneralError, error);
                        g_free (error_string);
			return CORBA_OBJECT_NIL;
		}
	}

	retval = CORBA_OBJECT_NIL;

	orb = bonobo_activation_orb_get ();
	poa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references (orb, "RootPOA", ev);

	/* Index into the string list one element from the end to get the iid of the shlib */
	iid = sh->res._u.res_shlib._buffer[sh->res._u.res_shlib._length - 2];
	for (pobj = plugin->plugin_object_list; pobj->iid; pobj++) {
		if (strcmp (iid, pobj->iid) == 0) {
			/* Found a match */
			break;
		}
	}

	if (pobj->iid) {
		/* Activate the shlib */
		retval = pobj->activate (poa, pobj->iid, local_plugin_info, ev);

		if (ev->_major != CORBA_NO_EXCEPTION)
			retval = CORBA_OBJECT_NIL;

		/* Activate the factiories contained in the shlib */
		i =  sh->res._u.res_shlib._length - 2;
		for (i--; i >= 0 && !CORBA_Object_is_nil (retval, ev); i--) {
			CORBA_Object new_retval;

			iid = sh->res._u.res_shlib._buffer[i];

			new_retval =
				Bonobo_GenericFactory_createObject (
                                        retval, (char *) iid, ev);

			if (ev->_major != CORBA_NO_EXCEPTION
			    || CORBA_Object_is_nil (new_retval, ev)) {
                                if (ev->_major == CORBA_NO_EXCEPTION) {
                                        Bonobo_GeneralError *error = Bonobo_GeneralError__alloc ();
                                        char *error_string = g_strdup_printf (
                                                _("Factory `%s' returned NIL for `%s'"),
                                                pobj->iid, iid);
                                        error->description = CORBA_string_dup (error_string);
                                        CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
                                                             ex_Bonobo_GeneralError, error);
                                        g_free (error_string);
                                        
                                }
				new_retval = CORBA_OBJECT_NIL;
                        }

			CORBA_Object_release (retval, ev);
			retval = new_retval;
		}
	} else {
                Bonobo_GeneralError *error = Bonobo_GeneralError__alloc ();
                char *error_string = g_strdup_printf (
                        _("Shlib `%s' didn't contain `%s'"),
                        filename, iid);
                error->description = CORBA_string_dup (error_string);
                CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
                                     ex_Bonobo_GeneralError, error);
                g_free (error_string);
        }

        CORBA_Object_release ((CORBA_Object) poa, ev);

	return retval;
}

/**
 * bonobo_activation_plugin_use:
 * @servant: The servant that was created
 * @impl_ptr: The impl_ptr that was passed to the original activation routine
 *
 * You should call this routine to activate a shared library-based 
 * CORBA Object. It will be called by OAF if the component exports 
 * correctly an %BonoboActivationPlugin structure named "Bonobo_Plugin_info".
 */
void
bonobo_activation_plugin_use (PortableServer_Servant servant, gpointer impl_ptr)
{
	ActivePluginInfo *local_plugin_info = impl_ptr;

	local_plugin_info->refcount++;

#if 0
	ORBit_servant_set_deathwatch (servant, &(local_plugin_info->refcount),
				      gnome_plugin_unload, local_plugin_info);
#endif
}

static gboolean
bonobo_activation_plugin_real_unuse (gpointer impl_ptr)
{
	ActivePluginInfo *api;

	g_return_val_if_fail (impl_ptr, FALSE);

	api = impl_ptr;

	api->refcount--;

	if (api->refcount <= 0) {
		gnome_plugin_unload (&(api->refcount), api);
        }        

        return FALSE;
}

/**
 * bonobo_activation_plugin_unuse:
 * @impl_ptr: The impl_ptr that was passed to the activation routine
 *
 * Side effects: May arrange for the shared library that the
 * implementation is in to be unloaded.
 *
 * When a shlib plugin for a CORBA object is destroying an
 * implementation, it should call this function to make sure that the
 * shared library is unloaded as needed.
 */
void
bonobo_activation_plugin_unuse (gpointer impl_ptr)
{
        g_idle_add (bonobo_activation_plugin_real_unuse, impl_ptr);
}

