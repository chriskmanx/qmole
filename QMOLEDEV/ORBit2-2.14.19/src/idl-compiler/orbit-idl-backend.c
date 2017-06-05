/*
 * orbit-idl-backend.c:
 *
 * Copyright (C) 2002 Sun Microsystems, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * Authors:
 *	Mark McLoughlin <mark@skynet.ie>
 */

#include "config.h"

#include "orbit-idl-backend.h"
#include "orbit-idl2.h"

#include <glib.h>
#include <gmodule.h>

static GSList *
prepend_from_env_var (GSList     *paths,
		      const char *env_var)
{
	char  *val;
	char **strv;
	int    i;

	if (!(val = getenv ("GNOME2_PATH")))
		return paths;

	strv = g_strsplit (val, ";", -1);
	for (i = 0; strv [i]; i++)
		paths = g_slist_prepend (
				paths, g_strconcat (strv [i], "/lib/orbit-2.0/idl-backends", NULL));

	g_strfreev (strv);

	return paths;
}

static ORBitIDLBackendFunc
load_language_backend (const char *path,
		       const char *language)
{
	ORBitIDLBackendFunc  retval = NULL;
	GModule             *module;
	char                *modname;
	char                *modpath;

	modname = g_strconcat ("ORBit-idl-backend-", language, NULL);
	modpath = g_module_build_path (path, modname);
	g_free (modname);

	if (!(module = g_module_open (modpath, G_MODULE_BIND_LAZY))) {
		g_free (modpath);
		return NULL;
	}

	if (!g_module_symbol (module, "orbit_idl_backend_func", (gpointer *) &retval))
		g_warning ("backend %s has no \"orbit_idl_backend_func\" defined", modpath);

	g_free (modpath);

	return retval;
}

gboolean
orbit_idl_backend_output (OIDL_Run_Info *rinfo,
			  IDL_tree       tree)
{
	ORBitIDLBackendFunc     func = NULL;
	ORBitIDLBackendContext  context;
	GSList                 *paths = NULL;
	GSList                 *l;

	paths = prepend_from_env_var (paths, "GNOME2_PATH");
	paths = prepend_from_env_var (paths, "ORBIT_BACKENDS_PATH");

	paths = g_slist_prepend (paths, g_strdup (ORBIT_BACKENDS_DIR));

	if (rinfo->backend_directory)
		paths = g_slist_prepend (paths, g_strdup (rinfo->backend_directory));

	for (l = paths; l; l = l->next) {
		func = load_language_backend (l->data, rinfo->output_language);

		g_free (l->data);

		if (func)
			break;
	}

	g_slist_free (paths);

	if (!func) {
		g_warning("idl-compiler backend not found.");
		return FALSE;
	}

	context.tree      = tree;
	context.filename  = rinfo->input_filename;
	context.do_stubs  = (rinfo->enabled_passes & OUTPUT_STUBS ? 1 : 0);
	context.do_skels  = (rinfo->enabled_passes & OUTPUT_SKELS ? 1 : 0);
	context.do_common = (rinfo->enabled_passes & OUTPUT_COMMON ? 1 : 0);

	return func (&context);
}
