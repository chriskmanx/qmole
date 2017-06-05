/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-modules.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2010, Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gck.h"
#include "gck-private.h"
#include "gck-marshal.h"

#include <string.h>

/**
 * SECTION:gck-modules
 * @title: GckModule lists
 * @short_description: Dealing with lists of PKCS#11 modules.
 *
 * Xxxxx
 */

gchar**
gck_modules_list_registered_paths (GError **err)
{
	GError *error = NULL;
	const gchar *name;
	gchar *path;
	GDir *dir;
	GArray *paths;

	g_return_val_if_fail (!err || !*err, NULL);

	/* We use this below */
	if (!err)
		err = &error;

	paths = g_array_new (TRUE, TRUE, sizeof (gchar*));

	dir = g_dir_open (PKCS11_REGISTRY_DIR, 0, err);

	if (dir == NULL) {
		if (g_error_matches (*err, G_FILE_ERROR, G_FILE_ERROR_NOENT) ||
		    g_error_matches (*err, G_FILE_ERROR, G_FILE_ERROR_NOTDIR)) {
			g_clear_error (err);
			return (gchar**)g_array_free (paths, FALSE);
		} else {
			g_array_free (paths, TRUE);
			g_clear_error (&error);
			return NULL;
		}
	}

	for (;;) {
		name = g_dir_read_name (dir);
		if (!name)
			break;

		path = g_build_filename (PKCS11_REGISTRY_DIR, name, NULL);
		if (g_file_test (path, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_REGULAR))
			g_array_append_val (paths, path);
		else
			g_free (path);
	}

	g_dir_close (dir);

	return (gchar**)g_array_free (paths, FALSE);
}

GList*
gck_modules_initialize_registered (guint options)
{
	GError *err = NULL;
	gchar **paths, **p;
	GckModule *module;
	GList *results = NULL;

	paths = gck_modules_list_registered_paths (&err);
	if (!paths && err) {
		g_warning ("couldn't list registered PKCS#11 module paths: %s",
		           err && err->message ? err->message : "");
		g_clear_error (&err);
		return NULL;
	}

	for (p = paths; *p; ++p) {
		module = gck_module_initialize (*p, NULL, 0, &err);
		if (module) {
			results = g_list_prepend (results, module);

		} else {
			g_warning ("couldn't load PKCS#11 module: %s: %s",
			           *p, err && err->message ? err->message : "");
			g_clear_error (&err);
		}
	}

	g_strfreev (paths);
	return results;
}

GList*
gck_modules_get_slots (GList *modules, gboolean token_present)
{
	GList *result = NULL;
	GList *m;

	for (m = modules; m; m = g_list_next (m)) {
		result = g_list_concat (result, gck_module_get_slots (m->data, token_present));
	}

	return result;
}

/**
 * gck_module_enumerate_objects_full:
 * @self: The module to enumerate objects.
 * @attrs: Attributes that the objects must have, or empty for all objects.
 * @session_flags: Flags for opening a session.
 *
 * Setup an enumerator for listing matching objects on the modules.
 *
 * This call will not block but will return an enumerator immediately.
 *
 * XXX
 *
 * Return value: A new enumerator
 **/
GckEnumerator*
gck_modules_enumerate_objects (GList *modules, GckAttributes *attrs, guint session_options)
{
	return _gck_enumerator_new (modules, session_options, NULL, attrs);
}

GckSlot*
gck_modules_token_for_uri (GList *modules, const gchar *uri, GError **error)
{
	GckTokenInfo *match, *token;
	GckSlot *result = NULL;
	GList *slots;
	GList *m, *s;

	if (!gck_uri_parse (uri, &match, NULL, error))
		return NULL;

	for (m = modules; result == NULL && m != NULL; m = g_list_next (m)) {
		slots = gck_module_get_slots (m->data, TRUE);
		for (s = slots; result == NULL && s != NULL; s = g_list_next (s)) {
			token = gck_slot_get_token_info (s->data);
			if (token && _gck_token_info_match (match, token))
				result = g_object_ref (s->data);
			gck_token_info_free (token);
		}
		gck_list_unref_free (slots);
	}

	gck_token_info_free (match);
	return result;
}

GckObject*
gck_modules_object_for_uri (GList *modules, const gchar *uri, guint session_options,
                            GError **error)
{
	GckEnumerator *en;
	GckObject *result;

	g_return_val_if_fail (uri, NULL);
	g_return_val_if_fail (!error || !*error, NULL);

	en = gck_modules_enumerate_uri (modules, uri, session_options, error);
	if (en == NULL)
		return NULL;

	result = gck_enumerator_next (en, NULL, error);
	g_object_unref (en);

	return result;
}

GList*
gck_modules_objects_for_uri (GList *modules, const gchar *uri, guint session_options,
                             GError **error)
{
	GckEnumerator *en;
	GList *results;

	g_return_val_if_fail (uri, NULL);
	g_return_val_if_fail (!error || !*error, NULL);

	en = gck_modules_enumerate_uri (modules, uri, session_options, error);
	if (en == NULL)
		return NULL;

	results = gck_enumerator_next_n (en, -1, NULL, error);
	g_object_unref (en);

	return results;
}

GckEnumerator*
gck_modules_enumerate_uri (GList *modules, const gchar *uri, guint session_options,
                           GError **error)
{
	GckTokenInfo *token;
	GckAttributes *attrs;
	GckEnumerator *en;

	if (!gck_uri_parse (uri, &token, &attrs, error))
		return NULL;

	/* Takes ownership of token info */
	en = _gck_enumerator_new (modules, session_options, token, attrs);
	gck_attributes_unref (attrs);

	return en;
}
