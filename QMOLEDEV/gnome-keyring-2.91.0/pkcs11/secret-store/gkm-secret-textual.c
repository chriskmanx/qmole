/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-secret-textual.c - Textual non-encrypted format for the keyring

   Copyright (C) 2007, 2009 Stefan Walter

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

#include "gkm-secret-collection.h"
#include "gkm-secret-compat.h"
#include "gkm-secret-data.h"
#include "gkm-secret-fields.h"
#include "gkm-secret-item.h"
#include "gkm-secret-textual.h"

#include "egg/egg-error.h"
#include "egg/egg-hex.h"
#include "egg/egg-secure-memory.h"

#include "gkm/gkm-secret.h"

#include <glib.h>

#include <sys/types.h>

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static void
key_file_set_uint64 (GKeyFile *file, const gchar *group,
                     const gchar *key, guint64 value)
{
	gchar buffer[64];
	g_snprintf (buffer, sizeof (buffer), "%llu",
	            (long long unsigned int)value);
	g_key_file_set_value (file, group, key, buffer);
}

static gboolean
key_file_get_uint64 (GKeyFile *file, const gchar *group,
                     const gchar *key, guint64 *value)
{
	gchar *str, *end;

	str = g_key_file_get_value (file, group, key, NULL);
	if (!str)
		return FALSE;

	*value = g_ascii_strtoull (str, &end, 10);
	if (end[0]) {
		g_free (str);
		return FALSE;
	}

	g_free (str);
	return TRUE;
}

static void
generate_attributes (GKeyFile *file, GkmSecretItem *item)
{
	GHashTable *attributes;
	gchar *groupname;
	GList *names, *l;
	guint32 number;
	gint index = 0;

	attributes = gkm_secret_item_get_fields (item);
	g_return_if_fail (attributes);

	names = gkm_secret_fields_get_names (attributes);
	for (l = names; l; l = g_list_next (l)) {
		groupname = g_strdup_printf ("%s:attribute%d",
		                             gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item)),
		                             index);

		g_key_file_set_string (file, groupname, "name", l->data);

		/*
		 * COMPATIBILITY:
		 *
		 * Our new Secrets API doesn't support integer attributes. However, to have
		 * compatibility with old keyring code reading this file, we need to set
		 * the type=uint32 attribute appropriately where expected.
		 *
		 * If there's an extra compat-uint32 attribute and the name of this attribute
		 * is contained in that list, then write as a uint32.
		 */

		/* Determine if it's a uint32 compatible value, and store as such if it is */
		if (gkm_secret_fields_get_compat_uint32 (attributes, l->data, &number)) {
			g_key_file_set_string (file, groupname, "type", "uint32");
			key_file_set_uint64 (file, groupname, "value", number);

		/* A normal string attribute */
		} else {
			g_key_file_set_string (file, groupname, "type", "string");
			g_key_file_set_string (file, groupname, "value", gkm_secret_fields_get (attributes, l->data));
		}

		g_free (groupname);
		++index;
	}
}

static void
parse_attributes (GKeyFile *file, GkmSecretItem *item, const gchar **groups)
{
	GHashTable *attributes;
	const gchar *identifier;
	const gchar **g;
	gchar *prefix;
	gchar *name, *type;
	guint64 number;

	/* Now do the attributes */

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item));
	prefix = g_strdup_printf ("%s:attribute", identifier);
	attributes = gkm_secret_fields_new ();

	for (g = groups; *g; ++g) {
		if (!g_str_has_prefix (*g, prefix))
			continue;

		name = g_key_file_get_string (file, *g, "name", NULL);
		if (!name)
			continue;

		type = g_key_file_get_string (file, *g, "type", NULL);

		/* A uint32 type value */
		if (type && g_str_equal (type, "uint32")) {
			if (key_file_get_uint64 (file, *g, "value", &number))
				gkm_secret_fields_add_compat_uint32 (attributes, name, number);
			g_free (name);

		/* A string type value */
		} else {
			gkm_secret_fields_take (attributes, name,
			                        g_key_file_get_string (file, *g, "value", NULL));
		}

		g_free (type);
	}

	gkm_secret_item_set_fields (item, attributes);
	g_hash_table_unref (attributes);
	g_free (prefix);
}

static void
generate_acl (GKeyFile *file, GkmSecretItem *item)
{
	const gchar *identifier;
	GkmSecretAccess *ac;
	gchar *groupname;
	GList *acl;
	gint i;

	/*
	 * COMPATIBILITY: If we loaded ACLs and they're set on the item,
	 * then store them back in.
	 */

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item));
	acl = g_object_get_data (G_OBJECT (item), "compat-acl");
	for (i = 0; acl != NULL; acl = g_list_next (acl), ++i) {
		ac = acl->data;

		/* Build a group name */
		groupname = g_strdup_printf ("%s:acl%d", identifier, i);

		if (ac->display_name)
			g_key_file_set_string (file, groupname, "display-name", ac->display_name);
		if (ac->pathname)
			g_key_file_set_string (file, groupname, "path", ac->pathname);

		g_key_file_set_boolean (file, groupname, "read-access",
		                        ac->types_allowed & GKM_SECRET_ACCESS_READ);
		g_key_file_set_boolean (file, groupname, "write-access",
		                        ac->types_allowed & GKM_SECRET_ACCESS_WRITE);
		g_key_file_set_boolean (file, groupname, "remove-access",
		                        ac->types_allowed & GKM_SECRET_ACCESS_REMOVE);

		g_free (groupname);
	}
}

static void
parse_acl (GKeyFile *file, GkmSecretItem *item, const gchar **groups)
{
	GkmSecretAccessType access_type;
	GkmSecretAccess *ac;
	const gchar *identifier;
	const gchar **g;
	gchar *prefix;
	gchar *path, *display;
	GError *err = NULL;
	GList *acl;

	/*
	 * COMPATIBILITY: We don't actually use ACLs, but if we find them in the
	 * file, then load them and save back later.
	 */

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item));
	prefix = g_strdup_printf ("%s:acl", identifier);
	acl = NULL;

	for (g = groups; *g; ++g) {
		if (!g_str_has_prefix (*g, prefix))
			continue;
		path = g_key_file_get_string (file, *g, "path", NULL);
		if (!path)
			continue;

		display = g_key_file_get_string (file, *g, "display-name", NULL);

		access_type = 0;

		if (g_key_file_get_boolean (file, *g, "read-access", &err) && !err)
			access_type |= GKM_SECRET_ACCESS_READ;
		g_clear_error (&err);

		if (g_key_file_get_boolean (file, *g, "write-access", &err) && !err)
			access_type |= GKM_SECRET_ACCESS_WRITE;
		g_clear_error (&err);

		if (g_key_file_get_boolean (file, *g, "remove-access", &err) && !err)
			access_type |= GKM_SECRET_ACCESS_REMOVE;
		g_clear_error (&err);

		ac = g_new0 (GkmSecretAccess, 1);
		ac->display_name = display;
		ac->pathname = path;
		ac->types_allowed = access_type;

		acl = g_list_prepend (acl, ac);
	}

	g_object_set_data_full (G_OBJECT (item), "compat-acl", acl, gkm_secret_compat_acl_free);
	g_free (prefix);
}

static void
generate_item (GKeyFile *file, GkmSecretItem *item, GkmSecretData *sdata)
{
	GkmSecretObject *obj;
	GHashTable *attributes;
	const gchar *value;
	const gchar *identifier;
	const guchar *secret;
	gsize n_secret;
	gchar *hex;

	g_assert (file);
	g_assert (GKM_IS_SECRET_ITEM (item));
	g_assert (GKM_IS_SECRET_DATA (sdata));

	obj = GKM_SECRET_OBJECT (item);
	identifier = gkm_secret_object_get_identifier (obj);
	attributes = gkm_secret_item_get_fields (item);

	value = gkm_secret_item_get_schema (item);
	g_key_file_set_integer (file, identifier, "item-type",
	                        gkm_secret_compat_parse_item_type (value));

	value = gkm_secret_object_get_label (obj);
	if (value != NULL)
		g_key_file_set_string (file, identifier, "display-name", value);

	secret = gkm_secret_data_get_raw (sdata, identifier, &n_secret);
	if (secret != NULL) {
		/* A textual secret. Note that secrets are always null-terminated. */
		if (g_utf8_validate ((gchar*)secret, n_secret, NULL)) {
			g_key_file_set_value (file, identifier, "secret", (gchar*)secret);

		/* A non-textual secret */
		} else {
			hex = egg_hex_encode (secret, n_secret);
			g_key_file_set_value (file, identifier, "binary-secret", hex);
			g_free (hex);
		}
	}

	key_file_set_uint64 (file, identifier, "mtime", gkm_secret_object_get_modified (obj));
	key_file_set_uint64 (file, identifier, "ctime", gkm_secret_object_get_created (obj));

	generate_attributes (file, item);
	generate_acl (file, item);
}

static void
parse_item (GKeyFile *file, GkmSecretItem *item, GkmSecretData *sdata,
            const gchar **groups)
{
	GkmSecretObject *obj;
	GHashTable *attributes;
	const gchar *identifier;
	GError *err = NULL;
	GkmSecret *secret;
	guchar *binary;
	gsize n_binary;
	gchar *val;
	guint64 num;
	gint type;

	/* First the main item data */

	obj = GKM_SECRET_OBJECT (item);
	identifier = gkm_secret_object_get_identifier (obj);
	attributes = gkm_secret_item_get_fields (item);

	type = g_key_file_get_integer (file, identifier, "item-type", &err);
	if (err) {
		g_clear_error (&err);
		type = 0;
	}
	gkm_secret_item_set_schema (item, gkm_secret_compat_format_item_type (type));

	val = g_key_file_get_string (file, identifier, "display-name", NULL);
	gkm_secret_object_set_label (obj, val);
	g_free (val);

	if (sdata) {
		secret = NULL;

		/* A textual secret */
		val = g_key_file_get_string (file, identifier, "secret", NULL);
		if (val != NULL) {
			secret = gkm_secret_new_from_password (val);
			g_free (val);

		/* A binary secret */
		} else {
			val = g_key_file_get_string (file, identifier, "binary-secret", NULL);
			if (val != NULL) {
				binary = egg_hex_decode (val, -1, &n_binary);
				secret = gkm_secret_new (binary, n_binary);
				g_free (binary);
				g_free (val);
			}
		}

		/* Put the secret in the right place */
		if (secret == NULL) {
			gkm_secret_data_remove_secret (sdata, identifier);
		} else {
			gkm_secret_data_set_secret (sdata, identifier, secret);
			g_object_unref (secret);
		}
	}

	num = 0;
	if (key_file_get_uint64 (file, identifier, "mtime", &num))
		gkm_secret_object_set_modified (obj, num);
	num = 0;
	if (key_file_get_uint64 (file, identifier, "ctime", &num))
		gkm_secret_object_set_created (obj, num);

	/* Now the other stuff */
	parse_attributes (file, item, groups);
	parse_acl (file, item, groups);
}

GkmDataResult
gkm_secret_textual_write (GkmSecretCollection *collection, GkmSecretData *sdata,
                          guchar **data, gsize *n_data)
{
	GkmSecretObject *obj;
	GList *items, *l;
	const gchar *value;
	GKeyFile *file;
	GError *err = NULL;
	gint idle_timeout;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (collection), GKM_DATA_FAILURE);
	g_return_val_if_fail (GKM_IS_SECRET_DATA (sdata), GKM_DATA_LOCKED);
	g_return_val_if_fail (data && n_data, GKM_DATA_FAILURE);

	obj = GKM_SECRET_OBJECT (collection);
	file = g_key_file_new ();

	value = gkm_secret_object_get_label (obj);
	if (value != NULL)
		g_key_file_set_string (file, "keyring", "display-name", value);

	key_file_set_uint64 (file, "keyring", "ctime", gkm_secret_object_get_created (obj));
	key_file_set_uint64 (file, "keyring", "mtime", gkm_secret_object_get_modified (obj));

	idle_timeout = gkm_secret_collection_get_lock_idle (collection);
	g_key_file_set_boolean (file, "keyring", "lock-on-idle", idle_timeout > 0);
	if (idle_timeout)
		g_key_file_set_integer (file, "keyring", "lock-timeout", idle_timeout);
	idle_timeout = gkm_secret_collection_get_lock_after (collection);
	g_key_file_set_boolean (file, "keyring", "lock-after", idle_timeout > 0);
	if (idle_timeout)
		g_key_file_set_integer (file, "keyring", "lock-timeout", idle_timeout);

	items = gkm_secret_collection_get_items (collection);
	for (l = items; l; l = g_list_next (l))
		generate_item (file, l->data, sdata);
	g_list_free (items);

	*data = (guchar*)g_key_file_to_data (file, n_data, &err);
	g_key_file_free (file);

	if (!*data) {
		g_warning ("couldn't generate textual keyring file: %s", egg_error_message (err));
		return GKM_DATA_FAILURE;
	}

	return GKM_DATA_SUCCESS;
}

static void
remove_unavailable_item (gpointer key, gpointer dummy, gpointer user_data)
{
	/* Called to remove items from a keyring that no longer exist */

	GkmSecretCollection *collection = GKM_SECRET_COLLECTION (user_data);
	GkmSecretItem *item;

	g_assert (GKM_IS_SECRET_COLLECTION (collection));

	item = gkm_secret_collection_get_item (collection, key);
	if (item != NULL)
		gkm_secret_collection_remove_item (collection, item);
}

GkmDataResult
gkm_secret_textual_read (GkmSecretCollection *collection, GkmSecretData *sdata,
                         const guchar *data, gsize n_data)
{
	GkmSecretObject *obj;
	GkmSecretItem *item;
	GList *items, *l;
	GError *err = NULL;
	GKeyFile *file = NULL;
	gchar **groups = NULL;
	GkmDataResult res = GKM_DATA_FAILURE;
	gchar *start = NULL;
	const gchar *identifier;
	GHashTable *checks = NULL;
	gint lock_timeout;
	gchar *value;
	guint64 num;
	gchar **g;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (collection), GKM_DATA_FAILURE);
	g_return_val_if_fail (!sdata || GKM_IS_SECRET_DATA (sdata), GKM_DATA_FAILURE);

	file = g_key_file_new ();
	obj = GKM_SECRET_OBJECT (collection);

	if (!n_data) {
		res = GKM_DATA_UNRECOGNIZED;
		goto done;
	}

	if (!g_key_file_load_from_data (file, (const gchar*)data, n_data, G_KEY_FILE_NONE, &err)) {
		if (g_error_matches (err, G_KEY_FILE_ERROR, G_KEY_FILE_ERROR_PARSE))
			res = GKM_DATA_UNRECOGNIZED;
		goto done;
	}

	start = g_key_file_get_start_group (file);
	if (!start || !g_str_equal (start, "keyring")) {
		g_message ("invalid keyring file: wrong header group");
		goto done;
	}

	value = g_key_file_get_string (file, "keyring", "display-name", NULL);
	gkm_secret_object_set_label (obj, value);
	g_free (value);

	num = 0;
	key_file_get_uint64 (file, "keyring", "ctime", &num);
	gkm_secret_object_set_created (obj, num);

	num = 0;
	key_file_get_uint64 (file, "keyring", "mtime", &num);
	gkm_secret_object_set_modified (obj, num);

	/* Not currently used :( */
	lock_timeout = g_key_file_get_integer (file, "keyring", "lock-timeout", NULL);
	if (g_key_file_get_boolean (file, "keyring", "lock-after", NULL))
		gkm_secret_collection_set_lock_idle (collection, lock_timeout);
	else if (g_key_file_get_boolean (file, "keyring", "lock-on-idle", NULL))
		gkm_secret_collection_set_lock_idle (collection, lock_timeout);

	g_object_set_data (G_OBJECT (collection), "lock-timeout", GINT_TO_POINTER (lock_timeout));

	/* Build a Hash table where we can track ids we haven't yet seen */
	checks = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	items = gkm_secret_collection_get_items (collection);
	for (l = items; l; l = g_list_next (l)) {
		identifier = gkm_secret_object_get_identifier (l->data);
		g_hash_table_replace (checks, g_strdup (identifier), "unused");
	}
	g_list_free (items);

	groups = g_key_file_get_groups (file, NULL);
	for (g = groups; *g; ++g) {
		identifier = *g;
		if (g_str_equal (identifier, "keyring") || strchr (identifier, ':'))
			continue;

		/* We've seen this id */
		g_hash_table_remove (checks, identifier);

		item = gkm_secret_collection_get_item (collection, identifier);
		if (item == NULL)
			item = gkm_secret_collection_new_item (collection, identifier);
		parse_item (file, item, sdata, (const gchar**)groups);
	}

	g_hash_table_foreach (checks, (GHFunc)remove_unavailable_item, collection);
	res = GKM_DATA_SUCCESS;

done:
	if (checks)
		g_hash_table_destroy (checks);
	if (file)
		g_key_file_free (file);
	g_strfreev (groups);
	g_free (start);
	g_clear_error (&err);

	return res;
}
