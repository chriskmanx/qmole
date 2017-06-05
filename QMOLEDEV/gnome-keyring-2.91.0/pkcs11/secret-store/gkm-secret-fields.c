/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gkm-secret-fields.h"

#include "egg/egg-hex.h"

#include "gkm/gkm-attributes.h"

#include <ctype.h>
#include <string.h>

static gboolean
begins_with (const gchar *string, const gchar *prefix)
{
	gsize len = strlen (prefix);
	return (strncmp (string, prefix, len) == 0);
}

static gboolean
is_compat_name (const gchar *name)
{
	g_assert (name);
	return begins_with (name, "gkr:compat:");
}

static gchar*
make_compat_hashed_name (const gchar *name)
{
	g_assert (!is_compat_name (name));
	return g_strdup_printf ("gkr:compat:hashed:%s", name);
}

static gchar*
make_compat_uint32_name (const gchar *name)
{
	g_assert (!is_compat_name (name));
	return g_strdup_printf ("gkr:compat:uint32:%s", name);
}

static gboolean
string_ptr_equal (const gchar *one, const gchar *two)
{
	if (one == two)
		return TRUE;
	if (!one || !two)
		return FALSE;
	return g_str_equal (one, two);
}

static gint
string_ptr_compare (gconstpointer one, gconstpointer two)
{
	if (one == two)
		return 0;
	if (!one || !two)
		return one < two;
	return strcmp (one, two);
}

static gboolean
parse_uint32 (const gchar *value, guint32 *result)
{
	gchar *end;
	g_assert (value);
	g_assert (result);
	*result = strtoul(value, &end, 10);
	return (*end == '\0');
}

static gchar*
format_uint32 (guint32 value)
{
	return g_strdup_printf ("%u", value);
}

static gboolean
compat_hash_value_as_uint32 (const gchar *value, guint32 *hash)
{
	guint32 x;

	if (!value || !parse_uint32 (value, &x))
		return FALSE;

	/* The same algorithm as the old keyring code used */
	*hash = 0x18273645 ^ x ^ (x << 16 | x >> 16);
	return TRUE;
}

static gchar*
compat_hash_value_as_string (const gchar *value)
{
	guchar digest[16];

	if (!value)
		return NULL;

	g_assert (gcry_md_get_algo_dlen (GCRY_MD_MD5) == sizeof (digest));
	gcry_md_hash_buffer (GCRY_MD_MD5, (void*)digest, value, strlen (value));

	/* The old keyring code used lower case hex */
	return egg_hex_encode_full (digest, sizeof (digest), FALSE, '\0', 0);
}

GType
gkm_secret_fields_boxed_type (void)
{
	static volatile gsize type_inited = 0;
	static GType type = 0;

	if (g_once_init_enter (&type_inited)) {
		type = g_boxed_type_register_static ("GHashTable_Fields",
		                                     (GBoxedCopyFunc)g_hash_table_ref,
		                                     (GBoxedFreeFunc)g_hash_table_unref);
		g_once_init_leave (&type_inited, 1);
	}

	return type;
}

GHashTable*
gkm_secret_fields_new (void)
{
	return g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
}

CK_RV
gkm_secret_fields_parse (CK_ATTRIBUTE_PTR attr, GHashTable **fields)
{
	GHashTable *result;
	const gchar *name;
	gsize n_name;
	const gchar *value;
	gsize n_value;
	const gchar *ptr;
	const gchar *last;

	g_assert (attr);
	g_assert (fields);

	ptr = attr->pValue;
	last = ptr + attr->ulValueLen;

	if (!ptr && last != ptr)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	result = gkm_secret_fields_new ();

	while (ptr && ptr != last) {
		g_assert (ptr < last);

		name = ptr;
		ptr = memchr (ptr, 0, last - ptr);

		/* No value is present? */
		if (!ptr) {
			g_hash_table_unref (result);
			return CKR_ATTRIBUTE_VALUE_INVALID;
		}

		n_name = ptr - name;
		value = ++ptr;
		ptr = memchr (ptr, 0, last - ptr);

		/* Missing null terminator on value */
		if (ptr == NULL) {
			g_hash_table_unref (result);
			return CKR_ATTRIBUTE_VALUE_INVALID;
		}

		n_value = ptr - value;
		++ptr;

		/* Validate the name and value*/
		if (!g_utf8_validate (name, n_name, NULL) ||
		    !g_utf8_validate (value, n_value, NULL)) {
			g_hash_table_unref (result);
			return CKR_ATTRIBUTE_VALUE_INVALID;
		}

		g_hash_table_replace (result, g_strndup (name, n_name), g_strndup (value, n_value));
	}

	*fields = result;
	return CKR_OK;
}

static void
each_field_append (gpointer key, gpointer value, gpointer user_data)
{
	GString *result = user_data;
	g_string_append (result, key);
	g_string_append_c (result, '\0');
	g_string_append (result, value);
	g_string_append_c (result, '\0');
}

static void
each_field_length (gpointer key, gpointer value, gpointer user_data)
{
	gsize *length = user_data;
	*length += strlen (key);
	*length += strlen (value);
	*length += 2;
}

CK_RV
gkm_secret_fields_serialize (CK_ATTRIBUTE_PTR attr, GHashTable *fields)
{
	GString *result;
	gsize length;
	CK_RV rv;

	g_assert (attr);
	g_assert (fields);

	if (!attr->pValue) {
		length = 0;
		g_hash_table_foreach (fields, each_field_length, &length);
		attr->ulValueLen = length;
		return CKR_OK;
	}

	result = g_string_sized_new (256);
	g_hash_table_foreach (fields, each_field_append, result);

	rv = gkm_attribute_set_data (attr, result->str, result->len);
	g_string_free (result, TRUE);

	return rv;
}

gboolean
gkm_secret_fields_match (GHashTable *haystack, GHashTable *needle)
{
	GHashTableIter iter;
	const gchar *key, *value, *hay;
	gchar *other_key, *hashed;
	gboolean match;
	guint32 number;

	g_return_val_if_fail (haystack, FALSE);
	g_return_val_if_fail (needle, FALSE);

	g_hash_table_iter_init (&iter, needle);
	while (g_hash_table_iter_next (&iter, (gpointer*)&key, (gpointer*)&value)) {
		g_assert (key && value);

		/* Compat attributes in the needle make no difference */
		if (is_compat_name (key))
			continue;

		/* A direct match? */
		if (g_hash_table_lookup_extended (haystack, key, NULL, (gpointer*)&hay)) {
			match = string_ptr_equal (hay, value);
			if (!match)
				return FALSE;
			continue;
		}

		/* Try to find a hashed value? */
		other_key = make_compat_hashed_name (key);
		match = g_hash_table_lookup_extended (haystack, other_key, NULL, (gpointer*)&hay);
		g_free (other_key);

		if (!match)
			return FALSE;

		/*
		 * Now since the old keyring code would hash in two different
		 * ways depending on whether it was a uint32 or string,
		 * we need to do the same here.
		 */

		other_key = make_compat_uint32_name (key);
		if (g_hash_table_lookup (haystack, other_key)) {
			hashed = NULL;
			if (compat_hash_value_as_uint32 (value, &number))
				hashed = format_uint32 (number);
		} else {
			hashed = compat_hash_value_as_string (value);
		}
		g_free (other_key);

		/* Does the incoming hashed value match our hashed value? */
		match = string_ptr_equal (hay, hashed);
		g_free (hashed);

		if (!match)
			return FALSE;
	}

	return TRUE;
}

void
gkm_secret_fields_take (GHashTable *fields, gchar *name, gchar *value)
{
	g_return_if_fail (fields);
	g_return_if_fail (name);
	if (value == NULL)
		value = g_strdup ("");
	g_hash_table_replace (fields, name, value);
}

void
gkm_secret_fields_add (GHashTable *fields, const gchar *name,
                       const gchar *value)
{
	g_return_if_fail (fields);
	g_return_if_fail (name);
	gkm_secret_fields_take (fields, g_strdup (name), g_strdup (value));
}

const gchar*
gkm_secret_fields_get (GHashTable *fields, const gchar *name)
{
	g_return_val_if_fail (fields, NULL);
	g_return_val_if_fail (name, NULL);
	g_return_val_if_fail (!is_compat_name (name), NULL);
	return g_hash_table_lookup (fields, name);
}

GList*
gkm_secret_fields_get_names (GHashTable *fields)
{
	const gchar *prefix = "gkr:compat:hashed:";
	GList *keys, *l, *next;
	gsize len = strlen (prefix);
	gchar *last = NULL;

	g_return_val_if_fail (fields, NULL);

	keys = g_hash_table_get_keys (fields);

	/* Include hashed compat attributes as their base name */
	for (l = keys; l; l = g_list_next (l)) {
		if (strncmp (prefix, l->data, len) == 0)
			l->data = (gchar*)(l->data) + len;
	}

	/* Sort the list nicely */
	keys = g_list_sort (keys, string_ptr_compare);

	/* Remove all compat attributes, duplicates */
	for (l = keys; l; l = next) {
		next = g_list_next (l);
		if (is_compat_name (l->data) || string_ptr_equal (last, l->data))
			keys = g_list_delete_link (keys, l);
		else
			last = l->data;
	}

	return keys;
}

void
gkm_secret_fields_add_compat_uint32 (GHashTable *fields, const gchar *name,
                                     guint32 value)
{
	g_return_if_fail (fields);
	g_return_if_fail (name);
	g_return_if_fail (!is_compat_name (name));
	g_hash_table_replace (fields, g_strdup (name), format_uint32 (value));
	g_hash_table_replace (fields, make_compat_uint32_name (name), g_strdup (""));
}

gboolean
gkm_secret_fields_get_compat_uint32 (GHashTable *fields, const gchar *name,
                                     guint32 *value)
{
	gchar *other_key;
	gboolean ret;

	g_return_val_if_fail (fields, FALSE);
	g_return_val_if_fail (name, FALSE);
	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (!is_compat_name (name), FALSE);

	other_key = make_compat_uint32_name (name);
	ret = g_hash_table_lookup (fields, other_key) != NULL;
	g_free (other_key);

	if (ret)
		ret = parse_uint32 (g_hash_table_lookup (fields, name), value);

	return ret;
}

void
gkm_secret_fields_add_compat_hashed_string (GHashTable *fields, const gchar *name,
                                            const gchar *value)
{
	g_return_if_fail (fields);
	g_return_if_fail (name);
	g_return_if_fail (!is_compat_name (name));
	g_hash_table_replace (fields, make_compat_hashed_name (name), g_strdup (value));
}

gboolean
gkm_secret_fields_get_compat_hashed_string (GHashTable *fields, const gchar *name,
                                            gchar **value)
{
	gchar *other_key;
	gboolean ret;
	const gchar *val;

	g_return_val_if_fail (fields, FALSE);
	g_return_val_if_fail (name, FALSE);
	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (!is_compat_name (name), FALSE);

	/* Even though this is more expensive, it's far more common */
	if (g_hash_table_lookup_extended (fields, name, NULL, (gpointer*)&val)) {
		*value = compat_hash_value_as_string (val);
		return TRUE;
	}

	/* See if we already have it hashed */
	other_key = make_compat_hashed_name (name);
	ret = g_hash_table_lookup_extended (fields, other_key, NULL, (gpointer*)&val);
	g_free (other_key);

	if (ret)
		*value = g_strdup (val);
	return ret;
}

void
gkm_secret_fields_add_compat_hashed_uint32 (GHashTable *fields, const gchar *name,
                                            guint32 value)
{
	g_return_if_fail (fields);
	g_return_if_fail (name);
	g_return_if_fail (!is_compat_name (name));
	g_hash_table_replace (fields, make_compat_hashed_name (name), format_uint32 (value));
	g_hash_table_replace (fields, make_compat_uint32_name (name), g_strdup (name));
}

gboolean
gkm_secret_fields_get_compat_hashed_uint32 (GHashTable *fields, const gchar *name,
                                            guint32 *value)
{
	const gchar *val;
	gchar *other_key;
	gboolean ret;

	g_return_val_if_fail (fields, FALSE);
	g_return_val_if_fail (name, FALSE);
	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (!is_compat_name (name), FALSE);

	/* Even though this is more expensive, it's far more common */

	/* Check if it's a uint32 */
	other_key = make_compat_uint32_name (name);
	ret = g_hash_table_lookup_extended (fields, other_key, NULL, NULL);
	g_free (other_key);

	/* It is a uint32 */
	if (ret == TRUE) {
		val = g_hash_table_lookup (fields, name);
		if (val && compat_hash_value_as_uint32 (val, value))
			return TRUE;
	}

	/* See if we already have it hashed */
	other_key = make_compat_hashed_name (name);
	ret = g_hash_table_lookup_extended (fields, other_key, NULL, (gpointer*)&val);
	g_free (other_key);
	if (ret)
		ret = parse_uint32 (val, value);
	return ret;
}
