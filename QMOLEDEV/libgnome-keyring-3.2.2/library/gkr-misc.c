/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-misc.c - miscellaneous utilities

   Copyright (C) 2009 Stefan Walter

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

#include "gkr-misc.h"
#include "gnome-keyring-private.h"

#include <stdlib.h>
#include <string.h>

/**
 * GnomeKeyringAttributeType:
 * @GNOME_KEYRING_ATTRIBUTE_TYPE_STRING: A UTF-8 encoded string attribute.
 * @GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32: A unsigned integer attribute.
 *
 * The data type of the item attribute.
 */

/**
 * GnomeKeyringItemInfoFlags:
 * @GNOME_KEYRING_ITEM_INFO_BASICS: Retrieve just the basic info about the item.
 * @GNOME_KEYRING_ITEM_INFO_SECRET: Retrieve the item secret.
 *
 * The type of item info to retrieve with gnome_keyring_item_get_info_full().
 */

const gchar*
gkr_service_name (void)
{
#ifdef WITH_TESTABLE
	const gchar *service = g_getenv ("GNOME_KEYRING_TEST_SERVICE");
	if (service && service[0])
		return service;
#endif
	return SECRETS_SERVICE;
}

static void
encode_object_identifier (GString *string, const gchar* name, gssize length)
{
	g_assert (name);

	if (length < 0)
		length = strlen (name);

	while (length > 0) {
		char ch = *(name++);
		--length;

		/* Normal characters can go right through */
		if (G_LIKELY ((ch >= 'A' && ch <= 'Z') ||
		              (ch >= 'a' && ch <= 'z') ||
		              (ch >= '0' && ch <= '9'))) {
			g_string_append_c (string, ch);

		/* Special characters are encoded with a _ */
		} else {
			g_string_append_printf (string, "_%02x", (unsigned int)(unsigned char)ch);
		}
	}
}

static void
encode_keyring_string (GString *string, const gchar *keyring)
{
	if (!keyring) {
		g_string_append (string, COLLECTION_DEFAULT);
	} else {
		g_string_append (string, COLLECTION_PREFIX);
		encode_object_identifier (string, keyring, -1);
	}
}

gchar*
gkr_encode_keyring_name (const gchar *keyring)
{
	GString *result = g_string_sized_new (128);
	encode_keyring_string (result, keyring);
	return g_string_free (result, FALSE);
}

gchar*
gkr_encode_keyring_item_id (const gchar *keyring, guint32 id)
{
	GString *result = g_string_sized_new (128);
	encode_keyring_string (result, keyring);
	g_string_append_c (result, '/');
	g_string_append_printf (result, "%lu", (unsigned long)id);
	return g_string_free (result, FALSE);
}

static gchar*
decode_object_identifier (const gchar* enc, gssize length)
{
	GString *result;

	g_assert (enc);

	if (length < 0)
		length = strlen (enc);

	result = g_string_sized_new (length);
	while (length > 0) {
		char ch = *(enc++);
		--length;

		/* Underscores get special handling */
		if (G_UNLIKELY (ch == '_' &&
		                g_ascii_isxdigit(enc[0]) &&
		                g_ascii_isxdigit (enc[1]))) {
			ch = (g_ascii_xdigit_value (enc[0]) * 16) +
			     (g_ascii_xdigit_value (enc[1]));
			enc += 2;
			length -= 2;
		}

		g_string_append_c (result, ch);
	}

	return g_string_free (result, FALSE);
}

gchar*
gkr_decode_keyring_name (const char *path)
{
	gchar *result;

	g_return_val_if_fail (path, NULL);

	if (!g_str_has_prefix (path, COLLECTION_PREFIX)) {
		g_message ("response from daemon contained a bad collection path: %s", path);
		return NULL;
	}

	path += strlen (COLLECTION_PREFIX);
	result = decode_object_identifier (path, -1);
	if (result == NULL) {
		g_message ("response from daemon contained a bad collection path: %s", path);
		return NULL;
	}

	return result;
}

gboolean
gkr_decode_is_keyring (const char *path)
{
	g_return_val_if_fail (path, FALSE);

	if (!g_str_has_prefix (path, COLLECTION_PREFIX))
		return FALSE;

	path += strlen (COLLECTION_PREFIX);
	return strchr (path, '/') == NULL;
}

gboolean
gkr_decode_item_id (const char *path, guint32 *id)
{
	const gchar *part;
	gchar *end;

	g_return_val_if_fail (path, FALSE);
	g_assert (id);

	part = strrchr (path, '/');
	if (part == NULL || part[1] == '\0') {
		g_message ("response from daemon contained a bad item path: %s", path);
		return FALSE;
	}

	*id = strtoul (part + 1, &end, 10);
	if (!end || end[0] != '\0') {
		g_message ("item has unsupported non-numeric item identifier: %s", path);
		return FALSE;
	}

	return TRUE;
}

gchar*
gkr_decode_keyring_item_id (const char *path, guint32* id)
{
	const gchar *part;
	const gchar *coll;
	gchar *result;
	gchar *end;

	g_return_val_if_fail (path, NULL);

	if (!g_str_has_prefix (path, COLLECTION_PREFIX)) {
		g_message ("response from daemon contained a bad collection path: %s", path);
		return NULL;
	}

	coll = path + strlen (COLLECTION_PREFIX);
	part = strrchr (coll, '/');
	if (part == NULL || part[1] == '\0') {
		g_message ("response from daemon contained a bad item path: %s", path);
		return NULL;
	}

	*id = strtoul (part + 1, &end, 10);
	if (!end || end[0] != '\0') {
		g_message ("item has unsupported non-numeric item identifier: %s", path);
		return NULL;
	}

	result = decode_object_identifier (coll, part - coll);
	if (result == NULL) {
		g_message ("response from daemon contained a bad collection path: %s", path);
		return NULL;
	}

	return result;
}
