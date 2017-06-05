/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
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

#include "gkd-secret-util.h"

#include "pkcs11/pkcs11i.h"

#include <string.h>

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

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

gboolean
gkd_secret_util_parse_path (const gchar *path, gchar **collection, gchar **item)
{
	const gchar *pos;

	g_return_val_if_fail (path, FALSE);

	/* Make sure it starts with our prefix */
	if (g_str_has_prefix (path, SECRET_COLLECTION_PREFIX))
		path += strlen (SECRET_COLLECTION_PREFIX);
	else if (g_str_has_prefix (path, SECRET_ALIAS_PREFIX))
		path += strlen (SECRET_ALIAS_PREFIX);
	else
		return FALSE;

	/* Skip the path separator */
	if (path[0] != '/')
		return FALSE;
	++path;

	/* Make sure we have something */
	if (path[0] == '\0')
		return FALSE;

	pos = strchr (path, '/');

	/* No item, just a collection */
	if (pos == NULL) {
		if (collection)
			*collection = decode_object_identifier (path, -1);
		if (item)
			*item = NULL;
		return TRUE;
	}

	/* Make sure we have an item, and no further path bits */
	if (pos[1] == '\0' || strchr (pos + 1, '/'))
		return FALSE;

	if (collection)
		*collection = decode_object_identifier (path, pos - path);
	if (item)
		*item = decode_object_identifier (pos + 1, -1);
	return TRUE;
}

gchar*
gkd_secret_util_build_path (const gchar *base, gconstpointer identifier, gssize n_identifier)
{
	GString *result;
	const gchar *name;
	gsize length;

	g_assert (base);
	g_assert (base[0] == '/');
	g_assert (identifier);

	name = identifier;
	if (n_identifier < 0)
		length = strlen (name);
	else
		length = n_identifier;

	result = g_string_new (base);
	if (!g_str_has_suffix (base, "/"))
		g_string_append_c (result, '/');

	while (length > 0) {
		char ch = *(name++);
		--length;

		/* Normal characters can go right through */
		if (G_LIKELY ((ch >= 'A' && ch <= 'Z') ||
		              (ch >= 'a' && ch <= 'z') ||
		              (ch >= '0' && ch <= '9'))) {
			g_string_append_c (result, ch);

		/* Special characters are encoded with a _ */
		} else {
			g_string_append_printf (result, "_%02x", (unsigned int)(unsigned char)ch);
		}
	}

	return g_string_free (result, FALSE);
}
