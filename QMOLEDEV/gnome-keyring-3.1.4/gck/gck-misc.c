/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-misc.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2008, Stefan Walter

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

   Author: Stef Walter <nielsen@memberwebs.com>
*/

#include "config.h"

#include "gck.h"
#include "gck-private.h"

#include <p11-kit/p11-kit.h>

#include <glib/gi18n-lib.h>

/**
 * SECTION:gck-error
 * @title: Errors
 * @short_description: Gck Errors and error codes.
 *
 * Errors are returned as GError structures. The code member of GError
 * contains the raw PKCS11 CK_RV result value.
 */

/**
 * GCK_INVALID:
 *
 * Used as a terminator at the end of variable argument lists.
 */

/**
 * GCK_VENDOR_CODE:
 *
 * Custom PKCS11 errors that originate from the gck library, are
 * based at this error code.
 */

/**
 * CKR_GCK_MODULE_PROBLEM:
 *
 * A result code that signifies there was a problem loading a PKCS11
 * module, usually a shared library.
 *
 * More details can be found in the error string.
 */

/**
 * GCK_ERROR:
 *
 * The error domain for gck library errors.
 */
GQuark
gck_get_error_quark (void)
{
	static GQuark domain = 0;
	static volatile gsize quark_inited = 0;

	if (g_once_init_enter (&quark_inited)) {
		domain = g_quark_from_static_string ("gck-error");
		g_once_init_leave (&quark_inited, 1);
	}

	return domain;
}

/**
 * gck_message_from_rv:
 * @rv: The PKCS\#11 return value to get a message for.
 *
 * Get a message for a PKCS\#11 return value or error code. Do not
 * pass CKR_OK or other such non errors to this function.
 *
 * Return value: The user readable message.
 **/
const gchar*
gck_message_from_rv (CK_RV rv)
{
	switch (rv) {

	/* These are not really errors, or not current */
	case CKR_OK:
	case CKR_NO_EVENT:
	case CKR_FUNCTION_NOT_PARALLEL:
	case CKR_SESSION_PARALLEL_NOT_SUPPORTED:
		g_return_val_if_reached ("");

	default:
		return p11_kit_strerror (rv);
	}
}

/**
 * SECTION:gck-misc
 * @title: Miscellaneous Functions
 * @short_description: Other miscellaneous functions.
 *
 * A few supporting functions that come in handy when dealing with the gck
 * library or PKCS11 in general.
 */

/**
 * gck_list_unref_free:
 * @reflist: List of Gobject reference counted pointers.
 *
 * Free a list of GObject based pointers. All objects in the list
 * will be unreffed and then the list itself will be freed.
 **/
void
gck_list_unref_free (GList *reflist)
{
	GList *l;
	for (l = reflist; l; l = g_list_next (l)) {
		g_return_if_fail (G_IS_OBJECT (l->data));
		g_object_unref (l->data);
	}
	g_list_free (reflist);
}

/**
 * gck_list_ref_copy:
 * @reflist: List of GObject reference counted objects.
 *
 * Copy a list of GObject based pointers. All objects
 * in the list will be reffed and the list will be copied.
 *
 * Return value: The copied and reffed list. When done, free it with
 * gck_list_unref_free ()
 **/
GList*
gck_list_ref_copy (GList *reflist)
{
	GList *l, *copy = g_list_copy (reflist);
	for (l = copy; l; l = g_list_next (l)) {
		g_return_val_if_fail (G_IS_OBJECT (l->data), NULL);
		g_object_ref (l->data);
	}
	return copy;
}

/**
 * gck_string_from_chars:
 * @data: The character data to turn into a null terminated string.
 * @max: The maximum length of the charater data.
 *
 * Create a string from a set of PKCS\#11 characters. This is
 * similar to g_strndup, except for that it also strips trailing
 * spaces. These space padded strings are often used in PKCS\#11
 * structures.
 *
 * If the space padded string is filled with null characters then
 * this function will return %NULL.
 *
 * Return value: The null terminated string.
 */
gchar*
gck_string_from_chars (const guchar *data, gsize max)
{
	gchar *string;

	g_return_val_if_fail (data, NULL);
	g_return_val_if_fail (max, NULL);

	/* Means no value */
	if (!data[0])
		return NULL;

	string = g_strndup ((gchar*)data, max);
	g_strchomp (string);
	return string;
}

/**
 * gck_string_to_chars:
 * @data: The character buffer to place string into.
 * @max: The maximum length of the charater buffer.
 * @string: The string to place in the buffer.
 *
 * Create a space padded PKCS\#11 string from a null terminated string.
 * The string must be shorter than the buffer or %FALSE will be
 * returned.
 *
 * If a %NULL string is passed, then the space padded string will be
 * set to zero characters.
 *
 * Return value: The null terminated string.
 */
gboolean
gck_string_to_chars (guchar *data, gsize max, const gchar *string)
{
	gsize len;

	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (max, FALSE);

	if (!string) {
		memset (data, 0, max);
		return TRUE;
	}

	len = strlen (string);
	if (len > max)
		return FALSE;

	memset (data, ' ', max);
	memcpy (data, string, len);
	return TRUE;
}

guint
_gck_ulong_hash (gconstpointer v)
{
	const signed char *p = v;
	guint32 i, h = *p;

	for(i = 0; i < sizeof (gulong); ++i)
		h = (h << 5) - h + *(p++);

	return h;
}

gboolean
_gck_ulong_equal (gconstpointer v1, gconstpointer v2)
{
	return *((const gulong*)v1) == *((const gulong*)v2);
}

/**
 * gck_value_to_ulong:
 * @value: memory to convert
 * @length: length of memory
 * @result: A location to store the result
 *
 * Convert CK_ULONG type memory to a boolean.
 *
 * Returns: Whether the conversion was successful.
 */
gboolean
gck_value_to_ulong (gconstpointer value, gsize length, gulong *result)
{
	if (!value || length != sizeof (CK_ULONG))
		return FALSE;
	if (result)
		*result = *((CK_ULONG*)value);
	return TRUE;
}

/**
 * gck_value_to_boolean:
 * @value: memory to convert
 * @length: length of memory
 * @result: A location to store the result
 *
 * Convert CK_BBOOL type memory to a boolean.
 *
 * Returns: Whether the conversion was successful.
 */
gboolean
gck_value_to_boolean (gconstpointer value, gsize length, gboolean *result)
{
	if (!value || length != sizeof (CK_BBOOL))
		return FALSE;
	if (result)
		*result = *((CK_BBOOL*)value) ? TRUE : FALSE;
	return TRUE;
}

static gboolean
match_info_string (const gchar *match, const gchar *string)
{
	/* NULL matches anything */
	if (match == NULL)
		return TRUE;

	if (string == NULL)
		return FALSE;

	return g_str_equal (match, string);
}

gboolean
_gck_module_info_match (GckModuleInfo *match, GckModuleInfo *info)
{
	/* Matches two GckModuleInfo for use in PKCS#11 URI's */

	g_return_val_if_fail (match, FALSE);
	g_return_val_if_fail (info, FALSE);

	return (match_info_string (match->library_description, info->library_description) &&
	        match_info_string (match->manufacturer_id, info->manufacturer_id));
}

gboolean
_gck_token_info_match (GckTokenInfo *match, GckTokenInfo *info)
{
	/* Matches two GckTokenInfo for use in PKCS#11 URI's */

	g_return_val_if_fail (match, FALSE);
	g_return_val_if_fail (info, FALSE);

	return (match_info_string (match->label, info->label) &&
	        match_info_string (match->manufacturer_id, info->manufacturer_id) &&
	        match_info_string (match->model, info->model) &&
	        match_info_string (match->serial_number, info->serial_number));
}
