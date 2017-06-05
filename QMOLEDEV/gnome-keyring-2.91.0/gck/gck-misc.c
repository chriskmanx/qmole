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
 * @rv: The PKCS#11 return value to get a message for.
 *
 * Get a message for a PKCS#11 return value or error code. Do not
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

	case CKR_CANCEL:
	case CKR_FUNCTION_CANCELED:
		return _("The operation was cancelled");

	case CKR_HOST_MEMORY:
		return _("Insufficient memory available");
	case CKR_SLOT_ID_INVALID:
		return _("The specified slot ID is not valid");
	case CKR_GENERAL_ERROR:
		return _("Internal error");
	case CKR_FUNCTION_FAILED:
		return _("The operation failed");
	case CKR_ARGUMENTS_BAD:
		return _("Invalid arguments");
	case CKR_NEED_TO_CREATE_THREADS:
		return _("The module cannot create needed threads");
	case CKR_CANT_LOCK:
		return _("The module cannot lock data properly");
	case CKR_ATTRIBUTE_READ_ONLY:
		return _("The field is read-only");
	case CKR_ATTRIBUTE_SENSITIVE:
		return _("The field is sensitive and cannot be revealed");
	case CKR_ATTRIBUTE_TYPE_INVALID:
		return _("The field is invalid or does not exist");
	case CKR_ATTRIBUTE_VALUE_INVALID:
		return _("Invalid value for field");
	case CKR_DATA_INVALID:
		return _("The data is not valid or unrecognized");
	case CKR_DATA_LEN_RANGE:
		return _("The data is too long");
	case CKR_DEVICE_ERROR:
		return _("An error occurred on the device");
	case CKR_DEVICE_MEMORY:
		return _("Insufficient memory available on the device");
	case CKR_DEVICE_REMOVED:
		return _("The device was removed or unplugged");
	case CKR_ENCRYPTED_DATA_INVALID:
		return _("The encrypted data is not valid or unrecognized");
	case CKR_ENCRYPTED_DATA_LEN_RANGE:
		return _("The encrypted data is too long");
	case CKR_FUNCTION_NOT_SUPPORTED:
		return _("This operation is not supported");
	case CKR_KEY_HANDLE_INVALID:
		return _("The key is missing or invalid");
	case CKR_KEY_SIZE_RANGE:
		return _("The key is the wrong size");
	case CKR_KEY_TYPE_INCONSISTENT:
		return _("The key is of the wrong type");
	case CKR_KEY_NOT_NEEDED:
		return _("No key is needed");
	case CKR_KEY_CHANGED:
		return _("The key is different than before");
	case CKR_KEY_NEEDED:
		return _("A key is needed");
	case CKR_KEY_INDIGESTIBLE:
		return _("Cannot include the key in the digest");
	case CKR_KEY_FUNCTION_NOT_PERMITTED:
		return _("This operation cannot be done with this key");
	case CKR_KEY_NOT_WRAPPABLE:
		return _("The key cannot be wrapped");
	case CKR_KEY_UNEXTRACTABLE:
		return _("Cannot export this key");
	case CKR_MECHANISM_INVALID:
		return _("The crypto mechanism is invalid or unrecognized");
	case CKR_MECHANISM_PARAM_INVALID:
		return _("The crypto mechanism has an invalid argument");
	case CKR_OBJECT_HANDLE_INVALID:
		return _("The object is missing or invalid");
	case CKR_OPERATION_ACTIVE:
		return _("Another operation is already taking place");
	case CKR_OPERATION_NOT_INITIALIZED:
		return _("No operation is taking place");
	case CKR_PIN_INCORRECT:
		return _("The password or PIN is incorrect");
	case CKR_PIN_INVALID:
		return _("The password or PIN is invalid");
	case CKR_PIN_LEN_RANGE:
		return _("The password or PIN is of an invalid length");
	case CKR_PIN_EXPIRED:
		return _("The password or PIN has expired");
	case CKR_PIN_LOCKED:
		return _("The password or PIN is locked");
	case CKR_SESSION_CLOSED:
		return _("The session is closed");
	case CKR_SESSION_COUNT:
		return _("Too many sessions are active");
	case CKR_SESSION_HANDLE_INVALID:
		return _("The session is invalid");
	case CKR_SESSION_READ_ONLY:
		return _("The session is read-only");
	case CKR_SESSION_EXISTS:
		return _("An open session exists");
	case CKR_SESSION_READ_ONLY_EXISTS:
		return _("A read-only session exists");
	case CKR_SESSION_READ_WRITE_SO_EXISTS:
		return _("An administrator session exists");
	case CKR_SIGNATURE_INVALID:
		return _("The signature is bad or corrupted");
	case CKR_SIGNATURE_LEN_RANGE:
		return _("The signature is unrecognized or corrupted");
	case CKR_TEMPLATE_INCOMPLETE:
		return _("Certain required fields are missing");
	case CKR_TEMPLATE_INCONSISTENT:
		return _("Certain fields have invalid values");
	case CKR_TOKEN_NOT_PRESENT:
		return _("The device is not present or unplugged");
	case CKR_TOKEN_NOT_RECOGNIZED:
		return _("The device is invalid or unrecognizable");
	case CKR_TOKEN_WRITE_PROTECTED:
		return _("The device is write protected");
	case CKR_UNWRAPPING_KEY_HANDLE_INVALID:
		return _("Cannot import because the key is invalid");
	case CKR_UNWRAPPING_KEY_SIZE_RANGE:
		return _("Cannot import because the key is of the wrong size");
	case CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT:
		return _("Cannot import because the key is of the wrong type");
	case CKR_USER_ALREADY_LOGGED_IN:
		return _("You are already logged in");
	case CKR_USER_NOT_LOGGED_IN:
		return _("No user has logged in");
	case CKR_USER_PIN_NOT_INITIALIZED:
		return _("The user's password or PIN is not set");
	case CKR_USER_TYPE_INVALID:
		return _("The user is of an invalid type");
	case CKR_USER_ANOTHER_ALREADY_LOGGED_IN:
		return _("Another user is already logged in");
	case CKR_USER_TOO_MANY_TYPES:
		return _("Too many users of different types are logged in");
	case CKR_WRAPPED_KEY_INVALID:
		return _("Cannot import an invalid key");
	case CKR_WRAPPED_KEY_LEN_RANGE:
		return _("Cannot import a key of the wrong size");
	case CKR_WRAPPING_KEY_HANDLE_INVALID:
		return _("Cannot export because the key is invalid");
	case CKR_WRAPPING_KEY_SIZE_RANGE:
		return _("Cannot export because the key is of the wrong size");
	case CKR_WRAPPING_KEY_TYPE_INCONSISTENT:
		return _("Cannot export because the key is of the wrong type");
	case CKR_RANDOM_SEED_NOT_SUPPORTED:
		return _("Unable to initialize the random number generator");
	case CKR_RANDOM_NO_RNG:
		return _("No random number generator available");
	case CKR_DOMAIN_PARAMS_INVALID:
		return _("The crypto mechanism has an invalid parameter");
	case CKR_BUFFER_TOO_SMALL:
		return _("Not enough space to store the result");
	case CKR_SAVED_STATE_INVALID:
		return _("The saved state is invalid");
	case CKR_INFORMATION_SENSITIVE:
		return _("The information is sensitive and cannot be revealed");
	case CKR_STATE_UNSAVEABLE:
		return _("The state cannot be saved");
	case CKR_CRYPTOKI_NOT_INITIALIZED:
		return _("The module has not been initialized");
	case CKR_CRYPTOKI_ALREADY_INITIALIZED:
		return _("The module has already been initialized");
	case CKR_MUTEX_BAD:
		return _("Cannot lock data");
	case CKR_MUTEX_NOT_LOCKED:
		return _("The data cannot be locked");
	case CKR_FUNCTION_REJECTED:
		return _("The signature request was rejected by the user");

	default:
		g_message ("unknown error: %lu", (gulong)rv);
		return _("Unknown error");
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
 * Create a string from a set of PKCS#11 characters. This is
 * similar to g_strndup, except for that it also strips trailing
 * spaces. These space padded strings are often used in PKCS#11
 * structures.
 *
 * Return value: The null terminated string.
 */
gchar*
gck_string_from_chars (const guchar *data, gsize max)
{
	gchar *string;

	g_return_val_if_fail (data, NULL);
	g_return_val_if_fail (max, NULL);

	string = g_strndup ((gchar*)data, max);
	g_strchomp (string);
	return string;
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

gboolean
gck_value_to_ulong (gconstpointer value, gsize length, gulong *result)
{
	if (!value || length != sizeof (CK_ULONG))
		return FALSE;
	if (result)
		*result = *((CK_ULONG*)value);
	return TRUE;
}

gboolean
gck_value_to_boolean (gconstpointer value, gsize length, gboolean *result)
{
	if (!value || length != sizeof (CK_BBOOL))
		return FALSE;
	if (result)
		*result = *((CK_BBOOL*)value) ? TRUE : FALSE;
	return TRUE;
}
