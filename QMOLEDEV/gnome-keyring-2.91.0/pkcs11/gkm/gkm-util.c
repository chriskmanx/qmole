/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gkm-util.h"

#include <glib.h>
#include <glib-object.h>

#include <stdio.h>
#include <string.h>

/* Only access using atomic operations */
static gint next_handle = 0x00000010;

gulong*
gkm_util_ulong_alloc (gulong value)
{
	return g_slice_dup (gulong, &value);
}

void
gkm_util_ulong_free (gpointer ptr_to_ulong)
{
	g_slice_free (gulong, ptr_to_ulong);
}

guint
gkm_util_ulong_hash (gconstpointer v)
{
	const signed char *p = v;
	guint32 i, h = *p;
	for(i = 0; i < sizeof (gulong); ++i)
		h = (h << 5) - h + *(p++);
	return h;
}

gboolean
gkm_util_ulong_equal (gconstpointer v1, gconstpointer v2)
{
	return *((const gulong*)v1) == *((const gulong*)v2);
}

CK_RV
gkm_util_return_data (CK_VOID_PTR output, CK_ULONG_PTR n_output,
                      gconstpointer input, gsize n_input)
{
	g_return_val_if_fail (n_output, CKR_GENERAL_ERROR);
	g_return_val_if_fail (input || !n_input, CKR_GENERAL_ERROR);

	/* Just asking for the length */
	if (!output) {
		*n_output = n_input;
		return CKR_OK;
	}

	/* Buffer is too short */
	if (n_input > *n_output) {
		*n_output = n_input;
		return CKR_BUFFER_TOO_SMALL;
	}

	*n_output = n_input;
	if (n_input)
		memcpy (output, input, n_input);
	return CKR_OK;
}

CK_ULONG
gkm_util_next_handle (void)
{
	return (CK_ULONG)g_atomic_int_exchange_and_add (&next_handle, 1);
}

void
gkm_util_dispose_unref (gpointer object)
{
	g_return_if_fail (G_IS_OBJECT (object));
	g_object_run_dispose (G_OBJECT (object));
	g_object_unref (object);
}

static const gchar*
defined_rv_to_string (CK_RV rv)
{
	#define GKM_X(rv) case rv: return #rv;
	switch (rv) {

	/* These are not really errors, or not current */
	GKM_X (CKR_OK)
	GKM_X (CKR_NO_EVENT)
	GKM_X (CKR_FUNCTION_NOT_PARALLEL)
	GKM_X (CKR_SESSION_PARALLEL_NOT_SUPPORTED)
	GKM_X (CKR_CANCEL)
	GKM_X (CKR_FUNCTION_CANCELED)
	GKM_X (CKR_HOST_MEMORY)
	GKM_X (CKR_SLOT_ID_INVALID)
	GKM_X (CKR_GENERAL_ERROR)
	GKM_X (CKR_FUNCTION_FAILED)
	GKM_X (CKR_ARGUMENTS_BAD)
	GKM_X (CKR_NEED_TO_CREATE_THREADS)
	GKM_X (CKR_CANT_LOCK)
	GKM_X (CKR_ATTRIBUTE_READ_ONLY)
	GKM_X (CKR_ATTRIBUTE_SENSITIVE)
	GKM_X (CKR_ATTRIBUTE_TYPE_INVALID)
	GKM_X (CKR_ATTRIBUTE_VALUE_INVALID)
	GKM_X (CKR_DATA_INVALID)
	GKM_X (CKR_DATA_LEN_RANGE)
	GKM_X (CKR_DEVICE_ERROR)
	GKM_X (CKR_DEVICE_MEMORY)
	GKM_X (CKR_DEVICE_REMOVED)
	GKM_X (CKR_ENCRYPTED_DATA_INVALID)
	GKM_X (CKR_ENCRYPTED_DATA_LEN_RANGE)
	GKM_X (CKR_FUNCTION_NOT_SUPPORTED)
	GKM_X (CKR_KEY_HANDLE_INVALID)
	GKM_X (CKR_KEY_SIZE_RANGE)
	GKM_X (CKR_KEY_TYPE_INCONSISTENT)
	GKM_X (CKR_KEY_NOT_NEEDED)
	GKM_X (CKR_KEY_CHANGED)
	GKM_X (CKR_KEY_NEEDED)
	GKM_X (CKR_KEY_INDIGESTIBLE)
	GKM_X (CKR_KEY_FUNCTION_NOT_PERMITTED)
	GKM_X (CKR_KEY_NOT_WRAPPABLE)
	GKM_X (CKR_KEY_UNEXTRACTABLE)
	GKM_X (CKR_MECHANISM_INVALID)
	GKM_X (CKR_MECHANISM_PARAM_INVALID)
	GKM_X (CKR_OBJECT_HANDLE_INVALID)
	GKM_X (CKR_OPERATION_ACTIVE)
	GKM_X (CKR_OPERATION_NOT_INITIALIZED)
	GKM_X (CKR_PIN_INCORRECT)
	GKM_X (CKR_PIN_INVALID)
	GKM_X (CKR_PIN_LEN_RANGE)
	GKM_X (CKR_PIN_EXPIRED)
	GKM_X (CKR_PIN_LOCKED)
	GKM_X (CKR_SESSION_CLOSED)
	GKM_X (CKR_SESSION_COUNT)
	GKM_X (CKR_SESSION_HANDLE_INVALID)
	GKM_X (CKR_SESSION_READ_ONLY)
	GKM_X (CKR_SESSION_EXISTS)
	GKM_X (CKR_SESSION_READ_ONLY_EXISTS)
	GKM_X (CKR_SESSION_READ_WRITE_SO_EXISTS)
	GKM_X (CKR_SIGNATURE_INVALID)
	GKM_X (CKR_SIGNATURE_LEN_RANGE)
	GKM_X (CKR_TEMPLATE_INCOMPLETE)
	GKM_X (CKR_TEMPLATE_INCONSISTENT)
	GKM_X (CKR_TOKEN_NOT_PRESENT)
	GKM_X (CKR_TOKEN_NOT_RECOGNIZED)
	GKM_X (CKR_TOKEN_WRITE_PROTECTED)
	GKM_X (CKR_UNWRAPPING_KEY_HANDLE_INVALID)
	GKM_X (CKR_UNWRAPPING_KEY_SIZE_RANGE)
	GKM_X (CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT)
	GKM_X (CKR_USER_ALREADY_LOGGED_IN)
	GKM_X (CKR_USER_NOT_LOGGED_IN)
	GKM_X (CKR_USER_PIN_NOT_INITIALIZED)
	GKM_X (CKR_USER_TYPE_INVALID)
	GKM_X (CKR_USER_ANOTHER_ALREADY_LOGGED_IN)
	GKM_X (CKR_USER_TOO_MANY_TYPES)
	GKM_X (CKR_WRAPPED_KEY_INVALID)
	GKM_X (CKR_WRAPPED_KEY_LEN_RANGE)
	GKM_X (CKR_WRAPPING_KEY_HANDLE_INVALID)
	GKM_X (CKR_WRAPPING_KEY_SIZE_RANGE)
	GKM_X (CKR_WRAPPING_KEY_TYPE_INCONSISTENT)
	GKM_X (CKR_RANDOM_SEED_NOT_SUPPORTED)
	GKM_X (CKR_RANDOM_NO_RNG)
	GKM_X (CKR_DOMAIN_PARAMS_INVALID)
	GKM_X (CKR_BUFFER_TOO_SMALL)
	GKM_X (CKR_SAVED_STATE_INVALID)
	GKM_X (CKR_INFORMATION_SENSITIVE)
	GKM_X (CKR_STATE_UNSAVEABLE)
	GKM_X (CKR_CRYPTOKI_NOT_INITIALIZED)
	GKM_X (CKR_CRYPTOKI_ALREADY_INITIALIZED)
	GKM_X (CKR_MUTEX_BAD)
	GKM_X (CKR_MUTEX_NOT_LOCKED)
	GKM_X (CKR_FUNCTION_REJECTED)
	default:
		g_message ("unknown error: %lu", (gulong)rv);
		return "CKR_?UNKNOWN?";
	}

	#undef GKM_X
}

gchar*
gkm_util_rv_to_string (CK_RV rv)
{
	gchar *string = g_strdup (defined_rv_to_string (rv));
	if (string == NULL)
		string = g_strdup_printf ("0x%08lx", (gulong)rv);
	return string;
}

const gchar*
gkm_util_rv_stringize (CK_RV rv)
{
	const gchar *string = defined_rv_to_string (rv);
	if (string == NULL) {
		g_message ("unknown error: %lu", (gulong)rv);
		string = "CKR_?UNKNOWN?";
	}
	return string;
}
