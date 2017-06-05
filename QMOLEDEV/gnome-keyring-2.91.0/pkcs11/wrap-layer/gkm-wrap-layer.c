/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#include "gkm-wrap-layer.h"
#include "gkm-wrap-prompt.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11g.h"
#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <string.h>

typedef struct _Mapping {
	CK_SLOT_ID wrap_slot;
	CK_SLOT_ID real_slot;
	CK_FUNCTION_LIST_PTR funcs;
} Mapping;

typedef struct _Session {
	gint wrap_session;
	CK_SESSION_HANDLE real_session;
	CK_G_APPLICATION_ID app_id;
	CK_SLOT_ID wrap_slot;
	CK_OBJECT_HANDLE specific;
} Session;

G_LOCK_DEFINE_STATIC (wrap_layer);

static GList *wrap_modules = NULL;
static Mapping *wrap_mappings = NULL;
static guint n_wrap_mappings = 0;
static GHashTable *wrap_sessions = NULL;
static gint last_handle = 16;

#define MANUFACTURER_ID         "GNOME Keyring                   "
#define LIBRARY_DESCRIPTION     "GNOME Keyring Daemon Core       "
#define LIBRARY_VERSION_MAJOR   1
#define LIBRARY_VERSION_MINOR   1

/* Start wrap slots slightly higher for testing */
#define PLEX_MAPPING_OFFSET 0x10

static CK_RV
map_slot_unlocked (CK_SLOT_ID slot, Mapping *mapping)
{
	if (!wrap_mappings)
		return CKR_CRYPTOKI_NOT_INITIALIZED;

	if (slot < PLEX_MAPPING_OFFSET)
		return CKR_SLOT_ID_INVALID;
	slot -= PLEX_MAPPING_OFFSET;

	g_assert (mapping);

	if (slot > n_wrap_mappings) {
		return CKR_SLOT_ID_INVALID;
	} else {
		memcpy (mapping, &wrap_mappings[slot], sizeof (Mapping));
		return CKR_OK;
	}
}

static CK_RV
map_slot_to_real (CK_SLOT_ID_PTR slot, Mapping *mapping)
{
	CK_RV rv;

	g_assert (mapping);

	G_LOCK (wrap_layer);

		rv = map_slot_unlocked (*slot, mapping);
		if (rv == CKR_OK)
			*slot = mapping->real_slot;

	G_UNLOCK (wrap_layer);

	return rv;
}

static CK_RV
map_session_to_real (CK_SESSION_HANDLE_PTR handle, Mapping *mapping, Session *session)
{
	CK_RV rv = CKR_OK;
	Session *sess;

	g_assert (handle);
	g_assert (mapping);

	G_LOCK (wrap_layer);

		if (!wrap_sessions) {
			rv = CKR_CRYPTOKI_NOT_INITIALIZED;
		} else {
			sess = g_hash_table_lookup (wrap_sessions, GINT_TO_POINTER ((gint)*handle));
			if (sess != NULL) {
				*handle = sess->real_session;
				rv = map_slot_unlocked (sess->wrap_slot, mapping);
				if (session != NULL)
					memcpy (session, sess, sizeof (Session));
			} else {
				rv = CKR_SESSION_HANDLE_INVALID;
			}
		}

	G_UNLOCK (wrap_layer);

	return rv;
}

static void
lookup_session_specific (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE_PTR key)
{
	Session *sess;

	g_assert (key);
	*key = 0;

	G_LOCK (wrap_layer);

		if (wrap_sessions) {
			sess = g_hash_table_lookup (wrap_sessions, GINT_TO_POINTER ((gint)handle));
			if (sess == NULL)
				g_warning ("sessions out of sync with lower layer");
			else
				*key = sess->specific;
		}

	G_UNLOCK (wrap_layer);
}

static void
store_session_specific (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE key)
{
	Session *sess;

	G_LOCK (wrap_layer);

		if (wrap_sessions) {
			sess = g_hash_table_lookup (wrap_sessions, GINT_TO_POINTER ((gint)handle));
			if (sess == NULL)
				g_warning ("sessions out of sync with lower layer");
			else
				sess->specific = key;
		}

	G_UNLOCK (wrap_layer);
}

static CK_RV
wrap_C_Initialize (CK_VOID_PTR init_args)
{
	CK_FUNCTION_LIST_PTR funcs;
	GArray *mappings = NULL;
	CK_SLOT_ID_PTR slots;
	Mapping mapping;
	CK_ULONG i, count;
	CK_RV rv = CKR_OK;
	GList *l;

	mappings = g_array_new (FALSE, TRUE, sizeof (Mapping));

	G_LOCK (wrap_layer);

		if (wrap_mappings)
			rv = CKR_CRYPTOKI_ALREADY_INITIALIZED;

		for (l = wrap_modules; rv == CKR_OK && l != NULL; l = g_list_next (l)) {
			funcs = l->data;

			/* Initialize each module */
			rv = (funcs->C_Initialize) (init_args);
			if (rv == CKR_CRYPTOKI_ALREADY_INITIALIZED)
				rv = CKR_OK;
			if (rv != CKR_OK)
				break;

			/* And then ask it for its slots */
			rv = (funcs->C_GetSlotList) (FALSE, NULL, &count);
			if (rv != CKR_OK)
				break;
			if (!count)
				continue;
			slots = g_new0 (CK_SLOT_ID, count);
			rv = (funcs->C_GetSlotList) (FALSE, slots, &count);
			if (rv != CKR_OK) {
				 g_free (slots);
				 break;
			}

			/* And now add a mapping for each of those slots */
			for (i = 0; i < count; ++i) {
				memset (&mapping, 0, sizeof (mapping));
				mapping.wrap_slot = mappings->len + PLEX_MAPPING_OFFSET;
				mapping.real_slot = slots[i];
				mapping.funcs = funcs;
				g_array_append_val (mappings, mapping);
			}

			g_free (slots);
		}

		/* If failed, then finalize all the ones that succeeded */
		if (rv != CKR_OK && l != NULL) {
			for (l = g_list_previous (l); l; l = g_list_previous (l)) {
				funcs = l->data;
				(funcs->C_Finalize) (NULL);
			}
		}

		/* If succeeded then swap in mappings */
		if (rv == CKR_OK) {
			g_assert (!wrap_mappings);
			n_wrap_mappings = mappings->len;
			wrap_mappings = (Mapping*)g_array_free (mappings, FALSE);
			mappings = NULL;
			wrap_sessions = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, g_free);
		}

	G_UNLOCK (wrap_layer);

	/* If failed or somehow unused then free */
	if (mappings)
		g_array_free (mappings, TRUE);

	return rv;
}

static CK_RV
wrap_C_Finalize (CK_VOID_PTR reserved)
{
	CK_FUNCTION_LIST_PTR funcs;
	GList *l;

	G_LOCK (wrap_layer);

		for (l = wrap_modules; l != NULL; l = g_list_next (l)) {
			funcs = l->data;
			(funcs->C_Finalize) (NULL);
		}
		g_free (wrap_mappings);
		wrap_mappings = NULL;

		g_hash_table_destroy (wrap_sessions);
		wrap_sessions = NULL;

	G_UNLOCK (wrap_layer);

	return CKR_OK;
}

static CK_RV
wrap_C_GetInfo (CK_INFO_PTR info)
{
	if (info == NULL)
		return CKR_ARGUMENTS_BAD;

	info->cryptokiVersion.major = CRYPTOKI_VERSION_MAJOR;
	info->cryptokiVersion.minor = CRYPTOKI_VERSION_MINOR;
	info->libraryVersion.major = LIBRARY_VERSION_MAJOR;
	info->libraryVersion.minor = LIBRARY_VERSION_MINOR;
	info->flags = 0;
	strncpy ((char*)info->manufacturerID, MANUFACTURER_ID, 32);
	strncpy ((char*)info->libraryDescription, LIBRARY_DESCRIPTION, 32);
	return CKR_OK;
}

static CK_RV
wrap_C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	if (!list)
		return CKR_ARGUMENTS_BAD;
	*list = gkm_wrap_layer_get_functions_no_prompts ();
	return CKR_OK;
}

static CK_RV
auth_C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	if (!list)
		return CKR_ARGUMENTS_BAD;
	*list = gkm_wrap_layer_get_functions ();
	return CKR_OK;
}

static CK_RV
wrap_C_GetSlotList (CK_BBOOL token_present, CK_SLOT_ID_PTR slot_list, CK_ULONG_PTR count)
{
	CK_SLOT_INFO info;
	Mapping *mapping;
	CK_ULONG index;
	CK_RV rv;

	guint i;

	if (!count)
		return CKR_ARGUMENTS_BAD;

	G_LOCK (wrap_layer);

		rv = CKR_OK;
		index = 0;

		/* Go through and build up a map */
		for (i = 0; i < n_wrap_mappings; ++i) {
			mapping = &wrap_mappings[i];

			/* Skip ones without a token if requested */
			if (token_present) {
				rv = (mapping->funcs->C_GetSlotInfo) (mapping->real_slot, &info);
				if (rv != CKR_OK)
					break;
				if (!(info.flags & CKF_TOKEN_PRESENT))
					continue;
			}

			/* Fill in the slot if we can */
			if (slot_list && *count > index)
				slot_list[index] = mapping->wrap_slot;

			++index;
		}

		if (slot_list && *count < index)
			rv = CKR_BUFFER_TOO_SMALL;

		*count = index;

	G_UNLOCK (wrap_layer);

	return rv;
}

static CK_RV
wrap_C_GetSlotInfo (CK_SLOT_ID id, CK_SLOT_INFO_PTR info)
{
	Mapping map;
	CK_RV rv;

	rv = map_slot_to_real (&id, &map);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetSlotInfo) (id, info);
}

static CK_RV
wrap_C_GetTokenInfo (CK_SLOT_ID id, CK_TOKEN_INFO_PTR info)
{
	Mapping map;
	CK_RV rv;

	rv = map_slot_to_real (&id, &map);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetTokenInfo) (id, info);
}

static CK_RV
auth_C_GetTokenInfo (CK_SLOT_ID id, CK_TOKEN_INFO_PTR info)
{
	CK_RV rv = wrap_C_GetTokenInfo (id, info);
	if (rv == CKR_OK)
		info->flags |= CKF_PROTECTED_AUTHENTICATION_PATH;
	return rv;
}

static CK_RV
wrap_C_GetMechanismList (CK_SLOT_ID id, CK_MECHANISM_TYPE_PTR mechanism_list, CK_ULONG_PTR count)
{
	Mapping map;
	CK_RV rv;

	rv = map_slot_to_real (&id, &map);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetMechanismList) (id, mechanism_list, count);
}

static CK_RV
wrap_C_GetMechanismInfo (CK_SLOT_ID id, CK_MECHANISM_TYPE type, CK_MECHANISM_INFO_PTR info)
{
	Mapping map;
	CK_RV rv;

	rv = map_slot_to_real (&id, &map);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetMechanismInfo) (id, type, info);
}

static CK_RV
wrap_C_InitToken (CK_SLOT_ID id, CK_UTF8CHAR_PTR pin, CK_ULONG pin_len, CK_UTF8CHAR_PTR label)
{
	Mapping map;
	CK_RV rv;

	rv = map_slot_to_real (&id, &map);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_InitToken) (id, pin, pin_len, label);
}

static CK_RV
wrap_C_WaitForSlotEvent (CK_FLAGS flags, CK_SLOT_ID_PTR slot, CK_VOID_PTR reserved)
{
	/* TODO: We could implement this by polling, esp. the nonblock case. */
	return CKR_NO_EVENT;
}

static CK_RV
wrap_C_OpenSession (CK_SLOT_ID id, CK_FLAGS flags, CK_VOID_PTR user_data, CK_NOTIFY callback, CK_SESSION_HANDLE_PTR handle)
{
	Session *sess;
	Mapping map;
	CK_RV rv;

	if (handle == NULL)
		return CKR_ARGUMENTS_BAD;

	rv = map_slot_to_real (&id, &map);
	if (rv != CKR_OK)
		return rv;

	rv = (map.funcs->C_OpenSession) (id, flags, user_data, callback, handle);

	if (rv == CKR_OK) {
		G_LOCK (wrap_layer);

			sess = g_new (Session, 1);
			if (flags & CKF_G_APPLICATION_SESSION)
				sess->app_id = ((CK_G_APPLICATION_PTR)user_data)->applicationId;
			sess->wrap_slot = map.wrap_slot;
			sess->real_session = *handle;
			sess->wrap_session = ++last_handle; /* TODO: Handle wrapping, and then collisions */
			g_hash_table_replace (wrap_sessions, GINT_TO_POINTER (sess->wrap_session), sess);

			*handle = (CK_ULONG)sess->wrap_session;

		G_UNLOCK (wrap_layer);
	}

	return rv;
}

static CK_RV
wrap_C_CloseSession (CK_SESSION_HANDLE handle)
{
	gint key = (gint)handle;
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	rv = (map.funcs->C_CloseSession) (handle);

	if (rv == CKR_OK) {
		G_LOCK (wrap_layer);

			g_hash_table_remove (wrap_sessions, GINT_TO_POINTER (key));

		G_UNLOCK (wrap_layer);
	}

	return rv;
}

static CK_RV
wrap_C_CloseAllSessions (CK_SLOT_ID id)
{
	GHashTableIter iter;
	CK_SESSION_HANDLE handle;
	gpointer key, value;
	Session *sess;
	GArray *to_close;
	gint i;

	to_close = g_array_new (FALSE, FALSE, sizeof (CK_SESSION_HANDLE));

	G_LOCK (wrap_layer);

		g_hash_table_iter_init (&iter, wrap_sessions);
		while (g_hash_table_iter_next (&iter, &key, &value)) {
			sess = value;
			if ((sess->app_id | sess->wrap_slot) == id) {
				handle = (CK_SESSION_HANDLE)sess->wrap_session;
				g_array_append_val (to_close, handle);
			}
		}

	G_UNLOCK (wrap_layer);

	for (i = 0; i < to_close->len; ++i)
		wrap_C_CloseSession (g_array_index (to_close, CK_SESSION_HANDLE, i));

	g_array_free (to_close, TRUE);
	return CKR_OK;
}

static CK_RV
wrap_C_GetFunctionStatus (CK_SESSION_HANDLE handle)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetFunctionStatus) (handle);
}

static CK_RV
wrap_C_CancelFunction (CK_SESSION_HANDLE handle)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_CancelFunction) (handle);
}

static CK_RV
wrap_C_GetSessionInfo (CK_SESSION_HANDLE handle, CK_SESSION_INFO_PTR info)
{
	Mapping map;
	CK_RV rv;

	if (info == NULL)
		return CKR_ARGUMENTS_BAD;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;

	rv = (map.funcs->C_GetSessionInfo) (handle, info);
	if (rv == CKR_OK)
		info->slotID = map.wrap_slot;

	return rv;
}

static CK_RV
wrap_C_InitPIN (CK_SESSION_HANDLE handle, CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;

	return (map.funcs->C_InitPIN) (handle, pin, pin_len);
}

static CK_RV
auth_C_InitPIN (CK_SESSION_HANDLE handle, CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	GkmWrapPrompt *prompt;
	CK_RV rv = CKR_OK;

	prompt = gkm_wrap_prompt_for_init_pin (gkm_wrap_layer_get_functions_no_prompts(),
	                                       handle, pin, pin_len);

	for (;;) {
		if (prompt && !gkm_wrap_prompt_do_init_pin (prompt, rv, &pin, &pin_len))
			break;

		rv = wrap_C_InitPIN (handle, pin, pin_len);

		if (!prompt || rv != CKR_PIN_INVALID || rv != CKR_PIN_LEN_RANGE)
			break;
	}

	if (prompt) {
		gkm_wrap_prompt_done_init_pin (prompt, rv);
		g_object_unref (prompt);
	}

	return rv;
}

static CK_RV
wrap_C_SetPIN (CK_SESSION_HANDLE handle, CK_UTF8CHAR_PTR old_pin, CK_ULONG old_pin_len,
               CK_UTF8CHAR_PTR new_pin, CK_ULONG new_pin_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;

	return (map.funcs->C_SetPIN) (handle, old_pin, old_pin_len, new_pin, new_pin_len);
}

static CK_RV
auth_C_SetPIN (CK_SESSION_HANDLE handle, CK_UTF8CHAR_PTR old_pin, CK_ULONG old_pin_len,
               CK_UTF8CHAR_PTR new_pin, CK_ULONG new_pin_len)
{
	GkmWrapPrompt *prompt;
	CK_RV rv = CKR_OK;

	prompt = gkm_wrap_prompt_for_set_pin (gkm_wrap_layer_get_functions_no_prompts(),
	                                      handle, old_pin, old_pin_len, new_pin, new_pin_len);

	for (;;) {
		if (prompt && !gkm_wrap_prompt_do_set_pin (prompt, rv, &old_pin, &old_pin_len,
		                                           &new_pin, &new_pin_len))
			break;

		rv = wrap_C_SetPIN (handle, old_pin, old_pin_len, new_pin, new_pin_len);

		if (!prompt || rv != CKR_PIN_INCORRECT ||
		    rv != CKR_PIN_INVALID || rv != CKR_PIN_LEN_RANGE)
			break;
	}

	if (prompt) {
		gkm_wrap_prompt_done_set_pin (prompt, rv);
		g_object_unref (prompt);
	}

	return rv;
}

static CK_RV
wrap_C_GetOperationState (CK_SESSION_HANDLE handle, CK_BYTE_PTR operation_state, CK_ULONG_PTR operation_state_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetOperationState) (handle, operation_state, operation_state_len);
}

static CK_RV
wrap_C_SetOperationState (CK_SESSION_HANDLE handle, CK_BYTE_PTR operation_state,
                          CK_ULONG operation_state_len, CK_OBJECT_HANDLE encryption_key,
                          CK_OBJECT_HANDLE authentication_key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SetOperationState) (handle, operation_state, operation_state_len, encryption_key, authentication_key);
}

static CK_RV
wrap_C_Login (CK_SESSION_HANDLE handle, CK_USER_TYPE user_type,
              CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;

	return (map.funcs->C_Login) (handle, user_type, pin, pin_len);
}

static CK_RV
auth_C_Login (CK_SESSION_HANDLE handle, CK_USER_TYPE user_type,
              CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	GkmWrapPrompt *prompt;
	CK_OBJECT_HANDLE specific;
	CK_RV rv;

	lookup_session_specific (handle, &specific);
	prompt = gkm_wrap_prompt_for_login (gkm_wrap_layer_get_functions_no_prompts(),
	                                    user_type, handle, specific, pin, pin_len);

	for (;;) {
		rv = wrap_C_Login (handle, user_type, pin, pin_len);

		if (!prompt || rv != CKR_PIN_INCORRECT)
			break;

		if (!gkm_wrap_prompt_do_login (prompt, user_type, rv, &pin, &pin_len))
			break;
	}

	if (prompt) {
		gkm_wrap_prompt_done_login (prompt, user_type, rv);
		g_object_unref (prompt);
	}

	return rv;
}

static CK_RV
wrap_C_Logout (CK_SESSION_HANDLE handle)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_Logout) (handle);
}

static CK_RV
wrap_C_CreateObject (CK_SESSION_HANDLE handle, CK_ATTRIBUTE_PTR template,
                     CK_ULONG count, CK_OBJECT_HANDLE_PTR new_object)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;

	return (map.funcs->C_CreateObject) (handle, template, count, new_object);
}

static CK_RV
auth_C_CreateObject (CK_SESSION_HANDLE handle, CK_ATTRIBUTE_PTR template,
                     CK_ULONG count, CK_OBJECT_HANDLE_PTR new_object)
{
	GkmWrapPrompt *prompt = NULL;
	CK_RV rv;

	for (;;) {
		rv = wrap_C_CreateObject (handle, template, count, new_object);

		if (rv != CKR_PIN_INCORRECT)
			break;

		if (!prompt) {
			prompt = gkm_wrap_prompt_for_credential (gkm_wrap_layer_get_functions_no_prompts(),
			                                         handle, template, count);
			if (prompt == NULL)
				break;
		}

		if (!gkm_wrap_prompt_do_credential (prompt, &template, &count))
			break;
	}


	if (prompt) {
		gkm_wrap_prompt_done_credential (prompt, rv);
		g_object_unref (prompt);
	}

	return rv;
}

static CK_RV
wrap_C_CopyObject (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                   CK_ATTRIBUTE_PTR template, CK_ULONG count,
                   CK_OBJECT_HANDLE_PTR new_object)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_CopyObject) (handle, object, template, count, new_object);
}

static CK_RV
wrap_C_DestroyObject (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DestroyObject) (handle, object);
}

static CK_RV
wrap_C_GetObjectSize (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                      CK_ULONG_PTR size)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetObjectSize) (handle, object, size);
}

static CK_RV
wrap_C_GetAttributeValue (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                          CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GetAttributeValue) (handle, object, template, count);
}

static CK_RV
wrap_C_SetAttributeValue (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                         CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SetAttributeValue) (handle, object, template, count);
}

static CK_RV
wrap_C_FindObjectsInit (CK_SESSION_HANDLE handle, CK_ATTRIBUTE_PTR template,
                        CK_ULONG count)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_FindObjectsInit) (handle, template, count);
}

static CK_RV
wrap_C_FindObjects (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE_PTR objects,
                    CK_ULONG max_count, CK_ULONG_PTR count)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_FindObjects) (handle, objects, max_count, count);
}

static CK_RV
wrap_C_FindObjectsFinal (CK_SESSION_HANDLE handle)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_FindObjectsFinal) (handle);
}

static CK_RV
wrap_C_EncryptInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                    CK_OBJECT_HANDLE key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_EncryptInit) (handle, mechanism, key);
}

static CK_RV
auth_C_EncryptInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                    CK_OBJECT_HANDLE key)
{
	CK_RV rv = wrap_C_EncryptInit (handle, mechanism, key);
	if (rv == CKR_OK)
		store_session_specific (handle, key);
	return rv;
}

static CK_RV
wrap_C_Encrypt (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
                CK_BYTE_PTR encrypted_data, CK_ULONG_PTR encrypted_data_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_Encrypt) (handle, data, data_len, encrypted_data, encrypted_data_len);
}

static CK_RV
wrap_C_EncryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part,
                      CK_ULONG part_len, CK_BYTE_PTR encrypted_part,
                      CK_ULONG_PTR encrypted_part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_EncryptUpdate) (handle, part, part_len, encrypted_part, encrypted_part_len);
}

static CK_RV
wrap_C_EncryptFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR last_part,
                     CK_ULONG_PTR last_part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_EncryptFinal) (handle, last_part, last_part_len);
}

static CK_RV
wrap_C_DecryptInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                    CK_OBJECT_HANDLE key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DecryptInit) (handle, mechanism, key);
}

static CK_RV
auth_C_DecryptInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                    CK_OBJECT_HANDLE key)
{
	CK_RV rv = wrap_C_DecryptInit (handle, mechanism, key);
	if (rv == CKR_OK)
		store_session_specific (handle, key);
	return rv;
}

static CK_RV
wrap_C_Decrypt (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_data,
                CK_ULONG enc_data_len, CK_BYTE_PTR data, CK_ULONG_PTR data_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_Decrypt) (handle, enc_data, enc_data_len, data, data_len);
}

static CK_RV
wrap_C_DecryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_part,
                     CK_ULONG enc_part_len, CK_BYTE_PTR part, CK_ULONG_PTR part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DecryptUpdate) (handle, enc_part, enc_part_len, part, part_len);
}

static CK_RV
wrap_C_DecryptFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR last_part,
                     CK_ULONG_PTR last_part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DecryptFinal) (handle, last_part, last_part_len);
}

static CK_RV
wrap_C_DigestInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DigestInit) (handle, mechanism);
}

static CK_RV
wrap_C_Digest (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
               CK_BYTE_PTR digest, CK_ULONG_PTR digest_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_Digest) (handle, data, data_len, digest, digest_len);
}

static CK_RV
wrap_C_DigestUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part, CK_ULONG part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DigestUpdate) (handle, part, part_len);
}

static CK_RV
wrap_C_DigestKey (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DigestKey) (handle, key);
}

static CK_RV
wrap_C_DigestFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR digest,
                    CK_ULONG_PTR digest_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DigestFinal) (handle, digest, digest_len);
}

static CK_RV
wrap_C_SignInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                 CK_OBJECT_HANDLE key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SignInit) (handle, mechanism, key);
}

static CK_RV
auth_C_SignInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                 CK_OBJECT_HANDLE key)
{
	CK_RV rv = wrap_C_SignInit (handle, mechanism, key);
	if (rv == CKR_OK)
		store_session_specific (handle, key);
	return rv;
}

static CK_RV
wrap_C_Sign (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
             CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_Sign) (handle, data, data_len, signature, signature_len);
}

static CK_RV
wrap_C_SignUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part, CK_ULONG part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SignUpdate) (handle, part, part_len);
}

static CK_RV
wrap_C_SignFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR signature,
                  CK_ULONG_PTR signature_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SignFinal) (handle, signature, signature_len);
}

static CK_RV
wrap_C_SignRecoverInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                        CK_OBJECT_HANDLE key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SignRecoverInit) (handle, mechanism, key);
}

static CK_RV
auth_C_SignRecoverInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                        CK_OBJECT_HANDLE key)
{
	CK_RV rv = wrap_C_SignRecoverInit (handle, mechanism, key);
	if (rv == CKR_OK)
		store_session_specific (handle, key);
	return rv;
}

static CK_RV
wrap_C_SignRecover (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
                    CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SignRecover) (handle, data, data_len, signature, signature_len);
}

static CK_RV
wrap_C_VerifyInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                   CK_OBJECT_HANDLE key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_VerifyInit) (handle, mechanism, key);
}

static CK_RV
auth_C_VerifyInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                   CK_OBJECT_HANDLE key)
{
	CK_RV rv = wrap_C_VerifyInit (handle, mechanism, key);
	if (rv == CKR_OK)
		store_session_specific (handle, key);
	return rv;
}

static CK_RV
wrap_C_Verify (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
               CK_BYTE_PTR signature, CK_ULONG signature_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_Verify) (handle, data, data_len, signature, signature_len);
}

static CK_RV
wrap_C_VerifyUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part, CK_ULONG part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_VerifyUpdate) (handle, part, part_len);
}

static CK_RV
wrap_C_VerifyFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR signature,
                    CK_ULONG signature_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_VerifyFinal) (handle, signature, signature_len);
}

static CK_RV
wrap_C_VerifyRecoverInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                          CK_OBJECT_HANDLE key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_VerifyRecoverInit) (handle, mechanism, key);
}

static CK_RV
auth_C_VerifyRecoverInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                          CK_OBJECT_HANDLE key)
{
	CK_RV rv = wrap_C_VerifyInit (handle, mechanism, key);
	if (rv == CKR_OK)
		store_session_specific (handle, key);
	return rv;
}

static CK_RV
wrap_C_VerifyRecover (CK_SESSION_HANDLE handle, CK_BYTE_PTR signature,
                     CK_ULONG signature_len, CK_BYTE_PTR data, CK_ULONG_PTR data_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_VerifyRecover) (handle, signature, signature_len, data, data_len);
}

static CK_RV
wrap_C_DigestEncryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part,
                            CK_ULONG part_len, CK_BYTE_PTR enc_part,
                            CK_ULONG_PTR enc_part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DigestEncryptUpdate) (handle, part, part_len, enc_part, enc_part_len);
}

static CK_RV
wrap_C_DecryptDigestUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_part,
                            CK_ULONG enc_part_len, CK_BYTE_PTR part,
                            CK_ULONG_PTR part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DecryptDigestUpdate) (handle, enc_part, enc_part_len, part, part_len);
}

static CK_RV
wrap_C_SignEncryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part,
                          CK_ULONG part_len, CK_BYTE_PTR enc_part,
                          CK_ULONG_PTR enc_part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SignEncryptUpdate) (handle, part, part_len, enc_part, enc_part_len);
}

static CK_RV
wrap_C_DecryptVerifyUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_part,
                            CK_ULONG enc_part_len, CK_BYTE_PTR part,
                            CK_ULONG_PTR part_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DecryptVerifyUpdate) (handle, enc_part, enc_part_len, part, part_len);
}

static CK_RV
wrap_C_GenerateKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                    CK_ATTRIBUTE_PTR template, CK_ULONG count,
                    CK_OBJECT_HANDLE_PTR key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GenerateKey) (handle, mechanism, template, count, key);
}

static CK_RV
wrap_C_GenerateKeyPair (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                        CK_ATTRIBUTE_PTR pub_template, CK_ULONG pub_count,
                        CK_ATTRIBUTE_PTR priv_template, CK_ULONG priv_count,
                        CK_OBJECT_HANDLE_PTR pub_key, CK_OBJECT_HANDLE_PTR priv_key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GenerateKeyPair) (handle, mechanism, pub_template, pub_count, priv_template, priv_count, pub_key, priv_key);
}

static CK_RV
wrap_C_WrapKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                CK_OBJECT_HANDLE wrapping_key, CK_OBJECT_HANDLE key,
                CK_BYTE_PTR wrapped_key, CK_ULONG_PTR wrapped_key_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_WrapKey) (handle, mechanism, wrapping_key, key, wrapped_key, wrapped_key_len);
}

static CK_RV
wrap_C_UnwrapKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                  CK_OBJECT_HANDLE unwrapping_key, CK_BYTE_PTR wrapped_key,
                  CK_ULONG wrapped_key_len, CK_ATTRIBUTE_PTR template,
                  CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_UnwrapKey) (handle, mechanism, unwrapping_key, wrapped_key, wrapped_key_len, template, count, key);
}

static CK_RV
wrap_C_DeriveKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                  CK_OBJECT_HANDLE base_key, CK_ATTRIBUTE_PTR template,
                  CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_DeriveKey) (handle, mechanism, base_key, template, count, key);
}

static CK_RV
wrap_C_SeedRandom (CK_SESSION_HANDLE handle, CK_BYTE_PTR seed, CK_ULONG seed_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_SeedRandom) (handle, seed, seed_len);
}

static CK_RV
wrap_C_GenerateRandom (CK_SESSION_HANDLE handle, CK_BYTE_PTR random_data,
                      CK_ULONG random_len)
{
	Mapping map;
	CK_RV rv;

	rv = map_session_to_real (&handle, &map, NULL);
	if (rv != CKR_OK)
		return rv;
	return (map.funcs->C_GenerateRandom) (handle, random_data, random_len);
}

/* --------------------------------------------------------------------
 * MODULE ENTRY POINT
 */

static CK_FUNCTION_LIST wrap_function_list = {
	{ CRYPTOKI_VERSION_MAJOR, CRYPTOKI_VERSION_MINOR },  /* version */
	wrap_C_Initialize,
	wrap_C_Finalize,
	wrap_C_GetInfo,
	wrap_C_GetFunctionList,
	wrap_C_GetSlotList,
	wrap_C_GetSlotInfo,
	wrap_C_GetTokenInfo,
	wrap_C_GetMechanismList,
	wrap_C_GetMechanismInfo,
	wrap_C_InitToken,
	wrap_C_InitPIN,
	wrap_C_SetPIN,
	wrap_C_OpenSession,
	wrap_C_CloseSession,
	wrap_C_CloseAllSessions,
	wrap_C_GetSessionInfo,
	wrap_C_GetOperationState,
	wrap_C_SetOperationState,
	wrap_C_Login,
	wrap_C_Logout,
	wrap_C_CreateObject,
	wrap_C_CopyObject,
	wrap_C_DestroyObject,
	wrap_C_GetObjectSize,
	wrap_C_GetAttributeValue,
	wrap_C_SetAttributeValue,
	wrap_C_FindObjectsInit,
	wrap_C_FindObjects,
	wrap_C_FindObjectsFinal,
	wrap_C_EncryptInit,
	wrap_C_Encrypt,
	wrap_C_EncryptUpdate,
	wrap_C_EncryptFinal,
	wrap_C_DecryptInit,
	wrap_C_Decrypt,
	wrap_C_DecryptUpdate,
	wrap_C_DecryptFinal,
	wrap_C_DigestInit,
	wrap_C_Digest,
	wrap_C_DigestUpdate,
	wrap_C_DigestKey,
	wrap_C_DigestFinal,
	wrap_C_SignInit,
	wrap_C_Sign,
	wrap_C_SignUpdate,
	wrap_C_SignFinal,
	wrap_C_SignRecoverInit,
	wrap_C_SignRecover,
	wrap_C_VerifyInit,
	wrap_C_Verify,
	wrap_C_VerifyUpdate,
	wrap_C_VerifyFinal,
	wrap_C_VerifyRecoverInit,
	wrap_C_VerifyRecover,
	wrap_C_DigestEncryptUpdate,
	wrap_C_DecryptDigestUpdate,
	wrap_C_SignEncryptUpdate,
	wrap_C_DecryptVerifyUpdate,
	wrap_C_GenerateKey,
	wrap_C_GenerateKeyPair,
	wrap_C_WrapKey,
	wrap_C_UnwrapKey,
	wrap_C_DeriveKey,
	wrap_C_SeedRandom,
	wrap_C_GenerateRandom,
	wrap_C_GetFunctionStatus,
	wrap_C_CancelFunction,
	wrap_C_WaitForSlotEvent
};

static CK_FUNCTION_LIST auth_function_list = {
	{ CRYPTOKI_VERSION_MAJOR, CRYPTOKI_VERSION_MINOR },  /* version */
	wrap_C_Initialize,
	wrap_C_Finalize,
	wrap_C_GetInfo,
	auth_C_GetFunctionList,
	wrap_C_GetSlotList,
	wrap_C_GetSlotInfo,
	auth_C_GetTokenInfo,
	wrap_C_GetMechanismList,
	wrap_C_GetMechanismInfo,
	wrap_C_InitToken,
	auth_C_InitPIN,
	auth_C_SetPIN,
	wrap_C_OpenSession,
	wrap_C_CloseSession,
	wrap_C_CloseAllSessions,
	wrap_C_GetSessionInfo,
	wrap_C_GetOperationState,
	wrap_C_SetOperationState,
	auth_C_Login,
	wrap_C_Logout,
	auth_C_CreateObject,
	wrap_C_CopyObject,
	wrap_C_DestroyObject,
	wrap_C_GetObjectSize,
	wrap_C_GetAttributeValue,
	wrap_C_SetAttributeValue,
	wrap_C_FindObjectsInit,
	wrap_C_FindObjects,
	wrap_C_FindObjectsFinal,
	auth_C_EncryptInit,
	wrap_C_Encrypt,
	wrap_C_EncryptUpdate,
	wrap_C_EncryptFinal,
	auth_C_DecryptInit,
	wrap_C_Decrypt,
	wrap_C_DecryptUpdate,
	wrap_C_DecryptFinal,
	wrap_C_DigestInit,
	wrap_C_Digest,
	wrap_C_DigestUpdate,
	wrap_C_DigestKey,
	wrap_C_DigestFinal,
	auth_C_SignInit,
	wrap_C_Sign,
	wrap_C_SignUpdate,
	wrap_C_SignFinal,
	auth_C_SignRecoverInit,
	wrap_C_SignRecover,
	auth_C_VerifyInit,
	wrap_C_Verify,
	wrap_C_VerifyUpdate,
	wrap_C_VerifyFinal,
	auth_C_VerifyRecoverInit,
	wrap_C_VerifyRecover,
	wrap_C_DigestEncryptUpdate,
	wrap_C_DecryptDigestUpdate,
	wrap_C_SignEncryptUpdate,
	wrap_C_DecryptVerifyUpdate,
	wrap_C_GenerateKey,
	wrap_C_GenerateKeyPair,
	wrap_C_WrapKey,
	wrap_C_UnwrapKey,
	wrap_C_DeriveKey,
	wrap_C_SeedRandom,
	wrap_C_GenerateRandom,
	wrap_C_GetFunctionStatus,
	wrap_C_CancelFunction,
	wrap_C_WaitForSlotEvent
};

/* -----------------------------------------------------------------------------------------
 * PUBLIC FUNCTIONS
 */

CK_FUNCTION_LIST_PTR
gkm_wrap_layer_get_functions (void)
{
	return &auth_function_list;
}

CK_FUNCTION_LIST_PTR
gkm_wrap_layer_get_functions_no_prompts (void)
{
	return &wrap_function_list;
}

void
gkm_wrap_layer_reset_modules (void)
{
	G_LOCK (wrap_layer);

		g_assert (!wrap_mappings);
		g_assert (!wrap_sessions);
		g_list_free (wrap_modules);
		wrap_modules = NULL;

	G_UNLOCK (wrap_layer);
}

void
gkm_wrap_layer_add_module (CK_FUNCTION_LIST_PTR funcs)
{
	g_assert (funcs);

	G_LOCK (wrap_layer);

		g_assert (!wrap_mappings);
		g_assert (!wrap_sessions);
		wrap_modules = g_list_append (wrap_modules, funcs);

	G_UNLOCK (wrap_layer);
}
