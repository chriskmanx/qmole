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

#include "gkm-crypto.h"
#include "gkm-module.h"
#include "gkm-session.h"

#include "pkcs11/pkcs11.h"

#include <unistd.h>

/* Forward declaration, this must be defined manually or using GKM_DEFINE_MODULE */
static GkmModule* gkm_module_instantiate (CK_C_INITIALIZE_ARGS_PTR args, GMutex *mutex);

static GkmModule *pkcs11_module = NULL;
static pid_t pkcs11_module_pid = 0;
static GStaticMutex pkcs11_module_mutex = G_STATIC_MUTEX_INIT;

static CK_FUNCTION_LIST gkm_module_function_list;

static CK_RV
gkm_C_Initialize (CK_VOID_PTR init_args)
{
	CK_C_INITIALIZE_ARGS_PTR args = (CK_C_INITIALIZE_ARGS_PTR)init_args;
	CK_RV rv = CKR_OK;
	pid_t pid = getpid ();
	gboolean supplied_ok;

	if (args) {

		/* ALL supplied function pointers need to have the value either NULL or non-NULL. */
		supplied_ok = (args->CreateMutex == NULL && args->DestroyMutex == NULL &&
		               args->LockMutex == NULL && args->UnlockMutex == NULL) ||
		              (args->CreateMutex != NULL && args->DestroyMutex != NULL &&
		               args->LockMutex != NULL && args->UnlockMutex != NULL);

		if (!supplied_ok) {
			g_message ("invalid set of mutex calls supplied");
			return CKR_ARGUMENTS_BAD;
		}

		if (!(args->flags & CKF_OS_LOCKING_OK)) {
			g_message ("must be able to use our own locking and multi-thread primitives");
			return CKR_CANT_LOCK;
		}
	}

	gkm_crypto_initialize ();

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			if (pkcs11_module_pid == pid)
				rv = CKR_CRYPTOKI_ALREADY_INITIALIZED;
			else
				pkcs11_module_pid = pid;
		} else {
			pkcs11_module = gkm_module_instantiate (args, g_static_mutex_get_mutex (&pkcs11_module_mutex));
			if (!pkcs11_module) {
				g_warning ("module could not be instantiated");
				rv = CKR_GENERAL_ERROR;
			} else {
				pkcs11_module_pid = pid;
			}
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Finalize (CK_VOID_PTR reserved)
{
	CK_RV rv = CKR_OK;

	if (reserved)
		return CKR_ARGUMENTS_BAD;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module == NULL) {
			rv = CKR_CRYPTOKI_NOT_INITIALIZED;
		} else {
			g_object_run_dispose (G_OBJECT (pkcs11_module));
			g_object_unref (pkcs11_module);
			pkcs11_module = NULL;
			pkcs11_module_pid = 0;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetInfo (CK_INFO_PTR info)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_GetInfo (pkcs11_module, info);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	if (!list)
		return CKR_ARGUMENTS_BAD;
	*list = &gkm_module_function_list;
	return CKR_OK;
}

static CK_RV
gkm_C_GetSlotList (CK_BBOOL token_present, CK_SLOT_ID_PTR slot_list, CK_ULONG_PTR count)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_GetSlotList (pkcs11_module, token_present, slot_list, count);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetSlotInfo (CK_SLOT_ID id, CK_SLOT_INFO_PTR info)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_GetSlotInfo (pkcs11_module, id, info);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetTokenInfo (CK_SLOT_ID id, CK_TOKEN_INFO_PTR info)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_GetTokenInfo (pkcs11_module, id, info);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetMechanismList (CK_SLOT_ID id, CK_MECHANISM_TYPE_PTR mechanism_list, CK_ULONG_PTR count)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_GetMechanismList (pkcs11_module, id, mechanism_list, count);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetMechanismInfo (CK_SLOT_ID id, CK_MECHANISM_TYPE type, CK_MECHANISM_INFO_PTR info)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_GetMechanismInfo (pkcs11_module, id, type, info);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_InitToken (CK_SLOT_ID id, CK_UTF8CHAR_PTR pin, CK_ULONG pin_len, CK_UTF8CHAR_PTR label)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_InitToken (pkcs11_module, id, pin, pin_len, label);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_WaitForSlotEvent (CK_FLAGS flags, CK_SLOT_ID_PTR slot, CK_VOID_PTR reserved)
{
	/*
	 * PKCS#11 GRAY AREA: What happens when we know we'll *never*
	 * have any slot events, and someone calls us without CKR_DONT_BLOCK?
	 * In case there's a thread dedicated to calling this function in a
	 * loop, we wait 5 seconds when called without CKR_DONT_BLOCK.
	 */

	if (!(flags & CKF_DONT_BLOCK))
		sleep (5);

	return CKR_NO_EVENT;
}

static CK_RV
gkm_C_OpenSession (CK_SLOT_ID id, CK_FLAGS flags, CK_VOID_PTR user_data, CK_NOTIFY callback, CK_SESSION_HANDLE_PTR handle)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_OpenSession (pkcs11_module, id, flags, user_data, callback, handle);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_CloseSession (CK_SESSION_HANDLE handle)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_CloseSession (pkcs11_module, handle);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_CloseAllSessions (CK_SLOT_ID id)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_CloseAllSessions (pkcs11_module, id);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetFunctionStatus (CK_SESSION_HANDLE handle)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GetFunctionStatus (session);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_CancelFunction (CK_SESSION_HANDLE handle)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_CancelFunction (session);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetSessionInfo (CK_SESSION_HANDLE handle, CK_SESSION_INFO_PTR info)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GetSessionInfo (session,
				                                   info);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_InitPIN (CK_SESSION_HANDLE handle, CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_InitPIN (pkcs11_module, handle, pin, pin_len);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SetPIN (CK_SESSION_HANDLE handle, CK_UTF8CHAR_PTR old_pin, CK_ULONG old_pin_len, CK_UTF8CHAR_PTR new_pin, CK_ULONG new_pin_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_SetPIN (pkcs11_module, handle, old_pin, old_pin_len, new_pin, new_pin_len);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetOperationState (CK_SESSION_HANDLE handle, CK_BYTE_PTR operation_state, CK_ULONG_PTR operation_state_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GetOperationState (session,
				                                      operation_state, operation_state_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SetOperationState (CK_SESSION_HANDLE handle, CK_BYTE_PTR operation_state,
                         CK_ULONG operation_state_len, CK_OBJECT_HANDLE encryption_key,
                         CK_OBJECT_HANDLE authentication_key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SetOperationState (session,
				                                      operation_state, operation_state_len, encryption_key, authentication_key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Login (CK_SESSION_HANDLE handle, CK_USER_TYPE user_type,
             CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_Login (pkcs11_module, handle, user_type, pin, pin_len);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Logout (CK_SESSION_HANDLE handle)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL)
			rv = gkm_module_C_Logout (pkcs11_module, handle);

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_CreateObject (CK_SESSION_HANDLE handle, CK_ATTRIBUTE_PTR template,
                    CK_ULONG count, CK_OBJECT_HANDLE_PTR new_object)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_CreateObject (session,
				                                 template, count, new_object);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_CopyObject (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                  CK_ATTRIBUTE_PTR template, CK_ULONG count,
                  CK_OBJECT_HANDLE_PTR new_object)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_CopyObject (session,
				                               object, template, count, new_object);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}


static CK_RV
gkm_C_DestroyObject (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DestroyObject (session, object);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetObjectSize (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                     CK_ULONG_PTR size)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GetObjectSize (session, object, size);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GetAttributeValue (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                         CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GetAttributeValue (session, object, template, count);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SetAttributeValue (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE object,
                         CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SetAttributeValue (session, object, template, count);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_FindObjectsInit (CK_SESSION_HANDLE handle, CK_ATTRIBUTE_PTR template,
                       CK_ULONG count)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_FindObjectsInit (session, template, count);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_FindObjects (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE_PTR objects,
                   CK_ULONG max_count, CK_ULONG_PTR count)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_FindObjects (session, objects, max_count, count);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_FindObjectsFinal (CK_SESSION_HANDLE handle)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_FindObjectsFinal (session);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_EncryptInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                   CK_OBJECT_HANDLE key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_EncryptInit (session, mechanism, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Encrypt (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
               CK_BYTE_PTR encrypted_data, CK_ULONG_PTR encrypted_data_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_Encrypt (session, data, data_len, encrypted_data, encrypted_data_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_EncryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part,
                     CK_ULONG part_len, CK_BYTE_PTR encrypted_part,
                     CK_ULONG_PTR encrypted_part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_EncryptUpdate (session, part, part_len, encrypted_part, encrypted_part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_EncryptFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR last_part,
                    CK_ULONG_PTR last_part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_EncryptFinal (session, last_part, last_part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DecryptInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                   CK_OBJECT_HANDLE key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DecryptInit (session, mechanism, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Decrypt (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_data,
               CK_ULONG enc_data_len, CK_BYTE_PTR data, CK_ULONG_PTR data_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_Decrypt (session, enc_data, enc_data_len, data, data_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DecryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_part,
                     CK_ULONG enc_part_len, CK_BYTE_PTR part, CK_ULONG_PTR part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DecryptUpdate (session, enc_part, enc_part_len, part, part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DecryptFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR last_part,
                    CK_ULONG_PTR last_part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DecryptFinal (session, last_part, last_part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DigestInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DigestInit (session, mechanism);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Digest (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
              CK_BYTE_PTR digest, CK_ULONG_PTR digest_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_Digest (session, data, data_len, digest, digest_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DigestUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part, CK_ULONG part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DigestUpdate (session, part, part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DigestKey (CK_SESSION_HANDLE handle, CK_OBJECT_HANDLE key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DigestKey (session, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DigestFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR digest,
                   CK_ULONG_PTR digest_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DigestFinal (session, digest, digest_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SignInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                CK_OBJECT_HANDLE key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SignInit (session, mechanism, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Sign (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
            CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_Sign (session, data, data_len, signature, signature_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SignUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part, CK_ULONG part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SignUpdate (session, part, part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SignFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR signature,
                 CK_ULONG_PTR signature_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SignFinal (session, signature, signature_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SignRecoverInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                       CK_OBJECT_HANDLE key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SignRecoverInit (session, mechanism, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SignRecover (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
                   CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SignRecover (session, data, data_len, signature, signature_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_VerifyInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                  CK_OBJECT_HANDLE key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_VerifyInit (session, mechanism, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_Verify (CK_SESSION_HANDLE handle, CK_BYTE_PTR data, CK_ULONG data_len,
              CK_BYTE_PTR signature, CK_ULONG signature_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_Verify (session, data, data_len, signature, signature_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_VerifyUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part, CK_ULONG part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_VerifyUpdate (session, part, part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_VerifyFinal (CK_SESSION_HANDLE handle, CK_BYTE_PTR signature,
                   CK_ULONG signature_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_VerifyFinal (session, signature, signature_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_VerifyRecoverInit (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                         CK_OBJECT_HANDLE key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_VerifyRecoverInit (session, mechanism, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_VerifyRecover (CK_SESSION_HANDLE handle, CK_BYTE_PTR signature,
                     CK_ULONG signature_len, CK_BYTE_PTR data, CK_ULONG_PTR data_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_VerifyRecover (session, signature, signature_len, data, data_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DigestEncryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part,
                           CK_ULONG part_len, CK_BYTE_PTR enc_part,
                           CK_ULONG_PTR enc_part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DigestEncryptUpdate (session, part, part_len, enc_part, enc_part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DecryptDigestUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_part,
                           CK_ULONG enc_part_len, CK_BYTE_PTR part,
                           CK_ULONG_PTR part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DecryptDigestUpdate (session, enc_part, enc_part_len, part, part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SignEncryptUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR part,
                         CK_ULONG part_len, CK_BYTE_PTR enc_part,
                         CK_ULONG_PTR enc_part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SignEncryptUpdate (session, part, part_len, enc_part, enc_part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DecryptVerifyUpdate (CK_SESSION_HANDLE handle, CK_BYTE_PTR enc_part,
                           CK_ULONG enc_part_len, CK_BYTE_PTR part,
                           CK_ULONG_PTR part_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DecryptVerifyUpdate (session, enc_part, enc_part_len, part, part_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GenerateKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                   CK_ATTRIBUTE_PTR template, CK_ULONG count,
                   CK_OBJECT_HANDLE_PTR key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GenerateKey (session, mechanism, template, count, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GenerateKeyPair (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                       CK_ATTRIBUTE_PTR pub_template, CK_ULONG pub_count,
                       CK_ATTRIBUTE_PTR priv_template, CK_ULONG priv_count,
                       CK_OBJECT_HANDLE_PTR pub_key, CK_OBJECT_HANDLE_PTR priv_key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GenerateKeyPair (session, mechanism, pub_template, pub_count, priv_template, priv_count, pub_key, priv_key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_WrapKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
               CK_OBJECT_HANDLE wrapping_key, CK_OBJECT_HANDLE key,
               CK_BYTE_PTR wrapped_key, CK_ULONG_PTR wrapped_key_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_WrapKey (session, mechanism, wrapping_key, key, wrapped_key, wrapped_key_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_UnwrapKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                 CK_OBJECT_HANDLE unwrapping_key, CK_BYTE_PTR wrapped_key,
                 CK_ULONG wrapped_key_len, CK_ATTRIBUTE_PTR template,
                 CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_UnwrapKey (session, mechanism, unwrapping_key, wrapped_key, wrapped_key_len, template, count, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_DeriveKey (CK_SESSION_HANDLE handle, CK_MECHANISM_PTR mechanism,
                 CK_OBJECT_HANDLE base_key, CK_ATTRIBUTE_PTR template,
                 CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_DeriveKey (session, mechanism, base_key, template, count, key);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_SeedRandom (CK_SESSION_HANDLE handle, CK_BYTE_PTR seed, CK_ULONG seed_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_SeedRandom (session, seed, seed_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

static CK_RV
gkm_C_GenerateRandom (CK_SESSION_HANDLE handle, CK_BYTE_PTR random_data,
                      CK_ULONG random_len)
{
	CK_RV rv = CKR_CRYPTOKI_NOT_INITIALIZED;
	GkmSession *session;

	g_static_mutex_lock (&pkcs11_module_mutex);

		if (pkcs11_module != NULL) {
			session = gkm_module_lookup_session (pkcs11_module, handle);
			if (session != NULL)
				rv = gkm_session_C_GenerateRandom (session, random_data, random_len);
			else
				rv = CKR_SESSION_HANDLE_INVALID;
		}

	g_static_mutex_unlock (&pkcs11_module_mutex);

	return rv;
}

/* --------------------------------------------------------------------
 * MODULE ENTRY POINT
 */

/*
 * PKCS#11 is broken here. It states that Unix compilers automatically byte
 * pack structures. This is wrong. GCC on Linux aligns to 4 by default.
 *
 * This results in incompatibilities. Where this structure's first version
 * members take up too much or too little space depending on how this module
 * is compiled.
 */

static CK_FUNCTION_LIST gkm_module_function_list = {
	{ CRYPTOKI_VERSION_MAJOR, CRYPTOKI_VERSION_MINOR },  /* version */
	gkm_C_Initialize,
	gkm_C_Finalize,
	gkm_C_GetInfo,
	gkm_C_GetFunctionList,
	gkm_C_GetSlotList,
	gkm_C_GetSlotInfo,
	gkm_C_GetTokenInfo,
	gkm_C_GetMechanismList,
	gkm_C_GetMechanismInfo,
	gkm_C_InitToken,
	gkm_C_InitPIN,
	gkm_C_SetPIN,
	gkm_C_OpenSession,
	gkm_C_CloseSession,
	gkm_C_CloseAllSessions,
	gkm_C_GetSessionInfo,
	gkm_C_GetOperationState,
	gkm_C_SetOperationState,
	gkm_C_Login,
	gkm_C_Logout,
	gkm_C_CreateObject,
	gkm_C_CopyObject,
	gkm_C_DestroyObject,
	gkm_C_GetObjectSize,
	gkm_C_GetAttributeValue,
	gkm_C_SetAttributeValue,
	gkm_C_FindObjectsInit,
	gkm_C_FindObjects,
	gkm_C_FindObjectsFinal,
	gkm_C_EncryptInit,
	gkm_C_Encrypt,
	gkm_C_EncryptUpdate,
	gkm_C_EncryptFinal,
	gkm_C_DecryptInit,
	gkm_C_Decrypt,
	gkm_C_DecryptUpdate,
	gkm_C_DecryptFinal,
	gkm_C_DigestInit,
	gkm_C_Digest,
	gkm_C_DigestUpdate,
	gkm_C_DigestKey,
	gkm_C_DigestFinal,
	gkm_C_SignInit,
	gkm_C_Sign,
	gkm_C_SignUpdate,
	gkm_C_SignFinal,
	gkm_C_SignRecoverInit,
	gkm_C_SignRecover,
	gkm_C_VerifyInit,
	gkm_C_Verify,
	gkm_C_VerifyUpdate,
	gkm_C_VerifyFinal,
	gkm_C_VerifyRecoverInit,
	gkm_C_VerifyRecover,
	gkm_C_DigestEncryptUpdate,
	gkm_C_DecryptDigestUpdate,
	gkm_C_SignEncryptUpdate,
	gkm_C_DecryptVerifyUpdate,
	gkm_C_GenerateKey,
	gkm_C_GenerateKeyPair,
	gkm_C_WrapKey,
	gkm_C_UnwrapKey,
	gkm_C_DeriveKey,
	gkm_C_SeedRandom,
	gkm_C_GenerateRandom,
	gkm_C_GetFunctionStatus,
	gkm_C_CancelFunction,
	gkm_C_WaitForSlotEvent
};
