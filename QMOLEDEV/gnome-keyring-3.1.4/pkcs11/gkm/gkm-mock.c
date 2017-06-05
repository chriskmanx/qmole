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

#include "gkm-attributes.h"
#include "gkm-mock.h"
#include "gkm-util.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11i.h"

#include <glib.h>

#include <string.h>

#define GKM_TEST_SLOT_ONE  52
#define GKM_TEST_SLOT_TWO  134


static gboolean initialized = FALSE;
static gchar *the_pin = NULL;
static gulong n_the_pin = 0;

static gboolean logged_in = FALSE;
static CK_USER_TYPE user_type = 0;
static CK_FUNCTION_LIST functionList;

typedef enum _Operation {
	OP_FIND = 1,
	OP_CRYPTO
} Operation;

typedef struct _Session {
	CK_SESSION_HANDLE handle;
	CK_SESSION_INFO info;
	GHashTable *objects;

	Operation operation;

	/* For find operations */
	GList *matches;

	/* For crypto operations */
	CK_OBJECT_HANDLE crypto_key;
	CK_ATTRIBUTE_TYPE crypto_method;
	CK_MECHANISM_TYPE crypto_mechanism;
	CK_BBOOL want_context_login;

	/* For 'signing' with CKM_MOCK_PREFIX */
	CK_BYTE sign_prefix[128];
	CK_ULONG n_sign_prefix;
} Session;

static guint unique_identifier = 100;
static GHashTable *the_sessions = NULL;
static GHashTable *the_objects = NULL;

enum {
	PRIVATE_KEY_CAPITALIZE = 3,
	PUBLIC_KEY_CAPITALIZE = 4,
	PRIVATE_KEY_PREFIX = 5,
	PUBLIC_KEY_PREFIX = 6
};

#define SIGNED_PREFIX "signed-prefix:"

static void
free_session (gpointer data)
{
	Session *sess = (Session*)data;
	if (sess)
		g_hash_table_destroy (sess->objects);
	g_free (sess);
}

static GArray*
lookup_object (Session *session, CK_OBJECT_HANDLE hObject)
{
	GArray *attrs;
	attrs = g_hash_table_lookup (the_objects, GUINT_TO_POINTER (hObject));
	if (!attrs)
		attrs = g_hash_table_lookup (session->objects, GUINT_TO_POINTER (hObject));
	return attrs;
}

CK_OBJECT_HANDLE
gkm_mock_module_take_object (GArray *template)
{
	gboolean token;
	guint handle;

	g_return_val_if_fail (the_objects, 0);

	handle = ++unique_identifier;
	if (gkm_template_find_boolean (template, CKA_TOKEN, &token))
		g_return_val_if_fail (token == TRUE, 0);
	else
		gkm_template_set_boolean (template, CKA_TOKEN, CK_TRUE);
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (handle), template);
	return handle;
}

void
gkm_mock_module_enumerate_objects (CK_SESSION_HANDLE handle, GkmMockEnumerator func,
                                   gpointer user_data)
{
	GHashTableIter iter;
	gpointer key;
	gpointer value;
	Session *session;

	g_assert (the_objects);
	g_assert (func);

	/* Token objects */
	g_hash_table_iter_init (&iter, the_objects);
	while (g_hash_table_iter_next (&iter, &key, &value)) {
		if (!(func) (GPOINTER_TO_UINT (key), value, user_data))
			return;
	}

	/* session objects */
	if (handle) {
		session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (handle));
		if (session) {
			g_hash_table_iter_init (&iter, session->objects);
			while (g_hash_table_iter_next (&iter, &key, &value)) {
				if (!(func) (GPOINTER_TO_UINT (key), value, user_data))
					return;
			}
		}
	}
}

typedef struct _FindObject {
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;
	CK_OBJECT_HANDLE object;
} FindObject;

static gboolean
enumerate_and_find_object (CK_OBJECT_HANDLE object, GArray *template, gpointer user_data)
{
	FindObject *ctx = user_data;
	CK_ATTRIBUTE_PTR match, attr;
	CK_ULONG i;

	for (i = 0; i < ctx->n_attrs; ++i) {
		match = ctx->attrs + i;
		attr = gkm_template_find (template, match->type);
		if (!attr)
			return TRUE; /* Continue */

		if (attr->ulValueLen != match->ulValueLen ||
		    memcmp (attr->pValue, match->pValue, attr->ulValueLen) != 0)
			return TRUE; /* Continue */
	}

	ctx->object = object;
	return FALSE; /* Stop iteration */
}

CK_OBJECT_HANDLE
gkm_mock_module_find_object (CK_SESSION_HANDLE session, CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	FindObject ctx;

	ctx.attrs = attrs;
	ctx.n_attrs = n_attrs;
	ctx.object = 0;

	gkm_mock_module_enumerate_objects (session, enumerate_and_find_object, &ctx);

	return ctx.object;
}

static gboolean
enumerate_and_count_objects (CK_OBJECT_HANDLE object, GArray *template, gpointer user_data)
{
	guint *n_objects = user_data;
	++(*n_objects);
	return TRUE; /* Continue */
}

guint
gkm_mock_module_count_objects (CK_SESSION_HANDLE session)
{
	guint n_objects = 0;
	gkm_mock_module_enumerate_objects (session, enumerate_and_count_objects, &n_objects);
	return n_objects;
}

void
gkm_mock_module_set_object (CK_OBJECT_HANDLE object, CK_ATTRIBUTE_PTR attrs,
                            CK_ULONG n_attrs)
{
	CK_ULONG i;
	GArray *template;

	g_return_if_fail (object != 0);
	g_return_if_fail (the_objects);

	template = g_hash_table_lookup (the_objects, GUINT_TO_POINTER (object));
	g_return_if_fail (template);

	for (i = 0; i < n_attrs; ++i)
		gkm_template_set (template, attrs + i);
}

void
gkm_mock_module_set_pin (const gchar *password)
{
	g_free (the_pin);
	the_pin = g_strdup (password);
	n_the_pin = strlen (password);
}

CK_RV
gkm_mock_C_Initialize (CK_VOID_PTR pInitArgs)
{
	GArray *attrs;
	CK_ULONG value;
	CK_C_INITIALIZE_ARGS_PTR args;

	g_return_val_if_fail (initialized == FALSE, CKR_CRYPTOKI_ALREADY_INITIALIZED);

	args = (CK_C_INITIALIZE_ARGS_PTR)pInitArgs;
	if (args) {
		g_return_val_if_fail(
		              (args->CreateMutex == NULL && args->DestroyMutex == NULL &&
		               args->LockMutex == NULL && args->UnlockMutex == NULL) ||
		              (args->CreateMutex != NULL && args->DestroyMutex != NULL &&
		               args->LockMutex != NULL && args->UnlockMutex != NULL),
		               CKR_ARGUMENTS_BAD);

		/* Flags should allow OS locking and os threads */
		g_return_val_if_fail (args->flags & CKF_OS_LOCKING_OK, CKR_CANT_LOCK);
		g_return_val_if_fail (args->flags & CKF_LIBRARY_CANT_CREATE_OS_THREADS, CKR_NEED_TO_CREATE_THREADS);
	}

	the_pin = g_strdup ("booo");
	n_the_pin = strlen (the_pin);
	the_sessions = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, free_session);
	the_objects = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, (GDestroyNotify)gkm_template_free);

	/* Our token object */
	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_DATA);
	gkm_template_set_string (attrs, CKA_LABEL, "TEST LABEL");
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (2), attrs);

	/* Private capitalize key */
	value = CKM_MOCK_CAPITALIZE;
	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_PRIVATE_KEY);
	gkm_template_set_string (attrs, CKA_LABEL, "Private Capitalize Key");
	gkm_template_set_value (attrs, CKA_ALLOWED_MECHANISMS, &value, sizeof (value));
	gkm_template_set_boolean (attrs, CKA_DECRYPT, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_PRIVATE, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_WRAP, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_UNWRAP, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_DERIVE, CK_TRUE);
	gkm_template_set_string (attrs, CKA_VALUE, "value");
	gkm_template_set_string (attrs, CKA_GNOME_UNIQUE, "unique1");
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PRIVATE_KEY_CAPITALIZE), attrs);

	/* Public capitalize key */
	value = CKM_MOCK_CAPITALIZE;
	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_PUBLIC_KEY);
	gkm_template_set_string (attrs, CKA_LABEL, "Public Capitalize Key");
	gkm_template_set_value (attrs, CKA_ALLOWED_MECHANISMS, &value, sizeof (value));
	gkm_template_set_boolean (attrs, CKA_ENCRYPT, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_PRIVATE, CK_FALSE);
	gkm_template_set_string (attrs, CKA_VALUE, "value");
	gkm_template_set_string (attrs, CKA_GNOME_UNIQUE, "unique2");
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PUBLIC_KEY_CAPITALIZE), attrs);

	/* Private prefix key */
	value = CKM_MOCK_PREFIX;
	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_PRIVATE_KEY);
	gkm_template_set_string (attrs, CKA_LABEL, "Private prefix key");
	gkm_template_set_value (attrs, CKA_ALLOWED_MECHANISMS, &value, sizeof (value));
	gkm_template_set_boolean (attrs, CKA_SIGN, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_PRIVATE, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_ALWAYS_AUTHENTICATE, CK_TRUE);
	gkm_template_set_string (attrs, CKA_VALUE, "value");
	gkm_template_set_string (attrs, CKA_GNOME_UNIQUE, "unique3");
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PRIVATE_KEY_PREFIX), attrs);

	/* Private prefix key */
	value = CKM_MOCK_PREFIX;
	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_PUBLIC_KEY);
	gkm_template_set_string (attrs, CKA_LABEL, "Public prefix key");
	gkm_template_set_value (attrs, CKA_ALLOWED_MECHANISMS, &value, sizeof (value));
	gkm_template_set_boolean (attrs, CKA_VERIFY, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_PRIVATE, CK_FALSE);
	gkm_template_set_string (attrs, CKA_VALUE, "value");
	gkm_template_set_string (attrs, CKA_GNOME_UNIQUE, "unique4");
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PUBLIC_KEY_PREFIX), attrs);

	initialized = TRUE;
	return CKR_OK;
}

CK_RV
gkm_mock_C_Finalize (CK_VOID_PTR pReserved)
{
	g_return_val_if_fail (pReserved == NULL, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (initialized == TRUE, CKR_CRYPTOKI_NOT_INITIALIZED);

	initialized = FALSE;
	logged_in = FALSE;
	g_hash_table_destroy (the_objects);
	the_objects = NULL;

	g_hash_table_destroy (the_sessions);
	the_sessions = NULL;

	g_free (the_pin);
	return CKR_OK;
}

static const CK_INFO TEST_INFO = {
	{ CRYPTOKI_VERSION_MAJOR, CRYPTOKI_VERSION_MINOR },
	"TEST MANUFACTURER              ",
	0,
	"TEST LIBRARY                   ",
	{ 45, 145 }
};

CK_RV
gkm_mock_C_GetInfo (CK_INFO_PTR pInfo)
{
	g_assert (pInfo != NULL && "Invalid pointer to GetInfo");
	memcpy (pInfo, &TEST_INFO, sizeof (*pInfo));
	return CKR_OK;
}

CK_RV
gkm_mock_C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	if (!list)
		return CKR_ARGUMENTS_BAD;
	*list = &functionList;
	return CKR_OK;
}

/*
 * Two slots
 *  ONE: token present
 *  TWO: token not present
 */

CK_RV
gkm_mock_C_GetSlotList (CK_BBOOL tokenPresent, CK_SLOT_ID_PTR pSlotList, CK_ULONG_PTR pulCount)
{
	CK_ULONG count;

	g_assert (pulCount != NULL && "Invalid pulCount");

	count = tokenPresent ? 1 : 2;

	/* Application only wants to know the number of slots. */
	if (pSlotList == NULL) {
		*pulCount = count;
		return CKR_OK;
	}

	if (*pulCount < count) {
		g_assert (*pulCount && "Passed in a bad count");
		return CKR_BUFFER_TOO_SMALL;
	}

	*pulCount = count;
	pSlotList[0] = GKM_TEST_SLOT_ONE;
	if (!tokenPresent)
		pSlotList[1] = GKM_TEST_SLOT_TWO;

	return CKR_OK;
}

static const CK_SLOT_INFO TEST_INFO_ONE = {
	"TEST SLOT                                                       ",
	"TEST MANUFACTURER              ",
	CKF_TOKEN_PRESENT | CKF_REMOVABLE_DEVICE,
	{ 55, 155 },
	{ 65, 165 },
};

static const CK_SLOT_INFO TEST_INFO_TWO = {
	"TEST SLOT                                                       ",
	"TEST MANUFACTURER              ",
	CKF_REMOVABLE_DEVICE,
	{ 55, 155 },
	{ 65, 165 },
};

CK_RV
gkm_mock_C_GetSlotInfo (CK_SLOT_ID slotID, CK_SLOT_INFO_PTR pInfo)
{
	g_assert (pInfo != NULL && "Invalid pInfo");

	if (slotID == GKM_TEST_SLOT_ONE) {
		memcpy (pInfo, &TEST_INFO_ONE, sizeof (*pInfo));
		return CKR_OK;
	} else if (slotID == GKM_TEST_SLOT_TWO) {
		memcpy (pInfo, &TEST_INFO_TWO, sizeof (*pInfo));
		return CKR_OK;
	} else {
		g_assert_not_reached (); /* "Invalid slot id" */
		return CKR_SLOT_ID_INVALID;
	}
}

static const CK_TOKEN_INFO TEST_TOKEN_ONE = {
	"TEST LABEL                      ",
	"TEST MANUFACTURER               ",
	"TEST MODEL      ",
	"TEST SERIAL     ",
	CKF_LOGIN_REQUIRED | CKF_USER_PIN_INITIALIZED | CKF_CLOCK_ON_TOKEN | CKF_TOKEN_INITIALIZED,
	1,
	2,
	3,
	4,
	5,
	6,
	7,
	8,
	9,
	10,
	{ 75, 175 },
	{ 85, 185 },
	{ '1', '9', '9', '9', '0', '5', '2', '5', '0', '9', '1', '9', '5', '9', '0', '0' }
};

CK_RV
gkm_mock_C_GetTokenInfo (CK_SLOT_ID slotID, CK_TOKEN_INFO_PTR pInfo)
{
	g_return_val_if_fail (pInfo != NULL, CKR_ARGUMENTS_BAD);

	if (slotID == GKM_TEST_SLOT_ONE) {
		memcpy (pInfo, &TEST_TOKEN_ONE, sizeof (*pInfo));
		return CKR_OK;
	} else if (slotID == GKM_TEST_SLOT_TWO) {
		return CKR_TOKEN_NOT_PRESENT;
	} else {
		g_return_val_if_reached (CKR_SLOT_ID_INVALID);
	}
}

CK_RV
gkm_mock_fail_C_GetTokenInfo (CK_SLOT_ID slotID, CK_TOKEN_INFO_PTR pInfo)
{
	return CKR_GENERAL_ERROR;
}

/*
 * TWO mechanisms:
 *  CKM_MOCK_CAPITALIZE
 *  CKM_MOCK_PREFIX
 */

CK_RV
gkm_mock_C_GetMechanismList (CK_SLOT_ID slotID, CK_MECHANISM_TYPE_PTR pMechanismList,
                             CK_ULONG_PTR pulCount)
{
	g_assert (slotID == GKM_TEST_SLOT_ONE && "Invalid slotID");
	g_assert (pulCount != NULL && "Invalid pulCount");

	/* Application only wants to know the number of slots. */
	if (pMechanismList == NULL) {
		*pulCount = 2;
		return CKR_OK;
	}

	if (*pulCount != 2) {
		g_assert (*pulCount && "Passed in a bad count");
		return CKR_BUFFER_TOO_SMALL;
	}

	pMechanismList[0] = CKM_MOCK_CAPITALIZE;
	pMechanismList[1] = CKM_MOCK_PREFIX;
	return CKR_OK;
}

static const CK_MECHANISM_INFO TEST_MECH_CAPITALIZE = {
	512, 4096, 0
};

static const CK_MECHANISM_INFO TEST_MECH_PREFIX = {
	2048, 2048, 0
};

CK_RV
gkm_mock_C_GetMechanismInfo (CK_SLOT_ID slotID, CK_MECHANISM_TYPE type,
                             CK_MECHANISM_INFO_PTR pInfo)
{
	g_assert (slotID == GKM_TEST_SLOT_ONE && "Invalid slotID");
	g_assert (pInfo != NULL && "Invalid pInfo");

	if (type == CKM_MOCK_CAPITALIZE) {
		memcpy (pInfo, &TEST_MECH_CAPITALIZE, sizeof (*pInfo));
		return CKR_OK;
	} else if (type == CKM_MOCK_PREFIX) {
		memcpy (pInfo, &TEST_MECH_PREFIX, sizeof (*pInfo));
		return CKR_OK;
	} else {
		g_assert_not_reached (); /* "Invalid type" */
		return CKR_MECHANISM_INVALID;
	}
}

CK_RV
gkm_mock_C_InitToken (CK_SLOT_ID slotID, CK_UTF8CHAR_PTR pPin, CK_ULONG ulPinLen,
                      CK_UTF8CHAR_PTR pLabel)
{
	g_assert (slotID == GKM_TEST_SLOT_ONE && "Invalid slotID");
	g_assert (pPin != NULL && "Invalid pPin");
	g_assert (strlen ("TEST PIN") && "Invalid ulPinLen");
	g_assert (strncmp ((gchar*)pPin, "TEST PIN", ulPinLen) == 0 && "Invalid pPin string");
	g_assert (pLabel != NULL && "Invalid pLabel");
	g_assert (strcmp ((gchar*)pPin, "TEST LABEL") == 0 && "Invalid pLabel string");

	g_free (the_pin);
	the_pin = g_strndup ((gchar*)pPin, ulPinLen);
	n_the_pin = ulPinLen;
	return CKR_OK;
}

CK_RV
gkm_mock_unsupported_C_WaitForSlotEvent (CK_FLAGS flags, CK_SLOT_ID_PTR pSlot, CK_VOID_PTR pReserved)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_C_OpenSession (CK_SLOT_ID slotID, CK_FLAGS flags, CK_VOID_PTR pApplication,
                        CK_NOTIFY Notify, CK_SESSION_HANDLE_PTR phSession)
{
	Session *sess;

	g_return_val_if_fail (slotID == GKM_TEST_SLOT_ONE, CKR_SLOT_ID_INVALID);
	g_return_val_if_fail (phSession != NULL, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail ((flags & CKF_SERIAL_SESSION) == CKF_SERIAL_SESSION, CKR_SESSION_PARALLEL_NOT_SUPPORTED);

	sess = g_new0 (Session, 1);
	sess->handle = ++unique_identifier;
	sess->info.flags = flags;
	sess->info.slotID = slotID;
	sess->info.state = 0;
	sess->info.ulDeviceError = 1414;
	sess->objects = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, (GDestroyNotify)gkm_template_free);
	*phSession = sess->handle;

	g_hash_table_replace (the_sessions, GUINT_TO_POINTER (sess->handle), sess);
	return CKR_OK;
}

CK_RV
gkm_mock_fail_C_OpenSession (CK_SLOT_ID slotID, CK_FLAGS flags, CK_VOID_PTR pApplication,
                             CK_NOTIFY Notify, CK_SESSION_HANDLE_PTR phSession)
{
	return CKR_GENERAL_ERROR;
}

CK_RV
gkm_mock_C_CloseSession (CK_SESSION_HANDLE hSession)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	g_hash_table_remove (the_sessions, GUINT_TO_POINTER (hSession));
	return CKR_OK;
}

CK_RV
gkm_mock_C_CloseAllSessions (CK_SLOT_ID slotID)
{
	g_assert (slotID == GKM_TEST_SLOT_ONE && "Invalid slotID");

	g_hash_table_remove_all (the_sessions);
	return CKR_OK;
}

CK_RV
gkm_mock_C_GetFunctionStatus (CK_SESSION_HANDLE hSession)
{
	return CKR_FUNCTION_NOT_PARALLEL;
}

CK_RV
gkm_mock_C_CancelFunction (CK_SESSION_HANDLE hSession)
{
	return CKR_FUNCTION_NOT_PARALLEL;
}

CK_RV
gkm_mock_C_GetSessionInfo (CK_SESSION_HANDLE hSession, CK_SESSION_INFO_PTR pInfo)
{
	Session *session;

	g_return_val_if_fail (pInfo != NULL, CKR_ARGUMENTS_BAD);

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	if (logged_in) {
		if (session->info.flags & CKF_RW_SESSION)
			session->info.state = CKS_RW_USER_FUNCTIONS;
		else
			session->info.state = CKS_RO_USER_FUNCTIONS;
	} else {
		if (session->info.flags & CKF_RW_SESSION)
			session->info.state = CKS_RW_PUBLIC_SESSION;
		else
			session->info.state = CKS_RO_PUBLIC_SESSION;
	}

	memcpy (pInfo, &session->info, sizeof (*pInfo));
	return CKR_OK;
}

CK_RV
gkm_mock_fail_C_GetSessionInfo (CK_SESSION_HANDLE hSession, CK_SESSION_INFO_PTR pInfo)
{
	return CKR_GENERAL_ERROR;
}

CK_RV
gkm_mock_C_InitPIN (CK_SESSION_HANDLE hSession, CK_UTF8CHAR_PTR pPin,
                    CK_ULONG ulPinLen)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_return_val_if_fail (session, CKR_SESSION_HANDLE_INVALID);

	g_free (the_pin);
	the_pin = g_strndup ((gchar*)pPin, ulPinLen);
	n_the_pin = ulPinLen;
	return CKR_OK;
}

CK_RV
gkm_mock_C_SetPIN (CK_SESSION_HANDLE hSession, CK_UTF8CHAR_PTR pOldPin,
                   CK_ULONG ulOldLen, CK_UTF8CHAR_PTR pNewPin, CK_ULONG ulNewLen)
{
	Session *session;
	gchar *old;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_return_val_if_fail (session, CKR_SESSION_HANDLE_INVALID);

	old = g_strndup ((gchar*)pOldPin, ulOldLen);
	if (!old || !g_str_equal (old, the_pin))
		return CKR_PIN_INCORRECT;

	g_free (the_pin);
	the_pin = g_strndup ((gchar*)pNewPin, ulNewLen);
	n_the_pin = ulNewLen;
	return CKR_OK;
}

CK_RV
gkm_mock_unsupported_C_GetOperationState (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pOperationState,
                                          CK_ULONG_PTR pulOperationStateLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_SetOperationState (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pOperationState,
                                          CK_ULONG ulOperationStateLen, CK_OBJECT_HANDLE hEncryptionKey,
                                          CK_OBJECT_HANDLE hAuthenticationKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_C_Login (CK_SESSION_HANDLE hSession, CK_USER_TYPE userType,
                  CK_UTF8CHAR_PTR pPin, CK_ULONG pPinLen)
{
	Session *session;

	g_return_val_if_fail (userType == CKU_SO ||
	                      userType == CKU_USER ||
	                      userType == CKU_CONTEXT_SPECIFIC,
	                      CKR_USER_TYPE_INVALID);

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_return_val_if_fail (session != NULL, CKR_SESSION_HANDLE_INVALID);
	g_return_val_if_fail (logged_in == FALSE, CKR_USER_ALREADY_LOGGED_IN);

	if (!pPin)
		return CKR_PIN_INCORRECT;

	if (pPinLen != strlen (the_pin))
		return CKR_PIN_INCORRECT;
	if (strncmp ((gchar*)pPin, the_pin, pPinLen) != 0)
		return CKR_PIN_INCORRECT;

	if (userType == CKU_CONTEXT_SPECIFIC) {
		g_return_val_if_fail (session->want_context_login == TRUE, CKR_OPERATION_NOT_INITIALIZED);
		session->want_context_login = CK_FALSE;
	} else {
		logged_in = TRUE;
		user_type = userType;
	}

	return CKR_OK;
}

CK_RV
gkm_mock_C_Logout (CK_SESSION_HANDLE hSession)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	g_assert (logged_in && "Not logged in");
	logged_in = FALSE;
	user_type = 0;
	return CKR_OK;
}

CK_RV
gkm_mock_C_CreateObject (CK_SESSION_HANDLE hSession, CK_ATTRIBUTE_PTR pTemplate,
                         CK_ULONG ulCount, CK_OBJECT_HANDLE_PTR phObject)
{
	GArray *attrs;
	Session *session;
	gboolean token, priv;
	CK_OBJECT_CLASS klass;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_PTR attr;

	g_assert (phObject != NULL);

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	attrs = gkm_template_new (pTemplate, ulCount);

	if (gkm_template_find_boolean (attrs, CKA_PRIVATE, &priv) && priv) {
		if (!logged_in) {
			gkm_template_free (attrs);
			return CKR_USER_NOT_LOGGED_IN;
		}
	}

	/* In order to create a credential we must check CK_VALUE */
	if (gkm_template_find_ulong (attrs, CKA_CLASS, &klass) && klass == CKO_G_CREDENTIAL) {
		if (gkm_template_find_ulong (attrs, CKA_G_OBJECT, &object)) {
			attr = gkm_template_find (attrs, CKA_VALUE);
			if (!attr || attr->ulValueLen != n_the_pin ||
			    memcmp (attr->pValue, the_pin, attr->ulValueLen) != 0) {
				gkm_template_free (attrs);
				return CKR_PIN_INCORRECT;
			}
		}
	}

	*phObject = ++unique_identifier;
	if (gkm_template_find_boolean (attrs, CKA_TOKEN, &token) && token)
		g_hash_table_insert (the_objects, GUINT_TO_POINTER (*phObject), attrs);
	else
		g_hash_table_insert (session->objects, GUINT_TO_POINTER (*phObject), attrs);

	return CKR_OK;
}

CK_RV
gkm_mock_unsupported_C_CopyObject (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                                   CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount,
                                   CK_OBJECT_HANDLE_PTR phNewObject)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_C_DestroyObject (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject)
{
	GArray *attrs;
	Session *session;
	gboolean priv;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_return_val_if_fail (session, CKR_SESSION_HANDLE_INVALID);

	attrs = lookup_object (session, hObject);
	g_return_val_if_fail (attrs, CKR_OBJECT_HANDLE_INVALID);

	if (gkm_template_find_boolean (attrs, CKA_PRIVATE, &priv) && priv) {
		if (!logged_in)
			return CKR_USER_NOT_LOGGED_IN;
	}

	g_hash_table_remove (the_objects, GUINT_TO_POINTER (hObject));
	g_hash_table_remove (session->objects, GUINT_TO_POINTER (hObject));

	return CKR_OK;
}

CK_RV
gkm_mock_unsupported_C_GetObjectSize (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                                      CK_ULONG_PTR pulSize)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_C_GetAttributeValue (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                              CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount)
{
	CK_ATTRIBUTE_PTR result;
	CK_RV ret = CKR_OK;
	GArray *attrs;
	CK_ATTRIBUTE_PTR attr;
	Session *session;
	CK_ULONG i;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	attrs = lookup_object (session, hObject);
	if (!attrs) {
		g_assert_not_reached (); /* "invalid object handle passed" */
		return CKR_OBJECT_HANDLE_INVALID;
	}

	for (i = 0; i < ulCount; ++i) {
		result = pTemplate + i;
		attr = gkm_template_find (attrs, result->type);
		if (!attr) {
			result->ulValueLen = (CK_ULONG)-1;
			ret = CKR_ATTRIBUTE_TYPE_INVALID;
			continue;
		}

		if (!result->pValue) {
			result->ulValueLen = attr->ulValueLen;
			continue;
		}

		if (result->ulValueLen >= attr->ulValueLen) {
			memcpy (result->pValue, attr->pValue, attr->ulValueLen);
			continue;
		}

		result->ulValueLen = (CK_ULONG)-1;
		ret = CKR_BUFFER_TOO_SMALL;
	}

	return ret;
}

CK_RV
gkm_mock_C_SetAttributeValue (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                              CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount)
{
	Session *session;
	GArray *attrs;
	CK_ULONG i;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	attrs = lookup_object (session, hObject);
	if (!attrs) {
		g_assert_not_reached (); /* "invalid object handle passed" */
		return CKR_OBJECT_HANDLE_INVALID;
	}

	for (i = 0; i < ulCount; ++i)
		gkm_template_set (attrs, pTemplate + i);

	return CKR_OK;
}

typedef struct _FindObjects {
	CK_ATTRIBUTE_PTR template;
	CK_ULONG count;
	Session *session;
} FindObjects;

static gboolean
enumerate_and_find_objects (CK_OBJECT_HANDLE object, GArray *attrs, gpointer user_data)
{
	FindObjects *ctx = user_data;
	CK_ATTRIBUTE_PTR match, attr;
	CK_ULONG i;

	for (i = 0; i < ctx->count; ++i) {
		match = ctx->template + i;
		attr = gkm_template_find (attrs, match->type);
		if (!attr)
			return TRUE; /* Continue */

		if (attr->ulValueLen != match->ulValueLen ||
		    memcmp (attr->pValue, match->pValue, attr->ulValueLen) != 0)
			return TRUE; /* Continue */
	}

	ctx->session->matches = g_list_prepend (ctx->session->matches, GUINT_TO_POINTER (object));
	return TRUE; /* Continue */
}

CK_RV
gkm_mock_C_FindObjectsInit (CK_SESSION_HANDLE hSession, CK_ATTRIBUTE_PTR pTemplate,
                            CK_ULONG ulCount)
{
	Session *session;
	FindObjects ctx;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_return_val_if_fail (session != NULL, CKR_SESSION_HANDLE_INVALID);

	/* Starting an operation, cancels any previous one */
	if (session->operation != 0)
		session->operation = 0;

	session->operation = OP_FIND;

	ctx.template = pTemplate;
	ctx.count = ulCount;
	ctx.session = session;

	gkm_mock_module_enumerate_objects (hSession, enumerate_and_find_objects, &ctx);
	return CKR_OK;
}

CK_RV
gkm_mock_C_FindObjects (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE_PTR phObject,
                        CK_ULONG ulMaxObjectCount, CK_ULONG_PTR pulObjectCount)
{
	Session *session;

	g_assert (phObject != NULL);
	g_assert (pulObjectCount != NULL);
	g_assert (ulMaxObjectCount != 0);

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	if (session->operation != OP_FIND) {
		g_assert_not_reached (); /* "invalid call to FindObjects" */
		return CKR_OPERATION_NOT_INITIALIZED;
	}

	*pulObjectCount = 0;
	while (ulMaxObjectCount > 0 && session->matches) {
		*phObject = GPOINTER_TO_UINT (session->matches->data);
		++phObject;
		--ulMaxObjectCount;
		++(*pulObjectCount);
		session->matches = g_list_remove (session->matches, session->matches->data);
	}

	return CKR_OK;
}

CK_RV
gkm_mock_C_FindObjectsFinal (CK_SESSION_HANDLE hSession)
{

	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	if (session->operation != OP_FIND) {
		g_assert_not_reached (); /* "invalid call to FindObjectsFinal" */
		return CKR_OPERATION_NOT_INITIALIZED;
	}

	session->operation = 0;
	g_list_free (session->matches);
	session->matches = NULL;

	return CKR_OK;
}

CK_RV
gkm_mock_C_EncryptInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                        CK_OBJECT_HANDLE hKey)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	/* Starting an operation, cancels any previous one */
	if (session->operation != 0)
		session->operation = 0;

	g_assert (pMechanism);
	g_assert (pMechanism->mechanism == CKM_MOCK_CAPITALIZE);
	g_assert (hKey == PUBLIC_KEY_CAPITALIZE);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_ENCRYPT;
	session->crypto_mechanism = CKM_MOCK_CAPITALIZE;
	session->crypto_key = hKey;
	return CKR_OK;
}

CK_RV
gkm_mock_C_Encrypt (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
                    CK_BYTE_PTR pEncryptedData, CK_ULONG_PTR pulEncryptedDataLen)
{
	Session *session;
	CK_ULONG i;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	if (session->operation != OP_CRYPTO) {
		g_assert_not_reached (); /* "invalid call to Encrypt" */
		return CKR_OPERATION_NOT_INITIALIZED;
	}

	g_assert (pData);
	g_assert (pulEncryptedDataLen);
	g_assert (session->crypto_method == CKA_ENCRYPT);
	g_assert (session->crypto_mechanism == CKM_MOCK_CAPITALIZE);
	g_assert (session->crypto_key == PUBLIC_KEY_CAPITALIZE);

	if (!pEncryptedData) {
		*pulEncryptedDataLen = ulDataLen;
		return CKR_OK;
	}

	if (*pulEncryptedDataLen < ulDataLen) {
		*pulEncryptedDataLen = ulDataLen;
		return CKR_BUFFER_TOO_SMALL;
	}

	for (i = 0; i < ulDataLen; ++i)
		pEncryptedData[i] = g_ascii_toupper (pData[i]);
	*pulEncryptedDataLen = ulDataLen;

	session->operation = 0;
	session->crypto_method = 0;
	session->crypto_mechanism = 0;
	session->crypto_key = 0;

	return CKR_OK;
}

CK_RV
gkm_mock_unsupported_C_EncryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart,
                                      CK_ULONG ulPartLen, CK_BYTE_PTR pEncryptedPart,
                                      CK_ULONG_PTR pulEncryptedPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_EncryptFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pLastEncryptedPart,
                                     CK_ULONG_PTR pulLastEncryptedPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_C_DecryptInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                        CK_OBJECT_HANDLE hKey)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	/* Starting an operation, cancels any previous one */
	if (session->operation != 0)
		session->operation = 0;

	g_assert (pMechanism);
	g_assert (pMechanism->mechanism == CKM_MOCK_CAPITALIZE);
	g_assert (hKey == PRIVATE_KEY_CAPITALIZE);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_DECRYPT;
	session->crypto_mechanism = CKM_MOCK_CAPITALIZE;
	session->crypto_key = hKey;
	return CKR_OK;
}

CK_RV
gkm_mock_C_Decrypt (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedData,
                    CK_ULONG ulEncryptedDataLen, CK_BYTE_PTR pData, CK_ULONG_PTR pulDataLen)
{
	Session *session;
	CK_ULONG i;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	if (session->operation != OP_CRYPTO) {
		g_assert_not_reached (); /* "invalid call to Encrypt" */
		return CKR_OPERATION_NOT_INITIALIZED;
	}

	g_assert (pEncryptedData);
	g_assert (pulDataLen);
	g_assert (session->crypto_method == CKA_DECRYPT);
	g_assert (session->crypto_mechanism == CKM_MOCK_CAPITALIZE);
	g_assert (session->crypto_key == PRIVATE_KEY_CAPITALIZE);

	if (!pData) {
		*pulDataLen = ulEncryptedDataLen;
		return CKR_OK;
	}

	if (*pulDataLen < ulEncryptedDataLen) {
		*pulDataLen = ulEncryptedDataLen;
		return CKR_BUFFER_TOO_SMALL;
	}

	for (i = 0; i < ulEncryptedDataLen; ++i)
		pData[i] = g_ascii_tolower (pEncryptedData[i]);
	*pulDataLen = ulEncryptedDataLen;

	session->operation = 0;
	session->crypto_method = 0;
	session->crypto_mechanism = 0;
	session->crypto_key = 0;

	return CKR_OK;
}

CK_RV
gkm_mock_unsupported_C_DecryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedPart,
                                      CK_ULONG ulEncryptedPartLen, CK_BYTE_PTR pPart, CK_ULONG_PTR pulPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DecryptFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pLastPart,
                                     CK_ULONG_PTR pulLastPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DigestInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_Digest (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
                               CK_BYTE_PTR pDigest, CK_ULONG_PTR pulDigestLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DigestUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart, CK_ULONG ulPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DigestKey (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DigestFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pDigest,
                                    CK_ULONG_PTR pulDigestLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_C_SignInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                     CK_OBJECT_HANDLE hKey)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	/* Starting an operation, cancels any previous one */
	if (session->operation != 0)
		session->operation = 0;

	g_assert (pMechanism);
	g_assert (pMechanism->mechanism == CKM_MOCK_PREFIX);
	g_assert (hKey == PRIVATE_KEY_PREFIX);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_SIGN;
	session->crypto_mechanism = CKM_MOCK_PREFIX;
	session->crypto_key = hKey;

	if (pMechanism->pParameter) {
		g_assert (pMechanism->ulParameterLen < sizeof (session->sign_prefix));
		memcpy (session->sign_prefix, pMechanism->pParameter, pMechanism->ulParameterLen);
		session->n_sign_prefix = pMechanism->ulParameterLen;
	} else {
		g_assert (strlen (SIGNED_PREFIX) + 1 < sizeof (session->sign_prefix));
		strcpy ((gchar*)session->sign_prefix, SIGNED_PREFIX);
		session->n_sign_prefix = strlen (SIGNED_PREFIX);
	}

	/* The private key has CKA_ALWAYS_AUTHENTICATE above */
	session->want_context_login = CK_TRUE;

	return CKR_OK;
}

CK_RV
gkm_mock_C_Sign (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
                 CK_BYTE_PTR pSignature, CK_ULONG_PTR pulSignatureLen)
{
	Session *session;
	CK_ULONG length;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	if (session->operation != OP_CRYPTO) {
		g_assert_not_reached (); /* "invalid call to Encrypt" */
		return CKR_OPERATION_NOT_INITIALIZED;
	}

	if (session->want_context_login)
		return CKR_USER_NOT_LOGGED_IN;

	g_assert (pData);
	g_assert (pulSignatureLen);
	g_assert (session->crypto_method == CKA_SIGN);
	g_assert (session->crypto_mechanism == CKM_MOCK_PREFIX);
	g_assert (session->crypto_key == PRIVATE_KEY_PREFIX);

	length = session->n_sign_prefix + ulDataLen;

	if (!pSignature) {
		*pulSignatureLen = length;
		return CKR_OK;
	}

	if (*pulSignatureLen < length) {
		*pulSignatureLen = length;
		return CKR_BUFFER_TOO_SMALL;
	}

	memcpy (pSignature, session->sign_prefix, session->n_sign_prefix);
	memcpy (pSignature + session->n_sign_prefix, pData, ulDataLen);
	*pulSignatureLen = length;

	session->operation = 0;
	session->crypto_method = 0;
	session->crypto_mechanism = 0;
	session->crypto_key = 0;

	return CKR_OK;
}

CK_RV
gkm_mock_unsupported_C_SignUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart, CK_ULONG ulPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_SignFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSignature,
                                  CK_ULONG_PTR pulSignatureLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_SignRecoverInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                                        CK_OBJECT_HANDLE hKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_SignRecover (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
                                    CK_BYTE_PTR pSignature, CK_ULONG_PTR pulSignatureLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_C_VerifyInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                       CK_OBJECT_HANDLE hKey)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	/* Starting an operation, cancels any previous one */
	if (session->operation != 0)
		session->operation = 0;

	g_assert (pMechanism);
	g_assert (pMechanism->mechanism == CKM_MOCK_PREFIX);
	g_assert (hKey == PUBLIC_KEY_PREFIX);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_VERIFY;
	session->crypto_mechanism = CKM_MOCK_PREFIX;
	session->crypto_key = hKey;

	if (pMechanism->pParameter) {
		g_assert (pMechanism->ulParameterLen < sizeof (session->sign_prefix));
		memcpy (session->sign_prefix, pMechanism->pParameter, pMechanism->ulParameterLen);
		session->n_sign_prefix = pMechanism->ulParameterLen;
	} else {
		g_assert (strlen (SIGNED_PREFIX) + 1 < sizeof (session->sign_prefix));
		strcpy ((gchar*)session->sign_prefix, SIGNED_PREFIX);
		session->n_sign_prefix = strlen (SIGNED_PREFIX);
	}

	return CKR_OK;
}

CK_RV
gkm_mock_C_Verify (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
                   CK_BYTE_PTR pSignature, CK_ULONG ulSignatureLen)
{
	Session *session;
	CK_ULONG length;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	if (session->operation != OP_CRYPTO) {
		g_assert_not_reached (); /* "invalid call to Encrypt" */
		return CKR_OPERATION_NOT_INITIALIZED;
	}

	g_assert (pData);
	g_assert (pSignature);
	g_assert (session->crypto_method == CKA_VERIFY);
	g_assert (session->crypto_mechanism == CKM_MOCK_PREFIX);
	g_assert (session->crypto_key == PUBLIC_KEY_PREFIX);

	length = session->n_sign_prefix + ulDataLen;

	if (ulSignatureLen < length) {
		g_assert (FALSE);
		return CKR_SIGNATURE_LEN_RANGE;
	}

	if (memcmp (pSignature, session->sign_prefix, session->n_sign_prefix) == 0 &&
	    memcmp (pSignature + session->n_sign_prefix, pData, ulDataLen) == 0)
		return CKR_OK;

	return CKR_SIGNATURE_INVALID;
}

CK_RV
gkm_mock_unsupported_C_VerifyUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart, CK_ULONG ulPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_VerifyFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSignature,
                                    CK_ULONG pulSignatureLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_VerifyRecoverInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                                          CK_OBJECT_HANDLE hKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_VerifyRecover (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSignature,
                                      CK_ULONG pulSignatureLen, CK_BYTE_PTR pData, CK_ULONG_PTR pulDataLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DigestEncryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart,
                                            CK_ULONG ulPartLen, CK_BYTE_PTR pEncryptedPart,
                                            CK_ULONG_PTR ulEncryptedPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DecryptDigestUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedPart,
                                            CK_ULONG ulEncryptedPartLen, CK_BYTE_PTR pPart,
                                            CK_ULONG_PTR pulPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_SignEncryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart,
                                          CK_ULONG ulPartLen, CK_BYTE_PTR pEncryptedPart,
                                          CK_ULONG_PTR ulEncryptedPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DecryptVerifyUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedPart,
                                            CK_ULONG ulEncryptedPartLen, CK_BYTE_PTR pPart,
                                            CK_ULONG_PTR pulPartLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_GenerateKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                                    CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount,
                                    CK_OBJECT_HANDLE_PTR phKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_GenerateKeyPair (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                                        CK_ATTRIBUTE_PTR pPublicKeyTemplate, CK_ULONG ulPublicKeyAttributeCount,
                                        CK_ATTRIBUTE_PTR pPrivateKeyTemplate, CK_ULONG ulPrivateKeyAttributeCount,
                                        CK_OBJECT_HANDLE_PTR phPublicKey, CK_OBJECT_HANDLE_PTR phPrivateKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_WrapKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                                CK_OBJECT_HANDLE hWrappingKey, CK_OBJECT_HANDLE hKey,
                                CK_BYTE_PTR pWrappedKey, CK_ULONG_PTR pulWrappedKeyLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_UnwrapKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                                  CK_OBJECT_HANDLE pUnwrappingKey, CK_BYTE_PTR pWrappedKey,
                                  CK_ULONG pulWrappedKeyLen, CK_ATTRIBUTE_PTR pTemplate,
                                  CK_ULONG ulCount, CK_OBJECT_HANDLE_PTR phKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_DeriveKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                                  CK_OBJECT_HANDLE hBaseKey, CK_ATTRIBUTE_PTR pTemplate,
                                  CK_ULONG ulCount, CK_OBJECT_HANDLE_PTR phKey)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_SeedRandom (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSeed, CK_ULONG ulSeedLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_mock_unsupported_C_GenerateRandom (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pRandomData,
                           CK_ULONG ulRandomLen)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_FUNCTION_LIST functionList = {
	{ 2, 11 },	/* version */
	gkm_mock_C_Initialize,
	gkm_mock_C_Finalize,
	gkm_mock_C_GetInfo,
	gkm_mock_C_GetFunctionList,
	gkm_mock_C_GetSlotList,
	gkm_mock_C_GetSlotInfo,
	gkm_mock_C_GetTokenInfo,
	gkm_mock_C_GetMechanismList,
	gkm_mock_C_GetMechanismInfo,
	gkm_mock_C_InitToken,
	gkm_mock_C_InitPIN,
	gkm_mock_C_SetPIN,
	gkm_mock_C_OpenSession,
	gkm_mock_C_CloseSession,
	gkm_mock_C_CloseAllSessions,
	gkm_mock_C_GetSessionInfo,
	gkm_mock_unsupported_C_GetOperationState,
	gkm_mock_unsupported_C_SetOperationState,
	gkm_mock_C_Login,
	gkm_mock_C_Logout,
	gkm_mock_C_CreateObject,
	gkm_mock_unsupported_C_CopyObject,
	gkm_mock_C_DestroyObject,
	gkm_mock_unsupported_C_GetObjectSize,
	gkm_mock_C_GetAttributeValue,
	gkm_mock_C_SetAttributeValue,
	gkm_mock_C_FindObjectsInit,
	gkm_mock_C_FindObjects,
	gkm_mock_C_FindObjectsFinal,
	gkm_mock_C_EncryptInit,
	gkm_mock_C_Encrypt,
	gkm_mock_unsupported_C_EncryptUpdate,
	gkm_mock_unsupported_C_EncryptFinal,
	gkm_mock_C_DecryptInit,
	gkm_mock_C_Decrypt,
	gkm_mock_unsupported_C_DecryptUpdate,
	gkm_mock_unsupported_C_DecryptFinal,
	gkm_mock_unsupported_C_DigestInit,
	gkm_mock_unsupported_C_Digest,
	gkm_mock_unsupported_C_DigestUpdate,
	gkm_mock_unsupported_C_DigestKey,
	gkm_mock_unsupported_C_DigestFinal,
	gkm_mock_C_SignInit,
	gkm_mock_C_Sign,
	gkm_mock_unsupported_C_SignUpdate,
	gkm_mock_unsupported_C_SignFinal,
	gkm_mock_unsupported_C_SignRecoverInit,
	gkm_mock_unsupported_C_SignRecover,
	gkm_mock_C_VerifyInit,
	gkm_mock_C_Verify,
	gkm_mock_unsupported_C_VerifyUpdate,
	gkm_mock_unsupported_C_VerifyFinal,
	gkm_mock_unsupported_C_VerifyRecoverInit,
	gkm_mock_unsupported_C_VerifyRecover,
	gkm_mock_unsupported_C_DigestEncryptUpdate,
	gkm_mock_unsupported_C_DecryptDigestUpdate,
	gkm_mock_unsupported_C_SignEncryptUpdate,
	gkm_mock_unsupported_C_DecryptVerifyUpdate,
	gkm_mock_unsupported_C_GenerateKey,
	gkm_mock_unsupported_C_GenerateKeyPair,
	gkm_mock_unsupported_C_WrapKey,
	gkm_mock_unsupported_C_UnwrapKey,
	gkm_mock_unsupported_C_DeriveKey,
	gkm_mock_unsupported_C_SeedRandom,
	gkm_mock_unsupported_C_GenerateRandom,
	gkm_mock_C_GetFunctionStatus,
	gkm_mock_C_CancelFunction,
	gkm_mock_unsupported_C_WaitForSlotEvent
};
