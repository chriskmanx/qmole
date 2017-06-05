#include "config.h"

#include "gp11.h"
#include "gp11-test.h"

#include "pkcs11.h"

#include <glib.h>

#include <string.h>

/* 
 * This is *NOT* how you'd want to implement a PKCS#11 module. This 
 * fake module simply provides enough for GP11 library to test against.
 * It doesn't pass any tests, or behave as expected from a PKCS#11 module.
 */ 

static gboolean initialized = FALSE;
static gchar *the_pin = NULL;

static gboolean logged_in = FALSE;
static CK_USER_TYPE user_type = 0;

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
	
	/* For 'signing' with CKM_PREFIX */
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

/* 
 * This is not a generic test module, it works in concert with the 
 * unit-test-gp11-module.c
 */ 

static void
free_session (gpointer data)
{
	Session *sess = (Session*)data;
	if (sess)
		g_hash_table_destroy (sess->objects);
	g_free (sess);
}

static GP11Attributes*
lookup_object (Session *session, CK_OBJECT_HANDLE hObject)
{
	GP11Attributes *attrs;
	attrs = g_hash_table_lookup (the_objects, GUINT_TO_POINTER (hObject));
	if (!attrs)
		attrs = g_hash_table_lookup (session->objects, GUINT_TO_POINTER (hObject));
	return attrs;
}

static CK_RV
test_C_Initialize (CK_VOID_PTR pInitArgs)
{
	GP11Attributes *attrs;
	CK_C_INITIALIZE_ARGS_PTR args;
	CK_ULONG value;
	void *mutex;
	CK_RV rv;
	
	g_assert (initialized == FALSE && "Initialized same module twice, maybe module was not finalized, outstanding refs?");
	g_assert (pInitArgs != NULL && "Missing arguments");
	
	args = (CK_C_INITIALIZE_ARGS_PTR)pInitArgs;
	g_assert (args->CreateMutex != NULL && "Missing CreateMutex");
	g_assert (args->DestroyMutex != NULL && "Missing DestroyMutex");
	g_assert (args->LockMutex != NULL && "Missing LockMutex");
	g_assert (args->UnlockMutex != NULL && "Missing UnlockMutex");

	g_assert ((args->CreateMutex) (NULL) == CKR_ARGUMENTS_BAD && "CreateMutex succeeded wrong");
	g_assert ((args->DestroyMutex) (NULL) == CKR_MUTEX_BAD && "DestroyMutex succeeded wrong");
	g_assert ((args->LockMutex) (NULL) == CKR_MUTEX_BAD && "LockMutex succeeded wrong");
	g_assert ((args->UnlockMutex) (NULL) == CKR_MUTEX_BAD && "UnlockMutex succeeded wrong");

	/* Try to create an actual mutex */
	rv = (args->CreateMutex) (&mutex);
	g_assert (rv == CKR_OK && "CreateMutex g_assert_not_reacheded");
	g_assert (mutex != NULL && "CreateMutex created null mutex");
	
	/* Try and lock the mutex */
	rv = (args->LockMutex) (mutex);
	g_assert (rv == CKR_OK && "LockMutex g_assert_not_reacheded");

	/* Try and unlock the mutex */
	rv = (args->UnlockMutex) (mutex);
	g_assert (rv == CKR_OK && "UnlockMutex g_assert_not_reacheded");

	/* Try and destroy the mutex */
	rv = (args->DestroyMutex) (mutex);
	g_assert (rv == CKR_OK && "DestroyMutex g_assert_not_reacheded");
	
	/* Flags should allow OS locking and os threads */
	g_assert ((args->flags & CKF_OS_LOCKING_OK) == CKF_OS_LOCKING_OK && "Invalid CKF_OS_LOCKING_OK flag");
	g_assert ((args->flags & CKF_LIBRARY_CANT_CREATE_OS_THREADS) == 0 && "Invalid CKF_LIBRARY_CANT_CREATE_OS_THREADS flag");
	
	the_pin = g_strdup ("booo");
	the_sessions = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, free_session);
	the_objects = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, (GDestroyNotify)gp11_attributes_unref);
	
	/* Our token object */
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_DATA,
	                              CKA_LABEL, GP11_STRING, "TEST LABEL",
	                              GP11_INVALID);
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (2), attrs);
	
	/* Private capitalize key */
	value = CKM_CAPITALIZE;
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_PRIVATE_KEY,
	                              CKA_LABEL, GP11_STRING, "Private Capitalize Key",
	                              CKA_ALLOWED_MECHANISMS, sizeof (value), &value,
	                              CKA_DECRYPT, GP11_BOOLEAN, TRUE,
	                              CKA_PRIVATE, GP11_BOOLEAN, TRUE,
	                              CKA_WRAP, GP11_BOOLEAN, TRUE,
	                              CKA_UNWRAP, GP11_BOOLEAN, TRUE,
	                              CKA_DERIVE, GP11_BOOLEAN, TRUE,
	                              CKA_VALUE, GP11_STRING, "value",
	                              GP11_INVALID);
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PRIVATE_KEY_CAPITALIZE), attrs);

	/* Public capitalize key */
	value = CKM_CAPITALIZE;
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_PUBLIC_KEY,
	                              CKA_LABEL, GP11_STRING, "Public Capitalize Key",
	                              CKA_ALLOWED_MECHANISMS, sizeof (value), &value,
	                              CKA_ENCRYPT, GP11_BOOLEAN, TRUE,
	                              CKA_PRIVATE, GP11_BOOLEAN, FALSE,
	                              CKA_VALUE, GP11_STRING, "value",
	                              GP11_INVALID);
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PUBLIC_KEY_CAPITALIZE), attrs);

	/* Private prefix key */
	value = CKM_PREFIX;
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_PRIVATE_KEY,
	                              CKA_LABEL, GP11_STRING, "Private prefix key",
	                              CKA_ALLOWED_MECHANISMS, sizeof (value), &value,
	                              CKA_SIGN, GP11_BOOLEAN, TRUE,
	                              CKA_PRIVATE, GP11_BOOLEAN, TRUE,
	                              CKA_ALWAYS_AUTHENTICATE, GP11_BOOLEAN, TRUE,
	                              CKA_VALUE, GP11_STRING, "value",
	                              GP11_INVALID);
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PRIVATE_KEY_PREFIX), attrs);

	/* Private prefix key */
	value = CKM_PREFIX;
	attrs = gp11_attributes_newv (CKA_CLASS, GP11_ULONG, CKO_PUBLIC_KEY,
	                              CKA_LABEL, GP11_STRING, "Public prefix key",
	                              CKA_ALLOWED_MECHANISMS, sizeof (value), &value,
	                              CKA_VERIFY, GP11_BOOLEAN, TRUE,
	                              CKA_PRIVATE, GP11_BOOLEAN, FALSE,
	                              CKA_VALUE, GP11_STRING, "value",
	                              GP11_INVALID);
	g_hash_table_insert (the_objects, GUINT_TO_POINTER (PUBLIC_KEY_PREFIX), attrs);
	
	initialized = TRUE;
	return CKR_OK;
}

static CK_RV
test_C_Finalize (CK_VOID_PTR pReserved)
{
	g_assert (pReserved == NULL && "Invalid reserved pointer");
	g_assert (initialized == TRUE && "Finalize without being initialized");
	
	initialized = FALSE;
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
	
static CK_RV
test_C_GetInfo (CK_INFO_PTR pInfo)
{
	g_assert (pInfo != NULL && "Invalid pointer to GetInfo");
	memcpy (pInfo, &TEST_INFO, sizeof (*pInfo));
	return CKR_OK;
}

static CK_RV
test_C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	g_assert (list != NULL && "Invalid pointer passed to GetFunctionList");
	return C_GetFunctionList (list);
}

#define TEST_SLOT_ONE  52
#define TEST_SLOT_TWO  134

/* 
 * Two slots 
 *  ONE: token present
 *  TWO: token not present
 */

static CK_RV
test_C_GetSlotList (CK_BBOOL tokenPresent, CK_SLOT_ID_PTR pSlotList, CK_ULONG_PTR pulCount)
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
	pSlotList[0] = TEST_SLOT_ONE;
	if (!tokenPresent)
		pSlotList[1] = TEST_SLOT_TWO;

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

static CK_RV
test_C_GetSlotInfo (CK_SLOT_ID slotID, CK_SLOT_INFO_PTR pInfo)
{
	g_assert (pInfo != NULL && "Invalid pInfo");
	
	if (slotID == TEST_SLOT_ONE) {
		memcpy (pInfo, &TEST_INFO_ONE, sizeof (*pInfo));
		return CKR_OK;
	} else if (slotID == TEST_SLOT_TWO) {
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

static CK_RV
test_C_GetTokenInfo (CK_SLOT_ID slotID, CK_TOKEN_INFO_PTR pInfo)
{
	g_assert (pInfo != NULL && "Invalid pInfo");
	
	if (slotID == TEST_SLOT_ONE) {
		memcpy (pInfo, &TEST_TOKEN_ONE, sizeof (*pInfo));
		return CKR_OK;
	} else if (slotID == TEST_SLOT_TWO) {
		return CKR_TOKEN_NOT_PRESENT;
	} else {
		g_assert_not_reached (); /* "Invalid slot id" */
		return CKR_SLOT_ID_INVALID;
	}
}

/* 
 * TWO mechanisms: 
 *  CKM_CAPITALIZE 
 *  CKM_PREFIX
 */

static CK_RV
test_C_GetMechanismList (CK_SLOT_ID slotID, CK_MECHANISM_TYPE_PTR pMechanismList,
                         CK_ULONG_PTR pulCount)
{
	g_assert (slotID == TEST_SLOT_ONE && "Invalid slotID");
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

	pMechanismList[0] = CKM_CAPITALIZE;
	pMechanismList[1] = CKM_PREFIX;
	return CKR_OK;
}

static const CK_MECHANISM_INFO TEST_MECH_CAPITALIZE = {
	512, 4096, 0
};

static const CK_MECHANISM_INFO TEST_MECH_PREFIX = {
	2048, 2048, 0
};

static CK_RV
test_C_GetMechanismInfo (CK_SLOT_ID slotID, CK_MECHANISM_TYPE type, 
                         CK_MECHANISM_INFO_PTR pInfo)
{
	g_assert (slotID == TEST_SLOT_ONE && "Invalid slotID");
	g_assert (pInfo != NULL && "Invalid pInfo");

	if (type == CKM_CAPITALIZE) {
		memcpy (pInfo, &TEST_MECH_CAPITALIZE, sizeof (*pInfo));
		return CKR_OK;
	} else if (type == CKM_PREFIX) {
		memcpy (pInfo, &TEST_MECH_PREFIX, sizeof (*pInfo));
		return CKR_OK;
	} else {
		g_assert_not_reached (); /* "Invalid type" */
		return CKR_MECHANISM_INVALID;
	}
}

static CK_RV
test_C_InitToken (CK_SLOT_ID slotID, CK_UTF8CHAR_PTR pPin, CK_ULONG ulPinLen, 
                  CK_UTF8CHAR_PTR pLabel)
{
	g_assert (slotID == TEST_SLOT_ONE && "Invalid slotID");
	g_assert (pPin != NULL && "Invalid pPin");
	g_assert (strlen ("TEST PIN") && "Invalid ulPinLen");
	g_assert (strncmp ((gchar*)pPin, "TEST PIN", ulPinLen) == 0 && "Invalid pPin string");
	g_assert (pLabel != NULL && "Invalid pLabel");
	g_assert (strcmp ((gchar*)pPin, "TEST LABEL") == 0 && "Invalid pLabel string");

	g_free (the_pin);
	the_pin = g_strndup ((gchar*)pPin, ulPinLen);
	return CKR_OK;
}

static CK_RV
test_C_WaitForSlotEvent (CK_FLAGS flags, CK_SLOT_ID_PTR pSlot, CK_VOID_PTR pReserved)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_OpenSession (CK_SLOT_ID slotID, CK_FLAGS flags, CK_VOID_PTR pApplication,
                    CK_NOTIFY Notify, CK_SESSION_HANDLE_PTR phSession)
{
	Session *sess;
	
	g_assert (slotID == TEST_SLOT_ONE && "Invalid slotID");
	g_assert (pApplication == NULL && "pApplication should be null");
	g_assert (Notify == NULL && "Notify should be null");
	g_assert (phSession != NULL && "Invalid phSession");
	g_assert ((flags & CKF_SERIAL_SESSION) == CKF_SERIAL_SESSION);

	sess = g_new0 (Session, 1);
	sess->handle = ++unique_identifier;
	sess->info.flags = flags;
	sess->info.slotID = slotID;
	sess->info.state = 0;
	sess->info.ulDeviceError = 1414;
	sess->objects = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, (GDestroyNotify)gp11_attributes_unref);
	*phSession = sess->handle;

	g_hash_table_replace (the_sessions, GUINT_TO_POINTER (sess->handle), sess);
	return CKR_OK;
}

static CK_RV
test_C_CloseSession (CK_SESSION_HANDLE hSession)
{
	Session *session;
	
	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;
	
	g_hash_table_remove (the_sessions, GUINT_TO_POINTER (hSession));
	return CKR_OK;
}

static CK_RV
test_C_CloseAllSessions (CK_SLOT_ID slotID)
{
	g_assert (slotID == TEST_SLOT_ONE && "Invalid slotID");	

	g_hash_table_remove_all (the_sessions);
	return CKR_OK;
}

static CK_RV
test_C_GetFunctionStatus (CK_SESSION_HANDLE hSession)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_PARALLEL;
}

static CK_RV
test_C_CancelFunction (CK_SESSION_HANDLE hSession)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_PARALLEL;
}

static CK_RV
test_C_GetSessionInfo (CK_SESSION_HANDLE hSession, CK_SESSION_INFO_PTR pInfo)
{
	Session *session;

	g_assert (pInfo != NULL && "Invalid pInfo");

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	memcpy (pInfo, &session->info, sizeof (*pInfo));
	return CKR_OK;
}

static CK_RV
test_C_InitPIN (CK_SESSION_HANDLE hSession, CK_UTF8CHAR_PTR pPin, 
                CK_ULONG ulPinLen)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	g_free (the_pin);
	the_pin = g_strndup ((gchar*)pPin, ulPinLen);
	return CKR_OK;
}

static CK_RV
test_C_SetPIN (CK_SESSION_HANDLE hSession, CK_UTF8CHAR_PTR pOldPin,
               CK_ULONG ulOldLen, CK_UTF8CHAR_PTR pNewPin, CK_ULONG ulNewLen)
{
	Session *session;
	gchar *old;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	old = g_strndup ((gchar*)pOldPin, ulOldLen);
	if (!g_str_equal (old, the_pin))
		return CKR_PIN_INCORRECT;

	g_free (the_pin);
	the_pin = g_strndup ((gchar*)pNewPin, ulNewLen);
	return CKR_OK;
}

static CK_RV
test_C_GetOperationState (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pOperationState,
                        CK_ULONG_PTR pulOperationStateLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_SetOperationState (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pOperationState,
                        CK_ULONG ulOperationStateLen, CK_OBJECT_HANDLE hEncryptionKey,
                        CK_OBJECT_HANDLE hAuthenticationKey)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_Login (CK_SESSION_HANDLE hSession, CK_USER_TYPE userType,
              CK_UTF8CHAR_PTR pPin, CK_ULONG pPinLen)
{
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;
	
	if (!pPin)
		return CKR_PIN_INCORRECT;
	
	g_assert (pPinLen == strlen (the_pin) && "Wrong PIN length");
	g_assert (strncmp ((gchar*)pPin, the_pin, pPinLen) == 0 && "Wrong PIN");
	g_assert ((userType == CKU_SO || userType == CKU_USER || userType == CKU_CONTEXT_SPECIFIC) && "Bad user type");
	g_assert (logged_in == FALSE && "Already logged in");
	
	if (userType == CKU_CONTEXT_SPECIFIC) {
		session->want_context_login = CK_FALSE;
	} else {
		logged_in = TRUE;
		user_type = userType;
	}
	
	return CKR_OK;
}

static CK_RV
test_C_Logout (CK_SESSION_HANDLE hSession)
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

static CK_RV
test_C_CreateObject (CK_SESSION_HANDLE hSession, CK_ATTRIBUTE_PTR pTemplate,
                     CK_ULONG ulCount, CK_OBJECT_HANDLE_PTR phObject)
{
	GP11Attributes *attrs;
	Session *session;
	gboolean token, priv;
	CK_ULONG i;

	g_assert (phObject != NULL);
	
	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;
	
	attrs = gp11_attributes_new ();
	for (i = 0; i < ulCount; ++i) 
		gp11_attributes_add_data (attrs, pTemplate[i].type, pTemplate[i].pValue, pTemplate[i].ulValueLen);
	
	if (gp11_attributes_find_boolean (attrs, CKA_PRIVATE, &priv) && priv) {
		if (!logged_in) {
			gp11_attributes_unref (attrs);
			return CKR_USER_NOT_LOGGED_IN;
		}
	}
	
	*phObject = ++unique_identifier;
	if (gp11_attributes_find_boolean (attrs, CKA_TOKEN, &token) && token)
		g_hash_table_insert (the_objects, GUINT_TO_POINTER (*phObject), attrs);
	else
		g_hash_table_insert (session->objects, GUINT_TO_POINTER (*phObject), attrs);
	
	return CKR_OK;
}

static CK_RV
test_C_CopyObject (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                 CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount,
                 CK_OBJECT_HANDLE_PTR phNewObject)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}


static CK_RV
test_C_DestroyObject (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject)
{
	GP11Attributes *attrs;
	Session *session;
	gboolean priv;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	attrs = lookup_object (session, hObject);
	if (!attrs) {
		g_assert_not_reached (); /* "no such object found" */
		return CKR_OBJECT_HANDLE_INVALID;
	}
	
	if (gp11_attributes_find_boolean (attrs, CKA_PRIVATE, &priv) && priv) {
		if (!logged_in)
			return CKR_USER_NOT_LOGGED_IN;
	}

	return CKR_OK;
}

static CK_RV
test_C_GetObjectSize (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                    CK_ULONG_PTR pulSize)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_GetAttributeValue (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                          CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount)
{
	CK_ATTRIBUTE_PTR result;
	CK_RV ret = CKR_OK;
	GP11Attributes *attrs;
	GP11Attribute *attr;
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
		attr = gp11_attributes_find (attrs, result->type);
		if (!attr) {
			result->ulValueLen = (CK_ULONG)-1;
			ret = CKR_ATTRIBUTE_TYPE_INVALID;
			continue;
		}
		
		if (!result->pValue) {
			result->ulValueLen = attr->length;
			continue;
		}
		
		if (result->ulValueLen >= attr->length) {
			memcpy (result->pValue, attr->value, attr->length);
			continue;
		}
		
		result->ulValueLen = (CK_ULONG)-1;
		ret = CKR_BUFFER_TOO_SMALL;
	}
	
	return ret;
}

static CK_RV
test_C_SetAttributeValue (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hObject,
                        CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount)
{
	Session *session;
	CK_ATTRIBUTE_PTR set;
	GP11Attributes *attrs;
	GP11Attribute *attr;
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
		set = pTemplate + i;
		attr = gp11_attributes_find (attrs, set->type);
		if (!attr) {
			gp11_attributes_add_data (attrs, set->type, set->pValue, set->ulValueLen);
		} else {
			gp11_attribute_clear (attr);
			gp11_attribute_init (attr, set->type, set->pValue, set->ulValueLen);
		}
	}
	
	return CKR_OK;
}

static CK_RV
test_C_FindObjectsInit (CK_SESSION_HANDLE hSession, CK_ATTRIBUTE_PTR pTemplate,
                        CK_ULONG ulCount)
{
	GHashTableIter iter;
	GP11Attributes *attrs;
	GP11Attribute *attr;
	CK_ATTRIBUTE_PTR match;
	Session *session;
	gpointer key, value;
	gboolean matched = TRUE;
	CK_ULONG i;
	
	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	/* Starting an operation, cancels any previous one */
	if (session->operation != 0) 
		session->operation = 0;
	
	session->operation = OP_FIND;
	
	/* Token objects */
	g_hash_table_iter_init (&iter, the_objects);
	while (g_hash_table_iter_next (&iter, &key, &value)) {
		attrs = (GP11Attributes*)value;
		matched = TRUE;
		for (i = 0; i < ulCount; ++i) {
			match = pTemplate + i;
			attr = gp11_attributes_find (attrs, match->type);
			if (!attr) {
				matched = FALSE;
				break;
			}
			
			if (attr->length != match->ulValueLen || 
			    memcmp (attr->value, match->pValue, attr->length) != 0) {
				matched = FALSE;
				break;
			}
		}
		
		if (matched)
			session->matches = g_list_prepend (session->matches, key);
	}

	/* session objects */
	g_hash_table_iter_init (&iter, session->objects);
	while (g_hash_table_iter_next (&iter, &key, &value)) {
		attrs = (GP11Attributes*)value;
		matched = TRUE;
		for (i = 0; i < ulCount; ++i) {
			match = pTemplate + i;
			attr = gp11_attributes_find (attrs, match->type);
			if (!attr) {
				matched = FALSE;
				break;
			}
			
			if (attr->length != match->ulValueLen || 
			    memcmp (attr->value, match->pValue, attr->length) != 0) {
				matched = FALSE;
				break;
			}
		}
		
		if (matched)
			session->matches = g_list_prepend (session->matches, key);
	}

	return CKR_OK;
}

static CK_RV
test_C_FindObjects (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE_PTR phObject,
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

static CK_RV
test_C_FindObjectsFinal (CK_SESSION_HANDLE hSession)
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

static CK_RV
test_C_EncryptInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
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
	g_assert (pMechanism->mechanism == CKM_CAPITALIZE);
	g_assert (hKey == PUBLIC_KEY_CAPITALIZE);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_ENCRYPT;
	session->crypto_mechanism = CKM_CAPITALIZE;
	session->crypto_key = hKey;
	return CKR_OK;
}

static CK_RV
test_C_Encrypt (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
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
	g_assert (session->crypto_mechanism == CKM_CAPITALIZE);
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

static CK_RV
test_C_EncryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart,
                    CK_ULONG ulPartLen, CK_BYTE_PTR pEncryptedPart,
                    CK_ULONG_PTR pulEncryptedPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_EncryptFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pLastEncryptedPart,
                   CK_ULONG_PTR pulLastEncryptedPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DecryptInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
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
	g_assert (pMechanism->mechanism == CKM_CAPITALIZE);
	g_assert (hKey == PRIVATE_KEY_CAPITALIZE);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_DECRYPT;
	session->crypto_mechanism = CKM_CAPITALIZE;
	session->crypto_key = hKey;
	return CKR_OK;
}

static CK_RV
test_C_Decrypt (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedData,
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
	g_assert (session->crypto_mechanism == CKM_CAPITALIZE);
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

static CK_RV
test_C_DecryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedPart,
                    CK_ULONG ulEncryptedPartLen, CK_BYTE_PTR pPart, CK_ULONG_PTR pulPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DecryptFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pLastPart,
                   CK_ULONG_PTR pulLastPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DigestInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_Digest (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
             CK_BYTE_PTR pDigest, CK_ULONG_PTR pulDigestLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DigestUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart, CK_ULONG ulPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DigestKey (CK_SESSION_HANDLE hSession, CK_OBJECT_HANDLE hKey)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DigestFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pDigest,
                  CK_ULONG_PTR pulDigestLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_SignInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
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
	g_assert (pMechanism->mechanism == CKM_PREFIX);
	g_assert (hKey == PRIVATE_KEY_PREFIX);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_SIGN;
	session->crypto_mechanism = CKM_PREFIX;
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

static CK_RV
test_C_Sign (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
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
	g_assert (session->crypto_mechanism == CKM_PREFIX);
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

static CK_RV
test_C_SignUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart, CK_ULONG ulPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_SignFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSignature,
                CK_ULONG_PTR pulSignatureLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_SignRecoverInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                      CK_OBJECT_HANDLE hKey)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_SignRecover (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen, 
                  CK_BYTE_PTR pSignature, CK_ULONG_PTR pulSignatureLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_VerifyInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
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
	g_assert (pMechanism->mechanism == CKM_PREFIX);
	g_assert (hKey == PUBLIC_KEY_PREFIX);

	session->operation = OP_CRYPTO;
	session->crypto_method = CKA_VERIFY;
	session->crypto_mechanism = CKM_PREFIX;
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

static CK_RV
test_C_Verify (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pData, CK_ULONG ulDataLen,
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
	g_assert (session->crypto_mechanism == CKM_PREFIX);
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

static CK_RV
test_C_VerifyUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart, CK_ULONG ulPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_VerifyFinal (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSignature,
                  CK_ULONG pulSignatureLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_VerifyRecoverInit (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                        CK_OBJECT_HANDLE hKey)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_VerifyRecover (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSignature,
                    CK_ULONG pulSignatureLen, CK_BYTE_PTR pData, CK_ULONG_PTR pulDataLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DigestEncryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart,
                          CK_ULONG ulPartLen, CK_BYTE_PTR pEncryptedPart,
                          CK_ULONG_PTR ulEncryptedPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DecryptDigestUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedPart,
                          CK_ULONG ulEncryptedPartLen, CK_BYTE_PTR pPart, 
                          CK_ULONG_PTR pulPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_SignEncryptUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pPart,
                        CK_ULONG ulPartLen, CK_BYTE_PTR pEncryptedPart,
                        CK_ULONG_PTR ulEncryptedPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_DecryptVerifyUpdate (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pEncryptedPart,
                          CK_ULONG ulEncryptedPartLen, CK_BYTE_PTR pPart, 
                          CK_ULONG_PTR pulPartLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_GenerateKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                  CK_ATTRIBUTE_PTR pTemplate, CK_ULONG ulCount, 
                  CK_OBJECT_HANDLE_PTR phKey)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_GenerateKeyPair (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                        CK_ATTRIBUTE_PTR pPublicKeyTemplate, CK_ULONG ulPublicKeyAttributeCount,
                        CK_ATTRIBUTE_PTR pPrivateKeyTemplate, CK_ULONG ulPrivateKeyAttributeCount,
                        CK_OBJECT_HANDLE_PTR phPublicKey, CK_OBJECT_HANDLE_PTR phPrivateKey)
{
	GP11Attributes *attrs;
	Session *session;
	gboolean token;
	CK_ULONG i;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	g_assert (session != NULL && "No such session found");
	if (!session)
		return CKR_SESSION_HANDLE_INVALID;

	g_assert (pMechanism);
	g_assert (pPublicKeyTemplate);
	g_assert (ulPublicKeyAttributeCount);
	g_assert (pPrivateKeyTemplate);
	g_assert (phPublicKey);
	g_assert (phPrivateKey);

	if (pMechanism->mechanism != CKM_GENERATE)
		return CKR_MECHANISM_INVALID;

	if (!pMechanism->pParameter || pMechanism->ulParameterLen != 9 ||
	    memcmp (pMechanism->pParameter, "generate", 9) != 0) {
		g_assert_not_reached ();
		return CKR_MECHANISM_PARAM_INVALID;
	}

	attrs = gp11_attributes_new ();
	gp11_attributes_add_string (attrs, CKA_VALUE, "generated");
	for (i = 0; i < ulPublicKeyAttributeCount; ++i)
		gp11_attributes_add_data (attrs, pPublicKeyTemplate[i].type,
		                          pPublicKeyTemplate[i].pValue,
		                          pPublicKeyTemplate[i].ulValueLen);
	*phPublicKey = ++unique_identifier;
	if (gp11_attributes_find_boolean (attrs, CKA_TOKEN, &token) && token)
		g_hash_table_insert (the_objects, GUINT_TO_POINTER (*phPublicKey), attrs);
	else
		g_hash_table_insert (session->objects, GUINT_TO_POINTER (*phPublicKey), attrs);

	attrs = gp11_attributes_new ();
	gp11_attributes_add_string (attrs, CKA_VALUE, "generated");
	for (i = 0; i < ulPrivateKeyAttributeCount; ++i)
		gp11_attributes_add_data (attrs, pPrivateKeyTemplate[i].type,
		                          pPrivateKeyTemplate[i].pValue,
		                          pPrivateKeyTemplate[i].ulValueLen);
	*phPrivateKey = ++unique_identifier;
	if (gp11_attributes_find_boolean (attrs, CKA_TOKEN, &token) && token)
		g_hash_table_insert (the_objects, GUINT_TO_POINTER (*phPrivateKey), attrs);
	else
		g_hash_table_insert (session->objects, GUINT_TO_POINTER (*phPrivateKey), attrs);
	return CKR_OK;
}

static CK_RV
test_C_WrapKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                CK_OBJECT_HANDLE hWrappingKey, CK_OBJECT_HANDLE hKey,
                CK_BYTE_PTR pWrappedKey, CK_ULONG_PTR pulWrappedKeyLen)
{
	GP11Attributes *attrs;
	GP11Attribute *attr;
	Session *session;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	if (!session) {
		g_assert_not_reached ();
		return CKR_SESSION_HANDLE_INVALID;
	}

	g_assert (pMechanism);
	g_assert (hWrappingKey);
	g_assert (hKey);
	g_assert (pulWrappedKeyLen);

	attrs = lookup_object (session, hWrappingKey);
	if (!attrs) {
		g_assert_not_reached ();
		return CKR_WRAPPING_KEY_HANDLE_INVALID;
	}

	attrs = lookup_object (session, hKey);
	if (!attrs) {
		g_assert_not_reached ();
		return CKR_WRAPPED_KEY_INVALID;
	}

	if (pMechanism->mechanism != CKM_WRAP)
		return CKR_MECHANISM_INVALID;

	if (pMechanism->pParameter) {
		if (pMechanism->ulParameterLen != 4 ||
		    memcmp (pMechanism->pParameter, "wrap", 4) != 0) {
			g_assert_not_reached ();
			return CKR_MECHANISM_PARAM_INVALID;
		}
	}

	attr = gp11_attributes_find (attrs, CKA_VALUE);
	if (attr == NULL)
		return CKR_WRAPPED_KEY_INVALID;

	if (!pWrappedKey) {
		*pulWrappedKeyLen = attr->length;
		return CKR_OK;
	}

	if (*pulWrappedKeyLen < attr->length) {
		*pulWrappedKeyLen = attr->length;
		return CKR_BUFFER_TOO_SMALL;
	}

	memcpy (pWrappedKey, attr->value, attr->length);
	*pulWrappedKeyLen = attr->length;

	return CKR_OK;
}

static CK_RV
test_C_UnwrapKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                CK_OBJECT_HANDLE pUnwrappingKey, CK_BYTE_PTR pWrappedKey,
                CK_ULONG pulWrappedKeyLen, CK_ATTRIBUTE_PTR pTemplate,
                CK_ULONG ulCount, CK_OBJECT_HANDLE_PTR phKey)
{
	GP11Attributes *attrs;
	Session *session;
	gboolean token;
	CK_ULONG i;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	if (!session) {
		g_assert_not_reached ();
		return CKR_SESSION_HANDLE_INVALID;
	}

	g_assert (pMechanism);
	g_assert (pUnwrappingKey);
	g_assert (pWrappedKey);
	g_assert (pulWrappedKeyLen);
	g_assert (phKey);
	g_assert (ulCount);
	g_assert (pTemplate);
	g_assert (phKey);

	attrs = lookup_object (session, pUnwrappingKey);
	if (!attrs) {
		g_assert_not_reached ();
		return CKR_WRAPPING_KEY_HANDLE_INVALID;
	}

	if (pMechanism->mechanism != CKM_WRAP)
		return CKR_MECHANISM_INVALID;

	if (pMechanism->pParameter) {
		if (pMechanism->ulParameterLen != 4 ||
		    memcmp (pMechanism->pParameter, "wrap", 4) != 0) {
			g_assert_not_reached ();
			return CKR_MECHANISM_PARAM_INVALID;
		}
	}

	attrs = gp11_attributes_new ();
	gp11_attributes_add_data (attrs, CKA_VALUE, pWrappedKey, pulWrappedKeyLen);
	for (i = 0; i < ulCount; ++i)
		gp11_attributes_add_data (attrs, pTemplate[i].type,
		                          pTemplate[i].pValue,
		                          pTemplate[i].ulValueLen);
	*phKey = ++unique_identifier;
	if (gp11_attributes_find_boolean (attrs, CKA_TOKEN, &token) && token)
		g_hash_table_insert (the_objects, GUINT_TO_POINTER (*phKey), attrs);
	else
		g_hash_table_insert (session->objects, GUINT_TO_POINTER (*phKey), attrs);

	return CKR_OK;
}

static CK_RV
test_C_DeriveKey (CK_SESSION_HANDLE hSession, CK_MECHANISM_PTR pMechanism,
                CK_OBJECT_HANDLE hBaseKey, CK_ATTRIBUTE_PTR pTemplate,
                CK_ULONG ulCount, CK_OBJECT_HANDLE_PTR phKey)
{
	GP11Attributes *attrs, *copy;
	Session *session;
	gboolean token;
	CK_ULONG i;

	session = g_hash_table_lookup (the_sessions, GUINT_TO_POINTER (hSession));
	if (!session) {
		g_assert_not_reached ();
		return CKR_SESSION_HANDLE_INVALID;
	}

	g_assert (pMechanism);
	g_assert (ulCount);
	g_assert (pTemplate);
	g_assert (phKey);

	attrs = lookup_object (session, hBaseKey);
	if (!attrs) {
		g_assert_not_reached ();
		return CKR_WRAPPING_KEY_HANDLE_INVALID;
	}

	if (pMechanism->mechanism != CKM_DERIVE)
		return CKR_MECHANISM_INVALID;

	if (pMechanism->pParameter) {
		if (pMechanism->ulParameterLen != 6 ||
		    memcmp (pMechanism->pParameter, "derive", 6) != 0) {
			g_assert_not_reached ();
			return CKR_MECHANISM_PARAM_INVALID;
		}
	}

	copy = gp11_attributes_new ();
	gp11_attributes_add_string (copy, CKA_VALUE, "derived");
	for (i = 0; i < ulCount; ++i)
		gp11_attributes_add_data (copy, pTemplate[i].type,
		                          pTemplate[i].pValue,
		                          pTemplate[i].ulValueLen);
	for (i = 0; i < gp11_attributes_count (attrs); ++i)
		gp11_attributes_add (copy, gp11_attributes_at (attrs, i));
	*phKey = ++unique_identifier;
	if (gp11_attributes_find_boolean (copy, CKA_TOKEN, &token) && token)
		g_hash_table_insert (the_objects, GUINT_TO_POINTER (*phKey), copy);
	else
		g_hash_table_insert (session->objects, GUINT_TO_POINTER (*phKey), copy);

	return CKR_OK;
}

static CK_RV
test_C_SeedRandom (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pSeed, CK_ULONG ulSeedLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
test_C_GenerateRandom (CK_SESSION_HANDLE hSession, CK_BYTE_PTR pRandomData,
                      CK_ULONG ulRandomLen)
{
	g_assert_not_reached (); /* Not yet used by library */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_FUNCTION_LIST functionList = {
	{ 2, 11 },	/* version */
	test_C_Initialize,
	test_C_Finalize,
	test_C_GetInfo,
	test_C_GetFunctionList,
	test_C_GetSlotList,
	test_C_GetSlotInfo,
	test_C_GetTokenInfo,
	test_C_GetMechanismList,
	test_C_GetMechanismInfo,
	test_C_InitToken,
	test_C_InitPIN,
	test_C_SetPIN,
	test_C_OpenSession,
	test_C_CloseSession,
	test_C_CloseAllSessions,
	test_C_GetSessionInfo,
	test_C_GetOperationState,
	test_C_SetOperationState,
	test_C_Login,
	test_C_Logout,
	test_C_CreateObject,
	test_C_CopyObject,
	test_C_DestroyObject,
	test_C_GetObjectSize,
	test_C_GetAttributeValue,
	test_C_SetAttributeValue,
	test_C_FindObjectsInit,
	test_C_FindObjects,
	test_C_FindObjectsFinal,
	test_C_EncryptInit,
	test_C_Encrypt,
	test_C_EncryptUpdate,
	test_C_EncryptFinal,
	test_C_DecryptInit,
	test_C_Decrypt,
	test_C_DecryptUpdate,
	test_C_DecryptFinal,
	test_C_DigestInit,
	test_C_Digest,
	test_C_DigestUpdate,
	test_C_DigestKey,
	test_C_DigestFinal,
	test_C_SignInit,
	test_C_Sign,
	test_C_SignUpdate,
	test_C_SignFinal,
	test_C_SignRecoverInit,
	test_C_SignRecover,
	test_C_VerifyInit,
	test_C_Verify,
	test_C_VerifyUpdate,
	test_C_VerifyFinal,
	test_C_VerifyRecoverInit,
	test_C_VerifyRecover,
	test_C_DigestEncryptUpdate,
	test_C_DecryptDigestUpdate,
	test_C_SignEncryptUpdate,
	test_C_DecryptVerifyUpdate,
	test_C_GenerateKey,
	test_C_GenerateKeyPair,
	test_C_WrapKey,
	test_C_UnwrapKey,
	test_C_DeriveKey,
	test_C_SeedRandom,
	test_C_GenerateRandom,
	test_C_GetFunctionStatus,
	test_C_CancelFunction,
	test_C_WaitForSlotEvent
};

CK_RV
C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	if (!list)
		return CKR_ARGUMENTS_BAD;

	*list = &functionList;
	return CKR_OK;
}
