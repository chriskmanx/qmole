/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#include "test-suite.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-mock.h"
#include "gkm/gkm-test.h"

#include "wrap-layer/gkm-wrap-layer.h"

#include "ui/gku-prompt.h"

static guint secret_identifier = 8800;

static CK_RV
mock_secret_C_Initialize (CK_VOID_PTR pInitArgs)
{
	GArray *attrs;
	CK_RV rv;

	rv = gkm_mock_C_Initialize (pInitArgs);
	if (rv != CKR_OK)
		return rv;

	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_G_COLLECTION);
	gkm_template_set_boolean (attrs, CKA_G_LOGIN_COLLECTION, CK_TRUE);
	gkm_template_set_string (attrs, CKA_ID, "login");
	gkm_template_set_string (attrs, CKA_LABEL, "Login Keyring");
	gkm_template_set_boolean (attrs, CKA_TRUSTED, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_G_LOCKED, CK_FALSE);
	gkm_template_set_boolean (attrs, CKA_TOKEN, CK_TRUE);
	gkm_mock_module_take_object (attrs);

	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_G_COLLECTION);
	gkm_template_set_boolean (attrs, CKA_G_LOGIN_COLLECTION, CK_FALSE);
	gkm_template_set_string (attrs, CKA_ID, "other");
	gkm_template_set_string (attrs, CKA_LABEL, "Other Keyring");
	gkm_template_set_boolean (attrs, CKA_TRUSTED, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_G_LOCKED, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_TOKEN, CK_TRUE);
	gkm_mock_module_take_object (attrs);

	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_string (attrs, CKA_G_COLLECTION, "login");
	gkm_template_set_string (attrs, CKA_ID, "23");
	gkm_template_set_string (attrs, CKA_LABEL, "Unlock password for: Mock");
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_SECRET_KEY);
	gkm_template_set_value (attrs, CKA_G_FIELDS, "one\0" "1\0" "two\0" "2\0", 12);
	gkm_template_set_string (attrs, CKA_VALUE, "mock");
	gkm_template_set_boolean (attrs, CKA_TOKEN, CK_TRUE);
	gkm_template_set_boolean (attrs, CKA_G_LOCKED, CK_FALSE);
	gkm_mock_module_take_object (attrs);

	return CKR_OK;
}

typedef struct {
	GArray *template;
	GArray *objects;
} FieldSearch;

static gboolean
match_fields (gconstpointer fields, gsize n_fields, gconstpointer all, gsize n_all)
{
	const guchar *field;
	gsize n_field;
	const guchar *ptr;
	const guchar *last;

	g_assert (all);
	g_assert (fields);

	ptr = fields;
	last = ptr + n_fields;

	g_assert (ptr || last == ptr);
	while (ptr && ptr != last) {
		g_assert (ptr < last);

		field = ptr;
		ptr = memchr (ptr, 0, last - ptr);
		g_assert (ptr);
		++ptr;

		ptr = memchr (ptr, 0, last - ptr);
		g_assert (ptr);
		++ptr;

		n_field = ptr - field;
		if (!memmem (all, n_all, field, n_field))
			return FALSE;
	}

	return TRUE;
}

static gboolean
enumerate_field_search (CK_OBJECT_HANDLE handle, GArray *attrs, gpointer user_data)
{
	FieldSearch *ctx = user_data;
	CK_ATTRIBUTE_PTR tattr;
	CK_ATTRIBUTE_PTR oattr;

	tattr = gkm_template_find (ctx->template, CKA_G_FIELDS);
	g_assert (tattr);

	oattr = gkm_template_find (attrs, CKA_G_FIELDS);
	if (!oattr)
		return TRUE; /* Continue */
	if (!match_fields (tattr->pValue, tattr->ulValueLen, oattr->pValue, oattr->ulValueLen))
		return TRUE; /* Continue */

	tattr = gkm_template_find (ctx->template, CKA_G_COLLECTION);
	if (tattr) {
		oattr = gkm_template_find (attrs, CKA_G_COLLECTION);
		if (!oattr || oattr->ulValueLen != tattr->ulValueLen)
			return TRUE; /* Continue */
		if (memcmp (oattr->pValue, tattr->pValue, oattr->ulValueLen) != 0)
			return TRUE; /* Continue */
	}

	/* Add it to the set */
	g_array_append_val (ctx->objects, handle);
	return TRUE; /* Continue */
}

static CK_RV
mock_secret_C_CreateObject (CK_SESSION_HANDLE hSession, CK_ATTRIBUTE_PTR pTemplate,
                            CK_ULONG ulCount, CK_OBJECT_HANDLE_PTR phObject)
{
	GArray *template = NULL;
	CK_OBJECT_CLASS klass;
	CK_ATTRIBUTE_PTR attr;
	gchar *text;
	CK_RV rv;

	g_return_val_if_fail (phObject, CKR_ARGUMENTS_BAD);

	if (!gkm_attributes_find_ulong (pTemplate, ulCount, CKA_CLASS, &klass))
		klass = (CK_ULONG)-1;

	/* Is it a search object? */
	if (klass == CKO_G_SEARCH) {

		FieldSearch ctx;
		ctx.template = template = gkm_template_new (pTemplate, ulCount);
		ctx.objects = g_array_new (FALSE, FALSE, sizeof (CK_OBJECT_HANDLE));

		/* Find all the fields */
		gkm_mock_module_enumerate_objects (hSession, enumerate_field_search, &ctx);

		gkm_template_set_value (template, CKA_G_MATCHED, ctx.objects->data,
		                        ctx.objects->len * sizeof (CK_OBJECT_HANDLE));
		g_array_free (ctx.objects, TRUE);

	} else if (klass == CKO_SECRET_KEY) {

		/* If it's a secret key and id set, just overwrite */
		attr = gkm_attributes_find (pTemplate, ulCount, CKA_ID);
		if (attr) {
			CK_ATTRIBUTE attrs[2];

			memcpy (attrs, attr, sizeof (CK_ATTRIBUTE));
			attrs[1].type = CKA_CLASS;
			attrs[1].ulValueLen = sizeof (klass);
			attrs[1].pValue = &klass;

			*phObject = gkm_mock_module_find_object (hSession, attrs, 2);
			g_return_val_if_fail (*phObject, CKR_TEMPLATE_INCONSISTENT);
			return gkm_mock_C_SetAttributeValue (hSession, *phObject, pTemplate, ulCount);
		}

		/* Otherwise add a unique identifier */
		template = gkm_template_new (pTemplate, ulCount);
		text = g_strdup_printf ("%d", ++secret_identifier);
		gkm_template_set_string (template, CKA_ID, text);
		g_free (text);
	}

	if (template) {
		pTemplate = (CK_ATTRIBUTE_PTR)template->data;
		ulCount = template->len;
	}

	rv = gkm_mock_C_CreateObject (hSession, pTemplate, ulCount, phObject);

	if (template)
		g_array_free (template, TRUE);

	return rv;
}

CK_FUNCTION_LIST mock_secret_store = {
	{ 2, 11 },	/* version */
	mock_secret_C_Initialize,
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
	mock_secret_C_CreateObject,
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
