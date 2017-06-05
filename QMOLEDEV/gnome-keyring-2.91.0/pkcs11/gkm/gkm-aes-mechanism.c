/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#include "gkm-aes-key.h"
#include "gkm-aes-mechanism.h"
#include "gkm-session.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

#include "egg/egg-libgcrypt.h"
#include "egg/egg-padding.h"
#include "egg/egg-secure-memory.h"

static CK_RV
retrieve_length (GkmSession *session, GkmObject *wrapped, gsize *length)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	attr.type = CKA_VALUE;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = gkm_object_get_attribute (wrapped, session, &attr);
	if (rv == CKR_OK)
		*length = attr.ulValueLen;
	return rv;
}

static CK_RV
retrieve_value (GkmSession *session, GkmObject *wrapped,
                gpointer *value, gsize *n_value)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	rv = retrieve_length (session, wrapped, n_value);
	if (rv != CKR_OK)
		return rv;

	attr.type = CKA_VALUE;
	attr.pValue = egg_secure_alloc (*n_value);
	attr.ulValueLen = *n_value;

	rv = gkm_object_get_attribute (wrapped, session, &attr);
	if (rv == CKR_OK)
		*value = attr.pValue;
	else
		egg_secure_free (attr.pValue);

	return rv;
}

CK_RV
gkm_aes_mechanism_wrap (GkmSession *session, CK_MECHANISM_PTR mech,
                        GkmObject *wrapper, GkmObject *wrapped,
                        CK_BYTE_PTR output, CK_ULONG_PTR n_output)
{
	gcry_cipher_hd_t cih;
	gcry_error_t gcry;
	GkmAesKey *key;
	gpointer value, padded;
	gsize n_value, n_padded;
	gsize block, pos;
	gboolean ret;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech, CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech->mechanism == CKM_AES_CBC_PAD, CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (wrapped), CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_output, CKR_GENERAL_ERROR);

	if (!GKM_IS_AES_KEY (wrapper))
		return CKR_WRAPPING_KEY_TYPE_INCONSISTENT;
	key = GKM_AES_KEY (wrapper);

	block = gkm_aes_key_get_block_size (key);
	g_return_val_if_fail (block != 0, CKR_GENERAL_ERROR);

	/* They just want the length */
	if (!output) {
		rv = retrieve_length (session, wrapped, &n_value);
		if (rv != CKR_OK)
			return rv;
		if (!egg_padding_pkcs7_pad (NULL, block, NULL, n_value, NULL, &n_padded))
			return CKR_KEY_SIZE_RANGE;
		*n_output = n_padded;
		return CKR_OK;
	}

	cih = gkm_aes_key_get_cipher (key, GCRY_CIPHER_MODE_CBC);
	if (cih == NULL)
		return CKR_FUNCTION_FAILED;

	if (!mech->pParameter || gcry_cipher_setiv (cih, mech->pParameter, mech->ulParameterLen) != 0) {
		gcry_cipher_close (cih);
		return CKR_MECHANISM_PARAM_INVALID;
	}

	rv = retrieve_value (session, wrapped, &value, &n_value);
	if (rv != CKR_OK) {
		gcry_cipher_close (cih);
		return rv;
	}

	ret = egg_padding_pkcs7_pad (egg_secure_realloc, block, value, n_value, &padded, &n_padded);
	egg_secure_free (value);

	if (ret == FALSE) {
		gcry_cipher_close (cih);
		return CKR_KEY_SIZE_RANGE;
	}

	/* In place encryption */
	for (pos = 0; pos < n_padded; pos += block) {
		gcry = gcry_cipher_encrypt (cih, (guchar*)padded + pos, block, NULL, 0);
		g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	}

	gcry_cipher_close (cih);

	rv = gkm_util_return_data (output, n_output, padded, n_padded);
	egg_secure_free (padded);
	return rv;
}

CK_RV
gkm_aes_mechanism_unwrap (GkmSession *session, CK_MECHANISM_PTR mech,
                          GkmObject *wrapper, CK_VOID_PTR input, CK_ULONG n_input,
                          CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs,
                          GkmObject **unwrapped)
{
	gcry_cipher_hd_t cih;
	gcry_error_t gcry;
	CK_ATTRIBUTE attr;
	GArray *array;
	GkmAesKey *key;
	gpointer padded, value;
	gsize n_padded, n_value;
	GkmTransaction *transaction;
	gsize block, pos;
	gboolean ret;

	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech, CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech->mechanism == CKM_AES_CBC_PAD, CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (wrapper), CKR_GENERAL_ERROR);

	if (!GKM_IS_AES_KEY (wrapper))
		return CKR_WRAPPING_KEY_TYPE_INCONSISTENT;
	key = GKM_AES_KEY (wrapper);

	block = gkm_aes_key_get_block_size (key);
	g_return_val_if_fail (block != 0, CKR_GENERAL_ERROR);

	if (n_input == 0 || n_input % block != 0)
		return CKR_WRAPPED_KEY_LEN_RANGE;

	cih = gkm_aes_key_get_cipher (key, GCRY_CIPHER_MODE_CBC);
	if (cih == NULL)
		return CKR_FUNCTION_FAILED;

	if (!mech->pParameter || gcry_cipher_setiv (cih, mech->pParameter, mech->ulParameterLen) != 0) {
		gcry_cipher_close (cih);
		return CKR_MECHANISM_PARAM_INVALID;
	}

	padded = egg_secure_alloc (n_input);
	memcpy (padded, input, n_input);
	n_padded = n_input;

	/* In place decryption */
	for (pos = 0; pos < n_padded; pos += block) {
		gcry = gcry_cipher_decrypt (cih, (guchar*)padded + pos, block, NULL, 0);
		g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	}

	gcry_cipher_close (cih);

	/* Unpad the resulting value */
	ret = egg_padding_pkcs7_unpad (egg_secure_realloc, block, padded, n_padded, &value, &n_value);
	egg_secure_free (padded);

	/* TODO: This is dubious, there doesn't seem to be an rv for 'bad decrypt' */
	if (ret == FALSE)
		return CKR_WRAPPED_KEY_INVALID;

	/* Now setup the attributes with our new value */
	array = g_array_new (FALSE, FALSE, sizeof (CK_ATTRIBUTE));

	/* Prepend the value */
	attr.type = CKA_VALUE;
	attr.pValue = value;
	attr.ulValueLen = n_value;
	g_array_append_val (array, attr);

	/* Add the remainder of the attributes */
	g_array_append_vals (array, attrs, n_attrs);

	transaction = gkm_transaction_new ();

	/* Now create an object with these attributes */
	*unwrapped = gkm_session_create_object_for_attributes (session, transaction,
	                                                       (CK_ATTRIBUTE_PTR)array->data, array->len);

	egg_secure_free (value);
	g_array_free (array, TRUE);

	return gkm_transaction_complete_and_unref (transaction);
}
