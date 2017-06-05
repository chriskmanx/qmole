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

#include "gkm-null-key.h"
#include "gkm-null-mechanism.h"
#include "gkm-session.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

#include "egg/egg-libgcrypt.h"
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
gkm_null_mechanism_wrap (GkmSession *session, CK_MECHANISM_PTR mech,
                        GkmObject *wrapper, GkmObject *wrapped,
                        CK_BYTE_PTR output, CK_ULONG_PTR n_output)
{
	GkmNullKey *key;
	gpointer value;
	gsize n_value;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech, CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech->mechanism == CKM_G_NULL, CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (wrapped), CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_output, CKR_GENERAL_ERROR);

	if (!GKM_IS_NULL_KEY (wrapper))
		return CKR_WRAPPING_KEY_TYPE_INCONSISTENT;
	key = GKM_NULL_KEY (wrapper);

	/* They just want the length */
	if (!output) {
		rv = retrieve_length (session, wrapped, &n_value);
		if (rv == CKR_OK)
			*n_output = n_value;
		return rv;
	}

	if (mech->ulParameterLen)
		return CKR_MECHANISM_PARAM_INVALID;

	rv = retrieve_value (session, wrapped, &value, &n_value);
	if (rv != CKR_OK)
		return rv;

	rv = gkm_util_return_data (output, n_output, value, n_value);
	egg_secure_free (value);
	return rv;
}

CK_RV
gkm_null_mechanism_unwrap (GkmSession *session, CK_MECHANISM_PTR mech,
                          GkmObject *wrapper, CK_VOID_PTR input, CK_ULONG n_input,
                          CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs,
                          GkmObject **unwrapped)
{
	CK_ATTRIBUTE attr;
	GArray *array;
	GkmNullKey *key;
	GkmTransaction *transaction;

	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech, CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech->mechanism == CKM_G_NULL, CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (wrapper), CKR_GENERAL_ERROR);

	if (!GKM_IS_NULL_KEY (wrapper))
		return CKR_WRAPPING_KEY_TYPE_INCONSISTENT;
	key = GKM_NULL_KEY (wrapper);

	if (mech->ulParameterLen)
		return CKR_MECHANISM_PARAM_INVALID;

	/* Now setup the attributes with our new value */
	array = g_array_new (FALSE, FALSE, sizeof (CK_ATTRIBUTE));

	/* Prepend the value */
	attr.type = CKA_VALUE;
	attr.pValue = input;
	attr.ulValueLen = n_input;
	g_array_append_val (array, attr);

	/* Add the remainder of the attributes */
	g_array_append_vals (array, attrs, n_attrs);

	transaction = gkm_transaction_new ();

	/* Now create an object with these attributes */
	*unwrapped = gkm_session_create_object_for_attributes (session, transaction,
	                                                       (CK_ATTRIBUTE_PTR)array->data, array->len);

	g_array_free (array, TRUE);

	return gkm_transaction_complete_and_unref (transaction);
}
