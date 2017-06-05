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

#include "gkm-attributes.h"
#include "gkm-crypto.h"
#include "gkm-hkdf-mechanism.h"
#include "gkm-secret-key.h"
#include "gkm-session.h"
#include "gkm-transaction.h"

#include "egg/egg-hkdf.h"
#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

CK_RV
gkm_hkdf_mechanism_derive (GkmSession *session, const char *algo,
                           CK_MECHANISM_PTR mech, GkmObject *base,
                           CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs,
                           GkmObject **derived)
{
	CK_ATTRIBUTE attr;
	GArray *array;
	gconstpointer value;
	gpointer output;
	gsize n_value;
	CK_ULONG n_output = 0;
	GkmTransaction *transaction;
	CK_KEY_TYPE type;

	g_return_val_if_fail (GKM_IS_SECRET_KEY (base), CKR_GENERAL_ERROR);

	/* Get the value of the original key */
	value = gkm_secret_key_get_key_value (GKM_SECRET_KEY (base), &n_value);
	g_return_val_if_fail (value, CKR_GENERAL_ERROR);

	/* What length should we truncate to? */
	if (!gkm_attributes_find_ulong (attrs, n_attrs, CKA_VALUE_LEN, &n_output)) {
		if (gkm_attributes_find_ulong (attrs, n_attrs, CKA_KEY_TYPE, &type))
			n_output = gkm_crypto_secret_key_length (type);
	}

	/* Default to input length */
	if (n_output == 0)
		n_output = n_value;

	output = egg_secure_alloc (n_output);
	if (!egg_hkdf_perform ("sha256", value, n_value, mech->pParameter,
	                       mech->ulParameterLen, NULL, 0, output, n_output)) {
		egg_secure_free (output);
		return CKR_FUNCTION_FAILED;
	}

	/* Now setup the attributes with our new value */
	array = g_array_new (FALSE, FALSE, sizeof (CK_ATTRIBUTE));

	/* Prepend the value */
	attr.type = CKA_VALUE;
	attr.pValue = output;
	attr.ulValueLen = n_output;

	g_array_append_val (array, attr);

	/* Add the remainder of the attributes */
	g_array_append_vals (array, attrs, n_attrs);

	transaction = gkm_transaction_new ();

	/* Now create an object with these attributes */
	*derived = gkm_session_create_object_for_attributes (session, transaction,
	                                                     (CK_ATTRIBUTE_PTR)array->data, array->len);

	egg_secure_free (output);
	g_array_free (array, TRUE);

	return gkm_transaction_complete_and_unref (transaction);
}
