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
#include "gkm-dh-mechanism.h"
#include "gkm-dh-private-key.h"
#include "gkm-session.h"
#include "gkm-transaction.h"

#include "egg/egg-dh.h"
#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

static GkmObject*
create_dh_object (GkmSession *session, GkmTransaction *transaction, CK_OBJECT_CLASS klass,
                  CK_ATTRIBUTE_PTR value, CK_ATTRIBUTE_PTR prime, CK_ATTRIBUTE_PTR base,
                  CK_ATTRIBUTE_PTR id, CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	CK_KEY_TYPE type = CKK_DH;
	CK_ATTRIBUTE attr;
	GkmObject *object;
	GArray *array;

	array = g_array_new (FALSE, TRUE, sizeof (CK_ATTRIBUTE));

	/* Setup the value */
	g_array_append_val (array, *value);

	/* Add in the DH params */
	g_array_append_val (array, *prime);
	g_array_append_val (array, *base);

	/* Setup the class */
	attr.type = CKA_CLASS;
	attr.pValue = &klass;
	attr.ulValueLen = sizeof (klass);
	g_array_append_val (array, attr);

	/* Setup the key type */
	attr.type = CKA_KEY_TYPE;
	attr.pValue = &type;
	attr.ulValueLen = sizeof (type);
	g_array_append_val (array, attr);

	/* All the other values */
	g_array_append_vals (array, attrs, n_attrs);

	/* Add in the identifier last */
	g_array_append_val (array, *id);

	/* Create the public key object */
	object = gkm_session_create_object_for_attributes (session, transaction,
	                                                   (CK_ATTRIBUTE_PTR)array->data, array->len);

	g_array_free (array, TRUE);
	return object;
}

CK_RV
gkm_dh_mechanism_generate (GkmSession *session, CK_ATTRIBUTE_PTR pub_atts,
                           CK_ULONG n_pub_atts, CK_ATTRIBUTE_PTR priv_atts,
                           CK_ULONG n_priv_atts, GkmObject **pub_key,
                           GkmObject **priv_key)
{
	gcry_mpi_t prime = NULL;
	gcry_mpi_t base = NULL;
	gcry_mpi_t pub = NULL;
	gcry_mpi_t priv = NULL;
	gcry_error_t gcry;
	CK_ATTRIBUTE value, id;
	CK_ATTRIBUTE_PTR aprime;
	CK_ATTRIBUTE_PTR abase;
	GkmTransaction *transaction;
	gboolean ret;
	gsize length;
	gulong bits;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (pub_key, CKR_GENERAL_ERROR);
	g_return_val_if_fail (priv_key, CKR_GENERAL_ERROR);

	*priv_key = NULL;
	*pub_key = NULL;

	aprime = gkm_attributes_find (pub_atts, n_pub_atts, CKA_PRIME);
	abase = gkm_attributes_find (pub_atts, n_pub_atts, CKA_BASE);
	if (!aprime || !abase)
		return CKR_TEMPLATE_INCOMPLETE;

	rv = gkm_attribute_get_mpi (aprime, &prime);
	if (rv != CKR_OK)
		return rv;

	rv = gkm_attribute_get_mpi (abase, &base);
	if (rv != CKR_OK) {
		gcry_mpi_release (prime);
		return rv;
	}

	if (!gkm_attributes_find_ulong (priv_atts, n_priv_atts, CKA_VALUE_BITS, &bits))
		bits = gcry_mpi_get_nbits (prime);
	gkm_attributes_consume (priv_atts, n_priv_atts, CKA_VALUE_BITS, G_MAXULONG);

	/* The private key must be less than or equal to prime */
	if (bits > gcry_mpi_get_nbits (prime)) {
		gcry_mpi_release (prime);
		gcry_mpi_release (base);
		return CKR_TEMPLATE_INCONSISTENT;
	}

	ret = egg_dh_gen_pair (prime, base, bits, &pub, &priv);

	gcry_mpi_release (prime);
	gcry_mpi_release (base);

	if (ret == FALSE)
		return CKR_FUNCTION_FAILED;

	/* Write the public key out to raw data */
	value.type = CKA_VALUE;
	gcry = gcry_mpi_print (GCRYMPI_FMT_USG, NULL, 0, &length, pub);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	value.pValue = g_malloc (length);
	gcry = gcry_mpi_print (GCRYMPI_FMT_USG, value.pValue, length, &length, pub);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	value.ulValueLen = length;

	/* Create an identifier */
	id.type = CKA_ID;
	if (value.ulValueLen < 16) {
		id.ulValueLen = value.ulValueLen;
		id.pValue = g_memdup (value.pValue, value.ulValueLen);
	} else {
		id.ulValueLen = 16;
		id.pValue = g_memdup ((guchar*)value.pValue + (value.ulValueLen - 16), id.ulValueLen);
	}

	transaction = gkm_transaction_new ();

	*pub_key = create_dh_object (session, transaction, CKO_PUBLIC_KEY, &value,
	                            aprime, abase, &id, pub_atts, n_pub_atts);
	g_free (value.pValue);

	if (!gkm_transaction_get_failed (transaction)) {

		/* Write the private key out to raw data */
		value.type = CKA_VALUE;
		gcry = gcry_mpi_print (GCRYMPI_FMT_USG, NULL, 0, &length, priv);
		g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
		value.pValue = egg_secure_alloc (length);
		gcry = gcry_mpi_print (GCRYMPI_FMT_USG, value.pValue, length, &length, priv);
		g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
		value.ulValueLen = length;

		*priv_key = create_dh_object (session, transaction, CKO_PRIVATE_KEY, &value,
		                              aprime, abase, &id, priv_atts, n_priv_atts);
		egg_secure_clear (value.pValue, value.ulValueLen);
		egg_secure_free (value.pValue);
	}

	g_free (id.pValue);

	gkm_transaction_complete (transaction);
	if (gkm_transaction_get_failed (transaction)) {
		if (*pub_key)
			g_object_unref (*pub_key);
		if (*priv_key)
			g_object_unref (*priv_key);
		*priv_key = *pub_key = NULL;
	}

	rv = gkm_transaction_get_result (transaction);
	g_object_unref (transaction);

	gkm_attributes_consume (pub_atts, n_pub_atts, CKA_PRIME, CKA_BASE, G_MAXULONG);

	return rv;
}

CK_RV
gkm_dh_mechanism_derive (GkmSession *session, CK_MECHANISM_PTR mech, GkmObject *base,
                         CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, GkmObject **derived)
{
	gcry_mpi_t peer = NULL;
	gcry_mpi_t prime;
	gcry_mpi_t priv;
	gcry_error_t gcry;
	CK_ATTRIBUTE attr;
	GArray *array;
	CK_ULONG n_value = 0;
	gpointer value;
	GkmTransaction *transaction;
	CK_KEY_TYPE type;

	g_return_val_if_fail (GKM_IS_DH_PRIVATE_KEY (base), CKR_GENERAL_ERROR);

	if (mech->ulParameterLen && mech->pParameter) {
		gcry = gcry_mpi_scan (&peer, GCRYMPI_FMT_USG, mech->pParameter,
		                      mech->ulParameterLen, NULL);
		if (gcry != 0)
			peer = NULL;
	}

	if (peer == NULL)
		return CKR_MECHANISM_PARAM_INVALID;

	prime = gkm_dh_key_get_prime (GKM_DH_KEY (base));
	priv = gkm_dh_private_key_get_value (GKM_DH_PRIVATE_KEY (base));

	/* What length should we truncate to? */
	if (!gkm_attributes_find_ulong (attrs, n_attrs, CKA_VALUE_LEN, &n_value)) {
		if (gkm_attributes_find_ulong (attrs, n_attrs, CKA_KEY_TYPE, &type))
			n_value = gkm_crypto_secret_key_length (type);
	}

	/* Default to full length of the DH prime */
	if (n_value == 0)
		n_value = (gcry_mpi_get_nbits (prime) + 7) / 8;

	value = egg_dh_gen_secret (peer, priv, prime, n_value);
	gcry_mpi_release (peer);

	if (value == NULL)
		return CKR_FUNCTION_FAILED;

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
	*derived = gkm_session_create_object_for_attributes (session, transaction,
	                                                     (CK_ATTRIBUTE_PTR)array->data, array->len);

	egg_secure_free (value);
	g_array_free (array, TRUE);

	return gkm_transaction_complete_and_unref (transaction);
}
