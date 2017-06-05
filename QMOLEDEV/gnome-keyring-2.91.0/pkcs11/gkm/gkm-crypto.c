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

#include "gkm-crypto.h"
#include "gkm-aes-mechanism.h"
#include "gkm-dh-mechanism.h"
#include "gkm-dsa-mechanism.h"
#include "gkm-null-mechanism.h"
#include "gkm-rsa-mechanism.h"
#include "gkm-session.h"
#include "gkm-sexp.h"
#include "gkm-sexp-key.h"

#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

CK_RV
gkm_crypto_data_to_sexp (const gchar *format, guint nbits, EggPadding padding,
                         CK_BYTE_PTR data, CK_ULONG n_data, gcry_sexp_t *sexp)
{
	gpointer padded = NULL;
	gcry_error_t gcry;
	gcry_mpi_t mpi;
	gsize n_padded;
	gsize block;

	g_assert (format);
	g_assert (sexp);

	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	block = (nbits + 7) / 8;
	if (n_data > block)
		return CKR_DATA_LEN_RANGE;

	if (padding) {
		if (!(padding) (g_realloc, block, data, n_data, &padded, &n_padded))
			return CKR_DATA_LEN_RANGE;
	}

	/* Prepare the input s expression */
	gcry = gcry_mpi_scan (&mpi, GCRYMPI_FMT_USG,
	                      padded ? padded : data,
	                      padded ? n_padded : n_data, NULL);
	g_free (padded);

	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	gcry = gcry_sexp_build (sexp, NULL, format, mpi);
	gcry_mpi_release (mpi);

	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	g_assert (*sexp);
	return CKR_OK;
}

CK_RV
gkm_crypto_sexp_to_data (gcry_sexp_t sexp, guint bits, CK_BYTE_PTR data,
                         CK_ULONG *n_data, EggPadding padding, ...)
{
	gcry_sexp_t at = NULL;
	gsize n_block, offset, len;
	gcry_mpi_t mpi = NULL;
	gpointer padded;
	guchar *block;
	va_list va;
	gboolean ret;
	gcry_error_t gcry;

	g_assert (sexp);
	g_assert (data);
	g_assert (n_data);
	g_assert (bits);

	/* First try and dig out sexp child based on arguments */
	va_start (va, padding);
	at = gkm_sexp_get_childv (sexp, va);
	va_end (va);

	/* It's expected we would find it */
	g_return_val_if_fail (at != NULL, CKR_GENERAL_ERROR);

	/* Parse out the MPI */
	mpi = gcry_sexp_nth_mpi (at, 1, GCRYMPI_FMT_USG);
	g_return_val_if_fail (at != NULL, CKR_GENERAL_ERROR);
	gcry_sexp_release (at);

	/* Print out the MPI into the end of a temporary buffer */
	n_block = (bits + 7) / 8;
	gcry = gcry_mpi_print (GCRYMPI_FMT_USG, NULL, 0, &len, mpi);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	g_return_val_if_fail (len <= n_block, CKR_GENERAL_ERROR);
	offset = n_block - len;
	block = g_malloc0 (n_block);
	memset (block, 0, offset);
	gcry = gcry_mpi_print (GCRYMPI_FMT_USG, block + offset, len, &len, mpi);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	g_return_val_if_fail (len == n_block - offset, CKR_GENERAL_ERROR);
	gcry_mpi_release (mpi);

	/* Pad it properly if necessary */
	if (padding != NULL) {
		ret = (padding) (g_realloc, n_block, block, n_block, &padded, &n_block);
		g_free (block);
		if (ret == FALSE)
			return CKR_DATA_LEN_RANGE;
		block = padded;
	}

	/* Now stuff it into the output buffer */
	if (n_block > *n_data)
		return CKR_BUFFER_TOO_SMALL;

	memcpy (data, block, n_block);
	*n_data = n_block;
	g_free (block);

	return CKR_OK;
}


CK_RV
gkm_crypto_encrypt (GkmSession *session, CK_MECHANISM_TYPE mech, CK_BYTE_PTR data,
                    CK_ULONG n_data, CK_BYTE_PTR encrypted, CK_ULONG_PTR n_encrypted)
{
	GkmSexp *sexp;

	switch (mech) {
	case CKM_RSA_PKCS:
	case CKM_RSA_X_509:
		sexp = gkm_session_get_crypto_state (session);
		g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
		return gkm_crypto_encrypt_xsa (gkm_sexp_get (sexp), mech, data, n_data, encrypted, n_encrypted);
	default:
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	}
}

CK_RV
gkm_crypto_encrypt_xsa (gcry_sexp_t sexp, CK_MECHANISM_TYPE mech, CK_BYTE_PTR data,
                        CK_ULONG n_data, CK_BYTE_PTR encrypted, CK_ULONG_PTR n_encrypted)
{
	int algorithm;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_encrypted, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	if (!gkm_sexp_parse_key (sexp, &algorithm, NULL, NULL))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	/*
	 * The algorithm checks below are merely sanity checks.
	 * Other code should have checed this at an earlier stage
	 * and return the right error codes if invalid.
	 */

	switch (mech) {
	case CKM_RSA_PKCS:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_encrypt (sexp, egg_padding_pkcs1_pad_02, data, n_data, encrypted, n_encrypted);
		break;
	case CKM_RSA_X_509:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_encrypt (sexp, egg_padding_zero_pad, data, n_data, encrypted, n_encrypted);
		break;
	default:
		/* Again shouldn't be reached */
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	};

	return rv;
}

CK_RV
gkm_crypto_decrypt (GkmSession *session, CK_MECHANISM_TYPE mech, CK_BYTE_PTR encrypted,
                    CK_ULONG n_encrypted, CK_BYTE_PTR data, CK_ULONG_PTR n_data)
{
	GkmSexp *sexp;

	switch (mech) {
	case CKM_RSA_PKCS:
	case CKM_RSA_X_509:
		sexp = gkm_session_get_crypto_state (session);
		g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
		return gkm_crypto_decrypt_xsa (gkm_sexp_get (sexp), mech, encrypted, n_encrypted, data, n_data);
	default:
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	}
}

CK_RV
gkm_crypto_decrypt_xsa (gcry_sexp_t sexp, CK_MECHANISM_TYPE mech, CK_BYTE_PTR encrypted,
                        CK_ULONG n_encrypted, CK_BYTE_PTR data, CK_ULONG_PTR n_data)
{
	int algorithm;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_data, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (encrypted, CKR_ARGUMENTS_BAD);

	if (!gkm_sexp_parse_key (sexp, &algorithm, NULL, NULL))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	/*
	 * The algorithm checks below are merely sanity checks.
	 * Other code should have checed this at an earlier stage
	 * and return the right error codes if invalid.
	 */

	switch (mech) {
	case CKM_RSA_PKCS:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_decrypt (sexp, egg_padding_pkcs1_unpad_02, encrypted, n_encrypted, data, n_data);
		break;
	case CKM_RSA_X_509:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_decrypt (sexp, NULL, encrypted, n_encrypted, data, n_data);
		break;
	default:
		/* Again shouldn't be reached */
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	};

	return rv;
}

CK_RV
gkm_crypto_sign (GkmSession *session, CK_MECHANISM_TYPE mech, CK_BYTE_PTR data,
                 CK_ULONG n_data, CK_BYTE_PTR signature, CK_ULONG_PTR n_signature)
{
	GkmSexp *sexp;

	switch (mech) {
	case CKM_RSA_PKCS:
	case CKM_RSA_X_509:
	case CKM_DSA:
		sexp = gkm_session_get_crypto_state (session);
		g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
		return gkm_crypto_sign_xsa (gkm_sexp_get (sexp), mech, data, n_data, signature, n_signature);
	default:
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	}
}

CK_RV
gkm_crypto_sign_xsa (gcry_sexp_t sexp, CK_MECHANISM_TYPE mech, CK_BYTE_PTR data,
                     CK_ULONG n_data, CK_BYTE_PTR signature, CK_ULONG_PTR n_signature)
{
	int algorithm;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_signature, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	if (!gkm_sexp_parse_key (sexp, &algorithm, NULL, NULL))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	/*
	 * The algorithm checks below are merely sanity checks.
	 * Other code should have checed this at an earlier stage
	 * and return the right error codes if invalid.
	 */

	switch (mech) {
	case CKM_RSA_PKCS:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_sign (sexp, egg_padding_pkcs1_pad_01, data, n_data, signature, n_signature);
		break;
	case CKM_RSA_X_509:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_sign (sexp, egg_padding_zero_pad, data, n_data, signature, n_signature);
		break;
	case CKM_DSA:
		g_return_val_if_fail (algorithm == GCRY_PK_DSA, CKR_GENERAL_ERROR);
		rv = gkm_dsa_mechanism_sign (sexp, data, n_data, signature, n_signature);
		break;
	default:
		/* Again shouldn't be reached */
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	};

	return rv;
}

CK_RV
gkm_crypto_verify (GkmSession *session, CK_MECHANISM_TYPE mech, CK_BYTE_PTR data,
                   CK_ULONG n_data, CK_BYTE_PTR signature, CK_ULONG n_signature)
{
	GkmSexp *sexp;

	switch (mech) {
	case CKM_RSA_PKCS:
	case CKM_RSA_X_509:
	case CKM_DSA:
		sexp = gkm_session_get_crypto_state (session);
		g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
		return gkm_crypto_verify_xsa (gkm_sexp_get (sexp), mech, data, n_data, signature, n_signature);
	default:
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	}
}

CK_RV
gkm_crypto_verify_xsa (gcry_sexp_t sexp, CK_MECHANISM_TYPE mech, CK_BYTE_PTR data,
                       CK_ULONG n_data, CK_BYTE_PTR signature, CK_ULONG n_signature)
{
	int algorithm;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (signature, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	if (!gkm_sexp_parse_key (sexp, &algorithm, NULL, NULL))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	/*
	 * The algorithm checks below are merely sanity checks.
	 * Other code should have checed this at an earlier stage
	 * and return the right error codes if invalid.
	 */

	switch (mech) {
	case CKM_RSA_PKCS:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_verify (sexp, egg_padding_pkcs1_pad_01, data, n_data, signature, n_signature);
		break;
	case CKM_RSA_X_509:
		g_return_val_if_fail (algorithm == GCRY_PK_RSA, CKR_GENERAL_ERROR);
		rv = gkm_rsa_mechanism_verify (sexp, egg_padding_zero_pad, data, n_data, signature, n_signature);
		break;
	case CKM_DSA:
		g_return_val_if_fail (algorithm == GCRY_PK_DSA, CKR_GENERAL_ERROR);
		rv = gkm_dsa_mechanism_verify (sexp, data, n_data, signature, n_signature);
		break;
	default:
		/* Again shouldn't be reached */
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	};

	return rv;
}

CK_RV
gkm_crypto_perform (GkmSession *session, CK_MECHANISM_TYPE mech, CK_ATTRIBUTE_TYPE method,
                    CK_BYTE_PTR bufone, CK_ULONG n_bufone, CK_BYTE_PTR buftwo, CK_ULONG_PTR n_buftwo)
{
	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (method, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_buftwo, CKR_GENERAL_ERROR);

	switch (method) {
	case CKA_ENCRYPT:
		return gkm_crypto_encrypt (session, mech, bufone, n_bufone, buftwo, n_buftwo);
	case CKA_DECRYPT:
		return gkm_crypto_decrypt (session, mech, bufone, n_bufone, buftwo, n_buftwo);
	case CKA_SIGN:
		return gkm_crypto_sign (session, mech, bufone, n_bufone, buftwo, n_buftwo);
	case CKA_VERIFY:
		return gkm_crypto_verify (session, mech, bufone, n_bufone, buftwo, *n_buftwo);
	default:
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	}
}

CK_RV
gkm_crypto_generate_key_pair (GkmSession *session, CK_MECHANISM_TYPE mech,
                              CK_ATTRIBUTE_PTR pub_atts, CK_ULONG n_pub_atts,
                              CK_ATTRIBUTE_PTR priv_atts, CK_ULONG n_priv_atts,
                              GkmObject **pub_key, GkmObject **priv_key)
{
	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (pub_key, CKR_GENERAL_ERROR);
	g_return_val_if_fail (priv_key, CKR_GENERAL_ERROR);

	switch (mech) {
	case CKM_DH_PKCS_KEY_PAIR_GEN:
		return gkm_dh_mechanism_generate (session, pub_atts, n_pub_atts,
		                                  priv_atts, n_priv_atts,
		                                  pub_key, priv_key);
	default:
		return CKR_MECHANISM_INVALID;
	}
}

CK_RV
gkm_crypto_derive_key (GkmSession *session, CK_MECHANISM_PTR mech, GkmObject *base,
                       CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, GkmObject **derived)
{
	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (base), CKR_GENERAL_ERROR);
	g_return_val_if_fail (derived, CKR_GENERAL_ERROR);

	if (!gkm_object_has_attribute_ulong (base, session, CKA_ALLOWED_MECHANISMS, mech->mechanism))
		return CKR_KEY_TYPE_INCONSISTENT;

	if (!gkm_object_has_attribute_boolean (base, session, CKA_DERIVE, TRUE))
		return CKR_KEY_FUNCTION_NOT_PERMITTED;

	switch (mech->mechanism) {
	case CKM_DH_PKCS_DERIVE:
		return gkm_dh_mechanism_derive (session, mech, base, attrs,
		                                n_attrs, derived);
	default:
		return CKR_MECHANISM_INVALID;
	}
}

CK_RV
gkm_crypto_wrap_key (GkmSession *session, CK_MECHANISM_PTR mech, GkmObject *wrapper,
                     GkmObject *wrapped, CK_BYTE_PTR output, CK_ULONG_PTR n_output)
{
	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (wrapper), CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (wrapped), CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_output, CKR_GENERAL_ERROR);

	if (!gkm_object_has_attribute_ulong (wrapper, session, CKA_ALLOWED_MECHANISMS, mech->mechanism))
		return CKR_KEY_TYPE_INCONSISTENT;

	if (!gkm_object_has_attribute_boolean (wrapper, session, CKA_WRAP, TRUE))
		return CKR_KEY_FUNCTION_NOT_PERMITTED;

	switch (mech->mechanism) {
	case CKM_AES_CBC_PAD:
		return gkm_aes_mechanism_wrap (session, mech, wrapper, wrapped,
		                               output, n_output);
	case CKM_G_NULL:
		return gkm_null_mechanism_wrap (session, mech, wrapper, wrapped,
		                                output, n_output);
	default:
		return CKR_MECHANISM_INVALID;
	}
}

CK_RV
gkm_crypto_unwrap_key (GkmSession *session, CK_MECHANISM_PTR mech, GkmObject *wrapper,
                       CK_VOID_PTR input, CK_ULONG n_input, CK_ATTRIBUTE_PTR attrs,
                       CK_ULONG n_attrs, GkmObject **unwrapped)
{
	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (wrapper), CKR_GENERAL_ERROR);
	g_return_val_if_fail (mech, CKR_GENERAL_ERROR);
	g_return_val_if_fail (unwrapped, CKR_GENERAL_ERROR);

	if (!gkm_object_has_attribute_ulong (wrapper, session, CKA_ALLOWED_MECHANISMS, mech->mechanism))
		return CKR_KEY_TYPE_INCONSISTENT;

	if (!gkm_object_has_attribute_boolean (wrapper, session, CKA_UNWRAP, TRUE))
		return CKR_KEY_FUNCTION_NOT_PERMITTED;

	switch (mech->mechanism) {
	case CKM_AES_CBC_PAD:
		return gkm_aes_mechanism_unwrap (session, mech, wrapper, input,
		                                 n_input, attrs, n_attrs, unwrapped);
	case CKM_G_NULL:
		return gkm_null_mechanism_unwrap (session, mech, wrapper, input,
		                                  n_input, attrs, n_attrs, unwrapped);
	default:
		return CKR_MECHANISM_INVALID;
	}
}

/* ----------------------------------------------------------------------------
 * PREPARE FUNCTIONS
 */

CK_RV
gkm_crypto_prepare (GkmSession *session, CK_MECHANISM_TYPE mech, GkmObject *key)
{
	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);

	switch (mech) {
	case CKM_RSA_PKCS:
	case CKM_RSA_X_509:
	case CKM_DSA:
		return gkm_crypto_prepare_xsa (session, mech, key);
	default:
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	}
}

CK_RV
gkm_crypto_prepare_xsa (GkmSession *session, CK_MECHANISM_TYPE mech, GkmObject *key)
{
	GkmSexp *sexp;

	g_return_val_if_fail (GKM_IS_SESSION (session), CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_SEXP_KEY (key), CKR_GENERAL_ERROR);

	/* Load up the actual sexp we're going to use */
	sexp = gkm_sexp_key_acquire_crypto_sexp (GKM_SEXP_KEY (key), session);
	if (sexp == NULL)
		return CKR_USER_NOT_LOGGED_IN;

	gkm_session_set_crypto_state (session, sexp, gkm_sexp_unref);
	return CKR_OK;
}

/* --------------------------------------------------------------------------
 * UTILITY
 */


void
gkm_crypto_initialize (void)
{
	egg_libgcrypt_initialize ();
}

gulong
gkm_crypto_secret_key_length (CK_KEY_TYPE type)
{
	switch (type) {
	case CKK_AES:
		return GKM_AES_MECHANISM_MIN_LENGTH;
	default:
		return 0;
	}
}
