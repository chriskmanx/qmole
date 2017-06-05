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

#include "gkm-rsa-mechanism.h"
#include "gkm-sexp.h"

#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

CK_RV
gkm_rsa_mechanism_encrypt (gcry_sexp_t sexp, EggPadding padding, CK_BYTE_PTR data,
                           CK_ULONG n_data, CK_BYTE_PTR encrypted, CK_ULONG_PTR n_encrypted)
{
	gcry_sexp_t splain, sdata;
	gcry_error_t gcry;
	guint nbits;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_encrypted, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	nbits = gcry_pk_get_nbits (sexp);
	g_return_val_if_fail (nbits > 0, CKR_GENERAL_ERROR);

	/* Just want to know the length */
	if (!encrypted) {
		*n_encrypted = (nbits + 7) / 8;
		return CKR_OK;
	}

	/* Prepare the input s expression */
	rv = gkm_crypto_data_to_sexp ("(data (flags raw) (value %m))",
	                              nbits, padding, data, n_data, &splain);
	if (rv != CKR_OK)
		return rv;

	/* Do the magic */
	gcry = gcry_pk_encrypt (&sdata, splain, sexp);
	gcry_sexp_release (splain);

	/* TODO: Certain codes should be returned (data too big etc... ) */
	if (gcry) {
		g_message ("encrypting of the data failed: %s", gcry_strerror (gcry));
		return CKR_FUNCTION_FAILED;
	}

	/* Now extract and send it back out */
	rv = gkm_crypto_sexp_to_data (sdata, nbits, encrypted, n_encrypted, NULL,
	                              "enc-val", "rsa", "a", NULL);
	gcry_sexp_release (sdata);

	return rv;
}

CK_RV
gkm_rsa_mechanism_decrypt (gcry_sexp_t sexp, EggPadding padding, CK_BYTE_PTR encrypted,
                           CK_ULONG n_encrypted, CK_BYTE_PTR data, CK_ULONG_PTR n_data)
{
	gcry_sexp_t splain, sdata;
	gcry_error_t gcry;
	guint nbits;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_data, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (encrypted, CKR_ARGUMENTS_BAD);

	nbits = gcry_pk_get_nbits (sexp);
	g_return_val_if_fail (nbits > 0, CKR_GENERAL_ERROR);

	/* Just want to know the length */
	if (!data) {
		*n_data = (nbits + 7) / 8;
		return CKR_OK;
	}

	if (n_encrypted != (nbits + 7) / 8)
		return CKR_DATA_LEN_RANGE;

	/* Prepare the input s expression */
	rv = gkm_crypto_data_to_sexp ("(enc-val (flags) (rsa (a %m)))",
	                              nbits, NULL, encrypted, n_encrypted, &sdata);
	if (rv != CKR_OK)
		return rv;

	/* Do the magic */
	gcry = gcry_pk_decrypt (&splain, sdata, sexp);
	gcry_sexp_release (sdata);

	/* TODO: Certain codes should be returned (data too big etc... ) */
	if (gcry) {
		g_message ("decrypting of the data failed: %s", gcry_strerror (gcry));
		return CKR_FUNCTION_FAILED;
	}

	/* Now extract and send it back out */
	rv = gkm_crypto_sexp_to_data (splain, nbits, data, n_data, padding, "value", NULL);
	gcry_sexp_release (splain);

	return rv;
}

CK_RV
gkm_rsa_mechanism_sign (gcry_sexp_t sexp, EggPadding padding, CK_BYTE_PTR data,
                        CK_ULONG n_data, CK_BYTE_PTR signature, CK_ULONG_PTR n_signature)
{
	gcry_sexp_t ssig, sdata;
	guint nbits;
	gcry_error_t gcry;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_signature, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	nbits = gcry_pk_get_nbits (sexp);
	g_return_val_if_fail (nbits > 0, CKR_GENERAL_ERROR);

	/* Just want to know the length */
	if (!signature) {
		*n_signature = (nbits + 7) / 8;
		return CKR_OK;
	}

	/* Prepare the input sexp */
	rv = gkm_crypto_data_to_sexp ("(data (flags raw) (value %m))",
	                              nbits, padding, data, n_data, &sdata);
	if (rv != CKR_OK)
		return rv;

	/* Do the magic */
	gcry = gcry_pk_sign (&ssig, sdata, sexp);
	gcry_sexp_release (sdata);

	/* TODO: Certain codes should be returned (data too big etc... ) */
	if (gcry) {
		g_message ("signing of the data failed: %s", gcry_strerror (gcry));
		return CKR_FUNCTION_FAILED;
	}

	/* Now extract and send it back out */
	rv = gkm_crypto_sexp_to_data (ssig, nbits, signature, n_signature, NULL, "rsa", "s", NULL);
	gcry_sexp_release (ssig);

	return rv;
}

CK_RV
gkm_rsa_mechanism_verify (gcry_sexp_t sexp, EggPadding padding, CK_BYTE_PTR data,
                          CK_ULONG n_data, CK_BYTE_PTR signature, CK_ULONG n_signature)
{
	gcry_sexp_t ssig, sdata;
	gcry_error_t gcry;
	guint nbits;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (signature, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	/* The key size */
	nbits = gcry_pk_get_nbits (sexp);
	g_return_val_if_fail (nbits > 0, CKR_GENERAL_ERROR);

	if (n_signature != (nbits + 7) / 8)
		return CKR_SIGNATURE_LEN_RANGE;

	/* Prepare the input s expressions */
	rv = gkm_crypto_data_to_sexp ("(data (flags raw) (value %m))",
	                   nbits, padding, data, n_data, &sdata);
	if (rv != CKR_OK)
		return rv;

	rv = gkm_crypto_data_to_sexp ("(sig-val (rsa (s %m)))",
	                   nbits, NULL, signature, n_signature, &ssig);
	if (rv != CKR_OK) {
		gcry_sexp_release (sdata);
		return rv;
	}

	/* Do the magic */
	gcry = gcry_pk_verify (ssig, sdata, sexp);
	gcry_sexp_release (sdata);
	gcry_sexp_release (ssig);

	/* TODO: See if any other codes should be mapped */
	if (gcry_err_code (gcry) == GPG_ERR_BAD_SIGNATURE) {
		return CKR_SIGNATURE_INVALID;
	} else if (gcry) {
		g_message ("signing of the data failed: %s", gcry_strerror (gcry));
		return CKR_FUNCTION_FAILED;
	}

	return CKR_OK;
}
