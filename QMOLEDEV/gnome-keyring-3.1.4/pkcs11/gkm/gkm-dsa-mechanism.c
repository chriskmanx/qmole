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
#include "gkm-dsa-mechanism.h"
#include "gkm-session.h"
#include "gkm-sexp.h"
#include "gkm-sexp-key.h"

#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

CK_RV
gkm_dsa_mechanism_sign (gcry_sexp_t sexp, CK_BYTE_PTR data, CK_ULONG n_data,
                        CK_BYTE_PTR signature, CK_ULONG_PTR n_signature)
{
	gcry_sexp_t ssig, splain;
	gcry_error_t gcry;
	gcry_mpi_t mpi;
	CK_ULONG size;
	CK_RV rv;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (n_signature, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	if (n_data != 20)
		return CKR_DATA_LEN_RANGE;

	/* If no output, then don't process */
	if (!signature) {
		*n_signature = 40;
		return CKR_OK;
	} else if (*n_signature < 40) {
		*n_signature = 40;
		return CKR_BUFFER_TOO_SMALL;
	}

	/* Prepare the input s-expression */
	gcry = gcry_mpi_scan (&mpi, GCRYMPI_FMT_USG, data, n_data, NULL);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	gcry = gcry_sexp_build (&splain, NULL, "(data (flags raw) (value %m))", mpi);
	gcry_mpi_release (mpi);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	/* Do the magic */
	gcry = gcry_pk_sign (&ssig, splain, sexp);
	gcry_sexp_release (splain);

	/* TODO: Certain codes should be returned (data too big etc... ) */
	if (gcry) {
		g_message ("signing of the data failed: %s", gcry_strerror (gcry));
		return CKR_FUNCTION_FAILED;
	}

	g_assert (*n_signature >= 40);

	size = 20;
	rv = gkm_crypto_sexp_to_data (ssig, 20 * 8, signature, &size, NULL, "dsa", "r", NULL);
	if (rv == CKR_OK) {
		g_return_val_if_fail (size == 20, CKR_GENERAL_ERROR);
		rv = gkm_crypto_sexp_to_data (ssig, 20 * 8, signature + 20, &size, NULL, "dsa", "s", NULL);
		if (rv == CKR_OK) {
			g_return_val_if_fail (size == 20, CKR_GENERAL_ERROR);
			*n_signature = 40;
		}
	}

	gcry_sexp_release (ssig);
	return CKR_OK;
}

CK_RV
gkm_dsa_mechanism_verify (gcry_sexp_t sexp, CK_BYTE_PTR data, CK_ULONG n_data,
                          CK_BYTE_PTR signature, CK_ULONG n_signature)
{
	gcry_sexp_t ssig, splain;
	gcry_error_t gcry;
	gcry_mpi_t mpi, mpi2;

	g_return_val_if_fail (sexp, CKR_GENERAL_ERROR);
	g_return_val_if_fail (signature, CKR_ARGUMENTS_BAD);
	g_return_val_if_fail (data, CKR_ARGUMENTS_BAD);

	if (n_data != 20)
		return CKR_DATA_LEN_RANGE;
	if (n_signature != 40)
		return CKR_SIGNATURE_LEN_RANGE;

	/* Prepare the input s-expressions */
	gcry = gcry_mpi_scan (&mpi, GCRYMPI_FMT_USG, data, n_data, NULL);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	gcry = gcry_sexp_build (&splain, NULL, "(data (flags raw) (value %m))", mpi);
	gcry_mpi_release (mpi);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	gcry = gcry_mpi_scan (&mpi, GCRYMPI_FMT_USG, signature, 20, NULL);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	gcry = gcry_mpi_scan (&mpi2, GCRYMPI_FMT_USG, signature + 20, 20, NULL);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);
	gcry = gcry_sexp_build (&ssig, NULL, "(sig-val (dsa (r %m) (s %m)))", mpi, mpi2);
	gcry_mpi_release (mpi);
	gcry_mpi_release (mpi2);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	/* Do the magic */
	gcry = gcry_pk_verify (ssig, splain, sexp);
	gcry_sexp_release (splain);
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
