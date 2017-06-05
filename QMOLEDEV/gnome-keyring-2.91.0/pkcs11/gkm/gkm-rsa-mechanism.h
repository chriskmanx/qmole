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

#ifndef GKM_RSA_MECHANISM_H_
#define GKM_RSA_MECHANISM_H_

#include "gkm-crypto.h"
#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

#include <glib.h>

#include <gcrypt.h>

static const CK_MECHANISM_TYPE GKM_RSA_MECHANISMS[] = {
	CKM_RSA_PKCS,
	CKM_RSA_X_509
};

CK_RV                    gkm_rsa_mechanism_encrypt                     (gcry_sexp_t sexp,
                                                                        EggPadding padding,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR encrypted,
                                                                        CK_ULONG_PTR n_encrypted);

CK_RV                    gkm_rsa_mechanism_decrypt                     (gcry_sexp_t sexp,
                                                                        EggPadding padding,
                                                                        CK_BYTE_PTR encrypted,
                                                                        CK_ULONG n_encrypted,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG_PTR n_data);

CK_RV                    gkm_rsa_mechanism_sign                        (gcry_sexp_t sexp,
                                                                        EggPadding padding,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG_PTR n_signature);

CK_RV                    gkm_rsa_mechanism_verify                      (gcry_sexp_t sexp,
                                                                        EggPadding padding,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG n_signature);

#endif /* GKM_RSA_MECHANISM_H_ */
