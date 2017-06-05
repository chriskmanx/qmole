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

#ifndef GKM_DSA_MECHANISM_H_
#define GKM_DSA_MECHANISM_H_

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

#include <glib.h>

#include <gcrypt.h>

static const CK_MECHANISM_TYPE GKM_DSA_MECHANISMS[] = {
	CKM_DSA
};

CK_RV                    gkm_dsa_mechanism_sign                        (gcry_sexp_t sexp,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG_PTR n_signature);

CK_RV                    gkm_dsa_mechanism_verify                      (gcry_sexp_t sexp,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG n_signature);

#endif /* GKM_DSA_MECHANISM_H_ */
