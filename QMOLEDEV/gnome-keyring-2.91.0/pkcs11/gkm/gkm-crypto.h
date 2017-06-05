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

#ifndef GKM_CRYPTO_H_
#define GKM_CRYPTO_H_

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

#include "egg/egg-padding.h"

#include <glib.h>

#include <gcrypt.h>

void                     gkm_crypto_initialize                         (void);

CK_RV                    gkm_crypto_prepare                            (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        GkmObject *key);

CK_RV                    gkm_crypto_prepare_xsa                        (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        GkmObject *key);

CK_RV                    gkm_crypto_perform                            (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_ATTRIBUTE_TYPE method,
                                                                        CK_BYTE_PTR bufone,
                                                                        CK_ULONG n_bufone,
                                                                        CK_BYTE_PTR buftwo,
                                                                        CK_ULONG_PTR n_buftwo);

CK_RV                    gkm_crypto_encrypt                            (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR encrypted,
                                                                        CK_ULONG_PTR n_encrypted);

CK_RV                    gkm_crypto_encrypt_xsa                        (gcry_sexp_t sexp,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR encrypted,
                                                                        CK_ULONG_PTR n_encrypted);

CK_RV                    gkm_crypto_decrypt                            (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR encrypted,
                                                                        CK_ULONG n_encrypted,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG_PTR n_data);

CK_RV                    gkm_crypto_decrypt_xsa                        (gcry_sexp_t sexp,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR encrypted,
                                                                        CK_ULONG n_encrypted,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG_PTR n_data);

CK_RV                    gkm_crypto_sign                               (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG_PTR n_signature);

CK_RV                    gkm_crypto_sign_xsa                           (gcry_sexp_t sexp,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG_PTR n_signature);

CK_RV                    gkm_crypto_verify                             (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG n_signature);

CK_RV                    gkm_crypto_verify_xsa                         (gcry_sexp_t sexp,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        CK_BYTE_PTR signature,
                                                                        CK_ULONG n_signature);

CK_RV                    gkm_crypto_sexp_to_data                       (gcry_sexp_t sexp,
                                                                        guint bits,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG *n_data,
                                                                        EggPadding padding,
                                                                        ...) G_GNUC_NULL_TERMINATED;

CK_RV                    gkm_crypto_data_to_sexp                       (const gchar *format,
                                                                        guint nbits,
                                                                        EggPadding padding,
                                                                        CK_BYTE_PTR data,
                                                                        CK_ULONG n_data,
                                                                        gcry_sexp_t *sexp);

CK_RV                    gkm_crypto_generate_key_pair                  (GkmSession *session,
                                                                        CK_MECHANISM_TYPE mech,
                                                                        CK_ATTRIBUTE_PTR pub_atts,
                                                                        CK_ULONG n_pub_atts,
                                                                        CK_ATTRIBUTE_PTR priv_atts,
                                                                        CK_ULONG n_priv_atts,
                                                                        GkmObject **pub_key,
                                                                        GkmObject **priv_key);

CK_RV                    gkm_crypto_derive_key                         (GkmSession *session,
                                                                        CK_MECHANISM_PTR mech,
                                                                        GkmObject *base,
                                                                        CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        GkmObject **derived);

CK_RV                    gkm_crypto_wrap_key                           (GkmSession *session,
                                                                        CK_MECHANISM_PTR mech,
                                                                        GkmObject *wrapper,
                                                                        GkmObject *wrapped,
                                                                        CK_BYTE_PTR output,
                                                                        CK_ULONG_PTR n_output);

CK_RV                    gkm_crypto_unwrap_key                         (GkmSession *session,
                                                                        CK_MECHANISM_PTR mech,
                                                                        GkmObject *wrapper,
                                                                        CK_VOID_PTR input,
                                                                        CK_ULONG n_input,
                                                                        CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        GkmObject **unwrapped);

gulong                   gkm_crypto_secret_key_length                  (CK_KEY_TYPE type);

#endif /* GKM_CRYPTO_H_ */
