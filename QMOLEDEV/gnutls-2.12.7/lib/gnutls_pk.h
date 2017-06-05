/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2008, 2010 Free
 * Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

#ifndef GNUTLS_PK_H
#define GNUTLS_PK_H

extern int crypto_pk_prio;
extern gnutls_crypto_pk_st _gnutls_pk_ops;

#define _gnutls_pk_encrypt( algo, ciphertext, plaintext, params) _gnutls_pk_ops.encrypt( algo, ciphertext, plaintext, params)
#define _gnutls_pk_decrypt( algo, ciphertext, plaintext, params) _gnutls_pk_ops.decrypt( algo, ciphertext, plaintext, params)
#define _gnutls_pk_sign( algo, sig, data, params) _gnutls_pk_ops.sign( algo, sig, data, params)
#define _gnutls_pk_verify( algo, data, sig, params) _gnutls_pk_ops.verify( algo, data, sig, params)

inline static int
_gnutls_pk_fixup (gnutls_pk_algorithm_t algo, gnutls_direction_t direction,
                  gnutls_pk_params_st * params)
{
  if (_gnutls_pk_ops.pk_fixup_private_params)
    return _gnutls_pk_ops.pk_fixup_private_params (algo, direction, params);
  return 0;
}

int _gnutls_pk_params_copy (gnutls_pk_params_st * dst, bigint_t * params,
                            int params_len);

int _gnutls_rsa_generate_params (bigint_t * resarr, unsigned int *resarr_len,
                                 int bits);
int _gnutls_dsa_generate_params (bigint_t * resarr, unsigned int *resarr_len,
                                 int bits);

/* The internal PK interface */
int _gnutls_pkcs1_rsa_encrypt (gnutls_datum_t * ciphertext,
                               const gnutls_datum_t * plaintext,
                               bigint_t * params, unsigned params_len,
                               unsigned btype);
int _gnutls_dsa_sign (gnutls_datum_t * signature,
                      const gnutls_datum_t * plaintext, bigint_t * params,
                      unsigned params_len);
int _gnutls_pkcs1_rsa_decrypt (gnutls_datum_t * plaintext,
                               const gnutls_datum_t * ciphertext,
                               bigint_t * params, unsigned params_len,
                               unsigned btype);
int _gnutls_rsa_verify (const gnutls_datum_t * vdata,
                        const gnutls_datum_t * ciphertext, bigint_t * params,
                        int params_len, int btype);
int _gnutls_dsa_verify (const gnutls_datum_t * vdata,
                        const gnutls_datum_t * sig_value, bigint_t * params,
                        int params_len);

int
_gnutls_encode_ber_rs (gnutls_datum_t * sig_value, bigint_t r, bigint_t s);

int
_gnutls_decode_ber_rs (const gnutls_datum_t * sig_value, bigint_t * r,
                       bigint_t * s);

int _gnutls_calc_rsa_exp (bigint_t * params, unsigned int params_size);

int _gnutls_pk_get_hash_algorithm (gnutls_pk_algorithm_t pk,
                                   bigint_t * params, int params_size,
                                   gnutls_digest_algorithm_t * dig,
                                   unsigned int *mand);

#endif /* GNUTLS_PK_H */
