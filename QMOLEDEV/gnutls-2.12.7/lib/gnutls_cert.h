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

#ifndef GNUTLS_CERT_H
#define GNUTLS_CERT_H

#include <gnutls_pk.h>
#include "x509/x509_int.h"
#include <gnutls/openpgp.h>

#define MAX_PUBLIC_PARAMS_SIZE 4        /* ok for RSA and DSA */

/* parameters should not be larger than this limit */
#define DSA_PUBLIC_PARAMS 4
#define RSA_PUBLIC_PARAMS 2

typedef struct gnutls_cert
{
  /* the size of params depends on the public
   * key algorithm
   * RSA: [0] is modulus
   *      [1] is public exponent
   * DSA: [0] is p
   *      [1] is q
   *      [2] is g
   *      [3] is public key
   */
  bigint_t params[MAX_PUBLIC_PARAMS_SIZE];
  int params_size;              /* holds the size of MPI params */

  gnutls_pk_algorithm_t subject_pk_algorithm;

  unsigned int key_usage;       /* bits from KEY_*
                                 */

  unsigned int version;
  /* holds the type (PGP, X509)
   */
  gnutls_certificate_type_t cert_type;
  gnutls_sign_algorithm_t sign_algo;

  gnutls_datum_t raw;

#ifdef ENABLE_OPENPGP
  int use_subkey;
  uint8_t subkey_id[GNUTLS_OPENPGP_KEYID_SIZE];
#endif
} gnutls_cert;

/* because gnutls_session_t is not defined when this file is included */
struct gnutls_session_int;

typedef enum ConvFlags
{
  CERT_NO_COPY = 2,
  CERT_ONLY_PUBKEY = 4,
  CERT_ONLY_EXTENSIONS = 16
} ConvFlags;

int _gnutls_x509_raw_cert_to_gcert (gnutls_cert * gcert,
                                    const gnutls_datum_t * derCert,
                                    int flags);
int _gnutls_x509_crt_to_gcert (gnutls_cert * gcert, gnutls_x509_crt_t cert,
                               unsigned int flags);

void _gnutls_gcert_deinit (gnutls_cert * cert);

int _gnutls_selected_cert_supported_kx (struct gnutls_session_int *session,
                                        gnutls_kx_algorithm_t ** alg,
                                        int *alg_size);

#endif
