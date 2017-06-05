/*
 * Copyright (C) 2002, 2003, 2004, 2005, 2008, 2010 Free Software
 * Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS-EXTRA.
 *
 * GnuTLS-extra is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 * GnuTLS-extra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GnuTLS-EXTRA; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 */

#ifndef GNUTLS_COMPAT8_H
#define GNUTLS_COMPAT8_H

/* Extra definitions */
#include <gnutls/openssl.h>

int gnutls_x509_extract_dn (const gnutls_datum_t *, gnutls_x509_dn *);
int gnutls_x509_extract_dn_string (const gnutls_datum_t * idn,
                                   char *buf, unsigned int sizeof_buf);
int gnutls_x509_extract_certificate_dn (const gnutls_datum_t *,
                                        gnutls_x509_dn *);
int gnutls_x509_extract_certificate_dn_string (char *buf,
                                               unsigned int sizeof_buf,
                                               const gnutls_datum_t * cert,
                                               int issuer);
int gnutls_x509_extract_certificate_issuer_dn (const gnutls_datum_t *,
                                               gnutls_x509_dn *);
int gnutls_x509_extract_certificate_version (const gnutls_datum_t *);
int gnutls_x509_extract_certificate_serial (const gnutls_datum_t * cert,
                                            char *result, int *result_size);
time_t gnutls_x509_extract_certificate_activation_time (const gnutls_datum_t
                                                        *);
time_t gnutls_x509_extract_certificate_expiration_time (const gnutls_datum_t
                                                        *);
int gnutls_x509_extract_certificate_subject_alt_name (const gnutls_datum_t
                                                      *, int seq, char *,
                                                      int *);
int gnutls_x509_pkcs7_extract_certificate (const gnutls_datum_t *
                                           pkcs7_struct, int indx,
                                           char *certificate,
                                           int *certificate_size);
int gnutls_x509_extract_certificate_pk_algorithm (const gnutls_datum_t *
                                                  cert, int *bits);
int gnutls_x509_extract_certificate_ca_status (const gnutls_datum_t * cert);
int gnutls_x509_extract_key_pk_algorithm (const gnutls_datum_t * key);

int gnutls_x509_verify_certificate (const gnutls_datum_t * cert_list,
                                    int cert_list_length,
                                    const gnutls_datum_t * CA_list,
                                    int CA_list_length,
                                    const gnutls_datum_t * CRL_list,
                                    int CRL_list_length);

#define gnutls_x509_fingerprint gnutls_fingerprint
#define gnutls_x509_certificate_format gnutls_x509_crt_fmt_t

#define gnutls_certificate_set_rsa_params gnutls_certificate_set_rsa_export_params

#endif
