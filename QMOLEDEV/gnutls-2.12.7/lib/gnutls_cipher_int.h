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

#ifndef GNUTLS_CIPHER_INT
#define GNUTLS_CIPHER_INT

#include <gnutls/crypto.h>

extern int crypto_cipher_prio;
extern gnutls_crypto_cipher_st _gnutls_cipher_ops;

typedef int (*cipher_encrypt_func) (void *hd, const void *plaintext, size_t,
                                    void *ciphertext, size_t);
typedef int (*cipher_decrypt_func) (void *hd, const void *ciphertext, size_t,
                                    void *plaintext, size_t);
typedef void (*cipher_deinit_func) (void *hd);

typedef struct
{
  void *handle;
  cipher_encrypt_func encrypt;
  cipher_decrypt_func decrypt;
  cipher_deinit_func deinit;
} cipher_hd_st;

int _gnutls_cipher_init (cipher_hd_st *, gnutls_cipher_algorithm_t cipher,
                         const gnutls_datum_t * key,
                         const gnutls_datum_t * iv);
int _gnutls_cipher_encrypt (const cipher_hd_st * handle, void *text,
                            int textlen);
int _gnutls_cipher_decrypt (const cipher_hd_st * handle, void *ciphertext,
                            int ciphertextlen);
int _gnutls_cipher_encrypt2 (const cipher_hd_st * handle, const void *text,
                             int textlen, void *ciphertext,
                             int ciphertextlen);
int _gnutls_cipher_decrypt2 (const cipher_hd_st * handle,
                             const void *ciphertext, int ciphertextlen,
                             void *text, int textlen);
void _gnutls_cipher_deinit (cipher_hd_st * handle);

#endif /* GNUTLS_CIPHER_INT */
