/*
 * Copyright (C) 2008, 2010 Free Software Foundation, Inc.
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

/* Here lie everything that has to do with large numbers, libgcrypt and
 * other stuff that didn't fit anywhere else.
 */

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_cipher_int.h>
#include <gcrypt.h>

/* Functions that refer to the libgcrypt library.
 */

static int
wrap_gcry_cipher_init (gnutls_cipher_algorithm_t algo, void **ctx)
{
  int err;

  switch (algo)
    {
    case GNUTLS_CIPHER_AES_128_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_AES128,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;

    case GNUTLS_CIPHER_AES_192_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_AES192,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;

    case GNUTLS_CIPHER_AES_256_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_AES256,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;

    case GNUTLS_CIPHER_3DES_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_3DES,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;

    case GNUTLS_CIPHER_DES_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_DES,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;

    case GNUTLS_CIPHER_ARCFOUR_128:
    case GNUTLS_CIPHER_ARCFOUR_40:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_ARCFOUR,
                          GCRY_CIPHER_MODE_STREAM, 0);
      break;

    case GNUTLS_CIPHER_RC2_40_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_RFC2268_40,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;

#ifdef	ENABLE_CAMELLIA
    case GNUTLS_CIPHER_CAMELLIA_128_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_CAMELLIA128,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;

    case GNUTLS_CIPHER_CAMELLIA_256_CBC:
      err =
        gcry_cipher_open ((gcry_cipher_hd_t *) ctx, GCRY_CIPHER_CAMELLIA256,
                          GCRY_CIPHER_MODE_CBC, 0);
      break;
#endif
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (err == 0)
    return 0;

  gnutls_assert ();
  return GNUTLS_E_ENCRYPTION_FAILED;
}

static int
wrap_gcry_cipher_setkey (void *ctx, const void *key, size_t keysize)
{
  gcry_cipher_setkey (ctx, key, keysize);
  return 0;
}

static int
wrap_gcry_cipher_setiv (void *ctx, const void *iv, size_t ivsize)
{
  gcry_cipher_setiv (ctx, iv, ivsize);
  return 0;
}

static int
wrap_gcry_cipher_decrypt (void *ctx, const void *encr, size_t encrsize,
                          void *plain, size_t plainsize)
{
  int err;

  err = gcry_cipher_decrypt (ctx, plain, plainsize, encr, encrsize);
  if (err == 0)
    return 0;

  gnutls_assert ();
  return GNUTLS_E_ENCRYPTION_FAILED;
}

static int
wrap_gcry_cipher_encrypt (void *ctx, const void *plain, size_t plainsize,
                          void *encr, size_t encrsize)
{
  int err;

  err = gcry_cipher_encrypt (ctx, encr, encrsize, plain, plainsize);
  if (err == 0)
    return 0;

  gnutls_assert ();
  return GNUTLS_E_ENCRYPTION_FAILED;
}

static void
wrap_gcry_cipher_close (void *h)
{
  gcry_cipher_close (h);
}


gnutls_crypto_cipher_st _gnutls_cipher_ops = {
  .init = wrap_gcry_cipher_init,
  .setkey = wrap_gcry_cipher_setkey,
  .setiv = wrap_gcry_cipher_setiv,
  .encrypt = wrap_gcry_cipher_encrypt,
  .decrypt = wrap_gcry_cipher_decrypt,
  .deinit = wrap_gcry_cipher_close,
};
