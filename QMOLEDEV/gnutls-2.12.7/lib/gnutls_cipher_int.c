/*
 * Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_cipher_int.h>
#include <gnutls_datum.h>
#include <gnutls/crypto.h>
#include <crypto.h>

#define SR(x, cleanup) if ( (x)<0 ) { \
  gnutls_assert(); \
  ret = GNUTLS_E_INTERNAL_ERROR; \
  goto cleanup; \
  }

int
_gnutls_cipher_init (cipher_hd_st * handle, gnutls_cipher_algorithm_t cipher,
                     const gnutls_datum_t * key, const gnutls_datum_t * iv)
{
  int ret = GNUTLS_E_INTERNAL_ERROR;
  const gnutls_crypto_cipher_st *cc = NULL;

  /* check if a cipher has been registered
   */
  cc = _gnutls_get_crypto_cipher (cipher);
  if (cc != NULL)
    {
      SR (cc->init (cipher, &handle->handle), cc_cleanup);
      SR (cc->setkey (handle->handle, key->data, key->size), cc_cleanup);

      handle->encrypt = cc->encrypt;
      handle->decrypt = cc->decrypt;
      handle->deinit = cc->deinit;

      if (iv && iv->data && iv->size && cc->setiv)
        SR (cc->setiv (handle->handle, iv->data, iv->size), cc_cleanup);
      return 0;
    }

  /* otherwise use generic cipher interface
   */
  ret = _gnutls_cipher_ops.init (cipher, &handle->handle);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_cipher_ops.setkey (handle->handle, key->data, key->size);
  if (ret < 0)
    {
      _gnutls_cipher_ops.deinit (handle->handle);
      gnutls_assert ();
      return ret;
    }

  handle->encrypt = _gnutls_cipher_ops.encrypt;
  handle->decrypt = _gnutls_cipher_ops.decrypt;
  handle->deinit = _gnutls_cipher_ops.deinit;

  if (iv && iv->data != NULL && iv->size > 0)
    _gnutls_cipher_ops.setiv (handle->handle, iv->data, iv->size);

  return 0;

cc_cleanup:

  if (handle->handle)
    cc->deinit (handle->handle);

  return ret;
}

int
_gnutls_cipher_encrypt (const cipher_hd_st * handle, void *text, int textlen)
{
  if (handle != NULL && handle->handle != NULL)
    {
      return handle->encrypt (handle->handle, text, textlen, text, textlen);
    }
  return 0;
}

int
_gnutls_cipher_decrypt (const cipher_hd_st * handle, void *ciphertext,
                        int ciphertextlen)
{
  if (handle != NULL && handle->handle != NULL)
    {
      return handle->decrypt (handle->handle, ciphertext, ciphertextlen,
                              ciphertext, ciphertextlen);
    }
  return 0;
}

int
_gnutls_cipher_encrypt2 (const cipher_hd_st * handle, const void *text,
                         int textlen, void *ciphertext, int ciphertextlen)
{
  if (handle != NULL && handle->handle != NULL)
    {
      return handle->encrypt (handle->handle, text, textlen, ciphertext,
                              ciphertextlen);
    }
  return 0;
}

int
_gnutls_cipher_decrypt2 (const cipher_hd_st * handle, const void *ciphertext,
                         int ciphertextlen, void *text, int textlen)
{
  if (handle != NULL && handle->handle != NULL)
    {
      return handle->decrypt (handle->handle, ciphertext, ciphertextlen,
                              text, textlen);
    }
  return 0;
}

void
_gnutls_cipher_deinit (cipher_hd_st * handle)
{
  if (handle != NULL && handle->handle != NULL)
    {
      handle->deinit (handle->handle);
      handle->handle = NULL;
    }
}
