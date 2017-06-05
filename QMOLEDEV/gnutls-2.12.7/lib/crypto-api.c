/*
 * Copyright (C) 2000, 2004, 2005, 2008, 2010 Free Software Foundation,
 * Inc.
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

/**
 * gnutls_cipher_init:
 * @handle: is a #gnutls_cipher_hd_t structure.
 * @cipher: the encryption algorithm to use
 * @key: The key to be used for encryption
 * @iv: The IV to use (if not applicable set NULL)
 *
 * This function will initialize an context that can be used for
 * encryption/decryption of data. This will effectively use the
 * current crypto backend in use by gnutls or the cryptographic
 * accelerator in use.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_cipher_init (gnutls_cipher_hd_t * handle,
                    gnutls_cipher_algorithm_t cipher,
                    const gnutls_datum_t * key, const gnutls_datum_t * iv)
{
  *handle = gnutls_malloc (sizeof (cipher_hd_st));
  if (*handle == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return _gnutls_cipher_init (((cipher_hd_st *) * handle), cipher, key, iv);
}

/**
 * gnutls_cipher_encrypt:
 * @handle: is a #gnutls_cipher_hd_t structure.
 * @text: the data to encrypt
 * @textlen: The length of data to encrypt
 *
 * This function will encrypt the given data using the algorithm
 * specified by the context.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_cipher_encrypt (gnutls_cipher_hd_t handle, void *text, size_t textlen)
{
  return _gnutls_cipher_encrypt ((cipher_hd_st *) handle, text, textlen);
}

/**
 * gnutls_cipher_decrypt:
 * @handle: is a #gnutls_cipher_hd_t structure.
 * @ciphertext: the data to encrypt
 * @ciphertextlen: The length of data to encrypt
 *
 * This function will decrypt the given data using the algorithm
 * specified by the context.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_cipher_decrypt (gnutls_cipher_hd_t handle, void *ciphertext,
                       size_t ciphertextlen)
{
  return _gnutls_cipher_decrypt ((cipher_hd_st *) handle, ciphertext,
                                 ciphertextlen);
}

/**
 * gnutls_cipher_encrypt2:
 * @handle: is a #gnutls_cipher_hd_t structure.
 * @text: the data to encrypt
 * @textlen: The length of data to encrypt
 * @ciphertext: the encrypted data
 * @ciphertextlen: The available length for encrypted data
 *
 * This function will encrypt the given data using the algorithm
 * specified by the context.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_cipher_encrypt2 (gnutls_cipher_hd_t handle, void *text, size_t textlen,
                        void *ciphertext, size_t ciphertextlen)
{
  return _gnutls_cipher_encrypt2 ((cipher_hd_st *) handle, text, textlen,
                                  ciphertext, ciphertextlen);
}

/**
 * gnutls_cipher_decrypt2:
 * @handle: is a #gnutls_cipher_hd_t structure.
 * @ciphertext: the data to encrypt
 * @ciphertextlen: The length of data to encrypt
 * @text: the decrypted data
 * @textlen: The available length for decrypted data
 *
 * This function will decrypt the given data using the algorithm
 * specified by the context.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_cipher_decrypt2 (gnutls_cipher_hd_t handle, const void *ciphertext,
                        size_t ciphertextlen, void *text, size_t textlen)
{
  return _gnutls_cipher_decrypt2 ((cipher_hd_st *) handle, ciphertext,
                                  ciphertextlen, text, textlen);
}

/**
 * gnutls_cipher_deinit:
 * @handle: is a #gnutls_cipher_hd_t structure.
 *
 * This function will deinitialize all resources occupied by the given
 * encryption context.
 *
 * Since: 2.10.0
 **/
void
gnutls_cipher_deinit (gnutls_cipher_hd_t handle)
{
  _gnutls_cipher_deinit ((cipher_hd_st *) handle);
  gnutls_free (handle);
}


/* HMAC */

/**
 * gnutls_hmac_init:
 * @dig: is a #gnutls_hmac_hd_t structure.
 * @algorithm: the HMAC algorithm to use
 * @key: The key to be used for encryption
 * @keylen: The length of the key
 *
 * This function will initialize an context that can be used to
 * produce a Message Authentication Code (MAC) of data.  This will
 * effectively use the current crypto backend in use by gnutls or the
 * cryptographic accelerator in use.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hmac_init (gnutls_hmac_hd_t * dig,
                  gnutls_digest_algorithm_t algorithm,
                  const void *key, size_t keylen)
{
  *dig = gnutls_malloc (sizeof (digest_hd_st));
  if (*dig == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return _gnutls_hmac_init (((digest_hd_st *) * dig), algorithm, key, keylen);
}

/**
 * gnutls_hmac:
 * @handle: is a #gnutls_cipher_hd_t structure.
 * @text: the data to hash
 * @textlen: The length of data to hash
 *
 * This function will hash the given data using the algorithm
 * specified by the context.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hmac (gnutls_hmac_hd_t handle, const void *text, size_t textlen)
{
  return _gnutls_hmac ((digest_hd_st *) handle, text, textlen);
}

/**
 * gnutls_hmac_output:
 * @handle: is a #gnutls_hmac_hd_t structure.
 * @digest: is the output value of the MAC
 *
 * This function will output the current MAC value.
 *
 * Since: 2.10.0
 **/
void
gnutls_hmac_output (gnutls_hmac_hd_t handle, void *digest)
{
  _gnutls_hmac_output ((digest_hd_st *) handle, digest);
}

/**
 * gnutls_hmac_deinit:
 * @handle: is a #gnutls_hmac_hd_t structure.
 * @digest: is the output value of the MAC
 *
 * This function will deinitialize all resources occupied by
 * the given hmac context.
 *
 * Since: 2.10.0
 **/
void
gnutls_hmac_deinit (gnutls_hmac_hd_t handle, void *digest)
{
  _gnutls_hmac_deinit ((digest_hd_st *) handle, digest);
  gnutls_free (handle);
}

/**
 * gnutls_hmac_get_len:
 * @algorithm: the hmac algorithm to use
 *
 * This function will return the length of the output data
 * of the given hmac algorithm.
 *
 * Returns: The length or zero on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hmac_get_len (gnutls_mac_algorithm_t algorithm)
{
  return _gnutls_hmac_get_algo_len (algorithm);
}

/**
 * gnutls_hmac_fast:
 * @algorithm: the hash algorithm to use
 * @key: the key to use
 * @keylen: The length of the key
 * @text: the data to hash
 * @textlen: The length of data to hash
 * @digest: is the output value of the hash
 *
 * This convenience function will hash the given data and return output
 * on a single call.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hmac_fast (gnutls_mac_algorithm_t algorithm,
                  const void *key, size_t keylen,
                  const void *text, size_t textlen, void *digest)
{
  return _gnutls_hmac_fast (algorithm, key, keylen, text, textlen, digest);
}

/* HASH */

/**
 * gnutls_hash_init:
 * @dig: is a #gnutls_hash_hd_t structure.
 * @algorithm: the hash algorithm to use
 *
 * This function will initialize an context that can be used to
 * produce a Message Digest of data.  This will effectively use the
 * current crypto backend in use by gnutls or the cryptographic
 * accelerator in use.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hash_init (gnutls_hash_hd_t * dig, gnutls_digest_algorithm_t algorithm)
{
  *dig = gnutls_malloc (sizeof (digest_hd_st));
  if (*dig == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return _gnutls_hash_init (((digest_hd_st *) * dig), algorithm);
}

/**
 * gnutls_hash:
 * @handle: is a #gnutls_cipher_hd_t structure.
 * @text: the data to hash
 * @textlen: The length of data to hash
 *
 * This function will hash the given data using the algorithm
 * specified by the context.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hash (gnutls_hash_hd_t handle, const void *text, size_t textlen)
{
  return _gnutls_hash ((digest_hd_st *) handle, text, textlen);
}

/**
 * gnutls_hash_output:
 * @handle: is a #gnutls_hash_hd_t structure.
 * @digest: is the output value of the hash
 *
 * This function will output the current hash value.
 *
 * Since: 2.10.0
 **/
void
gnutls_hash_output (gnutls_hash_hd_t handle, void *digest)
{
  _gnutls_hash_output ((digest_hd_st *) handle, digest);
}

/**
 * gnutls_hash_deinit:
 * @handle: is a #gnutls_hash_hd_t structure.
 * @digest: is the output value of the hash
 *
 * This function will deinitialize all resources occupied by
 * the given hash context.
 *
 * Since: 2.10.0
 **/
void
gnutls_hash_deinit (gnutls_hash_hd_t handle, void *digest)
{
  _gnutls_hash_deinit ((digest_hd_st *) handle, digest);
  gnutls_free (handle);
}

/**
 * gnutls_hash_get_len:
 * @algorithm: the hash algorithm to use
 *
 * This function will return the length of the output data
 * of the given hash algorithm.
 *
 * Returns: The length or zero on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hash_get_len (gnutls_digest_algorithm_t algorithm)
{
  return _gnutls_hash_get_algo_len (algorithm);
}

/**
 * gnutls_hash_fast:
 * @algorithm: the hash algorithm to use
 * @text: the data to hash
 * @textlen: The length of data to hash
 * @digest: is the output value of the hash
 *
 * This convenience function will hash the given data and return output
 * on a single call.
 *
 * Returns: Zero or a negative value on error.
 *
 * Since: 2.10.0
 **/
int
gnutls_hash_fast (gnutls_digest_algorithm_t algorithm,
                  const void *text, size_t textlen, void *digest)
{
  return _gnutls_hash_fast (algorithm, text, textlen, digest);
}
