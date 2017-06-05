/*
 * Copyright (C) 2000, 2001, 2004, 2005, 2007, 2008, 2010 Free Software
 * Foundation, Inc.
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

/* This file handles all the internal functions that cope with hashes
 * and HMACs.
 */

#include <gnutls_int.h>
#include <gnutls_hash_int.h>
#include <gnutls_errors.h>

static int
digest_length (gnutls_digest_algorithm_t algo)
{
  switch (algo)
    {
    case GNUTLS_DIG_NULL:
      return 0;
    case GNUTLS_DIG_MD5:
    case GNUTLS_DIG_MD2:
      return 16;
    case GNUTLS_DIG_SHA1:
    case GNUTLS_DIG_RMD160:
      return 20;
    case GNUTLS_DIG_SHA256:
      return 32;
    case GNUTLS_DIG_SHA384:
      return 48;
    case GNUTLS_DIG_SHA512:
      return 64;
    case GNUTLS_DIG_SHA224:
      return 28;
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }
}

int
_gnutls_hash_init (digest_hd_st * dig, gnutls_digest_algorithm_t algorithm)
{
  int result;
  const gnutls_crypto_digest_st *cc = NULL;

  dig->algorithm = algorithm;

  /* check if a digest has been registered 
   */
  cc = _gnutls_get_crypto_digest (algorithm);
  if (cc != NULL)
    {
      if (cc->init (algorithm, &dig->handle) < 0)
        {
          gnutls_assert ();
          return GNUTLS_E_HASH_FAILED;
        }

      dig->hash = cc->hash;
      dig->copy = cc->copy;
      dig->output = cc->output;
      dig->deinit = cc->deinit;

      return 0;
    }

  result = _gnutls_digest_ops.init (algorithm, &dig->handle);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  dig->hash = _gnutls_digest_ops.hash;
  dig->copy = _gnutls_digest_ops.copy;
  dig->output = _gnutls_digest_ops.output;
  dig->deinit = _gnutls_digest_ops.deinit;

  return 0;
}

/* returns the output size of the given hash/mac algorithm
 */
int
_gnutls_hash_get_algo_len (gnutls_digest_algorithm_t algorithm)
{
  return digest_length (algorithm);
}

int
_gnutls_hash (digest_hd_st * handle, const void *text, size_t textlen)
{
  if (textlen > 0)
    {
      handle->hash (handle->handle, text, textlen);
    }
  return 0;
}

int
_gnutls_hash_copy (digest_hd_st * dst, digest_hd_st * src)
{

  memset (dst, 0, sizeof (*dst));
  dst->algorithm = src->algorithm;

  dst->hash = src->hash;
  dst->copy = src->copy;
  dst->output = src->output;
  dst->deinit = src->deinit;

  return src->copy (&dst->handle, src->handle);
}

/* when the current output is needed without calling deinit
 */
void
_gnutls_hash_output (digest_hd_st * handle, void *digest)
{
  size_t maclen;

  maclen = _gnutls_hash_get_algo_len (handle->algorithm);

  if (digest != NULL)
    {
      handle->output (handle->handle, digest, maclen);
    }
}

void
_gnutls_hash_deinit (digest_hd_st * handle, void *digest)
{
  if (handle->handle == NULL)
    {
      return;
    }

  if (digest != NULL)
    _gnutls_hash_output (handle, digest);

  handle->deinit (handle->handle);
  handle->handle = NULL;
}

int
_gnutls_hash_fast (gnutls_digest_algorithm_t algorithm,
                   const void *text, size_t textlen, void *digest)
{
  digest_hd_st dig;
  int ret;

  ret = _gnutls_hash_init (&dig, algorithm);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_hash (&dig, text, textlen);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (&dig, NULL);
      return ret;
    }

  _gnutls_hash_deinit (&dig, digest);
  return 0;
}


/* HMAC interface */

int
_gnutls_hmac_get_algo_len (gnutls_mac_algorithm_t algorithm)
{
  return digest_length (algorithm);
}

int
_gnutls_hmac_fast (gnutls_mac_algorithm_t algorithm, const void *key,
                   int keylen, const void *text, size_t textlen, void *digest)
{
  digest_hd_st dig;
  int ret;

  ret = _gnutls_hmac_init (&dig, algorithm, key, keylen);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_hmac (&dig, text, textlen);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_hmac_deinit (&dig, NULL);
      return ret;
    }

  _gnutls_hmac_deinit (&dig, digest);
  return 0;
}

int
_gnutls_hmac_init (digest_hd_st * dig, gnutls_mac_algorithm_t algorithm,
                   const void *key, int keylen)
{
  int result;
  const gnutls_crypto_mac_st *cc = NULL;

  dig->algorithm = algorithm;
  dig->key = key;
  dig->keysize = keylen;

  /* check if a digest has been registered 
   */
  cc = _gnutls_get_crypto_mac (algorithm);
  if (cc != NULL)
    {
      if (cc->init (algorithm, &dig->handle) < 0)
        {
          gnutls_assert ();
          return GNUTLS_E_HASH_FAILED;
        }

      if (cc->setkey (dig->handle, key, keylen) < 0)
        {
          gnutls_assert ();
          cc->deinit (dig->handle);
          return GNUTLS_E_HASH_FAILED;
        }

      dig->hash = cc->hash;
      dig->output = cc->output;
      dig->deinit = cc->deinit;

      return 0;
    }

  result = _gnutls_mac_ops.init (algorithm, &dig->handle);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  dig->hash = _gnutls_mac_ops.hash;
  dig->output = _gnutls_mac_ops.output;
  dig->deinit = _gnutls_mac_ops.deinit;

  if (_gnutls_mac_ops.setkey (dig->handle, key, keylen) < 0)
    {
      gnutls_assert();
      dig->deinit(dig->handle);
      return GNUTLS_E_HASH_FAILED;
    }

  return 0;
}

int
_gnutls_hmac (digest_hd_st * handle, const void *text, size_t textlen)
{
  if (textlen > 0)
    {
      return handle->hash (handle->handle, text, textlen);
    }
  return 0;
}

void
_gnutls_hmac_output (digest_hd_st * handle, void *digest)
{
  int maclen;

  maclen = _gnutls_hmac_get_algo_len (handle->algorithm);

  if (digest != NULL)
    {
      handle->output (handle->handle, digest, maclen);
    }
}

void
_gnutls_hmac_deinit (digest_hd_st * handle, void *digest)
{
  if (handle->handle == NULL)
    {
      return;
    }

  if (digest)
    _gnutls_hmac_output (handle, digest);

  handle->deinit (handle->handle);
  handle->handle = NULL;
}

inline static int
get_padsize (gnutls_mac_algorithm_t algorithm)
{
  switch (algorithm)
    {
    case GNUTLS_MAC_MD5:
      return 48;
    case GNUTLS_MAC_SHA1:
      return 40;
    default:
      return 0;
    }
}


/* Special functions for SSL3 MAC
 */

int
_gnutls_mac_init_ssl3 (digest_hd_st * ret, gnutls_mac_algorithm_t algorithm,
                       void *key, int keylen)
{
  opaque ipad[48];
  int padsize, result;

  padsize = get_padsize (algorithm);
  if (padsize == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_HASH_FAILED;
    }

  memset (ipad, 0x36, padsize);

  result = _gnutls_hash_init (ret, algorithm);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  ret->key = key;
  ret->keysize = keylen;

  if (keylen > 0)
    _gnutls_hash (ret, key, keylen);
  _gnutls_hash (ret, ipad, padsize);

  return 0;
}

void
_gnutls_mac_deinit_ssl3 (digest_hd_st * handle, void *digest)
{
  opaque ret[MAX_HASH_SIZE];
  digest_hd_st td;
  opaque opad[48];
  int padsize;
  int block, rc;

  padsize = get_padsize (handle->algorithm);
  if (padsize == 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (handle, NULL);
      return;
    }

  memset (opad, 0x5C, padsize);

  rc = _gnutls_hash_init (&td, handle->algorithm);
  if (rc < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (handle, NULL);
      return;
    }

  if (handle->keysize > 0)
    _gnutls_hash (&td, handle->key, handle->keysize);

  _gnutls_hash (&td, opad, padsize);
  block = _gnutls_hmac_get_algo_len (handle->algorithm);
  _gnutls_hash_deinit (handle, ret);    /* get the previous hash */
  _gnutls_hash (&td, ret, block);

  _gnutls_hash_deinit (&td, digest);

  return;
}

void
_gnutls_mac_deinit_ssl3_handshake (digest_hd_st * handle,
                                   void *digest, opaque * key,
                                   uint32_t key_size)
{
  opaque ret[MAX_HASH_SIZE];
  digest_hd_st td;
  opaque opad[48];
  opaque ipad[48];
  int padsize;
  int block, rc;

  padsize = get_padsize (handle->algorithm);
  if (padsize == 0)
    {
      gnutls_assert ();
      return;
    }

  memset (opad, 0x5C, padsize);
  memset (ipad, 0x36, padsize);

  rc = _gnutls_hash_init (&td, handle->algorithm);
  if (rc < 0)
    {
      gnutls_assert ();
      return;
    }

  if (key_size > 0)
    _gnutls_hash (&td, key, key_size);

  _gnutls_hash (&td, opad, padsize);
  block = _gnutls_hmac_get_algo_len (handle->algorithm);

  if (key_size > 0)
    _gnutls_hash (handle, key, key_size);
  _gnutls_hash (handle, ipad, padsize);
  _gnutls_hash_deinit (handle, ret);    /* get the previous hash */

  _gnutls_hash (&td, ret, block);

  _gnutls_hash_deinit (&td, digest);

  return;
}

static int
ssl3_sha (int i, opaque * secret, int secret_len,
          opaque * rnd, int rnd_len, void *digest)
{
  int j, ret;
  opaque text1[26];

  digest_hd_st td;

  for (j = 0; j < i + 1; j++)
    {
      text1[j] = 65 + i;        /* A==65 */
    }

  ret = _gnutls_hash_init (&td, GNUTLS_MAC_SHA1);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_hash (&td, text1, i + 1);
  _gnutls_hash (&td, secret, secret_len);
  _gnutls_hash (&td, rnd, rnd_len);

  _gnutls_hash_deinit (&td, digest);
  return 0;
}

static int
ssl3_md5 (int i, opaque * secret, int secret_len,
          opaque * rnd, int rnd_len, void *digest)
{
  opaque tmp[MAX_HASH_SIZE];
  digest_hd_st td;
  int ret;

  ret = _gnutls_hash_init (&td, GNUTLS_MAC_MD5);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_hash (&td, secret, secret_len);

  ret = ssl3_sha (i, secret, secret_len, rnd, rnd_len, tmp);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (&td, digest);
      return ret;
    }

  _gnutls_hash (&td, tmp, _gnutls_hash_get_algo_len (GNUTLS_MAC_SHA1));

  _gnutls_hash_deinit (&td, digest);
  return 0;
}

int
_gnutls_ssl3_hash_md5 (const void *first, int first_len,
                       const void *second, int second_len,
                       int ret_len, opaque * ret)
{
  opaque digest[MAX_HASH_SIZE];
  digest_hd_st td;
  int block = _gnutls_hash_get_algo_len (GNUTLS_MAC_MD5);
  int rc;

  rc = _gnutls_hash_init (&td, GNUTLS_MAC_MD5);
  if (rc < 0)
    {
      gnutls_assert ();
      return rc;
    }

  _gnutls_hash (&td, first, first_len);
  _gnutls_hash (&td, second, second_len);

  _gnutls_hash_deinit (&td, digest);

  if (ret_len > block)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  memcpy (ret, digest, ret_len);

  return 0;

}

int
_gnutls_ssl3_generate_random (void *secret, int secret_len,
                              void *rnd, int rnd_len,
                              int ret_bytes, opaque * ret)
{
  int i = 0, copy, output_bytes;
  opaque digest[MAX_HASH_SIZE];
  int block = _gnutls_hash_get_algo_len (GNUTLS_MAC_MD5);
  int result, times;

  output_bytes = 0;
  do
    {
      output_bytes += block;
    }
  while (output_bytes < ret_bytes);

  times = output_bytes / block;

  for (i = 0; i < times; i++)
    {

      result = ssl3_md5 (i, secret, secret_len, rnd, rnd_len, digest);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }

      if ((1 + i) * block < ret_bytes)
        {
          copy = block;
        }
      else
        {
          copy = ret_bytes - (i) * block;
        }

      memcpy (&ret[i * block], digest, copy);
    }

  return 0;
}
