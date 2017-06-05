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

/* This file provides is the backend hash/mac API for libgcrypt.
 */

#include <gnutls_int.h>
#include <gnutls_hash_int.h>
#include <gnutls_errors.h>
#include <gcrypt.h>

static int
wrap_gcry_mac_init (gnutls_mac_algorithm_t algo, void **ctx)
{
  int err;
  unsigned int flags = GCRY_MD_FLAG_HMAC;

  switch (algo)
    {
    case GNUTLS_MAC_MD5:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_MD5, flags);
      break;
    case GNUTLS_MAC_SHA1:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA1, flags);
      break;
    case GNUTLS_MAC_RMD160:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_RMD160, flags);
      break;
    case GNUTLS_MAC_MD2:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_MD2, flags);
      break;
    case GNUTLS_MAC_SHA256:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA256, flags);
      break;
    case GNUTLS_MAC_SHA384:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA384, flags);
      break;
    case GNUTLS_MAC_SHA512:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA512, flags);
      break;
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
wrap_gcry_md_setkey (void *ctx, const void *key, size_t keylen)
{
  return gcry_md_setkey ((gcry_md_hd_t) ctx, key, keylen);
}

static int
wrap_gcry_md_write (void *ctx, const void *text, size_t textsize)
{
  gcry_md_write (ctx, text, textsize);
  return GNUTLS_E_SUCCESS;
}

static int
wrap_gcry_md_copy (void **bhd, void *ahd)
{
  return gcry_md_copy ((gcry_md_hd_t *) bhd, (gcry_md_hd_t) ahd);
}

static void
wrap_gcry_md_close (void *hd)
{
  gcry_md_close (hd);
}

static int
wrap_gcry_hash_init (gnutls_mac_algorithm_t algo, void **ctx)
{
  int err;
  unsigned int flags = 0;

  switch (algo)
    {
    case GNUTLS_DIG_MD5:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_MD5, flags);
      break;
    case GNUTLS_DIG_SHA1:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA1, flags);
      break;
    case GNUTLS_DIG_RMD160:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_RMD160, flags);
      break;
    case GNUTLS_DIG_MD2:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_MD2, flags);
      break;
    case GNUTLS_DIG_SHA256:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA256, flags);
      break;
    case GNUTLS_DIG_SHA224:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA224, flags);
      break;
    case GNUTLS_DIG_SHA384:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA384, flags);
      break;
    case GNUTLS_DIG_SHA512:
      err = gcry_md_open ((gcry_md_hd_t *) ctx, GCRY_MD_SHA512, flags);
      break;
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
wrap_gcry_mac_output (void *src_ctx, void *digest, size_t digestsize)
{
  opaque *_digest = gcry_md_read (src_ctx, 0);

  if (_digest != NULL)
    {
      unsigned int len = gcry_md_get_algo_dlen (gcry_md_get_algo (src_ctx));

      if (len <= digestsize && digest != NULL)
        memcpy (digest, _digest, len);

      return 0;
    }

  gnutls_assert ();
  return GNUTLS_E_HASH_FAILED;
}


gnutls_crypto_mac_st _gnutls_mac_ops = {
  .init = wrap_gcry_mac_init,
  .setkey = wrap_gcry_md_setkey,
  .hash = wrap_gcry_md_write,
  .output = wrap_gcry_mac_output,
  .deinit = wrap_gcry_md_close,
};

gnutls_crypto_digest_st _gnutls_digest_ops = {
  .init = wrap_gcry_hash_init,
  .hash = wrap_gcry_md_write,
  .copy = wrap_gcry_md_copy,
  .output = wrap_gcry_mac_output,
  .deinit = wrap_gcry_md_close,
};
