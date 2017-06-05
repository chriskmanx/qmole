/*
 * Copyright (C) 2008, 2010 Free Software Foundation, Inc.
 *
 * Author: Simon Josefsson
 *
 * This file is part of GnuTLS-EXTRA.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <gnutls_int.h>
#include <gnutls/crypto.h>
#include <gnutls/extra.h>

#include <md5.h>
#include <hmac.h>

static int
md5init (gnutls_mac_algorithm_t mac, void **ctx)
{
  *ctx = gnutls_malloc (sizeof (struct md5_ctx));
  if (!*ctx)
    return GNUTLS_E_MEMORY_ERROR;
  md5_init_ctx (*ctx);
  return 0;
}

static int
md5hash (void *ctx, const void *text, size_t textsize)
{
  md5_process_bytes (text, textsize, ctx);
  return 0;
}

static int
md5copy (void **dst_ctx, void *src_ctx)
{
  *dst_ctx = gnutls_malloc (sizeof (struct md5_ctx));
  if (!*dst_ctx)
    return GNUTLS_E_MEMORY_ERROR;
  memcpy (*dst_ctx, src_ctx, sizeof (struct md5_ctx));
  return 0;
}

static int
md5output (void *src_ctx, void *digest, size_t digestsize)
{
  char out[MD5_DIGEST_SIZE];
  md5_finish_ctx (src_ctx, out);
  memcpy (digest, out, digestsize);
  return 0;
}

static void
md5deinit (void *ctx)
{
  gnutls_free (ctx);
}

struct hmacctx
{
  char *data;
  size_t datasize;
  char *key;
  size_t keysize;
};

static int
hmacmd5init (gnutls_mac_algorithm_t mac, void **ctx)
{
  struct hmacctx *p;

  p = gnutls_malloc (sizeof (struct hmacctx));
  if (!p)
    return -1;

  p->data = NULL;
  p->datasize = 0;
  p->key = NULL;
  p->keysize = 0;

  *ctx = p;

  return 0;
}

static int
hmacmd5setkey (void *ctx, const void *key, size_t keysize)
{
  struct hmacctx *p = ctx;

  if (p->key)
    gnutls_free (p->key);

  p->key = gnutls_malloc (keysize);
  if (!p->key)
    return -1;

  memcpy (p->key, key, keysize);
  p->keysize = keysize;

  return 0;
}

static int
hmacmd5hash (void *ctx, const void *text, size_t textsize)
{
  struct hmacctx *p = ctx;
  char *new;

  new = gnutls_realloc (p->data, p->datasize + textsize);
  if (!new)
    return -1;

  memcpy (new + p->datasize, text, textsize);

  p->data = new;
  p->datasize += textsize;

  return 0;
}

static int
hmacmd5output (void *ctx, void *digest, size_t digestsize)
{
  struct hmacctx *p = ctx;
  char out[MD5_DIGEST_SIZE];
  int ret;

  ret = hmac_md5 (p->key, p->keysize, p->data, p->datasize, out);
  if (ret)
    return GNUTLS_E_HASH_FAILED;

  memcpy (digest, out, digestsize);

  return 0;
}

static void
hmacmd5deinit (void *ctx)
{
  struct hmacctx *p = ctx;

  if (p->data)
    gnutls_free (p->data);
  if (p->key)
    gnutls_free (p->key);

  gnutls_free (p);
}

static gnutls_crypto_digest_st dig = {
  .init = md5init,
  .hash = md5hash,
  .copy = md5copy,
  .output = md5output,
  .deinit = md5deinit
};

static gnutls_crypto_mac_st mac = {
  .init = hmacmd5init,
  .setkey = hmacmd5setkey,
  .hash = hmacmd5hash,
  .output = hmacmd5output,
  .deinit = hmacmd5deinit
};

/**
 * gnutls_register_md5_handler:
 *
 * Register a non-libgcrypt based MD5 and HMAC-MD5 handler.  This is
 * useful if you run Libgcrypt in FIPS-mode.  Normally TLS requires
 * use of MD5, so without this you cannot use GnuTLS with libgcrypt in
 * FIPS mode.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, otherwise an error.
 *
 * Since: 2.6.0
 **/
int
gnutls_register_md5_handler (void)
{
  int ret;

  ret = gnutls_crypto_single_digest_register (GNUTLS_DIG_MD5, INT_MAX, &dig);
  if (ret)
    return ret;

  ret = gnutls_crypto_single_mac_register (GNUTLS_MAC_MD5, INT_MAX, &mac);
  if (ret)
    return ret;

  return 0;
}
