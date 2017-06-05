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

#include <gnutls_errors.h>
#include <gnutls_int.h>
#include <gnutls/crypto.h>
#include <gnutls_cryptodev.h>
#include <gnutls_errors.h>

#ifdef ENABLE_CRYPTODEV

#include <fcntl.h>
#include <sys/ioctl.h>
#include <crypto/cryptodev.h>

#ifndef CRYPTO_CIPHER_MAX_KEY_LEN
#define CRYPTO_CIPHER_MAX_KEY_LEN 64
#endif

#ifndef EALG_MAX_BLOCK_LEN
#define EALG_MAX_BLOCK_LEN 16
#endif

static int cryptodev_fd = -1;

static int register_mac (int cfd);

struct cryptodev_ctx
{
  struct session_op sess;
  struct crypt_op cryp;
  opaque iv[EALG_MAX_BLOCK_LEN];
  opaque key[CRYPTO_CIPHER_MAX_KEY_LEN];
  int cfd;
};

static const int gnutls_cipher_map[] = {
  [GNUTLS_CIPHER_AES_128_CBC] = CRYPTO_AES_CBC,
  [GNUTLS_CIPHER_AES_192_CBC] = CRYPTO_AES_CBC,
  [GNUTLS_CIPHER_AES_256_CBC] = CRYPTO_AES_CBC,
  [GNUTLS_CIPHER_3DES_CBC] = CRYPTO_3DES_CBC,
  [GNUTLS_CIPHER_CAMELLIA_128_CBC] = CRYPTO_CAMELLIA_CBC,
  [GNUTLS_CIPHER_CAMELLIA_256_CBC] = CRYPTO_CAMELLIA_CBC,
  [GNUTLS_CIPHER_DES_CBC] = CRYPTO_DES_CBC,
};

static int
cryptodev_cipher_init (gnutls_cipher_algorithm_t algorithm, void **_ctx)
{
  struct cryptodev_ctx *ctx;
  int cipher = gnutls_cipher_map[algorithm];

  *_ctx = gnutls_calloc (1, sizeof (struct cryptodev_ctx));
  if (*_ctx == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ctx = *_ctx;

  ctx->cfd = cryptodev_fd;
  ctx->sess.cipher = cipher;
  ctx->sess.key = ctx->key;
  ctx->cryp.iv = ctx->iv;

  return 0;
}

static int
cryptodev_cipher_setkey (void *_ctx, const void *key, size_t keysize)
{
  struct cryptodev_ctx *ctx = _ctx;

  ctx->sess.keylen = keysize;
  memcpy (ctx->key, key, keysize);

  if (ioctl (ctx->cfd, CIOCGSESSION, &ctx->sess))
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
    }
  ctx->cryp.ses = ctx->sess.ses;

  return 0;
}

static int
cryptodev_setiv (void *_ctx, const void *iv, size_t iv_size)
{
  struct cryptodev_ctx *ctx = _ctx;

  memcpy (ctx->iv, iv, iv_size);

  return 0;
}

static int
cryptodev_encrypt (void *_ctx, const void *plain, size_t plainsize,
                   void *encr, size_t encrsize)
{
  struct cryptodev_ctx *ctx = _ctx;
  ctx->cryp.len = plainsize;
  ctx->cryp.src = (void *) plain;
  ctx->cryp.dst = encr;
  ctx->cryp.op = COP_ENCRYPT;
  if (ioctl (ctx->cfd, CIOCCRYPT, &ctx->cryp))
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
    }
  return 0;
}

static int
cryptodev_decrypt (void *_ctx, const void *encr, size_t encrsize,
                   void *plain, size_t plainsize)
{
  struct cryptodev_ctx *ctx = _ctx;

  ctx->cryp.len = encrsize;
  ctx->cryp.src = (void *) encr;
  ctx->cryp.dst = plain;
  ctx->cryp.op = COP_DECRYPT;
  if (ioctl (ctx->cfd, CIOCCRYPT, &ctx->cryp))
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
    }
  return 0;

}

static void
cryptodev_deinit (void *_ctx)
{
  struct cryptodev_ctx *ctx = _ctx;

  ioctl (ctx->cfd, CIOCFSESSION, &ctx->sess);
  gnutls_free (ctx);
}

static const gnutls_crypto_cipher_st cipher_struct = {
  .init = cryptodev_cipher_init,
  .setkey = cryptodev_cipher_setkey,
  .setiv = cryptodev_setiv,
  .encrypt = cryptodev_encrypt,
  .decrypt = cryptodev_decrypt,
  .deinit = cryptodev_deinit,
};

static int
register_crypto (int cfd)
{
  struct session_op sess;
  char fake_key[CRYPTO_CIPHER_MAX_KEY_LEN];
  int i = 0, ret;

  memset (&sess, 0, sizeof (sess));

  for (i = 0; i < sizeof (gnutls_cipher_map) / sizeof (gnutls_cipher_map[0]);
       i++)
    {
      if (gnutls_cipher_map[i] == 0)
        continue;

      /* test if a cipher is support it and if yes register it */
      sess.cipher = gnutls_cipher_map[i];
      sess.keylen = gnutls_cipher_get_key_size (i);
      sess.key = fake_key;

      if (ioctl (cfd, CIOCGSESSION, &sess))
        {
          continue;
        }

      ioctl (cfd, CIOCFSESSION, &sess);

      _gnutls_debug_log ("/dev/crypto: registering: %s\n",
                         gnutls_cipher_get_name (i));
      ret = gnutls_crypto_single_cipher_register (i, 90, &cipher_struct);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

    }

  return 0;
}

int
_gnutls_cryptodev_init (void)
{
  int ret;

  /* Open the crypto device */
  cryptodev_fd = open ("/dev/crypto", O_RDWR, 0);
  if (cryptodev_fd < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTODEV_DEVICE_ERROR;
    }

#ifndef CRIOGET_NOT_NEEDED
  {
    int cfd = -1;
    /* Clone file descriptor */
    if (ioctl (cryptodev_fd, CRIOGET, &cfd))
      {
        gnutls_assert ();
        return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
      }

    /* Set close-on-exec (not really neede here) */
    if (fcntl (cfd, F_SETFD, 1) == -1)
      {
        gnutls_assert ();
        return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
      }

    close (cryptodev_fd);
    cryptodev_fd = cfd;
  }
#endif

  ret = register_crypto (cryptodev_fd);
  if (ret < 0)
    gnutls_assert ();

  if (ret >= 0)
    {
      ret = register_mac (cryptodev_fd);
      if (ret < 0)
        gnutls_assert ();
    }

  if (ret < 0)
    {
      gnutls_assert ();
      close (cryptodev_fd);
    }

  return ret;
}

void
_gnutls_cryptodev_deinit (void)
{
  close (cryptodev_fd);
}

/* MAC and digest stuff */

/* if we are using linux /dev/crypto
 */
#if defined COP_FLAG_UPDATE

static const int gnutls_mac_map[] = {
  [GNUTLS_MAC_MD5] = CRYPTO_MD5_HMAC,
  [GNUTLS_MAC_SHA1] = CRYPTO_SHA1_HMAC,
  [GNUTLS_MAC_SHA256] = CRYPTO_SHA2_256_HMAC,
  [GNUTLS_MAC_SHA384] = CRYPTO_SHA2_384_HMAC,
  [GNUTLS_MAC_SHA512] = CRYPTO_SHA2_512_HMAC,
};

static int
cryptodev_mac_init (gnutls_mac_algorithm_t algorithm, void **_ctx)
{
  struct cryptodev_ctx *ctx;
  int mac = gnutls_mac_map[algorithm];

  *_ctx = gnutls_calloc (1, sizeof (struct cryptodev_ctx));
  if (*_ctx == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ctx = *_ctx;

  ctx->cfd = cryptodev_fd;

  ctx->sess.mac = mac;
  ctx->sess.mackey = ctx->key;

  return 0;
}

static int
cryptodev_mac_setkey (void *_ctx, const void *key, size_t keysize)
{
  struct cryptodev_ctx *ctx = _ctx;

  ctx->sess.mackeylen = keysize;
  memcpy (ctx->key, key, keysize);

  if (ioctl (ctx->cfd, CIOCGSESSION, &ctx->sess))
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
    }
  ctx->cryp.ses = ctx->sess.ses;

  return 0;
}

static int
cryptodev_mac_hash (void *_ctx, const void *text, size_t textsize)
{
  struct cryptodev_ctx *ctx = _ctx;
  ctx->cryp.len = textsize;
  ctx->cryp.src = (void *) text;
  ctx->cryp.dst = NULL;
  ctx->cryp.op = COP_ENCRYPT;
  ctx->cryp.flags = COP_FLAG_UPDATE;
  if (ioctl (ctx->cfd, CIOCCRYPT, &ctx->cryp))
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
    }
  return 0;
}

static int
cryptodev_mac_output (void *_ctx, void *digest, size_t digestsize)
{
  struct cryptodev_ctx *ctx = _ctx;
  ctx->cryp.len = 0;
  ctx->cryp.src = NULL;
  ctx->cryp.mac = digest;
  ctx->cryp.op = COP_ENCRYPT;
  ctx->cryp.flags = COP_FLAG_FINAL;
  if (ioctl (ctx->cfd, CIOCCRYPT, &ctx->cryp))
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTODEV_IOCTL_ERROR;
    }

  return 0;
}

#define cryptodev_mac_deinit cryptodev_deinit

static const gnutls_crypto_mac_st mac_struct = {
  .init = cryptodev_mac_init,
  .setkey = cryptodev_mac_setkey,
  .hash = cryptodev_mac_hash,
  .output = cryptodev_mac_output,
  .deinit = cryptodev_mac_deinit
};

static int
register_mac (int cfd)
{
  struct session_op sess;
  char fake_key[CRYPTO_CIPHER_MAX_KEY_LEN];
  int i = 0, ret;

  memset (&sess, 0, sizeof (sess));
  for (i = 0; i < sizeof (gnutls_mac_map) / sizeof (gnutls_mac_map[0]); i++)
    {
      if (gnutls_mac_map[i] == 0)
        continue;

      sess.mac = gnutls_mac_map[i];
      sess.keylen = 8;
      sess.key = fake_key;

      if (ioctl (cfd, CIOCGSESSION, &sess))
        {
          continue;
        }

      ioctl (cfd, CIOCFSESSION, &sess);

      _gnutls_debug_log ("/dev/crypto: registering: %s\n",
                         gnutls_mac_get_name (i));
      ret = gnutls_crypto_single_mac_register (i, 90, &mac_struct);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

    }

  return 0;
}

#else
static int
register_mac (int cfd)
{
  return 0;
}

#endif /* defined(CIOCSESSIONCLONE) && defined(COP_FLAG_UPDATE) */

#else /* ENABLE_CRYPTODEV */
int
_gnutls_cryptodev_init ()
{
  return 0;
}

void
_gnutls_cryptodev_deinit ()
{
  return;
}
#endif /* ENABLE_CRYPTODEV */
