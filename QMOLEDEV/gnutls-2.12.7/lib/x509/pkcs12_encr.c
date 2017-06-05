/* minip12.c - A mini pkcs-12 implementation (modified for gnutls)
 *
 * Copyright (C) 2002, 2004, 2005, 2009, 2010 Free Software Foundation,
 * Inc.
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

#ifdef ENABLE_PKI

#include <gnutls_mpi.h>
#include <gnutls_errors.h>

/* Returns 0 if the password is ok, or a negative error
 * code instead.
 */
static int
_pkcs12_check_pass (const char *pass, size_t plen)
{
  const unsigned char *p = pass;
  unsigned int i;

  for (i = 0; i < plen; i++)
    {
      if (isascii (p[i]))
        continue;
      return GNUTLS_E_INVALID_PASSWORD;
    }

  return 0;
}

/* ID should be:
 * 3 for MAC
 * 2 for IV
 * 1 for encryption key
 */
int
_gnutls_pkcs12_string_to_key (unsigned int id, const opaque * salt,
                              unsigned int salt_size, unsigned int iter,
                              const char *pw, unsigned int req_keylen,
                              opaque * keybuf)
{
  int rc;
  unsigned int i, j;
  digest_hd_st md;
  bigint_t num_b1 = NULL, num_ij = NULL;
  bigint_t mpi512 = NULL;
  unsigned int pwlen;
  opaque hash[20], buf_b[64], buf_i[128], *p;
  size_t cur_keylen;
  size_t n, m;
  const opaque buf_512[] =      /* 2^64 */
  { 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  };

  cur_keylen = 0;

  if (pw == NULL)
    pwlen = 0;
  else
    pwlen = strlen (pw);

  if (pwlen > 63 / 2)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if ((rc = _pkcs12_check_pass (pw, pwlen)) < 0)
    {
      gnutls_assert ();
      return rc;
    }

  rc = _gnutls_mpi_scan (&mpi512, buf_512, sizeof (buf_512));
  if (rc < 0)
    {
      gnutls_assert ();
      return rc;
    }

  /* Store salt and password in BUF_I */
  p = buf_i;
  for (i = 0; i < 64; i++)
    *p++ = salt[i % salt_size];
  if (pw)
    {
      for (i = j = 0; i < 64; i += 2)
        {
          *p++ = 0;
          *p++ = pw[j];
          if (++j > pwlen)      /* Note, that we include the trailing zero */
            j = 0;
        }
    }
  else
    memset (p, 0, 64);

  for (;;)
    {
      rc = _gnutls_hash_init (&md, GNUTLS_MAC_SHA1);
      if (rc < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      for (i = 0; i < 64; i++)
        {
          unsigned char lid = id & 0xFF;
          _gnutls_hash (&md, &lid, 1);
        }
      _gnutls_hash (&md, buf_i, pw ? 128 : 64);
      _gnutls_hash_deinit (&md, hash);
      for (i = 1; i < iter; i++)
        {
          rc = _gnutls_hash_init (&md, GNUTLS_MAC_SHA1);
          if (rc < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
          _gnutls_hash (&md, hash, 20);
          _gnutls_hash_deinit (&md, hash);
        }
      for (i = 0; i < 20 && cur_keylen < req_keylen; i++)
        keybuf[cur_keylen++] = hash[i];
      if (cur_keylen == req_keylen)
        {
          rc = 0;               /* ready */
          goto cleanup;
        }

      /* need more bytes. */
      for (i = 0; i < 64; i++)
        buf_b[i] = hash[i % 20];
      n = 64;
      rc = _gnutls_mpi_scan (&num_b1, buf_b, n);
      if (rc < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      _gnutls_mpi_add_ui (num_b1, num_b1, 1);
      for (i = 0; i < 128; i += 64)
        {
          n = 64;
          rc = _gnutls_mpi_scan (&num_ij, buf_i + i, n);
          if (rc < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
          _gnutls_mpi_addm (num_ij, num_ij, num_b1, mpi512);
          n = 64;
#ifndef PKCS12_BROKEN_KEYGEN
          m = (_gnutls_mpi_get_nbits (num_ij) + 7) / 8;
#else
          m = n;
#endif
          memset (buf_i + i, 0, n - m);
          rc = _gnutls_mpi_print (num_ij, buf_i + i + n - m, &n);
          if (rc < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
          _gnutls_mpi_release (&num_ij);
        }
    }
cleanup:
  _gnutls_mpi_release (&num_ij);
  _gnutls_mpi_release (&num_b1);
  _gnutls_mpi_release (&mpi512);

  return rc;
}

#endif /* ENABLE_PKI */
