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

/* Here is the libgcrypt random generator layer.
 */

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_num.h>
#include <gnutls_mpi.h>
#include <gcrypt.h>

static int
wrap_gcry_rnd_init (void **ctx)
{
  char c;

  gcry_create_nonce (&c, 1);
  gcry_randomize (&c, 1, GCRY_STRONG_RANDOM);

  return 0;
}

static int
wrap_gcry_rnd (void *ctx, int level, void *data, size_t datasize)
{
  if (level == GNUTLS_RND_NONCE)
    gcry_create_nonce (data, datasize);
  else
    gcry_randomize (data, datasize, level);

  return 0;
}

int crypto_rnd_prio = INT_MAX;

gnutls_crypto_rnd_st _gnutls_rnd_ops = {
  .init = wrap_gcry_rnd_init,
  .deinit = NULL,
  .rnd = wrap_gcry_rnd,
};
