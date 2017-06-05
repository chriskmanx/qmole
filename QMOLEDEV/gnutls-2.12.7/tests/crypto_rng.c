/*
 * Copyright (C) 2008, 2010 Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GnuTLS.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#include "utils.h"

#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
#include "../lib/random.h"

static int
rng_init (void **ctx)
{
  return 0;
}

static int
rng_rnd (void *ctx, int level, void *data, size_t datasize)
{
  memset (data, 1, datasize);
  return 0;
}

static void
rng_deinit (void *ctx)
{
}

void
doit (void)
{
  int rc;
  char buf1[32];
  char buf2[32];
  int failed = 0;
  gnutls_crypto_rnd_st rng = { rng_init, rng_rnd, rng_deinit };


  rc = gnutls_crypto_rnd_register (0, &rng);

  gnutls_global_init ();

  memset (buf2, 1, sizeof (buf2));

  _gnutls_rnd (GNUTLS_RND_RANDOM, buf1, sizeof (buf1));

  if (memcmp (buf1, buf2, sizeof (buf1)) != 0)
    failed = 1;

  gnutls_global_deinit ();

  if (failed == 0)
    {
      success ("rng registered ok\n");
    }
  else
    {
      fail ("rng register test failed: %d\n", rc);
    }
}
