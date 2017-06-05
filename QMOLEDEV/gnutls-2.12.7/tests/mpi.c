/*
 * Copyright (C) 2007, 2009, 2010 Free Software Foundation, Inc.
 *
 * Author: Simon Josefsson
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GnuTLS; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#include "utils.h"
#include "../lib/gnutls_int.h"
#include "../lib/gnutls_mpi.h"
#include "../lib/gnutls_errors.h"
#include "../lib/debug.h"

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}

#define RND_BITS 510            /* not multiple of 8 */
void
doit (void)
{
  int rc;
  bigint_t n1, n2, n3, n4;

  gnutls_global_init ();

  gnutls_global_set_log_function (tls_log_func);
  if (debug)
    gnutls_global_set_log_level (99);

  n1 = _gnutls_mpi_new (1000);
  if (n1 == NULL)
    fail ("mpi_new failed\n");

  n2 = _gnutls_mpi_set_ui (NULL, 2);
  if (n2 == NULL)
    fail ("mpi_set_ui failed\n");

  n3 = _gnutls_mpi_set_ui (NULL, 5);
  if (n3 == NULL)
    fail ("mpi_set_ui failed\n");

  _gnutls_mpi_randomize (n1, RND_BITS, GNUTLS_RND_NONCE);

  _gnutls_mpi_log ("rand:", n1);

  rc = _gnutls_mpi_get_nbits (n1);
  if (rc > RND_BITS)
    fail ("mpi_get_nbits failed... returned %d\n", rc);

  n4 = _gnutls_mpi_addm (NULL, n1, n3, n2);
  if (n4 == NULL)
    fail ("mpi_set_ui failed\n");

  if (_gnutls_mpi_cmp_ui (n4, 0) != 0 && _gnutls_mpi_cmp_ui (n4, 1) != 0)
    fail ("mpi_cmp_ui failed\n");

  _gnutls_mpi_release (&n1);
  _gnutls_mpi_release (&n2);
  _gnutls_mpi_release (&n3);
  _gnutls_mpi_release (&n4);

  success ("mpi ops ok\n");
}
