/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2008, 2010 Free
 * Software Foundation, Inc.
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
#include <gnutls_dh.h>


/* 
	--Example-- 
	you: X = g ^ x mod p;
	peer:Y = g ^ y mod p;

	your_key = Y ^ x mod p;
	his_key  = X ^ y mod p;

//      generate our secret and the public value (X) for it
	X = gnutls_calc_dh_secret(&x, g, p);
//      now we can calculate the shared secret
	key = gnutls_calc_dh_key(Y, x, g, p);
	_gnutls_mpi_release(x);
	_gnutls_mpi_release(g);
*/

#define MAX_BITS 18000

/* returns the public value (X), and the secret (ret_x).
 */
bigint_t
gnutls_calc_dh_secret (bigint_t * ret_x, bigint_t g, bigint_t prime)
{
  bigint_t e, x = NULL;
  int x_size = _gnutls_mpi_get_nbits (prime) - 1;
  /* The size of the secret key is less than
   * prime/2
   */

  if (x_size > MAX_BITS || x_size <= 0)
    {
      gnutls_assert ();
      return NULL;
    }

  x = _gnutls_mpi_randomize (NULL, x_size, GNUTLS_RND_RANDOM);
  if (x == NULL)
    {
      gnutls_assert ();
      return NULL;
    }

  e = _gnutls_mpi_alloc_like (prime);
  if (e == NULL)
    {
      gnutls_assert ();
      if (ret_x)
        *ret_x = NULL;

      _gnutls_mpi_release (&x);
      return NULL;
    }

  _gnutls_mpi_powm (e, g, x, prime);

  if (ret_x)
    *ret_x = x;
  else
    _gnutls_mpi_release (&x);
  return e;
}


bigint_t
gnutls_calc_dh_key (bigint_t f, bigint_t x, bigint_t prime)
{
  bigint_t k;
  int bits;

  bits = _gnutls_mpi_get_nbits (prime);
  if (bits <= 0 || bits > MAX_BITS)
    {
      gnutls_assert ();
      return NULL;
    }

  k = _gnutls_mpi_alloc_like (prime);
  if (k == NULL)
    return NULL;
  _gnutls_mpi_powm (k, f, x, prime);
  return k;
}

/*-
 * _gnutls_get_dh_params - Returns the DH parameters pointer
 * @dh_params: is an DH parameters structure, or NULL.
 * @func: is a callback function to receive the parameters or NULL.
 * @session: a gnutls session.
 *
 * This function will return the dh parameters pointer.
 -*/
gnutls_dh_params_t
_gnutls_get_dh_params (gnutls_dh_params_t dh_params,
                       gnutls_params_function * func,
                       gnutls_session_t session)
{
  gnutls_params_st params;
  int ret;

  /* if cached return the cached */
  if (session->internals.params.dh_params)
    return session->internals.params.dh_params;

  if (dh_params)
    {
      session->internals.params.dh_params = dh_params;
    }
  else if (func)
    {
      ret = func (session, GNUTLS_PARAMS_DH, &params);
      if (ret == 0 && params.type == GNUTLS_PARAMS_DH)
        {
          session->internals.params.dh_params = params.params.dh;
          session->internals.params.free_dh_params = params.deinit;
        }
    }

  return session->internals.params.dh_params;
}
