/*
 * Copyright (C) 2002, 2003, 2004, 2005, 2007, 2009, 2010 Free Software
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

/* This file contains common stuff in Ephemeral Diffie-Hellman (DHE)
 * and Anonymous DH key exchange(DHA). These are used in the handshake
 * procedure of the certificate and anoymous authentication.
 */

#include "gnutls_int.h"
#include "gnutls_auth.h"
#include "gnutls_errors.h"
#include "gnutls_dh.h"
#include "gnutls_num.h"
#include "gnutls_sig.h"
#include <gnutls_datum.h>
#include <gnutls_x509.h>
#include <gnutls_state.h>
#include <auth_dh_common.h>
#include <gnutls_algorithms.h>
#include <auth_psk.h>

/* Frees the dh_info_st structure.
 */
void
_gnutls_free_dh_info (dh_info_st * dh)
{
  dh->secret_bits = 0;
  _gnutls_free_datum (&dh->prime);
  _gnutls_free_datum (&dh->generator);
  _gnutls_free_datum (&dh->public_key);
}

int
_gnutls_proc_dh_common_client_kx (gnutls_session_t session,
                                  opaque * data, size_t _data_size,
                                  bigint_t g, bigint_t p)
{
  uint16_t n_Y;
  size_t _n_Y;
  int ret;
  ssize_t data_size = _data_size;


  DECR_LEN (data_size, 2);
  n_Y = _gnutls_read_uint16 (&data[0]);
  _n_Y = n_Y;

  DECR_LEN (data_size, n_Y);
  if (_gnutls_mpi_scan_nz (&session->key->client_Y, &data[2], _n_Y))
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  _gnutls_dh_set_peer_public (session, session->key->client_Y);

  session->key->KEY =
    gnutls_calc_dh_key (session->key->client_Y, session->key->dh_secret, p);

  if (session->key->KEY == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_mpi_release (&session->key->client_Y);
  _gnutls_mpi_release (&session->key->dh_secret);


  if (_gnutls_cipher_suite_get_kx_algo
      (&session->security_parameters.current_cipher_suite)
      != GNUTLS_KX_DHE_PSK)
    {
      ret = _gnutls_mpi_dprint (session->key->KEY, &session->key->key);
    }
  else                          /* In DHE_PSK the key is set differently */
    {
      gnutls_datum_t tmp_dh_key;
      ret = _gnutls_mpi_dprint (session->key->KEY, &tmp_dh_key);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      ret = _gnutls_set_psk_session_key (session, NULL, &tmp_dh_key);
      _gnutls_free_datum (&tmp_dh_key);

    }

  _gnutls_mpi_release (&session->key->KEY);

  if (ret < 0)
    {
      return ret;
    }

  return 0;
}

int _gnutls_gen_dh_common_client_kx (gnutls_session_t session, opaque** data)
{
  return _gnutls_gen_dh_common_client_kx_int(session, data, NULL);
}

int
_gnutls_gen_dh_common_client_kx_int (gnutls_session_t session, opaque ** data, gnutls_datum_t* pskkey)
{
  bigint_t x = NULL, X = NULL;
  size_t n_X;
  int ret;

  *data = NULL;

  X = gnutls_calc_dh_secret (&x, session->key->client_g,
                             session->key->client_p);
  if (X == NULL || x == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto error;
    }

  _gnutls_dh_set_secret_bits (session, _gnutls_mpi_get_nbits (x));

  _gnutls_mpi_print (X, NULL, &n_X);
  (*data) = gnutls_malloc (n_X + 2);
  if (*data == NULL)
    {
      ret = GNUTLS_E_MEMORY_ERROR;
      goto error;
    }

  _gnutls_mpi_print (X, &(*data)[2], &n_X);
  _gnutls_mpi_release (&X);

  _gnutls_write_uint16 (n_X, &(*data)[0]);

  /* calculate the key after calculating the message */
  session->key->KEY =
    gnutls_calc_dh_key (session->key->client_Y, x, session->key->client_p);

  if (session->key->KEY == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto error;
    }

  /* THESE SHOULD BE DISCARDED */
  _gnutls_mpi_release (&session->key->client_Y);
  _gnutls_mpi_release (&session->key->client_p);
  _gnutls_mpi_release (&session->key->client_g);

  if (_gnutls_cipher_suite_get_kx_algo
      (&session->security_parameters.current_cipher_suite)
      != GNUTLS_KX_DHE_PSK)
    {
      ret = _gnutls_mpi_dprint (session->key->KEY, &session->key->key);
    }
  else                          /* In DHE_PSK the key is set differently */
    {
      gnutls_datum_t tmp_dh_key;

      ret = _gnutls_mpi_dprint (session->key->KEY, &tmp_dh_key);
      if (ret < 0)
        {
          gnutls_assert ();
          goto error;
        }

      ret = _gnutls_set_psk_session_key (session, pskkey, &tmp_dh_key);
      _gnutls_free_datum (&tmp_dh_key);
    }

  _gnutls_mpi_release (&session->key->KEY);

  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  ret = n_X + 2;

error:
  _gnutls_mpi_release (&x);
  _gnutls_mpi_release (&X);
  
  if (ret < 0)
    {
      gnutls_free (*data);
      *data = NULL;
    }
  return ret;
}

int
_gnutls_proc_dh_common_server_kx (gnutls_session_t session,
                                  opaque * data, size_t _data_size, int psk)
{
  uint16_t n_Y, n_g, n_p;
  size_t _n_Y, _n_g, _n_p;
  uint8_t *data_p;
  uint8_t *data_g;
  uint8_t *data_Y;
  int i, bits, psk_size, ret;
  ssize_t data_size = _data_size;

  i = 0;

  if (psk != 0)
    {
      DECR_LEN (data_size, 2);
      psk_size = _gnutls_read_uint16 (&data[i]);
      DECR_LEN (data_size, psk_size);
      i += 2 + psk_size;
    }

  DECR_LEN (data_size, 2);
  n_p = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_p);
  data_p = &data[i];
  i += n_p;

  DECR_LEN (data_size, 2);
  n_g = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_g);
  data_g = &data[i];
  i += n_g;

  DECR_LEN (data_size, 2);
  n_Y = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_Y);
  data_Y = &data[i];
  i += n_Y;

  _n_Y = n_Y;
  _n_g = n_g;
  _n_p = n_p;

  if (_gnutls_mpi_scan_nz (&session->key->client_Y, data_Y, _n_Y) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  if (_gnutls_mpi_scan_nz (&session->key->client_g, data_g, _n_g) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }
  if (_gnutls_mpi_scan_nz (&session->key->client_p, data_p, _n_p) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  bits = _gnutls_dh_get_allowed_prime_bits (session);
  if (bits < 0)
    {
      gnutls_assert ();
      return bits;
    }

  if (_gnutls_mpi_get_nbits (session->key->client_p) < (size_t) bits)
    {
      /* the prime used by the peer is not acceptable
       */
      gnutls_assert ();
      return GNUTLS_E_DH_PRIME_UNACCEPTABLE;
    }

  _gnutls_dh_set_group (session, session->key->client_g,
                        session->key->client_p);
  _gnutls_dh_set_peer_public (session, session->key->client_Y);

  ret = n_Y + n_p + n_g + 6;
  if (psk != 0)
    ret += 2;

  return ret;
}

/* If the psk flag is set, then an empty psk_identity_hint will
 * be inserted */
int
_gnutls_dh_common_print_server_kx (gnutls_session_t session,
                                   bigint_t g, bigint_t p, opaque ** data,
                                   int psk)
{
  bigint_t x, X;
  size_t n_X, n_g, n_p;
  int ret, data_size, pos;
  uint8_t *pdata;

  X = gnutls_calc_dh_secret (&x, g, p);
  if (X == NULL || x == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  session->key->dh_secret = x;
  _gnutls_dh_set_secret_bits (session, _gnutls_mpi_get_nbits (x));

  _gnutls_mpi_print (g, NULL, &n_g);
  _gnutls_mpi_print (p, NULL, &n_p);
  _gnutls_mpi_print (X, NULL, &n_X);

  data_size = n_g + n_p + n_X + 6;
  if (psk != 0)
    data_size += 2;

  (*data) = gnutls_malloc (data_size);
  if (*data == NULL)
    {
      _gnutls_mpi_release (&X);
      return GNUTLS_E_MEMORY_ERROR;
    }

  pos = 0;
  pdata = *data;

  if (psk != 0)
    {
      _gnutls_write_uint16 (0, &pdata[pos]);
      pos += 2;
    }

  _gnutls_mpi_print (p, &pdata[pos + 2], &n_p);
  _gnutls_write_uint16 (n_p, &pdata[pos]);

  pos += n_p + 2;

  _gnutls_mpi_print (g, &pdata[pos + 2], &n_g);
  _gnutls_write_uint16 (n_g, &pdata[pos]);

  pos += n_g + 2;

  _gnutls_mpi_print (X, &pdata[pos + 2], &n_X);
  _gnutls_mpi_release (&X);

  _gnutls_write_uint16 (n_X, &pdata[pos]);

  /* do not use data_size. _gnutls_mpi_print() might
   * have been pessimist and might have returned initially
   * more data */
  ret = n_g + n_p + n_X + 6;
  if (psk != 0)
    ret += 2;

  return ret;
}
