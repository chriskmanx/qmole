/*
 * Copyright (C) 2005, 2007, 2009, 2010 Free Software Foundation, Inc.
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

/* This file contains the PSK Diffie-Hellman key exchange part of the
 * PSK authentication.  The functions here are used in the handshake.
 */

#include <gnutls_int.h>

#ifdef ENABLE_PSK

#include "gnutls_auth.h"
#include "gnutls_errors.h"
#include "gnutls_dh.h"
#include "auth_psk.h"
#include "gnutls_num.h"
#include "gnutls_mpi.h"
#include <gnutls_state.h>
#include <auth_dh_common.h>
#include <gnutls_datum.h>

static int gen_psk_server_kx (gnutls_session_t, opaque **);
static int gen_psk_client_kx (gnutls_session_t, opaque **);
static int proc_psk_client_kx (gnutls_session_t, opaque *, size_t);
static int proc_psk_server_kx (gnutls_session_t, opaque *, size_t);

const mod_auth_st dhe_psk_auth_struct = {
  "DHE PSK",
  NULL,
  NULL,
  gen_psk_server_kx,
  gen_psk_client_kx,
  NULL,
  NULL,

  NULL,
  NULL,                         /* certificate */
  proc_psk_server_kx,
  proc_psk_client_kx,
  NULL,
  NULL
};

static int
gen_psk_client_kx (gnutls_session_t session, opaque ** data)
{
  int ret, free;
  opaque *tmp_data = NULL;
  int data_size, tmp_data_size;
  gnutls_psk_client_credentials_t cred;
  gnutls_datum_t username, key;

  cred = (gnutls_psk_client_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_PSK, NULL);

  if (cred == NULL)
    return gnutls_assert_val(GNUTLS_E_INSUFFICIENT_CREDENTIALS);


  ret = _gnutls_find_psk_key( session, cred, &username, &key, &free);
  if (ret < 0)
    return gnutls_assert_val(ret);

  /* The PSK key is set in there */
  ret = _gnutls_gen_dh_common_client_kx_int (session, &tmp_data, &key);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  tmp_data_size = ret;
  data_size = tmp_data_size + username.size + 2;

  (*data) = gnutls_malloc (data_size);
  if ((*data) == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  _gnutls_write_datum16 (*data, username);
  memcpy (&(*data)[username.size + 2], tmp_data, tmp_data_size);

  ret = data_size;

cleanup:
  gnutls_free (tmp_data);
  if (free)
    {
      _gnutls_free_datum(&username);
      _gnutls_free_datum(&key);
    }

  return ret;

}

static int
gen_psk_server_kx (gnutls_session_t session, opaque ** data)
{
  bigint_t g, p;
  const bigint_t *mpis;
  int ret;
  gnutls_dh_params_t dh_params;
  gnutls_psk_server_credentials_t cred;

  cred = (gnutls_psk_server_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_PSK, NULL);
  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  dh_params =
    _gnutls_get_dh_params (cred->dh_params, cred->params_func, session);
  mpis = _gnutls_dh_params_to_mpi (dh_params);
  if (mpis == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_NO_TEMPORARY_DH_PARAMS;
    }

  p = mpis[0];
  g = mpis[1];

  if ((ret =
       _gnutls_auth_info_set (session, GNUTLS_CRD_PSK,
                              sizeof (psk_auth_info_st), 1)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_dh_set_group (session, g, p);

  ret = _gnutls_dh_common_print_server_kx (session, g, p, data, 1);
  if (ret < 0)
    {
      gnutls_assert ();
    }

  return ret;
}


static int
proc_psk_client_kx (gnutls_session_t session, opaque * data,
                    size_t _data_size)
{
  int ret;
  bigint_t p, g;
  gnutls_dh_params_t dh_params;
  const bigint_t *mpis;
  gnutls_psk_server_credentials_t cred;
  psk_auth_info_t info;
  gnutls_datum_t username;
  ssize_t data_size = _data_size;

  cred = (gnutls_psk_server_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_PSK, NULL);

  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  if ((ret =
       _gnutls_auth_info_set (session, GNUTLS_CRD_PSK,
                              sizeof (psk_auth_info_st), 1)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  dh_params =
    _gnutls_get_dh_params (cred->dh_params, cred->params_func, session);
  mpis = _gnutls_dh_params_to_mpi (dh_params);
  if (mpis == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_NO_TEMPORARY_DH_PARAMS;
    }

  p = mpis[0];
  g = mpis[1];

  DECR_LEN (data_size, 2);
  username.size = _gnutls_read_uint16 (&data[0]);

  DECR_LEN (data_size, username.size);

  username.data = &data[2];

  /* copy the username to the auth info structures
   */
  info = _gnutls_get_auth_info (session);

  if (username.size > MAX_USERNAME_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_ILLEGAL_SRP_USERNAME;
    }

  memcpy (info->username, username.data, username.size);
  info->username[username.size] = 0;

  /* Adjust the data */
  data += username.size + 2;

  ret = _gnutls_proc_dh_common_client_kx (session, data, data_size, g, p);

  return ret;

}

int
proc_psk_server_kx (gnutls_session_t session, opaque * data,
                    size_t _data_size)
{

  int ret;

  /* set auth_info */
  if ((ret =
       _gnutls_auth_info_set (session, GNUTLS_CRD_PSK,
                              sizeof (psk_auth_info_st), 1)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_proc_dh_common_server_kx (session, data, _data_size, 1);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

#endif /* ENABLE_PSK */
