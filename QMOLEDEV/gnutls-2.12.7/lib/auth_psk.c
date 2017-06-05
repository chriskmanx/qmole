/*
 * Copyright (C) 2005, 2007, 2008, 2010 Free Software Foundation, Inc.
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

#ifdef ENABLE_PSK

#include "gnutls_errors.h"
#include "gnutls_auth.h"
#include "gnutls_auth.h"
#include "debug.h"
#include "gnutls_num.h"
#include <auth_psk.h>
#include <auth_psk_passwd.h>
#include <gnutls_str.h>
#include <gnutls_datum.h>

int _gnutls_gen_psk_server_kx (gnutls_session_t session, opaque ** data);
int _gnutls_gen_psk_client_kx (gnutls_session_t, opaque **);

int _gnutls_proc_psk_client_kx (gnutls_session_t, opaque *, size_t);

int _gnutls_proc_psk_server_kx (gnutls_session_t session, opaque * data,
                                size_t _data_size);

const mod_auth_st psk_auth_struct = {
  "PSK",
  NULL,
  NULL,
  _gnutls_gen_psk_server_kx,
  _gnutls_gen_psk_client_kx,
  NULL,
  NULL,

  NULL,
  NULL,                         /* certificate */
  _gnutls_proc_psk_server_kx,
  _gnutls_proc_psk_client_kx,
  NULL,
  NULL
};

/* Set the PSK premaster secret.
 */
int
_gnutls_set_psk_session_key (gnutls_session_t session,
    gnutls_datum_t * ppsk /* key */,
    gnutls_datum_t * dh_secret)
{
  gnutls_datum_t pwd_psk = { NULL, 0 };
  size_t dh_secret_size;
  int ret;

  if (session->security_parameters.entity == GNUTLS_SERVER)
    {                           /* SERVER side */
      psk_auth_info_t info;

      info = _gnutls_get_auth_info (session);

      /* find the key of this username
       */
      ret = _gnutls_psk_pwd_find_entry (session, info->username, &pwd_psk);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      ppsk = &pwd_psk;
    }


  if (dh_secret == NULL)
    dh_secret_size = ppsk->size;
  else
    dh_secret_size = dh_secret->size;

  /* set the session key
   */
  session->key->key.size = 4 + dh_secret_size + ppsk->size;
  session->key->key.data = gnutls_malloc (session->key->key.size);
  if (session->key->key.data == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto error;
    }

  /* format of the premaster secret:
   * (uint16_t) psk_size
   * psk_size bytes of zeros
   * (uint16_t) psk_size
   * the psk
   */
  _gnutls_write_uint16 (dh_secret_size, session->key->key.data);
  if (dh_secret == NULL)
    memset (&session->key->key.data[2], 0, dh_secret_size);
  else
    memcpy (&session->key->key.data[2], dh_secret->data, dh_secret->size);
  _gnutls_write_datum16 (&session->key->key.data[dh_secret_size + 2], *ppsk);

  ret = 0;

error:
  _gnutls_free_datum (&pwd_psk);
  return ret;
}

/* returns the username and they key for the PSK session.
 * Free is non zero if they have to be freed.
 */
int _gnutls_find_psk_key( gnutls_session_t session, gnutls_psk_client_credentials_t cred, 
  gnutls_datum_t * username, gnutls_datum* key, int* free)
{
char* user_p;
int ret;

   *free = 0;

  if (cred->username.data != NULL && cred->key.data != NULL)
    {
      username->data = cred->username.data;
      username->size = cred->username.size;
      key->data = cred->key.data;
      key->size = cred->key.size;
    }
  else if (cred->get_function != NULL)
    {
      ret = cred->get_function (session, &user_p, key);
      if (ret)
        return gnutls_assert_val(ret);
      
      username->data = user_p;
      username->size = strlen(user_p);
      
      *free = 1;
    }
  else
    return gnutls_assert_val(GNUTLS_E_INSUFFICIENT_CREDENTIALS);
  
  return 0;
}


/* Generates the PSK client key exchange
 *
 * 
 * struct {
 *    select (KeyExchangeAlgorithm) {
 *       opaque psk_identity<0..2^16-1>;
 *    } exchange_keys;
 * } ClientKeyExchange;
 *
 */
int
_gnutls_gen_psk_client_kx (gnutls_session_t session, opaque ** data)
{
  int ret, free;
  gnutls_datum_t username;
  gnutls_datum_t key;
  gnutls_psk_client_credentials_t cred;

  cred = (gnutls_psk_client_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_PSK, NULL);

  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  ret = _gnutls_find_psk_key( session, cred, &username, &key, &free);
  if (ret < 0)
    return gnutls_assert_val(ret);

  ret = _gnutls_set_psk_session_key (session, &key, NULL);
  if (ret < 0)
    {
      gnutls_assert();
      goto cleanup;
    }
  
  (*data) = gnutls_malloc (2 + username.size);
  if ((*data) == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  _gnutls_write_datum16 (*data, username);

cleanup:
  if (free) 
    {
      gnutls_free(username.data);
      gnutls_free(key.data);
    }
  
  return (username.size + 2);
}


/* just read the username from the client key exchange.
 */
int
_gnutls_proc_psk_client_kx (gnutls_session_t session, opaque * data,
                            size_t _data_size)
{
  ssize_t data_size = _data_size;
  int ret;
  gnutls_datum_t username;
  gnutls_psk_server_credentials_t cred;
  psk_auth_info_t info;

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

  ret = _gnutls_set_psk_session_key (session, NULL, NULL);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  ret = 0;

error:
  return ret;
}


/* Generates the PSK server key exchange
 *
 * struct {
 *     select (KeyExchangeAlgorithm) {
 *         // other cases for rsa, diffie_hellman, etc.
 *         case psk:  // NEW
 *             opaque psk_identity_hint<0..2^16-1>;
 *     };
 * } ServerKeyExchange;
 *
 */
int
_gnutls_gen_psk_server_kx (gnutls_session_t session, opaque ** data)
{
  gnutls_psk_server_credentials_t cred;
  gnutls_datum_t hint;

  cred = (gnutls_psk_server_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_PSK, NULL);

  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  /* Abort sending this message if there is no PSK identity hint. */
  if (cred->hint == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INT_RET_0;
    }

  hint.data = cred->hint;
  hint.size = strlen (cred->hint);

  (*data) = gnutls_malloc (2 + hint.size);
  if ((*data) == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_write_datum16 (*data, hint);

  return hint.size + 2;
}


/* just read the hint from the server key exchange.
 */
int
_gnutls_proc_psk_server_kx (gnutls_session_t session, opaque * data,
                            size_t _data_size)
{
  ssize_t data_size = _data_size;
  int ret;
  gnutls_datum_t hint;
  gnutls_psk_client_credentials_t cred;
  psk_auth_info_t info;

  cred = (gnutls_psk_client_credentials_t)
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

  DECR_LENGTH_RET (data_size, 2, 0);
  hint.size = _gnutls_read_uint16 (&data[0]);

  DECR_LEN (data_size, hint.size);

  hint.data = &data[2];

  /* copy the hint to the auth info structures
   */
  info = _gnutls_get_auth_info (session);

  if (hint.size > MAX_USERNAME_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_ILLEGAL_SRP_USERNAME;
    }

  memcpy (info->hint, hint.data, hint.size);
  info->hint[hint.size] = 0;

  ret = _gnutls_set_psk_session_key (session, &cred->key, NULL);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  ret = 0;

error:
  return ret;
}

#endif /* ENABLE_PSK */
