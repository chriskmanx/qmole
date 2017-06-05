/*
 * Copyright (C) 2001, 2004, 2005, 2006, 2008, 2010 Free Software
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

/* Functions to parse the SSLv2.0 hello message.
 */

#include "gnutls_int.h"
#include "gnutls_errors.h"
#include "gnutls_dh.h"
#include "debug.h"
#include "gnutls_algorithms.h"
#include "gnutls_compress.h"
#include "gnutls_cipher.h"
#include "gnutls_buffers.h"
#include "gnutls_kx.h"
#include "gnutls_handshake.h"
#include "gnutls_num.h"
#include "gnutls_hash_int.h"
#include "gnutls_db.h"
#include "gnutls_extensions.h"
#include "gnutls_auth.h"
#include "gnutls_v2_compat.h"
#include "gnutls_constate.h"

/* This selects the best supported ciphersuite from the ones provided */
static int
_gnutls_handshake_select_v2_suite (gnutls_session_t session,
                                   opaque * data, int datalen)
{
  int i, j, ret;
  opaque *_data;
  int _datalen;

  _gnutls_handshake_log ("HSK[%p]: Parsing a version 2.0 client hello.\n",
                         session);

  _data = gnutls_malloc (datalen);
  if (_data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  if (datalen % 3 != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

  i = _datalen = 0;
  for (j = 0; j < datalen; j += 3)
    {
      if (data[j] == 0)
        {
          memcpy (&_data[i], &data[j + 1], 2);
          i += 2;
          _datalen += 2;
        }
    }

  ret = _gnutls_server_select_suite (session, _data, _datalen);
  gnutls_free (_data);

  return ret;

}


/* Read a v2 client hello. Some browsers still use that beast!
 * However they set their version to 3.0 or 3.1.
 */
int
_gnutls_read_client_hello_v2 (gnutls_session_t session, opaque * data,
                              int datalen)
{
  uint16_t session_id_len = 0;
  int pos = 0;
  int ret = 0;
  uint16_t sizeOfSuites;
  gnutls_protocol_t adv_version;
  opaque rnd[GNUTLS_RANDOM_SIZE];
  int len = datalen;
  int err;
  uint16_t challenge;
  opaque session_id[TLS_MAX_SESSION_ID_SIZE];

  /* we only want to get here once - only in client hello */
  session->internals.v2_hello = 0;

  DECR_LEN (len, 2);

  _gnutls_handshake_log
    ("HSK[%p]: SSL 2.0 Hello: Client's version: %d.%d\n", session,
     data[pos], data[pos + 1]);

  set_adv_version (session, data[pos], data[pos + 1]);

  adv_version = _gnutls_version_get (data[pos], data[pos + 1]);

  ret = _gnutls_negotiate_version (session, adv_version);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  pos += 2;

  /* Read uint16_t cipher_spec_length */
  DECR_LEN (len, 2);
  sizeOfSuites = _gnutls_read_uint16 (&data[pos]);
  pos += 2;

  /* read session id length */
  DECR_LEN (len, 2);
  session_id_len = _gnutls_read_uint16 (&data[pos]);
  pos += 2;

  if (session_id_len > TLS_MAX_SESSION_ID_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
    }

  /* read challenge length */
  DECR_LEN (len, 2);
  challenge = _gnutls_read_uint16 (&data[pos]);
  pos += 2;

  if (challenge < 16 || challenge > GNUTLS_RANDOM_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_UNSUPPORTED_VERSION_PACKET;
    }

  /* call the user hello callback
   */
  ret = _gnutls_user_hello_func (session, adv_version);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* find an appropriate cipher suite */

  DECR_LEN (len, sizeOfSuites);
  ret = _gnutls_handshake_select_v2_suite (session, &data[pos], sizeOfSuites);

  pos += sizeOfSuites;
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* check if the credentials (username, public key etc.) are ok
   */
  if (_gnutls_get_kx_cred
      (session,
       _gnutls_cipher_suite_get_kx_algo (&session->
                                         security_parameters.current_cipher_suite),
       &err) == NULL && err != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  /* set the mod_auth_st to the appropriate struct
   * according to the KX algorithm. This is needed since all the
   * handshake functions are read from there;
   */
  session->internals.auth_struct =
    _gnutls_kx_auth_struct (_gnutls_cipher_suite_get_kx_algo
                            (&session->
                             security_parameters.current_cipher_suite));
  if (session->internals.auth_struct == NULL)
    {

      _gnutls_handshake_log
        ("HSK[%p]: SSL 2.0 Hello: Cannot find the appropriate handler for the KX algorithm\n",
         session);

      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }



  /* read random new values -skip session id for now */
  DECR_LEN (len, session_id_len);       /* skip session id for now */
  memcpy (session_id, &data[pos], session_id_len);
  pos += session_id_len;

  DECR_LEN (len, challenge);
  memset (rnd, 0, GNUTLS_RANDOM_SIZE);

  memcpy (&rnd[GNUTLS_RANDOM_SIZE - challenge], &data[pos], challenge);

  _gnutls_set_client_random (session, rnd);

  /* generate server random value */

  _gnutls_tls_create_random (rnd);
  _gnutls_set_server_random (session, rnd);

  session->security_parameters.timestamp = gnutls_time (NULL);


  /* RESUME SESSION */

  DECR_LEN (len, session_id_len);
  ret = _gnutls_server_restore_session (session, session_id, session_id_len);

  if (ret == 0)
    {                           /* resumed! */
      /* get the new random values */
      memcpy (session->internals.resumed_security_parameters.server_random,
              session->security_parameters.server_random, GNUTLS_RANDOM_SIZE);
      memcpy (session->internals.resumed_security_parameters.client_random,
              session->security_parameters.client_random, GNUTLS_RANDOM_SIZE);

      session->internals.resumed = RESUME_TRUE;
      return 0;
    }
  else
    {
      _gnutls_generate_session_id (session->security_parameters.session_id,
                                   &session->
                                   security_parameters.session_id_size);
      session->internals.resumed = RESUME_FALSE;
    }

  session->internals.compression_method = GNUTLS_COMP_NULL;
  _gnutls_epoch_set_compression (session, EPOCH_NEXT, session->internals.compression_method);

  return 0;
}
