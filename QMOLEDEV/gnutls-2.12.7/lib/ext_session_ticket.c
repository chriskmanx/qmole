/*
 * Copyright (C) 2009, 2010 Free Software Foundation, Inc.
 *
 * Author: Daiki Ueno
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
#include <gnutls_datum.h>
#include <gnutls_algorithms.h>
#include <gnutls_handshake.h>
#include <gnutls_num.h>
#include <gnutls_constate.h>
#include <gnutls_session_pack.h>
#include <random.h>
#include <ext_session_ticket.h>
#include <gnutls_mbuffers.h>
#include <gnutls_extensions.h>
#include <gnutls_constate.h>
#include <system.h>

#ifdef ENABLE_SESSION_TICKET

#define KEY_NAME_SIZE SESSION_TICKET_KEY_NAME_SIZE
#define KEY_SIZE SESSION_TICKET_KEY_SIZE
#define IV_SIZE SESSION_TICKET_IV_SIZE
#define MAC_SECRET_SIZE SESSION_TICKET_MAC_SECRET_SIZE

#define MAC_SIZE 32

static int session_ticket_recv_params (gnutls_session_t session,
                                       const opaque * data, size_t data_size);
static int session_ticket_send_params (gnutls_session_t session,
                                       opaque * data, size_t data_size);
static int session_ticket_unpack (gnutls_buffer_st * ps,
                                  extension_priv_data_t * _priv);
static int session_ticket_pack (extension_priv_data_t _priv,
                                gnutls_buffer_st * ps);
static void session_ticket_deinit_data (extension_priv_data_t priv);

extension_entry_st ext_mod_session_ticket = {
  .name = "SESSION TICKET",
  .type = GNUTLS_EXTENSION_SESSION_TICKET,
  .parse_type = GNUTLS_EXT_TLS,

  .recv_func = session_ticket_recv_params,
  .send_func = session_ticket_send_params,
  .pack_func = session_ticket_pack,
  .unpack_func = session_ticket_unpack,
  .deinit_func = session_ticket_deinit_data,
};

struct gnutls_session_ticket_key_st
{
  opaque key_name[SESSION_TICKET_KEY_NAME_SIZE];
  opaque key[SESSION_TICKET_KEY_SIZE];
  opaque mac_secret[SESSION_TICKET_MAC_SECRET_SIZE];
};

typedef struct
{
  int session_ticket_enable;
  int session_ticket_renew;
  opaque session_ticket_IV[SESSION_TICKET_IV_SIZE];

  opaque *session_ticket;
  int session_ticket_len;

  struct gnutls_session_ticket_key_st key;
} session_ticket_ext_st;

struct ticket
{
  opaque key_name[KEY_NAME_SIZE];
  opaque IV[IV_SIZE];
  opaque *encrypted_state;
  uint16_t encrypted_state_len;
  opaque mac[MAC_SIZE];
};

static int
digest_ticket (const gnutls_datum_t * key, struct ticket *ticket,
               opaque * digest)
{
  digest_hd_st digest_hd;
  uint16_t length16;
  int ret;

  ret = _gnutls_hmac_init (&digest_hd, GNUTLS_MAC_SHA256, key->data,
                           key->size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  _gnutls_hmac (&digest_hd, ticket->key_name, KEY_NAME_SIZE);
  _gnutls_hmac (&digest_hd, ticket->IV, IV_SIZE);
  length16 = _gnutls_conv_uint16 (ticket->encrypted_state_len);
  _gnutls_hmac (&digest_hd, &length16, 2);
  _gnutls_hmac (&digest_hd, ticket->encrypted_state,
                ticket->encrypted_state_len);
  _gnutls_hmac_deinit (&digest_hd, digest);

  return 0;
}

static int
decrypt_ticket (gnutls_session_t session, session_ticket_ext_st * priv,
                struct ticket *ticket)
{
  cipher_hd_st cipher_hd;
  gnutls_datum_t key, IV, mac_secret, state;
  opaque final[MAC_SECRET_SIZE];
  time_t timestamp = gnutls_time (0);
  int ret;

  /* Check the integrity of ticket using HMAC-SHA-256. */
  mac_secret.data = (void *) priv->key.mac_secret;
  mac_secret.size = MAC_SECRET_SIZE;
  ret = digest_ticket (&mac_secret, ticket, final);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (memcmp (ticket->mac, final, MAC_SIZE))
    {
      gnutls_assert ();
      return GNUTLS_E_DECRYPTION_FAILED;
    }

  /* Decrypt encrypted_state using 128-bit AES in CBC mode. */
  key.data = (void *) priv->key.key;
  key.size = KEY_SIZE;
  IV.data = ticket->IV;
  IV.size = IV_SIZE;
  ret =
    _gnutls_cipher_init (&cipher_hd, GNUTLS_CIPHER_AES_128_CBC, &key, &IV);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  ret = _gnutls_cipher_decrypt (&cipher_hd, ticket->encrypted_state,
                                ticket->encrypted_state_len);
  _gnutls_cipher_deinit (&cipher_hd);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* Unpack security parameters. */
  state.data = ticket->encrypted_state;
  state.size = ticket->encrypted_state_len;
  ret = _gnutls_session_unpack (session, &state);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (timestamp - session->internals.resumed_security_parameters.timestamp >
      session->internals.expire_time
      || session->internals.resumed_security_parameters.timestamp > timestamp)
    {
      gnutls_assert ();
      return GNUTLS_E_EXPIRED;
    }

  session->internals.resumed = RESUME_TRUE;

  return 0;
}

static int
encrypt_ticket (gnutls_session_t session, session_ticket_ext_st * priv,
                struct ticket *ticket)
{
  cipher_hd_st cipher_hd;
  gnutls_datum_t key, IV, mac_secret, state, encrypted_state;
  int blocksize;
  int ret;

  /* Pack security parameters. */
  ret = _gnutls_session_pack (session, &state);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
  blocksize = gnutls_cipher_get_block_size (GNUTLS_CIPHER_AES_128_CBC);

  encrypted_state.size =
    ((state.size + blocksize - 1) / blocksize) * blocksize;
  encrypted_state.data = gnutls_malloc (encrypted_state.size);
  if (!encrypted_state.data)
    {
      gnutls_assert ();
      _gnutls_free_datum (&state);
      return GNUTLS_E_MEMORY_ERROR;
    }
  memset (encrypted_state.data, 0, encrypted_state.size);
  memcpy (encrypted_state.data, state.data, state.size);
  _gnutls_free_datum (&state);

  /* Encrypt state using 128-bit AES in CBC mode. */
  key.data = (void *) priv->key.key;
  key.size = KEY_SIZE;
  IV.data = priv->session_ticket_IV;
  IV.size = IV_SIZE;
  ret =
    _gnutls_cipher_init (&cipher_hd, GNUTLS_CIPHER_AES_128_CBC, &key, &IV);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&encrypted_state);
      return ret;
    }

  ret = _gnutls_cipher_encrypt (&cipher_hd, encrypted_state.data,
                                encrypted_state.size);
  _gnutls_cipher_deinit (&cipher_hd);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&encrypted_state);
      return ret;
    }

  /* Fill the ticket structure to compute MAC. */
  memcpy (ticket->key_name, priv->key.key_name, KEY_NAME_SIZE);
  memcpy (ticket->IV, IV.data, IV.size);
  ticket->encrypted_state_len = encrypted_state.size;
  ticket->encrypted_state = encrypted_state.data;

  mac_secret.data = priv->key.mac_secret;
  mac_secret.size = MAC_SECRET_SIZE;
  ret = digest_ticket (&mac_secret, ticket, ticket->mac);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&encrypted_state);
      return ret;
    }

  return 0;
}

static int
session_ticket_recv_params (gnutls_session_t session,
                            const opaque * data, size_t _data_size)
{
  ssize_t data_size = _data_size;
  session_ticket_ext_st *priv = NULL;
  extension_priv_data_t epriv;
  int ret;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_SESSION_TICKET,
                                  &epriv);
  if (ret < 0)
    {
      return 0;
    }
  priv = epriv.ptr;

  if (!priv->session_ticket_enable)
    return 0;

  if (session->security_parameters.entity == GNUTLS_SERVER)
    {
      struct ticket ticket;
      const opaque *encrypted_state;
      int ret;

      /* The client requested a new session ticket. */
      if (data_size == 0)
        {
          priv->session_ticket_renew = 1;
          return 0;
        }

      DECR_LEN (data_size, KEY_NAME_SIZE);
      memcpy (ticket.key_name, data, KEY_NAME_SIZE);
      data += KEY_NAME_SIZE;

      /* If the key name of the ticket does not match the one that we
         hold, issue a new ticket. */
      if (memcmp (ticket.key_name, priv->key.key_name, KEY_NAME_SIZE))
        {
          priv->session_ticket_renew = 1;
          return 0;
        }

      DECR_LEN (data_size, IV_SIZE);
      memcpy (ticket.IV, data, IV_SIZE);
      data += IV_SIZE;

      DECR_LEN (data_size, 2);
      ticket.encrypted_state_len = _gnutls_read_uint16 (data);
      data += 2;

      encrypted_state = data;

      DECR_LEN (data_size, ticket.encrypted_state_len);
      data += ticket.encrypted_state_len;

      DECR_LEN (data_size, MAC_SIZE);
      memcpy (ticket.mac, data, MAC_SIZE);

      ticket.encrypted_state = gnutls_malloc (ticket.encrypted_state_len);
      if (!ticket.encrypted_state)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }
      memcpy (ticket.encrypted_state, encrypted_state,
              ticket.encrypted_state_len);

      ret = decrypt_ticket (session, priv, &ticket);
      gnutls_free (ticket.encrypted_state);
      if (ret < 0)
        {
          priv->session_ticket_renew = 1;
          return 0;
        }
    }
  else                          /* Client */
    {
      if (data_size == 0)
        {
          priv->session_ticket_renew = 1;
          return 0;
        }
    }

  return 0;
}

/* returns a positive number if we send the extension data, zero if we
   do not want to send it, and a negative number on failure.
 */
static int
session_ticket_send_params (gnutls_session_t session,
                            opaque * data, size_t _data_size)
{
  ssize_t data_size = _data_size;
  session_ticket_ext_st *priv = NULL;
  extension_priv_data_t epriv;
  int ret;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_SESSION_TICKET,
                                  &epriv);
  if (ret >= 0)
    priv = epriv.ptr;

  if (priv == NULL || !priv->session_ticket_enable)
    return 0;

  if (session->security_parameters.entity == GNUTLS_SERVER)
    {
      if (priv && priv->session_ticket_renew)
        {
          return GNUTLS_E_INT_RET_0;
        }
    }
  else
    {
      ret =
        _gnutls_ext_get_resumed_session_data (session,
                                              GNUTLS_EXTENSION_SESSION_TICKET,
                                              &epriv);
      if (ret >= 0)
        priv = epriv.ptr;

      /* no previous data. Just advertize it */
      if (ret < 0)
        return GNUTLS_E_INT_RET_0;

      /* previous data had session tickets disabled. Don't advertize. Ignore. */
      if (!priv->session_ticket_enable)
        return 0;

      if (priv->session_ticket_len > 0)
        {
          DECR_LENGTH_RET (data_size, priv->session_ticket_len,
                           GNUTLS_E_SHORT_MEMORY_BUFFER);
          memcpy (data, priv->session_ticket, priv->session_ticket_len);

          return priv->session_ticket_len;
        }
    }
  return 0;
}


static void
session_ticket_deinit_data (extension_priv_data_t epriv)
{
  session_ticket_ext_st *priv = epriv.ptr;

  gnutls_free (priv->session_ticket);
  gnutls_free (priv);
}

static int
session_ticket_pack (extension_priv_data_t epriv, gnutls_buffer_st * ps)
{
  session_ticket_ext_st *priv = epriv.ptr;
  int ret;

  BUFFER_APPEND_PFX (ps, priv->session_ticket, priv->session_ticket_len);
  BUFFER_APPEND_NUM (ps, priv->session_ticket_enable);

  return 0;
}

static int
session_ticket_unpack (gnutls_buffer_st * ps, extension_priv_data_t * _priv)
{
  session_ticket_ext_st *priv = NULL;
  int ret;
  extension_priv_data_t epriv;
  gnutls_datum ticket;

  priv = gnutls_calloc (1, sizeof (*priv));
  if (priv == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  BUFFER_POP_DATUM (ps, &ticket);
  priv->session_ticket = ticket.data;
  priv->session_ticket_len = ticket.size;
  BUFFER_POP_NUM (ps, priv->session_ticket_enable);

  epriv.ptr = priv;
  *_priv = epriv;

  return 0;

error:
  gnutls_free (priv);
  return ret;
}



/**
 * gnutls_session_ticket_key_generate:
 * @key: is a pointer to a #gnutls_datum_t which will contain a newly
 * created key.
 *
 * Generate a random key to encrypt security parameters within
 * SessionTicket.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 * error code.
 *
 * Since: 2.10.0
 **/
int
gnutls_session_ticket_key_generate (gnutls_datum_t * key)
{
  int ret;

  key->size = sizeof (struct gnutls_session_ticket_key_st);
  key->data = gnutls_malloc (key->size);
  if (!key->data)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = _gnutls_rnd (GNUTLS_RND_RANDOM, key->data, key->size);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (key);
      return ret;
    }

  return 0;
}

/**
 * gnutls_session_ticket_enable_client:
 * @session: is a #gnutls_session_t structure.
 *
 * Request that the client should attempt session resumption using
 * SessionTicket.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 * error code.
 *
 * Since: 2.10.0
 **/
int
gnutls_session_ticket_enable_client (gnutls_session_t session)
{
  session_ticket_ext_st *priv = NULL;
  extension_priv_data_t epriv;

  if (!session)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  priv = gnutls_calloc (1, sizeof (*priv));
  if (priv == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }
  priv->session_ticket_enable = 1;
  epriv.ptr = priv;

  _gnutls_ext_set_session_data (session,
                                GNUTLS_EXTENSION_SESSION_TICKET, epriv);

  return 0;
}

/**
 * gnutls_session_ticket_enable_server:
 * @session: is a #gnutls_session_t structure.
 * @key: key to encrypt session parameters.
 *
 * Request that the server should attempt session resumption using
 * SessionTicket.  @key must be initialized with
 * gnutls_session_ticket_key_generate().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, or an
 * error code.
 *
 * Since: 2.10.0
 **/
int
gnutls_session_ticket_enable_server (gnutls_session_t session,
                                     const gnutls_datum_t * key)
{
  int ret;
  session_ticket_ext_st *priv = NULL;
  extension_priv_data_t epriv;

  if (!session || !key
      || key->size != sizeof (struct gnutls_session_ticket_key_st))
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  priv = gnutls_calloc (1, sizeof (*priv));
  if (priv == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }
  epriv.ptr = priv;

  ret = _gnutls_rnd (GNUTLS_RND_RANDOM, priv->session_ticket_IV, IV_SIZE);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  memcpy (&priv->key, key->data, key->size);
  priv->session_ticket_enable = 1;

  _gnutls_ext_set_session_data (session,
                                GNUTLS_EXTENSION_SESSION_TICKET, epriv);

  return 0;
}

int
_gnutls_send_new_session_ticket (gnutls_session_t session, int again)
{
  mbuffer_st *bufel = NULL;
  uint8_t *data = NULL, *p;
  int data_size = 0;
  int ret;
  struct ticket ticket;
  uint16_t ticket_len;
  session_ticket_ext_st *priv = NULL;
  extension_priv_data_t epriv;
  uint16_t epoch_saved = session->security_parameters.epoch_write;

  if (again == 0)
    {
      ret =
        _gnutls_ext_get_session_data (session,
                                      GNUTLS_EXTENSION_SESSION_TICKET,
                                      &epriv);
      if (ret < 0)
        return 0;
      priv = epriv.ptr;

      if (!priv->session_ticket_renew)
        return 0;

      /* XXX: Temporarily set write algorithms to be used.
         _gnutls_write_connection_state_init() does this job, but it also
         triggers encryption, while NewSessionTicket should not be
         encrypted in the record layer. */
      ret =
        _gnutls_epoch_set_keys (session,
                                session->security_parameters.epoch_next);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      session->security_parameters.epoch_write =
        session->security_parameters.epoch_next;

      ret = encrypt_ticket (session, priv, &ticket);
      session->security_parameters.epoch_write = epoch_saved;
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      ticket_len = KEY_NAME_SIZE + IV_SIZE + 2 + ticket.encrypted_state_len
        + MAC_SIZE;

      bufel =
        _gnutls_handshake_alloc (4 + 2 + ticket_len, 4 + 2 + ticket_len);
      if (!bufel)
        {
          gnutls_assert ();
          gnutls_free (ticket.encrypted_state);
          return GNUTLS_E_MEMORY_ERROR;
        }

      data = _mbuffer_get_udata_ptr (bufel);
      p = data;

      _gnutls_write_uint32 (session->internals.expire_time, p);
      p += 4;

      _gnutls_write_uint16 (ticket_len, p);
      p += 2;

      memcpy (p, ticket.key_name, KEY_NAME_SIZE);
      p += KEY_NAME_SIZE;

      memcpy (p, ticket.IV, IV_SIZE);
      p += IV_SIZE;

      _gnutls_write_uint16 (ticket.encrypted_state_len, p);
      p += 2;

      memcpy (p, ticket.encrypted_state, ticket.encrypted_state_len);
      gnutls_free (ticket.encrypted_state);
      p += ticket.encrypted_state_len;

      memcpy (p, ticket.mac, MAC_SIZE);
      p += MAC_SIZE;

      data_size = p - data;
    }
  ret = _gnutls_send_handshake (session, data_size ? bufel : NULL,
                                GNUTLS_HANDSHAKE_NEW_SESSION_TICKET);

  return ret;
}

int
_gnutls_recv_new_session_ticket (gnutls_session_t session)
{
  uint8_t *data = NULL, *p;
  int data_size;
  uint32_t lifetime_hint;
  uint16_t ticket_len;
  int ret;
  session_ticket_ext_st *priv = NULL;
  extension_priv_data_t epriv;

  ret =
    _gnutls_ext_get_session_data (session, GNUTLS_EXTENSION_SESSION_TICKET,
                                  &epriv);
  if (ret < 0)
    {
      gnutls_assert ();
      return 0;
    }
  priv = epriv.ptr;

  if (!priv->session_ticket_renew)
    return 0;

  ret = _gnutls_recv_handshake (session, &data, &data_size,
                                GNUTLS_HANDSHAKE_NEW_SESSION_TICKET,
                                MANDATORY_PACKET);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  p = data;
  DECR_LENGTH_COM (data_size, 4, goto error);
  lifetime_hint = _gnutls_read_uint32 (p);
  p += 4;

  DECR_LENGTH_COM (data_size, 2, goto error);
  ticket_len = _gnutls_read_uint16 (p);
  p += 2;

  DECR_LENGTH_COM (data_size, ticket_len, goto error);
  priv->session_ticket = gnutls_realloc (priv->session_ticket, ticket_len);
  if (!priv->session_ticket)
    {
      gnutls_assert ();
      gnutls_free (data);
      return GNUTLS_E_MEMORY_ERROR;
    }
  memcpy (priv->session_ticket, p, ticket_len);
  gnutls_free (data);
  priv->session_ticket_len = ticket_len;

  /* Discard the current session ID.  (RFC5077 3.4) */
  ret = _gnutls_generate_session_id (session->security_parameters.session_id,
                                     &session->
                                     security_parameters.session_id_size);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_free (priv->session_ticket);
      priv->session_ticket = NULL;
      return GNUTLS_E_INTERNAL_ERROR;
    }
  return 0;

error:
  gnutls_free (data);
  return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
}

#endif
