/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2010 Free
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

/* Functions that are supposed to run after the handshake procedure is
 * finished. These functions activate the established security parameters.
 */

#include <gnutls_int.h>
#include <gnutls_constate.h>
#include <gnutls_errors.h>
#include <gnutls_kx.h>
#include <gnutls_algorithms.h>
#include <gnutls_num.h>
#include <gnutls_datum.h>
#include <gnutls_state.h>
#include <gnutls_extensions.h>
#include <gnutls_buffers.h>

static const char keyexp[] = "key expansion";
static const int keyexp_length = sizeof (keyexp) - 1;

static const char ivblock[] = "IV block";
static const int ivblock_length = sizeof (ivblock) - 1;

static const char cliwrite[] = "client write key";
static const int cliwrite_length = sizeof (cliwrite) - 1;

static const char servwrite[] = "server write key";
static const int servwrite_length = sizeof (servwrite) - 1;

#define EXPORT_FINAL_KEY_SIZE 16

/* This function is to be called after handshake, when master_secret,
 *  client_random and server_random have been initialized. 
 * This function creates the keys and stores them into pending session.
 * (session->cipher_specs)
 */
static int
_gnutls_set_keys (gnutls_session_t session, record_parameters_st * params,
                  int hash_size, int IV_size, int key_size, int export_flag)
{
  /* FIXME: This function is too long
   */
  opaque rnd[2 * GNUTLS_RANDOM_SIZE];
  opaque rrnd[2 * GNUTLS_RANDOM_SIZE];
  int pos, ret;
  int block_size;
  char buf[65];
  /* avoid using malloc */
  opaque key_block[2 * MAX_HASH_SIZE + 2 * MAX_CIPHER_KEY_SIZE +
                   2 * MAX_CIPHER_BLOCK_SIZE];
  record_state_st *client_write, *server_write;

  client_write =
    session->security_parameters.entity ==
    GNUTLS_CLIENT ? &params->write : &params->read;
  server_write =
    session->security_parameters.entity ==
    GNUTLS_SERVER ? &params->write : &params->read;

  block_size = 2 * hash_size + 2 * key_size;
  if (export_flag == 0)
    block_size += 2 * IV_size;

  memcpy (rnd, session->security_parameters.server_random,
          GNUTLS_RANDOM_SIZE);
  memcpy (&rnd[GNUTLS_RANDOM_SIZE],
          session->security_parameters.client_random, GNUTLS_RANDOM_SIZE);

  memcpy (rrnd, session->security_parameters.client_random,
          GNUTLS_RANDOM_SIZE);
  memcpy (&rrnd[GNUTLS_RANDOM_SIZE],
          session->security_parameters.server_random, GNUTLS_RANDOM_SIZE);

  if (session->security_parameters.version == GNUTLS_SSL3)
    {                           /* SSL 3 */
      ret =
        _gnutls_ssl3_generate_random
        (session->security_parameters.master_secret, GNUTLS_MASTER_SIZE, rnd,
         2 * GNUTLS_RANDOM_SIZE, block_size, key_block);
    }
  else
    {                           /* TLS 1.0 */
      ret =
        _gnutls_PRF (session, session->security_parameters.master_secret,
                     GNUTLS_MASTER_SIZE, keyexp, keyexp_length,
                     rnd, 2 * GNUTLS_RANDOM_SIZE, block_size, key_block);
    }

  if (ret < 0)
    return gnutls_assert_val (ret);

  _gnutls_hard_log ("INT: KEY BLOCK[%d]: %s\n", block_size,
                    _gnutls_bin2hex (key_block, block_size, buf,
                                     sizeof (buf), NULL));

  pos = 0;
  if (hash_size > 0)
    {

      if (_gnutls_sset_datum
          (&client_write->mac_secret, &key_block[pos], hash_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

      pos += hash_size;

      if (_gnutls_sset_datum
          (&server_write->mac_secret, &key_block[pos], hash_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

      pos += hash_size;
    }

  if (key_size > 0)
    {
      opaque key1[EXPORT_FINAL_KEY_SIZE];
      opaque key2[EXPORT_FINAL_KEY_SIZE];
      opaque *client_write_key, *server_write_key;
      int client_write_key_size, server_write_key_size;

      if (export_flag == 0)
        {
          client_write_key = &key_block[pos];
          client_write_key_size = key_size;

          pos += key_size;

          server_write_key = &key_block[pos];
          server_write_key_size = key_size;

          pos += key_size;

        }
      else
        {                       /* export */
          client_write_key = key1;
          server_write_key = key2;

          /* generate the final keys */

          if (session->security_parameters.version == GNUTLS_SSL3)
            {                   /* SSL 3 */
              ret =
                _gnutls_ssl3_hash_md5 (&key_block[pos],
                                       key_size, rrnd,
                                       2 * GNUTLS_RANDOM_SIZE,
                                       EXPORT_FINAL_KEY_SIZE,
                                       client_write_key);

            }
          else
            {                   /* TLS 1.0 */
              ret =
                _gnutls_PRF (session, &key_block[pos], key_size,
                             cliwrite, cliwrite_length,
                             rrnd,
                             2 * GNUTLS_RANDOM_SIZE,
                             EXPORT_FINAL_KEY_SIZE, client_write_key);
            }

          if (ret < 0)
            return gnutls_assert_val (ret);

          client_write_key_size = EXPORT_FINAL_KEY_SIZE;
          pos += key_size;

          if (session->security_parameters.version == GNUTLS_SSL3)
            {                   /* SSL 3 */
              ret =
                _gnutls_ssl3_hash_md5 (&key_block[pos], key_size,
                                       rnd, 2 * GNUTLS_RANDOM_SIZE,
                                       EXPORT_FINAL_KEY_SIZE,
                                       server_write_key);
            }
          else
            {                   /* TLS 1.0 */
              ret =
                _gnutls_PRF (session, &key_block[pos], key_size,
                             servwrite, servwrite_length,
                             rrnd, 2 * GNUTLS_RANDOM_SIZE,
                             EXPORT_FINAL_KEY_SIZE, server_write_key);
            }

          if (ret < 0)
            return gnutls_assert_val (ret);

          server_write_key_size = EXPORT_FINAL_KEY_SIZE;
          pos += key_size;
        }

      if (_gnutls_sset_datum
          (&client_write->key, client_write_key, client_write_key_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

      _gnutls_hard_log ("INT: CLIENT WRITE KEY [%d]: %s\n",
                        client_write_key_size,
                        _gnutls_bin2hex (client_write_key,
                                         client_write_key_size, buf,
                                         sizeof (buf), NULL));

      if (_gnutls_sset_datum
          (&server_write->key, server_write_key, server_write_key_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

      _gnutls_hard_log ("INT: SERVER WRITE KEY [%d]: %s\n",
                        server_write_key_size,
                        _gnutls_bin2hex (server_write_key,
                                         server_write_key_size, buf,
                                         sizeof (buf), NULL));

    }


  /* IV generation in export and non export ciphers.
   */
  if (IV_size > 0 && export_flag == 0)
    {
      if (_gnutls_sset_datum
          (&client_write->IV, &key_block[pos], IV_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

      pos += IV_size;

      if (_gnutls_sset_datum
          (&server_write->IV, &key_block[pos], IV_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

      pos += IV_size;

    }
  else if (IV_size > 0 && export_flag != 0)
    {
      opaque iv_block[MAX_CIPHER_BLOCK_SIZE * 2];

      if (session->security_parameters.version == GNUTLS_SSL3)
        {                       /* SSL 3 */
          ret = _gnutls_ssl3_hash_md5 ("", 0,
                                       rrnd, GNUTLS_RANDOM_SIZE * 2,
                                       IV_size, iv_block);

          if (ret < 0)
            return gnutls_assert_val (ret);


          ret = _gnutls_ssl3_hash_md5 ("", 0, rnd,
                                       GNUTLS_RANDOM_SIZE * 2,
                                       IV_size, &iv_block[IV_size]);

        }
      else
        {                       /* TLS 1.0 */
          ret = _gnutls_PRF (session, "", 0,
                             ivblock, ivblock_length, rrnd,
                             2 * GNUTLS_RANDOM_SIZE, IV_size * 2, iv_block);
        }

      if (ret < 0)
        return gnutls_assert_val (ret);

      if (_gnutls_sset_datum (&client_write->IV, iv_block, IV_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

      if (_gnutls_sset_datum
          (&server_write->IV, &iv_block[IV_size], IV_size) < 0)
        return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);
    }

  return 0;
}

static int
_gnutls_init_record_state (record_parameters_st * params, int read,
                           record_state_st * state)
{
  int ret;

  ret = _gnutls_cipher_init (&state->cipher_state,
                             params->cipher_algorithm,
                             &state->key, &state->IV);
  if (ret < 0 && params->cipher_algorithm != GNUTLS_CIPHER_NULL)
    return gnutls_assert_val (ret);

  state->compression_state =
    _gnutls_comp_init (params->compression_algorithm, read);

  if (state->compression_state == GNUTLS_COMP_FAILED)
    return gnutls_assert_val (GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM);

  return 0;
}

int
_gnutls_epoch_set_cipher_suite (gnutls_session_t session,
                                int epoch_rel, cipher_suite_st * suite)
{
  gnutls_cipher_algorithm_t cipher_algo;
  gnutls_mac_algorithm_t mac_algo;
  record_parameters_st *params;
  int ret;

  ret = _gnutls_epoch_get (session, epoch_rel, &params);
  if (ret < 0)
    return gnutls_assert_val (ret);

  if (params->initialized
      || params->cipher_algorithm != GNUTLS_CIPHER_UNKNOWN
      || params->mac_algorithm != GNUTLS_MAC_UNKNOWN)
    return gnutls_assert_val (GNUTLS_E_INTERNAL_ERROR);

  cipher_algo = _gnutls_cipher_suite_get_cipher_algo (suite);
  mac_algo = _gnutls_cipher_suite_get_mac_algo (suite);

  if (_gnutls_cipher_is_ok (cipher_algo) != 0
      || _gnutls_mac_is_ok (mac_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_UNWANTED_ALGORITHM);

  params->cipher_algorithm = cipher_algo;
  params->mac_algorithm = mac_algo;

  return 0;
}

int
_gnutls_epoch_set_compression (gnutls_session_t session,
                               int epoch_rel,
                               gnutls_compression_method_t comp_algo)
{
  record_parameters_st *params;
  int ret;

  ret = _gnutls_epoch_get (session, epoch_rel, &params);
  if (ret < 0)
    return gnutls_assert_val (ret);

  if (params->initialized
      || params->compression_algorithm != GNUTLS_COMP_UNKNOWN)
    return gnutls_assert_val (GNUTLS_E_INTERNAL_ERROR);

  if (_gnutls_compression_is_ok (comp_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM);

  params->compression_algorithm = comp_algo;

  return 0;
}

void
_gnutls_epoch_set_null_algos (gnutls_session_t session,
                              record_parameters_st * params)
{
  /* This is only called on startup. We are extra paranoid about this
     because it may cause unencrypted application data to go out on
     the wire. */
  if (params->initialized || params->epoch != 0)
    {
      gnutls_assert ();
      return;
    }

  params->cipher_algorithm = GNUTLS_CIPHER_NULL;
  params->mac_algorithm = GNUTLS_MAC_NULL;
  params->compression_algorithm = GNUTLS_COMP_NULL;
  params->initialized = 1;
}

int
_gnutls_epoch_set_keys (gnutls_session_t session, uint16_t epoch)
{
  int hash_size;
  int IV_size;
  int key_size, export_flag;
  gnutls_cipher_algorithm_t cipher_algo;
  gnutls_mac_algorithm_t mac_algo;
  gnutls_compression_method_t comp_algo;
  record_parameters_st *params;
  int ret;

  ret = _gnutls_epoch_get (session, epoch, &params);
  if (ret < 0)
    return gnutls_assert_val (ret);

  if (params->initialized)
    return 0;

  _gnutls_record_log
    ("REC[%p]: Initializing epoch #%u\n", session, params->epoch);

  cipher_algo = params->cipher_algorithm;
  mac_algo = params->mac_algorithm;
  comp_algo = params->compression_algorithm;

  if (_gnutls_cipher_is_ok (cipher_algo) != 0
      || _gnutls_mac_is_ok (mac_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_INTERNAL_ERROR);

  if (_gnutls_compression_is_ok (comp_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM);

  IV_size = _gnutls_cipher_get_iv_size (cipher_algo);
  key_size = gnutls_cipher_get_key_size (cipher_algo);
  export_flag = _gnutls_cipher_get_export_flag (cipher_algo);
  hash_size = _gnutls_hash_get_algo_len (mac_algo);

  ret = _gnutls_set_keys
    (session, params, hash_size, IV_size, key_size, export_flag);
  if (ret < 0)
    return gnutls_assert_val (ret);

  ret = _gnutls_init_record_state (params, 1, &params->read);
  if (ret < 0)
    return gnutls_assert_val (ret);

  ret = _gnutls_init_record_state (params, 0, &params->write);
  if (ret < 0)
    return gnutls_assert_val (ret);

  _gnutls_record_log ("REC[%p]: Epoch #%u ready\n", session, params->epoch);

  params->initialized = 1;
  return 0;
}


#define CPY_COMMON dst->entity = src->entity; \
	dst->kx_algorithm = src->kx_algorithm; \
	memcpy( &dst->current_cipher_suite, &src->current_cipher_suite, sizeof(cipher_suite_st)); \
	memcpy( dst->master_secret, src->master_secret, GNUTLS_MASTER_SIZE); \
	memcpy( dst->client_random, src->client_random, GNUTLS_RANDOM_SIZE); \
	memcpy( dst->server_random, src->server_random, GNUTLS_RANDOM_SIZE); \
	memcpy( dst->session_id, src->session_id, TLS_MAX_SESSION_ID_SIZE); \
	dst->session_id_size = src->session_id_size; \
	dst->cert_type = src->cert_type; \
	dst->timestamp = src->timestamp; \
	dst->max_record_recv_size = src->max_record_recv_size; \
	dst->max_record_send_size = src->max_record_send_size; \
	dst->version = src->version

static void
_gnutls_set_resumed_parameters (gnutls_session_t session)
{
  security_parameters_st *src =
    &session->internals.resumed_security_parameters;
  security_parameters_st *dst = &session->security_parameters;

  CPY_COMMON;
}

/* Sets the current connection session to conform with the
 * Security parameters(pending session), and initializes encryption.
 * Actually it initializes and starts encryption ( so it needs
 * secrets and random numbers to have been negotiated)
 * This is to be called after sending the Change Cipher Spec packet.
 */
int
_gnutls_connection_state_init (gnutls_session_t session)
{
  int ret;

/* Setup the master secret 
 */
  if ((ret = _gnutls_generate_master (session, 0)) < 0)
    return gnutls_assert_val (ret);

  return 0;
}



static int
_gnutls_check_algos (gnutls_session_t session,
                     cipher_suite_st * suite,
                     gnutls_compression_method_t comp_algo)
{
  gnutls_cipher_algorithm_t cipher_algo;
  gnutls_mac_algorithm_t mac_algo;

  cipher_algo = _gnutls_cipher_suite_get_cipher_algo (suite);
  mac_algo = _gnutls_cipher_suite_get_mac_algo (suite);

  if (_gnutls_cipher_is_ok (cipher_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_INTERNAL_ERROR);

  if (_gnutls_cipher_priority (session, cipher_algo) < 0)
    return gnutls_assert_val (GNUTLS_E_UNWANTED_ALGORITHM);


  if (_gnutls_mac_is_ok (mac_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_INTERNAL_ERROR);

  if (_gnutls_mac_priority (session, mac_algo) < 0)
    return gnutls_assert_val (GNUTLS_E_UNWANTED_ALGORITHM);


  if (_gnutls_compression_is_ok (comp_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM);

  return 0;
}

/* Initializes the read connection session
 * (read encrypted data)
 */
int
_gnutls_read_connection_state_init (gnutls_session_t session)
{
  const uint16_t epoch_next = session->security_parameters.epoch_next;
  int ret;

  /* Update internals from CipherSuite selected.
   * If we are resuming just copy the connection session
   */
  if (session->internals.resumed == RESUME_FALSE)
    {
      ret = _gnutls_check_algos (session,
                                 &session->
                                 security_parameters.current_cipher_suite,
                                 session->internals.compression_method);
      if (ret < 0)
        return ret;

      ret = _gnutls_set_kx (session,
                            _gnutls_cipher_suite_get_kx_algo
                            (&session->
                             security_parameters.current_cipher_suite));
      if (ret < 0)
        return ret;
    }
  else if (session->security_parameters.entity == GNUTLS_CLIENT)
    _gnutls_set_resumed_parameters (session);

  ret = _gnutls_epoch_set_keys (session, epoch_next);
  if (ret < 0)
    return ret;

  _gnutls_handshake_log ("HSK[%p]: Cipher Suite: %s\n",
                         session,
                         _gnutls_cipher_suite_get_name
                         (&session->
                          security_parameters.current_cipher_suite));

  session->security_parameters.epoch_read = epoch_next;
  _gnutls_epoch_gc (session);

  return 0;
}



/* Initializes the write connection session
 * (write encrypted data)
 */
int
_gnutls_write_connection_state_init (gnutls_session_t session)
{
  const uint16_t epoch_next = session->security_parameters.epoch_next;
  int ret;

/* Update internals from CipherSuite selected.
 * If we are resuming just copy the connection session
 */
  if (session->internals.resumed == RESUME_FALSE)
    {
      ret = _gnutls_check_algos (session,
                                 &session->
                                 security_parameters.current_cipher_suite,
                                 session->internals.compression_method);
      if (ret < 0)
        return ret;

      ret = _gnutls_set_kx (session,
                            _gnutls_cipher_suite_get_kx_algo
                            (&session->
                             security_parameters.current_cipher_suite));
      if (ret < 0)
        return ret;
    }
  else if (session->security_parameters.entity == GNUTLS_SERVER)
    _gnutls_set_resumed_parameters (session);

  ret = _gnutls_epoch_set_keys (session, epoch_next);
  if (ret < 0)
    return gnutls_assert_val (ret);

  _gnutls_handshake_log ("HSK[%p]: Cipher Suite: %s\n", session,
                         _gnutls_cipher_suite_get_name
                         (&session->
                          security_parameters.current_cipher_suite));

  _gnutls_handshake_log
    ("HSK[%p]: Initializing internal [write] cipher sessions\n", session);

  session->security_parameters.epoch_write = epoch_next;
  _gnutls_epoch_gc (session);

  return 0;
}

/* Sets the specified kx algorithm into pending session
 */
int
_gnutls_set_kx (gnutls_session_t session, gnutls_kx_algorithm_t algo)
{

  if (_gnutls_kx_is_ok (algo) == 0)
    {
      session->security_parameters.kx_algorithm = algo;
    }
  else
    return gnutls_assert_val (GNUTLS_E_INTERNAL_ERROR);

  if (_gnutls_kx_priority (session, algo) < 0)
    return gnutls_assert_val (GNUTLS_E_UNWANTED_ALGORITHM);

  return 0;
}

static inline int
epoch_resolve (gnutls_session_t session,
               unsigned int epoch_rel, uint16_t * epoch_out)
{
  switch (epoch_rel)
    {
    case EPOCH_READ_CURRENT:
      *epoch_out = session->security_parameters.epoch_read;
      return 0;

    case EPOCH_WRITE_CURRENT:
      *epoch_out = session->security_parameters.epoch_write;
      return 0;

    case EPOCH_NEXT:
      *epoch_out = session->security_parameters.epoch_next;
      return 0;

    default:
      if (epoch_rel > 0xffffu)
        return gnutls_assert_val (GNUTLS_E_INVALID_REQUEST);

      *epoch_out = epoch_rel;
      return 0;
    }
}

static inline record_parameters_st **
epoch_get_slot (gnutls_session_t session, uint16_t epoch)
{
  uint16_t epoch_index = epoch - session->security_parameters.epoch_min;

  if (epoch_index >= MAX_EPOCH_INDEX)
    {
      gnutls_assert ();
      return NULL;
    }

  /* The slot may still be empty (NULL) */
  return &session->record_parameters[epoch_index];
}

int
_gnutls_epoch_get (gnutls_session_t session, unsigned int epoch_rel,
                   record_parameters_st ** params_out)
{
  uint16_t epoch;
  record_parameters_st **params;
  int ret;

  ret = epoch_resolve (session, epoch_rel, &epoch);
  if (ret < 0)
    return gnutls_assert_val (ret);

  params = epoch_get_slot (session, epoch);
  if (params == NULL || *params == NULL)
    return gnutls_assert_val (GNUTLS_E_INVALID_REQUEST);

  *params_out = *params;

  return 0;
}

int
_gnutls_epoch_alloc (gnutls_session_t session, uint16_t epoch,
                     record_parameters_st ** out)
{
  record_parameters_st **slot;

  _gnutls_record_log ("REC[%p]: Allocating epoch #%u\n", session, epoch);

  slot = epoch_get_slot (session, epoch);

  /* If slot out of range or not empty. */
  if (slot == NULL)
    return gnutls_assert_val (GNUTLS_E_INVALID_REQUEST);

  if (*slot != NULL)
    return gnutls_assert_val (GNUTLS_E_INVALID_REQUEST);

  *slot = gnutls_calloc (1, sizeof (record_parameters_st));
  if (*slot == NULL)
    return gnutls_assert_val (GNUTLS_E_MEMORY_ERROR);

  (*slot)->epoch = epoch;
  (*slot)->cipher_algorithm = GNUTLS_CIPHER_UNKNOWN;
  (*slot)->mac_algorithm = GNUTLS_MAC_UNKNOWN;
  (*slot)->compression_algorithm = GNUTLS_COMP_UNKNOWN;

  if (out != NULL)
    *out = *slot;

  return 0;
}

static inline int
epoch_alive (gnutls_session_t session, record_parameters_st * params)
{
  const security_parameters_st *sp = &session->security_parameters;

  /* DTLS will, in addition, need to check the epoch timeout value. */
  return (params->epoch == sp->epoch_read
          || params->epoch == sp->epoch_write
          || params->epoch == sp->epoch_next);
}

void
_gnutls_epoch_gc (gnutls_session_t session)
{
  int i, j;
  unsigned int min_index = 0;

  _gnutls_record_log ("REC[%p]: Start of epoch cleanup\n", session);

  /* Free all dead cipher state */
  for (i = 0; i < MAX_EPOCH_INDEX; i++)
    if (session->record_parameters[i] != NULL
        && !epoch_alive (session, session->record_parameters[i]))
      {
        _gnutls_epoch_free (session, session->record_parameters[i]);
        session->record_parameters[i] = NULL;
      }

  /* Look for contiguous NULLs at the start of the array */
  for (i = 0; i < MAX_EPOCH_INDEX && session->record_parameters[i] == NULL;
       i++);
  min_index = i;

  /* Pick up the slack in the epoch window. */
  for (i = 0, j = min_index; j < MAX_EPOCH_INDEX; i++, j++)
    session->record_parameters[i] = session->record_parameters[j];

  /* Set the new epoch_min */
  if (session->record_parameters[0] != NULL)
    session->security_parameters.epoch_min =
      session->record_parameters[0]->epoch;

  _gnutls_record_log ("REC[%p]: End of epoch cleanup\n", session);
}

static inline void
free_record_state (record_state_st * state, int read)
{
  _gnutls_free_datum (&state->mac_secret);
  _gnutls_free_datum (&state->IV);
  _gnutls_free_datum (&state->key);

  _gnutls_cipher_deinit (&state->cipher_state);

  if (state->compression_state != NULL)
    _gnutls_comp_deinit (state->compression_state, read);
}

void
_gnutls_epoch_free (gnutls_session_t session, record_parameters_st * params)
{
  _gnutls_record_log ("REC[%p]: Epoch #%u freed\n", session, params->epoch);

  free_record_state (&params->read, 1);
  free_record_state (&params->write, 0);

  gnutls_free (params);
}
