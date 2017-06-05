/*
 * Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
 * Free Software Foundation, Inc.
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

/* Functions to manipulate the session (gnutls_int.h), and some other stuff
 * are included here. The file's name is traditionally gnutls_state even if the
 * state has been renamed to session.
 */

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_auth.h>
#include <gnutls_num.h>
#include <gnutls_datum.h>
#include <gnutls_db.h>
#include <gnutls_record.h>
#include <gnutls_handshake.h>
#include <gnutls_dh.h>
#include <gnutls_buffers.h>
#include <gnutls_mbuffers.h>
#include <gnutls_state.h>
#include <gnutls_constate.h>
#include <auth_cert.h>
#include <auth_anon.h>
#include <auth_psk.h>
#include <gnutls_algorithms.h>
#include <gnutls_rsa_export.h>
#include <gnutls_extensions.h>
#include <system.h>

/* These should really be static, but src/tests.c calls them.  Make
   them public functions?  */
void
_gnutls_rsa_pms_set_version (gnutls_session_t session,
                             unsigned char major, unsigned char minor);

void
_gnutls_session_cert_type_set (gnutls_session_t session,
                               gnutls_certificate_type_t ct)
{
  session->security_parameters.cert_type = ct;
}

/**
 * gnutls_cipher_get:
 * @session: is a #gnutls_session_t structure.
 *
 * Get currently used cipher.
 *
 * Returns: the currently used cipher, a #gnutls_cipher_algorithm_t
 *   type.
 **/
gnutls_cipher_algorithm_t
gnutls_cipher_get (gnutls_session_t session)
{
  record_parameters_st *record_params;
  _gnutls_epoch_get (session, EPOCH_READ_CURRENT, &record_params);

  return record_params->cipher_algorithm;
}

/**
 * gnutls_certificate_type_get:
 * @session: is a #gnutls_session_t structure.
 *
 * The certificate type is by default X.509, unless it is negotiated
 * as a TLS extension.
 *
 * Returns: the currently used #gnutls_certificate_type_t certificate
 *   type.
 **/
gnutls_certificate_type_t
gnutls_certificate_type_get (gnutls_session_t session)
{
  return session->security_parameters.cert_type;
}

/**
 * gnutls_kx_get:
 * @session: is a #gnutls_session_t structure.
 *
 * Get currently used key exchange algorithm.
 *
 * Returns: the key exchange algorithm used in the last handshake, a
 *   #gnutls_kx_algorithm_t value.
 **/
gnutls_kx_algorithm_t
gnutls_kx_get (gnutls_session_t session)
{
  return session->security_parameters.kx_algorithm;
}

/**
 * gnutls_mac_get:
 * @session: is a #gnutls_session_t structure.
 *
 * Get currently used MAC algorithm.
 *
 * Returns: the currently used mac algorithm, a
 *   #gnutls_mac_algorithm_t value.
 **/
gnutls_mac_algorithm_t
gnutls_mac_get (gnutls_session_t session)
{
  record_parameters_st *record_params;
  _gnutls_epoch_get (session, EPOCH_READ_CURRENT, &record_params);

  return record_params->mac_algorithm;
}

/**
 * gnutls_compression_get:
 * @session: is a #gnutls_session_t structure.
 *
 * Get currently used compression algorithm.
 *
 * Returns: the currently used compression method, a
 *   #gnutls_compression_method_t value.
 **/
gnutls_compression_method_t
gnutls_compression_get (gnutls_session_t session)
{
  record_parameters_st *record_params;
  _gnutls_epoch_get (session, EPOCH_READ_CURRENT, &record_params);

  return record_params->compression_algorithm;
}

/* Check if the given certificate type is supported.
 * This means that it is enabled by the priority functions,
 * and a matching certificate exists.
 */
int
_gnutls_session_cert_type_supported (gnutls_session_t session,
                                     gnutls_certificate_type_t cert_type)
{
  unsigned i;
  unsigned cert_found = 0;
  gnutls_certificate_credentials_t cred;

  if (session->security_parameters.entity == GNUTLS_SERVER)
    {
      cred = (gnutls_certificate_credentials_t)
        _gnutls_get_cred (session->key, GNUTLS_CRD_CERTIFICATE, NULL);

      if (cred == NULL)
        return GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE;

      if (cred->server_get_cert_callback == NULL
          && cred->get_cert_callback == NULL)
        {
          for (i = 0; i < cred->ncerts; i++)
            {
              if (cred->cert_list[i][0].cert_type == cert_type)
                {
                  cert_found = 1;
                  break;
                }
            }

          if (cert_found == 0)
            /* no certificate is of that type.
             */
            return GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE;
        }
    }

  if (session->internals.priorities.cert_type.algorithms == 0
      && cert_type == DEFAULT_CERT_TYPE)
    return 0;

  for (i = 0; i < session->internals.priorities.cert_type.algorithms; i++)
    {
      if (session->internals.priorities.cert_type.priority[i] == cert_type)
        {
          return 0;             /* ok */
        }
    }

  return GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE;
}


/* this function deinitializes all the internal parameters stored
 * in a session struct.
 */
inline static void
deinit_internal_params (gnutls_session_t session)
{
  if (session->internals.params.free_dh_params)
    gnutls_dh_params_deinit (session->internals.params.dh_params);

  if (session->internals.params.free_rsa_params)
    gnutls_rsa_params_deinit (session->internals.params.rsa_params);

  _gnutls_handshake_hash_buffers_clear (session);

  memset (&session->internals.params, 0, sizeof (session->internals.params));
}

/* This function will clear all the variables in internals
 * structure within the session, which depend on the current handshake.
 * This is used to allow further handshakes.
 */
static void
_gnutls_handshake_internal_state_init (gnutls_session_t session)
{
  session->internals.extensions_sent_size = 0;

  /* by default no selected certificate */
  session->internals.adv_version_major = 0;
  session->internals.adv_version_minor = 0;
  session->internals.v2_hello = 0;
  memset (&session->internals.handshake_header_buffer, 0,
          sizeof (handshake_header_buffer_st));
  session->internals.direction = 0;

  /* use out of band data for the last
   * handshake messages received.
   */
  session->internals.last_handshake_in = -1;
  session->internals.last_handshake_out = -1;

  session->internals.resumable = RESUME_TRUE;
}

void
_gnutls_handshake_internal_state_clear (gnutls_session_t session)
{
  _gnutls_handshake_internal_state_init (session);

  _gnutls_free_datum (&session->internals.recv_buffer);

  deinit_internal_params (session);

}

#define MIN_DH_BITS 727
/**
 * gnutls_init:
 * @con_end: indicate if this session is to be used for server or client.
 * @session: is a pointer to a #gnutls_session_t structure.
 *
 * This function initializes the current session to null. Every
 * session must be initialized before use, so internal structures can
 * be allocated.  This function allocates structures which can only
 * be free'd by calling gnutls_deinit().  Returns zero on success.
 *
 * @con_end can be one of %GNUTLS_CLIENT and %GNUTLS_SERVER.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_init (gnutls_session_t * session, gnutls_connection_end_t con_end)
{
  int ret;
  record_parameters_st *epoch;

  *session = gnutls_calloc (1, sizeof (struct gnutls_session_int));
  if (*session == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  ret = _gnutls_epoch_alloc (*session, 0, &epoch);
  if (ret < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* Set all NULL algos on epoch 0 */
  _gnutls_epoch_set_null_algos (*session, epoch);

  (*session)->security_parameters.epoch_next = 1;

  (*session)->security_parameters.entity = con_end;

  /* the default certificate type for TLS */
  (*session)->security_parameters.cert_type = DEFAULT_CERT_TYPE;

  /* Initialize buffers */
  _gnutls_buffer_init (&(*session)->internals.application_data_buffer);
  _gnutls_buffer_init (&(*session)->internals.handshake_data_buffer);
  _gnutls_buffer_init (&(*session)->internals.handshake_hash_buffer);
  _gnutls_buffer_init (&(*session)->internals.ia_data_buffer);

  _mbuffer_init (&(*session)->internals.record_send_buffer);
  _mbuffer_init (&(*session)->internals.record_recv_buffer);

  _mbuffer_init (&(*session)->internals.handshake_send_buffer);
  _gnutls_buffer_init (&(*session)->internals.handshake_recv_buffer);

  (*session)->key = gnutls_calloc (1, sizeof (struct gnutls_key_st));
  if ((*session)->key == NULL)
    {
      gnutls_free (*session);
      *session = NULL;
      return GNUTLS_E_MEMORY_ERROR;
    }

  (*session)->internals.expire_time = DEFAULT_EXPIRE_TIME;      /* one hour default */

  gnutls_dh_set_prime_bits ((*session), MIN_DH_BITS);

  gnutls_transport_set_lowat ((*session), DEFAULT_LOWAT);       /* the default for tcp */

  gnutls_handshake_set_max_packet_length ((*session),
                                          MAX_HANDSHAKE_PACKET_SIZE);

  /* set the socket pointers to -1;
   */
  (*session)->internals.transport_recv_ptr = (gnutls_transport_ptr_t) - 1;
  (*session)->internals.transport_send_ptr = (gnutls_transport_ptr_t) - 1;

  /* set the default maximum record size for TLS
   */
  (*session)->security_parameters.max_record_recv_size =
    DEFAULT_MAX_RECORD_SIZE;
  (*session)->security_parameters.max_record_send_size =
    DEFAULT_MAX_RECORD_SIZE;

  /* everything else not initialized here is initialized
   * as NULL or 0. This is why calloc is used.
   */

  _gnutls_handshake_internal_state_init (*session);

  /* emulate old gnutls behavior for old applications that do not use the priority_*
   * functions.
   */
  (*session)->internals.priorities.sr = SR_PARTIAL;

#ifdef HAVE_WRITEV
  gnutls_transport_set_vec_push_function (*session, system_writev);
#else
  gnutls_transport_set_push_function (*session, system_write);
#endif
  gnutls_transport_set_pull_function (*session, system_read);
  gnutls_transport_set_errno_function (*session, system_errno);

  return 0;
}

/* returns RESUME_FALSE or RESUME_TRUE.
 */
int
_gnutls_session_is_resumable (gnutls_session_t session)
{
  return session->internals.resumable;
}


/**
 * gnutls_deinit:
 * @session: is a #gnutls_session_t structure.
 *
 * This function clears all buffers associated with the @session.
 * This function will also remove session data from the session
 * database if the session was terminated abnormally.
 **/
void
gnutls_deinit (gnutls_session_t session)
{
  unsigned int i;

  if (session == NULL)
    return;

  /* remove auth info firstly */
  _gnutls_free_auth_info (session);

  _gnutls_handshake_internal_state_clear (session);
  _gnutls_handshake_io_buffer_clear (session);
  _gnutls_ext_free_session_data (session);

  for (i = 0; i < MAX_EPOCH_INDEX; i++)
    if (session->record_parameters[i] != NULL)
      {
        _gnutls_epoch_free (session, session->record_parameters[i]);
        session->record_parameters[i] = NULL;
      }

  _gnutls_buffer_clear (&session->internals.ia_data_buffer);
  _gnutls_buffer_clear (&session->internals.handshake_hash_buffer);
  _gnutls_buffer_clear (&session->internals.handshake_data_buffer);
  _gnutls_buffer_clear (&session->internals.application_data_buffer);
  _mbuffer_clear (&session->internals.record_recv_buffer);
  _mbuffer_clear (&session->internals.record_send_buffer);

  gnutls_credentials_clear (session);
  _gnutls_selected_certs_deinit (session);

  if (session->key != NULL)
    {
      _gnutls_mpi_release (&session->key->KEY);
      _gnutls_mpi_release (&session->key->client_Y);
      _gnutls_mpi_release (&session->key->client_p);
      _gnutls_mpi_release (&session->key->client_g);

      _gnutls_mpi_release (&session->key->u);
      _gnutls_mpi_release (&session->key->a);
      _gnutls_mpi_release (&session->key->x);
      _gnutls_mpi_release (&session->key->A);
      _gnutls_mpi_release (&session->key->B);
      _gnutls_mpi_release (&session->key->b);

      /* RSA */
      _gnutls_mpi_release (&session->key->rsa[0]);
      _gnutls_mpi_release (&session->key->rsa[1]);

      _gnutls_mpi_release (&session->key->dh_secret);
      gnutls_free (session->key);

      session->key = NULL;
    }

  memset (session, 0, sizeof (struct gnutls_session_int));
  gnutls_free (session);
}

/* Returns the minimum prime bits that are acceptable.
 */
int
_gnutls_dh_get_allowed_prime_bits (gnutls_session_t session)
{
  return session->internals.dh_prime_bits;
}

int
_gnutls_dh_set_peer_public (gnutls_session_t session, bigint_t public)
{
  dh_info_st *dh;
  int ret;

  switch (gnutls_auth_get_type (session))
    {
    case GNUTLS_CRD_ANON:
      {
        anon_auth_info_t info;
        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        dh = &info->dh;
        break;
      }
    case GNUTLS_CRD_PSK:
      {
        psk_auth_info_t info;
        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        dh = &info->dh;
        break;
      }
    case GNUTLS_CRD_CERTIFICATE:
      {
        cert_auth_info_t info;

        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        dh = &info->dh;
        break;
      }
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  if (dh->public_key.data)
    _gnutls_free_datum (&dh->public_key);

  ret = _gnutls_mpi_dprint_lz (public, &dh->public_key);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

int
_gnutls_dh_set_secret_bits (gnutls_session_t session, unsigned bits)
{
  switch (gnutls_auth_get_type (session))
    {
    case GNUTLS_CRD_ANON:
      {
        anon_auth_info_t info;
        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;
        info->dh.secret_bits = bits;
        break;
      }
    case GNUTLS_CRD_PSK:
      {
        psk_auth_info_t info;
        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;
        info->dh.secret_bits = bits;
        break;
      }
    case GNUTLS_CRD_CERTIFICATE:
      {
        cert_auth_info_t info;

        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        info->dh.secret_bits = bits;
        break;
    default:
        gnutls_assert ();
        return GNUTLS_E_INTERNAL_ERROR;
      }
    }

  return 0;
}

/* This function will set in the auth info structure the
 * RSA exponent and the modulus.
 */
int
_gnutls_rsa_export_set_pubkey (gnutls_session_t session,
                               bigint_t exponent, bigint_t modulus)
{
  cert_auth_info_t info;
  int ret;

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    return GNUTLS_E_INTERNAL_ERROR;

  if (info->rsa_export.modulus.data)
    _gnutls_free_datum (&info->rsa_export.modulus);

  if (info->rsa_export.exponent.data)
    _gnutls_free_datum (&info->rsa_export.exponent);

  ret = _gnutls_mpi_dprint_lz (modulus, &info->rsa_export.modulus);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_mpi_dprint_lz (exponent, &info->rsa_export.exponent);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&info->rsa_export.modulus);
      return ret;
    }

  return 0;
}


/* Sets the prime and the generator in the auth info structure.
 */
int
_gnutls_dh_set_group (gnutls_session_t session, bigint_t gen, bigint_t prime)
{
  dh_info_st *dh;
  int ret;

  switch (gnutls_auth_get_type (session))
    {
    case GNUTLS_CRD_ANON:
      {
        anon_auth_info_t info;
        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        dh = &info->dh;
        break;
      }
    case GNUTLS_CRD_PSK:
      {
        psk_auth_info_t info;
        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        dh = &info->dh;
        break;
      }
    case GNUTLS_CRD_CERTIFICATE:
      {
        cert_auth_info_t info;

        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        dh = &info->dh;
        break;
      }
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  if (dh->prime.data)
    _gnutls_free_datum (&dh->prime);

  if (dh->generator.data)
    _gnutls_free_datum (&dh->generator);

  /* prime
   */
  ret = _gnutls_mpi_dprint_lz (prime, &dh->prime);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* generator
   */
  ret = _gnutls_mpi_dprint_lz (gen, &dh->generator);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&dh->prime);
      return ret;
    }

  return 0;
}

#ifdef ENABLE_OPENPGP
/**
 * gnutls_openpgp_send_cert:
 * @session: is a pointer to a #gnutls_session_t structure.
 * @status: is one of GNUTLS_OPENPGP_CERT, or GNUTLS_OPENPGP_CERT_FINGERPRINT
 *
 * This function will order gnutls to send the key fingerprint
 * instead of the key in the initial handshake procedure. This should
 * be used with care and only when there is indication or knowledge
 * that the server can obtain the client's key.
 **/
void
gnutls_openpgp_send_cert (gnutls_session_t session,
                          gnutls_openpgp_crt_status_t status)
{
  session->internals.pgp_fingerprint = status;
}
#endif

/**
 * gnutls_certificate_send_x509_rdn_sequence:
 * @session: is a pointer to a #gnutls_session_t structure.
 * @status: is 0 or 1
 *
 * If status is non zero, this function will order gnutls not to send
 * the rdnSequence in the certificate request message. That is the
 * server will not advertize it's trusted CAs to the peer. If status
 * is zero then the default behaviour will take effect, which is to
 * advertize the server's trusted CAs.
 *
 * This function has no effect in clients, and in authentication
 * methods other than certificate with X.509 certificates.
 **/
void
gnutls_certificate_send_x509_rdn_sequence (gnutls_session_t session,
                                           int status)
{
  session->internals.ignore_rdn_sequence = status;
}

#ifdef ENABLE_OPENPGP
int
_gnutls_openpgp_send_fingerprint (gnutls_session_t session)
{
  return session->internals.pgp_fingerprint;
}
#endif

/*-
 * _gnutls_record_set_default_version - Used to set the default version for the first record packet
 * @session: is a #gnutls_session_t structure.
 * @major: is a tls major version
 * @minor: is a tls minor version
 *
 * This function sets the default version that we will use in the first
 * record packet (client hello). This function is only useful to people
 * that know TLS internals and want to debug other implementations.
 -*/
void
_gnutls_record_set_default_version (gnutls_session_t session,
                                    unsigned char major, unsigned char minor)
{
  session->internals.default_record_version[0] = major;
  session->internals.default_record_version[1] = minor;
}

/**
 * gnutls_handshake_set_private_extensions:
 * @session: is a #gnutls_session_t structure.
 * @allow: is an integer (0 or 1)
 *
 * This function will enable or disable the use of private cipher
 * suites (the ones that start with 0xFF).  By default or if @allow
 * is 0 then these cipher suites will not be advertized nor used.
 *
 * Unless this function is called with the option to allow (1), then
 * no compression algorithms, like LZO.  That is because these
 * algorithms are not yet defined in any RFC or even internet draft.
 *
 * Enabling the private ciphersuites when talking to other than
 * gnutls servers and clients may cause interoperability problems.
 **/
void
gnutls_handshake_set_private_extensions (gnutls_session_t session, int allow)
{
  session->internals.enable_private = allow;
}

inline static int
_gnutls_cal_PRF_A (gnutls_mac_algorithm_t algorithm,
                   const void *secret, int secret_size,
                   const void *seed, int seed_size, void *result)
{
  digest_hd_st td1;
  int ret;

  ret = _gnutls_hmac_init (&td1, algorithm, secret, secret_size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_hmac (&td1, seed, seed_size);
  _gnutls_hmac_deinit (&td1, result);

  return 0;
}

#define MAX_SEED_SIZE 200

/* Produces "total_bytes" bytes using the hash algorithm specified.
 * (used in the PRF function)
 */
static int
_gnutls_P_hash (gnutls_mac_algorithm_t algorithm,
                const opaque * secret, int secret_size,
                const opaque * seed, int seed_size,
                int total_bytes, opaque * ret)
{

  digest_hd_st td2;
  int i, times, how, blocksize, A_size;
  opaque final[MAX_HASH_SIZE], Atmp[MAX_SEED_SIZE];
  int output_bytes, result;

  if (seed_size > MAX_SEED_SIZE || total_bytes <= 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  blocksize = _gnutls_hmac_get_algo_len (algorithm);

  output_bytes = 0;
  do
    {
      output_bytes += blocksize;
    }
  while (output_bytes < total_bytes);

  /* calculate A(0) */

  memcpy (Atmp, seed, seed_size);
  A_size = seed_size;

  times = output_bytes / blocksize;

  for (i = 0; i < times; i++)
    {
      result = _gnutls_hmac_init (&td2, algorithm, secret, secret_size);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }

      /* here we calculate A(i+1) */
      if ((result =
           _gnutls_cal_PRF_A (algorithm, secret, secret_size, Atmp,
                              A_size, Atmp)) < 0)
        {
          gnutls_assert ();
          _gnutls_hmac_deinit (&td2, final);
          return result;
        }

      A_size = blocksize;

      _gnutls_hmac (&td2, Atmp, A_size);
      _gnutls_hmac (&td2, seed, seed_size);
      _gnutls_hmac_deinit (&td2, final);

      if ((1 + i) * blocksize < total_bytes)
        {
          how = blocksize;
        }
      else
        {
          how = total_bytes - (i) * blocksize;
        }

      if (how > 0)
        {
          memcpy (&ret[i * blocksize], final, how);
        }
    }

  return 0;
}

/* Xor's two buffers and puts the output in the first one.
 */
inline static void
_gnutls_xor (opaque * o1, opaque * o2, int length)
{
  int i;
  for (i = 0; i < length; i++)
    {
      o1[i] ^= o2[i];
    }
}



#define MAX_PRF_BYTES 200

/* The PRF function expands a given secret 
 * needed by the TLS specification. ret must have a least total_bytes
 * available.
 */
int
_gnutls_PRF (gnutls_session_t session,
             const opaque * secret, int secret_size, const char *label,
             int label_size, const opaque * seed, int seed_size,
             int total_bytes, void *ret)
{
  int l_s, s_seed_size;
  const opaque *s1, *s2;
  opaque s_seed[MAX_SEED_SIZE];
  opaque o1[MAX_PRF_BYTES], o2[MAX_PRF_BYTES];
  int result;
  gnutls_protocol_t ver = gnutls_protocol_get_version (session);

  if (total_bytes > MAX_PRF_BYTES)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }
  /* label+seed = s_seed */
  s_seed_size = seed_size + label_size;

  if (s_seed_size > MAX_SEED_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  memcpy (s_seed, label, label_size);
  memcpy (&s_seed[label_size], seed, seed_size);

  if (_gnutls_version_has_selectable_prf (ver))
    {
      result =
        _gnutls_P_hash (GNUTLS_MAC_SHA256, secret, secret_size,
                        s_seed, s_seed_size, total_bytes, ret);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }
    }
  else
    {
      l_s = secret_size / 2;

      s1 = &secret[0];
      s2 = &secret[l_s];

      if (secret_size % 2 != 0)
        {
          l_s++;
        }

      result =
        _gnutls_P_hash (GNUTLS_MAC_MD5, s1, l_s, s_seed, s_seed_size,
                        total_bytes, o1);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }

      result =
        _gnutls_P_hash (GNUTLS_MAC_SHA1, s2, l_s, s_seed, s_seed_size,
                        total_bytes, o2);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }

      _gnutls_xor (o1, o2, total_bytes);

      memcpy (ret, o1, total_bytes);
    }

  return 0;                     /* ok */

}

/**
 * gnutls_prf_raw:
 * @session: is a #gnutls_session_t structure.
 * @label_size: length of the @label variable.
 * @label: label used in PRF computation, typically a short string.
 * @seed_size: length of the @seed variable.
 * @seed: optional extra data to seed the PRF with.
 * @outsize: size of pre-allocated output buffer to hold the output.
 * @out: pre-allocate buffer to hold the generated data.
 *
 * Apply the TLS Pseudo-Random-Function (PRF) using the master secret
 * on some data.
 *
 * The @label variable usually contain a string denoting the purpose
 * for the generated data.  The @seed usually contain data such as the
 * client and server random, perhaps together with some additional
 * data that is added to guarantee uniqueness of the output for a
 * particular purpose.
 *
 * Because the output is not guaranteed to be unique for a particular
 * session unless @seed include the client random and server random
 * fields (the PRF would output the same data on another connection
 * resumed from the first one), it is not recommended to use this
 * function directly.  The gnutls_prf() function seed the PRF with the
 * client and server random fields directly, and is recommended if you
 * want to generate pseudo random data unique for each session.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_prf_raw (gnutls_session_t session,
                size_t label_size,
                const char *label,
                size_t seed_size, const char *seed, size_t outsize, char *out)
{
  int ret;

  ret = _gnutls_PRF (session,
                     session->security_parameters.master_secret,
                     GNUTLS_MASTER_SIZE,
                     label,
                     label_size, (opaque *) seed, seed_size, outsize, out);

  return ret;
}

/**
 * gnutls_prf:
 * @session: is a #gnutls_session_t structure.
 * @label_size: length of the @label variable.
 * @label: label used in PRF computation, typically a short string.
 * @server_random_first: non-0 if server random field should be first in seed
 * @extra_size: length of the @extra variable.
 * @extra: optional extra data to seed the PRF with.
 * @outsize: size of pre-allocated output buffer to hold the output.
 * @out: pre-allocate buffer to hold the generated data.
 *
 * Apply the TLS Pseudo-Random-Function (PRF) using the master secret
 * on some data, seeded with the client and server random fields.
 *
 * The @label variable usually contain a string denoting the purpose
 * for the generated data.  The @server_random_first indicate whether
 * the client random field or the server random field should be first
 * in the seed.  Non-0 indicate that the server random field is first,
 * 0 that the client random field is first.
 *
 * The @extra variable can be used to add more data to the seed, after
 * the random variables.  It can be used to tie make sure the
 * generated output is strongly connected to some additional data
 * (e.g., a string used in user authentication).
 *
 * The output is placed in *@OUT, which must be pre-allocated.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_prf (gnutls_session_t session,
            size_t label_size,
            const char *label,
            int server_random_first,
            size_t extra_size, const char *extra, size_t outsize, char *out)
{
  int ret;
  opaque *seed;
  size_t seedsize = 2 * GNUTLS_RANDOM_SIZE + extra_size;

  seed = gnutls_malloc (seedsize);
  if (!seed)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  memcpy (seed, server_random_first ?
          session->security_parameters.server_random :
          session->security_parameters.client_random, GNUTLS_RANDOM_SIZE);
  memcpy (seed + GNUTLS_RANDOM_SIZE, server_random_first ?
          session->security_parameters.client_random :
          session->security_parameters.server_random, GNUTLS_RANDOM_SIZE);

  memcpy (seed + 2 * GNUTLS_RANDOM_SIZE, extra, extra_size);

  ret = _gnutls_PRF (session, session->security_parameters.master_secret,
                     GNUTLS_MASTER_SIZE,
                     label, label_size, seed, seedsize, outsize, out);

  gnutls_free (seed);

  return ret;
}

/*-
 * gnutls_session_get_client_random:
 * @session: is a #gnutls_session_t structure.
 *
 * Return a pointer to the 32-byte client random field used in the
 * session.  The pointer must not be modified or deallocated.
 *
 * If a client random value has not yet been established, the output
 * will be garbage; in particular, a %NULL return value should not be
 * expected.
 *
 * Returns: pointer to client random data.
 *
 * Deprecated in: 2.11.0
 -*/
const void *
gnutls_session_get_client_random (gnutls_session_t session)
{
  return (char *) session->security_parameters.client_random;
}

/*-
 * gnutls_session_get_server_random:
 * @session: is a #gnutls_session_t structure.
 *
 * Return a pointer to the 32-byte server random field used in the
 * session.  The pointer must not be modified or deallocated.
 *
 * If a server random value has not yet been established, the output
 * will be garbage; in particular, a %NULL return value should not be
 * expected.
 *
 * Returns: pointer to server random data.
 *
 * Deprecated in: 2.11.0
 -*/
const void *
gnutls_session_get_server_random (gnutls_session_t session)
{
  return (char *) session->security_parameters.server_random;
}

/*-
 * gnutls_session_get_master_secret:
 * @session: is a #gnutls_session_t structure.
 *
 * Return a pointer to the 48-byte master secret in the session.  The
 * pointer must not be modified or deallocated.
 *
 * If a master secret value has not yet been established, the output
 * will be garbage; in particular, a %NULL return value should not be
 * expected.
 *
 * Consider using gnutls_prf() rather than extracting the master
 * secret and use it to derive further data.
 *
 * Returns: pointer to master secret data.
 *
 * Deprecated in: 2.11.0
 -*/
const void *
gnutls_session_get_master_secret (gnutls_session_t session)
{
  return (char *) session->security_parameters.master_secret;
}

/*-
 * gnutls_session_set_finished_function:
 * @session: is a #gnutls_session_t structure.
 * @func: a #gnutls_finished_callback_func callback.
 *
 * Register a callback function for the session that will be called
 * when a TLS Finished message has been generated.  The function is
 * typically used to copy away the TLS finished message for later use
 * as a channel binding or similar purpose.
 *
 * The callback should follow this prototype:
 *
 * void callback (gnutls_session_t @session, const void *@finished, size_t @len);
 *
 * The @finished parameter will contain the binary TLS finished
 * message, and @len will contains its length.  For SSLv3 connections,
 * the @len parameter will be 36 and for TLS connections it will be
 * 12.
 *
 * It is recommended that the function returns quickly in order to not
 * delay the handshake.  Use the function to store a copy of the TLS
 * finished message for later use.
 *
 * Since: 2.6.0
 * Deprecated in: 2.11.0
 -*/
void
gnutls_session_set_finished_function (gnutls_session_t session,
                                      gnutls_finished_callback_func func)
{
  session->internals.finished_func = func;
}

/**
 * gnutls_session_is_resumed:
 * @session: is a #gnutls_session_t structure.
 *
 * Check whether session is resumed or not.
 *
 * Returns: non zero if this session is resumed, or a zero if this is
 *   a new session.
 **/
int
gnutls_session_is_resumed (gnutls_session_t session)
{
  if (session->security_parameters.entity == GNUTLS_CLIENT)
    {
      if (session->security_parameters.session_id_size > 0 &&
          session->security_parameters.session_id_size ==
          session->internals.resumed_security_parameters.session_id_size
          && memcmp (session->security_parameters.session_id,
                     session->internals.
                     resumed_security_parameters.session_id,
                     session->security_parameters.session_id_size) == 0)
        return 1;
    }
  else
    {
      if (session->internals.resumed == RESUME_TRUE)
        return 1;
    }

  return 0;
}

/*-
 * _gnutls_session_is_export - Used to check whether this session is of export grade
 * @session: is a #gnutls_session_t structure.
 *
 * This function will return non zero if this session is of export grade.
 -*/
int
_gnutls_session_is_export (gnutls_session_t session)
{
  gnutls_cipher_algorithm_t cipher;

  cipher =
    _gnutls_cipher_suite_get_cipher_algo (&session->
                                          security_parameters.current_cipher_suite);

  if (_gnutls_cipher_get_export_flag (cipher) != 0)
    return 1;

  return 0;
}

/*-
 * _gnutls_session_is_psk - Used to check whether this session uses PSK kx
 * @session: is a #gnutls_session_t structure.
 *
 * This function will return non zero if this session uses a PSK key
 * exchange algorithm.
 -*/
int
_gnutls_session_is_psk (gnutls_session_t session)
{
  gnutls_kx_algorithm_t kx;

  kx =
    _gnutls_cipher_suite_get_kx_algo (&session->
                                      security_parameters.current_cipher_suite);
  if (kx == GNUTLS_KX_PSK || kx == GNUTLS_KX_DHE_PSK)
    return 1;

  return 0;
}

/**
 * gnutls_session_get_ptr:
 * @session: is a #gnutls_session_t structure.
 *
 * Get user pointer for session.  Useful in callbacks.  This is the
 *   pointer set with gnutls_session_set_ptr().
 *
 * Returns: the user given pointer from the session structure, or
 *   %NULL if it was never set.
 **/
void *
gnutls_session_get_ptr (gnutls_session_t session)
{
  return session->internals.user_ptr;
}

/**
 * gnutls_session_set_ptr:
 * @session: is a #gnutls_session_t structure.
 * @ptr: is the user pointer
 *
 * This function will set (associate) the user given pointer @ptr to
 * the session structure.  This is pointer can be accessed with
 * gnutls_session_get_ptr().
 **/
void
gnutls_session_set_ptr (gnutls_session_t session, void *ptr)
{
  session->internals.user_ptr = ptr;
}


/**
 * gnutls_record_get_direction:
 * @session: is a #gnutls_session_t structure.
 *
 * This function provides information about the internals of the
 * record protocol and is only useful if a prior gnutls function call
 * (e.g.  gnutls_handshake()) was interrupted for some reason, that
 * is, if a function returned %GNUTLS_E_INTERRUPTED or
 * %GNUTLS_E_AGAIN.  In such a case, you might want to call select()
 * or poll() before calling the interrupted gnutls function again.  To
 * tell you whether a file descriptor should be selected for either
 * reading or writing, gnutls_record_get_direction() returns 0 if the
 * interrupted function was trying to read data, and 1 if it was
 * trying to write data.
 *
 * Returns: 0 if trying to read data, 1 if trying to write data.
 **/
int
gnutls_record_get_direction (gnutls_session_t session)
{
  return session->internals.direction;
}

/*-
 * _gnutls_rsa_pms_set_version - Sets a version to be used at the RSA PMS
 * @session: is a #gnutls_session_t structure.
 * @major: is the major version to use
 * @minor: is the minor version to use
 *
 * This function will set the given version number to be used at the
 * RSA PMS secret. This is only useful to clients, which want to
 * test server's capabilities.
 -*/
void
_gnutls_rsa_pms_set_version (gnutls_session_t session,
                             unsigned char major, unsigned char minor)
{
  session->internals.rsa_pms_version[0] = major;
  session->internals.rsa_pms_version[1] = minor;
}

/**
 * gnutls_handshake_set_post_client_hello_function:
 * @session: is a #gnutls_session_t structure.
 * @func: is the function to be called
 *
 * This function will set a callback to be called after the client
 * hello has been received (callback valid in server side only). This
 * allows the server to adjust settings based on received extensions.
 *
 * Those settings could be ciphersuites, requesting certificate, or
 * anything else except for version negotiation (this is done before
 * the hello message is parsed).
 *
 * This callback must return 0 on success or a gnutls error code to
 * terminate the handshake.
 *
 * Warning: You should not use this function to terminate the
 * handshake based on client input unless you know what you are
 * doing. Before the handshake is finished there is no way to know if
 * there is a man-in-the-middle attack being performed.
 **/
void
gnutls_handshake_set_post_client_hello_function (gnutls_session_t session,
                                                 gnutls_handshake_post_client_hello_func
                                                 func)
{
  session->internals.user_hello_func = func;
}

/**
 * gnutls_session_enable_compatibility_mode:
 * @session: is a #gnutls_session_t structure.
 *
 * This function can be used to disable certain (security) features in
 * TLS in order to maintain maximum compatibility with buggy
 * clients. It is equivalent to calling:
 * gnutls_record_disable_padding()
 *
 * Normally only servers that require maximum compatibility with
 * everything out there, need to call this function.
 **/
void
gnutls_session_enable_compatibility_mode (gnutls_session_t session)
{
  gnutls_record_disable_padding (session);
}

/**
 * gnutls_session_channel_binding:
 * @session: is a #gnutls_session_t structure.
 * @cbtype: an #gnutls_channel_binding_t enumeration type
 * @cb: output buffer array with data
 *
 * Extract given channel binding data of the @cbtype (e.g.,
 * %GNUTLS_CB_TLS_UNIQUE) type.
 *
 * Returns: %GNUTLS_E_SUCCESS on success,
 * %GNUTLS_E_UNIMPLEMENTED_FEATURE if the @cbtype is unsupported,
 * %GNUTLS_E_CHANNEL_BINDING_NOT_AVAILABLE if the data is not
 * currently available, or an error code.
 *
 * Since: 2.12.0
 **/
int
gnutls_session_channel_binding (gnutls_session_t session,
                                gnutls_channel_binding_t cbtype,
                                gnutls_datum_t * cb)
{
  if (cbtype != GNUTLS_CB_TLS_UNIQUE)
    return GNUTLS_E_UNIMPLEMENTED_FEATURE;

  if (!session->internals.initial_negotiation_completed)
    return GNUTLS_E_CHANNEL_BINDING_NOT_AVAILABLE;

  cb->size = session->internals.cb_tls_unique_len;
  cb->data = gnutls_malloc (cb->size);
  if (cb->data == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  memcpy (cb->data, session->internals.cb_tls_unique, cb->size);

  return 0;
}
