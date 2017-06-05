/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2010 Free
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

/* This file contains the RSA key exchange part of the certificate
 * authentication.
 */

#include "gnutls_int.h"
#include "gnutls_auth.h"
#include "gnutls_errors.h"
#include "gnutls_dh.h"
#include "gnutls_num.h"
#include "gnutls_datum.h"
#include "auth_cert.h"
#include <gnutls_pk.h>
#include <gnutls_algorithms.h>
#include <gnutls_global.h>
#include "debug.h"
#include <gnutls_sig.h>
#include <gnutls_x509.h>
#include <gnutls_rsa_export.h>
#include <gnutls_state.h>
#include <random.h>

int _gnutls_gen_rsa_client_kx (gnutls_session_t, opaque **);
static int gen_rsa_export_server_kx (gnutls_session_t, opaque **);
static int proc_rsa_export_server_kx (gnutls_session_t, opaque *, size_t);
static int proc_rsa_export_client_kx (gnutls_session_t session, opaque * data,
                                      size_t _data_size);

const mod_auth_st rsa_export_auth_struct = {
  "RSA EXPORT",
  _gnutls_gen_cert_server_certificate,
  _gnutls_gen_cert_client_certificate,
  gen_rsa_export_server_kx,
  _gnutls_gen_rsa_client_kx,
  _gnutls_gen_cert_client_cert_vrfy,    /* gen client cert vrfy */
  _gnutls_gen_cert_server_cert_req,     /* server cert request */

  _gnutls_proc_cert_server_certificate,
  _gnutls_proc_cert_client_certificate,
  proc_rsa_export_server_kx,
  proc_rsa_export_client_kx,    /* proc client kx */
  _gnutls_proc_cert_client_cert_vrfy,   /* proc client cert vrfy */
  _gnutls_proc_cert_cert_req    /* proc server cert request */
};

/* This function reads the RSA parameters from the private key
 */
static int
_gnutls_get_private_rsa_params (gnutls_session_t session,
                                bigint_t ** params, int *params_size)
{
  int bits;
  gnutls_certificate_credentials_t cred;
  gnutls_rsa_params_t rsa_params;

  cred = (gnutls_certificate_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_CERTIFICATE, NULL);
  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  if (session->internals.selected_cert_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  bits =
    _gnutls_mpi_get_nbits (session->internals.
                           selected_cert_list[0].params[0]);

  if (_gnutls_cipher_suite_get_kx_algo
      (&session->security_parameters.current_cipher_suite)
      != GNUTLS_KX_RSA_EXPORT || bits < 512)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  rsa_params =
    _gnutls_certificate_get_rsa_params (cred->rsa_params,
                                        cred->params_func, session);
  /* EXPORT case: */
  if (rsa_params == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_NO_TEMPORARY_RSA_PARAMS;
    }

  /* In the export case, we do use temporary RSA params
   * of 512 bits size. The params in the certificate are
   * used to sign this temporary stuff.
   */
  *params_size = RSA_PRIVATE_PARAMS;
  *params = rsa_params->params;

  return 0;
}

int
proc_rsa_export_client_kx (gnutls_session_t session, opaque * data,
                           size_t _data_size)
{
  gnutls_datum_t plaintext;
  gnutls_datum_t ciphertext;
  int ret, dsize;
  bigint_t *params;
  int params_len;
  int randomize_key = 0;
  ssize_t data_size = _data_size;

  if (gnutls_protocol_get_version (session) == GNUTLS_SSL3)
    {
      /* SSL 3.0 
       */
      ciphertext.data = data;
      ciphertext.size = data_size;
    }
  else
    {
      /* TLS 1.0
       */
      DECR_LEN (data_size, 2);
      ciphertext.data = &data[2];
      dsize = _gnutls_read_uint16 (data);

      if (dsize != data_size)
        {
          gnutls_assert ();
          return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;
        }
      ciphertext.size = dsize;
    }

  ret = _gnutls_get_private_rsa_params (session, &params, &params_len);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_pkcs1_rsa_decrypt (&plaintext, &ciphertext, params, params_len, 2);     /* btype==2 */

  if (ret < 0 || plaintext.size != GNUTLS_MASTER_SIZE)
    {
      /* In case decryption fails then don't inform
       * the peer. Just use a random key. (in order to avoid
       * attack against pkcs-1 formating).
       */
      gnutls_assert ();
      _gnutls_x509_log ("auth_rsa: Possible PKCS #1 format attack\n");
      randomize_key = 1;
    }
  else
    {
      /* If the secret was properly formatted, then
       * check the version number.
       */
      if (_gnutls_get_adv_version_major (session) != plaintext.data[0]
          || _gnutls_get_adv_version_minor (session) != plaintext.data[1])
        {
          /* No error is returned here, if the version number check
           * fails. We proceed normally.
           * That is to defend against the attack described in the paper
           * "Attacking RSA-based sessions in SSL/TLS" by Vlastimil Klima,
           * Ondej Pokorny and Tomas Rosa.
           */
          gnutls_assert ();
          _gnutls_x509_log
            ("auth_rsa: Possible PKCS #1 version check format attack\n");
        }
    }

  if (randomize_key != 0)
    {
      session->key->key.size = GNUTLS_MASTER_SIZE;
      session->key->key.data = gnutls_malloc (session->key->key.size);
      if (session->key->key.data == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      /* we do not need strong random numbers here.
       */
      ret = _gnutls_rnd (GNUTLS_RND_NONCE, session->key->key.data,
                         session->key->key.size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

    }
  else
    {
      session->key->key.data = plaintext.data;
      session->key->key.size = plaintext.size;
    }

  /* This is here to avoid the version check attack
   * discussed above.
   */
  session->key->key.data[0] = _gnutls_get_adv_version_major (session);
  session->key->key.data[1] = _gnutls_get_adv_version_minor (session);

  return 0;
}

static int
gen_rsa_export_server_kx (gnutls_session_t session, opaque ** data)
{
  gnutls_rsa_params_t rsa_params;
  const bigint_t *rsa_mpis;
  size_t n_e, n_m;
  uint8_t *data_e, *data_m;
  int ret = 0, data_size;
  gnutls_cert *apr_cert_list;
  gnutls_privkey_t apr_pkey;
  int apr_cert_list_length;
  gnutls_datum_t signature, ddata;
  gnutls_certificate_credentials_t cred;
  gnutls_sign_algorithm_t sign_algo;
  unsigned int bits = 0;

  cred = (gnutls_certificate_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_CERTIFICATE, NULL);
  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  /* find the appropriate certificate */
  if ((ret =
       _gnutls_get_selected_cert (session, &apr_cert_list,
                                  &apr_cert_list_length, &apr_pkey)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* abort sending this message if we have a certificate
   * of 512 bits or less.
   */
  gnutls_privkey_get_pk_algorithm (apr_pkey, &bits);
  if (apr_pkey && bits <= 512)
    {
      gnutls_assert ();
      return GNUTLS_E_INT_RET_0;
    }

  rsa_params =
    _gnutls_certificate_get_rsa_params (cred->rsa_params, cred->params_func,
                                        session);
  rsa_mpis = _gnutls_rsa_params_to_mpi (rsa_params);
  if (rsa_mpis == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_NO_TEMPORARY_RSA_PARAMS;
    }

  if ((ret = _gnutls_auth_info_set (session, GNUTLS_CRD_CERTIFICATE,
                                    sizeof (cert_auth_info_st), 0)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_rsa_export_set_pubkey (session, rsa_mpis[1], rsa_mpis[0]);

  _gnutls_mpi_print (rsa_mpis[0], NULL, &n_m);
  _gnutls_mpi_print (rsa_mpis[1], NULL, &n_e);

  (*data) = gnutls_malloc (n_e + n_m + 4);
  if (*data == NULL)
    {
      return GNUTLS_E_MEMORY_ERROR;
    }

  data_m = &(*data)[0];
  _gnutls_mpi_print (rsa_mpis[0], &data_m[2], &n_m);

  _gnutls_write_uint16 (n_m, data_m);

  data_e = &data_m[2 + n_m];
  _gnutls_mpi_print (rsa_mpis[1], &data_e[2], &n_e);

  _gnutls_write_uint16 (n_e, data_e);

  data_size = n_m + n_e + 4;


  /* Generate the signature. */

  ddata.data = *data;
  ddata.size = data_size;

  if (apr_cert_list_length > 0)
    {
      if ((ret =
           _gnutls_handshake_sign_data (session, &apr_cert_list[0],
                                        apr_pkey, &ddata, &signature,
                                        &sign_algo)) < 0)
        {
          gnutls_assert ();
          gnutls_free (*data);
          *data = NULL;
          return ret;
        }
    }
  else
    {
      gnutls_assert ();
      return data_size;         /* do not put a signature - ILLEGAL! */
    }

  *data = gnutls_realloc_fast (*data, data_size + signature.size + 2);
  if (*data == NULL)
    {
      _gnutls_free_datum (&signature);
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  _gnutls_write_datum16 (&((*data)[data_size]), signature);
  data_size += signature.size + 2;

  _gnutls_free_datum (&signature);

  return data_size;
}

/* if the peer's certificate is of 512 bits or less, returns non zero.
 */
int
_gnutls_peers_cert_less_512 (gnutls_session_t session)
{
  gnutls_cert peer_cert;
  int ret;
  cert_auth_info_t info = _gnutls_get_auth_info (session);

  if (info == NULL || info->ncerts == 0)
    {
      gnutls_assert ();
      /* we need this in order to get peer's certificate */
      return 0;
    }

  if ((ret =
       _gnutls_get_auth_info_gcert (&peer_cert,
                                    session->security_parameters.cert_type,
                                    info, CERT_NO_COPY)) < 0)
    {
      gnutls_assert ();
      return 0;
    }

  if (peer_cert.subject_pk_algorithm != GNUTLS_PK_RSA)
    {
      gnutls_assert ();
      _gnutls_gcert_deinit (&peer_cert);
      return 0;
    }

  if (_gnutls_mpi_get_nbits (peer_cert.params[0]) <= 512)
    {
      _gnutls_gcert_deinit (&peer_cert);
      return 1;
    }

  _gnutls_gcert_deinit (&peer_cert);

  return 0;
}

static int
proc_rsa_export_server_kx (gnutls_session_t session,
                           opaque * data, size_t _data_size)
{
  uint16_t n_m, n_e;
  size_t _n_m, _n_e;
  uint8_t *data_m;
  uint8_t *data_e;
  int i, sigsize;
  gnutls_datum_t vparams, signature;
  int ret;
  ssize_t data_size = _data_size;
  cert_auth_info_t info;
  gnutls_cert peer_cert;

  info = _gnutls_get_auth_info (session);
  if (info == NULL || info->ncerts == 0)
    {
      gnutls_assert ();
      /* we need this in order to get peer's certificate */
      return GNUTLS_E_INTERNAL_ERROR;
    }


  i = 0;

  DECR_LEN (data_size, 2);
  n_m = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_m);
  data_m = &data[i];
  i += n_m;

  DECR_LEN (data_size, 2);
  n_e = _gnutls_read_uint16 (&data[i]);
  i += 2;

  DECR_LEN (data_size, n_e);
  data_e = &data[i];
  i += n_e;

  _n_e = n_e;
  _n_m = n_m;

  if (_gnutls_mpi_scan_nz (&session->key->rsa[0], data_m, _n_m) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  if (_gnutls_mpi_scan_nz (&session->key->rsa[1], data_e, _n_e) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  _gnutls_rsa_export_set_pubkey (session, session->key->rsa[1],
                                 session->key->rsa[0]);

  /* VERIFY SIGNATURE */

  vparams.size = n_m + n_e + 4;
  vparams.data = data;

  DECR_LEN (data_size, 2);
  sigsize = _gnutls_read_uint16 (&data[vparams.size]);

  DECR_LEN (data_size, sigsize);
  signature.data = &data[vparams.size + 2];
  signature.size = sigsize;

  if ((ret =
       _gnutls_get_auth_info_gcert (&peer_cert,
                                    session->security_parameters.cert_type,
                                    info, CERT_NO_COPY)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    _gnutls_handshake_verify_data (session, &peer_cert, &vparams, &signature,
                                   GNUTLS_SIGN_UNKNOWN);

  _gnutls_gcert_deinit (&peer_cert);
  if (ret < 0)
    {
      gnutls_assert ();
    }

  return ret;
}
