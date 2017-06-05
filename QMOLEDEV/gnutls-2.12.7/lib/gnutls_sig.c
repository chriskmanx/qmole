/*
 * Copyright (C) 2001, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free
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
#include <x509_b64.h>
#include <auth_cert.h>
#include <gnutls_algorithms.h>
#include <gnutls_cert.h>
#include <gnutls_datum.h>
#include <gnutls_mpi.h>
#include <gnutls_global.h>
#include <gnutls_pk.h>
#include <debug.h>
#include <gnutls_buffers.h>
#include <gnutls_sig.h>
#include <gnutls_kx.h>
#include <libtasn1.h>
#include <ext_signature.h>
#include <gnutls_state.h>
#include <x509/common.h>

static int
sign_tls_hash (gnutls_session_t session, gnutls_digest_algorithm_t hash_algo,
                  gnutls_cert * cert, gnutls_privkey_t pkey,
                  const gnutls_datum_t * hash_concat,
                  gnutls_datum_t * signature);

/* While this is currently equal to the length of RSA/SHA512
 * signature, it should also be sufficient for DSS signature and any
 * other RSA signatures including one with the old MD5/SHA1-combined
 * format.
 */
#define MAX_SIG_SIZE 19 + MAX_HASH_SIZE

/* Generates a signature of all the random data and the parameters.
 * Used in DHE_* ciphersuites.
 */
int
_gnutls_handshake_sign_data (gnutls_session_t session, gnutls_cert * cert,
                             gnutls_privkey_t pkey, gnutls_datum_t * params,
                             gnutls_datum_t * signature,
                             gnutls_sign_algorithm_t * sign_algo)
{
  gnutls_datum_t dconcat;
  int ret;
  digest_hd_st td_sha;
  opaque concat[MAX_SIG_SIZE];
  gnutls_protocol_t ver = gnutls_protocol_get_version (session);
  gnutls_digest_algorithm_t hash_algo;

  *sign_algo =
    _gnutls_session_get_sign_algo (session, cert);
  if (*sign_algo == GNUTLS_SIGN_UNKNOWN)
    {
      gnutls_assert ();
      return GNUTLS_E_UNKNOWN_PK_ALGORITHM;
    }

  hash_algo = _gnutls_sign_get_hash_algorithm (*sign_algo);

  _gnutls_handshake_log ("HSK[%p]: signing handshake data: using %s\n",
                    session, gnutls_sign_algorithm_get_name (*sign_algo));

  ret = _gnutls_hash_init (&td_sha, hash_algo);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_hash (&td_sha, session->security_parameters.client_random,
                GNUTLS_RANDOM_SIZE);
  _gnutls_hash (&td_sha, session->security_parameters.server_random,
                GNUTLS_RANDOM_SIZE);
  _gnutls_hash (&td_sha, params->data, params->size);

  switch (cert->subject_pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      if (!_gnutls_version_has_selectable_sighash (ver))
        {
          digest_hd_st td_md5;

          ret = _gnutls_hash_init (&td_md5, GNUTLS_MAC_MD5);
          if (ret < 0)
            {
              gnutls_assert ();
              return ret;
            }

          _gnutls_hash (&td_md5, session->security_parameters.client_random,
                        GNUTLS_RANDOM_SIZE);
          _gnutls_hash (&td_md5, session->security_parameters.server_random,
                        GNUTLS_RANDOM_SIZE);
          _gnutls_hash (&td_md5, params->data, params->size);

          _gnutls_hash_deinit (&td_md5, concat);
          _gnutls_hash_deinit (&td_sha, &concat[16]);

          dconcat.data = concat;
          dconcat.size = 36;
        }
      else
        { /* TLS 1.2 way */

          _gnutls_hash_deinit (&td_sha, concat);

          dconcat.data = concat;
          dconcat.size = _gnutls_hash_get_algo_len (hash_algo);
        }
      break;
    case GNUTLS_PK_DSA:
      _gnutls_hash_deinit (&td_sha, concat);

      if ((hash_algo != GNUTLS_DIG_SHA1) && (hash_algo != GNUTLS_DIG_SHA224)
          && (hash_algo != GNUTLS_DIG_SHA256))
        {
          gnutls_assert ();
          return GNUTLS_E_INTERNAL_ERROR;
        }
      dconcat.data = concat;
      dconcat.size = _gnutls_hash_get_algo_len (hash_algo);
      break;

    default:
      gnutls_assert ();
      _gnutls_hash_deinit (&td_sha, NULL);
      return GNUTLS_E_INTERNAL_ERROR;
    }

  ret = sign_tls_hash (session, hash_algo, cert, pkey, &dconcat, signature);
  if (ret < 0)
    {
      gnutls_assert ();
    }

  return ret;

}


/* This will create a PKCS1 or DSA signature, using the given parameters, and the
 * given data. The output will be allocated and be put in signature.
 */
int
_gnutls_soft_sign (gnutls_pk_algorithm_t algo, bigint_t * params,
                   int params_size, const gnutls_datum_t * data,
                   gnutls_datum_t * signature)
{
  int ret;

  switch (algo)
    {
    case GNUTLS_PK_RSA:
      /* encrypt */
      if ((ret = _gnutls_pkcs1_rsa_encrypt (signature, data, params,
                                            params_size, 1)) < 0)
        {
          gnutls_assert ();
          return ret;
        }

      break;
    case GNUTLS_PK_DSA:
      /* sign */
      if ((ret = _gnutls_dsa_sign (signature, data, params, params_size)) < 0)
        {
          gnutls_assert ();
          return ret;
        }
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
      break;
    }

  return 0;
}

/* This will create a PKCS1 or DSA signature, as defined in the TLS protocol.
 * Cert is the certificate of the corresponding private key. It is only checked if
 * it supports signing.
 */
static int
sign_tls_hash (gnutls_session_t session, gnutls_digest_algorithm_t hash_algo,
                  gnutls_cert * cert, gnutls_privkey_t pkey,
                  const gnutls_datum_t * hash_concat,
                  gnutls_datum_t * signature)
{
  gnutls_protocol_t ver = gnutls_protocol_get_version (session);

  /* If our certificate supports signing
   */

  if (cert != NULL)
    {
      if (cert->key_usage != 0)
        if (!(cert->key_usage & GNUTLS_KEY_DIGITAL_SIGNATURE))
          {
            gnutls_assert ();
            return GNUTLS_E_KEY_USAGE_VIOLATION;
          }

      /* External signing. */
      if (!pkey)
        {
          if (!session->internals.sign_func)
            return GNUTLS_E_INSUFFICIENT_CREDENTIALS;

          return (*session->internals.sign_func)
            (session, session->internals.sign_func_userdata,
             cert->cert_type, &cert->raw, hash_concat, signature);
        }
    }

   if (!_gnutls_version_has_selectable_sighash (ver))
    return _gnutls_privkey_sign_hash (pkey, hash_concat, signature);
  else
    return gnutls_privkey_sign_hash (pkey, hash_algo, 0, hash_concat, signature);
}

static int
verify_tls_hash (gnutls_protocol_t ver, gnutls_cert * cert,
                    const gnutls_datum_t * hash_concat,
                    gnutls_datum_t * signature, size_t sha1pos,
                    gnutls_pk_algorithm_t pk_algo)
{
  int ret;
  gnutls_datum_t vdata;

  if (cert == NULL || cert->version == 0)
    {                           /* this is the only way to check
                                 * if it is initialized
                                 */
      gnutls_assert ();
      return GNUTLS_E_CERTIFICATE_ERROR;
    }

  /* If the certificate supports signing continue.
   */
  if (cert->key_usage != 0)
    if (!(cert->key_usage & GNUTLS_KEY_DIGITAL_SIGNATURE))
      {
        gnutls_assert ();
        return GNUTLS_E_KEY_USAGE_VIOLATION;
      }

  if (pk_algo == GNUTLS_PK_UNKNOWN)
    pk_algo = cert->subject_pk_algorithm;
  switch (pk_algo)
    {
    case GNUTLS_PK_RSA:

      vdata.data = hash_concat->data;
      vdata.size = hash_concat->size;

      /* verify signature */
      if (!_gnutls_version_has_selectable_sighash (ver))
        ret = _gnutls_rsa_verify (&vdata, signature, cert->params,
                                     cert->params_size, 1);
      else
        ret = pubkey_verify_sig( NULL, &vdata, signature, pk_algo, 
          cert->params, cert->params_size);

      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      break;
    case GNUTLS_PK_DSA:

      vdata.data = &hash_concat->data[sha1pos];
      vdata.size = hash_concat->size - sha1pos;

      ret = pubkey_verify_sig( NULL, &vdata, signature, pk_algo, 
        cert->params, cert->params_size);
      /* verify signature */
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }



  return 0;
}


/* Generates a signature of all the random data and the parameters.
 * Used in DHE_* ciphersuites.
 */
int
_gnutls_handshake_verify_data (gnutls_session_t session, gnutls_cert * cert,
                               const gnutls_datum_t * params,
                               gnutls_datum_t * signature,
                               gnutls_sign_algorithm_t algo)
{
  gnutls_datum_t dconcat;
  int ret;
  digest_hd_st td_md5;
  digest_hd_st td_sha;
  opaque concat[MAX_SIG_SIZE];
  gnutls_protocol_t ver = gnutls_protocol_get_version (session);
  gnutls_digest_algorithm_t hash_algo;

  if (_gnutls_version_has_selectable_sighash (ver))
    {
      _gnutls_handshake_log ("HSK[%p]: verify handshake data: using %s\n",
                    session, gnutls_sign_algorithm_get_name (algo));

      ret = cert_compatible_with_sig(cert, ver, algo);
      if (ret < 0)
        return gnutls_assert_val(ret);

      ret = _gnutls_session_sign_algo_enabled (session, algo);
      if (ret < 0)
        return gnutls_assert_val(ret);

      hash_algo = _gnutls_sign_get_hash_algorithm (algo);
    }
  else
    {
      ret = _gnutls_hash_init (&td_md5, GNUTLS_MAC_MD5);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      _gnutls_hash (&td_md5, session->security_parameters.client_random,
                    GNUTLS_RANDOM_SIZE);
      _gnutls_hash (&td_md5, session->security_parameters.server_random,
                    GNUTLS_RANDOM_SIZE);
      _gnutls_hash (&td_md5, params->data, params->size);

      hash_algo = GNUTLS_DIG_SHA1;
    }

  ret = _gnutls_hash_init (&td_sha, hash_algo);
  if (ret < 0)
    {
      gnutls_assert ();
      if (!_gnutls_version_has_selectable_sighash (ver))
        _gnutls_hash_deinit (&td_md5, NULL);
      return ret;
    }

  _gnutls_hash (&td_sha, session->security_parameters.client_random,
                GNUTLS_RANDOM_SIZE);
  _gnutls_hash (&td_sha, session->security_parameters.server_random,
                GNUTLS_RANDOM_SIZE);
  _gnutls_hash (&td_sha, params->data, params->size);

  if (!_gnutls_version_has_selectable_sighash (ver))
    {
      _gnutls_hash_deinit (&td_md5, concat);
      _gnutls_hash_deinit (&td_sha, &concat[16]);
      dconcat.data = concat;
      dconcat.size = 36;
    }
  else
    {
      _gnutls_hash_deinit (&td_sha, concat);

      dconcat.data = concat;
      dconcat.size = _gnutls_hash_get_algo_len (hash_algo);
    }

  ret = verify_tls_hash (ver, cert, &dconcat, signature,
                            dconcat.size -
                            _gnutls_hash_get_algo_len (hash_algo),
                            _gnutls_sign_get_pk_algorithm (algo));
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return ret;

}

/* Client certificate verify calculations
 */

/* this is _gnutls_handshake_verify_cert_vrfy for TLS 1.2
 */
static int
_gnutls_handshake_verify_cert_vrfy12 (gnutls_session_t session,
                                      gnutls_cert * cert,
                                      gnutls_datum_t * signature,
                                      gnutls_sign_algorithm_t sign_algo)
{
  int ret;
  opaque concat[MAX_SIG_SIZE];
  digest_hd_st td;
  gnutls_datum_t dconcat;
  gnutls_sign_algorithm_t _sign_algo;
  gnutls_digest_algorithm_t hash_algo;
  digest_hd_st *handshake_td;
  gnutls_protocol_t ver = gnutls_protocol_get_version (session);

  handshake_td = &session->internals.handshake_mac_handle.tls12.sha1;
  hash_algo = handshake_td->algorithm;
  _sign_algo =
    _gnutls_x509_pk_to_sign (cert->subject_pk_algorithm, hash_algo);

  if (_sign_algo != sign_algo)
    {
      handshake_td = &session->internals.handshake_mac_handle.tls12.sha256;
      hash_algo = handshake_td->algorithm;
      _sign_algo =
        _gnutls_x509_pk_to_sign (cert->subject_pk_algorithm, hash_algo);
      if (sign_algo != _sign_algo)
        {
          gnutls_assert ();
          return GNUTLS_E_UNSUPPORTED_SIGNATURE_ALGORITHM;
        }
    }

  ret = _gnutls_hash_copy (&td, handshake_td);
  if (ret < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_HASH_FAILED;
    }

  _gnutls_hash_deinit (&td, concat);

  dconcat.data = concat;
  dconcat.size = _gnutls_hash_get_algo_len (hash_algo);

  ret =
    verify_tls_hash (ver, cert, &dconcat, signature, 0,
                        cert->subject_pk_algorithm);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return ret;

}

/* Verifies a TLS signature (like the one in the client certificate
 * verify message). 
 */
int
_gnutls_handshake_verify_cert_vrfy (gnutls_session_t session,
                                    gnutls_cert * cert,
                                    gnutls_datum_t * signature,
                                    gnutls_sign_algorithm_t sign_algo)
{
  int ret;
  opaque concat[MAX_SIG_SIZE];
  digest_hd_st td_md5;
  digest_hd_st td_sha;
  gnutls_datum_t dconcat;
  gnutls_protocol_t ver = gnutls_protocol_get_version (session);

  _gnutls_handshake_log ("HSK[%p]: verify cert vrfy: using %s\n",
                    session, gnutls_sign_algorithm_get_name (sign_algo));

  if (session->security_parameters.handshake_mac_handle_type ==
      HANDSHAKE_MAC_TYPE_12)
    {
      return _gnutls_handshake_verify_cert_vrfy12 (session, cert, signature,
                                                   sign_algo);
    }
  else if (session->security_parameters.handshake_mac_handle_type !=
           HANDSHAKE_MAC_TYPE_10)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  ret =
    _gnutls_hash_copy (&td_md5,
                       &session->internals.handshake_mac_handle.tls10.md5);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    _gnutls_hash_copy (&td_sha,
                       &session->internals.handshake_mac_handle.tls10.sha);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (&td_md5, NULL);
      return GNUTLS_E_HASH_FAILED;
    }

  if (ver == GNUTLS_SSL3)
    {
      ret = _gnutls_generate_master (session, 1);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      _gnutls_mac_deinit_ssl3_handshake (&td_md5, concat,
                                         session->
                                         security_parameters.master_secret,
                                         GNUTLS_MASTER_SIZE);
      _gnutls_mac_deinit_ssl3_handshake (&td_sha, &concat[16],
                                         session->
                                         security_parameters.master_secret,
                                         GNUTLS_MASTER_SIZE);
    }
  else
    {
      _gnutls_hash_deinit (&td_md5, concat);
      _gnutls_hash_deinit (&td_sha, &concat[16]);
    }

  dconcat.data = concat;
  dconcat.size = 20 + 16;       /* md5+ sha */

  ret =
    verify_tls_hash (ver, cert, &dconcat, signature, 16,
                        cert->subject_pk_algorithm);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return ret;

}

/* the same as _gnutls_handshake_sign_cert_vrfy except that it is made for TLS 1.2
 */
static int
_gnutls_handshake_sign_cert_vrfy12 (gnutls_session_t session,
                                    gnutls_cert * cert, gnutls_privkey_t pkey,
                                    gnutls_datum_t * signature)
{
  gnutls_datum_t dconcat;
  int ret;
  opaque concat[MAX_SIG_SIZE];
  digest_hd_st td;
  gnutls_sign_algorithm_t sign_algo;
  gnutls_digest_algorithm_t hash_algo;
  digest_hd_st *handshake_td;

  sign_algo =
    _gnutls_session_get_sign_algo (session, cert);
  if (sign_algo == GNUTLS_SIGN_UNKNOWN)
    {
      gnutls_assert ();
      return GNUTLS_E_UNKNOWN_PK_ALGORITHM;
    }

  hash_algo = _gnutls_sign_get_hash_algorithm (sign_algo);

  _gnutls_debug_log ("sign handshake cert vrfy: picked %s with %s\n",
                    gnutls_sign_algorithm_get_name (sign_algo),
                    gnutls_mac_get_name (hash_algo));

  if ((gnutls_mac_algorithm_t)hash_algo == session->internals.handshake_mac_handle.tls12.sha1.algorithm)
    handshake_td = &session->internals.handshake_mac_handle.tls12.sha1;
  else if ((gnutls_mac_algorithm_t)hash_algo == session->internals.handshake_mac_handle.tls12.sha256.algorithm)
    handshake_td = &session->internals.handshake_mac_handle.tls12.sha256;
  else
    return gnutls_assert_val(GNUTLS_E_INTERNAL_ERROR); /* too bad we only support SHA1 and SHA256 */

  ret = _gnutls_hash_copy (&td, handshake_td);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_hash_deinit (&td, concat);

  dconcat.data = concat;
  dconcat.size = _gnutls_hash_get_algo_len (hash_algo);

  ret = sign_tls_hash (session, hash_algo, cert, pkey, &dconcat, signature);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return sign_algo;
}


/* Generates a signature of all the previous sent packets in the 
 * handshake procedure. 
 * 20040227: now it works for SSL 3.0 as well
 * 20091031: works for TLS 1.2 too!
 *
 * For TLS1.x, x<2 returns negative for failure and zero or unspecified for success.
 * For TLS1.2 returns the signature algorithm used on success, or a negative value;
 */
int
_gnutls_handshake_sign_cert_vrfy (gnutls_session_t session,
                                  gnutls_cert * cert, gnutls_privkey_t pkey,
                                  gnutls_datum_t * signature)
{
  gnutls_datum_t dconcat;
  int ret, hash_algo;
  opaque concat[MAX_SIG_SIZE];
  digest_hd_st td_md5;
  digest_hd_st td_sha;
  gnutls_protocol_t ver = gnutls_protocol_get_version (session);

  if (session->security_parameters.handshake_mac_handle_type ==
      HANDSHAKE_MAC_TYPE_12)
    {
      return _gnutls_handshake_sign_cert_vrfy12 (session, cert, pkey,
                                                 signature);
    }
  else if (session->security_parameters.handshake_mac_handle_type !=
           HANDSHAKE_MAC_TYPE_10)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  ret =
    _gnutls_hash_copy (&td_sha,
                       &session->internals.handshake_mac_handle.tls10.sha);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (ver == GNUTLS_SSL3)
    {
      ret = _gnutls_generate_master (session, 1);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      _gnutls_mac_deinit_ssl3_handshake (&td_sha, &concat[16],
                                         session->
                                         security_parameters.master_secret,
                                         GNUTLS_MASTER_SIZE);
    }
  else
    _gnutls_hash_deinit (&td_sha, &concat[16]);

  switch (cert->subject_pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      ret =
        _gnutls_hash_copy (&td_md5,
                           &session->internals.handshake_mac_handle.tls10.
                           md5);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      if (ver == GNUTLS_SSL3)
        _gnutls_mac_deinit_ssl3_handshake (&td_md5, concat,
                                           session->
                                           security_parameters.master_secret,
                                           GNUTLS_MASTER_SIZE);
      else
        _gnutls_hash_deinit (&td_md5, concat);

      dconcat.data = concat;
      dconcat.size = 36;
      break;
    case GNUTLS_PK_DSA:
      /* ensure 1024 bit DSA keys are used */
      hash_algo = _gnutls_dsa_q_to_hash (cert->params[1], NULL);
      if (!_gnutls_version_has_selectable_sighash (ver) && hash_algo != GNUTLS_DIG_SHA1)
        return gnutls_assert_val(GNUTLS_E_INCOMPAT_DSA_KEY_WITH_TLS_PROTOCOL);

      dconcat.data = &concat[16];
      dconcat.size = 20;
      break;

    default:
      return gnutls_assert_val(GNUTLS_E_INTERNAL_ERROR);
    }
  ret = sign_tls_hash (session, GNUTLS_DIG_NULL, cert, pkey, &dconcat, signature);
  if (ret < 0)
    {
      gnutls_assert ();
    }

  return ret;
}

int
pk_hash_data (gnutls_pk_algorithm_t pk, gnutls_digest_algorithm_t hash,
              bigint_t * params,
              const gnutls_datum_t * data, gnutls_datum_t * digest)
{
  int ret;

  digest->size = _gnutls_hash_get_algo_len (hash);
  digest->data = gnutls_malloc (digest->size);
  if (digest->data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = _gnutls_hash_fast (hash, data->data, data->size, digest->data);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  return 0;

cleanup:
  gnutls_free (digest->data);
  return ret;
}

/* Writes the digest information and the digest in a DER encoded
 * structure. The digest info is allocated and stored into the info structure.
 */
static int
encode_ber_digest_info (gnutls_digest_algorithm_t hash,
                        const gnutls_datum_t * digest,
                        gnutls_datum_t * output)
{
  ASN1_TYPE dinfo = ASN1_TYPE_EMPTY;
  int result;
  const char *algo;
  opaque *tmp_output;
  int tmp_output_size;

  algo = _gnutls_x509_mac_to_oid ((gnutls_mac_algorithm_t) hash);
  if (algo == NULL)
    {
      gnutls_assert ();
      _gnutls_x509_log ("Hash algorithm: %d\n", hash);
      return GNUTLS_E_UNKNOWN_PK_ALGORITHM;
    }

  if ((result = asn1_create_element (_gnutls_get_gnutls_asn (),
                                     "GNUTLS.DigestInfo",
                                     &dinfo)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_write_value (dinfo, "digestAlgorithm.algorithm", algo, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&dinfo);
      return _gnutls_asn2err (result);
    }

  /* Write an ASN.1 NULL in the parameters field.  This matches RFC
     3279 and RFC 4055, although is arguable incorrect from a historic
     perspective (see those documents for more information).
     Regardless of what is correct, this appears to be what most
     implementations do.  */
  result = asn1_write_value (dinfo, "digestAlgorithm.parameters",
                             ASN1_NULL, ASN1_NULL_SIZE);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&dinfo);
      return _gnutls_asn2err (result);
    }

  result = asn1_write_value (dinfo, "digest", digest->data, digest->size);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&dinfo);
      return _gnutls_asn2err (result);
    }

  tmp_output_size = 0;
  asn1_der_coding (dinfo, "", NULL, &tmp_output_size, NULL);

  tmp_output = gnutls_malloc (tmp_output_size);
  if (output->data == NULL)
    {
      gnutls_assert ();
      asn1_delete_structure (&dinfo);
      return GNUTLS_E_MEMORY_ERROR;
    }

  result = asn1_der_coding (dinfo, "", tmp_output, &tmp_output_size, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&dinfo);
      return _gnutls_asn2err (result);
    }

  asn1_delete_structure (&dinfo);

  output->size = tmp_output_size;
  output->data = tmp_output;

  return 0;
}

/* 
 * This function will do RSA PKCS #1 1.5 encoding
 * on the given digest. The given digest must be allocated
 * and will be freed if replacement is required.
 */
int
pk_prepare_hash (gnutls_pk_algorithm_t pk,
                 gnutls_digest_algorithm_t hash, gnutls_datum_t * digest)
{
  int ret;
  gnutls_datum old_digest = { digest->data, digest->size };

  switch (pk)
    {
    case GNUTLS_PK_RSA:
      /* Encode the digest as a DigestInfo
       */
      if ((ret = encode_ber_digest_info (hash, &old_digest, digest)) != 0)
        {
          gnutls_assert ();
          return ret;
        }

      _gnutls_free_datum (&old_digest);
      break;
    case GNUTLS_PK_DSA:
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_UNIMPLEMENTED_FEATURE;
    }

  return 0;
}
