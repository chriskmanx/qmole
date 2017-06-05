/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2008, 2009, 2010 Free
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

/* This file contains certificate authentication functions to be exported in the
 * API and did not fit elsewhere.
 */

#include <gnutls_int.h>
#include <auth_srp.h>
#include <auth_anon.h>
#include <auth_cert.h>
#include <auth_psk.h>
#include <gnutls_errors.h>
#include <gnutls_auth.h>
#include <gnutls_state.h>
#include <gnutls_datum.h>

/* ANON & DHE */

/**
 * gnutls_dh_set_prime_bits:
 * @session: is a #gnutls_session_t structure.
 * @bits: is the number of bits
 *
 * This function sets the number of bits, for use in an Diffie-Hellman
 * key exchange.  This is used both in DH ephemeral and DH anonymous
 * cipher suites.  This will set the minimum size of the prime that
 * will be used for the handshake.
 *
 * In the client side it sets the minimum accepted number of bits.  If
 * a server sends a prime with less bits than that
 * %GNUTLS_E_DH_PRIME_UNACCEPTABLE will be returned by the handshake.
 *
 * This function has no effect in server side.
 *
 **/
void
gnutls_dh_set_prime_bits (gnutls_session_t session, unsigned int bits)
{
  session->internals.dh_prime_bits = bits;
}


/**
 * gnutls_dh_get_group:
 * @session: is a gnutls session
 * @raw_gen: will hold the generator.
 * @raw_prime: will hold the prime.
 *
 * This function will return the group parameters used in the last
 * Diffie-Hellman key exchange with the peer.  These are the prime and
 * the generator used.  This function should be used for both
 * anonymous and ephemeral Diffie-Hellman.  The output parameters must
 * be freed with gnutls_free().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, otherwise
 *   an error code is returned.
 **/
int
gnutls_dh_get_group (gnutls_session_t session,
                     gnutls_datum_t * raw_gen, gnutls_datum_t * raw_prime)
{
  dh_info_st *dh;
  int ret;
  anon_auth_info_t anon_info;
  cert_auth_info_t cert_info;
  psk_auth_info_t psk_info;

  switch (gnutls_auth_get_type (session))
    {
    case GNUTLS_CRD_ANON:
      anon_info = _gnutls_get_auth_info (session);
      if (anon_info == NULL)
        return GNUTLS_E_INTERNAL_ERROR;
      dh = &anon_info->dh;
      break;
    case GNUTLS_CRD_PSK:
      psk_info = _gnutls_get_auth_info (session);
      if (psk_info == NULL)
        return GNUTLS_E_INTERNAL_ERROR;
      dh = &psk_info->dh;
      break;
    case GNUTLS_CRD_CERTIFICATE:
      cert_info = _gnutls_get_auth_info (session);
      if (cert_info == NULL)
        return GNUTLS_E_INTERNAL_ERROR;
      dh = &cert_info->dh;
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = _gnutls_set_datum (raw_prime, dh->prime.data, dh->prime.size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_set_datum (raw_gen, dh->generator.data, dh->generator.size);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (raw_prime);
      return ret;
    }

  return 0;
}

/**
 * gnutls_dh_get_pubkey:
 * @session: is a gnutls session
 * @raw_key: will hold the public key.
 *
 * This function will return the peer's public key used in the last
 * Diffie-Hellman key exchange.  This function should be used for both
 * anonymous and ephemeral Diffie-Hellman.  The output parameters must
 * be freed with gnutls_free().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, otherwise
 *   an error code is returned.
 **/
int
gnutls_dh_get_pubkey (gnutls_session_t session, gnutls_datum_t * raw_key)
{
  dh_info_st *dh;
  anon_auth_info_t anon_info;
  cert_auth_info_t cert_info;
  cert_auth_info_t psk_info;

  switch (gnutls_auth_get_type (session))
    {
    case GNUTLS_CRD_ANON:
      {
        anon_info = _gnutls_get_auth_info (session);
        if (anon_info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;
        dh = &anon_info->dh;
        break;
      }
    case GNUTLS_CRD_PSK:
      {
        psk_info = _gnutls_get_auth_info (session);
        if (psk_info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;
        dh = &psk_info->dh;
        break;
      }
    case GNUTLS_CRD_CERTIFICATE:
      {

        cert_info = _gnutls_get_auth_info (session);
        if (cert_info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;
        dh = &cert_info->dh;
        break;
      }
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return _gnutls_set_datum (raw_key, dh->public_key.data,
                            dh->public_key.size);
}

/**
 * gnutls_rsa_export_get_pubkey:
 * @session: is a gnutls session
 * @exponent: will hold the exponent.
 * @modulus: will hold the modulus.
 *
 * This function will return the peer's public key exponent and
 * modulus used in the last RSA-EXPORT authentication.  The output
 * parameters must be freed with gnutls_free().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, otherwise
 *   an error code is returned.
 **/
int
gnutls_rsa_export_get_pubkey (gnutls_session_t session,
                              gnutls_datum_t * exponent,
                              gnutls_datum_t * modulus)
{
  cert_auth_info_t info;
  int ret;

  if (gnutls_auth_get_type (session) == GNUTLS_CRD_CERTIFICATE)
    {
      info = _gnutls_get_auth_info (session);
      if (info == NULL)
        return GNUTLS_E_INTERNAL_ERROR;

      ret = _gnutls_set_datum (modulus, info->rsa_export.modulus.data,
                               info->rsa_export.modulus.size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      ret = _gnutls_set_datum (exponent, info->rsa_export.exponent.data,
                               info->rsa_export.exponent.size);
      if (ret < 0)
        {
          gnutls_assert ();
          _gnutls_free_datum (modulus);
          return ret;
        }

      return 0;
    }

  return GNUTLS_E_INVALID_REQUEST;
}


/**
 * gnutls_dh_get_secret_bits:
 * @session: is a gnutls session
 *
 * This function will return the bits used in the last Diffie-Hellman
 * key exchange with the peer.  Should be used for both anonymous and
 * ephemeral Diffie-Hellman.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, otherwise
 *   an error code is returned.
 **/
int
gnutls_dh_get_secret_bits (gnutls_session_t session)
{
  switch (gnutls_auth_get_type (session))
    {
    case GNUTLS_CRD_ANON:
      {
        anon_auth_info_t info;

        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;
        return info->dh.secret_bits;
      }
    case GNUTLS_CRD_PSK:
      {
        psk_auth_info_t info;

        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;
        return info->dh.secret_bits;
      }
    case GNUTLS_CRD_CERTIFICATE:
      {
        cert_auth_info_t info;

        info = _gnutls_get_auth_info (session);
        if (info == NULL)
          return GNUTLS_E_INTERNAL_ERROR;

        return info->dh.secret_bits;
      }
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }
}

static int
mpi_buf2bits (gnutls_datum_t * mpi_buf)
{
  bigint_t mpi;
  int rc;

  rc = _gnutls_mpi_scan_nz (&mpi, mpi_buf->data, mpi_buf->size);
  if (rc)
    {
      gnutls_assert ();
      return rc;
    }

  rc = _gnutls_mpi_get_nbits (mpi);
  _gnutls_mpi_release (&mpi);

  return rc;
}

/**
 * gnutls_dh_get_prime_bits:
 * @session: is a gnutls session
 *
 * This function will return the bits of the prime used in the last
 * Diffie-Hellman key exchange with the peer.  Should be used for both
 * anonymous and ephemeral Diffie-Hellman.  Note that some ciphers,
 * like RSA and DSA without DHE, does not use a Diffie-Hellman key
 * exchange, and then this function will return 0.
 *
 * Returns: The Diffie-Hellman bit strength is returned, or 0 if no
 *   Diffie-Hellman key exchange was done, or a negative error code on
 *   failure.
 **/
int
gnutls_dh_get_prime_bits (gnutls_session_t session)
{
  dh_info_st *dh;

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
      return GNUTLS_E_INVALID_REQUEST;
    }

  return mpi_buf2bits (&dh->prime);
}

/**
 * gnutls_rsa_export_get_modulus_bits:
 * @session: is a gnutls session
 *
 * Get the export RSA parameter's modulus size.
 *
 * Returns: the bits used in the last RSA-EXPORT key exchange with the
 *   peer, or a negative value in case of error.
 **/
int
gnutls_rsa_export_get_modulus_bits (gnutls_session_t session)
{
  cert_auth_info_t info;

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    return GNUTLS_E_INTERNAL_ERROR;

  return mpi_buf2bits (&info->rsa_export.modulus);
}

/**
 * gnutls_dh_get_peers_public_bits:
 * @session: is a gnutls session
 *
 * Get the Diffie-Hellman public key bit size.  Can be used for both
 * anonymous and ephemeral Diffie-Hellman.
 *
 * Returns: the public key bit size used in the last Diffie-Hellman
 *   key exchange with the peer, or a negative value in case of error.
 **/
int
gnutls_dh_get_peers_public_bits (gnutls_session_t session)
{
  dh_info_st *dh;

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
      return GNUTLS_E_INVALID_REQUEST;
    }

  return mpi_buf2bits (&dh->public_key);
}

/* CERTIFICATE STUFF */

/**
 * gnutls_certificate_get_ours:
 * @session: is a gnutls session
 *
 * Get the certificate as sent to the peer, in the last handshake.
 * These certificates are in raw format.  In X.509 this is a
 * certificate list. In OpenPGP this is a single certificate.
 *
 * Returns: return a pointer to a #gnutls_datum_t containing our
 *   certificates, or %NULL in case of an error or if no certificate
 *   was used.
 **/
const gnutls_datum_t *
gnutls_certificate_get_ours (gnutls_session_t session)
{
  gnutls_certificate_credentials_t cred;

  CHECK_AUTH (GNUTLS_CRD_CERTIFICATE, NULL);

  cred = (gnutls_certificate_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_CERTIFICATE, NULL);
  if (cred == NULL || cred->cert_list == NULL)
    {
      gnutls_assert ();
      return NULL;
    }

  if (session->internals.selected_cert_list == NULL)
    return NULL;

  return &session->internals.selected_cert_list[0].raw;
}

/**
 * gnutls_certificate_get_peers:
 * @session: is a gnutls session
 * @list_size: is the length of the certificate list
 *
 * Get the peer's raw certificate (chain) as sent by the peer.  These
 * certificates are in raw format (DER encoded for X.509).  In case of
 * a X.509 then a certificate list may be present.  The first
 * certificate in the list is the peer's certificate, following the
 * issuer's certificate, then the issuer's issuer etc.
 *
 * In case of OpenPGP keys a single key will be returned in raw
 * format.
 *
 * Returns: return a pointer to a #gnutls_datum_t containing our
 *   certificates, or %NULL in case of an error or if no certificate
 *   was used.
 **/
const gnutls_datum_t *
gnutls_certificate_get_peers (gnutls_session_t
                              session, unsigned int *list_size)
{
  cert_auth_info_t info;

  CHECK_AUTH (GNUTLS_CRD_CERTIFICATE, NULL);

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    return NULL;

  *list_size = info->ncerts;
  return info->raw_certificate_list;
}


/**
 * gnutls_certificate_client_get_request_status:
 * @session: is a gnutls session
 *
 * Get whether client certificate is requested or not.
 *
 * Returns: 0 if the peer (server) did not request client
 *   authentication or 1 otherwise, or a negative value in case of
 *   error.
 **/
int
gnutls_certificate_client_get_request_status (gnutls_session_t session)
{
  return session->key->certificate_requested;
}

/**
 * gnutls_fingerprint:
 * @algo: is a digest algorithm
 * @data: is the data
 * @result: is the place where the result will be copied (may be null).
 * @result_size: should hold the size of the result. The actual size
 * of the returned result will also be copied there.
 *
 * This function will calculate a fingerprint (actually a hash), of
 * the given data.  The result is not printable data.  You should
 * convert it to hex, or to something else printable.
 *
 * This is the usual way to calculate a fingerprint of an X.509 DER
 * encoded certificate.  Note however that the fingerprint of an
 * OpenPGP is not just a hash and cannot be calculated with this
 * function.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (0) is returned, otherwise
 *   an error code is returned.
 **/
int
gnutls_fingerprint (gnutls_digest_algorithm_t algo,
                    const gnutls_datum_t * data, void *result,
                    size_t * result_size)
{
  digest_hd_st td;
  int hash_len = _gnutls_hash_get_algo_len (HASH2MAC (algo));

  if (hash_len < 0 || (unsigned) hash_len > *result_size || result == NULL)
    {
      *result_size = hash_len;
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }
  *result_size = hash_len;

  if (result)
    {
      int ret = _gnutls_hash_init (&td, HASH2MAC (algo));
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      _gnutls_hash (&td, data->data, data->size);

      _gnutls_hash_deinit (&td, result);
    }

  return 0;
}


/**
 * gnutls_certificate_set_dh_params:
 * @res: is a gnutls_certificate_credentials_t structure
 * @dh_params: is a structure that holds Diffie-Hellman parameters.
 *
 * This function will set the Diffie-Hellman parameters for a
 * certificate server to use. These parameters will be used in
 * Ephemeral Diffie-Hellman cipher suites.  Note that only a pointer
 * to the parameters are stored in the certificate handle, so if you
 * deallocate the parameters before the certificate is deallocated,
 * you must change the parameters stored in the certificate first.
 *
 **/
void
gnutls_certificate_set_dh_params (gnutls_certificate_credentials_t res,
                                  gnutls_dh_params_t dh_params)
{
  res->dh_params = dh_params;
}

/**
 * gnutls_certificate_set_params_function:
 * @res: is a gnutls_certificate_credentials_t structure
 * @func: is the function to be called
 *
 * This function will set a callback in order for the server to get
 * the Diffie-Hellman or RSA parameters for certificate
 * authentication.  The callback should return zero on success.
 **/
void
gnutls_certificate_set_params_function (gnutls_certificate_credentials_t res,
                                        gnutls_params_function * func)
{
  res->params_func = func;
}


/**
 * gnutls_certificate_set_verify_flags:
 * @res: is a gnutls_certificate_credentials_t structure
 * @flags: are the flags
 *
 * This function will set the flags to be used at verification of the
 * certificates.  Flags must be OR of the
 * #gnutls_certificate_verify_flags enumerations.
 *
 **/
void
gnutls_certificate_set_verify_flags (gnutls_certificate_credentials_t
                                     res, unsigned int flags)
{
  res->verify_flags = flags;
}

/**
 * gnutls_certificate_set_verify_limits:
 * @res: is a gnutls_certificate_credentials structure
 * @max_bits: is the number of bits of an acceptable certificate (default 8200)
 * @max_depth: is maximum depth of the verification of a certificate chain (default 5)
 *
 * This function will set some upper limits for the default
 * verification function, gnutls_certificate_verify_peers2(), to avoid
 * denial of service attacks.  You can set them to zero to disable
 * limits.
 **/
void
gnutls_certificate_set_verify_limits (gnutls_certificate_credentials_t res,
                                      unsigned int max_bits,
                                      unsigned int max_depth)
{
  res->verify_depth = max_depth;
  res->verify_bits = max_bits;
}

/**
 * gnutls_certificate_set_rsa_export_params:
 * @res: is a gnutls_certificate_credentials_t structure
 * @rsa_params: is a structure that holds temporary RSA parameters.
 *
 * This function will set the temporary RSA parameters for a
 * certificate server to use.  These parameters will be used in
 * RSA-EXPORT cipher suites.
 **/
void
gnutls_certificate_set_rsa_export_params (gnutls_certificate_credentials_t
                                          res, gnutls_rsa_params_t rsa_params)
{
  res->rsa_params = rsa_params;
}

/**
 * gnutls_psk_set_params_function:
 * @res: is a gnutls_psk_server_credentials_t structure
 * @func: is the function to be called
 *
 * This function will set a callback in order for the server to get
 * the Diffie-Hellman or RSA parameters for PSK authentication.  The
 * callback should return zero on success.
 **/
void
gnutls_psk_set_params_function (gnutls_psk_server_credentials_t res,
                                gnutls_params_function * func)
{
  res->params_func = func;
}

/**
 * gnutls_anon_set_params_function:
 * @res: is a gnutls_anon_server_credentials_t structure
 * @func: is the function to be called
 *
 * This function will set a callback in order for the server to get
 * the Diffie-Hellman or RSA parameters for anonymous authentication.
 * The callback should return zero on success.
 **/
void
gnutls_anon_set_params_function (gnutls_anon_server_credentials_t res,
                                 gnutls_params_function * func)
{
  res->params_func = func;
}
