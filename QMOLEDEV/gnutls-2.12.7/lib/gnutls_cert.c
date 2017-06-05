/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
 * 2010 Free Software Foundation, Inc.
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

/* Some of the stuff needed for Certificate authentication is contained
 * in this file.
 */

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <auth_cert.h>
#include <gnutls_cert.h>
#include <gnutls_datum.h>
#include <gnutls_mpi.h>
#include <gnutls_global.h>
#include <gnutls_algorithms.h>
#include <gnutls_dh.h>
#include <gnutls_str.h>
#include <gnutls_state.h>
#include <gnutls_auth.h>
#include <gnutls_x509.h>
#include "x509/x509_int.h"
#ifdef ENABLE_OPENPGP
#include "openpgp/gnutls_openpgp.h"
#endif

/**
 * gnutls_certificate_free_keys:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 *
 * This function will delete all the keys and the certificates associated
 * with the given credentials. This function must not be called when a
 * TLS negotiation that uses the credentials is in progress.
 *
 **/
void
gnutls_certificate_free_keys (gnutls_certificate_credentials_t sc)
{
  unsigned i, j;

  for (i = 0; i < sc->ncerts; i++)
    {
      for (j = 0; j < sc->cert_list_length[i]; j++)
        {
          _gnutls_gcert_deinit (&sc->cert_list[i][j]);
        }
      gnutls_free (sc->cert_list[i]);
    }

  gnutls_free (sc->cert_list_length);
  sc->cert_list_length = NULL;

  gnutls_free (sc->cert_list);
  sc->cert_list = NULL;

  for (i = 0; i < sc->ncerts; i++)
    {
      gnutls_privkey_deinit (sc->pkey[i]);
    }

  gnutls_free (sc->pkey);
  sc->pkey = NULL;

  sc->ncerts = 0;

}

/**
 * gnutls_certificate_get_issuer:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 * @cert: is the certificate to find issuer for
 * @issuer: Will hold the issuer if any. Should be treated as constant.
 * @flags: Use zero.
 *
 * This function will return the issuer of a given certificate.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_certificate_get_issuer (gnutls_certificate_credentials_t sc,
  gnutls_x509_crt_t cert, gnutls_x509_crt_t* issuer, unsigned int flags)
{
int i, ret;

  for (i=0;i<sc->x509_ncas;i++)
    {
      ret = gnutls_x509_crt_check_issuer(cert, sc->x509_ca_list[i]);
      if (ret > 0)
        {
          *issuer = sc->x509_ca_list[i];
          return 0;
        }
    }
  
  return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
}

/**
 * gnutls_certificate_free_cas:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 *
 * This function will delete all the CAs associated with the given
 * credentials. Servers that do not use
 * gnutls_certificate_verify_peers2() may call this to save some
 * memory.
 **/
void
gnutls_certificate_free_cas (gnutls_certificate_credentials_t sc)
{
  unsigned j;

  for (j = 0; j < sc->x509_ncas; j++)
    {
      gnutls_x509_crt_deinit (sc->x509_ca_list[j]);
    }

  sc->x509_ncas = 0;

  gnutls_free (sc->x509_ca_list);
  sc->x509_ca_list = NULL;

}

/**
 * gnutls_certificate_get_x509_cas:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 * @x509_ca_list: will point to the CA list. Should be treated as constant
 * @ncas: the number of CAs
 *
 * This function will export all the CAs associated with the given
 * credentials.
 *
 * Since: 2.4.0
 **/
void
gnutls_certificate_get_x509_cas (gnutls_certificate_credentials_t sc,
                                 gnutls_x509_crt_t ** x509_ca_list,
                                 unsigned int *ncas)
{
  *x509_ca_list = sc->x509_ca_list;
  *ncas = sc->x509_ncas;
}

/**
 * gnutls_certificate_get_x509_crls:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 * @x509_crl_list: the exported CRL list. Should be treated as constant
 * @ncrls: the number of exported CRLs
 *
 * This function will export all the CRLs associated with the given
 * credentials.
 *
 * Since: 2.4.0
 **/
void
gnutls_certificate_get_x509_crls (gnutls_certificate_credentials_t sc,
                                  gnutls_x509_crl_t ** x509_crl_list,
                                  unsigned int *ncrls)
{
  *x509_crl_list = sc->x509_crl_list;
  *ncrls = sc->x509_ncrls;
}

#ifdef ENABLE_OPENPGP

/**
 * gnutls_certificate_get_openpgp_keyring:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 * @keyring: the exported keyring. Should be treated as constant
 *
 * This function will export the OpenPGP keyring associated with the
 * given credentials.
 *
 * Since: 2.4.0
 **/
void
gnutls_certificate_get_openpgp_keyring (gnutls_certificate_credentials_t sc,
                                        gnutls_openpgp_keyring_t * keyring)
{
  *keyring = sc->keyring;
}

#endif

/**
 * gnutls_certificate_free_ca_names:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 *
 * This function will delete all the CA name in the given
 * credentials. Clients may call this to save some memory since in
 * client side the CA names are not used. Servers might want to use
 * this function if a large list of trusted CAs is present and
 * sending the names of it would just consume bandwidth without providing 
 * information to client.
 *
 * CA names are used by servers to advertize the CAs they support to
 * clients.
 **/
void
gnutls_certificate_free_ca_names (gnutls_certificate_credentials_t sc)
{
  _gnutls_free_datum (&sc->x509_rdn_sequence);
}

/*-
 * _gnutls_certificate_get_rsa_params - Returns the RSA parameters pointer
 * @rsa_params: holds the RSA parameters or NULL.
 * @func: function to retrieve the parameters or NULL.
 * @session: The session.
 *
 * This function will return the rsa parameters pointer.
 -*/
gnutls_rsa_params_t
_gnutls_certificate_get_rsa_params (gnutls_rsa_params_t rsa_params,
                                    gnutls_params_function * func,
                                    gnutls_session_t session)
{
  gnutls_params_st params;
  int ret;

  if (session->internals.params.rsa_params)
    {
      return session->internals.params.rsa_params;
    }

  if (rsa_params)
    {
      session->internals.params.rsa_params = rsa_params;
    }
  else if (func)
    {
      ret = func (session, GNUTLS_PARAMS_RSA_EXPORT, &params);
      if (ret == 0 && params.type == GNUTLS_PARAMS_RSA_EXPORT)
        {
          session->internals.params.rsa_params = params.params.rsa_export;
          session->internals.params.free_rsa_params = params.deinit;
        }
    }

  return session->internals.params.rsa_params;
}


/**
 * gnutls_certificate_free_credentials:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus this
 * helper function is provided in order to free (deallocate) it.
 *
 * This function does not free any temporary parameters associated
 * with this structure (ie RSA and DH parameters are not freed by this
 * function).
 **/
void
gnutls_certificate_free_credentials (gnutls_certificate_credentials_t sc)
{
  gnutls_certificate_free_keys (sc);
  gnutls_certificate_free_cas (sc);
  gnutls_certificate_free_ca_names (sc);
#ifdef ENABLE_PKI
  gnutls_certificate_free_crls (sc);
#endif

#ifdef ENABLE_OPENPGP
  gnutls_openpgp_keyring_deinit (sc->keyring);
#endif

  gnutls_free (sc);
}


/**
 * gnutls_certificate_allocate_credentials:
 * @res: is a pointer to a #gnutls_certificate_credentials_t structure.
 *
 * This structure is complex enough to manipulate directly thus this
 * helper function is provided in order to allocate it.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_certificate_allocate_credentials (gnutls_certificate_credentials_t *
                                         res)
{
  *res = gnutls_calloc (1, sizeof (certificate_credentials_st));

  if (*res == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  (*res)->verify_bits = DEFAULT_VERIFY_BITS;
  (*res)->verify_depth = DEFAULT_VERIFY_DEPTH;

  return 0;
}


/* returns the KX algorithms that are supported by a
 * certificate. (Eg a certificate with RSA params, supports
 * GNUTLS_KX_RSA algorithm).
 * This function also uses the KeyUsage field of the certificate
 * extensions in order to disable unneded algorithms.
 */
int
_gnutls_selected_cert_supported_kx (gnutls_session_t session,
                                    gnutls_kx_algorithm_t ** alg,
                                    int *alg_size)
{
  gnutls_kx_algorithm_t kx;
  gnutls_pk_algorithm_t pk;
  gnutls_kx_algorithm_t kxlist[MAX_ALGOS];
  gnutls_cert *cert;
  int i;

  if (session->internals.selected_cert_list_length == 0)
    {
      *alg_size = 0;
      *alg = NULL;
      return 0;
    }

  cert = &session->internals.selected_cert_list[0];
  i = 0;

  for (kx = 0; kx < MAX_ALGOS; kx++)
    {
      pk = _gnutls_map_pk_get_pk (kx);
      if (pk == cert->subject_pk_algorithm)
        {
          /* then check key usage */
          if (_gnutls_check_key_usage (cert, kx) == 0)
            {
              kxlist[i] = kx;
              i++;
            }
        }
    }

  if (i == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  *alg = gnutls_calloc (i, sizeof (gnutls_kx_algorithm_t));
  if (*alg == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  *alg_size = i;

  memcpy (*alg, kxlist, i * sizeof (gnutls_kx_algorithm_t));

  return 0;
}


/**
 * gnutls_certificate_server_set_request:
 * @session: is a #gnutls_session_t structure.
 * @req: is one of GNUTLS_CERT_REQUEST, GNUTLS_CERT_REQUIRE
 *
 * This function specifies if we (in case of a server) are going to
 * send a certificate request message to the client. If @req is
 * GNUTLS_CERT_REQUIRE then the server will return an error if the
 * peer does not provide a certificate. If you do not call this
 * function then the client will not be asked to send a certificate.
 **/
void
gnutls_certificate_server_set_request (gnutls_session_t session,
                                       gnutls_certificate_request_t req)
{
  session->internals.send_cert_req = req;
}

/**
 * gnutls_certificate_client_set_retrieve_function:
 * @cred: is a #gnutls_certificate_credentials_t structure.
 * @func: is the callback function
 *
 * This function sets a callback to be called in order to retrieve the
 * certificate to be used in the handshake.
 *
 * The callback's function prototype is:
 * int (*callback)(gnutls_session_t, const gnutls_datum_t* req_ca_dn, int nreqs,
 * const gnutls_pk_algorithm_t* pk_algos, int pk_algos_length, gnutls_retr_st* st);
 *
 * @req_ca_cert is only used in X.509 certificates.
 * Contains a list with the CA names that the server considers trusted.
 * Normally we should send a certificate that is signed
 * by one of these CAs. These names are DER encoded. To get a more
 * meaningful value use the function gnutls_x509_rdn_get().
 *
 * @pk_algos contains a list with server's acceptable signature algorithms.
 * The certificate returned should support the server's given algorithms.
 *
 * @st should contain the certificates and private keys.
 *
 * If the callback function is provided then gnutls will call it, in the
 * handshake, after the certificate request message has been received.
 *
 * The callback function should set the certificate list to be sent,
 * and return 0 on success. If no certificate was selected then the
 * number of certificates should be set to zero. The value (-1)
 * indicates error and the handshake will be terminated.
 **/
void gnutls_certificate_client_set_retrieve_function
  (gnutls_certificate_credentials_t cred,
   gnutls_certificate_client_retrieve_function * func)
{
  cred->client_get_cert_callback = func;
}

/**
 * gnutls_certificate_server_set_retrieve_function:
 * @cred: is a #gnutls_certificate_credentials_t structure.
 * @func: is the callback function
 *
 * This function sets a callback to be called in order to retrieve the
 * certificate to be used in the handshake.
 *
 * The callback's function prototype is:
 * int (*callback)(gnutls_session_t, gnutls_retr_st* st);
 *
 * @st should contain the certificates and private keys.
 *
 * If the callback function is provided then gnutls will call it, in the
 * handshake, after the certificate request message has been received.
 *
 * The callback function should set the certificate list to be sent, and
 * return 0 on success.  The value (-1) indicates error and the handshake
 * will be terminated.
 **/
void gnutls_certificate_server_set_retrieve_function
  (gnutls_certificate_credentials_t cred,
   gnutls_certificate_server_retrieve_function * func)
{
  cred->server_get_cert_callback = func;
}

/**
 * gnutls_certificate_set_retrieve_function:
 * @cred: is a #gnutls_certificate_credentials_t structure.
 * @func: is the callback function
 *
 * This function sets a callback to be called in order to retrieve the
 * certificate to be used in the handshake.
 *
 * The callback's function prototype is:
 * int (*callback)(gnutls_session_t, const gnutls_datum_t* req_ca_dn, int nreqs,
 * const gnutls_pk_algorithm_t* pk_algos, int pk_algos_length, gnutls_retr2_st* st);
 *
 * @req_ca_cert is only used in X.509 certificates.
 * Contains a list with the CA names that the server considers trusted.
 * Normally we should send a certificate that is signed
 * by one of these CAs. These names are DER encoded. To get a more
 * meaningful value use the function gnutls_x509_rdn_get().
 *
 * @pk_algos contains a list with server's acceptable signature algorithms.
 * The certificate returned should support the server's given algorithms.
 *
 * @st should contain the certificates and private keys.
 *
 * If the callback function is provided then gnutls will call it, in the
 * handshake, after the certificate request message has been received.
 *
 * In server side pk_algos and req_ca_dn are NULL.
 *
 * The callback function should set the certificate list to be sent,
 * and return 0 on success. If no certificate was selected then the
 * number of certificates should be set to zero. The value (-1)
 * indicates error and the handshake will be terminated.
 **/
void gnutls_certificate_set_retrieve_function
  (gnutls_certificate_credentials_t cred,
   gnutls_certificate_retrieve_function * func)
{
  cred->get_cert_callback = func;
}

/**
 * gnutls_certificate_set_verify_function:
 * @cred: is a #gnutls_certificate_credentials_t structure.
 * @func: is the callback function
 *
 * This function sets a callback to be called when peer's certificate
 * has been received in order to verify it on receipt rather than
 * doing after the handshake is completed.
 *
 * The callback's function prototype is:
 * int (*callback)(gnutls_session_t);
 *
 * If the callback function is provided then gnutls will call it, in the
 * handshake, just after the certificate message has been received.
 * To verify or obtain the certificate the gnutls_certificate_verify_peers2(),
 * gnutls_certificate_type_get(), gnutls_certificate_get_peers() functions
 * can be used.
 *
 * The callback function should return 0 for the handshake to continue
 * or non-zero to terminate.
 *
 * Since: 2.10.0
 **/
void
  gnutls_certificate_set_verify_function
  (gnutls_certificate_credentials_t cred,
   gnutls_certificate_verify_function * func)
{
  cred->verify_callback = func;
}

/*-
 * _gnutls_x509_extract_certificate_activation_time - return the peer's certificate activation time
 * @cert: should contain an X.509 DER encoded certificate
 *
 * This function will return the certificate's activation time in UNIX time
 * (ie seconds since 00:00:00 UTC January 1, 1970).
 *
 * Returns a (time_t) -1 in case of an error.
 *
 -*/
static time_t
_gnutls_x509_get_raw_crt_activation_time (const gnutls_datum_t * cert)
{
  gnutls_x509_crt_t xcert;
  time_t result;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return (time_t) - 1;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return (time_t) - 1;
    }

  result = gnutls_x509_crt_get_activation_time (xcert);

  gnutls_x509_crt_deinit (xcert);

  return result;
}

/*-
 * gnutls_x509_extract_certificate_expiration_time:
 * @cert: should contain an X.509 DER encoded certificate
 *
 * This function will return the certificate's expiration time in UNIX
 * time (ie seconds since 00:00:00 UTC January 1, 1970).  Returns a
 *
 * (time_t) -1 in case of an error.
 *
 -*/
static time_t
_gnutls_x509_get_raw_crt_expiration_time (const gnutls_datum_t * cert)
{
  gnutls_x509_crt_t xcert;
  time_t result;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return (time_t) - 1;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return (time_t) - 1;
    }

  result = gnutls_x509_crt_get_expiration_time (xcert);

  gnutls_x509_crt_deinit (xcert);

  return result;
}

#ifdef ENABLE_OPENPGP
/*-
 * _gnutls_openpgp_crt_verify_peers - return the peer's certificate status
 * @session: is a gnutls session
 *
 * This function will try to verify the peer's certificate and return its status (TRUSTED, INVALID etc.).
 * Returns a negative error code in case of an error, or GNUTLS_E_NO_CERTIFICATE_FOUND if no certificate was sent.
 -*/
static int
_gnutls_openpgp_crt_verify_peers (gnutls_session_t session,
                                  unsigned int *status)
{
  cert_auth_info_t info;
  gnutls_certificate_credentials_t cred;
  int peer_certificate_list_size, ret;

  CHECK_AUTH (GNUTLS_CRD_CERTIFICATE, GNUTLS_E_INVALID_REQUEST);

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    return GNUTLS_E_INVALID_REQUEST;

  cred = (gnutls_certificate_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_CERTIFICATE, NULL);
  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  if (info->raw_certificate_list == NULL || info->ncerts == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_NO_CERTIFICATE_FOUND;
    }

  /* generate a list of gnutls_certs based on the auth info
   * raw certs.
   */
  peer_certificate_list_size = info->ncerts;

  if (peer_certificate_list_size != 1)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  /* Verify certificate 
   */
  ret =
    _gnutls_openpgp_verify_key (cred, &info->raw_certificate_list[0],
                                peer_certificate_list_size, status);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}
#endif

/**
 * gnutls_certificate_verify_peers2:
 * @session: is a gnutls session
 * @status: is the output of the verification
 *
 * This function will try to verify the peer's certificate and return
 * its status (trusted, invalid etc.).  The value of @status should
 * be one or more of the gnutls_certificate_status_t enumerated
 * elements bitwise or'd. To avoid denial of service attacks some
 * default upper limits regarding the certificate key size and chain
 * size are set. To override them use
 * gnutls_certificate_set_verify_limits().
 *
 * Note that you must also check the peer's name in order to check if
 * the verified certificate belongs to the actual peer.
 *
 * This function uses gnutls_x509_crt_list_verify() with the CAs in
 * the credentials as trusted CAs.
 *
 * Returns: a negative error code on error and zero on success.
 **/
int
gnutls_certificate_verify_peers2 (gnutls_session_t session,
                                  unsigned int *status)
{
  cert_auth_info_t info;

  CHECK_AUTH (GNUTLS_CRD_CERTIFICATE, GNUTLS_E_INVALID_REQUEST);

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    {
      return GNUTLS_E_NO_CERTIFICATE_FOUND;
    }

  if (info->raw_certificate_list == NULL || info->ncerts == 0)
    return GNUTLS_E_NO_CERTIFICATE_FOUND;

  switch (gnutls_certificate_type_get (session))
    {
    case GNUTLS_CRT_X509:
      return _gnutls_x509_cert_verify_peers (session, status);
#ifdef ENABLE_OPENPGP
    case GNUTLS_CRT_OPENPGP:
      return _gnutls_openpgp_crt_verify_peers (session, status);
#endif
    default:
      return GNUTLS_E_INVALID_REQUEST;
    }
}

/**
 * gnutls_certificate_verify_peers:
 * @session: is a gnutls session
 *
 * This function will try to verify the peer's certificate and return
 * its status (trusted, invalid etc.).  However you must also check
 * the peer's name in order to check if the verified certificate
 * belongs to the actual peer.
 *
 * This function uses gnutls_x509_crt_list_verify().
 *
 * Returns: one or more of the #gnutls_certificate_status_t
 * enumerated elements bitwise or'd, or a negative value on error.
 *
 * Deprecated: Use gnutls_certificate_verify_peers2() instead.
 **/
int
gnutls_certificate_verify_peers (gnutls_session_t session)
{
  unsigned int status;
  int ret;

  ret = gnutls_certificate_verify_peers2 (session, &status);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return status;
}

/**
 * gnutls_certificate_expiration_time_peers:
 * @session: is a gnutls session
 *
 * This function will return the peer's certificate expiration time.
 *
 * Returns: (time_t)-1 on error.
 *
 * Deprecated: gnutls_certificate_verify_peers2() now verifies expiration times.
 **/
time_t
gnutls_certificate_expiration_time_peers (gnutls_session_t session)
{
  cert_auth_info_t info;

  CHECK_AUTH (GNUTLS_CRD_CERTIFICATE, GNUTLS_E_INVALID_REQUEST);

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    {
      return (time_t) - 1;
    }

  if (info->raw_certificate_list == NULL || info->ncerts == 0)
    {
      gnutls_assert ();
      return (time_t) - 1;
    }

  switch (gnutls_certificate_type_get (session))
    {
    case GNUTLS_CRT_X509:
      return
        _gnutls_x509_get_raw_crt_expiration_time (&info->raw_certificate_list
                                                  [0]);
#ifdef ENABLE_OPENPGP
    case GNUTLS_CRT_OPENPGP:
      return
        _gnutls_openpgp_get_raw_key_expiration_time
        (&info->raw_certificate_list[0]);
#endif
    default:
      return (time_t) - 1;
    }
}

/**
 * gnutls_certificate_activation_time_peers:
 * @session: is a gnutls session
 *
 * This function will return the peer's certificate activation time.
 * This is the creation time for openpgp keys.
 *
 * Returns: (time_t)-1 on error.
 *
 * Deprecated: gnutls_certificate_verify_peers2() now verifies activation times.
 **/
time_t
gnutls_certificate_activation_time_peers (gnutls_session_t session)
{
  cert_auth_info_t info;

  CHECK_AUTH (GNUTLS_CRD_CERTIFICATE, GNUTLS_E_INVALID_REQUEST);

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    {
      return (time_t) - 1;
    }

  if (info->raw_certificate_list == NULL || info->ncerts == 0)
    {
      gnutls_assert ();
      return (time_t) - 1;
    }

  switch (gnutls_certificate_type_get (session))
    {
    case GNUTLS_CRT_X509:
      return
        _gnutls_x509_get_raw_crt_activation_time (&info->raw_certificate_list
                                                  [0]);
#ifdef ENABLE_OPENPGP
    case GNUTLS_CRT_OPENPGP:
      return
        _gnutls_openpgp_get_raw_key_creation_time (&info->raw_certificate_list
                                                   [0]);
#endif
    default:
      return (time_t) - 1;
    }
}

/* Converts the first certificate for the cert_auth_info structure
 * to a gcert.
 */
int
_gnutls_get_auth_info_gcert (gnutls_cert * gcert,
                             gnutls_certificate_type_t type,
                             cert_auth_info_t info,
                             int flags /* OR of ConvFlags */ )
{
  switch (type)
    {
    case GNUTLS_CRT_X509:
      return _gnutls_x509_raw_cert_to_gcert (gcert,
                                             &info->raw_certificate_list[0],
                                             flags);
#ifdef ENABLE_OPENPGP
    case GNUTLS_CRT_OPENPGP:
      return _gnutls_openpgp_raw_crt_to_gcert (gcert,
                                               &info->raw_certificate_list[0],
                                               info->use_subkey ? info->
                                               subkey_id : NULL);
#endif
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }
}

/* This function will convert a der certificate to a format
 * (structure) that gnutls can understand and use. Actually the
 * important thing on this function is that it extracts the 
 * certificate's (public key) parameters.
 *
 * The noext flag is used to complete the handshake even if the
 * extensions found in the certificate are unsupported and critical. 
 * The critical extensions will be catched by the verification functions.
 */
int
_gnutls_x509_raw_cert_to_gcert (gnutls_cert * gcert,
                                const gnutls_datum_t * derCert,
                                int flags /* OR of ConvFlags */ )
{
  int ret;
  gnutls_x509_crt_t cert;

  ret = gnutls_x509_crt_init (&cert);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_x509_crt_import (cert, derCert, GNUTLS_X509_FMT_DER);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_x509_crt_deinit (cert);
      return ret;
    }

  ret = _gnutls_x509_crt_to_gcert (gcert, cert, flags);
  gnutls_x509_crt_deinit (cert);

  return ret;
}

/* Like above but it accepts a parsed certificate instead.
 */
int
_gnutls_x509_crt_to_gcert (gnutls_cert * gcert,
                           gnutls_x509_crt_t cert, unsigned int flags)
{
  int ret = 0;

  memset (gcert, 0, sizeof (gnutls_cert));
  gcert->cert_type = GNUTLS_CRT_X509;
  gcert->sign_algo = gnutls_x509_crt_get_signature_algorithm (cert);

  if (!(flags & CERT_NO_COPY))
    {
#define SMALL_DER 1536
      opaque *der;
      size_t der_size = SMALL_DER;

      /* initially allocate a bogus size, just in case the certificate
       * fits in it. That way we minimize the DER encodings performed.
       */
      der = gnutls_malloc (SMALL_DER);
      if (der == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      ret =
        gnutls_x509_crt_export (cert, GNUTLS_X509_FMT_DER, der, &der_size);
      if (ret < 0 && ret != GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          gnutls_assert ();
          gnutls_free (der);
          return ret;
        }

      if (ret == GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          der = gnutls_realloc (der, der_size);
          if (der == NULL)
            {
              gnutls_assert ();
              return GNUTLS_E_MEMORY_ERROR;
            }

          ret =
            gnutls_x509_crt_export (cert, GNUTLS_X509_FMT_DER, der,
                                    &der_size);
          if (ret < 0)
            {
              gnutls_assert ();
              gnutls_free (der);
              return ret;
            }
        }

      gcert->raw.data = der;
      gcert->raw.size = der_size;
    }
  else
    /* now we have 0 or a bitwise or of things to decode */
    flags ^= CERT_NO_COPY;


  if (flags & CERT_ONLY_EXTENSIONS || flags == 0)
    {
      ret = gnutls_x509_crt_get_key_usage (cert, &gcert->key_usage, NULL);
      if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
        gcert->key_usage = 0;
      else if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      gcert->version = gnutls_x509_crt_get_version (cert);
    }
  gcert->subject_pk_algorithm = gnutls_x509_crt_get_pk_algorithm (cert, NULL);

  if (flags & CERT_ONLY_PUBKEY || flags == 0)
    {
      gcert->params_size = MAX_PUBLIC_PARAMS_SIZE;
      ret =
        _gnutls_x509_crt_get_mpis (cert, gcert->params, &gcert->params_size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
    }

  return 0;

}

void
_gnutls_gcert_deinit (gnutls_cert * cert)
{
  int i;

  if (cert == NULL)
    return;

  for (i = 0; i < cert->params_size; i++)
    {
      _gnutls_mpi_release (&cert->params[i]);
    }

  _gnutls_free_datum (&cert->raw);
}

/**
 * gnutls_sign_callback_set:
 * @session: is a gnutls session
 * @sign_func: function pointer to application's sign callback.
 * @userdata: void pointer that will be passed to sign callback.
 *
 * Set the callback function.  The function must have this prototype:
 *
 * typedef int (*gnutls_sign_func) (gnutls_session_t session,
 *                                  void *userdata,
 *                                  gnutls_certificate_type_t cert_type,
 *                                  const gnutls_datum_t * cert,
 *                                  const gnutls_datum_t * hash,
 *                                  gnutls_datum_t * signature);
 *
 * The @userdata parameter is passed to the @sign_func verbatim, and
 * can be used to store application-specific data needed in the
 * callback function.  See also gnutls_sign_callback_get().
 *
 * Deprecated: Use the PKCS 11 interfaces instead.
 */
void
gnutls_sign_callback_set (gnutls_session_t session,
                          gnutls_sign_func sign_func, void *userdata)
{
  session->internals.sign_func = sign_func;
  session->internals.sign_func_userdata = userdata;
}

/**
 * gnutls_sign_callback_get:
 * @session: is a gnutls session
 * @userdata: if non-%NULL, will be set to abstract callback pointer.
 *
 * Retrieve the callback function, and its userdata pointer.
 *
 * Returns: The function pointer set by gnutls_sign_callback_set(), or
 *   if not set, %NULL.
 *
 * Deprecated: Use the PKCS 11 interfaces instead.
 */
gnutls_sign_func
gnutls_sign_callback_get (gnutls_session_t session, void **userdata)
{
  if (userdata)
    *userdata = session->internals.sign_func_userdata;
  return session->internals.sign_func;
}
