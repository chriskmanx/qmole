/*
 * Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2010 Free Software
 * Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS-EXTRA.
 *
 * GnuTLS-extra is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *               
 * GnuTLS-extra is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *                               
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* This file includes all functions that were in the 0.5.x and 0.8.x
 * gnutls API. They are now implemented over the new certificate parsing
 * API.
 */

#include "gnutls_int.h"

#include <gnutls_global.h>
#include <gnutls_errors.h>
#include <string.h>             /* memset */
#include <x509/x509_int.h>
#include <libtasn1.h>
#include <gnutls/x509.h>
#include <openssl_compat.h>

/*-
 * gnutls_x509_extract_dn:
 * @idn: should contain a DER encoded RDN sequence
 * @rdn: a pointer to a structure to hold the name
 *
 * This function will return the name of the given RDN sequence.
 * The name will be returned as a gnutls_x509_dn structure.
 * Returns a negative error code in case of an error.
 *
 -*/
int
gnutls_x509_extract_dn (const gnutls_datum_t * idn, gnutls_x509_dn * rdn)
{
  ASN1_TYPE dn = ASN1_TYPE_EMPTY;
  int result;
  size_t len;

  if ((result =
       asn1_create_element (_gnutls_get_pkix (),
                            "PKIX1.Name", &dn)) != ASN1_SUCCESS)
    {
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&dn, idn->data, idn->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      /* couldn't decode DER */
      asn1_delete_structure (&dn);
      return _gnutls_asn2err (result);
    }

  memset (rdn, 0, sizeof (gnutls_x509_dn));

  len = sizeof (rdn->country);
  _gnutls_x509_parse_dn_oid (dn, "", GNUTLS_OID_X520_COUNTRY_NAME, 0, 0,
                             rdn->country, &len);

  len = sizeof (rdn->organization);
  _gnutls_x509_parse_dn_oid (dn, "", GNUTLS_OID_X520_ORGANIZATION_NAME, 0,
                             0, rdn->organization, &len);

  len = sizeof (rdn->organizational_unit_name);
  _gnutls_x509_parse_dn_oid (dn, "",
                             GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME, 0,
                             0, rdn->organizational_unit_name, &len);

  len = sizeof (rdn->common_name);
  _gnutls_x509_parse_dn_oid (dn, "", GNUTLS_OID_X520_COMMON_NAME, 0, 0,
                             rdn->common_name, &len);

  len = sizeof (rdn->locality_name);
  _gnutls_x509_parse_dn_oid (dn, "", GNUTLS_OID_X520_LOCALITY_NAME, 0, 0,
                             rdn->locality_name, &len);

  len = sizeof (rdn->state_or_province_name);
  _gnutls_x509_parse_dn_oid (dn, "",
                             GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME, 0, 0,
                             rdn->state_or_province_name, &len);

  len = sizeof (rdn->email);
  _gnutls_x509_parse_dn_oid (dn, "", GNUTLS_OID_PKCS9_EMAIL, 0, 0,
                             rdn->email, &len);

  asn1_delete_structure (&dn);

  return 0;
}

/*-
 * gnutls_x509_extract_certificate_dn:
 * @cert: should contain an X.509 DER encoded certificate
 * @ret: a pointer to a structure to hold the peer's name
 *
 * This function will return the name of the certificate holder. The name is gnutls_x509_dn structure and
 * is a obtained by the peer's certificate. If the certificate send by the
 * peer is invalid, or in any other failure this function returns error.
 * Returns a negative error code in case of an error.
 -*/
int
gnutls_x509_extract_certificate_dn (const gnutls_datum_t * cert,
                                    gnutls_x509_dn * ret)
{
  gnutls_x509_crt_t xcert;
  int result;
  size_t len;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  len = sizeof (ret->country);
  gnutls_x509_crt_get_dn_by_oid (xcert, GNUTLS_OID_X520_COUNTRY_NAME, 0,
                                 0, ret->country, &len);

  len = sizeof (ret->organization);
  gnutls_x509_crt_get_dn_by_oid (xcert, GNUTLS_OID_X520_ORGANIZATION_NAME,
                                 0, 0, ret->organization, &len);

  len = sizeof (ret->organizational_unit_name);
  gnutls_x509_crt_get_dn_by_oid (xcert,
                                 GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME,
                                 0, 0, ret->organizational_unit_name, &len);

  len = sizeof (ret->common_name);
  gnutls_x509_crt_get_dn_by_oid (xcert, GNUTLS_OID_X520_COMMON_NAME, 0, 0,
                                 ret->common_name, &len);

  len = sizeof (ret->locality_name);
  gnutls_x509_crt_get_dn_by_oid (xcert, GNUTLS_OID_X520_LOCALITY_NAME, 0,
                                 0, ret->locality_name, &len);

  len = sizeof (ret->state_or_province_name);
  gnutls_x509_crt_get_dn_by_oid (xcert,
                                 GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME,
                                 0, 0, ret->state_or_province_name, &len);

  len = sizeof (ret->email);
  gnutls_x509_crt_get_dn_by_oid (xcert, GNUTLS_OID_PKCS9_EMAIL, 0, 0,
                                 ret->email, &len);

  gnutls_x509_crt_deinit (xcert);

  return 0;
}

/*-
 * gnutls_x509_extract_certificate_issuer_dn:
 * @cert: should contain an X.509 DER encoded certificate
 * @ret: a pointer to a structure to hold the issuer's name
 *
 * This function will return the name of the issuer stated in the certificate. The name is a gnutls_x509_dn structure and
 * is a obtained by the peer's certificate. If the certificate send by the
 * peer is invalid, or in any other failure this function returns error.
 * Returns a negative error code in case of an error.
 -*/
int
gnutls_x509_extract_certificate_issuer_dn (const gnutls_datum_t * cert,
                                           gnutls_x509_dn * ret)
{
  gnutls_x509_crt_t xcert;
  int result;
  size_t len;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  len = sizeof (ret->country);
  gnutls_x509_crt_get_issuer_dn_by_oid (xcert,
                                        GNUTLS_OID_X520_COUNTRY_NAME, 0,
                                        0, ret->country, &len);

  len = sizeof (ret->organization);
  gnutls_x509_crt_get_issuer_dn_by_oid (xcert,
                                        GNUTLS_OID_X520_ORGANIZATION_NAME,
                                        0, 0, ret->organization, &len);

  len = sizeof (ret->organizational_unit_name);
  gnutls_x509_crt_get_issuer_dn_by_oid (xcert,
                                        GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME,
                                        0, 0,
                                        ret->organizational_unit_name, &len);

  len = sizeof (ret->common_name);
  gnutls_x509_crt_get_issuer_dn_by_oid (xcert,
                                        GNUTLS_OID_X520_COMMON_NAME, 0, 0,
                                        ret->common_name, &len);

  len = sizeof (ret->locality_name);
  gnutls_x509_crt_get_issuer_dn_by_oid (xcert,
                                        GNUTLS_OID_X520_LOCALITY_NAME, 0,
                                        0, ret->locality_name, &len);

  len = sizeof (ret->state_or_province_name);
  gnutls_x509_crt_get_issuer_dn_by_oid (xcert,
                                        GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME,
                                        0, 0, ret->state_or_province_name,
                                        &len);

  len = sizeof (ret->email);
  gnutls_x509_crt_get_issuer_dn_by_oid (xcert, GNUTLS_OID_PKCS9_EMAIL, 0,
                                        0, ret->email, &len);

  gnutls_x509_crt_deinit (xcert);

  return 0;
}


/*-
 * gnutls_x509_extract_certificate_subject_alt_name:
 * @cert: should contain an X.509 DER encoded certificate
 * @seq: specifies the sequence number of the alt name (0 for the first one, 1 for the second etc.)
 * @ret: is the place where the alternative name will be copied to
 * @ret_size: holds the size of ret.
 *
 * This function will return the alternative names, contained in the
 * given certificate.
 *
 * Returns GNUTLS_E_SHORT_MEMORY_BUFFER if ret_size is not enough to hold the alternative
 * name, or the type of alternative name if everything was ok. The type is
 * one of the enumerated GNUTLS_X509_SUBJECT_ALT_NAME.
 *
 * If the certificate does not have an Alternative name with the specified
 * sequence number then returns GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
 -*/
int
gnutls_x509_extract_certificate_subject_alt_name (const gnutls_datum_t *
                                                  cert, int seq,
                                                  char *ret, int *ret_size)
{
  gnutls_x509_crt_t xcert;
  int result;
  size_t size = *ret_size;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  result =
    gnutls_x509_crt_get_subject_alt_name (xcert, seq, ret, &size, NULL);
  *ret_size = size;

  gnutls_x509_crt_deinit (xcert);

  return result;
}

/*-
 * gnutls_x509_extract_certificate_ca_status:
 * @cert: should contain an X.509 DER encoded certificate
 *
 * This function will return certificates CA status, by reading the
 * basicConstraints X.509 extension. If the certificate is a CA a positive
 * value will be returned, or zero if the certificate does not have
 * CA flag set.
 *
 * A negative value may be returned in case of parsing error.
 * If the certificate does not contain the basicConstraints extension
 * GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE will be returned.
 -*/
int
gnutls_x509_extract_certificate_ca_status (const gnutls_datum_t * cert)
{
  gnutls_x509_crt_t xcert;
  int result;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  result = gnutls_x509_crt_get_ca_status (xcert, NULL);

  gnutls_x509_crt_deinit (xcert);

  return result;
}

/*-
 * gnutls_x509_extract_certificate_activation_time:
 * @cert: should contain an X.509 DER encoded certificate
 *
 * This function will return the certificate's activation time in UNIX time
 * (ie seconds since 00:00:00 UTC January 1, 1970).
 * Returns a (time_t) -1 in case of an error.
 -*/
time_t
gnutls_x509_extract_certificate_activation_time (const gnutls_datum_t * cert)
{
  gnutls_x509_crt_t xcert;
  time_t result;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  result = gnutls_x509_crt_get_activation_time (xcert);

  gnutls_x509_crt_deinit (xcert);

  return result;
}

/*-
 * gnutls_x509_extract_certificate_expiration_time:
 * @cert: should contain an X.509 DER encoded certificate
 *
 * This function will return the certificate's expiration time in UNIX time
 * (ie seconds since 00:00:00 UTC January 1, 1970).
 * Returns a (time_t) -1 in case of an error.
 -*/
time_t
gnutls_x509_extract_certificate_expiration_time (const gnutls_datum_t * cert)
{
  gnutls_x509_crt_t xcert;
  time_t result;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  result = gnutls_x509_crt_get_expiration_time (xcert);

  gnutls_x509_crt_deinit (xcert);

  return result;
}

/*-
 * gnutls_x509_extract_certificate_version:
 * @cert: is an X.509 DER encoded certificate
 *
 * This function will return the X.509 certificate's version (1, 2, 3). This is obtained by the X509 Certificate
 * Version field. Returns a negative value in case of an error.
 -*/
int
gnutls_x509_extract_certificate_version (const gnutls_datum_t * cert)
{
  gnutls_x509_crt_t xcert;
  int result;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  result = gnutls_x509_crt_get_version (xcert);

  gnutls_x509_crt_deinit (xcert);

  return result;

}

/*-
 * gnutls_x509_extract_certificate_serial:
 * @cert: is an X.509 DER encoded certificate
 * @result: The place where the serial number will be copied
 * @result_size: Holds the size of the result field.
 *
 * This function will return the X.509 certificate's serial number.
 * This is obtained by the X509 Certificate serialNumber
 * field. Serial is not always a 32 or 64bit number. Some CAs use
 * large serial numbers, thus it may be wise to handle it as something
 * opaque.
 *
 * Returns a negative value in case of an error.
 -*/
int
gnutls_x509_extract_certificate_serial (const gnutls_datum_t * cert,
                                        char *result, int *result_size)
{
  gnutls_x509_crt_t xcert;
  size_t size = *result_size;
  int ret;

  ret = gnutls_x509_crt_init (&xcert);
  if (ret < 0)
    return ret;

  ret = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (ret < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return ret;
    }

  ret = gnutls_x509_crt_get_serial (xcert, result, &size);
  *result_size = size;

  gnutls_x509_crt_deinit (xcert);

  return ret;
}


/*-
 * gnutls_x509_extract_certificate_pk_algorithm:
 * @cert: is a DER encoded X.509 certificate
 * @bits: if bits is non null it will hold the size of the parameters' in bits
 *
 * This function will return the public key algorithm of an X.509
 * certificate.
 *
 * If bits is non null, it should have enough size to hold the parameters
 * size in bits. For RSA the bits returned is the modulus.
 * For DSA the bits returned are of the public
 * exponent.
 *
 * Returns a member of the gnutls_pk_algorithm_t enumeration on success,
 * or a negative value on error.
 -*/
int
gnutls_x509_extract_certificate_pk_algorithm (const gnutls_datum_t *
                                              cert, int *bits)
{
  gnutls_x509_crt_t xcert;
  int result;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  result = gnutls_x509_crt_get_pk_algorithm (xcert, bits);

  gnutls_x509_crt_deinit (xcert);

  return result;
}


/*-
 * gnutls_x509_extract_certificate_dn_string:
 * @cert: should contain an X.509 DER encoded certificate
 * @buf: a pointer to a structure to hold the peer's name
 * @sizeof_buf: holds the size of 'buf'
 * @issuer: if non zero, then extract the name of the issuer, instead of the holder
 *
 * This function will copy the name of the certificate holder in the
 * provided buffer. The name will be in the form
 * "C=xxxx,O=yyyy,CN=zzzz" as described in RFC2253.
 *
 * Returns GNUTLS_E_SHORT_MEMORY_BUFFER if the provided buffer is not
 * long enough, and 0 on success.
 -*/
int
gnutls_x509_extract_certificate_dn_string (char *buf,
                                           unsigned int sizeof_buf,
                                           const gnutls_datum_t * cert,
                                           int issuer)
{
  gnutls_x509_crt_t xcert;
  int result;
  size_t size;

  result = gnutls_x509_crt_init (&xcert);
  if (result < 0)
    return result;

  result = gnutls_x509_crt_import (xcert, cert, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_x509_crt_deinit (xcert);
      return result;
    }

  size = sizeof_buf;
  if (!issuer)
    result = gnutls_x509_crt_get_dn (xcert, buf, &size);
  else
    result = gnutls_x509_crt_get_issuer_dn (xcert, buf, &size);

  gnutls_x509_crt_deinit (xcert);

  return result;
}

/*-
 * gnutls_x509_verify_certificate:
 * @cert_list: is the certificate list to be verified
 * @cert_list_length: holds the number of certificate in cert_list
 * @CA_list: is the CA list which will be used in verification
 * @CA_list_length: holds the number of CA certificate in CA_list
 * @CRL_list: not used
 * @CRL_list_length: not used
 *
 * This function will try to verify the given certificate list and
 * return its status (TRUSTED, EXPIRED etc.).  The return value
 * (status) should be one or more of the gnutls_certificate_status_t
 * enumerated elements bitwise or'd. Note that expiration and
 * activation dates are not checked by this function, you should
 * check them using the appropriate functions.
 *
 * This function understands the basicConstraints (2.5.29.19) PKIX
 * extension.  This means that only a certificate authority can sign
 * a certificate.
 *
 * However you must also check the peer's name in order to check if
 * the verified certificate belongs to the actual peer.
 *
 * The return value (status) should be one or more of the
 * gnutls_certificate_status_t enumerated elements bitwise or'd.
 *
 * GNUTLS_CERT_INVALID: the peer's certificate is not valid.
 *
 * GNUTLS_CERT_REVOKED: the certificate has been revoked.
 *
 * A negative error code is returned in case of an error.
 * GNUTLS_E_NO_CERTIFICATE_FOUND is returned to indicate that
 * no certificate was sent by the peer.
 -*/
int
gnutls_x509_verify_certificate (const gnutls_datum_t * cert_list,
                                int cert_list_length,
                                const gnutls_datum_t * CA_list,
                                int CA_list_length,
                                const gnutls_datum_t * CRL_list,
                                int CRL_list_length)
{
  unsigned int verify;
  gnutls_x509_crt_t *peer_certificate_list = NULL;
  gnutls_x509_crt_t *ca_certificate_list = NULL;
  gnutls_x509_crl_t *crl_list = NULL;
  int peer_certificate_list_size = 0, i, x, ret;
  int ca_certificate_list_size = 0, crl_list_size = 0;

  if (cert_list == NULL || cert_list_length == 0)
    return GNUTLS_E_NO_CERTIFICATE_FOUND;

  /* generate a list of gnutls_certs based on the auth info
   * raw certs.
   */
  peer_certificate_list_size = cert_list_length;
  peer_certificate_list =
    gnutls_calloc (peer_certificate_list_size, sizeof (gnutls_x509_crt_t));
  if (peer_certificate_list == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  ca_certificate_list_size = CA_list_length;
  ca_certificate_list =
    gnutls_calloc (ca_certificate_list_size, sizeof (gnutls_x509_crt_t));
  if (ca_certificate_list == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  /* allocate memory for CRL
   */
  crl_list_size = CRL_list_length;
  crl_list = gnutls_calloc (crl_list_size, sizeof (gnutls_x509_crl_t));
  if (crl_list == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  /* convert certA_list to gnutls_cert* list
   */
  for (i = 0; i < peer_certificate_list_size; i++)
    {
      ret = gnutls_x509_crt_init (&peer_certificate_list[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

      ret =
        gnutls_x509_crt_import (peer_certificate_list[i],
                                &cert_list[i], GNUTLS_X509_FMT_DER);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }

  /* convert CA_list to gnutls_x509_cert* list
   */
  for (i = 0; i < ca_certificate_list_size; i++)
    {
      ret = gnutls_x509_crt_init (&ca_certificate_list[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

      ret =
        gnutls_x509_crt_import (ca_certificate_list[i],
                                &CA_list[i], GNUTLS_X509_FMT_DER);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }

#ifdef ENABLE_PKI
  /* convert CRL_list to gnutls_x509_crl* list
   */
  for (i = 0; i < crl_list_size; i++)
    {
      ret = gnutls_x509_crl_init (&crl_list[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

      ret =
        gnutls_x509_crl_import (crl_list[i],
                                &CRL_list[i], GNUTLS_X509_FMT_DER);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }
#endif

  /* Verify certificate 
   */
  ret =
    gnutls_x509_crt_list_verify (peer_certificate_list,
                                 peer_certificate_list_size,
                                 ca_certificate_list,
                                 ca_certificate_list_size, crl_list,
                                 crl_list_size, 0, &verify);

  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = verify;

cleanup:

  if (peer_certificate_list != NULL)
    for (x = 0; x < peer_certificate_list_size; x++)
      {
        if (peer_certificate_list[x] != NULL)
          gnutls_x509_crt_deinit (peer_certificate_list[x]);
      }

  if (ca_certificate_list != NULL)
    for (x = 0; x < ca_certificate_list_size; x++)
      {
        if (ca_certificate_list[x] != NULL)
          gnutls_x509_crt_deinit (ca_certificate_list[x]);
      }
#ifdef ENABLE_PKI
  if (crl_list != NULL)
    for (x = 0; x < crl_list_size; x++)
      {
        if (crl_list[x] != NULL)
          gnutls_x509_crl_deinit (crl_list[x]);
      }

  gnutls_free (crl_list);
#endif

  gnutls_free (ca_certificate_list);
  gnutls_free (peer_certificate_list);

  return ret;
}

/*-
 * gnutls_x509_extract_key_pk_algorithm:
 * @cert: is a DER encoded private key
 *
 * This function will return the public key algorithm of a DER encoded private
 * key.
 *
 * Returns a member of the gnutls_pk_algorithm_t enumeration on success,
 * or GNUTLS_E_UNKNOWN_PK_ALGORITHM on error.
 -*/
int
gnutls_x509_extract_key_pk_algorithm (const gnutls_datum_t * key)
{
  gnutls_x509_privkey_t pkey;
  int ret, pk;

  ret = gnutls_x509_privkey_init (&pkey);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_x509_privkey_import (pkey, key, GNUTLS_X509_FMT_DER);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  pk = gnutls_x509_privkey_get_pk_algorithm (pkey);

  gnutls_x509_privkey_deinit (pkey);
  return pk;
}

#ifdef ENABLE_PKI

/*-
 * gnutls_x509_pkcs7_extract_certificate:
 * @pkcs7_struct: should contain a PKCS7 DER formatted structure
 * @indx: contains the index of the certificate to extract
 * @certificate: the contents of the certificate will be copied there
 * @certificate_size: should hold the size of the certificate
 *
 * This function will return a certificate of the PKCS7 or RFC2630
 * certificate set.  Returns 0 on success. If the provided buffer is
 * not long enough, then GNUTLS_E_SHORT_MEMORY_BUFFER is returned.
 *
 * After the last certificate has been read
 * GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE will be returned.
 -*/
int
gnutls_x509_pkcs7_extract_certificate (const gnutls_datum_t *
                                       pkcs7_struct, int indx,
                                       char *certificate,
                                       int *certificate_size)
{
  gnutls_pkcs7_t pkcs7;
  int result;
  size_t size = *certificate_size;

  result = gnutls_pkcs7_init (&pkcs7);
  if (result < 0)
    return result;

  result = gnutls_pkcs7_import (pkcs7, pkcs7_struct, GNUTLS_X509_FMT_DER);
  if (result < 0)
    {
      gnutls_pkcs7_deinit (pkcs7);
      return result;
    }

  result = gnutls_pkcs7_get_crt_raw (pkcs7, indx, certificate, &size);
  *certificate_size = size;

  gnutls_pkcs7_deinit (pkcs7);

  return result;
}

#endif
