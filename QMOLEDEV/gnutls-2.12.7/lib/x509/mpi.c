/*
 * Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010 Free Software
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

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_global.h>
#include <libtasn1.h>
#include <gnutls_datum.h>
#include "common.h"
#include "x509_int.h"
#include <gnutls_num.h>

/*
 * some x509 certificate parsing functions that relate to MPI parameter
 * extraction. This reads the BIT STRING subjectPublicKey.
 * Returns 2 parameters (m,e).
 */
int
_gnutls_x509_read_rsa_params (opaque * der, int dersize, bigint_t * params)
{
  int result;
  ASN1_TYPE spk = ASN1_TYPE_EMPTY;

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.RSAPublicKey", &spk))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&spk, der, dersize, NULL);

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      return _gnutls_asn2err (result);
    }


  if ((result = _gnutls_x509_read_int (spk, "modulus", &params[0])) < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      return GNUTLS_E_ASN1_GENERIC_ERROR;
    }

  if ((result = _gnutls_x509_read_int (spk, "publicExponent",
                                       &params[1])) < 0)
    {
      gnutls_assert ();
      _gnutls_mpi_release (&params[0]);
      asn1_delete_structure (&spk);
      return GNUTLS_E_ASN1_GENERIC_ERROR;
    }

  asn1_delete_structure (&spk);

  return 0;

}


/* reads p,q and g 
 * from the certificate (subjectPublicKey BIT STRING).
 * params[0-2]
 */
int
_gnutls_x509_read_dsa_params (opaque * der, int dersize, bigint_t * params)
{
  int result;
  ASN1_TYPE spk = ASN1_TYPE_EMPTY;

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.Dss-Parms", &spk)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&spk, der, dersize, NULL);

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      return _gnutls_asn2err (result);
    }

  /* FIXME: If the parameters are not included in the certificate
   * then the issuer's parameters should be used. This is not
   * done yet.
   */

  /* Read p */

  if ((result = _gnutls_x509_read_int (spk, "p", &params[0])) < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      return GNUTLS_E_ASN1_GENERIC_ERROR;
    }

  /* Read q */

  if ((result = _gnutls_x509_read_int (spk, "q", &params[1])) < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      _gnutls_mpi_release (&params[0]);
      return GNUTLS_E_ASN1_GENERIC_ERROR;
    }

  /* Read g */

  if ((result = _gnutls_x509_read_int (spk, "g", &params[2])) < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      _gnutls_mpi_release (&params[0]);
      _gnutls_mpi_release (&params[1]);
      return GNUTLS_E_ASN1_GENERIC_ERROR;
    }

  asn1_delete_structure (&spk);

  return 0;

}

/* Reads an Integer from the DER encoded data
 */

int
_gnutls_x509_read_der_int (opaque * der, int dersize, bigint_t * out)
{
  int result;
  ASN1_TYPE spk = ASN1_TYPE_EMPTY;

  /* == INTEGER */
  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.DSAPublicKey",
        &spk)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&spk, der, dersize, NULL);

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      return _gnutls_asn2err (result);
    }

  /* Read Y */

  if ((result = _gnutls_x509_read_int (spk, "", out)) < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&spk);
      return _gnutls_asn2err (result);
    }

  asn1_delete_structure (&spk);

  return 0;

}

/* reads DSA's Y
 * from the certificate 
 * only sets params[3]
 */
int
_gnutls_x509_read_dsa_pubkey (opaque * der, int dersize, bigint_t * params)
{
  return _gnutls_x509_read_der_int (der, dersize, &params[3]);
}


/* Extracts DSA and RSA parameters from a certificate.
 */
int
_gnutls_get_asn_mpis (ASN1_TYPE asn, const char *root,
                      bigint_t * params, int *params_size)
{
  int result;
  char name[256];
  gnutls_datum_t tmp = { NULL, 0 };
  gnutls_pk_algorithm pk_algorithm;

  result = _gnutls_x509_get_pk_algorithm (asn, root, NULL);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  pk_algorithm = result;

  /* Read the algorithm's parameters
   */
  _asnstr_append_name (name, sizeof (name), root, ".subjectPublicKey");
  result = _gnutls_x509_read_value (asn, name, &tmp, 2);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  switch (pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      /* params[0] is the modulus,
       * params[1] is the exponent
       */
      if (*params_size < RSA_PUBLIC_PARAMS)
        {
          gnutls_assert ();
          /* internal error. Increase the bigint_ts in params */
          result = GNUTLS_E_INTERNAL_ERROR;
          goto error;
        }

      if ((result =
           _gnutls_x509_read_rsa_params (tmp.data, tmp.size, params)) < 0)
        {
          gnutls_assert ();
          goto error;
        }
      *params_size = RSA_PUBLIC_PARAMS;

      break;
    case GNUTLS_PK_DSA:
      /* params[0] is p,
       * params[1] is q,
       * params[2] is q,
       * params[3] is pub.
       */

      if (*params_size < DSA_PUBLIC_PARAMS)
        {
          gnutls_assert ();
          /* internal error. Increase the bigint_ts in params */
          result = GNUTLS_E_INTERNAL_ERROR;
          goto error;
        }

      if ((result =
           _gnutls_x509_read_dsa_pubkey (tmp.data, tmp.size, params)) < 0)
        {
          gnutls_assert ();
          goto error;
        }

      /* Now read the parameters
       */
      _gnutls_free_datum (&tmp);

      _asnstr_append_name (name, sizeof (name), root,
                           ".algorithm.parameters");
      result = _gnutls_x509_read_value (asn, name, &tmp, 0);

      /* FIXME: If the parameters are not included in the certificate
       * then the issuer's parameters should be used. This is not
       * done yet.
       */

      if (result < 0)
        {
          gnutls_assert ();
          goto error;
        }

      if ((result =
           _gnutls_x509_read_dsa_params (tmp.data, tmp.size, params)) < 0)
        {
          gnutls_assert ();
          goto error;
        }
      *params_size = DSA_PUBLIC_PARAMS;

      break;

    default:
      /* other types like DH
       * currently not supported
       */
      gnutls_assert ();
      result = GNUTLS_E_X509_CERTIFICATE_ERROR;
      goto error;
    }

  result = 0;

error:
  _gnutls_free_datum (&tmp);
  return result;
}

/* Extracts DSA and RSA parameters from a certificate.
 */
int
_gnutls_x509_crt_get_mpis (gnutls_x509_crt_t cert,
                           bigint_t * params, int *params_size)
{
  /* Read the algorithm's OID
   */
  return _gnutls_get_asn_mpis (cert->cert,
                               "tbsCertificate.subjectPublicKeyInfo", params,
                               params_size);
}

#ifdef ENABLE_PKI

/* Extracts DSA and RSA parameters from a certificate.
 */
int
_gnutls_x509_crq_get_mpis (gnutls_x509_crq_t cert,
                           bigint_t * params, int *params_size)
{
  /* Read the algorithm's OID
   */
  return _gnutls_get_asn_mpis (cert->crq,
                               "certificationRequestInfo.subjectPKInfo",
                               params, params_size);
}

#endif

/*
 * some x509 certificate functions that relate to MPI parameter
 * setting. This writes the BIT STRING subjectPublicKey.
 * Needs 2 parameters (m,e).
 *
 * Allocates the space used to store the DER data.
 */
int
_gnutls_x509_write_rsa_params (bigint_t * params, int params_size,
                               gnutls_datum_t * der)
{
  int result;
  ASN1_TYPE spk = ASN1_TYPE_EMPTY;

  der->data = NULL;
  der->size = 0;

  if (params_size < 2)
    {
      gnutls_assert ();
      result = GNUTLS_E_INVALID_REQUEST;
      goto cleanup;
    }

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.RSAPublicKey", &spk))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_write_int (spk, "modulus", params[0], 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_write_int (spk, "publicExponent", params[1], 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_der_encode (spk, "", der, 0);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  asn1_delete_structure (&spk);
  return 0;

cleanup:
  asn1_delete_structure (&spk);

  return result;
}

/*
 * This function writes and encodes the parameters for DSS or RSA keys.
 * This is the "signatureAlgorithm" fields.
 */
int
_gnutls_x509_write_sig_params (ASN1_TYPE dst, const char *dst_name,
                               gnutls_pk_algorithm_t pk_algorithm,
                               gnutls_digest_algorithm_t dig)
{
  int result;
  char name[128];
  const char *pk;

  _gnutls_str_cpy (name, sizeof (name), dst_name);
  _gnutls_str_cat (name, sizeof (name), ".algorithm");

  pk = _gnutls_x509_sign_to_oid (pk_algorithm, HASH2MAC (dig));
  if (pk == NULL)
    {
      gnutls_assert ();
      _gnutls_debug_log
        ("Cannot find OID for sign algorithm pk: %d dig: %d\n",
         (int) pk_algorithm, (int) dig);
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* write the OID.
   */
  result = asn1_write_value (dst, name, pk, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }


  _gnutls_str_cpy (name, sizeof (name), dst_name);
  _gnutls_str_cat (name, sizeof (name), ".parameters");

  if (pk_algorithm == GNUTLS_PK_RSA)
    result = asn1_write_value (dst, name, ASN1_NULL, ASN1_NULL_SIZE);
  else
    result = asn1_write_value (dst, name, NULL, 0);

  if (result != ASN1_SUCCESS && result != ASN1_ELEMENT_NOT_FOUND)
    {
      /* Here we ignore the element not found error, since this
       * may have been disabled before.
       */
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  return 0;
}

/*
 * This function writes the parameters for DSS keys.
 * Needs 3 parameters (p,q,g).
 *
 * Allocates the space used to store the DER data.
 */
int
_gnutls_x509_write_dsa_params (bigint_t * params, int params_size,
                               gnutls_datum_t * der)
{
  int result;
  ASN1_TYPE spk = ASN1_TYPE_EMPTY;

  der->data = NULL;
  der->size = 0;

  if (params_size < 3)
    {
      gnutls_assert ();
      result = GNUTLS_E_INVALID_REQUEST;
      goto cleanup;
    }

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.DSAParameters", &spk))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_write_int (spk, "p", params[0], 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_write_int (spk, "q", params[1], 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_write_int (spk, "g", params[2], 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_der_encode (spk, "", der, 0);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = 0;

cleanup:
  asn1_delete_structure (&spk);
  return result;
}

/*
 * This function writes the public parameters for DSS keys.
 * Needs 1 parameter (y).
 *
 * Allocates the space used to store the DER data.
 */
int
_gnutls_x509_write_dsa_public_key (bigint_t * params, int params_size,
                                   gnutls_datum_t * der)
{
  int result;
  ASN1_TYPE spk = ASN1_TYPE_EMPTY;

  der->data = NULL;
  der->size = 0;

  if (params_size < 3)
    {
      gnutls_assert ();
      result = GNUTLS_E_INVALID_REQUEST;
      goto cleanup;
    }

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.DSAPublicKey", &spk))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_write_int (spk, "", params[3], 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_der_encode (spk, "", der, 0);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = 0;

cleanup:
  asn1_delete_structure (&spk);
  return result;
}


/* this function reads a (small) unsigned integer
 * from asn1 structs. Combines the read and the convertion
 * steps.
 */
int
_gnutls_x509_read_uint (ASN1_TYPE node, const char *value, unsigned int *ret)
{
  int len, result;
  opaque *tmpstr;

  len = 0;
  result = asn1_read_value (node, value, NULL, &len);
  if (result != ASN1_MEM_ERROR)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  tmpstr = gnutls_malloc (len);
  if (tmpstr == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  result = asn1_read_value (node, value, tmpstr, &len);

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      gnutls_free (tmpstr);
      return _gnutls_asn2err (result);
    }

  if (len == 1)
    *ret = tmpstr[0];
  else if (len == 2)
    *ret = _gnutls_read_uint16 (tmpstr);
  else if (len == 3)
    *ret = _gnutls_read_uint24 (tmpstr);
  else if (len == 4)
    *ret = _gnutls_read_uint32 (tmpstr);
  else
    {
      gnutls_assert ();
      gnutls_free (tmpstr);
      return GNUTLS_E_INTERNAL_ERROR;
    }

  gnutls_free (tmpstr);

  return 0;
}

/* Writes the specified integer into the specified node.
 */
int
_gnutls_x509_write_uint32 (ASN1_TYPE node, const char *value, uint32_t num)
{
  opaque tmpstr[4];
  int result;

  _gnutls_write_uint32 (num, tmpstr);

  result = asn1_write_value (node, value, tmpstr, 4);

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  return 0;
}
