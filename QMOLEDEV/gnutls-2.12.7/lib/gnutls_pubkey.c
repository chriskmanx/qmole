/*
 * GnuTLS PKCS#11 support
 * Copyright (C) 2010 Free Software Foundation
 * 
 * Author: Nikos Mavrogiannopoulos
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
*/

#include <gnutls_int.h>
#include <pakchois/pakchois.h>
#include <gnutls/pkcs11.h>
#include <stdio.h>
#include <string.h>
#include <gnutls_errors.h>
#include <gnutls_datum.h>
#include <pkcs11_int.h>
#include <gnutls/abstract.h>
#include <gnutls_pk.h>
#include <x509_int.h>
#include <openpgp/openpgp_int.h>
#include <pkcs11_int.h>
#include <gnutls_num.h>
#include <x509/common.h>
#include <x509_b64.h>
#include <abstract_int.h>

#define PK_PEM_HEADER "PUBLIC KEY"


struct gnutls_pubkey_st
{
  gnutls_pk_algorithm_t pk_algorithm;
  unsigned int bits;            /* an indication of the security parameter */

  /* the size of params depends on the public
   * key algorithm
   * RSA: [0] is modulus
   *      [1] is public exponent
   * DSA: [0] is p
   *      [1] is q
   *      [2] is g
   *      [3] is public key
   */
  bigint_t params[MAX_PUBLIC_PARAMS_SIZE];
  int params_size;              /* holds the size of MPI params */

  unsigned int key_usage;       /* bits from GNUTLS_KEY_* */
};

static int pubkey_to_bits(gnutls_pk_algorithm_t pk, bigint_t* params, int params_size)
{
  switch(pk) 
    {
      case GNUTLS_PK_RSA:
        return _gnutls_mpi_get_nbits(params[0]);
      case GNUTLS_PK_DSA:
        if (params_size < 3) return 0;
        return _gnutls_mpi_get_nbits(params[3]);
      default:
        return 0;
    }
}

/**
 * gnutls_pubkey_get_pk_algorithm:
 * @key: should contain a #gnutls_pubkey_t structure
 * @bits: If set will return the number of bits of the parameters (may be NULL)
 *
 * This function will return the public key algorithm of a public
 * key and if possible will return a number of bits that indicates
 * the security parameter of the key.
 *
 * Returns: a member of the #gnutls_pk_algorithm_t enumeration on
 *   success, or a negative value on error.
 **/
int
gnutls_pubkey_get_pk_algorithm (gnutls_pubkey_t key, unsigned int *bits)
{
  if (bits)
    *bits = key->bits;

  return key->pk_algorithm;
}

/**
 * gnutls_pubkey_get_key_usage:
 * @key: should contain a #gnutls_pubkey_t structure
 * @usage: If set will return the number of bits of the parameters (may be NULL)
 *
 * This function will return the key usage of the public key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pubkey_get_key_usage (gnutls_pubkey_t key, unsigned int *usage)
{
  if (usage)
    *usage = key->key_usage;

  return 0;
}

/**
 * gnutls_pubkey_init:
 * @key: The structure to be initialized
 *
 * This function will initialize an public key structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pubkey_init (gnutls_pubkey_t * key)
{
  *key = gnutls_calloc (1, sizeof (struct gnutls_pubkey_st));
  if (*key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;
}

/**
 * gnutls_pubkey_deinit:
 * @key: The structure to be deinitialized
 *
 * This function will deinitialize a public key structure.
 **/
void
gnutls_pubkey_deinit (gnutls_pubkey_t key)
{
int i;

  for (i = 0; i < key->params_size; i++)
    {
      _gnutls_mpi_release (&key->params[i]);
    }

  gnutls_free (key);
}

/**
 * gnutls_pubkey_import_x509:
 * @key: The public key
 * @crt: The certificate to be imported
 * @flags: should be zero
 *
 * This function will import the given public key to the abstract
 * #gnutls_pubkey_t structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pubkey_import_x509 (gnutls_pubkey_t key, gnutls_x509_crt_t crt,
                           unsigned int flags)
{
  int ret;

  key->pk_algorithm = gnutls_x509_crt_get_pk_algorithm (crt, &key->bits);

  ret = gnutls_x509_crt_get_key_usage (crt, &key->key_usage, NULL);
  if (ret < 0)
    key->key_usage = 0;

  key->params_size = sizeof (key->params) / sizeof (key->params[0]);
  switch (key->pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      ret = _gnutls_x509_crt_get_mpis (crt, key->params, &key->params_size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      break;
    case GNUTLS_PK_DSA:
      ret = _gnutls_x509_crt_get_mpis (crt, key->params, &key->params_size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return 0;
}

/**
 * gnutls_pubkey_import_privkey: Imports the public key from a private
 * @key: The public key
 * @pkey: The private key
 * @usage: GNUTLS_KEY_* key usage flags.
 * @flags: should be zero
 *
 * This function will import the given public key to the abstract
 * #gnutls_pubkey_t structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Since: 2.12.0
 **/
int
gnutls_pubkey_import_privkey (gnutls_pubkey_t key, gnutls_privkey_t pkey,
                              unsigned int usage, unsigned int flags)
{
  key->pk_algorithm = gnutls_privkey_get_pk_algorithm (pkey, &key->bits);

  key->key_usage = usage;

  key->params_size = sizeof (key->params) / sizeof (key->params[0]);

  return _gnutls_privkey_get_public_mpis (pkey, key->params,
                                          &key->params_size);
}

/**
 * gnutls_pubkey_get_preferred_hash_algorithm:
 * @key: Holds the certificate
 * @hash: The result of the call with the hash algorithm used for signature
 * @mand: If non zero it means that the algorithm MUST use this hash. May be NULL.
 *
 * This function will read the certifcate and return the appropriate digest
 * algorithm to use for signing with this certificate. Some certificates (i.e.
 * DSA might not be able to sign without the preferred algorithm).
 *
 * Returns: the 0 if the hash algorithm is found. A negative value is
 * returned on error.
 *
 * Since: 2.11.0
 **/
int
gnutls_pubkey_get_preferred_hash_algorithm (gnutls_pubkey_t key,
                                            gnutls_digest_algorithm_t *
                                            hash, unsigned int *mand)
{
  int ret;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = _gnutls_pk_get_hash_algorithm (key->pk_algorithm,
                                       key->params, key->params_size,
                                       hash, mand);

  return ret;
}


/**
 * gnutls_pubkey_import_pkcs11: Imports a public key from a pkcs11 key
 * @key: The public key
 * @obj: The parameters to be imported
 * @flags: should be zero
 *
 * This function will import the given public key to the abstract
 * #gnutls_pubkey_t structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pubkey_import_pkcs11 (gnutls_pubkey_t key,
                             gnutls_pkcs11_obj_t obj, unsigned int flags)
{
  int ret;

  ret = gnutls_pkcs11_obj_get_type (obj);
  if (ret != GNUTLS_PKCS11_OBJ_PUBKEY)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  key->key_usage = obj->key_usage;

  switch (obj->pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      ret = gnutls_pubkey_import_rsa_raw (key, &obj->pubkey[0],
                                          &obj->pubkey[1]);
      break;
    case GNUTLS_PK_DSA:
      ret = gnutls_pubkey_import_dsa_raw (key, &obj->pubkey[0],
                                          &obj->pubkey[1],
                                          &obj->pubkey[2], &obj->pubkey[3]);
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_UNIMPLEMENTED_FEATURE;
    }

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

#ifdef ENABLE_OPENPGP
/**
 * gnutls_pubkey_import_openpgp: Imports a public key from an openpgp key
 * @key: The public key
 * @crt: The certificate to be imported
 * @flags: should be zero
 *
 * This function will import the given public key to the abstract
 * #gnutls_pubkey_t structure. The subkey set as preferred will be
 * imported or the master key otherwise.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pubkey_import_openpgp (gnutls_pubkey_t key,
                              gnutls_openpgp_crt_t crt,
                              unsigned int flags)
{
  int ret, idx;
  uint32_t kid32[2];
  uint32_t *k;
  uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];

  ret = gnutls_openpgp_crt_get_preferred_key_id (crt, keyid);
  if (ret == GNUTLS_E_OPENPGP_PREFERRED_KEY_ERROR)
    {
      key->pk_algorithm = gnutls_openpgp_crt_get_pk_algorithm (crt, &key->bits);

      ret = gnutls_openpgp_crt_get_key_usage (crt, &key->key_usage);
      if (ret < 0)
        key->key_usage = 0;
      
      k = NULL;
    }
  else
    {
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

        KEYID_IMPORT (kid32, keyid);
        k = kid32;

        idx = gnutls_openpgp_crt_get_subkey_idx (crt, keyid);

        ret = gnutls_openpgp_crt_get_subkey_usage (crt, idx, &key->key_usage);
        if (ret < 0)
          key->key_usage = 0;

      key->pk_algorithm = gnutls_openpgp_crt_get_subkey_pk_algorithm (crt, idx, NULL);
    }

  switch (key->pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      ret =
        _gnutls_openpgp_crt_get_mpis (crt, k, key->params,
                                      &key->params_size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      break;
    case GNUTLS_PK_DSA:
      ret =
        _gnutls_openpgp_crt_get_mpis (crt, k, key->params,
                                      &key->params_size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return 0;
}

#endif

/**
 * gnutls_pubkey_export:
 * @key: Holds the certificate
 * @format: the format of output params. One of PEM or DER.
 * @output_data: will contain a certificate PEM or DER encoded
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will export the certificate to DER or PEM format.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *output_data_size is updated and GNUTLS_E_SHORT_MEMORY_BUFFER will
 * be returned.
 *
 * If the structure is PEM encoded, it will have a header
 * of "BEGIN CERTIFICATE".
 *
 * Return value: In case of failure a negative value will be
 *   returned, and 0 on success.
 **/
int
gnutls_pubkey_export (gnutls_pubkey_t key,
                      gnutls_x509_crt_fmt_t format, void *output_data,
                      size_t * output_data_size)
{
  int result;
  ASN1_TYPE spk = ASN1_TYPE_EMPTY;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.SubjectPublicKeyInfo", &spk))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result =
    _gnutls_x509_encode_and_copy_PKI_params (spk, "",
                                             key->pk_algorithm,
                                             key->params, key->params_size);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_export_int_named (spk, "",
                                          format, PK_PEM_HEADER,
                                          output_data, output_data_size);
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

/**
 * gnutls_pubkey_get_key_id:
 * @key: Holds the public key
 * @flags: should be 0 for now
 * @output_data: will contain the key ID
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will return a unique ID the depends on the public
 * key parameters. This ID can be used in checking whether a
 * certificate corresponds to the given public key.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *output_data_size is updated and GNUTLS_E_SHORT_MEMORY_BUFFER will
 * be returned.  The output will normally be a SHA-1 hash output,
 * which is 20 bytes.
 *
 * Return value: In case of failure a negative value will be
 *   returned, and 0 on success.
 **/
int
gnutls_pubkey_get_key_id (gnutls_pubkey_t key, unsigned int flags,
                          unsigned char *output_data,
                          size_t * output_data_size)
{
  int ret = 0;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret =
    _gnutls_get_key_id (key->pk_algorithm, key->params,
                        key->params_size, output_data, output_data_size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/**
 * gnutls_pubkey_get_pk_rsa_raw:
 * @key: Holds the certificate
 * @m: will hold the modulus
 * @e: will hold the public exponent
 *
 * This function will export the RSA public key's parameters found in
 * the given structure.  The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, otherwise an error.
 **/
int
gnutls_pubkey_get_pk_rsa_raw (gnutls_pubkey_t key,
                              gnutls_datum_t * m, gnutls_datum_t * e)
{
  int ret;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (key->pk_algorithm != GNUTLS_PK_RSA)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = _gnutls_mpi_dprint_lz (key->params[0], m);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_mpi_dprint_lz (key->params[1], e);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (m);
      return ret;
    }

  return 0;
}

/**
 * gnutls_pubkey_get_pk_dsa_raw:
 * @key: Holds the public key
 * @p: will hold the p
 * @q: will hold the q
 * @g: will hold the g
 * @y: will hold the y
 *
 * This function will export the DSA public key's parameters found in
 * the given certificate.  The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, otherwise an error.
 **/
int
gnutls_pubkey_get_pk_dsa_raw (gnutls_pubkey_t key,
                              gnutls_datum_t * p, gnutls_datum_t * q,
                              gnutls_datum_t * g, gnutls_datum_t * y)
{
  int ret;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (key->pk_algorithm != GNUTLS_PK_DSA)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* P */
  ret = _gnutls_mpi_dprint_lz (key->params[0], p);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* Q */
  ret = _gnutls_mpi_dprint_lz (key->params[1], q);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (p);
      return ret;
    }


  /* G */
  ret = _gnutls_mpi_dprint_lz (key->params[2], g);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (p);
      _gnutls_free_datum (q);
      return ret;
    }


  /* Y */
  ret = _gnutls_mpi_dprint_lz (key->params[3], y);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (p);
      _gnutls_free_datum (g);
      _gnutls_free_datum (q);
      return ret;
    }

  return 0;
}

/**
 * gnutls_pubkey_import:
 * @key: The structure to store the parsed public key. 
 * @data: The DER or PEM encoded certificate. 
 * @format: One of DER or PEM 
 * 
 * This function will convert the given DER or PEM encoded Public key 
 * to the native gnutls_pubkey_t format.The output will be stored * in @ key. 
 * If the Certificate is PEM encoded it should have a header of "PUBLIC KEY". 
 * 
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 * negative error value.
 **/
int
gnutls_pubkey_import (gnutls_pubkey_t key,
                      const gnutls_datum_t * data,
                      gnutls_x509_crt_fmt_t format)
{
  int result = 0, need_free = 0;
  gnutls_datum_t _data;
  ASN1_TYPE spk;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  _data.data = data->data;
  _data.size = data->size;

  /* If the Certificate is in PEM format then decode it
   */
  if (format == GNUTLS_X509_FMT_PEM)
    {
      opaque *out;

      /* Try the first header */
      result =
        _gnutls_fbase64_decode (PK_PEM_HEADER, data->data, data->size, &out);

      if (result <= 0)
        {
          if (result == 0)
            result = GNUTLS_E_INTERNAL_ERROR;
          gnutls_assert ();
          return result;
        }

      _data.data = out;
      _data.size = result;

      need_free = 1;
    }

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.SubjectPublicKeyInfo", &spk))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result = asn1_der_decoding (&spk, _data.data, _data.size, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  key->params_size = sizeof (key->params) / sizeof (key->params[0]);
  result = _gnutls_get_asn_mpis (spk, "", key->params, &key->params_size);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* this has already been called by get_asn_mpis() thus it cannot
   * fail.
   */
  key->pk_algorithm = _gnutls_x509_get_pk_algorithm (spk, "", NULL);
  key->bits = pubkey_to_bits(key->pk_algorithm, key->params, key->params_size);

  result = 0;

cleanup:
  asn1_delete_structure (&spk);

  if (need_free)
    _gnutls_free_datum (&_data);
  return result;
}

/**
 * gnutls_x509_crt_set_pubkey:
 * @crt: should contain a #gnutls_x509_crt_t structure
 * @key: holds a public key
 *
 * This function will set the public parameters from the given public
 * key to the request.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_crt_set_pubkey (gnutls_x509_crt_t crt, gnutls_pubkey_t key)
{
  int result;

  if (crt == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result = _gnutls_x509_encode_and_copy_PKI_params (crt->cert,
                                                    "tbsCertificate.subjectPublicKeyInfo",
                                                    key->pk_algorithm,
                                                    key->params,
                                                    key->params_size);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  if (key->key_usage)
    gnutls_x509_crt_set_key_usage (crt, key->key_usage);

  return 0;
}

/**
 * gnutls_x509_crq_set_pubkey:
 * @crq: should contain a #gnutls_x509_crq_t structure
 * @key: holds a public key
 *
 * This function will set the public parameters from the given public
 * key to the request.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_crq_set_pubkey (gnutls_x509_crq_t crq, gnutls_pubkey_t key)
{
  int result;

  if (crq == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result = _gnutls_x509_encode_and_copy_PKI_params
    (crq->crq,
     "certificationRequestInfo.subjectPKInfo",
     key->pk_algorithm, key->params, key->params_size);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  if (key->key_usage)
    gnutls_x509_crq_set_key_usage (crq, key->key_usage);

  return 0;
}

/**
 * gnutls_pubkey_set_key_usage:
 * @key: a certificate of type #gnutls_x509_crt_t
 * @usage: an ORed sequence of the GNUTLS_KEY_* elements.
 *
 * This function will set the key usage flags of the public key. This
 * is only useful if the key is to be exported to a certificate or
 * certificate request.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pubkey_set_key_usage (gnutls_pubkey_t key, unsigned int usage)
{
  key->key_usage = usage;

  return 0;
}

/**
 * gnutls_pubkey_import_pkcs11_url:
 * @key: A key of type #gnutls_pubkey_t
 * @url: A PKCS 11 url
 * @flags: One of GNUTLS_PKCS11_OBJ_* flags
 *
 * This function will import a PKCS 11 certificate to a #gnutls_pubkey_t
 * structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/

int
gnutls_pubkey_import_pkcs11_url (gnutls_pubkey_t key, const char *url,
                                 unsigned int flags)
{
  gnutls_pkcs11_obj_t pcrt;
  int ret;

  ret = gnutls_pkcs11_obj_init (&pcrt);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_pkcs11_obj_import_url (pcrt, url, flags);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = gnutls_pubkey_import_pkcs11 (key, pcrt, 0);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = 0;
cleanup:

  gnutls_pkcs11_obj_deinit (pcrt);

  return ret;
}

/**
 * gnutls_pubkey_import_rsa_raw:
 * @key: Is a structure will hold the parameters
 * @m: holds the modulus
 * @e: holds the public exponent
 *
 * This function will replace the parameters in the given structure.
 * The new parameters should be stored in the appropriate
 * gnutls_datum.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an negative error code.
 **/
int
gnutls_pubkey_import_rsa_raw (gnutls_pubkey_t key,
                              const gnutls_datum_t * m,
                              const gnutls_datum_t * e)
{
  size_t siz = 0;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  siz = m->size;
  if (_gnutls_mpi_scan_nz (&key->params[0], m->data, siz))
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = e->size;
  if (_gnutls_mpi_scan_nz (&key->params[1], e->data, siz))
    {
      gnutls_assert ();
      _gnutls_mpi_release (&key->params[0]);
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  key->params_size = RSA_PUBLIC_PARAMS;
  key->pk_algorithm = GNUTLS_PK_RSA;
  key->bits = pubkey_to_bits(GNUTLS_PK_RSA, key->params, key->params_size);

  return 0;
}

/**
 * gnutls_pubkey_import_dsa_raw:
 * @key: The structure to store the parsed key
 * @p: holds the p
 * @q: holds the q
 * @g: holds the g
 * @y: holds the y
 *
 * This function will convert the given DSA raw parameters to the
 * native #gnutls_pubkey_t format.  The output will be stored
 * in @key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pubkey_import_dsa_raw (gnutls_pubkey_t key,
                              const gnutls_datum_t * p,
                              const gnutls_datum_t * q,
                              const gnutls_datum_t * g,
                              const gnutls_datum_t * y)
{
  size_t siz = 0;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  siz = p->size;
  if (_gnutls_mpi_scan_nz (&key->params[0], p->data, siz))
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = q->size;
  if (_gnutls_mpi_scan_nz (&key->params[1], q->data, siz))
    {
      gnutls_assert ();
      _gnutls_mpi_release (&key->params[0]);
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = g->size;
  if (_gnutls_mpi_scan_nz (&key->params[2], g->data, siz))
    {
      gnutls_assert ();
      _gnutls_mpi_release (&key->params[1]);
      _gnutls_mpi_release (&key->params[0]);
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = y->size;
  if (_gnutls_mpi_scan_nz (&key->params[3], y->data, siz))
    {
      gnutls_assert ();
      _gnutls_mpi_release (&key->params[2]);
      _gnutls_mpi_release (&key->params[1]);
      _gnutls_mpi_release (&key->params[0]);
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  key->params_size = DSA_PUBLIC_PARAMS;
  key->pk_algorithm = GNUTLS_PK_DSA;
  key->bits = pubkey_to_bits(GNUTLS_PK_DSA, key->params, key->params_size);

  return 0;

}

/**
 * gnutls_pubkey_verify_data:
 * @pubkey: Holds the public key
 * @flags: should be 0 for now
 * @data: holds the data to be signed
 * @signature: contains the signature
 *
 * This function will verify the given signed data, using the
 * parameters from the certificate.
 *
 * Returns: In case of a verification failure
 *   %GNUTLS_E_PK_SIG_VERIFY_FAILED is returned, and a positive code
 *   on success.
 *
 * Since: 2.12.0
 **/
int
gnutls_pubkey_verify_data (gnutls_pubkey_t pubkey, unsigned int flags,
			   const gnutls_datum_t * data,
			   const gnutls_datum_t * signature)
{
  int ret;

  if (pubkey == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = pubkey_verify_sig( data, NULL, signature, pubkey->pk_algorithm, 
    pubkey->params, pubkey->params_size);
  if (ret < 0)
    {
      gnutls_assert();
    }

  return ret;
}


/**
 * gnutls_pubkey_verify_hash:
 * @key: Holds the certificate
 * @flags: should be 0 for now
 * @hash: holds the hash digest to be verified
 * @signature: contains the signature
 *
 * This function will verify the given signed digest, using the
 * parameters from the certificate.
 *
 * Returns: In case of a verification failure %GNUTLS_E_PK_SIG_VERIFY_FAILED 
 * is returned, and a positive code on success.
 **/
int
gnutls_pubkey_verify_hash (gnutls_pubkey_t key, unsigned int flags,
                           const gnutls_datum_t * hash,
                           const gnutls_datum_t * signature)
{
  int ret;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret =
    pubkey_verify_sig (NULL, hash, signature, key->pk_algorithm,
                       key->params, key->params_size);

  return ret;
}

/**
 * gnutls_pubkey_get_verify_algorithm:
 * @key: Holds the certificate
 * @signature: contains the signature
 * @hash: The result of the call with the hash algorithm used for signature
 *
 * This function will read the certifcate and the signed data to
 * determine the hash algorithm used to generate the signature.
 *
 * Returns: the 0 if the hash algorithm is found. A negative value is
 * returned on error.
 **/
int
gnutls_pubkey_get_verify_algorithm (gnutls_pubkey_t key,
                                    const gnutls_datum_t * signature,
                                    gnutls_digest_algorithm_t * hash)
{
  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return _gnutls_x509_verify_algorithm ((gnutls_mac_algorithm_t *)
                                        hash, signature,
                                        key->pk_algorithm,
                                        key->params, key->params_size);

}
