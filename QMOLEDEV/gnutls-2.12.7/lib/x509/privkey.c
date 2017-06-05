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
#include <gnutls_datum.h>
#include <gnutls_global.h>
#include <gnutls_errors.h>
#include <gnutls_rsa_export.h>
#include <gnutls_sig.h>
#include <common.h>
#include <gnutls_x509.h>
#include <x509_b64.h>
#include <x509_int.h>
#include <gnutls_pk.h>
#include <gnutls_mpi.h>

static int _gnutls_asn1_encode_rsa (ASN1_TYPE * c2, bigint_t * params);

/**
 * gnutls_x509_privkey_init:
 * @key: The structure to be initialized
 *
 * This function will initialize an private key structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_init (gnutls_x509_privkey_t * key)
{
  *key = gnutls_calloc (1, sizeof (gnutls_x509_privkey_int));

  if (*key)
    {
      (*key)->key = ASN1_TYPE_EMPTY;
      (*key)->pk_algorithm = GNUTLS_PK_UNKNOWN;
      return 0;                 /* success */
    }

  return GNUTLS_E_MEMORY_ERROR;
}

/**
 * gnutls_x509_privkey_deinit:
 * @key: The structure to be deinitialized
 *
 * This function will deinitialize a private key structure.
 **/
void
gnutls_x509_privkey_deinit (gnutls_x509_privkey_t key)
{
  int i;

  if (!key)
    return;

  for (i = 0; i < key->params_size; i++)
    {
      _gnutls_mpi_release (&key->params[i]);
    }

  asn1_delete_structure (&key->key);
  gnutls_free (key);
}

/**
 * gnutls_x509_privkey_cpy:
 * @dst: The destination key, which should be initialized.
 * @src: The source key
 *
 * This function will copy a private key from source to destination
 * key. Destination has to be initialized.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_cpy (gnutls_x509_privkey_t dst, gnutls_x509_privkey_t src)
{
  int i, ret;

  if (!src || !dst)
    return GNUTLS_E_INVALID_REQUEST;

  for (i = 0; i < src->params_size; i++)
    {
      dst->params[i] = _gnutls_mpi_copy (src->params[i]);
      if (dst->params[i] == NULL)
        return GNUTLS_E_MEMORY_ERROR;
    }

  dst->params_size = src->params_size;
  dst->pk_algorithm = src->pk_algorithm;
  dst->crippled = src->crippled;

  if (!src->crippled)
    {
      switch (dst->pk_algorithm)
        {
        case GNUTLS_PK_DSA:
          ret = _gnutls_asn1_encode_dsa (&dst->key, dst->params);
          if (ret < 0)
            {
              gnutls_assert ();
              return ret;
            }
          break;
        case GNUTLS_PK_RSA:
          ret = _gnutls_asn1_encode_rsa (&dst->key, dst->params);
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
    }

  return 0;
}

/* Converts an RSA PKCS#1 key to
 * an internal structure (gnutls_private_key)
 */
ASN1_TYPE
_gnutls_privkey_decode_pkcs1_rsa_key (const gnutls_datum_t * raw_key,
                                      gnutls_x509_privkey_t pkey)
{
  int result;
  ASN1_TYPE pkey_asn;
  gnutls_pk_params_st pk_params;

  memset (&pk_params, 0, sizeof (pk_params));
  pk_params.params_nr = RSA_PRIVATE_PARAMS;

  if ((result =
       asn1_create_element (_gnutls_get_gnutls_asn (),
                            "GNUTLS.RSAPrivateKey",
                            &pkey_asn)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return NULL;
    }

  result = asn1_der_decoding (&pkey_asn, raw_key->data, raw_key->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (pkey_asn, "modulus",
                                       &pk_params.params[0])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result =
       _gnutls_x509_read_int (pkey_asn, "publicExponent",
                              &pk_params.params[1])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result =
       _gnutls_x509_read_int (pkey_asn, "privateExponent",
                              &pk_params.params[2])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (pkey_asn, "prime1",
                                       &pk_params.params[3])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (pkey_asn, "prime2",
                                       &pk_params.params[4])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (pkey_asn, "coefficient",
                                       &pk_params.params[5])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (pkey_asn, "exponent1",
                                       &pk_params.params[6])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (pkey_asn, "exponent2",
                                       &pk_params.params[7])) < 0)
    {
      gnutls_assert ();
      goto error;
    }


  result = _gnutls_pk_fixup (GNUTLS_PK_RSA, GNUTLS_IMPORT, &pk_params);
  if (result < 0)
    {
      gnutls_assert ();
      goto error;
    }

  pkey->params[0] = pk_params.params[0];
  pkey->params[1] = pk_params.params[1];
  pkey->params[2] = pk_params.params[2];
  pkey->params[3] = pk_params.params[3];
  pkey->params[4] = pk_params.params[4];
  pkey->params[5] = pk_params.params[5];
  pkey->params[6] = pk_params.params[6];
  pkey->params[7] = pk_params.params[7];
  pkey->params_size = pk_params.params_nr;

  return pkey_asn;

error:
  asn1_delete_structure (&pkey_asn);
  gnutls_pk_params_release (&pk_params);
  return NULL;

}

static ASN1_TYPE
decode_dsa_key (const gnutls_datum_t * raw_key, gnutls_x509_privkey_t pkey)
{
  int result;
  ASN1_TYPE dsa_asn;

  if ((result =
       asn1_create_element (_gnutls_get_gnutls_asn (),
                            "GNUTLS.DSAPrivateKey",
                            &dsa_asn)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return NULL;
    }

  result = asn1_der_decoding (&dsa_asn, raw_key->data, raw_key->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (dsa_asn, "p", &pkey->params[0])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (dsa_asn, "q", &pkey->params[1])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (dsa_asn, "g", &pkey->params[2])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (dsa_asn, "Y", &pkey->params[3])) < 0)
    {
      gnutls_assert ();
      goto error;
    }

  if ((result = _gnutls_x509_read_int (dsa_asn, "priv",
                                       &pkey->params[4])) < 0)
    {
      gnutls_assert ();
      goto error;
    }
  pkey->params_size = 5;

  return dsa_asn;

error:
  asn1_delete_structure (&dsa_asn);
  _gnutls_mpi_release (&pkey->params[0]);
  _gnutls_mpi_release (&pkey->params[1]);
  _gnutls_mpi_release (&pkey->params[2]);
  _gnutls_mpi_release (&pkey->params[3]);
  _gnutls_mpi_release (&pkey->params[4]);
  return NULL;

}


#define PEM_KEY_DSA "DSA PRIVATE KEY"
#define PEM_KEY_RSA "RSA PRIVATE KEY"

/**
 * gnutls_x509_privkey_import:
 * @key: The structure to store the parsed key
 * @data: The DER or PEM encoded certificate.
 * @format: One of DER or PEM
 *
 * This function will convert the given DER or PEM encoded key to the
 * native #gnutls_x509_privkey_t format. The output will be stored in
 * @key .
 *
 * If the key is PEM encoded it should have a header of "RSA PRIVATE
 * KEY", or "DSA PRIVATE KEY".
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_import (gnutls_x509_privkey_t key,
                            const gnutls_datum_t * data,
                            gnutls_x509_crt_fmt_t format)
{
  int result = 0, need_free = 0;
  gnutls_datum_t _data;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  _data.data = data->data;
  _data.size = data->size;

  key->pk_algorithm = GNUTLS_PK_UNKNOWN;

  /* If the Certificate is in PEM format then decode it
   */
  if (format == GNUTLS_X509_FMT_PEM)
    {
      opaque *out;

      /* Try the first header */
      result =
        _gnutls_fbase64_decode (PEM_KEY_RSA, data->data, data->size, &out);

      if (result >= 0)
        key->pk_algorithm = GNUTLS_PK_RSA;

      if (result == GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR)
        {
          /* try for the second header */
          result =
            _gnutls_fbase64_decode (PEM_KEY_DSA, data->data, data->size,
                                    &out);
          if (result <= 0)
            {
              if (result == 0)
                result = GNUTLS_E_INTERNAL_ERROR;
              gnutls_assert ();

              goto failover;
            }

          key->pk_algorithm = GNUTLS_PK_DSA;
        }

      _data.data = out;
      _data.size = result;

      need_free = 1;
    }

  if (key->pk_algorithm == GNUTLS_PK_RSA)
    {
      key->key = _gnutls_privkey_decode_pkcs1_rsa_key (&_data, key);
      if (key->key == NULL)
        gnutls_assert ();
    }
  else if (key->pk_algorithm == GNUTLS_PK_DSA)
    {
      key->key = decode_dsa_key (&_data, key);
      if (key->key == NULL)
        gnutls_assert ();
    }
  else
    {
      /* Try decoding with both, and accept the one that
       * succeeds.
       */
      key->pk_algorithm = GNUTLS_PK_RSA;
      key->key = _gnutls_privkey_decode_pkcs1_rsa_key (&_data, key);

      if (key->key == NULL)
        {
          key->pk_algorithm = GNUTLS_PK_DSA;
          key->key = decode_dsa_key (&_data, key);
          if (key->key == NULL)
            gnutls_assert ();
        }
    }

  if (key->key == NULL)
    {
      gnutls_assert ();
      result = GNUTLS_E_ASN1_DER_ERROR;
      goto failover;
    }

  if (need_free)
    _gnutls_free_datum (&_data);

  /* The key has now been decoded.
   */

  return 0;

failover:
  /* Try PKCS #8 */
#ifdef ENABLE_PKI
  if (result == GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR)
    {
      _gnutls_debug_log ("Falling back to PKCS #8 key decoding\n");
      result = gnutls_x509_privkey_import_pkcs8 (key, data, format,
                                                 NULL, GNUTLS_PKCS_PLAIN);
    }
#endif

  if (need_free)
    _gnutls_free_datum (&_data);

  return result;
}

#define FREE_RSA_PRIVATE_PARAMS for (i=0;i<RSA_PRIVATE_PARAMS;i++) \
		_gnutls_mpi_release(&key->params[i])
#define FREE_DSA_PRIVATE_PARAMS for (i=0;i<DSA_PRIVATE_PARAMS;i++) \
		_gnutls_mpi_release(&key->params[i])

/**
 * gnutls_x509_privkey_import_rsa_raw:
 * @key: The structure to store the parsed key
 * @m: holds the modulus
 * @e: holds the public exponent
 * @d: holds the private exponent
 * @p: holds the first prime (p)
 * @q: holds the second prime (q)
 * @u: holds the coefficient
 *
 * This function will convert the given RSA raw parameters to the
 * native #gnutls_x509_privkey_t format.  The output will be stored in
 * @key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_import_rsa_raw (gnutls_x509_privkey_t key,
                                    const gnutls_datum_t * m,
                                    const gnutls_datum_t * e,
                                    const gnutls_datum_t * d,
                                    const gnutls_datum_t * p,
                                    const gnutls_datum_t * q,
                                    const gnutls_datum_t * u)
{
  return gnutls_x509_privkey_import_rsa_raw2 (key, m, e, d, p, q, u, NULL,
                                              NULL);
}

/**
 * gnutls_x509_privkey_import_rsa_raw2:
 * @key: The structure to store the parsed key
 * @m: holds the modulus
 * @e: holds the public exponent
 * @d: holds the private exponent
 * @p: holds the first prime (p)
 * @q: holds the second prime (q)
 * @u: holds the coefficient
 * @e1: holds e1 = d mod (p-1)
 * @e2: holds e2 = d mod (q-1)
 *
 * This function will convert the given RSA raw parameters to the
 * native #gnutls_x509_privkey_t format.  The output will be stored in
 * @key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_import_rsa_raw2 (gnutls_x509_privkey_t key,
                                     const gnutls_datum_t * m,
                                     const gnutls_datum_t * e,
                                     const gnutls_datum_t * d,
                                     const gnutls_datum_t * p,
                                     const gnutls_datum_t * q,
                                     const gnutls_datum_t * u,
                                     const gnutls_datum_t * e1,
                                     const gnutls_datum_t * e2)
{
  int i = 0, ret;
  size_t siz = 0;
  gnutls_pk_params_st pk_params;

  memset (&pk_params, 0, sizeof (pk_params));

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  key->params_size = 0;

  siz = m->size;
  if (_gnutls_mpi_scan_nz (&key->params[0], m->data, siz))
    {
      gnutls_assert ();
      FREE_RSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }
  key->params_size++;

  siz = e->size;
  if (_gnutls_mpi_scan_nz (&key->params[1], e->data, siz))
    {
      gnutls_assert ();
      FREE_RSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }
  key->params_size++;

  siz = d->size;
  if (_gnutls_mpi_scan_nz (&key->params[2], d->data, siz))
    {
      gnutls_assert ();
      FREE_RSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }
  key->params_size++;

  siz = p->size;
  if (_gnutls_mpi_scan_nz (&key->params[3], p->data, siz))
    {
      gnutls_assert ();
      FREE_RSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }
  key->params_size++;

  siz = q->size;
  if (_gnutls_mpi_scan_nz (&key->params[4], q->data, siz))
    {
      gnutls_assert ();
      FREE_RSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }
  key->params_size++;

  siz = u->size;
  if (_gnutls_mpi_scan_nz (&key->params[5], u->data, siz))
    {
      gnutls_assert ();
      FREE_RSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }
  key->params_size++;

  if (e1 && e2)
    {
      siz = e1->size;
      if (_gnutls_mpi_scan_nz (&key->params[6], e1->data, siz))
        {
          gnutls_assert ();
          FREE_RSA_PRIVATE_PARAMS;
          return GNUTLS_E_MPI_SCAN_FAILED;
        }
      key->params_size++;

      siz = e2->size;
      if (_gnutls_mpi_scan_nz (&key->params[7], e2->data, siz))
        {
          gnutls_assert ();
          FREE_RSA_PRIVATE_PARAMS;
          return GNUTLS_E_MPI_SCAN_FAILED;
        }
      key->params_size++;
    }

  for (i = 0; i < key->params_size; i++)
    {
      pk_params.params[i] = key->params[i];
    }

  pk_params.params_nr = key->params_size;

  ret = _gnutls_pk_fixup (GNUTLS_PK_RSA, GNUTLS_IMPORT, &pk_params);
  if (ret < 0)
    {
      gnutls_assert ();
      FREE_RSA_PRIVATE_PARAMS;
      return ret;
    }

  for (i = 0; i < pk_params.params_nr; i++)
    {
      key->params[i] = pk_params.params[i];
    }
  key->params_size = pk_params.params_nr;

  if (!key->crippled)
    {
      ret = _gnutls_asn1_encode_rsa (&key->key, key->params);
      if (ret < 0)
        {
          gnutls_assert ();
          FREE_RSA_PRIVATE_PARAMS;
          return ret;
        }
    }

  key->params_size = RSA_PRIVATE_PARAMS;
  key->pk_algorithm = GNUTLS_PK_RSA;

  return 0;

}

/**
 * gnutls_x509_privkey_import_dsa_raw:
 * @key: The structure to store the parsed key
 * @p: holds the p
 * @q: holds the q
 * @g: holds the g
 * @y: holds the y
 * @x: holds the x
 *
 * This function will convert the given DSA raw parameters to the
 * native #gnutls_x509_privkey_t format.  The output will be stored
 * in @key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_import_dsa_raw (gnutls_x509_privkey_t key,
                                    const gnutls_datum_t * p,
                                    const gnutls_datum_t * q,
                                    const gnutls_datum_t * g,
                                    const gnutls_datum_t * y,
                                    const gnutls_datum_t * x)
{
  int i = 0, ret;
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
      FREE_DSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = q->size;
  if (_gnutls_mpi_scan_nz (&key->params[1], q->data, siz))
    {
      gnutls_assert ();
      FREE_DSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = g->size;
  if (_gnutls_mpi_scan_nz (&key->params[2], g->data, siz))
    {
      gnutls_assert ();
      FREE_DSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = y->size;
  if (_gnutls_mpi_scan_nz (&key->params[3], y->data, siz))
    {
      gnutls_assert ();
      FREE_DSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = x->size;
  if (_gnutls_mpi_scan_nz (&key->params[4], x->data, siz))
    {
      gnutls_assert ();
      FREE_DSA_PRIVATE_PARAMS;
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  if (!key->crippled)
    {
      ret = _gnutls_asn1_encode_dsa (&key->key, key->params);
      if (ret < 0)
        {
          gnutls_assert ();
          FREE_DSA_PRIVATE_PARAMS;
          return ret;
        }
    }

  key->params_size = DSA_PRIVATE_PARAMS;
  key->pk_algorithm = GNUTLS_PK_DSA;

  return 0;

}


/**
 * gnutls_x509_privkey_get_pk_algorithm:
 * @key: should contain a #gnutls_x509_privkey_t structure
 *
 * This function will return the public key algorithm of a private
 * key.
 *
 * Returns: a member of the #gnutls_pk_algorithm_t enumeration on
 *   success, or a negative value on error.
 **/
int
gnutls_x509_privkey_get_pk_algorithm (gnutls_x509_privkey_t key)
{
  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return key->pk_algorithm;
}

/**
 * gnutls_x509_privkey_export:
 * @key: Holds the key
 * @format: the format of output params. One of PEM or DER.
 * @output_data: will contain a private key PEM or DER encoded
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will export the private key to a PKCS1 structure for
 * RSA keys, or an integer sequence for DSA keys.  The DSA keys are in
 * the same format with the parameters used by openssl.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *@output_data_size is updated and %GNUTLS_E_SHORT_MEMORY_BUFFER
 * will be returned.
 *
 * If the structure is PEM encoded, it will have a header
 * of "BEGIN RSA PRIVATE KEY".
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_export (gnutls_x509_privkey_t key,
                            gnutls_x509_crt_fmt_t format, void *output_data,
                            size_t * output_data_size)
{
  const char *msg;
  int ret;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (key->pk_algorithm == GNUTLS_PK_RSA)
    msg = PEM_KEY_RSA;
  else if (key->pk_algorithm == GNUTLS_PK_DSA)
    msg = PEM_KEY_DSA;
  else
    msg = NULL;

  if (key->crippled)
    {                           /* encode the parameters on the fly.
                                 */
      switch (key->pk_algorithm)
        {
        case GNUTLS_PK_DSA:
          ret = _gnutls_asn1_encode_dsa (&key->key, key->params);
          if (ret < 0)
            {
              gnutls_assert ();
              return ret;
            }
          break;
        case GNUTLS_PK_RSA:
          ret = _gnutls_asn1_encode_rsa (&key->key, key->params);
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
    }

  return _gnutls_x509_export_int (key->key, format, msg,
                                  output_data, output_data_size);
}

/**
 * gnutls_x509_privkey_sec_param:
 * @key: a key structure
 *
 * This function will return the security parameter appropriate with
 * this private key.
 *
 * Returns: On success, a valid security parameter is returned otherwise
 * %GNUTLS_SEC_PARAM_UNKNOWN is returned.
 **/
gnutls_sec_param_t
gnutls_x509_privkey_sec_param (gnutls_x509_privkey_t key)
{
  int ret;

  switch (key->pk_algorithm)
    {
    case GNUTLS_PK_RSA:
      ret = gnutls_pk_bits_to_sec_param (GNUTLS_PK_RSA, _gnutls_mpi_get_nbits (key->params[0]   /*m */
                                         ));
      break;
    case GNUTLS_PK_DSA:
      ret = gnutls_pk_bits_to_sec_param (GNUTLS_PK_DSA, _gnutls_mpi_get_nbits (key->params[0]   /*p */
                                         ));
      break;
    default:
      ret = GNUTLS_SEC_PARAM_UNKNOWN;
    }

  return ret;
}

/**
 * gnutls_x509_privkey_export_rsa_raw:
 * @key: a structure that holds the rsa parameters
 * @m: will hold the modulus
 * @e: will hold the public exponent
 * @d: will hold the private exponent
 * @p: will hold the first prime (p)
 * @q: will hold the second prime (q)
 * @u: will hold the coefficient
 *
 * This function will export the RSA private key's parameters found
 * in the given structure. The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_export_rsa_raw (gnutls_x509_privkey_t key,
                                    gnutls_datum_t * m, gnutls_datum_t * e,
                                    gnutls_datum_t * d, gnutls_datum_t * p,
                                    gnutls_datum_t * q, gnutls_datum_t * u)
{

  return gnutls_x509_privkey_export_rsa_raw2 (key, m, e, d, p, q, u, NULL,
                                              NULL);
}

/**
 * gnutls_x509_privkey_export_rsa_raw2:
 * @key: a structure that holds the rsa parameters
 * @m: will hold the modulus
 * @e: will hold the public exponent
 * @d: will hold the private exponent
 * @p: will hold the first prime (p)
 * @q: will hold the second prime (q)
 * @u: will hold the coefficient
 * @e1: will hold e1 = d mod (p-1)
 * @e2: will hold e2 = d mod (q-1)
 *
 * This function will export the RSA private key's parameters found
 * in the given structure. The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_export_rsa_raw2 (gnutls_x509_privkey_t key,
                                     gnutls_datum_t * m, gnutls_datum_t * e,
                                     gnutls_datum_t * d, gnutls_datum_t * p,
                                     gnutls_datum_t * q, gnutls_datum_t * u,
                                     gnutls_datum_t * e1, gnutls_datum_t * e2)
{
  int ret;
  gnutls_pk_params_st pk_params;

  memset (&pk_params, 0, sizeof (pk_params));

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  m->data = e->data = d->data = p->data = q->data = u->data = NULL;
  m->size = e->size = d->size = p->size = q->size = u->size = 0;

  ret = _gnutls_pk_params_copy (&pk_params, key->params, RSA_PRIVATE_PARAMS);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_pk_fixup (GNUTLS_PK_RSA, GNUTLS_EXPORT, &pk_params);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  ret = _gnutls_mpi_dprint_lz (pk_params.params[0], m);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  /* E */
  ret = _gnutls_mpi_dprint_lz (pk_params.params[1], e);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  /* D */
  ret = _gnutls_mpi_dprint_lz (pk_params.params[2], d);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  /* P */
  ret = _gnutls_mpi_dprint_lz (pk_params.params[3], p);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  /* Q */
  ret = _gnutls_mpi_dprint_lz (pk_params.params[4], q);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  /* U */
  ret = _gnutls_mpi_dprint_lz (key->params[5], u);
  if (ret < 0)
    {
      gnutls_assert ();
      goto error;
    }

  /* E1 */
  if (e1)
    {
      ret = _gnutls_mpi_dprint_lz (key->params[6], e1);
      if (ret < 0)
        {
          gnutls_assert ();
          goto error;
        }
    }

  /* E2 */
  if (e2)
    {
      ret = _gnutls_mpi_dprint_lz (key->params[7], e2);
      if (ret < 0)
        {
          gnutls_assert ();
          goto error;
        }
    }

  gnutls_pk_params_release (&pk_params);

  return 0;

error:
  _gnutls_free_datum (m);
  _gnutls_free_datum (d);
  _gnutls_free_datum (e);
  _gnutls_free_datum (p);
  _gnutls_free_datum (q);
  gnutls_pk_params_release (&pk_params);

  return ret;
}

/**
 * gnutls_x509_privkey_export_dsa_raw:
 * @key: a structure that holds the DSA parameters
 * @p: will hold the p
 * @q: will hold the q
 * @g: will hold the g
 * @y: will hold the y
 * @x: will hold the x
 *
 * This function will export the DSA private key's parameters found
 * in the given structure. The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_export_dsa_raw (gnutls_x509_privkey_t key,
                                    gnutls_datum_t * p, gnutls_datum_t * q,
                                    gnutls_datum_t * g, gnutls_datum_t * y,
                                    gnutls_datum_t * x)
{
  int ret;

  if (key == NULL)
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

  /* X */
  ret = _gnutls_mpi_dprint_lz (key->params[4], x);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (y);
      _gnutls_free_datum (p);
      _gnutls_free_datum (g);
      _gnutls_free_datum (q);
      return ret;
    }

  return 0;
}


/* Encodes the RSA parameters into an ASN.1 RSA private key structure.
 */
static int
_gnutls_asn1_encode_rsa (ASN1_TYPE * c2, bigint_t * params)
{
  int result;
  opaque null = '\0';
  gnutls_pk_params_st pk_params;
  gnutls_datum_t m, e, d, p, q, u, exp1, exp2;

  memset (&pk_params, 0, sizeof (pk_params));

  memset (&m, 0, sizeof (m));
  memset (&p, 0, sizeof (p));
  memset (&q, 0, sizeof (q));
  memset (&p, 0, sizeof (p));
  memset (&u, 0, sizeof (u));
  memset (&e, 0, sizeof (e));
  memset (&d, 0, sizeof (d));
  memset (&exp1, 0, sizeof (exp1));
  memset (&exp2, 0, sizeof (exp2));

  result = _gnutls_pk_params_copy (&pk_params, params, RSA_PRIVATE_PARAMS);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  result = _gnutls_pk_fixup (GNUTLS_PK_RSA, GNUTLS_EXPORT, &pk_params);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* retrieve as data */

  result = _gnutls_mpi_dprint_lz (pk_params.params[0], &m);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_mpi_dprint_lz (pk_params.params[1], &e);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_mpi_dprint_lz (pk_params.params[2], &d);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_mpi_dprint_lz (pk_params.params[3], &p);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_mpi_dprint_lz (pk_params.params[4], &q);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_mpi_dprint_lz (pk_params.params[5], &u);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_mpi_dprint_lz (pk_params.params[6], &exp1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_mpi_dprint_lz (pk_params.params[7], &exp2);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* Ok. Now we have the data. Create the asn1 structures
   */

  /* first make sure that no previously allocated data are leaked */
  if (*c2 != ASN1_TYPE_EMPTY)
    {
      asn1_delete_structure (c2);
      *c2 = ASN1_TYPE_EMPTY;
    }

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.RSAPrivateKey", c2))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* Write PRIME 
   */
  if ((result = asn1_write_value (*c2, "modulus",
                                  m.data, m.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "publicExponent",
                                  e.data, e.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "privateExponent",
                                  d.data, d.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "prime1",
                                  p.data, p.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "prime2",
                                  q.data, q.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "coefficient",
                                  u.data, u.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);

      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "exponent1",
                                  exp1.data, exp1.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "exponent2",
                                  exp2.data, exp2.size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "otherPrimeInfos",
                                  NULL, 0)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "version", &null, 1)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result = 0;

cleanup:
  if (result != 0)
    asn1_delete_structure (c2);

  gnutls_pk_params_release (&pk_params);

  _gnutls_free_datum (&m);
  _gnutls_free_datum (&d);
  _gnutls_free_datum (&e);
  _gnutls_free_datum (&p);
  _gnutls_free_datum (&q);
  _gnutls_free_datum (&u);
  _gnutls_free_datum (&exp1);
  _gnutls_free_datum (&exp2);

  return result;
}

/* Encodes the DSA parameters into an ASN.1 DSAPrivateKey structure.
 */
int
_gnutls_asn1_encode_dsa (ASN1_TYPE * c2, bigint_t * params)
{
  int result, i;
  size_t size[DSA_PRIVATE_PARAMS], total;
  opaque *p_data, *q_data, *g_data, *x_data, *y_data;
  opaque *all_data = NULL, *p;
  opaque null = '\0';

  /* Read all the sizes */
  total = 0;
  for (i = 0; i < DSA_PRIVATE_PARAMS; i++)
    {
      _gnutls_mpi_print_lz (params[i], NULL, &size[i]);
      total += size[i];
    }

  /* Encoding phase.
   * allocate data enough to hold everything
   */
  all_data = gnutls_secure_malloc (total);
  if (all_data == NULL)
    {
      gnutls_assert ();
      result = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  p = all_data;
  p_data = p;
  p += size[0];
  q_data = p;
  p += size[1];
  g_data = p;
  p += size[2];
  y_data = p;
  p += size[3];
  x_data = p;

  _gnutls_mpi_print_lz (params[0], p_data, &size[0]);
  _gnutls_mpi_print_lz (params[1], q_data, &size[1]);
  _gnutls_mpi_print_lz (params[2], g_data, &size[2]);
  _gnutls_mpi_print_lz (params[3], y_data, &size[3]);
  _gnutls_mpi_print_lz (params[4], x_data, &size[4]);

  /* Ok. Now we have the data. Create the asn1 structures
   */

  /* first make sure that no previously allocated data are leaked */
  if (*c2 != ASN1_TYPE_EMPTY)
    {
      asn1_delete_structure (c2);
      *c2 = ASN1_TYPE_EMPTY;
    }

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.DSAPrivateKey", c2))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* Write PRIME 
   */
  if ((result = asn1_write_value (*c2, "p", p_data, size[0])) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "q", q_data, size[1])) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "g", g_data, size[2])) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "Y", y_data, size[3])) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if ((result = asn1_write_value (*c2, "priv",
                                  x_data, size[4])) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  gnutls_free (all_data);

  if ((result = asn1_write_value (*c2, "version", &null, 1)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  return 0;

cleanup:
  asn1_delete_structure (c2);
  gnutls_free (all_data);

  return result;
}


/**
 * gnutls_x509_privkey_generate:
 * @key: should contain a #gnutls_x509_privkey_t structure
 * @algo: is one of RSA or DSA.
 * @bits: the size of the modulus
 * @flags: unused for now.  Must be 0.
 *
 * This function will generate a random private key. Note that this
 * function must be called on an empty private key.
 *
 * Do not set the number of bits directly, use gnutls_sec_param_to_pk_bits().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_generate (gnutls_x509_privkey_t key,
                              gnutls_pk_algorithm_t algo, unsigned int bits,
                              unsigned int flags)
{
  int ret;
  unsigned int params_len = MAX_PRIV_PARAMS_SIZE;
  unsigned int i;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  switch (algo)
    {
    case GNUTLS_PK_DSA:
      ret = _gnutls_dsa_generate_params (key->params, &params_len, bits);
      if (params_len != DSA_PRIVATE_PARAMS)
        {
          gnutls_assert ();
          ret = GNUTLS_E_INTERNAL_ERROR;
        }

      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      if (!key->crippled)
        {
          ret = _gnutls_asn1_encode_dsa (&key->key, key->params);
          if (ret < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
        }
      key->params_size = params_len;
      key->pk_algorithm = GNUTLS_PK_DSA;

      break;
    case GNUTLS_PK_RSA:
      ret = _gnutls_rsa_generate_params (key->params, &params_len, bits);
      if (params_len != RSA_PRIVATE_PARAMS)
        {
          gnutls_assert ();
          ret = GNUTLS_E_INTERNAL_ERROR;
        }
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      if (!key->crippled)
        {
          ret = _gnutls_asn1_encode_rsa (&key->key, key->params);
          if (ret < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
        }

      key->params_size = params_len;
      key->pk_algorithm = GNUTLS_PK_RSA;

      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return 0;

cleanup:
  key->pk_algorithm = GNUTLS_PK_UNKNOWN;
  key->params_size = 0;
  for (i = 0; i < params_len; i++)
    _gnutls_mpi_release (&key->params[i]);

  return ret;
}

/**
 * gnutls_x509_privkey_get_key_id:
 * @key: Holds the key
 * @flags: should be 0 for now
 * @output_data: will contain the key ID
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will return a unique ID the depends on the public key
 * parameters. This ID can be used in checking whether a certificate
 * corresponds to the given key.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *@output_data_size is updated and %GNUTLS_E_SHORT_MEMORY_BUFFER will
 * be returned.  The output will normally be a SHA-1 hash output,
 * which is 20 bytes.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_get_key_id (gnutls_x509_privkey_t key,
                                unsigned int flags,
                                unsigned char *output_data,
                                size_t * output_data_size)
{
  int result;
  digest_hd_st hd;
  gnutls_datum_t der = { NULL, 0 };

  if (key == NULL || key->crippled)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (*output_data_size < 20)
    {
      gnutls_assert ();
      *output_data_size = 20;
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }

  if (key->pk_algorithm == GNUTLS_PK_RSA)
    {
      result =
        _gnutls_x509_write_rsa_params (key->params, key->params_size, &der);
      if (result < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }
  else if (key->pk_algorithm == GNUTLS_PK_DSA)
    {
      result =
        _gnutls_x509_write_dsa_public_key (key->params,
                                           key->params_size, &der);
      if (result < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }
  else
    return GNUTLS_E_INTERNAL_ERROR;

  result = _gnutls_hash_init (&hd, GNUTLS_MAC_SHA1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  _gnutls_hash (&hd, der.data, der.size);

  _gnutls_hash_deinit (&hd, output_data);
  *output_data_size = 20;

  result = 0;

cleanup:

  _gnutls_free_datum (&der);
  return result;
}


#ifdef ENABLE_PKI
/*-
 * _gnutls_x509_privkey_sign_hash2:
 * @signer: Holds the signer's key
 * @hash_algo: The hash algorithm used
 * @hash_data: holds the data to be signed
 * @signature: will contain newly allocated signature
 * @flags: zero for now
 *
 * This function will sign the given hashed data using a signature algorithm
 * supported by the private key. Signature algorithms are always used
 * together with a hash functions.  Different hash functions may be
 * used for the RSA algorithm, but only SHA-1,SHA-224 and SHA-256 
 * for the DSA keys, depending on their bit size.
 *
 * Use gnutls_x509_crt_get_preferred_hash_algorithm() to determine
 * the hash algorithm.
 *
 * The RSA algorithm is used in PKCS #1 v1.5 mode.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 -*/
static int
_gnutls_x509_privkey_sign_hash2 (gnutls_x509_privkey_t signer,
                                gnutls_digest_algorithm_t hash_algo,
                                unsigned int flags,
                                const gnutls_datum_t * hash_data,
                                gnutls_datum_t * signature)
{
  int ret;
  gnutls_datum_t digest;

  digest.data = gnutls_malloc (hash_data->size);
  if (digest.data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }
  digest.size = hash_data->size;
  memcpy (digest.data, hash_data->data, digest.size);

  ret = pk_prepare_hash (signer->pk_algorithm, hash_algo, &digest);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = _gnutls_soft_sign (signer->pk_algorithm, signer->params,
                           signer->params_size, &digest, signature);

  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = 0;

cleanup:
  _gnutls_free_datum (&digest);
  return ret;
}

/**
 * gnutls_x509_privkey_sign_hash:
 * @key: Holds the key
 * @hash: holds the data to be signed
 * @signature: will contain newly allocated signature
 *
 * This function will sign the given hash using the private key. Do not
 * use this function directly unless you know what it is. Typical signing
 * requires the data to be hashed and stored in special formats 
 * (e.g. BER Digest-Info for RSA).
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Deprecated in: 2.12.0
 */
int
gnutls_x509_privkey_sign_hash (gnutls_x509_privkey_t key,
                               const gnutls_datum_t * hash,
                               gnutls_datum_t * signature)
{
  int result;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result = _gnutls_soft_sign (key->pk_algorithm, key->params,
                              key->params_size, hash, signature);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/**
 * gnutls_x509_privkey_sign_data:
 * @key: Holds the key
 * @digest: should be MD5 or SHA1
 * @flags: should be 0 for now
 * @data: holds the data to be signed
 * @signature: will contain the signature
 * @signature_size: holds the size of signature (and will be replaced
 *   by the new size)
 *
 * This function will sign the given data using a signature algorithm
 * supported by the private key. Signature algorithms are always used
 * together with a hash functions.  Different hash functions may be
 * used for the RSA algorithm, but only SHA-1 for the DSA keys.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *@signature_size is updated and %GNUTLS_E_SHORT_MEMORY_BUFFER will
 * be returned.
 *
 * Use gnutls_x509_crt_get_preferred_hash_algorithm() to determine
 * the hash algorithm.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Deprecated: Use gnutls_privkey_sign_data().
 */
int
gnutls_x509_privkey_sign_data (gnutls_x509_privkey_t key,
                               gnutls_digest_algorithm_t digest,
                               unsigned int flags,
                               const gnutls_datum_t * data,
                               void *signature, size_t * signature_size)
{
  int result;
  gnutls_datum_t sig = { NULL, 0 };
  gnutls_datum_t hash;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result =
    pk_hash_data (key->pk_algorithm, digest, key->params, data, &hash);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  result =
    _gnutls_x509_privkey_sign_hash2 (key, digest, flags, &hash, signature);

  _gnutls_free_datum(&hash);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }
    
  if (*signature_size < sig.size)
    {
      *signature_size = sig.size;
      _gnutls_free_datum (&sig);
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }

  *signature_size = sig.size;
  memcpy (signature, sig.data, sig.size);

  _gnutls_free_datum (&sig);

  return 0;
}


/**
 * gnutls_x509_privkey_verify_data:
 * @key: Holds the key
 * @flags: should be 0 for now
 * @data: holds the data to be signed
 * @signature: contains the signature
 *
 * This function will verify the given signed data, using the
 * parameters in the private key.
 *
 * Returns: In case of a verification failure %GNUTLS_E_PK_SIG_VERIFY_FAILED 
 * is returned, and a positive code on success.
 *
 * Deprecated: Use gnutls_pubkey_verify_data().
 */
int
gnutls_x509_privkey_verify_data (gnutls_x509_privkey_t key,
                                 unsigned int flags,
                                 const gnutls_datum_t * data,
                                 const gnutls_datum_t * signature)
{
  int result;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  result = _gnutls_x509_privkey_verify_signature (data, signature, key);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return result;
}

/**
 * gnutls_x509_privkey_fix:
 * @key: Holds the key
 *
 * This function will recalculate the secondary parameters in a key.
 * In RSA keys, this can be the coefficient and exponent1,2.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_privkey_fix (gnutls_x509_privkey_t key)
{
  int ret;

  if (key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (!key->crippled)
    asn1_delete_structure (&key->key);
  switch (key->pk_algorithm)
    {
    case GNUTLS_PK_DSA:
      ret = _gnutls_asn1_encode_dsa (&key->key, key->params);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      break;
    case GNUTLS_PK_RSA:
      ret = _gnutls_asn1_encode_rsa (&key->key, key->params);
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
