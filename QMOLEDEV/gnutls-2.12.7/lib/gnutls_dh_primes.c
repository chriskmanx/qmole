/*
 * Copyright (C) 2000, 2001, 2003, 2004, 2005, 2008, 2009, 2010 Free
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
#include <gnutls_datum.h>
#include <x509_b64.h>           /* for PKCS3 PEM decoding */
#include <gnutls_global.h>
#include <gnutls_dh.h>
#include <gnutls_pk.h>
#include <gnutls/crypto.h>
#include "x509/x509_int.h"
#include "debug.h"


/* returns the prime and the generator of DH params.
 */
const bigint_t *
_gnutls_dh_params_to_mpi (gnutls_dh_params_t dh_primes)
{
  if (dh_primes == NULL || dh_primes->params[1] == NULL ||
      dh_primes->params[0] == NULL)
    {
      return NULL;
    }

  return dh_primes->params;
}


/**
 * gnutls_dh_params_import_raw:
 * @dh_params: Is a structure that will hold the prime numbers
 * @prime: holds the new prime
 * @generator: holds the new generator
 *
 * This function will replace the pair of prime and generator for use
 * in the Diffie-Hellman key exchange.  The new parameters should be
 * stored in the appropriate gnutls_datum.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_dh_params_import_raw (gnutls_dh_params_t dh_params,
                             const gnutls_datum_t * prime,
                             const gnutls_datum_t * generator)
{
  bigint_t tmp_prime, tmp_g;
  size_t siz;

  siz = prime->size;
  if (_gnutls_mpi_scan_nz (&tmp_prime, prime->data, siz))
    {
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  siz = generator->size;
  if (_gnutls_mpi_scan_nz (&tmp_g, generator->data, siz))
    {
      _gnutls_mpi_release (&tmp_prime);
      gnutls_assert ();
      return GNUTLS_E_MPI_SCAN_FAILED;
    }

  /* store the generated values
   */
  dh_params->params[0] = tmp_prime;
  dh_params->params[1] = tmp_g;

  return 0;

}

/**
 * gnutls_dh_params_init:
 * @dh_params: Is a structure that will hold the prime numbers
 *
 * This function will initialize the DH parameters structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_dh_params_init (gnutls_dh_params_t * dh_params)
{

  (*dh_params) = gnutls_calloc (1, sizeof (dh_params_st));
  if (*dh_params == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;

}

/**
 * gnutls_dh_params_deinit:
 * @dh_params: Is a structure that holds the prime numbers
 *
 * This function will deinitialize the DH parameters structure.
 **/
void
gnutls_dh_params_deinit (gnutls_dh_params_t dh_params)
{
  if (dh_params == NULL)
    return;

  _gnutls_mpi_release (&dh_params->params[0]);
  _gnutls_mpi_release (&dh_params->params[1]);

  gnutls_free (dh_params);

}

/**
 * gnutls_dh_params_cpy:
 * @dst: Is the destination structure, which should be initialized.
 * @src: Is the source structure
 *
 * This function will copy the DH parameters structure from source
 * to destination.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_dh_params_cpy (gnutls_dh_params_t dst, gnutls_dh_params_t src)
{
  if (src == NULL)
    return GNUTLS_E_INVALID_REQUEST;

  dst->params[0] = _gnutls_mpi_copy (src->params[0]);
  dst->params[1] = _gnutls_mpi_copy (src->params[1]);

  if (dst->params[0] == NULL || dst->params[1] == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  return 0;
}


/**
 * gnutls_dh_params_generate2:
 * @params: Is the structure that the DH parameters will be stored
 * @bits: is the prime's number of bits
 *
 * This function will generate a new pair of prime and generator for use in
 * the Diffie-Hellman key exchange. The new parameters will be allocated using
 * gnutls_malloc() and will be stored in the appropriate datum.
 * This function is normally slow.
 *
 * Do not set the number of bits directly, use gnutls_sec_param_to_pk_bits() to
 * get bits for %GNUTLS_PK_DSA.
 * Also note that the DH parameters are only useful to servers.
 * Since clients use the parameters sent by the server, it's of
 * no use to call this in client side.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_dh_params_generate2 (gnutls_dh_params_t params, unsigned int bits)
{
  int ret;
  gnutls_group_st group;

  ret = _gnutls_mpi_generate_group (&group, bits);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  params->params[0] = group.p;
  params->params[1] = group.g;

  return 0;
}

/**
 * gnutls_dh_params_import_pkcs3:
 * @params: A structure where the parameters will be copied to
 * @pkcs3_params: should contain a PKCS3 DHParams structure PEM or DER encoded
 * @format: the format of params. PEM or DER.
 *
 * This function will extract the DHParams found in a PKCS3 formatted
 * structure. This is the format generated by "openssl dhparam" tool.
 *
 * If the structure is PEM encoded, it should have a header
 * of "BEGIN DH PARAMETERS".
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_dh_params_import_pkcs3 (gnutls_dh_params_t params,
                               const gnutls_datum_t * pkcs3_params,
                               gnutls_x509_crt_fmt_t format)
{
  ASN1_TYPE c2;
  int result, need_free = 0;
  gnutls_datum_t _params;

  if (format == GNUTLS_X509_FMT_PEM)
    {
      opaque *out;

      result = _gnutls_fbase64_decode ("DH PARAMETERS",
                                       pkcs3_params->data,
                                       pkcs3_params->size, &out);

      if (result <= 0)
        {
          if (result == 0)
            result = GNUTLS_E_INTERNAL_ERROR;
          gnutls_assert ();
          return result;
        }

      _params.data = out;
      _params.size = result;

      need_free = 1;

    }
  else
    {
      _params.data = pkcs3_params->data;
      _params.size = pkcs3_params->size;
    }

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.DHParameter", &c2))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      if (need_free != 0)
        {
          gnutls_free (_params.data);
          _params.data = NULL;
        }
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&c2, _params.data, _params.size, NULL);

  if (need_free != 0)
    {
      gnutls_free (_params.data);
      _params.data = NULL;
    }

  if (result != ASN1_SUCCESS)
    {
      /* couldn't decode DER */

      _gnutls_x509_log ("DHParams: Decoding error %d\n", result);
      gnutls_assert ();
      asn1_delete_structure (&c2);
      return _gnutls_asn2err (result);
    }

  /* Read PRIME 
   */
  result = _gnutls_x509_read_int (c2, "prime", &params->params[0]);
  if (result < 0)
    {
      asn1_delete_structure (&c2);
      gnutls_assert ();
      return result;
    }

  /* read the generator
   */
  result = _gnutls_x509_read_int (c2, "base", &params->params[1]);
  if (result < 0)
    {
      asn1_delete_structure (&c2);
      _gnutls_mpi_release (&params->params[0]);
      gnutls_assert ();
      return result;
    }

  asn1_delete_structure (&c2);

  return 0;
}

/**
 * gnutls_dh_params_export_pkcs3:
 * @params: Holds the DH parameters
 * @format: the format of output params. One of PEM or DER.
 * @params_data: will contain a PKCS3 DHParams structure PEM or DER encoded
 * @params_data_size: holds the size of params_data (and will be replaced by the actual size of parameters)
 *
 * This function will export the given dh parameters to a PKCS3
 * DHParams structure. This is the format generated by "openssl dhparam" tool.
 * If the buffer provided is not long enough to hold the output, then
 * GNUTLS_E_SHORT_MEMORY_BUFFER will be returned.
 *
 * If the structure is PEM encoded, it will have a header
 * of "BEGIN DH PARAMETERS".
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_dh_params_export_pkcs3 (gnutls_dh_params_t params,
                               gnutls_x509_crt_fmt_t format,
                               unsigned char *params_data,
                               size_t * params_data_size)
{
  ASN1_TYPE c2;
  int result, _params_data_size;
  size_t g_size, p_size;
  opaque *p_data, *g_data;
  opaque *all_data;

  _gnutls_mpi_print_lz (params->params[1], NULL, &g_size);
  _gnutls_mpi_print_lz (params->params[0], NULL, &p_size);

  all_data = gnutls_malloc (g_size + p_size);
  if (all_data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  p_data = &all_data[0];
  _gnutls_mpi_print_lz (params->params[0], p_data, &p_size);

  g_data = &all_data[p_size];
  _gnutls_mpi_print_lz (params->params[1], g_data, &g_size);


  /* Ok. Now we have the data. Create the asn1 structures
   */

  if ((result = asn1_create_element
       (_gnutls_get_gnutls_asn (), "GNUTLS.DHParameter", &c2))
      != ASN1_SUCCESS)
    {
      gnutls_assert ();
      gnutls_free (all_data);
      return _gnutls_asn2err (result);
    }

  /* Write PRIME 
   */
  if ((result = asn1_write_value (c2, "prime",
                                  p_data, p_size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      gnutls_free (all_data);
      asn1_delete_structure (&c2);
      return _gnutls_asn2err (result);
    }

  /* Write the GENERATOR
   */
  if ((result = asn1_write_value (c2, "base",
                                  g_data, g_size)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      gnutls_free (all_data);
      asn1_delete_structure (&c2);
      return _gnutls_asn2err (result);
    }

  gnutls_free (all_data);

  if ((result = asn1_write_value (c2, "privateValueLength",
                                  NULL, 0)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&c2);
      return _gnutls_asn2err (result);
    }

  if (format == GNUTLS_X509_FMT_DER)
    {
      if (params_data == NULL)
        *params_data_size = 0;

      _params_data_size = *params_data_size;
      result =
        asn1_der_coding (c2, "", params_data, &_params_data_size, NULL);
      *params_data_size = _params_data_size;
      asn1_delete_structure (&c2);

      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          if (result == ASN1_MEM_ERROR)
            return GNUTLS_E_SHORT_MEMORY_BUFFER;

          return _gnutls_asn2err (result);
        }

    }
  else
    {                           /* PEM */
      opaque *tmp;
      opaque *out;
      int len;

      len = 0;
      asn1_der_coding (c2, "", NULL, &len, NULL);

      tmp = gnutls_malloc (len);
      if (tmp == NULL)
        {
          gnutls_assert ();
          asn1_delete_structure (&c2);
          return GNUTLS_E_MEMORY_ERROR;
        }

      if ((result =
           asn1_der_coding (c2, "", tmp, &len, NULL)) != ASN1_SUCCESS)
        {
          gnutls_assert ();
          gnutls_free (tmp);
          asn1_delete_structure (&c2);
          return _gnutls_asn2err (result);
        }

      asn1_delete_structure (&c2);

      result = _gnutls_fbase64_encode ("DH PARAMETERS", tmp, len, &out);

      gnutls_free (tmp);

      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }

      if (result == 0)
        {                       /* oooops */
          gnutls_assert ();
          gnutls_free (out);
          return GNUTLS_E_INTERNAL_ERROR;
        }

      if ((unsigned) result > *params_data_size)
        {
          gnutls_assert ();
          gnutls_free (out);
          *params_data_size = result;
          return GNUTLS_E_SHORT_MEMORY_BUFFER;
        }

      *params_data_size = result - 1;

      if (params_data)
        memcpy (params_data, out, result);

      gnutls_free (out);

    }

  return 0;
}

/**
 * gnutls_dh_params_export_raw:
 * @params: Holds the DH parameters
 * @prime: will hold the new prime
 * @generator: will hold the new generator
 * @bits: if non null will hold is the prime's number of bits
 *
 * This function will export the pair of prime and generator for use
 * in the Diffie-Hellman key exchange.  The new parameters will be
 * allocated using gnutls_malloc() and will be stored in the
 * appropriate datum.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_dh_params_export_raw (gnutls_dh_params_t params,
                             gnutls_datum_t * prime,
                             gnutls_datum_t * generator, unsigned int *bits)
{
  int ret;

  if (params->params[1] == NULL || params->params[0] == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = _gnutls_mpi_dprint (params->params[1], generator);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_mpi_dprint (params->params[0], prime);
  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (generator);
      return ret;
    }

  if (bits)
    *bits = _gnutls_mpi_get_nbits (params->params[0]);

  return 0;

}
