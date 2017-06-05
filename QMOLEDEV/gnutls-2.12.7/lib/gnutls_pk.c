/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2009, 2010
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

/* This file contains the functions needed for RSA/DSA public key
 * encryption and signatures. 
 */

#include <gnutls_int.h>
#include <gnutls_mpi.h>
#include <gnutls_pk.h>
#include <gnutls_errors.h>
#include <gnutls_datum.h>
#include <gnutls_global.h>
#include <gnutls_num.h>
#include "debug.h"
#include <x509/x509_int.h>
#include <x509/common.h>
#include <random.h>

/* Do PKCS-1 RSA encryption. 
 * params is modulus, public exp.
 */
int
_gnutls_pkcs1_rsa_encrypt (gnutls_datum_t * ciphertext,
                           const gnutls_datum_t * plaintext,
                           bigint_t * params, unsigned params_len,
                           unsigned btype)
{
  unsigned int i, pad;
  int ret;
  opaque *edata, *ps;
  size_t k, psize;
  size_t mod_bits;
  gnutls_pk_params_st pk_params;
  gnutls_datum_t to_encrypt, encrypted;

  for (i = 0; i < params_len; i++)
    pk_params.params[i] = params[i];
  pk_params.params_nr = params_len;

  mod_bits = _gnutls_mpi_get_nbits (params[0]);
  k = mod_bits / 8;
  if (mod_bits % 8 != 0)
    k++;

  if (plaintext->size > k - 11)
    {
      gnutls_assert ();
      return GNUTLS_E_PK_ENCRYPTION_FAILED;
    }

  edata = gnutls_malloc (k);
  if (edata == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* EB = 00||BT||PS||00||D 
   * (use block type 'btype')
   */

  edata[0] = 0;
  edata[1] = btype;
  psize = k - 3 - plaintext->size;

  ps = &edata[2];
  switch (btype)
    {
    case 2:
      /* using public key */
      if (params_len < RSA_PUBLIC_PARAMS)
        {
          gnutls_assert ();
          gnutls_free (edata);
          return GNUTLS_E_INTERNAL_ERROR;
        }

      ret = _gnutls_rnd (GNUTLS_RND_RANDOM, ps, psize);
      if (ret < 0)
        {
          gnutls_assert ();
          gnutls_free (edata);
          return ret;
        }
      for (i = 0; i < psize; i++)
        while (ps[i] == 0)
          {
            ret = _gnutls_rnd (GNUTLS_RND_RANDOM, &ps[i], 1);
            if (ret < 0)
              {
                gnutls_assert ();
                gnutls_free (edata);
                return ret;
              }
          }
      break;
    case 1:
      /* using private key */

      if (params_len < RSA_PRIVATE_PARAMS)
        {
          gnutls_assert ();
          gnutls_free (edata);
          return GNUTLS_E_INTERNAL_ERROR;
        }

      for (i = 0; i < psize; i++)
        ps[i] = 0xff;
      break;
    default:
      gnutls_assert ();
      gnutls_free (edata);
      return GNUTLS_E_INTERNAL_ERROR;
    }

  ps[psize] = 0;
  memcpy (&ps[psize + 1], plaintext->data, plaintext->size);

  to_encrypt.data = edata;
  to_encrypt.size = k;

  if (btype == 2)               /* encrypt */
    ret =
      _gnutls_pk_encrypt (GNUTLS_PK_RSA, &encrypted, &to_encrypt, &pk_params);
  else                          /* sign */
    ret =
      _gnutls_pk_sign (GNUTLS_PK_RSA, &encrypted, &to_encrypt, &pk_params);

  gnutls_free (edata);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  psize = encrypted.size;
  if (psize < k)
    {
      /* padding psize */
      pad = k - psize;
      psize = k;
    }
  else if (psize == k)
    {
      /* pad = 0; 
       * no need to do anything else
       */
      ciphertext->data = encrypted.data;
      ciphertext->size = encrypted.size;
      return 0;
    }
  else
    {                           /* psize > k !!! */
      /* This is an impossible situation */
      gnutls_assert ();
      _gnutls_free_datum (&encrypted);
      return GNUTLS_E_INTERNAL_ERROR;
    }

  ciphertext->data = gnutls_malloc (psize);
  if (ciphertext->data == NULL)
    {
      gnutls_assert ();
      _gnutls_free_datum (&encrypted);
      return GNUTLS_E_MEMORY_ERROR;
    }

  memcpy (&ciphertext->data[pad], encrypted.data, encrypted.size);
  for (i = 0; i < pad; i++)
    ciphertext->data[i] = 0;

  ciphertext->size = k;

  _gnutls_free_datum (&encrypted);

  return 0;
}


/* Do PKCS-1 RSA decryption. 
 * params is modulus, public exp., private key
 * Can decrypt block type 1 and type 2 packets.
 */
int
_gnutls_pkcs1_rsa_decrypt (gnutls_datum_t * plaintext,
                           const gnutls_datum_t * ciphertext,
                           bigint_t * params, unsigned params_len,
                           unsigned btype)
{
  unsigned int k, i;
  int ret;
  size_t esize, mod_bits;
  gnutls_pk_params_st pk_params;

  for (i = 0; i < params_len; i++)
    pk_params.params[i] = params[i];
  pk_params.params_nr = params_len;

  mod_bits = _gnutls_mpi_get_nbits (params[0]);
  k = mod_bits / 8;
  if (mod_bits % 8 != 0)
    k++;

  esize = ciphertext->size;

  if (esize != k)
    {
      gnutls_assert ();
      return GNUTLS_E_PK_DECRYPTION_FAILED;
    }

  /* we can use btype to see if the private key is
   * available.
   */
  if (btype == 2)
    {
      ret =
        _gnutls_pk_decrypt (GNUTLS_PK_RSA, plaintext, ciphertext, &pk_params);
    }
  else
    {
      ret =
        _gnutls_pk_encrypt (GNUTLS_PK_RSA, plaintext, ciphertext, &pk_params);
    }

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* EB = 00||BT||PS||00||D
   * (use block type 'btype')
   *
   * From now on, return GNUTLS_E_DECRYPTION_FAILED on errors, to
   * avoid attacks similar to the one described by Bleichenbacher in:
   * "Chosen Ciphertext Attacks against Protocols Based on RSA
   * Encryption Standard PKCS #1".
   */
  if (plaintext->data[0] != 0 || plaintext->data[1] != btype)
    {
      gnutls_assert ();
      return GNUTLS_E_DECRYPTION_FAILED;
    }

  ret = GNUTLS_E_DECRYPTION_FAILED;
  switch (btype)
    {
    case 2:
      for (i = 2; i < plaintext->size; i++)
        {
          if (plaintext->data[i] == 0)
            {
              ret = 0;
              break;
            }
        }
      break;
    case 1:
      for (i = 2; i < plaintext->size; i++)
        {
          if (plaintext->data[i] == 0 && i > 2)
            {
              ret = 0;
              break;
            }
          if (plaintext->data[i] != 0xff)
            {
              _gnutls_handshake_log ("PKCS #1 padding error");
              _gnutls_free_datum (plaintext);
              /* PKCS #1 padding error.  Don't use
                 GNUTLS_E_PKCS1_WRONG_PAD here.  */
              break;
            }
        }
      break;
    default:
      gnutls_assert ();
      _gnutls_free_datum (plaintext);
      break;
    }
  i++;

  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (plaintext);
      return GNUTLS_E_DECRYPTION_FAILED;
    }

  memmove (plaintext->data, &plaintext->data[i], esize - i);
  plaintext->size = esize - i;

  return 0;
}


int
_gnutls_rsa_verify (const gnutls_datum_t * vdata,
                    const gnutls_datum_t * ciphertext, bigint_t * params,
                    int params_len, int btype)
{

  gnutls_datum_t plain;
  int ret;

  /* decrypt signature */
  if ((ret =
       _gnutls_pkcs1_rsa_decrypt (&plain, ciphertext, params, params_len,
                                  btype)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (plain.size != vdata->size)
    {
      gnutls_assert ();
      _gnutls_free_datum (&plain);
      return GNUTLS_E_PK_SIG_VERIFY_FAILED;
    }

  if (memcmp (plain.data, vdata->data, plain.size) != 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&plain);
      return GNUTLS_E_PK_SIG_VERIFY_FAILED;
    }

  _gnutls_free_datum (&plain);

  return 0;                     /* ok */
}

/* encodes the Dss-Sig-Value structure
 */
int
_gnutls_encode_ber_rs (gnutls_datum_t * sig_value, bigint_t r, bigint_t s)
{
  ASN1_TYPE sig;
  int result;

  if ((result =
       asn1_create_element (_gnutls_get_gnutls_asn (),
                            "GNUTLS.DSASignatureValue",
                            &sig)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_write_int (sig, "r", r, 1);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&sig);
      return result;
    }

  result = _gnutls_x509_write_int (sig, "s", s, 1);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&sig);
      return result;
    }

  result = _gnutls_x509_der_encode (sig, "", sig_value, 0);

  asn1_delete_structure (&sig);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}


/* Do DSA signature calculation. params is p, q, g, y, x in that order.
 */
int
_gnutls_dsa_sign (gnutls_datum_t * signature,
                  const gnutls_datum_t * hash, bigint_t * params,
                  unsigned int params_len)
{
  int ret;
  size_t i;
  size_t k;
  gnutls_pk_params_st pk_params;

  for (i = 0; i < params_len; i++)
    pk_params.params[i] = params[i];
  pk_params.params_nr = params_len;

  k = hash->size;
  if (k < 20)
    {                           /* SHA1 or better only */
      gnutls_assert ();
      return GNUTLS_E_PK_SIGN_FAILED;
    }

  ret = _gnutls_pk_sign (GNUTLS_PK_DSA, signature, hash, &pk_params);
  /* rs[0], rs[1] now hold r,s */

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/* decodes the Dss-Sig-Value structure
 */
int
_gnutls_decode_ber_rs (const gnutls_datum_t * sig_value, bigint_t * r,
                       bigint_t * s)
{
  ASN1_TYPE sig;
  int result;

  if ((result =
       asn1_create_element (_gnutls_get_gnutls_asn (),
                            "GNUTLS.DSASignatureValue",
                            &sig)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&sig, sig_value->data, sig_value->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&sig);
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_read_int (sig, "r", r);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&sig);
      return result;
    }

  result = _gnutls_x509_read_int (sig, "s", s);
  if (result < 0)
    {
      gnutls_assert ();
      _gnutls_mpi_release (s);
      asn1_delete_structure (&sig);
      return result;
    }

  asn1_delete_structure (&sig);

  return 0;
}

/* params is p, q, g, y in that order
 */
int
_gnutls_dsa_verify (const gnutls_datum_t * vdata,
                    const gnutls_datum_t * sig_value, bigint_t * params,
                    int params_len)
{

  int ret, i;
  gnutls_pk_params_st pk_params;

  for (i = 0; i < params_len; i++)
    pk_params.params[i] = params[i];
  pk_params.params_nr = params_len;

  if (vdata->size < 20)
    { /* SHA1 or better only */
      gnutls_assert ();
      return GNUTLS_E_PK_SIG_VERIFY_FAILED;
    }

  /* decrypt signature */
  ret = _gnutls_pk_verify (GNUTLS_PK_DSA, vdata, sig_value, &pk_params);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;                     /* ok */
}

/* some generic pk functions */
static int
_generate_params (int algo, bigint_t * resarr, unsigned int *resarr_len,
                  int bits)
{
  gnutls_pk_params_st params;
  int ret;
  unsigned int i;

  ret = _gnutls_pk_ops.generate (algo, bits, &params);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (resarr && resarr_len && *resarr_len >= params.params_nr)
    {
      *resarr_len = params.params_nr;
      for (i = 0; i < params.params_nr; i++)
        resarr[i] = params.params[i];
    }
  else
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }
  return 0;
}



int
_gnutls_rsa_generate_params (bigint_t * resarr, unsigned int *resarr_len,
                             int bits)
{
  return _generate_params (GNUTLS_PK_RSA, resarr, resarr_len, bits);
}

int
_gnutls_dsa_generate_params (bigint_t * resarr, unsigned int *resarr_len,
                             int bits)
{
  return _generate_params (GNUTLS_PK_DSA, resarr, resarr_len, bits);
}

int
_gnutls_pk_params_copy (gnutls_pk_params_st * dst, bigint_t * params,
                        int params_len)
{
  int i, j;
  dst->params_nr = 0;

  if (params_len == 0 || params == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  for (i = 0; i < params_len; i++)
    {
      dst->params[i] = _gnutls_mpi_set (NULL, params[i]);
      if (dst->params[i] == NULL)
        {
          for (j = 0; j < i; j++)
            _gnutls_mpi_release (&dst->params[j]);
          return GNUTLS_E_MEMORY_ERROR;
        }
      dst->params_nr++;
    }

  return 0;
}

void
gnutls_pk_params_init (gnutls_pk_params_st * p)
{
  memset (p, 0, sizeof (gnutls_pk_params_st));
}

void
gnutls_pk_params_release (gnutls_pk_params_st * p)
{
  unsigned int i;
  for (i = 0; i < p->params_nr; i++)
    {
      _gnutls_mpi_release (&p->params[i]);
    }
}

int
_gnutls_calc_rsa_exp (bigint_t * params, unsigned int params_size)
{
  bigint_t tmp = _gnutls_mpi_alloc_like (params[0]);

  if (params_size < RSA_PRIVATE_PARAMS - 2)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  if (tmp == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* [6] = d % p-1, [7] = d % q-1 */
  _gnutls_mpi_sub_ui (tmp, params[3], 1);
  params[6] = _gnutls_mpi_mod (params[2] /*d */ , tmp);

  _gnutls_mpi_sub_ui (tmp, params[4], 1);
  params[7] = _gnutls_mpi_mod (params[2] /*d */ , tmp);

  _gnutls_mpi_release (&tmp);

  if (params[7] == NULL || params[6] == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;
}

int
_gnutls_pk_get_hash_algorithm (gnutls_pk_algorithm_t pk, bigint_t * params,
                               int params_size,
                               gnutls_digest_algorithm_t * dig,
                               unsigned int *mand)
{
  if (mand)
    {
      if (pk == GNUTLS_PK_DSA)
        *mand = 1;
      else
        *mand = 0;
    }

  return _gnutls_x509_verify_algorithm ((gnutls_mac_algorithm_t *) dig,
                                        NULL, pk, params, params_size);

}
