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
#include <openpgp/gnutls_openpgp.h>
#include <gnutls_sig.h>
#include <abstract_int.h>

struct gnutls_privkey_st
{
  gnutls_privkey_type_t type;
  gnutls_pk_algorithm_t pk_algorithm;

  union
  {
    gnutls_x509_privkey_t x509;
    gnutls_pkcs11_privkey_t pkcs11;
#ifdef ENABLE_OPENPGP
    gnutls_openpgp_privkey_t openpgp;
#endif
  } key;

  unsigned int flags;
};

/**
 * gnutls_privkey_get_type:
 * @key: should contain a #gnutls_privkey_t structure
 *
 * This function will return the type of the private key. This is
 * actually the type of the subsystem used to set this private key.
 *
 * Returns: a member of the #gnutls_privkey_type_t enumeration on
 *   success, or a negative value on error.
 **/
gnutls_privkey_type_t
gnutls_privkey_get_type (gnutls_privkey_t key)
{
  return key->type;
}

/**
 * gnutls_privkey_get_pk_algorithm:
 * @key: should contain a #gnutls_privkey_t structure
 * @bits: If set will return the number of bits of the parameters (may be NULL)
 *
 * This function will return the public key algorithm of a private
 * key and if possible will return a number of bits that indicates
 * the security parameter of the key.
 *
 * Returns: a member of the #gnutls_pk_algorithm_t enumeration on
 *   success, or a negative value on error.
 **/
int
gnutls_privkey_get_pk_algorithm (gnutls_privkey_t key, unsigned int *bits)
{
  switch (key->type)
    {
#ifdef ENABLE_OPENPGP
    case GNUTLS_PRIVKEY_OPENPGP:
      return gnutls_openpgp_privkey_get_pk_algorithm (key->key.openpgp, bits);
#endif
    case GNUTLS_PRIVKEY_PKCS11:
      return gnutls_pkcs11_privkey_get_pk_algorithm (key->key.pkcs11, bits);
    case GNUTLS_PRIVKEY_X509:
      if (bits)
        *bits = _gnutls_mpi_get_nbits (key->key.x509->params[0]);
      return gnutls_x509_privkey_get_pk_algorithm (key->key.x509);
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

}

static int
privkey_to_pubkey (gnutls_pk_algorithm_t pk,
                   const bigint_t * params, int params_size,
                   bigint_t * new_params, int *new_params_size)
{
  int ret, i;

  switch (pk)
    {
    case GNUTLS_PK_RSA:
      if (*new_params_size < RSA_PUBLIC_PARAMS
          || params_size < RSA_PRIVATE_PARAMS)
        {
          gnutls_assert ();
          return GNUTLS_E_INVALID_REQUEST;
        }

      new_params[0] = _gnutls_mpi_copy (params[0]);
      new_params[1] = _gnutls_mpi_copy (params[1]);

      *new_params_size = RSA_PUBLIC_PARAMS;

      if (new_params[0] == NULL || new_params[1] == NULL)
        {
          gnutls_assert ();
          ret = GNUTLS_E_MEMORY_ERROR;
          goto cleanup;
        }

      break;
    case GNUTLS_PK_DSA:
      if (*new_params_size < DSA_PUBLIC_PARAMS
          || params_size < DSA_PRIVATE_PARAMS)
        {
          gnutls_assert ();
          return GNUTLS_E_INVALID_REQUEST;
        }

      new_params[0] = _gnutls_mpi_copy (params[0]);
      new_params[1] = _gnutls_mpi_copy (params[1]);
      new_params[2] = _gnutls_mpi_copy (params[2]);
      new_params[3] = _gnutls_mpi_copy (params[3]);

      *new_params_size = DSA_PUBLIC_PARAMS;

      if (new_params[0] == NULL || new_params[1] == NULL ||
          new_params[2] == NULL || new_params[3] == NULL)
        {
          gnutls_assert ();
          ret = GNUTLS_E_MEMORY_ERROR;
          goto cleanup;
        }

      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return 0;
cleanup:
  for (i = 0; i < *new_params_size; i++)
    _gnutls_mpi_release (new_params[i]);
  return ret;
}


/* Returns the public key of the private key (if possible)
 */
int
_gnutls_privkey_get_public_mpis (gnutls_privkey_t key,
                                 bigint_t * params, int *params_size)
{
  int ret;
  gnutls_pk_algorithm_t pk = gnutls_privkey_get_pk_algorithm (key, NULL);

  switch (key->type)
    {
#ifdef ENABLE_OPENPGP
    case GNUTLS_PRIVKEY_OPENPGP:
      {
        bigint_t tmp_params[MAX_PRIV_PARAMS_SIZE];
        int tmp_params_size = MAX_PRIV_PARAMS_SIZE;
        uint32_t kid[2], i;
        uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];

        ret =
          gnutls_openpgp_privkey_get_preferred_key_id (key->key.openpgp,
                                                       keyid);
        if (ret == 0)
          {
            KEYID_IMPORT (kid, keyid);
            ret = _gnutls_openpgp_privkey_get_mpis (key->key.openpgp, kid,
                                                    tmp_params,
                                                    &tmp_params_size);
          }
        else
          ret = _gnutls_openpgp_privkey_get_mpis (key->key.openpgp, NULL,
                                                  tmp_params,
                                                  &tmp_params_size);

        if (ret < 0)
          {
            gnutls_assert ();
            return ret;
          }

        ret = privkey_to_pubkey (pk,
                                 tmp_params, tmp_params_size,
                                 params, params_size);

        for (i = 0; i < tmp_params_size; i++)
          _gnutls_mpi_release (&tmp_params[i]);

      }

      break;
#endif
    case GNUTLS_PRIVKEY_X509:
      ret = privkey_to_pubkey (pk,
                               key->key.x509->params,
                               key->key.x509->params_size, params,
                               params_size);
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return ret;
}

/**
 * gnutls_privkey_init:
 * @key: The structure to be initialized
 *
 * This function will initialize an private key structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_privkey_init (gnutls_privkey_t * key)
{
  *key = gnutls_calloc (1, sizeof (struct gnutls_privkey_st));
  if (*key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;
}

/**
 * gnutls_privkey_deinit:
 * @key: The structure to be deinitialized
 *
 * This function will deinitialize a private key structure.
 **/
void
gnutls_privkey_deinit (gnutls_privkey_t key)
{
  if (key->flags & GNUTLS_PRIVKEY_IMPORT_AUTO_RELEASE)
    switch (key->type)
      {
#ifdef ENABLE_OPENPGP
      case GNUTLS_PRIVKEY_OPENPGP:
        gnutls_openpgp_privkey_deinit (key->key.openpgp);
        break;
#endif
      case GNUTLS_PRIVKEY_PKCS11:
        gnutls_pkcs11_privkey_deinit (key->key.pkcs11);
        break;
      case GNUTLS_PRIVKEY_X509:
        gnutls_x509_privkey_deinit (key->key.x509);
        break;
      }
  gnutls_free (key);
}

/* will fail if the private key contains an actual key.
 */
static int check_if_clean(gnutls_privkey_t key)
{
  if (key->type != 0)
    return GNUTLS_E_INVALID_REQUEST;

  return 0;
}

/**
 * gnutls_privkey_import_pkcs11:
 * @pkey: The private key
 * @key: The private key to be imported
 * @flags: should be zero
 *
 * This function will import the given private key to the abstract
 * #gnutls_privkey_t structure.
 *
 * The #gnutls_pkcs11_privkey_t object must not be deallocated
 * during the lifetime of this structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_privkey_import_pkcs11 (gnutls_privkey_t pkey,
                              gnutls_pkcs11_privkey_t key, unsigned int flags)
{
int ret;

  ret = check_if_clean(pkey);
  if (ret < 0)
    {
      gnutls_assert();
      return ret;
    }

  pkey->key.pkcs11 = key;
  pkey->type = GNUTLS_PRIVKEY_PKCS11;
  pkey->pk_algorithm = gnutls_pkcs11_privkey_get_pk_algorithm (key, NULL);
  pkey->flags = flags;

  return 0;
}

/**
 * gnutls_privkey_import_x509:
 * @pkey: The private key
 * @key: The private key to be imported
 * @flags: should be zero
 *
 * This function will import the given private key to the abstract
 * #gnutls_privkey_t structure.
 *
 * The #gnutls_x509_privkey_t object must not be deallocated
 * during the lifetime of this structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_privkey_import_x509 (gnutls_privkey_t pkey,
                            gnutls_x509_privkey_t key, unsigned int flags)
{
int ret;

  ret = check_if_clean(pkey);
  if (ret < 0)
    {
      gnutls_assert();
      return ret;
    }

  pkey->key.x509 = key;
  pkey->type = GNUTLS_PRIVKEY_X509;
  pkey->pk_algorithm = gnutls_x509_privkey_get_pk_algorithm (key);
  pkey->flags = flags;

  return 0;
}

#ifdef ENABLE_OPENPGP
/**
 * gnutls_privkey_import_openpgp:
 * @pkey: The private key
 * @key: The private key to be imported
 * @flags: should be zero
 *
 * This function will import the given private key to the abstract
 * #gnutls_privkey_t structure.
 *
 * The #gnutls_openpgp_privkey_t object must not be deallocated
 * during the lifetime of this structure. The subkey set as
 * preferred will be used, or the master key otherwise.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_privkey_import_openpgp (gnutls_privkey_t pkey,
                               gnutls_openpgp_privkey_t key,
                               unsigned int flags)
{
int ret, idx;
uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];

  ret = check_if_clean(pkey);
  if (ret < 0)
    {
      gnutls_assert();
      return ret;
    }

  pkey->key.openpgp = key;
  pkey->type = GNUTLS_PRIVKEY_OPENPGP;
  
  ret = gnutls_openpgp_privkey_get_preferred_key_id (key, keyid);
  if (ret == GNUTLS_E_OPENPGP_PREFERRED_KEY_ERROR)
    {
      pkey->pk_algorithm = gnutls_openpgp_privkey_get_pk_algorithm(key, NULL);
    }
  else
    {
      if (ret < 0)
        return gnutls_assert_val(ret);

      idx = gnutls_openpgp_privkey_get_subkey_idx (key, keyid);
  
      pkey->pk_algorithm = gnutls_openpgp_privkey_get_subkey_pk_algorithm (key, idx, NULL);
    }

  pkey->flags = flags;

  return 0;
}
#endif

/**
 * gnutls_privkey_sign_data:
 * @signer: Holds the key
 * @hash: should be a digest algorithm
 * @flags: should be 0 for now
 * @data: holds the data to be signed
 * @signature: will contain the signature allocate with gnutls_malloc()
 *
 * This function will sign the given data using a signature algorithm
 * supported by the private key. Signature algorithms are always used
 * together with a hash functions.  Different hash functions may be
 * used for the RSA algorithm, but only SHA-1 for the DSA keys.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 * negative error value.
 *
 * Since: 2.12.0
 **/
int
gnutls_privkey_sign_data (gnutls_privkey_t signer,
                          gnutls_digest_algorithm_t hash,
                          unsigned int flags,
                          const gnutls_datum_t * data,
                          gnutls_datum_t * signature)
{
  int ret;
  gnutls_datum_t digest;

  ret = pk_hash_data (signer->pk_algorithm, hash, NULL, data, &digest);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = pk_prepare_hash (signer->pk_algorithm, hash, &digest);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = _gnutls_privkey_sign_hash (signer, &digest, signature);
  _gnutls_free_datum (&digest);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;

cleanup:
  _gnutls_free_datum (&digest);
  return ret;
}

/**
 * gnutls_privkey_sign_hash:
 * @signer: Holds the signer's key
 * @hash_algo: The hash algorithm used
 * @flags: zero for now
 * @hash_data: holds the data to be signed
 * @signature: will contain newly allocated signature
 *
 * This function will sign the given hashed data using a signature algorithm
 * supported by the private key. Signature algorithms are always used
 * together with a hash functions.  Different hash functions may be
 * used for the RSA algorithm, but only SHA-XXX for the DSA keys.
 *
 * Use gnutls_x509_crt_get_preferred_hash_algorithm() to determine
 * the hash algorithm.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Since: 2.12.0
 **/
int
gnutls_privkey_sign_hash (gnutls_privkey_t signer,
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

  ret = _gnutls_privkey_sign_hash (signer, &digest, signature);
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

/*-
 * _gnutls_privkey_sign_hash:
 * @key: Holds the key
 * @data: holds the data to be signed
 * @signature: will contain the signature allocate with gnutls_malloc()
 *
 * This function will sign the given data using a signature algorithm
 * supported by the private key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 * negative error value.
 -*/
int
_gnutls_privkey_sign_hash (gnutls_privkey_t key,
                           const gnutls_datum_t * hash,
                           gnutls_datum_t * signature)
{
  switch (key->type)
    {
#ifdef ENABLE_OPENPGP
    case GNUTLS_PRIVKEY_OPENPGP:
      return gnutls_openpgp_privkey_sign_hash (key->key.openpgp,
                                                hash, signature);
#endif
    case GNUTLS_PRIVKEY_PKCS11:
      return _gnutls_pkcs11_privkey_sign_hash (key->key.pkcs11,
                                               hash, signature);
    case GNUTLS_PRIVKEY_X509:
      return _gnutls_soft_sign (key->key.x509->pk_algorithm,
                                key->key.x509->params,
                                key->key.x509->params_size, hash, signature);
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }
}

/**
 * gnutls_privkey_decrypt_data:
 * @key: Holds the key
 * @flags: zero for now
 * @ciphertext: holds the data to be decrypted
 * @plaintext: will contain the decrypted data, allocated with gnutls_malloc()
 *
 * This function will decrypt the given data using the algorithm
 * supported by the private key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 * negative error value.
 **/
int
gnutls_privkey_decrypt_data (gnutls_privkey_t key,
                             unsigned int flags,
                             const gnutls_datum_t * ciphertext,
                             gnutls_datum_t * plaintext)
{
  if (key->pk_algorithm != GNUTLS_PK_RSA)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  switch (key->type)
    {
#ifdef ENABLE_OPENPGP
    case GNUTLS_PRIVKEY_OPENPGP:
      return _gnutls_openpgp_privkey_decrypt_data (key->key.openpgp, flags,
                                                  ciphertext, plaintext);
#endif
    case GNUTLS_PRIVKEY_X509:
      return _gnutls_pkcs1_rsa_decrypt (plaintext, ciphertext,
                                        key->key.x509->params,
                                        key->key.x509->params_size, 2);
    case GNUTLS_PRIVKEY_PKCS11:
      return _gnutls_pkcs11_privkey_decrypt_data (key->key.pkcs11,
                                                 flags,
                                                 ciphertext, plaintext);
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }
}
