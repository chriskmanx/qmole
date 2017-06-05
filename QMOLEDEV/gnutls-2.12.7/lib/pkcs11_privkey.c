/*
 * GnuTLS PKCS#11 support
 * Copyright (C) 2010,2011 Free Software Foundation
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
#include <gnutls_sig.h>

struct gnutls_pkcs11_privkey_st
{
  gnutls_pk_algorithm_t pk_algorithm;
  unsigned int flags;
  struct pkcs11_url_info info;
};

/**
 * gnutls_pkcs11_privkey_init:
 * @key: The structure to be initialized
 *
 * This function will initialize an private key structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_privkey_init (gnutls_pkcs11_privkey_t * key)
{
  *key = gnutls_calloc (1, sizeof (struct gnutls_pkcs11_privkey_st));
  if (*key == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;
}

/**
 * gnutls_pkcs11_privkey_deinit:
 * @key: The structure to be initialized
 *
 * This function will deinitialize a private key structure.
 **/
void
gnutls_pkcs11_privkey_deinit (gnutls_pkcs11_privkey_t key)
{
  gnutls_free (key);
}

/**
 * gnutls_pkcs11_privkey_get_pk_algorithm:
 * @key: should contain a #gnutls_pkcs11_privkey_t structure
 *
 * This function will return the public key algorithm of a private
 * key.
 *
 * Returns: a member of the #gnutls_pk_algorithm_t enumeration on
 *   success, or a negative value on error.
 **/
int
gnutls_pkcs11_privkey_get_pk_algorithm (gnutls_pkcs11_privkey_t key,
                                        unsigned int *bits)
{
  if (bits)
    *bits = 0;                  /* FIXME */
  return key->pk_algorithm;
}

/**
 * gnutls_pkcs11_privkey_get_info:
 * @pkey: should contain a #gnutls_pkcs11_privkey_t structure
 * @itype: Denotes the type of information requested
 * @output: where output will be stored
 * @output_size: contains the maximum size of the output and will be overwritten with actual
 *
 * This function will return information about the PKCS 11 private key such
 * as the label, id as well as token information where the key is stored. When
 * output is text it returns null terminated string although #output_size contains
 * the size of the actual data only.
 *
 * Returns: zero on success or a negative value on error.
 **/
int
gnutls_pkcs11_privkey_get_info (gnutls_pkcs11_privkey_t pkey,
                                gnutls_pkcs11_obj_info_t itype,
                                void *output, size_t * output_size)
{
  return pkcs11_get_info (&pkey->info, itype, output, output_size);
}


#define FIND_OBJECT(pks, obj, key) \
	do { \
		int retries = 0; \
		int rret; \
		ret = pkcs11_find_object (&pks, &obj, &key->info, \
			SESSION_LOGIN); \
		if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE) { \
			if (token_func) \
			  { \
			    rret = token_func(token_data, key->info.token, retries++); \
  			    if (rret == 0) continue; \
                          } \
			gnutls_assert(); \
			return ret; \
		} \
	} while (ret < 0);

/*-
 * _gnutls_pkcs11_privkey_sign_hash:
 * @key: Holds the key
 * @hash: holds the data to be signed (should be output of a hash)
 * @signature: will contain the signature allocated with gnutls_malloc()
 *
 * This function will sign the given data using a signature algorithm
 * supported by the private key. It is assumed that the given data
 * are the output of a hash function.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 -*/
int
_gnutls_pkcs11_privkey_sign_hash (gnutls_pkcs11_privkey_t key,
                                  const gnutls_datum_t * hash,
                                  gnutls_datum_t * signature)
{
  ck_rv_t rv;
  int ret;
  struct ck_mechanism mech;
  unsigned long siglen;
  pakchois_session_t *pks;
  ck_object_handle_t obj;

  FIND_OBJECT (pks, obj, key);

  mech.mechanism =
    key->pk_algorithm == GNUTLS_PK_DSA ? CKM_DSA : CKM_RSA_PKCS;
  mech.parameter = NULL;
  mech.parameter_len = 0;

  /* Initialize signing operation; using the private key discovered
   * earlier. */
  rv = pakchois_sign_init (pks, &mech, obj);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  /* Work out how long the signature must be: */
  rv = pakchois_sign (pks, hash->data, hash->size, NULL, &siglen);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  signature->data = gnutls_malloc (siglen);
  signature->size = siglen;

  rv = pakchois_sign (pks, hash->data, hash->size, signature->data, &siglen);
  if (rv != CKR_OK)
    {
      gnutls_free (signature->data);
      gnutls_assert ();
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  signature->size = siglen;

  ret = 0;

cleanup:
  pakchois_close_session (pks);

  return ret;
}

/**
 * gnutls_pkcs11_privkey_import_url:
 * @pkey: The structure to store the parsed key
 * @url: a PKCS 11 url identifying the key
 * @flags: sequence of GNUTLS_PKCS_PRIVKEY_*
 *
 * This function will "import" a PKCS 11 URL identifying a private
 * key to the #gnutls_pkcs11_privkey_t structure. In reality since
 * in most cases keys cannot be exported, the private key structure
 * is being associated with the available operations on the token.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_privkey_import_url (gnutls_pkcs11_privkey_t pkey,
                                  const char *url, unsigned int flags)
{
  int ret;
  pakchois_session_t *pks;
  ck_object_handle_t obj;
  struct ck_attribute a[4];
  ck_key_type_t key_type;

  ret = pkcs11_url_to_info (url, &pkey->info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  pkey->flags = flags;

  if (pkey->info.type[0] != 0 && strcmp (pkey->info.type, "private") != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (pkey->info.id[0] == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  FIND_OBJECT (pks, obj, pkey);
  a[0].type = CKA_KEY_TYPE;
  a[0].value = &key_type;
  a[0].value_len = sizeof (key_type);

  if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
    {
      switch (key_type)
        {
        case CKK_RSA:
          pkey->pk_algorithm = GNUTLS_PK_RSA;
          break;
        case CKK_DSA:
          pkey->pk_algorithm = GNUTLS_PK_DSA;
          break;
        default:
          _gnutls_debug_log("Cannot determine PKCS #11 key algorithm\n");
          ret = GNUTLS_E_UNKNOWN_ALGORITHM;
          goto cleanup;
        }
    }

  ret = 0;

cleanup:
  pakchois_close_session (pks);

  return ret;
}

/*-
 * _gnutls_pkcs11_privkey_decrypt_data:
 * @key: Holds the key
 * @flags: should be 0 for now
 * @ciphertext: holds the data to be signed
 * @plaintext: will contain the plaintext, allocated with gnutls_malloc()
 *
 * This function will decrypt the given data using the public key algorithm
 * supported by the private key. 
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 -*/
int
_gnutls_pkcs11_privkey_decrypt_data (gnutls_pkcs11_privkey_t key,
                                    unsigned int flags,
                                    const gnutls_datum_t * ciphertext,
                                    gnutls_datum_t * plaintext)
{
  ck_rv_t rv;
  int ret;
  struct ck_mechanism mech;
  unsigned long siglen;
  pakchois_session_t *pks;
  ck_object_handle_t obj;

  FIND_OBJECT (pks, obj, key);

  mech.mechanism =
    key->pk_algorithm == GNUTLS_PK_DSA ? CKM_DSA : CKM_RSA_PKCS;
  mech.parameter = NULL;
  mech.parameter_len = 0;

  /* Initialize signing operation; using the private key discovered
   * earlier. */
  rv = pakchois_decrypt_init (pks, &mech, obj);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  /* Work out how long the plaintext must be: */
  rv = pakchois_decrypt (pks, ciphertext->data, ciphertext->size,
                         NULL, &siglen);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  plaintext->data = gnutls_malloc (siglen);
  plaintext->size = siglen;

  rv = pakchois_decrypt (pks, ciphertext->data, ciphertext->size,
                         plaintext->data, &siglen);
  if (rv != CKR_OK)
    {
      gnutls_free (plaintext->data);
      gnutls_assert ();
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  plaintext->size = siglen;

  ret = 0;

cleanup:
  pakchois_close_session (pks);

  return ret;
}

/**
 * gnutls_pkcs11_privkey_export_url:
 * @key: Holds the PKCS 11 key
 * @detailed: non zero if a detailed URL is required
 * @url: will contain an allocated url
 *
 * This function will export a URL identifying the given key.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_privkey_export_url (gnutls_pkcs11_privkey_t key,
                                  gnutls_pkcs11_url_type_t detailed,
                                  char **url)
{
  int ret;

  ret = pkcs11_info_to_url (&key->info, detailed, url);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}
