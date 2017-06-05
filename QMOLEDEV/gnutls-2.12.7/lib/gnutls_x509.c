/*
 * Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
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

#include <gnutls_int.h>
#include "gnutls_auth.h"
#include "gnutls_errors.h"
#include <gnutls_cert.h>
#include <auth_cert.h>
#include "gnutls_dh.h"
#include "gnutls_num.h"
#include "gnutls_datum.h"
#include <gnutls_pk.h>
#include <gnutls_algorithms.h>
#include <gnutls_global.h>
#include <gnutls_record.h>
#include <gnutls_sig.h>
#include <gnutls_state.h>
#include <gnutls_pk.h>
#include <gnutls_str.h>
#include <debug.h>
#include <x509_b64.h>
#include <gnutls_x509.h>
#include "x509/common.h"
#include "x509/x509_int.h"
#include "read-file.h"


/*
 * some x509 certificate parsing functions.
 */

/* Check if the number of bits of the key in the certificate
 * is unacceptable.
  */
inline static int
check_bits (gnutls_x509_crt_t crt, unsigned int max_bits)
{
  int ret;
  unsigned int bits;

  ret = gnutls_x509_crt_get_pk_algorithm (crt, &bits);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (bits > max_bits && max_bits > 0)
    {
      gnutls_assert ();
      return GNUTLS_E_CONSTRAINT_ERROR;
    }

  return 0;
}


#define CLEAR_CERTS for(x=0;x<peer_certificate_list_size;x++) { \
	if (peer_certificate_list[x]) \
		gnutls_x509_crt_deinit(peer_certificate_list[x]); \
	} \
	gnutls_free( peer_certificate_list)

/*-
 * _gnutls_x509_cert_verify_peers - return the peer's certificate status
 * @session: is a gnutls session
 *
 * This function will try to verify the peer's certificate and return its status (TRUSTED, REVOKED etc.).
 * The return value (status) should be one of the gnutls_certificate_status_t enumerated elements.
 * However you must also check the peer's name in order to check if the verified certificate belongs to the
 * actual peer. Returns a negative error code in case of an error, or GNUTLS_E_NO_CERTIFICATE_FOUND if no certificate was sent.
 -*/
int
_gnutls_x509_cert_verify_peers (gnutls_session_t session,
                                unsigned int *status)
{
  cert_auth_info_t info;
  gnutls_certificate_credentials_t cred;
  gnutls_x509_crt_t *peer_certificate_list;
  int peer_certificate_list_size, i, x, ret;

  CHECK_AUTH (GNUTLS_CRD_CERTIFICATE, GNUTLS_E_INVALID_REQUEST);

  info = _gnutls_get_auth_info (session);
  if (info == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  cred = (gnutls_certificate_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_CERTIFICATE, NULL);
  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  if (info->raw_certificate_list == NULL || info->ncerts == 0)
    return GNUTLS_E_NO_CERTIFICATE_FOUND;

  if (info->ncerts > cred->verify_depth && cred->verify_depth > 0)
    {
      gnutls_assert ();
      return GNUTLS_E_CONSTRAINT_ERROR;
    }

  /* generate a list of gnutls_certs based on the auth info
   * raw certs.
   */
  peer_certificate_list_size = info->ncerts;
  peer_certificate_list =
    gnutls_calloc (peer_certificate_list_size, sizeof (gnutls_x509_crt_t));
  if (peer_certificate_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  for (i = 0; i < peer_certificate_list_size; i++)
    {
      ret = gnutls_x509_crt_init (&peer_certificate_list[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          CLEAR_CERTS;
          return ret;
        }

      ret =
        gnutls_x509_crt_import (peer_certificate_list[i],
                                &info->raw_certificate_list[i],
                                GNUTLS_X509_FMT_DER);
      if (ret < 0)
        {
          gnutls_assert ();
          CLEAR_CERTS;
          return ret;
        }

      ret = check_bits (peer_certificate_list[i], cred->verify_bits);
      if (ret < 0)
        {
          gnutls_assert ();
          CLEAR_CERTS;
          return ret;
        }

    }

  /* Verify certificate 
   */

  ret = gnutls_x509_crt_list_verify (peer_certificate_list,
                                     peer_certificate_list_size,
                                     cred->x509_ca_list, cred->x509_ncas,
                                     cred->x509_crl_list, cred->x509_ncrls,
                                     cred->verify_flags | session->internals.
                                     priorities.additional_verify_flags,
                                     status);

  CLEAR_CERTS;

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/*
 * Read certificates and private keys, from files, memory etc.
 */

/* returns error if the certificate has different algorithm than
 * the given key parameters.
 */
static int
_gnutls_check_key_cert_match (gnutls_certificate_credentials_t res)
{
  unsigned int pk = res->cert_list[res->ncerts - 1][0].subject_pk_algorithm;

  if (gnutls_privkey_get_pk_algorithm (res->pkey[res->ncerts - 1], NULL) !=
      pk)
    {
      gnutls_assert ();
      return GNUTLS_E_CERTIFICATE_KEY_MISMATCH;
    }

  return 0;
}

/* Reads a DER encoded certificate list from memory and stores it to a
 * gnutls_cert structure. Returns the number of certificates parsed.
 */
static int
parse_der_cert_mem (gnutls_certificate_credentials_t res,
                    const void *input_cert, int input_cert_size)
{
  gnutls_datum_t tmp;
  gnutls_x509_crt_t crt;
  gnutls_cert *ccert;
  int ret;

  ccert = gnutls_malloc (sizeof (*ccert));
  if (ccert == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = gnutls_x509_crt_init (&crt);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  tmp.data = (opaque *) input_cert;
  tmp.size = input_cert_size;

  ret = gnutls_x509_crt_import (crt, &tmp, GNUTLS_X509_FMT_DER);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_x509_crt_deinit (crt);
      goto cleanup;
    }

  ret = _gnutls_x509_crt_to_gcert (ccert, crt, 0);
  gnutls_x509_crt_deinit (crt);

  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = certificate_credential_append_crt_list (res, ccert, 1);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  return ret;

cleanup:
  gnutls_free (ccert);
  return ret;
}

/* Reads a base64 encoded certificate list from memory and stores it to
 * a gnutls_cert structure. Returns the number of certificate parsed.
 */
static int
parse_pem_cert_mem (gnutls_certificate_credentials_t res,
                    const char *input_cert, int input_cert_size)
{
  int size, siz2;
  const char *ptr;
  opaque *ptr2;
  gnutls_datum_t tmp;
  int ret, count, i;
  gnutls_cert *certs = NULL;

  /* move to the certificate
   */
  ptr = memmem (input_cert, input_cert_size,
                PEM_CERT_SEP, sizeof (PEM_CERT_SEP) - 1);
  if (ptr == NULL)
    ptr = memmem (input_cert, input_cert_size,
                  PEM_CERT_SEP2, sizeof (PEM_CERT_SEP2) - 1);

  if (ptr == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_BASE64_DECODING_ERROR;
    }
  size = input_cert_size - (ptr - input_cert);

  count = 0;

  do
    {

      siz2 = _gnutls_fbase64_decode (NULL, ptr, size, &ptr2);
      if (siz2 < 0)
        {
          gnutls_assert ();
          ret = GNUTLS_E_BASE64_DECODING_ERROR;
          goto cleanup;
        }

      certs = gnutls_realloc_fast (certs, (count + 1) * sizeof (gnutls_cert));

      if (certs == NULL)
        {
          gnutls_assert ();
          ret = GNUTLS_E_MEMORY_ERROR;
          goto cleanup;
        }

      tmp.data = ptr2;
      tmp.size = siz2;

      ret = _gnutls_x509_raw_cert_to_gcert (&certs[count], &tmp, 0);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

      _gnutls_free_datum (&tmp);        /* free ptr2 */

      /* now we move ptr after the pem header 
       */
      ptr++;
      /* find the next certificate (if any)
       */
      size = input_cert_size - (ptr - input_cert);

      if (size > 0)
        {
          char *ptr3;

          ptr3 = memmem (ptr, size, PEM_CERT_SEP, sizeof (PEM_CERT_SEP) - 1);
          if (ptr3 == NULL)
            ptr3 = memmem (ptr, size, PEM_CERT_SEP2,
                           sizeof (PEM_CERT_SEP2) - 1);

          ptr = ptr3;
        }
      else
        ptr = NULL;

      count++;

    }
  while (ptr != NULL);

  ret = certificate_credential_append_crt_list (res, certs, count);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  return count;

cleanup:
  for (i=0;i<count;i++)
    _gnutls_gcert_deinit(&certs[i]);
  gnutls_free(certs);
  return ret;
}



/* Reads a DER or PEM certificate from memory
 */
static int
read_cert_mem (gnutls_certificate_credentials_t res, const void *cert,
               int cert_size, gnutls_x509_crt_fmt_t type)
{
  int ret;

  if (type == GNUTLS_X509_FMT_DER)
    ret = parse_der_cert_mem (res, cert, cert_size);
  else
    ret = parse_pem_cert_mem (res, cert, cert_size);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return ret;
}

static int
_gnutls_x509_raw_privkey_to_privkey (gnutls_privkey_t * privkey,
                                     const gnutls_datum_t * raw_key,
                                     gnutls_x509_crt_fmt_t type)
{
  gnutls_x509_privkey_t tmpkey;
  int ret;

  ret = gnutls_x509_privkey_init (&tmpkey);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_x509_privkey_import (tmpkey, raw_key, type);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_x509_privkey_deinit (tmpkey);
      return ret;
    }

  ret = gnutls_privkey_init (privkey);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_x509_privkey_deinit (tmpkey);
      return ret;
    }

  ret =
    gnutls_privkey_import_x509 (*privkey, tmpkey,
                                GNUTLS_PRIVKEY_IMPORT_AUTO_RELEASE);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_x509_privkey_deinit (tmpkey);
      gnutls_privkey_deinit (*privkey);
      return ret;
    }

  return 0;
}

/* Reads a PEM encoded PKCS-1 RSA/DSA private key from memory.  Type
 * indicates the certificate format.  KEY can be NULL, to indicate
 * that GnuTLS doesn't know the private key.
 */
static int
read_key_mem (gnutls_certificate_credentials_t res,
              const void *key, int key_size, gnutls_x509_crt_fmt_t type)
{
  int ret;
  gnutls_datum_t tmp;
  gnutls_privkey_t privkey;

  if (key)
    {
      tmp.data = (opaque *) key;
      tmp.size = key_size;

      ret = _gnutls_x509_raw_privkey_to_privkey (&privkey, &tmp, type);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      ret = certificate_credentials_append_pkey (res, privkey);
      if (ret < 0)
        {
          gnutls_assert ();
          gnutls_privkey_deinit (privkey);
          return ret;
        }

    }
  else
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }


  return 0;
}

/* Reads a private key from a token.
 */
static int
read_key_url (gnutls_certificate_credentials_t res, const char *url)
{
  int ret;
  gnutls_pkcs11_privkey_t key1 = NULL;
  gnutls_privkey_t pkey = NULL;

  /* allocate space for the pkey list
   */

  ret = gnutls_pkcs11_privkey_init (&key1);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_pkcs11_privkey_import_url (key1, url, 0);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = gnutls_privkey_init (&pkey);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret =
    gnutls_privkey_import_pkcs11 (pkey, key1,
                                  GNUTLS_PRIVKEY_IMPORT_AUTO_RELEASE);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = certificate_credentials_append_pkey (res, pkey);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  return 0;

cleanup:
  if (pkey)
    gnutls_privkey_deinit (pkey);

  if (key1)
    gnutls_pkcs11_privkey_deinit (key1);

  return ret;
}

/* Reads a private key from a token.
 */
static int
read_cas_url (gnutls_certificate_credentials_t res, const char *url)
{
  int ret;
  gnutls_x509_crt_t *xcrt_list = NULL;
  gnutls_pkcs11_obj_t *pcrt_list = NULL;
  unsigned int pcrt_list_size = 0;

  /* FIXME: should we use login? */
  ret =
    gnutls_pkcs11_obj_list_import_url (NULL, &pcrt_list_size, url,
                                       GNUTLS_PKCS11_OBJ_ATTR_CRT_TRUSTED, 0);
  if (ret < 0 && ret != GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      gnutls_assert ();
      return ret;
    }

  if (pcrt_list_size == 0)
    {
      gnutls_assert ();
      return 0;
    }

  pcrt_list = gnutls_malloc (sizeof (*pcrt_list) * pcrt_list_size);
  if (pcrt_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret =
    gnutls_pkcs11_obj_list_import_url (pcrt_list, &pcrt_list_size, url,
                                       GNUTLS_PKCS11_OBJ_ATTR_CRT_TRUSTED, 0);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  xcrt_list = gnutls_malloc (sizeof (*xcrt_list) * pcrt_list_size);
  if (xcrt_list == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  ret =
    gnutls_x509_crt_list_import_pkcs11 (xcrt_list, pcrt_list_size, pcrt_list,
                                        0);
  if (xcrt_list == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  res->x509_ca_list = xcrt_list;
  res->x509_ncas = pcrt_list_size;

  gnutls_free (pcrt_list);

  return pcrt_list_size;

cleanup:
  gnutls_free (xcrt_list);
  gnutls_free (pcrt_list);

  return ret;

}


/* Reads a private key from a token.
 */
static int
read_cert_url (gnutls_certificate_credentials_t res, const char *url)
{
  int ret;
  gnutls_x509_crt_t crt;
  gnutls_cert *ccert;

  ccert = gnutls_malloc (sizeof (*ccert));
  if (ccert == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = gnutls_x509_crt_init (&crt);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_free (ccert);
      return ret;
    }

  ret = gnutls_x509_crt_import_pkcs11_url (crt, url, 0);
  if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
    ret =
      gnutls_x509_crt_import_pkcs11_url (crt, url,
                                         GNUTLS_PKCS11_OBJ_FLAG_LOGIN);

  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_free (ccert);
      gnutls_x509_crt_deinit (crt);
      return ret;
    }

  ret = _gnutls_x509_crt_to_gcert (ccert, crt, 0);
  gnutls_x509_crt_deinit (crt);

  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_free (ccert);
      return ret;
    }

  ret = certificate_credential_append_crt_list (res, ccert, 1);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_free (ccert);
      return ret;
    }

  return 0;

}

/* Reads a certificate file
 */
static int
read_cert_file (gnutls_certificate_credentials_t res,
                const char *certfile, gnutls_x509_crt_fmt_t type)
{
  int ret;
  size_t size;
  char *data;

  if (strncmp (certfile, "pkcs11:", 7) == 0)
    {
      return read_cert_url (res, certfile);
    }

  data = read_binary_file (certfile, &size);

  if (data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  ret = read_cert_mem (res, data, size, type);
  free (data);

  return ret;

}



/* Reads PKCS-1 RSA private key file or a DSA file (in the format openssl
 * stores it).
 */
static int
read_key_file (gnutls_certificate_credentials_t res,
               const char *keyfile, gnutls_x509_crt_fmt_t type)
{
  int ret;
  size_t size;
  char *data;

  if (strncmp (keyfile, "pkcs11:", 7) == 0)
    {
      return read_key_url (res, keyfile);
    }

  data = read_binary_file (keyfile, &size);

  if (data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  ret = read_key_mem (res, data, size, type);
  free (data);

  return ret;
}

/**
 * gnutls_certificate_set_x509_key_mem:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @cert: contains a certificate list (path) for the specified private key
 * @key: is the private key, or %NULL
 * @type: is PEM or DER
 *
 * This function sets a certificate/private key pair in the
 * gnutls_certificate_credentials_t structure. This function may be called
 * more than once (in case multiple keys/certificates exist for the
 * server).
 *
 * Currently are supported: RSA PKCS-1 encoded private keys,
 * DSA private keys.
 *
 * DSA private keys are encoded the OpenSSL way, which is an ASN.1
 * DER sequence of 6 INTEGERs - version, p, q, g, pub, priv.
 *
 * Note that the keyUsage (2.5.29.15) PKIX extension in X.509 certificates
 * is supported. This means that certificates intended for signing cannot
 * be used for ciphersuites that require encryption.
 *
 * If the certificate and the private key are given in PEM encoding
 * then the strings that hold their values must be null terminated.
 *
 * The @key may be %NULL if you are using a sign callback, see
 * gnutls_sign_callback_set().
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_certificate_set_x509_key_mem (gnutls_certificate_credentials_t res,
                                     const gnutls_datum_t * cert,
                                     const gnutls_datum_t * key,
                                     gnutls_x509_crt_fmt_t type)
{
  int ret;

  /* this should be first
   */
  if ((ret = read_key_mem (res, key ? key->data : NULL,
                           key ? key->size : 0, type)) < 0)
    return ret;

  if ((ret = read_cert_mem (res, cert->data, cert->size, type)) < 0)
    return ret;

  res->ncerts++;

  if (key && (ret = _gnutls_check_key_cert_match (res)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

int
certificate_credential_append_crt_list (gnutls_certificate_credentials_t res,
                                        gnutls_cert * crt, int nr)
{
  res->cert_list = gnutls_realloc_fast (res->cert_list,
                                        (1 +
                                         res->ncerts) *
                                        sizeof (gnutls_cert *));
  if (res->cert_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  res->cert_list_length = gnutls_realloc_fast (res->cert_list_length,
                                               (1 +
                                                res->ncerts) * sizeof (int));
  if (res->cert_list_length == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  res->cert_list[res->ncerts] = crt;
  res->cert_list_length[res->ncerts] = nr;

  return 0;

}

int
certificate_credentials_append_pkey (gnutls_certificate_credentials_t res,
                                     gnutls_privkey_t pkey)
{
  res->pkey = gnutls_realloc_fast (res->pkey,
                                   (1 + res->ncerts) *
                                   sizeof (gnutls_privkey_t));
  if (res->pkey == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }
  res->pkey[res->ncerts] = pkey;
  return 0;

}

/**
 * gnutls_certificate_set_x509_key:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @cert_list: contains a certificate list (path) for the specified private key
 * @cert_list_size: holds the size of the certificate list
 * @key: is a gnutls_x509_privkey_t key
 *
 * This function sets a certificate/private key pair in the
 * gnutls_certificate_credentials_t structure.  This function may be
 * called more than once (in case multiple keys/certificates exist for
 * the server).  For clients that wants to send more than its own end
 * entity certificate (e.g., also an intermediate CA cert) then put
 * the certificate chain in @cert_list.
 *
 * 
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 *
 * Since: 2.4.0
 **/
int
gnutls_certificate_set_x509_key (gnutls_certificate_credentials_t res,
                                 gnutls_x509_crt_t * cert_list,
                                 int cert_list_size,
                                 gnutls_x509_privkey_t key)
{
  int ret, i;
  gnutls_privkey_t pkey;
  gnutls_cert *pcerts = NULL;

  /* this should be first
   */
  ret = gnutls_privkey_init (&pkey);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_privkey_import_x509 (pkey, key, 0);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = certificate_credentials_append_pkey (res, pkey);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* load certificates */
  pcerts = gnutls_malloc (sizeof (gnutls_cert) * cert_list_size);
  if (pcerts == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  for (i = 0; i < cert_list_size; i++)
    {
      ret = _gnutls_x509_crt_to_gcert (&pcerts[i], cert_list[i], 0);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
    }

  ret = certificate_credential_append_crt_list (res, pcerts, cert_list_size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  res->ncerts++;

  if ((ret = _gnutls_check_key_cert_match (res)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/**
 * gnutls_certificate_set_x509_key_file:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @certfile: is a file that containing the certificate list (path) for
 *   the specified private key, in PKCS7 format, or a list of certificates
 * @keyfile: is a file that contains the private key
 * @type: is PEM or DER
 *
 * This function sets a certificate/private key pair in the
 * gnutls_certificate_credentials_t structure.  This function may be
 * called more than once (in case multiple keys/certificates exist for
 * the server).  For clients that wants to send more than its own end
 * entity certificate (e.g., also an intermediate CA cert) then put
 * the certificate chain in @certfile.
 *
 * Currently only PKCS-1 encoded RSA and DSA private keys are accepted by
 * this function.
 *
 * This function can also accept PKCS #11 URLs. In that case it
 * will import the private key and certificate indicated by the urls.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_certificate_set_x509_key_file (gnutls_certificate_credentials_t res,
                                      const char *certfile,
                                      const char *keyfile,
                                      gnutls_x509_crt_fmt_t type)
{
  int ret;

  /* this should be first
   */
  if ((ret = read_key_file (res, keyfile, type)) < 0)
    return ret;

  if ((ret = read_cert_file (res, certfile, type)) < 0)
    return ret;

  res->ncerts++;

  if ((ret = _gnutls_check_key_cert_match (res)) < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

static int
add_new_crt_to_rdn_seq (gnutls_certificate_credentials_t res, int new)
{
  gnutls_datum_t tmp;
  int ret;
  size_t newsize;
  unsigned char *newdata;
  unsigned i;

  /* Add DN of the last added CAs to the RDN sequence
   * This will be sent to clients when a certificate
   * request message is sent.
   */

  /* FIXME: in case of a client it is not needed
   * to do that. This would save time and memory.
   * However we don't have that information available
   * here.
   * Further, this function is now much more efficient,
   * so optimizing that is less important.
   */

  for (i = res->x509_ncas - new; i < res->x509_ncas; i++)
    {
      if ((ret = gnutls_x509_crt_get_raw_dn (res->x509_ca_list[i], &tmp)) < 0)
        {
          gnutls_assert ();
          return ret;
        }

      newsize = res->x509_rdn_sequence.size + 2 + tmp.size;
      if (newsize < res->x509_rdn_sequence.size)
        {
          gnutls_assert ();
          _gnutls_free_datum (&tmp);
          return GNUTLS_E_SHORT_MEMORY_BUFFER;
        }

      newdata = gnutls_realloc (res->x509_rdn_sequence.data, newsize);
      if (newdata == NULL)
        {
          gnutls_assert ();
          _gnutls_free_datum (&tmp);
          return GNUTLS_E_MEMORY_ERROR;
        }

      _gnutls_write_datum16 (newdata + res->x509_rdn_sequence.size, tmp);
      _gnutls_free_datum (&tmp);

      res->x509_rdn_sequence.size = newsize;
      res->x509_rdn_sequence.data = newdata;
    }

  return 0;
}

/* Returns 0 if it's ok to use the gnutls_kx_algorithm_t with this 
 * certificate (uses the KeyUsage field). 
 */
int
_gnutls_check_key_usage (const gnutls_cert * cert, gnutls_kx_algorithm_t alg)
{
  unsigned int key_usage = 0;
  int encipher_type;

  if (cert == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  if (_gnutls_map_kx_get_cred (alg, 1) == GNUTLS_CRD_CERTIFICATE ||
      _gnutls_map_kx_get_cred (alg, 0) == GNUTLS_CRD_CERTIFICATE)
    {

      key_usage = cert->key_usage;

      encipher_type = _gnutls_kx_encipher_type (alg);

      if (key_usage != 0 && encipher_type != CIPHER_IGN)
        {
          /* If key_usage has been set in the certificate
           */

          if (encipher_type == CIPHER_ENCRYPT)
            {
              /* If the key exchange method requires an encipher
               * type algorithm, and key's usage does not permit
               * encipherment, then fail.
               */
              if (!(key_usage & GNUTLS_KEY_KEY_ENCIPHERMENT))
                {
                  gnutls_assert ();
                  return GNUTLS_E_KEY_USAGE_VIOLATION;
                }
            }

          if (encipher_type == CIPHER_SIGN)
            {
              /* The same as above, but for sign only keys
               */
              if (!(key_usage & GNUTLS_KEY_DIGITAL_SIGNATURE))
                {
                  gnutls_assert ();
                  return GNUTLS_E_KEY_USAGE_VIOLATION;
                }
            }
        }
    }
  return 0;
}



static int
parse_pem_ca_mem (gnutls_x509_crt_t ** cert_list, unsigned *ncerts,
                  const opaque * input_cert, int input_cert_size)
{
  int i, size;
  const opaque *ptr;
  gnutls_datum_t tmp;
  int ret, count;

  /* move to the certificate
   */
  ptr = memmem (input_cert, input_cert_size,
                PEM_CERT_SEP, sizeof (PEM_CERT_SEP) - 1);
  if (ptr == NULL)
    ptr = memmem (input_cert, input_cert_size,
                  PEM_CERT_SEP2, sizeof (PEM_CERT_SEP2) - 1);

  if (ptr == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_BASE64_DECODING_ERROR;
    }
  size = input_cert_size - (ptr - input_cert);

  i = *ncerts + 1;
  count = 0;

  do
    {

      *cert_list =
        (gnutls_x509_crt_t *) gnutls_realloc_fast (*cert_list,
                                                   i *
                                                   sizeof
                                                   (gnutls_x509_crt_t));

      if (*cert_list == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      ret = gnutls_x509_crt_init (&cert_list[0][i - 1]);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      tmp.data = (opaque *) ptr;
      tmp.size = size;

      ret =
        gnutls_x509_crt_import (cert_list[0][i - 1],
                                &tmp, GNUTLS_X509_FMT_PEM);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      /* now we move ptr after the pem header 
       */
      ptr++;
      size--;
      /* find the next certificate (if any)
       */

      if (size > 0)
        {
          char *ptr3;

          ptr3 = memmem (ptr, size, PEM_CERT_SEP, sizeof (PEM_CERT_SEP) - 1);
          if (ptr3 == NULL)
            ptr3 = memmem (ptr, size,
                           PEM_CERT_SEP2, sizeof (PEM_CERT_SEP2) - 1);

          ptr = ptr3;
          size = input_cert_size - (ptr - input_cert);
        }
      else
        ptr = NULL;

      i++;
      count++;

    }
  while (ptr != NULL);

  *ncerts = i - 1;

  return count;
}

/* Reads a DER encoded certificate list from memory and stores it to a
 * gnutls_cert structure.  Returns the number of certificates parsed.
 */
static int
parse_der_ca_mem (gnutls_x509_crt_t ** cert_list, unsigned *ncerts,
                  const void *input_cert, int input_cert_size)
{
  int i;
  gnutls_datum_t tmp;
  int ret;

  i = *ncerts + 1;

  *cert_list =
    (gnutls_x509_crt_t *) gnutls_realloc_fast (*cert_list,
                                               i *
                                               sizeof (gnutls_x509_crt_t));

  if (*cert_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  tmp.data = (opaque *) input_cert;
  tmp.size = input_cert_size;

  ret = gnutls_x509_crt_init (&cert_list[0][i - 1]);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    gnutls_x509_crt_import (cert_list[0][i - 1], &tmp, GNUTLS_X509_FMT_DER);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  *ncerts = i;

  return 1;                     /* one certificate parsed */
}

/**
 * gnutls_certificate_set_x509_trust_mem:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @ca: is a list of trusted CAs or a DER certificate
 * @type: is DER or PEM
 *
 * This function adds the trusted CAs in order to verify client or
 * server certificates. In case of a client this is not required to be
 * called if the certificates are not verified using
 * gnutls_certificate_verify_peers2().  This function may be called
 * multiple times.
 *
 * In case of a server the CAs set here will be sent to the client if
 * a certificate request is sent. This can be disabled using
 * gnutls_certificate_send_x509_rdn_sequence().
 *
 * Returns: the number of certificates processed or a negative value
 * on error.
 **/
int
gnutls_certificate_set_x509_trust_mem (gnutls_certificate_credentials_t res,
                                       const gnutls_datum_t * ca,
                                       gnutls_x509_crt_fmt_t type)
{
  int ret, ret2;

  if (type == GNUTLS_X509_FMT_DER)
    ret = parse_der_ca_mem (&res->x509_ca_list, &res->x509_ncas,
                            ca->data, ca->size);
  else
    ret = parse_pem_ca_mem (&res->x509_ca_list, &res->x509_ncas,
                            ca->data, ca->size);

  if ((ret2 = add_new_crt_to_rdn_seq (res, ret)) < 0)
    return ret2;

  return ret;
}

/**
 * gnutls_certificate_set_x509_trust:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @ca_list: is a list of trusted CAs
 * @ca_list_size: holds the size of the CA list
 *
 * This function adds the trusted CAs in order to verify client
 * or server certificates. In case of a client this is not required
 * to be called if the certificates are not verified using
 * gnutls_certificate_verify_peers2().
 * This function may be called multiple times.
 *
 * In case of a server the CAs set here will be sent to the client if
 * a certificate request is sent. This can be disabled using
 * gnutls_certificate_send_x509_rdn_sequence().
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 *
 * Since: 2.4.0
 **/
int
gnutls_certificate_set_x509_trust (gnutls_certificate_credentials_t res,
                                   gnutls_x509_crt_t * ca_list,
                                   int ca_list_size)
{
  int ret, i, ret2;

  res->x509_ca_list = gnutls_realloc_fast (res->x509_ca_list,
                                           (ca_list_size +
                                            res->x509_ncas) *
                                           sizeof (gnutls_x509_crt_t));
  if (res->x509_ca_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  for (i = 0; i < ca_list_size; i++)
    {
      ret = gnutls_x509_crt_init (&res->x509_ca_list[res->x509_ncas]);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      ret = _gnutls_x509_crt_cpy (res->x509_ca_list[res->x509_ncas],
                                  ca_list[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          gnutls_x509_crt_deinit (res->x509_ca_list[res->x509_ncas]);
          return ret;
        }
      res->x509_ncas++;
    }

  if ((ret2 = add_new_crt_to_rdn_seq (res, ca_list_size)) < 0)
    return ret2;

  return 0;
}

/**
 * gnutls_certificate_set_x509_trust_file:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @cafile: is a file containing the list of trusted CAs (DER or PEM list)
 * @type: is PEM or DER
 *
 * This function adds the trusted CAs in order to verify client or
 * server certificates. In case of a client this is not required to
 * be called if the certificates are not verified using
 * gnutls_certificate_verify_peers2().  This function may be called
 * multiple times.
 *
 * In case of a server the names of the CAs set here will be sent to
 * the client if a certificate request is sent. This can be disabled
 * using gnutls_certificate_send_x509_rdn_sequence().
 *
 * This function can also accept PKCS #11 URLs. In that case it
 * will import all certificates that are marked as trusted.
 *
 * Returns: number of certificates processed, or a negative value on
 * error.
 **/
int
gnutls_certificate_set_x509_trust_file (gnutls_certificate_credentials_t res,
                                        const char *cafile,
                                        gnutls_x509_crt_fmt_t type)
{
  int ret, ret2;
  size_t size;
  char *data;

  if (strncmp (cafile, "pkcs11:", 7) == 0)
    {
      return read_cas_url (res, cafile);
    }

  data = read_binary_file (cafile, &size);
  if (data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  if (type == GNUTLS_X509_FMT_DER)
    ret = parse_der_ca_mem (&res->x509_ca_list, &res->x509_ncas, data, size);
  else
    ret = parse_pem_ca_mem (&res->x509_ca_list, &res->x509_ncas, data, size);

  free (data);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if ((ret2 = add_new_crt_to_rdn_seq (res, ret)) < 0)
    return ret2;

  return ret;
}

#ifdef ENABLE_PKI

static int
parse_pem_crl_mem (gnutls_x509_crl_t ** crl_list, unsigned *ncrls,
                   const opaque * input_crl, int input_crl_size)
{
  int size, i;
  const opaque *ptr;
  gnutls_datum_t tmp;
  int ret, count;

  /* move to the certificate
   */
  ptr = memmem (input_crl, input_crl_size,
                PEM_CRL_SEP, sizeof (PEM_CRL_SEP) - 1);
  if (ptr == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_BASE64_DECODING_ERROR;
    }

  size = input_crl_size - (ptr - input_crl);

  i = *ncrls + 1;
  count = 0;

  do
    {

      *crl_list =
        (gnutls_x509_crl_t *) gnutls_realloc_fast (*crl_list,
                                                   i *
                                                   sizeof
                                                   (gnutls_x509_crl_t));

      if (*crl_list == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      ret = gnutls_x509_crl_init (&crl_list[0][i - 1]);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      tmp.data = (char *) ptr;
      tmp.size = size;

      ret =
        gnutls_x509_crl_import (crl_list[0][i - 1],
                                &tmp, GNUTLS_X509_FMT_PEM);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      /* now we move ptr after the pem header 
       */
      ptr++;
      /* find the next certificate (if any)
       */

      size = input_crl_size - (ptr - input_crl);

      if (size > 0)
        ptr = memmem (ptr, size, PEM_CRL_SEP, sizeof (PEM_CRL_SEP) - 1);
      else
        ptr = NULL;
      i++;
      count++;

    }
  while (ptr != NULL);

  *ncrls = i - 1;

  return count;
}

/* Reads a DER encoded certificate list from memory and stores it to a
 * gnutls_cert structure. Returns the number of certificates parsed.
 */
static int
parse_der_crl_mem (gnutls_x509_crl_t ** crl_list, unsigned *ncrls,
                   const void *input_crl, int input_crl_size)
{
  int i;
  gnutls_datum_t tmp;
  int ret;

  i = *ncrls + 1;

  *crl_list =
    (gnutls_x509_crl_t *) gnutls_realloc_fast (*crl_list,
                                               i *
                                               sizeof (gnutls_x509_crl_t));

  if (*crl_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  tmp.data = (opaque *) input_crl;
  tmp.size = input_crl_size;

  ret = gnutls_x509_crl_init (&crl_list[0][i - 1]);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    gnutls_x509_crl_import (crl_list[0][i - 1], &tmp, GNUTLS_X509_FMT_DER);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  *ncrls = i;

  return 1;                     /* one certificate parsed */
}


/* Reads a DER or PEM CRL from memory
 */
static int
read_crl_mem (gnutls_certificate_credentials_t res, const void *crl,
              int crl_size, gnutls_x509_crt_fmt_t type)
{
  int ret;

  /* allocate space for the certificate to add
   */
  res->x509_crl_list = gnutls_realloc_fast (res->x509_crl_list,
                                            (1 +
                                             res->x509_ncrls) *
                                            sizeof (gnutls_x509_crl_t));
  if (res->x509_crl_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  if (type == GNUTLS_X509_FMT_DER)
    ret = parse_der_crl_mem (&res->x509_crl_list,
                             &res->x509_ncrls, crl, crl_size);
  else
    ret = parse_pem_crl_mem (&res->x509_crl_list,
                             &res->x509_ncrls, crl, crl_size);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return ret;
}

/**
 * gnutls_certificate_set_x509_crl_mem:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @CRL: is a list of trusted CRLs. They should have been verified before.
 * @type: is DER or PEM
 *
 * This function adds the trusted CRLs in order to verify client or
 * server certificates.  In case of a client this is not required to
 * be called if the certificates are not verified using
 * gnutls_certificate_verify_peers2().  This function may be called
 * multiple times.
 *
 * Returns: number of CRLs processed, or a negative value on error.
 **/
int
gnutls_certificate_set_x509_crl_mem (gnutls_certificate_credentials_t res,
                                     const gnutls_datum_t * CRL,
                                     gnutls_x509_crt_fmt_t type)
{
  int ret;

  if ((ret = read_crl_mem (res, CRL->data, CRL->size, type)) < 0)
    return ret;

  return ret;
}

/**
 * gnutls_certificate_set_x509_crl:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @crl_list: is a list of trusted CRLs. They should have been verified before.
 * @crl_list_size: holds the size of the crl_list
 *
 * This function adds the trusted CRLs in order to verify client or
 * server certificates.  In case of a client this is not required to
 * be called if the certificates are not verified using
 * gnutls_certificate_verify_peers2().  This function may be called
 * multiple times.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 *
 * Since: 2.4.0
 **/
int
gnutls_certificate_set_x509_crl (gnutls_certificate_credentials_t res,
                                 gnutls_x509_crl_t * crl_list,
                                 int crl_list_size)
{
  int ret, i;

  res->x509_crl_list = gnutls_realloc_fast (res->x509_crl_list,
                                            (crl_list_size +
                                             res->x509_ncrls) *
                                            sizeof (gnutls_x509_crl_t));
  if (res->x509_crl_list == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  for (i = 0; i < crl_list_size; i++)
    {
      ret = gnutls_x509_crl_init (&res->x509_crl_list[res->x509_ncrls]);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      ret = _gnutls_x509_crl_cpy (res->x509_crl_list[res->x509_ncrls],
                                  crl_list[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
      res->x509_ncrls++;
    }

  return 0;
}

/**
 * gnutls_certificate_set_x509_crl_file:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @crlfile: is a file containing the list of verified CRLs (DER or PEM list)
 * @type: is PEM or DER
 *
 * This function adds the trusted CRLs in order to verify client or server
 * certificates.  In case of a client this is not required
 * to be called if the certificates are not verified using
 * gnutls_certificate_verify_peers2().
 * This function may be called multiple times.
 *
 * Returns: number of CRLs processed or a negative value on error.
 **/
int
gnutls_certificate_set_x509_crl_file (gnutls_certificate_credentials_t res,
                                      const char *crlfile,
                                      gnutls_x509_crt_fmt_t type)
{
  int ret;
  size_t size;
  char *data = read_binary_file (crlfile, &size);

  if (data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  if (type == GNUTLS_X509_FMT_DER)
    ret = parse_der_crl_mem (&res->x509_crl_list, &res->x509_ncrls,
                             data, size);
  else
    ret = parse_pem_crl_mem (&res->x509_crl_list, &res->x509_ncrls,
                             data, size);

  free (data);

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return ret;
}

#include <gnutls/pkcs12.h>

static int
parse_pkcs12 (gnutls_certificate_credentials_t res,
              gnutls_pkcs12_t p12,
              const char *password,
              gnutls_x509_privkey_t * key,
              gnutls_x509_crt_t * cert, gnutls_x509_crl_t * crl)
{
  gnutls_pkcs12_bag_t bag = NULL;
  int idx = 0;
  int ret;
  size_t cert_id_size = 0;
  size_t key_id_size = 0;
  opaque cert_id[20];
  opaque key_id[20];
  int privkey_ok = 0;

  *cert = NULL;
  *key = NULL;
  *crl = NULL;

  /* find the first private key */
  for (;;)
    {
      int elements_in_bag;
      int i;

      ret = gnutls_pkcs12_bag_init (&bag);
      if (ret < 0)
        {
          bag = NULL;
          gnutls_assert ();
          goto done;
        }

      ret = gnutls_pkcs12_get_bag (p12, idx, bag);
      if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
        break;
      if (ret < 0)
        {
          gnutls_assert ();
          goto done;
        }

      ret = gnutls_pkcs12_bag_get_type (bag, 0);
      if (ret < 0)
        {
          gnutls_assert ();
          goto done;
        }

      if (ret == GNUTLS_BAG_ENCRYPTED)
        {
          ret = gnutls_pkcs12_bag_decrypt (bag, password);
          if (ret < 0)
            {
              gnutls_assert ();
              goto done;
            }
        }

      elements_in_bag = gnutls_pkcs12_bag_get_count (bag);
      if (elements_in_bag < 0)
        {
          gnutls_assert ();
          goto done;
        }

      for (i = 0; i < elements_in_bag; i++)
        {
          int type;
          gnutls_datum_t data;

          type = gnutls_pkcs12_bag_get_type (bag, i);
          if (type < 0)
            {
              gnutls_assert ();
              goto done;
            }

          ret = gnutls_pkcs12_bag_get_data (bag, i, &data);
          if (ret < 0)
            {
              gnutls_assert ();
              goto done;
            }

          switch (type)
            {
            case GNUTLS_BAG_PKCS8_ENCRYPTED_KEY:
            case GNUTLS_BAG_PKCS8_KEY:
              if (*key != NULL) /* too simple to continue */
                {
                  gnutls_assert ();
                  break;
                }

              ret = gnutls_x509_privkey_init (key);
              if (ret < 0)
                {
                  gnutls_assert ();
                  goto done;
                }

              ret = gnutls_x509_privkey_import_pkcs8
                (*key, &data, GNUTLS_X509_FMT_DER, password,
                 type == GNUTLS_BAG_PKCS8_KEY ? GNUTLS_PKCS_PLAIN : 0);
              if (ret < 0)
                {
                  gnutls_assert ();
                  gnutls_x509_privkey_deinit (*key);
                  goto done;
                }

              key_id_size = sizeof (key_id);
              ret =
                gnutls_x509_privkey_get_key_id (*key, 0, key_id,
                                                &key_id_size);
              if (ret < 0)
                {
                  gnutls_assert ();
                  gnutls_x509_privkey_deinit (*key);
                  goto done;
                }

              privkey_ok = 1;   /* break */
              break;
            default:
              break;
            }
        }

      idx++;
      gnutls_pkcs12_bag_deinit (bag);

      if (privkey_ok != 0)      /* private key was found */
        break;
    }

  if (privkey_ok == 0)          /* no private key */
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  /* now find the corresponding certificate 
   */
  idx = 0;
  bag = NULL;
  for (;;)
    {
      int elements_in_bag;
      int i;

      ret = gnutls_pkcs12_bag_init (&bag);
      if (ret < 0)
        {
          bag = NULL;
          gnutls_assert ();
          goto done;
        }

      ret = gnutls_pkcs12_get_bag (p12, idx, bag);
      if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
        break;
      if (ret < 0)
        {
          gnutls_assert ();
          goto done;
        }

      ret = gnutls_pkcs12_bag_get_type (bag, 0);
      if (ret < 0)
        {
          gnutls_assert ();
          goto done;
        }

      if (ret == GNUTLS_BAG_ENCRYPTED)
        {
          ret = gnutls_pkcs12_bag_decrypt (bag, password);
          if (ret < 0)
            {
              gnutls_assert ();
              goto done;
            }
        }

      elements_in_bag = gnutls_pkcs12_bag_get_count (bag);
      if (elements_in_bag < 0)
        {
          gnutls_assert ();
          goto done;
        }

      for (i = 0; i < elements_in_bag; i++)
        {
          int type;
          gnutls_datum_t data;

          type = gnutls_pkcs12_bag_get_type (bag, i);
          if (type < 0)
            {
              gnutls_assert ();
              goto done;
            }

          ret = gnutls_pkcs12_bag_get_data (bag, i, &data);
          if (ret < 0)
            {
              gnutls_assert ();
              goto done;
            }

          switch (type)
            {
            case GNUTLS_BAG_CERTIFICATE:
              if (*cert != NULL)        /* no need to set it again */
                {
                  gnutls_assert ();
                  break;
                }

              ret = gnutls_x509_crt_init (cert);
              if (ret < 0)
                {
                  gnutls_assert ();
                  goto done;
                }

              ret =
                gnutls_x509_crt_import (*cert, &data, GNUTLS_X509_FMT_DER);
              if (ret < 0)
                {
                  gnutls_assert ();
                  gnutls_x509_crt_deinit (*cert);
                  goto done;
                }

              /* check if the key id match */
              cert_id_size = sizeof (cert_id);
              ret =
                gnutls_x509_crt_get_key_id (*cert, 0, cert_id, &cert_id_size);
              if (ret < 0)
                {
                  gnutls_assert ();
                  gnutls_x509_crt_deinit (*cert);
                  goto done;
                }

              if (memcmp (cert_id, key_id, cert_id_size) != 0)
                {               /* they don't match - skip the certificate */
                  gnutls_x509_crt_deinit (*cert);
                  *cert = NULL;
                }
              break;

            case GNUTLS_BAG_CRL:
              if (*crl != NULL)
                {
                  gnutls_assert ();
                  break;
                }

              ret = gnutls_x509_crl_init (crl);
              if (ret < 0)
                {
                  gnutls_assert ();
                  goto done;
                }

              ret = gnutls_x509_crl_import (*crl, &data, GNUTLS_X509_FMT_DER);
              if (ret < 0)
                {
                  gnutls_assert ();
                  gnutls_x509_crl_deinit (*crl);
                  goto done;
                }
              break;

            case GNUTLS_BAG_ENCRYPTED:
              /* XXX Bother to recurse one level down?  Unlikely to
                 use the same password anyway. */
            case GNUTLS_BAG_EMPTY:
            default:
              break;
            }
        }

      idx++;
      gnutls_pkcs12_bag_deinit (bag);
    }

  ret = 0;

done:
  if (bag)
    gnutls_pkcs12_bag_deinit (bag);

  return ret;
}

/**
 * gnutls_certificate_set_x509_simple_pkcs12_file:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @pkcs12file: filename of file containing PKCS#12 blob.
 * @type: is PEM or DER of the @pkcs12file.
 * @password: optional password used to decrypt PKCS#12 file, bags and keys.
 *
 * This function sets a certificate/private key pair and/or a CRL in
 * the gnutls_certificate_credentials_t structure.  This function may
 * be called more than once (in case multiple keys/certificates exist
 * for the server).
 *
 * MAC:ed PKCS#12 files are supported.  Encrypted PKCS#12 bags are
 * supported.  Encrypted PKCS#8 private keys are supported.  However,
 * only password based security, and the same password for all
 * operations, are supported.
 *
 * The private keys may be RSA PKCS#1 or DSA private keys encoded in
 * the OpenSSL way.
 *
 * PKCS#12 file may contain many keys and/or certificates, and there
 * is no way to identify which key/certificate pair you want.  You
 * should make sure the PKCS#12 file only contain one key/certificate
 * pair and/or one CRL.
 *
 * It is believed that the limitations of this function is acceptable
 * for most usage, and that any more flexibility would introduce
 * complexity that would make it harder to use this functionality at
 * all.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
  gnutls_certificate_set_x509_simple_pkcs12_file
  (gnutls_certificate_credentials_t res, const char *pkcs12file,
   gnutls_x509_crt_fmt_t type, const char *password)
{
  gnutls_datum_t p12blob;
  size_t size;
  int ret;

  p12blob.data = read_binary_file (pkcs12file, &size);
  p12blob.size = (unsigned int) size;
  if (p12blob.data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  ret =
    gnutls_certificate_set_x509_simple_pkcs12_mem (res, &p12blob, type,
                                                   password);
  free (p12blob.data);

  return ret;
}

/**
 * gnutls_certificate_set_x509_simple_pkcs12_mem:
 * @res: is a #gnutls_certificate_credentials_t structure.
 * @p12blob: the PKCS#12 blob.
 * @type: is PEM or DER of the @pkcs12file.
 * @password: optional password used to decrypt PKCS#12 file, bags and keys.
 *
 * This function sets a certificate/private key pair and/or a CRL in
 * the gnutls_certificate_credentials_t structure.  This function may
 * be called more than once (in case multiple keys/certificates exist
 * for the server).
 *
 * MAC:ed PKCS#12 files are supported.  Encrypted PKCS#12 bags are
 * supported.  Encrypted PKCS#8 private keys are supported.  However,
 * only password based security, and the same password for all
 * operations, are supported.
 *
 * The private keys may be RSA PKCS#1 or DSA private keys encoded in
 * the OpenSSL way.
 *
 * PKCS#12 file may contain many keys and/or certificates, and there
 * is no way to identify which key/certificate pair you want.  You
 * should make sure the PKCS#12 file only contain one key/certificate
 * pair and/or one CRL.
 *
 * It is believed that the limitations of this function is acceptable
 * for most usage, and that any more flexibility would introduce
 * complexity that would make it harder to use this functionality at
 * all.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 *
 * Since: 2.8.0
 **/
int
  gnutls_certificate_set_x509_simple_pkcs12_mem
  (gnutls_certificate_credentials_t res, const gnutls_datum_t * p12blob,
   gnutls_x509_crt_fmt_t type, const char *password)
{
  gnutls_pkcs12_t p12;
  gnutls_x509_privkey_t key = NULL;
  gnutls_x509_crt_t cert = NULL;
  gnutls_x509_crl_t crl = NULL;
  int ret;

  ret = gnutls_pkcs12_init (&p12);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_pkcs12_import (p12, p12blob, type, 0);
  if (ret < 0)
    {
      gnutls_assert ();
      gnutls_pkcs12_deinit (p12);
      return ret;
    }

  if (password)
    {
      ret = gnutls_pkcs12_verify_mac (p12, password);
      if (ret < 0)
        {
          gnutls_assert ();
          gnutls_pkcs12_deinit (p12);
          return ret;
        }
    }

  ret = parse_pkcs12 (res, p12, password, &key, &cert, &crl);
  gnutls_pkcs12_deinit (p12);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (key && cert)
    {
      ret = gnutls_certificate_set_x509_key (res, &cert, 1, key);
      if (ret < 0)
        {
          gnutls_assert ();
          goto done;
        }
    }

  if (crl)
    {
      ret = gnutls_certificate_set_x509_crl (res, &crl, 1);
      if (ret < 0)
        {
          gnutls_assert ();
          goto done;
        }
    }

  ret = 0;

done:
  if (cert)
    gnutls_x509_crt_deinit (cert);
  if (key)
    gnutls_x509_privkey_deinit (key);
  if (crl)
    gnutls_x509_crl_deinit (crl);

  return ret;
}



/**
 * gnutls_certificate_free_crls:
 * @sc: is a #gnutls_certificate_credentials_t structure.
 *
 * This function will delete all the CRLs associated
 * with the given credentials.
 **/
void
gnutls_certificate_free_crls (gnutls_certificate_credentials_t sc)
{
  unsigned j;

  for (j = 0; j < sc->x509_ncrls; j++)
    {
      gnutls_x509_crl_deinit (sc->x509_crl_list[j]);
    }

  sc->x509_ncrls = 0;

  gnutls_free (sc->x509_crl_list);
  sc->x509_crl_list = NULL;
}

#endif
