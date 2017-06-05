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
#include <gnutls/pkcs11.h>
#include <stdio.h>
#include <string.h>
#include <gnutls_errors.h>
#include <gnutls_datum.h>
#include <pkcs11_int.h>

/**
 * gnutls_pkcs11_copy_x509_crt:
 * @token_url: A PKCS #11 URL specifying a token
 * @crt: A certificate
 * @label: A name to be used for the stored data
 * @flags: One of GNUTLS_PKCS11_OBJ_FLAG_*
 *
 * This function will copy a certificate into a PKCS #11 token specified by
 * a URL. The certificate can be marked as trusted or not.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_copy_x509_crt (const char *token_url,
                             gnutls_x509_crt_t crt, const char *label,
                             unsigned int flags)
{
  int ret;
  pakchois_session_t *pks;
  struct pkcs11_url_info info;
  ck_rv_t rv;
  size_t der_size, id_size;
  opaque *der = NULL;
  opaque id[20];
  struct ck_attribute a[16];
  ck_object_class_t class = CKO_CERTIFICATE;
  ck_certificate_type_t type = CKC_X_509;
  ck_object_handle_t obj;
  ck_bool_t tval = 1;
  ck_bool_t fval = 0;
  int a_val;
  gnutls_datum_t subject = { NULL, 0 };

  ret = pkcs11_url_to_info (token_url, &info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    pkcs11_open_session (&pks, &info,
                         SESSION_WRITE | pkcs11_obj_flags_to_int (flags));
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_x509_crt_export (crt, GNUTLS_X509_FMT_DER, NULL, &der_size);
  if (ret < 0 && ret != GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      gnutls_assert ();
      goto cleanup;
    }

  der = gnutls_malloc (der_size);
  if (der == NULL)
    {
      gnutls_assert ();
      ret = GNUTLS_E_MEMORY_ERROR;
      goto cleanup;
    }

  ret = gnutls_x509_crt_export (crt, GNUTLS_X509_FMT_DER, der, &der_size);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  id_size = sizeof (id);
  ret = gnutls_x509_crt_get_key_id (crt, 0, id, &id_size);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }
  
  ret = gnutls_x509_crt_get_raw_dn (crt, &subject);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* FIXME: copy key usage flags */

  a[0].type = CKA_CLASS;
  a[0].value = &class;
  a[0].value_len = sizeof (class);
  a[1].type = CKA_ID;
  a[1].value = id;
  a[1].value_len = id_size;
  a[2].type = CKA_VALUE;
  a[2].value = der;
  a[2].value_len = der_size;
  a[3].type = CKA_TOKEN;
  a[3].value = &tval;
  a[3].value_len = sizeof (tval);
  a[4].type = CKA_CERTIFICATE_TYPE;
  a[4].value = &type;
  a[4].value_len = sizeof (type);

  a_val = 5;

  a[a_val].type = CKA_SUBJECT;
  a[a_val].value = subject.data;
  a[a_val].value_len = subject.size;
  a_val++;


  if (label)
    {
      a[a_val].type = CKA_LABEL;
      a[a_val].value = (void *) label;
      a[a_val].value_len = strlen (label);
      a_val++;
    }

  if (flags & GNUTLS_PKCS11_OBJ_FLAG_MARK_TRUSTED)
    {
      a[a_val].type = CKA_TRUSTED;
      a[a_val].value = &tval;
      a[a_val].value_len = sizeof (tval);
      a_val++;

      a[a_val].type = CKA_PRIVATE;
      a[a_val].value = &fval;
      a[a_val].value_len = sizeof(fval);
      a_val++;
    }

  rv = pakchois_create_object (pks, a, a_val, &obj);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pkcs11: %s\n", pakchois_error (rv));
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  /* generated! 
   */

  ret = 0;

cleanup:
  gnutls_free (der);
  _gnutls_free_datum(&subject);
  pakchois_close_session (pks);

  return ret;

}

/**
 * gnutls_pkcs11_copy_x509_privkey:
 * @token_url: A PKCS #11 URL specifying a token
 * @key: A private key
 * @label: A name to be used for the stored data
 * @key_usage: One of GNUTLS_KEY_*
 * @flags: One of GNUTLS_PKCS11_OBJ_* flags
 *
 * This function will copy a private key into a PKCS #11 token specified by
 * a URL. It is highly recommended flags to contain %GNUTLS_PKCS11_OBJ_FLAG_MARK_SENSITIVE
 * unless there is a strong reason not to.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_copy_x509_privkey (const char *token_url,
                                 gnutls_x509_privkey_t key,
                                 const char *label,
                                 unsigned int key_usage, unsigned int flags)
{
  int ret;
  pakchois_session_t *pks = NULL;
  struct pkcs11_url_info info;
  ck_rv_t rv;
  size_t id_size;
  opaque id[20];
  struct ck_attribute a[16];
  ck_object_class_t class = CKO_PRIVATE_KEY;
  ck_object_handle_t obj;
  ck_key_type_t type;
  ck_bool_t tval = 1;
  int a_val;
  gnutls_pk_algorithm_t pk;
  gnutls_datum_t p, q, g, y, x;
  gnutls_datum_t m, e, d, u, exp1, exp2;


  ret = pkcs11_url_to_info (token_url, &info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  id_size = sizeof (id);
  ret = gnutls_x509_privkey_get_key_id (key, 0, id, &id_size);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret =
    pkcs11_open_session (&pks, &info,
                         SESSION_WRITE | pkcs11_obj_flags_to_int (flags));
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* FIXME: copy key usage flags */
  a_val = 0;
  a[a_val].type = CKA_CLASS;
  a[a_val].value = &class;
  a[a_val].value_len = sizeof (class);
  a_val++;

  a[a_val].type = CKA_ID;
  a[a_val].value = id;
  a[a_val].value_len = id_size;
  a_val++;

  a[a_val].type = CKA_KEY_TYPE;
  a[a_val].value = &type;
  a[a_val].value_len = sizeof (type);
  a_val++;

  a[a_val].type = CKA_TOKEN;
  a[a_val].value = &tval;
  a[a_val].value_len = sizeof (tval);
  a_val++;

  a[a_val].type = CKA_PRIVATE;
  a[a_val].value = &tval;
  a[a_val].value_len = sizeof (tval);
  a_val++;

  if (label)
    {
      a[a_val].type = CKA_LABEL;
      a[a_val].value = (void *) label;
      a[a_val].value_len = strlen (label);
      a_val++;
    }

  if (flags & GNUTLS_PKCS11_OBJ_FLAG_MARK_SENSITIVE)
    tval = 1;
  else
    tval = 0;

  a[a_val].type = CKA_SENSITIVE;
  a[a_val].value = &tval;
  a[a_val].value_len = sizeof (tval);
  a_val++;

  pk = gnutls_x509_privkey_get_pk_algorithm (key);
  switch (pk)
    {
    case GNUTLS_PK_RSA:
      {

        ret =
          gnutls_x509_privkey_export_rsa_raw2 (key, &m,
                                               &e, &d, &p,
                                               &q, &u, &exp1, &exp2);
        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }

        type = CKK_RSA;

        a[a_val].type = CKA_MODULUS;
        a[a_val].value = m.data;
        a[a_val].value_len = m.size;
        a_val++;

        a[a_val].type = CKA_PUBLIC_EXPONENT;
        a[a_val].value = e.data;
        a[a_val].value_len = e.size;
        a_val++;

        a[a_val].type = CKA_PRIVATE_EXPONENT;
        a[a_val].value = d.data;
        a[a_val].value_len = d.size;
        a_val++;

        a[a_val].type = CKA_PRIME_1;
        a[a_val].value = p.data;
        a[a_val].value_len = p.size;
        a_val++;

        a[a_val].type = CKA_PRIME_2;
        a[a_val].value = q.data;
        a[a_val].value_len = q.size;
        a_val++;

        a[a_val].type = CKA_COEFFICIENT;
        a[a_val].value = u.data;
        a[a_val].value_len = u.size;
        a_val++;

        a[a_val].type = CKA_EXPONENT_1;
        a[a_val].value = exp1.data;
        a[a_val].value_len = exp1.size;
        a_val++;

        a[a_val].type = CKA_EXPONENT_2;
        a[a_val].value = exp2.data;
        a[a_val].value_len = exp2.size;
        a_val++;

        break;
      }
    case GNUTLS_PK_DSA:
      {
        ret = gnutls_x509_privkey_export_dsa_raw (key, &p, &q, &g, &y, &x);
        if (ret < 0)
          {
            gnutls_assert ();
            goto cleanup;
          }

        type = CKK_DSA;

        a[a_val].type = CKA_PRIME;
        a[a_val].value = p.data;
        a[a_val].value_len = p.size;
        a_val++;

        a[a_val].type = CKA_SUBPRIME;
        a[a_val].value = q.data;
        a[a_val].value_len = q.size;
        a_val++;

        a[a_val].type = CKA_BASE;
        a[a_val].value = g.data;
        a[a_val].value_len = g.size;
        a_val++;

        a[a_val].type = CKA_VALUE;
        a[a_val].value = x.data;
        a[a_val].value_len = x.size;
        a_val++;

        break;
      }
    default:
      gnutls_assert ();
      ret = GNUTLS_E_INVALID_REQUEST;
      goto cleanup;
    }

  rv = pakchois_create_object (pks, a, a_val, &obj);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pkcs11: %s\n", pakchois_error (rv));
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  /* generated! 
   */

  switch (pk)
    {
    case GNUTLS_PK_RSA:
      {
        gnutls_free (m.data);
        gnutls_free (e.data);
        gnutls_free (d.data);
        gnutls_free (p.data);
        gnutls_free (q.data);
        gnutls_free (u.data);
        gnutls_free (exp1.data);
        gnutls_free (exp2.data);
        break;
      }
    case GNUTLS_PK_DSA:
      {
        gnutls_free (p.data);
        gnutls_free (q.data);
        gnutls_free (g.data);
        gnutls_free (y.data);
        gnutls_free (x.data);
        break;
      }
    default:
      gnutls_assert ();
      ret = GNUTLS_E_INVALID_REQUEST;
      goto cleanup;
    }

  ret = 0;

cleanup:
  if (pks != NULL)
    pakchois_close_session (pks);

  return ret;

}

struct delete_data_st
{
  struct pkcs11_url_info info;
  unsigned int deleted;         /* how many */
};

static int
delete_obj_url (pakchois_session_t * pks,
                struct token_info *info,
                struct ck_info *lib_info, void *input)
{
  struct delete_data_st *find_data = input;
  struct ck_attribute a[4];
  ck_object_class_t class;
  ck_certificate_type_t type = -1;
  ck_rv_t rv;
  ck_object_handle_t obj;
  unsigned long count, a_vals;
  int found = 0, ret;


  if (info == NULL)
    {                           /* we don't support multiple calls */
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  /* do not bother reading the token if basic fields do not match
   */
  if (pkcs11_token_matches_info (&find_data->info, &info->tinfo, lib_info) <
      0)
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  class = CKO_CERTIFICATE;      /* default  */

  if (find_data->info.type[0] != 0)
    {
      class = pkcs11_strtype_to_class (find_data->info.type);
      if (class == CKO_CERTIFICATE)
        type = CKC_X_509;

      if (class == -1)
        {
          gnutls_assert ();
          return GNUTLS_E_INVALID_REQUEST;
        }
    }

  a_vals = 0;

  /* Find objects with given class and type */
  if (find_data->info.certid_raw_size > 0)
    {
      a[a_vals].type = CKA_ID;
      a[a_vals].value = find_data->info.certid_raw;
      a[a_vals].value_len = find_data->info.certid_raw_size;
      a_vals++;
    }

  if (class != -1)
    {
      a[a_vals].type = CKA_CLASS;
      a[a_vals].value = &class;
      a[a_vals].value_len = sizeof class;
      a_vals++;
    }

  if (type != -1)
    {
      a[a_vals].type = CKA_CERTIFICATE_TYPE;
      a[a_vals].value = &type;
      a[a_vals].value_len = sizeof type;
      a_vals++;
    }

  if (find_data->info.label[0] != 0)
    {
      a[a_vals].type = CKA_LABEL;
      a[a_vals].value = find_data->info.label;
      a[a_vals].value_len = strlen (find_data->info.label);
      a_vals++;
    }

  rv = pakchois_find_objects_init (pks, a, a_vals);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pk11: FindObjectsInit failed.\n");
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  while (pakchois_find_objects (pks, &obj, 1, &count) == CKR_OK && count == 1)
    {
      rv = pakchois_destroy_object (pks, obj);
      if (rv != CKR_OK)
        {
          _gnutls_debug_log
            ("pkcs11: Cannot destroy object: %s\n", pakchois_error (rv));
        }
      else
        {
          find_data->deleted++;
        }

      found = 1;
    }

  if (found == 0)
    {
      gnutls_assert ();
      ret = GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }
  else
    {
      ret = 0;
    }

cleanup:
  pakchois_find_objects_final (pks);

  return ret;
}


/**
 * gnutls_pkcs11_delete_url:
 * @object_url: The URL of the object to delete.
 * @flags: One of GNUTLS_PKCS11_OBJ_* flags
 * 
 * This function will delete objects matching the given URL.
 *
 * Returns: On success, the number of objects deleted is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_delete_url (const char *object_url, unsigned int flags)
{
  int ret;
  struct delete_data_st find_data;

  memset (&find_data, 0, sizeof (find_data));

  ret = pkcs11_url_to_info (object_url, &find_data.info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    _pkcs11_traverse_tokens (delete_obj_url, &find_data,
                             SESSION_WRITE | pkcs11_obj_flags_to_int (flags));
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return find_data.deleted;

}

/**
 * gnutls_pkcs11_token_init:
 * @token_url: A PKCS #11 URL specifying a token
 * @so_pin: Security Officer's PIN
 * @label: A name to be used for the token
 *
 * This function will initialize (format) a token. If the token is
 * at a factory defaults state the security officer's PIN given will be
 * set to be the default. Otherwise it should match the officer's PIN.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_token_init (const char *token_url,
                          const char *so_pin, const char *label)
{
  int ret;
  struct pkcs11_url_info info;
  ck_rv_t rv;
  pakchois_module_t *module;
  ck_slot_id_t slot;
  char flabel[32];

  ret = pkcs11_url_to_info (token_url, &info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = pkcs11_find_slot (&module, &slot, &info, NULL);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* so it seems memset has other uses than zeroing! */
  memset (flabel, ' ', sizeof (flabel));
  if (label != NULL)
    memcpy (flabel, label, strlen (label));

  rv =
    pakchois_init_token (module, slot, (char *) so_pin, strlen (so_pin),
                         flabel);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pkcs11: %s\n", pakchois_error (rv));
      return pkcs11_rv_to_err (rv);
    }

  return 0;

}

/**
 * gnutls_pkcs11_token_set_pin:
 * @token_url: A PKCS #11 URL specifying a token
 * @oldpin: old user's PIN
 * @newpin: new user's PIN
 * @flags: one of gnutls_pkcs11_pin_flag_t
 *
 * This function will modify or set a user's PIN for the given token. 
 * If it is called to set a user pin for first time the oldpin must
 * be NULL.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_token_set_pin (const char *token_url,
                             const char *oldpin,
                             const char *newpin, unsigned int flags)
{
  int ret;
  pakchois_session_t *pks;
  struct pkcs11_url_info info;
  ck_rv_t rv;
  unsigned int ses_flags;

  ret = pkcs11_url_to_info (token_url, &info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (((flags & GNUTLS_PKCS11_PIN_USER) && oldpin == NULL) ||
      (flags & GNUTLS_PKCS11_PIN_SO))
    ses_flags = SESSION_WRITE | SESSION_LOGIN | SESSION_SO;
  else
    ses_flags = SESSION_WRITE | SESSION_LOGIN;

  ret = pkcs11_open_session (&pks, &info, ses_flags);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (oldpin == NULL)
    {
      rv = pakchois_init_pin (pks, (char *) newpin, strlen (newpin));
      if (rv != CKR_OK)
        {
          gnutls_assert ();
          _gnutls_debug_log ("pkcs11: %s\n", pakchois_error (rv));
          ret = pkcs11_rv_to_err (rv);
          goto finish;
        }
    }
  else
    {
      rv = pakchois_set_pin (pks,
                             (char *) oldpin, strlen (oldpin),
                             (char *) newpin, strlen (newpin));
      if (rv != CKR_OK)
        {
          gnutls_assert ();
          _gnutls_debug_log ("pkcs11: %s\n", pakchois_error (rv));
          ret = pkcs11_rv_to_err (rv);
          goto finish;
        }
    }

  ret = 0;

finish:
  pakchois_close_session (pks);
  return ret;

}
