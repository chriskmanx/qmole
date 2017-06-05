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
#include <random.h>

/**
 * gnutls_pkcs11_copy_secret_key:
 * @token_url: A PKCS #11 URL specifying a token
 * @key: The raw key
 * @label: A name to be used for the stored data
 * @key_usage: One of GNUTLS_KEY_*
 * @flags: One of GNUTLS_PKCS11_OBJ_FLAG_*
 *
 * This function will copy a raw secret (symmetric) key into a PKCS #11 
 * token specified by a URL. The key can be marked as sensitive or not.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_copy_secret_key (const char *token_url, gnutls_datum_t * key,
                               const char *label,
                               unsigned int key_usage, unsigned int flags
                               /* GNUTLS_PKCS11_OBJ_FLAG_* */ )
{
  int ret;
  pakchois_session_t *pks;
  struct pkcs11_url_info info;
  ck_rv_t rv;
  struct ck_attribute a[12];
  ck_object_class_t class = CKO_SECRET_KEY;
  ck_object_handle_t obj;
  ck_key_type_t keytype = CKK_GENERIC_SECRET;
  ck_bool_t tval = 1;
  int a_val;
  opaque id[16];

  ret = pkcs11_url_to_info (token_url, &info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* generate a unique ID */
  ret = _gnutls_rnd (GNUTLS_RND_NONCE, id, sizeof (id));
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

  /* FIXME: copy key usage flags */

  a[0].type = CKA_CLASS;
  a[0].value = &class;
  a[0].value_len = sizeof (class);
  a[1].type = CKA_VALUE;
  a[1].value = key->data;
  a[1].value_len = key->size;
  a[2].type = CKA_TOKEN;
  a[2].value = &tval;
  a[2].value_len = sizeof (tval);
  a[3].type = CKA_PRIVATE;
  a[3].value = &tval;
  a[3].value_len = sizeof (tval);
  a[4].type = CKA_KEY_TYPE;
  a[4].value = &keytype;
  a[4].value_len = sizeof (keytype);
  a[5].type = CKA_ID;
  a[5].value = id;
  a[5].value_len = sizeof (id);

  a_val = 6;

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
  pakchois_close_session (pks);

  return ret;

}
