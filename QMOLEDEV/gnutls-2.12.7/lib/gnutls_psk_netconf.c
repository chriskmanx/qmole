/*
 * Copyright (C) 2008, 2010 Free Software Foundation, Inc.
 *
 * Author: Simon Josefsson
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

/* Functions to support draft-ietf-netconf-tls-01.txt. */

#include <gnutls_int.h>
#include <gnutls_hash_int.h>
#include <gnutls_errors.h>

#ifdef ENABLE_PSK


/**
 * gnutls_psk_netconf_derive_key:
 * @password: zero terminated string containing password.
 * @psk_identity: zero terminated string with PSK identity.
 * @psk_identity_hint: zero terminated string with PSK identity hint.
 * @output_key: output variable, contains newly allocated *data pointer.
 *
 * This function will derive a PSK key from a password, for use with
 * the Netconf protocol.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 *
 * Since: 2.4.0
 *
 * Deprecated: The need for this interface was dropped from the
 * standard on publication as a RFC.  The function works now but will
 * return a hard failure in a future release.
 */
int
gnutls_psk_netconf_derive_key (const char *password,
                               const char *psk_identity,
                               const char *psk_identity_hint,
                               gnutls_datum_t * output_key)
{
  const char netconf_key_pad[] = "Key Pad for Netconf";
  size_t sha1len = _gnutls_hash_get_algo_len (GNUTLS_DIG_SHA1);
  size_t hintlen = strlen (psk_identity_hint);
  digest_hd_st dig;
  char *inner;
  size_t innerlen;
  int rc;

  /*
   * PSK = SHA-1(SHA-1(psk_identity + "Key Pad for Netconf" + password) +
   *             psk_identity_hint)
   *
   */

  rc = _gnutls_hash_init (&dig, GNUTLS_DIG_SHA1);
  if (rc < 0)
    {
      gnutls_assert ();
      return rc;
    }

  rc = _gnutls_hash (&dig, psk_identity, strlen (psk_identity));
  if (rc < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (&dig, NULL);
      return rc;
    }

  rc = _gnutls_hash (&dig, netconf_key_pad, strlen (netconf_key_pad));
  if (rc < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (&dig, NULL);
      return rc;
    }

  rc = _gnutls_hash (&dig, password, strlen (password));
  if (rc < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (&dig, NULL);
      return rc;
    }

  innerlen = sha1len + hintlen;
  inner = gnutls_malloc (innerlen);
  _gnutls_hash_deinit (&dig, inner);
  if (inner == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  memcpy (inner + sha1len, psk_identity_hint, hintlen);

  rc = _gnutls_hash_init (&dig, GNUTLS_DIG_SHA1);
  if (rc < 0)
    {
      gnutls_assert ();
      gnutls_free (inner);
      return rc;
    }

  rc = _gnutls_hash (&dig, inner, innerlen);
  gnutls_free (inner);
  if (rc < 0)
    {
      gnutls_assert ();
      _gnutls_hash_deinit (&dig, NULL);
      return rc;
    }

  output_key->data = gnutls_malloc (sha1len);
  _gnutls_hash_deinit (&dig, output_key->data);
  if (output_key->data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }
  output_key->size = sha1len;

  return 0;
}

#endif /* ENABLE_PSK */
