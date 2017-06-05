/* GnuTLS-extra --- Guile bindings for GnuTLS-extra.
   Copyright (C) 2007, 2009, 2010 Free Software Foundation, Inc.

   GnuTLS-extra is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GnuTLS-extra is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuTLS-EXTRA; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

/* Important note: As written above, this part of the code is ditributed
   under the GPL, not the LGPL.  */

/* Written by Ludovic Courtès <ludo@chbouib.org>.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <gnutls/gnutls.h>
#include <gnutls/extra.h>
#include <gnutls/openpgp.h>
#include <libguile.h>

#include <alloca.h>

#include "errors.h"
#include "utils.h"
#include "smobs.h"
#include "enums.h"
#include "extra-enums.h"
#include "extra-smobs.h"



/* SMOB and enums type definitions.  */

#include "extra-smob-types.i.c"
#include "extra-enum-map.i.c"


/* OpenPGP keys.  */


/* Maximum size we support for the name of OpenPGP keys.  */
#define GUILE_GNUTLS_MAX_OPENPGP_NAME_LENGTH  2048

SCM_DEFINE (scm_gnutls_import_openpgp_certificate,
            "import-openpgp-certificate", 2, 0, 0, (SCM data, SCM format),
            "Return a new OpenPGP certificate object resulting from the "
            "import of @var{data} (a uniform array) according to "
            "@var{format}.")
#define FUNC_NAME s_scm_gnutls_import_openpgp_certificate
{
  int err;
  gnutls_openpgp_crt_t c_key;
  gnutls_openpgp_crt_fmt_t c_format;
  gnutls_datum_t c_data_d;
  scm_t_array_handle c_data_handle;
  const char *c_data;
  size_t c_data_len;

  SCM_VALIDATE_ARRAY (1, data);
  c_format = scm_to_gnutls_openpgp_certificate_format (format, 2, FUNC_NAME);

  c_data = scm_gnutls_get_array (data, &c_data_handle, &c_data_len,
                                 FUNC_NAME);
  c_data_d.data = (unsigned char *) c_data;
  c_data_d.size = c_data_len;

  err = gnutls_openpgp_crt_init (&c_key);
  if (EXPECT_FALSE (err))
    {
      scm_gnutls_release_array (&c_data_handle);
      scm_gnutls_error (err, FUNC_NAME);
    }

  err = gnutls_openpgp_crt_import (c_key, &c_data_d, c_format);
  scm_gnutls_release_array (&c_data_handle);

  if (EXPECT_FALSE (err))
    {
      gnutls_openpgp_crt_deinit (c_key);
      scm_gnutls_error (err, FUNC_NAME);
    }

  return (scm_from_gnutls_openpgp_certificate (c_key));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_import_openpgp_private_key,
            "import-openpgp-private-key", 2, 1, 0, (SCM data, SCM format,
                                                    SCM pass),
            "Return a new OpenPGP private key object resulting from the "
            "import of @var{data} (a uniform array) according to "
            "@var{format}.  Optionally, a passphrase may be provided.")
#define FUNC_NAME s_scm_gnutls_import_openpgp_private_key
{
  int err;
  gnutls_openpgp_privkey_t c_key;
  gnutls_openpgp_crt_fmt_t c_format;
  gnutls_datum_t c_data_d;
  scm_t_array_handle c_data_handle;
  const char *c_data;
  char *c_pass;
  size_t c_data_len, c_pass_len;

  SCM_VALIDATE_ARRAY (1, data);
  c_format = scm_to_gnutls_openpgp_certificate_format (format, 2, FUNC_NAME);
  if ((pass == SCM_UNDEFINED) || (scm_is_false (pass)))
    c_pass = NULL;
  else
    {
      c_pass_len = scm_c_string_length (pass);
      c_pass = (char *) alloca (c_pass_len + 1);
      (void) scm_to_locale_stringbuf (pass, c_pass, c_pass_len + 1);
      c_pass[c_pass_len] = '\0';
    }

  c_data = scm_gnutls_get_array (data, &c_data_handle, &c_data_len,
                                 FUNC_NAME);
  c_data_d.data = (unsigned char *) c_data;
  c_data_d.size = c_data_len;

  err = gnutls_openpgp_privkey_init (&c_key);
  if (EXPECT_FALSE (err))
    {
      scm_gnutls_release_array (&c_data_handle);
      scm_gnutls_error (err, FUNC_NAME);
    }

  err = gnutls_openpgp_privkey_import (c_key, &c_data_d, c_format, c_pass,
                                       0 /* currently unused */ );
  scm_gnutls_release_array (&c_data_handle);

  if (EXPECT_FALSE (err))
    {
      gnutls_openpgp_privkey_deinit (c_key);
      scm_gnutls_error (err, FUNC_NAME);
    }

  return (scm_from_gnutls_openpgp_private_key (c_key));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_id, "openpgp-certificate-id",
            1, 0, 0,
            (SCM key),
            "Return the ID (an 8-element u8vector) of certificate "
            "@var{key}.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_id
{
  int err;
  unsigned char *c_id;
  gnutls_openpgp_crt_t c_key;

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);

  c_id = (unsigned char *) malloc (8);
  if (c_id == NULL)
    scm_gnutls_error (GNUTLS_E_MEMORY_ERROR, FUNC_NAME);

  err = gnutls_openpgp_crt_get_key_id (c_key, c_id);
  if (EXPECT_FALSE (err))
    scm_gnutls_error (err, FUNC_NAME);

  return (scm_take_u8vector (c_id, 8));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_id_x, "openpgp-certificate-id!",
            2, 0, 0,
            (SCM key, SCM id),
            "Store the ID (an 8 byte sequence) of certificate "
            "@var{key} in @var{id} (a u8vector).")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_id_x
{
  int err;
  char *c_id;
  scm_t_array_handle c_id_handle;
  size_t c_id_size;
  gnutls_openpgp_crt_t c_key;

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);
  c_id = scm_gnutls_get_writable_array (id, &c_id_handle, &c_id_size,
                                        FUNC_NAME);

  if (EXPECT_FALSE (c_id_size < 8))
    {
      scm_gnutls_release_array (&c_id_handle);
      scm_misc_error (FUNC_NAME, "ID vector too small: ~A", scm_list_1 (id));
    }

  err = gnutls_openpgp_crt_get_key_id (c_key, (unsigned char *) c_id);
  scm_gnutls_release_array (&c_id_handle);

  if (EXPECT_FALSE (err))
    scm_gnutls_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_fingerpint_x,
            "openpgp-certificate-fingerprint!",
            2, 0, 0,
            (SCM key, SCM fpr),
            "Store in @var{fpr} (a u8vector) the fingerprint of @var{key}.  "
            "Return the number of bytes stored in @var{fpr}.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_fingerpint_x
{
  int err;
  gnutls_openpgp_crt_t c_key;
  char *c_fpr;
  scm_t_array_handle c_fpr_handle;
  size_t c_fpr_len, c_actual_len = 0;

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);
  SCM_VALIDATE_ARRAY (2, fpr);

  c_fpr = scm_gnutls_get_writable_array (fpr, &c_fpr_handle, &c_fpr_len,
                                         FUNC_NAME);

  err = gnutls_openpgp_crt_get_fingerprint (c_key, c_fpr, &c_actual_len);
  scm_gnutls_release_array (&c_fpr_handle);

  if (EXPECT_FALSE (err))
    scm_gnutls_error (err, FUNC_NAME);

  return (scm_from_size_t (c_actual_len));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_fingerprint,
            "openpgp-certificate-fingerprint",
            1, 0, 0,
            (SCM key),
            "Return a new u8vector denoting the fingerprint of " "@var{key}.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_fingerprint
{
  int err;
  gnutls_openpgp_crt_t c_key;
  unsigned char *c_fpr;
  size_t c_fpr_len, c_actual_len;

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);

  /* V4 fingerprints are 160-bit SHA-1 hashes (see RFC2440).  */
  c_fpr_len = 20;
  c_fpr = (unsigned char *) malloc (c_fpr_len);
  if (EXPECT_FALSE (c_fpr == NULL))
    scm_gnutls_error (GNUTLS_E_MEMORY_ERROR, FUNC_NAME);

  do
    {
      c_actual_len = 0;
      err = gnutls_openpgp_crt_get_fingerprint (c_key, c_fpr, &c_actual_len);
      if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          /* Grow C_FPR.  */
          unsigned char *c_new;

          c_new = (unsigned char *) realloc (c_fpr, c_fpr_len * 2);
          if (EXPECT_FALSE (c_new == NULL))
            {
              free (c_fpr);
              scm_gnutls_error (GNUTLS_E_MEMORY_ERROR, FUNC_NAME);
            }
          else
            {
              c_fpr_len *= 2;
              c_fpr = c_new;
            }
        }
    }
  while (err == GNUTLS_E_SHORT_MEMORY_BUFFER);

  if (EXPECT_FALSE (err))
    {
      free (c_fpr);
      scm_gnutls_error (err, FUNC_NAME);
    }

  if (c_actual_len < c_fpr_len)
    /* Shrink C_FPR.  */
    c_fpr = realloc (c_fpr, c_actual_len);

  return (scm_take_u8vector (c_fpr, c_actual_len));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_name, "openpgp-certificate-name",
            2, 0, 0,
            (SCM key, SCM index),
            "Return the @var{index}th name of @var{key}.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_name
{
  int err;
  gnutls_openpgp_crt_t c_key;
  int c_index;
  char c_name[GUILE_GNUTLS_MAX_OPENPGP_NAME_LENGTH];
  size_t c_name_len = sizeof (c_name);

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);
  c_index = scm_to_int (index);

  err = gnutls_openpgp_crt_get_name (c_key, c_index, c_name, &c_name_len);
  if (EXPECT_FALSE (err))
    scm_gnutls_error (err, FUNC_NAME);

  /* XXX: The name is really UTF-8.  */
  return (scm_from_locale_string (c_name));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_names, "openpgp-certificate-names",
            1, 0, 0, (SCM key), "Return the list of names for @var{key}.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_names
{
  int err;
  SCM result = SCM_EOL;
  gnutls_openpgp_crt_t c_key;
  int c_index = 0;
  char c_name[GUILE_GNUTLS_MAX_OPENPGP_NAME_LENGTH];
  size_t c_name_len = sizeof (c_name);

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);

  do
    {
      err = gnutls_openpgp_crt_get_name (c_key, c_index, c_name, &c_name_len);
      if (!err)
        {
          result = scm_cons (scm_from_locale_string (c_name), result);
          c_index++;
        }
    }
  while (!err);

  if (EXPECT_FALSE (err != GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE))
    scm_gnutls_error (err, FUNC_NAME);

  return (scm_reverse_x (result, SCM_EOL));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_algorithm,
            "openpgp-certificate-algorithm",
            1, 0, 0,
            (SCM key),
            "Return two values: the certificate algorithm used by "
            "@var{key} and the number of bits used.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_algorithm
{
  gnutls_openpgp_crt_t c_key;
  unsigned int c_bits;
  gnutls_pk_algorithm_t c_algo;

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);
  c_algo = gnutls_openpgp_crt_get_pk_algorithm (c_key, &c_bits);

  return (scm_values (scm_list_2 (scm_from_gnutls_pk_algorithm (c_algo),
                                  scm_from_uint (c_bits))));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_version,
            "openpgp-certificate-version",
            1, 0, 0,
            (SCM key),
            "Return the version of the OpenPGP message format (RFC2440) "
            "honored by @var{key}.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_version
{
  int c_version;
  gnutls_openpgp_crt_t c_key;

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);
  c_version = gnutls_openpgp_crt_get_version (c_key);

  return (scm_from_int (c_version));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_certificate_usage, "openpgp-certificate-usage",
            1, 0, 0,
            (SCM key),
            "Return a list of values denoting the key usage of @var{key}.")
#define FUNC_NAME s_scm_gnutls_openpgp_certificate_usage
{
  int err;
  unsigned int c_usage = 0;
  gnutls_openpgp_crt_t c_key;

  c_key = scm_to_gnutls_openpgp_certificate (key, 1, FUNC_NAME);

  err = gnutls_openpgp_crt_get_key_usage (c_key, &c_usage);
  if (EXPECT_FALSE (err))
    scm_gnutls_error (err, FUNC_NAME);

  return (scm_from_gnutls_key_usage_flags (c_usage));
}

#undef FUNC_NAME



/* OpenPGP keyrings.  */

SCM_DEFINE (scm_gnutls_import_openpgp_keyring, "import-openpgp-keyring",
            2, 0, 0,
            (SCM data, SCM format),
            "Import @var{data} (a u8vector) according to @var{format} "
            "and return the imported keyring.")
#define FUNC_NAME s_scm_gnutls_import_openpgp_keyring
{
  int err;
  gnutls_openpgp_keyring_t c_keyring;
  gnutls_openpgp_crt_fmt_t c_format;
  gnutls_datum_t c_data_d;
  scm_t_array_handle c_data_handle;
  const char *c_data;
  size_t c_data_len;

  SCM_VALIDATE_ARRAY (1, data);
  c_format = scm_to_gnutls_openpgp_certificate_format (format, 2, FUNC_NAME);

  c_data = scm_gnutls_get_array (data, &c_data_handle, &c_data_len,
                                 FUNC_NAME);

  c_data_d.data = (unsigned char *) c_data;
  c_data_d.size = c_data_len;

  err = gnutls_openpgp_keyring_init (&c_keyring);
  if (EXPECT_FALSE (err))
    {
      scm_gnutls_release_array (&c_data_handle);
      scm_gnutls_error (err, FUNC_NAME);
    }

  err = gnutls_openpgp_keyring_import (c_keyring, &c_data_d, c_format);
  scm_gnutls_release_array (&c_data_handle);

  if (EXPECT_FALSE (err))
    {
      gnutls_openpgp_keyring_deinit (c_keyring);
      scm_gnutls_error (err, FUNC_NAME);
    }

  return (scm_from_gnutls_openpgp_keyring (c_keyring));
}

#undef FUNC_NAME

SCM_DEFINE (scm_gnutls_openpgp_keyring_contains_key_id_p,
            "openpgp-keyring-contains-key-id?",
            2, 0, 0,
            (SCM keyring, SCM id),
            "Return @code{#f} if key ID @var{id} is in @var{keyring}, "
            "@code{#f} otherwise.")
#define FUNC_NAME s_scm_gnutls_openpgp_keyring_contains_key_id_p
{
  int c_result;
  gnutls_openpgp_keyring_t c_keyring;
  scm_t_array_handle c_id_handle;
  const char *c_id;
  size_t c_id_len;

  c_keyring = scm_to_gnutls_openpgp_keyring (keyring, 1, FUNC_NAME);
  SCM_VALIDATE_ARRAY (1, id);

  c_id = scm_gnutls_get_array (id, &c_id_handle, &c_id_len, FUNC_NAME);
  if (EXPECT_FALSE (c_id_len != 8))
    {
      scm_gnutls_release_array (&c_id_handle);
      scm_wrong_type_arg (FUNC_NAME, 1, id);
    }

  c_result = gnutls_openpgp_keyring_check_id (c_keyring,
                                              (unsigned char *) c_id,
                                              0 /* unused */ );

  scm_gnutls_release_array (&c_id_handle);

  return (scm_from_bool (c_result == 0));
}

#undef FUNC_NAME


/* Certificates.  */

SCM_DEFINE (scm_gnutls_set_certificate_credentials_openpgp_keys_x,
            "set-certificate-credentials-openpgp-keys!",
            3, 0, 0,
            (SCM cred, SCM pub, SCM sec),
            "Use certificate @var{pub} and secret key @var{sec} in "
            "certificate credentials @var{cred}.")
#define FUNC_NAME s_scm_gnutls_set_certificate_credentials_openpgp_keys_x
{
  int err;
  gnutls_certificate_credentials_t c_cred;
  gnutls_openpgp_crt_t c_pub;
  gnutls_openpgp_privkey_t c_sec;

  c_cred = scm_to_gnutls_certificate_credentials (cred, 1, FUNC_NAME);
  c_pub = scm_to_gnutls_openpgp_certificate (pub, 2, FUNC_NAME);
  c_sec = scm_to_gnutls_openpgp_private_key (sec, 3, FUNC_NAME);

  err = gnutls_certificate_set_openpgp_key (c_cred, c_pub, c_sec);
  if (EXPECT_FALSE (err))
    scm_gnutls_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}

#undef FUNC_NAME


/* Initialization.  */

void
scm_init_gnutls_extra (void)
{
#include "extra.x"

  (void) gnutls_global_init_extra ();

  scm_gnutls_define_enums ();
}

/* arch-tag: 655f308d-5643-4bc7-9db4-1f84bd902bef
 */
