/*
 * Copyright (C) 2005, 2007, 2008, 2010 Free Software Foundation, Inc.
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

/* Functions for operating in an PSK passwd file are included here */

#include <gnutls_int.h>

#ifdef ENABLE_PSK

#include "x509_b64.h"
#include "gnutls_errors.h"
#include <auth_psk_passwd.h>
#include "auth_psk.h"
#include "gnutls_auth.h"
#include "gnutls_dh.h"
#include "debug.h"
#include <gnutls_str.h>
#include <gnutls_datum.h>
#include <gnutls_num.h>
#include <random.h>


/* this function parses passwd.psk file. Format is:
 * string(username):hex(passwd)
 */
static int
pwd_put_values (gnutls_datum_t * psk, char *str)
{
  char *p;
  int len, ret;
  size_t size;

  p = strchr (str, ':');
  if (p == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  *p = '\0';
  p++;

  /* skip username
   */

  /* read the key
   */
  len = strlen (p);
  if (p[len - 1] == '\n' || p[len - 1] == ' ')
    len--;

  size = psk->size = len / 2;
  psk->data = gnutls_malloc (size);
  if (psk->data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = _gnutls_hex2bin ((opaque *) p, len, psk->data, &size);
  psk->size = (unsigned int) size;
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }


  return 0;

}


/* Randomizes the given password entry. It actually sets a random password. 
 * Returns 0 on success.
 */
static int
_randomize_psk (gnutls_datum_t * psk)
{
  int ret;

  psk->data = gnutls_malloc (16);
  if (psk->data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  psk->size = 16;

  ret = _gnutls_rnd (GNUTLS_RND_NONCE, (char *) psk->data, 16);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/* Returns the PSK key of the given user. 
 * If the user doesn't exist a random password is returned instead.
 */
int
_gnutls_psk_pwd_find_entry (gnutls_session_t session, char *username,
                            gnutls_datum_t * psk)
{
  gnutls_psk_server_credentials_t cred;
  FILE *fd;
  char line[2 * 1024];
  unsigned i, len;
  int ret;

  cred = (gnutls_psk_server_credentials_t)
    _gnutls_get_cred (session->key, GNUTLS_CRD_PSK, NULL);
  if (cred == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  /* if the callback which sends the parameters is
   * set, use it.
   */
  if (cred->pwd_callback != NULL)
    {
      ret = cred->pwd_callback (session, username, psk);

      if (ret == 1)
        {                       /* the user does not exist */
          ret = _randomize_psk (psk);
          if (ret < 0)
            {
              gnutls_assert ();
              return ret;
            }
          return 0;
        }

      if (ret < 0)
        {
          gnutls_assert ();
          return GNUTLS_E_SRP_PWD_ERROR;
        }

      return 0;
    }

  /* The callback was not set. Proceed.
   */
  if (cred->password_file == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_ERROR;
    }

  /* Open the selected password file.
   */
  fd = fopen (cred->password_file, "r");
  if (fd == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_ERROR;
    }

  len = strlen (username);
  while (fgets (line, sizeof (line), fd) != NULL)
    {
      /* move to first ':' */
      i = 0;
      while ((line[i] != ':') && (line[i] != '\0') && (i < sizeof (line)))
        {
          i++;
        }

      if (strncmp (username, line, MAX (i, len)) == 0)
        {
          ret = pwd_put_values (psk, line);
          fclose (fd);
          if (ret < 0)
            {
              gnutls_assert ();
              return GNUTLS_E_SRP_PWD_ERROR;
            }
          return 0;
        }
    }
  fclose (fd);

  /* user was not found. Fake him. 
   * the last index found and randomize the entry.
   */
  ret = _randomize_psk (psk);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;

}


#endif /* ENABLE PSK */
