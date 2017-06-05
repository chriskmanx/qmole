/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2008, 2010 Free Software
 * Foundation, Inc.
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

/* Functions for operating in an SRP passwd file are included here */

#include <gnutls_int.h>

#ifdef ENABLE_SRP

#include "x509_b64.h"
#include "gnutls_errors.h"
#include <auth_srp_passwd.h>
#include "auth_srp.h"
#include "gnutls_auth.h"
#include "gnutls_srp.h"
#include "gnutls_dh.h"
#include "debug.h"
#include <gnutls_str.h>
#include <gnutls_datum.h>
#include <gnutls_num.h>
#include <random.h>

static int _randomize_pwd_entry (SRP_PWD_ENTRY * entry);

/* this function parses tpasswd.conf file. Format is:
 * string(username):base64(v):base64(salt):int(index)
 */
static int
pwd_put_values (SRP_PWD_ENTRY * entry, char *str)
{
  char *p;
  int len, ret;
  opaque *verifier;
  size_t verifier_size;
  int indx;

  p = strrchr (str, ':');       /* we have index */
  if (p == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  *p = '\0';
  p++;

  indx = atoi (p);
  if (indx == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  /* now go for salt */
  p = strrchr (str, ':');       /* we have salt */
  if (p == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  *p = '\0';
  p++;

  len = strlen (p);

  entry->salt.size = _gnutls_sbase64_decode (p, len, &entry->salt.data);

  if (entry->salt.size <= 0)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  /* now go for verifier */
  p = strrchr (str, ':');       /* we have verifier */
  if (p == NULL)
    {
      _gnutls_free_datum (&entry->salt);
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  *p = '\0';
  p++;

  len = strlen (p);
  ret = _gnutls_sbase64_decode (p, len, &verifier);
  if (ret <= 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&entry->salt);
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  verifier_size = ret;
  entry->v.data = verifier;
  entry->v.size = verifier_size;

  /* now go for username */
  *p = '\0';

  entry->username = gnutls_strdup (str);
  if (entry->username == NULL)
    {
      _gnutls_free_datum (&entry->salt);
      _gnutls_free_datum (&entry->v);
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return indx;
}


/* this function parses tpasswd.conf file. Format is:
 * int(index):base64(n):int(g)
 */
static int
pwd_put_values2 (SRP_PWD_ENTRY * entry, char *str)
{
  char *p;
  int len;
  opaque *tmp;
  int ret;

  p = strrchr (str, ':');       /* we have g */
  if (p == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  *p = '\0';
  p++;

  /* read the generator */
  len = strlen (p);
  if (p[len - 1] == '\n' || p[len - 1] == ' ')
    len--;
  ret = _gnutls_sbase64_decode (p, len, &tmp);

  if (ret < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  entry->g.data = tmp;
  entry->g.size = ret;

  /* now go for n - modulo */
  p = strrchr (str, ':');       /* we have n */
  if (p == NULL)
    {
      _gnutls_free_datum (&entry->g);
      gnutls_assert ();
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  *p = '\0';
  p++;

  len = strlen (p);
  ret = _gnutls_sbase64_decode (p, len, &tmp);

  if (ret < 0)
    {
      gnutls_assert ();
      _gnutls_free_datum (&entry->g);
      return GNUTLS_E_SRP_PWD_PARSING_ERROR;
    }

  entry->n.data = tmp;
  entry->n.size = ret;

  return 0;
}


/* this function opens the tpasswd.conf file and reads the g and n
 * values. They are put in the entry.
 */
static int
pwd_read_conf (const char *pconf_file, SRP_PWD_ENTRY * entry, int idx)
{
  FILE *fd;
  char line[2 * 1024];
  unsigned i, len;
  char indexstr[10];

  snprintf (indexstr, sizeof(indexstr), "%u", (unsigned int)idx);

  fd = fopen (pconf_file, "r");
  if (fd == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_FILE_ERROR;
    }

  len = strlen (indexstr);
  while (fgets (line, sizeof (line), fd) != NULL)
    {
      /* move to first ':' */
      i = 0;
      while ((line[i] != ':') && (line[i] != '\0') && (i < sizeof (line)))
        {
          i++;
        }
      if (strncmp (indexstr, line, MAX (i, len)) == 0)
        {
          if ((idx = pwd_put_values2 (entry, line)) >= 0)
            return 0;
          else
            {
              return GNUTLS_E_SRP_PWD_ERROR;
            }
        }
    }
  return GNUTLS_E_SRP_PWD_ERROR;

}

int
_gnutls_srp_pwd_read_entry (gnutls_session_t state, char *username,
                            SRP_PWD_ENTRY ** _entry)
{
  gnutls_srp_server_credentials_t cred;
  FILE *fd;
  char line[2 * 1024];
  unsigned i, len;
  int ret;
  int idx, last_idx;
  SRP_PWD_ENTRY *entry;

  *_entry = gnutls_calloc (1, sizeof (SRP_PWD_ENTRY));
  if (*_entry == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }
  entry = *_entry;

  cred = (gnutls_srp_server_credentials_t)
    _gnutls_get_cred (state->key, GNUTLS_CRD_SRP, NULL);
  if (cred == NULL)
    {
      gnutls_assert ();
      _gnutls_srp_entry_free (entry);
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  /* if the callback which sends the parameters is
   * set, use it.
   */
  if (cred->pwd_callback != NULL)
    {
      ret = cred->pwd_callback (state, username, &entry->salt,
                                &entry->v, &entry->g, &entry->n);

      if (ret == 1)
        {                       /* the user does not exist */
          if (entry->g.size != 0 && entry->n.size != 0)
            {
              ret = _randomize_pwd_entry (entry);
              if (ret < 0)
                {
                  gnutls_assert ();
                  _gnutls_srp_entry_free (entry);
                  return ret;
                }
              return 0;
            }
          else
            {
              gnutls_assert ();
              ret = -1;         /* error in the callback */
            }
        }

      if (ret < 0)
        {
          gnutls_assert ();
          _gnutls_srp_entry_free (entry);
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
      _gnutls_srp_entry_free (entry);
      return GNUTLS_E_SRP_PWD_ERROR;
    }

  last_idx = 1;                 /* a default value */

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
          if ((idx = pwd_put_values (entry, line)) >= 0)
            {
              /* Keep the last index in memory, so we can retrieve fake parameters (g,n)
               * when the user does not exist.
               */
              /* XXX: last_idx will not be read as both if block branches return. */
              last_idx = idx;
              if (pwd_read_conf (cred->password_conf_file, entry, idx) == 0)
                {
                  return 0;
                }
              else
                {
                  gnutls_assert ();
                  _gnutls_srp_entry_free (entry);
                  return GNUTLS_E_SRP_PWD_ERROR;
                }
            }
          else
            {
              gnutls_assert ();
              _gnutls_srp_entry_free (entry);
              return GNUTLS_E_SRP_PWD_ERROR;
            }
        }
    }

  /* user was not found. Fake him. Actually read the g,n values from
   * the last index found and randomize the entry.
   */
  if (pwd_read_conf (cred->password_conf_file, entry, last_idx) == 0)
    {
      ret = _randomize_pwd_entry (entry);
      if (ret < 0)
        {
          gnutls_assert ();
          _gnutls_srp_entry_free (entry);
          return ret;
        }

      return 0;
    }

  gnutls_assert ();
  _gnutls_srp_entry_free (entry);
  return GNUTLS_E_SRP_PWD_ERROR;

}

/* Randomizes the given password entry. It actually sets the verifier
 * and the salt. Returns 0 on success.
 */
static int
_randomize_pwd_entry (SRP_PWD_ENTRY * entry)
{
  unsigned char rnd;
  int ret;

  if (entry->g.size == 0 || entry->n.size == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  ret = _gnutls_rnd (GNUTLS_RND_NONCE, &rnd, 1);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  entry->salt.size = (rnd % 10) + 9;

  entry->v.data = gnutls_malloc (20);
  entry->v.size = 20;
  if (entry->v.data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = _gnutls_rnd (GNUTLS_RND_RANDOM, entry->v.data, 20);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  entry->salt.data = gnutls_malloc (entry->salt.size);
  if (entry->salt.data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  ret = _gnutls_rnd (GNUTLS_RND_NONCE, entry->salt.data, entry->salt.size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/* Free all the entry parameters, except if g and n are
 * the static ones defined in extra.h
 */
void
_gnutls_srp_entry_free (SRP_PWD_ENTRY * entry)
{
  _gnutls_free_datum (&entry->v);
  _gnutls_free_datum (&entry->salt);

  if (entry->g.data != gnutls_srp_1024_group_generator.data)
    _gnutls_free_datum (&entry->g);

  if (entry->n.data != gnutls_srp_1024_group_prime.data &&
      entry->n.data != gnutls_srp_1536_group_prime.data &&
      entry->n.data != gnutls_srp_2048_group_prime.data)
    _gnutls_free_datum (&entry->n);

  gnutls_free (entry->username);
  gnutls_free (entry);
}


#endif /* ENABLE SRP */
