/*
 * Copyright (C) 2011 Free Software Foundation, Inc.
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <getpass.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gnutls/pkcs11.h>
#include <p11common.h>

#define MIN(x,y) ((x)<(y))?(x):(y)

#define MAX_CACHE_TRIES 5
static int
pin_callback (void *user, int attempt, const char *token_url,
              const char *token_label, unsigned int flags, char *pin,
              size_t pin_max)
{
  const char *password;
  const char * desc;
  int len, cache = MAX_CACHE_TRIES;
/* allow caching of PIN */
  static char *cached_url = NULL;
  static char cached_pin[32] = "";

  if (flags & GNUTLS_PKCS11_PIN_SO)
    desc = "security officer";
  else
    desc = "user";

  if (flags & GNUTLS_PKCS11_PIN_FINAL_TRY)
    {
      cache = 0;
      printf ("*** This is the final try before locking!\n");
    }
  if (flags & GNUTLS_PKCS11_PIN_COUNT_LOW)
    {
      cache = 0;
      printf ("*** Only few tries left before locking!\n");
    }
    
  if (cache > 0 && cached_url != NULL)
    {
      if (strcmp (cached_url, token_url) == 0)
        {
          if (strlen(pin) >= sizeof(cached_pin))
            {
              fprintf (stderr, "Too long PIN given\n");
              exit (1);
            }

          strcpy (pin, cached_pin);
          cache--;
          return 0;
        }
    }

  printf ("Token '%s' with URL '%s' ", token_label, token_url);
  printf ("requires %s PIN\n", desc);

  password = getpass ("Enter PIN: ");
  if (password == NULL || password[0] == 0)
    {
      fprintf (stderr, "No password given\n");
      exit (1);
    }

  len = MIN (pin_max, strlen (password));
  memcpy (pin, password, len);
  pin[len] = 0;

  /* cache */
  strcpy (cached_pin, pin);
  free (cached_url);
  cached_url = strdup (token_url);
  cache = MAX_CACHE_TRIES;

  return 0;
}

static int
token_callback (void *user, const char *label, const unsigned retry)
{
  char buf[32];
  char *p;

  if (retry > 0)
    {
      fprintf (stderr, "Could not find token %s\n", label);
      return -1;
    }
  printf ("Please insert token '%s' in slot and press enter\n", label);
  p = fgets (buf, sizeof (buf), stdin);

  return 0;
}

void
pkcs11_common (void)
{

  gnutls_pkcs11_set_pin_function (pin_callback, NULL);
  gnutls_pkcs11_set_token_function (token_callback, NULL);

}
