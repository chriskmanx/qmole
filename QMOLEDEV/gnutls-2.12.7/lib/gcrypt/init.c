/*
 * Copyright (C) 2010 Free Software Foundation, Inc.
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
#include <gnutls_errors.h>
#include <gcrypt.h>
#include <locks.h>

#define GNUTLS_MIN_LIBGCRYPT_VERSION "1.2.4"

/* Functions that refer to the initialization of the libgcrypt library.
 */

static struct gcry_thread_cbs gct = {
  .option = (GCRY_THREAD_OPTION_PTHREAD | (GCRY_THREAD_OPTION_VERSION << 8)),
  .init = NULL,
  .select = NULL,
  .waitpid = NULL,
  .accept = NULL,
  .connect = NULL,
  .sendmsg = NULL,
  .recvmsg = NULL,
};

int
gnutls_crypto_init (void)
{
  /* Initialize libgcrypt if it hasn't already been initialized. */
  if (gcry_control (GCRYCTL_ANY_INITIALIZATION_P) == 0)
    {
      const char *p;

      if (gnutls_mutex_init != NULL)
        {
          gct.mutex_init = gnutls_mutex_init;
          gct.mutex_destroy = gnutls_mutex_deinit;
          gct.mutex_lock = gnutls_mutex_lock;
          gct.mutex_unlock = gnutls_mutex_unlock;

          gcry_control (GCRYCTL_SET_THREAD_CBS, &gct);
        }

      p = gcry_check_version (GNUTLS_MIN_LIBGCRYPT_VERSION);

      if (p == NULL)
        {
          gnutls_assert ();
          _gnutls_debug_log ("Checking for libgcrypt failed: %s < %s\n",
                             gcry_check_version (NULL),
                             GNUTLS_MIN_LIBGCRYPT_VERSION);
          return GNUTLS_E_INCOMPATIBLE_GCRYPT_LIBRARY;
        }

      /* for gcrypt in order to be able to allocate memory */
      gcry_control (GCRYCTL_DISABLE_SECMEM, NULL, 0);

      gcry_control (GCRYCTL_INITIALIZATION_FINISHED, NULL, 0);

      gcry_control (GCRYCTL_ENABLE_QUICK_RANDOM, 0);
    }

  return 0;
}
