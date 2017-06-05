/*
 * Copyright (C) 2001, 2004, 2005, 2007, 2008, 2009, 2010 Free Software
 * Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS-EXTRA.
 *
 * GnuTLS-extra is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * GnuTLS-extra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_extensions.h>
#include <gnutls_algorithms.h>
#include <ext_inner_application.h>

#ifdef HAVE_GCRYPT
#include <gcrypt.h>
#endif

#ifdef USE_LZO
#ifdef USE_MINILZO
#include "minilzo/minilzo.h"
#elif HAVE_LZO_LZO1X_H
#include <lzo/lzo1x.h>
#elif HAVE_LZO1X_H
#include <lzo1x.h>
#endif
#endif
#include <gnutls/extra.h>

#ifdef USE_LZO
#include <gnutls_compress.h>

/* the number of the compression algorithms available in the compression
 * structure.
 */
extern int _gnutls_comp_algorithms_size;

typedef int (*LZO_FUNC) ();
extern LZO_FUNC _gnutls_lzo1x_decompress_safe;
extern LZO_FUNC _gnutls_lzo1x_1_compress;

extern gnutls_compression_entry _gnutls_compression_algorithms[];

static int
_gnutls_add_lzo_comp (void)
{
  int i;

  /* find the last element */
  for (i = 0; i < _gnutls_comp_algorithms_size; i++)
    {
      if (_gnutls_compression_algorithms[i].name == NULL)
        break;
    }

  if (_gnutls_compression_algorithms[i].name == NULL
      && (i < _gnutls_comp_algorithms_size - 1))
    {
      _gnutls_compression_algorithms[i].name = "GNUTLS_COMP_LZO";
      _gnutls_compression_algorithms[i].id = GNUTLS_COMP_LZO;
      _gnutls_compression_algorithms[i].num = 0xf2;

      _gnutls_compression_algorithms[i + 1].name = 0;

      /* Now enable the lzo functions: */
      _gnutls_lzo1x_decompress_safe = lzo1x_decompress_safe;
      _gnutls_lzo1x_1_compress = lzo1x_1_compress;

      return 0;                 /* ok */
    }


  return GNUTLS_E_MEMORY_ERROR;
}
#endif

static int _gnutls_init_extra = 0;

/**
 * gnutls_global_init_extra:
 *
 * This function initializes the global state of gnutls-extra library
 * to defaults.
 *
 * Note that gnutls_global_init() has to be called before this
 * function.  If this function is not called then the gnutls-extra
 * library will not be usable.
 *
 * This function is not thread safe, see the discussion for
 * gnutls_global_init() on how to deal with that.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_global_init_extra (void)
{
  int ret;

  /* If the version of libgnutls != version of
   * libextra, then do not initialize the library.
   * This is because it may break things.
   */
  if (strcmp (gnutls_check_version (NULL), VERSION) != 0)
    {
      return GNUTLS_E_LIBRARY_VERSION_MISMATCH;
    }

  _gnutls_init_extra++;

  if (_gnutls_init_extra != 1)
    return 0;

  ret = _gnutls_ext_register (&ext_mod_ia);
  if (ret != GNUTLS_E_SUCCESS)
    return ret;

  /* Initialize the LZO library
   */
#ifdef USE_LZO
  if (lzo_init () != LZO_E_OK)
    return GNUTLS_E_LZO_INIT_FAILED;

  /* Add the LZO compression method in the list of compression
   * methods.
   */
  ret = _gnutls_add_lzo_comp ();
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }
#endif


#ifdef HAVE_GCRYPT
#ifdef gcry_fips_mode_active
  /* Libgcrypt manual says that gcry_version_check must be called
     before calling gcry_fips_mode_active. */
  gcry_check_version (NULL);
  if (gcry_fips_mode_active ())
    {
      ret = gnutls_register_md5_handler ();
      if (ret)
        fprintf (stderr, "gnutls_register_md5_handler: %s\n",
                 gnutls_strerror (ret));
    }
#endif
#endif

  return 0;
}

/**
 * gnutls_extra_check_version:
 * @req_version: version string to compare with, or %NULL.
 *
 * Check GnuTLS Extra Library version.
 *
 * See %GNUTLS_EXTRA_VERSION for a suitable @req_version string.
 *
 * Return value: Check that the version of the library is at
 *   minimum the one given as a string in @req_version and return the
 *   actual version string of the library; return %NULL if the
 *   condition is not met.  If %NULL is passed to this function no
 *   check is done and only the version string is returned.
 **/
const char *
gnutls_extra_check_version (const char *req_version)
{
  if (!req_version || strverscmp (req_version, VERSION) <= 0)
    return VERSION;

  return NULL;
}
