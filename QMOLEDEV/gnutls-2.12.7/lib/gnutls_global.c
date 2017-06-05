/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2009, 2010
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
#include <gnutls_errors.h>
#include <libtasn1.h>
#include <gnutls_dh.h>
#include <random.h>
#include <gnutls/pkcs11.h>

#include <gnutls_extensions.h>  /* for _gnutls_ext_init */
#include <gnutls_cryptodev.h>
#include <locks.h>
#include <system.h>

#include "sockets.h"
#include "gettext.h"

/* Minimum library versions we accept. */
#define GNUTLS_MIN_LIBTASN1_VERSION "0.3.4"

/* created by asn1c */
extern const ASN1_ARRAY_TYPE gnutls_asn1_tab[];
extern const ASN1_ARRAY_TYPE pkix_asn1_tab[];

ASN1_TYPE _gnutls_pkix1_asn;
ASN1_TYPE _gnutls_gnutls_asn;

gnutls_log_func _gnutls_log_func;
int _gnutls_log_level = 0;      /* default log level */

/**
 * gnutls_global_set_log_function:
 * @log_func: it's a log function
 *
 * This is the function where you set the logging function gnutls is
 * going to use.  This function only accepts a character array.
 * Normally you may not use this function since it is only used for
 * debugging purposes.
 *
 * gnutls_log_func is of the form,
 * void (*gnutls_log_func)( int level, const char*);
 **/
void
gnutls_global_set_log_function (gnutls_log_func log_func)
{
  _gnutls_log_func = log_func;
}

/**
 * gnutls_global_set_time_function:
 * @time_func: it's the system time function
 *
 * This is the function where you can override the default system
 * time function.
 *
 * gnutls_time_func is of the form,
 * time_t (*gnutls_time_func)( time*);
 **/
void
gnutls_global_set_time_function (gnutls_time_func time_func)
{
  gnutls_time = time_func;
}

/**
 * gnutls_global_set_log_level:
 * @level: it's an integer from 0 to 9.
 *
 * This is the function that allows you to set the log level.  The
 * level is an integer between 0 and 9.  Higher values mean more
 * verbosity. The default value is 0.  Larger values should only be
 * used with care, since they may reveal sensitive information.
 *
 * Use a log level over 10 to enable all debugging options.
 **/
void
gnutls_global_set_log_level (int level)
{
  _gnutls_log_level = level;
}

/**
 * gnutls_global_set_mem_functions:
 * @alloc_func: it's the default memory allocation function. Like malloc().
 * @secure_alloc_func: This is the memory allocation function that will be used for sensitive data.
 * @is_secure_func: a function that returns 0 if the memory given is not secure. May be NULL.
 * @realloc_func: A realloc function
 * @free_func: The function that frees allocated data. Must accept a NULL pointer.
 *
 * This is the function were you set the memory allocation functions
 * gnutls is going to use. By default the libc's allocation functions
 * (malloc(), free()), are used by gnutls, to allocate both sensitive
 * and not sensitive data.  This function is provided to set the
 * memory allocation functions to something other than the defaults
 *
 * This function must be called before gnutls_global_init() is called.
 * This function is not thread safe.
 **/
void
gnutls_global_set_mem_functions (gnutls_alloc_function alloc_func,
                                 gnutls_alloc_function secure_alloc_func,
                                 gnutls_is_secure_function is_secure_func,
                                 gnutls_realloc_function realloc_func,
                                 gnutls_free_function free_func)
{
  gnutls_secure_malloc = secure_alloc_func;
  gnutls_malloc = alloc_func;
  gnutls_realloc = realloc_func;
  gnutls_free = free_func;

  if (is_secure_func != NULL)
    _gnutls_is_secure_memory = is_secure_func;
  else
    _gnutls_is_secure_memory = _gnutls_is_secure_mem_null;

  /* if using the libc's default malloc
   * use libc's calloc as well.
   */
  if (gnutls_malloc == malloc)
    {
      gnutls_calloc = calloc;
    }
  else
    {                           /* use the included ones */
      gnutls_calloc = _gnutls_calloc;
    }
  gnutls_strdup = _gnutls_strdup;

}

static int _gnutls_init = 0;


/**
 * gnutls_global_init:
 *
 * This function initializes the global data to defaults.  Every
 * gnutls application has a global data which holds common parameters
 * shared by gnutls session structures.  You should call
 * gnutls_global_deinit() when gnutls usage is no longer needed
 *
 * Note that this function will also initialize the underlying crypto
 * backend, if it has not been initialized before.  
 *
 * This function increment a global counter, so that
 * gnutls_global_deinit() only releases resources when it has been
 * called as many times as gnutls_global_init().  This is useful when
 * GnuTLS is used by more than one library in an application.  This
 * function can be called many times, but will only do something the
 * first time.
 *
 * Note!  This function is not thread safe.  If two threads call this
 * function simultaneously, they can cause a race between checking
 * the global counter and incrementing it, causing both threads to
 * execute the library initialization code.  That would lead to a
 * memory leak.  To handle this, your application could invoke this
 * function after aquiring a thread mutex.  To ignore the potential
 * memory leak is also an option.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS (zero) is returned,
 *   otherwise an error code is returned.
 **/
int
gnutls_global_init (void)
{
  int result = 0;
  int res;

  if (_gnutls_init++)
    goto out;

  if (gl_sockets_startup (SOCKETS_1_1))
    return GNUTLS_E_LIBRARY_VERSION_MISMATCH;

  bindtextdomain (PACKAGE, LOCALEDIR);

  res = gnutls_crypto_init ();
  if (res != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_CRYPTO_INIT_FAILED;
    }

  /* initialize ASN.1 parser
   * This should not deal with files in the final
   * version.
   */
  if (asn1_check_version (GNUTLS_MIN_LIBTASN1_VERSION) == NULL)
    {
      gnutls_assert ();
      _gnutls_debug_log ("Checking for libtasn1 failed: %s < %s\n",
                         asn1_check_version (NULL),
                         GNUTLS_MIN_LIBTASN1_VERSION);
      return GNUTLS_E_INCOMPATIBLE_LIBTASN1_LIBRARY;
    }

  res = asn1_array2tree (pkix_asn1_tab, &_gnutls_pkix1_asn, NULL);
  if (res != ASN1_SUCCESS)
    {
      result = _gnutls_asn2err (res);
      goto out;
    }

  res = asn1_array2tree (gnutls_asn1_tab, &_gnutls_gnutls_asn, NULL);
  if (res != ASN1_SUCCESS)
    {
      asn1_delete_structure (&_gnutls_pkix1_asn);
      result = _gnutls_asn2err (res);
      goto out;
    }

  /* Initialize the random generator */
  result = _gnutls_rnd_init ();
  if (result < 0)
    {
      gnutls_assert ();
      goto out;
    }

  /* Initialize the default TLS extensions */
  result = _gnutls_ext_init ();
  if (result < 0)
    {
      gnutls_assert ();
      goto out;
    }

  gnutls_pkcs11_init (GNUTLS_PKCS11_FLAG_AUTO, NULL);

  _gnutls_cryptodev_init ();

out:
  return result;
}

/**
 * gnutls_global_deinit:
 *
 * This function deinitializes the global data, that were initialized
 * using gnutls_global_init().
 *
 * Note!  This function is not thread safe.  See the discussion for
 * gnutls_global_init() for more information.
 **/
void
gnutls_global_deinit (void)
{
  if (_gnutls_init == 1)
    {
      gl_sockets_cleanup ();
      _gnutls_rnd_deinit ();
      _gnutls_ext_deinit ();
      asn1_delete_structure (&_gnutls_gnutls_asn);
      asn1_delete_structure (&_gnutls_pkix1_asn);
      _gnutls_crypto_deregister ();
      _gnutls_cryptodev_deinit ();
      gnutls_pkcs11_deinit ();
    }
  _gnutls_init--;
}

/* These functions should be elsewere. Kept here for
 * historical reasons.
 */


/**
 * gnutls_check_version:
 * @req_version: version string to compare with, or %NULL.
 *
 * Check GnuTLS Library version.
 *
 * See %GNUTLS_VERSION for a suitable @req_version string.
 *
 * Return value: Check that the version of the library is at
 *   minimum the one given as a string in @req_version and return the
 *   actual version string of the library; return %NULL if the
 *   condition is not met.  If %NULL is passed to this function no
 *   check is done and only the version string is returned.
  **/
const char *
gnutls_check_version (const char *req_version)
{
  if (!req_version || strverscmp (req_version, VERSION) <= 0)
    return VERSION;

  return NULL;
}
