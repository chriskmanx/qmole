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

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_num.h>
#include <xsize.h>

gnutls_alloc_function gnutls_secure_malloc = malloc;
gnutls_alloc_function gnutls_malloc = malloc;
gnutls_free_function gnutls_free = free;
gnutls_realloc_function gnutls_realloc = realloc;

void *(*gnutls_calloc) (size_t, size_t) = calloc;
char *(*gnutls_strdup) (const char *) = _gnutls_strdup;

int
_gnutls_is_secure_mem_null (const void *ign)
{
  return 0;
}

int (*_gnutls_is_secure_memory) (const void *) = _gnutls_is_secure_mem_null;


void *
_gnutls_calloc (size_t nmemb, size_t size)
{
  void *ret;
  size_t n = xtimes (nmemb, size);
  ret = (size_in_bounds_p (n) ? gnutls_malloc (n) : NULL);
  if (ret != NULL)
    memset (ret, 0, size);
  return ret;
}

svoid *
gnutls_secure_calloc (size_t nmemb, size_t size)
{
  svoid *ret;
  size_t n = xtimes (nmemb, size);
  ret = (size_in_bounds_p (n) ? gnutls_secure_malloc (n) : NULL);
  if (ret != NULL)
    memset (ret, 0, size);
  return ret;
}

/* This realloc will free ptr in case realloc
 * fails.
 */
void *
gnutls_realloc_fast (void *ptr, size_t size)
{
  void *ret;

  if (size == 0)
    return ptr;

  ret = gnutls_realloc (ptr, size);
  if (ret == NULL)
    {
      gnutls_free (ptr);
    }

  return ret;
}

char *
_gnutls_strdup (const char *str)
{
  size_t siz = strlen (str) + 1;
  char *ret;

  ret = gnutls_malloc (siz);
  if (ret != NULL)
    memcpy (ret, str, siz);
  return ret;
}


#if 0
/* don't use them. They are included for documentation.
 */

/**
 * gnutls_malloc:
 *
 * This function will allocate 's' bytes data, and
 * return a pointer to memory. This function is supposed
 * to be used by callbacks.
 *
 * The allocation function used is the one set by
 * gnutls_global_set_mem_functions().
 **/
void *
gnutls_malloc (size_t s)
{
}

/**
 * gnutls_free:
 * @d: pointer to memory
 *
 * This function will free data pointed by ptr.
 *
 * The deallocation function used is the one set by
 * gnutls_global_set_mem_functions().
 *
 **/
void
gnutls_free (void *ptr)
{
}

#endif
