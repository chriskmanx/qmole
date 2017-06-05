/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2010 Free Software
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

/* contains functions that make it easier to
 * write vectors of <size|data>. The destination size
 * should be preallocated (datum.size+(bits/8))
 */

#include <gnutls_int.h>
#include <gnutls_num.h>
#include <gnutls_datum.h>
#include <gnutls_errors.h>


void
_gnutls_write_datum16 (opaque * dest, gnutls_datum_t dat)
{
  _gnutls_write_uint16 (dat.size, dest);
  if (dat.data != NULL)
    memcpy (&dest[2], dat.data, dat.size);
}

void
_gnutls_write_datum24 (opaque * dest, gnutls_datum_t dat)
{
  _gnutls_write_uint24 (dat.size, dest);
  if (dat.data != NULL)
    memcpy (&dest[3], dat.data, dat.size);
}

void
_gnutls_write_datum32 (opaque * dest, gnutls_datum_t dat)
{
  _gnutls_write_uint32 (dat.size, dest);
  if (dat.data != NULL)
    memcpy (&dest[4], dat.data, dat.size);
}

void
_gnutls_write_datum8 (opaque * dest, gnutls_datum_t dat)
{
  dest[0] = (uint8_t) dat.size;
  if (dat.data != NULL)
    memcpy (&dest[1], dat.data, dat.size);
}


int
_gnutls_set_datum_m (gnutls_datum_t * dat, const void *data,
                     size_t data_size, gnutls_alloc_function galloc_func)
{
  if (data_size == 0 || data == NULL)
    {
      dat->data = NULL;
      dat->size = 0;
      return 0;
    }

  dat->data = galloc_func (data_size);
  if (dat->data == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  dat->size = data_size;
  memcpy (dat->data, data, data_size);

  return 0;
}

int
_gnutls_datum_append_m (gnutls_datum_t * dst, const void *data,
                        size_t data_size,
                        gnutls_realloc_function grealloc_func)
{

  dst->data = grealloc_func (dst->data, data_size + dst->size);
  if (dst->data == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  memcpy (&dst->data[dst->size], data, data_size);
  dst->size += data_size;

  return 0;
}

void
_gnutls_free_datum_m (gnutls_datum_t * dat, gnutls_free_function gfree_func)
{
  if (dat->data != NULL)
    gfree_func (dat->data);

  dat->data = NULL;
  dat->size = 0;
}
