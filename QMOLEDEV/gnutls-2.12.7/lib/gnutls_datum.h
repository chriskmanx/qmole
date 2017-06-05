/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2010 Free Software
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

#ifndef GNUTLS_DATUM_H
#define GNUTLS_DATUM_H

void _gnutls_write_datum16 (opaque * dest, gnutls_datum_t dat);
void _gnutls_write_datum24 (opaque * dest, gnutls_datum_t dat);
void _gnutls_write_datum32 (opaque * dest, gnutls_datum_t dat);
void _gnutls_write_datum8 (opaque * dest, gnutls_datum_t dat);

int _gnutls_set_datum_m (gnutls_datum_t * dat, const void *data,
                         size_t data_size, gnutls_alloc_function);
#define _gnutls_set_datum( x, y, z) _gnutls_set_datum_m(x,y,z, gnutls_malloc)
#define _gnutls_sset_datum( x, y, z) _gnutls_set_datum_m(x,y,z, gnutls_secure_malloc)

int _gnutls_datum_append_m (gnutls_datum_t * dat, const void *data,
                            size_t data_size, gnutls_realloc_function);
#define _gnutls_datum_append(x,y,z) _gnutls_datum_append_m(x,y,z, gnutls_realloc)

void _gnutls_free_datum_m (gnutls_datum_t * dat, gnutls_free_function);
#define _gnutls_free_datum(x) _gnutls_free_datum_m(x, gnutls_free)

#endif
