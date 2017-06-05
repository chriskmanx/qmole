/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2008, 2010 Free
 * Software Foundation, Inc.
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

#ifndef GNUTLS_NUM_H
#define GNUTLS_NUM_H

#include <gnutls_int.h>

#include <minmax.h>

uint32_t _gnutls_uint24touint32 (uint24 num);
uint24 _gnutls_uint32touint24 (uint32_t num);
uint32_t _gnutls_read_uint32 (const opaque * data);
uint16_t _gnutls_read_uint16 (const opaque * data);
uint32_t _gnutls_conv_uint32 (uint32_t data);
uint16_t _gnutls_conv_uint16 (uint16_t data);
uint32_t _gnutls_read_uint24 (const opaque * data);
void _gnutls_write_uint24 (uint32_t num, opaque * data);
void _gnutls_write_uint32 (uint32_t num, opaque * data);
void _gnutls_write_uint16 (uint16_t num, opaque * data);
uint32_t _gnutls_uint64touint32 (const uint64 *);

int _gnutls_uint64pp (uint64 *);
#define _gnutls_uint64zero(x) x.i[0] = x.i[1] = x.i[2] = x.i[3] = x.i[4] = x.i[5] = x.i[6] = x.i[7] = 0
#define UINT64DATA(x) (x.i)

#endif /* GNUTLS_NUM_H */
