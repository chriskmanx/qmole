/*
 * Copyright (C) 2008, 2010 Free Software Foundation, Inc.
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

#ifndef RANDOM_H
#define RANDOM_H

#include <gnutls/crypto.h>

extern int crypto_rnd_prio;
extern gnutls_crypto_rnd_st _gnutls_rnd_ops;

int _gnutls_rnd (gnutls_rnd_level_t level, void *data, size_t len);
#define _gnutls_rnd gnutls_rnd
void _gnutls_rnd_deinit (void);
int _gnutls_rnd_init (void);

#endif
