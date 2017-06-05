/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2009, 2010 Free
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

#ifndef GNUTLS_GLOBAL_H
#define GNUTLS_GLOBAL_H

#include <libtasn1.h>
#include <gnutls/gnutls.h>

int gnutls_is_secure_memory (const void *mem);

extern ASN1_TYPE _gnutls_pkix1_asn;
extern ASN1_TYPE _gnutls_gnutls_asn;

/* removed const from node_asn* to
 * prevent warnings, since libtasn1 doesn't
 * use the const keywork in its functions.
 */
#define _gnutls_get_gnutls_asn() ((ASN1_TYPE) _gnutls_gnutls_asn)
#define _gnutls_get_pkix() ((ASN1_TYPE) _gnutls_pkix1_asn)

extern gnutls_log_func _gnutls_log_func;
extern int _gnutls_log_level;
extern int gnutls_crypto_init (void);

#endif
