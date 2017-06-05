/*
 * Copyright (C) 2002, 2003, 2004, 2005, 2010 Free Software Foundation,
 * Inc.
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

#ifndef AUTH_DH_COMMON
#define AUTH_DH_COMMON

typedef struct
{
  int secret_bits;

  gnutls_datum_t prime;
  gnutls_datum_t generator;
  gnutls_datum_t public_key;
} dh_info_st;

void _gnutls_free_dh_info (dh_info_st * dh);
int _gnutls_gen_dh_common_client_kx_int (gnutls_session_t, opaque **, gnutls_datum_t* pskkey);
int _gnutls_gen_dh_common_client_kx (gnutls_session_t, opaque **);
int _gnutls_proc_dh_common_client_kx (gnutls_session_t session,
                                      opaque * data, size_t _data_size,
                                      bigint_t p, bigint_t g);
int _gnutls_dh_common_print_server_kx (gnutls_session_t, bigint_t g,
                                       bigint_t p, opaque ** data, int psk);
int _gnutls_proc_dh_common_server_kx (gnutls_session_t session, opaque * data,
                                      size_t _data_size, int psk);

#endif
