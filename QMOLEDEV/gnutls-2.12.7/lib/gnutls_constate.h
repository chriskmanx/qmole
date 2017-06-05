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

#ifndef GNUTLS_CONSTATE_H
#define GNUTLS_CONSTATE_H

int _gnutls_epoch_set_cipher_suite (gnutls_session_t session, int epoch_rel,
                                    cipher_suite_st * suite);
int _gnutls_epoch_set_compression (gnutls_session_t session, int epoch_rel,
                                   gnutls_compression_method_t comp_algo);
void _gnutls_epoch_set_null_algos (gnutls_session_t session,
                                   record_parameters_st * params);
int _gnutls_epoch_set_keys (gnutls_session_t session, uint16_t epoch);
int _gnutls_connection_state_init (gnutls_session_t session);
int _gnutls_read_connection_state_init (gnutls_session_t session);
int _gnutls_write_connection_state_init (gnutls_session_t session);

int _gnutls_set_kx (gnutls_session_t session, gnutls_kx_algorithm_t algo);

int _gnutls_epoch_get (gnutls_session_t session, unsigned int epoch_rel,
                       record_parameters_st ** params_out);
int _gnutls_epoch_alloc (gnutls_session_t session, uint16_t epoch,
                         record_parameters_st ** out);
void _gnutls_epoch_gc (gnutls_session_t session);
void _gnutls_epoch_free (gnutls_session_t session,
                         record_parameters_st * state);

#endif
