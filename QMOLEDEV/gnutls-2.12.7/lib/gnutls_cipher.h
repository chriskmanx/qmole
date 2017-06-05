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

int _gnutls_encrypt (gnutls_session_t session, const opaque * headers,
                     size_t headers_size, const opaque * data,
                     size_t data_size, opaque * ciphertext,
                     size_t ciphertext_size, content_type_t type,
                     int random_pad, record_parameters_st * params);

int _gnutls_decrypt (gnutls_session_t session, opaque * ciphertext,
                     size_t ciphertext_size, uint8_t * data, size_t data_size,
                     content_type_t type, record_parameters_st * params);
int _gnutls_compressed2ciphertext (gnutls_session_t session,
                                   opaque * cipher_data, int cipher_size,
                                   gnutls_datum_t compressed,
                                   content_type_t _type, int random_pad,
                                   record_parameters_st * params);
int _gnutls_ciphertext2compressed (gnutls_session_t session,
                                   opaque * compress_data,
                                   int compress_size,
                                   gnutls_datum_t ciphertext, uint8_t type,
                                   record_parameters_st * params);
