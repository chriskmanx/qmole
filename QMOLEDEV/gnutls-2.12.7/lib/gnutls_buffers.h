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
#ifndef GNUTLS_BUFFERS_H
#define GNUTLS_BUFFERS_H

#define MBUFFER_FLUSH 1

int _gnutls_record_buffer_put (content_type_t type,
                               gnutls_session_t session, opaque * data,
                               size_t length);
int _gnutls_record_buffer_get_size (content_type_t type,
                                    gnutls_session_t session);
int _gnutls_record_buffer_get (content_type_t type,
                               gnutls_session_t session, opaque * data,
                               size_t length);
ssize_t _gnutls_io_read_buffered (gnutls_session_t, size_t n, content_type_t);
int _gnutls_io_clear_peeked_data (gnutls_session_t session);

ssize_t _gnutls_io_write_buffered (gnutls_session_t session,
                                   mbuffer_st * bufel, unsigned int mflag);

int _gnutls_handshake_buffer_get_size (gnutls_session_t session);
int _gnutls_handshake_buffer_put (gnutls_session_t session, opaque * data,
                                  size_t length);
int _gnutls_handshake_buffer_clear (gnutls_session_t session);
int _gnutls_handshake_buffer_empty (gnutls_session_t session);
int _gnutls_handshake_buffer_get_ptr (gnutls_session_t session,
                                      opaque ** data_ptr, size_t * length);

#define _gnutls_handshake_io_buffer_clear( session) \
        _mbuffer_clear( &session->internals.handshake_send_buffer); \
        _gnutls_buffer_clear( &session->internals.handshake_recv_buffer);

ssize_t _gnutls_handshake_io_recv_int (gnutls_session_t, content_type_t,
                                       gnutls_handshake_description_t, void *,
                                       size_t);
void _gnutls_handshake_io_cache_int (gnutls_session_t,
                                     gnutls_handshake_description_t,
                                     mbuffer_st * bufel);
ssize_t _gnutls_io_write_flush (gnutls_session_t session);
ssize_t _gnutls_handshake_io_write_flush (gnutls_session_t session);

#endif
