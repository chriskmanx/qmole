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

typedef enum Optional
{ OPTIONAL_PACKET, MANDATORY_PACKET } Optional;

int _gnutls_send_handshake (gnutls_session_t session, mbuffer_st * bufel,
                            gnutls_handshake_description_t type);
int _gnutls_recv_hello_request (gnutls_session_t session, void *data,
                                uint32_t data_size);
int _gnutls_send_hello (gnutls_session_t session, int again);
int _gnutls_recv_hello (gnutls_session_t session, opaque * data, int datalen);
int _gnutls_recv_handshake (gnutls_session_t session, uint8_t **, int *,
                            gnutls_handshake_description_t,
                            Optional optional);
int _gnutls_generate_session_id (opaque * session_id, uint8_t * len);
int _gnutls_handshake_common (gnutls_session_t session);
int _gnutls_handshake_client (gnutls_session_t session);
int _gnutls_handshake_server (gnutls_session_t session);
void _gnutls_set_server_random (gnutls_session_t session, uint8_t * rnd);
void _gnutls_set_client_random (gnutls_session_t session, uint8_t * rnd);
int _gnutls_tls_create_random (opaque * dst);
int _gnutls_remove_unwanted_ciphersuites (gnutls_session_t session,
                                          cipher_suite_st ** cipherSuites,
                                          int numCipherSuites,
                                          gnutls_pk_algorithm_t);
int _gnutls_find_pk_algos_in_ciphersuites (opaque * data, int datalen);
int _gnutls_server_select_suite (gnutls_session_t session, opaque * data,
                                 int datalen);

int _gnutls_negotiate_version (gnutls_session_t session,
                               gnutls_protocol_t adv_version);
int _gnutls_user_hello_func (gnutls_session_t session,
                             gnutls_protocol_t adv_version);

void _gnutls_handshake_hash_buffers_clear (gnutls_session_t session);

#define STATE session->internals.handshake_state
/* This returns true if we have got there
 * before (and not finished due to an interrupt).
 */
#define AGAIN(target) (STATE==target?1:0)
#define AGAIN2(state, target) (state==target?1:0)
