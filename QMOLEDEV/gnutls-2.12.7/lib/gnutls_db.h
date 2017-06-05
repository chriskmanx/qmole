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

int _gnutls_server_register_current_session (gnutls_session_t session);
int _gnutls_server_restore_session (gnutls_session_t session,
                                    uint8_t * session_id,
                                    int session_id_size);
int _gnutls_db_remove_session (gnutls_session_t session, uint8_t * session_id,
                               int session_id_size);
int _gnutls_store_session (gnutls_session_t session,
                           gnutls_datum_t session_id,
                           gnutls_datum_t session_data);
gnutls_datum_t _gnutls_retrieve_session (gnutls_session_t session,
                                         gnutls_datum_t session_id);
int _gnutls_remove_session (gnutls_session_t session,
                            gnutls_datum_t session_id);
