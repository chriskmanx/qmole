/*
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2008, 2010 Free Software
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

#ifndef AUTH_SRP_H
#define AUTH_SRP_H

#include <gnutls_auth.h>

typedef struct gnutls_srp_client_credentials_st
{
  char *username;
  char *password;
  gnutls_srp_client_credentials_function *get_function;
} srp_client_credentials_st;

typedef struct gnutls_srp_server_credentials_st
{
  char *password_file;
  char *password_conf_file;
  /* callback function, instead of reading the
   * password files.
   */
  gnutls_srp_server_credentials_function *pwd_callback;
} srp_server_cred_st;

/* these structures should not use allocated data */
typedef struct srp_server_auth_info_st
{
  char username[MAX_USERNAME_SIZE + 1];
} *srp_server_auth_info_t;

#ifdef ENABLE_SRP

int _gnutls_proc_srp_server_hello (gnutls_session_t state,
                                   const opaque * data, size_t data_size);
int _gnutls_gen_srp_server_hello (gnutls_session_t state, opaque * data,
                                  size_t data_size);

int _gnutls_gen_srp_server_kx (gnutls_session_t, opaque **);
int _gnutls_gen_srp_client_kx (gnutls_session_t, opaque **);

int _gnutls_proc_srp_server_kx (gnutls_session_t, opaque *, size_t);
int _gnutls_proc_srp_client_kx (gnutls_session_t, opaque *, size_t);

typedef struct srp_server_auth_info_st srp_server_auth_info_st;

#endif /* ENABLE_SRP */

#endif
