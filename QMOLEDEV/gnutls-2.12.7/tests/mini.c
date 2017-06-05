/*
 * Copyright (C) 2008, 2010 Free Software Foundation, Inc.
 *
 * Author: Simon Josefsson
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GnuTLS; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <gnutls/gnutls.h>
#include "eagain-common.h"

#include "utils.h"

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}

#define MAX_BUF 1024
#define MSG "Hello TLS"

void
doit (void)
{
  /* Server stuff. */
  gnutls_anon_server_credentials_t s_anoncred;
  const gnutls_datum_t p3 = { (char *) pkcs3, strlen (pkcs3) };
  static gnutls_dh_params_t dh_params;
  gnutls_session_t server;
  int sret = GNUTLS_E_AGAIN;
  /* Client stuff. */
  gnutls_anon_client_credentials_t c_anoncred;
  gnutls_session_t client;
  int cret = GNUTLS_E_AGAIN;
  /* Need to enable anonymous KX specifically. */
  char buffer[MAX_BUF + 1];
  ssize_t ns;
  int ret, transferred = 0, msglen;

  /* General init. */
  gnutls_global_init ();
  gnutls_global_set_log_function (tls_log_func);
  if (debug)
    gnutls_global_set_log_level (4711);

  /* Init server */
  gnutls_anon_allocate_server_credentials (&s_anoncred);
  gnutls_dh_params_init (&dh_params);
  gnutls_dh_params_import_pkcs3 (dh_params, &p3, GNUTLS_X509_FMT_PEM);
  gnutls_anon_set_server_dh_params (s_anoncred, dh_params);
  gnutls_init (&server, GNUTLS_SERVER);
  gnutls_priority_set_direct (server, "NONE:+VERS-TLS-ALL:+CIPHER-ALL:+MAC-ALL:+SIGN-ALL:+COMP-ALL:+ANON-DH", NULL);
  gnutls_credentials_set (server, GNUTLS_CRD_ANON, s_anoncred);
  gnutls_dh_set_prime_bits (server, 1024);
  gnutls_transport_set_push_function (server, server_push);
  gnutls_transport_set_pull_function (server, server_pull);
  gnutls_transport_set_ptr (server, (gnutls_transport_ptr_t)server);

  /* Init client */
  gnutls_anon_allocate_client_credentials (&c_anoncred);
  gnutls_init (&client, GNUTLS_CLIENT);
  gnutls_priority_set_direct (client, "NONE:+VERS-TLS-ALL:+CIPHER-ALL:+MAC-ALL:+SIGN-ALL:+COMP-ALL:+ANON-DH", NULL);
  gnutls_credentials_set (client, GNUTLS_CRD_ANON, c_anoncred);
  gnutls_transport_set_push_function (client, client_push);
  gnutls_transport_set_pull_function (client, client_pull);
  gnutls_transport_set_ptr (client, (gnutls_transport_ptr_t)client);

  HANDSHAKE(client, server);

  if (debug)
    success ("Handshake established\n");

  msglen = strlen(MSG);
  TRANSFER(client, server, MSG, msglen, buffer, MAX_BUF);
  if (debug)
    fputs ("\n", stdout);

  gnutls_bye (client, GNUTLS_SHUT_RDWR);
  gnutls_bye (server, GNUTLS_SHUT_RDWR);

  gnutls_deinit (client);
  gnutls_deinit (server);

  gnutls_anon_free_client_credentials (c_anoncred);
  gnutls_anon_free_server_credentials (s_anoncred);

  gnutls_dh_params_deinit (dh_params);

  gnutls_global_deinit ();
}
