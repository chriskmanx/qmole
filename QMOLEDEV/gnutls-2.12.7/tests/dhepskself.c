/*
 * Copyright (C) 2004, 2005, 2008, 2009, 2010 Free Software Foundation,
 * Inc.
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

/* Parts copied from GnuTLS example programs. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#if !defined(_WIN32)
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif
#include <unistd.h>
#include <gnutls/gnutls.h>

#include "tcp.c"

#include "utils.h"

/* A very basic TLS client, with PSK authentication.
 */

#define MAX_BUF 1024
#define MSG "Hello TLS"

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}

static void
client (void)
{
  int ret, sd, ii;
  gnutls_session_t session;
  char buffer[MAX_BUF + 1];
  gnutls_psk_client_credentials_t pskcred;
  const gnutls_datum_t key = { (char *) "DEADBEEF", 8 };

  gnutls_global_init ();

  gnutls_global_set_log_function (tls_log_func);
  if (debug)
    gnutls_global_set_log_level (5);

  gnutls_psk_allocate_client_credentials (&pskcred);
  gnutls_psk_set_client_credentials (pskcred, "test", &key,
                                     GNUTLS_PSK_KEY_HEX);

  /* Initialize TLS session
   */
  gnutls_init (&session, GNUTLS_CLIENT);

  /* Use default priorities */
  gnutls_priority_set_direct (session, "NORMAL:+DHE-PSK", NULL);

  /* put the anonymous credentials to the current session
   */
  gnutls_credentials_set (session, GNUTLS_CRD_PSK, pskcred);

  /* connect to the peer
   */
  sd = tcp_connect ();

  gnutls_transport_set_ptr (session, (gnutls_transport_ptr_t) sd);

  /* Perform the TLS handshake
   */
  ret = gnutls_handshake (session);

  if (ret < 0)
    {
      fail ("client: Handshake failed\n");
      gnutls_perror (ret);
      goto end;
    }
  else
    {
      if (debug)
        success ("client: Handshake was completed\n");
    }

  gnutls_record_send (session, MSG, strlen (MSG));

  ret = gnutls_record_recv (session, buffer, MAX_BUF);
  if (ret == 0)
    {
      if (debug)
        success ("client: Peer has closed the TLS connection\n");
      goto end;
    }
  else if (ret < 0)
    {
      fail ("client: Error: %s\n", gnutls_strerror (ret));
      goto end;
    }

  if (debug)
    {
      printf ("- Received %d bytes: ", ret);
      for (ii = 0; ii < ret; ii++)
        fputc (buffer[ii], stdout);
      fputs ("\n", stdout);
    }

  gnutls_bye (session, GNUTLS_SHUT_RDWR);

end:

  tcp_close (sd);

  gnutls_deinit (session);

  gnutls_psk_free_client_credentials (pskcred);

  gnutls_global_deinit ();
}

/* This is a sample TLS 1.0 echo server, for PSK authentication.
 */

#define SA struct sockaddr
#define MAX_BUF 1024
#define PORT 5556               /* listen to 5556 port */

/* These are global */
gnutls_psk_server_credentials_t server_pskcred;

static gnutls_session_t
initialize_tls_session (void)
{
  gnutls_session_t session;

  gnutls_init (&session, GNUTLS_SERVER);

  /* avoid calling all the priority functions, since the defaults
   * are adequate.
   */
  gnutls_priority_set_direct(session, "NORMAL:+DHE-PSK", NULL);

  gnutls_credentials_set (session, GNUTLS_CRD_PSK, server_pskcred);

  return session;
}

static gnutls_dh_params_t dh_params;

static int
generate_dh_params (void)
{
  const gnutls_datum_t p3 = { (char *) pkcs3, strlen (pkcs3) };
  /* Generate Diffie-Hellman parameters - for use with DHE
   * kx algorithms. These should be discarded and regenerated
   * once a day, once a week or once a month. Depending on the
   * security requirements.
   */
  gnutls_dh_params_init (&dh_params);
  return gnutls_dh_params_import_pkcs3 (dh_params, &p3, GNUTLS_X509_FMT_PEM);
}

static int
pskfunc (gnutls_session_t session, const char *username, gnutls_datum_t * key)
{
  if (debug)
    printf ("psk callback to get %s's password\n", username);
  key->data = gnutls_malloc (4);
  key->data[0] = 0xDE;
  key->data[1] = 0xAD;
  key->data[2] = 0xBE;
  key->data[3] = 0xEF;
  key->size = 4;
  return 0;
}

int err, listen_sd, i;
int sd, ret;
struct sockaddr_in sa_serv;
struct sockaddr_in sa_cli;
int client_len;
char topbuf[512];
gnutls_session_t session;
char buffer[MAX_BUF + 1];
int optval = 1;

static void
server_start (void)
{
  if (debug)
    success ("Launched, generating DH parameters...\n");

  /* Socket operations
   */
  listen_sd = socket (AF_INET, SOCK_STREAM, 0);
  if (err == -1)
    {
      perror ("socket");
      fail ("server: socket failed\n");
      return;
    }

  memset (&sa_serv, '\0', sizeof (sa_serv));
  sa_serv.sin_family = AF_INET;
  sa_serv.sin_addr.s_addr = INADDR_ANY;
  sa_serv.sin_port = htons (PORT);      /* Server Port number */

  setsockopt (listen_sd, SOL_SOCKET, SO_REUSEADDR, (void *) &optval,
              sizeof (int));

  err = bind (listen_sd, (SA *) & sa_serv, sizeof (sa_serv));
  if (err == -1)
    {
      perror ("bind");
      fail ("server: bind failed\n");
      return;
    }

  err = listen (listen_sd, 1024);
  if (err == -1)
    {
      perror ("listen");
      fail ("server: listen failed\n");
      return;
    }

  if (debug)
    success ("server: ready. Listening to port '%d'.\n", PORT);
}

static void
server (void)
{
  /* this must be called once in the program
   */
  gnutls_global_init ();

  gnutls_global_set_log_function (tls_log_func);
  if (debug)
    gnutls_global_set_log_level (4711);

  generate_dh_params ();

  gnutls_psk_allocate_server_credentials (&server_pskcred);
  gnutls_psk_set_server_credentials_function (server_pskcred, pskfunc);
  gnutls_psk_set_server_dh_params (server_pskcred, dh_params);

  client_len = sizeof (sa_cli);

  session = initialize_tls_session ();

  sd = accept (listen_sd, (SA *) & sa_cli, &client_len);

  if (debug)
    success ("server: connection from %s, port %d\n",
             inet_ntop (AF_INET, &sa_cli.sin_addr, topbuf,
                        sizeof (topbuf)), ntohs (sa_cli.sin_port));

  gnutls_transport_set_ptr (session, (gnutls_transport_ptr_t) sd);
  ret = gnutls_handshake (session);
  if (ret < 0)
    {
      close (sd);
      gnutls_deinit (session);
      fail ("server: Handshake has failed (%s)\n\n", gnutls_strerror (ret));
      return;
    }
  if (debug)
    success ("server: Handshake was completed\n");

  /* see the Getting peer's information example */
  /* print_info(session); */

  i = 0;
  for (;;)
    {
      memset (buffer, 0, MAX_BUF + 1);
      ret = gnutls_record_recv (session, buffer, MAX_BUF);

      if (ret == 0)
        {
          if (debug)
            success ("server: Peer has closed the GnuTLS connection\n");
          break;
        }
      else if (ret < 0)
        {
          fail ("server: Received corrupted data(%d). Closing...\n", ret);
          break;
        }
      else if (ret > 0)
        {
          /* echo data back to the client
           */
          gnutls_record_send (session, buffer, strlen (buffer));
        }
    }
  /* do not wait for the peer to close the connection.
   */
  gnutls_bye (session, GNUTLS_SHUT_WR);

  close (sd);
  gnutls_deinit (session);

  close (listen_sd);

  gnutls_psk_free_server_credentials (server_pskcred);

  gnutls_dh_params_deinit (dh_params);

  gnutls_global_deinit ();

  if (debug)
    success ("server: finished\n");
}

void
doit (void)
{
  pid_t child;

  server_start ();
  if (error_count)
    return;

  child = fork ();
  if (child < 0)
    {
      perror ("fork");
      fail ("fork");
      return;
    }

  if (child)
    {
      int status;
      /* parent */
      server ();
      wait (&status);
    }
  else
    client ();
}
