/*
 * Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software
 * Foundation, Inc.
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
#include <netinet/in.h>
#include <sys/socket.h>
#if !defined(_WIN32)
#include <sys/wait.h>
#include <arpa/inet.h>
#endif
#include <unistd.h>
#include <gnutls/gnutls.h>

#include "tcp.c"

#include "utils.h"

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}

/* A very basic TLS client, with anonymous authentication.
 */

#define MAX_BUF 1024
#define MSG "Hello TLS"

static void
client (void)
{
  int ret, sd, ii;
  gnutls_session_t session;
  char buffer[MAX_BUF + 1];
  gnutls_anon_client_credentials_t anoncred;
  /* Need to enable anonymous KX specifically. */

  gnutls_global_init ();

  gnutls_global_set_log_function (tls_log_func);
  if (debug)
    gnutls_global_set_log_level (4711);

  gnutls_anon_allocate_client_credentials (&anoncred);

  /* Initialize TLS session
   */
  gnutls_init (&session, GNUTLS_CLIENT);

  /* Use default priorities */
  gnutls_priority_set_direct (session, "NONE:+VERS-TLS-ALL:+CIPHER-ALL:+MAC-ALL:+SIGN-ALL:+COMP-ALL:+ANON-DH", NULL);

  /* put the anonymous credentials to the current session
   */
  gnutls_credentials_set (session, GNUTLS_CRD_ANON, anoncred);

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

  if (debug)
    success ("client: TLS version is: %s\n",
             gnutls_protocol_get_name (gnutls_protocol_get_version
                                       (session)));

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
        {
          fputc (buffer[ii], stdout);
        }
      fputs ("\n", stdout);
    }

  gnutls_bye (session, GNUTLS_SHUT_RDWR);

end:

  tcp_close (sd);

  gnutls_deinit (session);

  gnutls_anon_free_client_credentials (anoncred);

  gnutls_global_deinit ();
}

/* This is a sample TLS 1.0 echo server, for anonymous authentication only.
 */

#define SA struct sockaddr
#define MAX_BUF 1024
#define PORT 5556               /* listen to 5556 port */
#define DH_BITS 1024

/* These are global */
gnutls_anon_server_credentials_t anoncred;

static gnutls_session_t
initialize_tls_session (void)
{
  gnutls_session_t session;

  gnutls_init (&session, GNUTLS_SERVER);

  /* avoid calling all the priority functions, since the defaults
   * are adequate.
   */
  gnutls_priority_set_direct (session, "NONE:+VERS-TLS-ALL:+CIPHER-ALL:+MAC-ALL:+SIGN-ALL:+COMP-ALL:+ANON-DH", NULL);

  gnutls_credentials_set (session, GNUTLS_CRD_ANON, anoncred);

  gnutls_dh_set_prime_bits (session, DH_BITS);

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

  gnutls_anon_allocate_server_credentials (&anoncred);

  if (debug)
    success ("Launched, generating DH parameters...\n");

  generate_dh_params ();

  gnutls_anon_set_server_dh_params (anoncred, dh_params);

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

  if (debug)
    success ("server: TLS version is: %s\n",
             gnutls_protocol_get_name (gnutls_protocol_get_version
                                       (session)));

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

  gnutls_anon_free_server_credentials (anoncred);

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
