/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
 * 2009, 2010  Free Software Foundation, Inc.
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <unistd.h>
#include <fcntl.h>
#include <error.h>

#include <gnutls/gnutls.h>
#include <gnutls/extra.h>
#include <gnutls/x509.h>
#include <gnutls/openpgp.h>
#include <gnutls/pkcs11.h>

/* Gnulib portability files. */
#include <progname.h>
#include <version-etc.h>
#include <read-file.h>
#include <getpass.h>
#include <minmax.h>
#include "sockets.h"

#include "common.h"
#include "cli-gaa.h"

#define MAX_BUF 4096

/* global stuff here */
int resume, starttls, insecure, rehandshake;
const char *hostname = NULL;
char *service;
int record_max_size;
int fingerprint;
int crlf;
int verbose = 0;
extern int print_cert;

char *srp_passwd = NULL;
char *srp_username;
char *pgp_keyfile;
char *pgp_certfile;
char *pgp_keyring;
char *x509_keyfile;
char *x509_certfile;
char *x509_cafile;
char *x509_crlfile = NULL;
static int x509ctype;
static int disable_extensions;

char *psk_username = NULL;
gnutls_datum_t psk_key = { NULL, 0 };

static gnutls_srp_client_credentials_t srp_cred;
static gnutls_psk_client_credentials_t psk_cred;
static gnutls_anon_client_credentials_t anon_cred;
static gnutls_certificate_credentials_t xcred;

static gaainfo info;

/* end of global stuff */

/* prototypes */
typedef struct
{
  int fd;
  gnutls_session_t session;
  int secure;
  char *hostname;
  char *ip;
  char *service;
  struct addrinfo *ptr;
  struct addrinfo *addr_info;
} socket_st;

ssize_t socket_recv (const socket_st * socket, void *buffer, int buffer_size);
ssize_t socket_send (const socket_st * socket, const void *buffer,
                     int buffer_size);
void socket_open (socket_st * hd, const char *hostname, const char *service);
void socket_connect (const socket_st * hd);
void socket_bye (socket_st * socket);

static void check_rehandshake (socket_st * socket, int ret);
static int do_handshake (socket_st * socket);
static void init_global_tls_stuff (void);

/* Helper functions to load a certificate and key
 * files into memory.
 */
static gnutls_datum_t
load_file (const char *file)
{
  gnutls_datum_t loaded_file = { NULL, 0 };
  size_t length;

  loaded_file.data = read_binary_file (file, &length);
  if (loaded_file.data)
    loaded_file.size = (unsigned int) length;

  return loaded_file;
}

static void
unload_file (gnutls_datum_t data)
{
  free (data.data);
}

#define MAX_CRT 6
static unsigned int x509_crt_size;
static gnutls_x509_crt_t x509_crt[MAX_CRT];
static gnutls_x509_privkey_t x509_key = NULL;

static gnutls_pkcs11_privkey_t pkcs11_key = NULL;

static gnutls_openpgp_crt_t pgp_crt = NULL;
static gnutls_openpgp_privkey_t pgp_key = NULL;

static void
get_keyid (gnutls_openpgp_keyid_t keyid, const char *str)
{
  size_t keyid_size = sizeof (keyid);

  if (strlen (str) != 16)
    {
      fprintf (stderr,
               "The OpenPGP subkey ID has to be 16 hexadecimal characters.\n");
      exit (1);
    }

  if (gnutls_hex2bin (str, strlen (str), keyid, &keyid_size) < 0)
    {
      fprintf (stderr, "Error converting hex string: %s.\n", str);
      exit (1);
    }

  return;
}

/* Load the certificate and the private key.
 */
static void
load_keys (void)
{
  unsigned int crt_num;
  int ret;
  gnutls_datum_t data;

  if (x509_certfile != NULL && x509_keyfile != NULL)
    {
      if (strncmp (x509_certfile, "pkcs11:", 7) == 0)
        {
          crt_num = 1;
          gnutls_x509_crt_init (&x509_crt[0]);

          ret =
            gnutls_x509_crt_import_pkcs11_url (x509_crt[0], x509_certfile, 0);

          if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
            ret =
              gnutls_x509_crt_import_pkcs11_url (x509_crt[0], x509_certfile,
                                                 GNUTLS_PKCS11_OBJ_FLAG_LOGIN);

          if (ret < 0)
            {
              fprintf (stderr, "*** Error loading cert file.\n");
              exit (1);
            }
          x509_crt_size = 1;
        }
      else
        {

          data = load_file (x509_certfile);
          if (data.data == NULL)
            {
              fprintf (stderr, "*** Error loading cert file.\n");
              exit (1);
            }

          crt_num = MAX_CRT;
          ret =
            gnutls_x509_crt_list_import (x509_crt, &crt_num, &data,
                                         GNUTLS_X509_FMT_PEM,
                                         GNUTLS_X509_CRT_LIST_IMPORT_FAIL_IF_EXCEED);
          if (ret < 0)
            {
              if (ret == GNUTLS_E_SHORT_MEMORY_BUFFER)
                {
                  fprintf (stderr,
                           "*** Error loading cert file: Too many certs %d\n",
                           crt_num);

                }
              else
                {
                  fprintf (stderr,
                           "*** Error loading cert file: %s\n",
                           gnutls_strerror (ret));
                }
              exit (1);
            }
          x509_crt_size = ret;
        }
      fprintf (stderr, "Processed %d client certificates...\n", ret);

      unload_file (data);

      if (strncmp (x509_keyfile, "pkcs11:", 7) == 0)
        {
          gnutls_pkcs11_privkey_init (&pkcs11_key);

          ret =
            gnutls_pkcs11_privkey_import_url (pkcs11_key, x509_keyfile, 0);
          if (ret < 0)
            {
              fprintf (stderr, "*** Error loading url: %s\n",
                       gnutls_strerror (ret));
              exit (1);
            }
        }
      else
        {
          data = load_file (x509_keyfile);
          if (data.data == NULL)
            {
              fprintf (stderr, "*** Error loading key file.\n");
              exit (1);
            }

          gnutls_x509_privkey_init (&x509_key);

          ret =
            gnutls_x509_privkey_import (x509_key, &data, GNUTLS_X509_FMT_PEM);
          if (ret < 0)
            {
              fprintf (stderr, "*** Error loading key file: %s\n",
                       gnutls_strerror (ret));
              exit (1);
            }

          unload_file (data);
        }

      fprintf (stderr, "Processed %d client X.509 certificates...\n",
               x509_crt_size);
    }
#ifdef ENABLE_OPENPGP
  if (pgp_certfile != NULL && pgp_keyfile != NULL)
    {
      data = load_file (pgp_certfile);
      if (data.data == NULL)
        {
          fprintf (stderr, "*** Error loading PGP cert file.\n");
          exit (1);
        }
      gnutls_openpgp_crt_init (&pgp_crt);

      ret =
        gnutls_openpgp_crt_import (pgp_crt, &data, GNUTLS_OPENPGP_FMT_BASE64);
      if (ret < 0)
        {
          fprintf (stderr,
                   "*** Error loading PGP cert file: %s\n",
                   gnutls_strerror (ret));
          exit (1);
        }


      unload_file (data);

      if (strncmp (pgp_keyfile, "pkcs11:", 7) == 0)
        {
          gnutls_pkcs11_privkey_init (&pkcs11_key);

          ret = gnutls_pkcs11_privkey_import_url (pkcs11_key, pgp_keyfile, 0);
          if (ret < 0)
            {
              fprintf (stderr, "*** Error loading url: %s\n",
                       gnutls_strerror (ret));
              exit (1);
            }
        }
      else
        {

          data = load_file (pgp_keyfile);
          if (data.data == NULL)
            {
              fprintf (stderr, "*** Error loading PGP key file.\n");
              exit (1);
            }

          gnutls_openpgp_privkey_init (&pgp_key);

          ret =
            gnutls_openpgp_privkey_import (pgp_key, &data,
                                           GNUTLS_OPENPGP_FMT_BASE64, NULL,
                                           0);
          if (ret < 0)
            {
              fprintf (stderr,
                       "*** Error loading PGP key file: %s\n",
                       gnutls_strerror (ret));
              exit (1);
            }

          unload_file (data);
        }

      if (info.pgp_subkey != NULL)
        {
          uint8_t keyid[GNUTLS_OPENPGP_KEYID_SIZE];

          if (strcasecmp (info.pgp_subkey, "auto") == 0)
            {
              ret = gnutls_openpgp_crt_get_auth_subkey (pgp_crt, keyid, 1);
              if (ret < 0)
                {
                  fprintf (stderr,
                           "*** Error setting preferred sub key id (%s): %s\n",
                           info.pgp_subkey, gnutls_strerror (ret));
                  exit (1);
                }
            }
          else
            get_keyid (keyid, info.pgp_subkey);

          ret = gnutls_openpgp_crt_set_preferred_key_id (pgp_crt, keyid);
          if (ret >= 0)
            ret =
              gnutls_openpgp_privkey_set_preferred_key_id (pgp_key, keyid);
          if (ret < 0)
            {
              fprintf (stderr,
                       "*** Error setting preferred sub key id (%s): %s\n",
                       info.pgp_subkey, gnutls_strerror (ret));
              exit (1);
            }
        }

      fprintf (stderr, "Processed 1 client PGP certificate...\n");
    }
#endif

}

static int
cert_verify_callback (gnutls_session_t session)
{
  int rc;
  unsigned int status;

  if (!x509_cafile && !pgp_keyring)
    return 0;

  rc = gnutls_certificate_verify_peers2 (session, &status);
  if (rc != 0 || status != 0)
    {
      printf ("*** Verifying server certificate failed...\n");
      if (!insecure)
        return -1;
    }

  return 0;
}

/* This callback should be associated with a session by calling
 * gnutls_certificate_client_set_retrieve_function( session, cert_callback),
 * before a handshake.
 */

static int
cert_callback (gnutls_session_t session,
               const gnutls_datum_t * req_ca_rdn, int nreqs,
               const gnutls_pk_algorithm_t * sign_algos,
               int sign_algos_length, gnutls_retr2_st * st)
{
  char issuer_dn[256];
  int i, ret;
  size_t len;

  if (verbose)
    {
      /* Print the server's trusted CAs
       */
      if (nreqs > 0)
        printf ("- Server's trusted authorities:\n");
      else
        printf ("- Server did not send us any trusted authorities names.\n");

      /* print the names (if any) */
      for (i = 0; i < nreqs; i++)
        {
          len = sizeof (issuer_dn);
          ret = gnutls_x509_rdn_get (&req_ca_rdn[i], issuer_dn, &len);
          if (ret >= 0)
            {
              printf ("   [%d]: ", i);
              printf ("%s\n", issuer_dn);
            }
        }
    }

  /* Select a certificate and return it.
   * The certificate must be of any of the "sign algorithms"
   * supported by the server.
   */

  st->cert_type = gnutls_certificate_type_get (session);

  st->ncerts = 0;

  if (st->cert_type == GNUTLS_CRT_X509)
    {
      gnutls_sign_algorithm_t cert_algo, req_algo;
      int i, match = 0;

      if (x509_crt_size > 0)
        {
          ret = gnutls_x509_crt_get_signature_algorithm (x509_crt[0]);
          if (ret < 0)
            {
              /* error reading signature algorithm */
              return -1;
            }
          cert_algo = ret;

          i = 0;
          do
            {
              ret =
                gnutls_sign_algorithm_get_requested (session, i, &req_algo);
              if (ret >= 0 && cert_algo == req_algo)
                {
                  match = 1;
                  break;
                }

              /* server has not requested anything specific */
              if (i == 0 && ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
                {
                  match = 1;
                  break;
                }
              i++;
            }
          while (ret >= 0);

          if (match == 0)
            {
              printf
                ("- Could not find a suitable certificate to send to server\n");
              return -1;
            }

          if (x509_key != NULL)
            {
              st->key.x509 = x509_key;
              st->key_type = GNUTLS_PRIVKEY_X509;
            }
          else if (pkcs11_key != NULL)
            {
              st->key.pkcs11 = pkcs11_key;
              st->key_type = GNUTLS_PRIVKEY_PKCS11;
            }
          else
            {
              printf ("- Could not find a suitable key to send to server\n");
              return -1;
            }

          st->ncerts = x509_crt_size;

          st->cert.x509 = x509_crt;

          st->deinit_all = 0;

          return 0;
        }

    }
  else if (st->cert_type == GNUTLS_CRT_OPENPGP)
    {
      if (pgp_crt != NULL)
        {

          if (pgp_key != NULL)
            {
              st->key.pgp = pgp_key;
              st->key_type = GNUTLS_PRIVKEY_OPENPGP;
            }
          else if (pkcs11_key != NULL)
            {
              st->key.pkcs11 = pkcs11_key;
              st->key_type = GNUTLS_PRIVKEY_PKCS11;
            }
          else
            {
              printf ("- Could not find a suitable key to send to server\n");
              return -1;
            }

          st->ncerts = 1;

          st->cert.pgp = pgp_crt;

          st->deinit_all = 0;

          return 0;
        }
    }

  printf ("- Successfully sent %d certificate(s) to server.\n", st->ncerts);
  return 0;

}

/* initializes a gnutls_session_t with some defaults.
 */
static gnutls_session_t
init_tls_session (const char *hostname)
{
  const char *err;

  gnutls_session_t session;

  gnutls_init (&session, GNUTLS_CLIENT);

  if (gnutls_priority_set_direct (session, info.priorities, &err) < 0)
    {
      fprintf (stderr, "Syntax error at: %s\n", err);
      exit (1);
    }

  /* allow the use of private ciphersuites.
   */
  if (disable_extensions == 0)
    {
      gnutls_handshake_set_private_extensions (session, 1);
      gnutls_server_name_set (session, GNUTLS_NAME_DNS, hostname,
                              strlen (hostname));
    }

  gnutls_dh_set_prime_bits (session, 512);

  gnutls_credentials_set (session, GNUTLS_CRD_ANON, anon_cred);
  if (srp_cred)
    gnutls_credentials_set (session, GNUTLS_CRD_SRP, srp_cred);
  if (psk_cred)
    gnutls_credentials_set (session, GNUTLS_CRD_PSK, psk_cred);
  gnutls_credentials_set (session, GNUTLS_CRD_CERTIFICATE, xcred);

  gnutls_certificate_set_retrieve_function (xcred, cert_callback);
  gnutls_certificate_set_verify_function (xcred, cert_verify_callback);
  gnutls_certificate_set_verify_flags (xcred, 0);

  /* send the fingerprint */
#ifdef ENABLE_OPENPGP
  if (fingerprint != 0)
    gnutls_openpgp_send_cert (session, GNUTLS_OPENPGP_CERT_FINGERPRINT);
#endif

  /* use the max record size extension */
  if (record_max_size > 0 && disable_extensions == 0)
    {
      if (gnutls_record_set_max_size (session, record_max_size) < 0)
        {
          fprintf (stderr,
                   "Cannot set the maximum record size to %d.\n",
                   record_max_size);
          fprintf (stderr, "Possible values: 512, 1024, 2048, 4096.\n");
          exit (1);
        }
    }

#ifdef ENABLE_SESSION_TICKET
  if (disable_extensions == 0 && !info.noticket)
    gnutls_session_ticket_enable_client (session);
#endif

  return session;
}

static void gaa_parser (int argc, char **argv);

/* Returns zero if the error code was successfully handled.
 */
static int
handle_error (socket_st * hd, int err)
{
  int alert, ret;
  const char *err_type, *str;

  if (err >= 0)
    return 0;

  if (gnutls_error_is_fatal (err) == 0)
    {
      ret = 0;
      err_type = "Non fatal";
    }
  else
    {
      ret = err;
      err_type = "Fatal";
    }

  str = gnutls_strerror (err);
  if (str == NULL)
    str = str_unknown;
  fprintf (stderr, "*** %s error: %s\n", err_type, str);

  if (err == GNUTLS_E_WARNING_ALERT_RECEIVED
      || err == GNUTLS_E_FATAL_ALERT_RECEIVED)
    {
      alert = gnutls_alert_get (hd->session);
      str = gnutls_alert_get_name (alert);
      if (str == NULL)
        str = str_unknown;
      printf ("*** Received alert [%d]: %s\n", alert, str);

      /* In SRP if the alert is MISSING_SRP_USERNAME,
       * we should read the username/password and
       * call gnutls_handshake(). This is not implemented
       * here.
       */
    }

  check_rehandshake (hd, err);

  return ret;
}

int starttls_alarmed = 0;

#ifndef _WIN32
static void
starttls_alarm (int signum)
{
  starttls_alarmed = 1;
}
#endif

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}

#define IN_KEYBOARD 1
#define IN_NET 2
#define IN_NONE 0
/* returns IN_KEYBOARD for keyboard input and IN_NET for network input
 */
static int check_net_or_keyboard_input(socket_st* hd)
{
  int maxfd;
  fd_set rset;
  int err;
  struct timeval tv;

  do
    {
      FD_ZERO (&rset);
      FD_SET (fileno (stdin), &rset);
      FD_SET (hd->fd, &rset);

      maxfd = MAX (fileno (stdin), hd->fd);
      tv.tv_sec = 0;
      tv.tv_usec = 500 * 1000;

      if (hd->secure == 1)
        if (gnutls_record_check_pending(hd->session))
          return IN_NET;

      err = select (maxfd + 1, &rset, NULL, NULL, &tv);
      if (err < 0)
        continue;

      if (FD_ISSET (hd->fd, &rset))
        return IN_NET;


      if (FD_ISSET (fileno (stdin), &rset))
        return IN_KEYBOARD;
    }
  while(err == 0);
  
  return IN_NONE;
}

int
main (int argc, char **argv)
{
  int ret;
  int ii, i, inp;
  char buffer[MAX_BUF + 1];
  char *session_data = NULL;
  char *session_id = NULL;
  size_t session_data_size;
  size_t session_id_size = 0;
  int user_term = 0, retval = 0;
  socket_st hd;
  ssize_t bytes;

  set_program_name (argv[0]);

  if ((ret = gnutls_global_init ()) < 0)
    {
      fprintf (stderr, "global_init: %s\n", gnutls_strerror (ret));
      exit (1);
    }

  if ((ret = gnutls_global_init_extra ()) < 0)
    {
      fprintf (stderr, "global_init_extra: %s\n", gnutls_strerror (ret));
      exit (1);
    }

  pkcs11_common ();
  gaa_parser (argc, argv);
  if (hostname == NULL)
    {
      fprintf (stderr, "No hostname given\n");
      exit (1);
    }

  gnutls_global_set_log_function (tls_log_func);
  gnutls_global_set_log_level (info.debug);

  sockets_init ();

#ifndef _WIN32
  signal (SIGPIPE, SIG_IGN);
#endif

  init_global_tls_stuff ();

  socket_open (&hd, hostname, service);
  socket_connect (&hd);

  hd.session = init_tls_session (hostname);
  if (starttls)
    goto after_handshake;

  for (i = 0; i < 2; i++)
    {


      if (i == 1)
        {
          hd.session = init_tls_session (hostname);
          gnutls_session_set_data (hd.session, session_data,
                                   session_data_size);
          free (session_data);
        }

      ret = do_handshake (&hd);

      if (ret < 0)
        {
          fprintf (stderr, "*** Handshake has failed\n");
          gnutls_perror (ret);
          gnutls_deinit (hd.session);
          return 1;
        }
      else
        {
          printf ("- Handshake was completed\n");
          if (gnutls_session_is_resumed (hd.session) != 0)
            printf ("*** This is a resumed session\n");
        }

      if (resume != 0 && i == 0)
        {

          gnutls_session_get_data (hd.session, NULL, &session_data_size);
          session_data = malloc (session_data_size);

          gnutls_session_get_data (hd.session, session_data,
                                   &session_data_size);

          gnutls_session_get_id (hd.session, NULL, &session_id_size);

          session_id = malloc (session_id_size);
          gnutls_session_get_id (hd.session, session_id, &session_id_size);

          /* print some information */
          print_info (hd.session, hostname, info.insecure);

          printf ("- Disconnecting\n");
          socket_bye (&hd);

          printf
            ("\n\n- Connecting again- trying to resume previous session\n");
          socket_open (&hd, hostname, service);
          socket_connect (&hd);
        }
      else
        {
          break;
        }
    }

after_handshake:

  /* Warning!  Do not touch this text string, it is used by external
     programs to search for when gnutls-cli has reached this point. */
  printf ("\n- Simple Client Mode:\n\n");

  if (rehandshake)
    {
      ret = do_handshake (&hd);

      if (ret < 0)
        {
          fprintf (stderr, "*** ReHandshake has failed\n");
          gnutls_perror (ret);
          gnutls_deinit (hd.session);
          return 1;
        }
      else
        {
          printf ("- ReHandshake was completed\n");
        }
    }

#ifndef _WIN32
  signal (SIGALRM, &starttls_alarm);
#endif

  fflush (stdout);
  fflush (stderr);

  /* do not buffer */
#if !(defined _WIN32 || defined __WIN32__)
  setbuf (stdin, NULL);
#endif
  setbuf (stdout, NULL);
  setbuf (stderr, NULL);

  for (;;)
    {
      if (starttls_alarmed && !hd.secure)
        {
          /* Warning!  Do not touch this text string, it is used by
             external programs to search for when gnutls-cli has
             reached this point. */
          fprintf (stderr, "*** Starting TLS handshake\n");
          ret = do_handshake (&hd);
          if (ret < 0)
            {
              fprintf (stderr, "*** Handshake has failed\n");
              user_term = 1;
              retval = 1;
              break;
            }
        }

      inp = check_net_or_keyboard_input(&hd);

      if (inp == IN_NET)
        {
          memset (buffer, 0, MAX_BUF + 1);
          ret = socket_recv (&hd, buffer, MAX_BUF);

          if (ret == 0)
            {
              printf ("- Peer has closed the GnuTLS connection\n");
              break;
            }
          else if (handle_error (&hd, ret) < 0 && user_term == 0)
            {
              fprintf (stderr,
                       "*** Server has terminated the connection abnormally.\n");
              retval = 1;
              break;
            }
          else if (ret > 0)
            {
              if (verbose != 0)
                printf ("- Received[%d]: ", ret);
              for (ii = 0; ii < ret; ii++)
                {
                  fputc (buffer[ii], stdout);
                }
              fflush (stdout);
            }

          if (user_term != 0)
            break;
        }

      if (inp == IN_KEYBOARD)
        {
          if ((bytes = read (fileno (stdin), buffer, MAX_BUF - 1)) <= 0)
            {
              if (hd.secure == 0)
                {
                  /* Warning!  Do not touch this text string, it is
                     used by external programs to search for when
                     gnutls-cli has reached this point. */
                  fprintf (stderr, "*** Starting TLS handshake\n");
                  ret = do_handshake (&hd);
                  clearerr (stdin);
                  if (ret < 0)
                    {
                      fprintf (stderr, "*** Handshake has failed\n");
                      user_term = 1;
                      retval = 1;
                      break;
                    }
                }
              else
                {
                  user_term = 1;
                  break;
                }
              continue;
            }

          buffer[bytes] = 0;
          if (crlf != 0)
            {
              char *b = strchr (buffer, '\n');
              if (b != NULL)
                {
                  strcpy (b, "\r\n");
                  bytes++;
                }
            }

          ret = socket_send (&hd, buffer, bytes);

          if (ret > 0)
            {
              if (verbose != 0)
                printf ("- Sent: %d bytes\n", ret);
            }
          else
            handle_error (&hd, ret);

        }
    }

  if (user_term != 0)
    socket_bye (&hd);
  else
    gnutls_deinit (hd.session);

#ifdef ENABLE_SRP
  if (srp_cred)
    gnutls_srp_free_client_credentials (srp_cred);
#endif
#ifdef ENABLE_PSK
  if (psk_cred)
    gnutls_psk_free_client_credentials (psk_cred);
#endif

  gnutls_certificate_free_credentials (xcred);

#ifdef ENABLE_ANON
  gnutls_anon_free_client_credentials (anon_cred);
#endif

  gnutls_global_deinit ();

  return retval;
}

void
gaa_parser (int argc, char **argv)
{
  if (gaa (argc, argv, &info) != -1)
    {
      fprintf (stderr,
               "Error in the arguments. Use the --help or -h parameters to get more information.\n");
      exit (1);
    }

  verbose = info.verbose;
  disable_extensions = info.disable_extensions;
  print_cert = info.print_cert;
  starttls = info.starttls;
  resume = info.resume;
  rehandshake = info.rehandshake;
  insecure = info.insecure;
  service = info.port;
  record_max_size = info.record_size;
  fingerprint = info.fingerprint;

  if (info.fmtder == 0)
    x509ctype = GNUTLS_X509_FMT_PEM;
  else
    x509ctype = GNUTLS_X509_FMT_DER;

  srp_username = info.srp_username;
  srp_passwd = info.srp_passwd;
  x509_cafile = info.x509_cafile;
  x509_crlfile = info.x509_crlfile;
  x509_keyfile = info.x509_keyfile;
  x509_certfile = info.x509_certfile;
  pgp_keyfile = info.pgp_keyfile;
  pgp_certfile = info.pgp_certfile;

  psk_username = info.psk_username;
  psk_key.data = (unsigned char *) info.psk_key;
  if (info.psk_key != NULL)
    psk_key.size = strlen (info.psk_key);
  else
    psk_key.size = 0;

  pgp_keyring = info.pgp_keyring;

  crlf = info.crlf;

  if (info.rest_args == NULL)
    hostname = "localhost";
  else
    hostname = info.rest_args;
}

void cli_version (void);

void
cli_version (void)
{
  const char *p = PACKAGE_NAME;
  if (strcmp (gnutls_check_version (NULL), PACKAGE_VERSION) != 0)
    p = PACKAGE_STRING;
  version_etc (stdout, program_name, p, gnutls_check_version (NULL),
               "Nikos Mavrogiannopoulos", (char *) NULL);
}


static void
check_rehandshake (socket_st * socket, int ret)
{
  if (socket->secure && ret == GNUTLS_E_REHANDSHAKE)
    {
      /* There is a race condition here. If application
       * data is sent after the rehandshake request,
       * the server thinks we ignored his request.
       * This is a bad design of this client.
       */
      printf ("*** Received rehandshake request\n");
      /* gnutls_alert_send( session, GNUTLS_AL_WARNING, GNUTLS_A_NO_RENEGOTIATION); */

      ret = do_handshake (socket);

      if (ret == 0)
        {
          printf ("*** Rehandshake was performed.\n");
        }
      else
        {
          printf ("*** Rehandshake Failed.\n");
        }
    }
}


static int
do_handshake (socket_st * socket)
{
  int ret;

  gnutls_transport_set_ptr (socket->session,
                            (gnutls_transport_ptr_t)
                            gl_fd_to_handle (socket->fd));
  do
    {
      ret = gnutls_handshake (socket->session);

      if (ret < 0)
        {
          handle_error (socket, ret);
        }
    }
  while (ret < 0 && gnutls_error_is_fatal (ret) == 0);

  if (ret == 0)
    {
      /* print some information */
      print_info (socket->session, socket->hostname, info.insecure);


      socket->secure = 1;

    }
  else
    {
      gnutls_alert_send_appropriate (socket->session, ret);
      shutdown (socket->fd, SHUT_RDWR);
    }
  return ret;
}

static int
srp_username_callback (gnutls_session_t session,
                       char **username, char **password)
{
  if (srp_username == NULL || srp_passwd == NULL)
    {
      return -1;
    }

  *username = gnutls_strdup (srp_username);
  *password = gnutls_strdup (srp_passwd);

  return 0;
}

static int
psk_callback (gnutls_session_t session, char **username, gnutls_datum_t * key)
{
  const char *hint = gnutls_psk_client_get_hint (session);
  unsigned char *rawkey;
  char *passwd;
  int ret;
  size_t res_size;
  gnutls_datum_t tmp;

  printf ("- PSK client callback. ");
  if (hint)
    printf ("PSK hint '%s'\n", hint);
  else
    printf ("No PSK hint\n");

  if (info.psk_username)
    *username = gnutls_strdup (info.psk_username);
  else
    {
      char *tmp = NULL;
      size_t n;
      ssize_t len;

      printf ("Enter PSK identity: ");
      fflush (stdout);
      len = getline (&tmp, &n, stdin);

      if (tmp == NULL)
        {
          fprintf (stderr, "No username given, aborting...\n");
          return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
        }

      if (tmp[strlen (tmp) - 1] == '\n')
        tmp[strlen (tmp) - 1] = '\0';
      if (tmp[strlen (tmp) - 1] == '\r')
        tmp[strlen (tmp) - 1] = '\0';

      *username = gnutls_strdup (tmp);
      free (tmp);
    }
  if (!*username)
    return GNUTLS_E_MEMORY_ERROR;

  passwd = getpass ("Enter key: ");
  if (passwd == NULL)
    {
      fprintf (stderr, "No key given, aborting...\n");
      return GNUTLS_E_INSUFFICIENT_CREDENTIALS;
    }

  tmp.data = passwd;
  tmp.size = strlen (passwd);

  res_size = tmp.size / 2 + 1;
  rawkey = gnutls_malloc (res_size);
  if (rawkey == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  ret = gnutls_hex_decode (&tmp, rawkey, &res_size);
  if (ret < 0)
    {
      fprintf (stderr, "Error deriving password: %s\n",
               gnutls_strerror (ret));
      gnutls_free (*username);
      return ret;
    }

  key->data = rawkey;
  key->size = res_size;

  if (info.debug)
    {
      char hexkey[41];
      res_size = sizeof (hexkey);
      gnutls_hex_encode (key, hexkey, &res_size);
      fprintf (stderr, "PSK username: %s\n", *username);
      fprintf (stderr, "PSK hint: %s\n", hint);
      fprintf (stderr, "PSK key: %s\n", hexkey);
    }

  return 0;
}

static void
init_global_tls_stuff (void)
{
  int ret;

  /* X509 stuff */
  if (gnutls_certificate_allocate_credentials (&xcred) < 0)
    {
      fprintf (stderr, "Certificate allocation memory error\n");
      exit (1);
    }

  if (x509_cafile != NULL)
    {
      ret = gnutls_certificate_set_x509_trust_file (xcred,
                                                    x509_cafile, x509ctype);
      if (ret < 0)
        {
          fprintf (stderr, "Error setting the x509 trust file\n");
        }
      else
        {
          printf ("Processed %d CA certificate(s).\n", ret);
        }
    }
#ifdef ENABLE_PKI
  if (x509_crlfile != NULL)
    {
      ret = gnutls_certificate_set_x509_crl_file (xcred, x509_crlfile,
                                                  x509ctype);
      if (ret < 0)
        {
          fprintf (stderr, "Error setting the x509 CRL file\n");
        }
      else
        {
          printf ("Processed %d CRL(s).\n", ret);
        }
    }
#endif

  load_keys ();

#ifdef ENABLE_OPENPGP
  if (pgp_keyring != NULL)
    {
      ret =
        gnutls_certificate_set_openpgp_keyring_file (xcred, pgp_keyring,
                                                     GNUTLS_OPENPGP_FMT_BASE64);
      if (ret < 0)
        {
          fprintf (stderr, "Error setting the OpenPGP keyring file\n");
        }
    }
#endif

#ifdef ENABLE_SRP
  if (srp_username && srp_passwd)
    {
      /* SRP stuff */
      if (gnutls_srp_allocate_client_credentials (&srp_cred) < 0)
        {
          fprintf (stderr, "SRP authentication error\n");
        }

      gnutls_srp_set_client_credentials_function (srp_cred,
                                                  srp_username_callback);
    }
#endif

#ifdef ENABLE_PSK
  /* PSK stuff */
  if (gnutls_psk_allocate_client_credentials (&psk_cred) < 0)
    {
      fprintf (stderr, "PSK authentication error\n");
    }

  if (psk_username && psk_key.data)
    {
      ret = gnutls_psk_set_client_credentials (psk_cred,
                                               psk_username, &psk_key,
                                               GNUTLS_PSK_KEY_HEX);
      if (ret < 0)
        {
          fprintf (stderr, "Error setting the PSK credentials: %s\n",
                   gnutls_strerror (ret));
        }
    }
  gnutls_psk_set_client_credentials_function (psk_cred, psk_callback);
#endif

#ifdef ENABLE_ANON
  /* ANON stuff */
  if (gnutls_anon_allocate_client_credentials (&anon_cred) < 0)
    {
      fprintf (stderr, "Anonymous authentication error\n");
    }
#endif

}

/* Functions to manipulate sockets
 */

ssize_t
socket_recv (const socket_st * socket, void *buffer, int buffer_size)
{
  int ret;

  if (socket->secure)
    do
      {
        ret = gnutls_record_recv (socket->session, buffer, buffer_size);
      }
    while (ret == GNUTLS_E_INTERRUPTED || ret == GNUTLS_E_AGAIN);
  else
    do
      {
        ret = recv (socket->fd, buffer, buffer_size, 0);
      }
    while (ret == -1 && errno == EINTR);

  return ret;
}

ssize_t
socket_send (const socket_st * socket, const void *buffer, int buffer_size)
{
  int ret;

  if (socket->secure)
    do
      {
        ret = gnutls_record_send (socket->session, buffer, buffer_size);
      }
    while (ret == GNUTLS_E_AGAIN || ret == GNUTLS_E_INTERRUPTED);
  else
    do
      {
        ret = send (socket->fd, buffer, buffer_size, 0);
      }
    while (ret == -1 && errno == EINTR);

  if (ret > 0 && ret != buffer_size && verbose)
    fprintf (stderr,
             "*** Only sent %d bytes instead of %d.\n", ret, buffer_size);

  return ret;
}

void
socket_bye (socket_st * socket)
{
  int ret;
  if (socket->secure)
    {
      do
        ret = gnutls_bye (socket->session, GNUTLS_SHUT_WR);
      while (ret == GNUTLS_E_INTERRUPTED || ret == GNUTLS_E_AGAIN);
      if (ret < 0)
        fprintf (stderr, "*** gnutls_bye() error: %s\n",
                 gnutls_strerror (ret));
      gnutls_deinit (socket->session);
      socket->session = NULL;
    }

  freeaddrinfo (socket->addr_info);
  socket->addr_info = socket->ptr = NULL;

  free (socket->ip);
  free (socket->hostname);
  free (socket->service);

  shutdown (socket->fd, SHUT_RDWR);     /* no more receptions */
  close (socket->fd);

  socket->fd = -1;
  socket->secure = 0;
}

void
socket_connect (const socket_st * hd)
{
  int err;

  printf ("Connecting to '%s:%s'...\n", hd->ip, hd->service);

  err = connect (hd->fd, hd->ptr->ai_addr, hd->ptr->ai_addrlen);
  if (err < 0)
    {
      fprintf (stderr, "Cannot connect to %s:%s: %s\n", hd->hostname,
               hd->service, strerror (errno));
      exit (1);
    }
}

void
socket_open (socket_st * hd, const char *hostname, const char *service)
{
  struct addrinfo hints, *res, *ptr;
  int sd, err;
  char buffer[MAX_BUF + 1];
  char portname[16] = { 0 };

  printf ("Resolving '%s'...\n", hostname);
  /* get server name */
  memset (&hints, 0, sizeof (hints));
  hints.ai_socktype = SOCK_STREAM;
  if ((err = getaddrinfo (hostname, service, &hints, &res)))
    {
      fprintf (stderr, "Cannot resolve %s:%s: %s\n", hostname, service,
               gai_strerror (err));
      exit (1);
    }

  sd = -1;
  for (ptr = res; ptr != NULL; ptr = ptr->ai_next)
    {
      sd = socket (ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol);
      if (sd == -1)
        continue;

      if ((err = getnameinfo (ptr->ai_addr, ptr->ai_addrlen, buffer, MAX_BUF,
                              portname, sizeof (portname),
                              NI_NUMERICHOST | NI_NUMERICSERV)) != 0)
        {
          fprintf (stderr, "getnameinfo(): %s\n", gai_strerror (err));
          freeaddrinfo (res);
          exit (1);
        }

      break;
    }

  if (sd == -1)
    {
      fprintf (stderr, "socket(): %s\n", strerror (errno));
      exit (1);
    }

  hd->secure = 0;
  hd->fd = sd;
  hd->hostname = strdup (hostname);
  hd->ip = strdup (buffer);
  hd->service = strdup (portname);
  hd->ptr = ptr;
  hd->addr_info = res;

  return;
}
