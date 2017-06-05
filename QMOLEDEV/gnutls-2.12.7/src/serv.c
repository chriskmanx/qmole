/*
 * Copyright (C) 2004, 2006, 2007, 2008, 2009, 2010 Free Software
 * Foundation, Inc.
 * Copyright (C) 2001,2002 Paul Sheer
 * Portions Copyright (C) 2002,2003 Nikos Mavrogiannopoulos
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

/* This server is heavily modified for GnuTLS by Nikos Mavrogiannopoulos
 * (which means it is quite unreadable)
 */

#include <config.h>

#include "common.h"
#include "serv-gaa.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <string.h>
#include <gnutls/gnutls.h>
#include <gnutls/extra.h>
#include <gnutls/openpgp.h>
#include <sys/time.h>
#include <sys/select.h>
#include <fcntl.h>
#include <list.h>
#include <netdb.h>
#include <error.h>

/* Gnulib portability files. */
#include "progname.h"
#include "version-etc.h"
#include "read-file.h"
#include "minmax.h"
#include "sockets.h"

/* konqueror cannot handle sending the page in multiple
 * pieces.
 */
/* global stuff */
static int generate = 0;
static int http = 0;
static int port = 0;
static int x509ctype;
static int debug;

int verbose;
static int nodb;
static int noticket;
int require_cert;
int disable_client_cert;

char *psk_passwd;
char *srp_passwd;
char *srp_passwd_conf;
char *pgp_keyring;
char *pgp_keyfile;
char *pgp_certfile;
char *x509_keyfile;
char *x509_certfile;
char *x509_dsakeyfile;
char *x509_dsacertfile;
char *x509_cafile;
char *dh_params_file;
char *x509_crlfile = NULL;

gnutls_datum_t session_ticket_key;

/* end of globals */

/* This is a sample TCP echo server.
 * This will behave as an http server if any argument in the
 * command line is present
 */

#define SMALL_READ_TEST (2147483647)

#define GERR(ret) fprintf(stdout, "Error: %s\n", safe_strerror(ret))

#define HTTP_END  "</BODY></HTML>\n\n"

#define HTTP_UNIMPLEMENTED "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\r\n<HTML><HEAD>\r\n<TITLE>501 Method Not Implemented</TITLE>\r\n</HEAD><BODY>\r\n<H1>Method Not Implemented</H1>\r\n<HR>\r\n</BODY></HTML>\r\n"
#define HTTP_OK "HTTP/1.0 200 OK\r\nContent-type: text/html\r\n\r\n"

#define HTTP_BEGIN HTTP_OK \
		"\n" \
		"<HTML><BODY>\n" \
		"<CENTER><H1>This is <a href=\"http://www.gnu.org/software/gnutls\">" \
		"GnuTLS</a></H1></CENTER>\n\n"

/* These are global */
gnutls_srp_server_credentials_t srp_cred = NULL;
gnutls_psk_server_credentials_t psk_cred = NULL;
gnutls_anon_server_credentials_t dh_cred = NULL;
gnutls_certificate_credentials_t cert_cred = NULL;

static gaainfo info;

const int ssl_session_cache = 128;

static void wrap_db_init (void);
static void wrap_db_deinit (void);
static int wrap_db_store (void *dbf, gnutls_datum_t key, gnutls_datum_t data);
static gnutls_datum_t wrap_db_fetch (void *dbf, gnutls_datum_t key);
static int wrap_db_delete (void *dbf, gnutls_datum_t key);


#define HTTP_STATE_REQUEST	1
#define HTTP_STATE_RESPONSE	2
#define HTTP_STATE_CLOSING	3

LIST_TYPE_DECLARE (listener_item, char *http_request; char *http_response;
                   int request_length; int response_length;
                   int response_written; int http_state;
                   int listen_socket; int fd;
                   gnutls_session_t tls_session;
                   int handshake_ok;
  );

static const char *
safe_strerror (int value)
{
  const char *ret = gnutls_strerror (value);
  if (ret == NULL)
    ret = str_unknown;
  return ret;
}

static void
listener_free (listener_item * j)
{

  free (j->http_request);
  free (j->http_response);
  if (j->fd >= 0)
    {
      gnutls_bye (j->tls_session, GNUTLS_SHUT_WR);
      shutdown (j->fd, 2);
      close (j->fd);
      gnutls_deinit (j->tls_session);
    }
}


/* we use primes up to 1024 in this server.
 * otherwise we should add them here.
 */

gnutls_dh_params_t dh_params = NULL;
gnutls_rsa_params_t rsa_params = NULL;

static int
generate_dh_primes (void)
{
  int prime_bits = 768;

  if (gnutls_dh_params_init (&dh_params) < 0)
    {
      fprintf (stderr, "Error in dh parameter initialization\n");
      exit (1);
    }

  /* Generate Diffie-Hellman parameters - for use with DHE
   * kx algorithms. These should be discarded and regenerated
   * once a week or once a month. Depends on the
   * security requirements.
   */
  printf
    ("Generating Diffie-Hellman parameters [%d]. Please wait...\n",
     prime_bits);
  fflush (stdout);

  if (gnutls_dh_params_generate2 (dh_params, prime_bits) < 0)
    {
      fprintf (stderr, "Error in prime generation\n");
      exit (1);
    }

  return 0;
}

static void
read_dh_params (void)
{
  char tmpdata[2048];
  int size;
  gnutls_datum_t params;
  FILE *fd;

  if (gnutls_dh_params_init (&dh_params) < 0)
    {
      fprintf (stderr, "Error in dh parameter initialization\n");
      exit (1);
    }

  /* read the params file
   */
  fd = fopen (dh_params_file, "r");
  if (fd == NULL)
    {
      fprintf (stderr, "Could not open %s\n", dh_params_file);
      exit (1);
    }

  size = fread (tmpdata, 1, sizeof (tmpdata) - 1, fd);
  tmpdata[size] = 0;
  fclose (fd);

  params.data = (unsigned char *) tmpdata;
  params.size = size;

  size =
    gnutls_dh_params_import_pkcs3 (dh_params, &params, GNUTLS_X509_FMT_PEM);

  if (size < 0)
    {
      fprintf (stderr, "Error parsing dh params: %s\n", safe_strerror (size));
      exit (1);
    }

  printf ("Read Diffie-Hellman parameters.\n");
  fflush (stdout);

}

static char pkcs3[] =
  "-----BEGIN DH PARAMETERS-----\n"
  "MIGGAoGAtkxw2jlsVCsrfLqxrN+IrF/3W8vVFvDzYbLmxi2GQv9s/PQGWP1d9i22\n"
  "P2DprfcJknWt7KhCI1SaYseOQIIIAYP78CfyIpGScW/vS8khrw0rlQiyeCvQgF3O\n"
  "GeGOEywcw+oQT4SmFOD7H0smJe2CNyjYpexBXQ/A0mbTF9QKm1cCAQU=\n"
  "-----END DH PARAMETERS-----\n";

static int
static_dh_params (void)
{
  gnutls_datum_t params = { pkcs3, sizeof (pkcs3) };
  int ret;

  if (gnutls_dh_params_init (&dh_params) < 0)
    {
      fprintf (stderr, "Error in dh parameter initialization\n");
      exit (1);
    }

  ret = gnutls_dh_params_import_pkcs3 (dh_params, &params,
                                       GNUTLS_X509_FMT_PEM);

  if (ret < 0)
    {
      fprintf (stderr, "Error parsing dh params: %s\n", safe_strerror (ret));
      exit (1);
    }

  printf ("Set static Diffie-Hellman parameters, consider --dhparams.\n");

  return 0;
}

static int
get_params (gnutls_session_t session, gnutls_params_type_t type,
            gnutls_params_st * st)
{

  if (type == GNUTLS_PARAMS_RSA_EXPORT)
    {
      if (rsa_params == NULL)
        return -1;
      st->params.rsa_export = rsa_params;
    }
  else if (type == GNUTLS_PARAMS_DH)
    {
      if (dh_params == NULL)
        return -1;
      st->params.dh = dh_params;
    }
  else
    return -1;

  st->type = type;
  st->deinit = 0;

  return 0;
}

static int
generate_rsa_params (void)
{
  if (gnutls_rsa_params_init (&rsa_params) < 0)
    {
      fprintf (stderr, "Error in rsa parameter initialization\n");
      exit (1);
    }

  /* Generate RSA parameters - for use with RSA-export
   * cipher suites. These should be discarded and regenerated
   * once a day, once every 500 transactions etc. Depends on the
   * security requirements.
   */
  printf ("Generating temporary RSA parameters. Please wait...\n");
  fflush (stdout);

  if (gnutls_rsa_params_generate2 (rsa_params, 512) < 0)
    {
      fprintf (stderr, "Error in rsa parameter generation\n");
      exit (1);
    }

  return 0;
}

LIST_DECLARE_INIT (listener_list, listener_item, listener_free);

static gnutls_session_t
initialize_session (void)
{
  gnutls_session_t session;
  const char *err;

  gnutls_init (&session, GNUTLS_SERVER);

  /* allow the use of private ciphersuites.
   */
  gnutls_handshake_set_private_extensions (session, 1);

  if (nodb == 0)
    {
      gnutls_db_set_retrieve_function (session, wrap_db_fetch);
      gnutls_db_set_remove_function (session, wrap_db_delete);
      gnutls_db_set_store_function (session, wrap_db_store);
      gnutls_db_set_ptr (session, NULL);
    }
#ifdef ENABLE_SESSION_TICKET
  if (noticket == 0)
    gnutls_session_ticket_enable_server (session, &session_ticket_key);
#endif

  if (gnutls_priority_set_direct (session, info.priorities, &err) < 0)
    {
      fprintf (stderr, "Syntax error at: %s\n", err);
      exit (1);
    }

  gnutls_credentials_set (session, GNUTLS_CRD_ANON, dh_cred);

  if (srp_cred != NULL)
    gnutls_credentials_set (session, GNUTLS_CRD_SRP, srp_cred);

  if (psk_cred != NULL)
    gnutls_credentials_set (session, GNUTLS_CRD_PSK, psk_cred);

  if (cert_cred != NULL)
    gnutls_credentials_set (session, GNUTLS_CRD_CERTIFICATE, cert_cred);

  if (disable_client_cert)
    gnutls_certificate_server_set_request (session, GNUTLS_CERT_IGNORE);
  else
    {
      if (require_cert)
        gnutls_certificate_server_set_request (session, GNUTLS_CERT_REQUIRE);
      else
        gnutls_certificate_server_set_request (session, GNUTLS_CERT_REQUEST);
    }

  return session;
}

#include <gnutls/x509.h>

static const char DEFAULT_DATA[] =
  "This is the default message reported by the GnuTLS implementation. "
  "For more information please visit "
  "<a href=\"http://www.gnutls.org/\">http://www.gnutls.org/</a>.";

/* Creates html with the current session information.
 */
#define tmp_buffer &http_buffer[strlen(http_buffer)]
#define tmp_buffer_size len-strlen(http_buffer)
static char *
peer_print_info (gnutls_session_t session, int *ret_length,
                 const char *header)
{
  const char *tmp;
  unsigned char sesid[32];
  size_t i, sesid_size;
  char *http_buffer;
  gnutls_kx_algorithm_t kx_alg;
  size_t len = 20 * 1024 + strlen (header);
  char *crtinfo = NULL;
  size_t ncrtinfo = 0;

  if (verbose != 0)
    {
      http_buffer = malloc (len);
      if (http_buffer == NULL)
        return NULL;

      strcpy (http_buffer, HTTP_BEGIN);
      strcpy (&http_buffer[sizeof (HTTP_BEGIN) - 1], DEFAULT_DATA);
      strcpy (&http_buffer[sizeof (HTTP_BEGIN) + sizeof (DEFAULT_DATA) - 2],
              HTTP_END);
      *ret_length =
        sizeof (DEFAULT_DATA) + sizeof (HTTP_BEGIN) + sizeof (HTTP_END) - 3;
      return http_buffer;

    }

  if (gnutls_certificate_type_get (session) == GNUTLS_CRT_X509)
    {
      const gnutls_datum_t *cert_list;
      unsigned int cert_list_size = 0;

      cert_list = gnutls_certificate_get_peers (session, &cert_list_size);

      for (i = 0; i < cert_list_size; i++)
        {
          gnutls_x509_crt_t cert;
          gnutls_datum_t info;

          if (gnutls_x509_crt_init (&cert) == 0 &&
              gnutls_x509_crt_import (cert, &cert_list[i],
                                      GNUTLS_X509_FMT_DER) == 0 &&
              gnutls_x509_crt_print (cert, GNUTLS_CRT_PRINT_FULL, &info) == 0)
            {
              const char *post = "</PRE><P><PRE>";

              crtinfo = realloc (crtinfo, ncrtinfo + info.size +
                                 strlen (post) + 1);
              if (crtinfo == NULL)
                return NULL;
              memcpy (crtinfo + ncrtinfo, info.data, info.size);
              ncrtinfo += info.size;
              memcpy (crtinfo + ncrtinfo, post, strlen (post));
              ncrtinfo += strlen (post);
              crtinfo[ncrtinfo] = '\0';
              gnutls_free (info.data);
            }
        }
    }

  http_buffer = malloc (len);
  if (http_buffer == NULL)
    {
      free (crtinfo);
      return NULL;
    }

  strcpy (http_buffer, HTTP_BEGIN);

  /* print session_id */
  gnutls_session_get_id (session, sesid, &sesid_size);
  snprintf (tmp_buffer, tmp_buffer_size, "\n<p>Session ID: <i>");
  for (i = 0; i < sesid_size; i++)
    snprintf (tmp_buffer, tmp_buffer_size, "%.2X", sesid[i]);
  snprintf (tmp_buffer, tmp_buffer_size, "</i></p>\n");
  snprintf (tmp_buffer, tmp_buffer_size,
            "<h5>If your browser supports session resuming, then you should see the "
            "same session ID, when you press the <b>reload</b> button.</h5>\n");

  /* Here unlike print_info() we use the kx algorithm to distinguish
   * the functions to call.
   */
  {
    char dns[256];
    size_t dns_size = sizeof (dns);
    unsigned int type;

    if (gnutls_server_name_get (session, dns, &dns_size, &type, 0) == 0)
      {
        snprintf (tmp_buffer, tmp_buffer_size, "\n<p>Server Name: %s</p>\n", dns);
      }

  }

  kx_alg = gnutls_kx_get (session);

  /* print srp specific data */
#ifdef ENABLE_SRP
  if (kx_alg == GNUTLS_KX_SRP)
    {
      snprintf (tmp_buffer, tmp_buffer_size, "<p>Connected as user '%s'.</p>\n",
                gnutls_srp_server_get_username (session));
    }
#endif

#ifdef ENABLE_PSK
  if (kx_alg == GNUTLS_KX_PSK)
    {
      snprintf (tmp_buffer, tmp_buffer_size, "<p>Connected as user '%s'.</p>\n",
                gnutls_psk_server_get_username (session));
    }
#endif

#ifdef ENABLE_ANON
  if (kx_alg == GNUTLS_KX_ANON_DH)
    {
      snprintf (tmp_buffer, tmp_buffer_size,
                "<p> Connect using anonymous DH (prime of %d bits)</p>\n",
                gnutls_dh_get_prime_bits (session));
    }
#endif

  if (kx_alg == GNUTLS_KX_DHE_RSA || kx_alg == GNUTLS_KX_DHE_DSS)
    {
      snprintf (tmp_buffer, tmp_buffer_size,
                "Ephemeral DH using prime of <b>%d</b> bits.<br>\n",
                gnutls_dh_get_prime_bits (session));
    }

  /* print session information */
  strcat (http_buffer, "<P>\n");

  tmp = gnutls_protocol_get_name (gnutls_protocol_get_version (session));
  if (tmp == NULL)
    tmp = str_unknown;
  snprintf (tmp_buffer, tmp_buffer_size,
            "<TABLE border=1><TR><TD>Protocol version:</TD><TD>%s</TD></TR>\n",
            tmp);

  if (gnutls_auth_get_type (session) == GNUTLS_CRD_CERTIFICATE)
    {
      tmp =
        gnutls_certificate_type_get_name (gnutls_certificate_type_get
                                          (session));
      if (tmp == NULL)
        tmp = str_unknown;
      snprintf (tmp_buffer, tmp_buffer_size, "<TR><TD>Certificate Type:</TD><TD>%s</TD></TR>\n",
                tmp);
    }

  tmp = gnutls_kx_get_name (kx_alg);
  if (tmp == NULL)
    tmp = str_unknown;
  snprintf (tmp_buffer, tmp_buffer_size, "<TR><TD>Key Exchange:</TD><TD>%s</TD></TR>\n", tmp);

  tmp = gnutls_compression_get_name (gnutls_compression_get (session));
  if (tmp == NULL)
    tmp = str_unknown;
  snprintf (tmp_buffer, tmp_buffer_size, "<TR><TD>Compression</TD><TD>%s</TD></TR>\n", tmp);

  tmp = gnutls_cipher_get_name (gnutls_cipher_get (session));
  if (tmp == NULL)
    tmp = str_unknown;
  snprintf (tmp_buffer, tmp_buffer_size, "<TR><TD>Cipher</TD><TD>%s</TD></TR>\n", tmp);

  tmp = gnutls_mac_get_name (gnutls_mac_get (session));
  if (tmp == NULL)
    tmp = str_unknown;
  snprintf (tmp_buffer, tmp_buffer_size, "<TR><TD>MAC</TD><TD>%s</TD></TR>\n", tmp);

  tmp = gnutls_cipher_suite_get_name (kx_alg,
                                      gnutls_cipher_get (session),
                                      gnutls_mac_get (session));
  if (tmp == NULL)
    tmp = str_unknown;
  snprintf (tmp_buffer, tmp_buffer_size, "<TR><TD>Ciphersuite</TD><TD>%s</TD></TR></p></TABLE>\n",
            tmp);

  if (crtinfo)
    {
      snprintf (tmp_buffer, tmp_buffer_size, "<hr><PRE>%s\n</PRE>\n", crtinfo);
      free (crtinfo);
    }

  snprintf (tmp_buffer, tmp_buffer_size, "<hr><P>Your HTTP header was:<PRE>%s</PRE></P>\n" HTTP_END,
            header);

  *ret_length = strlen (http_buffer);

  return http_buffer;
}

static const char *
human_addr (const struct sockaddr *sa, socklen_t salen,
            char *buf, size_t buflen)
{
  const char *save_buf = buf;
  size_t l;

  if (!buf || !buflen)
    return NULL;

  *buf = '\0';

  switch (sa->sa_family)
    {
#if HAVE_IPV6
    case AF_INET6:
      snprintf (buf, buflen, "IPv6 ");
      break;
#endif

    case AF_INET:
      snprintf (buf, buflen, "IPv4 ");
      break;
    }

  l = strlen (buf);
  buf += l;
  buflen -= l;

  if (getnameinfo (sa, salen, buf, buflen, NULL, 0, NI_NUMERICHOST) != 0)
    return NULL;

  l = strlen (buf);
  buf += l;
  buflen -= l;

  strncat (buf, " port ", buflen);

  l = strlen (buf);
  buf += l;
  buflen -= l;

  if (getnameinfo (sa, salen, NULL, 0, buf, buflen, NI_NUMERICSERV) != 0)
    return NULL;

  return save_buf;
}

static int
listen_socket (const char *name, int listen_port)
{
  struct addrinfo hints, *res, *ptr;
  char portname[6];
  int s;
  int yes;
  listener_item *j = NULL;

  snprintf (portname, sizeof (portname), "%d", listen_port);
  memset (&hints, 0, sizeof (hints));
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;

  if ((s = getaddrinfo (NULL, portname, &hints, &res)) != 0)
    {
      fprintf (stderr, "getaddrinfo() failed: %s\n", gai_strerror (s));
      return -1;
    }

  for (ptr = res; ptr != NULL; ptr = ptr->ai_next)
    {
      /* Print what we are doing. */
      {
        char topbuf[512];

        fprintf (stderr, "%s listening on %s...",
                 name, human_addr (ptr->ai_addr, ptr->ai_addrlen,
                                   topbuf, sizeof (topbuf)));
      }

      if ((s = socket (ptr->ai_family, ptr->ai_socktype,
                       ptr->ai_protocol)) < 0)
        {
          perror ("socket() failed");
          continue;
        }

      yes = 1;
      if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR,
                      (const void *) &yes, sizeof (yes)) < 0)
        {
          perror ("setsockopt() failed");
        failed:
          close (s);
          continue;
        }

      if (bind (s, ptr->ai_addr, ptr->ai_addrlen) < 0)
        {
          perror ("bind() failed");
          goto failed;
        }

      if (listen (s, 10) < 0)
        {
          perror ("listen() failed");
          goto failed;
        }

      /* new list entry for the connection */
      lappend (listener_list);
      j = listener_list.tail;
      j->listen_socket = 1;
      j->fd = s;

      /* Complete earlier message. */
      fprintf (stderr, "done\n");
    }

  fflush (stderr);

  freeaddrinfo (res);
  if (!j)
    return -1;

  return 0;
}

/* strips \r\n from the end of the string 
 */
static void
strip (char *data)
{
  int i;
  int len = strlen (data);

  for (i = 0; i < len; i++)
    {
      if (data[i] == '\r' && data[i + 1] == '\n' && data[i + 1] == 0)
        {
          data[i] = '\n';
          data[i + 1] = 0;
          break;
        }
    }
}

static void
get_response (gnutls_session_t session, char *request,
              char **response, int *response_length)
{
  char *p, *h;

  if (http != 0)
    {
      if (strncmp (request, "GET ", 4))
        goto unimplemented;

      if (!(h = strchr (request, '\n')))
        goto unimplemented;

      *h++ = '\0';
      while (*h == '\r' || *h == '\n')
        h++;

      if (!(p = strchr (request + 4, ' ')))
        goto unimplemented;
      *p = '\0';
    }
/*    *response = peer_print_info(session, request+4, h, response_length); */
  if (http != 0)
    {
      *response = peer_print_info (session, response_length, h);
    }
  else
    {
      strip (request);
      fprintf (stderr, "received: %s\n", request);
      if (request[0] == request[1] && request[0] == '*')
        {
          if (strncmp
              (request, "**REHANDSHAKE**",
               sizeof ("**REHANDSHAKE**") - 1) == 0)
            {
              fprintf (stderr, "*** Sending rehandshake request\n");
              gnutls_rehandshake (session);
            }
          *response = NULL;
          *response_length = 0;
          return;
        }
      *response = strdup (request);
      *response_length = ((*response) ? strlen (*response) : 0);
    }

  return;

unimplemented:
  *response = strdup (HTTP_UNIMPLEMENTED);
  *response_length = ((*response) ? strlen (*response) : 0);
}

static void terminate (int sig) __attribute__ ((noreturn));

static void
terminate (int sig)
{
  fprintf (stderr, "Exiting via signal %d\n", sig);
  exit (1);
}


static void
check_alert (gnutls_session_t session, int ret)
{
  if (ret == GNUTLS_E_WARNING_ALERT_RECEIVED
      || ret == GNUTLS_E_FATAL_ALERT_RECEIVED)
    {
      int last_alert = gnutls_alert_get (session);
      if (last_alert == GNUTLS_A_NO_RENEGOTIATION &&
          ret == GNUTLS_E_WARNING_ALERT_RECEIVED)
        printf
          ("* Received NO_RENEGOTIATION alert. Client does not support renegotiation.\n");
      else
        printf ("* Received alert '%d': %s.\n", last_alert,
                gnutls_alert_get_name (last_alert));
    }
}

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}

static void gaa_parser (int argc, char **argv);

int
main (int argc, char **argv)
{
  int ret, n;
  char topbuf[512];
  char name[256];
  int accept_fd;
  struct sockaddr_storage client_address;
  socklen_t calen;

  set_program_name (argv[0]);

#ifndef _WIN32
  signal (SIGPIPE, SIG_IGN);
  signal (SIGHUP, SIG_IGN);
  signal (SIGTERM, terminate);
  if (signal (SIGINT, terminate) == SIG_IGN)
    signal (SIGINT, SIG_IGN);   /* e.g. background process */
#endif

  sockets_init ();

  gaa_parser (argc, argv);

  if (nodb == 0)
    wrap_db_init ();

  if (http == 1)
    {
      strcpy (name, "HTTP Server");
    }
  else
    {
      strcpy (name, "Echo Server");
    }

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
  gnutls_global_set_log_function (tls_log_func);
  gnutls_global_set_log_level (debug);

  /* Note that servers must generate parameters for
   * Diffie-Hellman. See gnutls_dh_params_generate(), and
   * gnutls_dh_params_set().
   */
  if (generate != 0)
    {
      generate_rsa_params ();
      generate_dh_primes ();
    }
  else if (dh_params_file)
    {
      read_dh_params ();
    }
  else
    {
      static_dh_params ();
    }

  if (gnutls_certificate_allocate_credentials (&cert_cred) < 0)
    {
      fprintf (stderr, "memory error\n");
      exit (1);
    }

  if (x509_cafile != NULL)
    {
      if ((ret = gnutls_certificate_set_x509_trust_file
           (cert_cred, x509_cafile, x509ctype)) < 0)
        {
          fprintf (stderr, "Error reading '%s'\n", x509_cafile);
          GERR (ret);
          exit (1);
        }
      else
        {
          printf ("Processed %d CA certificate(s).\n", ret);
        }
    }
#ifdef ENABLE_PKI
  if (x509_crlfile != NULL)
    {
      if ((ret = gnutls_certificate_set_x509_crl_file
           (cert_cred, x509_crlfile, x509ctype)) < 0)
        {
          fprintf (stderr, "Error reading '%s'\n", x509_crlfile);
          GERR (ret);
          exit (1);
        }
      else
        {
          printf ("Processed %d CRL(s).\n", ret);
        }
    }
#endif

#ifdef ENABLE_OPENPGP
  if (pgp_keyring != NULL)
    {
      ret =
        gnutls_certificate_set_openpgp_keyring_file (cert_cred, pgp_keyring,
                                                     GNUTLS_OPENPGP_FMT_BASE64);
      if (ret < 0)
        {
          fprintf (stderr, "Error setting the OpenPGP keyring file\n");
          GERR (ret);
        }
    }

  if (pgp_certfile != NULL)
    {
      if (info.pgp_subkey != NULL)
        ret = gnutls_certificate_set_openpgp_key_file2
          (cert_cred, pgp_certfile, pgp_keyfile, info.pgp_subkey,
           GNUTLS_OPENPGP_FMT_BASE64);
      else
        ret = gnutls_certificate_set_openpgp_key_file
          (cert_cred, pgp_certfile, pgp_keyfile, GNUTLS_OPENPGP_FMT_BASE64);

      if (ret < 0)
        {
          fprintf (stderr,
                   "Error[%d] while reading the OpenPGP key pair ('%s', '%s')\n",
                   ret, pgp_certfile, pgp_keyfile);
          GERR (ret);
        }
    }
#endif

  if (x509_certfile != NULL)
    if ((ret = gnutls_certificate_set_x509_key_file
         (cert_cred, x509_certfile, x509_keyfile, x509ctype)) < 0)
      {
        fprintf (stderr,
                 "Error reading '%s' or '%s'\n", x509_certfile, x509_keyfile);
        GERR (ret);
        exit (1);
      }

  if (x509_dsacertfile != NULL)
    if ((ret = gnutls_certificate_set_x509_key_file
         (cert_cred, x509_dsacertfile, x509_dsakeyfile, x509ctype)) < 0)
      {
        fprintf (stderr, "Error reading '%s' or '%s'\n",
                 x509_dsacertfile, x509_dsakeyfile);
        GERR (ret);
        exit (1);
      }

  gnutls_certificate_set_params_function (cert_cred, get_params);
/*     gnutls_certificate_set_dh_params(cert_cred, dh_params);
 *     gnutls_certificate_set_rsa_export_params(cert_cred, rsa_params);
 */

  /* this is a password file (created with the included srpcrypt utility) 
   * Read README.crypt prior to using SRP.
   */
#ifdef ENABLE_SRP
  if (srp_passwd != NULL)
    {
      gnutls_srp_allocate_server_credentials (&srp_cred);

      if ((ret =
           gnutls_srp_set_server_credentials_file (srp_cred, srp_passwd,
                                                   srp_passwd_conf)) < 0)
        {
          /* only exit is this function is not disabled 
           */
          fprintf (stderr, "Error while setting SRP parameters\n");
          GERR (ret);
        }
    }
#endif

  /* this is a password file 
   */
#ifdef ENABLE_PSK
  if (psk_passwd != NULL)
    {
      gnutls_psk_allocate_server_credentials (&psk_cred);

      if ((ret =
           gnutls_psk_set_server_credentials_file (psk_cred, psk_passwd)) < 0)
        {
          /* only exit is this function is not disabled 
           */
          fprintf (stderr, "Error while setting PSK parameters\n");
          GERR (ret);
        }

      if (info.psk_hint)
        {
          ret = gnutls_psk_set_server_credentials_hint (psk_cred,
                                                        info.psk_hint);
          if (ret)
            {
              fprintf (stderr, "Error setting PSK identity hint.\n");
              GERR (ret);
            }
        }

      gnutls_psk_set_server_params_function (psk_cred, get_params);
    }
#endif

#ifdef ENABLE_ANON
  gnutls_anon_allocate_server_credentials (&dh_cred);
  gnutls_anon_set_server_params_function (dh_cred, get_params);

/*      gnutls_anon_set_server_dh_params(dh_cred, dh_params); */
#endif

#ifdef ENABLE_SESSION_TICKET
  if (noticket == 0)
    gnutls_session_ticket_key_generate (&session_ticket_key);
#endif

  if (listen_socket (name, port) < 0)
    exit (1);

  for (;;)
    {
      listener_item *j;
      fd_set rd, wr;
#ifndef _WIN32
      int val;
#endif

      FD_ZERO (&rd);
      FD_ZERO (&wr);
      n = 0;

/* flag which connections we are reading or writing to within the fd sets */
      lloopstart (listener_list, j)
      {

#ifndef _WIN32
        val = fcntl (j->fd, F_GETFL, 0);
        if ((val == -1) || (fcntl (j->fd, F_SETFL, val | O_NONBLOCK) < 0))
          {
            perror ("fcntl()");
            exit (1);
          }
#endif

        if (j->listen_socket)
          {
            FD_SET (j->fd, &rd);
            n = MAX (n, j->fd);
          }
        if (j->http_state == HTTP_STATE_REQUEST)
          {
            FD_SET (j->fd, &rd);
            n = MAX (n, j->fd);
          }
        if (j->http_state == HTTP_STATE_RESPONSE)
          {
            FD_SET (j->fd, &wr);
            n = MAX (n, j->fd);
          }
      }
      lloopend (listener_list, j);

/* core operation */
      n = select (n + 1, &rd, &wr, NULL, NULL);
      if (n == -1 && errno == EINTR)
        continue;
      if (n < 0)
        {
          perror ("select()");
          exit (1);
        }

/* read or write to each connection as indicated by select()'s return argument */
      lloopstart (listener_list, j)
      {

        /* a new connection has arrived */
        if (FD_ISSET (j->fd, &rd) && j->listen_socket)
          {
            gnutls_session_t tls_session;

            tls_session = initialize_session ();

            calen = sizeof (client_address);
            memset (&client_address, 0, calen);
            accept_fd = accept (j->fd, (struct sockaddr *) &client_address,
                                &calen);

            if (accept_fd < 0)
              {
                perror ("accept()");
              }
            else
              {
                time_t tt;
                char *ctt;

                /* new list entry for the connection */
                lappend (listener_list);
                j = listener_list.tail;
                j->http_request = (char *) strdup ("");
                j->http_state = HTTP_STATE_REQUEST;
                j->fd = accept_fd;

                j->tls_session = tls_session;
                gnutls_transport_set_ptr (tls_session,
                                          (gnutls_transport_ptr_t)
                                          gl_fd_to_handle (accept_fd));
                j->handshake_ok = 0;

                if (verbose == 0)
                  {
                    tt = time (0);
                    ctt = ctime (&tt);
                    ctt[strlen (ctt) - 1] = 0;

                    printf ("\n* Accepted connection from %s on %s\n",
                            human_addr ((struct sockaddr *)
                                        &client_address, calen, topbuf,
                                        sizeof (topbuf)), ctt);
                  }
              }
          }

        if (FD_ISSET (j->fd, &rd) && !j->listen_socket)
          {
/* read partial GET request */
            char buf[1024];
            int r, ret;

            if (j->handshake_ok == 0)
              {
                r = gnutls_handshake (j->tls_session);
                if (r < 0 && gnutls_error_is_fatal (r) == 0)
                  {
                    check_alert (j->tls_session, r);
                    /* nothing */
                  }
                else if (r < 0 && gnutls_error_is_fatal (r) == 1)
                  {
                    check_alert (j->tls_session, r);
                    fprintf (stderr, "Error in handshake\n");
                    GERR (r);

                    do
                      {
                        ret =
                          gnutls_alert_send_appropriate (j->tls_session, r);
                      }
                    while (ret == GNUTLS_E_AGAIN
                           || ret == GNUTLS_E_INTERRUPTED);
                    j->http_state = HTTP_STATE_CLOSING;
                  }
                else if (r == 0)
                  {
                    if (gnutls_session_is_resumed (j->tls_session) != 0
                        && verbose == 0)
                      printf ("*** This is a resumed session\n");

                    if (verbose == 0)
                      {
                        printf ("\n* Successful handshake from %s\n",
                                human_addr ((struct sockaddr *)
                                            &client_address, calen, topbuf,
                                            sizeof (topbuf)));
                        print_info (j->tls_session, NULL, 1);
                      }
                    j->handshake_ok = 1;
                  }
              }

            if (j->handshake_ok == 1)
              {
                r = gnutls_record_recv (j->tls_session, buf,
                                        MIN (1024, SMALL_READ_TEST));
                if (r == GNUTLS_E_INTERRUPTED || r == GNUTLS_E_AGAIN)
                  {
                    /* do nothing */
                  }
                else if (r <= 0)
                  {
                    if (r == GNUTLS_E_REHANDSHAKE)
                      {
                        fprintf (stderr, "*** Received hello message\n");
                        do
                          {
                            r = gnutls_handshake (j->tls_session);
                          }
                        while (r == GNUTLS_E_INTERRUPTED
                               || r == GNUTLS_E_AGAIN);

                        if (r < 0)
                          {
                            do
                              {
                                ret = gnutls_alert_send_appropriate
                                  (j->tls_session, r);
                              }
                            while (ret == GNUTLS_E_AGAIN
                                   || ret == GNUTLS_E_INTERRUPTED);

                            GERR (r);
                            j->http_state = HTTP_STATE_CLOSING;
                          }
                      }
                    else
                      {
                        j->http_state = HTTP_STATE_CLOSING;
                        if (r < 0 && r != GNUTLS_E_UNEXPECTED_PACKET_LENGTH)
                          {
                            check_alert (j->tls_session, r);
                            fprintf (stderr, "Error while receiving data\n");
                            GERR (r);
                          }
                      }
                  }
                else
                  {
                    j->http_request =
                      realloc (j->http_request, j->request_length + r + 1);
                    if (j->http_request != NULL)
                      {
                        memcpy (j->http_request + j->request_length, buf, r);
                        j->request_length += r;
                        j->http_request[j->request_length] = '\0';
                      }
                    else
                      j->http_state = HTTP_STATE_CLOSING;

                  }
/* check if we have a full HTTP header */

                j->http_response = NULL;
                if (j->http_request != NULL)
                  {
                    if ((http == 0 && strchr (j->http_request, '\n'))
                        || strstr (j->http_request, "\r\n\r\n")
                        || strstr (j->http_request, "\n\n"))
                      {
                        get_response (j->tls_session, j->http_request,
                                      &j->http_response, &j->response_length);
                        j->http_state = HTTP_STATE_RESPONSE;
                        j->response_written = 0;
                      }
                  }
              }
          }
        if (FD_ISSET (j->fd, &wr))
          {
/* write partial response request */
            int r;

            if (j->handshake_ok == 0)
              {
                r = gnutls_handshake (j->tls_session);
                if (r < 0 && gnutls_error_is_fatal (r) == 0)
                  {
                    check_alert (j->tls_session, r);
                    /* nothing */
                  }
                else if (r < 0 && gnutls_error_is_fatal (r) == 1)
                  {
                    int ret;

                    j->http_state = HTTP_STATE_CLOSING;
                    check_alert (j->tls_session, r);
                    fprintf (stderr, "Error in handshake\n");
                    GERR (r);

                    do
                      {
                        ret =
                          gnutls_alert_send_appropriate (j->tls_session, r);
                      }
                    while (ret == GNUTLS_E_AGAIN);
                  }
                else if (r == 0)
                  {
                    if (gnutls_session_is_resumed (j->tls_session) != 0
                        && verbose == 0)
                      printf ("*** This is a resumed session\n");
                    if (verbose == 0)
                      {
                        printf ("- connection from %s\n",
                                human_addr ((struct sockaddr *)
                                            &client_address, calen, topbuf,
                                            sizeof (topbuf)));

                        print_info (j->tls_session, NULL, 1);
                      }
                    j->handshake_ok = 1;
                  }
              }

            if (j->handshake_ok == 1 && j->http_response != NULL)
              {
                /* FIXME if j->http_response == NULL? */
                r = gnutls_record_send (j->tls_session,
                                        j->http_response +
                                        j->response_written,
                                        MIN (j->response_length -
                                             j->response_written,
                                             SMALL_READ_TEST));
                if (r == GNUTLS_E_INTERRUPTED || r == GNUTLS_E_AGAIN)
                  {
                    /* do nothing */
                  }
                else if (r <= 0)
                  {
                    if (http != 0)
                      j->http_state = HTTP_STATE_CLOSING;
                    else
                      {
                        j->http_state = HTTP_STATE_REQUEST;
                        free (j->http_response);
                        j->response_length = 0;
                        j->request_length = 0;
                        j->http_request[0] = 0;
                      }

                    if (r < 0)
                      {
                        fprintf (stderr, "Error while sending data\n");
                        GERR (r);
                      }
                    check_alert (j->tls_session, r);
                  }
                else
                  {
                    j->response_written += r;
/* check if we have written a complete response */
                    if (j->response_written == j->response_length)
                      {
                        if (http != 0)
                          j->http_state = HTTP_STATE_CLOSING;
                        else
                          {
                            j->http_state = HTTP_STATE_REQUEST;
                            free (j->http_response);
                            j->response_length = 0;
                            j->request_length = 0;
                            j->http_request[0] = 0;
                          }
                      }
                  }
              }
            else
              {
                j->request_length = 0;
                j->http_request[0] = 0;
                j->http_state = HTTP_STATE_REQUEST;
              }
          }
      }
      lloopend (listener_list, j);

/* loop through all connections, closing those that are in error */
      lloopstart (listener_list, j)
      {
        if (j->http_state == HTTP_STATE_CLOSING)
          {
            ldeleteinc (listener_list, j);
          }
      }
      lloopend (listener_list, j);
    }


  gnutls_certificate_free_credentials (cert_cred);

#ifdef ENABLE_SRP
  if (srp_cred)
    gnutls_srp_free_server_credentials (srp_cred);
#endif

#ifdef ENABLE_PSK
  if (psk_cred)
    gnutls_psk_free_server_credentials (psk_cred);
#endif

#ifdef ENABLE_ANON
  gnutls_anon_free_server_credentials (dh_cred);
#endif

#ifdef ENABLE_SESSION_TICKET
  if (noticket == 0)
    gnutls_free (session_ticket_key.data);
#endif

  if (nodb == 0)
    wrap_db_deinit ();
  gnutls_global_deinit ();

  return 0;

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

  disable_client_cert = info.disable_client_cert;
  require_cert = info.require_cert;
  debug = info.debug;
  verbose = info.quiet;
  nodb = info.nodb;
  noticket = info.noticket;

  if (info.http == 0)
    http = 0;
  else
    http = 1;

  if (info.fmtder == 0)
    x509ctype = GNUTLS_X509_FMT_PEM;
  else
    x509ctype = GNUTLS_X509_FMT_DER;

  if (info.generate == 0)
    generate = 0;
  else
    generate = 1;

  dh_params_file = info.dh_params_file;

  port = info.port;

  x509_certfile = info.x509_certfile;
  x509_keyfile = info.x509_keyfile;
  x509_dsacertfile = info.x509_dsacertfile;
  x509_dsakeyfile = info.x509_dsakeyfile;
  x509_cafile = info.x509_cafile;
  x509_crlfile = info.x509_crlfile;
  pgp_certfile = info.pgp_certfile;
  pgp_keyfile = info.pgp_keyfile;
  srp_passwd = info.srp_passwd;
  srp_passwd_conf = info.srp_passwd_conf;

  psk_passwd = info.psk_passwd;

  pgp_keyring = info.pgp_keyring;
}

extern void serv_version (void);

void
serv_version (void)
{
  const char *p = PACKAGE_NAME;
  if (strcmp (gnutls_check_version (NULL), PACKAGE_VERSION) != 0)
    p = PACKAGE_STRING;
  version_etc (stdout, program_name, p, gnutls_check_version (NULL),
               "Nikos Mavrogiannopoulos", (char *) NULL);
}

/* session resuming support */

#define SESSION_ID_SIZE 32
#define SESSION_DATA_SIZE 1024

typedef struct
{
  char session_id[SESSION_ID_SIZE];
  unsigned int session_id_size;

  char session_data[SESSION_DATA_SIZE];
  unsigned int session_data_size;
} CACHE;

static CACHE *cache_db;
int cache_db_ptr = 0;

static void
wrap_db_init (void)
{
  /* allocate cache_db */
  cache_db = calloc (1, ssl_session_cache * sizeof (CACHE));
}

static void
wrap_db_deinit (void)
{
}

static int
wrap_db_store (void *dbf, gnutls_datum_t key, gnutls_datum_t data)
{

  if (cache_db == NULL)
    return -1;

  if (key.size > SESSION_ID_SIZE)
    return -1;
  if (data.size > SESSION_DATA_SIZE)
    return -1;

  memcpy (cache_db[cache_db_ptr].session_id, key.data, key.size);
  cache_db[cache_db_ptr].session_id_size = key.size;

  memcpy (cache_db[cache_db_ptr].session_data, data.data, data.size);
  cache_db[cache_db_ptr].session_data_size = data.size;

  cache_db_ptr++;
  cache_db_ptr %= ssl_session_cache;

  return 0;
}

static gnutls_datum_t
wrap_db_fetch (void *dbf, gnutls_datum_t key)
{
  gnutls_datum_t res = { NULL, 0 };
  int i;

  if (cache_db == NULL)
    return res;

  for (i = 0; i < ssl_session_cache; i++)
    {
      if (key.size == cache_db[i].session_id_size &&
          memcmp (key.data, cache_db[i].session_id, key.size) == 0)
        {
          res.size = cache_db[i].session_data_size;

          res.data = gnutls_malloc (res.size);
          if (res.data == NULL)
            return res;

          memcpy (res.data, cache_db[i].session_data, res.size);

          return res;
        }
    }
  return res;
}

static int
wrap_db_delete (void *dbf, gnutls_datum_t key)
{
  int i;

  if (cache_db == NULL)
    return -1;

  for (i = 0; i < ssl_session_cache; i++)
    {
      if (key.size == (unsigned int) cache_db[i].session_id_size &&
          memcmp (key.data, cache_db[i].session_id, key.size) == 0)
        {

          cache_db[i].session_id_size = 0;
          cache_db[i].session_data_size = 0;

          return 0;
        }
    }

  return -1;
}
