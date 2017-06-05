/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
 * 2009, 2010 Free Software Foundation, Inc.
 * Author: Nikos Mavrogiannopoulos
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

/* Work around problem reported in
   <http://permalink.gmane.org/gmane.comp.lib.gnulib.bugs/15755>.*/
#if GETTIMEOFDAY_CLOBBERS_LOCALTIME
#undef localtime
#endif

#include <getpass.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>
#include <gnutls/openpgp.h>
#include <time.h>
#include <common.h>

#define SU(x) (x!=NULL?x:"Unknown")

int print_cert;
extern int verbose;

const char str_unknown[] = "(unknown)";

/* Hex encodes the given data.
 */
const char *
raw_to_string (const unsigned char *raw, size_t raw_size)
{
  static char buf[1024];
  size_t i;
  if (raw_size == 0)
    return NULL;

  if (raw_size * 3 + 1 >= sizeof (buf))
    return NULL;

  for (i = 0; i < raw_size; i++)
    {
      sprintf (&(buf[i * 3]), "%02X%s", raw[i],
               (i == raw_size - 1) ? "" : ":");
    }
  buf[sizeof (buf) - 1] = '\0';

  return buf;
}

static void
print_x509_info (gnutls_session_t session, const char *hostname, int insecure)
{
  gnutls_x509_crt_t crt;
  const gnutls_datum_t *cert_list;
  unsigned int cert_list_size = 0, j;
  int hostname_ok = 0;
  int ret;

  cert_list = gnutls_certificate_get_peers (session, &cert_list_size);
  if (cert_list_size == 0)
    {
      fprintf (stderr, "No certificates found!\n");
      return;
    }

  printf (" - Got a certificate list of %d certificates.\n", cert_list_size);

  for (j = 0; j < cert_list_size; j++)
    {
      gnutls_datum_t cinfo;

      gnutls_x509_crt_init (&crt);
      ret = gnutls_x509_crt_import (crt, &cert_list[j], GNUTLS_X509_FMT_DER);
      if (ret < 0)
        {
          fprintf (stderr, "Decoding error: %s\n", gnutls_strerror (ret));
          return;
        }

      printf (" - Certificate[%d] info:\n  - ", j);

      if (verbose)
        ret = gnutls_x509_crt_print (crt, GNUTLS_CRT_PRINT_FULL, &cinfo);
      else
        ret = gnutls_x509_crt_print (crt, GNUTLS_CRT_PRINT_ONELINE, &cinfo);
      if (ret == 0)
        {
          printf ("%s\n", cinfo.data);
          gnutls_free (cinfo.data);
        }

      if (print_cert)
        {
          size_t size = 0;
          char *p = NULL;

          ret = gnutls_x509_crt_export (crt, GNUTLS_X509_FMT_PEM, p, &size);
          if (ret == GNUTLS_E_SHORT_MEMORY_BUFFER)
            {
              p = malloc (size);
              if (!p)
                {
                  fprintf (stderr, "gnutls_malloc\n");
                  exit (1);
                }

              ret = gnutls_x509_crt_export (crt, GNUTLS_X509_FMT_PEM,
                                            p, &size);
            }
          if (ret < 0)
            {
              fprintf (stderr, "Encoding error: %s\n", gnutls_strerror (ret));
              return;
            }

          fputs ("\n", stdout);
          fputs (p, stdout);
          fputs ("\n", stdout);

          gnutls_free (p);
        }

      if (j == 0 && hostname != NULL)
        {
          /* Check the hostname of the first certificate if it matches
           * the name of the host we connected to.
           */
          if (gnutls_x509_crt_check_hostname (crt, hostname) == 0)
            hostname_ok = 1;
          else
            hostname_ok = 2;
        }

      gnutls_x509_crt_deinit (crt);
    }

  if (hostname_ok == 1)
    {
      printf ("- The hostname in the certificate does NOT match '%s'\n",
              hostname);
      if (!insecure)
        exit (1);
    }
  else if (hostname_ok == 2)
    {
      printf ("- The hostname in the certificate matches '%s'.\n", hostname);
    }
}

#ifdef ENABLE_OPENPGP

static void
print_openpgp_info (gnutls_session_t session, const char *hostname,
                    int insecure)
{

  gnutls_openpgp_crt_t crt;
  const gnutls_datum_t *cert_list;
  int cert_list_size = 0;
  int hostname_ok = 0;
  int ret;

  cert_list = gnutls_certificate_get_peers (session, &cert_list_size);

  if (cert_list_size > 0)
    {
      gnutls_datum_t cinfo;

      gnutls_openpgp_crt_init (&crt);
      ret = gnutls_openpgp_crt_import (crt, &cert_list[0],
                                       GNUTLS_OPENPGP_FMT_RAW);
      if (ret < 0)
        {
          fprintf (stderr, "Decoding error: %s\n", gnutls_strerror (ret));
          return;
        }

      if (verbose)
        ret = gnutls_openpgp_crt_print (crt, GNUTLS_CRT_PRINT_FULL, &cinfo);
      else
        ret =
          gnutls_openpgp_crt_print (crt, GNUTLS_CRT_PRINT_ONELINE, &cinfo);
      if (ret == 0)
        {
          printf (" - %s\n", cinfo.data);
          gnutls_free (cinfo.data);
        }

      if (print_cert)
        {
          size_t size = 0;
          char *p = NULL;

          ret = gnutls_openpgp_crt_export (crt, GNUTLS_OPENPGP_FMT_BASE64,
                                           p, &size);
          if (ret == GNUTLS_E_SHORT_MEMORY_BUFFER)
            {
              p = malloc (size);
              if (!p)
                {
                  fprintf (stderr, "gnutls_malloc\n");
                  exit (1);
                }

              ret = gnutls_openpgp_crt_export (crt, GNUTLS_OPENPGP_FMT_BASE64,
                                               p, &size);
            }
          if (ret < 0)
            {
              fprintf (stderr, "Encoding error: %s\n", gnutls_strerror (ret));
              return;
            }

          fputs (p, stdout);
          fputs ("\n", stdout);

          gnutls_free (p);
        }

      if (hostname != NULL)
        {
          /* Check the hostname of the first certificate if it matches
           * the name of the host we connected to.
           */
          if (gnutls_openpgp_crt_check_hostname (crt, hostname) == 0)
            hostname_ok = 1;
          else
            hostname_ok = 2;
        }

      gnutls_openpgp_crt_deinit (crt);
    }

  if (hostname_ok == 1)
    {
      printf ("- The hostname in the certificate does NOT match '%s'\n",
              hostname);
      if (!insecure)
        exit (1);
    }
  else if (hostname_ok == 2)
    {
      printf ("- The hostname in the certificate matches '%s'.\n", hostname);
    }
}

#endif

static void
print_cert_vrfy (gnutls_session_t session)
{
  int rc;
  unsigned int status;

  rc = gnutls_certificate_verify_peers2 (session, &status);
  if (rc < 0)
    {
      printf ("- Could not verify certificate (err: %s)\n",
              gnutls_strerror (rc));
      return;
    }

  if (rc == GNUTLS_E_NO_CERTIFICATE_FOUND)
    {
      printf ("- Peer did not send any certificate.\n");
      return;
    }

  if (gnutls_certificate_type_get (session) == GNUTLS_CRT_X509)
    {
      if (status & GNUTLS_CERT_REVOKED)
        printf ("- Peer's certificate chain revoked\n");
      if (status & GNUTLS_CERT_SIGNER_NOT_FOUND)
        printf ("- Peer's certificate issuer is unknown\n");
      if (status & GNUTLS_CERT_SIGNER_NOT_CA)
        printf ("- Peer's certificate issuer is not a CA\n");
      if (status & GNUTLS_CERT_INSECURE_ALGORITHM)
        printf ("- Peer's certificate chain uses insecure algorithm\n");
      if (status & GNUTLS_CERT_NOT_ACTIVATED)
        printf
          ("- Peer's certificate chain uses not yet valid certificate\n");
      if (status & GNUTLS_CERT_EXPIRED)
        printf ("- Peer's certificate chain uses expired certificate\n");
      if (status & GNUTLS_CERT_INVALID)
        printf ("- Peer's certificate is NOT trusted\n");
      else
        printf ("- Peer's certificate is trusted\n");
    }
  else
    {
      if (status & GNUTLS_CERT_INVALID)
        printf ("- Peer's key is invalid\n");
      else
        printf ("- Peer's key is valid\n");
      if (status & GNUTLS_CERT_SIGNER_NOT_FOUND)
        printf ("- Could not find a signer of the peer's key\n");
    }
}

static void
print_dh_info (gnutls_session_t session, const char *str)
{
  printf ("- %sDiffie-Hellman parameters\n", str);
  printf (" - Using prime: %d bits\n", gnutls_dh_get_prime_bits (session));
  printf (" - Secret key: %d bits\n", gnutls_dh_get_secret_bits (session));
  printf (" - Peer's public key: %d bits\n",
          gnutls_dh_get_peers_public_bits (session));

  if (print_cert)
    {
      int ret;
      gnutls_datum_t raw_gen = { NULL, 0 };
      gnutls_datum_t raw_prime = { NULL, 0 };
      gnutls_dh_params_t dh_params = NULL;
      unsigned char *params_data = NULL;
      size_t params_data_size = 0;

      ret = gnutls_dh_get_group (session, &raw_gen, &raw_prime);
      if (ret)
        {
          fprintf (stderr, "gnutls_dh_get_group %d\n", ret);
          goto out;
        }

      ret = gnutls_dh_params_init (&dh_params);
      if (ret)
        {
          fprintf (stderr, "gnutls_dh_params_init %d\n", ret);
          goto out;
        }

      ret = gnutls_dh_params_import_raw (dh_params, &raw_prime, &raw_gen);
      if (ret)
        {
          fprintf (stderr, "gnutls_dh_params_import_raw %d\n", ret);
          goto out;
        }

      ret = gnutls_dh_params_export_pkcs3 (dh_params,
                                           GNUTLS_X509_FMT_PEM,
                                           params_data, &params_data_size);
      if (ret != GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          fprintf (stderr, "gnutls_dh_params_export_pkcs3 %d\n", ret);
          goto out;
        }

      params_data = gnutls_malloc (params_data_size);
      if (!params_data)
        {
          fprintf (stderr, "gnutls_malloc %d\n", ret);
          goto out;
        }

      ret = gnutls_dh_params_export_pkcs3 (dh_params,
                                           GNUTLS_X509_FMT_PEM,
                                           params_data, &params_data_size);
      if (ret)
        {
          fprintf (stderr, "gnutls_dh_params_export_pkcs3-2 %d\n", ret);
          goto out;
        }

      printf (" - PKCS#3 format:\n\n%.*s\n", (int) params_data_size,
              params_data);

    out:
      gnutls_free (params_data);
      gnutls_free (raw_prime.data);
      gnutls_free (raw_gen.data);
      gnutls_dh_params_deinit (dh_params);
    }
}

int
print_info (gnutls_session_t session, const char *hostname, int insecure)
{
  const char *tmp;
  gnutls_credentials_type_t cred;
  gnutls_kx_algorithm_t kx;


  /* print the key exchange's algorithm name
   */
  kx = gnutls_kx_get (session);

  cred = gnutls_auth_get_type (session);
  switch (cred)
    {
#ifdef ENABLE_ANON
    case GNUTLS_CRD_ANON:
      print_dh_info (session, "Anonymous ");
      break;
#endif
#ifdef ENABLE_SRP
    case GNUTLS_CRD_SRP:
      /* This should be only called in server
       * side.
       */
      if (gnutls_srp_server_get_username (session) != NULL)
        printf ("- SRP authentication. Connected as '%s'\n",
                gnutls_srp_server_get_username (session));
      break;
#endif
#ifdef ENABLE_PSK
    case GNUTLS_CRD_PSK:
      /* This returns NULL in server side.
       */
      if (gnutls_psk_client_get_hint (session) != NULL)
        printf ("- PSK authentication. PSK hint '%s'\n",
                gnutls_psk_client_get_hint (session));
      /* This returns NULL in client side.
       */
      if (gnutls_psk_server_get_username (session) != NULL)
        printf ("- PSK authentication. Connected as '%s'\n",
                gnutls_psk_server_get_username (session));
      if (kx == GNUTLS_KX_DHE_PSK)
        print_dh_info (session, "Ephemeral ");
      break;
#endif
    case GNUTLS_CRD_IA:
      printf ("- TLS/IA authentication\n");
      break;
    case GNUTLS_CRD_CERTIFICATE:
      {
        char dns[256];
        size_t dns_size = sizeof (dns);
        unsigned int type;

        /* This fails in client side */
        if (gnutls_server_name_get (session, dns, &dns_size, &type, 0) == 0)
          {
            printf ("- Given server name[%d]: %s\n", type, dns);
          }
      }

      if (kx == GNUTLS_KX_DHE_RSA || kx == GNUTLS_KX_DHE_DSS)
        print_dh_info (session, "Ephemeral ");

      print_cert_info (session, hostname, insecure);

      print_cert_vrfy (session);

    }

  tmp = SU (gnutls_protocol_get_name (gnutls_protocol_get_version (session)));
  printf ("- Version: %s\n", tmp);

  tmp = SU (gnutls_kx_get_name (kx));
  printf ("- Key Exchange: %s\n", tmp);

  tmp = SU (gnutls_cipher_get_name (gnutls_cipher_get (session)));
  printf ("- Cipher: %s\n", tmp);

  tmp = SU (gnutls_mac_get_name (gnutls_mac_get (session)));
  printf ("- MAC: %s\n", tmp);

  tmp = SU (gnutls_compression_get_name (gnutls_compression_get (session)));
  printf ("- Compression: %s\n", tmp);

  if (verbose)
    {
      char id[32];
      size_t id_size = sizeof (id);
      gnutls_session_get_id (session, id, &id_size);
      printf ("- Session ID: %s\n", raw_to_string (id, id_size));
    }

  if (verbose)
    {
      gnutls_datum cb;
      int rc;

      rc =
        gnutls_session_channel_binding (session, GNUTLS_CB_TLS_UNIQUE, &cb);
      if (rc)
        fprintf (stderr, "Channel binding error: %s\n", gnutls_strerror (rc));
      else
        {
          size_t i;

          printf ("- Channel binding 'tls-unique': ");
          for (i = 0; i < cb.size; i++)
            printf ("%02x", cb.data[i]);
          printf ("\n");
        }
    }

  /* Warning: Do not print anything more here. The 'Compression:'
     output MUST be the last non-verbose output.  This is used by
     Emacs starttls.el code. */

  fflush (stdout);

  return 0;
}

void
print_cert_info (gnutls_session_t session, const char *hostname, int insecure)
{

  if (gnutls_certificate_client_get_request_status (session) != 0)
    printf ("- Server has requested a certificate.\n");

  printf ("- Certificate type: ");
  switch (gnutls_certificate_type_get (session))
    {
    case GNUTLS_CRT_UNKNOWN:
      printf ("Unknown\n");

      if (!insecure)
        exit (1);
      break;
    case GNUTLS_CRT_X509:
      printf ("X.509\n");
      print_x509_info (session, hostname, insecure);
      break;
#ifdef ENABLE_OPENPGP
    case GNUTLS_CRT_OPENPGP:
      printf ("OpenPGP\n");
      print_openpgp_info (session, hostname, insecure);
      break;
#endif
    }
}

void
print_list (int verbose)
{
  {
    size_t i;
    const char *name;
    char id[2];
    gnutls_kx_algorithm_t kx;
    gnutls_cipher_algorithm_t cipher;
    gnutls_mac_algorithm_t mac;
    gnutls_protocol_t version;

    printf ("Cipher suites:\n");
    for (i = 0; (name = gnutls_cipher_suite_info
                 (i, id, &kx, &cipher, &mac, &version)); i++)
      {
        printf ("%-50s\t0x%02x, 0x%02x\t%s\n",
                name,
                (unsigned char) id[0], (unsigned char) id[1],
                gnutls_protocol_get_name (version));
        if (verbose)
          printf ("\tKey exchange: %s\n\tCipher: %s\n\tMAC: %s\n\n",
                  gnutls_kx_get_name (kx),
                  gnutls_cipher_get_name (cipher), gnutls_mac_get_name (mac));
      }
  }

  {
    const gnutls_certificate_type_t *p = gnutls_certificate_type_list ();

    printf ("Certificate types: ");
    for (; *p; p++)
      {
        printf ("CTYPE-%s", gnutls_certificate_type_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }

  {
    const gnutls_protocol_t *p = gnutls_protocol_list ();

    printf ("Protocols: ");
    for (; *p; p++)
      {
        printf ("VERS-%s", gnutls_protocol_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }

  {
    const gnutls_cipher_algorithm_t *p = gnutls_cipher_list ();

    printf ("Ciphers: ");
    for (; *p; p++)
      {
        printf ("%s", gnutls_cipher_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }

  {
    const gnutls_mac_algorithm_t *p = gnutls_mac_list ();

    printf ("MACs: ");
    for (; *p; p++)
      {
        printf ("%s", gnutls_mac_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }

  {
    const gnutls_kx_algorithm_t *p = gnutls_kx_list ();

    printf ("Key exchange algorithms: ");
    for (; *p; p++)
      {
        printf ("%s", gnutls_kx_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }

  {
    const gnutls_compression_method_t *p = gnutls_compression_list ();

    printf ("Compression: ");
    for (; *p; p++)
      {
        printf ("COMP-%s", gnutls_compression_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }

  {
    const gnutls_pk_algorithm_t *p = gnutls_pk_list ();

    printf ("Public Key Systems: ");
    for (; *p; p++)
      {
        printf ("%s", gnutls_pk_algorithm_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }

  {
    const gnutls_sign_algorithm_t *p = gnutls_sign_list ();

    printf ("PK-signatures: ");
    for (; *p; p++)
      {
        printf ("SIGN-%s", gnutls_sign_algorithm_get_name (*p));
        if (*(p + 1))
          printf (", ");
        else
          printf ("\n");
      }
  }
}

void
sockets_init (void)
{
#ifdef _WIN32
  WORD wVersionRequested;
  WSADATA wsaData;

  wVersionRequested = MAKEWORD (1, 1);
  if (WSAStartup (wVersionRequested, &wsaData) != 0)
    {
      perror ("WSA_STARTUP_ERROR");
    }
#endif
}

/* converts a service name or a port (in string) to a
 * port number. The protocol is assumed to be TCP.
 *
 * returns -1 on error;
 */
int
service_to_port (const char *service)
{
  int port;
  struct servent *server_port;

  port = atoi (service);
  if (port != 0)
    return port;

  server_port = getservbyname (service, "tcp");
  if (server_port == NULL)
    {
      perror ("getservbyname()");
      return (-1);
    }

  return ntohs (server_port->s_port);
}

