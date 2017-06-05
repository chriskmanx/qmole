/* This example code is placed in the public domain. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <getpass.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>
#include <gnutls/pkcs11.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* A TLS client that loads the certificate and key.
 */

#define MAX_BUF 1024
#define MSG "GET / HTTP/1.0\r\n\r\n"
#define MIN(x,y) (((x)<(y))?(x):(y))

#define CAFILE "ca.pem"
#define KEY_URL "pkcs11:manufacturer=SomeManufacturer;object=Private%20Key" \
  ";objecttype=private;id=db:5b:3e:b5:72:33"
#define CERT_URL "pkcs11:manufacturer=SomeManufacturer;object=Certificate;" \
  "objecttype=cert;id=db:5b:3e:b5:72:33"

extern int tcp_connect (void);
extern void tcp_close (int sd);

static int cert_callback (gnutls_session_t session,
                          const gnutls_datum_t * req_ca_rdn, int nreqs,
                          const gnutls_pk_algorithm_t * sign_algos,
                          int sign_algos_length, gnutls_retr2_st * st);

gnutls_x509_crt_t crt;
gnutls_pkcs11_privkey_t key;

/* Load the certificate and the private key.
 */
static void
load_keys (void)
{
  int ret;

  gnutls_x509_crt_init (&crt);

  ret = gnutls_x509_crt_import_pkcs11_url (crt, CERT_URL, 0);

  /* some tokens require login to read data */
  if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
    ret = gnutls_x509_crt_import_pkcs11_url (crt, CERT_URL,
                                             GNUTLS_PKCS11_OBJ_FLAG_LOGIN);

  if (ret < 0)
    {
      fprintf (stderr, "*** Error loading key file: %s\n",
               gnutls_strerror (ret));
      exit (1);
    }

  gnutls_pkcs11_privkey_init (&key);

  ret = gnutls_pkcs11_privkey_import_url (key, KEY_URL, 0);
  if (ret < 0)
    {
      fprintf (stderr, "*** Error loading key file: %s\n",
               gnutls_strerror (ret));
      exit (1);
    }

}

static int
pin_callback (void *user, int attempt, const char *token_url,
              const char *token_label, unsigned int flags, char *pin,
              size_t pin_max)
{
  const char *password;
  int len;

  printf ("PIN required for token '%s' with URL '%s'\n", token_label,
          token_url);
  if (flags & GNUTLS_PKCS11_PIN_FINAL_TRY)
    printf ("*** This is the final try before locking!\n");
  if (flags & GNUTLS_PKCS11_PIN_COUNT_LOW)
    printf ("*** Only few tries left before locking!\n");

  password = getpass ("Enter pin: ");
  if (password == NULL || password[0] == 0)
    {
      fprintf (stderr, "No password given\n");
      exit (1);
    }

  len = MIN (pin_max, strlen (password));
  memcpy (pin, password, len);
  pin[len] = 0;

  return 0;
}

int
main (void)
{
  int ret, sd, ii;
  gnutls_session_t session;
  gnutls_priority_t priorities_cache;
  char buffer[MAX_BUF + 1];
  gnutls_certificate_credentials_t xcred;
  /* Allow connections to servers that have OpenPGP keys as well.
   */

  gnutls_global_init ();
  /* PKCS11 private key operations might require PIN.
   * Register a callback.
   */
  gnutls_pkcs11_set_pin_function (pin_callback, NULL);

  load_keys ();

  /* X509 stuff */
  gnutls_certificate_allocate_credentials (&xcred);

  /* priorities */
  gnutls_priority_init (&priorities_cache, "NORMAL", NULL);


  /* sets the trusted cas file
   */
  gnutls_certificate_set_x509_trust_file (xcred, CAFILE, GNUTLS_X509_FMT_PEM);

  gnutls_certificate_set_retrieve_function (xcred, cert_callback);

  /* Initialize TLS session
   */
  gnutls_init (&session, GNUTLS_CLIENT);

  /* Use default priorities */
  gnutls_priority_set (session, priorities_cache);

  /* put the x509 credentials to the current session
   */
  gnutls_credentials_set (session, GNUTLS_CRD_CERTIFICATE, xcred);

  /* connect to the peer
   */
  sd = tcp_connect ();

  gnutls_transport_set_ptr (session, (gnutls_transport_ptr_t) sd);

  /* Perform the TLS handshake
   */
  ret = gnutls_handshake (session);

  if (ret < 0)
    {
      fprintf (stderr, "*** Handshake failed\n");
      gnutls_perror (ret);
      goto end;
    }
  else
    {
      printf ("- Handshake was completed\n");
    }

  gnutls_record_send (session, MSG, strlen (MSG));

  ret = gnutls_record_recv (session, buffer, MAX_BUF);
  if (ret == 0)
    {
      printf ("- Peer has closed the TLS connection\n");
      goto end;
    }
  else if (ret < 0)
    {
      fprintf (stderr, "*** Error: %s\n", gnutls_strerror (ret));
      goto end;
    }

  printf ("- Received %d bytes: ", ret);
  for (ii = 0; ii < ret; ii++)
    {
      fputc (buffer[ii], stdout);
    }
  fputs ("\n", stdout);

  gnutls_bye (session, GNUTLS_SHUT_RDWR);

end:

  tcp_close (sd);

  gnutls_deinit (session);

  gnutls_certificate_free_credentials (xcred);
  gnutls_priority_deinit (priorities_cache);

  gnutls_global_deinit ();

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
  gnutls_certificate_type_t type;

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

  /* Select a certificate and return it.
   * The certificate must be of any of the "sign algorithms"
   * supported by the server.
   */

  type = gnutls_certificate_type_get (session);
  if (type == GNUTLS_CRT_X509)
    {
      /* check if the certificate we are sending is signed
       * with an algorithm that the server accepts */
      gnutls_sign_algorithm_t cert_algo, req_algo;
      int i, match = 0;

      ret = gnutls_x509_crt_get_signature_algorithm (crt);
      if (ret < 0)
        {
          /* error reading signature algorithm
           */
          return -1;
        }
      cert_algo = ret;

      i = 0;
      do
        {
          ret = gnutls_sign_algorithm_get_requested (session, i, &req_algo);
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

      st->cert_type = type;
      st->ncerts = 1;

      st->cert.x509 = &crt;
      st->key.pkcs11 = key;
      st->key_type = GNUTLS_PRIVKEY_PKCS11;

      st->deinit_all = 0;
    }
  else
    {
      return -1;
    }

  return 0;

}
