/* This example code is placed in the public domain. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>

#include "examples.h"

/* All the available CRLs
 */
gnutls_x509_crl_t *crl_list;
int crl_list_size;

/* All the available trusted CAs
 */
gnutls_x509_crt_t *ca_list;
int ca_list_size;

static void verify_cert2 (gnutls_x509_crt_t crt,
                          gnutls_x509_crt_t issuer,
                          gnutls_x509_crl_t * crl_list, int crl_list_size);
static void verify_last_cert (gnutls_x509_crt_t crt,
                              gnutls_x509_crt_t * ca_list, int ca_list_size,
                              gnutls_x509_crl_t * crl_list,
                              int crl_list_size);


/* This function will try to verify the peer's certificate chain, and
 * also check if the hostname matches, and the activation, expiration dates.
 */
void
verify_certificate_chain (gnutls_session_t session,
                          const char *hostname,
                          const gnutls_datum_t * cert_chain,
                          int cert_chain_length)
{
  int i;
  gnutls_x509_crt_t *cert;

  cert = malloc (sizeof (*cert) * cert_chain_length);

  /* Import all the certificates in the chain to
   * native certificate format.
   */
  for (i = 0; i < cert_chain_length; i++)
    {
      gnutls_x509_crt_init (&cert[i]);
      gnutls_x509_crt_import (cert[i], &cert_chain[i], GNUTLS_X509_FMT_DER);
    }

  /* If the last certificate in the chain is self signed ignore it.
   * That is because we want to check against our trusted certificate
   * list.
   */
  if (gnutls_x509_crt_check_issuer (cert[cert_chain_length - 1],
                                    cert[cert_chain_length - 1]) > 0
      && cert_chain_length > 0)
    {
      cert_chain_length--;
    }

  /* Now verify the certificates against their issuers
   * in the chain.
   */
  for (i = 1; i < cert_chain_length; i++)
    {
      verify_cert2 (cert[i - 1], cert[i], crl_list, crl_list_size);
    }

  /* Here we must verify the last certificate in the chain against
   * our trusted CA list.
   */
  verify_last_cert (cert[cert_chain_length - 1],
                    ca_list, ca_list_size, crl_list, crl_list_size);

  /* Check if the name in the first certificate matches our destination!
   */
  if (!gnutls_x509_crt_check_hostname (cert[0], hostname))
    {
      printf ("The certificate's owner does not match hostname '%s'\n",
              hostname);
    }

  for (i = 0; i < cert_chain_length; i++)
    gnutls_x509_crt_deinit (cert[i]);

  return;
}


/* Verifies a certificate against an other certificate
 * which is supposed to be it's issuer. Also checks the
 * crl_list if the certificate is revoked.
 */
static void
verify_cert2 (gnutls_x509_crt_t crt, gnutls_x509_crt_t issuer,
              gnutls_x509_crl_t * crl_list, int crl_list_size)
{
  unsigned int output;
  int ret;
  size_t name_size;
  char name[64];

  /* Print information about the certificates to
   * be checked.
   */
  name_size = sizeof (name);
  gnutls_x509_crt_get_dn (crt, name, &name_size);

  fprintf (stderr, "\nCertificate: %s\n", name);

  name_size = sizeof (name);
  gnutls_x509_crt_get_issuer_dn (crt, name, &name_size);

  fprintf (stderr, "Issued by: %s\n", name);

  /* Get the DN of the issuer cert.
   */
  name_size = sizeof (name);
  gnutls_x509_crt_get_dn (issuer, name, &name_size);

  fprintf (stderr, "Checking against: %s\n", name);

  /* Do the actual verification.
   */
  gnutls_x509_crt_verify (crt, &issuer, 1, 0, &output);

  if (output & GNUTLS_CERT_INVALID)
    {
      fprintf (stderr, "Not trusted");

      if (output & GNUTLS_CERT_SIGNER_NOT_FOUND)
        fprintf (stderr, ": no issuer was found");
      if (output & GNUTLS_CERT_SIGNER_NOT_CA)
        fprintf (stderr, ": issuer is not a CA");
      if (output & GNUTLS_CERT_NOT_ACTIVATED)
        fprintf (stderr, ": not yet activated\n");
      if (output & GNUTLS_CERT_EXPIRED)
        fprintf (stderr, ": expired\n");

      fprintf (stderr, "\n");
    }
  else
    fprintf (stderr, "Trusted\n");

  /* Check if the certificate is revoked.
   */
  ret = gnutls_x509_crt_check_revocation (crt, crl_list, crl_list_size);
  if (ret == 1)
    {                           /* revoked */
      fprintf (stderr, "Revoked\n");
    }
}


/* Verifies a certificate against our trusted CA list.
 * Also checks the crl_list if the certificate is revoked.
 */
static void
verify_last_cert (gnutls_x509_crt_t crt,
                  gnutls_x509_crt_t * ca_list, int ca_list_size,
                  gnutls_x509_crl_t * crl_list, int crl_list_size)
{
  unsigned int output;
  int ret;
  size_t name_size;
  char name[64];

  /* Print information about the certificates to
   * be checked.
   */
  name_size = sizeof (name);
  gnutls_x509_crt_get_dn (crt, name, &name_size);

  fprintf (stderr, "\nCertificate: %s\n", name);

  name_size = sizeof (name);
  gnutls_x509_crt_get_issuer_dn (crt, name, &name_size);

  fprintf (stderr, "Issued by: %s\n", name);

  /* Do the actual verification.
   */
  gnutls_x509_crt_verify (crt, ca_list, ca_list_size,
                          GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT, &output);

  if (output & GNUTLS_CERT_INVALID)
    {
      fprintf (stderr, "Not trusted");

      if (output & GNUTLS_CERT_SIGNER_NOT_CA)
        fprintf (stderr, ": Issuer is not a CA\n");
      if (output & GNUTLS_CERT_NOT_ACTIVATED)
        fprintf (stderr, ": Not yet activated\n");
      if (output & GNUTLS_CERT_EXPIRED)
        fprintf (stderr, ": Expired\n");
      fprintf (stderr, "\n");
    }
  else
    fprintf (stderr, "Trusted\n");


  /* Check if the certificate is revoked.
   */
  ret = gnutls_x509_crt_check_revocation (crt, crl_list, crl_list_size);
  if (ret == 1)
    {                           /* revoked */
      fprintf (stderr, "Revoked\n");
    }
}
