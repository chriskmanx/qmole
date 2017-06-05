/*
 * Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
 *
 * Author: Simon Josefsson
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 *
 */

/* Functions for printing X.509 Certificate structures
 */

#include <gnutls_int.h>
#include <common.h>
#include <gnutls_x509.h>
#include <x509_int.h>
#include <gnutls_num.h>
#include <gnutls_errors.h>
#include <c-ctype.h>

/* I18n of error codes. */
#include "gettext.h"
#define _(String) dgettext (PACKAGE, String)

#define addf _gnutls_buffer_append_printf
#define adds _gnutls_buffer_append_str

#define ERROR_STR (char*) "(error)"

static void
hexdump (gnutls_buffer_st * str, const char *data, size_t len,
         const char *spc)
{
  size_t j;

  if (spc)
    adds (str, spc);
  for (j = 0; j < len; j++)
    {
      if (((j + 1) % 16) == 0)
        {
          addf (str, "%.2x\n", (unsigned char) data[j]);
          if (spc && j != (len - 1))
            adds (str, spc);
        }
      else if (j == (len - 1))
        addf (str, "%.2x", (unsigned char) data[j]);
      else
        addf (str, "%.2x:", (unsigned char) data[j]);
    }
  if ((j % 16) != 0)
    adds (str, "\n");
}

static void
hexprint (gnutls_buffer_st * str, const char *data, size_t len)
{
  size_t j;

  if (len == 0)
    adds (str, "00");
  else
    {
      for (j = 0; j < len; j++)
        addf (str, "%.2x", (unsigned char) data[j]);
    }
}


static void
asciiprint (gnutls_buffer_st * str, const char *data, size_t len)
{
  size_t j;

  for (j = 0; j < len; j++)
    if (c_isprint (data[j]))
      addf (str, "%c", (unsigned char) data[j]);
    else
      addf (str, ".");
}

static char *
ip_to_string (void *_ip, int ip_size, char *string, int string_size)
{
  uint8_t *ip;

  if (ip_size != 4 && ip_size != 16)
    {
      gnutls_assert ();
      return NULL;
    }

  if (ip_size == 4 && string_size < 16)
    {
      gnutls_assert ();
      return NULL;
    }

  if (ip_size == 16 && string_size < 48)
    {
      gnutls_assert ();
      return NULL;
    }

  ip = _ip;
  switch (ip_size)
    {
    case 4:
      snprintf (string, string_size, "%u.%u.%u.%u", ip[0], ip[1], ip[2], ip[3]);
      break;
    case 16:
      snprintf (string, string_size, "%x:%x:%x:%x:%x:%x:%x:%x",
               (ip[0] << 8) | ip[1], (ip[2] << 8) | ip[3],
               (ip[4] << 8) | ip[5], (ip[6] << 8) | ip[7],
               (ip[8] << 8) | ip[9], (ip[10] << 8) | ip[11],
               (ip[12] << 8) | ip[13], (ip[14] << 8) | ip[15]);
      break;
    }

  return string;
}

static void
print_proxy (gnutls_buffer_st * str, gnutls_x509_crt_t cert)
{
  int pathlen;
  char *policyLanguage;
  char *policy;
  size_t npolicy;
  int err;

  err = gnutls_x509_crt_get_proxy (cert, NULL,
                                   &pathlen, &policyLanguage,
                                   &policy, &npolicy);
  if (err < 0)
    {
      addf (str, "error: get_proxy: %s\n", gnutls_strerror (err));
      return;
    }

  if (pathlen >= 0)
    addf (str, _("\t\t\tPath Length Constraint: %d\n"), pathlen);
  addf (str, _("\t\t\tPolicy Language: %s"), policyLanguage);
  if (strcmp (policyLanguage, "1.3.6.1.5.5.7.21.1") == 0)
    adds (str, " (id-ppl-inheritALL)\n");
  else if (strcmp (policyLanguage, "1.3.6.1.5.5.7.21.2") == 0)
    adds (str, " (id-ppl-independent)\n");
  else
    adds (str, "\n");
  if (npolicy)
    {
      adds (str, _("\t\t\tPolicy:\n\t\t\t\tASCII: "));
      asciiprint (str, policy, npolicy);
      adds (str, _("\n\t\t\t\tHexdump: "));
      hexprint (str, policy, npolicy);
      adds (str, "\n");
    }
}

static void
print_ski (gnutls_buffer_st * str, gnutls_x509_crt_t cert)
{
  char *buffer = NULL;
  size_t size = 0;
  int err;

  err = gnutls_x509_crt_get_subject_key_id (cert, buffer, &size, NULL);
  if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      addf (str, "error: get_subject_key_id: %s\n", gnutls_strerror (err));
      return;
    }

  buffer = gnutls_malloc (size);
  if (!buffer)
    {
      addf (str, "error: malloc: %s\n",
            gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
      return;
    }

  err = gnutls_x509_crt_get_subject_key_id (cert, buffer, &size, NULL);
  if (err < 0)
    {
      gnutls_free (buffer);
      addf (str, "error: get_subject_key_id2: %s\n", gnutls_strerror (err));
      return;
    }

  adds (str, "\t\t\t");
  hexprint (str, buffer, size);
  adds (str, "\n");

  gnutls_free (buffer);
}

#define TYPE_CRL 1
#define TYPE_CRT 2
#define TYPE_CRQ 3

#define TYPE_CRT_SAN TYPE_CRT
#define TYPE_CRQ_SAN TYPE_CRQ
#define TYPE_CRT_IAN 4

typedef union
{
  gnutls_x509_crt_t crt;
  gnutls_x509_crq_t crq;
  gnutls_x509_crl_t crl;
} cert_type_t;

static void
print_aki (gnutls_buffer_st * str, int type, cert_type_t cert)
{
  char *buffer = NULL;
  size_t size = 0;
  int err;

  if (type == TYPE_CRT)
    err =
      gnutls_x509_crt_get_authority_key_id (cert.crt, buffer, &size, NULL);
  else if (type == TYPE_CRL)
    err =
      gnutls_x509_crl_get_authority_key_id (cert.crl, buffer, &size, NULL);
  else
    {
      gnutls_assert ();
      return;
    }

  if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      addf (str, "error: get_authority_key_id: %s\n", gnutls_strerror (err));
      return;
    }

  buffer = gnutls_malloc (size);
  if (!buffer)
    {
      addf (str, "error: malloc: %s\n",
            gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
      return;
    }

  if (type == TYPE_CRT)
    err =
      gnutls_x509_crt_get_authority_key_id (cert.crt, buffer, &size, NULL);
  else
    err =
      gnutls_x509_crl_get_authority_key_id (cert.crl, buffer, &size, NULL);

  if (err < 0)
    {
      gnutls_free (buffer);
      addf (str, "error: get_authority_key_id2: %s\n", gnutls_strerror (err));
      return;
    }

  adds (str, "\t\t\t");
  hexprint (str, buffer, size);
  adds (str, "\n");

  gnutls_free (buffer);
}

static void
print_key_usage (gnutls_buffer_st * str, const char *prefix, int type,
                 cert_type_t cert)
{
  unsigned int key_usage;
  int err;

  if (type == TYPE_CRT)
    err = gnutls_x509_crt_get_key_usage (cert.crt, &key_usage, NULL);
  else if (type == TYPE_CRQ)
    err = gnutls_x509_crq_get_key_usage (cert.crq, &key_usage, NULL);
  else
    return;

  if (err < 0)
    {
      addf (str, "error: get_key_usage: %s\n", gnutls_strerror (err));
      return;
    }

  if (key_usage & GNUTLS_KEY_DIGITAL_SIGNATURE)
    addf (str, _("%s\t\t\tDigital signature.\n"), prefix);
  if (key_usage & GNUTLS_KEY_NON_REPUDIATION)
    addf (str, _("%s\t\t\tNon repudiation.\n"), prefix);
  if (key_usage & GNUTLS_KEY_KEY_ENCIPHERMENT)
    addf (str, _("%s\t\t\tKey encipherment.\n"), prefix);
  if (key_usage & GNUTLS_KEY_DATA_ENCIPHERMENT)
    addf (str, _("%s\t\t\tData encipherment.\n"), prefix);
  if (key_usage & GNUTLS_KEY_KEY_AGREEMENT)
    addf (str, _("%s\t\t\tKey agreement.\n"), prefix);
  if (key_usage & GNUTLS_KEY_KEY_CERT_SIGN)
    addf (str, _("%s\t\t\tCertificate signing.\n"), prefix);
  if (key_usage & GNUTLS_KEY_CRL_SIGN)
    addf (str, _("%s\t\t\tCRL signing.\n"), prefix);
  if (key_usage & GNUTLS_KEY_ENCIPHER_ONLY)
    addf (str, _("%s\t\t\tKey encipher only.\n"), prefix);
  if (key_usage & GNUTLS_KEY_DECIPHER_ONLY)
    addf (str, _("%s\t\t\tKey decipher only.\n"), prefix);
}

#ifdef ENABLE_PKI

static void
print_crldist (gnutls_buffer_st * str, gnutls_x509_crt_t cert)
{
  char *buffer = NULL;
  size_t size;
  char str_ip[64];
  char *p;
  int err;
  int indx;

  for (indx = 0;; indx++)
    {
      size = 0;
      err = gnutls_x509_crt_get_crl_dist_points (cert, indx, buffer, &size,
                                                 NULL, NULL);
      if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
        return;
      if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          addf (str, "error: get_crl_dist_points: %s\n",
                gnutls_strerror (err));
          return;
        }

      buffer = gnutls_malloc (size);
      if (!buffer)
        {
          addf (str, "error: malloc: %s\n",
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
          return;
        }

      err = gnutls_x509_crt_get_crl_dist_points (cert, indx, buffer, &size,
                                                 NULL, NULL);
      if (err < 0)
        {
          gnutls_free (buffer);
          addf (str, "error: get_crl_dist_points2: %s\n",
                gnutls_strerror (err));
          return;
        }

      if ((err == GNUTLS_SAN_DNSNAME
           || err == GNUTLS_SAN_RFC822NAME
           || err == GNUTLS_SAN_URI) && strlen (buffer) != size)
        {
          adds (str, _("warning: distributionPoint contains an embedded NUL, "
                       "replacing with '!'\n"));
          while (strlen (buffer) < size)
            buffer[strlen (buffer)] = '!';
        }

      switch (err)
        {
        case GNUTLS_SAN_DNSNAME:
          addf (str, "\t\t\tDNSname: %.*s\n", (int) size, buffer);
          break;

        case GNUTLS_SAN_RFC822NAME:
          addf (str, "\t\t\tRFC822name: %.*s\n", (int) size, buffer);
          break;

        case GNUTLS_SAN_URI:
          addf (str, "\t\t\tURI: %.*s\n", (int) size, buffer);
          break;

        case GNUTLS_SAN_IPADDRESS:
          p = ip_to_string (buffer, size, str_ip, sizeof (str_ip));
          if (p == NULL)
            p = ERROR_STR;
          addf (str, "\t\t\tIPAddress: %s\n", p);
          break;

        case GNUTLS_SAN_DN:
          addf (str, "\t\t\tdirectoryName: %.*s\n", (int) size, buffer);
          break;

        default:
          addf (str, "error: unknown SAN\n");
          break;
        }
      gnutls_free (buffer);
    }
}

static void
print_key_purpose (gnutls_buffer_st * str, const char *prefix, int type,
                   cert_type_t cert)
{
  int indx;
  char *buffer = NULL;
  size_t size;
  int err;

  for (indx = 0;; indx++)
    {
      size = 0;
      if (type == TYPE_CRT)
        err = gnutls_x509_crt_get_key_purpose_oid (cert.crt, indx, buffer,
                                                   &size, NULL);
      else if (type == TYPE_CRQ)
        err = gnutls_x509_crq_get_key_purpose_oid (cert.crq, indx, buffer,
                                                   &size, NULL);
      else
        return;

      if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
        return;
      if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          addf (str, "error: get_key_purpose_oid: %s\n",
                gnutls_strerror (err));
          return;
        }

      buffer = gnutls_malloc (size);
      if (!buffer)
        {
          addf (str, "error: malloc: %s\n",
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
          return;
        }

      if (type == TYPE_CRT)
        err = gnutls_x509_crt_get_key_purpose_oid (cert.crt, indx, buffer,
                                                   &size, NULL);
      else
        err = gnutls_x509_crq_get_key_purpose_oid (cert.crq, indx, buffer,
                                                   &size, NULL);

      if (err < 0)
        {
          gnutls_free (buffer);
          addf (str, "error: get_key_purpose_oid2: %s\n",
                gnutls_strerror (err));
          return;
        }

      if (strcmp (buffer, GNUTLS_KP_TLS_WWW_SERVER) == 0)
        addf (str, _("%s\t\t\tTLS WWW Server.\n"), prefix);
      else if (strcmp (buffer, GNUTLS_KP_TLS_WWW_CLIENT) == 0)
        addf (str, _("%s\t\t\tTLS WWW Client.\n"), prefix);
      else if (strcmp (buffer, GNUTLS_KP_CODE_SIGNING) == 0)
        addf (str, _("%s\t\t\tCode signing.\n"), prefix);
      else if (strcmp (buffer, GNUTLS_KP_EMAIL_PROTECTION) == 0)
        addf (str, _("%s\t\t\tEmail protection.\n"), prefix);
      else if (strcmp (buffer, GNUTLS_KP_TIME_STAMPING) == 0)
        addf (str, _("%s\t\t\tTime stamping.\n"), prefix);
      else if (strcmp (buffer, GNUTLS_KP_OCSP_SIGNING) == 0)
        addf (str, _("%s\t\t\tOCSP signing.\n"), prefix);
      else if (strcmp (buffer, GNUTLS_KP_IPSEC_IKE) == 0)
        addf (str, _("%s\t\t\tIpsec IKE.\n"), prefix);
      else if (strcmp (buffer, GNUTLS_KP_ANY) == 0)
        addf (str, _("%s\t\t\tAny purpose.\n"), prefix);
      else
        addf (str, "%s\t\t\t%s\n", prefix, buffer);

      gnutls_free (buffer);
    }
}

#endif

static void
print_basic (gnutls_buffer_st * str, const char *prefix, int type,
             cert_type_t cert)
{
  int pathlen;
  int err;

  if (type == TYPE_CRT)
    err =
      gnutls_x509_crt_get_basic_constraints (cert.crt, NULL, NULL, &pathlen);
  else if (type == TYPE_CRQ)
    err =
      gnutls_x509_crq_get_basic_constraints (cert.crq, NULL, NULL, &pathlen);
  else
    return;

  if (err < 0)
    {
      addf (str, "error: get_basic_constraints: %s\n", gnutls_strerror (err));
      return;
    }

  if (err == 0)
    addf (str, _("%s\t\t\tCertificate Authority (CA): FALSE\n"), prefix);
  else
    addf (str, _("%s\t\t\tCertificate Authority (CA): TRUE\n"), prefix);

  if (pathlen >= 0)
    addf (str, _("%s\t\t\tPath Length Constraint: %d\n"), prefix, pathlen);
}


static void
print_altname (gnutls_buffer_st * str, const char *prefix, int altname_type,
               cert_type_t cert)
{
  unsigned int altname_idx;
  char str_ip[64];
  char *p;

  for (altname_idx = 0;; altname_idx++)
    {
      char *buffer = NULL;
      size_t size = 0;
      int err;

      if (altname_type == TYPE_CRT_SAN)
        err =
          gnutls_x509_crt_get_subject_alt_name (cert.crt, altname_idx, buffer,
                                                &size, NULL);
      else if (altname_type == TYPE_CRQ_SAN)
        err =
          gnutls_x509_crq_get_subject_alt_name (cert.crq, altname_idx, buffer,
                                                &size, NULL, NULL);
      else if (altname_type == TYPE_CRT_IAN)
        err =
          gnutls_x509_crt_get_issuer_alt_name (cert.crt, altname_idx, buffer,
                                               &size, NULL);
      else
        return;

      if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
        break;
      if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          addf (str, "error: get_subject/issuer_alt_name: %s\n",
                gnutls_strerror (err));
          return;
        }

      buffer = gnutls_malloc (size);
      if (!buffer)
        {
          addf (str, "error: malloc: %s\n",
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
          return;
        }

      if (altname_type == TYPE_CRT_SAN)
        err =
          gnutls_x509_crt_get_subject_alt_name (cert.crt, altname_idx, buffer,
                                                &size, NULL);
      else if (altname_type == TYPE_CRQ_SAN)
        err =
          gnutls_x509_crq_get_subject_alt_name (cert.crq, altname_idx, buffer,
                                                &size, NULL, NULL);
      else if (altname_type == TYPE_CRT_IAN)
        err = gnutls_x509_crt_get_issuer_alt_name (cert.crt, altname_idx,
                                                   buffer, &size, NULL);

      if (err < 0)
        {
          gnutls_free (buffer);
          addf (str, "error: get_subject/issuer_alt_name2: %s\n",
                gnutls_strerror (err));
          return;
        }

      if ((err == GNUTLS_SAN_DNSNAME
           || err == GNUTLS_SAN_RFC822NAME
           || err == GNUTLS_SAN_URI) && strlen (buffer) != size)
        {
          adds (str, _("warning: altname contains an embedded NUL, "
                       "replacing with '!'\n"));
          while (strlen (buffer) < size)
            buffer[strlen (buffer)] = '!';
        }

      switch (err)
        {
        case GNUTLS_SAN_DNSNAME:
          addf (str, "%s\t\t\tDNSname: %.*s\n", prefix, (int) size, buffer);
          break;

        case GNUTLS_SAN_RFC822NAME:
          addf (str, "%s\t\t\tRFC822name: %.*s\n", prefix, (int) size,
                buffer);
          break;

        case GNUTLS_SAN_URI:
          addf (str, "%s\t\t\tURI: %.*s\n", prefix, (int) size, buffer);
          break;

        case GNUTLS_SAN_IPADDRESS:
          p = ip_to_string (buffer, size, str_ip, sizeof (str_ip));
          if (p == NULL)
            p = ERROR_STR;
          addf (str, "%s\t\t\tIPAddress: %s\n", prefix, p);
          break;

        case GNUTLS_SAN_DN:
          addf (str, "%s\t\t\tdirectoryName: %.*s\n", prefix,
                (int) size, buffer);
          break;

        case GNUTLS_SAN_OTHERNAME:
          {
            char *oid = NULL;
            size_t oidsize;

            oidsize = 0;
            if (altname_type == TYPE_CRT_SAN)
              err = gnutls_x509_crt_get_subject_alt_othername_oid
                (cert.crt, altname_idx, oid, &oidsize);
            else if (altname_type == TYPE_CRQ_SAN)
              err = gnutls_x509_crq_get_subject_alt_othername_oid
                (cert.crq, altname_idx, oid, &oidsize);
            else if (altname_type == TYPE_CRT_IAN)
              err = gnutls_x509_crt_get_issuer_alt_othername_oid
                (cert.crt, altname_idx, oid, &oidsize);

            if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
              {
                gnutls_free (buffer);
                addf (str,
                      "error: get_subject/issuer_alt_othername_oid: %s\n",
                      gnutls_strerror (err));
                return;
              }

            oid = gnutls_malloc (oidsize);
            if (!oid)
              {
                gnutls_free (buffer);
                addf (str, "error: malloc: %s\n",
                      gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
                return;
              }

            if (altname_type == TYPE_CRT_SAN)
              err = gnutls_x509_crt_get_subject_alt_othername_oid
                (cert.crt, altname_idx, oid, &oidsize);
            else if (altname_type == TYPE_CRQ_SAN)
              err = gnutls_x509_crq_get_subject_alt_othername_oid
                (cert.crq, altname_idx, oid, &oidsize);
            else if (altname_type == TYPE_CRT_IAN)
              err = gnutls_x509_crt_get_issuer_alt_othername_oid
                (cert.crt, altname_idx, oid, &oidsize);

            if (err < 0)
              {
                gnutls_free (buffer);
                gnutls_free (oid);
                addf (str, "error: get_subject_alt_othername_oid2: %s\n",
                      gnutls_strerror (err));
                return;
              }

            if (err == GNUTLS_SAN_OTHERNAME_XMPP)
              {
                if (strlen (buffer) != size)
                  {
                    adds (str, _("warning: altname contains an embedded NUL, "
                                 "replacing with '!'\n"));
                    while (strlen (buffer) < size)
                      buffer[strlen (buffer)] = '!';
                  }

                addf (str, _("%s\t\t\tXMPP Address: %.*s\n"), prefix,
                      (int) size, buffer);
              }
            else
              {
                addf (str, _("%s\t\t\totherName OID: %.*s\n"), prefix,
                      (int) oidsize, oid);
                addf (str, _("%s\t\t\totherName DER: "), prefix);
                hexprint (str, buffer, size);
                addf (str, _("\n%s\t\t\totherName ASCII: "), prefix);
                asciiprint (str, buffer, size);
                addf (str, "\n");
              }
            gnutls_free (oid);
          }
          break;

        default:
          addf (str, "error: unknown altname\n");
          break;
        }

      gnutls_free (buffer);
    }
}

static void
guiddump (gnutls_buffer_st * str, const char *data, size_t len,
          const char *spc)
{
  size_t j;

  if (spc)
    adds (str, spc);
  addf (str, "{");
  addf (str, "%.2X", (unsigned char) data[3]);
  addf (str, "%.2X", (unsigned char) data[2]);
  addf (str, "%.2X", (unsigned char) data[1]);
  addf (str, "%.2X", (unsigned char) data[0]);
  addf (str, "-");
  addf (str, "%.2X", (unsigned char) data[5]);
  addf (str, "%.2X", (unsigned char) data[4]);
  addf (str, "-");
  addf (str, "%.2X", (unsigned char) data[7]);
  addf (str, "%.2X", (unsigned char) data[6]);
  addf (str, "-");
  addf (str, "%.2X", (unsigned char) data[8]);
  addf (str, "%.2X", (unsigned char) data[9]);
  addf (str, "-");
  for (j = 10; j < 16; j++)
    {
      addf (str, "%.2X", (unsigned char) data[j]);
    }
  addf (str, "}\n");
}

static void
print_unique_ids (gnutls_buffer_st * str, const gnutls_x509_crt_t cert)
{
  int result;
  char buf[256];                /* if its longer, we won't bother to print it */
  size_t buf_size = 256;

  result = gnutls_x509_crt_get_issuer_unique_id (cert, buf, &buf_size);
  if (result >= 0)
    {
      addf (str, ("\t\tIssuer Unique ID:\n"));
      hexdump (str, buf, buf_size, "\t\t\t");
      if (buf_size == 16)
        {                       /* this could be a GUID */
          guiddump (str, buf, buf_size, "\t\t\t");
        }
    }

  buf_size = 256;
  result = gnutls_x509_crt_get_subject_unique_id (cert, buf, &buf_size);
  if (result >= 0)
    {
      addf (str, ("\t\tSubject Unique ID:\n"));
      hexdump (str, buf, buf_size, "\t\t\t");
      if (buf_size == 16)
        {                       /* this could be a GUID */
          guiddump (str, buf, buf_size, "\t\t\t");
        }
    }
}

static void
print_extensions (gnutls_buffer_st * str, const char *prefix, int type,
                  cert_type_t cert)
{
  int i, err;
  int san_idx = 0;
  int ian_idx = 0;
  int proxy_idx = 0;
  int basic_idx = 0;
  int keyusage_idx = 0;
  int keypurpose_idx = 0;
  int ski_idx = 0;
  int aki_idx = 0;
  int crldist_idx = 0;

  for (i = 0;; i++)
    {
      char oid[MAX_OID_SIZE] = "";
      size_t sizeof_oid = sizeof (oid);
      int critical;

      if (type == TYPE_CRT)
        err = gnutls_x509_crt_get_extension_info (cert.crt, i,
                                                  oid, &sizeof_oid,
                                                  &critical);

      else if (type == TYPE_CRQ)
        err = gnutls_x509_crq_get_extension_info (cert.crq, i,
                                                  oid, &sizeof_oid,
                                                  &critical);
      else
        {
          gnutls_assert ();
          return;
        }

      if (err < 0)
        {
          if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
            break;
          addf (str, "error: get_extension_info: %s\n",
                gnutls_strerror (err));
          continue;
        }

      if (i == 0)
        addf (str, _("%s\tExtensions:\n"), prefix);

      if (strcmp (oid, "2.5.29.19") == 0)
        {
          if (basic_idx)
            {
              addf (str, "error: more than one basic constraint\n");
              continue;
            }

          addf (str, _("%s\t\tBasic Constraints (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

          print_basic (str, prefix, type, cert);

          basic_idx++;
        }
      else if (strcmp (oid, "2.5.29.14") == 0)
        {
          if (ski_idx)
            {
              addf (str, "error: more than one SKI extension\n");
              continue;
            }

          addf (str, _("%s\t\tSubject Key Identifier (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

          if (type == TYPE_CRT)
            print_ski (str, cert.crt);

          ski_idx++;
        }
      else if (strcmp (oid, "2.5.29.35") == 0)
        {

          if (aki_idx)
            {
              addf (str, "error: more than one AKI extension\n");
              continue;
            }

          addf (str, _("%s\t\tAuthority Key Identifier (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

          if (type == TYPE_CRT)
            print_aki (str, TYPE_CRT, cert);

          aki_idx++;
        }
      else if (strcmp (oid, "2.5.29.15") == 0)
        {
          if (keyusage_idx)
            {
              addf (str, "error: more than one key usage extension\n");
              continue;
            }

          addf (str, _("%s\t\tKey Usage (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

          print_key_usage (str, prefix, type, cert);

          keyusage_idx++;
        }
      else if (strcmp (oid, "2.5.29.37") == 0)
        {
          if (keypurpose_idx)
            {
              addf (str, "error: more than one key purpose extension\n");
              continue;
            }

          addf (str, _("%s\t\tKey Purpose (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

#ifdef ENABLE_PKI
          print_key_purpose (str, prefix, type, cert);
#endif

          keypurpose_idx++;
        }
      else if (strcmp (oid, "2.5.29.17") == 0)
        {
          if (san_idx)
            {
              addf (str, "error: more than one SKI extension\n");
              continue;
            }

          addf (str, _("%s\t\tSubject Alternative Name (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

          print_altname (str, prefix, type, cert);

          san_idx++;
        }
      else if (strcmp (oid, "2.5.29.18") == 0)
        {
          if (ian_idx)
            {
              addf (str, "error: more than one Issuer AltName extension\n");
              continue;
            }

          addf (str, _("%s\t\tIssuer Alternative Name (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

          print_altname (str, prefix, TYPE_CRT_IAN, cert);

          ian_idx++;
        }
      else if (strcmp (oid, "2.5.29.31") == 0)
        {
          if (crldist_idx)
            {
              addf (str, "error: more than one CRL distribution point\n");
              continue;
            }

          addf (str, _("%s\t\tCRL Distribution points (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

#ifdef ENABLE_PKI
          if (type == TYPE_CRT)
            print_crldist (str, cert.crt);
#endif

          crldist_idx++;
        }
      else if (strcmp (oid, "1.3.6.1.5.5.7.1.14") == 0)
        {
          if (proxy_idx)
            {
              addf (str, "error: more than one proxy extension\n");
              continue;
            }

          addf (str, _("%s\t\tProxy Certificate Information (%s):\n"), prefix,
                critical ? _("critical") : _("not critical"));

          if (type == TYPE_CRT)
            print_proxy (str, cert.crt);

          proxy_idx++;
        }
      else
        {
          char *buffer;
          size_t extlen = 0;

          addf (str, _("%s\t\tUnknown extension %s (%s):\n"), prefix, oid,
                critical ? _("critical") : _("not critical"));

          if (type == TYPE_CRT)
            err =
              gnutls_x509_crt_get_extension_data (cert.crt, i, NULL, &extlen);
          else if (type == TYPE_CRQ)
            err =
              gnutls_x509_crq_get_extension_data (cert.crq, i, NULL, &extlen);
          else
            {
              gnutls_assert ();
              return;
            }

          if (err < 0)
            {
              addf (str, "error: get_extension_data: %s\n",
                    gnutls_strerror (err));
              continue;
            }

          buffer = gnutls_malloc (extlen);
          if (!buffer)
            {
              addf (str, "error: malloc: %s\n",
                    gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
              continue;
            }

          if (type == TYPE_CRT)
            err =
              gnutls_x509_crt_get_extension_data (cert.crt, i, buffer,
                                                  &extlen);
          else if (type == TYPE_CRQ)
            err =
              gnutls_x509_crq_get_extension_data (cert.crq, i, buffer,
                                                  &extlen);

          if (err < 0)
            {
              gnutls_free (buffer);
              addf (str, "error: get_extension_data2: %s\n",
                    gnutls_strerror (err));
              continue;
            }

          addf (str, _("%s\t\t\tASCII: "), prefix);
          asciiprint (str, buffer, extlen);
          addf (str, "\n");

          addf (str, _("%s\t\t\tHexdump: "), prefix);
          hexprint (str, buffer, extlen);
          adds (str, "\n");

          gnutls_free (buffer);
        }
    }
}

static void
print_cert (gnutls_buffer_st * str, gnutls_x509_crt_t cert, int notsigned)
{
  /* Version. */
  {
    int version = gnutls_x509_crt_get_version (cert);
    if (version < 0)
      addf (str, "error: get_version: %s\n", gnutls_strerror (version));
    else
      addf (str, _("\tVersion: %d\n"), version);
  }

  /* Serial. */
  {
    char serial[128];
    size_t serial_size = sizeof (serial);
    int err;

    err = gnutls_x509_crt_get_serial (cert, serial, &serial_size);
    if (err < 0)
      addf (str, "error: get_serial: %s\n", gnutls_strerror (err));
    else
      {
        adds (str, _("\tSerial Number (hex): "));
        hexprint (str, serial, serial_size);
        adds (str, "\n");
      }
  }

  /* Issuer. */
  if (!notsigned)
    {
      char *dn;
      size_t dn_size = 0;
      int err;

      err = gnutls_x509_crt_get_issuer_dn (cert, NULL, &dn_size);
      if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
        addf (str, "error: get_issuer_dn: %s\n", gnutls_strerror (err));
      else
        {
          dn = gnutls_malloc (dn_size);
          if (!dn)
            addf (str, "error: malloc (%d): %s\n", (int) dn_size,
                  gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
          else
            {
              err = gnutls_x509_crt_get_issuer_dn (cert, dn, &dn_size);
              if (err < 0)
                addf (str, "error: get_issuer_dn: %s\n",
                      gnutls_strerror (err));
              else
                addf (str, _("\tIssuer: %s\n"), dn);
              gnutls_free (dn);
            }
        }
    }

  /* Validity. */
  {
    time_t tim;

    adds (str, _("\tValidity:\n"));

    tim = gnutls_x509_crt_get_activation_time (cert);
    {
      char s[42];
      size_t max = sizeof (s);
      struct tm t;

      if (gmtime_r (&tim, &t) == NULL)
        addf (str, "error: gmtime_r (%ld)\n", (unsigned long) tim);
      else if (strftime (s, max, "%a %b %d %H:%M:%S UTC %Y", &t) == 0)
        addf (str, "error: strftime (%ld)\n", (unsigned long) tim);
      else
        addf (str, _("\t\tNot Before: %s\n"), s);
    }

    tim = gnutls_x509_crt_get_expiration_time (cert);
    {
      char s[42];
      size_t max = sizeof (s);
      struct tm t;

      if (gmtime_r (&tim, &t) == NULL)
        addf (str, "error: gmtime_r (%ld)\n", (unsigned long) tim);
      else if (strftime (s, max, "%a %b %d %H:%M:%S UTC %Y", &t) == 0)
        addf (str, "error: strftime (%ld)\n", (unsigned long) tim);
      else
        addf (str, _("\t\tNot After: %s\n"), s);
    }
  }

  /* Subject. */
  {
    char *dn;
    size_t dn_size = 0;
    int err;

    err = gnutls_x509_crt_get_dn (cert, NULL, &dn_size);
    if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
      addf (str, "error: get_dn: %s\n", gnutls_strerror (err));
    else
      {
        dn = gnutls_malloc (dn_size);
        if (!dn)
          addf (str, "error: malloc (%d): %s\n", (int) dn_size,
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
        else
          {
            err = gnutls_x509_crt_get_dn (cert, dn, &dn_size);
            if (err < 0)
              addf (str, "error: get_dn: %s\n", gnutls_strerror (err));
            else
              addf (str, _("\tSubject: %s\n"), dn);
            gnutls_free (dn);
          }
      }
  }

  /* SubjectPublicKeyInfo. */
  {
    int err;
    unsigned int bits;

    err = gnutls_x509_crt_get_pk_algorithm (cert, &bits);
    if (err < 0)
      addf (str, "error: get_pk_algorithm: %s\n", gnutls_strerror (err));
    else
      {
        const char *name = gnutls_pk_algorithm_get_name (err);
        if (name == NULL)
          name = _("unknown");

        addf (str, _("\tSubject Public Key Algorithm: %s\n"), name);
        addf (str, _("\tCertificate Security Level: %s\n"),
              gnutls_sec_param_get_name (gnutls_pk_bits_to_sec_param
                                         (err, bits)));

#ifdef ENABLE_PKI
        switch (err)
          {
          case GNUTLS_PK_RSA:
            {
              gnutls_datum_t m, e;

              err = gnutls_x509_crt_get_pk_rsa_raw (cert, &m, &e);
              if (err < 0)
                addf (str, "error: get_pk_rsa_raw: %s\n",
                      gnutls_strerror (err));
              else
                {
                  addf (str, _("\t\tModulus (bits %d):\n"), bits);
                  hexdump (str, m.data, m.size, "\t\t\t");
                  addf (str, _("\t\tExponent (bits %d):\n"), e.size * 8);
                  hexdump (str, e.data, e.size, "\t\t\t");

                  gnutls_free (m.data);
                  gnutls_free (e.data);
                }

            }
            break;

          case GNUTLS_PK_DSA:
            {
              gnutls_datum_t p, q, g, y;

              err = gnutls_x509_crt_get_pk_dsa_raw (cert, &p, &q, &g, &y);
              if (err < 0)
                addf (str, "error: get_pk_dsa_raw: %s\n",
                      gnutls_strerror (err));
              else
                {
                  addf (str, _("\t\tPublic key (bits %d):\n"), bits);
                  hexdump (str, y.data, y.size, "\t\t\t");
                  adds (str, _("\t\tP:\n"));
                  hexdump (str, p.data, p.size, "\t\t\t");
                  adds (str, _("\t\tQ:\n"));
                  hexdump (str, q.data, q.size, "\t\t\t");
                  adds (str, _("\t\tG:\n"));
                  hexdump (str, g.data, g.size, "\t\t\t");

                  gnutls_free (p.data);
                  gnutls_free (q.data);
                  gnutls_free (g.data);
                  gnutls_free (y.data);

                }
            }
            break;

          default:
            break;
          }
#endif
      }
  }

  print_unique_ids (str, cert);

  /* Extensions. */
  if (gnutls_x509_crt_get_version (cert) >= 3)
    {
      cert_type_t ccert;

      ccert.crt = cert;
      print_extensions (str, "", TYPE_CRT, ccert);
    }

  /* Signature. */
  if (!notsigned)
    {
      int err;
      size_t size = 0;
      char *buffer = NULL;

      err = gnutls_x509_crt_get_signature_algorithm (cert);
      if (err < 0)
        addf (str, "error: get_signature_algorithm: %s\n",
              gnutls_strerror (err));
      else
        {
          const char *name = gnutls_sign_algorithm_get_name (err);
          if (name == NULL)
            name = _("unknown");
          addf (str, _("\tSignature Algorithm: %s\n"), name);
        }
      if (err == GNUTLS_SIGN_RSA_MD5 || err == GNUTLS_SIGN_RSA_MD2)
        {
          adds (str, _("warning: signed using a broken signature "
                       "algorithm that can be forged.\n"));
        }

      err = gnutls_x509_crt_get_signature (cert, buffer, &size);
      if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          addf (str, "error: get_signature: %s\n", gnutls_strerror (err));
          return;
        }

      buffer = gnutls_malloc (size);
      if (!buffer)
        {
          addf (str, "error: malloc: %s\n",
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
          return;
        }

      err = gnutls_x509_crt_get_signature (cert, buffer, &size);
      if (err < 0)
        {
          gnutls_free (buffer);
          addf (str, "error: get_signature2: %s\n", gnutls_strerror (err));
          return;
        }

      adds (str, _("\tSignature:\n"));
      hexdump (str, buffer, size, "\t\t");

      gnutls_free (buffer);
    }
}

static void
print_fingerprint (gnutls_buffer_st * str, gnutls_x509_crt_t cert,
                   gnutls_digest_algorithm_t algo)
{
  int err;
  char buffer[MAX_HASH_SIZE];
  size_t size = sizeof (buffer);

  err = gnutls_x509_crt_get_fingerprint (cert, algo, buffer, &size);
  if (err < 0)
    {
      addf (str, "error: get_fingerprint: %s\n", gnutls_strerror (err));
      return;
    }

  if (algo == GNUTLS_DIG_MD5)
    adds (str, _("\tMD5 fingerprint:\n\t\t"));
  else
    adds (str, _("\tSHA-1 fingerprint:\n\t\t"));
  hexprint (str, buffer, size);
  adds (str, "\n");
}

static void
print_keyid (gnutls_buffer_st * str, gnutls_x509_crt_t cert)
{
  int err;
  char buffer[20];
  size_t size = 20;

  err = gnutls_x509_crt_get_key_id (cert, 0, buffer, &size);
  if (err < 0)
    {
      addf (str, "error: get_key_id: %s\n", gnutls_strerror (err));
      return;
    }

  adds (str, _("\tPublic Key Id:\n\t\t"));
  hexprint (str, buffer, size);
  adds (str, "\n");
}

static void
print_other (gnutls_buffer_st * str, gnutls_x509_crt_t cert, int notsigned)
{
  if (!notsigned)
    {
      print_fingerprint (str, cert, GNUTLS_DIG_MD5);
      print_fingerprint (str, cert, GNUTLS_DIG_SHA1);
    }
  print_keyid (str, cert);
}

static void
print_oneline (gnutls_buffer_st * str, gnutls_x509_crt_t cert)
{
  /* Subject. */
  {
    char *dn;
    size_t dn_size = 0;
    int err;

    err = gnutls_x509_crt_get_dn (cert, NULL, &dn_size);
    if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
      addf (str, "unknown subject (%s), ", gnutls_strerror (err));
    else
      {
        dn = gnutls_malloc (dn_size);
        if (!dn)
          addf (str, "unknown subject (%s), ",
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
        else
          {
            err = gnutls_x509_crt_get_dn (cert, dn, &dn_size);
            if (err < 0)
              addf (str, "unknown subject (%s), ", gnutls_strerror (err));
            else
              addf (str, "subject `%s', ", dn);
            gnutls_free (dn);
          }
      }
  }

  /* Issuer. */
  {
    char *dn;
    size_t dn_size = 0;
    int err;

    err = gnutls_x509_crt_get_issuer_dn (cert, NULL, &dn_size);
    if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
      addf (str, "unknown issuer (%s), ", gnutls_strerror (err));
    else
      {
        dn = gnutls_malloc (dn_size);
        if (!dn)
          addf (str, "unknown issuer (%s), ",
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
        else
          {
            err = gnutls_x509_crt_get_issuer_dn (cert, dn, &dn_size);
            if (err < 0)
              addf (str, "unknown issuer (%s), ", gnutls_strerror (err));
            else
              addf (str, "issuer `%s', ", dn);
            gnutls_free (dn);
          }
      }
  }

  /* Key algorithm and size. */
  {
    int bits;
    const char *name = gnutls_pk_algorithm_get_name
      (gnutls_x509_crt_get_pk_algorithm (cert, &bits));
    if (name == NULL)
      name = "Unknown";
    addf (str, "%s key %d bits, ", name, bits);
  }

  /* Signature Algorithm. */
  {
    int err;

    err = gnutls_x509_crt_get_signature_algorithm (cert);
    if (err < 0)
      addf (str, "unknown signature algorithm (%s), ", gnutls_strerror (err));
    else
      {
        const char *name = gnutls_sign_algorithm_get_name (err);
        if (name == NULL)
          name = _("unknown");
        if (err == GNUTLS_SIGN_RSA_MD5 || err == GNUTLS_SIGN_RSA_MD2)
          addf (str, _("signed using %s (broken!), "), name);
        else
          addf (str, _("signed using %s, "), name);
      }
  }

  /* Validity. */
  {
    time_t tim;

    tim = gnutls_x509_crt_get_activation_time (cert);
    {
      char s[42];
      size_t max = sizeof (s);
      struct tm t;

      if (gmtime_r (&tim, &t) == NULL)
        addf (str, "unknown activation (%ld), ", (unsigned long) tim);
      else if (strftime (s, max, "%Y-%m-%d %H:%M:%S UTC", &t) == 0)
        addf (str, "failed activation (%ld), ", (unsigned long) tim);
      else
        addf (str, "activated `%s', ", s);
    }

    tim = gnutls_x509_crt_get_expiration_time (cert);
    {
      char s[42];
      size_t max = sizeof (s);
      struct tm t;

      if (gmtime_r (&tim, &t) == NULL)
        addf (str, "unknown expiry (%ld), ", (unsigned long) tim);
      else if (strftime (s, max, "%Y-%m-%d %H:%M:%S UTC", &t) == 0)
        addf (str, "failed expiry (%ld), ", (unsigned long) tim);
      else
        addf (str, "expires `%s', ", s);
    }
  }

  {
    int pathlen;
    char *policyLanguage;
    int err;

    err = gnutls_x509_crt_get_proxy (cert, NULL,
                                     &pathlen, &policyLanguage, NULL, NULL);
    if (err == 0)
      {
        addf (str, "proxy certificate (policy=");
        if (strcmp (policyLanguage, "1.3.6.1.5.5.7.21.1") == 0)
          addf (str, "id-ppl-inheritALL");
        else if (strcmp (policyLanguage, "1.3.6.1.5.5.7.21.2") == 0)
          addf (str, "id-ppl-independent");
        else
          addf (str, "%s", policyLanguage);
        if (pathlen >= 0)
          addf (str, ", pathlen=%d), ", pathlen);
        else
          addf (str, "), ");
        gnutls_free (policyLanguage);
      }
  }

  {
    char buffer[20];
    size_t size = sizeof (buffer);
    int err;

    err = gnutls_x509_crt_get_fingerprint (cert, GNUTLS_DIG_SHA1,
                                           buffer, &size);
    if (err < 0)
      {
        addf (str, "unknown fingerprint (%s)", gnutls_strerror (err));
      }
    else
      {
        addf (str, "SHA-1 fingerprint `");
        hexprint (str, buffer, size);
        adds (str, "'");
      }
  }

}

/**
 * gnutls_x509_crt_print:
 * @cert: The structure to be printed
 * @format: Indicate the format to use
 * @out: Newly allocated datum with zero terminated string.
 *
 * This function will pretty print a X.509 certificate, suitable for
 * display to a human.
 *
 * If the format is %GNUTLS_CRT_PRINT_FULL then all fields of the
 * certificate will be output, on multiple lines.  The
 * %GNUTLS_CRT_PRINT_ONELINE format will generate one line with some
 * selected fields, which is useful for logging purposes.
 *
 * The output @out needs to be deallocate using gnutls_free().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_crt_print (gnutls_x509_crt_t cert,
                       gnutls_certificate_print_formats_t format,
                       gnutls_datum_t * out)
{
  gnutls_buffer_st str;

  if (format == GNUTLS_CRT_PRINT_FULL
      || format == GNUTLS_CRT_PRINT_UNSIGNED_FULL)
    {
      _gnutls_buffer_init (&str);

      _gnutls_buffer_append_str (&str, _("X.509 Certificate Information:\n"));

      print_cert (&str, cert, format == GNUTLS_CRT_PRINT_UNSIGNED_FULL);

      _gnutls_buffer_append_str (&str, _("Other Information:\n"));

      print_other (&str, cert, format == GNUTLS_CRT_PRINT_UNSIGNED_FULL);

      _gnutls_buffer_append_data (&str, "\0", 1);
      out->data = str.data;
      out->size = strlen (str.data);
    }
  else if (format == GNUTLS_CRT_PRINT_ONELINE)
    {
      _gnutls_buffer_init (&str);

      print_oneline (&str, cert);

      _gnutls_buffer_append_data (&str, "\0", 1);
      out->data = str.data;
      out->size = strlen (str.data);
    }
  else
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return 0;
}

#ifdef ENABLE_PKI

static void
print_crl (gnutls_buffer_st * str, gnutls_x509_crl_t crl, int notsigned)
{
  /* Version. */
  {
    int version = gnutls_x509_crl_get_version (crl);
    if (version == GNUTLS_E_ASN1_ELEMENT_NOT_FOUND)
      adds (str, _("\tVersion: 1 (default)\n"));
    else if (version < 0)
      addf (str, "error: get_version: %s\n", gnutls_strerror (version));
    else
      addf (str, _("\tVersion: %d\n"), version);
  }

  /* Issuer. */
  if (!notsigned)
    {
      char *dn;
      size_t dn_size = 0;
      int err;

      err = gnutls_x509_crl_get_issuer_dn (crl, NULL, &dn_size);
      if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
        addf (str, "error: get_issuer_dn: %s\n", gnutls_strerror (err));
      else
        {
          dn = gnutls_malloc (dn_size);
          if (!dn)
            addf (str, "error: malloc (%d): %s\n", (int) dn_size,
                  gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
          else
            {
              err = gnutls_x509_crl_get_issuer_dn (crl, dn, &dn_size);
              if (err < 0)
                addf (str, "error: get_issuer_dn: %s\n",
                      gnutls_strerror (err));
              else
                addf (str, _("\tIssuer: %s\n"), dn);
            }
          gnutls_free (dn);
        }
    }

  /* Validity. */
  {
    time_t tim;

    adds (str, _("\tUpdate dates:\n"));

    tim = gnutls_x509_crl_get_this_update (crl);
    {
      char s[42];
      size_t max = sizeof (s);
      struct tm t;

      if (gmtime_r (&tim, &t) == NULL)
        addf (str, "error: gmtime_r (%ld)\n", (unsigned long) tim);
      else if (strftime (s, max, "%a %b %d %H:%M:%S UTC %Y", &t) == 0)
        addf (str, "error: strftime (%ld)\n", (unsigned long) tim);
      else
        addf (str, _("\t\tIssued: %s\n"), s);
    }

    tim = gnutls_x509_crl_get_next_update (crl);
    {
      char s[42];
      size_t max = sizeof (s);
      struct tm t;

      if (tim == -1)
        addf (str, "\t\tNo next update time.\n");
      else if (gmtime_r (&tim, &t) == NULL)
        addf (str, "error: gmtime_r (%ld)\n", (unsigned long) tim);
      else if (strftime (s, max, "%a %b %d %H:%M:%S UTC %Y", &t) == 0)
        addf (str, "error: strftime (%ld)\n", (unsigned long) tim);
      else
        addf (str, _("\t\tNext at: %s\n"), s);
    }
  }

  /* Extensions. */
  if (gnutls_x509_crl_get_version (crl) >= 2)
    {
      size_t i;
      int err = 0;
      int aki_idx = 0;
      int crl_nr = 0;

      for (i = 0;; i++)
        {
          char oid[MAX_OID_SIZE] = "";
          size_t sizeof_oid = sizeof (oid);
          int critical;

          err = gnutls_x509_crl_get_extension_info (crl, i,
                                                    oid, &sizeof_oid,
                                                    &critical);
          if (err < 0)
            {
              if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
                break;
              addf (str, "error: get_extension_info: %s\n",
                    gnutls_strerror (err));
              continue;
            }

          if (i == 0)
            adds (str, _("\tExtensions:\n"));

          if (strcmp (oid, "2.5.29.20") == 0)
            {
              char nr[128];
              size_t nr_size = sizeof (nr);

              if (crl_nr)
                {
                  addf (str, "error: more than one CRL number\n");
                  continue;
                }

              err = gnutls_x509_crl_get_number (crl, nr, &nr_size, &critical);

              addf (str, _("\t\tCRL Number (%s): "),
                    critical ? _("critical") : _("not critical"));

              if (err < 0)
                addf (str, "error: get_number: %s\n", gnutls_strerror (err));
              else
                {
                  hexprint (str, nr, nr_size);
                  addf (str, "\n");
                }

              crl_nr++;
            }
          else if (strcmp (oid, "2.5.29.35") == 0)
            {
              cert_type_t ccert;

              if (aki_idx)
                {
                  addf (str, "error: more than one AKI extension\n");
                  continue;
                }

              addf (str, _("\t\tAuthority Key Identifier (%s):\n"),
                    critical ? _("critical") : _("not critical"));

              ccert.crl = crl;
              print_aki (str, TYPE_CRL, ccert);

              aki_idx++;
            }
          else
            {
              char *buffer;
              size_t extlen = 0;

              addf (str, _("\t\tUnknown extension %s (%s):\n"), oid,
                    critical ? _("critical") : _("not critical"));

              err = gnutls_x509_crl_get_extension_data (crl, i,
                                                        NULL, &extlen);
              if (err < 0)
                {
                  addf (str, "error: get_extension_data: %s\n",
                        gnutls_strerror (err));
                  continue;
                }

              buffer = gnutls_malloc (extlen);
              if (!buffer)
                {
                  addf (str, "error: malloc: %s\n",
                        gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
                  continue;
                }

              err = gnutls_x509_crl_get_extension_data (crl, i,
                                                        buffer, &extlen);
              if (err < 0)
                {
                  gnutls_free (buffer);
                  addf (str, "error: get_extension_data2: %s\n",
                        gnutls_strerror (err));
                  continue;
                }

              adds (str, _("\t\t\tASCII: "));
              asciiprint (str, buffer, extlen);
              adds (str, "\n");

              adds (str, _("\t\t\tHexdump: "));
              hexprint (str, buffer, extlen);
              adds (str, "\n");

              gnutls_free (buffer);
            }
        }
    }


  /* Revoked certificates. */
  {
    int num = gnutls_x509_crl_get_crt_count (crl);
    int j;

    if (num)
      addf (str, _("\tRevoked certificates (%d):\n"), num);
    else
      adds (str, _("\tNo revoked certificates.\n"));

    for (j = 0; j < num; j++)
      {
        char serial[128];
        size_t serial_size = sizeof (serial);
        int err;
        time_t tim;

        err = gnutls_x509_crl_get_crt_serial (crl, j, serial,
                                              &serial_size, &tim);
        if (err < 0)
          addf (str, "error: get_crt_serial: %s\n", gnutls_strerror (err));
        else
          {
            char s[42];
            size_t max = sizeof (s);
            struct tm t;

            adds (str, _("\t\tSerial Number (hex): "));
            hexprint (str, serial, serial_size);
            adds (str, "\n");

            if (gmtime_r (&tim, &t) == NULL)
              addf (str, "error: gmtime_r (%ld)\n", (unsigned long) tim);
            else if (strftime (s, max, "%a %b %d %H:%M:%S UTC %Y", &t) == 0)
              addf (str, "error: strftime (%ld)\n", (unsigned long) tim);
            else
              addf (str, _("\t\tRevoked at: %s\n"), s);
          }
      }
  }

  /* Signature. */
  if (!notsigned)
    {
      int err;
      size_t size = 0;
      char *buffer = NULL;

      err = gnutls_x509_crl_get_signature_algorithm (crl);
      if (err < 0)
        addf (str, "error: get_signature_algorithm: %s\n",
              gnutls_strerror (err));
      else
        {
          const char *name = gnutls_sign_algorithm_get_name (err);
          if (name == NULL)
            name = _("unknown");
          addf (str, _("\tSignature Algorithm: %s\n"), name);
        }
      if (err == GNUTLS_SIGN_RSA_MD5 || err == GNUTLS_SIGN_RSA_MD2)
        {
          adds (str, _("warning: signed using a broken signature "
                       "algorithm that can be forged.\n"));
        }

      err = gnutls_x509_crl_get_signature (crl, buffer, &size);
      if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
        {
          addf (str, "error: get_signature: %s\n", gnutls_strerror (err));
          return;
        }

      buffer = gnutls_malloc (size);
      if (!buffer)
        {
          addf (str, "error: malloc: %s\n",
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
          return;
        }

      err = gnutls_x509_crl_get_signature (crl, buffer, &size);
      if (err < 0)
        {
          gnutls_free (buffer);
          addf (str, "error: get_signature2: %s\n", gnutls_strerror (err));
          return;
        }

      adds (str, _("\tSignature:\n"));
      hexdump (str, buffer, size, "\t\t");

      gnutls_free (buffer);
    }
}

/**
 * gnutls_x509_crl_print:
 * @crl: The structure to be printed
 * @format: Indicate the format to use
 * @out: Newly allocated datum with zero terminated string.
 *
 * This function will pretty print a X.509 certificate revocation
 * list, suitable for display to a human.
 *
 * The output @out needs to be deallocate using gnutls_free().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_crl_print (gnutls_x509_crl_t crl,
                       gnutls_certificate_print_formats_t format,
                       gnutls_datum_t * out)
{
  gnutls_buffer_st str;

  _gnutls_buffer_init (&str);

  _gnutls_buffer_append_str
    (&str, _("X.509 Certificate Revocation List Information:\n"));

  print_crl (&str, crl, format == GNUTLS_CRT_PRINT_UNSIGNED_FULL);

  _gnutls_buffer_append_data (&str, "\0", 1);
  out->data = str.data;
  out->size = strlen (str.data);

  return 0;
}

static void
print_crq (gnutls_buffer_st * str, gnutls_x509_crq_t cert)
{
  /* Version. */
  {
    int version = gnutls_x509_crq_get_version (cert);
    if (version < 0)
      addf (str, "error: get_version: %s\n", gnutls_strerror (version));
    else
      addf (str, _("\tVersion: %d\n"), version);
  }

  /* Subject */
  {
    char *dn;
    size_t dn_size = 0;
    int err;

    err = gnutls_x509_crq_get_dn (cert, NULL, &dn_size);
    if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
      addf (str, "error: get_dn: %s\n", gnutls_strerror (err));
    else
      {
        dn = gnutls_malloc (dn_size);
        if (!dn)
          addf (str, "error: malloc (%d): %s\n", (int) dn_size,
                gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
        else
          {
            err = gnutls_x509_crq_get_dn (cert, dn, &dn_size);
            if (err < 0)
              addf (str, "error: get_dn: %s\n", gnutls_strerror (err));
            else
              addf (str, _("\tSubject: %s\n"), dn);
            gnutls_free (dn);
          }
      }
  }

  /* SubjectPublicKeyInfo. */
  {
    int err;
    unsigned int bits;

    err = gnutls_x509_crq_get_pk_algorithm (cert, &bits);
    if (err < 0)
      addf (str, "error: get_pk_algorithm: %s\n", gnutls_strerror (err));
    else
      {
        const char *name = gnutls_pk_algorithm_get_name (err);
        if (name == NULL)
          name = _("unknown");

        addf (str, _("\tSubject Public Key Algorithm: %s\n"), name);
        switch (err)
          {
          case GNUTLS_PK_RSA:
            {
              gnutls_datum_t m, e;

              err = gnutls_x509_crq_get_key_rsa_raw (cert, &m, &e);
              if (err < 0)
                addf (str, "error: get_pk_rsa_raw: %s\n",
                      gnutls_strerror (err));
              else
                {
                  addf (str, _("\t\tModulus (bits %d):\n"), bits);
                  hexdump (str, m.data, m.size, "\t\t\t");
                  adds (str, _("\t\tExponent:\n"));
                  hexdump (str, e.data, e.size, "\t\t\t");

                  gnutls_free (m.data);
                  gnutls_free (e.data);
                }

            }
            break;
#if 0                           /* not implemented yet */
          case GNUTLS_PK_DSA:
            {
              gnutls_datum_t p, q, g, y;

              err = gnutls_x509_crq_get_key_dsa_raw (cert, &p, &q, &g, &y);
              if (err < 0)
                addf (str, "error: get_pk_dsa_raw: %s\n",
                      gnutls_strerror (err));
              else
                {
                  addf (str, _("\t\tPublic key (bits %d):\n"), bits);
                  hexdump (str, y.data, y.size, "\t\t\t");
                  addf (str, _("\t\tP:\n"));
                  hexdump (str, p.data, p.size, "\t\t\t");
                  addf (str, _("\t\tQ:\n"));
                  hexdump (str, q.data, q.size, "\t\t\t");
                  addf (str, _("\t\tG:\n"));
                  hexdump (str, g.data, g.size, "\t\t\t");

                  gnutls_free (p.data);
                  gnutls_free (q.data);
                  gnutls_free (g.data);
                  gnutls_free (y.data);

                }
            }
            break;
#endif
          default:
            break;
          }
      }
  }

  /* parse attributes */
  {
    size_t i;
    int err = 0;
    int extensions = 0;
    int challenge = 0;

    for (i = 0;; i++)
      {
        char oid[MAX_OID_SIZE] = "";
        size_t sizeof_oid = sizeof (oid);

        err = gnutls_x509_crq_get_attribute_info (cert, i, oid, &sizeof_oid);
        if (err < 0)
          {
            if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
              break;
            addf (str, "error: get_extension_info: %s\n",
                  gnutls_strerror (err));
            continue;
          }

        if (i == 0)
          adds (str, _("\tAttributes:\n"));

        if (strcmp (oid, "1.2.840.113549.1.9.14") == 0)
          {
            cert_type_t ccert;

            if (extensions)
              {
                addf (str, "error: more than one extensionsRequest\n");
                continue;
              }

            ccert.crq = cert;
            print_extensions (str, "\t", TYPE_CRQ, ccert);

            extensions++;
          }
        else if (strcmp (oid, "1.2.840.113549.1.9.7") == 0)
          {
            char *pass;
            size_t size;

            if (challenge)
              {
                adds (str,
                      "error: more than one Challenge password attribute\n");
                continue;
              }

            err = gnutls_x509_crq_get_challenge_password (cert, NULL, &size);
            if (err < 0)
              {
                addf (str, "error: get_challenge_password: %s\n",
                      gnutls_strerror (err));
                continue;
              }

            size++;

            pass = gnutls_malloc (size);
            if (!pass)
              {
                addf (str, "error: malloc: %s\n",
                      gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
                continue;
              }

            err = gnutls_x509_crq_get_challenge_password (cert, pass, &size);
            if (err < 0)
              addf (str, "error: get_challenge_password: %s\n",
                    gnutls_strerror (err));
            else
              addf (str, _("\t\tChallenge password: %s\n"), pass);

            gnutls_free (pass);

            challenge++;
          }
        else
          {
            char *buffer;
            size_t extlen = 0;

            addf (str, _("\t\tUnknown attribute %s:\n"), oid);

            err = gnutls_x509_crq_get_attribute_data (cert, i, NULL, &extlen);
            if (err < 0)
              {
                addf (str, "error: get_attribute_data: %s\n",
                      gnutls_strerror (err));
                continue;
              }

            buffer = gnutls_malloc (extlen);
            if (!buffer)
              {
                addf (str, "error: malloc: %s\n",
                      gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
                continue;
              }

            err = gnutls_x509_crq_get_attribute_data (cert, i,
                                                      buffer, &extlen);
            if (err < 0)
              {
                gnutls_free (buffer);
                addf (str, "error: get_attribute_data2: %s\n",
                      gnutls_strerror (err));
                continue;
              }

            adds (str, _("\t\t\tASCII: "));
            asciiprint (str, buffer, extlen);
            adds (str, "\n");

            adds (str, _("\t\t\tHexdump: "));
            hexprint (str, buffer, extlen);
            adds (str, "\n");

            gnutls_free (buffer);
          }
      }
  }
}

static void
print_crq_other (gnutls_buffer_st * str, gnutls_x509_crq_t crq)
{
  int err;
  size_t size = 0;
  char *buffer = NULL;

  err = gnutls_x509_crq_get_key_id (crq, 0, buffer, &size);
  if (err != GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      addf (str, "error: get_key_id: %s\n", gnutls_strerror (err));
      return;
    }

  buffer = gnutls_malloc (size);
  if (!buffer)
    {
      addf (str, "error: malloc: %s\n",
            gnutls_strerror (GNUTLS_E_MEMORY_ERROR));
      return;
    }

  err = gnutls_x509_crq_get_key_id (crq, 0, buffer, &size);
  if (err < 0)
    {
      gnutls_free (buffer);
      addf (str, "error: get_key_id2: %s\n", gnutls_strerror (err));
      return;
    }

  adds (str, _("\tPublic Key Id:\n\t\t"));
  hexprint (str, buffer, size);
  adds (str, "\n");

  gnutls_free (buffer);
}

/**
 * gnutls_x509_crq_print:
 * @crq: The structure to be printed
 * @format: Indicate the format to use
 * @out: Newly allocated datum with zero terminated string.
 *
 * This function will pretty print a certificate request, suitable for
 * display to a human.
 *
 * The output @out needs to be deallocate using gnutls_free().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Since: 2.8.0
 **/
int
gnutls_x509_crq_print (gnutls_x509_crq_t crq,
                       gnutls_certificate_print_formats_t format,
                       gnutls_datum_t * out)
{
  gnutls_buffer_st str;

  _gnutls_buffer_init (&str);

  _gnutls_buffer_append_str
    (&str, _("PKCS #10 Certificate Request Information:\n"));

  print_crq (&str, crq);

  _gnutls_buffer_append_str (&str, _("Other Information:\n"));

  print_crq_other (&str, crq);

  _gnutls_buffer_append_data (&str, "\0", 1);
  out->data = str.data;
  out->size = strlen (str.data);

  return 0;
}

#endif /* ENABLE_PKI */
