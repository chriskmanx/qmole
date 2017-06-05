/*
 * Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free
 * Software Foundation, Inc.
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 * Written by Nikos Mavrogiannopoulos <nmav@gnutls.org>.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <certtool-cfg.h>
#include <cfg+.h>
#include <gnutls/x509.h>
#include <string.h>
#include <limits.h>
#include <inttypes.h>
#include <time.h>

/* for inet_pton */
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>

/* Gnulib portability files. */
#include <getpass.h>
#include "readline.h"
#include "certtool-common.h"

extern int batch;

typedef struct _cfg_ctx
{
  char *organization;
  char *unit;
  char *locality;
  char *state;
  char *cn;
  char *uid;
  char *challenge_password;
  char *pkcs9_email;
  char *country;
  char **dns_name;
  char **ip_addr;
  char **email;
  char **dn_oid;
  char *crl_dist_points;
  char *password;
  char *pkcs12_key_name;
  int serial;
  int expiration_days;
  int ca;
  int path_len;
  int tls_www_client;
  int tls_www_server;
  int signing_key;
  int encryption_key;
  int cert_sign_key;
  int crl_sign_key;
  int code_sign_key;
  int ocsp_sign_key;
  int time_stamping_key;
  int ipsec_ike_key;
  char **key_purpose_oids;
  int crl_next_update;
  int crl_number;
  int crq_extensions;
  char *proxy_policy_language;
} cfg_ctx;

cfg_ctx cfg;

void
cfg_init (void)
{
  memset (&cfg, 0, sizeof (cfg));
  cfg.path_len = -1;
  cfg.serial = -1;
}

int
template_parse (const char *template)
{
  /* libcfg+ parsing context */
  CFG_CONTEXT con;

  /* Parsing return code */
  register int ret;

  /* Option variables */

  /* Option set */
  struct cfg_option options[] = {
    {NULL, '\0', "organization", CFG_STR, (void *) &cfg.organization,
     0},
    {NULL, '\0', "unit", CFG_STR, (void *) &cfg.unit, 0},
    {NULL, '\0', "locality", CFG_STR, (void *) &cfg.locality, 0},
    {NULL, '\0', "state", CFG_STR, (void *) &cfg.state, 0},
    {NULL, '\0', "cn", CFG_STR, (void *) &cfg.cn, 0},
    {NULL, '\0', "uid", CFG_STR, (void *) &cfg.uid, 0},
    {NULL, '\0', "challenge_password", CFG_STR,
     (void *) &cfg.challenge_password, 0},
    {NULL, '\0', "password", CFG_STR, (void *) &cfg.password, 0},
    {NULL, '\0', "pkcs9_email", CFG_STR, (void *) &cfg.pkcs9_email, 0},
    {NULL, '\0', "country", CFG_STR, (void *) &cfg.country, 0},
    {NULL, '\0', "dns_name", CFG_STR | CFG_MULTI_ARRAY,
     (void *) &cfg.dns_name, 0},
    {NULL, '\0', "ip_address", CFG_STR | CFG_MULTI_ARRAY,
     (void *) &cfg.ip_addr, 0},
    {NULL, '\0', "email", CFG_STR | CFG_MULTI_ARRAY, (void *) &cfg.email, 0},

    {NULL, '\0', "dn_oid", CFG_STR + CFG_MULTI_SEPARATED,
     (void *) &cfg.dn_oid, 0},
    {NULL, '\0', "key_purpose_oids", CFG_STR + CFG_MULTI_SEPARATED,
     (void *) &cfg.key_purpose_oids, 0},

    {NULL, '\0', "crl_dist_points", CFG_STR,
     (void *) &cfg.crl_dist_points, 0},
    {NULL, '\0', "pkcs12_key_name", CFG_STR,
     (void *) &cfg.pkcs12_key_name, 0},

    {NULL, '\0', "serial", CFG_INT, (void *) &cfg.serial, 0},
    {NULL, '\0', "expiration_days", CFG_INT,
     (void *) &cfg.expiration_days, 0},

    {NULL, '\0', "crl_next_update", CFG_INT,
     (void *) &cfg.crl_next_update, 0},

    {NULL, '\0', "crl_number", CFG_INT,
     (void *) &cfg.crl_number, 0},

    {NULL, '\0', "ca", CFG_BOOL, (void *) &cfg.ca, 0},
    {NULL, '\0', "honor_crq_extensions", CFG_BOOL,
     (void *) &cfg.crq_extensions, 0},
    {NULL, '\0', "path_len", CFG_INT, (void *) &cfg.path_len, 0},
    {NULL, '\0', "tls_www_client", CFG_BOOL,
     (void *) &cfg.tls_www_client, 0},
    {NULL, '\0', "tls_www_server", CFG_BOOL,
     (void *) &cfg.tls_www_server, 0},
    {NULL, '\0', "signing_key", CFG_BOOL, (void *) &cfg.signing_key,
     0},
    {NULL, '\0', "encryption_key", CFG_BOOL,
     (void *) &cfg.encryption_key, 0},
    {NULL, '\0', "cert_signing_key", CFG_BOOL,
     (void *) &cfg.cert_sign_key, 0},
    {NULL, '\0', "crl_signing_key", CFG_BOOL,
     (void *) &cfg.crl_sign_key, 0},
    {NULL, '\0', "code_signing_key", CFG_BOOL,
     (void *) &cfg.code_sign_key, 0},
    {NULL, '\0', "ocsp_signing_key", CFG_BOOL,
     (void *) &cfg.ocsp_sign_key, 0},
    {NULL, '\0', "time_stamping_key", CFG_BOOL,
     (void *) &cfg.time_stamping_key, 0},
    {NULL, '\0', "ipsec_ike_key", CFG_BOOL,
     (void *) &cfg.ipsec_ike_key, 0},
    {NULL, '\0', "proxy_policy_language", CFG_STR,
     (void *) &cfg.proxy_policy_language, 0},
    CFG_END_OF_LIST
  };

  /* Creating context */
  con = cfg_get_context (options);
  if (con == NULL)
    {
      puts ("Not enough memory");
      exit (1);
    }

  cfg_set_cfgfile_context (con, 0, -1, (char *) template);

  /* Parsing command line */
  ret = cfg_parse (con);

  if (ret != CFG_OK)
    {
      printf ("error parsing command line: %s: ", template);
      cfg_fprint_error (con, stdout);
      putchar ('\n');
      exit (ret < 0 ? -ret : ret);
    }

  return 0;
}

void
read_crt_set (gnutls_x509_crt_t crt, const char *input_str, const char *oid)
{
  char input[128];
  int ret;

  fputs (input_str, stderr);
  if (fgets (input, sizeof (input), stdin) == NULL)
    return;

  if (strlen (input) == 1)      /* only newline */
    return;

  ret =
    gnutls_x509_crt_set_dn_by_oid (crt, oid, 0, input, strlen (input) - 1);
  if (ret < 0)
    {
      fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
      exit (1);
    }
}

void
read_crq_set (gnutls_x509_crq_t crq, const char *input_str, const char *oid)
{
  char input[128];
  int ret;

  fputs (input_str, stderr);
  if (fgets (input, sizeof (input), stdin) == NULL)
    return;

  if (strlen (input) == 1)      /* only newline */
    return;

  ret =
    gnutls_x509_crq_set_dn_by_oid (crq, oid, 0, input, strlen (input) - 1);
  if (ret < 0)
    {
      fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
      exit (1);
    }
}

/* The input_str should contain %d or %u to print the default.
 */
static int
read_int_with_default (const char *input_str, int def)
{
  char *in;
  char *endptr;
  long l;

  fprintf (stderr, input_str, def);
  in = readline ("");
  if (in == NULL)
    {
      return def;
    }

  l = strtol (in, &endptr, 0);

  if (*endptr != '\0')
    {
      fprintf (stderr, "Trailing garbage ignored: `%s'\n", endptr);
      free (in);
      return 0;
    }

  if (l <= INT_MIN || l >= INT_MAX)
    {
      fprintf (stderr, "Integer out of range: `%s'\n", in);
      free (in);
      return 0;
    }

  if (in == endptr)
    l = def;

  free (in);

  return (int) l;
}

int
read_int (const char *input_str)
{
  return read_int_with_default (input_str, 0);
}

const char *
read_str (const char *input_str)
{
  static char input[128];
  int len;

  fputs (input_str, stderr);
  if (fgets (input, sizeof (input), stdin) == NULL)
    return NULL;

  len = strlen (input);
  if ((len > 0) && (input[len - 1] == '\n'))
    input[len - 1] = 0;
  if (input[0] == 0)
    return NULL;

  return input;
}

/* Default is no
 */
int
read_yesno (const char *input_str)
{
  char input[128];

  fputs (input_str, stderr);
  if (fgets (input, sizeof (input), stdin) == NULL)
    return 0;

  if (strlen (input) == 1)      /* only newline */
    return 0;

  if (input[0] == 'y' || input[0] == 'Y')
    return 1;

  return 0;
}


/* Wrapper functions for non-interactive mode.
 */
const char *
get_pass (void)
{
  if (batch)
    return cfg.password;
  else
    return getpass ("Enter password: ");
}

const char *
get_confirmed_pass (bool empty_ok)
{
  if (batch)
    return cfg.password;
  else
    {
      const char *pass = NULL;
      char *copy = NULL;

      do
        {
          if (pass)
            printf ("Password missmatch, try again.\n");

          free (copy);

          pass = getpass ("Enter password: ");
          copy = strdup (pass);
          pass = getpass ("Confirm password: ");
        }
      while (strcmp (pass, copy) != 0 && !(empty_ok && *pass == '\0'));

      free (copy);

      return pass;
    }
}

const char *
get_challenge_pass (void)
{
  if (batch)
    return cfg.challenge_password;
  else
    return getpass ("Enter a challenge password: ");
}

const char *
get_crl_dist_point_url (void)
{
  if (batch)
    return cfg.crl_dist_points;
  else
    return read_str ("Enter the URI of the CRL distribution point: ");
}

void
get_country_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.country)
        return;
      ret =
        gnutls_x509_crt_set_dn_by_oid (crt,
                                       GNUTLS_OID_X520_COUNTRY_NAME, 0,
                                       cfg.country, strlen (cfg.country));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "Country name (2 chars): ",
                    GNUTLS_OID_X520_COUNTRY_NAME);
    }

}

void
get_organization_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.organization)
        return;

      ret =
        gnutls_x509_crt_set_dn_by_oid (crt,
                                       GNUTLS_OID_X520_ORGANIZATION_NAME,
                                       0, cfg.organization,
                                       strlen (cfg.organization));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "Organization name: ",
                    GNUTLS_OID_X520_ORGANIZATION_NAME);
    }

}

void
get_unit_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.unit)
        return;

      ret =
        gnutls_x509_crt_set_dn_by_oid (crt,
                                       GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME,
                                       0, cfg.unit, strlen (cfg.unit));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "Organizational unit name: ",
                    GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME);
    }

}

void
get_state_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.state)
        return;
      ret =
        gnutls_x509_crt_set_dn_by_oid (crt,
                                       GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME,
                                       0, cfg.state, strlen (cfg.state));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "State or province name: ",
                    GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME);
    }

}

void
get_locality_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.locality)
        return;
      ret =
        gnutls_x509_crt_set_dn_by_oid (crt,
                                       GNUTLS_OID_X520_LOCALITY_NAME, 0,
                                       cfg.locality, strlen (cfg.locality));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "Locality name: ", GNUTLS_OID_X520_LOCALITY_NAME);
    }

}

void
get_cn_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.cn)
        return;
      ret =
        gnutls_x509_crt_set_dn_by_oid (crt, GNUTLS_OID_X520_COMMON_NAME,
                                       0, cfg.cn, strlen (cfg.cn));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "Common name: ", GNUTLS_OID_X520_COMMON_NAME);
    }

}

void
get_uid_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.uid)
        return;
      ret = gnutls_x509_crt_set_dn_by_oid (crt, GNUTLS_OID_LDAP_UID, 0,
                                           cfg.uid, strlen (cfg.uid));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "UID: ", GNUTLS_OID_LDAP_UID);
    }

}

void
get_oid_crt_set (gnutls_x509_crt_t crt)
{
  int ret, i;

  if (batch)
    {
      if (!cfg.dn_oid)
        return;
      for (i = 0; cfg.dn_oid[i] != NULL; i += 2)
        {
          if (cfg.dn_oid[i + 1] == NULL)
            {
              fprintf (stderr, "dn_oid: %s does not have an argument.\n",
                       cfg.dn_oid[i]);
              exit (1);
            }
          ret = gnutls_x509_crt_set_dn_by_oid (crt, cfg.dn_oid[i], 0,
                                               cfg.dn_oid[i + 1],
                                               strlen (cfg.dn_oid[i + 1]));

          if (ret < 0)
            {
              fprintf (stderr, "set_dn_oid: %s\n", gnutls_strerror (ret));
              exit (1);
            }
        }
    }
}

void
get_key_purpose_set (gnutls_x509_crt_t crt)
{
  int ret, i;

  if (batch)
    {
      if (!cfg.key_purpose_oids)
        return;
      for (i = 0; cfg.key_purpose_oids[i] != NULL; i++)
        {
          ret =
            gnutls_x509_crt_set_key_purpose_oid (crt, cfg.key_purpose_oids[i],
                                                 0);

          if (ret < 0)
            {
              fprintf (stderr, "set_key_purpose_oid (%s): %s\n",
                       cfg.key_purpose_oids[i], gnutls_strerror (ret));
              exit (1);
            }
        }
    }

}


void
get_pkcs9_email_crt_set (gnutls_x509_crt_t crt)
{
  int ret;

  if (batch)
    {
      if (!cfg.pkcs9_email)
        return;
      ret = gnutls_x509_crt_set_dn_by_oid (crt, GNUTLS_OID_PKCS9_EMAIL, 0,
                                           cfg.pkcs9_email,
                                           strlen (cfg.pkcs9_email));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crt_set (crt, "E-mail: ", GNUTLS_OID_PKCS9_EMAIL);
    }

}

int
get_serial (void)
{
  int default_serial = time (NULL);

  if (batch)
    {
      if (cfg.serial < 0)
        return default_serial;
      return cfg.serial;
    }
  else
    {
      return read_int_with_default
        ("Enter the certificate's serial number in decimal (default: %u): ",
         default_serial);
    }
}

int
get_days (void)
{
  int days;

  if (batch)
    {
      if (cfg.expiration_days <= 0)
        return 365;
      else
        return cfg.expiration_days;
    }
  else
    {
      do
        {
          days = read_int ("The certificate will expire in (days): ");
        }
      while (days == 0);
      return days;
    }
}

int
get_ca_status (void)
{
  if (batch)
    {
      return cfg.ca;
    }
  else
    {
      return
        read_yesno ("Does the certificate belong to an authority? (y/N): ");
    }
}

int
get_crq_extensions_status (void)
{
  if (batch)
    {
      return cfg.crq_extensions;
    }
  else
    {
      return
        read_yesno
        ("Do you want to honour the extensions from the request? (y/N): ");
    }
}

int
get_crl_number (void)
{
  if (batch)
    {
      return cfg.crl_number;
    }
  else
    {
      return read_int_with_default ("CRL Number: ", 1);
    }
}

int
get_path_len (void)
{
  if (batch)
    {
      return cfg.path_len;
    }
  else
    {
      return read_int_with_default
        ("Path length constraint (decimal, %d for no constraint): ", -1);
    }
}

const char *
get_pkcs12_key_name (void)
{
  const char *name;

  if (batch)
    {
      if (!cfg.pkcs12_key_name)
        return "Anonymous";
      return cfg.pkcs12_key_name;
    }
  else
    {
      do
        {
          name = read_str ("Enter a name for the key: ");
        }
      while (name == NULL);
    }
  return name;
}

int
get_tls_client_status (void)
{
  if (batch)
    {
      return cfg.tls_www_client;
    }
  else
    {
      return read_yesno ("Is this a TLS web client certificate? (y/N): ");
    }
}

int
get_tls_server_status (void)
{
  if (batch)
    {
      return cfg.tls_www_server;
    }
  else
    {
      return
        read_yesno ("Is this also a TLS web server certificate? (y/N): ");
    }
}

#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>

/* convert a printable IP to binary */
static int
string_to_ip (unsigned char *ip, const char *str)
{
  int len = strlen (str);
  int ret;

#if HAVE_IPV6
  if (strchr (str, ':') != NULL || len > 16)
    {                           /* IPv6 */
      ret = inet_pton (AF_INET6, str, ip);
      if (ret <= 0)
        {
          fprintf (stderr, "Error in IPv6 address %s\n", str);
          exit (1);
        }

      /* To be done */
      return 16;
    }
  else
#endif
    {                           /* IPv4 */
      ret = inet_pton (AF_INET, str, ip);
      if (ret <= 0)
        {
          fprintf (stderr, "Error in IPv4 address %s\n", str);
          exit (1);
        }

      return 4;
    }

}

void
get_ip_addr_set (int type, void *crt)
{
  int ret = 0, i;
  unsigned char ip[16];
  int len;

  if (batch)
    {
      if (!cfg.ip_addr)
        return;

      for (i = 0; cfg.ip_addr[i] != NULL; i++)
        {
          len = string_to_ip (ip, cfg.ip_addr[i]);
          if (len <= 0)
            {
              fprintf (stderr, "Error parsing address: %s\n", cfg.ip_addr[i]);
              exit (1);
            }

          if (type == TYPE_CRT)
            ret =
              gnutls_x509_crt_set_subject_alt_name (crt, GNUTLS_SAN_IPADDRESS,
                                                    ip, len,
                                                    GNUTLS_FSAN_APPEND);
          else
            ret =
              gnutls_x509_crq_set_subject_alt_name (crt, GNUTLS_SAN_IPADDRESS,
                                                    ip, len,
                                                    GNUTLS_FSAN_APPEND);

          if (ret < 0)
            break;
        }
    }
  else
    {
      const char *p;

      p =
        read_str ("Enter the IP address of the subject of the certificate: ");
      if (!p)
        return;

      len = string_to_ip (ip, p);
      if (len <= 0)
        {
          fprintf (stderr, "Error parsing address: %s\n", p);
          exit (1);
        }

      if (type == TYPE_CRT)
        ret = gnutls_x509_crt_set_subject_alt_name (crt, GNUTLS_SAN_IPADDRESS,
                                                    ip, len,
                                                    GNUTLS_FSAN_APPEND);
      else
        ret = gnutls_x509_crq_set_subject_alt_name (crt, GNUTLS_SAN_IPADDRESS,
                                                    ip, len,
                                                    GNUTLS_FSAN_APPEND);
    }

  if (ret < 0)
    {
      fprintf (stderr, "set_subject_alt_name: %s\n", gnutls_strerror (ret));
      exit (1);
    }
}


void
get_email_set (int type, void *crt)
{
  int ret = 0, i;

  if (batch)
    {
      if (!cfg.email)
        return;

      for (i = 0; cfg.email[i] != NULL; i++)
        {
          if (type == TYPE_CRT)
            ret =
              gnutls_x509_crt_set_subject_alt_name (crt,
                                                    GNUTLS_SAN_RFC822NAME,
                                                    cfg.email[i],
                                                    strlen (cfg.email[i]),
                                                    GNUTLS_FSAN_APPEND);
          else
            ret =
              gnutls_x509_crq_set_subject_alt_name (crt,
                                                    GNUTLS_SAN_RFC822NAME,
                                                    cfg.email[i],
                                                    strlen (cfg.email[i]),
                                                    GNUTLS_FSAN_APPEND);

          if (ret < 0)
            break;
        }
    }
  else
    {
      const char *p;

      p = read_str ("Enter the e-mail of the subject of the certificate: ");
      if (!p)
        return;

      if (type == TYPE_CRT)
        ret =
          gnutls_x509_crt_set_subject_alt_name (crt, GNUTLS_SAN_RFC822NAME, p,
                                                strlen (p),
                                                GNUTLS_FSAN_APPEND);
      else
        ret =
          gnutls_x509_crq_set_subject_alt_name (crt, GNUTLS_SAN_RFC822NAME, p,
                                                strlen (p),
                                                GNUTLS_FSAN_APPEND);
    }

  if (ret < 0)
    {
      fprintf (stderr, "set_subject_alt_name: %s\n", gnutls_strerror (ret));
      exit (1);
    }
}

void
get_dns_name_set (int type, void *crt)
{
  int ret = 0, i;

  if (batch)
    {
      if (!cfg.dns_name)
        return;

      for (i = 0; cfg.dns_name[i] != NULL; i++)
        {
          if (type == TYPE_CRT)
            ret =
              gnutls_x509_crt_set_subject_alt_name (crt, GNUTLS_SAN_DNSNAME,
                                                    cfg.dns_name[i],
                                                    strlen (cfg.dns_name[i]),
                                                    GNUTLS_FSAN_APPEND);
          else
            ret =
              gnutls_x509_crq_set_subject_alt_name (crt, GNUTLS_SAN_DNSNAME,
                                                    cfg.dns_name[i],
                                                    strlen (cfg.dns_name[i]),
                                                    GNUTLS_FSAN_APPEND);

          if (ret < 0)
            break;
        }
    }
  else
    {
      const char *p;

      do
        {
          p =
            read_str ("Enter a dnsName of the subject of the certificate: ");
          if (!p)
            return;

          if (type == TYPE_CRT)
            ret = gnutls_x509_crt_set_subject_alt_name
              (crt, GNUTLS_SAN_DNSNAME, p, strlen (p), GNUTLS_FSAN_APPEND);
          else
            ret = gnutls_x509_crq_set_subject_alt_name
              (crt, GNUTLS_SAN_DNSNAME, p, strlen (p), GNUTLS_FSAN_APPEND);
        }
      while (p);
    }

  if (ret < 0)
    {
      fprintf (stderr, "set_subject_alt_name: %s\n", gnutls_strerror (ret));
      exit (1);
    }
}


int
get_sign_status (int server)
{
  const char *msg;

  if (batch)
    {
      return cfg.signing_key;
    }
  else
    {
      if (server)
        msg =
          "Will the certificate be used for signing (DHE and RSA-EXPORT ciphersuites)? (y/N): ";
      else
        msg =
          "Will the certificate be used for signing (required for TLS)? (y/N): ";
      return read_yesno (msg);
    }
}

int
get_encrypt_status (int server)
{
  const char *msg;

  if (batch)
    {
      return cfg.encryption_key;
    }
  else
    {
      if (server)
        msg =
          "Will the certificate be used for encryption (RSA ciphersuites)? (y/N): ";
      else
        msg =
          "Will the certificate be used for encryption (not required for TLS)? (y/N): ";
      return read_yesno (msg);
    }
}

int
get_cert_sign_status (void)
{
  if (batch)
    {
      return cfg.cert_sign_key;
    }
  else
    {
      return
        read_yesno
        ("Will the certificate be used to sign other certificates? (y/N): ");
    }
}

int
get_crl_sign_status (void)
{
  if (batch)
    {
      return cfg.crl_sign_key;
    }
  else
    {
      return
        read_yesno ("Will the certificate be used to sign CRLs? (y/N): ");
    }
}

int
get_code_sign_status (void)
{
  if (batch)
    {
      return cfg.code_sign_key;
    }
  else
    {
      return
        read_yesno ("Will the certificate be used to sign code? (y/N): ");
    }
}

int
get_ocsp_sign_status (void)
{
  if (batch)
    {
      return cfg.ocsp_sign_key;
    }
  else
    {
      return
        read_yesno
        ("Will the certificate be used to sign OCSP requests? (y/N): ");
    }
}

int
get_time_stamp_status (void)
{
  if (batch)
    {
      return cfg.time_stamping_key;
    }
  else
    {
      return
        read_yesno
        ("Will the certificate be used for time stamping? (y/N): ");
    }
}

int
get_ipsec_ike_status (void)
{
  if (batch)
    {
      return cfg.ipsec_ike_key;
    }
  else
    {
      return
        read_yesno
        ("Will the certificate be used for IPsec IKE operations? (y/N): ");
    }
}

int
get_crl_next_update (void)
{
  int days;

  if (batch)
    {
      if (cfg.crl_next_update <= 0)
        return 365;
      else
        return cfg.crl_next_update;
    }
  else
    {
      do
        {
          days = read_int ("The next CRL will be issued in (days): ");
        }
      while (days == 0);
      return days;
    }
}

const char *
get_proxy_policy (char **policy, size_t * policylen)
{
  const char *ret;

  if (batch)
    {
      ret = cfg.proxy_policy_language;
      if (!ret)
        ret = "1.3.6.1.5.5.7.21.1";
    }
  else
    {
      do
        {
          ret = read_str ("Enter the OID of the proxy policy language: ");
        }
      while (ret == NULL);
    }

  *policy = NULL;
  *policylen = 0;

  if (strcmp (ret, "1.3.6.1.5.5.7.21.1") != 0 &&
      strcmp (ret, "1.3.6.1.5.5.7.21.2") != 0)
    {
      fprintf (stderr, "Reading non-standard proxy policy not supported.\n");
    }

  return ret;
}

/* CRQ stuff.
 */
void
get_country_crq_set (gnutls_x509_crq_t crq)
{
  int ret;

  if (batch)
    {
      if (!cfg.country)
        return;
      ret =
        gnutls_x509_crq_set_dn_by_oid (crq,
                                       GNUTLS_OID_X520_COUNTRY_NAME, 0,
                                       cfg.country, strlen (cfg.country));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crq_set (crq, "Country name (2 chars): ",
                    GNUTLS_OID_X520_COUNTRY_NAME);
    }

}

void
get_organization_crq_set (gnutls_x509_crq_t crq)
{
  int ret;

  if (batch)
    {
      if (!cfg.organization)
        return;

      ret =
        gnutls_x509_crq_set_dn_by_oid (crq,
                                       GNUTLS_OID_X520_ORGANIZATION_NAME,
                                       0, cfg.organization,
                                       strlen (cfg.organization));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crq_set (crq, "Organization name: ",
                    GNUTLS_OID_X520_ORGANIZATION_NAME);
    }

}

void
get_unit_crq_set (gnutls_x509_crq_t crq)
{
  int ret;

  if (batch)
    {
      if (!cfg.unit)
        return;

      ret =
        gnutls_x509_crq_set_dn_by_oid (crq,
                                       GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME,
                                       0, cfg.unit, strlen (cfg.unit));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crq_set (crq, "Organizational unit name: ",
                    GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME);
    }

}

void
get_state_crq_set (gnutls_x509_crq_t crq)
{
  int ret;

  if (batch)
    {
      if (!cfg.state)
        return;
      ret =
        gnutls_x509_crq_set_dn_by_oid (crq,
                                       GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME,
                                       0, cfg.state, strlen (cfg.state));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crq_set (crq, "State or province name: ",
                    GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME);
    }

}

void
get_locality_crq_set (gnutls_x509_crq_t crq)
{
  int ret;

  if (batch)
    {
      if (!cfg.locality)
        return;
      ret =
        gnutls_x509_crq_set_dn_by_oid (crq,
                                       GNUTLS_OID_X520_LOCALITY_NAME, 0,
                                       cfg.locality, strlen (cfg.locality));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crq_set (crq, "Locality name: ", GNUTLS_OID_X520_LOCALITY_NAME);
    }

}

void
get_cn_crq_set (gnutls_x509_crq_t crq)
{
  int ret;

  if (batch)
    {
      if (!cfg.cn)
        return;
      ret =
        gnutls_x509_crq_set_dn_by_oid (crq, GNUTLS_OID_X520_COMMON_NAME,
                                       0, cfg.cn, strlen (cfg.cn));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crq_set (crq, "Common name: ", GNUTLS_OID_X520_COMMON_NAME);
    }

}

void
get_uid_crq_set (gnutls_x509_crq_t crq)
{
  int ret;

  if (batch)
    {
      if (!cfg.uid)
        return;
      ret = gnutls_x509_crq_set_dn_by_oid (crq, GNUTLS_OID_LDAP_UID, 0,
                                           cfg.uid, strlen (cfg.uid));
      if (ret < 0)
        {
          fprintf (stderr, "set_dn: %s\n", gnutls_strerror (ret));
          exit (1);
        }
    }
  else
    {
      read_crq_set (crq, "UID: ", GNUTLS_OID_LDAP_UID);
    }

}

void
get_oid_crq_set (gnutls_x509_crq_t crq)
{
  int ret, i;

  if (batch)
    {
      if (!cfg.dn_oid)
        return;
      for (i = 0; cfg.dn_oid[i] != NULL; i += 2)
        {
          if (cfg.dn_oid[i + 1] == NULL)
            {
              fprintf (stderr, "dn_oid: %s does not have an argument.\n",
                       cfg.dn_oid[i]);
              exit (1);
            }
          ret = gnutls_x509_crq_set_dn_by_oid (crq, cfg.dn_oid[i], 0,
                                               cfg.dn_oid[i + 1],
                                               strlen (cfg.dn_oid[i + 1]));

          if (ret < 0)
            {
              fprintf (stderr, "set_dn_oid: %s\n", gnutls_strerror (ret));
              exit (1);
            }
        }
    }

}
