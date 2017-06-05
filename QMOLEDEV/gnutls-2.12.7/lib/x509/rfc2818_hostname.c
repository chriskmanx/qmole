/*
 * Copyright (C) 2003, 2004, 2005, 2007, 2008, 2010 Free Software
 * Foundation, Inc.
 * Copyright (C) 2002 Andrew McDonald
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

#include <gnutls_int.h>
#include <gnutls_str.h>
#include <x509_int.h>
#include <common.h>
#include <gnutls_errors.h>

/**
 * gnutls_x509_crt_check_hostname:
 * @cert: should contain an gnutls_x509_crt_t structure
 * @hostname: A null terminated string that contains a DNS name
 *
 * This function will check if the given certificate's subject matches
 * the given hostname.  This is a basic implementation of the matching
 * described in RFC2818 (HTTPS), which takes into account wildcards,
 * and the DNSName/IPAddress subject alternative name PKIX extension.
 *
 * Returns: non zero for a successful match, and zero on failure.
 **/
int
gnutls_x509_crt_check_hostname (gnutls_x509_crt_t cert, const char *hostname)
{

  char dnsname[MAX_CN];
  size_t dnsnamesize;
  int found_dnsname = 0;
  int ret = 0;
  int i = 0;

  /* try matching against:
   *  1) a DNS name as an alternative name (subjectAltName) extension
   *     in the certificate
   *  2) the common name (CN) in the certificate
   *
   *  either of these may be of the form: *.domain.tld
   *
   *  only try (2) if there is no subjectAltName extension of
   *  type dNSName
   */

  /* Check through all included subjectAltName extensions, comparing
   * against all those of type dNSName.
   */
  for (i = 0; !(ret < 0); i++)
    {

      dnsnamesize = sizeof (dnsname);
      ret = gnutls_x509_crt_get_subject_alt_name (cert, i,
                                                  dnsname, &dnsnamesize,
                                                  NULL);

      if (ret == GNUTLS_SAN_DNSNAME)
        {
          found_dnsname = 1;
          if (_gnutls_hostname_compare (dnsname, dnsnamesize, hostname, 0))
            {
              return 1;
            }
        }
    }

  if (!found_dnsname)
    {
      /* not got the necessary extension, use CN instead
       */
      dnsnamesize = sizeof (dnsname);
      if (gnutls_x509_crt_get_dn_by_oid (cert, OID_X520_COMMON_NAME, 0,
                                         0, dnsname, &dnsnamesize) < 0)
        {
          /* got an error, can't find a name
           */
          return 0;
        }

      if (_gnutls_hostname_compare (dnsname, dnsnamesize, hostname, 0))
        {
          return 1;
        }
    }

  /* not found a matching name
   */
  return 0;
}
