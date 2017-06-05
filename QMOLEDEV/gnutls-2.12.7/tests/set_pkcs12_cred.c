/*
 * Copyright (C) 2005, 2006, 2008, 2010 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>

#include "utils.h"

void
doit (void)
{
  gnutls_certificate_credentials_t x509cred;
  const char *file, *password;
  int ret;

  ret = gnutls_global_init ();
  if (ret < 0)
    fail ("gnutls_global_init failed %d\n", ret);

  ret = gnutls_certificate_allocate_credentials (&x509cred);
  if (ret < 0)
    fail ("gnutls_certificate_allocate_credentials failed %d\n", ret);

  file = getenv ("PKCS12FILE");
  password = getenv ("PKCS12PASSWORD");

  if (!file)
    file = "pkcs12-decode/client.p12";
  if (!password)
    password = "foobar";

  if (debug)
    success ("Reading PKCS#12 blob from `%s' using password `%s'.\n",
             file, password);
  ret = gnutls_certificate_set_x509_simple_pkcs12_file (x509cred,
                                                        file,
                                                        GNUTLS_X509_FMT_DER,
                                                        password);
  if (ret < 0)
    fail ("x509_pkcs12 failed %d: %s\n", ret, gnutls_strerror (ret));

  if (debug)
    success ("Read file OK\n");

  gnutls_certificate_free_credentials (x509cred);

  /* try now if we can read correctly from a pkcs12 file that
   * contains two certificates (one unrelated with key)
   */
  ret = gnutls_certificate_allocate_credentials (&x509cred);
  if (ret < 0)
    fail ("gnutls_certificate_allocate_credentials failed %d\n", ret);

  file = getenv ("PKCS12FILE_2");
  password = getenv ("PKCS12PASSWORD_2");

  if (!file)
    file = "pkcs12-decode/pkcs12_2certs.p12";
  if (!password)
    password = "";

  if (debug)
    success ("Reading PKCS#12 blob from `%s' using password `%s'.\n",
             file, password);
  ret = gnutls_certificate_set_x509_simple_pkcs12_file (x509cred,
                                                        file,
                                                        GNUTLS_X509_FMT_DER,
                                                        password);
  if (ret < 0)
    fail ("x509_pkcs12 failed %d: %s\n", ret, gnutls_strerror (ret));

  if (debug)
    success ("Read file OK\n");

  gnutls_certificate_free_credentials (x509cred);

  gnutls_global_deinit ();
}
