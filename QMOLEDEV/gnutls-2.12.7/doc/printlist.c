/*
 * Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>
#include <gnutls/openpgp.h>

int
main (void)
{
  {
    size_t i;
    const char *name;
    char id[2];
    gnutls_kx_algorithm_t kx;
    gnutls_cipher_algorithm_t cipher;
    gnutls_mac_algorithm_t mac;
    gnutls_protocol_t version;

    printf ("Available cipher suites:\n");
    printf ("@multitable @columnfractions .60 .20 .20\n");
    for (i = 0; (name = gnutls_cipher_suite_info
                 (i, id, &kx, &cipher, &mac, &version)); i++)
      {
        printf ("@item %s\n@tab 0x%02x 0x%02x\n@tab %s\n",
                name,
                (unsigned char) id[0], (unsigned char) id[1],
                gnutls_protocol_get_name (version));
      }
    printf ("@end multitable\n");
  }

  {
    const gnutls_certificate_type_t *p = gnutls_certificate_type_list ();

    printf ("\n\nAvailable certificate types:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_certificate_type_get_name (*p));
      }
    printf ("@end itemize\n");
  }

  {
    const gnutls_protocol_t *p = gnutls_protocol_list ();

    printf ("\nAvailable protocols:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_protocol_get_name (*p));
      }
    printf ("@end itemize\n");
  }

  {
    const gnutls_cipher_algorithm_t *p = gnutls_cipher_list ();

    printf ("\nAvailable ciphers:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_cipher_get_name (*p));
      }
    printf ("@end itemize\n");
  }

  {
    const gnutls_mac_algorithm_t *p = gnutls_mac_list ();

    printf ("\nAvailable MAC algorithms:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_mac_get_name (*p));
      }
    printf ("@end itemize\n");
  }

  {
    const gnutls_kx_algorithm_t *p = gnutls_kx_list ();

    printf ("\nAvailable key exchange methods:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_kx_get_name (*p));
      }
    printf ("@end itemize\n");
  }

  {
    const gnutls_pk_algorithm_t *p = gnutls_pk_list ();

    printf ("\nAvailable public key algorithms:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_pk_get_name (*p));
      }
    printf ("@end itemize\n");
  }

  {
    const gnutls_sign_algorithm_t *p = gnutls_sign_list ();

    printf ("\nAvailable public key signature algorithms:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_sign_get_name (*p));
      }
    printf ("@end itemize\n");
  }

  {
    const gnutls_compression_method_t *p = gnutls_compression_list ();

    printf ("\nAvailable compression methods:\n@itemize\n");
    for (; *p; p++)
      {
        printf ("@item %s\n", gnutls_compression_get_name (*p));
      }
    printf ("@end itemize\n");
  }
}
