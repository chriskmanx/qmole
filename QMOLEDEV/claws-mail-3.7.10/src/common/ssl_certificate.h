/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Colin Leroy <colin@colino.net> 
 * and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __SSL_CERTIFICATE_H__
#define __SSL_CERTIFICATE_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef USE_GNUTLS
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>

#include <glib.h>

#define SSLCERT_ASK_HOOKLIST "sslcert_ask"
#define SSLCERT_GET_CLIENT_CERT_HOOKLIST "sslcert_get_client_cert"
#define SSL_CERT_GET_PASSWORD "sslcert_get_password"

typedef struct _SSLCertificate SSLCertificate;

struct _SSLCertificate
{
	gnutls_x509_crt x509_cert;
	gchar *host;
	gushort port;
	gchar *fingerprint;
	guint status;
};

typedef struct _SSLCertHookData SSLCertHookData;

struct _SSLCertHookData
{
	SSLCertificate *cert;
	SSLCertificate *old_cert;
	gboolean expired;
	gboolean accept;
};

SSLCertificate *ssl_certificate_find (gchar *host, gushort port, const gchar *fingerprint);
SSLCertificate *ssl_certificate_find_lookup (gchar *host, gushort port, const gchar *fingerprint, gboolean lookup);
gboolean ssl_certificate_check (gnutls_x509_crt x509_cert, guint status, gchar *fqdn, gchar *host, gushort port);
void ssl_certificate_destroy(SSLCertificate *cert);
void ssl_certificate_delete_from_disk(SSLCertificate *cert);
char * readable_fingerprint(unsigned char *src, int len);
char *ssl_certificate_check_signer (gnutls_x509_crt cert, guint status);

#ifdef USE_GNUTLS
gnutls_x509_crt ssl_certificate_get_x509_from_pem_file(const gchar *file);
gnutls_x509_privkey ssl_certificate_get_pkey_from_pem_file(const gchar *file);
void ssl_certificate_get_x509_and_pkey_from_p12_file(const gchar *file, 
			const gchar *password, gnutls_x509_crt *crt, gnutls_x509_privkey *key);
size_t gnutls_i2d_X509(gnutls_x509_crt x509_cert, unsigned char **output);
size_t gnutls_i2d_PrivateKey(gnutls_x509_privkey pkey, unsigned char **output);
#endif
#endif /* USE_GNUTLS */
#endif /* SSL_CERTIFICATE_H */
