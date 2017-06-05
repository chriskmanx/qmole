/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-ssl.c: temporary ssl integration
 *
 * Copyright (C) 2010 Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gio/gio.h>

#include "soup-ssl.h"
#include "soup-misc.h"

const gboolean soup_ssl_supported = TRUE;

struct SoupSSLCredentials {
	GList *ca_list;
	GTlsCertificateFlags validation_flags;
	GTlsCertificate *certificate;
};

SoupSSLCredentials *
soup_ssl_get_client_credentials (const char *ca_file)
{
	SoupSSLCredentials *creds;

	creds = g_slice_new0 (SoupSSLCredentials);

	if (ca_file) {
		GError *error = NULL;

		creds->ca_list = g_tls_certificate_list_new_from_file (ca_file, &error);
		if (error) {
			if (!g_error_matches (error, G_TLS_ERROR, G_TLS_ERROR_UNAVAILABLE)) {
				g_warning ("Could not set SSL credentials from '%s': %s",
					   ca_file, error->message);
			}
			g_error_free (error);
		}
		creds->validation_flags = G_TLS_CERTIFICATE_VALIDATE_ALL;
	}

	return creds;
}

gboolean
soup_ssl_credentials_verify_certificate (SoupSSLCredentials   *creds,
					 GTlsCertificate      *cert,
					 GTlsCertificateFlags  errors)
{
	errors = errors & creds->validation_flags;

	if (errors & G_TLS_CERTIFICATE_UNKNOWN_CA) {
		GList *ca;

		for (ca = creds->ca_list; ca; ca = ca->next) {
			if ((g_tls_certificate_verify (cert, NULL, ca->data) & G_TLS_CERTIFICATE_UNKNOWN_CA) == 0) {
				errors &= ~G_TLS_CERTIFICATE_UNKNOWN_CA;
				break;
			}
		}
	}

	return errors == 0;
}

void
soup_ssl_free_client_credentials (SoupSSLCredentials *client_creds)
{
	GList *c;

	for (c = client_creds->ca_list; c; c = c->next)
		g_object_unref (c->data);
	g_list_free (client_creds->ca_list);
	g_slice_free (SoupSSLCredentials, client_creds);
}

SoupSSLCredentials *
soup_ssl_get_server_credentials (const char *cert_file, const char *key_file)
{
	SoupSSLCredentials *creds;
	GError *error = NULL;

	creds = g_slice_new0 (SoupSSLCredentials);

	creds->certificate = g_tls_certificate_new_from_files (cert_file, key_file, &error);
	if (!creds->certificate) {
		g_warning ("Could not read SSL certificate from '%s': %s",
			   cert_file, error->message);
		g_error_free (error);
		g_slice_free (SoupSSLCredentials, creds);
		return NULL;
	}

	return creds;
}

GTlsCertificate *
soup_ssl_credentials_get_certificate (SoupSSLCredentials *creds)
{
	return creds->certificate;
}

void
soup_ssl_free_server_credentials (SoupSSLCredentials *server_creds)
{
	g_object_unref (server_creds->certificate);
	g_slice_free (SoupSSLCredentials, server_creds);
}

/**
 * SOUP_SSL_ERROR:
 *
 * A #GError domain representing an SSL error. Used with #SoupSSLError.
 **/
/**
 * soup_ssl_error_quark:
 *
 * The quark used as %SOUP_SSL_ERROR
 *
 * Return value: The quark used as %SOUP_SSL_ERROR
 **/
GQuark
soup_ssl_error_quark (void)
{
	static GQuark error;
	if (!error)
		error = g_quark_from_static_string ("soup_ssl_error_quark");
	return error;
}

/**
 * SoupSSLError:
 * @SOUP_SSL_ERROR_HANDSHAKE_NEEDS_READ: Internal error. Never exposed
 * outside of libsoup.
 * @SOUP_SSL_ERROR_HANDSHAKE_NEEDS_WRITE: Internal error. Never exposed
 * outside of libsoup.
 * @SOUP_SSL_ERROR_CERTIFICATE: Indicates an error validating an SSL
 * certificate
 *
 * SSL-related I/O errors.
 **/
