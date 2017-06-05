/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-message-client-io.c: client-side request/response
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include "soup-message-private.h"
#include "soup-auth.h"
#include "soup-headers.h"
#include "soup-uri.h"

static guint
parse_response_headers (SoupMessage *req,
			char *headers, guint headers_len,
			SoupEncoding *encoding,
			gpointer user_data)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (req);
	SoupHTTPVersion version;

	g_free((char*)req->reason_phrase);
	req->reason_phrase = NULL;
	if (!soup_headers_parse_response (headers, headers_len,
					  req->response_headers,
					  &version,
					  &req->status_code,
					  (char **) &req->reason_phrase))
		return SOUP_STATUS_MALFORMED;

	g_object_notify (G_OBJECT (req), SOUP_MESSAGE_STATUS_CODE);
	g_object_notify (G_OBJECT (req), SOUP_MESSAGE_REASON_PHRASE);

	if (version < priv->http_version) {
		priv->http_version = version;
		g_object_notify (G_OBJECT (req), SOUP_MESSAGE_HTTP_VERSION);
	}

	if ((req->method == SOUP_METHOD_HEAD ||
	     req->status_code  == SOUP_STATUS_NO_CONTENT ||
	     req->status_code  == SOUP_STATUS_NOT_MODIFIED ||
	     SOUP_STATUS_IS_INFORMATIONAL (req->status_code)) ||
	    (req->method == SOUP_METHOD_CONNECT &&
	     SOUP_STATUS_IS_SUCCESSFUL (req->status_code)))
		*encoding = SOUP_ENCODING_NONE;
	else
		*encoding = soup_message_headers_get_encoding (req->response_headers);

	if (*encoding == SOUP_ENCODING_UNRECOGNIZED)
		return SOUP_STATUS_MALFORMED;

	return SOUP_STATUS_OK;
}

static void
get_request_headers (SoupMessage *req, GString *header,
		     SoupEncoding *encoding, gpointer user_data)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (req);
	gboolean proxy = GPOINTER_TO_UINT (user_data);
	SoupURI *uri = soup_message_get_uri (req);
	char *uri_string;
	SoupMessageHeadersIter iter;
	const char *name, *value;

	if (req->method == SOUP_METHOD_CONNECT) {
		/* CONNECT URI is hostname:port for tunnel destination */
		uri_string = g_strdup_printf ("%s:%d", uri->host, uri->port);
	} else {
		/* Proxy expects full URI to destination. Otherwise
		 * just the path.
		 */
		uri_string = soup_uri_to_string (uri, !proxy);
	}

	if (priv->http_version == SOUP_HTTP_1_0) {
		g_string_append_printf (header, "%s %s HTTP/1.0\r\n",
					req->method, uri_string);
	} else {
		g_string_append_printf (header, "%s %s HTTP/1.1\r\n",
					req->method, uri_string);
		if (soup_uri_uses_default_port (uri)) {
			g_string_append_printf (header, "Host: %s\r\n",
						uri->host);
		} else {
			g_string_append_printf (header, "Host: %s:%d\r\n",
						uri->host, uri->port);
		}
	}
	g_free (uri_string);

	*encoding = soup_message_headers_get_encoding (req->request_headers);
	if (*encoding != SOUP_ENCODING_CHUNKED &&
	    req->request_body->length > 0) {
		soup_message_headers_set_content_length (req->request_headers,
							 req->request_body->length);
	}

	soup_message_headers_iter_init (&iter, req->request_headers);
	while (soup_message_headers_iter_next (&iter, &name, &value))
		g_string_append_printf (header, "%s: %s\r\n", name, value);
	g_string_append (header, "\r\n");
}

void
soup_message_send_request (SoupMessage *req, SoupSocket *sock,
			   SoupConnection *conn, gboolean is_via_proxy)
{
	soup_message_cleanup_response (req);
	soup_message_io_client (req, sock, conn,
				get_request_headers,
				parse_response_headers,
				GUINT_TO_POINTER (is_via_proxy));
}
