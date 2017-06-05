/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-message-server-io.c: server-side request/response
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include "soup-message-private.h"
#include "soup-address.h"
#include "soup-auth.h"
#include "soup-headers.h"
#include "soup-server.h"
#include "soup-socket.h"

static guint
parse_request_headers (SoupMessage *msg, char *headers, guint headers_len,
		       SoupEncoding *encoding, gpointer sock)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	char *req_method, *req_path, *url;
	SoupHTTPVersion version;
	const char *req_host;
	guint status;
	SoupURI *uri;

	status = soup_headers_parse_request (headers, headers_len,
					     msg->request_headers,
					     &req_method,
					     &req_path,
					     &version);
	if (!SOUP_STATUS_IS_SUCCESSFUL (status))
		return status;

	g_object_set (G_OBJECT (msg),
		      SOUP_MESSAGE_METHOD, req_method,
		      SOUP_MESSAGE_HTTP_VERSION, version,
		      NULL);
	g_free (req_method);

	/* Handle request body encoding */
	*encoding = soup_message_headers_get_encoding (msg->request_headers);
	if (*encoding == SOUP_ENCODING_UNRECOGNIZED) {
		if (soup_message_headers_get (msg->request_headers, "Transfer-Encoding"))
			return SOUP_STATUS_NOT_IMPLEMENTED;
		else
			return SOUP_STATUS_BAD_REQUEST;
	}

	/* Generate correct context for request */
	req_host = soup_message_headers_get (msg->request_headers, "Host");

	if (*req_path != '/') {
		/* Check for absolute URI */
		uri = soup_uri_new (req_path);
		if (!uri) {
			g_free (req_path);
			return SOUP_STATUS_BAD_REQUEST;
		}
	} else if (req_host) {
		url = g_strdup_printf ("%s://%s%s",
				       soup_socket_is_ssl (sock) ? "https" : "http",
				       req_host, req_path);
		uri = soup_uri_new (url);
		g_free (url);
	} else if (priv->http_version == SOUP_HTTP_1_0) {
		/* No Host header, no AbsoluteUri */
		SoupAddress *addr = soup_socket_get_local_address (sock);
		const char *host = soup_address_get_physical (addr);

		url = g_strdup_printf ("%s://%s:%d%s",
				       soup_socket_is_ssl (sock) ? "https" : "http",
				       host, soup_address_get_port (addr),
				       req_path);
		uri = soup_uri_new (url);
		g_free (url);
	} else
		uri = NULL;

	g_free (req_path);
	if (!uri)
		return SOUP_STATUS_BAD_REQUEST;
	soup_message_set_uri (msg, uri);
	soup_uri_free (uri);

	return SOUP_STATUS_OK;
}

static void
get_response_headers (SoupMessage *msg, GString *headers,
		      SoupEncoding *encoding, gpointer user_data)
{
	SoupEncoding claimed_encoding;
	SoupMessageHeadersIter iter;
	const char *name, *value;

	g_string_append_printf (headers, "HTTP/1.1 %d %s\r\n",
				msg->status_code, msg->reason_phrase);

	claimed_encoding = soup_message_headers_get_encoding (msg->response_headers);
	if ((msg->method == SOUP_METHOD_HEAD ||
	     msg->status_code  == SOUP_STATUS_NO_CONTENT ||
	     msg->status_code  == SOUP_STATUS_NOT_MODIFIED ||
	     SOUP_STATUS_IS_INFORMATIONAL (msg->status_code)) ||
	    (msg->method == SOUP_METHOD_CONNECT &&
	     SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)))
		*encoding = SOUP_ENCODING_NONE;
	else
		*encoding = claimed_encoding;

	if (claimed_encoding == SOUP_ENCODING_CONTENT_LENGTH &&
	    !soup_message_headers_get_content_length (msg->response_headers)) {
		soup_message_headers_set_content_length (msg->response_headers,
							 msg->response_body->length);
	}

	soup_message_headers_iter_init (&iter, msg->response_headers);
	while (soup_message_headers_iter_next (&iter, &name, &value))
		g_string_append_printf (headers, "%s: %s\r\n", name, value);
	g_string_append (headers, "\r\n");
}

void
soup_message_read_request (SoupMessage *req, SoupSocket *sock)
{
	soup_message_io_server (req, sock,
				get_response_headers,
				parse_request_headers,
				sock);
}
