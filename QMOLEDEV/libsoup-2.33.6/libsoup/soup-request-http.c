/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-request-http.c: http: URI request object
 *
 * Copyright (C) 2009, 2010 Red Hat, Inc.
 * Copyright (C) 2010 Igalia, S.L.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib/gi18n.h>

#define LIBSOUP_USE_UNSTABLE_REQUEST_API

#include "soup-request-http.h"
#include "soup-cache.h"
#include "soup-cache-private.h"
#include "soup-content-sniffer.h"
#include "soup-http-input-stream.h"
#include "soup-message.h"
#include "soup-session.h"
#include "soup-uri.h"

G_DEFINE_TYPE (SoupRequestHTTP, soup_request_http, SOUP_TYPE_REQUEST)

struct _SoupRequestHTTPPrivate {
	SoupMessage *msg;
};

static void
soup_request_http_init (SoupRequestHTTP *http)
{
	http->priv = G_TYPE_INSTANCE_GET_PRIVATE (http, SOUP_TYPE_REQUEST_HTTP, SoupRequestHTTPPrivate);
}

static gboolean
soup_request_http_check_uri (SoupRequest  *request,
			     SoupURI      *uri,
			     GError      **error)
{
	SoupRequestHTTP *http = SOUP_REQUEST_HTTP (request);

	if (!SOUP_URI_VALID_FOR_HTTP (uri))
		return FALSE;

	http->priv->msg = soup_message_new_from_uri (SOUP_METHOD_GET, uri);
	return TRUE;
}

static void
soup_request_http_finalize (GObject *object)
{
	SoupRequestHTTP *http = SOUP_REQUEST_HTTP (object);

	if (http->priv->msg)
		g_object_unref (http->priv->msg);

	G_OBJECT_CLASS (soup_request_http_parent_class)->finalize (object);
}

static GInputStream *
soup_request_http_send (SoupRequest          *request,
			GCancellable         *cancellable,
			GError              **error)
{
	SoupHTTPInputStream *httpstream;
	SoupRequestHTTP *http = SOUP_REQUEST_HTTP (request);

	httpstream = soup_http_input_stream_new (soup_request_get_session (request), http->priv->msg);
	if (!soup_http_input_stream_send (httpstream, cancellable, error)) {
		g_object_unref (httpstream);
		return NULL;
	}
	return (GInputStream *)httpstream;
}


static void
sent_async (GObject *source, GAsyncResult *result, gpointer user_data)
{
	SoupHTTPInputStream *httpstream = SOUP_HTTP_INPUT_STREAM (source);
	GSimpleAsyncResult *simple = user_data;
	GError *error = NULL;

	if (soup_http_input_stream_send_finish (httpstream, result, &error)) {
		g_simple_async_result_set_op_res_gpointer (simple, httpstream, g_object_unref);
	} else {
		g_simple_async_result_set_from_error (simple, error);
		g_error_free (error);
		g_object_unref (httpstream);
	}
	g_simple_async_result_complete (simple);
	g_object_unref (simple);
}


typedef struct {
	SoupRequestHTTP *req;
	SoupMessage *original;
	GCancellable *cancellable;
	GAsyncReadyCallback callback;
	gpointer user_data;
} ConditionalHelper;

static void
conditional_get_ready_cb (SoupSession *session, SoupMessage *msg, gpointer user_data)
{
	ConditionalHelper *helper = (ConditionalHelper *)user_data;
	GSimpleAsyncResult *simple;
	SoupHTTPInputStream *httpstream;

	simple = g_simple_async_result_new (G_OBJECT (helper->req),
					    helper->callback, helper->user_data,
					    conditional_get_ready_cb);

	if (msg->status_code == SOUP_STATUS_NOT_MODIFIED) {
		SoupCache *cache = (SoupCache *)soup_session_get_feature (session, SOUP_TYPE_CACHE);

		httpstream = (SoupHTTPInputStream *)soup_cache_send_response (cache, msg);
		if (httpstream) {
			g_simple_async_result_set_op_res_gpointer (simple, httpstream, g_object_unref);

			soup_message_got_headers (helper->original);

			if (soup_session_get_feature_for_message (session, SOUP_TYPE_CONTENT_SNIFFER, helper->original)) {
			 	const char *content_type = soup_message_headers_get_content_type (msg->response_headers, NULL);
			 	soup_message_content_sniffed (helper->original, content_type, NULL);
			}

			g_simple_async_result_complete (simple);

			soup_message_finished (helper->original);

			g_object_unref (simple);
		} else {
			/* Ask again for the resource, somehow the cache cannot locate it */
			httpstream = soup_http_input_stream_new (session, helper->original);
			soup_http_input_stream_send_async (httpstream, G_PRIORITY_DEFAULT,
							   helper->cancellable, sent_async, simple);
		}
	} else {
		/* It is in the cache but it was modified remotely */
		httpstream = soup_http_input_stream_new (session, helper->original);
		soup_http_input_stream_send_async (httpstream, G_PRIORITY_DEFAULT,
						   helper->cancellable, sent_async, simple);
	}

	g_object_unref (helper->req);
	g_object_unref (helper->original);
	g_slice_free (ConditionalHelper, helper);
}

typedef struct {
	SoupRequestHTTP *http;
	GAsyncReadyCallback callback;
	gpointer user_data;
	SoupHTTPInputStream *httpstream;
} SendAsyncHelper;

static void soup_request_http_send_async (SoupRequest          *request,
					  GCancellable         *cancellable,
					  GAsyncReadyCallback callback,
					  gpointer user_data);

static gboolean
send_async_cb (gpointer data)
{
	GSimpleAsyncResult *simple;
	SoupSession *session;
	SendAsyncHelper *helper = (SendAsyncHelper *)data;

	session = soup_request_get_session (SOUP_REQUEST (helper->http));
	simple = g_simple_async_result_new (G_OBJECT (helper->http),
					    helper->callback, helper->user_data,
					    soup_request_http_send_async);

	g_simple_async_result_set_op_res_gpointer (simple, helper->httpstream, g_object_unref);

	/* Update message status */
	soup_message_set_status (helper->http->priv->msg, SOUP_STATUS_OK);

	/* Issue signals  */
	soup_message_got_headers (helper->http->priv->msg);

	if (soup_session_get_feature_for_message (session, SOUP_TYPE_CONTENT_SNIFFER, helper->http->priv->msg)) {
		const char *content_type = soup_message_headers_get_content_type (helper->http->priv->msg->response_headers, NULL);
		soup_message_content_sniffed (helper->http->priv->msg, content_type, NULL);
	}

	g_simple_async_result_complete (simple);

	soup_message_finished (helper->http->priv->msg);

	g_object_unref (simple);

	g_object_unref (helper->http);
	g_slice_free (SendAsyncHelper, helper);

	return FALSE;
}

static void
soup_request_http_send_async (SoupRequest          *request,
			      GCancellable         *cancellable,
			      GAsyncReadyCallback   callback,
			      gpointer              user_data)
{
	SoupRequestHTTP *http = SOUP_REQUEST_HTTP (request);
	SoupHTTPInputStream *httpstream;
	GSimpleAsyncResult *simple;
	SoupSession *session;
	SoupCache *cache;

	session = soup_request_get_session (request);
	cache = (SoupCache *)soup_session_get_feature (session, SOUP_TYPE_CACHE);

	if (cache) {
		SoupCacheResponse response;

		response = soup_cache_has_response (cache, http->priv->msg);
		if (response == SOUP_CACHE_RESPONSE_FRESH) {
			SoupHTTPInputStream *httpstream;

			httpstream = (SoupHTTPInputStream *)
				soup_cache_send_response (cache, http->priv->msg);

			/* Cached resource file could have been deleted outside
			 */
			if (httpstream) {
				/* Do return the stream asynchronously as in
				 * the other cases. It's not enough to use
				 * g_simple_async_result_complete_in_idle as
				 * the signals must be also emitted
				 * asynchronously
				 */
				SendAsyncHelper *helper = g_slice_new (SendAsyncHelper);
				helper->http = g_object_ref (http);
				helper->callback = callback;
				helper->user_data = user_data;
				helper->httpstream = httpstream;
				g_timeout_add (0, send_async_cb, helper);
				return;
			}
		} else if (response == SOUP_CACHE_RESPONSE_NEEDS_VALIDATION) {
			SoupMessage *conditional_msg;
			ConditionalHelper *helper;

			conditional_msg = soup_cache_generate_conditional_request (cache, http->priv->msg);

			helper = g_slice_new0 (ConditionalHelper);
			helper->req = g_object_ref (http);
			helper->original = g_object_ref (http->priv->msg);
			helper->cancellable = cancellable;
			helper->callback = callback;
			helper->user_data = user_data;
			soup_session_queue_message (session, conditional_msg,
						    conditional_get_ready_cb,
						    helper);
			return;
		}
	}

	simple = g_simple_async_result_new (G_OBJECT (http),
					    callback, user_data,
					    soup_request_http_send_async);
	httpstream = soup_http_input_stream_new (soup_request_get_session (request),
							http->priv->msg);
	soup_http_input_stream_send_async (httpstream, G_PRIORITY_DEFAULT,
						  cancellable, sent_async, simple);
}

static GInputStream *
soup_request_http_send_finish (SoupRequest          *request,
			       GAsyncResult         *result,
			       GError              **error)
{
	GSimpleAsyncResult *simple;

	g_return_val_if_fail (g_simple_async_result_is_valid (result, G_OBJECT (request), soup_request_http_send_async) || g_simple_async_result_is_valid (result, G_OBJECT (request), conditional_get_ready_cb), NULL);

	simple = G_SIMPLE_ASYNC_RESULT (result);
	if (g_simple_async_result_propagate_error (simple, error))
		return NULL;
	return g_object_ref (g_simple_async_result_get_op_res_gpointer (simple));
}

static goffset
soup_request_http_get_content_length (SoupRequest *request)
{
	SoupRequestHTTP *http = SOUP_REQUEST_HTTP (request);

	return soup_message_headers_get_content_length (http->priv->msg->response_headers);
}

static const char *
soup_request_http_get_content_type (SoupRequest *request)
{
	SoupRequestHTTP *http = SOUP_REQUEST_HTTP (request);

	return soup_message_headers_get_content_type (http->priv->msg->response_headers, NULL);
}

static const char *http_schemes[] = { "http", "https", NULL };

static void
soup_request_http_class_init (SoupRequestHTTPClass *request_http_class)
{
	GObjectClass *object_class = G_OBJECT_CLASS (request_http_class);
	SoupRequestClass *request_class =
		SOUP_REQUEST_CLASS (request_http_class);

	g_type_class_add_private (request_http_class, sizeof (SoupRequestHTTPPrivate));

	request_class->schemes = http_schemes;

	object_class->finalize = soup_request_http_finalize;

	request_class->check_uri = soup_request_http_check_uri;
	request_class->send = soup_request_http_send;
	request_class->send_async = soup_request_http_send_async;
	request_class->send_finish = soup_request_http_send_finish;
	request_class->get_content_length = soup_request_http_get_content_length;
	request_class->get_content_type = soup_request_http_get_content_type;
}

/**
 * soup_request_http_get_message:
 * @http: a #SoupRequestHTTP object
 *
 * Gets a new reference to the #SoupMessage associated to this SoupRequest
 *
 * Returns: a new reference to the #SoupMessage
 **/
SoupMessage *
soup_request_http_get_message (SoupRequestHTTP *http)
{
	g_return_val_if_fail (SOUP_IS_REQUEST_HTTP (http), NULL);

	return g_object_ref (http->priv->msg);
}
