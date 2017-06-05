/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-request.c: Protocol-independent streaming request interface
 *
 * Copyright (C) 2009, 2010 Red Hat, Inc.
 * Copyright (C) 2010, Igalia S.L.
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

#include "soup-request.h"
#include "soup-requester.h"
#include "soup-session.h"
#include "soup-uri.h"

/**
 * SECTION:soup-request
 * @short_description: Protocol-independent streaming request interface
 *
 * FIXME
 **/

/**
 * SoupRequest:
 *
 * FIXME
 *
 * Since: 2.30
 **/

static void soup_request_initable_interface_init (GInitableIface *initable_interface);

G_DEFINE_TYPE_WITH_CODE (SoupRequest, soup_request, G_TYPE_OBJECT,
			 G_IMPLEMENT_INTERFACE (G_TYPE_INITABLE,
						soup_request_initable_interface_init))

enum {
	PROP_0,
	PROP_URI,
	PROP_SESSION
};

struct _SoupRequestPrivate {
	SoupURI *uri;
	SoupSession *session;
};

static void
soup_request_init (SoupRequest *request)
{
	request->priv = G_TYPE_INSTANCE_GET_PRIVATE (request, SOUP_TYPE_REQUEST, SoupRequestPrivate);
}

static void
soup_request_finalize (GObject *object)
{
	SoupRequest *request = SOUP_REQUEST (object);

	if (request->priv->uri)
		soup_uri_free (request->priv->uri);
	if (request->priv->session)
		g_object_unref (request->priv->session);

	G_OBJECT_CLASS (soup_request_parent_class)->finalize (object);
}

static void
soup_request_set_property (GObject      *object,
			   guint prop_id,
			   const GValue *value,
			   GParamSpec   *pspec)
{
	SoupRequest *request = SOUP_REQUEST (object);

	switch (prop_id) {
	case PROP_URI:
		if (request->priv->uri)
			soup_uri_free (request->priv->uri);
		request->priv->uri = g_value_dup_boxed (value);
		break;
	case PROP_SESSION:
		if (request->priv->session)
			g_object_unref (request->priv->session);
		request->priv->session = g_value_dup_object (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
soup_request_get_property (GObject    *object,
			   guint prop_id,
			   GValue     *value,
			   GParamSpec *pspec)
{
	SoupRequest *request = SOUP_REQUEST (object);

	switch (prop_id) {
	case PROP_URI:
		g_value_set_boxed (value, request->priv->uri);
		break;
	case PROP_SESSION:
		g_value_set_object (value, request->priv->session);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
soup_request_initable_init (GInitable     *initable,
			    GCancellable  *cancellable,
			    GError       **error)
{
	SoupRequest *request = SOUP_REQUEST (initable);
	gboolean ok;

	if (!request->priv->uri) {
		g_set_error (error, SOUP_REQUESTER_ERROR, SOUP_REQUESTER_ERROR_BAD_URI,
			     _("No URI provided"));
		return FALSE;
	}

	ok = SOUP_REQUEST_GET_CLASS (initable)->
		check_uri (request, request->priv->uri, error);

	if (!ok && error) {
		char *uri_string = soup_uri_to_string (request->priv->uri, FALSE);
		g_set_error (error, SOUP_REQUESTER_ERROR, SOUP_REQUESTER_ERROR_BAD_URI,
			     _("Invalid '%s' URI: %s"),
			     request->priv->uri->scheme,
			     uri_string);
		g_free (uri_string);
	}

	return ok;
}

static gboolean
soup_request_default_check_uri (SoupRequest  *request,
				SoupURI      *uri,
				GError      **error)
{
	return TRUE;
}

/* Default implementation: assume the sync implementation doesn't block */
static void
soup_request_default_send_async (SoupRequest          *request,
				 GCancellable         *cancellable,
				 GAsyncReadyCallback   callback,
				 gpointer              user_data)
{
	GSimpleAsyncResult *simple;

	simple = g_simple_async_result_new (G_OBJECT (request),
					    callback, user_data,
					    soup_request_default_send_async);
	g_simple_async_result_complete_in_idle (simple);
	g_object_unref (simple);
}

static GInputStream *
soup_request_default_send_finish (SoupRequest          *request,
				  GAsyncResult         *result,
				  GError              **error)
{
	g_return_val_if_fail (g_simple_async_result_is_valid (result, G_OBJECT (request), soup_request_default_send_async), NULL);

	return soup_request_send (request, NULL, error);
}

GInputStream *
soup_request_send (SoupRequest          *request,
		   GCancellable         *cancellable,
		   GError              **error)
{
	return SOUP_REQUEST_GET_CLASS (request)->
		send (request, cancellable, error);
}

void
soup_request_send_async (SoupRequest          *request,
			 GCancellable         *cancellable,
			 GAsyncReadyCallback callback,
			 gpointer user_data)
{
	SOUP_REQUEST_GET_CLASS (request)->
		send_async (request, cancellable, callback, user_data);
}

GInputStream *
soup_request_send_finish (SoupRequest          *request,
			  GAsyncResult         *result,
			  GError              **error)
{
	return SOUP_REQUEST_GET_CLASS (request)->
		send_finish (request, result, error);
}

static void
soup_request_class_init (SoupRequestClass *request_class)
{
	GObjectClass *object_class = G_OBJECT_CLASS (request_class);

	g_type_class_add_private (request_class, sizeof (SoupRequestPrivate));

	request_class->check_uri = soup_request_default_check_uri;
	request_class->send_async = soup_request_default_send_async;
	request_class->send_finish = soup_request_default_send_finish;

	object_class->finalize = soup_request_finalize;
	object_class->set_property = soup_request_set_property;
	object_class->get_property = soup_request_get_property;

	g_object_class_install_property (
		 object_class, PROP_URI,
		 g_param_spec_boxed (SOUP_REQUEST_URI,
				     "URI",
				     "The request URI",
				     SOUP_TYPE_URI,
				     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		 object_class, PROP_SESSION,
		 g_param_spec_object (SOUP_REQUEST_SESSION,
				      "Session",
				      "The request's session",
				      SOUP_TYPE_SESSION,
				      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
soup_request_initable_interface_init (GInitableIface *initable_interface)
{
	initable_interface->init = soup_request_initable_init;
}

SoupURI *
soup_request_get_uri (SoupRequest *request)
{
	return request->priv->uri;
}

SoupSession *
soup_request_get_session (SoupRequest *request)
{
	return request->priv->session;
}

goffset
soup_request_get_content_length (SoupRequest *request)
{
	return SOUP_REQUEST_GET_CLASS (request)->get_content_length (request);
}

const char *
soup_request_get_content_type (SoupRequest  *request)
{
	return SOUP_REQUEST_GET_CLASS (request)->get_content_type (request);
}
