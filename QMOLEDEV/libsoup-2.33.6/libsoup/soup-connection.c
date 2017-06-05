/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-connection.c: A single HTTP/HTTPS connection
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>

#include <fcntl.h>
#include <sys/types.h>

#include "soup-address.h"
#include "soup-connection.h"
#include "soup-marshal.h"
#include "soup-message.h"
#include "soup-message-private.h"
#include "soup-message-queue.h"
#include "soup-misc.h"
#include "soup-socket.h"
#include "soup-ssl.h"
#include "soup-uri.h"
#include "soup-enum-types.h"

typedef struct {
	SoupSocket  *socket;

	SoupAddress *remote_addr, *tunnel_addr;
	SoupURI     *proxy_uri;
	gpointer     ssl_creds;
	gboolean     ssl_strict;

	GMainContext      *async_context;

	SoupMessageQueueItem *cur_item;
	SoupConnectionState state;
	time_t       unused_timeout;
	guint        io_timeout, idle_timeout;
	GSource     *idle_timeout_src;
} SoupConnectionPrivate;
#define SOUP_CONNECTION_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SOUP_TYPE_CONNECTION, SoupConnectionPrivate))

G_DEFINE_TYPE (SoupConnection, soup_connection, G_TYPE_OBJECT)

enum {
	DISCONNECTED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

enum {
	PROP_0,

	PROP_REMOTE_ADDRESS,
	PROP_TUNNEL_ADDRESS,
	PROP_PROXY_URI,
	PROP_SSL_CREDS,
	PROP_SSL_STRICT,
	PROP_ASYNC_CONTEXT,
	PROP_TIMEOUT,
	PROP_IDLE_TIMEOUT,
	PROP_STATE,
	PROP_MESSAGE,

	LAST_PROP
};

static void set_property (GObject *object, guint prop_id,
			  const GValue *value, GParamSpec *pspec);
static void get_property (GObject *object, guint prop_id,
			  GValue *value, GParamSpec *pspec);

static void stop_idle_timer (SoupConnectionPrivate *priv);
static void clear_current_item (SoupConnection *conn);

/* Number of seconds after which we close a connection that hasn't yet
 * been used.
 */
#define SOUP_CONNECTION_UNUSED_TIMEOUT 3

static void
soup_connection_init (SoupConnection *conn)
{
	;
}

static void
finalize (GObject *object)
{
	SoupConnectionPrivate *priv = SOUP_CONNECTION_GET_PRIVATE (object);

	if (priv->remote_addr)
		g_object_unref (priv->remote_addr);
	if (priv->tunnel_addr)
		g_object_unref (priv->tunnel_addr);
	if (priv->proxy_uri)
		soup_uri_free (priv->proxy_uri);

	if (priv->async_context)
		g_main_context_unref (priv->async_context);

	G_OBJECT_CLASS (soup_connection_parent_class)->finalize (object);
}

static void
dispose (GObject *object)
{
	SoupConnection *conn = SOUP_CONNECTION (object);
	SoupConnectionPrivate *priv = SOUP_CONNECTION_GET_PRIVATE (conn);

	stop_idle_timer (priv);
	/* Make sure clear_current_item doesn't re-establish the timeout */
	priv->idle_timeout = 0;

	if (priv->cur_item) {
		g_warning ("Disposing connection with cur_item set");
		clear_current_item (conn);
	}
	if (priv->socket) {
		g_warning ("Disposing connection while connected");
		soup_connection_disconnect (conn);
	}

	G_OBJECT_CLASS (soup_connection_parent_class)->dispose (object);
}

static void
soup_connection_class_init (SoupConnectionClass *connection_class)
{
	GObjectClass *object_class = G_OBJECT_CLASS (connection_class);

	g_type_class_add_private (connection_class, sizeof (SoupConnectionPrivate));

	/* virtual method override */
	object_class->dispose = dispose;
	object_class->finalize = finalize;
	object_class->set_property = set_property;
	object_class->get_property = get_property;

	/* signals */
	signals[DISCONNECTED] =
		g_signal_new ("disconnected",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (SoupConnectionClass, disconnected),
			      NULL, NULL,
			      soup_marshal_NONE__NONE,
			      G_TYPE_NONE, 0);

	/* properties */
	g_object_class_install_property (
		object_class, PROP_REMOTE_ADDRESS,
		g_param_spec_object (SOUP_CONNECTION_REMOTE_ADDRESS,
				     "Remote address",
				     "The address of the HTTP or proxy server",
				     SOUP_TYPE_ADDRESS,
				     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_TUNNEL_ADDRESS,
		g_param_spec_object (SOUP_CONNECTION_TUNNEL_ADDRESS,
				     "Tunnel address",
				     "The address of the HTTPS server this tunnel connects to",
				     SOUP_TYPE_ADDRESS,
				     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_PROXY_URI,
		g_param_spec_boxed (SOUP_CONNECTION_PROXY_URI,
				    "Proxy URI",
				    "URI of the HTTP proxy this connection connects to",
				    SOUP_TYPE_URI,
				    G_PARAM_READWRITE));
	g_object_class_install_property (
		object_class, PROP_SSL_CREDS,
		g_param_spec_pointer (SOUP_CONNECTION_SSL_CREDENTIALS,
				      "SSL credentials",
				      "Opaque SSL credentials for this connection",
				      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_SSL_STRICT,
		g_param_spec_boolean (SOUP_CONNECTION_SSL_STRICT,
				      "Strictly validate SSL certificates",
				      "Whether certificate errors should be considered a connection error",
				      TRUE,
				      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_ASYNC_CONTEXT,
		g_param_spec_pointer (SOUP_CONNECTION_ASYNC_CONTEXT,
				      "Async GMainContext",
				      "GMainContext to dispatch this connection's async I/O in",
				      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_TIMEOUT,
		g_param_spec_uint (SOUP_CONNECTION_TIMEOUT,
				   "Timeout value",
				   "Value in seconds to timeout a blocking I/O",
				   0, G_MAXUINT, 0,
				   G_PARAM_READWRITE));
	g_object_class_install_property (
		object_class, PROP_IDLE_TIMEOUT,
		g_param_spec_uint (SOUP_CONNECTION_IDLE_TIMEOUT,
				   "Idle Timeout",
				   "Connection lifetime when idle",
				   0, G_MAXUINT, 0,
				   G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_STATE,
		g_param_spec_enum (SOUP_CONNECTION_STATE,
				   "Connection state",
				   "Current state of connection",
				   SOUP_TYPE_CONNECTION_STATE, SOUP_CONNECTION_NEW,
				   G_PARAM_READWRITE));
	g_object_class_install_property (
		object_class, PROP_MESSAGE,
		g_param_spec_object (SOUP_CONNECTION_MESSAGE,
				     "Message",
				     "Message being processed",
				     SOUP_TYPE_MESSAGE,
				     G_PARAM_READABLE));
}


SoupConnection *
soup_connection_new (const char *propname1, ...)
{
	SoupConnection *conn;
	va_list ap;

	va_start (ap, propname1);
	conn = (SoupConnection *)g_object_new_valist (SOUP_TYPE_CONNECTION,
						      propname1, ap);
	va_end (ap);

	return conn;
}

static void
set_property (GObject *object, guint prop_id,
	      const GValue *value, GParamSpec *pspec)
{
	SoupConnectionPrivate *priv = SOUP_CONNECTION_GET_PRIVATE (object);

	switch (prop_id) {
	case PROP_REMOTE_ADDRESS:
		priv->remote_addr = g_value_dup_object (value);
		break;
	case PROP_TUNNEL_ADDRESS:
		priv->tunnel_addr = g_value_dup_object (value);
		break;
	case PROP_PROXY_URI:
		if (priv->proxy_uri)
			soup_uri_free (priv->proxy_uri);
		priv->proxy_uri = g_value_dup_boxed (value);
		break;
	case PROP_SSL_CREDS:
		priv->ssl_creds = g_value_get_pointer (value);
		break;
	case PROP_SSL_STRICT:
		priv->ssl_strict = g_value_get_boolean (value);
		break;
	case PROP_ASYNC_CONTEXT:
		priv->async_context = g_value_get_pointer (value);
		if (priv->async_context)
			g_main_context_ref (priv->async_context);
		break;
	case PROP_TIMEOUT:
		priv->io_timeout = g_value_get_uint (value);
		break;
	case PROP_IDLE_TIMEOUT:
		priv->idle_timeout = g_value_get_uint (value);
		break;
	case PROP_STATE:
		soup_connection_set_state (SOUP_CONNECTION (object), g_value_get_uint (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
get_property (GObject *object, guint prop_id,
	      GValue *value, GParamSpec *pspec)
{
	SoupConnectionPrivate *priv = SOUP_CONNECTION_GET_PRIVATE (object);

	switch (prop_id) {
	case PROP_REMOTE_ADDRESS:
		g_value_set_object (value, priv->remote_addr);
		break;
	case PROP_TUNNEL_ADDRESS:
		g_value_set_object (value, priv->tunnel_addr);
		break;
	case PROP_PROXY_URI:
		g_value_set_boxed (value, priv->proxy_uri);
		break;
	case PROP_SSL_CREDS:
		g_value_set_pointer (value, priv->ssl_creds);
		break;
	case PROP_SSL_STRICT:
		g_value_set_boolean (value, priv->ssl_strict);
		break;
	case PROP_ASYNC_CONTEXT:
		g_value_set_pointer (value, priv->async_context ? g_main_context_ref (priv->async_context) : NULL);
		break;
	case PROP_TIMEOUT:
		g_value_set_uint (value, priv->io_timeout);
		break;
	case PROP_IDLE_TIMEOUT:
		g_value_set_uint (value, priv->idle_timeout);
		break;
	case PROP_STATE:
		g_value_set_enum (value, priv->state);
		break;
	case PROP_MESSAGE:
		if (priv->cur_item)
			g_value_set_object (value, priv->cur_item->msg);
		else
			g_value_set_object (value, NULL);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
idle_timeout (gpointer conn)
{
	soup_connection_disconnect (conn);
	return FALSE;
}

static void
start_idle_timer (SoupConnection *conn)
{
	SoupConnectionPrivate *priv = SOUP_CONNECTION_GET_PRIVATE (conn);

	if (priv->idle_timeout > 0 && !priv->idle_timeout_src) {
		priv->idle_timeout_src =
			soup_add_timeout (priv->async_context,
					  priv->idle_timeout * 1000,
					  idle_timeout, conn);
	}
}

static void
stop_idle_timer (SoupConnectionPrivate *priv)
{
	if (priv->idle_timeout_src) {
		g_source_destroy (priv->idle_timeout_src);
		priv->idle_timeout_src = NULL;
	}
}

static void
set_current_item (SoupConnection *conn, SoupMessageQueueItem *item)
{
	SoupConnectionPrivate *priv = SOUP_CONNECTION_GET_PRIVATE (conn);

	g_return_if_fail (priv->cur_item == NULL);

	g_object_freeze_notify (G_OBJECT (conn));

	stop_idle_timer (priv);

	item->state = SOUP_MESSAGE_RUNNING;
	priv->cur_item = item;
	g_object_notify (G_OBJECT (conn), "message");

	if (priv->state == SOUP_CONNECTION_IDLE ||
	    item->msg->method != SOUP_METHOD_CONNECT)
		soup_connection_set_state (conn, SOUP_CONNECTION_IN_USE);

	g_object_thaw_notify (G_OBJECT (conn));
}

static void
clear_current_item (SoupConnection *conn)
{
	SoupConnectionPrivate *priv = SOUP_CONNECTION_GET_PRIVATE (conn);

	g_object_freeze_notify (G_OBJECT (conn));

	priv->unused_timeout = 0;
	start_idle_timer (conn);

	if (priv->cur_item) {
		SoupMessageQueueItem *item;

		item = priv->cur_item;
		priv->cur_item = NULL;
		g_object_notify (G_OBJECT (conn), "message");

		if (item->msg->method == SOUP_METHOD_CONNECT &&
		    SOUP_STATUS_IS_SUCCESSFUL (item->msg->status_code)) {
			/* We're now effectively no longer proxying */
			soup_uri_free (priv->proxy_uri);
			priv->proxy_uri = NULL;
		}

		if (!soup_message_is_keepalive (item->msg))
			soup_connection_disconnect (conn);
	}

	g_object_thaw_notify (G_OBJECT (conn));
}

static void
socket_disconnected (SoupSocket *sock, gpointer conn)
{
	soup_connection_disconnect (conn);
}

typedef struct {
	SoupConnection *conn;
	SoupConnectionCallback callback;
	gpointer callback_data;
} SoupConnectionAsyncConnectData;

static void
socket_connect_result (SoupSocket *sock, guint status, gpointer user_data)
{
	SoupConnectionAsyncConnectData *data = user_data;
	SoupConnectionPrivate *priv;

	priv = SOUP_CONNECTION_GET_PRIVATE (data->conn);

	if (!SOUP_STATUS_IS_SUCCESSFUL (status))
		goto done;

	if (priv->ssl_creds && !priv->tunnel_addr) {
		if (!soup_socket_start_ssl (sock, NULL)) {
			status = SOUP_STATUS_SSL_FAILED;
			goto done;
		}
	}

	g_signal_connect (priv->socket, "disconnected",
			  G_CALLBACK (socket_disconnected), data->conn);

	soup_connection_set_state (data->conn, SOUP_CONNECTION_IN_USE);
	priv->unused_timeout = time (NULL) + SOUP_CONNECTION_UNUSED_TIMEOUT;
	start_idle_timer (data->conn);

 done:
	if (data->callback) {
		if (priv->proxy_uri != NULL)
			status = soup_status_proxify (status);
		data->callback (data->conn, status, data->callback_data);
	}
	g_object_unref (data->conn);
	g_slice_free (SoupConnectionAsyncConnectData, data);
}

void
soup_connection_connect_async (SoupConnection *conn,
			       GCancellable *cancellable,
			       SoupConnectionCallback callback,
			       gpointer user_data)
{
	SoupConnectionAsyncConnectData *data;
	SoupConnectionPrivate *priv;

	g_return_if_fail (SOUP_IS_CONNECTION (conn));
	priv = SOUP_CONNECTION_GET_PRIVATE (conn);
	g_return_if_fail (priv->socket == NULL);

	soup_connection_set_state (conn, SOUP_CONNECTION_CONNECTING);

	data = g_slice_new (SoupConnectionAsyncConnectData);
	data->conn = g_object_ref (conn);
	data->callback = callback;
	data->callback_data = user_data;

	priv->socket =
		soup_socket_new (SOUP_SOCKET_REMOTE_ADDRESS, priv->remote_addr,
				 SOUP_SOCKET_SSL_CREDENTIALS, priv->ssl_creds,
				 SOUP_SOCKET_SSL_STRICT, priv->ssl_strict,
				 SOUP_SOCKET_ASYNC_CONTEXT, priv->async_context,
				 SOUP_SOCKET_TIMEOUT, priv->io_timeout,
				 "clean-dispose", TRUE,
				 NULL);
	soup_socket_connect_async (priv->socket, cancellable,
				   socket_connect_result, data);
}

guint
soup_connection_connect_sync (SoupConnection *conn, GCancellable *cancellable)
{
	SoupConnectionPrivate *priv;
	guint status;

	g_return_val_if_fail (SOUP_IS_CONNECTION (conn), SOUP_STATUS_MALFORMED);
	priv = SOUP_CONNECTION_GET_PRIVATE (conn);
	g_return_val_if_fail (priv->socket == NULL, SOUP_STATUS_MALFORMED);

	soup_connection_set_state (conn, SOUP_CONNECTION_CONNECTING);

	priv->socket =
		soup_socket_new (SOUP_SOCKET_REMOTE_ADDRESS, priv->remote_addr,
				 SOUP_SOCKET_SSL_CREDENTIALS, priv->ssl_creds,
				 SOUP_SOCKET_SSL_STRICT, priv->ssl_strict,
				 SOUP_SOCKET_FLAG_NONBLOCKING, FALSE,
				 SOUP_SOCKET_TIMEOUT, priv->io_timeout,
				 "clean-dispose", TRUE,
				 NULL);

	status = soup_socket_connect_sync (priv->socket, cancellable);

	if (!SOUP_STATUS_IS_SUCCESSFUL (status))
		goto fail;
		
	g_signal_connect (priv->socket, "disconnected",
			  G_CALLBACK (socket_disconnected), conn);

	if (priv->ssl_creds && !priv->tunnel_addr) {
		if (!soup_socket_start_ssl (priv->socket, cancellable)) {
			status = SOUP_STATUS_SSL_FAILED;
			goto fail;
		}
	}

	if (SOUP_STATUS_IS_SUCCESSFUL (status)) {
		soup_connection_set_state (conn, SOUP_CONNECTION_IN_USE);
		priv->unused_timeout = time (NULL) + SOUP_CONNECTION_UNUSED_TIMEOUT;
		start_idle_timer (conn);
	} else {
	fail:
		if (priv->socket) {
			g_object_unref (priv->socket);
			priv->socket = NULL;
		}
	}

	if (priv->proxy_uri != NULL)
		status = soup_status_proxify (status);
	return status;
}

SoupAddress *
soup_connection_get_tunnel_addr (SoupConnection *conn)
{
	SoupConnectionPrivate *priv;

	g_return_val_if_fail (SOUP_IS_CONNECTION (conn), NULL);
	priv = SOUP_CONNECTION_GET_PRIVATE (conn);

	return priv->tunnel_addr;
}

gboolean
soup_connection_start_ssl (SoupConnection *conn)
{
	SoupConnectionPrivate *priv;
	const char *server_name;

	g_return_val_if_fail (SOUP_IS_CONNECTION (conn), FALSE);
	priv = SOUP_CONNECTION_GET_PRIVATE (conn);

	server_name = soup_address_get_name (priv->tunnel_addr ?
					     priv->tunnel_addr :
					     priv->remote_addr);
	return soup_socket_start_proxy_ssl (priv->socket, server_name, NULL);
}

/**
 * soup_connection_disconnect:
 * @conn: a connection
 *
 * Disconnects @conn's socket and emits a %disconnected signal.
 * After calling this, @conn will be essentially useless.
 **/
void
soup_connection_disconnect (SoupConnection *conn)
{
	SoupConnectionPrivate *priv;
	SoupConnectionState old_state;

	g_return_if_fail (SOUP_IS_CONNECTION (conn));
	priv = SOUP_CONNECTION_GET_PRIVATE (conn);

	old_state = priv->state;
	if (old_state != SOUP_CONNECTION_DISCONNECTED)
		soup_connection_set_state (conn, SOUP_CONNECTION_DISCONNECTED);

	if (priv->socket) {
		g_signal_handlers_disconnect_by_func (priv->socket,
						      socket_disconnected, conn);
		soup_socket_disconnect (priv->socket);
		g_object_unref (priv->socket);
		priv->socket = NULL;
	}

	if (old_state != SOUP_CONNECTION_DISCONNECTED)
		g_signal_emit (conn, signals[DISCONNECTED], 0);
}

SoupSocket *
soup_connection_get_socket (SoupConnection *conn)
{
	g_return_val_if_fail (SOUP_IS_CONNECTION (conn), NULL);

	return SOUP_CONNECTION_GET_PRIVATE (conn)->socket;
}

SoupURI *
soup_connection_get_proxy_uri (SoupConnection *conn)
{
	g_return_val_if_fail (SOUP_IS_CONNECTION (conn), NULL);

	return SOUP_CONNECTION_GET_PRIVATE (conn)->proxy_uri;
}

gboolean
soup_connection_is_via_proxy (SoupConnection *conn)
{
	g_return_val_if_fail (SOUP_IS_CONNECTION (conn), FALSE);

	return SOUP_CONNECTION_GET_PRIVATE (conn)->proxy_uri != NULL;
}

SoupConnectionState
soup_connection_get_state (SoupConnection *conn)
{
	SoupConnectionPrivate *priv;

	g_return_val_if_fail (SOUP_IS_CONNECTION (conn),
			      SOUP_CONNECTION_DISCONNECTED);
	priv = SOUP_CONNECTION_GET_PRIVATE (conn);

#ifdef G_OS_UNIX
	if (priv->state == SOUP_CONNECTION_IDLE) {
		GPollFD pfd;

		pfd.fd = soup_socket_get_fd (priv->socket);
		pfd.events = G_IO_IN;
		pfd.revents = 0;
		if (g_poll (&pfd, 1, 0) == 1)
			soup_connection_set_state (conn, SOUP_CONNECTION_REMOTE_DISCONNECTED);
	}
#endif
	if (priv->state == SOUP_CONNECTION_IDLE &&
	    priv->unused_timeout && priv->unused_timeout < time (NULL))
		soup_connection_set_state (conn, SOUP_CONNECTION_REMOTE_DISCONNECTED);

	return priv->state;
}

void
soup_connection_set_state (SoupConnection *conn, SoupConnectionState state)
{
	SoupConnectionPrivate *priv;
	SoupConnectionState old_state;

	g_return_if_fail (SOUP_IS_CONNECTION (conn));
	g_return_if_fail (state >= SOUP_CONNECTION_NEW &&
			  state <= SOUP_CONNECTION_DISCONNECTED);

	g_object_freeze_notify (G_OBJECT (conn));

	priv = SOUP_CONNECTION_GET_PRIVATE (conn);
	old_state = priv->state;
	priv->state = state;
	if ((state == SOUP_CONNECTION_IDLE ||
	     state == SOUP_CONNECTION_DISCONNECTED) &&
	    old_state == SOUP_CONNECTION_IN_USE)
		clear_current_item (conn);

	g_object_notify (G_OBJECT (conn), "state");
	g_object_thaw_notify (G_OBJECT (conn));
}

gboolean
soup_connection_get_ever_used (SoupConnection *conn)
{
	g_return_val_if_fail (SOUP_IS_CONNECTION (conn), FALSE);

	return SOUP_CONNECTION_GET_PRIVATE (conn)->unused_timeout == 0;
}

void
soup_connection_send_request (SoupConnection          *conn,
			      SoupMessageQueueItem    *item,
			      SoupMessageCompletionFn  completion_cb,
			      gpointer                 user_data)
{
	SoupConnectionPrivate *priv;

	g_return_if_fail (SOUP_IS_CONNECTION (conn));
	g_return_if_fail (item != NULL);
	priv = SOUP_CONNECTION_GET_PRIVATE (conn);
	g_return_if_fail (priv->state != SOUP_CONNECTION_NEW && priv->state != SOUP_CONNECTION_DISCONNECTED);

	if (item != priv->cur_item)
		set_current_item (conn, item);
	soup_message_send_request (item, completion_cb, user_data);
}
