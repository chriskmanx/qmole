/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-socket.c: Socket networking code.
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include "soup-address.h"
#include "soup-socket.h"
#include "soup-marshal.h"
#include "soup-misc.h"
#include "soup-ssl.h"

#include <sys/time.h>
#include <sys/types.h>

/**
 * SECTION:soup-socket
 * @short_description: A network socket
 *
 * #SoupSocket is libsoup's TCP socket type. While it is primarily
 * intended for internal use, #SoupSocket<!-- -->s are exposed in the
 * API in various places, and some of their methods (eg,
 * soup_socket_get_remote_address()) may be useful to applications.
 **/

G_DEFINE_TYPE (SoupSocket, soup_socket, G_TYPE_OBJECT)

enum {
	READABLE,
	WRITABLE,
	DISCONNECTED,
	NEW_CONNECTION,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

enum {
	PROP_0,

	PROP_LOCAL_ADDRESS,
	PROP_REMOTE_ADDRESS,
	PROP_NON_BLOCKING,
	PROP_IS_SERVER,
	PROP_SSL_CREDENTIALS,
	PROP_ASYNC_CONTEXT,
	PROP_TIMEOUT,

	LAST_PROP
};

typedef struct {
	int sockfd;
	SoupAddress *local_addr, *remote_addr;
	GIOChannel *iochannel;

	guint non_blocking:1;
	guint is_server:1;
	gpointer ssl_creds;

	GMainContext   *async_context;
	GSource        *watch_src;
	GSource        *read_src, *write_src;
	GByteArray     *read_buf;

	GMutex *iolock, *addrlock;
	guint timeout;
} SoupSocketPrivate;
#define SOUP_SOCKET_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SOUP_TYPE_SOCKET, SoupSocketPrivate))

#ifdef HAVE_IPV6
#define soup_sockaddr_max sockaddr_in6
#else
#define soup_sockaddr_max sockaddr_in
#endif

static void set_property (GObject *object, guint prop_id,
			  const GValue *value, GParamSpec *pspec);
static void get_property (GObject *object, guint prop_id,
			  GValue *value, GParamSpec *pspec);

#ifdef G_OS_WIN32
#define SOUP_IS_SOCKET_ERROR(status) ((status) == SOCKET_ERROR)
#define SOUP_IS_INVALID_SOCKET(socket) ((socket) == INVALID_SOCKET)
#define SOUP_IS_CONNECT_STATUS_INPROGRESS() (WSAGetLastError () == WSAEWOULDBLOCK)
#define SHUT_RDWR SD_BOTH
#else
#define SOUP_IS_SOCKET_ERROR(status) ((status) == -1)
#define SOUP_IS_INVALID_SOCKET(socket) ((socket) < 0)
#define SOUP_IS_CONNECT_STATUS_INPROGRESS() (errno == EINPROGRESS)
#endif

static void
soup_socket_init (SoupSocket *sock)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);

	priv->sockfd = -1;
	priv->non_blocking = TRUE;
	priv->addrlock = g_mutex_new ();
	priv->iolock = g_mutex_new ();
	priv->timeout = 0;
}

static void
disconnect_internal (SoupSocketPrivate *priv)
{
	g_io_channel_unref (priv->iochannel);
	priv->iochannel = NULL;
	priv->sockfd = -1;

	if (priv->read_src) {
		g_source_destroy (priv->read_src);
		priv->read_src = NULL;
	}
	if (priv->write_src) {
		g_source_destroy (priv->write_src);
		priv->write_src = NULL;
	}
}

static void
finalize (GObject *object)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (object);

	if (priv->iochannel)
		disconnect_internal (priv);

	if (priv->local_addr)
		g_object_unref (priv->local_addr);
	if (priv->remote_addr)
		g_object_unref (priv->remote_addr);

	if (priv->watch_src)
		g_source_destroy (priv->watch_src);
	if (priv->async_context)
		g_main_context_unref (priv->async_context);

	if (priv->read_buf)
		g_byte_array_free (priv->read_buf, TRUE);

	g_mutex_free (priv->addrlock);
	g_mutex_free (priv->iolock);

	G_OBJECT_CLASS (soup_socket_parent_class)->finalize (object);
}

static void
soup_socket_class_init (SoupSocketClass *socket_class)
{
	GObjectClass *object_class = G_OBJECT_CLASS (socket_class);

	g_type_class_add_private (socket_class, sizeof (SoupSocketPrivate));

	/* virtual method override */
	object_class->finalize = finalize;
	object_class->set_property = set_property;
	object_class->get_property = get_property;

	/* signals */

	/**
	 * SoupSocket::readable:
	 * @sock: the socket
	 *
	 * Emitted when an async socket is readable. See
	 * soup_socket_read() and soup_socket_read_until().
	 **/
	signals[READABLE] =
		g_signal_new ("readable",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (SoupSocketClass, readable),
			      NULL, NULL,
			      soup_marshal_NONE__NONE,
			      G_TYPE_NONE, 0);

	/**
	 * SoupSocket::writable:
	 * @sock: the socket
	 *
	 * Emitted when an async socket is writable. See
	 * soup_socket_write().
	 **/
	signals[WRITABLE] =
		g_signal_new ("writable",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (SoupSocketClass, writable),
			      NULL, NULL,
			      soup_marshal_NONE__NONE,
			      G_TYPE_NONE, 0);

	/**
	 * SoupSocket::disconnected:
	 * @sock: the socket
	 *
	 * Emitted when the socket is disconnected, for whatever
	 * reason.
	 **/
	signals[DISCONNECTED] =
		g_signal_new ("disconnected",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (SoupSocketClass, disconnected),
			      NULL, NULL,
			      soup_marshal_NONE__NONE,
			      G_TYPE_NONE, 0);

	/**
	 * SoupSocket::new-connection:
	 * @sock: the socket
	 * @new: the new socket
	 *
	 * Emitted when a listening socket (set up with
	 * soup_socket_listen()) receives a new connection.
	 *
	 * You must ref the @new if you want to keep it; otherwise it
	 * will be destroyed after the signal is emitted.
	 **/
	signals[NEW_CONNECTION] =
		g_signal_new ("new_connection",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (SoupSocketClass, new_connection),
			      NULL, NULL,
			      soup_marshal_NONE__OBJECT,
			      G_TYPE_NONE, 1,
			      SOUP_TYPE_SOCKET);

	/* properties */
	g_object_class_install_property (
		object_class, PROP_LOCAL_ADDRESS,
		g_param_spec_object (SOUP_SOCKET_LOCAL_ADDRESS,
				     "Local address",
				     "Address of local end of socket",
				     SOUP_TYPE_ADDRESS,
				     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_REMOTE_ADDRESS,
		g_param_spec_object (SOUP_SOCKET_REMOTE_ADDRESS,
				     "Remote address",
				     "Address of remote end of socket",
				     SOUP_TYPE_ADDRESS,
				     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_NON_BLOCKING,
		g_param_spec_boolean (SOUP_SOCKET_FLAG_NONBLOCKING,
				      "Non-blocking",
				      "Whether or not the socket uses non-blocking I/O",
				      TRUE,
				      G_PARAM_READWRITE));
	g_object_class_install_property (
		object_class, PROP_IS_SERVER,
		g_param_spec_boolean (SOUP_SOCKET_IS_SERVER,
				      "Server",
				      "Whether or not the socket is a server socket",
				      FALSE,
				      G_PARAM_READABLE));
	g_object_class_install_property (
		object_class, PROP_SSL_CREDENTIALS,
		g_param_spec_pointer (SOUP_SOCKET_SSL_CREDENTIALS,
				      "SSL credentials",
				      "SSL credential information, passed from the session to the SSL implementation",
				      G_PARAM_READWRITE));
	g_object_class_install_property (
		object_class, PROP_ASYNC_CONTEXT,
		g_param_spec_pointer (SOUP_SOCKET_ASYNC_CONTEXT,
				      "Async GMainContext",
				      "The GMainContext to dispatch this socket's async I/O in",
				      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (
		object_class, PROP_TIMEOUT,
		g_param_spec_uint (SOUP_SOCKET_TIMEOUT,
				   "Timeout value",
				   "Value in seconds to timeout a blocking I/O",
				   0, G_MAXUINT, 0,
				   G_PARAM_READWRITE));

#ifdef G_OS_WIN32
	/* Make sure WSAStartup() gets called. */
	soup_address_get_type ();
#endif
}


static void
set_nonblocking (SoupSocketPrivate *priv)
{
#ifndef G_OS_WIN32
	int flags;
#else
	u_long val;
#endif

	if (priv->sockfd == -1)
		return;

#ifndef G_OS_WIN32
	flags = fcntl (priv->sockfd, F_GETFL, 0);
	if (flags != -1) {
		if (priv->non_blocking)
			flags |= O_NONBLOCK;
		else
			flags &= ~O_NONBLOCK;
		fcntl (priv->sockfd, F_SETFL, flags);
	}
#else
	val = priv->non_blocking ? 1 : 0;
	ioctlsocket (priv->sockfd, FIONBIO, &val);
#endif
}

static void
set_fdflags (SoupSocketPrivate *priv)
{
	int opt;
	struct timeval timeout;
#ifndef G_OS_WIN32
	int flags;
#endif

	if (priv->sockfd == -1)
		return;

	set_nonblocking (priv);

#ifndef G_OS_WIN32
	flags = fcntl (priv->sockfd, F_GETFD, 0);
	if (flags != -1) {
		flags |= FD_CLOEXEC;
		fcntl (priv->sockfd, F_SETFD, flags);
	}
#endif

	opt = 1;
	setsockopt (priv->sockfd, IPPROTO_TCP,
		    TCP_NODELAY, (void *) &opt, sizeof (opt));
	setsockopt (priv->sockfd, SOL_SOCKET,
		    SO_REUSEADDR, (void *) &opt, sizeof (opt));

	timeout.tv_sec = priv->timeout;
	timeout.tv_usec = 0;
	setsockopt (priv->sockfd, SOL_SOCKET,
		    SO_RCVTIMEO, (void *) &timeout, sizeof (timeout));

	timeout.tv_sec = priv->timeout;
	timeout.tv_usec = 0;
	setsockopt (priv->sockfd, SOL_SOCKET,
		    SO_SNDTIMEO, (void *) &timeout, sizeof (timeout));

#ifndef G_OS_WIN32
	priv->iochannel =
		g_io_channel_unix_new (priv->sockfd);
#else
	priv->iochannel =
		g_io_channel_win32_new_socket (priv->sockfd);
#endif
	g_io_channel_set_close_on_unref (priv->iochannel, TRUE);
	g_io_channel_set_encoding (priv->iochannel, NULL, NULL);
	g_io_channel_set_buffered (priv->iochannel, FALSE);
}

static void
set_property (GObject *object, guint prop_id,
	      const GValue *value, GParamSpec *pspec)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (object);

	switch (prop_id) {
	case PROP_LOCAL_ADDRESS:
		priv->local_addr = (SoupAddress *)g_value_dup_object (value);
		break;
	case PROP_REMOTE_ADDRESS:
		priv->remote_addr = (SoupAddress *)g_value_dup_object (value);
		break;
	case PROP_NON_BLOCKING:
		priv->non_blocking = g_value_get_boolean (value);
		set_nonblocking (priv);
		break;
	case PROP_SSL_CREDENTIALS:
		priv->ssl_creds = g_value_get_pointer (value);
		break;
	case PROP_ASYNC_CONTEXT:
		priv->async_context = g_value_get_pointer (value);
		if (priv->async_context)
			g_main_context_ref (priv->async_context);
		break;
	case PROP_TIMEOUT:
		priv->timeout = g_value_get_uint (value);
		break;
	default:
		break;
	}
}

static void
get_property (GObject *object, guint prop_id,
	      GValue *value, GParamSpec *pspec)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (object);

	switch (prop_id) {
	case PROP_LOCAL_ADDRESS:
		g_value_set_object (value, soup_socket_get_local_address (SOUP_SOCKET (object)));
		break;
	case PROP_REMOTE_ADDRESS:
		g_value_set_object (value, soup_socket_get_remote_address (SOUP_SOCKET (object)));
		break;
	case PROP_NON_BLOCKING:
		g_value_set_boolean (value, priv->non_blocking);
		break;
	case PROP_IS_SERVER:
		g_value_set_boolean (value, priv->is_server);
		break;
	case PROP_SSL_CREDENTIALS:
		g_value_set_pointer (value, priv->ssl_creds);
		break;
	case PROP_ASYNC_CONTEXT:
		g_value_set_pointer (value, priv->async_context ? g_main_context_ref (priv->async_context) : NULL);
		break;
	case PROP_TIMEOUT:
		g_value_set_uint (value, priv->timeout);
		break;
	default:
		break;
	}
}


/**
 * soup_socket_new:
 * @optname1: name of first property to set (or %NULL)
 * @...: value of @optname1, followed by additional property/value pairs
 *
 * Creates a new (disconnected) socket
 *
 * Return value: the new socket
 **/
SoupSocket *
soup_socket_new (const char *optname1, ...)
{
	SoupSocket *sock;
	va_list ap;

	va_start (ap, optname1);
	sock = (SoupSocket *)g_object_new_valist (SOUP_TYPE_SOCKET,
						  optname1, ap);
	va_end (ap);

	return sock;
}

typedef struct {
	SoupSocket *sock;
	GCancellable *cancellable;
	guint cancel_id;
	SoupSocketCallback callback;
	gpointer user_data;
} SoupSocketAsyncConnectData;

static gboolean
idle_connect_result (gpointer user_data)
{
	SoupSocketAsyncConnectData *sacd = user_data;
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sacd->sock);
	guint status;

	priv->watch_src = NULL;
	if (sacd->cancel_id)
		g_signal_handler_disconnect (sacd->cancellable, sacd->cancel_id);

	if (priv->sockfd == -1) {
		if (g_cancellable_is_cancelled (sacd->cancellable))
			status = SOUP_STATUS_CANCELLED;
		else
			status = SOUP_STATUS_CANT_CONNECT;
	} else
		status = SOUP_STATUS_OK;

	sacd->callback (sacd->sock, status, sacd->user_data);
	g_slice_free (SoupSocketAsyncConnectData, sacd);
	return FALSE;
}

static gboolean
connect_watch (GIOChannel* iochannel, GIOCondition condition, gpointer data)
{
	SoupSocketAsyncConnectData *sacd = data;
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sacd->sock);
	int error = 0;
	int len = sizeof (error);

	/* Remove the watch now in case we don't return immediately */
	g_source_destroy (priv->watch_src);
	priv->watch_src = NULL;

	if ((condition & ~(G_IO_IN | G_IO_OUT)) ||
	    (getsockopt (priv->sockfd, SOL_SOCKET, SO_ERROR,
			 (void *)&error, (void *)&len) != 0) ||
	    error)
		disconnect_internal (priv);

	return idle_connect_result (sacd);
}

static void
got_address (SoupAddress *addr, guint status, gpointer user_data)
{
	SoupSocketAsyncConnectData *sacd = user_data;

	if (!SOUP_STATUS_IS_SUCCESSFUL (status)) {
		sacd->callback (sacd->sock, status, sacd->user_data);
		g_slice_free (SoupSocketAsyncConnectData, sacd);
		return;
	}

	soup_socket_connect_async (sacd->sock, sacd->cancellable,
				   sacd->callback, sacd->user_data);
	g_slice_free (SoupSocketAsyncConnectData, sacd);
}

static void
async_cancel (GCancellable *cancellable, gpointer user_data)
{
	SoupSocketAsyncConnectData *sacd = user_data;
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sacd->sock);

	if (priv->watch_src)
		g_source_destroy (priv->watch_src);
	disconnect_internal (priv);
	priv->watch_src = soup_add_idle (priv->async_context,
					 idle_connect_result, sacd);
}

static guint
socket_connect_internal (SoupSocket *sock)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);
	struct sockaddr *sa;
	int len, status;

	sa = soup_address_get_sockaddr (priv->remote_addr, &len);
	if (!sa)
		return SOUP_STATUS_CANT_RESOLVE;

	priv->sockfd = socket (sa->sa_family, SOCK_STREAM, 0);
	if (SOUP_IS_INVALID_SOCKET (priv->sockfd))
		return SOUP_STATUS_CANT_CONNECT;
	set_fdflags (priv);

	status = connect (priv->sockfd, sa, len);

	if (SOUP_IS_SOCKET_ERROR (status)) {
		if (SOUP_IS_CONNECT_STATUS_INPROGRESS ())
			return SOUP_STATUS_CONTINUE;

		disconnect_internal (priv);
		return SOUP_STATUS_CANT_CONNECT;
	} else
		return SOUP_STATUS_OK;
}

/**
 * SoupSocketCallback:
 * @sock: the #SoupSocket
 * @status: an HTTP status code indicating success or failure
 * @user_data: the data passed to soup_socket_connect_async()
 *
 * The callback function passed to soup_socket_connect_async().
 **/

/**
 * soup_socket_connect_async:
 * @sock: a client #SoupSocket (which must not already be connected)
 * @cancellable: a #GCancellable, or %NULL
 * @callback: callback to call after connecting
 * @user_data: data to pass to @callback
 *
 * Begins asynchronously connecting to @sock's remote address. The
 * socket will call @callback when it succeeds or fails (but not
 * before returning from this function).
 *
 * If @cancellable is non-%NULL, it can be used to cancel the
 * connection. @callback will still be invoked in this case, with a
 * status of %SOUP_STATUS_CANCELLED.
 **/
void
soup_socket_connect_async (SoupSocket *sock, GCancellable *cancellable,
			   SoupSocketCallback callback, gpointer user_data)
{
	SoupSocketPrivate *priv;
	SoupSocketAsyncConnectData *sacd;
	guint status;

	g_return_if_fail (SOUP_IS_SOCKET (sock));
	priv = SOUP_SOCKET_GET_PRIVATE (sock);
	g_return_if_fail (priv->remote_addr != NULL);

	sacd = g_slice_new0 (SoupSocketAsyncConnectData);
	sacd->sock = sock;
	sacd->cancellable = cancellable;
	sacd->callback = callback;
	sacd->user_data = user_data;

	if (!soup_address_get_sockaddr (priv->remote_addr, NULL)) {
		soup_address_resolve_async (priv->remote_addr,
					    priv->async_context,
					    cancellable,
					    got_address, sacd);
		return;
	}

	status = socket_connect_internal (sock);
	if (status == SOUP_STATUS_CONTINUE) {
		/* Wait for connect to succeed or fail */
		priv->watch_src =
			soup_add_io_watch (priv->async_context,
					   priv->iochannel,
					   G_IO_IN | G_IO_OUT |
					   G_IO_PRI | G_IO_ERR |
					   G_IO_HUP | G_IO_NVAL,
					   connect_watch, sacd);
		if (cancellable) {
			sacd->cancel_id =
				g_signal_connect (cancellable, "cancelled",
						  G_CALLBACK (async_cancel),
						  sacd);
		}
	} else {
		priv->watch_src = soup_add_idle (priv->async_context,
						 idle_connect_result, sacd);
	}
}

static void
sync_cancel (GCancellable *cancellable, gpointer sock)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);

	shutdown (priv->sockfd, SHUT_RDWR);
}

/**
 * soup_socket_connect_sync:
 * @sock: a client #SoupSocket (which must not already be connected)
 * @cancellable: a #GCancellable, or %NULL
 *
 * Attempt to synchronously connect @sock to its remote address.
 *
 * If @cancellable is non-%NULL, it can be used to cancel the
 * connection, in which case soup_socket_connect_sync() will return
 * %SOUP_STATUS_CANCELLED.
 *
 * Return value: a success or failure code.
 **/
guint
soup_socket_connect_sync (SoupSocket *sock, GCancellable *cancellable)
{
	SoupSocketPrivate *priv;
	guint status, cancel_id;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), SOUP_STATUS_MALFORMED);
	priv = SOUP_SOCKET_GET_PRIVATE (sock);
	g_return_val_if_fail (!priv->is_server, SOUP_STATUS_MALFORMED);
	g_return_val_if_fail (priv->sockfd == -1, SOUP_STATUS_MALFORMED);
	g_return_val_if_fail (priv->remote_addr != NULL, SOUP_STATUS_MALFORMED);

	if (!soup_address_get_sockaddr (priv->remote_addr, NULL)) {
		status = soup_address_resolve_sync (priv->remote_addr,
						    cancellable);
		if (!SOUP_STATUS_IS_SUCCESSFUL (status))
			return status;
	}

	if (cancellable) {
		cancel_id = g_signal_connect (cancellable, "cancelled",
					      G_CALLBACK (sync_cancel), sock);
	}

	status = socket_connect_internal (sock);

	if (cancellable) {
		if (status != SOUP_STATUS_OK &&
		    g_cancellable_is_cancelled (cancellable)) {
			status = SOUP_STATUS_CANCELLED;
			disconnect_internal (priv);
		}
		g_signal_handler_disconnect (cancellable, cancel_id);
	}

	return status;
}

static gboolean
listen_watch (GIOChannel* iochannel, GIOCondition condition, gpointer data)
{
	SoupSocket *sock = data, *new;
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock), *new_priv;
	struct soup_sockaddr_max sa;
	int sa_len, sockfd;

	if (condition & (G_IO_HUP | G_IO_ERR)) {
		g_source_destroy (priv->watch_src);
		priv->watch_src = NULL;
		return FALSE;
	}

	sa_len = sizeof (sa);
	sockfd = accept (priv->sockfd, (struct sockaddr *)&sa, (void *)&sa_len);
	if (SOUP_IS_INVALID_SOCKET (sockfd))
		return TRUE;

	new = g_object_new (SOUP_TYPE_SOCKET, NULL);
	new_priv = SOUP_SOCKET_GET_PRIVATE (new);
	new_priv->sockfd = sockfd;
	if (priv->async_context)
		new_priv->async_context = g_main_context_ref (priv->async_context);
	new_priv->non_blocking = priv->non_blocking;
	new_priv->is_server = TRUE;
	new_priv->ssl_creds = priv->ssl_creds;
	set_fdflags (new_priv);

	new_priv->remote_addr = soup_address_new_from_sockaddr ((struct sockaddr *)&sa, sa_len);

	if (new_priv->ssl_creds) {
		if (!soup_socket_start_ssl (new, NULL)) {
			g_object_unref (new);
			return TRUE;
		}
	}

	g_signal_emit (sock, signals[NEW_CONNECTION], 0, new);
	g_object_unref (new);

	return TRUE;
}

/**
 * soup_socket_listen:
 * @sock: a server #SoupSocket (which must not already be connected or
 * listening)
 *
 * Makes @sock start listening on its local address. When connections
 * come in, @sock will emit %new_connection.
 *
 * Return value: whether or not @sock is now listening.
 **/
gboolean
soup_socket_listen (SoupSocket *sock)

{
	SoupSocketPrivate *priv;
	struct sockaddr *sa;
	int sa_len;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), FALSE);
	priv = SOUP_SOCKET_GET_PRIVATE (sock);
	g_return_val_if_fail (priv->sockfd == -1, FALSE);
	g_return_val_if_fail (priv->local_addr != NULL, FALSE);

	priv->is_server = TRUE;

	/* @local_addr may have its port set to 0. So we intentionally
	 * don't store it in priv->local_addr, so that if the
	 * caller calls soup_socket_get_local_address() later, we'll
	 * have to make a new addr by calling getsockname(), which
	 * will have the right port number.
	 */
	sa = soup_address_get_sockaddr (priv->local_addr, &sa_len);
	g_return_val_if_fail (sa != NULL, FALSE);

	priv->sockfd = socket (sa->sa_family, SOCK_STREAM, 0);
	if (SOUP_IS_INVALID_SOCKET (priv->sockfd))
		goto cant_listen;
	set_fdflags (priv);

	/* Bind */
	if (bind (priv->sockfd, sa, sa_len) != 0)
		goto cant_listen;
	/* Force local_addr to be re-resolved now */
	g_object_unref (priv->local_addr);
	priv->local_addr = NULL;

	/* Listen */
	if (listen (priv->sockfd, 10) != 0)
		goto cant_listen;

	priv->watch_src = soup_add_io_watch (priv->async_context,
					     priv->iochannel,
					     G_IO_IN | G_IO_ERR | G_IO_HUP,
					     listen_watch, sock);
	return TRUE;

 cant_listen:
	if (priv->iochannel)
		disconnect_internal (priv);

	return FALSE;
}

/**
 * soup_socket_start_ssl:
 * @sock: the socket
 * @cancellable: a #GCancellable
 *
 * Starts using SSL on @socket.
 *
 * Return value: success or failure
 **/
gboolean
soup_socket_start_ssl (SoupSocket *sock, GCancellable *cancellable)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);

	return soup_socket_start_proxy_ssl (sock, soup_address_get_name (priv->remote_addr), cancellable);
}
	
/**
 * soup_socket_start_proxy_ssl:
 * @sock: the socket
 * @ssl_host: hostname of the SSL server
 * @cancellable: a #GCancellable
 *
 * Starts using SSL on @socket, expecting to find a host named
 * @ssl_host.
 *
 * Return value: success or failure
 **/
gboolean
soup_socket_start_proxy_ssl (SoupSocket *sock, const char *ssl_host,
			     GCancellable *cancellable)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);
	GIOChannel *ssl_chan;
	GIOChannel *real_chan;

	real_chan = priv->iochannel;
	ssl_chan = soup_ssl_wrap_iochannel (
		real_chan, priv->is_server ?
		SOUP_SSL_TYPE_SERVER : SOUP_SSL_TYPE_CLIENT,
		ssl_host, priv->ssl_creds);

	if (!ssl_chan)
		return FALSE;

	priv->iochannel = ssl_chan;
	g_io_channel_unref (real_chan);

	return TRUE;
}
	
/**
 * soup_socket_is_ssl:
 * @sock: a #SoupSocket
 *
 * Tests if @sock is set up to do SSL. Note that this simply means
 * that the %SOUP_SOCKET_SSL_CREDENTIALS property has been set; it
 * does not mean that soup_socket_start_ssl() has been called.
 *
 * Return value: %TRUE if @sock has SSL credentials set
 **/
gboolean
soup_socket_is_ssl (SoupSocket *sock)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);

	return priv->ssl_creds != NULL;
}

/**
 * soup_socket_disconnect:
 * @sock: a #SoupSocket
 *
 * Disconnects @sock. Any further read or write attempts on it will
 * fail.
 **/
void
soup_socket_disconnect (SoupSocket *sock)
{
	SoupSocketPrivate *priv;
	gboolean already_disconnected = FALSE;

	g_return_if_fail (SOUP_IS_SOCKET (sock));
	priv = SOUP_SOCKET_GET_PRIVATE (sock);

	if (g_mutex_trylock (priv->iolock)) {
		if (priv->iochannel)
			disconnect_internal (priv);
		else
			already_disconnected = TRUE;
		g_mutex_unlock (priv->iolock);
	} else {
		int sockfd;

		/* Another thread is currently doing IO, so
		 * we can't close the iochannel. So just shutdown
		 * the file descriptor to force the I/O to fail.
		 * (It will actually be closed when the socket is
		 * destroyed.)
		 */
		sockfd = priv->sockfd;
		priv->sockfd = -1;

		if (sockfd == -1)
			already_disconnected = TRUE;
		else
			shutdown (sockfd, SHUT_RDWR);
	}

	if (already_disconnected)
		return;

	/* Give all readers a chance to notice the connection close */
	g_signal_emit (sock, signals[READABLE], 0);

	/* FIXME: can't disconnect until all data is read */

	/* Then let everyone know we're disconnected */
	g_signal_emit (sock, signals[DISCONNECTED], 0);
}

/**
 * soup_socket_is_connected:
 * @sock: a #SoupSocket
 *
 * Tests if @sock is connected to another host
 *
 * Return value: %TRUE or %FALSE.
 **/
gboolean
soup_socket_is_connected (SoupSocket *sock)
{
	SoupSocketPrivate *priv;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), FALSE);
	priv = SOUP_SOCKET_GET_PRIVATE (sock);

	return priv->iochannel != NULL;
}

/**
 * soup_socket_get_local_address:
 * @sock: a #SoupSocket
 *
 * Returns the #SoupAddress corresponding to the local end of @sock.
 *
 * Return value: the #SoupAddress
 **/
SoupAddress *
soup_socket_get_local_address (SoupSocket *sock)
{
	SoupSocketPrivate *priv;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), NULL);
	priv = SOUP_SOCKET_GET_PRIVATE (sock);

	g_mutex_lock (priv->addrlock);
	if (!priv->local_addr) {
		struct soup_sockaddr_max bound_sa;
		int sa_len;

		sa_len = sizeof (bound_sa);
		getsockname (priv->sockfd, (struct sockaddr *)&bound_sa, (void *)&sa_len);
		priv->local_addr = soup_address_new_from_sockaddr ((struct sockaddr *)&bound_sa, sa_len);
	}
	g_mutex_unlock (priv->addrlock);

	return priv->local_addr;
}

/**
 * soup_socket_get_remote_address:
 * @sock: a #SoupSocket
 *
 * Returns the #SoupAddress corresponding to the remote end of @sock.
 *
 * Return value: the #SoupAddress
 **/
SoupAddress *
soup_socket_get_remote_address (SoupSocket *sock)
{
	SoupSocketPrivate *priv;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), NULL);
	priv = SOUP_SOCKET_GET_PRIVATE (sock);

	g_mutex_lock (priv->addrlock);
	if (!priv->remote_addr) {
		struct soup_sockaddr_max bound_sa;
		int sa_len;

		sa_len = sizeof (bound_sa);
		getpeername (priv->sockfd, (struct sockaddr *)&bound_sa, (void *)&sa_len);
		priv->remote_addr = soup_address_new_from_sockaddr ((struct sockaddr *)&bound_sa, sa_len);
	}
	g_mutex_unlock (priv->addrlock);

	return priv->remote_addr;
}




static gboolean
socket_read_watch (GIOChannel *chan, GIOCondition cond, gpointer user_data)
{
	SoupSocket *sock = user_data;
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);

	priv->read_src = NULL;

	if (cond & (G_IO_ERR | G_IO_HUP))
		soup_socket_disconnect (sock);
	else
		g_signal_emit (sock, signals[READABLE], 0);

	return FALSE;
}

static SoupSocketIOStatus
read_from_network (SoupSocket *sock, gpointer buffer, gsize len,
		   gsize *nread, GError **error)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);
	GIOStatus status;
	GIOCondition cond = G_IO_IN;
	GError *my_err = NULL;

	*nread = 0;

	if (!priv->iochannel)
		return SOUP_SOCKET_EOF;

	status = g_io_channel_read_chars (priv->iochannel,
					  buffer, len, nread, &my_err);
	if (my_err) {
		if (my_err->domain == SOUP_SSL_ERROR &&
		    my_err->code == SOUP_SSL_ERROR_HANDSHAKE_NEEDS_WRITE)
			cond = G_IO_OUT;
		g_propagate_error (error, my_err);
	}

	switch (status) {
	case G_IO_STATUS_NORMAL:
	case G_IO_STATUS_AGAIN:
		if (*nread > 0) {
			g_clear_error (error);
			return SOUP_SOCKET_OK;
		}

		/* If the socket is sync and we get EAGAIN, then it is
		 * a socket timeout and should be treated as an error
		 * condition.
		 */
		if (!priv->non_blocking)
			return SOUP_SOCKET_ERROR;

		if (!priv->read_src) {
			priv->read_src =
				soup_add_io_watch (priv->async_context,
						   priv->iochannel,
						   cond | G_IO_HUP | G_IO_ERR,
						   socket_read_watch, sock);
		}
		g_clear_error (error);
		return SOUP_SOCKET_WOULD_BLOCK;

	case G_IO_STATUS_EOF:
		g_clear_error (error);
		return SOUP_SOCKET_EOF;

	default:
		return SOUP_SOCKET_ERROR;
	}
}

static SoupSocketIOStatus
read_from_buf (SoupSocket *sock, gpointer buffer, gsize len, gsize *nread)
{
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);
	GByteArray *read_buf = priv->read_buf;

	*nread = MIN (read_buf->len, len);
	memcpy (buffer, read_buf->data, *nread);

	if (*nread == read_buf->len) {
		g_byte_array_free (read_buf, TRUE);
		priv->read_buf = NULL;
	} else {
		memmove (read_buf->data, read_buf->data + *nread, 
			 read_buf->len - *nread);
		g_byte_array_set_size (read_buf, read_buf->len - *nread);
	}

	return SOUP_SOCKET_OK;
}

/**
 * SoupSocketIOStatus:
 * @SOUP_SOCKET_OK: Success
 * @SOUP_SOCKET_WOULD_BLOCK: Cannot read/write any more at this time
 * @SOUP_SOCKET_EOF: End of file
 * @SOUP_SOCKET_ERROR: Other error
 *
 * Return value from the #SoupSocket IO methods.
 **/

/**
 * soup_socket_read:
 * @sock: the socket
 * @buffer: buffer to read into
 * @len: size of @buffer in bytes
 * @nread: on return, the number of bytes read into @buffer
 * @cancellable: a #GCancellable, or %NULL
 * @error: error pointer
 *
 * Attempts to read up to @len bytes from @sock into @buffer. If some
 * data is successfully read, soup_socket_read() will return
 * %SOUP_SOCKET_OK, and *@nread will contain the number of bytes
 * actually read.
 *
 * If @sock is non-blocking, and no data is available, the return
 * value will be %SOUP_SOCKET_WOULD_BLOCK. In this case, the caller
 * can connect to the %readable signal to know when there is more data
 * to read. (NB: You MUST read all available data off the socket
 * first. The %readable signal will only be emitted after
 * soup_socket_read() has returned %SOUP_SOCKET_WOULD_BLOCK.)
 *
 * Return value: a #SoupSocketIOStatus, as described above (or
 * %SOUP_SOCKET_EOF if the socket is no longer connected, or
 * %SOUP_SOCKET_ERROR on any other error, in which case @error will
 * also be set).
 **/
SoupSocketIOStatus
soup_socket_read (SoupSocket *sock, gpointer buffer, gsize len,
		  gsize *nread, GCancellable *cancellable, GError **error)
{
	SoupSocketPrivate *priv;
	SoupSocketIOStatus status;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), SOUP_SOCKET_ERROR);
	g_return_val_if_fail (nread != NULL, SOUP_SOCKET_ERROR);

	priv = SOUP_SOCKET_GET_PRIVATE (sock);

	g_mutex_lock (priv->iolock);
	if (priv->read_buf)
		status = read_from_buf (sock, buffer, len, nread);
	else
		status = read_from_network (sock, buffer, len, nread, error);
	g_mutex_unlock (priv->iolock);

	return status;
}

/**
 * soup_socket_read_until:
 * @sock: the socket
 * @buffer: buffer to read into
 * @len: size of @buffer in bytes
 * @boundary: boundary to read until
 * @boundary_len: length of @boundary in bytes
 * @nread: on return, the number of bytes read into @buffer
 * @got_boundary: on return, whether or not the data in @buffer
 * ends with the boundary string
 * @cancellable: a #GCancellable, or %NULL
 * @error: error pointer
 *
 * Like soup_socket_read(), but reads no further than the first
 * occurrence of @boundary. (If the boundary is found, it will be
 * included in the returned data, and *@got_boundary will be set to
 * %TRUE.) Any data after the boundary will returned in future reads.
 *
 * Return value: as for soup_socket_read()
 **/
SoupSocketIOStatus
soup_socket_read_until (SoupSocket *sock, gpointer buffer, gsize len,
			gconstpointer boundary, gsize boundary_len,
			gsize *nread, gboolean *got_boundary,
			GCancellable *cancellable, GError **error)
{
	SoupSocketPrivate *priv;
	SoupSocketIOStatus status;
	GByteArray *read_buf;
	guint match_len, prev_len;
	guint8 *p, *end;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), SOUP_SOCKET_ERROR);
	g_return_val_if_fail (nread != NULL, SOUP_SOCKET_ERROR);
	g_return_val_if_fail (len >= boundary_len, SOUP_SOCKET_ERROR);

	priv = SOUP_SOCKET_GET_PRIVATE (sock);

	g_mutex_lock (priv->iolock);

	*got_boundary = FALSE;

	if (!priv->read_buf)
		priv->read_buf = g_byte_array_new ();
	read_buf = priv->read_buf;

	if (read_buf->len < boundary_len) {
		prev_len = read_buf->len;
		g_byte_array_set_size (read_buf, len);
		status = read_from_network (sock,
					    read_buf->data + prev_len,
					    len - prev_len, nread, error);
		read_buf->len = prev_len + *nread;

		if (status != SOUP_SOCKET_OK) {
			g_mutex_unlock (priv->iolock);
			return status;
		}
	}

	/* Scan for the boundary */
	end = read_buf->data + read_buf->len;
	for (p = read_buf->data; p <= end - boundary_len; p++) {
		if (!memcmp (p, boundary, boundary_len)) {
			p += boundary_len;
			*got_boundary = TRUE;
			break;
		}
	}

	/* Return everything up to 'p' (which is either just after the
	 * boundary, or @boundary_len - 1 bytes before the end of the
	 * buffer).
	 */
	match_len = p - read_buf->data;
	status = read_from_buf (sock, buffer, MIN (len, match_len), nread);

	g_mutex_unlock (priv->iolock);
	return status;
}

static gboolean
socket_write_watch (GIOChannel *chan, GIOCondition cond, gpointer user_data)
{
	SoupSocket *sock = user_data;
	SoupSocketPrivate *priv = SOUP_SOCKET_GET_PRIVATE (sock);

	priv->write_src = NULL;

	if (cond & (G_IO_ERR | G_IO_HUP))
		soup_socket_disconnect (sock);
	else
		g_signal_emit (sock, signals[WRITABLE], 0);

	return FALSE;
}

/**
 * soup_socket_write:
 * @sock: the socket
 * @buffer: data to write
 * @len: size of @buffer, in bytes
 * @nwrote: on return, number of bytes written
 * @cancellable: a #GCancellable, or %NULL
 * @error: error pointer
 *
 * Attempts to write @len bytes from @buffer to @sock. If some data is
 * successfully written, the resturn status will be
 * %SOUP_SOCKET_OK, and *@nwrote will contain the number of bytes
 * actually written.
 *
 * If @sock is non-blocking, and no data could be written right away,
 * the return value will be %SOUP_SOCKET_WOULD_BLOCK. In this case,
 * the caller can connect to the %writable signal to know when more
 * data can be written. (NB: %writable is only emitted after a
 * %SOUP_SOCKET_WOULD_BLOCK.)
 *
 * Return value: a #SoupSocketIOStatus, as described above (or
 * %SOUP_SOCKET_EOF or %SOUP_SOCKET_ERROR. @error will be set if the
 * return value is %SOUP_SOCKET_ERROR.)
 **/
SoupSocketIOStatus
soup_socket_write (SoupSocket *sock, gconstpointer buffer,
		   gsize len, gsize *nwrote,
		   GCancellable *cancellable, GError **error)
{
	SoupSocketPrivate *priv;
	GIOStatus status;
#ifdef SIGPIPE
	gpointer pipe_handler;
#endif
	GIOCondition cond = G_IO_OUT;
	GError *my_err = NULL;

	g_return_val_if_fail (SOUP_IS_SOCKET (sock), SOUP_SOCKET_ERROR);
	g_return_val_if_fail (nwrote != NULL, SOUP_SOCKET_ERROR);

	priv = SOUP_SOCKET_GET_PRIVATE (sock);

	g_mutex_lock (priv->iolock);

	if (!priv->iochannel) {
		g_mutex_unlock (priv->iolock);
		return SOUP_SOCKET_EOF;
	}
	if (priv->write_src) {
		g_mutex_unlock (priv->iolock);
		return SOUP_SOCKET_WOULD_BLOCK;
	}

#ifdef SIGPIPE
	pipe_handler = signal (SIGPIPE, SIG_IGN);
#endif
	status = g_io_channel_write_chars (priv->iochannel,
					   buffer, len, nwrote, &my_err);
#ifdef SIGPIPE
	signal (SIGPIPE, pipe_handler);
#endif
	if (my_err) {
		if (my_err->domain == SOUP_SSL_ERROR &&
		    my_err->code == SOUP_SSL_ERROR_HANDSHAKE_NEEDS_READ)
			cond = G_IO_IN;
		g_propagate_error (error, my_err);
	}

	/* If the socket is sync and we get EAGAIN, then it is a
	 * socket timeout and should be treated as an error condition.
	 */
	if (!priv->non_blocking && status == G_IO_STATUS_AGAIN) {
		g_mutex_unlock (priv->iolock);
		return SOUP_SOCKET_ERROR;
	}

	if (status != G_IO_STATUS_NORMAL && status != G_IO_STATUS_AGAIN) {
		g_mutex_unlock (priv->iolock);
		return SOUP_SOCKET_ERROR;
	}

	g_clear_error (error);

	if (*nwrote) {
		g_mutex_unlock (priv->iolock);
		return SOUP_SOCKET_OK;
	}

	priv->write_src =
		soup_add_io_watch (priv->async_context,
				   priv->iochannel,
				   cond | G_IO_HUP | G_IO_ERR, 
				   socket_write_watch, sock);
	g_mutex_unlock (priv->iolock);
	return SOUP_SOCKET_WOULD_BLOCK;
}
