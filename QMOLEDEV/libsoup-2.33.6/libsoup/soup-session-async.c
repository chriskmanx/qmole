/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-session-async.c
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define LIBSOUP_I_HAVE_READ_BUG_594377_AND_KNOW_SOUP_PASSWORD_MANAGER_MIGHT_GO_AWAY

#include "soup-address.h"
#include "soup-session-async.h"
#include "soup-session-private.h"
#include "soup-address.h"
#include "soup-message-private.h"
#include "soup-message-queue.h"
#include "soup-misc.h"
#include "soup-password-manager.h"
#include "soup-proxy-uri-resolver.h"
#include "soup-uri.h"

/**
 * SECTION:soup-session-async
 * @short_description: Soup session for asynchronous (main-loop-based) I/O.
 *
 * #SoupSessionAsync is an implementation of #SoupSession that uses
 * non-blocking I/O via the glib main loop. It is intended for use in
 * single-threaded programs.
 **/

static void run_queue (SoupSessionAsync *sa);
static void do_idle_run_queue (SoupSession *session);

static void  queue_message   (SoupSession *session, SoupMessage *req,
			      SoupSessionCallback callback, gpointer user_data);
static guint send_message    (SoupSession *session, SoupMessage *req);
static void  cancel_message  (SoupSession *session, SoupMessage *msg,
			      guint status_code);

static void  auth_required   (SoupSession *session, SoupMessage *msg,
			      SoupAuth *auth, gboolean retrying);

G_DEFINE_TYPE (SoupSessionAsync, soup_session_async, SOUP_TYPE_SESSION)

typedef struct {
	GSource *idle_run_queue_source;
} SoupSessionAsyncPrivate;
#define SOUP_SESSION_ASYNC_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SOUP_TYPE_SESSION_ASYNC, SoupSessionAsyncPrivate))

static void
soup_session_async_init (SoupSessionAsync *sa)
{
}

static void
finalize (GObject *object)
{
	SoupSessionAsyncPrivate *priv = SOUP_SESSION_ASYNC_GET_PRIVATE (object);

	if (priv->idle_run_queue_source)
		g_source_destroy (priv->idle_run_queue_source);

	G_OBJECT_CLASS (soup_session_async_parent_class)->finalize (object);
}

static void
soup_session_async_class_init (SoupSessionAsyncClass *soup_session_async_class)
{
	SoupSessionClass *session_class = SOUP_SESSION_CLASS (soup_session_async_class);
	GObjectClass *object_class = G_OBJECT_CLASS (session_class);

	g_type_class_add_private (soup_session_async_class,
				  sizeof (SoupSessionAsyncPrivate));

	/* virtual method override */
	session_class->queue_message = queue_message;
	session_class->send_message = send_message;
	session_class->cancel_message = cancel_message;
	session_class->auth_required = auth_required;

	object_class->finalize = finalize;
}


/**
 * soup_session_async_new:
 *
 * Creates an asynchronous #SoupSession with the default options.
 *
 * Return value: the new session.
 **/
SoupSession *
soup_session_async_new (void)
{
	return g_object_new (SOUP_TYPE_SESSION_ASYNC, NULL);
}

/**
 * soup_session_async_new_with_options:
 * @optname1: name of first property to set
 * @...: value of @optname1, followed by additional property/value pairs
 *
 * Creates an asynchronous #SoupSession with the specified options.
 *
 * Return value: the new session.
 **/
SoupSession *
soup_session_async_new_with_options (const char *optname1, ...)
{
	SoupSession *session;
	va_list ap;

	va_start (ap, optname1);
	session = (SoupSession *)g_object_new_valist (SOUP_TYPE_SESSION_ASYNC,
						      optname1, ap);
	va_end (ap);

	return session;
}

static gboolean
item_failed (SoupMessageQueueItem *item, guint status)
{
	if (item->removed) {
		soup_message_queue_item_unref (item);
		return TRUE;
	}

	if (!SOUP_STATUS_IS_SUCCESSFUL (status)) {
		item->state = SOUP_MESSAGE_FINISHING;
		if (!item->msg->status_code)
			soup_session_set_item_status (item->session, item, status);
		do_idle_run_queue (item->session);
		soup_message_queue_item_unref (item);
		return TRUE;
	}

	return FALSE;
}

static void
resolved_proxy_addr (SoupAddress *addr, guint status, gpointer user_data)
{
	SoupMessageQueueItem *item = user_data;
	SoupSession *session = item->session;

	if (item_failed (item, soup_status_proxify (status)))
		return;

	item->proxy_addr = g_object_ref (addr);
	item->state = SOUP_MESSAGE_AWAITING_CONNECTION;

	soup_message_queue_item_unref (item);

	/* If we got here we know session still exists */
	run_queue ((SoupSessionAsync *)session);
}

static void
resolved_proxy_uri (SoupProxyURIResolver *proxy_resolver,
		    guint status, SoupURI *proxy_uri, gpointer user_data)
{
	SoupMessageQueueItem *item = user_data;
	SoupSession *session = item->session;

	if (item_failed (item, status))
		return;

	if (proxy_uri) {
		SoupAddress *proxy_addr;

		item->state = SOUP_MESSAGE_RESOLVING_PROXY_ADDRESS;

		item->proxy_uri = soup_uri_copy (proxy_uri);
		proxy_addr = soup_address_new (proxy_uri->host,
					       proxy_uri->port);
		soup_address_resolve_async (proxy_addr,
					    soup_session_get_async_context (session),
					    item->cancellable,
					    resolved_proxy_addr, item);
		g_object_unref (proxy_addr);
		return;
	}

	item->state = SOUP_MESSAGE_AWAITING_CONNECTION;
	soup_message_queue_item_unref (item);

	/* If we got here we know session still exists */
	run_queue ((SoupSessionAsync *)session);
}

static void
resolve_proxy_addr (SoupMessageQueueItem *item,
		    SoupProxyURIResolver *proxy_resolver)
{
	item->state = SOUP_MESSAGE_RESOLVING_PROXY_URI;

	soup_message_queue_item_ref (item);
	soup_proxy_uri_resolver_get_proxy_uri_async (
		proxy_resolver, soup_message_get_uri (item->msg),
		soup_session_get_async_context (item->session),
		item->cancellable, resolved_proxy_uri, item);
}

static void
connection_closed (SoupConnection *conn, gpointer session)
{
	/* Run the queue in case anyone was waiting for a connection
	 * to be closed.
	 */
	do_idle_run_queue (session);
}

static void
message_completed (SoupMessage *msg, gpointer user_data)
{
	SoupMessageQueueItem *item = user_data;

	if (item->state != SOUP_MESSAGE_RESTARTING)
		item->state = SOUP_MESSAGE_FINISHING;
	do_idle_run_queue (item->session);
}

static void
tunnel_message_completed (SoupMessage *msg, gpointer user_data)
{
	SoupMessageQueueItem *item = user_data;
	SoupSession *session = item->session;

	if (item->state == SOUP_MESSAGE_RESTARTING) {
		soup_message_restarted (msg);
		if (item->conn) {
			soup_session_send_queue_item (session, item, tunnel_message_completed);
			return;
		}

		soup_message_set_status (msg, SOUP_STATUS_TRY_AGAIN);
	}

	item->state = SOUP_MESSAGE_FINISHED;

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		if (item->conn)
			soup_connection_disconnect (item->conn);
		if (msg->status_code == SOUP_STATUS_TRY_AGAIN) {
			item->related->state = SOUP_MESSAGE_AWAITING_CONNECTION;
			g_object_unref (item->related->conn);
			item->related->conn = NULL;
		} else
			soup_message_set_status (item->related->msg, msg->status_code);
		goto done;
	}

	if (!soup_connection_start_ssl (item->conn)) {
		if (item->conn)
			soup_connection_disconnect (item->conn);
		soup_message_set_status (item->related->msg, SOUP_STATUS_SSL_FAILED);
		goto done;
	}

	g_signal_connect (item->conn, "disconnected",
			  G_CALLBACK (connection_closed), item->session);
	soup_connection_set_state (item->conn, SOUP_CONNECTION_IDLE);
	soup_connection_set_state (item->conn, SOUP_CONNECTION_IN_USE);

	item->related->state = SOUP_MESSAGE_READY;

done:
	soup_message_finished (msg);
	if (item->related->msg->status_code)
		item->related->state = SOUP_MESSAGE_FINISHING;

	do_idle_run_queue (item->session);
	soup_message_queue_item_unref (item->related);
	soup_session_unqueue_item (session, item);
	soup_message_queue_item_unref (item);
	g_object_unref (session);
}

static void
got_connection (SoupConnection *conn, guint status, gpointer user_data)
{
	SoupMessageQueueItem *item = user_data;
	SoupSession *session = item->session;
	SoupAddress *tunnel_addr;

	if (item->state != SOUP_MESSAGE_CONNECTING) {
		soup_connection_disconnect (conn);
		do_idle_run_queue (session);
		soup_message_queue_item_unref (item);
		g_object_unref (session);
		return;
	}

	if (status != SOUP_STATUS_OK) {
		soup_session_set_item_status (session, item, status);
		item->state = SOUP_MESSAGE_FINISHING;

		soup_connection_disconnect (conn);
		do_idle_run_queue (session);
		soup_message_queue_item_unref (item);
		g_object_unref (session);
		return;
	}

	tunnel_addr = soup_connection_get_tunnel_addr (conn);
	if (tunnel_addr) {
		SoupMessageQueueItem *tunnel_item;

		item->state = SOUP_MESSAGE_TUNNELING;

		tunnel_item = soup_session_make_connect_message (session, conn);
		tunnel_item->related = item;
		soup_session_send_queue_item (session, tunnel_item, tunnel_message_completed);
		return;
	}

	item->state = SOUP_MESSAGE_READY;
	g_signal_connect (conn, "disconnected",
			  G_CALLBACK (connection_closed), session);
	run_queue ((SoupSessionAsync *)session);
	soup_message_queue_item_unref (item);
	g_object_unref (session);
}

static void
process_queue_item (SoupMessageQueueItem *item,
		    gboolean             *should_prune,
		    gboolean              loop)
{
	SoupSession *session = item->session;
	SoupProxyURIResolver *proxy_resolver;

	do {
		switch (item->state) {
		case SOUP_MESSAGE_STARTING:
			proxy_resolver = (SoupProxyURIResolver *)soup_session_get_feature_for_message (session, SOUP_TYPE_PROXY_URI_RESOLVER, item->msg);
			if (!proxy_resolver) {
				item->state = SOUP_MESSAGE_AWAITING_CONNECTION;
				break;
			}
			resolve_proxy_addr (item, proxy_resolver);
			return;

		case SOUP_MESSAGE_AWAITING_CONNECTION:
			if (!soup_session_get_connection (session, item, should_prune))
				return;

			if (soup_connection_get_state (item->conn) != SOUP_CONNECTION_NEW) {
				item->state = SOUP_MESSAGE_READY;
				break;
			}

			item->state = SOUP_MESSAGE_CONNECTING;
			soup_message_queue_item_ref (item);
			g_object_ref (session);
			soup_connection_connect_async (item->conn, item->cancellable,
						       got_connection, item);
			return;

		case SOUP_MESSAGE_READY:
			item->state = SOUP_MESSAGE_RUNNING;
			soup_session_send_queue_item (session, item, message_completed);
			break;

		case SOUP_MESSAGE_RESTARTING:
			item->state = SOUP_MESSAGE_STARTING;
			soup_message_restarted (item->msg);
			break;

		case SOUP_MESSAGE_FINISHING:
			item->state = SOUP_MESSAGE_FINISHED;
			soup_message_finished (item->msg);
			if (item->state != SOUP_MESSAGE_FINISHED)
				break;

			g_object_ref (session);
			soup_session_unqueue_item (session, item);
			if (item->callback)
				item->callback (session, item->msg, item->callback_data);
			g_object_unref (item->msg);
			do_idle_run_queue (session);
			g_object_unref (session);
			return;

		default:
			/* Nothing to do with this message in any
			 * other state.
			 */
			return;
		}
	} while (loop && item->state != SOUP_MESSAGE_FINISHED);
}

static void
run_queue (SoupSessionAsync *sa)
{
	SoupSession *session = SOUP_SESSION (sa);
	SoupMessageQueue *queue = soup_session_get_queue (session);
	SoupMessageQueueItem *item;
	SoupMessage *msg;
	gboolean try_pruning = TRUE, should_prune = FALSE;

	g_object_ref (session);
	soup_session_cleanup_connections (session, FALSE);

 try_again:
	for (item = soup_message_queue_first (queue);
	     item;
	     item = soup_message_queue_next (queue, item)) {
		msg = item->msg;

		/* CONNECT messages are handled specially */
		if (msg->method != SOUP_METHOD_CONNECT)
			process_queue_item (item, &should_prune, TRUE);
	}

	if (try_pruning && should_prune) {
		/* There is at least one message in the queue that
		 * could be sent if we pruned an idle connection from
		 * some other server.
		 */
		if (soup_session_cleanup_connections (session, TRUE)) {
			try_pruning = should_prune = FALSE;
			goto try_again;
		}
	}

	g_object_unref (session);
}

static gboolean
idle_run_queue (gpointer sa)
{
	SoupSessionAsyncPrivate *priv = SOUP_SESSION_ASYNC_GET_PRIVATE (sa);

	priv->idle_run_queue_source = NULL;
	run_queue (sa);
	return FALSE;
}

static void
do_idle_run_queue (SoupSession *session)
{
	SoupSessionAsyncPrivate *priv = SOUP_SESSION_ASYNC_GET_PRIVATE (session);

	if (!priv->idle_run_queue_source) {
		priv->idle_run_queue_source = soup_add_completion (
			soup_session_get_async_context (session),
			idle_run_queue, session);
	}
}

static void
queue_message (SoupSession *session, SoupMessage *req,
	       SoupSessionCallback callback, gpointer user_data)
{
	SOUP_SESSION_CLASS (soup_session_async_parent_class)->queue_message (session, req, callback, user_data);

	do_idle_run_queue (session);
}

static guint
send_message (SoupSession *session, SoupMessage *req)
{
	SoupMessageQueueItem *item;
	GMainContext *async_context =
		soup_session_get_async_context (session);

	/* Balance out the unref that queuing will eventually do */
	g_object_ref (req);

	queue_message (session, req, NULL, NULL);

	item = soup_message_queue_lookup (soup_session_get_queue (session), req);
	g_return_val_if_fail (item != NULL, SOUP_STATUS_MALFORMED);

	while (item->state != SOUP_MESSAGE_FINISHED)
		g_main_context_iteration (async_context, TRUE);

	soup_message_queue_item_unref (item);

	return req->status_code;
}

static void
cancel_message (SoupSession *session, SoupMessage *msg,
		guint status_code)
{
	SoupMessageQueue *queue;
	SoupMessageQueueItem *item;
	gboolean dummy;

	SOUP_SESSION_CLASS (soup_session_async_parent_class)->
		cancel_message (session, msg, status_code);

	queue = soup_session_get_queue (session);
	item = soup_message_queue_lookup (queue, msg);
	if (!item)
		return;

	/* Force it to finish immediately, so that
	 * soup_session_abort (session); g_object_unref (session);
	 * will work. (The soup_session_cancel_message() docs
	 * point out that the callback will be invoked from
	 * within the cancel call.)
	 */
	if (soup_message_io_in_progress (msg))
		soup_message_io_finished (msg);
	else if (item->state != SOUP_MESSAGE_FINISHED)
		item->state = SOUP_MESSAGE_FINISHING;

	if (item->state != SOUP_MESSAGE_FINISHED)
		process_queue_item (item, &dummy, FALSE);

	soup_message_queue_item_unref (item);
}

static void
got_passwords (SoupPasswordManager *password_manager, SoupMessage *msg,
	       SoupAuth *auth, gboolean retrying, gpointer session)
{
	soup_session_unpause_message (session, msg);
	SOUP_SESSION_CLASS (soup_session_async_parent_class)->
		auth_required (session, msg, auth, retrying);
	g_object_unref (auth);
}

static void
auth_required (SoupSession *session, SoupMessage *msg,
	       SoupAuth *auth, gboolean retrying)
{
	SoupSessionFeature *password_manager;

	password_manager = soup_session_get_feature_for_message (
		session, SOUP_TYPE_PASSWORD_MANAGER, msg);
	if (password_manager) {
		soup_session_pause_message (session, msg);
		g_object_ref (auth);
		soup_password_manager_get_passwords_async (
			SOUP_PASSWORD_MANAGER (password_manager),
			msg, auth, retrying,
			soup_session_get_async_context (session),
			NULL, /* FIXME cancellable */
			got_passwords, session);
	} else {
		SOUP_SESSION_CLASS (soup_session_async_parent_class)->
			auth_required (session, msg, auth, retrying);
	}
}
