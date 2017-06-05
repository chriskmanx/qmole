/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-session-sync.c
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "soup-session-sync.h"
#include "soup-session-private.h"
#include "soup-message-private.h"
#include "soup-misc.h"

/**
 * SECTION:soup-session-sync
 * @short_description: Soup session for blocking I/O in multithreaded
 * programs.
 *
 * #SoupSessionSync is an implementation of #SoupSession that uses
 * synchronous I/O, intended for use in multi-threaded programs.
 *
 * You can use #SoupSessionSync from multiple threads concurrently.
 * Eg, you can send a #SoupMessage in one thread, and then while
 * waiting for the response, send another #SoupMessage from another
 * thread. You can also send a message from one thread and then call
 * soup_session_cancel_message() on it from any other thread (although
 * you need to be careful to avoid race conditions, where the message
 * finishes and is then unreffed by the sending thread just before you
 * cancel it).
 *
 * However, the majority of other types and methods in libsoup are not
 * MT-safe. In particular, you <emphasis>cannot</emphasis> modify or
 * examine a #SoupMessage while it is being transmitted by
 * #SoupSessionSync in another thread. Once a message has been handed
 * off to #SoupSessionSync, it can only be manipulated from its signal
 * handler callbacks, until I/O is complete.
 **/

typedef struct {
	GMutex *lock;
	GCond *cond;
} SoupSessionSyncPrivate;
#define SOUP_SESSION_SYNC_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SOUP_TYPE_SESSION_SYNC, SoupSessionSyncPrivate))

static void  queue_message  (SoupSession *session, SoupMessage *msg,
			     SoupSessionCallback callback, gpointer user_data);
static guint send_message   (SoupSession *session, SoupMessage *msg);
static void  cancel_message (SoupSession *session, SoupMessage *msg,
			     guint status_code);

G_DEFINE_TYPE (SoupSessionSync, soup_session_sync, SOUP_TYPE_SESSION)

static void
soup_session_sync_init (SoupSessionSync *ss)
{
	SoupSessionSyncPrivate *priv = SOUP_SESSION_SYNC_GET_PRIVATE (ss);

	priv->lock = g_mutex_new ();
	priv->cond = g_cond_new ();
}

static void
finalize (GObject *object)
{
	SoupSessionSyncPrivate *priv = SOUP_SESSION_SYNC_GET_PRIVATE (object);

	g_mutex_free (priv->lock);
	g_cond_free (priv->cond);

	G_OBJECT_CLASS (soup_session_sync_parent_class)->finalize (object);
}

static void
soup_session_sync_class_init (SoupSessionSyncClass *session_sync_class)
{
	GObjectClass *object_class = G_OBJECT_CLASS (session_sync_class);
	SoupSessionClass *session_class = SOUP_SESSION_CLASS (session_sync_class);

	g_type_class_add_private (session_sync_class, sizeof (SoupSessionSyncPrivate));

	/* virtual method override */
	session_class->queue_message = queue_message;
	session_class->send_message = send_message;
	session_class->cancel_message = cancel_message;
	object_class->finalize = finalize;
}


/**
 * soup_session_sync_new:
 *
 * Creates an synchronous #SoupSession with the default options.
 *
 * Return value: the new session.
 **/
SoupSession *
soup_session_sync_new (void)
{
	return g_object_new (SOUP_TYPE_SESSION_SYNC, NULL);
}

/**
 * soup_session_sync_new_with_options:
 * @optname1: name of first property to set
 * @...: value of @optname1, followed by additional property/value pairs
 *
 * Creates an synchronous #SoupSession with the specified options.
 *
 * Return value: the new session.
 **/
SoupSession *
soup_session_sync_new_with_options (const char *optname1, ...)
{
	SoupSession *session;
	va_list ap;

	va_start (ap, optname1);
	session = (SoupSession *)g_object_new_valist (SOUP_TYPE_SESSION_SYNC,
						      optname1, ap);
	va_end (ap);

	return session;
}

typedef struct {
	SoupSession *session;
	SoupMessage *msg;
	SoupSessionCallback callback;
	gpointer user_data;
} SoupSessionSyncAsyncData;

static void
async_data_free (SoupSessionSyncAsyncData *sad)
{
	g_object_unref (sad->session);
	g_object_unref (sad->msg);
	g_slice_free (SoupSessionSyncAsyncData, sad);
}

static gboolean
queue_message_callback (gpointer data)
{
	SoupSessionSyncAsyncData *sad = data;

	sad->callback (sad->session, sad->msg, sad->user_data);
	async_data_free (sad);
	return FALSE;
}

static gpointer
queue_message_thread (gpointer data)
{
	SoupSessionSyncAsyncData *sad = data;

	soup_session_send_message (sad->session, sad->msg);
	if (sad->callback) {
		soup_add_idle (soup_session_get_async_context (sad->session),
			       queue_message_callback, sad);
	} else
		async_data_free (sad);

	return NULL;
}

static void
queue_message (SoupSession *session, SoupMessage *msg,
	       SoupSessionCallback callback, gpointer user_data)
{
	SoupSessionSyncAsyncData *sad;

	sad = g_slice_new (SoupSessionSyncAsyncData);
	sad->session = g_object_ref (session);
	sad->msg = g_object_ref (msg);
	sad->callback = callback;
	sad->user_data = user_data;

	g_thread_create (queue_message_thread, sad, FALSE, NULL);
}

static SoupConnection *
wait_for_connection (SoupSession *session, SoupMessage *msg)
{
	SoupSessionSyncPrivate *priv = SOUP_SESSION_SYNC_GET_PRIVATE (session);
	SoupConnection *conn;
	gboolean try_pruning = FALSE, is_new = FALSE;
	guint status;

	g_mutex_lock (priv->lock);

 try_again:
	conn = soup_session_get_connection (session, msg,
					    &try_pruning, &is_new);
	if (conn) {
		if (is_new) {
			status = soup_connection_connect_sync (conn);

			/* If the connection attempt fails, SoupSession
			 * will notice, unref conn, and set an error
			 * status on msg. So all we need to do is just
			 * not return the no-longer-valid connection.
			 */

			if (status == SOUP_STATUS_TRY_AGAIN)
				goto try_again;
			else if (!SOUP_STATUS_IS_SUCCESSFUL (status))
				conn = NULL;
			else if (soup_message_get_io_status (msg) == SOUP_MESSAGE_IO_STATUS_FINISHED) {
				/* Message was cancelled while we were
				 * connecting.
				 */
				soup_connection_disconnect (conn);
				conn = NULL;
			}
		}

		g_mutex_unlock (priv->lock);
		return conn;
	}

	if (try_pruning && soup_session_try_prune_connection (session))
		goto try_again;

	/* Wait... */
	g_cond_wait (priv->cond, priv->lock);

	/* See if something bad happened */
	if (soup_message_get_io_status (msg) == SOUP_MESSAGE_IO_STATUS_FINISHED) {
		g_mutex_unlock (priv->lock);
		return NULL;
	}

	goto try_again;
}

static guint
send_message (SoupSession *session, SoupMessage *msg)
{
	SoupSessionSyncPrivate *priv = SOUP_SESSION_SYNC_GET_PRIVATE (session);
	SoupConnection *conn;

	SOUP_SESSION_CLASS (soup_session_sync_parent_class)->queue_message (session, msg, NULL, NULL);

	do {
		/* Get a connection */
		conn = wait_for_connection (session, msg);
		if (!conn)
			return msg->status_code;

		soup_connection_send_request (conn, msg);
		g_cond_broadcast (priv->cond);
	} while (soup_message_get_io_status (msg) != SOUP_MESSAGE_IO_STATUS_FINISHED);

	return msg->status_code;
}

static void
cancel_message (SoupSession *session, SoupMessage *msg, guint status_code)
{
	SoupSessionSyncPrivate *priv = SOUP_SESSION_SYNC_GET_PRIVATE (session);

	SOUP_SESSION_CLASS (soup_session_sync_parent_class)->cancel_message (session, msg, status_code);
	g_cond_broadcast (priv->cond);
}

