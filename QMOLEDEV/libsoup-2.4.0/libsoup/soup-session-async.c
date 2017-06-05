/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-session-async.c
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "soup-session-async.h"
#include "soup-session-private.h"
#include "soup-message-private.h"
#include "soup-misc.h"

/**
 * SECTION:soup-session-async
 * @short_description: Soup session for asynchronous (main-loop-based) I/O.
 *
 * #SoupSessionAsync is an implementation of #SoupSession that uses
 * non-blocking I/O via the glib main loop. It is intended for use in
 * single-threaded programs.
 **/

static gboolean run_queue (SoupSessionAsync *sa, gboolean try_pruning);

static void  queue_message   (SoupSession *session, SoupMessage *req,
			      SoupSessionCallback callback, gpointer user_data);
static guint send_message    (SoupSession *session, SoupMessage *req);

G_DEFINE_TYPE (SoupSessionAsync, soup_session_async, SOUP_TYPE_SESSION)

static void
soup_session_async_init (SoupSessionAsync *sa)
{
}

static void
soup_session_async_class_init (SoupSessionAsyncClass *soup_session_async_class)
{
	SoupSessionClass *session_class = SOUP_SESSION_CLASS (soup_session_async_class);

	/* virtual method override */
	session_class->queue_message = queue_message;
	session_class->send_message = send_message;
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


static void
connection_closed (SoupConnection *conn, SoupSessionAsync *sa)
{
	/* Run the queue in case anyone was waiting for a connection
	 * to be closed.
	 */
	run_queue (sa, FALSE);
}

static void
got_connection (SoupConnection *conn, guint status, gpointer user_data)
{
	SoupSessionAsync *sa = user_data;

	if (status != SOUP_STATUS_OK) {
		/* The connection attempt failed, and thus @conn was
		 * closed and the open connection count for the
		 * session has been decremented. (If the failure was
		 * fatal, then SoupSession itself will have dealt
		 * with cancelling any pending messages for that
		 * host, so we don't need to worry about that here.)
		 * However, there may be other messages in the
		 * queue that were waiting for the connection count
		 * to go down, so run the queue now.
		 */
		run_queue (sa, FALSE);
		return;
	}

	g_signal_connect (conn, "disconnected",
			  G_CALLBACK (connection_closed), sa);

	/* @conn has been marked reserved by SoupSession, but we don't
	 * actually have any specific message in mind for it. (In
	 * particular, the message we were originally planning to
	 * queue on it may have already been queued on some other
	 * connection that became available while we were waiting for
	 * this one to connect.) So we release the connection into the
	 * idle pool and then just run the queue and see what happens.
	 */
	soup_connection_release (conn);
	run_queue (sa, FALSE);
}

static gboolean
run_queue (SoupSessionAsync *sa, gboolean try_pruning)
{
	SoupSession *session = SOUP_SESSION (sa);
	SoupMessageQueue *queue = soup_session_get_queue (session);
	SoupMessageQueueIter iter;
	SoupMessage *msg;
	SoupConnection *conn;
	gboolean should_prune = FALSE, started_any = FALSE, is_new;

	/* FIXME: prefer CONNECTING messages */

 try_again:
	for (msg = soup_message_queue_first (queue, &iter);
	     msg;
	     msg = soup_message_queue_next (queue, &iter)) {

		if (!SOUP_MESSAGE_IS_STARTING (msg) ||
		    soup_message_io_in_progress (msg))
			continue;

		conn = soup_session_get_connection (session, msg,
						    &should_prune, &is_new);
		if (!conn)
			continue;

		if (is_new) {
			soup_connection_connect_async (conn, got_connection,
						       session);
		} else
			soup_connection_send_request (conn, msg);

		started_any = TRUE;
	}

	if (try_pruning && should_prune && !started_any) {
		/* We didn't manage to start any message, but there is
		 * at least one message in the queue that could be
		 * sent if we pruned an idle connection from some
		 * other server.
		 */
		if (soup_session_try_prune_connection (session)) {
			try_pruning = FALSE;
			goto try_again;
		}
	}

	return started_any;
}

static void
request_restarted (SoupMessage *req, gpointer sa)
{
	run_queue (sa, FALSE);
}

typedef struct {
	SoupSessionAsync *sa;
	SoupSessionCallback callback;
	gpointer callback_data;
} SoupSessionAsyncQueueData;

static void
final_finished (SoupMessage *req, gpointer user_data)
{
	SoupSessionAsyncQueueData *saqd = user_data;
	SoupSessionAsync *sa = saqd->sa;

	g_object_add_weak_pointer (G_OBJECT (sa), (gpointer)&sa);

	if (!SOUP_MESSAGE_IS_STARTING (req)) {
		g_signal_handlers_disconnect_by_func (req, final_finished, saqd);
		if (saqd->callback) {
			saqd->callback ((SoupSession *)sa, req,
					saqd->callback_data);
			/* callback might destroy sa */
		}

		g_object_unref (req);
		g_slice_free (SoupSessionAsyncQueueData, saqd);
	}

	if (sa) {
		g_object_remove_weak_pointer (G_OBJECT (sa), (gpointer)&sa);
		run_queue (sa, FALSE);
	}
}

static gboolean
idle_run_queue (gpointer user_data)
{
	SoupSessionAsync *sa = user_data;

	g_object_add_weak_pointer (G_OBJECT (sa), (gpointer)&sa);
	g_object_unref (sa);

	if (sa) {
		g_object_remove_weak_pointer (G_OBJECT (sa), (gpointer)&sa);
		run_queue (sa, TRUE);
	}
	return FALSE;
}

static void
queue_message (SoupSession *session, SoupMessage *req,
	       SoupSessionCallback callback, gpointer user_data)
{
	SoupSessionAsync *sa = SOUP_SESSION_ASYNC (session);
	SoupSessionAsyncQueueData *saqd;

	g_signal_connect (req, "restarted",
			  G_CALLBACK (request_restarted), sa);

	saqd = g_slice_new (SoupSessionAsyncQueueData);
	saqd->sa = sa;
	saqd->callback = callback;
	saqd->callback_data = user_data;
	g_signal_connect_after (req, "finished",
				G_CALLBACK (final_finished), saqd);

	SOUP_SESSION_CLASS (soup_session_async_parent_class)->queue_message (session, req, callback, user_data);

	g_object_ref (sa);
	soup_add_idle (soup_session_get_async_context (session),
		       idle_run_queue, sa);
}

static guint
send_message (SoupSession *session, SoupMessage *req)
{
	GMainContext *async_context =
		soup_session_get_async_context (session);

	/* Balance out the unref that final_finished will do */
	g_object_ref (req);

	queue_message (session, req, NULL, NULL);

	while (soup_message_get_io_status (req) != SOUP_MESSAGE_IO_STATUS_FINISHED &&
	       !SOUP_STATUS_IS_TRANSPORT_ERROR (req->status_code))
		g_main_context_iteration (async_context, TRUE);

	return req->status_code;
}
