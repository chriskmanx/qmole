/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2007 Red Hat, Inc.
 */

#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>
#include <libsoup/soup-address.h>
#include <libsoup/soup-message.h>
#include <libsoup/soup-misc.h>
#include <libsoup/soup-server.h>
#include <libsoup/soup-session-async.h>
#include <libsoup/soup-session-sync.h>

#include "test-utils.h"

char *base_uri;

typedef struct {
	SoupServer *server;
	SoupMessage *msg;
	GSource *timeout;
} SlowData;

static void
request_failed (SoupMessage *msg, gpointer data)
{
	SlowData *sd = data;

	if (SOUP_STATUS_IS_TRANSPORT_ERROR (msg->status_code))
		g_source_destroy (sd->timeout);
	g_free (sd);
}

static gboolean
add_body_chunk (gpointer data)
{
	SlowData *sd = data;

	soup_message_body_append (sd->msg->response_body,
				  SOUP_MEMORY_STATIC, "OK\r\n", 4);
	soup_message_body_complete (sd->msg->response_body);
	soup_server_unpause_message (sd->server, sd->msg);
	g_object_unref (sd->msg);

	return FALSE;
}

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	SlowData *sd;

	if (msg->method != SOUP_METHOD_GET) {
		soup_message_set_status (msg, SOUP_STATUS_NOT_IMPLEMENTED);
		return;
	}

	soup_message_set_status (msg, SOUP_STATUS_OK);
	if (!strcmp (path, "/fast")) {
		soup_message_set_response (msg, "text/plain",
					   SOUP_MEMORY_STATIC, "OK\r\n", 4);
		return;
	}

	soup_message_headers_set_encoding (msg->response_headers,
					   SOUP_ENCODING_CHUNKED);
	g_object_ref (msg);
	soup_server_pause_message (server, msg);

	sd = g_new (SlowData, 1);
	sd->server = server;
	sd->msg = msg;
	sd->timeout = soup_add_timeout (
		soup_server_get_async_context (server),
		200, add_body_chunk, sd);
	g_signal_connect (msg, "finished",
			  G_CALLBACK (request_failed), sd);
}

/* Test 1: An async session in another thread with its own
 * async_context can complete a request while the main thread's main
 * loop is stopped.
 */

static gboolean idle_start_test1_thread (gpointer loop);
static gpointer test1_thread (gpointer user_data);

GCond *test1_cond;
GMutex *test1_mutex;

static void
do_test1 (void)
{
	GMainLoop *loop;

	debug_printf (1, "Test 1: blocking the main thread does not block other thread\n");

	test1_cond = g_cond_new ();
	test1_mutex = g_mutex_new ();

	loop = g_main_loop_new (NULL, FALSE);
	g_idle_add (idle_start_test1_thread, loop);
	g_main_loop_run (loop);
	g_main_loop_unref (loop);

	g_mutex_free (test1_mutex);
	g_cond_free (test1_cond);
}

static gboolean
idle_start_test1_thread (gpointer loop)
{
	GTimeVal time;
	GThread *thread;

	g_mutex_lock (test1_mutex);
	thread = g_thread_create (test1_thread, base_uri, TRUE, NULL);

	g_get_current_time (&time);
	time.tv_sec += 5;
	if (g_cond_timed_wait (test1_cond, test1_mutex, &time))
		g_thread_join (thread);
	else {
		debug_printf (1, "  timeout!\n");
		errors++;
	}

	g_mutex_unlock (test1_mutex);
	g_main_loop_quit (loop);
	return FALSE;
}

static void
test1_finished (SoupSession *session, SoupMessage *msg, gpointer loop)
{
	g_main_loop_quit (loop);
}

static gpointer
test1_thread (gpointer user_data)
{
	SoupSession *session;
	GMainContext *async_context;
	char *uri;
	SoupMessage *msg;
	GMainLoop *loop;

	/* Wait for main thread to be waiting on test1_cond */
	g_mutex_lock (test1_mutex);
	g_mutex_unlock (test1_mutex);

	async_context = g_main_context_new ();
	session = soup_test_session_new (
		SOUP_TYPE_SESSION_ASYNC,
		SOUP_SESSION_ASYNC_CONTEXT, async_context,
		NULL);
	g_main_context_unref (async_context);

	uri = g_build_filename (base_uri, "slow", NULL);

	debug_printf (1, "  send_message\n");
	msg = soup_message_new ("GET", uri);
	soup_session_send_message (session, msg);
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "    unexpected status: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_object_unref (msg);

	debug_printf (1, "  queue_message\n");
	msg = soup_message_new ("GET", uri);
	loop = g_main_loop_new (async_context, FALSE);
	g_object_ref (msg);
	soup_session_queue_message (session, msg, test1_finished, loop);
	g_main_loop_run (loop);
	g_main_loop_unref (loop);
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "    unexpected status: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_object_unref (msg);

	soup_session_abort (session);
	g_object_unref (session);
	g_free (uri);

	g_cond_signal (test1_cond);
	return NULL;
}

/* Test 2: An async session in the main thread with its own
 * async_context runs independently of the default main loop.
 */

static gboolean idle_test2_fail (gpointer user_data);

static void
do_test2 (void)
{
	guint idle;
	GMainContext *async_context;
	SoupSession *session;
	char *uri;
	SoupMessage *msg;

	debug_printf (1, "Test 2: a session with its own context is independent of the main loop.\n");

	idle = g_idle_add_full (G_PRIORITY_HIGH, idle_test2_fail, NULL, NULL);

	async_context = g_main_context_new ();
	session = soup_test_session_new (
		SOUP_TYPE_SESSION_ASYNC,
		SOUP_SESSION_ASYNC_CONTEXT, async_context,
		NULL);
	g_main_context_unref (async_context);

	uri = g_build_filename (base_uri, "slow", NULL);

	debug_printf (1, "  send_message\n");
	msg = soup_message_new ("GET", uri);
	soup_session_send_message (session, msg);
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "    unexpected status: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_object_unref (msg);

	soup_session_abort (session);
	g_object_unref (session);
	g_free (uri);

	g_source_remove (idle);
}

static gboolean
idle_test2_fail (gpointer user_data)
{
	debug_printf (1, "  idle ran!\n");
	errors++;
	return FALSE;
}


int
main (int argc, char **argv)
{
	SoupServer *server;

	test_init (argc, argv, NULL);

	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, NULL, server_callback, NULL, NULL);
	base_uri = g_strdup_printf ("http://localhost:%u/",
				    soup_server_get_port (server));

	do_test1 ();
	do_test2 ();

	g_free (base_uri);

	test_cleanup ();
	return errors != 0;
}
