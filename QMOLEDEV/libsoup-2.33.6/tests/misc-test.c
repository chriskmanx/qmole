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
#include <libsoup/soup.h>

#include "test-utils.h"

SoupServer *server;
SoupURI *base_uri;
GMutex *server_mutex;

static gboolean
auth_callback (SoupAuthDomain *auth_domain, SoupMessage *msg,
	       const char *username, const char *password, gpointer data)
{
	return !strcmp (username, "user") && !strcmp (password, "password");
}

static void
forget_close (SoupMessage *msg, gpointer user_data)
{
	soup_message_headers_remove (msg->response_headers, "Connection");
}

static void
close_socket (SoupMessage *msg, gpointer user_data)
{
	SoupSocket *sock = user_data;

	soup_socket_disconnect (sock);
}

static void
timeout_socket (SoupSocket *sock, gpointer user_data)
{
	soup_socket_disconnect (sock);
}

static void
timeout_request_started (SoupServer *server, SoupMessage *msg,
			 SoupClientContext *client, gpointer user_data)
{
	SoupSocket *sock;
	GMainContext *context = soup_server_get_async_context (server);
	guint readable;

	sock = soup_client_context_get_socket (client);
	readable = g_signal_connect (sock, "readable",
				    G_CALLBACK (timeout_socket), NULL);
	while (soup_socket_is_connected (sock))
		g_main_context_iteration (context, TRUE);
	g_signal_handler_disconnect (sock, readable);
	g_signal_handlers_disconnect_by_func (server, timeout_request_started, NULL);
}

static void
setup_timeout_persistent (SoupServer *server, SoupSocket *sock)
{
	char buf[1];
	gsize nread;

	/* In order for the test to work correctly, we have to
	 * close the connection *after* the client side writes
	 * the request. To ensure that this happens reliably,
	 * regardless of thread scheduling, we:
	 *
	 *   1. Try to read off the socket now, knowing it will
	 *      fail (since the client is waiting for us to
	 *      return a response). This will cause it to
	 *      emit "readable" later.
	 *   2. Connect to the server's request-started signal.
	 *   3. Run an inner main loop from that signal handler
	 *      until the socket emits "readable". (If we don't
	 *      do this then it's possible the client's next
	 *      request would be ready before we returned to
	 *      the main loop, and so the signal would never be
	 *      emitted.)
	 *   4. Close the socket.
	 */

	soup_socket_read (sock, buf, 1, &nread, NULL, NULL);
	g_signal_connect (server, "request-started",
			  G_CALLBACK (timeout_request_started), NULL);
}

static gboolean
timeout_finish_message (gpointer msg)
{
	SoupServer *server = g_object_get_data (G_OBJECT (msg), "server");

	soup_server_unpause_message (server, msg);
	return FALSE;
}

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	SoupURI *uri = soup_message_get_uri (msg);

	/* The way this gets used in the tests, we don't actually
	 * need to hold it through the whole function, so it's simpler
	 * to just release it right away.
	 */
	g_mutex_lock (server_mutex);
	g_mutex_unlock (server_mutex);

	soup_message_headers_append (msg->response_headers,
				     "X-Handled-By", "server_callback");

	if (!strcmp (path, "*")) {
		debug_printf (1, "    default server_callback got request for '*'!\n");
		errors++;
		soup_message_set_status (msg, SOUP_STATUS_INTERNAL_SERVER_ERROR);
		return;
	}

	if (msg->method != SOUP_METHOD_GET) {
		soup_message_set_status (msg, SOUP_STATUS_NOT_IMPLEMENTED);
		return;
	}

	if (!strcmp (path, "/redirect")) {
		soup_message_set_status (msg, SOUP_STATUS_FOUND);
		soup_message_headers_append (msg->response_headers,
					     /* Kids: don't try this at home!
					      * RFC2616 says to use an
					      * absolute URI!
					      */
					     "Location", "/");
		return;
	}

	if (g_str_has_prefix (path, "/content-length/")) {
		gboolean too_long = strcmp (path, "/content-length/long") == 0;
		gboolean no_close = strcmp (path, "/content-length/noclose") == 0;

		soup_message_set_status (msg, SOUP_STATUS_OK);
		soup_message_set_response (msg, "text/plain",
					   SOUP_MEMORY_STATIC, "foobar", 6);
		if (too_long)
			soup_message_headers_set_content_length (msg->response_headers, 9);
		soup_message_headers_append (msg->response_headers,
					     "Connection", "close");

		if (too_long) {
			SoupSocket *sock;

			/* soup-message-io will wait for us to add
			 * another chunk after the first, to fill out
			 * the declared Content-Length. Instead, we
			 * forcibly close the socket at that point.
			 */
			sock = soup_client_context_get_socket (context);
			g_signal_connect (msg, "wrote-chunk",
					  G_CALLBACK (close_socket), sock);
		} else if (no_close) {
			/* Remove the 'Connection: close' after writing
			 * the headers, so that when we check it after
			 * writing the body, we'll think we aren't
			 * supposed to close it.
			 */
			g_signal_connect (msg, "wrote-headers",
					  G_CALLBACK (forget_close), NULL);
		}
		return;
	}

	if (!strcmp (path, "/timeout-persistent")) {
		SoupSocket *sock;

		sock = soup_client_context_get_socket (context);
		setup_timeout_persistent (server, sock);
	}

	if (!strcmp (path, "/slow")) {
		soup_server_pause_message (server, msg);
		g_object_set_data (G_OBJECT (msg), "server", server);
		soup_add_timeout (soup_server_get_async_context (server),
				  1000, timeout_finish_message, msg);
	}

	soup_message_set_status (msg, SOUP_STATUS_OK);
	if (!strcmp (uri->host, "foo")) {
		soup_message_set_response (msg, "text/plain",
					   SOUP_MEMORY_STATIC, "foo-index", 9);
		return;
	} else {
		soup_message_set_response (msg, "text/plain",
					   SOUP_MEMORY_STATIC, "index", 5);
		return;
	}
}

static void
server_star_callback (SoupServer *server, SoupMessage *msg,
		      const char *path, GHashTable *query,
		      SoupClientContext *context, gpointer data)
{
	soup_message_headers_append (msg->response_headers,
				     "X-Handled-By", "star_callback");

	if (strcmp (path, "*") != 0) {
		debug_printf (1, "    server_star_callback got request for '%s'!\n", path);
		errors++;
		soup_message_set_status (msg, SOUP_STATUS_INTERNAL_SERVER_ERROR);
		return;
	}

	if (msg->method != SOUP_METHOD_OPTIONS) {
		soup_message_set_status (msg, SOUP_STATUS_METHOD_NOT_ALLOWED);
		return;
	}

	soup_message_set_status (msg, SOUP_STATUS_OK);
}

/* Host header handling: client must be able to override the default
 * value, server must be able to recognize different Host values.
 * #539803.
 */
static void
do_host_test (void)
{
	SoupSession *session;
	SoupMessage *one, *two;

	debug_printf (1, "Host handling\n");

	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC, NULL);

	one = soup_message_new_from_uri ("GET", base_uri);
	two = soup_message_new_from_uri ("GET", base_uri);
	soup_message_headers_replace (two->request_headers, "Host", "foo");

	soup_session_send_message (session, one);
	soup_session_send_message (session, two);

	soup_test_session_abort_unref (session);

	if (!SOUP_STATUS_IS_SUCCESSFUL (one->status_code)) {
		debug_printf (1, "  Message 1 failed: %d %s\n",
			      one->status_code, one->reason_phrase);
		errors++;
	} else if (strcmp (one->response_body->data, "index") != 0) {
		debug_printf (1, "  Unexpected response to message 1: '%s'\n",
			      one->response_body->data);
		errors++;
	}
	g_object_unref (one);

	if (!SOUP_STATUS_IS_SUCCESSFUL (two->status_code)) {
		debug_printf (1, "  Message 2 failed: %d %s\n",
			      two->status_code, two->reason_phrase);
		errors++;
	} else if (strcmp (two->response_body->data, "foo-index") != 0) {
		debug_printf (1, "  Unexpected response to message 2: '%s'\n",
			      two->response_body->data);
		errors++;
	}
	g_object_unref (two);
}

/* Dropping the application's ref on the session from a callback
 * should not cause the session to be freed at an incorrect time.
 * (This test will crash if it fails.) #533473
 */
static void
cu_one_completed (SoupSession *session, SoupMessage *msg, gpointer loop)
{
	debug_printf (2, "  Message 1 completed\n");
	if (msg->status_code != SOUP_STATUS_CANT_CONNECT) {
		debug_printf (1, "  Unexpected status on Message 1: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_object_unref (session);
}

static gboolean
cu_idle_quit (gpointer loop)
{
	g_main_loop_quit (loop);
	return FALSE;
}

static void
cu_two_completed (SoupSession *session, SoupMessage *msg, gpointer loop)
{
	debug_printf (2, "  Message 2 completed\n");
	if (msg->status_code != SOUP_STATUS_CANT_CONNECT) {
		debug_printf (1, "  Unexpected status on Message 2: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_idle_add (cu_idle_quit, loop); 
}

static void
do_callback_unref_test (void)
{
	SoupServer *bad_server;
	SoupAddress *addr;
	SoupSession *session;
	SoupMessage *one, *two;
	GMainLoop *loop;
	char *bad_uri;

	debug_printf (1, "\nCallback unref handling\n");

	/* Get a guaranteed-bad URI */
	addr = soup_address_new ("127.0.0.1", SOUP_ADDRESS_ANY_PORT);
	soup_address_resolve_sync (addr, NULL);
	bad_server = soup_server_new (SOUP_SERVER_INTERFACE, addr,
				      NULL);
	g_object_unref (addr);

	bad_uri = g_strdup_printf ("http://127.0.0.1:%u/",
				   soup_server_get_port (bad_server));
	g_object_unref (bad_server);

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	g_object_add_weak_pointer (G_OBJECT (session), (gpointer *)&session);

	loop = g_main_loop_new (NULL, TRUE);

	one = soup_message_new ("GET", bad_uri);
	g_object_add_weak_pointer (G_OBJECT (one), (gpointer *)&one);
	two = soup_message_new ("GET", bad_uri);
	g_object_add_weak_pointer (G_OBJECT (two), (gpointer *)&two);
	g_free (bad_uri);

	soup_session_queue_message (session, one, cu_one_completed, loop);
	soup_session_queue_message (session, two, cu_two_completed, loop);

	g_main_loop_run (loop);
	g_main_loop_unref (loop);

	if (session) {
		g_object_remove_weak_pointer (G_OBJECT (session), (gpointer *)&session);
		debug_printf (1, "  Session not destroyed?\n");
		errors++;
		g_object_unref (session);
	}
	if (one) {
		g_object_remove_weak_pointer (G_OBJECT (one), (gpointer *)&one);
		debug_printf (1, "  Message 1 not destroyed?\n");
		errors++;
		g_object_unref (one);
	}
	if (two) {
		g_object_remove_weak_pointer (G_OBJECT (two), (gpointer *)&two);
		debug_printf (1, "  Message 2 not destroyed?\n");
		errors++;
		g_object_unref (two);
	}

	/* Otherwise, if we haven't crashed, we're ok. */
}

/* SoupSession should clean up all signal handlers on a message after
 * it is finished, allowing the message to be reused if desired.
 * #559054
 */
static void
ensure_no_signal_handlers (SoupMessage *msg, guint *signal_ids, guint n_signal_ids)
{
	int i;

	for (i = 0; i < n_signal_ids; i++) {
		if (g_signal_handler_find (msg, G_SIGNAL_MATCH_ID, signal_ids[i],
					   0, NULL, NULL, NULL)) {
			debug_printf (1, "    Message has handler for '%s'\n",
				      g_signal_name (signal_ids[i]));
			errors++;
		}
	}
}

static void
reuse_test_authenticate (SoupSession *session, SoupMessage *msg,
			 SoupAuth *auth, gboolean retrying)
{
	/* Get it wrong the first time, then succeed */
	if (!retrying)
		soup_auth_authenticate (auth, "user", "wrong password");
	else
		soup_auth_authenticate (auth, "user", "password");
}

static void
do_msg_reuse_test (void)
{
	SoupSession *session;
	SoupMessage *msg;
	SoupURI *uri;
	guint *signal_ids, n_signal_ids;

	debug_printf (1, "\nSoupMessage reuse\n");

	signal_ids = g_signal_list_ids (SOUP_TYPE_MESSAGE, &n_signal_ids);

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	g_signal_connect (session, "authenticate",
			  G_CALLBACK (reuse_test_authenticate), NULL);

	debug_printf (1, "  First message\n");
	msg = soup_message_new_from_uri ("GET", base_uri);
	soup_session_send_message (session, msg);
	ensure_no_signal_handlers (msg, signal_ids, n_signal_ids);

	debug_printf (1, "  Redirect message\n");
	uri = soup_uri_new_with_base (base_uri, "/redirect");
	soup_message_set_uri (msg, uri);
	soup_uri_free (uri);
	soup_session_send_message (session, msg);
	if (!soup_uri_equal (soup_message_get_uri (msg), base_uri)) {
		debug_printf (1, "    Message did not get redirected!\n");
		errors++;
	}
	ensure_no_signal_handlers (msg, signal_ids, n_signal_ids);

	debug_printf (1, "  Auth message\n");
	uri = soup_uri_new_with_base (base_uri, "/auth");
	soup_message_set_uri (msg, uri);
	soup_uri_free (uri);
	soup_session_send_message (session, msg);
	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "    Message did not get authenticated!\n");
		errors++;
	}
	ensure_no_signal_handlers (msg, signal_ids, n_signal_ids);

	/* One last try to make sure the auth stuff got cleaned up */
	debug_printf (1, "  Last message\n");
	soup_message_set_uri (msg, base_uri);
	soup_session_send_message (session, msg);
	ensure_no_signal_handlers (msg, signal_ids, n_signal_ids);

	soup_test_session_abort_unref (session);
	g_object_unref (msg);
	g_free (signal_ids);
}

/* Server handlers for "*" work but are separate from handlers for
 * all other URIs. #590751
 */
static void
do_star_test (void)
{
	SoupSession *session;
	SoupMessage *msg;
	SoupURI *star_uri;
	const char *handled_by;

	debug_printf (1, "\nOPTIONS *\n");

	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC, NULL);
	star_uri = soup_uri_copy (base_uri);
	soup_uri_set_path (star_uri, "*");

	debug_printf (1, "  Testing with no handler\n");
	msg = soup_message_new_from_uri ("OPTIONS", star_uri);
	soup_session_send_message (session, msg);

	if (msg->status_code != SOUP_STATUS_NOT_FOUND) {
		debug_printf (1, "    Unexpected response: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	handled_by = soup_message_headers_get_one (msg->response_headers,
						   "X-Handled-By");
	if (handled_by) {
		/* Should have been rejected by SoupServer directly */
		debug_printf (1, "    Message reached handler '%s'\n",
			      handled_by);
		errors++;
	}
	g_object_unref (msg);

	soup_server_add_handler (server, "*", server_star_callback, NULL, NULL);

	debug_printf (1, "  Testing with handler\n");
	msg = soup_message_new_from_uri ("OPTIONS", star_uri);
	soup_session_send_message (session, msg);

	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "    Unexpected response: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	handled_by = soup_message_headers_get_one (msg->response_headers,
						   "X-Handled-By");
	if (!handled_by) {
		debug_printf (1, "    Message did not reach handler!\n");
		errors++;
	} else if (strcmp (handled_by, "star_callback") != 0) {
		debug_printf (1, "    Message reached incorrect handler '%s'\n",
			      handled_by);
		errors++;
	}
	g_object_unref (msg);

	soup_test_session_abort_unref (session);
	soup_uri_free (star_uri);
}

/* Handle unexpectedly-early aborts. #596074, #618641 */
static void
ea_msg_completed_one (SoupSession *session, SoupMessage *msg, gpointer loop)
{
	debug_printf (2, "  Message 1 completed\n");
	if (msg->status_code != SOUP_STATUS_CANCELLED) {
		debug_printf (1, "  Unexpected status on Message 1: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_main_loop_quit (loop);
}

static gboolean
ea_abort_session (gpointer session)
{
	soup_session_abort (session);
	return FALSE;
}

static void
ea_connection_state_changed (GObject *conn, GParamSpec *pspec, gpointer session)
{
	SoupConnectionState state;

	g_object_get (conn, "state", &state, NULL);
	if (state == SOUP_CONNECTION_CONNECTING) {
		g_idle_add_full (G_PRIORITY_HIGH,
				 ea_abort_session,
				 session, NULL);
		g_signal_handlers_disconnect_by_func (conn, ea_connection_state_changed, session);
	}
}		

static void
ea_connection_created (SoupSession *session, GObject *conn, gpointer user_data)
{
	g_signal_connect (conn, "notify::state",
			  G_CALLBACK (ea_connection_state_changed), session);
	g_signal_handlers_disconnect_by_func (session, ea_connection_created, user_data);
}

static void
do_early_abort_test (void)
{
	SoupSession *session;
	SoupMessage *msg;
	GMainContext *context;
	GMainLoop *loop;

	debug_printf (1, "\nAbort with pending connection\n");

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	msg = soup_message_new_from_uri ("GET", base_uri);

	context = g_main_context_default ();
	loop = g_main_loop_new (context, TRUE);
	soup_session_queue_message (session, msg, ea_msg_completed_one, loop);
	g_main_context_iteration (context, FALSE);

	soup_session_abort (session);
	while (g_main_context_pending (context))
		g_main_context_iteration (context, FALSE);
	g_main_loop_unref (loop);
	soup_test_session_abort_unref (session);

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	msg = soup_message_new_from_uri ("GET", base_uri);

	g_signal_connect (session, "connection-created",
			  G_CALLBACK (ea_connection_created), NULL);
	soup_session_send_message (session, msg);
	debug_printf (2, "  Message 2 completed\n");

	if (msg->status_code != SOUP_STATUS_CANCELLED) {
		debug_printf (1, "    Unexpected response: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_object_unref (msg);

	while (g_main_context_pending (context))
		g_main_context_iteration (context, FALSE);

	soup_test_session_abort_unref (session);
}

static void
do_content_length_framing_test (void)
{
	SoupSession *session;
	SoupMessage *msg;
	SoupURI *request_uri;
	goffset declared_length;

	debug_printf (1, "\nInvalid Content-Length framing tests\n");

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);

	debug_printf (1, "  Content-Length larger than message body length\n");
	request_uri = soup_uri_new_with_base (base_uri, "/content-length/long");
	msg = soup_message_new_from_uri ("GET", request_uri);
	soup_session_send_message (session, msg);
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "    Unexpected response: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	} else {
		declared_length = soup_message_headers_get_content_length (msg->response_headers);
		debug_printf (2, "    Content-Length: %lu, body: %s\n",
			      (gulong)declared_length, msg->response_body->data);
		if (msg->response_body->length >= declared_length) {
			debug_printf (1, "    Body length %lu >= declared length %lu\n",
				      (gulong)msg->response_body->length,
				      (gulong)declared_length);
			errors++;
		}
	}
	soup_uri_free (request_uri);
	g_object_unref (msg);

	debug_printf (1, "  Server claims 'Connection: close' but doesn't\n");
	request_uri = soup_uri_new_with_base (base_uri, "/content-length/noclose");
	msg = soup_message_new_from_uri ("GET", request_uri);
	soup_session_send_message (session, msg);
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "    Unexpected response: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	} else {
		declared_length = soup_message_headers_get_content_length (msg->response_headers);
		debug_printf (2, "    Content-Length: %lu, body: %s\n",
			      (gulong)declared_length, msg->response_body->data);
		if (msg->response_body->length != declared_length) {
			debug_printf (1, "    Body length %lu != declared length %lu\n",
				      (gulong)msg->response_body->length,
				      (gulong)declared_length);
			errors++;
		}
	}
	soup_uri_free (request_uri);
	g_object_unref (msg);

	soup_test_session_abort_unref (session);
}

static void
do_one_accept_language_test (const char *language, const char *expected_header)
{
	SoupSession *session;
	SoupMessage *msg;
	const char *val;

	debug_printf (1, "  LANGUAGE=%s\n", language);
	g_setenv ("LANGUAGE", language, TRUE);
	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC,
					 SOUP_SESSION_ACCEPT_LANGUAGE_AUTO, TRUE,
					 NULL);
	msg = soup_message_new_from_uri ("GET", base_uri);
	soup_session_send_message (session, msg);
	soup_test_session_abort_unref (session);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "    Message failed? %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	val = soup_message_headers_get_list (msg->request_headers,
					     "Accept-Language");
	if (!val) {
		debug_printf (1, "    No Accept-Language set!\n");
		errors++;
	} else if (strcmp (val, expected_header) != 0) {
		debug_printf (1, "    Wrong Accept-Language: expected '%s', got '%s'\n",
			      expected_header, val);
		errors++;
	}

	g_object_unref (msg);
}

static void
do_accept_language_test (void)
{
	const char *orig_language;

	debug_printf (1, "\nAutomatic Accept-Language processing\n");

	orig_language = g_getenv ("LANGUAGE");
	do_one_accept_language_test ("C", "en");
	do_one_accept_language_test ("fr_FR", "fr-fr, fr;q=0.9");
	do_one_accept_language_test ("fr_FR:de:en_US", "fr-fr, fr;q=0.9, de;q=0.8, en-us;q=0.7, en;q=0.6");

	if (orig_language)
		g_setenv ("LANGUAGE", orig_language, TRUE);
	else
		g_unsetenv ("LANGUAGE");
}

static void
timeout_test_request_started (SoupSession *session, SoupMessage *msg,
			      SoupSocket *socket, gpointer user_data)
{
	SoupSocket **sockets = user_data;
	int i;

	debug_printf (2, "      msg %p => socket %p\n", msg, socket);
	for (i = 0; i < 4; i++) {
		if (!sockets[i]) {
			/* We ref the socket to make sure that even if
			 * it gets disconnected, it doesn't get freed,
			 * since our checks would get messed up if the
			 * slice allocator reused the same address for
			 * two consecutive sockets.
			 */
			sockets[i] = g_object_ref (socket);
			return;
		}
	}

	debug_printf (1, "      socket queue overflowed!\n");
	errors++;
	soup_session_cancel_message (session, msg, SOUP_STATUS_CANCELLED);
}

static void
do_timeout_test_for_session (SoupSession *session)
{
	SoupMessage *msg;
	SoupSocket *sockets[4] = { NULL, NULL, NULL, NULL };
	SoupURI *timeout_uri;
	int i;

	g_signal_connect (session, "request-started",
			  G_CALLBACK (timeout_test_request_started),
			  &sockets);

	debug_printf (1, "    First message\n");
	timeout_uri = soup_uri_new_with_base (base_uri, "/timeout-persistent");
	msg = soup_message_new_from_uri ("GET", timeout_uri);
	soup_uri_free (timeout_uri);
	soup_session_send_message (session, msg);
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "      Unexpected response: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	if (sockets[1]) {
		debug_printf (1, "      Message was retried??\n");
		errors++;
		sockets[1] = sockets[2] = sockets[3] = NULL;
	}
	g_object_unref (msg);

	debug_printf (1, "    Second message\n");
	msg = soup_message_new_from_uri ("GET", base_uri);
	soup_session_send_message (session, msg);
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "      Unexpected response: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	if (sockets[1] != sockets[0]) {
		debug_printf (1, "      Message was not retried on existing connection\n");
		errors++;
	} else if (!sockets[2]) {
		debug_printf (1, "      Message was not retried after disconnect\n");
		errors++;
	} else if (sockets[2] == sockets[1]) {
		debug_printf (1, "      Message was retried on closed connection??\n");
		errors++;
	} else if (sockets[3]) {
		debug_printf (1, "      Message was retried again??\n");
		errors++;
	}
	g_object_unref (msg);

	for (i = 0; sockets[i]; i++)
		g_object_unref (sockets[i]);
}

static void
do_persistent_connection_timeout_test (void)
{
	SoupSession *session;

	debug_printf (1, "\nUnexpected timing out of persistent connections\n");

	debug_printf (1, "  Async session\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	do_timeout_test_for_session (session);
	soup_test_session_abort_unref (session);

	debug_printf (1, "  Sync session\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC, NULL);
	do_timeout_test_for_session (session);
	soup_test_session_abort_unref (session);
}

static GMainLoop *max_conns_loop;
static int msgs_done;
#define MAX_CONNS 2
#define TEST_CONNS (MAX_CONNS * 2)

static gboolean
idle_start_server (gpointer data)
{
	g_mutex_unlock (server_mutex);
	return FALSE;
}

static gboolean
quit_loop (gpointer data)
{
	g_main_loop_quit (max_conns_loop);
	return FALSE;
}

static void
max_conns_request_started (SoupSession *session, SoupMessage *msg,
			   SoupSocket *socket, gpointer user_data)
{
	if (++msgs_done == MAX_CONNS)
		g_timeout_add (100, quit_loop, NULL);
}

static void
max_conns_message_complete (SoupSession *session, SoupMessage *msg, gpointer user_data)
{
	if (++msgs_done == TEST_CONNS)
		g_main_loop_quit (max_conns_loop);
}

static void
do_max_conns_test_for_session (SoupSession *session)
{
	SoupMessage *msgs[TEST_CONNS];
	int i;
	guint timeout_id;

	max_conns_loop = g_main_loop_new (NULL, TRUE);

	g_mutex_lock (server_mutex);

	g_signal_connect (session, "request-started",
			  G_CALLBACK (max_conns_request_started), NULL);
	msgs_done = 0;
	for (i = 0; i < TEST_CONNS; i++) {
		msgs[i] = soup_message_new_from_uri ("GET", base_uri);
		g_object_ref (msgs[i]);
		soup_session_queue_message (session, msgs[i],
					    max_conns_message_complete, NULL);
	}

	g_main_loop_run (max_conns_loop);
	if (msgs_done != MAX_CONNS) {
		debug_printf (1, "  Queued %d connections out of max %d?",
			      msgs_done, MAX_CONNS);
		errors++;
	}
	g_signal_handlers_disconnect_by_func (session, max_conns_request_started, NULL);

	msgs_done = 0;
	g_idle_add (idle_start_server, NULL);
	timeout_id = g_timeout_add (1000, quit_loop, NULL);
	g_main_loop_run (max_conns_loop);

	for (i = 0; i < TEST_CONNS; i++) {
		if (!SOUP_STATUS_IS_SUCCESSFUL (msgs[i]->status_code)) {
			debug_printf (1, "    Message %d failed? %d %s\n",
				      i, msgs[i]->status_code,
				      msgs[i]->reason_phrase ? msgs[i]->reason_phrase : "-");
			errors++;
		}
	}

	if (msgs_done != TEST_CONNS) {
		/* Clean up so we don't get a spurious "Leaked
		 * session" error.
		 */
		for (i = 0; i < TEST_CONNS; i++)
			soup_session_cancel_message (session, msgs[i], SOUP_STATUS_CANCELLED);
		g_main_loop_run (max_conns_loop);
		g_source_remove (timeout_id);
	}

	g_main_loop_unref (max_conns_loop);

	for (i = 0; i < TEST_CONNS; i++)
		g_object_unref (msgs[i]);
}

static void
do_max_conns_test (void)
{
	SoupSession *session;

	debug_printf (1, "\nExceeding max-conns\n");

	debug_printf (1, "  Async session\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC,
					 SOUP_SESSION_MAX_CONNS, MAX_CONNS,
					 NULL);
	do_max_conns_test_for_session (session);
	soup_test_session_abort_unref (session);

	debug_printf (1, "  Sync session\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC,
					 SOUP_SESSION_MAX_CONNS, MAX_CONNS,
					 NULL);
	do_max_conns_test_for_session (session);
	soup_test_session_abort_unref (session);
}

static gboolean
cancel_message_timeout (gpointer msg)
{
	SoupSession *session = g_object_get_data (G_OBJECT (msg), "session");

	soup_session_cancel_message (session, msg, SOUP_STATUS_CANCELLED);
	g_object_unref (msg);
	g_object_unref (session);
	return FALSE;
}

static gpointer
cancel_message_thread (gpointer msg)
{
	SoupSession *session = g_object_get_data (G_OBJECT (msg), "session");

	g_usleep (100000); /* .1s */
	soup_session_cancel_message (session, msg, SOUP_STATUS_CANCELLED);
	g_object_unref (msg);
	g_object_unref (session);
	return NULL;
}

static void
do_cancel_while_reading_test_for_session (SoupSession *session)
{
	SoupMessage *msg;
	GThread *thread = NULL;
	SoupURI *uri;

	uri = soup_uri_new_with_base (base_uri, "/slow");
	msg = soup_message_new_from_uri ("GET", uri);
	soup_uri_free (uri);

	g_object_set_data (G_OBJECT (msg), "session", session);
	g_object_ref (msg);
	g_object_ref (session);
	if (SOUP_IS_SESSION_ASYNC (session))
		g_timeout_add (100, cancel_message_timeout, msg);
	else
		thread = g_thread_create (cancel_message_thread, msg, TRUE, NULL);

	soup_session_send_message (session, msg);

	if (msg->status_code != SOUP_STATUS_CANCELLED) {
		debug_printf (1, "      FAILED: %d %s (expected Cancelled)\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	g_object_unref (msg);

	if (thread)
		g_thread_join (thread);
}

static void
do_cancel_while_reading_test (void)
{
	SoupSession *session;

	debug_printf (1, "\nCancelling message while reading response\n");

	debug_printf (1, "  Async session\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	do_cancel_while_reading_test_for_session (session);
	soup_test_session_abort_unref (session);

	debug_printf (1, "  Sync session\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC, NULL);
	do_cancel_while_reading_test_for_session (session);
	soup_test_session_abort_unref (session);
}

int
main (int argc, char **argv)
{
	SoupAuthDomain *auth_domain;

	test_init (argc, argv, NULL);

	server_mutex = g_mutex_new ();

	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, NULL, server_callback, NULL, NULL);
	base_uri = soup_uri_new ("http://127.0.0.1/");
	soup_uri_set_port (base_uri, soup_server_get_port (server));

	auth_domain = soup_auth_domain_basic_new (
		SOUP_AUTH_DOMAIN_REALM, "misc-test",
		SOUP_AUTH_DOMAIN_ADD_PATH, "/auth",
		SOUP_AUTH_DOMAIN_BASIC_AUTH_CALLBACK, auth_callback,
		NULL);
	soup_server_add_auth_domain (server, auth_domain);
	g_object_unref (auth_domain);

	do_host_test ();
	do_callback_unref_test ();
	do_msg_reuse_test ();
	do_star_test ();
	do_early_abort_test ();
	do_content_length_framing_test ();
	do_accept_language_test ();
	do_persistent_connection_timeout_test ();
	do_max_conns_test ();
	do_cancel_while_reading_test ();

	soup_uri_free (base_uri);
	soup_test_server_quit_unref (server);

	test_cleanup ();
	return errors != 0;
}
