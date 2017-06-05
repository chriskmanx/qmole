/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2008 Red Hat, Inc.
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include <libsoup/soup.h>

#include "test-utils.h"

GMainLoop *loop;

typedef struct {
	const char *method;
	const char *path;
	guint status_code;
} TestRequest;

struct {
	TestRequest requests[3];
} tests[] = {
	/* A redirecty response to a GET should cause a redirect */

	{ { { "GET", "/301", 301 },
	    { "GET", "/", 200 },
	    { NULL } } },
	{ { { "GET", "/302", 302 },
	    { "GET", "/", 200 },
	    { NULL } } },
	{ { { "GET", "/303", 303 },
	    { "GET", "/", 200 },
	    { NULL } } },
	{ { { "GET", "/307", 307 },
	    { "GET", "/", 200 },
	    { NULL } } },

	/* A non-redirecty response to a GET should not */

	{ { { "GET", "/300", 300 },
	    { NULL } } },
	{ { { "GET", "/304", 304 },
	    { NULL } } },
	{ { { "GET", "/305", 305 },
	    { NULL } } },
	{ { { "GET", "/306", 306 },
	    { NULL } } },
	{ { { "GET", "/308", 308 },
	    { NULL } } },
	
	/* Test double-redirect */

	{ { { "GET", "/301/302", 301 },
	    { "GET", "/302", 302 },
	    { "GET", "/", 200 } } },

	/* POST should only automatically redirect on 302 and 303 */

	{ { { "POST", "/301", 301 },
	    { NULL } } },
	{ { { "POST", "/302", 302 },
	    { "GET", "/", 200 },
	    { NULL } } },
	{ { { "POST", "/303", 303 },
	    { "GET", "/", 200 },
	    { NULL } } },
	{ { { "POST", "/307", 307 },
	    { NULL } } }
};
static const int n_tests = G_N_ELEMENTS (tests);

static void
got_headers (SoupMessage *msg, gpointer user_data)
{
	TestRequest **req = user_data;
	const char *location;

	debug_printf (2, "    -> %d %s\n", msg->status_code,
		      msg->reason_phrase);
	location = soup_message_headers_get (msg->response_headers, "Location");
	if (location)
		debug_printf (2, "       Location: %s\n", location);

	if (!(*req)->method)
		return;

	if (msg->status_code != (*req)->status_code) {
		debug_printf (1, "    - Expected %d !\n",
			      (*req)->status_code);
		errors++;
	}
}

static void
restarted (SoupMessage *msg, gpointer user_data)
{
	TestRequest **req = user_data;
	SoupURI *uri = soup_message_get_uri (msg);

	debug_printf (2, "    %s %s\n", msg->method, uri->path);

	if ((*req)->method)
		(*req)++;

	if (!(*req)->method) {
		debug_printf (1, "    - Expected to be done!\n");
		errors++;
		return;
	}

	if (strcmp (msg->method, (*req)->method) != 0) {
		debug_printf (1, "    - Expected %s !\n", (*req)->method);
		errors++;
	}
	if (strcmp (uri->path, (*req)->path) != 0) {
		debug_printf (1, "    - Expected %s !\n", (*req)->path);
		errors++;
	}
}

static void
do_test (SoupSession *session, SoupURI *base_uri, int n)
{
	SoupURI *uri;
	SoupMessage *msg;
	TestRequest *req;

	debug_printf (1, "%2d. %s %s\n", n + 1,
		      tests[n].requests[0].method,
		      tests[n].requests[0].path);

	uri = soup_uri_new_with_base (base_uri, tests[n].requests[0].path);
	msg = soup_message_new_from_uri (tests[n].requests[0].method, uri);
	soup_uri_free (uri);

	if (msg->method == SOUP_METHOD_POST) {
		soup_message_set_request (msg, "text/plain",
					  SOUP_MEMORY_STATIC,
					  "post body",
					  strlen ("post body"));
	}

	req = &tests[n].requests[0];
	g_signal_connect (msg, "got_headers",
			  G_CALLBACK (got_headers), &req);
	g_signal_connect (msg, "restarted",
			  G_CALLBACK (restarted), &req);

	soup_session_send_message (session, msg);
	g_object_unref (msg);
	debug_printf (2, "\n");
}

static void
do_redirect_tests (SoupURI *base_uri)
{
	SoupSession *session;
	int n;

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	debug_printf (1, "Async session\n");
	for (n = 0; n < n_tests; n++)
		do_test (session, base_uri, n);
	soup_session_abort (session);
	g_object_unref (session);

	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC, NULL);
	debug_printf (1, "Sync session\n");
	for (n = 0; n < n_tests; n++)
		do_test (session, base_uri, n);
	soup_session_abort (session);
	g_object_unref (session);
}

GThread *server_thread;

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	char *remainder;
	guint status_code;

	if (!strcmp (path, "/")) {
		if (msg->method != SOUP_METHOD_GET) {
			soup_message_set_status (msg, SOUP_STATUS_METHOD_NOT_ALLOWED);
			return;
		}

		/* Make sure that redirecting a POST clears the body */
		if (msg->request_body->length) {
			soup_message_set_status (msg, SOUP_STATUS_BAD_REQUEST);
			return;
		}

		soup_message_set_status (msg, SOUP_STATUS_OK);
		soup_message_set_response (msg, "text/plain",
					   SOUP_MEMORY_STATIC,
					   "OK\r\n", 4);
		return;
	}

	status_code = strtoul (path + 1, &remainder, 10);
	if (!SOUP_STATUS_IS_REDIRECTION (status_code) ||
	    (*remainder && *remainder != '/')) {
		soup_message_set_status (msg, SOUP_STATUS_NOT_FOUND);
		return;
	}

	soup_message_set_status (msg, status_code);
	if (*remainder) {
		soup_message_headers_replace (msg->response_headers,
					      "Location", remainder);
	} else {
		soup_message_headers_replace (msg->response_headers,
					      "Location", "/");
	}
}

gboolean run_tests = TRUE;

static GOptionEntry no_test_entry[] = {
        { "no-tests", 'n', G_OPTION_FLAG_NO_ARG | G_OPTION_FLAG_REVERSE,
          G_OPTION_ARG_NONE, &run_tests,
          "Don't run tests, just run the test server", NULL },
        { NULL }
};

int
main (int argc, char **argv)
{
	GMainLoop *loop;
	SoupServer *server;
	guint port;
	SoupURI *base_uri;

	test_init (argc, argv, no_test_entry);

	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, NULL,
				 server_callback, NULL, NULL);
	port = 	soup_server_get_port (server);

	loop = g_main_loop_new (NULL, TRUE);

	if (run_tests) {
		base_uri = soup_uri_new ("http://localhost");
		soup_uri_set_port (base_uri, port);
		do_redirect_tests (base_uri);
		soup_uri_free (base_uri);
	} else {
		printf ("Listening on port %d\n", port);
		g_main_loop_run (loop);
	}

	g_main_loop_unref (loop);

	if (run_tests)
		test_cleanup ();
	return errors != 0;
}
