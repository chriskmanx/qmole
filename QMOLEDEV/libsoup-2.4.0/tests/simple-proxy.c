/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2001-2003, Ximian, Inc.
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
#include <libsoup/soup-server.h>
#include <libsoup/soup-session-async.h>

/* WARNING: this is really really really not especially compliant with
 * RFC 2616. But it does work for basic stuff.
 */

SoupSession *session;
SoupServer *server;

static void
copy_header (const char *name, const char *value, gpointer dest_headers)
{
	soup_message_headers_append (dest_headers, name, value);
}

static void
send_headers (SoupMessage *from, SoupMessage *to)
{
	printf ("[%p] HTTP/1.%d %d %s\n", to,
		soup_message_get_http_version (from),
		from->status_code, from->reason_phrase);

	soup_message_set_status_full (to, from->status_code,
				      from->reason_phrase);
	soup_message_headers_foreach (from->response_headers, copy_header,
				      to->response_headers);
	soup_message_headers_remove (to->response_headers, "Content-Length");
	soup_server_unpause_message (server, to);
}

static void
send_chunk (SoupMessage *from, SoupBuffer *chunk, SoupMessage *to)
{
	printf ("[%p]   writing chunk of %lu bytes\n", to,
		(unsigned long)chunk->length);

	soup_message_body_append_buffer (to->response_body, chunk);
	soup_server_unpause_message (server, to);
}

static void
client_msg_failed (SoupMessage *msg, gpointer msg2)
{
	soup_session_cancel_message (session, msg2, SOUP_STATUS_IO_ERROR);
}

static void
finish_msg (SoupSession *session, SoupMessage *msg2, gpointer data)
{
	SoupMessage *msg = data;

	printf ("[%p]   done\n\n", msg);
	g_signal_handlers_disconnect_by_func (msg, client_msg_failed, msg2);

	soup_message_body_complete (msg->response_body);
	soup_server_unpause_message (server, msg);
	g_object_unref (msg);
}

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	SoupMessage *msg2;
	char *uristr;

	uristr = soup_uri_to_string (soup_message_get_uri (msg), FALSE);
	printf ("[%p] %s %s HTTP/1.%d\n", msg, msg->method, uristr,
		soup_message_get_http_version (msg));

	if (msg->method == SOUP_METHOD_CONNECT) {
		soup_message_set_status (msg, SOUP_STATUS_NOT_IMPLEMENTED);
		return;
	}

	msg2 = soup_message_new (msg->method, uristr);
        msg2 = soup_message_new (msg->method, uristr);
	soup_message_headers_foreach (msg->request_headers, copy_header,
				      msg2->request_headers);
	soup_message_headers_remove (msg2->request_headers, "Host");
	soup_message_headers_remove (msg2->request_headers, "Connection");

	if (msg->request_body->length) {
		SoupBuffer *request = soup_message_body_flatten (msg->request_body);
		soup_message_body_append_buffer (msg2->request_body, request);
		soup_buffer_free (request);
	}
	soup_message_headers_set_encoding (msg->response_headers,
					   SOUP_ENCODING_CHUNKED);

	g_signal_connect (msg2, "got_headers",
			  G_CALLBACK (send_headers), msg);
	g_signal_connect (msg2, "got_chunk",
			  G_CALLBACK (send_chunk), msg);

	g_signal_connect (msg, "finished", G_CALLBACK (client_msg_failed), msg2);

	soup_session_queue_message (session, msg2, finish_msg, msg);

	g_object_ref (msg);
	soup_server_pause_message (server, msg);
}

static void
quit (int sig)
{
	/* Exit cleanly on ^C in case we're valgrinding. */
	exit (0);
}

int
main (int argc, char **argv)
{
	GMainLoop *loop;
	int opt;
	int port = SOUP_ADDRESS_ANY_PORT;

	g_type_init ();
	g_thread_init (NULL);
	signal (SIGINT, quit);

	while ((opt = getopt (argc, argv, "p:s:")) != -1) {
		switch (opt) {
		case 'p':
			port = atoi (optarg);
			break;
		default:
			fprintf (stderr, "Usage: %s [-p port] [-n]\n",
				 argv[0]);
			exit (1);
		}
	}

	server = soup_server_new (SOUP_SERVER_PORT, port,
				  NULL);
	if (!server) {
		fprintf (stderr, "Unable to bind to server port %d\n", port);
		exit (1);
	}
	soup_server_add_handler (server, NULL,
				 server_callback, NULL, NULL);

	printf ("\nStarting proxy on port %d\n",
		soup_server_get_port (server));
	soup_server_run_async (server);

	session = soup_session_async_new ();

	printf ("\nWaiting for requests...\n");

	loop = g_main_loop_new (NULL, TRUE);
	g_main_loop_run (loop);
	g_main_loop_unref (loop);

	return 0;
}
