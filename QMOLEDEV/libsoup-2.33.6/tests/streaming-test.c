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

#define RESPONSE_CHUNK_SIZE 1024

char *full_response, *full_response_md5;
gsize full_response_length;

static void
get_full_response (void)
{
	GError *error = NULL;

	if (!g_file_get_contents (SRCDIR "/index.txt",
				  &full_response,
				  &full_response_length,
				  &error)) {
		fprintf (stderr, "Could not read index file %s: %s\n",
			 SRCDIR "/index.txt", error->message);
		g_error_free (error);
		exit (1);
	}

	full_response_md5 = g_compute_checksum_for_data (G_CHECKSUM_MD5,
							 (guchar *)full_response,
							 full_response_length);
}

static void
write_next_chunk (SoupMessage *msg, gpointer user_data)
{
	gsize *offset = user_data;
	gsize chunk_length;

	chunk_length = MIN (RESPONSE_CHUNK_SIZE, full_response_length - *offset);
	if (chunk_length > 0) {
		debug_printf (2, "  writing chunk\n");
		soup_message_body_append (msg->response_body,
					  SOUP_MEMORY_STATIC,
					  full_response + *offset,
					  chunk_length);
		*offset += chunk_length;
	} else {
		debug_printf (2, "  done\n");
		/* This is only actually needed in the chunked and eof
		 * cases, but it's harmless in the content-length
		 * case.
		 */
		soup_message_body_complete (msg->response_body);
	}
}

static void
free_offset (SoupMessage *msg, gpointer offset)
{
	g_free (offset);
}

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	gsize *offset;

	if (!strcmp (path, "/chunked")) {
		soup_message_headers_set_encoding (msg->response_headers,
						   SOUP_ENCODING_CHUNKED);
	} else if (!strcmp (path, "/content-length")) {
		soup_message_headers_set_encoding (msg->response_headers,
						   SOUP_ENCODING_CONTENT_LENGTH);
		soup_message_headers_set_content_length (msg->response_headers,
							 full_response_length);
	} else if (!strcmp (path, "/eof")) {
		soup_message_headers_set_encoding (msg->response_headers,
						   SOUP_ENCODING_EOF);
	} else {
		soup_message_set_status (msg, SOUP_STATUS_NOT_FOUND);
		return;
	}
	soup_message_set_status (msg, SOUP_STATUS_OK);

	offset = g_new0 (gsize, 1);
	g_signal_connect (msg, "wrote_headers",
			  G_CALLBACK (write_next_chunk), offset);
	g_signal_connect (msg, "wrote_chunk",
			  G_CALLBACK (write_next_chunk), offset);
	g_signal_connect (msg, "finished",
			  G_CALLBACK (free_offset), offset);
}

static void
do_request (SoupSession *session, SoupURI *base_uri, char *path)
{
	SoupURI *uri;
	SoupMessage *msg;
	char *md5;

	uri = soup_uri_new_with_base (base_uri, path);
	msg = soup_message_new_from_uri ("GET", uri);
	soup_uri_free (uri);

	soup_session_send_message (session, msg);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "  message failed: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}

	if (msg->response_body->length != full_response_length) {
		debug_printf (1, "  received length mismatch: expected %d, got %d\n",
			      (int)full_response_length, (int)msg->request_body->length);
		errors++;
	}

	md5 = g_compute_checksum_for_data (G_CHECKSUM_MD5,
					   (guchar *)msg->response_body->data,
					   msg->response_body->length);
	if (strcmp (md5, full_response_md5) != 0) {
		debug_printf (1, "  data mismatch: expected %s, got %s\n",
			      full_response_md5, md5);
		errors++;
	}
	g_free (md5);

	g_object_unref (msg);
}

static void
do_tests (SoupURI *base_uri)
{
	SoupSession *session;

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	debug_printf (1, "Chunked encoding\n");
	do_request (session, base_uri, "chunked");
	debug_printf (1, "\n");
	debug_printf (1, "Content-Length encoding\n");
	do_request (session, base_uri, "content-length");
	debug_printf (1, "\n");
	debug_printf (1, "EOF encoding\n");
	do_request (session, base_uri, "eof");
	soup_test_session_abort_unref (session);
}

int
main (int argc, char **argv)
{
	GMainLoop *loop;
	SoupServer *server;
	guint port;
	SoupURI *base_uri;

	test_init (argc, argv, NULL);
	get_full_response ();

	server = soup_test_server_new (FALSE);
	soup_server_add_handler (server, NULL,
				 server_callback, NULL, NULL);
	port = 	soup_server_get_port (server);

	loop = g_main_loop_new (NULL, TRUE);

	base_uri = soup_uri_new ("http://127.0.0.1");
	soup_uri_set_port (base_uri, port);
	do_tests (base_uri);
	soup_uri_free (base_uri);

	g_main_loop_unref (loop);

	g_free (full_response);
	g_free (full_response_md5);
	soup_test_server_quit_unref (server);
	test_cleanup ();
	return errors != 0;
}
