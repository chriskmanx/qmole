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

typedef struct {
	SoupSession *session;
	SoupBuffer *chunks[3];
	int next, nwrote;
} PutTestData;

static SoupBuffer *
error_chunk_allocator (SoupMessage *msg, gsize max_len, gpointer user_data)
{
	/* This should never be called, because there is no response body. */
	debug_printf (1, "  error_chunk_allocator called!\n");
	errors++;
	return soup_buffer_new (SOUP_MEMORY_TAKE, g_malloc (100), 100);
}

static void
write_next_chunk (SoupMessage *msg, gpointer user_data)
{
	PutTestData *ptd = user_data;

	debug_printf (2, "  writing chunk\n");

	if (ptd->next > 0 && ptd->chunks[ptd->next - 1]) {
#ifdef FIXME
		debug_printf (1, "  error: next chunk requested before last one freed!\n");
		errors++;
#else
		debug_printf (0, "  ignoring bug in test case... FIXME!\n");
#endif
	}

	if (ptd->next < G_N_ELEMENTS (ptd->chunks)) {
		soup_message_body_append_buffer (msg->request_body,
						 ptd->chunks[ptd->next]);
		soup_buffer_free (ptd->chunks[ptd->next]);
		ptd->next++;
	} else
		soup_message_body_complete (msg->request_body);
	soup_session_unpause_message (ptd->session, msg);
}

static void
wrote_body_data (SoupMessage *msg, SoupBuffer *chunk, gpointer user_data)
{
	PutTestData *ptd = user_data;

	debug_printf (2, "  wrote_body_data, %d bytes\n",
		      (int)chunk->length);
	ptd->nwrote += chunk->length;
}

static void
clear_buffer_ptr (gpointer data)
{
	SoupBuffer **buffer_ptr = data;

	debug_printf (2, "  clearing chunk\n");
	if (*buffer_ptr) {
		(*buffer_ptr)->length = 0;
		g_free ((char *)(*buffer_ptr)->data);
		*buffer_ptr = NULL;
	} else {
		debug_printf (2, "  chunk is already clear!\n");
		errors++;
	}
}

/* Put a chunk containing @text into *@buffer, set up so that it will
 * clear out *@buffer when the chunk is freed, allowing us to make sure
 * the set_accumulate(FALSE) is working.
 */
static void
make_put_chunk (SoupBuffer **buffer, const char *text)
{
	*buffer = soup_buffer_new_with_owner (g_strdup (text), strlen (text),
					      buffer, clear_buffer_ptr);
}

static void
do_request_test (SoupSession *session, SoupURI *base_uri)
{
	PutTestData ptd;
	SoupMessage *msg;
	const char *client_md5, *server_md5;
	GChecksum *check;
	int i, length;

	debug_printf (1, "PUT\n");

	ptd.session = session;
	make_put_chunk (&ptd.chunks[0], "one\r\n");
	make_put_chunk (&ptd.chunks[1], "two\r\n");
	make_put_chunk (&ptd.chunks[2], "three\r\n");
	ptd.next = ptd.nwrote = 0;

	check = g_checksum_new (G_CHECKSUM_MD5);
	length = 0;
	for (i = 0; i < 3; i++) {
		g_checksum_update (check, (guchar *)ptd.chunks[i]->data,
				   ptd.chunks[i]->length);
		length += ptd.chunks[i]->length;
	}
	client_md5 = g_checksum_get_string (check);

	msg = soup_message_new_from_uri ("PUT", base_uri);
	soup_message_headers_set_encoding (msg->request_headers, SOUP_ENCODING_CHUNKED);
	soup_message_body_set_accumulate (msg->request_body, FALSE);
	soup_message_set_chunk_allocator (msg, error_chunk_allocator, NULL, NULL);
	g_signal_connect (msg, "wrote_headers",
			  G_CALLBACK (write_next_chunk), &ptd);
	g_signal_connect (msg, "wrote_chunk",
			  G_CALLBACK (write_next_chunk), &ptd);
	g_signal_connect (msg, "wrote_body_data",
			  G_CALLBACK (wrote_body_data), &ptd);
	soup_session_send_message (session, msg);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "  message failed: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}

	if (msg->request_body->data) {
		debug_printf (1, "  msg->request_body set!\n");
		errors++;
	}
	if (msg->request_body->length != length || length != ptd.nwrote) {
		debug_printf (1, "  sent length mismatch: %d vs %d vs %d\n",
			      (int)msg->request_body->length, length, ptd.nwrote);
		errors++;
	}

	server_md5 = soup_message_headers_get_one (msg->response_headers,
						   "Content-MD5");
	if (!server_md5 || strcmp (client_md5, server_md5) != 0) {
		debug_printf (1, "  client/server data mismatch: %s vs %s\n",
			      client_md5, server_md5 ? server_md5 : "(null)");
		errors++;
	}

	g_object_unref (msg);
	g_checksum_free (check);
}

typedef struct {
	SoupBuffer *current_chunk;
	GChecksum *check;
	int length;
} GetTestData;

static SoupBuffer *
chunk_allocator (SoupMessage *msg, gsize max_len, gpointer user_data)
{
	GetTestData *gtd = user_data;

	debug_printf (2, "  allocating chunk\n");

	if (gtd->current_chunk) {
		debug_printf (1, "  error: next chunk allocated before last one freed!\n");
		errors++;
	}
	gtd->current_chunk = soup_buffer_new_with_owner (g_malloc (6), 6,
							 &gtd->current_chunk,
							 clear_buffer_ptr);
	return gtd->current_chunk;
}

static void
got_chunk (SoupMessage *msg, SoupBuffer *chunk, gpointer user_data)
{
	GetTestData *gtd = user_data;

	debug_printf (2, "  got chunk, %d bytes\n",
		      (int)chunk->length);
	if (chunk != gtd->current_chunk) {
		debug_printf (1, "chunk mismatch! %p vs %p\n",
			      chunk, gtd->current_chunk);
	}

	g_checksum_update (gtd->check, (guchar *)chunk->data, chunk->length);
	gtd->length += chunk->length;
}

static void
do_response_test (SoupSession *session, SoupURI *base_uri)
{
	GetTestData gtd;
	SoupMessage *msg;
	const char *client_md5, *server_md5;

	debug_printf (1, "GET\n");

	gtd.current_chunk = NULL;
	gtd.length = 0;
	gtd.check = g_checksum_new (G_CHECKSUM_MD5);

	msg = soup_message_new_from_uri ("GET", base_uri);
	soup_message_body_set_accumulate (msg->response_body, FALSE);
	soup_message_set_chunk_allocator (msg, chunk_allocator, &gtd, NULL);
	g_signal_connect (msg, "got_chunk",
			  G_CALLBACK (got_chunk), &gtd);
	soup_session_send_message (session, msg);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "  message failed: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}

	if (msg->response_body->data) {
		debug_printf (1, "  msg->response_body set!\n");
		errors++;
	}
	if (soup_message_headers_get_content_length (msg->response_headers) != gtd.length) {
		debug_printf (1, "  received length mismatch: %d vs %d\n",
			      (int)soup_message_headers_get_content_length (msg->response_headers), gtd.length);
		errors++;
	}

	client_md5 = g_checksum_get_string (gtd.check);
	server_md5 = soup_message_headers_get_one (msg->response_headers,
						   "Content-MD5");
	if (!server_md5 || strcmp (client_md5, server_md5) != 0) {
		debug_printf (1, "  client/server data mismatch: %s vs %s\n",
			      client_md5, server_md5 ? server_md5 : "(null)");
		errors++;
	}

	g_object_unref (msg);
	g_checksum_free (gtd.check);
}

/* Make sure TEMPORARY buffers are handled properly with non-accumulating
 * message bodies. Part of https://bugs.webkit.org/show_bug.cgi?id=18343
 */

static void
temp_test_wrote_chunk (SoupMessage *msg, gpointer session)
{
	SoupBuffer *chunk;

	chunk = soup_message_body_get_chunk (msg->request_body, 5);

	/* When the bug is present, the second chunk will also be
	 * discarded after the first is written, which will cause
	 * the I/O to stall since soup-message-io will think it's
	 * done, but it hasn't written Content-Length bytes yet.
	 */
	if (!chunk) {
		debug_printf (1, "  Lost second chunk!\n");
		errors++;
		soup_session_abort (session);
	} else
		soup_buffer_free (chunk);

	g_signal_handlers_disconnect_by_func (msg, temp_test_wrote_chunk, session);
}

static void
do_temporary_test (SoupSession *session, SoupURI *base_uri)
{
	SoupMessage *msg;
	char *client_md5;
	const char *server_md5;

	debug_printf (1, "PUT w/ temporary buffers\n");

	msg = soup_message_new_from_uri ("PUT", base_uri);
	soup_message_body_append (msg->request_body, SOUP_MEMORY_TEMPORARY,
				  "one\r\n", 5);
	soup_message_body_append (msg->request_body, SOUP_MEMORY_STATIC,
				  "two\r\n", 5);
	soup_message_body_set_accumulate (msg->request_body, FALSE);

	client_md5 = g_compute_checksum_for_string (G_CHECKSUM_MD5,
						    "one\r\ntwo\r\n", 10);
	g_signal_connect (msg, "wrote_chunk",
			  G_CALLBACK (temp_test_wrote_chunk), session);
	soup_session_send_message (session, msg);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "  message failed: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}

	server_md5 = soup_message_headers_get_one (msg->response_headers,
						   "Content-MD5");
	if (!server_md5 || strcmp (client_md5, server_md5) != 0) {
		debug_printf (1, "  client/server data mismatch: %s vs %s\n",
			      client_md5, server_md5 ? server_md5 : "(null)");
		errors++;
	}

	g_free (client_md5);
	g_object_unref (msg);
}

static void
do_chunk_tests (SoupURI *base_uri)
{
	SoupSession *session;

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	do_request_test (session, base_uri);
	debug_printf (2, "\n\n");
	do_response_test (session, base_uri);
	debug_printf (2, "\n\n");
	do_temporary_test (session, base_uri);
	soup_test_session_abort_unref (session);
}

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	SoupMessageBody *md5_body;
	char *md5;

	if (msg->method == SOUP_METHOD_GET) {
		soup_message_set_response (msg, "text/plain",
					   SOUP_MEMORY_STATIC,
					   "three\r\ntwo\r\none\r\n",
					   strlen ("three\r\ntwo\r\none\r\n"));
		soup_buffer_free (soup_message_body_flatten (msg->response_body));
		md5_body = msg->response_body;
		soup_message_set_status (msg, SOUP_STATUS_OK);
	} else if (msg->method == SOUP_METHOD_PUT) {
		soup_message_set_status (msg, SOUP_STATUS_CREATED);
		md5_body = msg->request_body;
	} else {
		soup_message_set_status (msg, SOUP_STATUS_METHOD_NOT_ALLOWED);
		return;
	}

	md5 = g_compute_checksum_for_data (G_CHECKSUM_MD5,
					   (guchar *)md5_body->data,
					   md5_body->length);
	soup_message_headers_append (msg->response_headers,
				     "Content-MD5", md5);
	g_free (md5);
}

int
main (int argc, char **argv)
{
	GMainLoop *loop;
	SoupServer *server;
	guint port;
	SoupURI *base_uri;

	test_init (argc, argv, NULL);

	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, NULL,
				 server_callback, NULL, NULL);
	port = 	soup_server_get_port (server);

	loop = g_main_loop_new (NULL, TRUE);

	base_uri = soup_uri_new ("http://127.0.0.1");
	soup_uri_set_port (base_uri, port);
	do_chunk_tests (base_uri);
	soup_uri_free (base_uri);

	g_main_loop_unref (loop);
	soup_test_server_quit_unref (server);

	test_cleanup ();
	return errors != 0;
}
