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

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	const char *accept_encoding, *junk, *noencode;
	GSList *codings;
	char *file = NULL, *contents;
	gsize length;

	accept_encoding = soup_message_headers_get_list (msg->request_headers,
							 "Accept-Encoding");
	if (accept_encoding)
		codings = soup_header_parse_quality_list (accept_encoding, NULL);
	else
		codings = NULL;

	if (codings && g_slist_find_custom (codings, "gzip", (GCompareFunc)g_ascii_strcasecmp)) {
		file = g_strdup_printf (SRCDIR "/resources%s.gz", path);
		if (g_file_test (file, G_FILE_TEST_EXISTS)) {
			soup_message_headers_append (msg->response_headers,
						     "Content-Encoding",
						     "gzip");
		} else {
			g_free (file);
			file = NULL;
		}
	}
	soup_header_free_list (codings);

	noencode = soup_message_headers_get_one (msg->request_headers,
						 "X-No-Encode");
	if (noencode) {
		/* Force it to send the ungzipped version, even though
		 * we already added "Content-Encoding: gzip"
		 */
		g_free (file);
		file = NULL;
	}

	if (!file)
		file = g_strdup_printf (SRCDIR "/resources%s", path);
	if (!g_file_get_contents (file, &contents, &length, NULL)) {
		/* If path.gz exists but can't be read, we'll send back
		 * the error with "Content-Encoding: gzip" but there's
		 * no body, so, eh.
		 */
		g_free (file);
		soup_message_set_status (msg, SOUP_STATUS_NOT_FOUND);
		return;
	}
	g_free (file);

	soup_message_set_status (msg, SOUP_STATUS_OK);
	soup_message_body_append (msg->response_body,
				  SOUP_MEMORY_TAKE, contents, length);

	junk = soup_message_headers_get_one (msg->request_headers,
					     "X-Trailing-Junk");
	if (junk) {
		soup_message_body_append (msg->response_body, SOUP_MEMORY_COPY,
					  junk, strlen (junk));
	}
}

static void
do_coding_test (void)
{
	SoupSession *session;
	SoupMessage *msg, *msgz, *msgj, *msgn;
	SoupURI *uri;
	const char *coding;

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	uri = soup_uri_new_with_base (base_uri, "/mbox");


	debug_printf (1, "GET /mbox, plain\n");
	msg = soup_message_new_from_uri ("GET", uri);
	soup_session_send_message (session, msg);
	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "  Unexpected status %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	coding = soup_message_headers_get_one (msg->response_headers, "Content-Encoding");
	if (coding) {
		debug_printf (1, "  Unexpected Content-Encoding: %s\n",
			      coding);
		errors++;
	}
	if (soup_message_get_flags (msg) & SOUP_MESSAGE_CONTENT_DECODED) {
		debug_printf (1, "  SOUP_MESSAGE_CONTENT_DECODED set!\n");
		errors++;
	}


	debug_printf (1, "GET /mbox, Accept-Encoding: gzip\n");
	soup_session_add_feature_by_type (session, SOUP_TYPE_CONTENT_DECODER);
	msgz = soup_message_new_from_uri ("GET", uri);
	soup_session_send_message (session, msgz);
	if (!SOUP_STATUS_IS_SUCCESSFUL (msgz->status_code)) {
		debug_printf (1, "  Unexpected status %d %s\n",
			      msgz->status_code, msgz->reason_phrase);
		errors++;
	}
	coding = soup_message_headers_get_one (msgz->response_headers, "Content-Encoding");
	if (!coding || g_ascii_strcasecmp (coding, "gzip") != 0) {
		debug_printf (1, "  Unexpected Content-Encoding: %s\n",
			      coding ? coding : "(none)");
		errors++;
	}
	if (!(soup_message_get_flags (msgz) & SOUP_MESSAGE_CONTENT_DECODED)) {
		debug_printf (1, "  SOUP_MESSAGE_CONTENT_DECODED not set!\n");
		errors++;
	}

	if (msg->response_body->length != msgz->response_body->length) {
		debug_printf (1, "  Message length mismatch: %lu (plain) vs %lu (compressed)\n",
			      (gulong)msg->response_body->length,
			      (gulong)msgz->response_body->length);
		errors++;
	} else if (memcmp (msg->response_body->data,
			   msgz->response_body->data,
			   msg->response_body->length) != 0) {
		debug_printf (1, "  Message data mismatch (plain/compressed)\n");
		errors++;
	}


	debug_printf (1, "GET /mbox, Accept-Encoding: gzip, plus trailing junk\n");
	msgj = soup_message_new_from_uri ("GET", uri);
	soup_message_headers_append (msgj->request_headers,
				     "X-Trailing-Junk", "junk!");
	soup_session_send_message (session, msgj);
	if (!SOUP_STATUS_IS_SUCCESSFUL (msgj->status_code)) {
		debug_printf (1, "  Unexpected status %d %s\n",
			      msgj->status_code, msgj->reason_phrase);
		errors++;
	}
	coding = soup_message_headers_get_one (msgj->response_headers, "Content-Encoding");
	if (!coding || g_ascii_strcasecmp (coding, "gzip") != 0) {
		debug_printf (1, "  Unexpected Content-Encoding: %s\n",
			      coding ? coding : "(none)");
		errors++;
	}
	if (!(soup_message_get_flags (msgj) & SOUP_MESSAGE_CONTENT_DECODED)) {
		debug_printf (1, "  SOUP_MESSAGE_CONTENT_DECODED not set!\n");
		errors++;
	}

	if (msg->response_body->length != msgj->response_body->length) {
		debug_printf (1, "  Message length mismatch: %lu (plain) vs %lu (compressed w/ junk)\n",
			      (gulong)msg->response_body->length,
			      (gulong)msgj->response_body->length);
		errors++;
	} else if (memcmp (msg->response_body->data,
			   msgj->response_body->data,
			   msg->response_body->length) != 0) {
		debug_printf (1, "  Message data mismatch (plain/compressed w/ junk)\n");
		errors++;
	}


	debug_printf (1, "GET /mbox, Accept-Encoding: gzip, with server error\n");
	msgn = soup_message_new_from_uri ("GET", uri);
	soup_message_headers_append (msgn->request_headers,
				     "X-No-Encode", "true");
	soup_session_send_message (session, msgn);
	if (!SOUP_STATUS_IS_SUCCESSFUL (msgn->status_code)) {
		debug_printf (1, "  Unexpected status %d %s\n",
			      msgn->status_code, msgn->reason_phrase);
		errors++;
	}
	coding = soup_message_headers_get_one (msgn->response_headers, "Content-Encoding");
	if (!coding || g_ascii_strcasecmp (coding, "gzip") != 0) {
		debug_printf (1, "  Unexpected Content-Encoding: %s\n",
			      coding ? coding : "(none)");
		errors++;
	}
	/* Since the content wasn't actually gzip-encoded, decoding it
	 * should have failed and so the flag won't be set.
	 */
	if (soup_message_get_flags (msgn) & SOUP_MESSAGE_CONTENT_DECODED) {
		debug_printf (1, "  SOUP_MESSAGE_CONTENT_DECODED set!\n");
		errors++;
	}
	/* Failed content-decoding should have left the body untouched
	 * from what the server sent... which happens to be the
	 * uncompressed data.
	 */
	if (msg->response_body->length != msgn->response_body->length) {
		debug_printf (1, "  Message length mismatch: %lu (plain) vs %lu (mis-encoded)\n",
			      (gulong)msg->response_body->length,
			      (gulong)msgn->response_body->length);
		errors++;
	} else if (memcmp (msg->response_body->data,
			   msgn->response_body->data,
			   msg->response_body->length) != 0) {
		debug_printf (1, "  Message data mismatch (plain/misencoded)\n");
		errors++;
	}


	g_object_unref (msg);
	g_object_unref (msgz);
	g_object_unref (msgj);
	g_object_unref (msgn);
	soup_uri_free (uri);

	soup_test_session_abort_unref (session);
}

int
main (int argc, char **argv)
{
	test_init (argc, argv, NULL);

	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, NULL, server_callback, NULL, NULL);
	base_uri = soup_uri_new ("http://127.0.0.1/");
	soup_uri_set_port (base_uri, soup_server_get_port (server));

	do_coding_test ();

	soup_uri_free (base_uri);
	soup_test_server_quit_unref (server);

	test_cleanup ();
	return errors != 0;
}
