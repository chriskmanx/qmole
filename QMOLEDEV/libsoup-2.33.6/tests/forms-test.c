/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2007, 2008 Red Hat, Inc.
 */

#include "config.h"

#include <ctype.h>
#include <dirent.h>
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

static struct {
	const char *title, *name;
	const char *result;
} tests[] = {
	/* Both fields must be filled in */
	{ NULL, "Name", "" },
	{ "Mr.", NULL, "" },

	/* Filled-in but empty is OK */
	{ "", "", "Hello,  " },
	{ "", "Name", "Hello,  Name" },
	{ "Mr.", "", "Hello, MR. " },

	/* Simple */
	{ "Mr.", "Name", "Hello, MR. Name" },

	/* Encoding of spaces */
	{ "Mr.", "Full Name", "Hello, MR. Full Name" },
	{ "Mr. and Mrs.", "Full Name", "Hello, MR. AND MRS. Full Name" },

	/* Encoding of "+" */
	{ "Mr.+Mrs.", "Full Name", "Hello, MR.+MRS. Full Name" },

	/* Encoding of non-ASCII. */
	{ "Se\xC3\xB1or", "Nombre", "Hello, SE\xC3\xB1OR Nombre" },

	/* Encoding of '%' */
	{ "Mr.", "Foo %2f Bar", "Hello, MR. Foo %2f Bar" },
};

static void
do_hello_test (int n, gboolean extra, const char *uri)
{
	GPtrArray *args;
	char *title_arg = NULL, *name_arg = NULL;
	char *str_stdout = NULL;

	debug_printf (1, "%2d. '%s' '%s'%s: ", n * 2 + (extra ? 2 : 1),
		      tests[n].title ? tests[n].title : "(null)",
		      tests[n].name  ? tests[n].name  : "(null)",
		      extra ? " + extra" : "");

	args = g_ptr_array_new ();
	g_ptr_array_add (args, "curl");
	g_ptr_array_add (args, "-G");
	if (tests[n].title) {
		title_arg = soup_form_encode ("title", tests[n].title, NULL);
		g_ptr_array_add (args, "-d");
		g_ptr_array_add (args, title_arg);
	}
	if (tests[n].name) {
		name_arg = soup_form_encode ("n@me", tests[n].name, NULL);
		g_ptr_array_add (args, "-d");
		g_ptr_array_add (args, name_arg);
	}
	if (extra) {
		g_ptr_array_add (args, "-d");
		g_ptr_array_add (args, "extra=something");
	}
	g_ptr_array_add (args, (char *)uri);
	g_ptr_array_add (args, NULL);

	if (g_spawn_sync (NULL, (char **)args->pdata, NULL,
			  G_SPAWN_SEARCH_PATH | G_SPAWN_STDERR_TO_DEV_NULL,
			  NULL, NULL,
			  &str_stdout, NULL, NULL, NULL)) {
		if (str_stdout && !strcmp (str_stdout, tests[n].result))
			debug_printf (1, "OK!\n");
		else {
			debug_printf (1, "WRONG!\n");
			debug_printf (1, "  expected '%s', got '%s'\n",
				      tests[n].result,
				      str_stdout ? str_stdout : "(error)");
			errors++;
		}
		g_free (str_stdout);
	} else {
		debug_printf (1, "ERROR!\n");
		errors++;
	}
	g_ptr_array_free (args, TRUE);
	g_free (title_arg);
	g_free (name_arg);
}

static void
do_hello_tests (const char *uri)
{
	int n;

	debug_printf (1, "Hello tests (GET, application/x-www-form-urlencoded)\n");
	for (n = 0; n < G_N_ELEMENTS (tests); n++) {
		do_hello_test (n, FALSE, uri);
		do_hello_test (n, TRUE, uri);
	}
}

static void
do_md5_test_curl (const char *uri, const char *file, const char *md5)
{
	GPtrArray *args;
	char *file_arg, *str_stdout;

	debug_printf (1, "  via curl: ");

	args = g_ptr_array_new ();
	g_ptr_array_add (args, "curl");
	g_ptr_array_add (args, "-L");
	g_ptr_array_add (args, "-F");
	file_arg = g_strdup_printf ("file=@%s", file);
	g_ptr_array_add (args, file_arg);
	g_ptr_array_add (args, "-F");
	g_ptr_array_add (args, "fmt=txt");
	g_ptr_array_add (args, (char *)uri);
	g_ptr_array_add (args, NULL);

	if (g_spawn_sync (NULL, (char **)args->pdata, NULL,
			  G_SPAWN_SEARCH_PATH | G_SPAWN_STDERR_TO_DEV_NULL,
			  NULL, NULL,
			  &str_stdout, NULL, NULL, NULL)) {
		if (str_stdout && !strcmp (str_stdout, md5))
			debug_printf (1, "OK!\n");
		else {
			debug_printf (1, "WRONG!\n");
			debug_printf (1, "  expected '%s', got '%s'\n",
				      md5, str_stdout ? str_stdout : "(error)");
			errors++;
		}
		g_free (str_stdout);
	} else {
		debug_printf (1, "ERROR!\n");
		errors++;
	}
	g_ptr_array_free (args, TRUE);
	g_free (file_arg);
}

#define MD5_TEST_FILE SRCDIR "/resources/home.gif"
#define MD5_TEST_FILE_BASENAME "home.gif"
#define MD5_TEST_FILE_MIME_TYPE "image/gif"

static void
do_md5_test_libsoup (const char *uri, const char *contents,
		     gsize length, const char *md5)
{
	SoupMultipart *multipart;
	SoupBuffer *buffer;
	SoupMessage *msg;
	SoupSession *session;

	debug_printf (1, "  via libsoup: ");

	multipart = soup_multipart_new (SOUP_FORM_MIME_TYPE_MULTIPART);
	buffer = soup_buffer_new (SOUP_MEMORY_COPY, contents, length);
	soup_multipart_append_form_file (multipart, "file",
					 MD5_TEST_FILE_BASENAME,
					 MD5_TEST_FILE_MIME_TYPE,
					 buffer);
	soup_buffer_free (buffer);
	soup_multipart_append_form_string (multipart, "fmt", "text");

	msg = soup_form_request_new_from_multipart (uri, multipart);
	soup_multipart_free (multipart);

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	soup_session_send_message (session, msg);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "ERROR: Unexpected status %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	} else if (strcmp (msg->response_body->data, md5) != 0) {
		debug_printf (1, "ERROR: Incorrect response: expected '%s' got '%s'\n",
			      md5, msg->response_body->data);
		errors++;
	} else
		debug_printf (1, "OK!\n");

	g_object_unref (msg);
	soup_test_session_abort_unref (session);
}

static void
do_md5_tests (const char *uri)
{
	char *contents, *md5;
	gsize length;
	GError *error = NULL;

	debug_printf (1, "\nMD5 tests (POST, multipart/form-data)\n");

	if (!g_file_get_contents (MD5_TEST_FILE, &contents, &length, &error)) {
		debug_printf (1, "  ERROR: Could not read " MD5_TEST_FILE ": %s\n", error->message);
		g_error_free (error);
		errors++;
		return;
	}

	md5 = g_compute_checksum_for_string (G_CHECKSUM_MD5, contents, length);

	do_md5_test_curl (uri, MD5_TEST_FILE, md5);
	do_md5_test_libsoup (uri, contents, length, md5);

	g_free (contents);
	g_free (md5);
}


static void
do_form_decode_test (void)
{
	GHashTable *table;
	const gchar *value;
	gchar *tmp;

	debug_printf (1, "\nDecode tests\n");

	/*  Test that the code handles multiple values with the same key.  */
	table = soup_form_decode ("foo=first&foo=second&foo=third");

	/*  Allocate some memory. We do this to test for a bug in
	 *  soup_form_decode() that resulted in values from the hash
	 *  table pointing to memory that is already released.
	 */
	tmp = g_strdup ("other");

	value = g_hash_table_lookup (table, "foo");
	if (g_strcmp0 (value, "third") != 0) {
		debug_printf (1, "  ERROR: expected '%s', got '%s'\n",
			      "third", value ? value : "(null)");
		errors++;
	}

	g_free (tmp);
	g_hash_table_destroy (table);
}

static void
hello_callback (SoupServer *server, SoupMessage *msg,
		const char *path, GHashTable *query,
		SoupClientContext *context, gpointer data)
{
	char *title, *name, *fmt;
	const char *content_type;
	GString *buf;

	if (msg->method != SOUP_METHOD_GET && msg->method != SOUP_METHOD_HEAD) {
		soup_message_set_status (msg, SOUP_STATUS_NOT_IMPLEMENTED);
		return;
	}

	if (query) {
		title = g_hash_table_lookup (query, "title");
		name = g_hash_table_lookup (query, "n@me");
		fmt = g_hash_table_lookup (query, "fmt");
	} else
		title = name = fmt = NULL;

	buf = g_string_new (NULL);
	if (!query || (fmt && !strcmp (fmt, "html"))) {
		content_type = "text/html";
		g_string_append (buf, "<html><head><title>forms-test: hello</title></head><body>\r\n");
		if (title && name) {
			/* mumble mumble html-escape... */
			g_string_append_printf (buf, "<p>Hello, <b><em>%s</em> %s</b></p>\r\n",
						title, name);
		}
		g_string_append (buf, "<form action='/hello' method='get'>"
				 "<p>Title: <input name='title'></p>"
				 "<p>Name: <input name='n@me'></p>"
				 "<p><input type=hidden name='fmt' value='html'></p>"
				 "<p><input type=submit></p>"
				 "</form>\r\n");
		g_string_append (buf, "</body></html>\r\n");
	} else {
		content_type = "text/plain";
		if (title && name) {
			char *uptitle = g_ascii_strup (title, -1);
			g_string_append_printf (buf, "Hello, %s %s",
						uptitle, name);
			g_free (uptitle);
		}
	}

	soup_message_set_response (msg, content_type,
				   SOUP_MEMORY_TAKE,
				   buf->str, buf->len);
	g_string_free (buf, FALSE);
	soup_message_set_status (msg, SOUP_STATUS_OK);
}

static void
md5_get_callback (SoupServer *server, SoupMessage *msg,
		  const char *path, GHashTable *query,
		  SoupClientContext *context, gpointer data)
{
	const char *file = NULL, *md5sum = NULL, *fmt;
	const char *content_type;
	GString *buf;

	if (query) {
		file = g_hash_table_lookup (query, "file");
		md5sum = g_hash_table_lookup (query, "md5sum");
		fmt = g_hash_table_lookup (query, "fmt");
	} else
		fmt = "html";

	buf = g_string_new (NULL);
	if (!strcmp (fmt, "html")) {
		content_type = "text/html";
		g_string_append (buf, "<html><head><title>forms-test: md5</title></head><body>\r\n");
		if (file && md5sum) {
			/* mumble mumble html-escape... */
			g_string_append_printf (buf, "<p>File: %s<br>MD5: <b>%s</b></p>\r\n",
						file, md5sum);
		}
		g_string_append (buf, "<form action='/md5' method='post' enctype='multipart/form-data'>"
				 "<p>File: <input type='file' name='file'></p>"
				 "<p><input type=hidden name='fmt' value='html'></p>"
				 "<p><input type=submit></p>"
				 "</form>\r\n");
		g_string_append (buf, "</body></html>\r\n");
	} else {
		content_type = "text/plain";
		if (md5sum)
			g_string_append_printf (buf, "%s", md5sum);
	}

	soup_message_set_response (msg, content_type,
				   SOUP_MEMORY_TAKE,
				   buf->str, buf->len);
	g_string_free (buf, FALSE);
	soup_message_set_status (msg, SOUP_STATUS_OK);
}

static void
md5_post_callback (SoupServer *server, SoupMessage *msg,
		   const char *path, GHashTable *query,
		   SoupClientContext *context, gpointer data)
{
	const char *content_type;
	GHashTable *params;
	const char *fmt;
	char *filename, *md5sum, *redirect_uri;
	SoupBuffer *file;
	SoupURI *uri;

	content_type = soup_message_headers_get_content_type (msg->request_headers, NULL);
	if (!content_type || strcmp (content_type, "multipart/form-data") != 0) {
		soup_message_set_status (msg, SOUP_STATUS_BAD_REQUEST);
		return;
	}

	params = soup_form_decode_multipart (msg, "file",
					     &filename, NULL, &file);
	if (!params) {
		soup_message_set_status (msg, SOUP_STATUS_BAD_REQUEST);
		return;
	}
	fmt = g_hash_table_lookup (params, "fmt");

	md5sum = g_compute_checksum_for_data (G_CHECKSUM_MD5,
					      (gpointer)file->data,
					      file->length);
	soup_buffer_free (file);

	uri = soup_uri_copy (soup_message_get_uri (msg));
	soup_uri_set_query_from_fields (uri,
					"file", filename ? filename : "",
					"md5sum", md5sum,
					"fmt", fmt ? fmt : "html",
					NULL);
	redirect_uri = soup_uri_to_string (uri, FALSE);

	soup_message_set_status (msg, SOUP_STATUS_SEE_OTHER);
	soup_message_headers_replace (msg->response_headers, "Location",
				      redirect_uri);

	g_free (redirect_uri);
	soup_uri_free (uri);
	g_free (md5sum);
	g_free (filename);
	g_hash_table_destroy (params);
}

static void
md5_callback (SoupServer *server, SoupMessage *msg,
	      const char *path, GHashTable *query,
	      SoupClientContext *context, gpointer data)
{
	if (msg->method == SOUP_METHOD_GET || msg->method == SOUP_METHOD_HEAD)
		md5_get_callback (server, msg, path, query, context, data);
	else if (msg->method == SOUP_METHOD_POST)
		md5_post_callback (server, msg, path, query, context, data);
	else
		soup_message_set_status (msg, SOUP_STATUS_METHOD_NOT_ALLOWED);
}

static gboolean run_tests = TRUE;

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
	char *uri_str;

	test_init (argc, argv, no_test_entry);

	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, "/hello",
				 hello_callback, NULL, NULL);
	soup_server_add_handler (server, "/md5",
				 md5_callback, NULL, NULL);
	port = 	soup_server_get_port (server);

	loop = g_main_loop_new (NULL, TRUE);

	if (run_tests) {
		uri_str = g_strdup_printf ("http://127.0.0.1:%u/hello", port);
		do_hello_tests (uri_str);
		g_free (uri_str);

		uri_str = g_strdup_printf ("http://127.0.0.1:%u/md5", port);
		do_md5_tests (uri_str);
		g_free (uri_str);

		do_form_decode_test ();
	} else {
		printf ("Listening on port %d\n", port);
		g_main_loop_run (loop);
	}

	g_main_loop_unref (loop);

	soup_test_server_quit_unref (server);
	if (run_tests)
		test_cleanup ();
	return errors != 0;
}
