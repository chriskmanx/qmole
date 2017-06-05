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
#include <libsoup/soup-form.h>
#include <libsoup/soup-message.h>
#include <libsoup/soup-server.h>
#include <libsoup/soup-session-sync.h>

#include "test-utils.h"

GMainLoop *loop;

struct {
	char *title, *name;
	char *result;
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
do_test (int n, gboolean extra, const char *uri)
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
		name_arg = soup_form_encode ("name", tests[n].name, NULL);
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
do_query_tests (const char *uri)
{
	int n;

	for (n = 0; n < G_N_ELEMENTS (tests); n++) {
		do_test (n, FALSE, uri);
		do_test (n, TRUE, uri);
	}
}

GThread *server_thread;

static void
server_callback (SoupServer *server, SoupMessage *msg,
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
		name = g_hash_table_lookup (query, "name");
		fmt = g_hash_table_lookup (query, "fmt");
	} else
		title = name = fmt = NULL;

	buf = g_string_new (NULL);
	if (!query || (fmt && !strcmp (fmt, "html"))) {
		content_type = "text/html";
		g_string_append (buf, "<html><head><title>query-test</title></head><body>\r\n");
		if (title && name) {
			/* mumble mumble html-escape... */
			g_string_append_printf (buf, "<p>Hello, <b><em>%s</em> %s</b></p>\r\n",
						title, name);
		}
		g_string_append (buf, "<form action='/' method='get'>"
				 "<p>Title: <input name='title'></p>"
				 "<p>Name: <input name='name'></p>"
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
	char *uri_str;

	test_init (argc, argv, no_test_entry);

	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, NULL,
				 server_callback, NULL, NULL);
	port = 	soup_server_get_port (server);

	loop = g_main_loop_new (NULL, TRUE);

	if (run_tests) {
		uri_str = g_strdup_printf ("http://localhost:%u", port);
		do_query_tests (uri_str);
		g_free (uri_str);
	} else {
		printf ("Listening on port %d\n", port);
		g_main_loop_run (loop);
	}

	g_main_loop_unref (loop);

	if (run_tests)
		test_cleanup ();
	return errors != 0;
}
