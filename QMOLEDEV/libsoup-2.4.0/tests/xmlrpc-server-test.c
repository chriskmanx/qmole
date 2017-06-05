/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2008 Red Hat, Inc.
 */

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <sys/wait.h>

#include <libsoup/soup.h>

#include "test-utils.h"

GMainLoop *loop;

static void
type_error (SoupMessage *msg, GType expected, GValueArray *params, int bad_value)
{
	soup_xmlrpc_set_fault (msg,
			       SOUP_XMLRPC_FAULT_SERVER_ERROR_INVALID_METHOD_PARAMETERS,
			       "Bad parameter #%d: expected %s, got %s",
			       bad_value + 1, g_type_name (expected),
			       g_type_name (G_VALUE_TYPE (&params->values[bad_value])));
}

static void
args_error (SoupMessage *msg, GValueArray *params, int expected)
{
	soup_xmlrpc_set_fault (msg,
			       SOUP_XMLRPC_FAULT_SERVER_ERROR_INVALID_METHOD_PARAMETERS,
			       "Wrong number of parameters: expected %d, got %d",
			       expected, params->n_values);
}

static void
do_sum (SoupMessage *msg, GValueArray *params)
{
	int sum = 0, i, val;
	GValueArray *nums;

	if (params->n_values != 1) {
		args_error (msg, params, 1);
		return;
	}
	if (!soup_value_array_get_nth (params, 0, G_TYPE_VALUE_ARRAY, &nums)) {
		type_error (msg, G_TYPE_VALUE_ARRAY, params, 0);
		return;
	}

	for (i = 0; i < nums->n_values; i++) {
		if (!soup_value_array_get_nth (nums, i, G_TYPE_INT, &val)) {
			type_error (msg, G_TYPE_INT, nums, i);
			return;
		}
		sum += val;
	}

	soup_xmlrpc_set_response (msg, G_TYPE_INT, sum);

}

static void
do_countBools (SoupMessage *msg, GValueArray *params)
{
	int i, trues = 0, falses = 0;
	GValueArray *bools;
	GHashTable *ret = soup_value_hash_new ();
	gboolean val;

	if (params->n_values != 1) {
		args_error (msg, params, 1);
		return;
	}
	if (!soup_value_array_get_nth (params, 0, G_TYPE_VALUE_ARRAY, &bools)) {
		type_error (msg, G_TYPE_VALUE_ARRAY, params, 0);
		return;
	}

	for (i = 0; i < bools->n_values; i++) {
		if (!soup_value_array_get_nth (bools, i, G_TYPE_BOOLEAN, &val)) {
			type_error (msg, G_TYPE_BOOLEAN, params, i);
			return;
		}
		if (val)
			trues++;
		else
			falses++;
	}

	soup_value_hash_insert (ret, "true", G_TYPE_INT, trues);
	soup_value_hash_insert (ret, "false", G_TYPE_INT, falses);
	soup_xmlrpc_set_response (msg, G_TYPE_HASH_TABLE, ret);
	g_hash_table_destroy (ret);

}

static void
do_md5sum (SoupMessage *msg, GValueArray *params)
{
	GChecksum *checksum;
	GByteArray *data, *digest;
	gsize digest_len = 16;

	if (params->n_values != 1) {
		args_error (msg, params, 1);
		return;
	}

	if (!soup_value_array_get_nth (params, 0, SOUP_TYPE_BYTE_ARRAY, &data)) {
		type_error (msg, SOUP_TYPE_BYTE_ARRAY, params, 0);
		return;
	}
	checksum = g_checksum_new (G_CHECKSUM_MD5);
	g_checksum_update (checksum, data->data, data->len);
	digest = g_byte_array_new ();
	g_byte_array_set_size (digest, digest_len);
	g_checksum_get_digest (checksum, digest->data, &digest_len);
	g_checksum_free (checksum);

	soup_xmlrpc_set_response (msg, SOUP_TYPE_BYTE_ARRAY, digest);
	g_byte_array_free (digest, TRUE);
}


static void
do_dateChange (SoupMessage *msg, GValueArray *params)
{
	GHashTable *arg;
	SoupDate *date;
	int val;

	if (params->n_values != 2) {
		args_error (msg, params, 2);
		return;
	}

	if (!soup_value_array_get_nth (params, 0, SOUP_TYPE_DATE, &date)) {
		type_error (msg, SOUP_TYPE_DATE, params, 0);
		return;
	}
	if (!soup_value_array_get_nth (params, 1, G_TYPE_HASH_TABLE, &arg)) {
		type_error (msg, G_TYPE_HASH_TABLE, params, 1);
		return;
	}

	if (soup_value_hash_lookup (arg, "tm_year", G_TYPE_INT, &val))
		date->year = val + 1900;
	if (soup_value_hash_lookup (arg, "tm_mon", G_TYPE_INT, &val))
		date->month = val + 1;
	if (soup_value_hash_lookup (arg, "tm_mday", G_TYPE_INT, &val))
		date->day = val;
	if (soup_value_hash_lookup (arg, "tm_hour", G_TYPE_INT, &val))
		date->hour = val;
	if (soup_value_hash_lookup (arg, "tm_min", G_TYPE_INT, &val))
		date->minute = val;
	if (soup_value_hash_lookup (arg, "tm_sec", G_TYPE_INT, &val))
		date->second = val;

	soup_xmlrpc_set_response (msg, SOUP_TYPE_DATE, date);
}

static void
do_echo (SoupMessage *msg, GValueArray *params)
{
	int i;
	const char *val;
	GValueArray *in, *out;

	if (!soup_value_array_get_nth (params, 0, G_TYPE_VALUE_ARRAY, &in)) {
		type_error (msg, G_TYPE_VALUE_ARRAY, params, 0);
		return;
	}

	out = g_value_array_new (in->n_values);
	for (i = 0; i < in->n_values; i++) {
		if (!soup_value_array_get_nth (in, i, G_TYPE_STRING, &val)) {
			type_error (msg, G_TYPE_STRING, in, i);
			return;
		}
		soup_value_array_append (out, G_TYPE_STRING, val);
	}

	soup_xmlrpc_set_response (msg, G_TYPE_VALUE_ARRAY, out);
	g_value_array_free (out);

}

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	char *method_name;
	GValueArray *params;

	if (msg->method != SOUP_METHOD_POST) {
		soup_message_set_status (msg, SOUP_STATUS_NOT_IMPLEMENTED);
		return;
	}

	soup_message_set_status (msg, SOUP_STATUS_OK);

	if (!soup_xmlrpc_parse_method_call (msg->request_body->data,
					    msg->request_body->length,
					    &method_name, &params)) {
		soup_xmlrpc_set_fault (msg, SOUP_XMLRPC_FAULT_PARSE_ERROR_NOT_WELL_FORMED,
				       "Could not parse method call");
		return;
	}

	if (!strcmp (method_name, "sum"))
		do_sum (msg, params);
	else if (!strcmp (method_name, "countBools"))
		do_countBools (msg, params);
	else if (!strcmp (method_name, "md5sum"))
		do_md5sum (msg, params);
	else if (!strcmp (method_name, "dateChange"))
		do_dateChange (msg, params);
	else if (!strcmp (method_name, "echo"))
		do_echo (msg, params);
	else {
		soup_xmlrpc_set_fault (msg, SOUP_XMLRPC_FAULT_SERVER_ERROR_REQUESTED_METHOD_NOT_FOUND,
				       "Unknown method %s", method_name);
	}

	g_free (method_name);
	g_value_array_free (params);
}

static void
xmlrpc_test_exited (GPid pid, int status, gpointer data)
{
	errors = WIFEXITED (status) ? WEXITSTATUS (status) : 1;
	g_main_loop_quit (loop);
}

static gboolean
xmlrpc_test_print (GIOChannel *io, GIOCondition cond, gpointer data)
{
	char *line;
	gsize len;
	GIOStatus status;

	if (!(cond & G_IO_IN))
		return FALSE;

	status = g_io_channel_read_line (io, &line, &len, NULL, NULL);
	if (status == G_IO_STATUS_NORMAL) {
		/* Don't print the exit status, just the debug stuff */
		if (strncmp (line, "xmlrpc-test:", strlen ("xmlrpc-test:")) != 0)
			printf ("%s", line);
		g_free (line);
		return TRUE;
	} else if (status == G_IO_STATUS_AGAIN)
		return TRUE;
	else
		return FALSE;
}

static void
do_xmlrpc_tests (SoupURI *uri)
{
	char *argv[7];
	int arg, out;
	gboolean ok;
	GPid pid;
	GError *error = NULL;
	GIOChannel *child_out;

	argv[0] = "./xmlrpc-test";
	argv[1] = "-u";
	argv[2] = soup_uri_to_string (uri, FALSE);

	for (arg = 0; arg < debug_level && arg < 3; arg++)
		argv[arg + 3] = "-d";
	argv[arg + 3] = NULL;

	ok = g_spawn_async_with_pipes (NULL, argv, NULL,
				       G_SPAWN_DO_NOT_REAP_CHILD,
				       NULL, NULL, &pid,
				       NULL, &out, NULL,
				       &error);
	g_free (argv[2]);

	if (!ok) {
		printf ("Could not run xmlrpc-test: %s\n", error->message);
		errors++;
		return;
	}

	g_child_watch_add (pid, xmlrpc_test_exited, NULL);
	child_out = g_io_channel_unix_new (out);
	g_io_add_watch (child_out, G_IO_IN | G_IO_ERR | G_IO_HUP,
			xmlrpc_test_print, NULL);
	g_io_channel_unref (child_out);
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
	SoupServer *server;
	SoupURI *uri;

	test_init (argc, argv, no_test_entry);

	server = soup_test_server_new (FALSE);
	soup_server_add_handler (server, "/xmlrpc-server.php",
				 server_callback, NULL, NULL);

	loop = g_main_loop_new (NULL, TRUE);

	if (run_tests) {
		uri = soup_uri_new ("http://localhost/xmlrpc-server.php");
		soup_uri_set_port (uri, soup_server_get_port (server));
		do_xmlrpc_tests (uri);
		soup_uri_free (uri);
	} else
		printf ("Listening on port %d\n", soup_server_get_port (server));

	g_main_loop_run (loop);
	g_main_loop_unref (loop);

	if (run_tests)
		test_cleanup ();
	return errors != 0;
}
