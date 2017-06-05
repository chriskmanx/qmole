/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2001-2003, Ximian, Inc.
 */

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <libsoup/soup.h>

GMainLoop *loop;

static void
print_value (GValue *value)
{
	if (G_VALUE_HOLDS_STRING (value))
		printf ("%s", g_value_get_string (value));
	else if (G_VALUE_HOLDS_INT (value))
		printf ("%d", g_value_get_int (value));
	else if (G_VALUE_HOLDS_DOUBLE (value))
		printf ("%f", g_value_get_double (value));
	else if (G_VALUE_TYPE (value) == G_TYPE_VALUE_ARRAY) {
		GValueArray *array = g_value_get_boxed (value);
		int i;
		printf ("[ ");
		for (i = 0; i < array->n_values; i++) {
			if (i != 0)
				printf (", ");
			print_value (&array->values[i]);
		}
		printf (" ]");
	} else
		printf ("(%s)", g_type_name (G_VALUE_TYPE (value)));
}

static void
print_struct_field (gpointer key, gpointer value, gpointer data)
{
	printf ("%s: ", (char *)key);
	print_value (value);
	printf ("\n");
}

static void
got_response (SoupSession *session, SoupMessage *msg, gpointer user_data)
{
	GHashTable *hash;
	GError *error = NULL;

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		fprintf (stderr, "%d %s\n", msg->status_code, msg->reason_phrase);
		exit (1);
	}

	if (!soup_xmlrpc_extract_method_response (msg->response_body->data,
						  msg->response_body->length,
						  &error,
						  G_TYPE_HASH_TABLE, &hash)) {
		if (!error) {
			fprintf (stderr, "Could not parse XMLRPC response:\n%d %s\n\n",
				 msg->status_code, msg->reason_phrase);
			fprintf (stderr, "%s\n", msg->response_body->data);
		} else {
			fprintf (stderr, "XML-RPC error: %d %s",
				 error->code, error->message);
		}
		exit (1);
	}

	g_hash_table_foreach (hash, print_struct_field, NULL);
	g_hash_table_destroy (hash);

	g_main_quit (loop);
}

static void
usage (void)
{
	fprintf (stderr, "Usage: getbug [-p proxy_uri] [bugzilla-uri] bug-number\n");
	exit (1);
}

int
main (int argc, char **argv)
{
	SoupSession *session;
	SoupURI *proxy = NULL;
	SoupMessage *msg;
	const char *uri = "http://bugzilla.redhat.com/bugzilla/xmlrpc.cgi";
	int opt, bug;

	g_type_init ();
	g_thread_init (NULL);

	while ((opt = getopt (argc, argv, "p:")) != -1) {
		switch (opt) {
		case 'p':
			proxy = soup_uri_new (optarg);
			if (!proxy) {
				fprintf (stderr, "Could not parse %s as URI\n",
					 optarg);
				exit (1);
			}
			break;

		case '?':
			usage ();
			break;
		}
	}
	argc -= optind;
	argv += optind;

	if (argc > 1) {
		uri = argv[0];
		argc--;
		argv++;
	}

	if (argc != 1 || (bug = atoi (argv[0])) == 0)
		usage ();

	session = soup_session_async_new_with_options (
		SOUP_SESSION_PROXY_URI, proxy,
		NULL);

	msg = soup_xmlrpc_request_new (uri, "bugzilla.getBug",
				       G_TYPE_INT, bug,
				       G_TYPE_INVALID);
	if (!msg) {
		fprintf (stderr, "Could not create web service request to '%s'\n", uri);
		exit (1);
	}
	soup_session_queue_message (session, SOUP_MESSAGE (msg),
				    got_response, NULL);

	loop = g_main_loop_new (NULL, TRUE);
	g_main_run (loop);
	g_main_loop_unref (loop);

	return 0;
}
