/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2001-2003, Ximian, Inc.
 */

#include "config.h"

#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif

#include <libsoup/soup.h>

#ifdef HAVE_MMAP
struct mapping {
	void   *start;
	size_t  length;
};

static void
free_mapping (gpointer data)
{
	struct mapping *mapping = data;
	munmap (mapping->start, mapping->length);
	g_slice_free (struct mapping, mapping);
}
#endif

static void
do_get (SoupServer *server, SoupMessage *msg, const char *path)
{
	char *slash;
	struct stat st;
	int fd;

	if (stat (path, &st) == -1) {
		if (errno == EPERM)
			soup_message_set_status (msg, SOUP_STATUS_FORBIDDEN);
		else if (errno == ENOENT)
			soup_message_set_status (msg, SOUP_STATUS_NOT_FOUND);
		else
			soup_message_set_status (msg, SOUP_STATUS_INTERNAL_SERVER_ERROR);
		return;
	}

	if (S_ISDIR (st.st_mode)) {
		char *index_path;

		slash = strrchr (path, '/');
		if (!slash || slash[1]) {
			char *uri, *redir_uri;

			uri = soup_uri_to_string (soup_message_get_uri (msg), FALSE);
			redir_uri = g_strdup_printf ("%s/", uri);
			soup_message_headers_append (msg->response_headers,
						     "Location", redir_uri);
			soup_message_set_status (msg, SOUP_STATUS_MOVED_PERMANENTLY);
			g_free (redir_uri);
			g_free (uri);
			return;
		}

		index_path = g_strdup_printf ("%s/index.html", path);
		do_get (server, msg, index_path);
		g_free (index_path);
		return;
	}

	fd = open (path, O_RDONLY);
	if (fd == -1) {
		soup_message_set_status (msg, SOUP_STATUS_INTERNAL_SERVER_ERROR);
		return;
	}

	if (msg->method == SOUP_METHOD_GET) {
#ifdef HAVE_MMAP
		struct mapping *mapping = g_slice_new (struct mapping);
		SoupBuffer *buffer;

		mapping->start = mmap (NULL, st.st_size, PROT_READ,
				       MAP_PRIVATE, fd, 0);
		mapping->length = st.st_size;
		buffer = soup_buffer_new_with_owner (mapping->start,
						     mapping->length,
						     mapping, free_mapping);
		soup_message_body_append_buffer (msg->response_body, buffer);
		soup_buffer_free (buffer);
#else
		char *buf;

		buf = g_malloc (st.st_size);
		read (fd, buf, st.st_size);
		close (fd);
		soup_message_body_append (msg->response_body, SOUP_MEMORY_TAKE,
					  buf, st.st_size);
#endif
	} else /* msg->method == SOUP_METHOD_HEAD */ {
		char *length;

		/* We could just use the same code for both GET and
		 * HEAD. But we'll optimize and avoid the extra
		 * malloc.
		 */
		length = g_strdup_printf ("%lu", (gulong)st.st_size);
		soup_message_headers_append (msg->response_headers,
					     "Content-Length", length);
		g_free (length);
	}

	soup_message_set_status (msg, SOUP_STATUS_OK);
}

static void
do_put (SoupServer *server, SoupMessage *msg, const char *path)
{
	struct stat st;
	FILE *f;
	gboolean created = TRUE;

	if (stat (path, &st) != -1) {
		const char *match = soup_message_headers_get (msg->request_headers, "If-None-Match");
		if (match && !strcmp (match, "*")) {
			soup_message_set_status (msg, SOUP_STATUS_CONFLICT);
			return;
		}

		if (!S_ISREG (st.st_mode)) {
			soup_message_set_status (msg, SOUP_STATUS_FORBIDDEN);
			return;
		}

		created = FALSE;
	}

	f = fopen (path, "w");
	if (!f) {
		soup_message_set_status (msg, SOUP_STATUS_INTERNAL_SERVER_ERROR);
		return;
	}

	fwrite (msg->request_body->data, 1, msg->request_body->length, f);
	fclose (f);

	soup_message_set_status (msg, created ? SOUP_STATUS_CREATED : SOUP_STATUS_OK);
}

static void
server_callback (SoupServer *server, SoupMessage *msg,
		 const char *path, GHashTable *query,
		 SoupClientContext *context, gpointer data)
{
	char *file_path;
	SoupMessageHeadersIter iter;
	const char *name, *value;

	printf ("%s %s HTTP/1.%d\n", msg->method, path,
		soup_message_get_http_version (msg));
	soup_message_headers_iter_init (&iter, msg->request_headers);
	while (soup_message_headers_iter_next (&iter, &name, &value))
		printf ("%s: %s\n", name, value);
	if (msg->request_body->length)
		printf ("%s\n", msg->request_body->data);

	file_path = g_strdup_printf (".%s", path);

	if (msg->method == SOUP_METHOD_GET || msg->method == SOUP_METHOD_HEAD)
		do_get (server, msg, file_path);
	else if (msg->method == SOUP_METHOD_PUT)
		do_put (server, msg, file_path);
	else
		soup_message_set_status (msg, SOUP_STATUS_NOT_IMPLEMENTED);

	g_free (file_path);
	printf ("  -> %d %s\n\n", msg->status_code, msg->reason_phrase);
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
	SoupServer *server, *ssl_server;
	int opt;
	int port = SOUP_ADDRESS_ANY_PORT;
	int ssl_port = SOUP_ADDRESS_ANY_PORT;
	const char *ssl_cert_file = NULL, *ssl_key_file = NULL;

	g_type_init ();
	g_thread_init (NULL);
	signal (SIGINT, quit);

	while ((opt = getopt (argc, argv, "p:k:c:s:")) != -1) {
		switch (opt) {
		case 'p':
			port = atoi (optarg);
			break;
		case 'k':
			ssl_key_file = optarg;
			break;
		case 'c':
			ssl_cert_file = optarg;
			break;
		case 's':
			ssl_port = atoi (optarg);
			break;
		default:
			fprintf (stderr, "Usage: %s [-p port] [-c ssl-cert-file -k ssl-key-file [-s ssl-port]]\n",
				 argv[0]);
			exit (1);
		}
	}

	server = soup_server_new (SOUP_SERVER_PORT, port,
				  SOUP_SERVER_SERVER_HEADER, "simple-httpd ",
				  NULL);
	if (!server) {
		fprintf (stderr, "Unable to bind to server port %d\n", port);
		exit (1);
	}
	soup_server_add_handler (server, NULL,
				 server_callback, NULL, NULL);
	printf ("\nStarting Server on port %d\n",
		soup_server_get_port (server));
	soup_server_run_async (server);

	if (ssl_cert_file && ssl_key_file) {
		ssl_server = soup_server_new (
			SOUP_SERVER_PORT, ssl_port,
			SOUP_SERVER_SSL_CERT_FILE, ssl_cert_file,
			SOUP_SERVER_SSL_KEY_FILE, ssl_key_file,
			NULL);

		if (!ssl_server) {
			fprintf (stderr, "Unable to bind to SSL server port %d\n", ssl_port);
			exit (1);
		}
		soup_server_add_handler (ssl_server, NULL,
					 server_callback, NULL, NULL);
		printf ("Starting SSL Server on port %d\n", 
			soup_server_get_port (ssl_server));
		soup_server_run_async (ssl_server);
	}

	printf ("\nWaiting for requests...\n");

	loop = g_main_loop_new (NULL, TRUE);
	g_main_loop_run (loop);

	return 0;
}
