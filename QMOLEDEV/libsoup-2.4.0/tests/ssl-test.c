#include <gnutls/gnutls.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <sys/socket.h>

#include "libsoup/soup-address.h"
#include "libsoup/soup-socket.h"
#include "libsoup/soup-ssl.h"

#define BUFSIZE 1024
#define DH_BITS 1024

GMainLoop *loop;
gnutls_dh_params_t dh_params;

/* SERVER */

/* Read @bufsize bytes into @buf from @session. */
static void
server_read (gnutls_session_t session, char *buf, int bufsize)
{
	int total, nread;

	total = 0;
	while (total < bufsize) {
		nread = gnutls_record_recv (session, buf + total,
					    bufsize - total);
		if (nread <= 0)
			g_error ("server read failed at position %d", total);
		total += nread;
	}
}

/* Write @bufsize bytes from @buf to @session, forcing 3 rehandshakes
 * along the way. (We do an odd number of rehandshakes to make sure
 * they occur at weird times relative to the client's read buffer
 * size.)
 */
static void
server_write (gnutls_session_t session, char *buf, int bufsize)
{
	int total, nwrote;
	int next_rehandshake = bufsize / 3;

	total = 0;
	while (total < bufsize) {
		if (total >= next_rehandshake) {
			if (gnutls_rehandshake (session) < 0)
				g_error ("client refused rehandshake at position %d", total);
			if (gnutls_handshake (session) < 0)
				g_error ("server rehandshake failed at position %d", total);
			next_rehandshake = MIN (bufsize, next_rehandshake + bufsize / 3);
		}

		nwrote = gnutls_record_send (session, buf + total,
					     next_rehandshake - total);
		if (nwrote <= 0)
			g_error ("server write failed at position %d: %d", total, nwrote);
		total += nwrote;
	}
}

const char *ssl_cert_file = SRCDIR "/test-cert.pem";
const char *ssl_key_file = SRCDIR "/test-key.pem";

static gpointer
server_thread (gpointer user_data)
{
	int listener = GPOINTER_TO_INT (user_data), client;
	gnutls_certificate_credentials creds;
	gnutls_session_t session;
	struct sockaddr_in sin;
	int len;
	char buf[BUFSIZE];
	int status;

	gnutls_certificate_allocate_credentials (&creds);
	if (gnutls_certificate_set_x509_key_file (creds,
						  ssl_cert_file, ssl_key_file,
						  GNUTLS_X509_FMT_PEM) != 0) {
		g_error ("Failed to set SSL certificate and key files "
			 "(%s, %s).", ssl_cert_file, ssl_key_file);
	}
	gnutls_certificate_set_dh_params (creds, dh_params);

	/* Create a new session */
	gnutls_init (&session, GNUTLS_SERVER);
	gnutls_set_default_priority (session);
	gnutls_credentials_set (session, GNUTLS_CRD_CERTIFICATE, creds);
	gnutls_dh_set_prime_bits (session, DH_BITS);

	/* Wait for client thread to connect */
	len = sizeof (sin);
	client = accept (listener, (struct sockaddr *) &sin, (void *)&len);
	gnutls_transport_set_ptr (session, GINT_TO_POINTER (client));

	/* Initial handshake */
	status = gnutls_handshake (session);
	if (status < 0)
		g_error ("initial handshake failed: %d", status);

	/* Synchronous client test. */
	server_read (session, buf, BUFSIZE);
	server_write (session, buf, BUFSIZE);

	/* Async client test. */
	server_read (session, buf, BUFSIZE);
	server_write (session, buf, BUFSIZE);

	/* That's all, folks. */
	gnutls_bye (session, GNUTLS_SHUT_WR);
	gnutls_deinit (session);
	close (client);
	gnutls_certificate_free_credentials (creds);

	return NULL;
}

/* async client code */

typedef struct {
	char writebuf[BUFSIZE], readbuf[BUFSIZE];
	int total;
} AsyncData;

static void
async_read (SoupSocket *sock, gpointer user_data)
{
	AsyncData *data = user_data;
	SoupSocketIOStatus status;
	gsize n;
	GError *error = NULL;

	do {
		status = soup_socket_read (sock, data->readbuf + data->total,
					   BUFSIZE - data->total, &n,
					   NULL, &error);
		if (status == SOUP_SOCKET_OK)
			data->total += n;
	} while (status == SOUP_SOCKET_OK && data->total < BUFSIZE);

	if (status == SOUP_SOCKET_ERROR || status == SOUP_SOCKET_EOF) {
		g_error ("Async read got status %d: %s", status,
			 error ? error->message : "(unknown)");
	} else if (status == SOUP_SOCKET_WOULD_BLOCK)
		return;

	if (memcmp (data->writebuf, data->readbuf, BUFSIZE) != 0)
		g_error ("Sync read didn't match write");

	g_free (data);
	g_main_loop_quit (loop);
}

static void
async_write (SoupSocket *sock, gpointer user_data)
{
	AsyncData *data = user_data;
	SoupSocketIOStatus status;
	gsize n;
	GError *error = NULL;

	do {
		status = soup_socket_write (sock, data->writebuf + data->total,
					    BUFSIZE - data->total, &n,
					    NULL, &error);
		if (status == SOUP_SOCKET_OK)
			data->total += n;
	} while (status == SOUP_SOCKET_OK && data->total < BUFSIZE);

	if (status == SOUP_SOCKET_ERROR || status == SOUP_SOCKET_EOF) {
		g_error ("Async write got status %d: %s", status,
			 error ? error->message : "(unknown)");
	} else if (status == SOUP_SOCKET_WOULD_BLOCK)
		return;

	data->total = 0;
	async_read (sock, user_data);
}

static gboolean
start_writing (gpointer user_data)
{
	SoupSocket *sock = user_data;
	AsyncData *data;
	int i;

	data = g_new (AsyncData, 1);
	for (i = 0; i < BUFSIZE; i++)
		data->writebuf[i] = i & 0xFF;
	data->total = 0;

	g_signal_connect (sock, "writable",
			  G_CALLBACK (async_write), data);
	g_signal_connect (sock, "readable",
			  G_CALLBACK (async_read), data);

	async_write (sock, data);
	return FALSE;
}

int debug;

static void
debug_log (int level, const char *str)
{
  fputs (str, stderr);
}

int
main (int argc, char **argv)
{
	int opt, listener, sin_len, port, i;
	struct sockaddr_in sin;
	GThread *server;
	char writebuf[BUFSIZE], readbuf[BUFSIZE];
	SoupAddress *addr;
	SoupSSLCredentials *creds;
	SoupSocket *sock;
	gsize n, total;
	SoupSocketIOStatus status;
	GError *error = NULL;

	g_type_init ();
	g_thread_init (NULL);

	while ((opt = getopt (argc, argv, "c:d:k:")) != -1) {
		switch (opt) {
		case 'c':
			ssl_cert_file = optarg;
			break;
		case 'd':
			debug = atoi (optarg);
			break;
		case 'k':
			ssl_key_file = optarg;
			break;

		case '?':
			fprintf (stderr, "Usage: %s [-d debuglevel] [-c ssl-cert-file] [-k ssl-key-file]\n",
				 argv[0]);
			break;
		}
	}

	if (debug) {
		gnutls_global_set_log_function (debug_log);
		gnutls_global_set_log_level (debug);
	}

	/* Create server socket */
	listener = socket (AF_INET, SOCK_STREAM, 0);
	if (listener == -1) {
		perror ("creating listening socket");
		exit (1);
	}

	memset (&sin, 0, sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;

	if (bind (listener, (struct sockaddr *) &sin, sizeof (sin))  == -1) {
		perror ("binding listening socket");
		exit (1);
	}

	if (listen (listener, 1) == -1) {
		perror ("listening on socket");
		exit (1);
	}

	sin_len = sizeof (sin);
	getsockname (listener, (struct sockaddr *)&sin, (void *)&sin_len);
	port = ntohs (sin.sin_port);

	/* Create the client */
	addr = soup_address_new ("127.0.0.1", port);
	creds = soup_ssl_get_client_credentials (NULL);
	sock = soup_socket_new (SOUP_SOCKET_REMOTE_ADDRESS, addr,
				SOUP_SOCKET_FLAG_NONBLOCKING, FALSE,
				SOUP_SOCKET_SSL_CREDENTIALS, creds,
				NULL);
	g_object_unref (addr);
	status = soup_socket_connect_sync (sock, NULL);
	if (status != SOUP_STATUS_OK) {
		g_error ("Could not create client socket: %s",
			 soup_status_get_phrase (status));
	}

	soup_socket_start_ssl (sock, NULL);

	/* Now spawn server thread */
	server = g_thread_create (server_thread, GINT_TO_POINTER (listener),
				  TRUE, NULL);

	/* Synchronous client test */
	for (i = 0; i < BUFSIZE; i++)
		writebuf[i] = i & 0xFF;

	total = 0;
	while (total < BUFSIZE) {
		status = soup_socket_write (sock, writebuf + total,
					    BUFSIZE - total, &n,
					    NULL, &error);
		if (status != SOUP_SOCKET_OK)
			g_error ("Sync write got status %d: %s", status,
				 error ? error->message : "(unknown)");
		total += n;
	}

	total = 0;
	while (total < BUFSIZE) {
		status = soup_socket_read (sock, readbuf + total,
					   BUFSIZE - total, &n,
					   NULL, &error);
		if (status != SOUP_SOCKET_OK)
			g_error ("Sync read got status %d: %s", status,
				 error ? error->message : "(unknown)");
		total += n;
	}

	if (memcmp (writebuf, readbuf, BUFSIZE) != 0)
		g_error ("Sync read didn't match write");

	printf ("SYNCHRONOUS SSL TEST PASSED\n");

	/* Switch socket to async and do it again */

	g_object_set (sock,
		      SOUP_SOCKET_FLAG_NONBLOCKING, TRUE,
		      NULL);

	g_idle_add (start_writing, sock);
	loop = g_main_loop_new (NULL, TRUE);
	g_main_loop_run (loop);
	g_main_loop_unref (loop);
	g_main_context_unref (g_main_context_default ());

	printf ("ASYNCHRONOUS SSL TEST PASSED\n");

	g_object_unref (sock);
	soup_ssl_free_client_credentials (creds);
	g_thread_join (server);

	/* Success */
	return 0;
}
