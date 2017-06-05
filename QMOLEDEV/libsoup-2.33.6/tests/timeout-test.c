#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "libsoup/soup.h"

#include "test-utils.h"

static void
do_message_to_session (SoupSession *session, const char *uri,
		       const char *comment, guint expected_status)
{
	SoupMessage *msg;

	debug_printf (1, "    %s\n", comment);
	msg = soup_message_new ("GET", uri);
	soup_session_send_message (session, msg);

	if (msg->status_code != expected_status) {
		debug_printf (1, "      FAILED: %d %s (expected %d %s)\n",
			      msg->status_code, msg->reason_phrase,
			      expected_status,
			      soup_status_get_phrase (expected_status));
		errors++;
	}

	if (SOUP_STATUS_IS_SUCCESSFUL (msg->status_code) &&
	    !soup_message_is_keepalive (msg)) {
		debug_printf (1, "      ERROR: message is not keepalive!");
		errors++;
	}

	g_object_unref (msg);
}

static void
request_started_cb (SoupSession *session, SoupMessage *msg,
		    SoupSocket *socket, gpointer user_data)
{
	SoupSocket **ret = user_data;

	*ret = socket;
}

static void
do_tests_for_session (SoupSession *timeout_session,
		      SoupSession *idle_session,
		      SoupSession *plain_session,
		      char *fast_uri, char *slow_uri)
{
	SoupSocket *ret, *idle_first, *idle_second;
	SoupSocket *plain_first, *plain_second;

	if (idle_session) {
		g_signal_connect (idle_session, "request-started",
				  G_CALLBACK (request_started_cb), &ret);
		do_message_to_session (idle_session, fast_uri, "fast to idle", SOUP_STATUS_OK);
		idle_first = ret;
	}

	if (plain_session) {
		g_signal_connect (plain_session, "request-started",
				  G_CALLBACK (request_started_cb), &ret);
		do_message_to_session (plain_session, fast_uri, "fast to plain", SOUP_STATUS_OK);
		plain_first = ret;
	}

	do_message_to_session (timeout_session, fast_uri, "fast to timeout", SOUP_STATUS_OK);
	do_message_to_session (timeout_session, slow_uri, "slow to timeout", SOUP_STATUS_IO_ERROR);

	if (idle_session) {
		do_message_to_session (idle_session, fast_uri, "fast to idle", SOUP_STATUS_OK);
		idle_second = ret;
		g_signal_handlers_disconnect_by_func (idle_session,
						      (gpointer)request_started_cb,
						      &ret);

		if (idle_first == idle_second) {
			debug_printf (1, "      ERROR: idle_session did not close first connection\n");
			errors++;
		}
	}

	if (plain_session) {
		do_message_to_session (plain_session, fast_uri, "fast to plain", SOUP_STATUS_OK);
		plain_second = ret;
		g_signal_handlers_disconnect_by_func (plain_session,
						      (gpointer)request_started_cb,
						      &ret);

		if (plain_first != plain_second) {
			debug_printf (1, "      ERROR: plain_session closed connection\n");
			errors++;
		}
	}
}

static void
do_timeout_tests (char *fast_uri, char *slow_uri)
{
	SoupSession *timeout_session, *idle_session, *plain_session;

	debug_printf (1, "  async\n");
	timeout_session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC,
					 SOUP_SESSION_TIMEOUT, 1,
					 NULL);
	idle_session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC,
					      SOUP_SESSION_IDLE_TIMEOUT, 1,
					      NULL);
	/* The "plain" session also has an idle timeout, but it's longer
	 * than the test takes, so for our purposes it should behave like
	 * it has no timeout.
	 */
	plain_session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC,
					       SOUP_SESSION_IDLE_TIMEOUT, 2,
					       NULL);
	do_tests_for_session (timeout_session, idle_session, plain_session,
			      fast_uri, slow_uri);
	soup_test_session_abort_unref (timeout_session);
	soup_test_session_abort_unref (idle_session);
	soup_test_session_abort_unref (plain_session);

	debug_printf (1, "  sync\n");
	timeout_session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC,
						 SOUP_SESSION_TIMEOUT, 1,
						 NULL);
	/* SOUP_SESSION_TIMEOUT doesn't work with sync sessions */
	plain_session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC,
					       NULL);
	do_tests_for_session (timeout_session, NULL, plain_session, fast_uri, slow_uri);
	soup_test_session_abort_unref (timeout_session);
}

static gboolean
timeout_finish_message (gpointer msg)
{
	SoupServer *server = g_object_get_data (G_OBJECT (msg), "server");

	soup_server_unpause_message (server, msg);
	return FALSE;
}

static void
server_handler (SoupServer        *server,
		SoupMessage       *msg, 
		const char        *path,
		GHashTable        *query,
		SoupClientContext *client,
		gpointer           user_data)
{
	soup_message_set_status (msg, SOUP_STATUS_OK);
	soup_message_set_response (msg, "text/plain",
				   SOUP_MEMORY_STATIC,
				   "ok\r\n", 4);

	if (!strcmp (path, "/slow")) {
		soup_server_pause_message (server, msg);
		g_object_set_data (G_OBJECT (msg), "server", server);
		soup_add_timeout (soup_server_get_async_context (server),
				  1100, timeout_finish_message, msg);
	}
}

int
main (int argc, char **argv)
{
	SoupServer *server;
	char *fast_uri, *slow_uri;

	test_init (argc, argv, NULL);

	debug_printf (1, "http\n");
	server = soup_test_server_new (TRUE);
	soup_server_add_handler (server, NULL, server_handler, NULL, NULL);
	fast_uri = g_strdup_printf ("http://127.0.0.1:%u/",
				    soup_server_get_port (server));
	slow_uri = g_strdup_printf ("http://127.0.0.1:%u/slow",
				    soup_server_get_port (server));
	do_timeout_tests (fast_uri, slow_uri);
	g_free (fast_uri);
	g_free (slow_uri);
	soup_test_server_quit_unref (server);

	if (tls_available) {
		debug_printf (1, "\nhttps\n");
		server = soup_test_server_new_ssl (TRUE);
		soup_server_add_handler (server, NULL, server_handler, NULL, NULL);
		fast_uri = g_strdup_printf ("https://127.0.0.1:%u/",
					    soup_server_get_port (server));
		slow_uri = g_strdup_printf ("https://127.0.0.1:%u/slow",
					    soup_server_get_port (server));
		do_timeout_tests (fast_uri, slow_uri);
		g_free (fast_uri);
		g_free (slow_uri);
		soup_test_server_quit_unref (server);
	}

	test_cleanup ();
	return errors != 0;
}
