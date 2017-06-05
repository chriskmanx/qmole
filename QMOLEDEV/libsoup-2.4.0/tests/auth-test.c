#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "libsoup/soup.h"
#include "libsoup/soup-auth.h"
#include "libsoup/soup-session.h"

#include "test-utils.h"

GMainLoop *loop;

typedef struct {
	/* Explanation of what you should see */
	const char *explanation;

	/* URL to test against */
	const char *url;

	/* Provided passwords, 1 character each. ('1', '2', and '3'
	 * mean the correct passwords for "realm1", "realm2", and
	 * "realm3" respectively. '4' means "use the wrong password".)
	 * The first password (if present) will be used by
	 * authenticate(), and the second (if present) will be used by
	 * reauthenticate().
	 */
	const char *provided;

	/* Expected passwords, 1 character each. (As with the provided
	 * passwords, with the addition that '0' means "no
	 * Authorization header expected".) Used to verify that soup
	 * used the password it was supposed to at each step.
	 */
	const char *expected;

	/* What the final status code should be. */
	guint final_status;
} SoupAuthTest;

SoupAuthTest tests[] = {
	{ "No auth available, should fail",
	  "Basic/realm1/", "", "0", SOUP_STATUS_UNAUTHORIZED },

	{ "Should fail with no auth, fail again with bad password, and give up",
	  "Basic/realm2/", "4", "04", SOUP_STATUS_UNAUTHORIZED },

	{ "Known realm, auth provided, so should succeed immediately",
	  "Basic/realm1/", "1", "1", SOUP_STATUS_OK },

	{ "Now should automatically reuse previous auth",
	  "Basic/realm1/", "", "1", SOUP_STATUS_OK },

	{ "Subdir should also automatically reuse auth",
	  "Basic/realm1/subdir/", "", "1", SOUP_STATUS_OK },

	{ "Subdir should retry last auth, but will fail this time",
	  "Basic/realm1/realm2/", "", "1", SOUP_STATUS_UNAUTHORIZED },

	{ "Now should use provided auth on first try",
	  "Basic/realm1/realm2/", "2", "2", SOUP_STATUS_OK },

	{ "Reusing last auth. Should succeed on first try",
	  "Basic/realm1/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Reuse will fail, but 2nd try will succeed because it's a known realm",
	  "Basic/realm1/realm2/realm1/", "", "21", SOUP_STATUS_OK },

	{ "Should succeed on first try. (Known realm with cached password)",
	  "Basic/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Fail once, then use typoed password, then use right password",
	  "Basic/realm3/", "43", "043", SOUP_STATUS_OK },


	{ "No auth available, should fail",
	  "Digest/realm1/", "", "0", SOUP_STATUS_UNAUTHORIZED },

	{ "Should fail with no auth, fail again with bad password, and give up",
	  "Digest/realm2/", "4", "04", SOUP_STATUS_UNAUTHORIZED },

	{ "Known realm, auth provided, so should succeed immediately",
	  "Digest/realm1/", "1", "1", SOUP_STATUS_OK },

	{ "Now should automatically reuse previous auth",
	  "Digest/realm1/", "", "1", SOUP_STATUS_OK },

	{ "Subdir should also automatically reuse auth",
	  "Digest/realm1/subdir/", "", "1", SOUP_STATUS_OK },

	{ "Should already know correct domain and use provided auth on first try",
	  "Digest/realm1/realm2/", "2", "2", SOUP_STATUS_OK },

	{ "Reusing last auth. Should succeed on first try",
	  "Digest/realm1/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Should succeed on first try because of earlier domain directive",
	  "Digest/realm1/realm2/realm1/", "", "1", SOUP_STATUS_OK },

	{ "Should succeed on first try. (Known realm with cached password)",
	  "Digest/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Fail once, then use typoed password, then use right password",
	  "Digest/realm3/", "43", "043", SOUP_STATUS_OK },


	{ "Make sure we haven't forgotten anything",
	  "Basic/realm1/", "", "1", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Basic/realm1/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Basic/realm1/realm2/realm1/", "", "1", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Basic/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Basic/realm3/", "", "3", SOUP_STATUS_OK },


	{ "Make sure we haven't forgotten anything",
	  "Digest/realm1/", "", "1", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Digest/realm1/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Digest/realm1/realm2/realm1/", "", "1", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Digest/realm2/", "", "2", SOUP_STATUS_OK },

	{ "Make sure we haven't forgotten anything",
	  "Digest/realm3/", "", "3", SOUP_STATUS_OK },

	{ "Now the server will reject the formerly-good password",
	  "Basic/realm1/not/", "1" /* should not be used */, "1", SOUP_STATUS_UNAUTHORIZED },

	{ "Make sure we've forgotten it",
	  "Basic/realm1/", "", "0", SOUP_STATUS_UNAUTHORIZED },

	{ "Likewise, reject the formerly-good Digest password",
	  "Digest/realm1/not/", "1" /* should not be used */, "1", SOUP_STATUS_UNAUTHORIZED },

	{ "Make sure we've forgotten it",
	  "Digest/realm1/", "", "0", SOUP_STATUS_UNAUTHORIZED }
};
int ntests = sizeof (tests) / sizeof (tests[0]);

static const char *auths[] = {
	"no password", "password 1",
	"password 2", "password 3",
	"intentionally wrong password",
};

static int
identify_auth (SoupMessage *msg)
{
	const char *header;
	int num;

	header = soup_message_headers_get (msg->request_headers,
					    "Authorization");
	if (!header)
		return 0;

	if (!g_ascii_strncasecmp (header, "Basic ", 6)) {
		char *token;
		gsize len;

		token = (char *)g_base64_decode (header + 6, &len);
		num = token[len - 1] - '0';
		g_free (token);
	} else {
		const char *user;

		user = strstr (header, "username=\"user");
		if (user)
			num = user[14] - '0';
		else
			num = 0;
	}

	g_assert (num >= 0 && num <= 4);

	return num;
}

static void
handler (SoupMessage *msg, gpointer data)
{
	char *expected = data;
	int auth, exp;

	auth = identify_auth (msg);

	debug_printf (1, "  %d %s (using %s)\n",
		      msg->status_code, msg->reason_phrase,
		      auths[auth]);

	if (*expected) {
		exp = *expected - '0';
		if (auth != exp) {
			debug_printf (1, "    expected %s!\n", auths[exp]);
			errors++;
		}
		memmove (expected, expected + 1, strlen (expected));
	} else {
		debug_printf (1, "    expected to be finished\n");
		errors++;
	}
}

static void
authenticate (SoupSession *session, SoupMessage *msg,
	      SoupAuth *auth, gboolean retrying, gpointer data)
{
	int *i = data;
	char *username, *password;
	char num;

	if (!tests[*i].provided[0])
		return;
	if (retrying) {
		if (!tests[*i].provided[1])
			return;
		num = tests[*i].provided[1];
	} else
		num = tests[*i].provided[0];

	username = g_strdup_printf ("user%c", num);
	password = g_strdup_printf ("realm%c", num);
	soup_auth_authenticate (auth, username, password);
	g_free (username);
	g_free (password);
}

static void
bug271540_sent (SoupMessage *msg, gpointer data)
{
	int n = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (msg), "#"));
	gboolean *authenticated = data;
	int auth = identify_auth (msg);

	if (!*authenticated && auth) {
		debug_printf (1, "    using auth on message %d before authenticating!!??\n", n);
		errors++;
	} else if (*authenticated && !auth) {
		debug_printf (1, "    sent unauthenticated message %d after authenticating!\n", n);
		errors++;
	}
}

static void
bug271540_authenticate (SoupSession *session, SoupMessage *msg,
			SoupAuth *auth, gboolean retrying, gpointer data)
{
	int n = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (msg), "#"));
	gboolean *authenticated = data;

	if (strcmp (soup_auth_get_scheme_name (auth), "Basic") != 0 ||
	    strcmp (soup_auth_get_realm (auth), "realm1") != 0)
		return;

	if (!*authenticated) {
		debug_printf (1, "    authenticating message %d\n", n);
		soup_auth_authenticate (auth, "user1", "realm1");
		*authenticated = TRUE;
	} else {
		debug_printf (1, "    asked to authenticate message %d after authenticating!\n", n);
		errors++;
	}
}

static void
bug271540_finished (SoupSession *session, SoupMessage *msg, gpointer data)
{
	int *left = data;
	int n = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (msg), "#"));

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "      got status '%d %s' on message %d!\n",
			      msg->status_code, msg->reason_phrase, n);
		errors++;
	}

	(*left)--;
	if (!*left)
		g_main_loop_quit (loop);
}

static void
digest_nonce_authenticate (SoupSession *session, SoupMessage *msg,
			   SoupAuth *auth, gboolean retrying, gpointer data)
{
	if (retrying)
		return;

	if (strcmp (soup_auth_get_scheme_name (auth), "Digest") != 0 ||
	    strcmp (soup_auth_get_realm (auth), "realm1") != 0)
		return;

	soup_auth_authenticate (auth, "user1", "realm1");
}

static void
digest_nonce_unauthorized (SoupMessage *msg, gpointer data)
{
	gboolean *got_401 = data;
	*got_401 = TRUE;
}

static void
do_digest_nonce_test (SoupSession *session,
		      const char *nth, const char *uri,
		      gboolean expect_401, gboolean expect_signal)
{
	SoupMessage *msg;
	gboolean got_401;

	msg = soup_message_new (SOUP_METHOD_GET, uri);
	if (expect_signal) {
		g_signal_connect (session, "authenticate",
				  G_CALLBACK (digest_nonce_authenticate),
				  NULL);
	}
	soup_message_add_status_code_handler (msg, "got_headers",
					      SOUP_STATUS_UNAUTHORIZED,
					      G_CALLBACK (digest_nonce_unauthorized),
					      &got_401);
	got_401 = FALSE;
	soup_session_send_message (session, msg);
	if (got_401 != expect_401) {
		debug_printf (1, "  %s request %s a 401 Unauthorized!\n", nth,
			      got_401 ? "got" : "did not get");
		errors++;
	}
	if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "  %s request got status %d %s!\n", nth,
			      msg->status_code, msg->reason_phrase);
		errors++;
	}
	if (errors == 0)
		debug_printf (1, "  %s request succeeded\n", nth);
	g_object_unref (msg);
}

/* Async auth test. We queue three requests to /Basic/realm1, ensuring
 * that they are sent in order. The first and third ones will be
 * paused from the authentication callback. The second will be allowed
 * to fail. Shortly after the third one requests auth, we'll provide
 * the auth and unpause the two remaining messages, allowing them to
 * succeed.
 */

static void
async_authenticate (SoupSession *session, SoupMessage *msg,
		    SoupAuth *auth, gboolean retrying, gpointer data)
{
	SoupAuth **saved_auth = data;
	int id = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (msg), "id"));

	debug_printf (2, "  async_authenticate msg%d\n", id);

	/* The session will try to authenticate msg3 *before* sending
	 * it, because it already knows it's going to need the auth.
	 * Ignore that.
	 */
	if (msg->status_code != SOUP_STATUS_UNAUTHORIZED) {
		debug_printf (2, "    (ignoring)\n");
		return;
	}

	soup_session_pause_message (session, msg);
	if (saved_auth)
		*saved_auth = g_object_ref (auth);
	g_main_loop_quit (loop);
}

static void
async_finished (SoupSession *session, SoupMessage *msg, gpointer user_data)
{
	int *finished = user_data;
	int id = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (msg), "id"));

	debug_printf (2, "  async_finished msg%d\n", id);

	(*finished)++;
	if (*finished == 2)
		g_main_loop_quit (loop);
}

static void
do_async_auth_test (const char *base_uri)
{
	SoupSession *session;
	SoupMessage *msg1, *msg2, *msg3, msg2_bak;
	guint auth_id;
	char *uri;
	SoupAuth *auth = NULL;
	int finished = 0;

	debug_printf (1, "\nTesting async auth:\n");

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);

	uri = g_strconcat (base_uri, "Basic/realm1/", NULL);

	msg1 = soup_message_new ("GET", uri);
	g_object_set_data (G_OBJECT (msg1), "id", GINT_TO_POINTER (1));
	auth_id = g_signal_connect (session, "authenticate",
				    G_CALLBACK (async_authenticate), &auth);
	g_object_ref (msg1);
	soup_session_queue_message (session, msg1, async_finished, &finished);
	g_main_loop_run (loop);
	g_signal_handler_disconnect (session, auth_id);

	/* async_authenticate will pause msg1 and quit loop */

	msg2 = soup_message_new ("GET", uri);
	g_object_set_data (G_OBJECT (msg2), "id", GINT_TO_POINTER (2));
	soup_session_send_message (session, msg2);

	if (msg2->status_code == SOUP_STATUS_UNAUTHORIZED)
		debug_printf (1, "  msg2 failed as expected\n");
	else {
		debug_printf (1, "  msg2 got wrong status! (%u)\n",
			      msg2->status_code);
		errors++;
	}

	/* msg2 should be done at this point; assuming everything is
	 * working correctly, the session won't look at it again; we
	 * ensure that if it does, it will crash the test program.
	 */
	memcpy (&msg2_bak, msg2, sizeof (SoupMessage));
	memset (msg2, 0, sizeof (SoupMessage));

	msg3 = soup_message_new ("GET", uri);
	g_object_set_data (G_OBJECT (msg3), "id", GINT_TO_POINTER (3));
	auth_id = g_signal_connect (session, "authenticate",
				    G_CALLBACK (async_authenticate), NULL);
	g_object_ref (msg3);
	soup_session_queue_message (session, msg3, async_finished, &finished);
	g_main_loop_run (loop);
	g_signal_handler_disconnect (session, auth_id);

	/* async_authenticate will pause msg3 and quit loop */

	/* Now do the auth, and restart */
	if (auth) {
		soup_auth_authenticate (auth, "user1", "realm1");
		soup_session_unpause_message (session, msg1);
		soup_session_unpause_message (session, msg3);

		g_main_loop_run (loop);

		/* async_finished will quit the loop */
	} else {
		debug_printf (1, "  msg1 didn't get authenticate signal!\n");
		errors++;
	}

	if (msg1->status_code == SOUP_STATUS_OK)
		debug_printf (1, "  msg1 succeeded\n");
	else {
		debug_printf (1, "  msg1 FAILED! (%u %s)\n",
			      msg1->status_code, msg1->reason_phrase);
		errors++;
	}
	if (msg3->status_code == SOUP_STATUS_OK)
		debug_printf (1, "  msg3 succeeded\n");
	else {
		debug_printf (1, "  msg3 FAILED! (%u %s)\n",
			      msg3->status_code, msg3->reason_phrase);
		errors++;
	}

	soup_session_abort (session);
	g_object_unref (session);

	g_object_unref (msg1);
	g_object_unref (msg3);
	memcpy (msg2, &msg2_bak, sizeof (SoupMessage));
	g_object_unref (msg2);
	g_object_unref (auth);
	g_free (uri);
}

int
main (int argc, char **argv)
{
	SoupSession *session;
	SoupMessage *msg;
	char *base_uri, *uri, *expected;
	gboolean authenticated;
	int i;

	test_init (argc, argv, NULL);
	apache_init ();

	base_uri = "http://localhost:47524/";

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	g_signal_connect (session, "authenticate",
			  G_CALLBACK (authenticate), &i);

	for (i = 0; i < ntests; i++) {
		debug_printf (1, "Test %d: %s\n", i + 1, tests[i].explanation);

		uri = g_strconcat (base_uri, tests[i].url, NULL);
		debug_printf (1, "  GET %s\n", uri);

		msg = soup_message_new (SOUP_METHOD_GET, uri);
		g_free (uri);
		if (!msg) {
			fprintf (stderr, "auth-test: Could not parse URI\n");
			exit (1);
		}

		expected = g_strdup (tests[i].expected);
		soup_message_add_status_code_handler (
			msg, "got_headers", SOUP_STATUS_UNAUTHORIZED,
			G_CALLBACK (handler), expected);
		soup_message_add_status_code_handler (
			msg, "got_headers", SOUP_STATUS_OK,
			G_CALLBACK (handler), expected);
		soup_session_send_message (session, msg);
		if (msg->status_code != SOUP_STATUS_UNAUTHORIZED &&
		    msg->status_code != SOUP_STATUS_OK) {
			debug_printf (1, "  %d %s !\n", msg->status_code,
				      msg->reason_phrase);
			errors++;
		}
		if (*expected) {
			debug_printf (1, "  expected %d more round(s)\n",
				      (int)strlen (expected));
			errors++;
		}
		g_free (expected);

		if (msg->status_code != tests[i].final_status) {
			debug_printf (1, "  expected %d\n",
				      tests[i].final_status);
		}

		debug_printf (1, "\n");

		g_object_unref (msg);
	}
	soup_session_abort (session);
	g_object_unref (session);

	/* And now for some regression tests */
	loop = g_main_loop_new (NULL, TRUE);

	debug_printf (1, "Testing pipelined auth (bug 271540):\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);

	authenticated = FALSE;
	g_signal_connect (session, "authenticate",
			  G_CALLBACK (bug271540_authenticate), &authenticated);

	uri = g_strconcat (base_uri, "Basic/realm1/", NULL);
	for (i = 0; i < 10; i++) {
		msg = soup_message_new (SOUP_METHOD_GET, uri);
		g_object_set_data (G_OBJECT (msg), "#", GINT_TO_POINTER (i + 1));
		g_signal_connect (msg, "wrote_headers",
				  G_CALLBACK (bug271540_sent), &authenticated);

		soup_session_queue_message (session, msg,
					    bug271540_finished, &i);
	}
	g_free (uri);

	g_main_loop_run (loop);
	soup_session_abort (session);
	g_object_unref (session);

	debug_printf (1, "\nTesting digest nonce expiration:\n");

	/* We test two different things here:
	 *
	 *   1. If we get a 401 response with
	 *      "WWW-Authenticate: Digest stale=true...", we should
	 *      retry and succeed *without* the session asking for a
	 *      password again.
	 *
	 *   2. If we get a successful response with
	 *      "Authentication-Info: nextnonce=...", we should update
	 *      the nonce automatically so as to avoid getting a
	 *      stale nonce error on the next request.
	 *
	 * In our Apache config, /Digest/realm1 and
	 * /Digest/realm1/expire are set up to use the same auth info,
	 * but only the latter has an AuthDigestNonceLifetime (of 2
	 * seconds). The way nonces work in Apache, a nonce received
	 * from /Digest/realm1 will still expire in
	 * /Digest/realm1/expire, but it won't issue a nextnonce for a
	 * request in /Digest/realm1. This lets us test both
	 * behaviors.
	 *
	 * The expected conversation is:
	 *
	 * First message
	 *   GET /Digest/realm1
	 *
	 *   401 Unauthorized
	 *   WWW-Authenticate: Digest nonce=A
	 *
	 *   [emit 'authenticate']
	 *
	 *   GET /Digest/realm1
	 *   Authorization: Digest nonce=A
	 *
	 *   200 OK
	 *   [No Authentication-Info]
	 *
	 * [sleep 2 seconds: nonce A is no longer valid, but we have no
	 * way of knowing that]
	 *
	 * Second message
	 *   GET /Digest/realm1/expire/
	 *   Authorization: Digest nonce=A
	 *
	 *   401 Unauthorized
	 *   WWW-Authenticate: Digest stale=true nonce=B
	 *
	 *   GET /Digest/realm1/expire/
	 *   Authorization: Digest nonce=B
	 *
	 *   200 OK
	 *   Authentication-Info: nextnonce=C
	 *
	 * [sleep 1 second]
	 *
	 * Third message
	 *   GET /Digest/realm1/expire/
	 *   Authorization: Digest nonce=C
	 *   [nonce=B would work here too]
	 *
	 *   200 OK
	 *   Authentication-Info: nextnonce=D
	 *
	 * [sleep 1 second; nonces B and C are no longer valid, but D is]
	 *
	 * Fourth message
	 *   GET /Digest/realm1/expire/
	 *   Authorization: Digest nonce=D
	 *
	 *   200 OK
	 *   Authentication-Info: nextnonce=D
	 *
	 */

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);

	uri = g_strconcat (base_uri, "Digest/realm1/", NULL);
	do_digest_nonce_test (session, "First", uri, TRUE, TRUE);
	g_free (uri);
	sleep (2);
	uri = g_strconcat (base_uri, "Digest/realm1/expire/", NULL);
	do_digest_nonce_test (session, "Second", uri, TRUE, FALSE);
	sleep (1);
	do_digest_nonce_test (session, "Third", uri, FALSE, FALSE);
	sleep (1);
	do_digest_nonce_test (session, "Fourth", uri, FALSE, FALSE);
	g_free (uri);

	soup_session_abort (session);
	g_object_unref (session);

	/* Async auth */
	do_async_auth_test (base_uri);

	g_main_loop_unref (loop);

	test_cleanup ();
	return errors != 0;
}
