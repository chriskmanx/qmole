#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "libsoup/soup.h"
#include "libsoup/soup-session.h"

#include "test-utils.h"

SoupBuffer *correct_response;

static void
authenticate (SoupSession *session, SoupMessage *msg,
	      SoupAuth *auth, gboolean retrying, gpointer data)
{
	if (!retrying)
		soup_auth_authenticate (auth, "user2", "realm2");
}

static void
get_correct_response (const char *uri)
{
	SoupSession *session;
	SoupMessage *msg;

	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	msg = soup_message_new (SOUP_METHOD_GET, uri);
	soup_session_send_message (session, msg);
	if (msg->status_code != SOUP_STATUS_OK) {
		fprintf (stderr, "Could not fetch %s: %d %s\n", uri,
			 msg->status_code, msg->reason_phrase);
		exit (1);
	}

	correct_response = soup_message_body_flatten (msg->response_body);

	g_object_unref (msg);
	soup_session_abort (session);
	g_object_unref (session);
}

/* Pull API version 1: fully-async. More like a "poke" API. Rather
 * than having SoupMessage emit "got_chunk" signals whenever it wants,
 * we stop it after it finishes reading the message headers, and then
 * tell it when we want to hear about new chunks.
 */

typedef struct {
	GMainLoop *loop;
	SoupSession *session;
	SoupMessage *msg;
	guint timeout;
	gboolean chunks_ready;
	gboolean chunk_wanted;
	gboolean did_first_timeout;
	gsize read_so_far;
	guint expected_status;
} FullyAsyncData;

static void fully_async_got_headers (SoupMessage *msg, gpointer user_data);
static void fully_async_got_chunk   (SoupMessage *msg, SoupBuffer *chunk,
				     gpointer user_data);
static void fully_async_finished    (SoupSession *session, SoupMessage *msg,
				     gpointer user_data);
static gboolean fully_async_request_chunk (gpointer user_data);

static void
do_fully_async_test (SoupSession *session,
		     const char *base_uri, const char *sub_uri,
		     gboolean fast_request, guint expected_status)
{
	GMainLoop *loop;
	FullyAsyncData ad;
	SoupMessage *msg;
	char *uri;

	loop = g_main_loop_new (NULL, FALSE);

	uri = g_build_filename (base_uri, sub_uri, NULL);
	debug_printf (1, "GET %s\n", uri);

	msg = soup_message_new (SOUP_METHOD_GET, uri);
	g_free (uri);

	ad.loop = loop;
	ad.session = session;
	ad.msg = msg;
	ad.chunks_ready = FALSE;
	ad.chunk_wanted = FALSE;
	ad.did_first_timeout = FALSE;
	ad.read_so_far = 0;
	ad.expected_status = expected_status;

	/* Since we aren't going to look at the final value of
	 * msg->response, we set OVERWRITE_CHUNKS, to tell libsoup to
	 * not even bother generating it.
	 */
	soup_message_set_flags (msg, SOUP_MESSAGE_OVERWRITE_CHUNKS);

	/* Connect to "got_headers", from which we'll decide where to
	 * go next.
	 */
	g_signal_connect (msg, "got_headers",
			  G_CALLBACK (fully_async_got_headers), &ad);

	/* Queue the request */
	soup_session_queue_message (session, msg, fully_async_finished, &ad);

	/* In a real program, we'd probably just return at this point.
	 * Eventually the caller would return all the way to the main
	 * loop, and then eventually, some event would cause the
	 * application to request a chunk of data from the message
	 * response.
	 *
	 * In our test program, there is no "real" main loop, so we
	 * had to create our own. We use a timeout to represent the
	 * event that causes the app to decide to request another body
	 * chunk. We use short timeouts in one set of tests, and long
	 * ones in another, to test both the
	 * chunk-requested-before-its-been-read and
	 * chunk-read-before-its-been-requested cases.
	 */
	ad.timeout = g_timeout_add (fast_request ? 0 : 100,
				    fully_async_request_chunk, &ad);
	g_main_loop_run (ad.loop);
	g_main_loop_unref (ad.loop);
}

static gboolean
fully_async_request_chunk (gpointer user_data)
{
	FullyAsyncData *ad = user_data;

	if (!ad->did_first_timeout) {
		debug_printf (1, "  first timeout\n");
		ad->did_first_timeout = TRUE;
	} else
		debug_printf (2, "  timeout\n");
	ad->timeout = 0;

	/* ad->chunks_ready and ad->chunk_wanted are used because
	 * there's a race condition between the application requesting
	 * the first chunk, and the message reaching a point where
	 * it's actually ready to read chunks. If chunks_ready has
	 * been set, we can just call soup_session_unpause_message() to
	 * cause the first chunk to be read. But if it's not, we just
	 * set chunk_wanted, to let the got_headers handler below know
	 * that a chunk has already been requested.
	 */
	if (ad->chunks_ready)
		soup_session_unpause_message (ad->session, ad->msg);
	else
		ad->chunk_wanted = TRUE;

	return FALSE;
}

static void
fully_async_got_headers (SoupMessage *msg, gpointer user_data)
{
	FullyAsyncData *ad = user_data;

	debug_printf (1, "  %d %s\n", msg->status_code, msg->reason_phrase);
	if (msg->status_code == SOUP_STATUS_UNAUTHORIZED) {
		/* Let soup handle this one; this got_headers handler
		 * will get called again next time around.
		 */
		return;
	} else if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "  unexpected status: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
		return;
	}

	/* OK, we're happy with the response. So, we connect to
	 * "got_chunk". If there has already been a chunk requested,
	 * we let I/O continue; but if there hasn't, we pause now
	 * until one is requested.
	 */
	ad->chunks_ready = TRUE;
	g_signal_connect (msg, "got_chunk",
			  G_CALLBACK (fully_async_got_chunk), ad);
	if (!ad->chunk_wanted)
		soup_session_pause_message (ad->session, msg);
}

static void
fully_async_got_chunk (SoupMessage *msg, SoupBuffer *chunk, gpointer user_data)
{
	FullyAsyncData *ad = user_data;

	debug_printf (2, "  got chunk from %lu - %lu\n",
		      (unsigned long) ad->read_so_far,
		      (unsigned long) ad->read_so_far + chunk->length);

	/* We've got a chunk, let's process it. In the case of the
	 * test program, that means comparing it against
	 * correct_response to make sure that we got the right data.
	 */
	if (ad->read_so_far + chunk->length > correct_response->length) {
		debug_printf (1, "  read too far! (%lu > %lu)\n",
			      (unsigned long) (ad->read_so_far + chunk->length),
			      (unsigned long) correct_response->length);
		errors++;
	} else if (memcmp (chunk->data,
			   correct_response->data + ad->read_so_far,
			   chunk->length) != 0) {
		debug_printf (1, "  data mismatch in block starting at %lu\n",
			      (unsigned long) ad->read_so_far);
		errors++;
	}
	ad->read_so_far += chunk->length;

	/* Now pause I/O, and prepare to read another chunk later.
	 * (Again, the timeout just abstractly represents the idea of
	 * the application requesting another chunk at some random
	 * point in the future. You wouldn't be using a timeout in a
	 * real program.)
	 */
	soup_session_pause_message (ad->session, msg);
	ad->chunk_wanted = FALSE;

	ad->timeout = g_timeout_add (10, fully_async_request_chunk, ad);
}

static void
fully_async_finished (SoupSession *session, SoupMessage *msg,
		      gpointer user_data)
{
	FullyAsyncData *ad = user_data;

	if (msg->status_code != ad->expected_status) {
		debug_printf (1, "  unexpected final status: %d %s !\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}

	if (ad->timeout != 0)
		g_source_remove (ad->timeout);

	/* Since our test program is only running the loop for the
	 * purpose of this one test, we quit the loop once the
	 * test is done.
	 */
	g_main_loop_quit (ad->loop);
}


/* Pull API version 2: synchronous pull API via async I/O. */

typedef struct {
	GMainLoop *loop;
	SoupSession *session;
	SoupBuffer *chunk;
} SyncAsyncData;

static void        sync_async_send       (SoupSession *session,
					  SoupMessage *msg);
static gboolean    sync_async_is_finished(SoupMessage *msg);
static SoupBuffer *sync_async_read_chunk (SoupMessage *msg);
static void        sync_async_cleanup    (SoupMessage *msg);

static void sync_async_got_headers (SoupMessage *msg, gpointer user_data);
static void sync_async_copy_chunk  (SoupMessage *msg, SoupBuffer *chunk,
				    gpointer user_data);
static void sync_async_finished    (SoupSession *session, SoupMessage *msg,
				    gpointer user_data);

static void
do_synchronously_async_test (SoupSession *session,
			     const char *base_uri, const char *sub_uri,
			     guint expected_status)
{
	SoupMessage *msg;
	char *uri;
	gsize read_so_far;
	SoupBuffer *chunk;

	uri = g_build_filename (base_uri, sub_uri, NULL);
	debug_printf (1, "GET %s\n", uri);

	msg = soup_message_new (SOUP_METHOD_GET, uri);
	g_free (uri);

	/* As in the fully-async case, we set OVERWRITE_CHUNKS as an
	 * optimization.
	 */
	soup_message_set_flags (msg, SOUP_MESSAGE_OVERWRITE_CHUNKS);

	/* Send the message, get back headers */
	sync_async_send (session, msg);
	if (sync_async_is_finished (msg) &&
	    expected_status == SOUP_STATUS_OK) {
		debug_printf (1, "  finished without reading response!\n");
		errors++;
	} else if (!sync_async_is_finished (msg) &&
		   expected_status != SOUP_STATUS_OK) {
		debug_printf (1, "  request failed to fail!\n");
		errors++;
	}

	/* Now we're ready to read the response body (though we could
	 * put that off until later if we really wanted).
	 */
	read_so_far = 0;
	while ((chunk = sync_async_read_chunk (msg))) {
		debug_printf (2, "  read chunk from %lu - %lu\n",
			      (unsigned long) read_so_far,
			      (unsigned long) read_so_far + chunk->length);

		if (read_so_far + chunk->length > correct_response->length) {
			debug_printf (1, "  read too far! (%lu > %lu)\n",
				      (unsigned long) read_so_far + chunk->length,
				      (unsigned long) correct_response->length);
			errors++;
		} else if (memcmp (chunk->data,
				   correct_response->data + read_so_far,
				   chunk->length) != 0) {
			debug_printf (1, "  data mismatch in block starting at %lu\n",
				      (unsigned long) read_so_far);
			errors++;
		}
		read_so_far += chunk->length;
		soup_buffer_free (chunk);
	}

	if (!sync_async_is_finished (msg) ||
	    (msg->status_code == SOUP_STATUS_OK &&
	     read_so_far != correct_response->length)) {
		debug_printf (1, "  loop ended before message was fully read!\n");
		errors++;
	} else if (msg->status_code != expected_status) {
		debug_printf (1, "  unexpected final status: %d %s !\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
	}

	sync_async_cleanup (msg);
	g_object_unref (msg);
}

/* Sends @msg on async session @session and returns after the headers
 * of a successful response (or the complete body of a failed
 * response) have been read.
 */
static void
sync_async_send (SoupSession *session, SoupMessage *msg)
{
	SyncAsyncData *ad;

	ad = g_new0 (SyncAsyncData, 1);
	g_object_set_data (G_OBJECT (msg), "SyncAsyncData", ad);

	/* In this case, unlike the fully-async case, the loop
	 * actually belongs to us, not the application; it will only
	 * be run when we're waiting for chunks, not at other times.
	 *
	 * If session has an async_context associated with it, we'd
	 * want to pass that, rather than NULL, here.
	 */
	ad->loop = g_main_loop_new (NULL, FALSE);
	ad->session = session;

	g_signal_connect (msg, "got_headers",
			  G_CALLBACK (sync_async_got_headers), ad);

	/* Start the request by queuing it and then running our main
	 * loop. Note: we have to use soup_session_queue_message()
	 * here; soup_session_send_message() won't work, for several
	 * reasons. Also, since soup_session_queue_message() steals a
	 * ref to the message and then unrefs it after invoking the
	 * callback, we have to add an extra ref before calling it.
	 */
	g_object_ref (msg);
	soup_session_queue_message (session, msg, sync_async_finished, ad);
	g_main_loop_run (ad->loop);

	/* At this point, one of two things has happened; either the
	 * got_headers handler got headers it liked, and so stopped
	 * the loop, or else the message was fully processed without
	 * the got_headers handler interrupting it, and so the final
	 * callback (sync_async_finished) was invoked, and stopped the
	 * loop.
	 *
	 * Either way, we're done, so we return to the caller.
	 */
}

static void
sync_async_got_headers (SoupMessage *msg, gpointer user_data)
{
	SyncAsyncData *ad = user_data;

	debug_printf (1, "  %d %s\n", msg->status_code, msg->reason_phrase);
	if (msg->status_code == SOUP_STATUS_UNAUTHORIZED) {
		/* Let soup handle this one; this got_headers handler
		 * will get called again next time around.
		 */
		return;
	} else if (msg->status_code != SOUP_STATUS_OK) {
		debug_printf (1, "  unexpected status: %d %s\n",
			      msg->status_code, msg->reason_phrase);
		errors++;
		return;
	}

	/* Stop I/O and return to the caller */
	soup_session_pause_message (ad->session, msg);
	g_main_loop_quit (ad->loop);
}

static gboolean
sync_async_is_finished (SoupMessage *msg)
{
	SyncAsyncData *ad = g_object_get_data (G_OBJECT (msg), "SyncAsyncData");

	/* sync_async_finished clears ad->loop */
	return ad->loop == NULL;
}

/* Tries to read a chunk. Returns %NULL on error/end-of-response. */
static SoupBuffer *
sync_async_read_chunk (SoupMessage *msg)
{
	SyncAsyncData *ad = g_object_get_data (G_OBJECT (msg), "SyncAsyncData");
	guint handler;

	if (sync_async_is_finished (msg))
		return NULL;

	ad->chunk = NULL;
	handler = g_signal_connect (msg, "got_chunk",
				    G_CALLBACK (sync_async_copy_chunk),
				    ad);
	soup_session_unpause_message (ad->session, msg);
	g_main_loop_run (ad->loop);
	g_signal_handler_disconnect (msg, handler);

	return ad->chunk;
}

static void
sync_async_copy_chunk (SoupMessage *msg, SoupBuffer *chunk, gpointer user_data)
{
	SyncAsyncData *ad = user_data;

	ad->chunk = soup_buffer_copy (chunk);

	/* Now pause and return from the g_main_loop_run() call in
	 * sync_async_read_chunk().
	 */
	soup_session_pause_message (ad->session, msg);
	g_main_loop_quit (ad->loop);
}

static void
sync_async_finished (SoupSession *session, SoupMessage *msg, gpointer user_data)
{
	SyncAsyncData *ad = user_data;

	/* Unlike in the fully_async_case, we don't need to do much
	 * here, because control will return to
	 * do_synchronously_async_test() when we're done, and we do
	 * the final tests there.
	 */
	g_main_loop_quit (ad->loop);
	g_main_loop_unref (ad->loop);
	ad->loop = NULL;
}

static void
sync_async_cleanup (SoupMessage *msg)
{
	SyncAsyncData *ad = g_object_get_data (G_OBJECT (msg), "SyncAsyncData");

	if (ad->loop)
		g_main_loop_unref (ad->loop);
	g_free (ad);
}


int
main (int argc, char **argv)
{
	SoupSession *session;
	char *base_uri;

	test_init (argc, argv, NULL);
	apache_init ();

	base_uri = "http://localhost:47524/";
	get_correct_response (base_uri);

	debug_printf (1, "\nFully async, fast requests\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	g_signal_connect (session, "authenticate",
			  G_CALLBACK (authenticate), NULL);
	do_fully_async_test (session, base_uri, "/",
			     TRUE, SOUP_STATUS_OK);
	do_fully_async_test (session, base_uri, "/Basic/realm1/",
			     TRUE, SOUP_STATUS_UNAUTHORIZED);
	do_fully_async_test (session, base_uri, "/Basic/realm2/",
			     TRUE, SOUP_STATUS_OK);
	soup_session_abort (session);
	g_object_unref (session);

	debug_printf (1, "\nFully async, slow requests\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	g_signal_connect (session, "authenticate",
			  G_CALLBACK (authenticate), NULL);
	do_fully_async_test (session, base_uri, "/",
			     FALSE, SOUP_STATUS_OK);
	do_fully_async_test (session, base_uri, "/Basic/realm1/",
			     FALSE, SOUP_STATUS_UNAUTHORIZED);
	do_fully_async_test (session, base_uri, "/Basic/realm2/",
			     FALSE, SOUP_STATUS_OK);
	soup_session_abort (session);
	g_object_unref (session);

	debug_printf (1, "\nSynchronously async\n");
	session = soup_test_session_new (SOUP_TYPE_SESSION_ASYNC, NULL);
	g_signal_connect (session, "authenticate",
			  G_CALLBACK (authenticate), NULL);
	do_synchronously_async_test (session, base_uri, "/",
				     SOUP_STATUS_OK);
	do_synchronously_async_test (session, base_uri, "/Basic/realm1/",
				     SOUP_STATUS_UNAUTHORIZED);
	do_synchronously_async_test (session, base_uri, "/Basic/realm2/",
				     SOUP_STATUS_OK);

	soup_session_abort (session);
	g_object_unref (session);

	soup_buffer_free (correct_response);

	test_cleanup ();
	return errors != 0;
}
