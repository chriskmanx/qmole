#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <orbit/orbit.h>
#include <orbit/poa/orbit-adaptor.h>
#include "GIOP/giop-debug.h"

#include "test-giop-frag.h"

#ifndef G_ENABLE_DEBUG
#ifdef __GNUC__
#  warning GIOP test hooks only enabled in a debugging build
#endif
int
main (int argc, char *argv[])
{
	g_warning ("GIOP test hooks only enabled in a debugging build");
	return 0;
}
#else

static LinkWriteOpts  *non_blocking = NULL;
static GIOPServer     *server     = NULL;
static GIOPConnection *server_cnx = NULL;
static GIOPConnection *cnx        = NULL;

static gboolean fragment_done;

static void
wait_for_disconnect (void)
{
	int i;

	/* a main_pending just looks for IO and not HUPs */
	for (i = 0; i < 10; i++) {
		if (link_main_pending ())
			i = 0;
		link_main_iteration (FALSE);
	}
}

static void
hook_unexpected_frag_reply (GIOPRecvBuffer *buf)
{
	char *p;
	const char testa[] = "ADVENTURE";  /* cf. Willard Price */
	const char testb[] = "MILLENNIUM"; /* cf. Robbie Williams */
	const char testc[] = "It isn't,  said the Caterpillar";
	const char testd[] = "Why?  said the Caterpillar";

	fragment_done = TRUE;

	g_assert (buf != NULL);
	g_assert (buf->left_to_read == 0);
	g_assert (buf->msg.header.message_size == 1727);

	p = buf->message_body + 52;
	g_assert (!strncmp (p, testa, sizeof (testa) - 1));

	p = buf->message_body + 97;
	g_assert (!strncmp (p, testb, sizeof (testb) - 1));

	p = buf->message_body + 1002;
	g_assert (!strncmp (p, testc, sizeof (testc) - 1));

	p = buf->message_body + 1702;
	g_assert (!strncmp (p, testd, sizeof (testd) - 1));
}

static void
test_fragments (void)
{
	link_connection_write (
		LINK_CONNECTION (cnx),
		giop_fragment_data,
		sizeof (giop_fragment_data),
		non_blocking);

	giop_debug_hook_unexpected_reply = hook_unexpected_frag_reply;

	fragment_done = FALSE;
	while (!fragment_done)
		link_main_iteration (FALSE);

	giop_debug_hook_unexpected_reply = NULL;
}

static gboolean spoof_done;
static gboolean spoof_succeeded;

static void
test_spoof_callback (GIOPMessageQueueEntry *ent)
{
	spoof_done = spoof_succeeded = TRUE;
}

static void
test_spoof_hook (GIOPRecvBuffer *buffer,
		 GIOPMessageQueueEntry *ent)
{
	spoof_done = TRUE;
}

static void
test_spoofing (void)
{
	int i;
	GIOPConnection *misc;
	GIOPSendBuffer *reply;
	GIOPMessageQueueEntry ent;
	CORBA_unsigned_long request_id;

	request_id = 0x12345;
	giop_debug_hook_spoofed_reply = test_spoof_hook;
	misc = g_object_new (giop_connection_get_type (), NULL);

	for (i = 0; i < 2; i++) {
		giop_recv_list_setup_queue_entry (&ent, !i ? server_cnx : misc,
						  GIOP_REPLY, request_id);
		giop_recv_list_setup_queue_entry_async (&ent, test_spoof_callback);

		reply = giop_send_buffer_use_reply (
			GIOP_1_2, request_id , CORBA_NO_EXCEPTION);

		spoof_done = FALSE;
		spoof_succeeded = FALSE;
		
		g_assert (!giop_send_buffer_write (reply, cnx, TRUE));
	
		giop_send_buffer_unuse (reply);

		while (!spoof_done)
			link_main_iteration (TRUE);

		switch (i) {
		case 0: /* valid */
			g_assert (spoof_succeeded);
			break;
		case 1: /* invalid */
			g_assert (!spoof_succeeded);
			link_connection_ref (cnx);
			wait_for_disconnect ();
			g_assert (LINK_CONNECTION (cnx)->status == LINK_DISCONNECTED);
			link_connection_unref (cnx);
			break;
		default:
			g_assert_not_reached ();
			break;
		}
	}
	link_connection_unref (misc);
	giop_debug_hook_spoofed_reply = NULL;
}

static void
run_test_hook_new_connection (GIOPServer     *server,
			      GIOPConnection *new_cnx)
{
	g_assert (g_type_is_a (G_TYPE_FROM_INSTANCE (server),
			       GIOP_TYPE_SERVER));
	g_assert (g_type_is_a (G_TYPE_FROM_INSTANCE (new_cnx),
			       GIOP_TYPE_CONNECTION));

	server_cnx = new_cnx;
}

static void
run_test (CORBA_ORB orb, void (*do_test) (void), gboolean reverse)
{
#ifndef G_OS_WIN32
	server = giop_server_new (GIOP_1_2, "UNIX", NULL, NULL, 0, orb);
#else
	server = giop_server_new (GIOP_1_2, "IPv4", NULL, NULL, 0, orb);
#endif
	server_cnx = NULL;
	g_assert (LINK_IS_SERVER (server));

	giop_debug_hook_new_connection = run_test_hook_new_connection;

#ifndef G_OS_WIN32
	cnx = giop_connection_initiate (
		orb, "UNIX",
		LINK_SERVER (server)->local_host_info,
		LINK_SERVER (server)->local_serv_info,
		LINK_CONNECTION_NONBLOCKING,
		GIOP_1_2);
#else
	cnx = giop_connection_initiate (
		orb, "IPv4",
		LINK_SERVER (server)->local_host_info,
		LINK_SERVER (server)->local_serv_info,
		LINK_CONNECTION_NONBLOCKING,
		GIOP_1_2);
#endif
	g_assert (cnx != NULL);

	while (server_cnx == NULL)
		link_main_iteration (TRUE);

	giop_debug_hook_new_connection = NULL;
	g_assert (server_cnx != NULL);

	if (reverse) {
		gpointer tmp = server_cnx;
		server_cnx = cnx;
		cnx = tmp;
	}

	do_test ();

	if (reverse) {
		gpointer tmp = server_cnx;
		server_cnx = cnx;
		cnx = tmp;
	}

	g_object_unref (G_OBJECT (server));
	server_cnx = NULL;
	server = NULL;
	link_connection_unref (cnx);
	cnx = NULL;
}

static void
test_cookie (CORBA_ORB orb)
{
	int i;
	ORBit_ObjectAdaptor adaptor;
	CORBA_sequence_CORBA_octet *seq;

	adaptor = g_ptr_array_index (orb->adaptors, 0);
	g_assert (adaptor != NULL);

	seq = &adaptor->adaptor_key;
	
	g_assert (seq->_length > 8);

	fprintf (stderr, "ORB cookie (%ld): ",
		 (long) seq->_length);

	for (i = 0; i < seq->_length; i++)
		fprintf (stderr, "%.2x", seq->_buffer [i]);

	fprintf (stderr, " - looks random ?\n");
}

#define MANGLE_ITERATIONS 1000

/* fraction denominators */
#define MANGLE_HEADER 8
#define MANGLE_BODY   64

static int bits_corrupted = 0;
static int cnx_closed = 0;

static void
test_incoming_mangler (GIOPRecvBuffer *buf)
{
	int r;
	guchar *start, *p;
	CORBA_long len;

	switch (buf->state) {
	case GIOP_MSG_READING_HEADER:
		start = (guchar *) &buf->msg.header;
		r = MANGLE_HEADER;
		break;
	case GIOP_MSG_READING_BODY:
		start = (guchar *) buf->message_body + 12;
		r = MANGLE_BODY;
		break;
	default:
		start = NULL;
		r = 0;
		g_error ("Odd msg status");
		break;
	}

	len = buf->end - start;

	for (p = start; p < buf->end; p++) {
		int i = rand ();

		if ((i * 1.0 * r) / (RAND_MAX + 1.0) <= 1.0) {
			int bit = 1 << ((i >> 8) & 0x7);
			p--; /* can do the same again */
			*p ^= bit;
			bits_corrupted++;
		}
	}
}

static void
test_mangling_exec (void)
{
	link_connection_write (
		LINK_CONNECTION (cnx),
		giop_fragment_data,
		sizeof (giop_fragment_data),
		non_blocking);

	wait_for_disconnect (); /* Wait around for things to blow up */

	if (cnx->parent.status == LINK_DISCONNECTED)
		cnx_closed++;
}

static void
test_mangling (CORBA_ORB orb)
{
	int i;

	giop_debug_hook_incoming_mangler = test_incoming_mangler;

	fprintf (stderr, "Testing data corruption ...\n");
	for (i = 0; i < MANGLE_ITERATIONS; i++) {
		run_test (orb, test_mangling_exec, i % 1);
	}

	fprintf (stderr, " %d bits corrupted, %d cnx terminated\n",
		 bits_corrupted, cnx_closed);

	giop_debug_hook_incoming_mangler = NULL;
}

int
main (int argc, char *argv[])
{
	CORBA_ORB orb;
	CORBA_Environment ev;

	g_thread_init (NULL);

	CORBA_exception_init (&ev);

	orb = CORBA_ORB_init (&argc, argv, "orbit-local-orb", &ev);
	g_assert (ev._major == CORBA_NO_EXCEPTION);
	non_blocking = link_write_options_new (FALSE);

	fprintf (stderr, "Testing fragment support ...\n");
	run_test (orb, test_fragments, FALSE);
	run_test (orb, test_fragments, TRUE);

	fprintf (stderr, "Testing spoofing ...\n");
	run_test (orb, test_spoofing, FALSE);
	run_test (orb, test_spoofing, TRUE);

	test_cookie (orb);
	test_mangling (orb);

	link_write_options_free (non_blocking);
	CORBA_ORB_destroy (orb, &ev);
	g_assert (ev._major == CORBA_NO_EXCEPTION);
	CORBA_Object_release ((CORBA_Object) orb, &ev);
	g_assert (ev._major == CORBA_NO_EXCEPTION);

	fprintf (stderr, "All tests passed.\n");

	return 0;
}

#endif /* G_ENABLE_DEBUG */
