#include <stdio.h>
#include <stdlib.h>

#include "echo.h"

static int niters = 10000;
static int nthreads = 8;
static char *server_ior;
static CORBA_ORB orb;

static gpointer
echo_client_thread (gpointer data)
{
	int i;
	Echo echo_client;
	CORBA_Environment *ev, real_ev;

	CORBA_exception_init ((ev = &real_ev));

	echo_client = CORBA_ORB_string_to_object (orb, server_ior, ev);
	if (!echo_client) {
		g_error ("[%p]: Cannot bind to %s\n",
			 g_thread_self (), server_ior);
		return NULL;
	}
	
	for (i = 0; i < 4; i++) /* let others get started */
		g_thread_yield ();

	for (i = 0; i < niters; i++) {
		char *str;
		CORBA_double tmp;
		Echo retval;
		str = g_strdup_printf ("[%p]: Hello, world [%d]",
				       g_thread_self (), i);

		Echo_doOneWay (echo_client, str, ev);
		
		retval = Echo_echoString (echo_client, str, &tmp, ev);

		g_free (str);

		if (ev->_major != CORBA_NO_EXCEPTION) {
			g_error ("[%p]: we got exception %s from echoString!\n",
				 g_thread_self (), ev->_id);
			return NULL;
		}

		CORBA_Object_release (retval, ev);
	}

	CORBA_Object_release (echo_client, ev);

	return data;
}

int
main (int argc, char *argv[])
{
	int i;
	GError *error = NULL;
	GThread **threads;
	CORBA_Environment *ev, real_ev;

	CORBA_exception_init ((ev = &real_ev));

	orb = CORBA_ORB_init (&argc, argv, "orbit-local-mt-orb", ev);

	if (argc < 2) {
		g_error ("Syntax: %s <server IOR> [<niters> [<nthreads>] ]\n",
			 argv [0]);
		return 1;
	}
	server_ior = argv [1];

	if (argc >= 3)
		niters = atoi (argv [2]);

	if (argc >= 4)
		nthreads = atoi (argv [3]);

	threads = g_new0 (GThread *, nthreads);

	for (i = 0; i < nthreads; i++) {
		threads [i] = g_thread_create (
			echo_client_thread, &threads[i],
			TRUE, &error);
		if (error)
			g_error ("Error spawning threads '%s'", 
				 error->message);
	}

	for (i = 0; i < nthreads; i++) {
		if (!(g_thread_join (threads [i]) == &threads [i]))
			g_error ("Wierd thread join problem '%d'", i);
	}

	CORBA_ORB_destroy (orb, ev);
	CORBA_Object_release ((CORBA_Object) orb, ev);

	return 0;
}
