#include <config.h>
#include <stdio.h>
#include <string.h>
#include <libbonobo.h>

static int idle_id, em_count, ev_count;

#define CHECK_RESULT(ev, evc, emc) {g_assert (!BONOBO_EX (ev)); g_assert (evc == ev_count); g_assert (emc == em_count);}

static void
event_cb (BonoboListener    *listener,
	  const char        *event_name, 
	  const CORBA_any   *any,
	  CORBA_Environment *ev,
	  gpointer           user_data)
{
	if (!strcmp (event_name, user_data))
		em_count++;
	ev_count++;

	printf ("Got Event %s %s %d %d\n",
		event_name, (char *)user_data, 
		ev_count, em_count);
}

static void
run_tests (void)
{
	BonoboEventSource *es;
	CORBA_Environment ev;
	CORBA_any *value;
	char *mask;

	CORBA_exception_init (&ev);

	g_source_remove (idle_id);

	value = bonobo_arg_new (BONOBO_ARG_LONG);

	es = bonobo_event_source_new ();
	g_assert (es != NULL);
	
	mask = "a/test";
	bonobo_event_source_client_add_listener (BONOBO_OBJREF (es), event_cb, mask, &ev, mask);

	mask = "=a/test";
	bonobo_event_source_client_add_listener (BONOBO_OBJREF (es), event_cb, mask, &ev, mask);
	
	bonobo_event_source_notify_listeners (es, "a/test", value, &ev); 
	CHECK_RESULT (&ev, 2, 1);

	bonobo_event_source_notify_listeners (es, "a/test/xyz", value, &ev); 
	CHECK_RESULT (&ev, 3, 1);

	bonobo_event_source_notify_listeners (es, "a/tes", value, &ev); 
	CHECK_RESULT (&ev, 3, 1);

	bonobo_event_source_notify_listeners (es, "test", value, &ev); 
	CHECK_RESULT (&ev, 3, 1);

	bonobo_event_source_notify_listeners (es, "a/test", value, &ev); 
	CHECK_RESULT (&ev, 5, 2);
	
	bonobo_event_source_notify_listeners (es, "a/test:", value, &ev); 
	CHECK_RESULT (&ev, 6, 2);

	bonobo_event_source_notify_listeners (es, "a/test:xyz", value, &ev); 
	CHECK_RESULT (&ev, 7, 2);

	bonobo_event_source_notify_listeners (es, "a/", value, &ev); 
	CHECK_RESULT (&ev, 7, 2);

	bonobo_event_source_notify_listeners (es, "a/test1:xyz", value, &ev); 
	CHECK_RESULT (&ev, 8, 2);

	bonobo_object_unref (BONOBO_OBJECT (es));

	bonobo_main_quit ();
}

int
main (int argc, char **argv)
{
	if (!bonobo_init (&argc, argv))
		g_error ("Cannot init bonobo");
	
	idle_id = g_idle_add ((GSourceFunc) run_tests, NULL);

	bonobo_main ();

	return bonobo_debug_shutdown ();
}
