#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libbonobo.h>
#include <orbit/poa/poa.h>

#define GENERAL_ERROR_MESSAGE "Hello World"

static int
ret_ex_test (CORBA_Environment *ev)
{
	BONOBO_RET_VAL_EX (ev, 1);

	return 0;
}

static void
ex_test (CORBA_Environment *ev)
{
	BONOBO_RET_EX (ev);
}

static int signal_emitted = 0;

static void
system_exception_cb (BonoboObject      *object,
		     CORBA_Object       cobject,
		     CORBA_Environment *ev,
		     gpointer           user_data)
{
	g_assert (BONOBO_IS_OBJECT (object));
	g_assert ((BonoboObject *)user_data == object);

	g_assert (ev != NULL);
	g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);
	g_assert (!strcmp (BONOBO_EX_REPOID (ev),
			   ex_CORBA_COMM_FAILURE));

	signal_emitted = 1;
}

int
main (int argc, char *argv [])
{
	BonoboObject     *object;
	Bonobo_Unknown    ref;
	CORBA_Environment *ev, real_ev;

	free (malloc (8));

	if (bonobo_init (&argc, argv) == FALSE)
		g_error ("Can not bonobo_init");
	bonobo_activate ();

	ev = &real_ev;
	CORBA_exception_init (ev);

	fprintf (stderr, "Local lifecycle\n");
	{
		object = BONOBO_OBJECT (g_object_new (
			bonobo_moniker_get_type (), NULL));

		g_assert (bonobo_object_ref (object) == object);
		g_assert (bonobo_object_unref (BONOBO_OBJECT (object)) == NULL);

		bonobo_object_unref (BONOBO_OBJECT (object));
	}

	fprintf (stderr, "In-proc lifecycle\n");
	{
		object = BONOBO_OBJECT (g_object_new (
			bonobo_moniker_get_type (), NULL));

		ref = CORBA_Object_duplicate (BONOBO_OBJREF (object), NULL);

		bonobo_object_release_unref (ref, NULL);
	}

	fprintf (stderr, "Query interface\n");
	{
		BonoboObject *a, *b;

		a = BONOBO_OBJECT (g_object_new (
			bonobo_moniker_get_type (), NULL));
		b = BONOBO_OBJECT (g_object_new (
			bonobo_stream_mem_get_type (), NULL));

		bonobo_object_add_interface (a, b);

		fprintf (stderr, "  invalid interface\n");
		object = bonobo_object_query_local_interface (
			a, "IDL:This/Is/Not/There:1.0");
		g_assert (object == CORBA_OBJECT_NIL);

		fprintf (stderr, "  symmetry\n");
		object = bonobo_object_query_local_interface (
			a, "IDL:Bonobo/Stream:1.0");
		g_assert (object == b);
		bonobo_object_unref (object);

		object = bonobo_object_query_local_interface (
			b, "IDL:Bonobo/Stream:1.0");
		g_assert (object == b);
		bonobo_object_unref (object);

		object = bonobo_object_query_local_interface (
			a, "IDL:Bonobo/Moniker:1.0");
		g_assert (object == a);
		bonobo_object_unref (object);

		object = bonobo_object_query_local_interface (
			b, "IDL:Bonobo/Moniker:1.0");
		g_assert (object == a);
		bonobo_object_unref (object);

		fprintf (stderr, "  remote\n");
		ref = Bonobo_Unknown_queryInterface (
			BONOBO_OBJREF (a), "IDL:Broken/1.0", ev);
		g_assert (!BONOBO_EX (ev));
		g_assert (ref == CORBA_OBJECT_NIL);

		ref = Bonobo_Unknown_queryInterface (
			BONOBO_OBJREF (a), "IDL:Bonobo/Stream:1.0", ev);
		g_assert (!BONOBO_EX (ev));
		g_assert (ref == BONOBO_OBJREF (b));
		bonobo_object_release_unref (ref, ev);
		g_assert (!BONOBO_EX (ev));

		bonobo_object_unref (a);
	}

	fprintf (stderr, "Environment exception checks\n");
	{
		object = BONOBO_OBJECT (g_object_new (
			bonobo_moniker_get_type (), NULL));

		g_signal_connect (G_OBJECT (object),
				  "system_exception",
				  G_CALLBACK (system_exception_cb),
				  object);

		CORBA_exception_set_system (
			ev, ex_CORBA_COMM_FAILURE,
			CORBA_COMPLETED_MAYBE);
		g_assert (BONOBO_EX (ev));

		signal_emitted = 0;
		BONOBO_OBJECT_CHECK (
			object, BONOBO_OBJREF (object), ev);
		g_assert (signal_emitted);

		CORBA_exception_free (ev);

		bonobo_object_unref (object);
	}

	fprintf (stderr, "Servant mapping...\n");
	{
		PortableServer_Servant servant;

		object = BONOBO_OBJECT (g_object_new (
			bonobo_moniker_get_type (), NULL));

		servant = (PortableServer_Servant) &object->servant;

		g_assert (bonobo_object (object) == object);
		g_assert (bonobo_object (&object->servant) == object);
		g_assert (bonobo_object_get_servant (object) == servant);
		g_assert (bonobo_object_from_servant (servant) == object);
		g_assert (bonobo_object_fast (object) == object);
		g_assert (bonobo_object_fast (servant) == object);

		bonobo_object_unref (object);
	}

	fprintf (stderr, "Ret-ex tests...\n");
	{
		g_assert (!ret_ex_test (ev));
		ex_test (ev);

		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_PropertyBag_NotFound, NULL);
		g_assert (ret_ex_test (ev));
		
		CORBA_exception_free (ev);
	}

	fprintf (stderr, "General error tests...\n");
	{
		bonobo_exception_general_error_set (
			ev, NULL, "a%s exception occured", "n exceptional");
		g_assert (BONOBO_EX (ev));
		g_assert (!strcmp (BONOBO_EX_REPOID (ev), ex_Bonobo_GeneralError));
		g_assert (!strcmp (bonobo_exception_get_text (ev),
				   "an exceptional exception occured"));
	}

	fprintf (stderr, "All tests passed\n");

	return bonobo_debug_shutdown ();
}
