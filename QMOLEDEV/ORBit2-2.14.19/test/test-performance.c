#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <orbit/orbit.h>

#include "test1.h"

static CORBA_ORB  orb;
static GTimer    *timer;
static double     bogomark = 0.0;
static double     elapsed_time;

static void
test_copy (void)
{
	int i, j;
#define ELEMS (sizeof (tc) / sizeof (tc[0]))
	CORBA_TypeCode tc[] = {
		TC_CORBA_octet,
		TC_CORBA_sequence_CORBA_octet,
		TC_CORBA_double,
		TC_CORBA_string,
		TC_CORBA_sequence_CORBA_string,
		TC_GIOP_TargetAddress		
	};
	gpointer data [ELEMS];
	const char *test_string = "This is a sample string, for dupping";

	fprintf (stderr, "Testing copy...\n");

	for (i = 0; i < ELEMS; i++) {
		data [i] = ORBit_dynany_new_default (tc [i]);

		g_timer_reset (timer);
		for (j = 0; j < 1000; j++) {
			gpointer foo = ORBit_copy_value (data [i], tc [i]);
			CORBA_free (foo);
		}
		elapsed_time = g_timer_elapsed (timer, NULL);
		bogomark += elapsed_time;
		fprintf (stderr, " copy %20s : %g(ms)\n",
			tc[i]->repo_id == NULL ? "(null)" : tc[i]->repo_id,
			elapsed_time);
	}

	fprintf (stderr, "Testing strdup ...\n");

	g_timer_reset (timer);
	for (i = 0; i < 10000; i++) {
		char *str = g_strdup (test_string);
		g_free (str);
	}
	elapsed_time = g_timer_elapsed (timer, NULL) / 10.0;
	bogomark += elapsed_time;
	fprintf (stderr, " g_strdup :     %g(ns)\n", elapsed_time * 1000.0);
	
	g_timer_reset (timer);
	for (i = 0; i < 10000; i++) {
		char *str = CORBA_string_dup (test_string);
		CORBA_free (str);
	}
	elapsed_time = g_timer_elapsed (timer, NULL) / 10.0;
	bogomark += elapsed_time;
	fprintf (stderr, " CORBA_strdup : %g(ns)\n", elapsed_time * 1000.0);
}

static PortableServer_POA
create_mult_id_poa (CORBA_Environment *ev)
{
	PortableServer_POA  rootpoa;
	PortableServer_POA  retval;
	CORBA_PolicyList   *policies;

	rootpoa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references (orb, "RootPOA", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	policies           = CORBA_PolicyList__alloc ();
	policies->_maximum = 1;
	policies->_length  = 1;
	policies->_buffer  = CORBA_PolicyList_allocbuf (1);
	CORBA_sequence_set_release (policies, CORBA_TRUE);

	policies->_buffer[0] = (CORBA_Policy)
					PortableServer_POA_create_id_uniqueness_policy (
							rootpoa,
							PortableServer_MULTIPLE_ID,
							ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	retval = PortableServer_POA_create_POA (rootpoa, "Multiple Id POA",
					        NULL, policies, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Policy_destroy (policies->_buffer[0], ev);
	CORBA_free (policies);

	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release ((CORBA_Object) rootpoa, ev);

	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	return retval;
}

static PortableServer_ServantBase__epv base_epv = { NULL, NULL, NULL };
static POA_Test__epv                   test_epv = { NULL, NULL };
static POA_Test__vepv                  test_vepv = { &base_epv, &test_epv };
static POA_Test                        test_servant = { NULL, &test_vepv };

static void
test_activation (void)
{
	CORBA_Environment   env;
	PortableServer_POA  poa;
	GSList             *objids = NULL, *l;
	int                 i;

	fprintf (stderr, "Testing object activation...\n");

	CORBA_exception_init (&env);

	POA_Test__init (&test_servant, &env);

	poa = create_mult_id_poa (&env);

	g_assert (env._major == CORBA_NO_EXCEPTION);

	g_timer_reset (timer);

	for (i = 0; i < 1000; i++) {
		PortableServer_ObjectId *objid;

		objid = PortableServer_POA_activate_object (poa, &test_servant, &env);
		g_assert (env._major == CORBA_NO_EXCEPTION);

		objids = g_slist_append (objids, objid);
	}

	elapsed_time = g_timer_elapsed (timer, NULL);
	bogomark += elapsed_time;
	fprintf (stderr, " activation : %g(ms)\n", elapsed_time);

	g_timer_reset (timer);

	for (l = objids; l; l = l->next) {
		PortableServer_POA_deactivate_object (poa, l->data, &env);
		g_assert (env._major == CORBA_NO_EXCEPTION);
	}

	elapsed_time = g_timer_elapsed (timer, NULL);
	bogomark += elapsed_time;
	fprintf (stderr, " de-activation : %g(ms)\n", elapsed_time);
	
	for (l = objids; l; l = l->next)
		CORBA_free (l->data);
	g_slist_free (objids);

	POA_Test__fini (&test_servant, &env);

	PortableServer_POA_destroy (poa, CORBA_FALSE, CORBA_FALSE, &env);
	g_assert (env._major == CORBA_NO_EXCEPTION);
	CORBA_Object_release ((CORBA_Object) poa, &env);
	g_assert (env._major == CORBA_NO_EXCEPTION);

	CORBA_exception_free (&env);
}

int
main (int argc, char *argv[])
{
	CORBA_Environment ev;

	free (malloc (8));

	g_thread_init (NULL);

	CORBA_exception_init (&ev);

	timer = g_timer_new ();
	g_timer_start (timer);

	g_timer_reset (timer);
	orb = CORBA_ORB_init (&argc, argv, "orbit-local-orb", &ev);
	g_assert (ev._major == CORBA_NO_EXCEPTION);
	fprintf (stderr, "ORB: init took %g(ms)\n",
		 (elapsed_time = g_timer_elapsed (timer, NULL)) * 1000.0);
	bogomark += elapsed_time;

	test_copy ();

	test_activation ();

	g_timer_reset (timer);
	CORBA_ORB_destroy (orb, &ev);
	g_assert (ev._major == CORBA_NO_EXCEPTION);
	fprintf (stderr, "ORB: destroy took %g(ms)\n",
		 (elapsed_time = g_timer_elapsed (timer, NULL)) * 1000.0);
	bogomark += elapsed_time;
	g_timer_reset (timer);

	CORBA_Object_release ((CORBA_Object) orb, &ev);
	g_assert (ev._major == CORBA_NO_EXCEPTION);

	g_timer_destroy (timer);

	fprintf (stderr, "Overall bogomark %g\n", 1000.0 / bogomark);

	return 0;
}
