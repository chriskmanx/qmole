#include <stdio.h>
#include <string.h>
#include <orbit/orbit.h>

CORBA_ORB orb = CORBA_OBJECT_NIL;

int
main (int argc, char **argv) 
{
	CORBA_Environment  ev[1];
	PortableServer_POA rootpoa, poa;

	CORBA_exception_init (ev);

	orb = CORBA_ORB_init (&argc, argv, "", ev);

	rootpoa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references (orb, "RootPOA", ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	{
		poa = ORBit_POA_new_from (orb, rootpoa, "Foo", NULL, ev);
		g_assert (ev->_major == CORBA_NO_EXCEPTION);
	
		PortableServer_POA_destroy (poa, FALSE, FALSE, ev);
		CORBA_Object_release ((CORBA_Object) poa, ev);
	}

	CORBA_Object_release ((CORBA_Object) rootpoa, ev);

	CORBA_ORB_destroy (orb, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release ((CORBA_Object) orb, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_exception_free (ev);

	fprintf (stderr, "PASS: test-poa\n");

	return 0;
}
