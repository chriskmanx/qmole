#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test-any.h"

static TestAny client;

static CORBA_any *
do_print(PortableServer_Servant servant,
	 const CORBA_any *any,
	 CORBA_Environment *ev);

static PortableServer_ServantBase__epv base_epv = {
	NULL,
	NULL,
	NULL
};

static POA_TestAny__epv TestAny_epv = { NULL, do_print };
static POA_TestAny__vepv poa_TestAny_vepv = { &base_epv, &TestAny_epv };
static POA_TestAny poa_TestAny_servant = { NULL, &poa_TestAny_vepv };

int
main (int argc, char *argv[])
{
	FILE *iorfile;
	PortableServer_ObjectId *objid;
	PortableServer_POA poa;

	CORBA_Environment ev;
	char *retval;
	CORBA_ORB orb;

	CORBA_exception_init(&ev);
	orb = CORBA_ORB_init(&argc, argv, "orbit-local-orb", &ev);

	POA_TestAny__init(&poa_TestAny_servant, &ev);

	poa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references(orb, "RootPOA", &ev);

	PortableServer_POAManager_activate(
		PortableServer_POA__get_the_POAManager(poa, &ev), &ev);

	objid = PortableServer_POA_activate_object(poa,
						   &poa_TestAny_servant, 
						   &ev);

	client = PortableServer_POA_servant_to_reference(poa,
							 &poa_TestAny_servant, 
							 &ev);
	if (!client) {
		printf("Cannot get objref\n");
		return 1;
	}
    
	retval = CORBA_ORB_object_to_string(orb, client, &ev);

	iorfile = fopen ("test-any-server.iorfile", "w");
	fprintf(iorfile, "%s\n", retval);
	fclose(iorfile);

	g_print("%s\n", retval); fflush(stdout);
    
	CORBA_free(retval);
    
	CORBA_ORB_run(orb, &ev);
    
	return 0;
}

static CORBA_any*
do_print(PortableServer_Servant servant,
	 const CORBA_any *any,
	 CORBA_Environment *ev)
{
	TestAnyStruct* any_struct = any->_value; 
	CORBA_any *retval;
  
	g_message("[server] %d: %s", any_struct->long_value, 
		  any_struct->string_value);

	retval = CORBA_any_alloc();
#if 1
	retval->_type = (CORBA_TypeCode)TC_TestAnyStruct;
	retval->_value = TestAnyStruct__alloc();
	((TestAnyStruct *)retval->_value)->long_value = 84;
	((TestAnyStruct *)retval->_value)->string_value 
		= CORBA_string_dup("Hi there");
#else
	retval->_type = (CORBA_TypeCode)TC_null;
	retval->_value = NULL;
#endif

	CORBA_any_set_release(retval, CORBA_TRUE);

	return retval;
}
