#include <stdio.h>
#include "test-any.h"

int
main (int argc, char *argv[])
{
	CORBA_Environment ev;
	CORBA_ORB orb;
  
	TestAny obj;
	TestAnyStruct any_value, *retany_value;

	CORBA_any *retany, any;

	CORBA_exception_init(&ev);
	orb = CORBA_ORB_init(&argc, argv, "orbit-local-orb", &ev);
  
	if(argc < 2)
	{
		printf("Need a binding ID thing as argv[1]\n");
		return 1;
	}

	obj = CORBA_ORB_string_to_object(orb, argv[1], &ev);
 
	if (!obj) 
	{
		printf("Cannot bind to %s\n", argv[1]);
		return 1;
	}

	any_value.long_value = 42;
	any_value.string_value = "fourty two.";

	any._type = (CORBA_TypeCode)TC_TestAnyStruct;
	any._value = &any_value;
	CORBA_any_set_release(&any, CORBA_FALSE );
	retany = TestAny_print (obj, &any, &ev);
	if(ev._major == CORBA_NO_EXCEPTION)
	{

		retany_value = retany->_value;
		if(retany_value)
			g_message("long %d string %s",
				  retany_value->long_value,
				  retany_value->string_value);

		CORBA_free(retany);
	}
	else
	{
		printf("we got exception %u from TestAny_print!\n", ev._major);
		return 1;
	}
  
	CORBA_Object_release(obj, &ev);
	CORBA_Object_release((CORBA_Object)orb, &ev);
  
	return 0;
}
