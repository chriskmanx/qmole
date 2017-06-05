/*
 * gnome-moniker-ior.c: Sample ior-system based Moniker implementation
 *
 * This is the ior (container) based Moniker implementation.
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 */
#include <config.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker-util.h>

#include "bonobo-moniker-std.h"

Bonobo_Unknown
bonobo_moniker_ior_resolve (BonoboMoniker               *moniker,
			    const Bonobo_ResolveOptions *options,
			    const CORBA_char            *requested_interface,
			    CORBA_Environment           *ev)
{
	const char    *ior;
	CORBA_Object   object;
	Bonobo_Unknown retval;
	gboolean       is_unknown, is_correct;

	ior = bonobo_moniker_get_name (moniker);
	
	object = CORBA_ORB_string_to_object (bonobo_orb (), ior, ev);
	BONOBO_RET_VAL_EX (ev, CORBA_OBJECT_NIL);

	is_unknown = CORBA_Object_is_a (object, "IDL:Bonobo/Unknown:1.0", ev);
	BONOBO_RET_VAL_EX (ev, CORBA_OBJECT_NIL);

	if (!is_unknown) {
		is_correct = CORBA_Object_is_a (object, requested_interface, ev);
		BONOBO_RET_VAL_EX (ev, CORBA_OBJECT_NIL);

		if (is_correct)
			return object;
		else {
			CORBA_exception_set (
				ev, CORBA_USER_EXCEPTION,
				ex_Bonobo_Moniker_InterfaceNotFound, NULL);
			return CORBA_OBJECT_NIL;
		}
	}

	retval = Bonobo_Unknown_queryInterface (
		object, requested_interface, ev);
	BONOBO_RET_VAL_EX (ev, CORBA_OBJECT_NIL);
	
	if (retval == CORBA_OBJECT_NIL)
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			ex_Bonobo_Moniker_InterfaceNotFound, NULL);

	return retval;
}
