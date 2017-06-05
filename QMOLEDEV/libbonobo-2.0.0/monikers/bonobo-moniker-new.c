/*
 * gnome-moniker-new.c: Sample generic factory 'new'
 * Moniker implementation.
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000, Ximian, Inc.
 */
#include <config.h>

#include <bonobo/bonobo-moniker-util.h>

#include "bonobo-moniker-std.h"

Bonobo_Unknown
bonobo_moniker_new_resolve (BonoboMoniker               *moniker,
			    const Bonobo_ResolveOptions *options,
			    const CORBA_char            *requested_interface,
			    CORBA_Environment           *ev)
{
	Bonobo_Moniker        parent;
	Bonobo_GenericFactory factory;
	Bonobo_Unknown        containee;
	Bonobo_Unknown        retval = CORBA_OBJECT_NIL;
	
	parent = bonobo_moniker_get_parent (moniker, ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		return CORBA_OBJECT_NIL;

	g_assert (parent != CORBA_OBJECT_NIL);

	factory = Bonobo_Moniker_resolve (parent, options,
					  "IDL:Bonobo/GenericFactory:1.0", ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		goto return_unref_parent;

	if (factory == CORBA_OBJECT_NIL) {
		g_warning ("Failed to extract a factory from our parent");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Moniker_InterfaceNotFound, NULL);
		goto return_unref_parent;
	}

	containee = Bonobo_GenericFactory_createObject (
		factory, requested_interface, ev);

	bonobo_object_release_unref (factory, ev);

	return bonobo_moniker_util_qi_return (containee, requested_interface, ev);

 return_unref_parent:
	bonobo_object_release_unref (parent, ev);

	return retval;
}
