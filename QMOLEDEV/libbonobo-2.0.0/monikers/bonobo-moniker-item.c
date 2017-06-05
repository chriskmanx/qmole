/*
 * gnome-moniker-item.c: Sample item-system based Moniker implementation
 *
 * This is the item (container) based Moniker implementation.
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 */
#include <config.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker-util.h>

#include "bonobo-moniker-std.h"

Bonobo_Unknown
bonobo_moniker_item_resolve (BonoboMoniker               *moniker,
			     const Bonobo_ResolveOptions *options,
			     const CORBA_char            *requested_interface,
			     CORBA_Environment           *ev)
{
	Bonobo_Moniker       parent;
	Bonobo_ItemContainer container;
	Bonobo_Unknown       containee;
	Bonobo_Unknown       retval = CORBA_OBJECT_NIL;
	
	parent = bonobo_moniker_get_parent (moniker, ev);

	if (BONOBO_EX (ev))
		return CORBA_OBJECT_NIL;
	
	if (parent == CORBA_OBJECT_NIL) {
		g_warning ("Item moniker with no parent !");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Moniker_InterfaceNotFound, NULL);
		return CORBA_OBJECT_NIL;
	}

	container = Bonobo_Moniker_resolve (parent, options,
					    "IDL:Bonobo/ItemContainer:1.0", ev);

	if (BONOBO_EX (ev))
		goto return_unref_parent;

	if (container == CORBA_OBJECT_NIL) {
		g_warning ("Failed to extract a container from our parent");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Moniker_InterfaceNotFound, NULL);
		goto return_unref_parent;
	}

	containee = Bonobo_ItemContainer_getObjectByName (
		container, bonobo_moniker_get_name (moniker),
		TRUE, ev);

	bonobo_object_release_unref (container, ev);

	return bonobo_moniker_util_qi_return (containee, requested_interface, ev);

 return_unref_parent:
	bonobo_object_release_unref (parent, ev);

	return retval;
}
