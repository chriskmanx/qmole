/*
 * gnome-moniker-query.c: Sample query-activation based Moniker implementation
 *
 * This is the Oaf query based Moniker implementation.
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 */
#include <config.h>
#include <string.h>
#include <bonobo/bonobo-moniker.h>
#include <bonobo/bonobo-moniker-util.h>

#include "bonobo-moniker-std.h"

Bonobo_Unknown
bonobo_moniker_query_resolve (BonoboMoniker               *moniker,
			      const Bonobo_ResolveOptions *options,
			      const CORBA_char            *requested_interface,
			      CORBA_Environment           *ev)
{
	Bonobo_Moniker       parent;
	Bonobo_Unknown       object;
	char                *query;
	
	parent = bonobo_moniker_get_parent (moniker, ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		return CORBA_OBJECT_NIL;
	
	if (parent != CORBA_OBJECT_NIL) {
		bonobo_object_release_unref (parent, ev);

		g_warning ("wierd; queryied moniker with a parent; strange");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Moniker_InterfaceNotFound, NULL);
		return CORBA_OBJECT_NIL;
	}

	query = g_strdup_printf ("%s AND repo_ids.has ('%s')",
				 bonobo_moniker_get_name (moniker),
				 requested_interface);

	object = bonobo_activation_activate (query, NULL, 0, NULL, ev);

	g_free (query);

	return bonobo_moniker_util_qi_return (object, requested_interface, ev);
}
