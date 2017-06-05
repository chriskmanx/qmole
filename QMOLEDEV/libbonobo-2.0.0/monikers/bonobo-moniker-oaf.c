/*
 * gnome-moniker-oaf.c: Sample oaf-system based Moniker implementation
 *
 * This is the oaf-activation based Moniker implementation.
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 */
#include <config.h>
#include <string.h>

#define BONOBO_EXPLICIT_TRANSLATION_DOMAIN PACKAGE
#include <bonobo/bonobo-i18n.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker-util.h>

#include "bonobo-moniker-std.h"

Bonobo_Unknown
bonobo_moniker_oaf_resolve (BonoboMoniker               *moniker,
			    const Bonobo_ResolveOptions *options,
			    const CORBA_char            *requested_interface,
			    CORBA_Environment           *ev)
{
	Bonobo_Moniker       parent;
	Bonobo_Unknown       object;
	
	parent = bonobo_moniker_get_parent (moniker, ev);

	if (ev->_major != CORBA_NO_EXCEPTION)
		return CORBA_OBJECT_NIL;
	
	if (parent != CORBA_OBJECT_NIL) {
		bonobo_object_release_unref (parent, ev);

		g_warning ("wierd; oafid moniker with a parent; strange");
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_Bonobo_Moniker_InterfaceNotFound, NULL);
		return CORBA_OBJECT_NIL;
	}

	object = bonobo_activation_activate_from_id (
		(char *) bonobo_moniker_get_name_full (moniker), 0, NULL, ev);

	if (BONOBO_EX (ev)) {
		if (ev->_major == CORBA_USER_EXCEPTION) {
			if (strcmp (ev->_id, ex_Bonobo_GeneralError)) {
				CORBA_exception_free (ev);

				bonobo_exception_general_error_set (
					ev, NULL, _("Exception activating '%s"),
					bonobo_moniker_get_name_full (moniker));
			}
		}
		return CORBA_OBJECT_NIL;

	} else if (object == CORBA_OBJECT_NIL) {

		bonobo_exception_general_error_set (
			ev, NULL, _("Failed to activate '%s"),
			bonobo_moniker_get_name_full (moniker));

		return CORBA_OBJECT_NIL;
	}

	return bonobo_moniker_util_qi_return (object, requested_interface, ev);
}
