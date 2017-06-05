/*
 * gnome-moniker-config.c: GConf Moniker implementation
 *
 * This is the GConf based Moniker implementation.
 *
 * Author:
 *	Rodrigo Moya (rodrigo@gnome-db.org)
 */

#include <string.h>
#include <bonobo/bonobo-exception.h>
#include "bonobo-config-bag.h"
#include "bonobo-moniker-extra.h"

Bonobo_Unknown
bonobo_moniker_config_resolve (BonoboMoniker *moniker,
			       const Bonobo_ResolveOptions *options,
			       const CORBA_char *requested_interface,
			       CORBA_Environment *ev)
{
	const gchar *name;

	name = bonobo_moniker_get_name (moniker);

	if (!strcmp (requested_interface, "IDL:Bonobo/PropertyBag:1.0")) {
		BonoboConfigBag *bag;

		bag = bonobo_config_bag_new (name);
		if (bag) {
			return (Bonobo_Unknown) CORBA_Object_duplicate (
				BONOBO_OBJREF (bag), ev);
		}

		bonobo_exception_set (ev, ex_Bonobo_Moniker_InterfaceNotFound);
	}
	else
		bonobo_exception_set (ev, ex_Bonobo_Moniker_InterfaceNotFound);

	return CORBA_OBJECT_NIL;
}
