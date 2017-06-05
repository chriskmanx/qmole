/*
 * gnome-moniker-conf-indirect.c: conf_indirect Moniker implementation
 *
 * This is the ior (container) based Moniker implementation.
 *
 * Author:
 *	Rodrigo Moya (rodrigo@gnome-db.org)
 */

#include <config.h>
#include <bonobo/bonobo-i18n.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker-util.h>
#include <gconf/gconf-client.h>

#include "bonobo-moniker-extra.h"

Bonobo_Unknown
bonobo_moniker_conf_indirect_resolve (BonoboMoniker *moniker,
				      const Bonobo_ResolveOptions *options,
				      const CORBA_char *requested_interface,
				      CORBA_Environment *ev)
{
	const char *key;
	char *oiid;
	Bonobo_Unknown object;
	GConfClient *conf_client;
	GError *err = NULL;

	/* retrieve the key contents from GConf */
	key = bonobo_moniker_get_name (moniker);

	if (!gconf_is_initialized ())
		gconf_init (0, NULL, NULL);

	conf_client = gconf_client_get_default ();
	oiid = gconf_client_get_string (conf_client, key, &err);
	g_object_unref (G_OBJECT (conf_client));

	if (!oiid) {
		bonobo_exception_general_error_set (
			ev, NULL,
			err ? err->message : _("Key %s not found in configuration"), key);
		g_error_free (err);
		return CORBA_OBJECT_NIL;
	}

	/* activate the object referenced in the GConf entry */
	object = bonobo_get_object ((const CORBA_char *) oiid,
				    requested_interface,
				    ev);

	return object;
}
