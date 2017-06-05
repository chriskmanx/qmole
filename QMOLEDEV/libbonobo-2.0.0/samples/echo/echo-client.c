/*
 * Sample user for the Echo Bonobo component
 *
 * Author:
 *   Miguel de Icaza  (miguel@helixcode.com)
 */

#include <config.h>
#include <libbonobo.h>
#include "Bonobo_Sample_Echo.h"

int 
main (int argc, char *argv [])
{
	Bonobo_Sample_Echo echo_server;
	CORBA_Environment  ev;

	/*
	 * Initialize bonobo.
	 */
	if (!bonobo_init (&argc, argv))
		g_error (_("I could not initialize Bonobo"));
	
	/*
	 * Enable CORBA/Bonobo to start processing requests
	 */
	bonobo_activate ();

	echo_server = bonobo_get_object ("OAFIID:Bonobo_Sample_Echo",
					 "Bonobo/Sample/Echo", NULL);

	if (echo_server == CORBA_OBJECT_NIL) {
		g_warning (_("Could not create an instance of the sample echo component"));
		return bonobo_debug_shutdown ();
	}

	/* Send a message */
	CORBA_exception_init (&ev);

	Bonobo_Sample_Echo_echo (echo_server, "This is the message from the client\n", &ev);

	/* Check for exceptions */
	if (BONOBO_EX (&ev)) {
		char *err = bonobo_exception_get_text (&ev);
		g_warning (_("An exception occured '%s'"), err);
		g_free (err);
	}

	CORBA_exception_free (&ev);

	bonobo_object_release_unref (echo_server, NULL);
	
	return bonobo_debug_shutdown ();
}
