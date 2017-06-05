/*
 * CORBA GIOP timeout test
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Author: Jules Colding <colding@omesc.com>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "timeout_impl.c"

/*
 * This method will return an ORB which is so initialized
 * as support IPv4 IORs
 *
 * orb_name     : Name of return ORB or empty string
 *
 * Return value : Initialized ORB or CORBA::ORB::_nil()
 *
 */
static CORBA_ORB
create_ipv4_orb(const char *orb_name,
		CORBA_Environment *ev)
{
	CORBA_ORB orb = CORBA_OBJECT_NIL;
	int argc = 0;
	char **argv = NULL;

	// sanity checks
	if (!orb_name)
		return CORBA_OBJECT_NIL;

	argc = 5;
	argv = (char**)malloc (sizeof(char*) * argc);
	if (!argv)
		return CORBA_OBJECT_NIL;

	//  dummy argument
	argv[0] = "timeout-server";

	// IPv4 enabled
	argv[1] = "--ORBIIOPIPv4=1";

	// IPv6 enabled
	argv[2] = "--ORBIIOPIPv6=1";

	// explicitly force ORBit2 to be non-local
	argv[3] = "--ORBLocalOnly=0";

	// do not use Unix domain sockets
	argv[4] = "--ORBIIOPUNIX=0";

	// initialize the ORB
	orb = CORBA_ORB_init (&argc, argv, (char*)orb_name, ev);
	if (ev->_major != CORBA_NO_EXCEPTION)
		orb = CORBA_OBJECT_NIL;

	free (argv);

	return orb;
}

int
main (int argc, char *argv[])
{
	PortableServer_ObjectId *objid = NULL;
	PortableServer_POAManager mgr;
	CORBA_Environment ev;
	CORBA_ORB orb = CORBA_OBJECT_NIL;
	PortableServer_POA poa = CORBA_OBJECT_NIL;
	Timeout servant = CORBA_OBJECT_NIL;
	FILE *iorfile;
	char *ior;
	int retv = EXIT_FAILURE;

	signal(SIGINT, exit);
	signal(SIGTERM, exit);

	g_thread_init (NULL);

	CORBA_exception_init (&ev);

	/* create IPv4 orb */
	orb = create_ipv4_orb ("orb-name", &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("create_ipv4_orb(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	/* get root poa */
	poa = (PortableServer_POA)CORBA_ORB_resolve_initial_references (orb, "RootPOA", &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("create_ipv4_orb(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	/* activate root poa */
	mgr = PortableServer_POA__get_the_POAManager (poa, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("PortableServer_POA__get_the_POAManager(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	PortableServer_POAManager_activate (mgr, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("PortableServer_POAManager_activate(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	CORBA_Object_release ((CORBA_Object)mgr, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("CORBA_Object_release(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	/* get corba object */
	servant = impl_Timeout__create (poa, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("CORBA_Object_release(): %s\n", CORBA_exception_id (&ev));
		goto out;
	}

	ior = CORBA_ORB_object_to_string (orb, servant, &ev);
	iorfile = fopen ("timeout-server.iorfile", "w");
	fprintf (iorfile, "%s", ior);
	fclose (iorfile);
	CORBA_free (ior);

	CORBA_ORB_run (orb, &ev);

	objid = PortableServer_POA_reference_to_id (poa, (CORBA_Object)servant, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Exception caught from reference_to_id() - exiting");
		if (objid)
			CORBA_free(objid);
		goto out;
	}

	PortableServer_POA_deactivate_object (poa, objid, &ev);
	CORBA_free(objid);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Exception caught from deactivate_object() - exiting");
		goto out;
	}

	CORBA_Object_release ((CORBA_Object)servant, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Exception caught from release() - exiting");
		goto out;
	}

	PortableServer_POA_destroy (poa, TRUE, FALSE, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Exception caught from destroy() - exiting");
		goto out;
	}

	CORBA_Object_release ((CORBA_Object)poa, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Exception caught from release() - exiting");
		goto out;
	}

	CORBA_ORB_destroy (orb, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Exception caught from destroy() - exiting");
		goto out;
	}
	CORBA_Object_release ((CORBA_Object) orb, &ev);
	if (ev._major != CORBA_NO_EXCEPTION) {
		g_print ("Exception caught from release() - exiting");
		goto out;
	}

	retv = EXIT_SUCCESS;
out:
	return retv;
}
