/*
 * CORBA empty test
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
 * Author: Elliot Lee <sopwith@redhat.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "empty.h"

static Empty empty_client = CORBA_OBJECT_NIL;

static void do_Nothing(PortableServer_Servant servant, CORBA_Environment *ev);

static PortableServer_ServantBase__epv base_epv = {
	NULL,
	NULL,
	NULL
};
static POA_Empty__epv empty_epv = { NULL, do_Nothing };
static POA_Empty__vepv poa_empty_vepv = { &base_epv, &empty_epv };
static POA_Empty poa_empty_servant = { NULL, &poa_empty_vepv };

static void do_exit(int arg)
{
	exit(2);
}

int
main (int argc, char *argv[])
{
	FILE *iorfile;
	PortableServer_ObjectId objid = {0, sizeof("myFoo"), "myFoo"};
	PortableServer_POA poa;

	CORBA_Environment ev;
	char *retval;
	CORBA_ORB orb;
	PortableServer_ObjectId *oid;

	signal(SIGINT, do_exit);
	signal(SIGTERM, do_exit);

	CORBA_exception_init(&ev);
	orb = CORBA_ORB_init(&argc, argv, "orbit-local-orb", &ev);

	POA_Empty__init(&poa_empty_servant, &ev);

	poa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references(orb,
						     "RootPOA", &ev);
	PortableServer_POAManager_activate(
		PortableServer_POA__get_the_POAManager(poa, &ev), 
		&ev);

	oid = PortableServer_POA_activate_object(poa, &poa_empty_servant, &ev);
	if(ev._major == CORBA_NO_EXCEPTION)
		CORBA_free(oid);

	empty_client = 
		PortableServer_POA_servant_to_reference(poa,
							&poa_empty_servant,
							&ev);
	if (ev._major != CORBA_NO_EXCEPTION)
	{
		printf("Cannot get objref\n");
		return 1;
	}

	retval = CORBA_ORB_object_to_string(orb, empty_client, &ev);

	iorfile = fopen ("empty-server.iorfile", "w");
	fprintf(iorfile, "%s\n", retval);
	fclose(iorfile);

	g_print("%s\n", retval); fflush(stdout);

	CORBA_free(retval);

	CORBA_ORB_run(orb, &ev);

	PortableServer_POA_deactivate_object(poa, &objid, &ev);

	return 0;
}

static void
do_Nothing(PortableServer_Servant servant,
	   CORBA_Environment *ev)
{
}
