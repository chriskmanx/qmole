/*
 * CORBA echo test
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
#include <ctype.h>
#include "echo.h"
#include "echo-share.h"

/**
   This is used by echo-server.c and echo-local.c
   It uses echo-skels.c
**/

static Echo			the_echo_client;
static CORBA_ORB		the_orb;
static PortableServer_POA	the_poa;
static PortableServer_ObjectId*	the_objid;


static CORBA_Object
do_echoString(PortableServer_Servant servant,
	      const CORBA_char *astring,
	      CORBA_double *outnum,
	      CORBA_Environment *ev)               
{
        
	if (!echo_opt_quiet)
		g_message ("[server] %s", astring);

	*outnum = rand() % 100;
	
	return CORBA_Object_duplicate (the_echo_client, ev);
}

static void
do_doNothing (PortableServer_Servant servant,
	      CORBA_Environment     *ev)        
{
}

static PortableServer_ServantBase__epv base_epv = {
	NULL,
	NULL,
	NULL
};

static POA_Echo__epv echo_epv = { NULL, do_echoString, do_doNothing, NULL };
static POA_Echo__vepv poa_echo_vepv = { &base_epv, &echo_epv };
static POA_Echo poa_echo_servant = { NULL, &poa_echo_vepv };

void
echo_srv_start_poa (CORBA_ORB orb, CORBA_Environment *ev)
{
	PortableServer_POAManager mgr;

	the_orb = orb;
	the_poa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references(the_orb,
						     "RootPOA", ev);

	mgr = PortableServer_POA__get_the_POAManager(the_poa, ev);
	PortableServer_POAManager_activate(mgr, ev);
	CORBA_Object_release((CORBA_Object)mgr, ev);
}

CORBA_Object
echo_srv_start_object(CORBA_Environment *ev)
{
	POA_Echo__init(&poa_echo_servant, ev);
	if ( ev->_major ) {
		printf("object__init failed: %u\n", ev->_major);
		exit(1);
	}
	the_objid = PortableServer_POA_activate_object(the_poa,
						       &poa_echo_servant, ev);
	if ( ev->_major ) {
		printf("activate_object failed: %u\n", ev->_major);
		exit(1);
	}
	the_echo_client = PortableServer_POA_servant_to_reference(the_poa,
								  &poa_echo_servant, ev);
	if ( ev->_major ) {
		printf("servant_to_reference failed: %u\n", ev->_major);
		exit(1);
	}
	return the_echo_client;
}

void
echo_srv_finish_object(CORBA_Environment *ev)
{
	CORBA_Object_release(the_echo_client, ev);
	if ( ev->_major ) {
		printf("object_release failed: %u\n", ev->_major);
		exit(1);
	}
	the_echo_client = NULL;
	PortableServer_POA_deactivate_object(the_poa, the_objid, ev);
	if ( ev->_major ) {
		printf("deactivate_object failed: %u\n", ev->_major);
		exit(1);
	}
	CORBA_free(the_objid);
	the_objid = NULL;
	POA_Echo__fini(&poa_echo_servant, ev);
	if ( ev->_major ) {
		printf("object__fini failed: %u\n", ev->_major);
		exit(1);
	}
}


void
echo_srv_finish_poa(CORBA_Environment *ev)
{
	CORBA_Object_release((CORBA_Object)the_poa, ev);
	if ( ev->_major ) {
		printf("POA release failed: %u\n", ev->_major);
		exit(1);
	}
	the_poa = NULL;
}
