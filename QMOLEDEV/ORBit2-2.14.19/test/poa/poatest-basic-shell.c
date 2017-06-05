/*
 * CORBA POA tests
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
 * Author: Mark McLoughlin <mark@skynet.ie>
 */

#include <stdio.h>
#include <stdlib.h>

#include <orbit/orbit.h>

#include "poatest-basic-shell.h"

PortableServer_POA child_poa = CORBA_OBJECT_NIL;
CORBA_ORB orb = CORBA_OBJECT_NIL;

int
main (int argc, char **argv) 
{
	CORBA_Environment         ev;
	PortableServer_POA        rootpoa;
	PortableServer_POAManager poa_mgr;
	poatest                   poatest_obj;

	g_thread_init (NULL);

	CORBA_exception_init (&ev);

	orb = CORBA_ORB_init (&argc, argv, "", &ev);

	/*
	 * Get the Root POA
	 */
	rootpoa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references (orb, "RootPOA", &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("resolve_initial_references : ", &ev);
		return 1;
	}


	/*
	 * Get the Root POA's POAManager
	 */
	poa_mgr = PortableServer_POA__get_the_POAManager (rootpoa, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("the_POAManager : ", &ev);
		return 1;
	}

	/*
	 * Run the test
	 */
	poatest_obj = poatest_run (rootpoa, poa_mgr);
	if (poatest_obj == CORBA_OBJECT_NIL)
		return 1;

	/*
	 * Call 'test' method and print out execption.
	 */
	poatest_test (poatest_obj, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("test : ", &ev);
		return 1;
	}

	CORBA_Object_release ((CORBA_Object) poatest_obj, &ev);
	CORBA_Object_release ((CORBA_Object) poa_mgr, &ev);
	CORBA_Object_release ((CORBA_Object) rootpoa, &ev);

	if (child_poa != CORBA_OBJECT_NIL) {
		PortableServer_POA_destroy (
			child_poa, CORBA_FALSE, CORBA_FALSE, &ev);
		CORBA_Object_release ((CORBA_Object) child_poa, &ev);
	}

	CORBA_ORB_shutdown (orb, CORBA_TRUE, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("ORB_shutdown : ", &ev);
		return 1;
	}

	CORBA_ORB_destroy (orb, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("ORB_destroy : ", &ev);
		return 1;
	}

	CORBA_Object_release ((CORBA_Object) orb, &ev);

	return 0;
}
