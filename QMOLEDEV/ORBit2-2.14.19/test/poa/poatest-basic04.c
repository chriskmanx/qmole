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

/*
 * Test 4 : poatest-basic04.c
 *     o POA with USER_ID Id Assignment policy.
 *     o activated object with user assigned Id.
 */

#include <stdio.h>
#include <stdlib.h>

#include <orbit/orbit.h>

#include "poatest-basic-shell.h"

static void
poatest_test_impl (PortableServer_Servant servant, CORBA_Environment *ev) { }

PortableServer_ServantBase__epv base_epv = {
	NULL,     /* _private    */
	NULL,     /* finalize    */
	NULL      /* default_POA */
};

POA_poatest__epv poatest_epv = {
	NULL,                /* _private */
	poatest_test_impl    /* test     */
};

POA_poatest__vepv poatest_vepv = {
	&base_epv,      /* _base_epv */
	&poatest_epv    /* poatest_epv  */
};

POA_poatest poatest_servant = {
	NULL,             /* _private */
	&poatest_vepv     /* vepv     */
};

poatest
poatest_run (PortableServer_POA        rootpoa,
             PortableServer_POAManager rootpoa_mgr)
{
	CORBA_Environment        ev;
	poatest                  poatest_obj;
	CORBA_PolicyList        *poa_policies;
	PortableServer_ObjectId *objid;

	CORBA_exception_init (&ev);
 
	/*
	 * Create child POA with USER_ID Id Assignment policy.
	 */
	poa_policies           = CORBA_PolicyList__alloc ();
	poa_policies->_maximum = 1;
	poa_policies->_length  = 1;
	poa_policies->_buffer  = CORBA_PolicyList_allocbuf (1);
	CORBA_sequence_set_release (poa_policies, CORBA_TRUE);

	poa_policies->_buffer[0] = (CORBA_Policy)
					PortableServer_POA_create_id_assignment_policy (
							rootpoa,
							PortableServer_USER_ID,
							&ev);

	child_poa = PortableServer_POA_create_POA (rootpoa,
						   "User Id POA",
						   rootpoa_mgr,
						   poa_policies,
						   &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("create_POA : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	CORBA_Policy_destroy (poa_policies->_buffer[0], &ev);
	CORBA_free (poa_policies);

	/*
	 * Initialise the servant.
	 */
	POA_poatest__init (&poatest_servant, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("POA_poatest__init : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	/*
	 * Set up the ObjectId.
	 */
	objid = PortableServer_string_to_ObjectId ("Test Id", &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("string_to_ObjectId : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	/*
	 * Activate object with user assigned Id.
	 */
	PortableServer_POA_activate_object_with_id (child_poa, objid, &poatest_servant, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("activate_object_with_id : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	CORBA_free (objid);

	poatest_obj = PortableServer_POA_servant_to_reference (child_poa, &poatest_servant, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("servant_to_reference : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	/*
	 * Activate the POAManager. POA will now accept requests
	 */
	PortableServer_POAManager_activate (rootpoa_mgr, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("POAManager_activate : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	return poatest_obj;
}
