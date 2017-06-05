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
 * Test 6 : poatest-basic06.c
 *     o POA with MULTIPLE_ID Object Id Uniqueness policy.
 *     o activate two objects with the same servant.
 */

#include <stdio.h>
#include <stdlib.h>

#include <orbit/orbit.h>

#include "poatest-basic-shell.h"

static void
poatest_test_impl (PortableServer_Servant servant, CORBA_Environment *ev) { }

PortableServer_ServantBase__epv base_epv = {
	NULL,    /* _private    */
	NULL,    /* finalize    */
	NULL     /* default_POA */
};

POA_poatest__epv poatest_epv = {
	NULL,                /* _private */
	poatest_test_impl    /* test     */
};

POA_poatest__vepv poatest_vepv = {
	&base_epv,         /* _base_epv    */
	&poatest_epv       /* poatest_epv  */
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
	PortableServer_ObjectId *obj1id, *obj2id;

	CORBA_exception_init (&ev);
 
	/*
	 * Create child POA with MULTIPLE_ID Object Id Uniqueness policy.
	 */
	poa_policies           = CORBA_PolicyList__alloc ();
	poa_policies->_maximum = 1;
	poa_policies->_length  = 1;
	poa_policies->_buffer  = CORBA_PolicyList_allocbuf (1);
	CORBA_sequence_set_release (poa_policies, CORBA_TRUE);

	poa_policies->_buffer[0] = (CORBA_Policy)
					PortableServer_POA_create_id_uniqueness_policy (
							rootpoa,
							PortableServer_MULTIPLE_ID,
							&ev);

	child_poa = PortableServer_POA_create_POA (rootpoa,
						   "Multiple Id POA",
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
	 * Activate two objects.
	 */
	obj1id = PortableServer_POA_activate_object (child_poa, &poatest_servant, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("activate_object : ", &ev);
		return CORBA_OBJECT_NIL;
	}
	CORBA_free (obj1id);

	obj2id = PortableServer_POA_activate_object (child_poa, &poatest_servant, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("activate_object : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	/*
	 * Get reference for second object.
	 */
	poatest_obj = PortableServer_POA_id_to_reference (child_poa, obj2id, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("id_to_reference : ", &ev);
		return CORBA_OBJECT_NIL;
	}
	CORBA_free (obj2id);

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
