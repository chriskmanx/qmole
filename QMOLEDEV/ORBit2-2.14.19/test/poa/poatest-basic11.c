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
 * Test 1 : poatest-basic01.c
 *     o Root POA.
 *     o activated object with system assigned id.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <orbit/orbit.h>

#include "poatest-basic-shell.h"

static void
poatest_test_impl (PortableServer_Servant servant, CORBA_Environment *ev) { }

static PortableServer_ServantBase__epv base_epv = {
	NULL,    /* _private    */
	NULL,    /* finalize    */
	NULL     /* default_POA */
};

static POA_poatest__epv poatest_epv = {
	NULL,               /* _private */
	poatest_test_impl   /* test     */
};

static POA_poatest__vepv poatest_vepv = {
	&base_epv,    /* _base_epv    */
	&poatest_epv  /* poatest_epv  */
};

static POA_poatest poatest_servant = {
	NULL,         /* _private */
	&poatest_vepv /* vepv     */
};

poatest
poatest_run (PortableServer_POA        rootpoa,
	     PortableServer_POAManager rootpoa_mgr )
{
	CORBA_Environment        ev;
	PortableServer_ObjectId *objid;
	PortableServer_ObjectId *objid_back;
	CORBA_char              *objid_str;

	CORBA_exception_init( &ev );

	/*
	 * Initialise the servant.
	 */
	POA_poatest__init (&poatest_servant, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("POA_poatest__init : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	/*
	 * Activate object. POA will assign an ObjectId.
	 */
	objid = PortableServer_POA_activate_object (rootpoa, &poatest_servant, &ev);
	if (POATEST_EX (&ev)) {
		POATEST_PRINT_EX ("activate_object : ", &ev);
		return CORBA_OBJECT_NIL;
	}

	objid_str = PortableServer_ObjectId_to_string (objid, &ev);
	g_assert (objid_str != NULL);
	g_assert (strlen (objid_str) == objid->_length);

	objid_back = PortableServer_string_to_ObjectId (objid_str, &ev);

	g_assert (objid->_length == objid_back->_length);
	g_assert (!memcmp (objid->_buffer, objid_back->_buffer,
			   objid->_length));
	
	CORBA_free (objid_back);
	CORBA_free (objid_str);
	CORBA_free (objid);


	return CORBA_OBJECT_NIL;
}
