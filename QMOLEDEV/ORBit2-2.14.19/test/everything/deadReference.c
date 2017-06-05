/*
 * CORBA C language mapping tests
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

#include "everything.h"

static void
DeadReferenceObj_test (PortableServer_Servant  servant,
		       CORBA_Environment      *ev)
{
	PortableServer_Current poa_current;
	PortableServer_POA     poa;
	CORBA_Object           obj = CORBA_OBJECT_NIL;

	poa_current = (PortableServer_Current)
			CORBA_ORB_resolve_initial_references (global_orb,
							      "POACurrent",
							      ev);

	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	poa = PortableServer_Current_get_POA (poa_current, ev);

	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	obj = PortableServer_POA_servant_to_reference (poa, servant, ev);

	g_assert (ev->_major == CORBA_NO_EXCEPTION);
	g_assert (obj != CORBA_OBJECT_NIL);

	CORBA_Object_release ((CORBA_Object) obj, ev);
	CORBA_Object_release ((CORBA_Object) poa, ev);
	CORBA_Object_release ((CORBA_Object) poa_current, ev);
}

static POA_test_DeadReferenceObj__epv DeadReferenceObj_epv = {
	NULL,
	DeadReferenceObj_test
};

static PortableServer_ServantBase__epv DeadReferenceObj_base_epv = {
	NULL,
	simple_finalize,
	NULL
};
static POA_test_DeadReferenceObj__vepv DeadReferenceObj_vepv = {
	&DeadReferenceObj_base_epv,
	&DeadReferenceObj_epv
};
