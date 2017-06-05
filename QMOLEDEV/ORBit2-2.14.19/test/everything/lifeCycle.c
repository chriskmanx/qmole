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
LifeCycleServer_deactivateOnReturn (PortableServer_Servant  servant,
				    CORBA_Environment      *ev)
{
	PortableServer_ObjectId *oid;

	oid = PortableServer_POA_servant_to_id (global_poa, servant, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	PortableServer_POA_deactivate_object (global_poa, oid, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_free (oid);
}

static void
LifeCycleServer_deactivateUnrefOnReturn (PortableServer_Servant  servant,
					 CORBA_Environment      *ev)
{
	CORBA_Object self_ref;

	/* Will only 'work' in-proc */
	PortableServer_ObjectId *oid;

	oid = PortableServer_POA_servant_to_id (global_poa, servant, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	PortableServer_POA_deactivate_object (global_poa, oid, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_free (oid);

	self_ref = PortableServer_POA_servant_to_reference (global_poa, servant, ev);
	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	CORBA_Object_release (self_ref, ev);
	CORBA_Object_release (self_ref, ev);
}

static POA_test_LifeCycleServer__epv LifeCycleServer_epv = {
	NULL,
	LifeCycleServer_deactivateOnReturn,
	LifeCycleServer_deactivateUnrefOnReturn
};

static PortableServer_ServantBase__epv LifeCycleServer_base_epv = {
	NULL,
	simple_finalize,
	NULL
};
static POA_test_LifeCycleServer__vepv LifeCycleServer_vepv = {
	&LifeCycleServer_base_epv,
	&LifeCycleServer_epv
};
