#include <config.h>
#include <orbit/poa/poa.h>

#include "poa-macros.h"

PortableServer_POA
PortableServer_ServantBase__default_POA (PortableServer_Servant  serv,
					 CORBA_Environment      *ev)
{
	PortableServer_ServantBase *servant = (PortableServer_ServantBase *) serv;

	poa_sys_exception_val_if_fail (servant != NULL, ex_CORBA_BAD_PARAM, NULL);
	poa_sys_exception_val_if_fail (servant->_private != NULL, ex_CORBA_BAD_PARAM, NULL);

	return ((ORBit_POAObject) servant->_private)->poa;
}

CORBA_InterfaceDef
PortableServer_ServantBase__get_interface (PortableServer_Servant  servant,
					   CORBA_Environment      *ev)
{
	return CORBA_OBJECT_NIL;
}

CORBA_boolean
PortableServer_ServantBase__is_a (PortableServer_Servant  servant,
				  const CORBA_char       *logical_type_id,
				  CORBA_Environment      *ev)
{
	poa_sys_exception_val_if_fail (servant != NULL, ex_CORBA_BAD_PARAM, FALSE);

	/*
	 * FIXME: actually implement this.
	 */

	return FALSE;
}

void
PortableServer_ServantBase__add_ref (PortableServer_Servant  servant,
				     CORBA_Environment      *ev)
{
}

void
PortableServer_ServantBase__remove_ref (PortableServer_Servant  servant,
					CORBA_Environment      *ev)
{
}

void
PortableServer_RefCountServantBase__add_ref (PortableServer_Servant  servant,
					     CORBA_Environment      *ev)
{
	poa_sys_exception_if_fail (servant != NULL, ex_CORBA_BAD_PARAM);

	/*
	 * FIXME: actually implement this.
	 */
}

void
PortableServer_RefCountServantBase__remove_ref (PortableServer_Servant  servant,
						CORBA_Environment      *ev)
{
	poa_sys_exception_if_fail (servant != NULL, ex_CORBA_BAD_PARAM);

	/*
	 * FIXME: actually implement this.
	 */
}

void
PortableServer_ServantBase__init (PortableServer_Servant  servant,
				  CORBA_Environment      *ev)
{
	PortableServer_ServantBase *servantbase = (PortableServer_ServantBase *) servant;

	poa_sys_exception_if_fail (servantbase != NULL, ex_CORBA_BAD_PARAM);
	poa_sys_exception_if_fail (servantbase->vepv && servantbase->vepv [0], ex_CORBA_BAD_PARAM);

	if (!servantbase->vepv[0]->finalize)
		servantbase->vepv[0]->finalize =
			PortableServer_ServantBase__fini;

	if (!servantbase->vepv[0]->default_POA)
		servantbase->vepv[0]->default_POA =
			PortableServer_ServantBase__default_POA;

	if (!servantbase->vepv[0]->get_interface)
		servantbase->vepv[0]->get_interface =
			PortableServer_ServantBase__get_interface;

	if (!servantbase->vepv[0]->is_a)
		servantbase->vepv[0]->is_a =
			PortableServer_ServantBase__is_a;

	if (!servantbase->vepv[0]->non_existent)
		servantbase->vepv[0]->add_ref =
			PortableServer_ServantBase__add_ref;

	if (!servantbase->vepv[0]->add_ref)
		servantbase->vepv[0]-> add_ref =
			PortableServer_ServantBase__add_ref;

	if (!servantbase->vepv[0]->remove_ref)
		servantbase->vepv[0]->remove_ref =
			PortableServer_ServantBase__remove_ref;
}

void
PortableServer_ServantBase__fini (PortableServer_Servant  servant,
				  CORBA_Environment      *ev)
{
	poa_sys_exception_if_fail (servant != NULL, ex_CORBA_BAD_PARAM);
}

void
PortableServer_RefCountServantBase__init (PortableServer_Servant  servant,
					  CORBA_Environment      *ev)
{
	PortableServer_ServantBase *servantbase = (PortableServer_ServantBase *) servant;

	poa_sys_exception_if_fail (servantbase != NULL, ex_CORBA_BAD_PARAM);
	poa_sys_exception_if_fail (servantbase->vepv && servantbase->vepv [0], ex_CORBA_BAD_PARAM);

	if (!servantbase->vepv[0]->finalize)
		servantbase->vepv[0]->finalize =
			PortableServer_RefCountServantBase__fini;

	if (!servantbase->vepv[0]->add_ref)
		servantbase->vepv[0]->add_ref = 
			PortableServer_RefCountServantBase__add_ref;

	if (!servantbase->vepv[0]->remove_ref)
		servantbase->vepv[0]->remove_ref =
			PortableServer_RefCountServantBase__remove_ref;

	PortableServer_ServantBase__init (servant, ev);
}

void
PortableServer_RefCountServantBase__fini (PortableServer_Servant  servant,
					  CORBA_Environment      *ev)
{
	poa_sys_exception_if_fail (servant != NULL, ex_CORBA_BAD_PARAM);

	PortableServer_RefCountServantBase__fini (servant, ev);
}
