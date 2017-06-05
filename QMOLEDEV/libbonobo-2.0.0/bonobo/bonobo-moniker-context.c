/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-moniker-context.c: A global moniker interface
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 *
 * Copyright (C) 2000, Ximian, Inc.
 */
#include <config.h>
#include <glib-object.h>

#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker-util.h>
#include <bonobo/bonobo-moniker-extender.h>
#include <bonobo/bonobo-moniker-context.h>

#define PARENT_TYPE BONOBO_TYPE_OBJECT

static Bonobo_Moniker
impl_Bonobo_MonikerContext_createFromName (PortableServer_Servant servant,
					      const CORBA_char      *name,
					      CORBA_Environment     *ev)
{
	return bonobo_moniker_client_new_from_name (name, ev);
}

static Bonobo_Unknown
impl_Bonobo_MonikerContext_getObject (PortableServer_Servant servant,
					 const CORBA_char      *name,
					 const CORBA_char      *repo_id,
					 CORBA_Environment     *ev)
{
	return bonobo_get_object (name, repo_id, ev);
}

static Bonobo_MonikerExtender
impl_Bonobo_MonikerContext_getExtender (PortableServer_Servant servant,
					   const CORBA_char      *monikerPrefix,
					   const CORBA_char      *interfaceId,
					   CORBA_Environment     *ev)
{
	return bonobo_moniker_find_extender (monikerPrefix, interfaceId, ev);
}

static void
bonobo_moniker_context_class_init (BonoboMonikerContextClass *klass)
{
	POA_Bonobo_MonikerContext__epv *epv = &klass->epv;

	epv->getObject        = impl_Bonobo_MonikerContext_getObject;
	epv->createFromName   = impl_Bonobo_MonikerContext_createFromName;
	epv->getExtender      = impl_Bonobo_MonikerContext_getExtender;
}

static void 
bonobo_moniker_context_init (GObject *object)
{
	/* nothing to do */
}

static
BONOBO_TYPE_FUNC_FULL (BonoboMonikerContext,
		       Bonobo_MonikerContext,
		       PARENT_TYPE,
		       bonobo_moniker_context);

BonoboObject *
bonobo_moniker_context_new (void)
{
        return BONOBO_OBJECT (g_object_new (
		bonobo_moniker_context_get_type (), NULL));
}
