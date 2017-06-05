/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-moniker-extender: extending monikers
 *
 * Author:
 *	Dietmar Maurer (dietmar@maurer-it.com)
 *
 * Copyright 2000, Ximian, Inc.
 */
#include <config.h>
  
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker.h>
#include <bonobo/bonobo-moniker-util.h>
#include <bonobo/bonobo-moniker-extender.h>

#define PARENT_TYPE BONOBO_TYPE_OBJECT

static GObjectClass *bonobo_moniker_extender_parent_class;

#define CLASS(o) BONOBO_MONIKER_EXTENDER_CLASS (G_OBJECT_GET_CLASS (o))

static inline BonoboMonikerExtender *
bonobo_moniker_extender_from_servant (PortableServer_Servant servant)
{
	return BONOBO_MONIKER_EXTENDER (bonobo_object_from_servant (servant));
}

static Bonobo_Unknown 
impl_Bonobo_MonikerExtender_resolve (PortableServer_Servant servant,
				     const Bonobo_Moniker   parent,
				     const Bonobo_ResolveOptions *options,
				     const CORBA_char      *display_name,
				     const CORBA_char      *requested_interface,
				     CORBA_Environment     *ev)
{
	BonoboMonikerExtender *extender = bonobo_moniker_extender_from_servant (servant);

	if (extender->resolve)
		return extender->resolve (extender, parent, options, display_name,
					  requested_interface, ev);
	else
		return CLASS (extender)->resolve (extender, parent, options, display_name,
						  requested_interface, ev);
}

static Bonobo_Unknown
bonobo_moniker_extender_resolve (BonoboMonikerExtender *extender,
				 const Bonobo_Moniker   parent,
				 const Bonobo_ResolveOptions *options,
				 const CORBA_char      *display_name,
				 const CORBA_char      *requested_interface,
				 CORBA_Environment     *ev)
{

	CORBA_exception_set (ev, CORBA_USER_EXCEPTION, 
			     ex_Bonobo_Moniker_InterfaceNotFound, NULL);

	return CORBA_OBJECT_NIL;
}

static void
bonobo_moniker_extender_finalize (GObject *object)
{
	bonobo_moniker_extender_parent_class->finalize (object);
}

static void
bonobo_moniker_extender_class_init (BonoboMonikerExtenderClass *klass)
{
	GObjectClass *oclass = (GObjectClass *)klass;
	POA_Bonobo_MonikerExtender__epv *epv = &klass->epv;

	bonobo_moniker_extender_parent_class = g_type_class_peek_parent (klass);

	oclass->finalize = bonobo_moniker_extender_finalize;

	klass->resolve = bonobo_moniker_extender_resolve;

	epv->resolve = impl_Bonobo_MonikerExtender_resolve;
}

static void
bonobo_moniker_extender_init (GObject *object)
{
	/* nothing to do */
}

BONOBO_TYPE_FUNC_FULL (BonoboMonikerExtender, 
		       Bonobo_MonikerExtender,
		       PARENT_TYPE,
		       bonobo_moniker_extender);

/**
 * bonobo_moniker_extender_new:
 * @resolve: the resolve function that will be used to do the extension
 * @data: user data to be passed back to the resolve function.
 * 
 * This creates a new moniker extender.
 * 
 * Return value: the extender object
 **/
BonoboMonikerExtender *
bonobo_moniker_extender_new (BonoboMonikerExtenderFn resolve, gpointer data)
{
	BonoboMonikerExtender *extender = NULL;
	
	extender = g_object_new (bonobo_moniker_extender_get_type (), NULL);

	extender->resolve = resolve;
	extender->data = data;

	return extender;
}

/**
 * bonobo_moniker_find_extender:
 * @name: the name of the moniker we want to extend eg. 'file:'
 * @interface: the interface we want to resolve to
 * @opt_ev: an optional corba exception environment.
 * 
 *  This routine tries to locate an extender for our moniker
 * by examining a registry of extenders that map new interfaces
 * to certain moniker names.
 * 
 * Return value: an appropriate extender or CORBA_OBJECT_NIL.
 **/
Bonobo_MonikerExtender
bonobo_moniker_find_extender (const gchar       *name, 
			      const gchar       *interface, 
			      CORBA_Environment *opt_ev)
{
	gchar            *query;
	Bonobo_ActivationID  ret_id;
	Bonobo_Unknown    extender;
	CORBA_Environment  *ev, temp_ev;

	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	query = g_strdup_printf (
		"repo_ids.has ('IDL:Bonobo/MonikerExtender:1.0') AND "
		"repo_ids.has ('%s') AND "
		"bonobo:moniker_extender.has ('%s')", interface, name);

	extender = bonobo_activation_activate (query, NULL, 0, &ret_id, ev);

	g_free (query);

	if (!opt_ev)
		CORBA_exception_free (&temp_ev);

	return extender;
}

/**
 * bonobo_moniker_use_extender:
 * @extender_oafiid: The IID of the extender to use
 * @moniker: the moniker to extend
 * @options: resolve options
 * @requested_interface: the requested interface
 * @opt_ev: optional corba environment
 * 
 *  Locates a known extender via. OAFIID; eg.
 * OAFIID:Bonobo_Moniker_Extender_file
 * 
 * Return value: the resolved result or CORBA_OBJECT_NIL.
 **/
Bonobo_Unknown
bonobo_moniker_use_extender (const gchar                 *extender_oafiid,
			     BonoboMoniker               *moniker,
			     const Bonobo_ResolveOptions *options,
			     const CORBA_char            *requested_interface,
			     CORBA_Environment           *opt_ev)
{
	Bonobo_MonikerExtender extender;
	Bonobo_Unknown         retval;
	CORBA_Environment  *ev, temp_ev;

	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	g_return_val_if_fail (ev != NULL, CORBA_OBJECT_NIL);
	g_return_val_if_fail (options != NULL, CORBA_OBJECT_NIL);
	g_return_val_if_fail (moniker != NULL, CORBA_OBJECT_NIL);
	g_return_val_if_fail (extender_oafiid != NULL, CORBA_OBJECT_NIL);
	g_return_val_if_fail (requested_interface != NULL, CORBA_OBJECT_NIL);

	extender = bonobo_activation_activate_from_id (
		(gchar *) extender_oafiid, 0, NULL, ev);

	if (BONOBO_EX (ev) || extender == CORBA_OBJECT_NIL)
		return CORBA_OBJECT_NIL;

	retval = Bonobo_MonikerExtender_resolve (extender, 
	        BONOBO_OBJREF (moniker), options, 
		bonobo_moniker_get_name_full (moniker),
		requested_interface, ev);

	bonobo_object_release_unref (extender, ev);

	if (!opt_ev)
		CORBA_exception_free (&temp_ev);

	return retval;
}
