/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-shlib-factory.c: a ShlibFactory object.
 *
 * The BonoboShlibFactory object is used to instantiate new
 * Bonobo::ShlibFactory objects.  It acts as a wrapper for the
 * Bonobo::ShlibFactory CORBA interface, and dispatches to
 * a user-specified factory function whenever its create_object()
 * method is invoked.
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 1999, 2001 Ximian, Inc.
 */
#include <config.h>
#include <glib-object.h>
#include <gobject/gmarshal.h>
#include <bonobo/Bonobo.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-shlib-factory.h>
#include <bonobo/bonobo-running-context.h>

static BonoboObjectClass *bonobo_shlib_factory_parent_class = NULL;

struct _BonoboShlibFactoryPrivate {
	int      live_objects;
	gpointer act_impl_ptr;
};

/**
 * bonobo_shlib_factory_construct:
 * @factory: The object to be initialized.
 * @act_iid: The GOAD id that the new factory will implement.
 * @act_impl_ptr: Activation shlib handle
 * @closure: The closure used to create new GnomeShlib object instances.
 *
 * Initializes @c_factory with the supplied data.
 *
 * Returns: The initialized BonoboShlibFactory object.
 */
BonoboShlibFactory *
bonobo_shlib_factory_construct (BonoboShlibFactory    *factory,
				const char            *act_iid,
				PortableServer_POA     poa,
				gpointer               act_impl_ptr,
				GClosure              *closure)
{
	g_return_val_if_fail (factory != NULL, NULL);
	g_return_val_if_fail (BONOBO_IS_SHLIB_FACTORY (factory), NULL);

	factory->priv->live_objects = 0;
	factory->priv->act_impl_ptr = act_impl_ptr;

        bonobo_activation_plugin_use (poa, act_impl_ptr);

	bonobo_generic_factory_construct_noreg (
		BONOBO_GENERIC_FACTORY (factory), act_iid, closure);

	return factory;
}

/**
 * bonobo_shlib_factory_new_closure:
 * @act_iid: The GOAD id that this factory implements
 * @poa: the poa.
 * @act_impl_ptr: Activation shlib handle
 * @factory_closure: A closure which is used to create new BonoboObject instances.
 *
 * This is a helper routine that simplifies the creation of factory
 * objects for GNOME objects.  The @factory_closure closure will be
 * invoked by the CORBA server when a request arrives to create a new
 * instance of an object supporting the Bonobo::Shlib interface.
 * The factory callback routine is passed the @data pointer to provide
 * the creation function with some state information.
 *
 * Returns: A BonoboShlibFactory object that has an activated
 * Bonobo::ShlibFactory object that has registered with the GNOME
 * name server.
 */
BonoboShlibFactory *
bonobo_shlib_factory_new_closure (const char           *act_iid,
				  PortableServer_POA    poa,
				  gpointer              act_impl_ptr,
				  GClosure             *factory_closure)
{
	BonoboShlibFactory *factory;

	g_return_val_if_fail (act_iid != NULL, NULL);
	g_return_val_if_fail (factory_closure != NULL, NULL);
	
	factory = g_object_new (bonobo_shlib_factory_get_type (), NULL);
	
	return bonobo_shlib_factory_construct (
		factory, act_iid, poa, act_impl_ptr, factory_closure);
}

/**
 * bonobo_shlib_factory_new:
 * @act_iid: The GOAD id that this factory implements
 * @poa: the poa.
 * @act_impl_ptr: Activation shlib handle
 * @factory_cb: A callback which is used to create new BonoboObject instances.
 * @user_data: The closure data to be passed to the @factory callback routine.
 *
 * This is a helper routine that simplifies the creation of factory
 * objects for GNOME objects.  The @factory function will be
 * invoked by the CORBA server when a request arrives to create a new
 * instance of an object supporting the Bonobo::Shlib interface.
 * The factory callback routine is passed the @data pointer to provide
 * the creation function with some state information.
 *
 * Returns: A BonoboShlibFactory object that has an activated
 * Bonobo::ShlibFactory object that has registered with the GNOME
 * name server.
 */
BonoboShlibFactory *
bonobo_shlib_factory_new (const char           *component_id,
			  PortableServer_POA    poa,
			  gpointer              act_impl_ptr,
			  BonoboFactoryCallback factory_cb,
			  gpointer              user_data)
{
	return bonobo_shlib_factory_new_closure (
		component_id, poa, act_impl_ptr,
		g_cclosure_new (G_CALLBACK (factory_cb), user_data, NULL));
}

/*
 * FIXME: at some time in the future we should look into trying
 * to unload the shlib's we have linked in with
 * bonobo_activation_plugin_unuse - this is dangerous though; we
 * need to be sure no code is using the shlib first.
 */
static void
bonobo_shlib_factory_finalize (GObject *object)
{
	BonoboShlibFactory *factory = BONOBO_SHLIB_FACTORY (object);

	/* act_plugin_unuse (c_factory->act_impl_ptr); */

	g_free (factory->priv);
	
	G_OBJECT_CLASS (bonobo_shlib_factory_parent_class)->finalize (object);
}

static BonoboObject *
bonobo_shlib_factory_new_generic (BonoboGenericFactory *factory,
				  const char           *act_iid)
{
	BonoboObject *retval;

	retval = BONOBO_GENERIC_FACTORY_CLASS (
		bonobo_shlib_factory_parent_class)->new_generic (factory, act_iid);

	/* The factory reference doesn't persist inside bonobo-activation */
	bonobo_object_unref (BONOBO_OBJECT (factory));

	return retval;
}

static void
bonobo_shlib_factory_class_init (BonoboGenericFactoryClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;

	bonobo_shlib_factory_parent_class = g_type_class_peek_parent (klass);

	klass->new_generic = bonobo_shlib_factory_new_generic;

	object_class->finalize = bonobo_shlib_factory_finalize;
}

static void
bonobo_shlib_factory_init (GObject *object)
{
	BonoboShlibFactory *factory = BONOBO_SHLIB_FACTORY (object);

	factory->priv = g_new0 (BonoboShlibFactoryPrivate, 1);
}

/**
 * bonobo_shlib_factory_get_type:
 *
 * Returns: The GType of the BonoboShlibFactory class.
 */
BONOBO_TYPE_FUNC (BonoboShlibFactory, 
		  BONOBO_TYPE_GENERIC_FACTORY,
		  bonobo_shlib_factory);

/**
 * bonobo_shlib_factory_std:
 * @component_id:
 * @poa:
 * @act_impl_ptr:
 * @factory_cb:
 * @user_data:
 * @ev:
 * 
 *    A Generic std shlib routine so we don't stick a load of code
 * inside a public macro.
 * 
 * Return value: 0 on success, 1 on failure.
 **/
Bonobo_Unknown
bonobo_shlib_factory_std (const char            *component_id,
			  PortableServer_POA     poa,
			  gpointer               act_impl_ptr,
			  BonoboFactoryCallback  factory_cb,
			  gpointer               user_data,
			  CORBA_Environment     *ev)
{
	BonoboShlibFactory *f;

	f = bonobo_shlib_factory_new (
		component_id, poa,
		act_impl_ptr,
		factory_cb, user_data);

        return CORBA_Object_duplicate (BONOBO_OBJREF (f), ev);
}

