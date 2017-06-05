/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-shlib-factory.h: a ShlibFactory object.
 *
 * Author:
 *    Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000, 2001 Ximian, Inc.
 */
#ifndef _BONOBO_SHLIB_FACTORY_H_
#define _BONOBO_SHLIB_FACTORY_H_


#include <glib-object.h>
#include <bonobo/Bonobo.h>
#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-generic-factory.h>
#include <bonobo/bonobo-exception.h>

G_BEGIN_DECLS
 
#define BONOBO_TYPE_SHLIB_FACTORY        (bonobo_shlib_factory_get_type ())
#define BONOBO_SHLIB_FACTORY_TYPE        BONOBO_TYPE_SHLIB_FACTORY /* deprecated, you should use BONOBO_TYPE_SHLIB_FACTORY */
#define BONOBO_SHLIB_FACTORY(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_SHLIB_FACTORY, BonoboShlibFactory))
#define BONOBO_SHLIB_FACTORY_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_SHLIB_FACTORY, BonoboShlibFactoryClass))
#define BONOBO_IS_SHLIB_FACTORY(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_SHLIB_FACTORY))
#define BONOBO_IS_SHLIB_FACTORY_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_SHLIB_FACTORY))

typedef struct _BonoboShlibFactoryPrivate BonoboShlibFactoryPrivate;
					
typedef struct {
	BonoboGenericFactory base;

	BonoboShlibFactoryPrivate *priv;
} BonoboShlibFactory;

typedef struct {
	BonoboGenericFactoryClass parent_class;
} BonoboShlibFactoryClass;

GType               bonobo_shlib_factory_get_type     (void) G_GNUC_CONST;

BonoboShlibFactory *bonobo_shlib_factory_construct    (BonoboShlibFactory    *factory,
						       const char            *component_id,
						       PortableServer_POA     poa,
						       gpointer               act_impl_ptr,
						       GClosure              *closure);

BonoboShlibFactory *bonobo_shlib_factory_new          (const char            *component_id,
						       PortableServer_POA     poa,
						       gpointer               act_impl_ptr,
						       BonoboFactoryCallback  factory_cb,
						       gpointer               user_data);

BonoboShlibFactory *bonobo_shlib_factory_new_closure  (const char            *component_id,
						       PortableServer_POA     poa,
						       gpointer               act_impl_ptr,
						       GClosure              *factory_closure);

Bonobo_Unknown      bonobo_shlib_factory_std          (const char            *component_id,
						       PortableServer_POA     poa,
						       gpointer               act_impl_ptr,
						       BonoboFactoryCallback  factory_cb,
						       gpointer               user_data,
						       CORBA_Environment     *ev);

#define BONOBO_OAF_SHLIB_FACTORY(oafiid, descr, fn, data)                     \
	BONOBO_ACTIVATION_SHLIB_FACTORY(oafiid, descr, fn, data)
#define BONOBO_OAF_SHLIB_FACTORY_MULTI(oafiid, descr, fn, data)               \
	BONOBO_ACTIVATION_SHLIB_FACTORY(oafiid, descr, fn, data)

#define BONOBO_ACTIVATION_SHLIB_FACTORY(oafiid, descr, fn, data)	      \
static Bonobo_Unknown                                                         \
make_factory (PortableServer_POA poa, const char *iid, gpointer impl_ptr,     \
	      CORBA_Environment *ev)                                          \
{                                                                             \
	return bonobo_shlib_factory_std (oafiid, poa, impl_ptr, fn, data, ev);\
}                                                                             \
static BonoboActivationPluginObject plugin_list[] = {{oafiid, make_factory}, { NULL } };   \
const  BonoboActivationPlugin Bonobo_Plugin_info = { plugin_list, descr };

G_END_DECLS

#endif
