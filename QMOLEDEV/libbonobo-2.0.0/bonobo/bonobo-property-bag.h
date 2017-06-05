/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-property-bag.h: property bag object implementation.
 *
 * Authors:
 *   Nat Friedman   (nat@ximian.com)
 *   Michael Meeks  (michael@ximian.com)
 *   Dietmar Maurer (dietmar@ximian.com)
 *
 * Copyright 2001 Ximian, Inc.
 */
#ifndef __BONOBO_PROPERTY_BAG_H__
#define __BONOBO_PROPERTY_BAG_H__

#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-arg.h>
#include <bonobo/bonobo-event-source.h>

G_BEGIN_DECLS

#define BONOBO_PROPERTY_READABLE      Bonobo_PROPERTY_READABLE
#define BONOBO_PROPERTY_WRITEABLE     Bonobo_PROPERTY_WRITEABLE
#define BONOBO_PROPERTY_WRITABLE      Bonobo_PROPERTY_WRITEABLE
#define BONOBO_PROPERTY_NO_LISTENING  Bonobo_PROPERTY_NO_LISTENING 
#define BONOBO_PROPERTY_NO_AUTONOTIFY Bonobo_PROPERTY_NO_AUTONOTIFY 

typedef struct _BonoboPropertyBagPrivate BonoboPropertyBagPrivate;
typedef struct _BonoboPropertyBag        BonoboPropertyBag;

typedef struct _BonoboProperty           BonoboProperty;
typedef struct _BonoboPropertyPrivate    BonoboPropertyPrivate;

typedef void (*BonoboPropertyGetFn) (BonoboPropertyBag *bag,
				     BonoboArg         *arg,
				     guint              arg_id,
				     CORBA_Environment *ev,
				     gpointer           user_data);
typedef void (*BonoboPropertySetFn) (BonoboPropertyBag *bag,
				     const BonoboArg   *arg,
				     guint              arg_id,
				     CORBA_Environment *ev,
				     gpointer           user_data);

struct _BonoboProperty {
	char		      *name;
	int                    idx;
	BonoboArgType          type;
	BonoboArg             *default_value;
	char		      *doctitle;
	char		      *docstring;
	Bonobo_PropertyFlags   flags;

	BonoboPropertyPrivate *priv;
};

struct _BonoboPropertyBag {
	BonoboObject             parent;
	BonoboPropertyBagPrivate *priv;
	BonoboEventSource        *es;
};

typedef struct {
	BonoboObjectClass        parent;

	POA_Bonobo_PropertyBag__epv epv;
} BonoboPropertyBagClass;

#define BONOBO_TYPE_PROPERTY_BAG        (bonobo_property_bag_get_type ())
#define BONOBO_PROPERTY_BAG_TYPE        BONOBO_TYPE_PROPERTY_BAG /* deprecated, you should use BONOBO_TYPE_PROPERTY_BAG */
#define BONOBO_PROPERTY_BAG(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_PROPERTY_BAG, BonoboPropertyBag))
#define BONOBO_PROPERTY_BAG_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_PROPERTY_BAG, BonoboPropertyBagClass))
#define BONOBO_IS_PROPERTY_BAG(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_PROPERTY_BAG))
#define BONOBO_IS_PROPERTY_BAG_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_PROPERTY_BAG))

GType		          
bonobo_property_bag_get_type  (void) G_GNUC_CONST;

BonoboPropertyBag *
bonobo_property_bag_new	           (BonoboPropertyGetFn get_prop_cb,
			            BonoboPropertySetFn set_prop_cb,
			            gpointer            user_data);

BonoboPropertyBag *
bonobo_property_bag_new_closure   (GClosure          *get_prop,
				   GClosure          *set_prop);

BonoboPropertyBag *
bonobo_property_bag_new_full      (GClosure          *get_prop,
				   GClosure          *set_prop,
				   BonoboEventSource *event_source);

BonoboPropertyBag *
bonobo_property_bag_construct     (BonoboPropertyBag *pb,
				   GClosure          *get_prop,
				   GClosure          *set_prop,
				   BonoboEventSource *event_source);

void                      
bonobo_property_bag_add           (BonoboPropertyBag   *pb,
				   const char          *name,
				   int                  idx,
				   BonoboArgType        type,
				   BonoboArg           *default_value,
				   const char          *doctitle,
				   Bonobo_PropertyFlags flags);

void
bonobo_property_bag_add_full      (BonoboPropertyBag    *pb,
				   const char           *name,
				   int                   idx,
				   BonoboArgType         type,
				   BonoboArg            *default_value,
				   const char           *doctitle,
				   const char           *docstring,
				   Bonobo_PropertyFlags  flags,
				   GClosure             *get_prop,
				   GClosure             *set_prop);

void
bonobo_property_bag_remove        (BonoboPropertyBag    *pb,
				   const char           *name);

void
bonobo_property_bag_map_params    (BonoboPropertyBag   *pb,
				   GObject             *on_instance,
				   const GParamSpec   **pspecs,
				   guint                n_params);

GList *
bonobo_property_bag_get_prop_list (BonoboPropertyBag *pb);

G_END_DECLS

#endif /* ! __BONOBO_PROPERTY_BAG_H__ */
