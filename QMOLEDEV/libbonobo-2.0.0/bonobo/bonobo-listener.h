/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-listener.h: Generic listener interface for callbacks.
 *
 * Authors:
 *	Alex Graveley (alex@helixcode.com)
 *	Mike Kestner  (mkestner@ameritech.net)
 *
 * Copyright (C) 2000, Helix Code, Inc.
 */
#ifndef _BONOBO_LISTENER_H_
#define _BONOBO_LISTENER_H_

#include <bonobo/bonobo-arg.h>
#include <bonobo/bonobo-object.h>

G_BEGIN_DECLS

#define BONOBO_TYPE_LISTENER        (bonobo_listener_get_type ())
#define BONOBO_LISTENER_TYPE        BONOBO_TYPE_LISTENER /* deprecated, you should use BONOBO_TYPE_LISTENER */
#define BONOBO_LISTENER(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_LISTENER, BonoboListener))
#define BONOBO_LISTENER_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_LISTENER, BonoboListenerClass))
#define BONOBO_IS_LISTENER(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_LISTENER))
#define BONOBO_IS_LISTENER_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_LISTENER))

typedef struct _BonoboListenerPrivate BonoboListenerPrivate;

typedef struct {
        BonoboObject          parent;

	BonoboListenerPrivate *priv;
} BonoboListener;

typedef struct {
	BonoboObjectClass     parent_class;

	POA_Bonobo_Listener__epv epv;

	/* Signals */
	void (* event_notify) (BonoboListener    *listener, 
			       char              *event_name,
			       BonoboArg         *event_data, 
			       CORBA_Environment *ev);
} BonoboListenerClass;


typedef void (*BonoboListenerCallbackFn)    (BonoboListener    *listener,
					     const char        *event_name, 
					     const CORBA_any   *any,
					     CORBA_Environment *ev,
					     gpointer           user_data);

GType           bonobo_listener_get_type    (void) G_GNUC_CONST;

BonoboListener *bonobo_listener_new         (BonoboListenerCallbackFn event_cb,
					     gpointer                 user_data);

BonoboListener *bonobo_listener_new_closure (GClosure                *event_closure);

char           *bonobo_event_make_name      (const char *idl_path, 
					     const char *kind,
					     const char *subtype);

char           *bonobo_event_type           (const char *event_name);
char           *bonobo_event_subtype        (const char *event_name);
char           *bonobo_event_kind           (const char *event_name);
char           *bonobo_event_idl_path       (const char *event_name);

G_END_DECLS

#endif /* _BONOBO_LISTENER_H_ */

