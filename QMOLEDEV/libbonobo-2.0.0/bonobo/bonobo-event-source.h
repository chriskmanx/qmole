/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-event-source.h: Generic event emitter.
 *
 * Author:
 *	Alex Graveley (alex@ximian.com)
 *
 * Copyright (C) 2001, Ximian, Inc.
 */
#ifndef _BONOBO_EVENT_SOURCE_H_
#define _BONOBO_EVENT_SOURCE_H_

#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-listener.h>

G_BEGIN_DECLS

#define BONOBO_TYPE_EVENT_SOURCE        (bonobo_event_source_get_type ())
#define BONOBO_EVENT_SOURCE_TYPE        BONOBO_TYPE_EVENT_SOURCE /* deprecated, you should use BONOBO_TYPE_EVENT_SOURCE */
#define BONOBO_EVENT_SOURCE(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_EVENT_SOURCE, BonoboEventSource))
#define BONOBO_EVENT_SOURCE_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_EVENT_SOURCE, BonoboEventSourceClass))
#define BONOBO_IS_EVENT_SOURCE(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_EVENT_SOURCE))
#define BONOBO_IS_EVENT_SOURCE_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_EVENT_SOURCE))

typedef struct _BonoboEventSourcePrivate BonoboEventSourcePrivate;

typedef struct {
	BonoboObject             parent;
	BonoboEventSourcePrivate *priv;
} BonoboEventSource;

typedef struct {
	BonoboObjectClass parent_class;

	POA_Bonobo_EventSource__epv epv;
} BonoboEventSourceClass;

GType              bonobo_event_source_get_type         (void) G_GNUC_CONST;
BonoboEventSource *bonobo_event_source_new              (void);
gboolean           bonobo_event_source_has_listener     (BonoboEventSource *event_source,
							 const char        *event_name);
void               bonobo_event_source_notify_listeners (BonoboEventSource *event_source,
							 const char        *event_name,
							 const CORBA_any   *opt_value,
							 CORBA_Environment *opt_ev);

void        bonobo_event_source_notify_listeners_full   (BonoboEventSource *event_source,
							 const char        *path,
							 const char        *type,
							 const char        *subtype,
							 const CORBA_any   *opt_value,
							 CORBA_Environment *opt_ev);

void        bonobo_event_source_client_remove_listener  (Bonobo_Unknown     object,
							 Bonobo_Listener    listener,
							 CORBA_Environment *opt_ev);

void        bonobo_event_source_client_add_listener     (Bonobo_Unknown           object,
							 BonoboListenerCallbackFn event_callback,
							 const char              *opt_mask,
							 CORBA_Environment       *opt_ev,
							 gpointer                 user_data);


void        bonobo_event_source_client_add_listener_closure   (Bonobo_Unknown     object,
							       GClosure          *callback,
							       const char        *opt_mask,
							       CORBA_Environment *opt_ev);

Bonobo_Listener bonobo_event_source_client_add_listener_full  (Bonobo_Unknown     object,
							       GClosure          *callback,
							       const char        *opt_mask,
							       CORBA_Environment *opt_ev);

/* You don't want this routine */
void            bonobo_event_source_ignore_listeners        (BonoboEventSource *event_source);

G_END_DECLS

#endif /* _BONOBO_EVENT_SOURCE_H_ */

