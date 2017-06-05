/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-event-source.c: Generic event emitter.
 *
 * Author:
 *	Alex Graveley (alex@ximian.com)
 *	Iain Holmes   (iain@ximian.com)
 *      docs, Miguel de Icaza (miguel@ximian.com)
 *
 * Copyright (C) 2001, Ximian, Inc.
 */
#include <config.h>
#include <time.h>
#include <string.h>
#include <glib-object.h>
#include <bonobo/bonobo-listener.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-event-source.h>
#include <bonobo/bonobo-running-context.h>

#define PARENT_TYPE BONOBO_TYPE_OBJECT

static GObjectClass *bonobo_event_source_parent_class;

struct _BonoboEventSourcePrivate {
	GSList  *listeners;  /* CONTAINS: ListenerDesc* */
	gboolean ignore;
	gint     counter;    /* to create unique listener Ids */
};

typedef struct {
	Bonobo_Listener listener;
	gchar         **event_masks; /* send all events if NULL */
} ListenerDesc;

static inline BonoboEventSource * 
bonobo_event_source_from_servant (PortableServer_Servant servant)
{
	return BONOBO_EVENT_SOURCE (bonobo_object_from_servant (servant));
}

static void
desc_free (ListenerDesc *desc, CORBA_Environment *ev)
{
	if (desc) {
		g_strfreev (desc->event_masks);
		bonobo_object_release_unref (desc->listener, ev);
		g_free (desc);
	}
}

static void
impl_Bonobo_EventSource_addListenerWithMask (PortableServer_Servant servant,
					     const Bonobo_Listener  l,
					     const CORBA_char      *event_mask,
					     CORBA_Environment     *ev)
{
	BonoboEventSource *event_source;
	ListenerDesc      *desc;

	g_return_if_fail (l != CORBA_OBJECT_NIL);

	event_source = bonobo_event_source_from_servant (servant);

	if (event_source->priv->ignore) /* Hook for running context */
		bonobo_running_context_ignore_object (l);

	desc = g_new0 (ListenerDesc, 1);
	desc->listener = bonobo_object_dup_ref (l, ev);

	if (event_mask)
		desc->event_masks = g_strsplit (event_mask, ",", 0);

	event_source->priv->listeners = g_slist_prepend (
		event_source->priv->listeners, desc);
}

static void
impl_Bonobo_EventSource_addListener (PortableServer_Servant servant,
				     const Bonobo_Listener  l,
				     CORBA_Environment     *ev)
{
	impl_Bonobo_EventSource_addListenerWithMask (servant, l, NULL, ev);
}

static void
impl_Bonobo_EventSource_removeListener (PortableServer_Servant servant,
					const Bonobo_Listener  listener,
					CORBA_Environment     *ev)
{
	GSList                   *l, *next;
	BonoboEventSourcePrivate *priv;

	priv = bonobo_event_source_from_servant (servant)->priv;

	for (l = priv->listeners; l; l = next) {
		ListenerDesc *desc = l->data;

		next = l->next;

		if (CORBA_Object_is_equivalent (listener, desc->listener, ev)) {
			priv->listeners = g_slist_remove_link (
				priv->listeners, l);
			g_slist_free_1 (l);
			desc_free (desc, ev);
			return;
		}
	}

	CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			     ex_Bonobo_EventSource_UnknownListener, 
			     NULL);
}

/*
 * if the mask starts with a '=', we do exact compares - else we only check 
 * if the mask is a prefix of name.
 */
static gboolean
event_match (const char *name, gchar **event_masks)
{
	int i = 0, j = 0;

	while (event_masks[j]) {
		char *mask = event_masks[j];
		
		if (mask [0] == '=')
			if (!strcmp (name, mask + 1))
				return TRUE;

		while (name [i] && mask [i] && name [i] == mask [i])
			i++;
		
		if (mask [i] == '\0')
			return TRUE;

		j++;
	}
	
	return FALSE;
} 

/**
 * bonobo_event_source_has_listener:
 * @event_source: the Event Source that will emit the event.
 * @event_name: Name of the event being emitted
 * 
 *   This method determines if there are any listeners for
 * the event to be broadcast. This can be used to detect
 * whether it is worth constructing a potentialy expensive
 * state update, before sending it to no-one.
 * 
 * Return value: TRUE if it's worth sending, else FALSE
 **/
gboolean
bonobo_event_source_has_listener (BonoboEventSource *event_source,
				  const char        *event_name)
{
	GSList  *l;
	gboolean notify;

	g_return_val_if_fail (BONOBO_IS_EVENT_SOURCE (event_source), FALSE);

	notify = FALSE;
	for (l = event_source->priv->listeners; l; l = l->next) {
		ListenerDesc *desc = (ListenerDesc *) l->data;

		if (desc->event_masks == NULL || 
		    event_match (event_name, desc->event_masks)) {
			notify = TRUE;
			break;
		}
	}

	return notify;
}

/**
 * bonobo_event_source_notify_listeners:
 * @event_source: the Event Source that will emit the event.
 * @event_name: Name of the event being emitted
 * @opt_value: A CORBA_any value that contains the data that is passed
 * to interested clients, or NULL for an empty value
 * @opt_ev: A CORBA_Environment where a failure code can be returned, can be NULL.
 *
 * This will notify all clients that have registered with this EventSource
 * (through the addListener or addListenerWithMask methods) of the availability
 * of the event named @event_name.  The @value CORBA::any value is passed to
 * all listeners.
 *
 * @event_name can not contain comma separators, as commas are used to
 * separate the various event names. 
 */
void
bonobo_event_source_notify_listeners (BonoboEventSource *event_source,
				      const char        *event_name,
				      const CORBA_any   *opt_value,
				      CORBA_Environment *opt_ev)
{
	GSList *l, *notify;
	CORBA_Environment  *ev, temp_ev;
	const BonoboArg *my_value;

	g_return_if_fail (BONOBO_IS_EVENT_SOURCE (event_source));
	
	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	if (!opt_value)
		my_value = bonobo_arg_new (BONOBO_ARG_NULL);
	else
		my_value = opt_value;
	
	notify = NULL;

	for (l = event_source->priv->listeners; l; l = l->next) {
		ListenerDesc *desc = (ListenerDesc *) l->data;

		if (desc->event_masks == NULL || 
		    event_match (event_name, desc->event_masks)) {
			notify = g_slist_prepend (
				notify,
				CORBA_Object_duplicate (desc->listener, ev));
		}
	}

	bonobo_object_ref (BONOBO_OBJECT (event_source));

	for (l = notify; l; l = l->next) {
		Bonobo_Listener_event (l->data, event_name, my_value, ev);
		CORBA_Object_release (l->data, ev);
	}

	bonobo_object_unref (BONOBO_OBJECT (event_source));

	g_slist_free (notify);

	if (!opt_ev)
		CORBA_exception_free (ev);

	if (!opt_value)
		bonobo_arg_release ((BonoboArg *) my_value);
}

void
bonobo_event_source_notify_listeners_full (BonoboEventSource *event_source,
					   const char        *path,
					   const char        *type,
					   const char        *subtype,
					   const CORBA_any   *opt_value,
					   CORBA_Environment *opt_ev)
{
	char *event_name;

	event_name = bonobo_event_make_name (path, type, subtype);

	bonobo_event_source_notify_listeners (event_source, event_name,
					      opt_value, opt_ev);

	g_free (event_name);
}

static void
bonobo_event_source_destroy (BonoboObject *object)
{
	CORBA_Environment         ev;
	BonoboEventSourcePrivate *priv = BONOBO_EVENT_SOURCE (object)->priv;
	
	CORBA_exception_init (&ev);

	while (priv->listeners) {
		ListenerDesc *d = priv->listeners->data;

		priv->listeners = g_slist_remove (priv->listeners, d);

		desc_free (d, &ev);
	}
	
	CORBA_exception_free (&ev);

	((BonoboObjectClass *)bonobo_event_source_parent_class)->destroy (object);
}

static void
bonobo_event_source_finalize (GObject *object)
{
	BonoboEventSourcePrivate *priv;
	
	priv = BONOBO_EVENT_SOURCE (object)->priv;

	/* in case of strange re-enterancy */
	bonobo_event_source_destroy (BONOBO_OBJECT (object));

	g_free (priv);

	bonobo_event_source_parent_class->finalize (object);
}

static void
bonobo_event_source_class_init (BonoboEventSourceClass *klass)
{
	GObjectClass *oclass = (GObjectClass *) klass;
	BonoboObjectClass *boclass = (BonoboObjectClass *) klass;
	POA_Bonobo_EventSource__epv *epv = &klass->epv;

	bonobo_event_source_parent_class = g_type_class_peek_parent (klass);

	oclass->finalize = bonobo_event_source_finalize;
	boclass->destroy = bonobo_event_source_destroy;

	epv->addListener         = impl_Bonobo_EventSource_addListener;
	epv->addListenerWithMask = impl_Bonobo_EventSource_addListenerWithMask;
	epv->removeListener      = impl_Bonobo_EventSource_removeListener;
}

static void
bonobo_event_source_init (GObject *object)
{
	BonoboEventSource *event_source;

	event_source = BONOBO_EVENT_SOURCE (object);
	event_source->priv = g_new0 (BonoboEventSourcePrivate, 1);
	event_source->priv->listeners = NULL;
}

BONOBO_TYPE_FUNC_FULL (BonoboEventSource, 
		       Bonobo_EventSource,
		       PARENT_TYPE,
		       bonobo_event_source);

/**
 * bonobo_event_source_new:
 *
 * Creates a new BonoboEventSource object.  Typically this
 * object will be exposed to clients through CORBA and they
 * will register and unregister functions to be notified
 * of events that this EventSource generates.
 * 
 * To notify clients of an event, use the bonobo_event_source_notify_listeners()
 * function.
 *
 * Returns: A new #BonoboEventSource server object.
 */
BonoboEventSource *
bonobo_event_source_new (void)
{
	return g_object_new (BONOBO_TYPE_EVENT_SOURCE, NULL);
}

/**
 * bonobo_event_source_ignore_listeners:
 * @event_source: 
 * 
 *  Instructs the event source to de-register any listeners
 * that are added from the global running context.
 **/
void
bonobo_event_source_ignore_listeners (BonoboEventSource *event_source)
{
	g_return_if_fail (BONOBO_IS_EVENT_SOURCE (event_source));

	event_source->priv->ignore = TRUE;
}

void
bonobo_event_source_client_remove_listener (Bonobo_Unknown     object,
					    Bonobo_Listener    listener,
					    CORBA_Environment *opt_ev)
{
	Bonobo_Unknown     es;
	CORBA_Environment *ev, temp_ev;

	g_return_if_fail (object != CORBA_OBJECT_NIL);
       
	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	es = Bonobo_Unknown_queryInterface (object, 
	        "IDL:Bonobo/EventSource:1.0", ev);

	if (!BONOBO_EX (ev) && es) {

		Bonobo_EventSource_removeListener (es, listener, ev);

		Bonobo_Unknown_unref (es, ev);
	}

	if (!opt_ev) {
		if (BONOBO_EX (ev))
			g_warning ("remove_listener failed '%s'",
				   bonobo_exception_get_text (ev));
		CORBA_exception_free (ev);
	}
}

Bonobo_Listener
bonobo_event_source_client_add_listener_full (Bonobo_Unknown     object,
					      GClosure          *event_callback,
					      const char        *opt_mask,
					      CORBA_Environment *opt_ev)
{
	BonoboListener    *listener = NULL;
	Bonobo_Listener    corba_listener = CORBA_OBJECT_NIL;
	Bonobo_Unknown     es;
	CORBA_Environment *ev, temp_ev;

	g_return_val_if_fail (event_callback != NULL, CORBA_OBJECT_NIL);
	
	if (!opt_ev) {
		ev = &temp_ev;
		CORBA_exception_init (ev);
	} else
		ev = opt_ev;

	es = Bonobo_Unknown_queryInterface (object, 
		"IDL:Bonobo/EventSource:1.0", ev);

	if (BONOBO_EX (ev) || !es)
		goto add_listener_end;

	if (!(listener = bonobo_listener_new_closure (event_callback)))
		goto add_listener_end;

	corba_listener = BONOBO_OBJREF (listener);
	
	if (opt_mask)
		Bonobo_EventSource_addListenerWithMask (
			es, corba_listener, opt_mask, ev);
	else 
		Bonobo_EventSource_addListener (
			es, corba_listener, ev);

	corba_listener = CORBA_Object_duplicate (corba_listener, ev);

	bonobo_object_unref (BONOBO_OBJECT (listener));

	bonobo_object_release_unref (es, ev);

 add_listener_end:

	if (!opt_ev) {
		if (BONOBO_EX (ev))
			g_warning ("add_listener failed '%s'",
				   bonobo_exception_get_text (ev));
		CORBA_exception_free (ev);
	}

	return corba_listener;
} 

void
bonobo_event_source_client_add_listener_closure (Bonobo_Unknown     object,
						 GClosure          *event_callback,
						 const char        *opt_mask,
						 CORBA_Environment *opt_ev)
{
	Bonobo_Listener l;

	l = bonobo_event_source_client_add_listener_full (
		object, event_callback, opt_mask, opt_ev);

	if (l != CORBA_OBJECT_NIL)
		CORBA_Object_release (l, NULL);
}

void
bonobo_event_source_client_add_listener (Bonobo_Unknown           object,
					 BonoboListenerCallbackFn event_callback,
					 const char              *opt_mask,
					 CORBA_Environment       *opt_ev,
					 gpointer                 user_data)
{
	bonobo_event_source_client_add_listener_closure (
		object, g_cclosure_new (G_CALLBACK (event_callback), user_data, NULL),
		opt_mask, opt_ev);
}

