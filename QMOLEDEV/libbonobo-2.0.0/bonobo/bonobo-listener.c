/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-listener.c: Generic listener interface for callbacks.
 *
 * Authors:
 *	Alex Graveley (alex@helixcode.com)
 *	Mike Kestner  (mkestner@ameritech.net)
 *
 * Copyright (C) 2000, Ximian, Inc.
 */
#include <config.h>
#include <string.h>

#include <glib-object.h>

#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-listener.h>
#include <bonobo/bonobo-marshal.h>
#include <bonobo/bonobo-types.h>

#define PARENT_TYPE BONOBO_TYPE_OBJECT

static GObjectClass *bonobo_listener_parent_class;

struct _BonoboListenerPrivate {
	GClosure *event_callback;
};

enum SIGNALS {
	EVENT_NOTIFY,
	LAST_SIGNAL
};
static guint signals [LAST_SIGNAL] = { 0 };

static void
impl_Bonobo_Listener_event (PortableServer_Servant servant, 
			    const CORBA_char      *event_name, 
			    const CORBA_any       *args, 
			    CORBA_Environment     *ev)
{
	BonoboListener *listener;

	listener = BONOBO_LISTENER (bonobo_object_from_servant (servant));

	bonobo_object_ref (BONOBO_OBJECT (listener));

	if (listener->priv->event_callback)
		bonobo_closure_invoke (
			listener->priv->event_callback,
			G_TYPE_NONE,
			BONOBO_TYPE_LISTENER,                       listener,
			G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE, event_name,
			BONOBO_TYPE_STATIC_CORBA_ANY,               args,
			BONOBO_TYPE_STATIC_CORBA_EXCEPTION,         ev,
			0);
		
	g_signal_emit (G_OBJECT (listener),
		       signals [EVENT_NOTIFY], 0,
		       event_name, args, ev);

	bonobo_object_unref (BONOBO_OBJECT (listener));
}

static void
bonobo_listener_destroy (BonoboObject *object)
{
	BonoboListener *listener = (BonoboListener *) object;

	if (listener->priv->event_callback) {
		g_closure_unref (listener->priv->event_callback);
		listener->priv->event_callback = NULL;
	}
	
	((BonoboObjectClass *)bonobo_listener_parent_class)->destroy (object);
}

static void
bonobo_listener_finalize (GObject *object)
{
	BonoboListener *listener;

	listener = BONOBO_LISTENER (object);

	if (listener->priv) {
		g_free (listener->priv);
		listener->priv = 0;
	}

	bonobo_listener_parent_class->finalize (object);
}

static void
bonobo_listener_class_init (BonoboListenerClass *klass)
{
	GObjectClass *oclass = (GObjectClass *) klass;
	BonoboObjectClass *boclass = (BonoboObjectClass *) klass;
	POA_Bonobo_Listener__epv *epv = &klass->epv;

	bonobo_listener_parent_class = g_type_class_peek_parent (klass);

	oclass->finalize = bonobo_listener_finalize;
	boclass->destroy = bonobo_listener_destroy;

	signals [EVENT_NOTIFY] = g_signal_new (
		"event_notify", G_TYPE_FROM_CLASS (oclass), G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (BonoboListenerClass, event_notify),
		NULL, NULL,
		bonobo_marshal_VOID__STRING_BOXED_BOXED,
		G_TYPE_NONE, 3,
		G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE,
		BONOBO_TYPE_STATIC_CORBA_ANY,
		BONOBO_TYPE_STATIC_CORBA_EXCEPTION);

	epv->event = impl_Bonobo_Listener_event;
}

static void
bonobo_listener_init (GObject *object)
{
	BonoboListener *listener;

	listener = BONOBO_LISTENER(object);
	listener->priv = g_new (BonoboListenerPrivate, 1);
	listener->priv->event_callback = 0;
}

BONOBO_TYPE_FUNC_FULL (BonoboListener, 
		       Bonobo_Listener,
		       PARENT_TYPE,
		       bonobo_listener);

/**
 * bonobo_listener_new_closure:
 * @event_closure: closure to be invoked when an event is emitted by the EventSource.
 *
 * Creates a generic event listener.  The listener invokes the @event_closure 
 * closure and emits an "event_notify" signal when notified of an event.  
 * The signal callback should be of the form:
 *
 * <informalexample>
 * <programlisting>
 *      void some_callback (BonoboListener *listener,
 *                          char *event_name, 
 *			    CORBA_any *any,
 *			    CORBA_Environment *ev,
 *			    gpointer user_data);
 * </programlisting>
 * </informalexample>
 *
 * You will typically pass the CORBA_Object reference in the return value
 * to an EventSource (by invoking EventSource::addListener).
 *
 * Returns: A BonoboListener object.
 */
BonoboListener*
bonobo_listener_new_closure (GClosure *event_closure)
{
	BonoboListener *listener;

	listener = g_object_new (BONOBO_TYPE_LISTENER, NULL);

	listener->priv->event_callback = bonobo_closure_store (
		event_closure, bonobo_marshal_VOID__STRING_BOXED_BOXED);

	return listener;
}

/**
 * bonobo_listener_new:
 * @event_callback: function to be invoked when an event is emitted by the EventSource.
 * @user_data: data passed to the functioned pointed by @event_call.
 *
 * Creates a generic event listener.  The listener calls the @event_callback 
 * function and emits an "event_notify" signal when notified of an event.  
 * The signal callback should be of the form:
 *
 * <informalexample>
 * <programlisting>
 *      void some_callback (BonoboListener *listener,
 *                          char *event_name, 
 *			    CORBA_any *any,
 *			    CORBA_Environment *ev,
 *			    gpointer user_data);
 * </programlisting>
 * </informalexample>
 *
 * You will typically pass the CORBA_Object reference in the return value
 * to an EventSource (by invoking EventSource::addListener).
 *
 * Returns: A BonoboListener object.
 */
BonoboListener *
bonobo_listener_new (BonoboListenerCallbackFn event_cb,
		     gpointer                 user_data)
{
	GClosure *closure;

	if (event_cb)
		closure = g_cclosure_new (G_CALLBACK (event_cb), user_data, NULL);
	else
		closure = NULL;

	return bonobo_listener_new_closure (closure);
}

/**
 * bonobo_event_make_name:
 * @idl_path: the IDL part of the event name.
 * @kind: the kind of the event
 * @subtype: an optional subtype
 *
 * Creates an event name. Event names consist of three parts. The @idl_path is
 * mainly to create a unique namespace, and should identify the interface 
 * which triggered the event, for example "Bonobo/Property". The @kind denotes
 * what happened, for example "change". Finally you can use the optional 
 * @subtype to make events more specific. All three parts of the name are 
 * joined together separated by colons. "Bonobo/Property:change" or 
 * "Bonobo/Property:change:autosave" are examples of valid event names.
 *
 * Returns: A valid event_name, or NULL on error.
 */
char *
bonobo_event_make_name (const char *idl_path, 
			const char *kind,
			const char *subtype)
{
	g_return_val_if_fail (idl_path != NULL, NULL);
	g_return_val_if_fail (kind != NULL, NULL);
	g_return_val_if_fail (!strchr (idl_path, ':'), NULL);
	g_return_val_if_fail (!strchr (kind, ':'), NULL);
	g_return_val_if_fail (!subtype || !strchr (subtype, ':'), NULL);
	g_return_val_if_fail (strlen (idl_path), NULL);
	g_return_val_if_fail (strlen (kind), NULL);
	g_return_val_if_fail (!subtype || strlen (subtype), NULL);

	if (subtype)
		return g_strconcat (idl_path, ":", kind, ":", 
				    subtype, NULL);
	else
		return g_strconcat (idl_path, ":", kind, NULL);
}

static gboolean
bonobo_event_name_valid (const char *event_name)
{
	gint i = 0, c = 0, l = -1;

	g_return_val_if_fail (event_name != NULL, FALSE);
	g_return_val_if_fail (strlen (event_name), FALSE);

	if (event_name [0] == ':') 
		return FALSE;

	if (event_name [strlen (event_name) - 1] == ':') 
		return FALSE;

	while (event_name [i]) {
		if (event_name [i] == ':') {
			if (l == (i -1))
				return FALSE;
			l = i;
			c++;
		}
		i++;
	}

	if ((c == 1) || (c == 2)) 
		return TRUE;

	return FALSE;
}

static char *
bonobo_event_token (const char *event_name, gint pos)
{
	char **str_array, *res;

	if (!bonobo_event_name_valid (event_name))
		return NULL;

	str_array = g_strsplit (event_name, ":", 3);

	res = g_strdup (str_array [pos]);

	g_strfreev (str_array);

	return res;
}

/**
 * bonobo_event_type:
 * @event_name: the event name
 *
 * The event type consists of the first two parts of the event name, the idl_path
 * combined with the kind.
 *
 * Returns: The event type, or NULL on error.
 */
char *
bonobo_event_type (const char *event_name)
{
	gint i = 0, c = 0;
       
	if (!bonobo_event_name_valid (event_name))
		return NULL;

	while (event_name [i]) { 
		if (event_name [i] == ':') 
			c++;
		if (c == 2) 
			break;
		i++;
	}

	return g_strndup (event_name, i);
}

/**
 * bonobo_event_type:
 * @event_name: the event name
 *
 * Returns: The event subtype, or NULL on error.
 */
char *
bonobo_event_subtype (const char *event_name)
{
	return bonobo_event_token (event_name, 2);
}

/**
 * bonobo_event_kind:
 * @event_name: the event name
 *
 * Returns: The event kind, or NULL on error.
 */
char *
bonobo_event_kind (const char *event_name)
{
	return bonobo_event_token (event_name, 1);
}

/**
 * bonobo_event_idl_path:
 * @event_name: the event name
 *
 * Returns: The event idl path, or NULL on error.
 */
char *
bonobo_event_idl_path (const char *event_name)
{
	return bonobo_event_token (event_name, 0);
}
