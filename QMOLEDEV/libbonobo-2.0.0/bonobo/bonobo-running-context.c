/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-running-context.c: A global running interface
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 *
 * Copyright (C) 2000, Ximian, Inc.
 */
#include <config.h>
#include <stdio.h>
#include <glib/gmain.h>
#include <glib-object.h>

#include <bonobo/bonobo-context.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-event-source.h>
#include <bonobo/bonobo-moniker-util.h>
#include <bonobo/bonobo-running-context.h>
#include <bonobo/bonobo-shutdown.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-debug.h>

#define PARENT_TYPE BONOBO_TYPE_OBJECT

/* you may debug by adding item "running" to BONOBO_DEBUG_FLAGS
   environment variable. */

static BonoboObjectClass *bonobo_running_context_parent_class = NULL;

typedef struct {
	gboolean    emitted_last_unref;
	GHashTable *objects;
	GHashTable *keys;
} BonoboRunningInfo;

BonoboRunningInfo *bonobo_running_info = NULL;
BonoboObject      *bonobo_running_context = NULL;
BonoboEventSource *bonobo_running_event_source = NULL;

enum {
	LAST_UNREF,
	LAST_SIGNAL
};

static guint signals [LAST_SIGNAL] = { 0 };

static void
key_free (gpointer name, gpointer dummy1, gpointer user_data)
{
	g_free (name);
}

static void
bonobo_ri_debug_foreach (gpointer key, gpointer value, gpointer user_data)
{
	CORBA_Object *o = value;
	
	bonobo_debug_print ("", "[%p]:CORBA_Object still running", o);
		
}

void
bonobo_running_context_shutdown (void)
{
	if (bonobo_running_info) {

		BonoboRunningInfo *ri = bonobo_running_info;

#ifdef G_ENABLE_DEBUG
		if(_bonobo_debug_flags & BONOBO_DEBUG_RUNNING) {
			bonobo_debug_print ("rinfo-start", 
					    "-------------------------------------------------");
			
			bonobo_debug_print ("running-objects", "%d running objects", 
					    g_hash_table_size (ri->objects));
			g_hash_table_foreach (ri->objects,
					      bonobo_ri_debug_foreach, NULL);
			bonobo_debug_print ("rinfo-end", 
					    "-------------------------------------------------");
		}
#endif /* G_ENABLE_DEBUG */
		if (ri->objects)
			g_hash_table_destroy (ri->objects);
		ri->objects = NULL;

		if (ri->keys) {
			g_hash_table_foreach_remove (
				ri->keys, (GHRFunc) key_free, NULL);
			g_hash_table_destroy (ri->keys);
			ri->keys = NULL;
		}
		g_free (ri);
	}
	bonobo_running_info = NULL;
	bonobo_running_context = NULL;
	bonobo_running_event_source = NULL;
}

static void
check_destroy (GObject *object)
{
	bonobo_running_context = NULL;
	bonobo_running_event_source = NULL;
}

static BonoboRunningInfo *
get_running_info (gboolean create)
{
	if (!bonobo_running_info && create) {
		bonobo_running_info = g_new (BonoboRunningInfo, 1);
		bonobo_running_info->objects = g_hash_table_new (NULL, NULL);
		bonobo_running_info->keys    = g_hash_table_new (g_str_hash, g_str_equal);
		bonobo_running_info->emitted_last_unref = FALSE;
	}

	return bonobo_running_info;
}

static void
check_empty (void)
{
	BonoboRunningInfo *ri = get_running_info (FALSE);

	if (!ri || !bonobo_running_context)
		return;

	if (!ri->emitted_last_unref &&
	    (g_hash_table_size (ri->objects) == 0) &&
	    (g_hash_table_size (ri->keys) == 0)) {

		ri->emitted_last_unref = TRUE;

		g_signal_emit (G_OBJECT (bonobo_running_context),
			       signals [LAST_UNREF], 0);

		g_return_if_fail (bonobo_running_event_source != NULL);

		bonobo_event_source_notify_listeners (
			bonobo_running_event_source,
			"bonobo:last_unref", NULL, NULL);
	}
}

#ifndef bonobo_running_context_add_object
void
bonobo_running_context_add_object (CORBA_Object object)
{
#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_RUNNING)
		bonobo_running_context_trace_objects (object, "local", 0, 0);
	else
#endif /* G_ENABLE_DEBUG */
	{
		BonoboRunningInfo *ri = get_running_info (TRUE);

		g_hash_table_insert (ri->objects, object, object);
	}
}
#endif


#ifndef bonobo_running_context_remove_object
void
bonobo_running_context_remove_object (CORBA_Object object)
{
#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_RUNNING)
		bonobo_running_context_trace_objects (object, "local", 0, 1);
	else
#endif /* G_ENABLE_DEBUG */
	{
		BonoboRunningInfo *ri = get_running_info (FALSE);

		if (ri) {
			g_hash_table_remove (ri->objects, object);

			check_empty ();
		}
	}
}
#endif

#ifndef bonobo_running_context_ignore_object
void
bonobo_running_context_ignore_object (CORBA_Object object)
{
#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_RUNNING)
		bonobo_running_context_trace_objects (object, "local", 0, 2);
	else
#endif /* G_ENABLE_DEBUG */
	{
		BonoboRunningInfo *ri = get_running_info (FALSE);

		if (ri) {
			g_hash_table_remove (ri->objects, object);
		}
	}
}
#endif

void          
bonobo_running_context_trace_objects (CORBA_Object object,
				      const char  *fn,
				      int          line,
				      int          mode)
{
	BonoboRunningInfo *ri = get_running_info (mode == 0);
	static char *cmode[] = {
		"add_object",
		"remove_object",
		"ignore_object"		
	};

	if (ri) {
		switch (mode) {
		case 0:
			g_hash_table_insert (ri->objects, object, object);
			break;
		case 1:
			g_hash_table_remove (ri->objects, object);

			check_empty ();
			break;
		case 2:
			g_hash_table_remove (ri->objects, object);
			break;
		}

#ifdef G_ENABLE_DEBUG
		if(_bonobo_debug_flags & BONOBO_DEBUG_RUNNING)
			bonobo_debug_print (cmode [mode], 
					    "[%p]:CORBA_Object %d running objects at %s:%d",
					    object, g_hash_table_size (ri->objects), fn, line);
#endif /* G_ENABLE_DEBUG */
	}
}

static void
impl_Bonobo_RunningContext_addObject (PortableServer_Servant servant,
				      const CORBA_Object     object,
				      CORBA_Environment     *ev)
{
	bonobo_running_context_add_object (object);
}

static void
impl_Bonobo_RunningContext_removeObject (PortableServer_Servant servant,
					 const CORBA_Object     object,
					 CORBA_Environment     *ev)
{
	bonobo_running_context_remove_object (object);
}

static void
impl_Bonobo_RunningContext_addKey (PortableServer_Servant servant,
				   const CORBA_char      *key,
				   CORBA_Environment     *ev)
{
	char              *key_copy, *old_key;
	BonoboRunningInfo *ri = get_running_info (TRUE);

	old_key = g_hash_table_lookup (ri->keys, key);
	if (old_key) {
		g_free (old_key);
		g_hash_table_remove (ri->keys, key);
	}
	key_copy = g_strdup (key);

	g_hash_table_insert (ri->keys, key_copy, key_copy);
}

static void
impl_Bonobo_RunningContext_removeKey (PortableServer_Servant servant,
				      const CORBA_char      *key,
				      CORBA_Environment     *ev)
{
	BonoboRunningInfo *ri = get_running_info (FALSE);
	char              *old_key;

	if (!ri)
		return;

	old_key = g_hash_table_lookup (ri->keys, key);
	if (old_key)
		g_free (old_key);
	g_hash_table_remove (ri->keys, key);

	check_empty ();
}

static void
impl_Bonobo_RunningContext_atExitUnref (PortableServer_Servant servant,
					const CORBA_Object     object,
					CORBA_Environment     *ev)
{
	bonobo_running_context_at_exit_unref (object);
}

static void
bonobo_running_context_class_init (BonoboRunningContextClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;
	POA_Bonobo_RunningContext__epv *epv = &klass->epv;

	bonobo_running_context_parent_class = g_type_class_peek_parent (klass);

	((BonoboRunningContextClass *)klass)->last_unref = NULL;

	signals [LAST_UNREF] = g_signal_new (
		"last_unref", G_TYPE_FROM_CLASS (object_class),
		G_SIGNAL_RUN_FIRST,
		G_STRUCT_OFFSET (BonoboRunningContextClass, last_unref),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID, G_TYPE_NONE, 0);

	epv->addObject     = impl_Bonobo_RunningContext_addObject;
	epv->removeObject  = impl_Bonobo_RunningContext_removeObject;
	epv->addKey        = impl_Bonobo_RunningContext_addKey;
	epv->removeKey     = impl_Bonobo_RunningContext_removeKey;
	epv->atExitUnref   = impl_Bonobo_RunningContext_atExitUnref;

}

static void 
bonobo_running_context_init (GObject *object)
{
	/* nothing to do */
}

BONOBO_TYPE_FUNC_FULL (BonoboRunningContext, 
		       Bonobo_RunningContext,
		       PARENT_TYPE,
		       bonobo_running_context);

BonoboObject *
bonobo_running_context_new (void)
{
	if (bonobo_running_context) {
		bonobo_object_ref (bonobo_running_context);
		return bonobo_running_context;
	}

	bonobo_running_context = g_object_new (
		bonobo_running_context_get_type (), NULL);

	bonobo_running_event_source = bonobo_event_source_new ();
	bonobo_running_context_ignore_object (
	        BONOBO_OBJREF (bonobo_running_event_source));
	bonobo_event_source_ignore_listeners (bonobo_running_event_source);

	bonobo_object_add_interface (BONOBO_OBJECT (bonobo_running_context),
				     BONOBO_OBJECT (bonobo_running_event_source));

	g_signal_connect (G_OBJECT (bonobo_running_context),
			  "destroy", G_CALLBACK (check_destroy), NULL);

	return bonobo_running_context;
}

BonoboObject *
bonobo_context_running_get (void)
{
	return bonobo_running_context_new ();
}

static void
last_unref_cb (gpointer      context,
	       CORBA_Object  object)
{
	bonobo_object_release_unref (object, NULL);
}

void 
bonobo_running_context_at_exit_unref (CORBA_Object object)
{
	CORBA_Environment ev;
	CORBA_Object obj_dup;

	CORBA_exception_init (&ev);

	obj_dup = CORBA_Object_duplicate (object, &ev);

	bonobo_running_context_ignore_object (obj_dup);

	if (bonobo_running_context)
		g_signal_connect (G_OBJECT (bonobo_running_context),
				  "last_unref", G_CALLBACK (last_unref_cb),
				  obj_dup);
	
	CORBA_exception_free (&ev);
}

static void
last_unref_exit_cb (gpointer      context,
		    BonoboObject *object)
{
        bonobo_object_unref (object);
	bonobo_main_quit ();
}

void 
bonobo_running_context_auto_exit_unref (BonoboObject *object)
{
	g_return_if_fail (object != NULL);
	g_return_if_fail (BONOBO_IS_OBJECT (object));

	bonobo_running_context_ignore_object (BONOBO_OBJREF (object));

	if (bonobo_running_context)
		g_signal_connect (G_OBJECT (bonobo_running_context),
				  "last_unref",
				  G_CALLBACK (last_unref_exit_cb),
				  object);
}

