/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-context.h: Handle Global Component contexts.
 *
 * Author:
 *     Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000 Ximian, Inc.
 */
#include <config.h>

#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-context.h>
#include <bonobo/bonobo-shutdown.h>
#include <bonobo/bonobo-running-context.h>
#include <bonobo/bonobo-moniker-context.h>

static GHashTable *bonobo_contexts = NULL;

/**
 * bonobo_context_add:
 * @context_name: the name to refer to the context by
 * @context: The Bonobo_Unknown; a ref. is taken on this.
 * 
 * This function adds a new context to the context system
 **/
void
bonobo_context_add (const CORBA_char *context_name,
		    Bonobo_Unknown    context)
{
	g_return_if_fail (context != CORBA_OBJECT_NIL);

	if (!bonobo_contexts)
		bonobo_contexts = g_hash_table_new (
			g_str_hash, g_str_equal);

	g_hash_table_insert (bonobo_contexts,
			     g_strdup (context_name),
			     bonobo_object_dup_ref (context, NULL));
}

/**
 * bonobo_context_get:
 * @context_name: the name of the context
 * @opt_ev: optional Environment, or NULL
 * 
 *  The most useful context is named 'Activation' and returns
 * the IDL:Bonobo/ActivationContext:1.0 interface.
 * 
 * Return value: a new reference to a global Bonobo context or CORBA_OBJECT_NIL
 **/
Bonobo_Unknown
bonobo_context_get (const CORBA_char  *context_name,
		    CORBA_Environment *opt_ev)
{
	Bonobo_Unknown ret;

	g_return_val_if_fail (context_name != NULL, CORBA_OBJECT_NIL);

	if ((ret = g_hash_table_lookup (bonobo_contexts, context_name)))
		return bonobo_object_dup_ref (ret, opt_ev);
	else
		return CORBA_OBJECT_NIL;
}

static void
context_add (BonoboObject *object, const char *name)
{
	CORBA_Object ref;

	ref = BONOBO_OBJREF (object);

	bonobo_context_add (name, ref);

	/* Don't count it as a running object; we always have it */
	bonobo_running_context_ignore_object (ref);

	bonobo_object_unref (object);
}

/**
 * bonobo_context_init:
 * @void: 
 * 
 * Sets up the context system, internal use only, called
 * by bonobo_init.
 **/
void
bonobo_context_init (void)
{
	context_add (bonobo_moniker_context_new (), "Moniker");
	context_add (bonobo_running_context_new (), "Running");
}

static gboolean
context_destroy (char *key, Bonobo_Unknown handle, gpointer dummy)
{
	g_free (key);
	bonobo_object_release_unref (handle, NULL);
	return TRUE;
}

/**
 * bonobo_context_shutdown:
 * @void: 
 * 
 * Shuts down the context system, internal use only
 **/
void
bonobo_context_shutdown (void)
{
	if (!bonobo_contexts)
		return;

	g_hash_table_foreach_remove (
		bonobo_contexts, (GHRFunc) context_destroy, NULL);
	g_hash_table_destroy (bonobo_contexts);
	bonobo_contexts = NULL;
}
