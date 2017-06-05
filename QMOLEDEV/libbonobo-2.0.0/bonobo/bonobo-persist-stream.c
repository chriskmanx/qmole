/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-persist-stream.c: PersistStream implementation.  Can be used as a
 * base class, or directly for implementing objects that use PersistStream.
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 1999 Ximian, Inc.
 */
#include <config.h>
#include <glib-object.h>
#include <gobject/gmarshal.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-persist-stream.h>

#define PARENT_TYPE BONOBO_TYPE_PERSIST

/* Parent GTK object class */
static BonoboPersistClass *bonobo_persist_stream_parent_class;

static void
impl_load (PortableServer_Servant servant,
	   Bonobo_Stream          stream,
	   const CORBA_char      *type,
	   CORBA_Environment     *ev)
{
	BonoboObject *object = bonobo_object_from_servant (servant);
	BonoboPersistStream *ps = BONOBO_PERSIST_STREAM (object);
	
	if (ps->load_fn != NULL)
		(*ps->load_fn)(ps, stream, (Bonobo_Persist_ContentType) type,
			       ps->closure, ev);
	else {
		GObjectClass *oc = G_OBJECT_GET_CLASS (ps);
		BonoboPersistStreamClass *class = BONOBO_PERSIST_STREAM_CLASS (oc);

		if (class->load)
			(*class->load)(ps, stream, (Bonobo_Persist_ContentType) type, ev);
		else
			CORBA_exception_set (
				ev, CORBA_USER_EXCEPTION,
				ex_Bonobo_NotSupported, NULL);
	}
}

static void
impl_save (PortableServer_Servant servant,
	   Bonobo_Stream          stream,
	   const CORBA_char      *type,
	   CORBA_Environment     *ev)
{
	BonoboObject *object = bonobo_object_from_servant (servant);
	BonoboPersistStream *ps = BONOBO_PERSIST_STREAM (object);
	
	if (ps->save_fn != NULL)
		(*ps->save_fn)(ps, stream, (Bonobo_Persist_ContentType) type, ps->closure, ev);
	else {
		GObjectClass *oc = G_OBJECT_GET_CLASS (ps);
		BonoboPersistStreamClass *class = BONOBO_PERSIST_STREAM_CLASS (oc);

		if (class->save)
			(*class->save)(ps, stream, (Bonobo_Persist_ContentType) type, ev);
		else
			CORBA_exception_set (
				ev, CORBA_USER_EXCEPTION,
				ex_Bonobo_NotSupported, NULL);
	}

	ps->is_dirty = FALSE;
}

static Bonobo_Persist_ContentTypeList *
get_content_types (BonoboPersist *persist, CORBA_Environment *ev)
{
	BonoboPersistStream *ps = BONOBO_PERSIST_STREAM (persist);

	if (ps->types_fn)
		return ps->types_fn (ps, ps->closure, ev);
	else
		return bonobo_persist_generate_content_types (1, "");
}

static void
bonobo_persist_stream_class_init (BonoboPersistStreamClass *klass)
{
	BonoboPersistClass *persist_class = BONOBO_PERSIST_CLASS (klass);
	POA_Bonobo_PersistStream__epv *epv = &klass->epv;

	bonobo_persist_stream_parent_class = g_type_class_peek_parent (klass);

	/* Override and initialize methods */
	klass->save = NULL;
	klass->load = NULL;

	persist_class->get_content_types = get_content_types;

	epv->load   = impl_load;
	epv->save   = impl_save;
}

static void
bonobo_persist_stream_init (BonoboPersistStream *ps)
{
	/* nothing to do */
}

BONOBO_TYPE_FUNC_FULL (BonoboPersistStream,
		       Bonobo_PersistStream,
		       PARENT_TYPE,
		       bonobo_persist_stream);

/**
 * bonobo_persist_stream_construct:
 * @ps: A BonoboPersistStream object
 * @load_fn: Loading routine
 * @save_fn: Saving routine
 * @types_fn: returns the supported types
 * @iid: OAF IID of the object this interface is aggregated to
 * @closure: Data passed to IO routines.
 *
 * Initializes the BonoboPersistStream object.  The load and save
 * operations for the object are performed by the provided @load_fn
 * and @save_fn callback functions, which are passed @closure when
 * they are invoked.  If either @load_fn or @save_fn is %NULL, the
 * corresponding operation is performed by the class load and save
 * routines.
 *
 * Returns: The initialized BonoboPersistStream object.
 */
BonoboPersistStream *
bonobo_persist_stream_construct (BonoboPersistStream       *ps,
				 BonoboPersistStreamIOFn    load_fn,
				 BonoboPersistStreamIOFn    save_fn,
				 BonoboPersistStreamTypesFn types_fn,
				 const gchar               *iid,
				 void                      *closure)
{
	g_return_val_if_fail (ps != NULL, NULL);
	g_return_val_if_fail (BONOBO_IS_PERSIST_STREAM (ps), NULL);

	ps->load_fn = load_fn;
	ps->save_fn = save_fn;
	ps->types_fn = types_fn;
	ps->closure = closure;

	bonobo_persist_construct (BONOBO_PERSIST (ps), iid);
	
	return ps;
}

/**
 * bonobo_persist_stream_new:
 * @load_fn: Loading routine
 * @save_fn: Saving routine
 * @types_fn: get_content_types routine
 * @iid: OAF IID of the object this interface is aggregated to
 * @closure: Data passed to IO routines.
 *
 * Creates a new BonoboPersistStream object. The various operations
 * for the object are performed by the provided callback functions,
 * which are passed @closure when they are invoked. If any callback is
 * %NULL, the corresponding operation is performed by the class load
 * and save routines.
 *
 * Returns: the newly-created BonoboPersistStream object.
 */
BonoboPersistStream *
bonobo_persist_stream_new (BonoboPersistStreamIOFn    load_fn,
			   BonoboPersistStreamIOFn    save_fn,
			   BonoboPersistStreamTypesFn types_fn,
			   const gchar               *iid,
			   void                      *closure)
{
	BonoboPersistStream *ps;

	ps = g_object_new (bonobo_persist_stream_get_type (), NULL);

	bonobo_persist_stream_construct (ps, load_fn, save_fn,
					 types_fn, iid, closure);

	return ps;
}

