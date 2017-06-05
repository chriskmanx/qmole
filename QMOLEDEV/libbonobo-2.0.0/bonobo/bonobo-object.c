/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-object.c: Bonobo Unknown interface base implementation
 *
 * Authors:
 *   Miguel de Icaza (miguel@kernel.org)
 *   Michael Meeks (michael@helixcode.com)
 *
 * Copyright 1999,2001 Ximian, Inc.
 */

#include <config.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <glib-object.h>
#include <gobject/gmarshal.h>
#include <bonobo/Bonobo.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-shlib-factory.h>
#include <bonobo/bonobo-running-context.h>
#include <bonobo/bonobo-marshal.h>
#include <bonobo/bonobo-types.h>
#include <bonobo/bonobo-shutdown.h>
#include <bonobo/bonobo-debug.h>

/* Some simple tracking - always on */
static GMutex *bonobo_total_aggregates_lock = NULL;
static glong   bonobo_total_aggregates      = 0;
static glong   bonobo_total_aggregate_refs  = 0;

/* you may debug by setting BONOBO_DEBUG_FLAGS environment variable to
   a colon separated list of a subset of {refs,aggregate,lifecycle} */

typedef struct {
	const char *fn;
	gboolean    ref;
	int         line;
} BonoboDebugRefData;

typedef struct {
	int      ref_count;
	gboolean immortal;
	GList   *objs;
#ifdef G_ENABLE_DEBUG
	/* the following is required for reference debugging */
	GList   *refs;
	int      destroyed;
#endif /* G_ENABLE_DEBUG */
} BonoboAggregateObject;

struct _BonoboObjectPrivate {
	BonoboAggregateObject *ao;
};

enum {
	DESTROY,
	SYSTEM_EXCEPTION,
	LAST_SIGNAL
};

static guint bonobo_object_signals [LAST_SIGNAL];
static GObjectClass *bonobo_object_parent_class;

#ifdef G_ENABLE_DEBUG
static GHashTable *living_ao_ht = NULL;
#endif /* G_ENABLE_DEBUG */

/* Do not use this function, it is not what you want; see unref */
static void
bonobo_object_destroy (BonoboAggregateObject *ao)
{
	GList *l;

	g_return_if_fail (ao->ref_count > 0);

	for (l = ao->objs; l; l = l->next) {
		GObject *o = l->data;

		if (o->ref_count >= 1) {
			g_object_ref (o);
			g_signal_emit (o, bonobo_object_signals [DESTROY], 0);
			g_object_unref (o);
		} else
			g_warning ("Serious ref-counting error [%p]", o);
	}
#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) 
		ao->destroyed = TRUE;
#endif /* G_ENABLE_DEBUG */
}

static void
bonobo_object_corba_deactivate (BonoboObject *object)
{
	CORBA_Environment        ev;
	PortableServer_ObjectId *oid;

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_LIFECYCLE)
		bonobo_debug_print("deactivate",
				   "BonoboObject corba deactivate %p", object);
#endif /* G_ENABLE_DEBUG */

	g_assert (object->priv->ao == NULL);

	CORBA_exception_init (&ev);

	if (object->corba_objref != CORBA_OBJECT_NIL) {
		bonobo_running_context_remove_object (object->corba_objref);
		CORBA_Object_release (object->corba_objref, &ev);
		object->corba_objref = CORBA_OBJECT_NIL;
	}

	oid = PortableServer_POA_servant_to_id (
		bonobo_poa(), &object->servant, &ev);
	PortableServer_POA_deactivate_object (bonobo_poa (), oid, &ev);
	
	CORBA_free (oid);
	CORBA_exception_free (&ev);
}

/*
 * bonobo_object_finalize_internal:
 * 
 * This method splits apart the aggregate object, so that each
 * GObject can be finalized individualy.
 *
 * Note that since the (embedded) servant keeps a ref on the
 * GObject, it won't neccessarily be finalized through this
 * routine, but from the poa later.
 */
static void
bonobo_object_finalize_internal (BonoboAggregateObject *ao)
{
	GList *l;

	g_return_if_fail (ao->ref_count == 0);

	for (l = ao->objs; l; l = l->next) {
		GObject *o = G_OBJECT (l->data);

		if (!o)
			g_error ("Serious bonobo object corruption");
		else {
			g_assert (BONOBO_OBJECT (o)->priv->ao != NULL);
#ifdef G_ENABLE_DEBUG
			if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
				g_assert (BONOBO_OBJECT (o)->priv->ao->destroyed);

				bonobo_debug_print ("finalize", 
						    "[%p] %-20s corba_objref=[%p]"
						    " g_ref_count=%d", o,
						    G_OBJECT_TYPE_NAME (o),
						    BONOBO_OBJECT (o)->corba_objref,
						    G_OBJECT (o)->ref_count);
			}
#endif /* G_ENABLE_DEBUG */

			/*
			 * Disconnect the GObject from the aggregate object
			 * and unref it so that it is possibly finalized ---
			 * other parts of glib may still have references to it.
			 *
			 * The GObject was already destroy()ed in
			 * bonobo_object_destroy().
			 */

			BONOBO_OBJECT (o)->priv->ao = NULL;

			bonobo_object_corba_deactivate (BONOBO_OBJECT (o));

			g_object_unref (o);
#ifdef G_ENABLE_DEBUG
			if(_bonobo_debug_flags & BONOBO_DEBUG_LIFECYCLE)
				bonobo_debug_print ("finalize",
						    "Done finalize internal on %p",
						    o);
#endif /* G_ENABLE_DEBUG */
		}
	}

	g_list_free (ao->objs);
	ao->objs = NULL;

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
		for (l = ao->refs; l; l = l->next)
			g_free (l->data);
		g_list_free (ao->refs);
	}
#endif

	g_free (ao);

	/* Some simple debugging - count aggregate free */
	LINC_MUTEX_LOCK   (bonobo_total_aggregates_lock);
	bonobo_total_aggregates--;
	LINC_MUTEX_UNLOCK (bonobo_total_aggregates_lock);
}


/*
 * bonobo_object_finalize_servant:
 * 
 * This routine is called from either an object de-activation
 * or from the poa. It is called to signal the fact that finaly
 * the object is no longer exposed to the world and thus we
 * can safely loose it's GObject reference, and thus de-allocate
 * the memory associated with it.
 */
static void
bonobo_object_finalize_servant (PortableServer_Servant servant,
				CORBA_Environment *ev)
{
	BonoboObject *object = bonobo_object(servant);

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_LIFECYCLE)
		bonobo_debug_print ("finalize",
				    "BonoboObject Servant finalize %p",
				    object);
#endif /* G_ENABLE_DEBUG */

	g_object_unref (G_OBJECT (object));
}

#ifndef bonobo_object_ref
/**
 * bonobo_object_ref:
 * @object: A BonoboObject you want to ref-count
 *
 * Increments the reference count for the aggregate BonoboObject.
 */
gpointer
bonobo_object_ref (gpointer obj)
{
	BonoboObject *object = obj;

	if (!object)
		return object;

	g_return_val_if_fail (BONOBO_IS_OBJECT (object), object);
	g_return_val_if_fail (object->priv->ao->ref_count > 0, object);

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
		bonobo_object_trace_refs (object, "local", 0, TRUE);
	}
	else
#endif /* G_ENABLE_DEBUG */
	if (!object->priv->ao->immortal) {
		object->priv->ao->ref_count++;
		bonobo_total_aggregate_refs++;
	}

	return object;
}
#endif /* bonobo_object_ref */


#ifndef bonobo_object_unref
/**
 * bonobo_object_unref:
 * @object: A BonoboObject you want to unref.
 *
 * Decrements the reference count for the aggregate BonoboObject.
 */
gpointer
bonobo_object_unref (gpointer obj)
{
#ifdef G_ENABLE_DEBUG
	if(!(_bonobo_debug_flags & BONOBO_DEBUG_REFS)) {
#endif /* G_ENABLE_DEBUG */
		BonoboAggregateObject *ao;
		BonoboObject *object = obj;

		if (!object)
			return NULL;

		g_return_val_if_fail (BONOBO_IS_OBJECT (object), NULL);

		ao = object->priv->ao;
		g_return_val_if_fail (ao != NULL, NULL);
		g_return_val_if_fail (ao->ref_count > 0, NULL);

		if (!ao->immortal) {
			if (ao->ref_count == 1)
				bonobo_object_destroy (ao);
			
			ao->ref_count--;
			bonobo_total_aggregate_refs--;
		
			if (ao->ref_count == 0)
				bonobo_object_finalize_internal (ao);
		}
		return NULL;
#ifdef G_ENABLE_DEBUG
	}
	else
		return bonobo_object_trace_refs (obj, "local", 0, FALSE);
#endif /* G_ENABLE_DEBUG */
}
#endif /* bonobo_object_unref */

gpointer
bonobo_object_trace_refs (gpointer    obj,
			  const char *fn,
			  int         line,
			  gboolean    ref)
{
#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
		BonoboObject *object = obj;
		BonoboAggregateObject *ao;
		BonoboDebugRefData *descr;

		if (!object)
			return NULL;
	
		g_return_val_if_fail (BONOBO_IS_OBJECT (object), ref ? object : NULL);
		ao = object->priv->ao;
		g_return_val_if_fail (ao != NULL, ref ? object : NULL);
		
		descr  = g_new (BonoboDebugRefData, 1);
		ao->refs = g_list_prepend (ao->refs, descr);
		descr->fn = fn;
		descr->ref = ref;
		descr->line = line;

		if (ref) {
			g_return_val_if_fail (ao->ref_count > 0, object);

			if (!object->priv->ao->immortal) {
				object->priv->ao->ref_count++;
				bonobo_total_aggregate_refs++;
			}
		
			bonobo_debug_print ("ref", "[%p]:[%p]:%s to %d at %s:%d", 
					    object, ao,
					    G_OBJECT_TYPE_NAME (object),
					    ao->ref_count, fn, line);

			return object;
		} else { /* unref */
			bonobo_debug_print ("unref", "[%p]:[%p]:%s from %d at %s:%d", 
					    object, ao,
					    G_OBJECT_TYPE_NAME (object),
					    ao->ref_count, fn, line);

			g_return_val_if_fail (ao->ref_count > 0, NULL);

			if (ao->immortal)
				bonobo_debug_print ("unusual", "immortal object");
			else {
				if (ao->ref_count == 1) {
					bonobo_object_destroy (ao);
					
					g_return_val_if_fail (ao->ref_count > 0, NULL);
				}
				
				/*
				 * If this blows it is likely some loony used
				 * g_object_unref somewhere instead of
				 * bonobo_object_unref, send them my regards.
				 */
				g_assert (object->priv->ao == ao);
				
				ao->ref_count--;
				bonobo_total_aggregate_refs--;
				
				if (ao->ref_count == 0) {
					
					g_assert (g_hash_table_lookup (living_ao_ht, ao) == ao);
					g_hash_table_remove (living_ao_ht, ao);
					
					bonobo_object_finalize_internal (ao);
					
				} else if (ao->ref_count < 0) {
					bonobo_debug_print ("unusual", 
							    "[%p] already finalized", ao);
				}
			}
			
			return NULL;
		}
	}
	else
#endif /* G_ENABLE_DEBUG */
	if (ref)
		return bonobo_object_ref (obj);
	else {
		bonobo_object_unref (obj);
		return NULL;
	}
}

static void
impl_Bonobo_Unknown_ref (PortableServer_Servant servant, CORBA_Environment *ev)
{
	BonoboObject *object;

	object = bonobo_object_from_servant (servant);

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
#ifndef bonobo_object_ref
		bonobo_object_trace_refs (object, "remote", 0, TRUE);
#else
		bonobo_object_ref (object);
#endif
	}
	else
#endif /* G_ENABLE_DEBUG */
		bonobo_object_ref (object);
}

void
bonobo_object_set_immortal (BonoboObject *object,
			    gboolean      immortal)
{
	BonoboAggregateObject *ao;

	g_return_if_fail (BONOBO_IS_OBJECT (object));
	g_return_if_fail (object->priv != NULL);
	g_return_if_fail (object->priv->ao != NULL);

	ao = object->priv->ao;

	ao->immortal = immortal;
}

/**
 * bonobo_object_dup_ref:
 * @object: a Bonobo_Unknown corba object
 * @opt_ev: an optional exception environment
 * 
 *   This function returns a duplicated CORBA Object reference;
 * it also bumps the ref count on the object. This is ideal to
 * use in any method returning a Bonobo_Object in a CORBA impl.
 * If object is CORBA_OBJECT_NIL it is returned unaffected.
 * 
 * Return value: duplicated & ref'd corba object reference.
 **/
Bonobo_Unknown
bonobo_object_dup_ref (Bonobo_Unknown     object,
		       CORBA_Environment *opt_ev)
{
	Bonobo_Unknown    ans;
	CORBA_Environment  *ev, temp_ev;
       
	if (object == CORBA_OBJECT_NIL)
		return CORBA_OBJECT_NIL;

	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	Bonobo_Unknown_ref (object, ev);
	ans = CORBA_Object_duplicate (object, ev);

	if (!opt_ev)
		CORBA_exception_free (&temp_ev);

	return ans;
}

/**
 * bonobo_object_release_unref:
 * @object: a Bonobo_Unknown corba object
 * @opt_ev: an optional exception environment
 * 
 *   This function releases a CORBA Object reference;
 * it also decrements the ref count on the bonobo object.
 * This is the converse of bonobo_object_dup_ref. We
 * tolerate object == CORBA_OBJECT_NIL silently.
 **/
Bonobo_Unknown
bonobo_object_release_unref (Bonobo_Unknown     object,
			     CORBA_Environment *opt_ev)
{
	CORBA_Environment *ev, temp_ev;

	if (object == CORBA_OBJECT_NIL)
		return CORBA_OBJECT_NIL;

	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	Bonobo_Unknown_unref (object, ev);
	CORBA_Object_release (object, ev);

	if (!opt_ev)
		CORBA_exception_free (&temp_ev);

	return CORBA_OBJECT_NIL;
}

static void
impl_Bonobo_Unknown_unref (PortableServer_Servant servant, CORBA_Environment *ev)
{
	BonoboObject *object;

	object = bonobo_object_from_servant (servant);

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
#ifndef bonobo_object_unref
		bonobo_object_trace_refs (object, "remote", 0, FALSE);
#else
		bonobo_object_unref (object);
#endif
	}
	else
#endif /* G_ENABLE_DEBUG */
		bonobo_object_unref (object);
}

/**
 * bonobo_object_query_local_interface:
 * @object: A #BonoboObject which is the aggregate of multiple objects.
 * @repo_id: The id of the interface being queried.
 *
 * Returns: A #BonoboObject for the requested interface.
 */
BonoboObject *
bonobo_object_query_local_interface (BonoboObject *object,
				     const char   *repo_id)
{
	GList             *l;
	CORBA_Object       corba_retval = CORBA_OBJECT_NIL;
	CORBA_Environment  ev;

	g_return_val_if_fail (BONOBO_IS_OBJECT (object), NULL);

	corba_retval = CORBA_OBJECT_NIL;

	CORBA_exception_init (&ev);

	for (l = object->priv->ao->objs; l; l = l->next){
		BonoboObject *tryme = l->data;

		if (CORBA_Object_is_a (
			tryme->corba_objref, (char *) repo_id, &ev)) {

			if (BONOBO_EX (&ev)) {
				CORBA_exception_free (&ev);
				continue;
			}
			
			bonobo_object_ref (object);

			return tryme;
		}
	}

	CORBA_exception_free (&ev);

	return NULL;
}

static CORBA_Object
impl_Bonobo_Unknown_queryInterface (PortableServer_Servant  servant,
				    const CORBA_char       *repoid,
				    CORBA_Environment      *ev)
{
	BonoboObject *object = bonobo_object_from_servant (servant);
	BonoboObject *local_interface;

	local_interface = bonobo_object_query_local_interface (
		object, repoid);

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
		bonobo_debug_print ("query-interface", 
				    "[%p]:[%p]:%s repoid=%s", 
				    object, object->priv->ao,
				    G_OBJECT_TYPE_NAME (object),
				    repoid);
	}
#endif /* G_ENABLE_DEBUG */

	if (local_interface == NULL)
		return CORBA_OBJECT_NIL;

	return CORBA_Object_duplicate (local_interface->corba_objref, ev);
}

static void
bonobo_object_epv_init (POA_Bonobo_Unknown__epv *epv)
{
	epv->ref            = impl_Bonobo_Unknown_ref;
	epv->unref          = impl_Bonobo_Unknown_unref;
	epv->queryInterface = impl_Bonobo_Unknown_queryInterface;
}

static void
bonobo_object_finalize_gobject (GObject *gobject)
{
	BonoboObject *object = (BonoboObject *) gobject;

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_LIFECYCLE)
		bonobo_debug_print ("finalize",
				    "Bonobo Object finalize GObject %p",
				    gobject);
#endif /* G_ENABLE_DEBUG */

	if (object->priv->ao != NULL)
		g_error ("g_object_unreffing a bonobo_object that "
			 "still has %d refs", object->priv->ao->ref_count);

	g_free (object->priv);

	bonobo_object_parent_class->finalize (gobject);
}

static void
bonobo_object_dummy_destroy (BonoboObject *object)
{
	/* Just to make chaining possibly cleaner */
}

/* VOID:CORBA_OBJECT,BOXED */
static void
bonobo_marshal_VOID__CORBA_BOXED (GClosure     *closure,
				  GValue       *return_value,
				  guint         n_param_values,
				  const GValue *param_values,
				  gpointer      invocation_hint,
				  gpointer      marshal_data)
{
	typedef void (*GMarshalFunc_VOID__OBJECT_BOXED) (gpointer     data1,
							 gpointer     arg_1,
							 gpointer     arg_2,
							 gpointer     data2);
	register GMarshalFunc_VOID__OBJECT_BOXED callback;
	register GCClosure *cc = (GCClosure*) closure;
	register gpointer data1, data2;
	CORBA_Object arg1;

	g_return_if_fail (n_param_values == 3);

	if (G_CCLOSURE_SWAP_DATA (closure)) {
		data1 = closure->data;
		data2 = g_value_peek_pointer (param_values + 0);
	} else {
		data1 = g_value_peek_pointer (param_values + 0);
		data2 = closure->data;
	}
	callback = (GMarshalFunc_VOID__OBJECT_BOXED) (
		marshal_data ? marshal_data : cc->callback);

	arg1 = bonobo_value_get_corba_object (param_values + 1);
	callback (data1, arg1, g_value_get_boxed (param_values + 2), data2);
	CORBA_Object_release (arg1, NULL);
}

static void
bonobo_object_class_init (BonoboObjectClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;

	/* Ensure that the signature checking is going to work */
	g_assert (sizeof (POA_Bonobo_Unknown) == sizeof (gpointer) * 2);
	g_assert (sizeof (BonoboObjectHeader) * 2 == sizeof (BonoboObject));

	bonobo_object_parent_class = g_type_class_peek_parent (klass);

	bonobo_object_signals [DESTROY] =
		g_signal_new ("destroy",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (BonoboObjectClass,destroy),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);
	bonobo_object_signals [SYSTEM_EXCEPTION] =
		g_signal_new ("system_exception",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (BonoboObjectClass,system_exception),
			      NULL, NULL,
			      bonobo_marshal_VOID__CORBA_BOXED,
			      G_TYPE_NONE, 2,
			      BONOBO_TYPE_STATIC_CORBA_OBJECT,
			      BONOBO_TYPE_STATIC_CORBA_EXCEPTION);

	klass->destroy = bonobo_object_dummy_destroy;

	object_class->finalize = bonobo_object_finalize_gobject;
}


static void
do_corba_setup (BonoboObject      *object,
		BonoboObjectClass *klass)
{
	CORBA_Object obj;
	CORBA_Environment ev;
	BonoboObjectClass *xklass;

	CORBA_exception_init (&ev);

	/* Setup the servant structure */
	object->servant._private = NULL;
	object->servant.vepv     = klass->vepv;

	/* Initialize the servant structure with our POA__init fn */
	{
		for (xklass = klass; xklass && !xklass->poa_init_fn;)
			xklass = g_type_class_peek_parent (xklass);
		if (!xklass || !xklass->epv_struct_offset) {
			/*   Also, people using BONOBO_TYPE_FUNC instead of
			 * BONOBO_TYPE_FUNC_FULL might see this; you need
			 * to tell it about the CORBA interface you're
			 * implementing - of course */
			g_warning ("It looks like you used g_type_unique "
				   "instead of b_type_unique on type '%s'",
				   G_OBJECT_CLASS_NAME (klass));
			return;
		}
		xklass->poa_init_fn ((PortableServer_Servant) &object->servant, &ev);
		if (BONOBO_EX (&ev)) {
			g_warning ("Exception initializing servant '%s'",
				   bonobo_exception_get_text (&ev));
			return;
		}
	}

	/*  Instantiate a CORBA_Object reference for the servant
	 * assumes the bonobo POA supports implicit activation */
	obj = PortableServer_POA_servant_to_reference (
		bonobo_poa (), &object->servant, &ev);

	if (BONOBO_EX (&ev)) {
		g_warning ("Exception '%s' getting reference for servant",
			   bonobo_exception_get_text (&ev));
		return;
	}

	object->corba_objref = obj;
	bonobo_running_context_add_object (obj);

	CORBA_exception_free (&ev);
}

static void
bonobo_object_instance_init (GObject    *g_object,
			     GTypeClass *klass)
{
	BonoboObject *object = BONOBO_OBJECT (g_object);
	BonoboAggregateObject *ao;

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_OBJECT) {
		bonobo_debug_print ("object",
				    "bonobo_object_instance init '%s' '%s' -> %p",
				    G_OBJECT_TYPE_NAME (g_object),
				    G_OBJECT_CLASS_NAME (klass), object);
	}
#endif /* G_ENABLE_DEBUG */

	/* Some simple debugging - count aggregate allocate */
	LINC_MUTEX_LOCK   (bonobo_total_aggregates_lock);
	bonobo_total_aggregates++;
	bonobo_total_aggregate_refs++;
	LINC_MUTEX_UNLOCK (bonobo_total_aggregates_lock);

	/* Setup aggregate */
	ao            = g_new0 (BonoboAggregateObject, 1);
	ao->objs      = g_list_append (ao->objs, object);
	ao->ref_count = 1;

	/* Setup Private fields */
	object->priv = g_new (BonoboObjectPrivate, 1);
	object->priv->ao = ao;

	/* Setup signatures */
	object->object_signature  = BONOBO_OBJECT_SIGNATURE;
	object->servant_signature = BONOBO_SERVANT_SIGNATURE;

	/* Though this make look strange, destruction of this object
	   can only occur when the servant is deactivated by the poa.
	   The poa maintains its own ref count over method invocations
	   and delays finalization which happens only after:
	   bonobo_object_finalize_servant: is invoked */
	g_object_ref (g_object);

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
		bonobo_debug_print ("create", "[%p]:[%p]:%s to %d", object, ao,
				    g_type_name (G_TYPE_FROM_CLASS (klass)),
				    ao->ref_count);

		g_assert (g_hash_table_lookup (living_ao_ht, ao) == NULL);
		g_hash_table_insert (living_ao_ht, ao, ao);
	}
#endif /* G_ENABLE_DEBUG */

	do_corba_setup (object, BONOBO_OBJECT_CLASS (klass));
}

/**
 * bonobo_object_get_type:
 *
 * Returns: the GType associated with the base BonoboObject class type.
 */
GType
bonobo_object_get_type (void)
{
	static GType type = 0;

	if (!type) {
		GTypeInfo info = {
			sizeof (BonoboObjectClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) bonobo_object_class_init,
			NULL, /* class_finalize */
			NULL, /* class_data */
			sizeof (BonoboObject),
			0, /* n_preallocs */
			(GInstanceInitFunc) bonobo_object_instance_init
		};
		
#ifdef G_ENABLE_DEBUG
		bonobo_debug_init();
		if(_bonobo_debug_flags & BONOBO_DEBUG_REFS)
			living_ao_ht = g_hash_table_new (NULL, NULL);
#endif /* G_ENABLE_DEBUG */
		type = g_type_register_static (G_TYPE_OBJECT, "BonoboObject",
					       &info, 0);
	}

	return type;
}

#ifdef G_ENABLE_DEBUG
static void
bonobo_ao_debug_foreach (gpointer key, gpointer value, gpointer user_data)
{
	BonoboAggregateObject *ao = value;
	GList *l;

	g_return_if_fail (ao != NULL);

	bonobo_debug_print ("object-status", 
			    "[%p] %-20s ref_count=%d, interfaces=%d", ao, "",
			    ao->ref_count, g_list_length (ao->objs));
		
	for (l = ao->objs; l; l = l->next) {
		BonoboObject *object = BONOBO_OBJECT (l->data);
		
		bonobo_debug_print ("", "[%p] %-20s corba_objref=[%p]"
				    " g_ref_count=%d", object,
				    G_OBJECT_TYPE_NAME (object),
				    object->corba_objref,
				    G_OBJECT (object)->ref_count);
	}

	l = g_list_last (ao->refs);

	if (l)
		bonobo_debug_print ("referencing" ,"");

	for (; l; l = l->prev) {
		BonoboDebugRefData *descr = l->data;

		bonobo_debug_print ("", "%-7s - %s:%d", 
				    descr->ref ? "ref" : "unref",
				    descr->fn, descr->line);
	}
}
#endif /* G_ENABLE_DEBUG */

int
bonobo_object_shutdown (void)
{
#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {	
		bonobo_debug_print ("shutdown-start", 
				    "-------------------------------------------------");

		if (living_ao_ht)
			g_hash_table_foreach (living_ao_ht,
					      bonobo_ao_debug_foreach, NULL);
		
		bonobo_debug_print ("living-objects",
				    "living bonobo objects count = %d",
				    g_hash_table_size (living_ao_ht));
		
		bonobo_debug_print ("shutdown-end", 
				    "-------------------------------------------------");
	}
#endif /* G_ENABLE_DEBUG */

	if (bonobo_total_aggregates > 0) {
		g_warning ("Leaked a total of %ld refs to %ld bonobo object(s)",
			   bonobo_total_aggregate_refs,
			   bonobo_total_aggregates);
		return 1;
	}

	return 0;
}

void
bonobo_object_init (void)
{
	bonobo_total_aggregates_lock = linc_mutex_new ();
}

/**
 * bonobo_object_add_interface:
 * @object: The BonoboObject to which an interface is going to be added.
 * @newobj: The BonoboObject containing the new interface to be added.
 *
 * Adds the interfaces supported by @newobj to the list of interfaces
 * for @object.  This function adds the interfaces supported by
 * @newobj to the list of interfaces support by @object. It should never
 * be used when the object has been exposed to the world. This is a firm
 * part of the contract.
 */
void
bonobo_object_add_interface (BonoboObject *object, BonoboObject *newobj)
{
       BonoboAggregateObject *oldao, *ao;
       GList *l;

       g_return_if_fail (object->priv->ao->ref_count > 0);
       g_return_if_fail (newobj->priv->ao->ref_count > 0);

       if (object->priv->ao == newobj->priv->ao)
               return;

       if (newobj->corba_objref == CORBA_OBJECT_NIL)
	       g_warning ("Adding an interface with a NULL Corba objref");

       /*
	* Explanation:
	*   Bonobo Objects should not be assembled after they have been
	*   exposed, or we would be breaking the contract we have with
	*   the other side.
	*/

       ao = object->priv->ao;
       oldao = newobj->priv->ao;
       ao->ref_count = ao->ref_count + oldao->ref_count - 1;
       bonobo_total_aggregate_refs--;

#ifdef G_ENABLE_DEBUG
       if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
	       bonobo_debug_print ("add_interface", 
				   "[%p]:[%p]:%s to [%p]:[%p]:%s ref_count=%d", 
				   object, object->priv->ao,
				   G_OBJECT_TYPE_NAME (object),
				   newobj, newobj->priv->ao,
				   G_OBJECT_TYPE_NAME (newobj),
				   ao->ref_count);
       }
#endif /* G_ENABLE_DEBUG */

       /* Merge the two AggregateObject lists */
       for (l = oldao->objs; l; l = l->next) {
	       BonoboObject *new_if = l->data;

#ifdef G_ENABLE_DEBUG
	       if(_bonobo_debug_flags & BONOBO_DEBUG_AGGREGATE) {
		       GList *i;
		       CORBA_Environment ev;
		       CORBA_char *new_id;
		       
		       CORBA_exception_init (&ev);

		       new_id = ORBit_small_get_type_id (new_if->corba_objref, &ev);

		       for (i = ao->objs; i; i = i->next) {
			       BonoboObject *old_if = i->data;

			       if (old_if == new_if)
				       g_error ("attempting to merge identical "
						"interfaces [%p]", new_if);
			       else {
				       CORBA_char *old_id;
				       
				       old_id = ORBit_small_get_type_id (old_if->corba_objref, &ev);
				       
				       if (!strcmp (new_id, old_id))
					       g_error ("Aggregating two BonoboObject that implement "
							"the same interface '%s' [%p]", new_id, new_if);
				       CORBA_free (old_id);
			       }
		       }
		       
		       CORBA_free (new_id);
		       CORBA_exception_free (&ev);
	       }
#endif /* G_ENABLE_DEBUG */

	       ao->objs = g_list_prepend (ao->objs, new_if);
	       new_if->priv->ao = ao;
       }

       g_assert (newobj->priv->ao == ao);

#ifdef G_ENABLE_DEBUG
       if(_bonobo_debug_flags & BONOBO_DEBUG_REFS) {
	       g_assert (g_hash_table_lookup (living_ao_ht, oldao) == oldao);
	       g_hash_table_remove (living_ao_ht, oldao);
	       ao->refs = g_list_concat (ao->refs, oldao->refs);
       }
#endif /* G_ENABLE_DEBUG */

       g_list_free (oldao->objs);
       g_free (oldao);

       /* Some simple debugging - count aggregate free */
       LINC_MUTEX_LOCK   (bonobo_total_aggregates_lock);
       bonobo_total_aggregates--;
       LINC_MUTEX_UNLOCK (bonobo_total_aggregates_lock);
}

/**
 * bonobo_object_query_interface:
 * @object: A BonoboObject to be queried for a given interface.
 * @repo_id: The name of the interface to be queried.
 * @opt_ev: optional exception environment
 *
 * Returns: The CORBA interface named @repo_id for @object.
 */
CORBA_Object
bonobo_object_query_interface (BonoboObject      *object,
			       const char        *repo_id,
			       CORBA_Environment *opt_ev)
{
	CORBA_Object retval;
	CORBA_Environment  *ev, temp_ev;
       
	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	retval = Bonobo_Unknown_queryInterface (
		object->corba_objref, repo_id, ev);

	if (BONOBO_EX (ev))
		retval = CORBA_OBJECT_NIL;

	if (!opt_ev)
		CORBA_exception_free (&temp_ev);

	return retval;
}

/**
 * bonobo_object_corba_objref:
 * @object: A BonoboObject whose CORBA object is requested.
 *
 * Returns: The CORBA interface object for which @object is a wrapper.
 */
CORBA_Object
bonobo_object_corba_objref (BonoboObject *object)
{
	g_return_val_if_fail (BONOBO_IS_OBJECT (object), NULL);

	return object->corba_objref;
}

/**
 * bonobo_object_check_env:
 * @object: The object on which we operate
 * @ev: CORBA Environment to check
 *
 * This routine verifies the @ev environment for any fatal system
 * exceptions.  If a system exception occurs, the object raises a
 * "system_exception" signal.  The idea is that GObjects which are
 * used to wrap a CORBA interface can use this function to notify
 * the user if a fatal exception has occurred, causing the object
 * to become defunct.
 */
void
bonobo_object_check_env (BonoboObject      *object,
			 CORBA_Object       obj,
			 CORBA_Environment *ev)
{
	g_return_if_fail (ev != NULL);
	g_return_if_fail (BONOBO_IS_OBJECT (object));

	if (!BONOBO_EX (ev))
		return;

	if (ev->_major == CORBA_SYSTEM_EXCEPTION)
		g_signal_emit (
			G_OBJECT (object),
			bonobo_object_signals [SYSTEM_EXCEPTION],
			0, obj, ev);
}

/**
 * bonobo_unknown_ping:
 * @object: a CORBA object reference of type Bonobo::Unknown
 * @opt_ev: optional exception environment
 *
 * Pings the object @object using the ref/unref methods from Bonobo::Unknown.
 * You can use this one to see if a remote object has gone away.
 *
 * Returns: %TRUE if the Bonobo::Unknown @object is alive.
 */
gboolean
bonobo_unknown_ping (Bonobo_Unknown     object,
		     CORBA_Environment *opt_ev)
{
	gboolean           alive;
	Bonobo_Unknown     unknown;
	CORBA_Environment *ev, temp_ev;
       
	g_return_val_if_fail (object != NULL, FALSE);

	if (!opt_ev) {
		CORBA_exception_init (&temp_ev);
		ev = &temp_ev;
	} else
		ev = opt_ev;

	alive = FALSE;

	unknown = CORBA_Object_duplicate (object, ev);

	Bonobo_Unknown_ref (unknown, ev);

	if (!BONOBO_EX (ev)) {
		Bonobo_Unknown_unref (unknown, ev);
		if (!BONOBO_EX (ev))
			alive = TRUE;
	}

	CORBA_Object_release (unknown, ev);

	if (!opt_ev)
		CORBA_exception_free (&temp_ev);

	return alive;
}

void
bonobo_object_dump_interfaces (BonoboObject *object)
{
	BonoboAggregateObject *ao;
	GList                 *l;
	CORBA_Environment      ev;

	g_return_if_fail (BONOBO_IS_OBJECT (object));

	ao = object->priv->ao;
	
	CORBA_exception_init (&ev);

	fprintf (stderr, "references %d\n", ao->ref_count);
	for (l = ao->objs; l; l = l->next) {
		BonoboObject *o = l->data;
		
		g_return_if_fail (BONOBO_IS_OBJECT (o));

		if (o->corba_objref != CORBA_OBJECT_NIL) {
			CORBA_char   *type_id;

			type_id = ORBit_small_get_type_id (o->corba_objref, &ev);
			fprintf (stderr, "I/F: '%s'\n", type_id);
			CORBA_free (type_id);
		} else
			fprintf (stderr, "I/F: NIL error\n");
	}

	CORBA_exception_free (&ev);
}

static gboolean
idle_unref_fn (BonoboObject *object)
{
	bonobo_object_unref (object);

	return FALSE;
}

void
bonobo_object_idle_unref (gpointer object)
{
	g_return_if_fail (BONOBO_IS_OBJECT (object));

	g_idle_add ((GSourceFunc) idle_unref_fn, object);
}

static void
unref_list (GSList *l)
{
	for (; l; l = l->next)
		bonobo_object_unref (l->data);
}

/**
 * bonobo_object_list_unref_all:
 * @list: A list of BonoboObjects *s
 * 
 *  This routine unrefs all valid objects in
 * the list and then removes them from @list if
 * they have not already been so removed.
 **/
void
bonobo_object_list_unref_all (GList **list)
{
	GList *l;
	GSList *unrefs = NULL, *u;

	g_return_if_fail (list != NULL);

	for (l = *list; l; l = l->next) {
		if (l->data && !BONOBO_IS_OBJECT (l->data))
			g_warning ("Non object in unref list");
		else if (l->data)
			unrefs = g_slist_prepend (unrefs, l->data);
	}

	unref_list (unrefs);

	for (u = unrefs; u; u = u->next)
		*list = g_list_remove (*list, u->data);

	g_slist_free (unrefs);
}

/**
 * bonobo_object_list_unref_all:
 * @list: A list of BonoboObjects *s
 * 
 *  This routine unrefs all valid objects in
 * the list and then removes them from @list if
 * they have not already been so removed.
 **/
void
bonobo_object_slist_unref_all (GSList **list)
{
	GSList *l;
	GSList *unrefs = NULL, *u;

	g_return_if_fail (list != NULL);

	for (l = *list; l; l = l->next) {
		if (l->data && !BONOBO_IS_OBJECT (l->data))
			g_warning ("Non object in unref list");
		else if (l->data)
			unrefs = g_slist_prepend (unrefs, l->data);
	}

	unref_list (unrefs);

	for (u = unrefs; u; u = u->next)
		*list = g_slist_remove (*list, u->data);

	g_slist_free (unrefs);
}

/**
 * bonobo_object:
 * @p: a pointer to something
 * 
 * This function can be passed a BonoboObject * or a
 * PortableServer_Servant, and it will return a BonoboObject *.
 * 
 * Return value: a BonoboObject or NULL on error.
 **/
BonoboObject *
bonobo_object (gpointer p)
{
	BonoboObjectHeader *header;

	if (!p)
		return NULL;

	header = (BonoboObjectHeader *) p;

	if (header->object_signature == BONOBO_OBJECT_SIGNATURE)
		return (BonoboObject *) p;

	else if (header->object_signature == BONOBO_SERVANT_SIGNATURE)
		return (BonoboObject *)(((guchar *) header) -
					BONOBO_OBJECT_HEADER_SIZE);

	g_warning ("Serious servant -> object mapping error '%p'", p);

	return NULL;
}

/**
 * bonobo_type_setup:
 * @type: The type to initialize
 * @init_fn: the POA_init function for the CORBA interface or NULL
 * @fini_fn: NULL or a custom POA free fn.
 * @epv_struct_offset: the offset in the class structure where the epv is or 0
 * 
 *   This function initializes a type derived from BonoboObject, such that
 * when you instantiate a new object of this type with g_type_new the
 * CORBA object will be correctly created and embedded.
 * 
 * Return value: TRUE on success, FALSE on error.
 **/
gboolean
bonobo_type_setup (GType             type,
		   BonoboObjectPOAFn init_fn,
		   BonoboObjectPOAFn fini_fn,
		   int               epv_struct_offset)
{
	GType       p, b_type;
	int           depth;
	BonoboObjectClass *klass;
	gpointer     *vepv;

	/* Setup our class data */
	klass = g_type_class_ref (type);
	klass->epv_struct_offset = epv_struct_offset;
	klass->poa_init_fn       = init_fn;
	klass->poa_fini_fn       = fini_fn;

	/* Calculate how far down the tree we are in epvs */
	b_type = bonobo_object_get_type ();
	for (depth = 0, p = type; p && p != b_type;
	     p = g_type_parent (p)) {
		BonoboObjectClass *xklass;

		xklass = g_type_class_peek (p);

		if (xklass->epv_struct_offset)
			depth++;
	}
	if (!p) {
		g_warning ("Trying to inherit '%s' from a BonoboObject, but "
			   "no BonoboObject in the ancestory",
			   g_type_name (type));
		return FALSE;
	}

#ifdef G_ENABLE_DEBUG
	if(_bonobo_debug_flags & BONOBO_DEBUG_OBJECT) {
		bonobo_debug_print ("object", "We are at depth %d with type '%s'",
				    depth, g_type_name (type));
	}
#endif /* G_ENABLE_DEBUG */

	/* Setup the Unknown epv */
	bonobo_object_epv_init (&klass->epv);
	klass->epv._private = NULL;

	klass->base_epv._private = NULL;
	klass->base_epv.finalize = bonobo_object_finalize_servant;
	klass->base_epv.default_POA = NULL;

	vepv = g_new0 (gpointer, depth + 2);
	klass->vepv = (POA_Bonobo_Unknown__vepv *) vepv;
	klass->vepv->_base_epv = &klass->base_epv;
	klass->vepv->Bonobo_Unknown_epv = &klass->epv;

	/* Build our EPV */
	if (depth > 0) {
		int i;

		for (p = type, i = depth; i > 0;) {
			BonoboObjectClass *xklass;

			xklass = g_type_class_peek (p);

			if (xklass->epv_struct_offset) {
				vepv [i + 1] = ((guchar *)klass) +
					xklass->epv_struct_offset;
				i--;
			}

			p = g_type_parent (p);
		}
	}

	return TRUE;
}

/**
 * bonobo_type_unique:
 * @parent_type: the parent GType
 * @init_fn: a POA initialization function
 * @fini_fn: a POA finialization function or NULL
 * @epv_struct_offset: the offset into the struct that the epv
 * commences at, or 0 if we are inheriting a plain GObject
 * from a BonoboObject, adding no new CORBA interfaces
 * @info: the standard GTypeInfo.
 * 
 * This function is the main entry point for deriving bonobo
 * server interfaces.
 * 
 * Return value: the constructed GType.
 **/
GType
bonobo_type_unique (GType             parent_type,
		    BonoboObjectPOAFn init_fn,
		    BonoboObjectPOAFn fini_fn,
		    int               epv_struct_offset,
		    const GTypeInfo  *info,
		    const gchar      *type_name)
{
	GType       type;

	/*
	 * Since we call g_type_class after the g_type_unique
	 * and before we can return the type to the get_type fn.
	 * it is possible we can re-enter here through eg. a
	 * type check macro, hence we need this guard.
	 */
	if ((type = g_type_from_name (type_name)))
		return type;

	type = g_type_register_static (parent_type, type_name, info, 0);
	if (!type)
		return 0;

	if (bonobo_type_setup (type, init_fn, fini_fn,
			       epv_struct_offset))
		return type;
	else
		return 0;
}
