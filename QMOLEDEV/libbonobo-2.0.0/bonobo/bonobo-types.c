#include <config.h>
#include <string.h>
#include <glib-object.h>
#include <gobject/gvaluecollector.h>
#include <bonobo/bonobo-types.h>
#include <bonobo/bonobo-arg.h>

/* The marshalers */
#include <bonobo/bonobo-marshal.h>
#include "bonobo-marshal.c"
/* end of marshalers */

typedef struct {
	GType            type;
	CORBA_TypeCode   tc;
} CorbaObjectProxy;

static GQuark corba_object_proxy_id = 0; 

static void
corba_object_proxy_set (GType             type,
			CorbaObjectProxy *proxy)
{
	if (!corba_object_proxy_id)
		corba_object_proxy_id = g_quark_from_static_string ("bonobo-object-proxy");

	g_type_set_qdata (type, corba_object_proxy_id, proxy);
}

static CorbaObjectProxy *
corba_object_proxy_get (GType type)
{
	if (!corba_object_proxy_id)
		corba_object_proxy_id = g_quark_from_static_string ("bonobo-object-proxy");

	return g_type_get_qdata (type, corba_object_proxy_id);
}

static void
corba_object_proxy_value_init (GValue *value)
{
	value->data[0].v_pointer = CORBA_OBJECT_NIL;
}

static void
corba_object_proxy_value_free (GValue *value)
{
	if (value->data[0].v_pointer) {
		CorbaObjectProxy *proxy;
		CORBA_Environment ev;

		proxy = corba_object_proxy_get (G_VALUE_TYPE (value));
		
		CORBA_exception_init (&ev);
		CORBA_Object_release (value->data[0].v_pointer, &ev);
		CORBA_exception_free (&ev);
	}
}

static void
corba_object_proxy_value_copy (const GValue *src_value,
			       GValue       *dest_value)
{
	if (src_value->data[0].v_pointer) {
		CorbaObjectProxy *proxy;

		proxy = corba_object_proxy_get (G_VALUE_TYPE (src_value));
		
		dest_value->data[0].v_pointer = CORBA_Object_duplicate (
			src_value->data[0].v_pointer, NULL);
	} else
		dest_value->data[0].v_pointer = NULL;
}

static gpointer
corba_object_proxy_value_peek_pointer (const GValue *value)
{
	return value->data[0].v_pointer;
}

static gchar*
corba_object_proxy_collect_value (GValue      *value,
				  guint        n_collect_values,
				  GTypeCValue *collect_values,
				  guint        collect_flags)
{
	if (!collect_values[0].v_pointer)
		value->data[0].v_pointer = NULL;
	else {
		CORBA_Environment ev;
		CORBA_Object corba_objref;
		CorbaObjectProxy *proxy;

		proxy = corba_object_proxy_get (G_VALUE_TYPE (value));;
		corba_objref = collect_values[0].v_pointer;

		CORBA_exception_init (&ev);
		if (!CORBA_Object_is_a (corba_objref, proxy->tc->repo_id, &ev))
			return g_strdup_printf ("CORBA Object %p is not a `%s'.",
						corba_objref, proxy->tc->repo_id);

		value->data[0].v_pointer = CORBA_Object_duplicate (corba_objref, &ev);
		CORBA_exception_free (&ev);
	}

	return NULL;
}

static gchar*
corba_object_proxy_lcopy_value (const GValue *value,
				guint         n_collect_values,
				GTypeCValue  *collect_values,
				guint         collect_flags)
{
	gpointer *corba_p = collect_values[0].v_pointer;

	if (!corba_p)
		return g_strdup_printf ("value location for `%s' passed as NULL",
					G_VALUE_TYPE_NAME (value));

	if (!value->data[0].v_pointer)
		*corba_p = NULL;
	else if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
		*corba_p = value->data[0].v_pointer;
	else {
		CorbaObjectProxy *proxy;

		proxy = corba_object_proxy_get (G_VALUE_TYPE (value));;

		*corba_p = CORBA_Object_duplicate (value->data[0].v_pointer, NULL);
	}

	return NULL;
}

GType
bonobo_corba_object_type_register_static (const gchar *name, const CORBA_TypeCode tc,
					  gboolean is_bonobo_unknown)
{
	static const GTypeValueTable vtable = {
		corba_object_proxy_value_init,
		corba_object_proxy_value_free,
		corba_object_proxy_value_copy,
		corba_object_proxy_value_peek_pointer,
		"p",
		corba_object_proxy_collect_value,
		"p",
		corba_object_proxy_lcopy_value,
	};
	static const GTypeInfo type_info = {
		0,		/* class_size */
		NULL,		/* base_init */
		NULL,		/* base_finalize */
		NULL,		/* class_init */
		NULL,		/* class_finalize */
		NULL,		/* class_data */
		0,		/* instance_size */
		0,		/* n_preallocs */
		NULL,		/* instance_init */
		&vtable,	/* value_table */
	};
	GType type;

	g_return_val_if_fail (tc != NULL, 0);
	g_return_val_if_fail (name != NULL, 0);
	g_return_val_if_fail (g_type_from_name (name) == 0, 0);

	type = g_type_register_static (G_TYPE_BOXED, name, &type_info, 0);

	/* install proxy functions upon successfull registration */
	if (type) {
		CorbaObjectProxy *proxy;

		proxy = g_new (CorbaObjectProxy, 1);
		proxy->type = type;
		proxy->tc = (CORBA_TypeCode)
			CORBA_Object_duplicate ((CORBA_Object) tc, NULL);
		corba_object_proxy_set (type, proxy);
	}
	
	return type;
}

#define BONOBO_TYPE_CORBA_OBJECT_IMPL(name,typename,tc,is_bonobo_unknown)	\
GType										\
bonobo_ ## name ## _get_type (void)						\
{										\
	static GType type = 0;							\
	if (!type)								\
		type = bonobo_corba_object_type_register_static (		\
			typename, tc, is_bonobo_unknown);			\
	return type;								\
}

BONOBO_TYPE_CORBA_OBJECT_IMPL (corba_object, "CorbaObject", TC_CORBA_Object, FALSE);
BONOBO_TYPE_CORBA_OBJECT_IMPL (unknown, "BonoboUnknown", TC_Bonobo_Unknown, TRUE);

static gpointer
corba_any_copy (gpointer any)
{
	return bonobo_arg_copy (any);
}

static void
corba_any_free (gpointer any)
{
	bonobo_arg_release (any);
}

GType
bonobo_corba_any_get_type (void)
{
	static GType type = 0;
	if (!type)
		type = g_boxed_type_register_static (
			"BonoboCorbaAny",
			corba_any_copy, corba_any_free);
	return type;
}

static gpointer
corba_typecode_copy (gpointer typecode)
{
	g_warning (G_STRLOC);
	CORBA_Object_duplicate ((CORBA_Object) typecode, NULL);
	return typecode;
}

static void
corba_typecode_free (gpointer typecode)
{
	g_warning (G_STRLOC);
	CORBA_Object_release ((CORBA_Object) typecode, NULL);
}

GType
bonobo_corba_typecode_get_type (void)
{
	static GType type = 0;
	if (!type)
		type = g_boxed_type_register_static (
			"BonoboCorbaTypecode",
			corba_typecode_copy,
			corba_typecode_free);
	return type;
}

static gpointer
corba_exception_copy (gpointer any)
{
	return CORBA_exception__copy (any);
}

static void
corba_exception_free (gpointer env)
{
	CORBA_free (env);
}

GType
bonobo_corba_exception_get_type (void)
{
	static GType type = 0;
	if (!type)
		type = g_boxed_type_register_static (
			"BonoboCorbaException",
			corba_exception_copy,
			corba_exception_free);
	return type;
}

Bonobo_Unknown
bonobo_value_get_unknown (const GValue *value)
{
	g_return_val_if_fail (
		BONOBO_VALUE_HOLDS_UNKNOWN (value),
		CORBA_OBJECT_NIL);

	return bonobo_object_dup_ref (value->data[0].v_pointer, NULL);
}

BonoboArg *
bonobo_value_get_corba_any (const GValue *value)
{
	g_return_val_if_fail (
		BONOBO_VALUE_HOLDS_CORBA_ANY (value),
		NULL);

	return bonobo_arg_copy (value->data[0].v_pointer);
}

CORBA_Object
bonobo_value_get_corba_object (const GValue *value)
{
	g_return_val_if_fail (
		BONOBO_VALUE_HOLDS_CORBA_OBJECT (value),
		CORBA_OBJECT_NIL);

	return CORBA_Object_duplicate (value->data[0].v_pointer, NULL);
}

CORBA_TypeCode
bonobo_value_get_corba_typecode (const GValue *value)
{
	g_return_val_if_fail (
		BONOBO_VALUE_HOLDS_CORBA_TYPECODE (value),
		CORBA_OBJECT_NIL);

	return (CORBA_TypeCode) CORBA_Object_duplicate (value->data[0].v_pointer, NULL);
}

const CORBA_Environment *
bonobo_value_get_corba_exception (const GValue *value)
{
	g_return_val_if_fail (
		BONOBO_VALUE_HOLDS_CORBA_EXCEPTION (value),
		NULL);

	return value->data[0].v_pointer;
}

void
bonobo_value_set_corba_object (GValue *value, const CORBA_Object object)
{
	g_return_if_fail (BONOBO_VALUE_HOLDS_CORBA_TYPECODE (value));
  
	if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS) &&
	    value->data[0].v_pointer != CORBA_OBJECT_NIL)
		CORBA_Object_release (value->data[0].v_pointer, NULL);

	value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
	value->data[0].v_pointer = CORBA_Object_duplicate (object, NULL);
}

void
bonobo_value_set_unknown (GValue *value, const Bonobo_Unknown unknown)
{
	g_return_if_fail (BONOBO_VALUE_HOLDS_UNKNOWN (value));
  
	if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS))
		bonobo_object_release_unref (value->data[0].v_pointer, NULL);
	value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
	value->data[0].v_pointer = unknown;
}

void
bonobo_value_set_corba_any (GValue *value, const CORBA_any *any)
{
	g_return_if_fail (BONOBO_VALUE_HOLDS_CORBA_ANY (value));
  
	if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS))
		bonobo_arg_release (value->data[0].v_pointer);
	value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
	value->data[0].v_pointer = (CORBA_any *) any;
}

void
bonobo_value_set_corba_typecode (GValue *value, const CORBA_TypeCode tc)
{
	g_return_if_fail (BONOBO_VALUE_HOLDS_CORBA_TYPECODE (value));
  
	if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS) &&
	    value->data[0].v_pointer != CORBA_OBJECT_NIL)
		CORBA_Object_release (value->data[0].v_pointer, NULL);

	value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
	value->data[0].v_pointer = CORBA_Object_duplicate ((CORBA_Object) tc, NULL);
}

void
bonobo_value_set_corba_environment (GValue *value, const CORBA_Environment *ev)
{
	g_return_if_fail (BONOBO_VALUE_HOLDS_CORBA_EXCEPTION (value));
  
	if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS))
		CORBA_free (value->data[0].v_pointer);

	value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
	value->data[0].v_pointer = CORBA_exception__copy (ev);
}

void
bonobo_closure_invoke_va_list (GClosure *closure,
			       GValue   *return_value,
			       va_list   var_args)
{
	int     i;
	GType   type;
	GArray *params;
  
	g_return_if_fail (closure != NULL);

	params = g_array_sized_new (FALSE, TRUE, sizeof (GValue), 6);

	while ((type = va_arg (var_args, GType)) != 0) {
		gboolean static_scope = type & G_SIGNAL_TYPE_STATIC_SCOPE;
		GValue value;
		gchar *error;

		value.g_type = 0;
		g_value_init  (&value, type & ~G_SIGNAL_TYPE_STATIC_SCOPE);

		G_VALUE_COLLECT (&value, var_args,
				 static_scope ? G_VALUE_NOCOPY_CONTENTS : 0,
				 &error);
		if (error) {
			g_warning ("%s: %s", G_STRLOC, error);
			g_free (error);
			break;
		}
      
		g_array_append_val (params, value);
	}

	g_closure_invoke (closure,
			  return_value,
			  params->len,
			  (GValue *)params->data,
			  NULL);

	for (i = 0; i < params->len; i++)
		g_value_unset (&g_array_index (params, GValue, i));

	g_array_free (params, TRUE);
}

/**
 * bonobo_closure_invoke:
 * @closure: a standard GClosure
 * @return_type: the type of the first va_arg argument in a
 * set of type / arg pairs.
 *
 * Invokes the closure with the arguments.
 *
 * Example:
 *
 *    bonobo_closure_invoke (closure, G_TYPE_NONE, G_TYPE_INT, first_arg, 0);
 *
 *    glong retval;
 *    bonobo_closure_invoke (closure, G_TYPE_LONG, &retval, 0);
 *
 **/
void
bonobo_closure_invoke (GClosure *closure,
		       GType     return_type,
		       ...)
{
	GType   rtype;
	GValue  return_value = { 0, };
	va_list var_args;

	if (!closure)
		return;

 	va_start (var_args, return_type);

	rtype = return_type & ~G_SIGNAL_TYPE_STATIC_SCOPE;
	if (rtype != G_TYPE_NONE) {
		gchar *error;

		g_value_init (&return_value, rtype);

		/* Initialize return value */
		G_VALUE_LCOPY (&return_value, var_args,
			       G_VALUE_NOCOPY_CONTENTS, &error);

		if (error) {
			g_warning ("%s: %s", G_STRLOC, error);
			g_free (error);
			return;
		}
	}
	
	bonobo_closure_invoke_va_list (
		closure, &return_value, var_args);

	va_end (var_args);

 	va_start (var_args, return_type);

	if (rtype != G_TYPE_NONE) {
		gchar *error;

		/*
		 * FIXME: performance here sucks, so we need a
		 * g_value_steal_contents type method
		 */
		G_VALUE_LCOPY (&return_value, var_args, 0, &error);

		if (error) {
			g_warning ("%s: %s", G_STRLOC, error);
			g_free (error);
			return;
		}

		g_value_unset (&return_value);
	}

	va_end (var_args);
}

/**
 * bonobo_closure_store:
 * @closure: a standard GClosure
 * @default_marshal: the default marshaller to use
 * 
 * Does the necessary refcounting magic and returns a directly
 * storable closure
 **/
GClosure *
bonobo_closure_store (GClosure        *closure,
		      GClosureMarshal  default_marshal)
{
	if (!closure)
		return NULL;

	g_closure_ref (closure);
	g_closure_sink (closure);
	if (G_CLOSURE_NEEDS_MARSHAL (closure))
		g_closure_set_marshal (closure, default_marshal);

	return closure;
}
