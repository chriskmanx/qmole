/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-item-handler.c: a generic Item Container resolver (implements ItemContainer)
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 2000 Miguel de Icaza.
 */
#include <config.h>
#include <glib-object.h>
#include <gobject/gmarshal.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-exception.h>

#include <bonobo/bonobo-types.h>
#include <bonobo/bonobo-marshal.h>

#include "bonobo-item-handler.h"

#define PARENT_TYPE BONOBO_TYPE_OBJECT

static GObjectClass *bonobo_item_handler_parent_class;

struct _BonoboItemHandlerPrivate
{
	GClosure *enum_objects;
	GClosure *get_object;
};

static void
bonobo_marshal_POINTER__DUMMY_BOXED (GClosure     *closure,
				     GValue       *return_value,
				     guint         n_param_values,
				     const GValue *param_values,
				     gpointer      invocation_hint,
				     gpointer      marshal_data)
{
	typedef gpointer (*GMarshalFunc_POINTER__POINTER_BOXED) (gpointer     data1,
								 gpointer     arg_1,
								 gpointer     arg_2,
								 gpointer     data2);
	register GMarshalFunc_POINTER__POINTER_BOXED callback;
	register GCClosure *cc = (GCClosure*) closure;
	register gpointer data1, data2;
	gpointer v_return;

	g_return_if_fail (return_value != NULL);
	g_return_if_fail (n_param_values == 2);

	if (G_CCLOSURE_SWAP_DATA (closure)) {
		data1 = closure->data;
		data2 = g_value_peek_pointer (param_values + 0);
	} else {
		data1 = g_value_peek_pointer (param_values + 0);
		data2 = closure->data;
	}
	callback = (GMarshalFunc_POINTER__POINTER_BOXED) (marshal_data ? marshal_data : cc->callback);

	v_return = callback (data1,
			     data2,
			     g_value_get_boxed (param_values + 1),
			     data2);

	g_value_set_pointer (return_value, v_return);
}

static void
bonobo_marshal_BOXED__STRING_BOOLEAN_DUMMY_BOXED (GClosure     *closure,
						  GValue       *return_value,
						  guint         n_param_values,
						  const GValue *param_values,
						  gpointer      invocation_hint,
						  gpointer      marshal_data)
{
	typedef gpointer (*GMarshalFunc_BOXED__STRING_BOOLEAN_POINTER_BOXED) (gpointer     data1,
									      gpointer     arg_1,
									      gboolean     arg_2,
									      gpointer     arg_3,
									      gpointer     arg_4,
									      gpointer     data2);
	register GMarshalFunc_BOXED__STRING_BOOLEAN_POINTER_BOXED callback;
	register GCClosure *cc = (GCClosure*) closure;
	register gpointer data1, data2;
	gpointer v_return;

	g_return_if_fail (return_value != NULL);
	g_return_if_fail (n_param_values == 4);

	if (G_CCLOSURE_SWAP_DATA (closure)) {
		data1 = closure->data;
		data2 = g_value_peek_pointer (param_values + 0);
	} else {
		data1 = g_value_peek_pointer (param_values + 0);
		data2 = closure->data;
	}
	callback = (GMarshalFunc_BOXED__STRING_BOOLEAN_POINTER_BOXED) (marshal_data ? marshal_data : cc->callback);

	v_return = callback (data1,
			     (char*) g_value_get_string (param_values + 1),
			     g_value_get_boolean (param_values + 2),
			     data2,
			     g_value_get_boxed (param_values + 3),
			     data2);

	g_value_set_boxed_take_ownership (return_value, v_return);
}

/*
 * Returns a list of the objects in this container
 */
static Bonobo_ItemContainer_ObjectNames *
impl_enum_objects (PortableServer_Servant servant, CORBA_Environment *ev)
{
	BonoboObject *object = bonobo_object_from_servant (servant);
	BonoboItemHandler *handler = BONOBO_ITEM_HANDLER (object);

	if (handler->priv->enum_objects)
	{
		Bonobo_ItemContainer_ObjectNames *ret;

		bonobo_closure_invoke (handler->priv->enum_objects,
				       G_TYPE_POINTER,                    &ret,
				       BONOBO_TYPE_ITEM_HANDLER,           handler,
				       BONOBO_TYPE_STATIC_CORBA_EXCEPTION, ev,
				       0);

		return ret;
	} else
		return Bonobo_ItemContainer_ObjectNames__alloc ();
}

static Bonobo_Unknown
impl_get_object (PortableServer_Servant servant,
		 const CORBA_char      *item_name,
		 CORBA_boolean          only_if_exists,
		 CORBA_Environment     *ev)
{
	BonoboObject *object = bonobo_object_from_servant (servant);
	BonoboItemHandler *handler = BONOBO_ITEM_HANDLER (object);

	if (handler->priv->get_object) {
		Bonobo_Unknown ret;

		bonobo_closure_invoke (handler->priv->get_object,
				       BONOBO_TYPE_STATIC_UNKNOWN,         &ret,
				       BONOBO_TYPE_ITEM_HANDLER,           handler,
				       G_TYPE_STRING,                      item_name,
				       G_TYPE_BOOLEAN,                     only_if_exists,
				       BONOBO_TYPE_STATIC_CORBA_EXCEPTION, ev,
				       0);
				       
		return ret;
	} else
		return CORBA_OBJECT_NIL;
}

static void
bonobo_item_handler_finalize (GObject *object)
{
	BonoboItemHandler *handler = BONOBO_ITEM_HANDLER (object);

	if (handler->priv) {
		if (handler->priv->enum_objects)
			g_closure_unref (handler->priv->enum_objects);

		if (handler->priv->enum_objects)
			g_closure_unref (handler->priv->get_object);

		g_free (handler->priv);
		handler->priv = 0;
	}

	bonobo_item_handler_parent_class->finalize (object);

}

static void 
bonobo_item_handler_init (GObject *object)
{
	BonoboItemHandler *handler = BONOBO_ITEM_HANDLER (object);

	handler->priv = g_new0 (BonoboItemHandlerPrivate, 1);
}


static void
bonobo_item_handler_class_init (BonoboItemHandlerClass *klass)
{
	POA_Bonobo_ItemContainer__epv *epv = &klass->epv;

	bonobo_item_handler_parent_class = g_type_class_peek_parent (klass);

	G_OBJECT_CLASS (klass)->finalize = bonobo_item_handler_finalize;

	epv->enumObjects     = impl_enum_objects;
	epv->getObjectByName = impl_get_object;

}

BONOBO_TYPE_FUNC_FULL (BonoboItemHandler, 
		       Bonobo_ItemContainer,
		       PARENT_TYPE,
		       bonobo_item_handler);

/**
 * bonobo_item_handler_construct:
 * @container: The handler object to construct
 * @enum_objects: The closure implementing enumObjects
 * @get_object: The closure implementing getObject
 *
 * Constructs the @container BonoboObject using the provided closures
 * for the actual implementation.
 *
 * Returns: The constructed BonoboItemContainer object.
 */
BonoboItemHandler *
bonobo_item_handler_construct (BonoboItemHandler *handler,
			       GClosure          *enum_objects,
			       GClosure          *get_object)
{
	g_return_val_if_fail (handler != NULL, NULL);
	g_return_val_if_fail (BONOBO_IS_ITEM_HANDLER (handler), NULL);

	if (enum_objects)
		handler->priv->enum_objects = bonobo_closure_store
			(enum_objects, bonobo_marshal_POINTER__DUMMY_BOXED);
	if (get_object)
		handler->priv->get_object = bonobo_closure_store
			(get_object, bonobo_marshal_BOXED__STRING_BOOLEAN_DUMMY_BOXED);
	
	return handler;
}

/**
 * bonobo_item_handler_new:
 *
 * Creates a new BonoboItemHandler object.  These are used to hold
 * client sites.
 *
 * Returns: The newly created BonoboItemHandler object
 */
BonoboItemHandler *
bonobo_item_handler_new (BonoboItemHandlerEnumObjectsFn enum_objects,
			 BonoboItemHandlerGetObjectFn   get_object,
			 gpointer                       user_data)
{
	GClosure *enum_objects_closure = NULL;
	GClosure *get_object_closure = NULL;

	if (enum_objects)
		enum_objects_closure =
			g_cclosure_new (G_CALLBACK (enum_objects), user_data, NULL);

	if (get_object)
		get_object_closure =
			g_cclosure_new (G_CALLBACK (get_object), user_data, NULL);

	return bonobo_item_handler_new_closure (enum_objects_closure, get_object_closure);
}

/**
 * bonobo_item_handler_new_closure:
 *
 * Creates a new BonoboItemHandler object.  These are used to hold
 * client sites.
 *
 * Returns: The newly created BonoboItemHandler object
 */
BonoboItemHandler *
bonobo_item_handler_new_closure (GClosure *enum_objects,
				 GClosure *get_object)
{
	BonoboItemHandler *handler;

	handler = g_object_new (bonobo_item_handler_get_type (), NULL);

	return bonobo_item_handler_construct (handler, enum_objects, get_object);
}

static GSList *
bonobo_item_option_new_append (GSList  *option_list,
			       GString *key,
			       GString *value)
{
	BonoboItemOption *option;

	g_assert (key && key->str);

	option = g_new0 (BonoboItemOption, 1);

	option->key  = key->str;
	g_string_free (key, FALSE);

	if (value) {
		option->value = value->str;
		g_string_free (value, FALSE);
	}

	return g_slist_append (option_list, option);
}

/**
 * bonobo_parse_item_options:
 * @option_string: a string with a list of options
 *
 * The bonobo_parse_item_options() routine parses the
 * @option_string which is a semi-colon separated list
 * of arguments.
 *
 * Each argument is of the form value[=key].  The entire
 * option string is defined by:
 *
 * option_string := keydef
 *                | keydef ; option_string
 *
 * keydef := value [=key]
 *
 * The key can be literal values, values with spaces, and the
 * \ character is used as an escape sequence.  To include a
 * literal ";" in a value you can use \;.
 *
 * Returns: A GSList that contains structures of type BonoboItemOption
 * each BonoboItemOption
 */
GSList *
bonobo_item_option_parse (const char *option_string)
{
	GSList     *list  = NULL;
	GString    *key   = NULL;
	GString    *value = NULL;
	const char *p;
	
	for (p = option_string; *p; p++)
		switch (*p) {
		case '=':
			if (!key || value)
				goto parse_error_free;

			value = g_string_new ("");
			break;
		case ';':
			if (!key)
				break;

			list = bonobo_item_option_new_append (list, key, value);
			key = NULL; value = NULL;
			break;
		case '\\':
			if (!key || !*++p)
				goto parse_error_free;

			/* drop through */
		default:
			if (!key)
				key = g_string_new ("");

			if (value)
				g_string_append_c (value, *p);
			else
				g_string_append_c (key, *p);
			break;
		}

	if (key)
		list = bonobo_item_option_new_append (list, key, value);

	return list;

 parse_error_free:
	if (key)
		g_string_free (key, TRUE);

	if (value)
		g_string_free (value, TRUE);

	return list;
}

/** 
 * bonobo_item_options_free:
 * @options: a GSList of BonoboItemOption structures that was returned by bonobo_item_option_parse()
 *
 * Use this to release a list returned by bonobo_item_option_parse()
 */
void
bonobo_item_options_free (GSList *options)
{
	GSList *l;

	for (l = options; l; l = l->next) {
		BonoboItemOption *option = l->data;

		g_free (option->key);
		if (option->value)
			g_free (option->value);

		g_free (option);
	}

	g_slist_free (options);
}
