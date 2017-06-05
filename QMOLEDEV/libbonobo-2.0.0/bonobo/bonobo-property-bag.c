/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-property-bag.c: property bag object implementation.
 *
 * Authors:
 *   Nat Friedman   (nat@ximian.com)
 *   Michael Meeks  (michael@ximian.com)
 *   Dietmar Maurer (dietmar@ximian.com)
 *
 * Copyright 2001 Ximian, Inc.
 */
#include <config.h>
#include <bonobo/Bonobo.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-property-bag.h>

#include <bonobo/bonobo-marshal.h>
#include <bonobo/bonobo-types.h>

#define PARENT_TYPE BONOBO_TYPE_OBJECT

#define CLASS(o) BONOBO_PROPERTY_BAG_CLASS (G_OBJECT_GET_CLASS (o))

#define BAG_FROM_SERVANT(servant) BONOBO_PROPERTY_BAG (bonobo_object (servant))

static GObjectClass *parent_class = NULL;
         

/*
 * Internal data structures.
 */
struct _BonoboPropertyPrivate {
	GClosure             *get_prop;
	GClosure             *set_prop;	
};
	
struct _BonoboPropertyBagPrivate {
	GHashTable *prop_hash;

	GClosure   *get_prop;
	GClosure   *set_prop;
};

static void
notify_listeners (BonoboPropertyBag  *pb,
		  BonoboProperty     *prop,
		  const BonoboArg    *new_value,
		  CORBA_Environment  *opt_ev)
{
	if (prop->flags & Bonobo_PROPERTY_NO_LISTENING)
		return;
	
	bonobo_event_source_notify_listeners_full (pb->es, "Bonobo/Property",
						   "change", prop->name,
						   new_value, opt_ev);
}

static void
bonobo_property_bag_foreach_create_list (gpointer key, 
					 gpointer value,
					 gpointer data)
{
	GList **l = (GList **) data;

	*l = g_list_prepend (*l, value);
}

/**
 * bonobo_property_bag_get_prop_list:
 * @pb: A #BonoboPropertyBag.
 *
 * Returns a #GList of #BonoboProperty structures.  This function is
 * private and should only be used internally, or in a PropertyBag
 * persistence implementation.  You should not touch the
 * #BonoboProperty structure unless you know what you're doing.
 */
GList *
bonobo_property_bag_get_prop_list (BonoboPropertyBag *pb)
{
	GList *l;

	g_return_val_if_fail (pb != NULL, NULL);
	g_return_val_if_fail (BONOBO_IS_PROPERTY_BAG (pb), NULL);

	l = NULL;

	g_hash_table_foreach (pb->priv->prop_hash,
			      bonobo_property_bag_foreach_create_list,
			      &l);

	return l;
}

static Bonobo_KeyList *
impl_Bonobo_PropertyBag_getKeys (PortableServer_Servant  servant,
				 const CORBA_char       *filter,
				 CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	Bonobo_KeyList  	*name_list;
	GList			*props;
	GList			*curr;
	int                      len;

	len = g_hash_table_size (pb->priv->prop_hash);

	name_list = Bonobo_KeyList__alloc ();

	if (len == 0)
		return name_list;

	name_list->_buffer = CORBA_sequence_CORBA_string_allocbuf (len);
	CORBA_sequence_set_release (name_list, TRUE);

	props = bonobo_property_bag_get_prop_list (pb);

	for (curr = props; curr != NULL; curr = curr->next) {
		BonoboProperty *prop = curr->data;

		name_list->_buffer [name_list->_length] = 
			CORBA_string_dup (prop->name);
		
		name_list->_length++;
	}

	g_list_free (props);

	return name_list;
}

static CORBA_TypeCode
impl_Bonobo_PropertyBag_getType (PortableServer_Servant  servant,
				 const CORBA_char       *key,
				 CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;

	prop = g_hash_table_lookup (pb->priv->prop_hash, key);

	if (!prop || !prop->type) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return TC_null;
	}

	return (CORBA_TypeCode) CORBA_Object_duplicate 
		((CORBA_Object) prop->type, ev);
}

static CORBA_any *
impl_Bonobo_PropertyBag_getValue (PortableServer_Servant  servant,
				  const CORBA_char       *key,
				  CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;
	BonoboArg               *arg;

	prop = g_hash_table_lookup (pb->priv->prop_hash, key);

	if (!prop || !prop->priv->get_prop) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	bonobo_closure_invoke (prop->priv->get_prop,
			       BONOBO_TYPE_STATIC_CORBA_ANY,       &arg,
			       BONOBO_TYPE_PROPERTY_BAG,           pb,
			       BONOBO_TYPE_STATIC_CORBA_TYPECODE,  prop->type,
			       G_TYPE_UINT,                        prop->idx,
			       BONOBO_TYPE_STATIC_CORBA_EXCEPTION, ev,
			       0);

	return arg;
}

static Bonobo_PropertySet *
impl_Bonobo_PropertyBag_getValues (PortableServer_Servant  servant,
				   const CORBA_char       *filter,
				   CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	Bonobo_PropertySet      *set;
	GList		        *props;
	GList		        *curr;
	int		         len;

	len = g_hash_table_size (pb->priv->prop_hash);

	set = Bonobo_PropertySet__alloc ();

	if (len == 0)
		return set;

	set->_buffer = CORBA_sequence_Bonobo_Pair_allocbuf (len);
	CORBA_sequence_set_release (set, TRUE);

	props = bonobo_property_bag_get_prop_list (pb);

	for (curr = props; curr != NULL; curr = curr->next) {
		BonoboProperty *prop = curr->data;
		BonoboArg *arg;

		set->_buffer [set->_length].name =  
			CORBA_string_dup (prop->name);

		bonobo_closure_invoke (prop->priv->get_prop,
				       BONOBO_TYPE_STATIC_CORBA_ANY,       &arg,
				       BONOBO_TYPE_PROPERTY_BAG,           pb,
				       BONOBO_TYPE_STATIC_CORBA_TYPECODE,  prop->type,
				       G_TYPE_UINT,                        prop->idx,
				       BONOBO_TYPE_STATIC_CORBA_EXCEPTION, ev,
				       0);

		set->_buffer [set->_length].value = *arg;

		set->_length++;
	}

	g_list_free (props);

	return set;
}

static void 
impl_Bonobo_PropertyBag_setValue (PortableServer_Servant  servant,
				  const CORBA_char       *key,
				  const CORBA_any        *value,
				  CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;

	prop = g_hash_table_lookup (pb->priv->prop_hash, key);

	if (!prop || !prop->priv->set_prop) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return;
	}

	if (!bonobo_arg_type_is_equal (prop->type, value->_type, ev)) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_InvalidType);
		return;
	}

	bonobo_closure_invoke (prop->priv->set_prop,
			       G_TYPE_NONE,
			       BONOBO_TYPE_PROPERTY_BAG,           pb,
			       BONOBO_TYPE_STATIC_CORBA_ANY,       value,
			       G_TYPE_UINT,                        prop->idx,
			       BONOBO_TYPE_STATIC_CORBA_EXCEPTION, ev,
			       0);

	if (prop->flags & Bonobo_PROPERTY_NO_AUTONOTIFY)
		return;
	
	if (!BONOBO_EX (ev))
		notify_listeners (pb, prop, value, NULL);
}

static void 
impl_Bonobo_PropertyBag_setValues (PortableServer_Servant    servant,
				   const Bonobo_PropertySet *set,
				   CORBA_Environment        *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;
	int i;

	for (i = 0; i < set->_length; i++) {
		prop = g_hash_table_lookup (pb->priv->prop_hash, 
					    set->_buffer [i].name);
		
		if (!prop || !prop->priv->set_prop) {
			bonobo_exception_set (ev, 
			        ex_Bonobo_PropertyBag_NotFound);
			return;
		}

		if (!bonobo_arg_type_is_equal (prop->type, 
					       set->_buffer [i].value._type,
					       ev)) {
			bonobo_exception_set (ev, 
			        ex_Bonobo_PropertyBag_InvalidType);
			return;
		}
	}

	for (i = 0; i < set->_length; i++) {
		prop = g_hash_table_lookup (pb->priv->prop_hash, 
					    set->_buffer [i].name);
		
		bonobo_closure_invoke (prop->priv->set_prop,
				       G_TYPE_NONE,
				       BONOBO_TYPE_PROPERTY_BAG,           pb,
				       BONOBO_TYPE_STATIC_CORBA_ANY,       &set->_buffer [i].value,
				       G_TYPE_UINT,                        prop->idx,
				       BONOBO_TYPE_STATIC_CORBA_EXCEPTION, ev,
				       0);

		if (BONOBO_EX (ev))
			return;

		if (! (prop->flags & Bonobo_PROPERTY_NO_AUTONOTIFY))
			notify_listeners (pb, prop, &set->_buffer [i].value, NULL);
	}
}

static CORBA_any *
impl_Bonobo_PropertyBag_getDefault (PortableServer_Servant  servant,
				    const CORBA_char       *key,
				    CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;
	
	if (!(prop = g_hash_table_lookup (pb->priv->prop_hash, key))) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	if (prop->default_value)
		return bonobo_arg_copy (prop->default_value);
	else {
		BonoboArg *value = bonobo_arg_new (prop->type);
		return value;
	}
}

static CORBA_char *
impl_Bonobo_PropertyBag_getDocTitle (PortableServer_Servant  servant,
				     const CORBA_char       *key,
				     CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;

	if (!(prop = g_hash_table_lookup (pb->priv->prop_hash, key))) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	return prop->doctitle ? CORBA_string_dup (prop->doctitle) :
		CORBA_string_dup ("");		
}

static CORBA_char *
impl_Bonobo_PropertyBag_getDoc (PortableServer_Servant  servant,
				const CORBA_char       *key,
				CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;

	if (!(prop = g_hash_table_lookup (pb->priv->prop_hash, key))) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	return prop->docstring ? CORBA_string_dup (prop->docstring) :
		CORBA_string_dup ("");
}

static CORBA_long
impl_Bonobo_PropertyBag_getFlags (PortableServer_Servant  servant,
				  const CORBA_char       *key,
				  CORBA_Environment      *ev)
{
	BonoboPropertyBag *pb = BAG_FROM_SERVANT (servant);
	BonoboProperty          *prop;

	if (!(prop = g_hash_table_lookup (pb->priv->prop_hash, key))) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return 0;
	}

	return prop->flags;
}



/*
 * BonoboPropertyBag construction/deconstruction functions. 
 */

static void
bonobo_marshal_ANY__TYPECODE_UINT_EXCEPTION (GClosure     *closure,
					     GValue       *return_value,
					     guint         n_param_values,
					     const GValue *param_values,
					     gpointer      invocation_hint,
					     gpointer      marshal_data)
{
	typedef void (*GMarshalFunc_VOID__BOXED_UINT_BOXED) (gpointer     data1,
							     gpointer     arg_1,
							     guint        arg_2,
							     gpointer     arg_3,
							     gpointer     data2);
	register GMarshalFunc_VOID__BOXED_UINT_BOXED callback;
	register GCClosure *cc = (GCClosure*) closure;
	register gpointer data1, data2;
	CORBA_TypeCode tc;
	BonoboArg *any;

	g_return_if_fail (n_param_values == 4);

	if (G_CCLOSURE_SWAP_DATA (closure)) {
		data1 = closure->data;
		data2 = g_value_peek_pointer (param_values + 0);
	} else {
		data1 = g_value_peek_pointer (param_values + 0);
		data2 = closure->data;
	}
	callback = (GMarshalFunc_VOID__BOXED_UINT_BOXED) (marshal_data ? marshal_data : cc->callback);

	tc = bonobo_value_get_corba_typecode (param_values + 1);
	any = bonobo_arg_new (tc);
	CORBA_Object_release ((CORBA_Object) tc, NULL);

	callback (data1,
		  any,
		  g_value_get_uint (param_values + 2),
		  g_value_peek_pointer (param_values + 3),
		  data2);

	g_value_set_boxed_take_ownership (return_value, any);
}


/**
 * bonobo_property_bag_construct:
 * @pb: #BonoboPropertyBag to construct
 * @get_prop: the property get closure
 * @set_prop: the property set closure
 * @es: an event source to aggregate
 * 
 * Constructor, only for use in wrappers and object derivation, please
 * refer to the #bonobo_property_bag_new for normal use.
 *
 * This function returns @pb, or %NULL in case of error.  If it returns %NULL,
 * the passed in @pb is unrefed.
 *
 * Returns:  #BonoboPropertyBag pointer or %NULL.
 */
BonoboPropertyBag *
bonobo_property_bag_construct (BonoboPropertyBag *pb,
			       GClosure          *get_prop,
			       GClosure          *set_prop,
			       BonoboEventSource *es)
{
	pb->es             = es;
	pb->priv->get_prop = bonobo_closure_store (get_prop, bonobo_marshal_ANY__TYPECODE_UINT_EXCEPTION);
	pb->priv->set_prop = bonobo_closure_store (set_prop, bonobo_marshal_VOID__BOXED_UINT_BOXED);

	bonobo_object_add_interface (BONOBO_OBJECT (pb), BONOBO_OBJECT (es));
	
	return pb;
}

/**
 * bonobo_property_bag_new_full:
 * @get_prop: the property get closure
 * @set_prop: the property set closure
 * @es: an event source to aggregate
 *
 * Creates a new property bag with the specified callbacks.
 *
 * Returns: A new #BonoboPropertyBag object.
 */
BonoboPropertyBag *
bonobo_property_bag_new_full (GClosure          *get_prop,
			      GClosure          *set_prop,
			      BonoboEventSource *es)
{
	BonoboPropertyBag *pb;

	g_return_val_if_fail (es != NULL, NULL);

	pb = g_object_new (BONOBO_TYPE_PROPERTY_BAG, NULL);

	return bonobo_property_bag_construct (pb, get_prop, set_prop, es);
}

/**
 * bonobo_property_bag_new:
 * @get_prop: the property get callback
 * @set_prop: the property set callback
 * @user_data: user data for the callbacks
 *
 * Creates a new property bag with the specified callbacks.
 *
 * Returns: A new #BonoboPropertyBag object.
 */
BonoboPropertyBag *
bonobo_property_bag_new	           (BonoboPropertyGetFn get_prop_cb,
			            BonoboPropertySetFn set_prop_cb,
			            gpointer            user_data)
{
	return bonobo_property_bag_new_closure (
		get_prop_cb ? g_cclosure_new (
			G_CALLBACK (get_prop_cb), user_data, NULL) : NULL,
		set_prop_cb ? g_cclosure_new (
			G_CALLBACK (set_prop_cb), user_data, NULL) : NULL);
}

/**
 * bonobo_property_bag_new_closure:
 * @get_prop: the property get closure
 * @set_prop: the property set closure
 *
 * Creates a new property bag with the specified callbacks.
 *
 * Returns: A new #BonoboPropertyBag object.
 */
BonoboPropertyBag *
bonobo_property_bag_new_closure (GClosure *get_prop,
				 GClosure *set_prop)
{
	BonoboEventSource *es;

	es = bonobo_event_source_new ();

	return bonobo_property_bag_new_full (get_prop, set_prop, es);
}

static gboolean
bonobo_property_bag_foreach_remove_prop (gpointer key, 
					 gpointer value,
					 gpointer user_data)
{
	BonoboProperty *prop = (BonoboProperty *)value;

	g_free (prop->name);
	prop->idx = -1;

	bonobo_arg_release (prop->default_value);

	if (prop->docstring)
		g_free (prop->docstring);
	
	if (prop->doctitle)
		g_free (prop->doctitle);

	if (prop->priv->get_prop)
		g_closure_unref (prop->priv->get_prop);
	if (prop->priv->set_prop)
		g_closure_unref (prop->priv->set_prop);

	g_free (prop->priv);
	g_free (prop);

	return TRUE;
}

static void
bonobo_property_bag_finalize (GObject *object)
{
	BonoboPropertyBag *pb = BONOBO_PROPERTY_BAG (object);
	
	/* Destroy all properties. */
	g_hash_table_foreach_remove (pb->priv->prop_hash,
				     bonobo_property_bag_foreach_remove_prop,
				     NULL);

	g_hash_table_destroy (pb->priv->prop_hash);

	if (pb->priv->get_prop)
		g_closure_unref (pb->priv->get_prop);
	if (pb->priv->set_prop)
		g_closure_unref (pb->priv->set_prop);
	
	g_free (pb->priv);

	parent_class->finalize (object);
}


/*
 * BonoboPropertyBag property manipulation API.
 */

/**
 * bonobo_property_bag_add_full:
 * @pb: property bag to add to
 * @name: name of new property
 * @idx: integer index for fast callback switch statement
 * @type: the CORBA type eg. TC_long
 * @default_value: the default value or NULL
 * @docstring: the translated documentation string
 * @flags: various flags
 * @get_prop: a per property get callback
 * @set_prop: a per property set callback
 * @user_data: user data for the callbacks
 * 
 * This adds a property to @pb at the full tilt of complexity.
 **/
void
bonobo_property_bag_add_full (BonoboPropertyBag    *pb,
			      const char           *name,
			      int                   idx,
			      BonoboArgType         type,
			      BonoboArg            *default_value,
			      const char           *doctitle,
			      const char           *docstring,
			      Bonobo_PropertyFlags  flags,
			      GClosure             *get_prop,
			      GClosure             *set_prop)
{
	BonoboProperty *prop;

	g_return_if_fail (pb != NULL);
	g_return_if_fail (BONOBO_IS_PROPERTY_BAG (pb));
	g_return_if_fail (name != NULL);
	g_return_if_fail (type != NULL);
	g_return_if_fail (g_hash_table_lookup (pb->priv->prop_hash, name) == NULL);

	if (flags == 0) { /* Compatibility hack */
		flags = Bonobo_PROPERTY_READABLE |
			Bonobo_PROPERTY_WRITEABLE;
	}
			    
	if (((flags & Bonobo_PROPERTY_READABLE)  && !get_prop) ||
	    ((flags & Bonobo_PROPERTY_WRITEABLE) && !set_prop)) {
		g_warning ("Serious property error, missing get/set fn. "
			   "on %s", name);
		return;
	}

	if (!(flags & Bonobo_PROPERTY_READABLE) && default_value)
		g_warning ("Assigning a default value to a non readable "
			   "property '%s'", name);
	
	prop = g_new0 (BonoboProperty, 1);

	prop->name           = g_strdup (name);
	prop->idx            = idx;
	prop->type           = type;
	prop->flags          = flags;
	prop->docstring      = g_strdup (docstring);
	prop->doctitle       = g_strdup (doctitle);

	prop->priv = g_new0 (BonoboPropertyPrivate, 1);
	prop->priv->get_prop = bonobo_closure_store (get_prop, bonobo_marshal_ANY__TYPECODE_UINT_EXCEPTION);
	prop->priv->set_prop = bonobo_closure_store (set_prop, bonobo_marshal_VOID__BOXED_UINT_BOXED);


	if (default_value)
		prop->default_value = bonobo_arg_copy (default_value);

	g_hash_table_insert (pb->priv->prop_hash, prop->name, prop);
}

/**
 * bonobo_property_bag_remove:
 * @pb: the property bag
 * @name: name of property to remove.
 * 
 * removes the property with @name from @b.
 **/
void
bonobo_property_bag_remove (BonoboPropertyBag *pb,
			    const char        *name)
{
	gpointer key, value;

	g_return_if_fail (BONOBO_IS_PROPERTY_BAG (pb));
	g_return_if_fail (pb->priv != NULL);
	g_return_if_fail (pb->priv->prop_hash != NULL);

	if (g_hash_table_lookup_extended (pb->priv->prop_hash,
					  name, &key, &value))
		bonobo_property_bag_foreach_remove_prop (key, value, NULL);
}

static Bonobo_PropertyFlags
flags_gparam_to_bonobo (guint flags)
{
	Bonobo_PropertyFlags f = 0;

	if (!flags & G_PARAM_READABLE)
		f |= Bonobo_PROPERTY_READABLE;

	if (!flags & G_PARAM_WRITABLE)
		f |= Bonobo_PROPERTY_WRITEABLE;

	return f;
}

#define BONOBO_GTK_MAP_KEY "BonoboGtkMapKey"

static GQuark quark_gobject_map = 0;

static void
get_prop (BonoboPropertyBag *bag,
	  BonoboArg         *arg,
	  guint              arg_id,
	  CORBA_Environment *ev,
	  gpointer           user_data)
{
	GParamSpec *pspec = user_data;
	GValue      new = { 0, };
	GObject    *obj;

	if (!(obj = g_object_get_qdata (G_OBJECT (bag), quark_gobject_map))) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return;
	}

/*	g_warning ("Get prop ... %d: %s", arg_id, g_arg->name);*/

	g_value_init (&new, G_PARAM_SPEC_VALUE_TYPE (pspec));
	g_object_get_property (obj, pspec->name, &new);

	bonobo_arg_from_gvalue (arg, &new);

	g_value_unset (&new);
}

static void
set_prop (BonoboPropertyBag *bag,
	  const BonoboArg   *arg,
	  guint              arg_id,
	  CORBA_Environment *ev,
	  gpointer           user_data)
{
	GParamSpec *pspec = user_data;
	GValue      new = { 0, };
	GObject    *obj;

	if (!(obj = g_object_get_qdata (G_OBJECT (bag), quark_gobject_map))) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return;
	}

/*	g_warning ("Set prop ... %d: %s", arg_id, g_arg->name);*/

	g_value_init (&new, G_PARAM_SPEC_VALUE_TYPE (pspec));

	bonobo_arg_to_gvalue (&new, arg);
	g_object_set_property (obj, pspec->name, &new);

	g_value_unset (&new);
}

#undef MAPPING_DEBUG

/**
 * bonobo_property_bag_add_gtk_args:
 * @pb: destination property bag
 * @on_instance: the instance to associate the properties with
 * @pspecs: a list of the parameters to map
 * @n_params: the size of the list.
 * 
 * Transfers @params from the @on_instance to the property bag,
 * setting up a mapping between the two objects property systems.
 **/
void
bonobo_property_bag_map_params (BonoboPropertyBag *pb,
				GObject           *on_instance,
				const GParamSpec **pspecs,
				guint              n_params)
{
	int          i;

	g_return_if_fail (G_IS_OBJECT (on_instance));
	g_return_if_fail (BONOBO_IS_PROPERTY_BAG (pb));

	if (!n_params)
		return;
	g_return_if_fail (pspecs != NULL);

	if (g_object_get_qdata (G_OBJECT (pb), quark_gobject_map)) {
		g_warning ("Cannot proxy two GObjects in the same bag yet");
		return;
	}
	g_object_set_qdata (G_OBJECT (pb), quark_gobject_map, on_instance);

	/* Setup types, and names */
	for (i = 0; i < n_params; i++) {
		const GParamSpec    *pspec;
		GType                value_type;
		Bonobo_PropertyFlags flags;
		BonoboArgType        type;

		pspec = pspecs [i];
		value_type = G_PARAM_SPEC_VALUE_TYPE (pspec);

		type = bonobo_arg_type_from_gtype (value_type);
		if (!type) {
#ifdef MAPPING_DEBUG
			g_warning ("Can't handle type '%s' on arg '%s'",
				   g_type_name (value_type), pspec->name);
#endif
			continue;
		}

		flags = flags_gparam_to_bonobo (pspec->flags);

		bonobo_property_bag_add_full
			(pb, 
			 g_param_spec_get_name ((GParamSpec *)pspec), 
			 i, type, NULL,
			 g_param_spec_get_nick ((GParamSpec *)pspec),
			 g_param_spec_get_blurb ((GParamSpec *)pspec), 
			 flags,
			 g_cclosure_new (G_CALLBACK (get_prop), (gpointer) pspec, NULL),
			 g_cclosure_new (G_CALLBACK (set_prop), (gpointer) pspec, NULL));
	}
}

/**
 * bonobo_property_bag_add:
 * @pb: property bag to add to
 * @name: name of new property
 * @idx: integer index for fast callback switch statement
 * @type: the CORBA type eg. TC_long
 * @default_value: the default value or NULL
 * @docstring: the translated documentation string
 * @flags: various flags
 * 
 *  Adds a property to the property bag.
 **/
void
bonobo_property_bag_add (BonoboPropertyBag   *pb,
			 const char          *name,
			 int                  idx,
			 BonoboArgType        type,
			 BonoboArg           *default_value,
			 const char          *doctitle,
			 Bonobo_PropertyFlags flags)
{
	g_return_if_fail (pb != NULL);

	bonobo_property_bag_add_full (pb, name, idx, type,
				      default_value, doctitle, 
				      NULL, flags,
				      pb->priv->get_prop,
				      pb->priv->set_prop);
}


/* Class/object initialization functions. */

static void
bonobo_property_bag_class_init (BonoboPropertyBagClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;
	POA_Bonobo_PropertyBag__epv *epv = &klass->epv;

	if (!quark_gobject_map)
		quark_gobject_map = g_quark_from_static_string (
			"BonoboGObjectMap");

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = bonobo_property_bag_finalize;

	epv->getKeys       = impl_Bonobo_PropertyBag_getKeys;
	epv->getType       = impl_Bonobo_PropertyBag_getType;
	epv->getValue      = impl_Bonobo_PropertyBag_getValue;
	epv->setValue      = impl_Bonobo_PropertyBag_setValue;
	epv->getValues     = impl_Bonobo_PropertyBag_getValues;
	epv->setValues     = impl_Bonobo_PropertyBag_setValues;
	epv->getDefault    = impl_Bonobo_PropertyBag_getDefault;
	epv->getDocTitle   = impl_Bonobo_PropertyBag_getDocTitle;
	epv->getDoc        = impl_Bonobo_PropertyBag_getDoc;
	epv->getFlags      = impl_Bonobo_PropertyBag_getFlags;
}

static void
bonobo_property_bag_init (BonoboPropertyBag *pb)
{
	pb->priv = g_new0 (BonoboPropertyBagPrivate, 1);

	pb->priv->prop_hash = g_hash_table_new (g_str_hash, g_str_equal);
}

BONOBO_TYPE_FUNC_FULL (BonoboPropertyBag, 
		       Bonobo_PropertyBag,
		       PARENT_TYPE,
		       bonobo_property_bag);
