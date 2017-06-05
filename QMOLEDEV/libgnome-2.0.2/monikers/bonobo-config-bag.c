/**
 * bonobo-config-bag.c: config bag object implementation.
 *
 * Author:
 *   Dietmar Maurer (dietmar@ximian.com)
 *   Rodrigo Moya   (rodrigo@ximian.com)
 *
 * Copyright 2000 Ximian, Inc.
 */
#include <config.h>
#include <bonobo/Bonobo.h>
#include <bonobo/bonobo-arg.h>
#include <bonobo/bonobo-i18n.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-exception.h>
#include <string.h>

#include "bonobo-config-bag.h"

#define PARENT_TYPE (BONOBO_TYPE_OBJECT)

#define GET_BAG_FROM_SERVANT(servant) BONOBO_CONFIG_BAG (bonobo_object (servant))

static GObjectClass *parent_class = NULL;

#define CLASS(o) BONOBO_CONFIG_BAG_CLASS (G_OBJECT_GET_CLASS (o))

static void
bonobo_config_bag_finalize (GObject *object)
{
	BonoboConfigBag *cb = BONOBO_CONFIG_BAG (object);

	g_free (cb->path);
	g_object_unref (G_OBJECT (cb->conf_client));

	parent_class->finalize (object);
}

static Bonobo_KeyList *
impl_Bonobo_PropertyBag_getKeys (PortableServer_Servant  servant,
				 const CORBA_char       *filter,
				 CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char            *path;
	Bonobo_KeyList  *retval;
	GSList          *slist, *sl;
	GError          *err = NULL;
	int              length;
	int              n;

	if (strchr (filter, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	path = g_strconcat (cb->path, "/", filter, NULL);

	/* get keys from GConf */
	slist = gconf_client_all_entries (cb->conf_client, path, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return CORBA_OBJECT_NIL;
	}

	/* create CORBA sequence */
	length = g_slist_length (slist);
	retval = Bonobo_KeyList__alloc ();
	retval->_length = length;
	CORBA_sequence_set_release (retval, TRUE);
	retval->_buffer = Bonobo_KeyList_allocbuf (length);

	for (sl = slist, n = 0; n < length; sl = sl->next, n++) {
		GConfEntry *entry = (GConfEntry *) sl->data;
		const char *entry_name;

		entry_name = gconf_entry_get_key (entry);
		retval->_buffer[n] = CORBA_string_dup (entry_name);
	}

	g_slist_free (slist);

	return retval;
}

static CORBA_TypeCode
impl_Bonobo_PropertyBag_getType (PortableServer_Servant  servant,
				 const CORBA_char       *key,
				 CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char            *path;
	GConfValue      *value;
	GError          *err = NULL;

	if (strchr (key, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return CORBA_OBJECT_NIL;
	}

	path = g_strconcat (cb->path, "/", key, NULL);

	/* get type for the given key */
	value = gconf_client_get (cb->conf_client, path, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return CORBA_OBJECT_NIL;
	}

	switch (value->type) {
	case GCONF_VALUE_STRING :
		return (CORBA_TypeCode)
			CORBA_Object_duplicate ((CORBA_Object) BONOBO_ARG_STRING, ev);
	case GCONF_VALUE_INT :
		return (CORBA_TypeCode)
			CORBA_Object_duplicate ((CORBA_Object) BONOBO_ARG_LONG, ev);
	case GCONF_VALUE_FLOAT :
		return (CORBA_TypeCode)
			CORBA_Object_duplicate ((CORBA_Object) BONOBO_ARG_DOUBLE, ev);
	case GCONF_VALUE_BOOL :
		return (CORBA_TypeCode)
			CORBA_Object_duplicate ((CORBA_Object) BONOBO_ARG_BOOLEAN, ev);
	default :
		/* FIXME */
	}

	return CORBA_OBJECT_NIL;
}

static BonoboArg*
bonobo_arg_new_from_gconf_value (GConfValue *value)
{
        if (value == NULL)
                return bonobo_arg_new (BONOBO_ARG_NULL);
        
	switch (value->type) {
	case GCONF_VALUE_STRING :
		return bonobo_arg_new_from (BONOBO_ARG_STRING,
					    gconf_value_get_string (value));
	case GCONF_VALUE_INT : {
                long v = gconf_value_get_int (value);
		return bonobo_arg_new_from (BONOBO_ARG_LONG, &v);
        }
	case GCONF_VALUE_FLOAT : {
                double v = gconf_value_get_float (value);
		return bonobo_arg_new_from (BONOBO_ARG_DOUBLE, &v);
        }
	case GCONF_VALUE_BOOL : {
                gboolean v = gconf_value_get_bool (value);
		return bonobo_arg_new_from (BONOBO_ARG_BOOLEAN, &v);
        }
	default :
		return bonobo_arg_new (BONOBO_ARG_NULL);
	}
}

static CORBA_any *
impl_Bonobo_PropertyBag_getValue (PortableServer_Servant  servant,
				  const CORBA_char       *key,
				  CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char            *path;
	GConfValue      *value;
	GError          *err = NULL;
 
	if (strchr (key, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	path = g_strconcat (cb->path, "/", key, NULL);

	value = gconf_client_get (cb->conf_client, path, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return CORBA_OBJECT_NIL;
	}

        /* FIXME The original code here returned BonoboArg*
         * as a CORBA_any*, is that OK?
         */
        return bonobo_arg_new_from_gconf_value (value);
}

static void 
impl_Bonobo_PropertyBag_setValue (PortableServer_Servant  servant,
				  const CORBA_char       *key,
				  const CORBA_any        *value,
				  CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char            *path;
	GError          *err = NULL;
	
	if (strchr (key, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return;
	}

	path = g_strconcat (cb->path, "/", key, NULL);

	if (bonobo_arg_type_is_equal (value->_type, BONOBO_ARG_STRING, ev)) {
		gconf_client_set_string (cb->conf_client, path,
					 BONOBO_ARG_GET_STRING (value), &err);
	}
	else if (bonobo_arg_type_is_equal (value->_type, BONOBO_ARG_LONG, ev)) {
		gconf_client_set_int (cb->conf_client, path,
				      BONOBO_ARG_GET_LONG (value), &err);
	}
	else if (bonobo_arg_type_is_equal (value->_type, BONOBO_ARG_DOUBLE, ev)) {
		gconf_client_set_float (cb->conf_client, path,
					BONOBO_ARG_GET_DOUBLE (value), &err);
	}
	else if (bonobo_arg_type_is_equal (value->_type, BONOBO_ARG_BOOLEAN, ev)) {
		gconf_client_set_bool (cb->conf_client, path,
				       BONOBO_ARG_GET_BOOLEAN (value), &err);
	}
	else if (bonobo_arg_type_is_equal (value->_type, BONOBO_ARG_NULL, ev)) {
		gconf_client_unset (cb->conf_client, path, &err);
	}
	else {
		g_free (path);
		bonobo_exception_general_error_set (ev, NULL, _("Unknown type"));
		return;
	}

	g_free (path);

	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
	}
}

static Bonobo_PropertySet *
impl_Bonobo_PropertyBag_getValues (PortableServer_Servant servant,
				   const CORBA_char       *filter,
				   CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char *path;
	Bonobo_PropertySet *retval;
	GSList *slist, *sl;
	GError *err = NULL;
	int length;
	int n;

	if (strchr (filter, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	path = g_strconcat (cb->path, "/", filter, NULL);

	/* get keys from GConf */
	slist = gconf_client_all_entries (cb->conf_client, path, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return CORBA_OBJECT_NIL;
	}

	/* create CORBA sequence */
	length = g_slist_length (slist);
	retval = Bonobo_PropertySet__alloc ();
	retval->_length = length;
	CORBA_sequence_set_release (retval, TRUE);
	retval->_buffer = CORBA_sequence_Bonobo_Pair_allocbuf (length);

	for (sl = slist, n = 0; n < length; sl = sl->next, n++) {
		GConfEntry *entry = (GConfEntry *) sl->data;
		BonoboArg *arg;
                GConfValue *value;

		retval->_buffer[n].name = CORBA_string_dup (gconf_entry_get_key (entry));
                value = gconf_entry_get_value (entry);

                arg = bonobo_arg_new_from_gconf_value (value);
                
		retval->_buffer[n].value = *arg;
	}

	g_slist_free (slist);

	return retval;
}

static void                  
impl_Bonobo_PropertyBag_setValues (PortableServer_Servant servant,
				   const Bonobo_PropertySet *set,
				   CORBA_Environment *ev)
{
	int i;

	for (i = 0; i < set->_length; i++) {
		impl_Bonobo_PropertyBag_setValue (servant, 
						  set->_buffer [i].name,
						  &set->_buffer [i].value, 
						  ev);
		if (BONOBO_EX (ev))
			return;
	}
}

static CORBA_any *
impl_Bonobo_PropertyBag_getDefault (PortableServer_Servant  servant,
				    const CORBA_char       *key,
				    CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char            *path;
	GConfValue      *value;
	GError          *err = NULL;

	if (strchr (key, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	path = g_strconcat (cb->path, "/", key, NULL);

	value = gconf_client_get_default_from_schema (cb->conf_client, path, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return CORBA_OBJECT_NIL;
	}

        return bonobo_arg_new_from_gconf_value (value);
}

static CORBA_char *
impl_Bonobo_PropertyBag_getDocTitle (PortableServer_Servant  servant,
				     const CORBA_char       *key,
				     CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char            *path;
	CORBA_char      *retval;
	GConfSchema     *schema;
	GError          *err = NULL;

	if (strchr (key, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	path = g_strconcat (cb->path, "/", key, NULL);
	schema = gconf_client_get_schema (cb->conf_client, path, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return NULL;
	}

	retval = CORBA_string_dup (gconf_schema_get_short_desc (schema));

	gconf_schema_free (schema);

	return retval;
}

static CORBA_char *
impl_Bonobo_PropertyBag_getDoc (PortableServer_Servant  servant,
				const CORBA_char       *key,
				CORBA_Environment      *ev)
{
	BonoboConfigBag *cb = GET_BAG_FROM_SERVANT (servant);
	char            *path;
	CORBA_char      *retval;
	GConfSchema     *schema;
	GError          *err = NULL;

	if (strchr (key, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return NULL;
	}

	path = g_strconcat (cb->path, "/", key, NULL);

	schema = gconf_client_get_schema (cb->conf_client, path, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return NULL;
	}

	retval = CORBA_string_dup (gconf_schema_get_long_desc (schema));

	gconf_schema_free (schema);

	return retval;
}

static Bonobo_PropertyFlags
impl_Bonobo_PropertyBag_getFlags (PortableServer_Servant  servant,
				  const CORBA_char       *key,
				  CORBA_Environment      *ev)
{
	BonoboConfigBag      *cb = GET_BAG_FROM_SERVANT (servant);
	char                 *path;
	Bonobo_PropertyFlags  retval = 0;
	GConfEntry           *entry;
	GError               *err = NULL;

	if (strchr (key, '/')) {
		bonobo_exception_set (ev, ex_Bonobo_PropertyBag_NotFound);
		return 0;
	}

	path = g_strconcat (cb->path, "/", key, NULL);
	entry = gconf_client_get_entry (cb->conf_client, path, NULL, TRUE, &err);
	g_free (path);
	if (err) {
		bonobo_exception_general_error_set (ev, NULL, err->message);
		g_error_free (err);
		return 0;
	}

	retval |= Bonobo_PROPERTY_READABLE;
	if (gconf_entry_get_is_writable (entry))
		retval |= Bonobo_PROPERTY_WRITEABLE;

	gconf_entry_free (entry);

	return retval;
}


void
notify_cb (BonoboListener    *listener,
	   const char        *event_name, 
	   const CORBA_any   *any,
	   CORBA_Environment *ev,
	   gpointer           user_data)
{
	BonoboConfigBag *cb = BONOBO_CONFIG_BAG (user_data);
	char *tmp, *ename;

	tmp = bonobo_event_subtype (event_name);
	ename = g_strconcat ("Bonobo/Property:change:", tmp, NULL); 
	g_free (tmp);

	bonobo_event_source_notify_listeners (cb->es, ename, any, NULL);

	g_free (ename);
}

BonoboConfigBag *
bonobo_config_bag_new (const gchar *path)
{
	BonoboConfigBag *cb;
	char *m;
	int l;

	g_return_val_if_fail (path != NULL, NULL);

	cb = g_object_new (BONOBO_TYPE_CONFIG_BAG, NULL);

	if (path[0] == '/')
		cb->path = g_strdup (path);
	else
		cb->path = g_strconcat ("/", path, NULL);

	while ((l = strlen (cb->path)) > 1 && path [l - 1] == '/') 
		cb->path [l] = '\0';
	
	cb->es = bonobo_event_source_new ();

	bonobo_object_add_interface (BONOBO_OBJECT (cb), 
				     BONOBO_OBJECT (cb->es));

	m = g_strconcat ("Bonobo/ConfigDatabase:change", cb->path, ":", NULL);

	//bonobo_event_source_client_add_listener (db, notify_cb, m, NULL, cb);

	g_free (m);

	/* initialize GConf client */
	if (!gconf_is_initialized ())
		gconf_init (0, NULL, NULL);
	cb->conf_client = gconf_client_get_default ();

	return cb;
}

static void
bonobo_config_bag_class_init (BonoboConfigBagClass *class)
{
	GObjectClass *object_class = (GObjectClass *) class;
	POA_Bonobo_PropertyBag__epv *epv= &class->epv;
	
	parent_class = g_type_class_peek_parent (class);

	object_class->finalize = bonobo_config_bag_finalize;

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
bonobo_config_bag_init (BonoboConfigBag *cb)
{
	/* nothing to do */
}

BONOBO_TYPE_FUNC_FULL (BonoboConfigBag, 
		       Bonobo_PropertyBag,
		       PARENT_TYPE,
		       bonobo_config_bag);

