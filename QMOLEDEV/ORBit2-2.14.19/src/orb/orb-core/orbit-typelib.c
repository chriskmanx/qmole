#include <config.h>
#include <string.h>
#include <stdlib.h>

#include <orbit/orbit.h>
#include <gmodule.h>
#include <glib.h>

#include "orb-core-private.h"
#include "orbit-debug.h"

CORBA_char *
ORBit_small_get_type_id (CORBA_Object       object,
			 CORBA_Environment *ev)
{
	ORBit_OAObject adaptor_obj = object->adaptor_obj;

	/* We stay always maximaly qualified localy */
	if (adaptor_obj && adaptor_obj->interface->is_active (adaptor_obj))
		return CORBA_string_dup (g_quark_to_string (object->type_qid));

	else {
		CORBA_char *retval = NULL;

		ORBit_small_invoke_stub (
			object,
			&CORBA_Object__imethods [
				CORBA_OBJECT_SMALL_GET_TYPE_ID],
			&retval, NULL, NULL, ev);

		return retval;
	}
}

static GHashTable *interfaces = NULL;

static GHashTable *
get_type_db (void)
{
	if (!interfaces)
		/* FIXME: need a g_atexit free */
		interfaces = g_hash_table_new (
			g_str_hash, g_str_equal);

	return interfaces;
}

static ORBit_IInterface *
lookup_iinterface (const CORBA_char *type_id)
{
	GHashTable *db = get_type_db ();

	return g_hash_table_lookup (db, type_id);
}

static void
add_iinterface (ORBit_IInterface *idata)
{
	GHashTable *db = get_type_db ();

	g_hash_table_insert (db, idata->tc->repo_id, idata);
}

static ORBit_IInterface *
copy_iinterface (const ORBit_IInterface *idata, gboolean shallow)
{
	/* FIXME: we deep copy always for now - we should speed this up */
	/* FIXME: we need to set a flag here */
	return ORBit_copy_value (idata, TC_ORBit_IInterface);
}

static GSList *type_list = NULL;

typedef struct {
	char *name;
	CORBA_sequence_CORBA_TypeCode *types;
	CORBA_sequence_ORBit_IInterface *iinterfaces;
} TypeList;

static void
add_types (const char *libname,
	   CORBA_sequence_CORBA_TypeCode *types,
	   CORBA_sequence_ORBit_IInterface *iinterfaces)
{
	TypeList *tl = g_new0 (TypeList, 1);

	tl->name = g_strdup (libname);
	tl->types = types;
	tl->iinterfaces = iinterfaces;

	/* FIXME: some g_atexit free loving ? */
	type_list = g_slist_prepend (type_list, tl);
}

static CORBA_sequence_CORBA_TypeCode *
get_types (const char *module_name)
{
	GSList *l;

	for (l = type_list; l; l = l->next) {
		TypeList *tl = l->data;

		if (!strcmp (tl->name, module_name)) {
			CORBA_sequence_CORBA_TypeCode *st;

			st = CORBA_sequence_CORBA_TypeCode__alloc ();
			*st = *tl->types;
			st->_release = FALSE;

			return st;
		}
	}

	return NULL;
}

static CORBA_sequence_ORBit_IInterface *
get_iinterfaces (const char *module_name)
{
	GSList *l;

	for (l = type_list; l; l = l->next) {
		TypeList *tl = l->data;

		if (!strcmp (tl->name, module_name)) {
			CORBA_sequence_ORBit_IInterface *st;

			st = CORBA_sequence_ORBit_IInterface__alloc ();
			*st = *tl->iinterfaces;
			st->_release = FALSE;

			return st;
		}
	}

	return NULL;
}

ORBit_IInterface *
ORBit_small_get_iinterface (CORBA_Object       opt_object,
			    const CORBA_char  *type_id,
			    CORBA_Environment *ev)
{
	ORBit_IInterface *retval;
	PortableServer_ClassInfo *ci;

	if ((retval = lookup_iinterface (type_id)))
		retval = copy_iinterface (retval, TRUE);

	else if ((ci = ORBit_classinfo_lookup (type_id))) {
		retval = copy_iinterface (ci->idata, TRUE);

	} else if (opt_object) {
		/* FIXME: first walk the object's data,
		   if local, we might have unregistered
		   interfaces for some reason */
		gpointer args [1];

		args [0] = &type_id;

		ORBit_small_invoke_stub (
			opt_object,
			&CORBA_Object__imethods [
				CORBA_OBJECT_SMALL_GET_IINTERFACE],
			&retval, args, NULL, ev);

		if (retval != CORBA_OBJECT_NIL) {
			ORBit_IInterface *cache;
			cache = copy_iinterface (retval, FALSE);
			add_iinterface (cache);
		}
	}

	if (!retval &&
	    ev->_major == CORBA_NO_EXCEPTION)
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			ex_ORBit_NoIInterface, NULL);

/*	g_warning ("get_iinterface returns '%s' %p",
		   retval ? retval->tc->repo_id : ev->_id,
		   retval); */

	return retval;
}

static void
add_if_unique (GPtrArray  *strings,
	       const char *new_str,
	       gboolean base_path)
{
	int i, len;

	len = strlen (new_str);

	for (i = 0; i < strings->len; i++) {
		if (!strncmp (g_ptr_array_index (strings, i),
			      new_str, len))
			return;
	}

	g_ptr_array_add (
		strings,
		base_path ? 
		  g_strconcat (new_str, "/lib/orbit-2.0", NULL) : 
    		  g_strdup(new_str));
}

/* FIXME: this should be called only once at
   ORB init time really */
char **
ORBit_get_typelib_paths (void)
{
	const char *path;
	int         i;
	GPtrArray  *paths;

	paths = g_ptr_array_sized_new (8);

	g_ptr_array_add (paths, g_strdup (ORBIT_TYPELIB_DIR));

	if ((path = g_getenv ("ORBIT_TYPELIB_PATH"))) {
		char **strv;

		strv = g_strsplit (path, G_SEARCHPATH_SEPARATOR_S, -1);
		for (i = 0; strv && strv [i]; i++)
			add_if_unique (paths, strv [i], FALSE);
		g_strfreev (strv);
	}

	if ((path = g_getenv ("GNOME2_PATH"))) {
		char **strv;

		strv = g_strsplit (path, G_SEARCHPATH_SEPARATOR_S, -1);
		for (i = 0; strv && strv [i]; i++)
			add_if_unique (paths, strv [i], TRUE);
		g_strfreev (strv);
	}

	g_ptr_array_add (paths, NULL);

	return (char **)g_ptr_array_free (paths, FALSE);
}

static gboolean
load_module (const char *fname, const char *libname)
{
	GModule *handle;
	ORBit_IModule *module;

	if (!(handle = g_module_open (fname, G_MODULE_BIND_LAZY))) {
		dprintf (TYPES, "Failed to load '%s': '%s'\n", fname,
			 g_module_error ());
		return FALSE;
	}

	else if (!g_module_symbol (handle, "orbit_imodule_data",
				     (gpointer *)&module)) {
		g_warning ("type library '%s' has no stored types", fname);
		g_module_close (handle);

		return FALSE;
	} else {
		CORBA_sequence_ORBit_IInterface *iinterfaces;
		CORBA_sequence_CORBA_TypeCode *types;
		ORBit_IInterface **p;
		gulong length, i;

		dprintf (TYPES, "Loaded interfaces of serial %d from '%s'\n",
			   module->version, fname);

		for (p = module->interfaces, length = 0; p && *p; p++)
			length++;

		iinterfaces = CORBA_sequence_ORBit_IInterface__alloc ();
		iinterfaces->_length = iinterfaces->_maximum = length;
		iinterfaces->_buffer = CORBA_sequence_ORBit_IInterface_allocbuf (length);
		iinterfaces->_release = CORBA_TRUE;

		for (i = 0; i < length; i++) {
			ORBit_IInterface *src, *dest;

			src = module->interfaces [i];
			dest = &iinterfaces->_buffer [i];

			ORBit_copy_value_core ((gconstpointer *) &src,
					       (gpointer *) &dest,
					       TC_ORBit_IInterface);
			dest = &iinterfaces->_buffer [i];
			add_iinterface (dest);

			dprintf (TYPES, "Type '%s'\n", dest->tc->repo_id);
		}

		types = ORBit_copy_value (
			&module->types, TC_CORBA_sequence_CORBA_TypeCode);


		add_types (libname, types, iinterfaces);

		/* FIXME: before we can close this,
		   we need to deep copy typecodes - that is if
		   in fact we want to close it ? */
/*		g_module_close (handle); */

		return TRUE;
	}
}


/**
 * ORBit_small_load_typelib:
 * @libname: the name of the type library to load
 * 
 * This method has security issues if you do not use
 * an absolute path in @libname. The environment variables
 * ORBIT_TYPELIB_PATH and GNOME2_PATH are used to scan for
 * type libraries.
 * 
 * Return value: FALSE if load failed.
 **/
gboolean
ORBit_small_load_typelib (const char *libname)
{
	gboolean loaded = FALSE;
	char    *fname = NULL;

	g_return_val_if_fail (libname != NULL, FALSE);

	if (!g_path_is_absolute (libname) &&
	    !(libname [0] == '.' &&
	      G_IS_DIR_SEPARATOR (libname [1]))) {
		char   **paths;
		int      i;

		paths = ORBit_get_typelib_paths ();

		for (i = 0; paths && paths [i]; i++) {
			fname = g_strconcat (
				paths [i], G_DIR_SEPARATOR_S, libname, "_module", NULL);

			if ((loaded = load_module (fname, libname)))
				break;

			else {
				g_free (fname);
				fname = NULL;
			}
		}

		g_strfreev (paths);
	} else
		loaded = load_module (libname, libname);

	return loaded;
}

CORBA_sequence_CORBA_TypeCode *
ORBit_small_get_types (const char *name)
{
	return get_types (name);
}

CORBA_sequence_ORBit_IInterface *
ORBit_small_get_iinterfaces (const char *name)
{
	return get_iinterfaces (name);
}
