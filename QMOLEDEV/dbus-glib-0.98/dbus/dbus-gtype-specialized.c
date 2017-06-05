/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gtype-specialized.c: Non-DBus-specific functions for specialized GTypes
 *
 * Copyright (C) 2005 Red Hat, Inc.
 * Copyright (C) 2007 Codethink Ltd.
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <config.h>

#include "dbus-glib.h"
#include "dbus-gtype-specialized-priv.h"
#include "dbus-gvalue-utils.h"
#include <glib.h>
#include <string.h>
#include <gobject/gvaluecollector.h>

/**
 * SECTION:dbus-gtype-specialized
 * @short_description: Specialized GTypes
 * @stability: Unstable
 *
 * Specialized gtypes are basically a way to allow the definition of
 * recursive #GTypes. It allows the definition of 'containers' which is
 * basically a user defined structure capable of holding other data, and a
 * set of functions defining how to access that structure. Containers come in
 * 3 flavors: collections, maps and structs.
 *
 * A collection is a container that holds an ordered set of items, all
 * of which must be the same type. (This is an <emphasis>array</emphasis>
 * in standard D-Bus terminology.) dbus-glib specialized collections can be
 * #GArray (for numeric elements), #GPtrArray (for string, object or
 * boxed elements), #GSList (for boxed elements, not recommended), or a
 * user-defined type.
 *
 * A map is a container that holds a set of key/value pairs.
 * The keys have one type, and the values another; the type of the keys
 * must be a numeric or string-like type. (This is a <emphasis>dict</emphasis>
 * (dictionary) or <emphasis>array of dict entry</emphasis> in standard D-Bus
 * terminology.) dbus-glib specialized maps can be #GHashTable or a
 * user-defined type.
 *
 * A struct is a container that holds a fixed number of members, each member
 * having a predefined type. (This is a <emphasis>struct</emphasis> in
 * standard D-Bus terminology.) It is analogous to the C @struct keyword, but
 * dbus-glib does not generally represent D-Bus structs in C structs.
 * dbus-glib specialized structs can be #GValueArray or a user-defined type.
 *
 * A specialization is a GType detailing a particular container with
 * particular types (a type specialization).
 *
 * Functions are provided for constructing and manipulating specializations.
 *
 * This documentation needs splitting into two pages, one for defining new
 * containers and using existing containers. I expect most users to only do
 * the latter. I also need to add some examples.
 */

/**
 * DBUS_TYPE_G_BOOLEAN_ARRAY:
 *
 * Expands to a function call returning the #GType of a #GArray of #gboolean
 * (corresponding to the D-Bus signature "ab").
 */

/**
 * DBUS_TYPE_G_INT_ARRAY:
 *
 * Expands to a function call returning the #GType of a #GArray of #gint
 * (corresponding to the D-Bus signature "ai").
 */

/**
 * DBUS_TYPE_G_UINT_ARRAY:
 *
 * Expands to a function call returning the #GType of a #GArray of #guint
 * (corresponding to the D-Bus signature "au").
 */

/**
 * DBUS_TYPE_G_INT64_ARRAY:
 *
 * Expands to a function call returning the #GType of a #GArray of #gint64
 * (corresponding to the D-Bus signature "ax").
 */

/**
 * DBUS_TYPE_G_UINT64_ARRAY:
 *
 * Expands to a function call returning the #GType of a #GArray of #guint64
 * (corresponding to the D-Bus signature "at").
 */

/**
 * DBUS_TYPE_G_UCHAR_ARRAY:
 *
 * Expands to a function call returning the #GType of a #GArray of #guchar
 * (corresponding to the D-Bus signature "ay").
 *
 * Note that this is not the same thing as a #GByteArray! dbus-glib does not
 * know about the #GByteArray type.
 */

/**
 * DBUS_TYPE_G_OBJECT_ARRAY:
 *
 * Expands to a function call returning the #GType of a #GPtrArray of #GObject.
 *
 * Use this type with caution: it can sometimes be used as a representation
 * of arrays whose D-Bus signature is "ao" (transferred as an array of object
 * paths), but the conventional type for such arrays is
 * <literal>(dbus_g_type_get_collection ("GPtrArray",
 * DBUS_TYPE_G_OBJECT_PATH))</literal>.
 */

/**
 * DBUS_TYPE_G_STRING_STRING_HASHTABLE:
 *
 * Expands to a function call returning the #GType of a #GHashTable where the
 * keys are strings and the values are also strings (corresponding to the
 * D-Bus signature "a{ss}").
 */

/**
 * DBusGTypeSpecializedVtable:
 * @constructor: returns a new, blank instance of the @type
 * @free_func: if not %NULL, frees the @type instance @val
 * @copy_func: returns a "deep copy" of the @type instance @val
 * @simple_free_func: if not %NULL, frees its argument
 *
 * A table of methods used to implement specialized container behaviour on
 * user-defined collections, maps and structs. Exactly one of @free_func and
 * @simple_free_func must be implemented; the other must be %NULL.
 * @constructor and @copy_func must always be implemented.
 *
 * There are additional members, which are reserved for future expansion and
 * must be %NULL.
 */

/**
 * DBusGTypeSpecializedConstructor:
 * @type: a specialized boxed type
 *
 * <!-- -->
 *
 * Returns: a new instance of @type
 */

/**
 * DBusGTypeSpecializedFreeFunc:
 * @type: a specialized boxed type
 * @val: an instance of @type
 *
 * Frees @val according to @type. This is analogous to #GBoxedFreeFunc, but
 * can use information from @type (for instance to free the contents of a
 * container before freeing the actual container).
 */

/**
 * DBusGTypeSpecializedCopyFunc:
 * @type: a specialized boxed type
 * @src: an instance of @type
 *
 * Copies @src according to @type. This is analogous to #GBoxedCopyFunc, but
 * can use information from @type (for instance to copy each element of a
 * collection).
 *
 * Returns: a deep copy of @src
 */

/**
 * DBusGTypeSpecializedCollectionFixedAccessorFunc:
 * @type: a specialized collection boxed type
 * @instance: an instance of @type
 * @values: used to return a pointer to the contents of @instance
 * @len: used to return the number of elements in @instance
 *
 * Implements dbus_g_type_collection_get_fixed() for a #GValue with type
 * @type, containing @instance.
 *
 * Returns: %TRUE on success
 */

/**
 * DBusGTypeSpecializedCollectionIterator:
 * @value: an element of the collection
 * @user_data: the data supplied when calling
 *    dbus_g_type_collection_value_iterate()
 *
 * A library-user-supplied function, called for each element in the
 * collection when dbus_g_type_collection_value_iterate() is called.
 */

/**
 * DBusGTypeSpecializedCollectionIteratorFunc:
 * @type: a specialized collection boxed type
 * @instance: an instance of @type
 * @iterator: the function to call for each element
 * @user_data: data to pass to @iterator
 *
 * Implements dbus_g_type_collection_value_iterate() for a #GValue with
 * type @type, containing @instance.
 */

/**
 * DBusGTypeSpecializedCollectionAppendFunc:
 * @ctx: an appending context returned by dbus_g_type_specialized_init_append()
 * @val: a value to copy into the collection
 *
 * Implements dbus_g_type_specialized_collection_append().
 *
 * This function should use the @val and @specialization_type members of @ctx.
 */

/**
 * DBusGTypeSpecializedCollectionEndAppendFunc:
 * @ctx: an appending context returned by dbus_g_type_specialized_init_append()
 *
 * Implements dbus_g_type_specialized_collection_end_append().
 *
 * This function should use the @val and @specialization_type members of @ctx.
 */

/**
 * DBusGTypeSpecializedCollectionVtable:
 * @base_vtable: base methods shared between collections and other types
 * @fixed_accessor: if not %NULL, provides access to the contents of this
 *    collection, as documented for dbus_g_type_collection_get_fixed()
 * @iterator: iterates through the members of @instance
 * @append_func: appends a new member to @instance
 * @end_append_func: if not %NULL, called after each group of calls to
 *    the @append_func
 *
 * A table of methods used to implement specialized collection behaviour
 * on user-defined types. At least @iterator and @append_func must be
 * implemented.
 */

/**
 * DBusGTypeSpecializedMapIterator:
 * @key_val: a key from the map
 * @value_val: a value from the map
 * @user_data: the data supplied when calling
 *    dbus_g_type_map_value_iterate()
 *
 * A library-user-supplied function, called for each key/value pair in the
 * collection when dbus_g_type_map_value_iterate() is called.
 */

/**
 * DBusGTypeSpecializedMapIteratorFunc:
 * @type: a specialized map boxed type
 * @instance: an instance of @type
 * @iterator: the function to call for each key/value pair
 * @user_data: data to pass to @iterator
 *
 * Implements dbus_g_type_map_value_iterate() for a #GValue with
 * type @type, containing @instance.
 */

/**
 * DBusGTypeSpecializedMapAppendFunc:
 * @ctx: an appending context returned by dbus_g_type_specialized_init_append()
 * @key: a key to add to the collection
 * @val: a value to add to the collection
 *
 * Implements dbus_g_type_specialized_map_append().
 *
 * This function should use the @val and @specialization_type members of @ctx,
 * and replace any existing value with key equal to @key.
 */

/**
 * DBusGTypeSpecializedMapVtable:
 * @base_vtable: base methods shared between maps and other types
 * @iterator: iterates through the members of @instance
 * @append_func: adds a new key/value pair to @instance
 *
 * A table of methods used to implement specialized collection behaviour
 * on user-defined types. Both methods must be implemented.
 */

/**
 * DBusGTypeSpecializedStructGetMember:
 * @type: a specialized struct boxed type
 * @instance: an instance of @type
 * @member: the index of the member, starting from 0
 * @ret_value: an initialized #GValue of the appropriate type for the given
 *    member of @type
 *
 * Implements dbus_g_type_struct_get_member() for a #GValue with type @type,
 * containing @instance.
 *
 * Returns: %TRUE on success
 */

/**
 * DBusGTypeSpecializedStructSetMember:
 * @type: a specialized struct boxed type
 * @instance: an instance of @type
 * @member: the index of the member, starting from 0
 * @new_value: an initialized #GValue of the appropriate type for the given
 *    member of @type
 *
 * Implements dbus_g_type_struct_set_member() for a #GValue with type @type,
 * containing @instance.
 *
 * Returns: %TRUE on success
 */

/**
 * DBusGTypeSpecializedStructVtable:
 * @base_vtable: base methods shared between maps and other types
 * @get_member: returns a member by its index
 * @set_member: sets a member by its index
 *
 * A table of methods used to implement specialized collection behaviour
 * on user-defined types. Both methods must be implemented.
 */

typedef enum {
  DBUS_G_SPECTYPE_COLLECTION,
  DBUS_G_SPECTYPE_MAP,
  DBUS_G_SPECTYPE_STRUCT
} DBusGTypeSpecializedType;

typedef struct {
  DBusGTypeSpecializedType type;
  const DBusGTypeSpecializedVtable     *vtable;
} DBusGTypeSpecializedContainer;

typedef struct {
  guint num_types;
  GType *types;
  const DBusGTypeSpecializedContainer     *klass;
} DBusGTypeSpecializedData;

static GHashTable /* char * -> data* */ *specialized_containers;

static GQuark
specialized_type_data_quark ()
{
  static GQuark quark;
  if (!quark)
    quark = g_quark_from_static_string ("DBusGTypeSpecializedData");
  
  return quark;
}

static gpointer
specialized_init (gpointer arg G_GNUC_UNUSED)
{
  g_assert (specialized_containers == NULL);

  specialized_containers = g_hash_table_new_full (g_str_hash, g_str_equal,
      g_free, NULL);

  _dbus_g_type_specialized_builtins_init ();
  return NULL;
}

/**
 * dbus_g_type_specialized_init:
 *
 * Initialize dbus-glib specialized #GType<!-- -->s.
 *
 * In older versions of dbus-glib, it was necessary to do this before
 * instantiating or registering any specialized type. It is now done
 * automatically whenever necessary.
 */
void
dbus_g_type_specialized_init (void)
{
  static GOnce once = G_ONCE_INIT;

  g_once (&once, specialized_init, NULL);
}

static DBusGTypeSpecializedData *
lookup_specialization_data (GType type)
{
  return g_type_get_qdata (type, specialized_type_data_quark ());
}


/* Copied from gboxed.c */
static void
proxy_value_init (GValue *value)
{
  value->data[0].v_pointer = NULL;
}

/* Adapted from gboxed.c */
static void
proxy_value_free (GValue *value)
{
  if (value->data[0].v_pointer && !(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS))
    {
      DBusGTypeSpecializedData *data;
      GType type;

      type = G_VALUE_TYPE (value);
      data = lookup_specialization_data (type);
      g_assert (data != NULL);

      if (data->klass->vtable->free_func)
        {
          data->klass->vtable->free_func (type, value->data[0].v_pointer);
        }
      else
        {
          g_assert (data->klass->vtable->simple_free_func != NULL);
          data->klass->vtable->simple_free_func (value->data[0].v_pointer);
        }
    }
}

/* Adapted from gboxed.c */
static void
proxy_value_copy (const GValue *src_value,
		  GValue       *dest_value)
{
  if (src_value->data[0].v_pointer)
    {
      DBusGTypeSpecializedData *data;
      GType type;
      type = G_VALUE_TYPE (src_value);
      data = lookup_specialization_data (type);
      g_assert (data != NULL);
      dest_value->data[0].v_pointer = data->klass->vtable->copy_func (type, src_value->data[0].v_pointer);
    }
  else
    dest_value->data[0].v_pointer = src_value->data[0].v_pointer;
}

/* Copied from gboxed.c */
static gpointer
proxy_value_peek_pointer (const GValue *value)
{
  return value->data[0].v_pointer;
}

/* Adapted from gboxed.c */
static gchar*
proxy_collect_value (GValue      *value,
		     guint        n_collect_values,
		     GTypeCValue *collect_values,
		     guint        collect_flags)
{
  DBusGTypeSpecializedData *data;
  GType type;

  type = G_VALUE_TYPE (value);
  data = lookup_specialization_data (type);

  if (!collect_values[0].v_pointer)
    value->data[0].v_pointer = NULL;
  else
    {
      if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
	{
	  value->data[0].v_pointer = collect_values[0].v_pointer;
	  value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
	}
      else
        {
	  value->data[0].v_pointer = data->klass->vtable->copy_func (type, collect_values[0].v_pointer);
        }
    }

  return NULL;
}

/* Adapted from gboxed.c */
static gchar*
proxy_lcopy_value (const GValue *value,
		   guint         n_collect_values,
		   GTypeCValue  *collect_values,
		   guint         collect_flags)
{
  gpointer *boxed_p = collect_values[0].v_pointer;

  if (!boxed_p)
    return g_strdup_printf ("value location for `%s' passed as NULL", G_VALUE_TYPE_NAME (value));

  if (!value->data[0].v_pointer)
    *boxed_p = NULL;
  else if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
    *boxed_p = value->data[0].v_pointer;
  else
    {
      DBusGTypeSpecializedData *data;
      GType type;

      type = G_VALUE_TYPE (value);
      data = lookup_specialization_data (type);

      *boxed_p = data->klass->vtable->copy_func (type, value->data[0].v_pointer);
    }

  return NULL;
}

static char *
build_specialization_name (const char *prefix, guint num_types, const GType *types)
{
  GString *fullname;
  guint i;

  fullname = g_string_new (prefix);

  g_string_append_c (fullname, '_');
  for (i=0; i < num_types; i++)
    {
      if (i!=0)
        g_string_append_c (fullname, '+');
      g_string_append (fullname, g_type_name (types[i]));
    }
  g_string_append_c (fullname, '_');
  return g_string_free (fullname, FALSE);
}

static void
register_container (const char                         *name,
		    DBusGTypeSpecializedType            type,
		    const DBusGTypeSpecializedVtable   *vtable)
{
  DBusGTypeSpecializedContainer *klass;

  g_warn_if_fail (vtable->constructor != NULL);
  /* must have either free_func or simple_free_func */
  g_warn_if_fail (vtable->free_func != NULL ||
                  vtable->simple_free_func != NULL);
  g_warn_if_fail (vtable->copy_func != NULL);

  klass = g_new0 (DBusGTypeSpecializedContainer, 1);
  klass->type = type;
  klass->vtable = vtable;

  g_hash_table_insert (specialized_containers, g_strdup (name), klass);
}

/**
 * dbus_g_type_register_collection:
 * @name: The name of a new collection container
 * @vtable: the vtable defining the new container
 * @flags: As yet unused.
 *
 * Defines a new collection container.
 */
void
dbus_g_type_register_collection (const char                                   *name,
				 const DBusGTypeSpecializedCollectionVtable   *vtable,
				 guint                                         flags)
{
  dbus_g_type_specialized_init();

  _dbus_g_type_register_collection (name, vtable, flags);
}

void
_dbus_g_type_register_collection (const char                                   *name,
				  const DBusGTypeSpecializedCollectionVtable   *vtable,
				  guint                                         flags)
{
  /* fixed_accessor is optional */
  g_warn_if_fail (vtable->iterator != NULL);
  g_warn_if_fail (vtable->append_func != NULL);
  /* end_append_func is optional */

  register_container (name, DBUS_G_SPECTYPE_COLLECTION, (const DBusGTypeSpecializedVtable*) vtable);
}

/**
 * dbus_g_type_register_map:
 * @name: The name of a new map container
 * @vtable: the vtable defining the new container
 * @flags: As yet unused.
 *
 * Defines a new map container.
 */
void
dbus_g_type_register_map (const char                            *name,
			  const DBusGTypeSpecializedMapVtable   *vtable,
			  guint                                  flags)
{
  dbus_g_type_specialized_init();

  _dbus_g_type_register_map (name, vtable, flags);
}

void
_dbus_g_type_register_map (const char                            *name,
			   const DBusGTypeSpecializedMapVtable   *vtable,
			   guint                                  flags)
{
  g_warn_if_fail (vtable->iterator != NULL);
  g_warn_if_fail (vtable->append_func != NULL);

  register_container (name, DBUS_G_SPECTYPE_MAP, (const DBusGTypeSpecializedVtable*) vtable);
}

/**
 * dbus_g_type_register_struct:
 * @name: The name of a new struct container
 * @vtable: the vtable defining the new container
 * @flags: As yet unused.
 *
 * Defines a new struct container.
 */
void
dbus_g_type_register_struct (const char                             *name,
			     const DBusGTypeSpecializedStructVtable *vtable,
			     guint                                   flags)
{
  dbus_g_type_specialized_init();

  _dbus_g_type_register_struct (name, vtable, flags);
}

void
_dbus_g_type_register_struct (const char                             *name,
			      const DBusGTypeSpecializedStructVtable *vtable,
			      guint                                   flags)
{
  g_warn_if_fail (vtable->get_member != NULL);
  g_warn_if_fail (vtable->set_member != NULL);

  register_container (name, DBUS_G_SPECTYPE_STRUCT, (const DBusGTypeSpecializedVtable*) vtable);
}

/**
 * dbus_g_type_map_peek_vtable:
 * @map_type: a gtype of a map specialization
 *
 * Peek the vtable for a given map specialization
 *
 * Returns: the vtable
 */
const DBusGTypeSpecializedMapVtable* dbus_g_type_map_peek_vtable (GType map_type)
{
  DBusGTypeSpecializedData *data;
  g_return_val_if_fail (dbus_g_type_is_map(map_type), NULL);

  data = lookup_specialization_data (map_type);
  g_assert (data != NULL);

  return (DBusGTypeSpecializedMapVtable *)(data->klass->vtable);
}

/**
 * dbus_g_type_collection_peek_vtable:
 * @collection_type: a gtype of a collection specialization
 *
 * Peek the vtable for a given collection specialization
 *
 * Returns: the vtable
 */
const DBusGTypeSpecializedCollectionVtable* dbus_g_type_collection_peek_vtable (GType collection_type)
{
  DBusGTypeSpecializedData *data;
  g_return_val_if_fail (dbus_g_type_is_collection(collection_type), NULL);

  data = lookup_specialization_data (collection_type);
  g_assert (data != NULL);

  return (DBusGTypeSpecializedCollectionVtable *)(data->klass->vtable);
}

/**
 * dbus_g_type_struct_peek_vtable:
 * @struct_type: a gtype of a struct specialization
 *
 * Peek the vtable for a given struct specialization
 *
 * Returns: the vtable
 */
const DBusGTypeSpecializedStructVtable* dbus_g_type_struct_peek_vtable (GType struct_type)
{
  DBusGTypeSpecializedData *data;
  g_return_val_if_fail (dbus_g_type_is_struct (struct_type), NULL);

  data = lookup_specialization_data (struct_type);
  g_assert (data != NULL);

  return (DBusGTypeSpecializedStructVtable *)(data->klass->vtable);
}

static GType
register_specialized_instance (const DBusGTypeSpecializedContainer   *klass,
			       const char                            *name,
			       guint                                  num_types,
			       const GType                           *types)
{
  GType ret;
  
  static const GTypeValueTable vtable =
    {
      proxy_value_init,
      proxy_value_free,
      proxy_value_copy,
      proxy_value_peek_pointer,
      "p",
      proxy_collect_value,
      "p",
      proxy_lcopy_value,
    };
  static const GTypeInfo derived_info =
    {
      0,		/* class_size */
      NULL,		/* base_init */
      NULL,		/* base_finalize */
      NULL,		/* class_init */
      NULL,		/* class_finalize */
      NULL,		/* class_data */
      0,		/* instance_size */
      0,		/* n_preallocs */
      NULL,		/* instance_init */
      &vtable,		/* value_table */
    };

  ret = g_type_register_static (G_TYPE_BOXED, name, &derived_info, 0);
  /* install proxy functions upon successfull registration */
  if (ret != G_TYPE_INVALID)
    {
      DBusGTypeSpecializedData *data;
      data = g_new0 (DBusGTypeSpecializedData, 1);
      data->num_types = num_types;
      data->types = g_memdup (types, sizeof (GType) * num_types);
      data->klass = klass;
      g_type_set_qdata (ret, specialized_type_data_quark (), data);
    }

  return ret;
}

static GType
lookup_or_register_specialized (const char  *container,
				guint        num_types,
				const GType *types)
{
  GType ret;
  char *name;
  const DBusGTypeSpecializedContainer *klass;

  dbus_g_type_specialized_init();

  klass = g_hash_table_lookup (specialized_containers, container);
  g_return_val_if_fail (klass != NULL, G_TYPE_INVALID);

  name = build_specialization_name (container, num_types, types);
  ret = g_type_from_name (name);
  if (ret == G_TYPE_INVALID)
    {
      /* Take ownership of name */
      ret = register_specialized_instance (klass, name,
					   num_types,
					   types);
    }
  g_free (name);
  return ret;
}


/**
 * dbus_g_type_get_collection:
 * @container: a string specifying a registered collection type
 * @specialization: #GType of collection elements
 *
 * Gets a #GType for a particular collection instance, 
 * creating the type if not already created.
 *
 * Returns: the #GType of that instance
 */
GType
dbus_g_type_get_collection (const char *container,
			    GType specialization)
{
  return lookup_or_register_specialized (container, 1, &specialization);
}

/**
 * dbus_g_type_get_map:
 * @container: a string specifying a registered map type
 * @key_specialization: #GType of keys
 * @value_specialization: #GType of values
 *
 * Gets a #GType for a particular map instance,
 * creating the type if not already created.
 *
 * Returns: the #GType of that instance
 */
GType
dbus_g_type_get_map (const char   *container,
		     GType         key_specialization,
		     GType         value_specialization)
{
  GType types[2];
  types[0] = key_specialization;
  types[1] = value_specialization;
  return lookup_or_register_specialized (container, 2, types);
}

/**
 * dbus_g_type_get_structv:
 * @container: a string specifying a registered struct type
 * @num_members: number of members in the struct
 * @types: an array specufying a GType for each struct element
 *
 * Gets a #GType for a particular struct instance,
 * creating the type if not already created.
 *
 * Returns: the #GType of that instance
 */
GType
dbus_g_type_get_structv (const char   *container,
                         guint         num_members,
                         GType        *types)
{
  return lookup_or_register_specialized (container, num_members, types);
}

/**
 * dbus_g_type_get_struct:
 * @container: a string specifying a registered struct type
 * @first_type: #GType for the struct's first member
 * @...: more GTypes for the struct's members, terminated by G_TYPE_INVALID
 *
 * Varags methsod to get a #GType for a particular struct instance,
 * creating the type if not already created.
 *
 * Returns: the #GType of that instance
 */
GType
dbus_g_type_get_struct (const char *container,
                        GType first_type,
                        ...)
{
  GArray *types;
  GType curtype, ret;
  va_list args;
  va_start (args, first_type);

  types = g_array_new (FALSE, FALSE, sizeof (GType));
  curtype = first_type;
  while (curtype != G_TYPE_INVALID)
    {
      g_array_append_val (types, curtype);
      curtype = va_arg (args, GType);
    }
  va_end (args);

  ret = lookup_or_register_specialized (container, types->len,
      (GType *) types->data);

  g_array_free (types, TRUE);

  return ret;
}


/**
 * dbus_g_type_is_collection:
 * @gtype: a GType to test
 *
 * Tests if a given GType is a collection.
 *
 * Returns: true if the given GType is a collection
 */
gboolean
dbus_g_type_is_collection (GType gtype)
{
  DBusGTypeSpecializedData *data;
  data = lookup_specialization_data (gtype);
  if (data == NULL)
    return FALSE;
  return data->klass->type == DBUS_G_SPECTYPE_COLLECTION;
}

/**
 * dbus_g_type_is_map:
 * @gtype: a GType to test
 *
 * Tests if a given GType is a map,
 * i.e. it was created with dbus_g_type_get_map().
 *
 * Returns: true if the given GType is a map
 */
gboolean
dbus_g_type_is_map (GType gtype)
{
  DBusGTypeSpecializedData *data;
  data = lookup_specialization_data (gtype);
  if (data == NULL)
    return FALSE;
  return data->klass->type == DBUS_G_SPECTYPE_MAP;
}

/**
 * dbus_g_type_is_struct:
 * @gtype: a GType to test
 *
 * Tests if a given GType is a struct,
 * i.e. it was created with dbus_g_type_get_struct()
 *
 * Returns: true if the given GType is a struct
 */
gboolean
dbus_g_type_is_struct (GType gtype)
{
  DBusGTypeSpecializedData *data;
  data = lookup_specialization_data (gtype);
  if (data == NULL)
    return FALSE;
  return data->klass->type == DBUS_G_SPECTYPE_STRUCT;
}


static GType
get_specialization_index (GType gtype, guint i)
{
  DBusGTypeSpecializedData *data;

  data = lookup_specialization_data (gtype);
  if (i < data->num_types)
    return data->types[i];
  else
    return G_TYPE_INVALID;
}

/**
 * dbus_g_type_get_collection_specialization:
 * @gtype: a collection #GType, as created by dbus_g_type_get_collection()
 *
 * Return the type of each element in collections of type @gtype.
 * It is an error to call this function on a non-collection type.
 *
 * Returns: the element type for a given collection GType.
 */
GType
dbus_g_type_get_collection_specialization (GType gtype)
{
  g_return_val_if_fail (dbus_g_type_is_collection (gtype), G_TYPE_INVALID);
  return get_specialization_index (gtype, 0);
}

/**
 * dbus_g_type_get_map_key_specialization:
 * @gtype: a map #GType, as created by dbus_g_type_get_map()
 *
 * Return the type of the keys in maps of type @gtype.
 * It is an error to call this function on a non-map type.
 *
 * Returns: the key type for a given map #GType.
 */
GType
dbus_g_type_get_map_key_specialization (GType gtype)
{
  g_return_val_if_fail (dbus_g_type_is_map (gtype), G_TYPE_INVALID);
  return get_specialization_index (gtype, 0);
}

/**
 * dbus_g_type_get_map_value_specialization:
 * @gtype: a map GType, as created by dbus_g_type_get_map().
 *
 * Return the type of the values in maps of type @gtype.
 * It is an error to call this function on a non-map type.
 *
 * Returns: the value type for a given map GType.
 */
GType
dbus_g_type_get_map_value_specialization (GType gtype)
{
  g_return_val_if_fail (dbus_g_type_is_map (gtype), G_TYPE_INVALID);
  return get_specialization_index (gtype, 1);
}

/**
 * dbus_g_type_get_struct_member_type
 * @gtype: a struct GType, as created with dbus_g_type_get_struct()
 * @member: the index of a struct member
 *
 * Get the type of a member of a specialized struct.
 * It is an error to call this function on a non-struct type.
 *
 * Returns: the type for a given member of a struct #GType,
 *    or %G_TYPE_INVALID if @member &gt;= dbus_g_type_get_struct_size()
 */
GType
dbus_g_type_get_struct_member_type (GType gtype, guint member)
{
  g_return_val_if_fail (dbus_g_type_is_struct (gtype), G_TYPE_INVALID);
  return get_specialization_index (gtype, member);
}

/**
 * dbus_g_type_get_struct_size
 * @gtype: a struct GType, as created with dbus_g_type_get_struct().
 *
 * Get the number of members in a specialized struct.
 * It is an error to call this function on a non-struct type.
 *
 * Returns: the number of members in a given struct #GType.
 */
guint
dbus_g_type_get_struct_size (GType gtype)
{
  DBusGTypeSpecializedData *data;
  g_return_val_if_fail (dbus_g_type_is_struct (gtype), G_TYPE_INVALID);

  data = lookup_specialization_data (gtype);
  return data->num_types;
}

/**
 * dbus_g_type_specialized_construct:
 * @gtype: a specialized #GType, as created by dbus_g_type_get_collection(),
 *  dbus_g_type_get_map() or dbus_g_type_get_struct()
 *
 * Create an instance of a given specialized type. 
 * The structure created and returned will depend on the container type of the 
 * GType. E.g. If the given type was created by 
 * dbus_g_type_get_collection("GArray", G_TYPE_INT), 
 * then this will return a GArray with element_size of sizeof(int)
 *
 * Returns: a pointer to a newly constructed instance of the given type.
 */
gpointer
dbus_g_type_specialized_construct (GType gtype)
{
  DBusGTypeSpecializedData *data;

  dbus_g_type_specialized_init();

  data = lookup_specialization_data (gtype);
  g_return_val_if_fail (data != NULL, FALSE);

  return data->klass->vtable->constructor (gtype);
}

/**
 * dbus_g_type_collection_get_fixed:
 * @value: a GValue containing a boxed specialized collection
 *    that has a @fixed_accessor in its vtable
 * @data_ret: used to return a pointer to the fixed data, which must not be
 *    modified (for instance, for a #GArray of #gint, this would point
 *    to an array of #gint)
 * @len_ret: used to return the length (counting collection elements, not
 *    bytes: in a #GArray containing one #gint, this would be 1)
 *
 * Calling this function is likely to be a bad idea. Consider using
 * dbus_g_type_collection_value_iterate() instead.
 *
 * On success, @data_ret is a pointer to the underlying data in a collection
 * of fixed-length fundamental types. Knowledge of the underlying data model
 * of the collection is needed in order to use @data_ret correctly.
 *
 * It is an error to call this function on a specialized type that is not a
 * collection, or on a collection that does not have a @fixed_accessor in its
 * #DBusGTypeSpecializedCollectionVtable.
 *
 * Specialized #GArray<!---->s are the only types provided by dbus-glib that
 * can be used with this function; user-defined types might also work.
 *
 * Returns: %TRUE on success
 */
gboolean
dbus_g_type_collection_get_fixed (GValue   *value,
				  gpointer *data_ret,
				  guint    *len_ret)
{
  DBusGTypeSpecializedData *data;
  DBusGTypeSpecializedCollectionVtable *vtable;
  GType gtype;

  dbus_g_type_specialized_init();

  g_return_val_if_fail (G_VALUE_HOLDS_BOXED (value), FALSE);

  gtype = G_VALUE_TYPE (value);
  g_return_val_if_fail (dbus_g_type_is_collection (gtype), FALSE);

  data = lookup_specialization_data (gtype);
  /* dbus_g_type_is_collection() already checked this */
  g_assert (data != NULL);

  vtable = (DBusGTypeSpecializedCollectionVtable *) (data->klass->vtable);
  g_return_val_if_fail (vtable->fixed_accessor != NULL, FALSE);

  return vtable->fixed_accessor (gtype, g_value_get_boxed (value),
                                 data_ret, len_ret);
}

/**
 * dbus_g_type_collection_value_iterate:
 * @value: a #GValue holding a collection type.
 * @iterator: a function to call for each element
 * @user_data: user data to pass to the @iterator
 *
 * Calls the given function for each element of the collection. 
 * The function is passed a #GValue containing the element and the given 
 * @user_data parameter. The collection may not be modified while iterating over 
 * it.
 */
void
dbus_g_type_collection_value_iterate (const GValue                           *value,
				      DBusGTypeSpecializedCollectionIterator  iterator,
				      gpointer                                user_data)
{
  DBusGTypeSpecializedData *data;
  GType gtype;

  dbus_g_type_specialized_init();

  g_return_if_fail (G_VALUE_HOLDS_BOXED (value));

  gtype = G_VALUE_TYPE (value);
  g_return_if_fail (dbus_g_type_is_collection (gtype));

  data = lookup_specialization_data (gtype);
  /* dbus_g_type_is_collection() already checked this */
  g_assert (data != NULL);

  ((DBusGTypeSpecializedCollectionVtable *) data->klass->vtable)->iterator (gtype,
									    g_value_get_boxed (value),
									    iterator, user_data);
}

/**
 * DBusGTypeSpecializedAppendContext:
 * @val: the #GValue containing the array to which you're appending
 * @specialization_type: the #GType of the array's elements
 *
 * A context for appending. There are more fields, which are private.
 */

typedef struct {
  GValue *val;
  GType specialization_type;
  DBusGTypeSpecializedData *specdata;
  guint c;
  gpointer d;
} DBusGTypeSpecializedAppendContextReal;

G_STATIC_ASSERT (sizeof (DBusGTypeSpecializedAppendContextReal) ==
                 sizeof (DBusGTypeSpecializedAppendContext));

/**
 * dbus_g_type_specialized_init_append:
 * @value: a #GValue containing an instance of specialized type
 * @ctx: a #DBusGTypeSpecializedAppendContext in which to return a new appending context.
 *
 * Create a new context for adding elements to a collection or key/value pairs 
 * to a map. You generally don't need or want to use this..
 */
void
dbus_g_type_specialized_init_append (GValue *value, DBusGTypeSpecializedAppendContext *ctx)
{
  DBusGTypeSpecializedAppendContextReal *realctx = (DBusGTypeSpecializedAppendContextReal *) ctx;
  GType gtype;
  DBusGTypeSpecializedData *specdata;
  
  dbus_g_type_specialized_init();

  g_return_if_fail (G_VALUE_HOLDS_BOXED (value));
  gtype = G_VALUE_TYPE (value);
  specdata = lookup_specialization_data (gtype);
  g_return_if_fail (specdata != NULL);
  g_return_if_fail (specdata->num_types != 0);

  realctx->val = value;
  realctx->specialization_type = specdata->types[0];
  realctx->specdata = specdata;
}

/**
 * dbus_g_type_specialized_collection_append:
 * @ctx: a context created by dbus_g_type_specialized_init_append()
 *    for a #GValue containing a collection
 * @elt: a #GValue containing an element to append to the collection
 *
 * Appends a given element to the end of a collection.
 */
void
dbus_g_type_specialized_collection_append (DBusGTypeSpecializedAppendContext *ctx,
					   GValue                            *elt)
{
  DBusGTypeSpecializedAppendContextReal *realctx = (DBusGTypeSpecializedAppendContextReal *) ctx;

  g_return_if_fail (dbus_g_type_is_collection (G_VALUE_TYPE (ctx->val)));

  ((DBusGTypeSpecializedCollectionVtable *) realctx->specdata->klass->vtable)->append_func (ctx, elt);
}

/**
 * dbus_g_type_specialized_collection_end_append:
 * @ctx: a context created by dbus_g_type_specialized_init_append()
 *    for a #GValue containing a collection
 *
 * Finish appending elements to a given collection
 */
void
dbus_g_type_specialized_collection_end_append (DBusGTypeSpecializedAppendContext *ctx)
{
  DBusGTypeSpecializedAppendContextReal *realctx = (DBusGTypeSpecializedAppendContextReal *) ctx;

  g_return_if_fail (dbus_g_type_is_collection (G_VALUE_TYPE (ctx->val)));

  if (((DBusGTypeSpecializedCollectionVtable *) realctx->specdata->klass->vtable)->end_append_func != NULL)
    ((DBusGTypeSpecializedCollectionVtable *) realctx->specdata->klass->vtable)->end_append_func (ctx);
}

/**
 * dbus_g_type_specialized_map_append:
 * @ctx: a context created by dbus_g_type_specialized_init_append()
 *    for a #GValue containing a map
 * @key: a GValue containing a key, whose contents will be stolen by @ctx
 * @val: a GValue containing a value, whose contents will be stolen by @ctx
 *
 * Inserts the given key/value pair into the map instance.
 */
void
dbus_g_type_specialized_map_append (DBusGTypeSpecializedAppendContext *ctx,
				    GValue                            *key,
				    GValue                            *val)
{
  DBusGTypeSpecializedAppendContextReal *realctx = (DBusGTypeSpecializedAppendContextReal *) ctx;

  g_return_if_fail (dbus_g_type_is_map (G_VALUE_TYPE (ctx->val)));

  ((DBusGTypeSpecializedMapVtable *) realctx->specdata->klass->vtable)->append_func (ctx, key, val);
}


/**
 * dbus_g_type_map_value_iterate:
 * @value: a #GValue holding a specialized map
 * @iterator: a function to call for each element
 * @user_data: user data to pass to the @iterator
 *
 * Calls the given function for each key/value pair of the map. 
 * The function is passed two GValues containing the key/value pair and the given 
 * @user_data parameter. The map may not be modified while iterating over 
 * it.
 */
void
dbus_g_type_map_value_iterate (const GValue                           *value,
			       DBusGTypeSpecializedMapIterator         iterator,
			       gpointer                                user_data)
{
  DBusGTypeSpecializedData *data;
  GType gtype;

  dbus_g_type_specialized_init();

  g_return_if_fail (G_VALUE_HOLDS_BOXED (value));

  gtype = G_VALUE_TYPE (value);
  g_return_if_fail (dbus_g_type_is_map (gtype));

  data = lookup_specialization_data (gtype);
  /* already checked by dbus_g_type_is_map() */
  g_assert (data != NULL);

  ((DBusGTypeSpecializedMapVtable *) data->klass->vtable)->iterator (gtype,
								     g_value_get_boxed (value),
								     iterator, user_data);
}

/**
 * dbus_g_type_struct_get_member:
 * @value: a #GValue containing a struct instance
 * @member: the index of a given member
 * @dest: an initialised #GValue in which to return the struct member
 *
 * Fetches a given member of a given struct instance. @dest must be initialised 
 * was the correct type for that member, e.g. as returned by 
 * @dbus_g_type_get_struct_member_type
 *
 * Returns: %TRUE if successful
 */
gboolean
dbus_g_type_struct_get_member (const GValue *value,
			       guint         member,
			       GValue       *dest)
{
  DBusGTypeSpecializedData *data;
  GType gtype;

  dbus_g_type_specialized_init();

  g_return_val_if_fail (G_VALUE_HOLDS_BOXED (value), FALSE);

  gtype = G_VALUE_TYPE (value);
  g_return_val_if_fail (dbus_g_type_is_struct (gtype), FALSE);

  data = lookup_specialization_data (gtype);
  /* already checked by dbus_g_type_is_struct() */
  g_assert (data != NULL);

  return ((DBusGTypeSpecializedStructVtable *) (data->klass->vtable))->get_member(gtype,
											   g_value_get_boxed (value),
											   member, dest);
}

/**
 * dbus_g_type_struct_set_member:
 * @value: a #GValue containing a struct instance
 * @member: the index of a given member
 * @src: an #GValue containing the new value for that struct member
 *
 * Sets a given member of a struct to a new value. The type of @src must match
 * the existing type of @member member of the struct.
 *
 * Returns: %TRUE if successful
 */
gboolean
dbus_g_type_struct_set_member (GValue       *value,
			       guint         member,
			       const GValue *src)
{
  DBusGTypeSpecializedData *data;
  GType gtype;

  dbus_g_type_specialized_init();

  g_return_val_if_fail (G_VALUE_HOLDS_BOXED (value), FALSE);

  gtype = G_VALUE_TYPE (value);
  g_return_val_if_fail (dbus_g_type_is_struct (gtype), FALSE);

  data = lookup_specialization_data (gtype);
  /* already checked by dbus_g_type_is_struct() */
  g_assert (data != NULL);

  return ((DBusGTypeSpecializedStructVtable *) (data->klass->vtable))->set_member(gtype,
											   g_value_get_boxed (value),
											   member, src);
}

/**
 * dbus_g_type_struct_get:
 * @value: a #GValue containing a struct instance
 * @member: struct member to get
 * @...: location in which to return the value of this member,
 *       followed optionally by more member/return locations pairs, followed by
 *       by %G_MAXUINT
 *
 * Collects the selected values of this struct into the return locations
 * provided.
 *
 * Returns: %FALSE on failure
 */

gboolean
dbus_g_type_struct_get                   (const GValue *value,
                                          guint first_member,
                                          ...)
{
  va_list var_args;
  GType type;
  guint size,i;
  gchar *error;
  GValue val = {0,};

  g_return_val_if_fail (dbus_g_type_is_struct (G_VALUE_TYPE (value)), FALSE);

  va_start (var_args, first_member);
  size = dbus_g_type_get_struct_size (G_VALUE_TYPE (value));
  i = first_member;
  while (i != G_MAXUINT)
    {
      if (i >= size)
        goto error;

      type = dbus_g_type_get_struct_member_type (G_VALUE_TYPE (value),i);

      g_value_init (&val, type);
      dbus_g_type_struct_get_member (value, i, &val);

      G_VALUE_LCOPY (&val, var_args, 0, &error);
      if (error)
        {
          g_warning ("%s, %s", G_STRFUNC, error);
          g_free (error);
          g_value_unset (&val);
          goto error;
        }
      g_value_unset (&val);
      i = va_arg (var_args, guint);
    }
  va_end (var_args);
  return TRUE;
error:
  va_end (var_args);
  return FALSE;
}

/**
 * dbus_g_type_struct_set:
 * @value: a #GValue containing a struct instance
 * @member: struct member to set
 * @...: value for the first member, followed optionally by
 *       more member/value pairs, followed by %G_MAXUINT
 *
 * Sets the selected members of the struct in @value.
 *
 * Returns: %FALSE on failure
 */

gboolean
dbus_g_type_struct_set                   (GValue *value,
                                          guint first_member,
                                          ...)
{
  va_list var_args;
  GType type;
  guint size,i;
  gchar *error;
  GValue val = {0,};

  g_return_val_if_fail (dbus_g_type_is_struct (G_VALUE_TYPE (value)), FALSE);

  va_start (var_args, first_member);
  size = dbus_g_type_get_struct_size (G_VALUE_TYPE (value));
  i = first_member;
  while (i != G_MAXUINT)
    {
      if (i >= size)
        goto error;

      type = dbus_g_type_get_struct_member_type (G_VALUE_TYPE (value),i);

      g_value_init (&val, type);

      G_VALUE_COLLECT (&val, var_args, 0, &error);
      if (error)
        {
          g_warning ("%s, %s", G_STRFUNC, error);
          g_free (error);
          g_value_unset (&val);
          goto error;
        }

      dbus_g_type_struct_set_member (value, i, &val);

      g_value_unset (&val);
      i = va_arg (var_args, guint);
    }
  va_end (var_args);
  return TRUE;
error:
  va_end (var_args);
  return FALSE;
}

static void
_collection_iterator (const GValue *value,
    gpointer user_data)
{
  GPtrArray *children = user_data;

  g_ptr_array_add (children, dbus_g_value_build_g_variant (value));
}

static void
_map_iterator (const GValue *kvalue,
    const GValue *vvalue,
    gpointer user_data)
{
  GPtrArray *children = user_data;

  g_ptr_array_add (children,
      g_variant_new_dict_entry (
        dbus_g_value_build_g_variant (kvalue),
        dbus_g_value_build_g_variant (vvalue)));
}

static GVariantType *
dbus_g_value_type_build_g_variant_type (GType type)
{
  if (dbus_g_type_is_collection (type))
    {
      GType element_type = dbus_g_type_get_collection_specialization (type);
      GVariantType *element_sig = dbus_g_value_type_build_g_variant_type (
          element_type);
      GVariantType *ret = g_variant_type_new_array (element_sig);

      g_variant_type_free (element_sig);
      return ret;
    }
  else if (dbus_g_type_is_map (type))
    {
      GType key_type = dbus_g_type_get_map_key_specialization (type);
      GType value_type = dbus_g_type_get_map_value_specialization (type);
      GVariantType *key_sig = dbus_g_value_type_build_g_variant_type (
          key_type);
      GVariantType *value_sig = dbus_g_value_type_build_g_variant_type (
          value_type);
      GVariantType *entry_sig = g_variant_type_new_dict_entry (key_sig,
          value_sig);
      GVariantType *ret = g_variant_type_new_array (entry_sig);

      g_variant_type_free (key_sig);
      g_variant_type_free (value_sig);
      g_variant_type_free (entry_sig);
      return ret;
    }
  else if (dbus_g_type_is_struct (type))
    {
      guint size = dbus_g_type_get_struct_size (type);
      guint i;
      GVariantType **sigs = g_new0 (GVariantType *, size);
      GVariantType *ret;

      for (i = 0; i < size; i++)
        {
          GType t = dbus_g_type_get_struct_member_type (type, i);

          sigs[i] = dbus_g_value_type_build_g_variant_type (t);
        }

      ret = g_variant_type_new_tuple ((const GVariantType * const *) sigs,
          size);

      for (i = 0; i < size; i++)
        g_variant_type_free (sigs[i]);

      g_free (sigs);
      return ret;
    }
  else if (type == G_TYPE_BOOLEAN)
    return g_variant_type_copy (G_VARIANT_TYPE_BOOLEAN);
  else if (type == G_TYPE_UCHAR)
    return g_variant_type_copy (G_VARIANT_TYPE_BYTE);
  else if (type == G_TYPE_INT)
    return g_variant_type_copy (G_VARIANT_TYPE_INT32);
  else if (type == G_TYPE_UINT)
    return g_variant_type_copy (G_VARIANT_TYPE_UINT32);
  else if (type == G_TYPE_INT64)
    return g_variant_type_copy (G_VARIANT_TYPE_INT64);
  else if (type == G_TYPE_UINT64)
    return g_variant_type_copy (G_VARIANT_TYPE_UINT64);
  else if (type == G_TYPE_DOUBLE)
    return g_variant_type_copy (G_VARIANT_TYPE_DOUBLE);
  else if (type == G_TYPE_STRING)
    return g_variant_type_copy (G_VARIANT_TYPE_STRING);
  else if (type == G_TYPE_STRV)
    return g_variant_type_copy (G_VARIANT_TYPE_STRING_ARRAY);
  else if (type == DBUS_TYPE_G_OBJECT_PATH)
    return g_variant_type_copy (G_VARIANT_TYPE_OBJECT_PATH);
  else if (type == DBUS_TYPE_G_SIGNATURE)
    return g_variant_type_copy (G_VARIANT_TYPE_SIGNATURE);
  else if (type == G_TYPE_VALUE)
    return g_variant_type_copy (G_VARIANT_TYPE_VARIANT);
  else
    g_error ("%s: Unknown type: %s", G_STRFUNC, g_type_name (type));
}

/**
 * dbus_g_value_build_g_variant:
 * @value: a simple or specialized #GValue to convert to a #GVariant
 *
 * Recurses @value and converts its contents to a #GVariant.
 *
 * The value must either be a simple value (integer, string, boolean,
 * object path etc.) or a specialized container registered with
 * dbus_g_type_get_collection(), dbus_g_type_get_map() or
 * dbus_g_type_get_struct(). Providing any other type is a programming error
 * (including as a child type).
 *
 * Returns: a new #GVariant containing @value with a floating reference
 */
GVariant *
dbus_g_value_build_g_variant (const GValue *value)
{
  GType type;

  g_return_val_if_fail (G_IS_VALUE (value), NULL);

  type = G_VALUE_TYPE (value);

  if (dbus_g_type_is_collection (type))
    {
      GVariant *variant;
      GPtrArray *children;
      GVariantType *signature = NULL;

      children = g_ptr_array_new ();
      dbus_g_type_collection_value_iterate (value, _collection_iterator,
          children);

      if (children->len == 0)
        {
          /* we can't cheat by saying "the type of the children? that!" */
          GType element_type = dbus_g_type_get_collection_specialization (
              type);

          signature = dbus_g_value_type_build_g_variant_type (element_type);
        }

      variant = g_variant_new_array (signature, (GVariant **) children->pdata,
          children->len);
      g_ptr_array_free (children, TRUE);
      g_variant_type_free (signature);

      return variant;
    }
  else if (dbus_g_type_is_map (type))
    {
      GVariant *variant;
      GPtrArray *children;
      GVariantType *signature = NULL;

      children = g_ptr_array_new ();
      dbus_g_type_map_value_iterate (value, _map_iterator, children);

      if (children->len == 0)
        {
          /* we can't cheat by saying "the type of the children? that!" */
          GType key_type = dbus_g_type_get_map_key_specialization (
              type);
          GType value_type = dbus_g_type_get_map_value_specialization (
              type);
          GVariantType *k = dbus_g_value_type_build_g_variant_type (key_type);
          GVariantType *v = dbus_g_value_type_build_g_variant_type (
              value_type);

          signature = g_variant_type_new_dict_entry (k, v);
          g_variant_type_free (k);
          g_variant_type_free (v);
        }

      variant = g_variant_new_array (signature, (GVariant **) children->pdata,
          children->len);
      g_ptr_array_free (children, TRUE);
      g_variant_type_free (signature);

      return variant;
    }
  else if (dbus_g_type_is_struct (type))
    {
      GVariant *variant, **children;
      guint size, i;

      size = dbus_g_type_get_struct_size (type);
      children = g_new0 (GVariant *, size);

      for (i = 0; i < size; i++)
        {
          GValue cvalue = { 0, };

          g_value_init (&cvalue, dbus_g_type_get_struct_member_type (type, i));
          dbus_g_type_struct_get_member (value, i, &cvalue);

          children[i] = dbus_g_value_build_g_variant (&cvalue);
          g_value_unset (&cvalue);
        }

      variant = g_variant_new_tuple (children, size);
      g_free (children);

      return variant;
    }
  else if (type == G_TYPE_BOOLEAN)
    return g_variant_new_boolean (g_value_get_boolean (value));
  else if (type == G_TYPE_UCHAR)
    return g_variant_new_byte (g_value_get_uchar (value));
  else if (type == G_TYPE_INT)
    return g_variant_new_int32 (g_value_get_int (value));
  else if (type == G_TYPE_UINT)
    return g_variant_new_uint32 (g_value_get_uint (value));
  else if (type == G_TYPE_INT64)
    return g_variant_new_int64 (g_value_get_int64 (value));
  else if (type == G_TYPE_UINT64)
    return g_variant_new_uint64 (g_value_get_uint64 (value));
  else if (type == G_TYPE_DOUBLE)
    return g_variant_new_double (g_value_get_double (value));
  else if (type == G_TYPE_STRING)
    return g_variant_new_string (g_value_get_string (value));
  else if (type == G_TYPE_STRV)
    return g_variant_new_strv (g_value_get_boxed (value), -1);
  else if (type == DBUS_TYPE_G_OBJECT_PATH)
    return g_variant_new_object_path (g_value_get_boxed (value));
  else if (type == DBUS_TYPE_G_SIGNATURE)
    return g_variant_new_signature (g_value_get_boxed (value));
  else if (type == G_TYPE_VALUE)
    return g_variant_new_variant (
        dbus_g_value_build_g_variant (g_value_get_boxed (value)));
  else
    {
      g_error ("%s: Unknown type: %s", G_STRFUNC, g_type_name (type));
    }
}
