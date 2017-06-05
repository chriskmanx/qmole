/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gvalue-utils.c: Non-DBus-specific functions related to GType/GValue 
 *
 * Copyright (C) 2005 Red Hat, Inc.
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
#include "dbus/dbus-glib.h"
#include "dbus-gtype-specialized-priv.h"
#include "dbus-gvalue-utils.h"
#include "dbus-gtest.h"
#include <glib.h>
#include <string.h>
#include <gobject/gvaluecollector.h>


static guint
fixed_type_get_size (GType type)
{
  switch (type)
    {
    case G_TYPE_CHAR:
    case G_TYPE_UCHAR:
      return sizeof (gchar);
    case G_TYPE_BOOLEAN:
      return sizeof (gboolean);
    case G_TYPE_LONG:
    case G_TYPE_ULONG:
      return sizeof (glong);
    case G_TYPE_INT:
    case G_TYPE_UINT:
      return sizeof (gint);
    case G_TYPE_INT64:
    case G_TYPE_UINT64:
      return sizeof (gint64);
    case G_TYPE_FLOAT:
      return sizeof (gfloat);
    case G_TYPE_DOUBLE:
      return sizeof (gdouble);
    default:
      return 0;
    }
}

gboolean
_dbus_g_type_is_fixed (GType type)
{
  return fixed_type_get_size (type) > 0;
}

guint
_dbus_g_type_fixed_get_size (GType type)
{
  g_assert (_dbus_g_type_is_fixed (type));
  return fixed_type_get_size (type);
}

gboolean
_dbus_gvalue_store (GValue          *value,
		   gpointer        storage)
{
  /* FIXME - can we use the GValue lcopy_value method
   * to do this in a cleaner way?
   */
  switch (g_type_fundamental (G_VALUE_TYPE (value)))
    {
    case G_TYPE_CHAR:
      *((gchar *) storage) = g_value_get_char (value);
      return TRUE;
    case G_TYPE_UCHAR:
      *((guchar *) storage) = g_value_get_uchar (value);
      return TRUE;
    case G_TYPE_BOOLEAN:
      *((gboolean *) storage) = g_value_get_boolean (value);
      return TRUE;
    case G_TYPE_LONG:
      *((glong *) storage) = g_value_get_long (value);
      return TRUE;
    case G_TYPE_ULONG:
      *((gulong *) storage) = g_value_get_ulong (value);
      return TRUE;
    case G_TYPE_INT:
      *((gint *) storage) = g_value_get_int (value);
      return TRUE;
    case G_TYPE_UINT:
      *((guint *) storage) = g_value_get_uint (value);
      return TRUE;
    case G_TYPE_INT64:
      *((gint64 *) storage) = g_value_get_int64 (value);
      return TRUE;
    case G_TYPE_UINT64:
      *((guint64 *) storage) = g_value_get_uint64 (value);
      return TRUE;
    case G_TYPE_DOUBLE:
      *((gdouble *) storage) = g_value_get_double (value);
      return TRUE;
    case G_TYPE_STRING:
      *((gchar **) storage) = (char*) g_value_get_string (value);
      return TRUE;
    case G_TYPE_OBJECT:
      *((gpointer *) storage) = g_value_get_object (value);
      return TRUE;
    case G_TYPE_BOXED:
      *((gpointer *) storage) = g_value_get_boxed (value);
      return TRUE;
    default:
      return FALSE;
    }
}

gboolean
_dbus_gvalue_set_from_pointer (GValue          *value,
			      gconstpointer    storage)
{
  /* FIXME - is there a better way to do this? */
  switch (g_type_fundamental (G_VALUE_TYPE (value)))
    {
    case G_TYPE_CHAR:
      g_value_set_char (value, *((gchar *) storage));
      return TRUE;
    case G_TYPE_UCHAR:
      g_value_set_uchar (value, *((guchar *) storage));
      return TRUE;
    case G_TYPE_BOOLEAN:
      g_value_set_boolean (value, *((gboolean *) storage));
      return TRUE;
    case G_TYPE_LONG:
      g_value_set_long (value, *((glong *) storage));
      return TRUE;
    case G_TYPE_ULONG:
      g_value_set_ulong (value, *((gulong *) storage));
      return TRUE;
    case G_TYPE_INT:
      g_value_set_int (value, *((gint *) storage));
      return TRUE;
    case G_TYPE_UINT:
      g_value_set_uint (value, *((guint *) storage));
      return TRUE;
    case G_TYPE_INT64:
      g_value_set_int64 (value, *((gint64 *) storage));
      return TRUE;
    case G_TYPE_UINT64:
      g_value_set_uint64 (value, *((guint64 *) storage));
      return TRUE;
    case G_TYPE_DOUBLE:
      g_value_set_double (value, *((gdouble *) storage));
      return TRUE;
    case G_TYPE_STRING:
      g_value_set_string (value, *((gchar **) storage));
      return TRUE;
    case G_TYPE_OBJECT:
      g_value_set_object (value, *((gpointer *) storage));
      return TRUE;
    case G_TYPE_BOXED:
      g_value_set_boxed (value, *((gpointer *) storage));
      return TRUE;
    default:
      return FALSE;
    }
}

gboolean
_dbus_gvalue_take (GValue          *value,
		  GTypeCValue     *cvalue)
{
  GType g_type;
  GTypeValueTable *value_table;
  char *error_msg;

  g_type = G_VALUE_TYPE (value);
  value_table = g_type_value_table_peek (g_type);

  error_msg = value_table->collect_value (value, 1, cvalue, G_VALUE_NOCOPY_CONTENTS);
  if (error_msg)
    {
      g_warning ("%s: %s", G_STRLOC, error_msg);
      g_free (error_msg);
      return FALSE;
    }
  /* Clear the NOCOPY_CONTENTS flag; we want to take ownership
   * of the value.
   */
  value->data[1].v_uint &= ~(G_VALUE_NOCOPY_CONTENTS);
  return TRUE;
}

gboolean
_dbus_gtype_can_signal_error (GType gtype)
{
  switch (gtype)
    {
    case G_TYPE_BOOLEAN:
    case G_TYPE_INT:
    case G_TYPE_UINT:
    case G_TYPE_STRING:
    case G_TYPE_BOXED:
    case G_TYPE_OBJECT:
      return TRUE;
    default:
      return FALSE;
    }
}

gboolean
_dbus_gvalue_signals_error (const GValue *value)
{
  /* Hardcoded rules for return value semantics for certain
   * types.  Perhaps in the future we'd want an annotation
   * specifying which return values are errors, but in
   * reality people will probably just use boolean and
   * boxed, and there the semantics are pretty standard.
   */
  switch (G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (value)))
    {
    case G_TYPE_BOOLEAN:
      return (g_value_get_boolean (value) == FALSE);
      break;
    case G_TYPE_INT:
      return (g_value_get_int (value) < 0);
      break;
    case G_TYPE_UINT:
      return (g_value_get_uint (value) == 0);
      break;
    case G_TYPE_STRING:
      return (g_value_get_string (value) == NULL);
      break;
    case G_TYPE_BOXED:
      return (g_value_get_boxed (value) == NULL);
      break;
    case G_TYPE_OBJECT:
      return (g_value_get_object (value) == NULL);
      break;
    default:
      g_assert_not_reached ();
      return FALSE;
    }
}


static gboolean
hash_func_from_gtype (GType gtype, GHashFunc *func)
{
  switch (gtype)
    {
    case G_TYPE_CHAR:
    case G_TYPE_UCHAR:
    case G_TYPE_BOOLEAN:
    case G_TYPE_INT:
    case G_TYPE_UINT:
      *func = NULL;
      return TRUE;
    case G_TYPE_STRING:
      *func = g_str_hash;
      return TRUE;
    default:
      if (gtype == DBUS_TYPE_G_OBJECT_PATH)
        {
          *func = g_str_hash;
          return TRUE;
        }
      else if (gtype == DBUS_TYPE_G_SIGNATURE)
        {
          *func = g_str_hash;
          return TRUE;
        }
      return FALSE;
    }
}

static void
unset_and_free_g_value (gpointer val)
{
  GValue *value = val;

  g_value_unset (value);
  g_free (value);
}

static gboolean
gtype_can_simple_free (GType type);

static gboolean
hash_simple_free_from_gtype (GType gtype, GDestroyNotify *func)
{
  switch (gtype)
    {
    case G_TYPE_CHAR:
    case G_TYPE_UCHAR:
    case G_TYPE_BOOLEAN:
    case G_TYPE_INT:
    case G_TYPE_UINT:
      *func = NULL;
      return TRUE;
    case G_TYPE_DOUBLE:
    case G_TYPE_STRING:
      *func = g_free;
      return TRUE;
    default:
      if (gtype == G_TYPE_VALUE)
	{
	  *func = unset_and_free_g_value;
	  return TRUE;
	}
      else if (gtype == G_TYPE_VALUE_ARRAY)
        {
          *func = (GDestroyNotify) g_value_array_free;
          return TRUE;
        }
      else if (gtype == G_TYPE_STRV)
        {
          *func = (GDestroyNotify) g_strfreev;
          return TRUE;
        }
      else if (gtype == DBUS_TYPE_G_OBJECT_PATH)
        {
          *func = g_free;
          return TRUE;
        }
      else if (gtype == DBUS_TYPE_G_SIGNATURE)
        {
          *func = g_free;
          return TRUE;
        }
      else if (dbus_g_type_is_collection (gtype))
        {
          const DBusGTypeSpecializedCollectionVtable* vtable;
          vtable = dbus_g_type_collection_peek_vtable (gtype);
          if (vtable->base_vtable.simple_free_func)
            {
              *func = vtable->base_vtable.simple_free_func;
              return TRUE;
            }
        }
      else if (dbus_g_type_is_map (gtype))
        {
          const DBusGTypeSpecializedMapVtable* vtable;
          GType key_gtype, value_gtype;

          key_gtype = dbus_g_type_get_map_key_specialization (gtype);
          value_gtype = dbus_g_type_get_map_value_specialization (gtype);

          /* if either the key or the value don't have "simple" (without a
           * GType) free functions, then the hashtable's contents must be freed
           * with hashtable_free, so the hashtable itself can't have a simple
           * free function. */
          if (!gtype_can_simple_free (key_gtype) ||
              !gtype_can_simple_free (value_gtype))
            return FALSE;

          vtable = dbus_g_type_map_peek_vtable (gtype);
          if (vtable->base_vtable.simple_free_func)
            {
              *func = vtable->base_vtable.simple_free_func;
              return TRUE;
            }
        }
      else if (dbus_g_type_is_struct (gtype))
        {
          const DBusGTypeSpecializedStructVtable *vtable;
          vtable = dbus_g_type_struct_peek_vtable (gtype);
          if (vtable->base_vtable.simple_free_func)
            {
              *func = vtable->base_vtable.simple_free_func;
              return TRUE;
            }
        }
      return FALSE;
    }
}

static gboolean
gtype_can_simple_free (GType type)
{
  GDestroyNotify func;
  return hash_simple_free_from_gtype (type, &func);
}

gboolean
_dbus_gtype_is_valid_hash_key (GType type)
{
  GHashFunc func;
  return hash_func_from_gtype (type, &func);
}

gboolean
_dbus_gtype_is_valid_hash_value (GType gtype)
{
  /* anything we can take into a GValue using gvalue_take_hash_value is fine */
  switch (g_type_fundamental (gtype))
    {
    case G_TYPE_CHAR:
    case G_TYPE_UCHAR:
    case G_TYPE_BOOLEAN:
    case G_TYPE_INT:
    case G_TYPE_UINT:
    case G_TYPE_DOUBLE:
    case G_TYPE_STRING:
    case G_TYPE_BOXED:
    case G_TYPE_OBJECT:
      return TRUE;
    }

  return FALSE;
}

GHashFunc
_dbus_g_hash_func_from_gtype (GType gtype)
{
  GHashFunc func;
  gboolean ret;
  ret = hash_func_from_gtype (gtype, &func);
  g_assert (ret != FALSE);
  return func;
}

GEqualFunc
_dbus_g_hash_equal_from_gtype (GType gtype)
{
  g_assert (_dbus_gtype_is_valid_hash_key (gtype));

  switch (gtype)
    {
    case G_TYPE_CHAR:
    case G_TYPE_UCHAR:
    case G_TYPE_BOOLEAN:
    case G_TYPE_INT:
    case G_TYPE_UINT:
      return NULL;
    case G_TYPE_STRING:
      return g_str_equal;
    default:
      if (gtype == DBUS_TYPE_G_OBJECT_PATH)
        return g_str_equal;
      else if (gtype == DBUS_TYPE_G_SIGNATURE)
        return g_str_equal;
      g_assert_not_reached ();
      return NULL;
    }
}

static void
hash_fake_simple_free_func (gpointer val)
{
  /* Havoc would be proud... :P */
  g_critical ("If you see this message then the author of this application or "
      "one of its libraries has tried to remove or replace the value %p in a "
      "hash table which was constructed by the D-Bus Glib bindings.\n\n"

      "However, it was not possible for the bindings to provide a destroy "
      "function to g_hash_table_new_full which is able to free this value, as "
      "its GType must be known in order to free it. This means the memory "
      "allocated to store the value has most likely just been leaked.\n\n"

      "To avoid this error, the GHashTable (or keys/values \"stolen\" from "
      "it) should be freed by using g_boxed_free as follows:\n"
      "  g_boxed_free (dbus_g_type_get_map (\"GHashTable\", key_gtype, "
      "value_gtype), hash_table);\n", val);
}

GDestroyNotify
_dbus_g_hash_free_from_gtype (GType gtype)
{
  GDestroyNotify func;
  gboolean ret;

  ret = hash_simple_free_from_gtype (gtype, &func);

  /* if the value doesn't have a simple free function, we cannot define a
   * meaningful free function here. instead, this hash table must be freed
   * using g_boxed_free so that the hash_free function gets invoked. if the
   * user does not do so, we provide a fake free function to provide a warning
   * in this case. */
  if (ret == FALSE)
    {
      g_assert (_dbus_gtype_is_valid_hash_value (gtype));

      func = hash_fake_simple_free_func;
    }

  return func;
}

static void gvalue_take_ptrarray_value (GValue *value, gpointer instance);

static void
gvalue_take_hash_value (GValue *value, gpointer instance)
{
  switch (g_type_fundamental (G_VALUE_TYPE (value)))
    {
    case G_TYPE_CHAR:
      g_value_set_char (value, (gchar) GPOINTER_TO_INT (instance));
      break;
    case G_TYPE_UCHAR:
      g_value_set_uchar (value, (guchar) GPOINTER_TO_UINT (instance));
      break;
    case G_TYPE_BOOLEAN:
      g_value_set_boolean (value, (gboolean) GPOINTER_TO_UINT (instance));
      break;
    case G_TYPE_INT:
      g_value_set_int (value, GPOINTER_TO_INT (instance));
      break;
    case G_TYPE_UINT:
      g_value_set_uint (value, GPOINTER_TO_UINT (instance));
      break;
    case G_TYPE_DOUBLE:
      g_value_set_double (value, *(gdouble *) instance);
      break;
    default:
      gvalue_take_ptrarray_value (value, instance);
      break;
    }
}

static gpointer ptrarray_value_from_gvalue (const GValue *value);

static gpointer
hash_value_from_gvalue (GValue *value)
{
  switch (g_type_fundamental (G_VALUE_TYPE (value)))
    {
    case G_TYPE_CHAR:
      return GINT_TO_POINTER ((int) g_value_get_char (value));
      break;
    case G_TYPE_UCHAR:
      return GUINT_TO_POINTER ((guint) g_value_get_uchar (value));
      break;
    case G_TYPE_BOOLEAN:
      return GUINT_TO_POINTER ((guint) g_value_get_boolean (value));
      break;
    case G_TYPE_INT:
      return GINT_TO_POINTER (g_value_get_int (value));
      break;
    case G_TYPE_UINT:
      return GUINT_TO_POINTER (g_value_get_uint (value));
      break;
    case G_TYPE_DOUBLE:
      {
        gdouble *p = (gdouble *) g_malloc0 (sizeof (gdouble));
        *p = g_value_get_double (value);
        return (gpointer) p;
      }
      break;
    default:
      return ptrarray_value_from_gvalue (value);
      break;
    }
}

struct DBusGHashTableValueForeachData
{
  DBusGTypeSpecializedMapIterator func;
  GType key_type;
  GType value_type;
  gpointer data;
};

static void
hashtable_foreach_with_values (gpointer key, gpointer value, gpointer user_data)
{
  GValue key_val = {0, };
  GValue value_val = {0, };
  struct DBusGHashTableValueForeachData *data = user_data;
  
  g_value_init (&key_val, data->key_type);
  g_value_init (&value_val, data->value_type);
  gvalue_take_hash_value (&key_val, key);
  gvalue_take_hash_value (&value_val, value);

  data->func (&key_val, &value_val, data->data);
}


static void
hashtable_iterator (GType                           hash_type,
		    gpointer                        instance,
		    DBusGTypeSpecializedMapIterator iterator,
		    gpointer                        user_data)
{
  struct DBusGHashTableValueForeachData data;
  GType key_gtype;
  GType value_gtype;

  key_gtype = dbus_g_type_get_map_key_specialization (hash_type);
  value_gtype = dbus_g_type_get_map_value_specialization (hash_type);

  data.func = iterator;
  data.key_type = key_gtype;
  data.value_type = value_gtype;
  data.data = user_data;

  g_hash_table_foreach (instance, hashtable_foreach_with_values, &data);
}

void
_dbus_g_hash_table_insert_steal_values (GHashTable *table,
				       GValue     *key_val,
				       GValue     *value_val)
{
  gpointer key, val;
  
  key = hash_value_from_gvalue (key_val);
  val = hash_value_from_gvalue (value_val);

  g_hash_table_insert (table, key, val);
}

static void
hashtable_append (DBusGTypeSpecializedAppendContext *ctx,
		  GValue                            *key,
		  GValue                            *val)
{
  GHashTable *table;

  table = g_value_get_boxed (ctx->val);
  _dbus_g_hash_table_insert_steal_values (table, key, val);
}

static gpointer
hashtable_constructor (GType type)
{
  GHashTable *ret;
  GType key_gtype;
  GType value_gtype;

  key_gtype = dbus_g_type_get_map_key_specialization (type);
  value_gtype = dbus_g_type_get_map_value_specialization (type);

  ret = g_hash_table_new_full (_dbus_g_hash_func_from_gtype (key_gtype),
			       _dbus_g_hash_equal_from_gtype (key_gtype),
			       _dbus_g_hash_free_from_gtype (key_gtype),
			       _dbus_g_hash_free_from_gtype (value_gtype));
  return ret;
}

static void
hashtable_insert_values (GHashTable       *table,
			 const GValue     *key_val,
			 const GValue     *value_val)
{
  GValue key_copy = {0, };
  GValue value_copy = {0, };

  g_value_init (&key_copy, G_VALUE_TYPE (key_val));
  g_value_copy (key_val, &key_copy);
  g_value_init (&value_copy, G_VALUE_TYPE (value_val));
  g_value_copy (value_val, &value_copy);
  
  _dbus_g_hash_table_insert_steal_values (table, &key_copy, &value_copy);
}

static void
hashtable_foreach_copy (const GValue *key, const GValue *val, gpointer data)
{
  hashtable_insert_values ((GHashTable *) data, key, val);
}

static gpointer
hashtable_copy (GType type, gpointer src)
{
  GHashTable *ghash;
  GHashTable *ret;
  GValue hashval = {0,};

  ghash = src;

  ret = hashtable_constructor (type);

  g_value_init (&hashval, type);
  g_value_set_static_boxed (&hashval, ghash); 
  dbus_g_type_map_value_iterate (&hashval, hashtable_foreach_copy, ret);
  return ret;
}

/* we leave this here for backwards compatibility - any hash tables nested
 * inside hash tables will use this as their free function if users were
 * already relying on it, but dbus-glib itself will never use it directly as
 * hashtable_free is also defined. */
static void
hashtable_simple_free (gpointer val)
{
  g_hash_table_unref (val);
}

struct DBusGHashTableFreeData
{
  GType key_gtype;
  GType value_gtype;
};

static gboolean
hashtable_free_foreach_steal (gpointer key,
                              gpointer value,
                              gpointer user_data)
{
  struct DBusGHashTableFreeData *data = user_data;
  GValue val = { 0, };

  g_value_init (&val, data->key_gtype);
  gvalue_take_hash_value (&val, key);
  g_value_unset (&val);

  g_value_init (&val, data->value_gtype);
  gvalue_take_hash_value (&val, value);
  g_value_unset (&val);

  return TRUE;
}

static void
hashtable_free (GType type,
                gpointer val)
{
  struct DBusGHashTableFreeData data = { 0, };
  GHashTable *hash = val;

  data.key_gtype = dbus_g_type_get_map_key_specialization (type);
  data.value_gtype = dbus_g_type_get_map_value_specialization (type);

  /* wheee, fun. two cases here. either:
   *
   * a) the keys and value types both have simple (ie, no * GType parameter is
   * needed to know how to free them) free functions, in which case they were
   * set as the hash free functions when the hash table was constructed.  in
   * this case, it's sufficient for us to unref the hash table as before. we
   * have to keep doing this in order to maintain compatibility with the ABI
   * which was around before hash tables could contain types which don't have
   * simple free functions (such as GPtrArrays of other stuff). for these
   * tables, users were able to ref the hash tables and add/remove values, and
   * rely on meaningful free functions.
   *
   * b) for any other key or value types where /do/ need to know the GType in
   * order to free it, this function is the only "right" way to free the hash
   * table. both the key and value free functions were set to print a big nasty
   * warning, and we free the contents of the hashtable with foreach_steal.
   */
  if (gtype_can_simple_free (data.key_gtype) &&
      gtype_can_simple_free (data.value_gtype))
    {
      g_hash_table_unref (hash);
    }
  else
    {
      g_hash_table_foreach_steal (hash, hashtable_free_foreach_steal, &data);
      g_hash_table_unref (hash);
    }
}

static gpointer
valuearray_constructor (GType type)
{
  GValueArray *ret;
  guint size = dbus_g_type_get_struct_size (type);
  guint i;
  ret = g_value_array_new (size);
  for (i=0; i < size; i++)
    {
      GValue val = {0,};
      g_value_init (&val, dbus_g_type_get_struct_member_type (type, i));
      g_value_array_append(ret, &val);
    }
  return (gpointer)ret;
}

static gpointer
valuearray_copy (GType type, gpointer src)
{
  return g_value_array_copy ((GValueArray*) src);
}

static void
valuearray_simple_free (gpointer val)
{
  g_value_array_free (val);
}

static gboolean
valuearray_get_member (GType type, gpointer instance,
                       guint member, GValue *ret)
{
  GValueArray *va = (GValueArray*) instance;
  const GValue *val;
  if (member < dbus_g_type_get_struct_size (type))
    {
      val = g_value_array_get_nth (va, member);
      g_value_copy (val, ret);
      return TRUE;
    }
  else
    return FALSE;
}

static gboolean
valuearray_set_member (GType type, gpointer instance,
                       guint member, const GValue *member_type)
{
  GValueArray *va = (GValueArray*) instance;
  GValue *vp;
  if (member < dbus_g_type_get_struct_size (type))
    {
      vp = g_value_array_get_nth (va, member);
      g_value_copy (member_type, vp);
      return TRUE;
    }
  else
    return FALSE;
}


static gpointer
array_constructor (GType type)
{
  GArray *array;
  guint elt_size;
  GType elt_type;
  gboolean zero_terminated;
  gboolean clear;

  elt_type = dbus_g_type_get_collection_specialization (type);
  g_assert (elt_type != G_TYPE_INVALID);

  elt_size = _dbus_g_type_fixed_get_size (elt_type);

  /* These are "safe" defaults */ 
  zero_terminated = TRUE; /* ((struct _DBusGRealArray*) garray)->zero_terminated; */
  clear = TRUE; /* ((struct _DBusGRealArray*) garray)->clear; */

  array = g_array_new (zero_terminated, clear, elt_size);
  return array;
}

static void
array_iterator (GType garray_type,
    gpointer instance,
    DBusGTypeSpecializedCollectionIterator iterator,
    gpointer user_data)
{
  GArray *array;
  GType elt_gtype;
  guint i;

  array = instance;

  elt_gtype = dbus_g_type_get_collection_specialization (garray_type);

  for (i = 0; i < array->len; i++)
    {
      GValue val = {0, };
      g_value_init (&val, elt_gtype);

      switch (elt_gtype)
        {
          case G_TYPE_BOOLEAN:
            g_value_set_boolean (&val, !!g_array_index (array, gboolean, i));
            break;

          case G_TYPE_FLOAT:
            g_value_set_float (&val, g_array_index (array, gfloat, i));
            break;

          case G_TYPE_DOUBLE:
            g_value_set_double (&val, g_array_index (array, gdouble, i));
            break;

          case G_TYPE_CHAR:
            g_value_set_char (&val, g_array_index (array, gchar, i));
            break;

          case G_TYPE_UCHAR:
            g_value_set_uchar (&val, g_array_index (array, guchar, i));
            break;

          case G_TYPE_INT:
            g_value_set_int (&val, g_array_index (array, gint, i));
            break;

          case G_TYPE_UINT:
            g_value_set_uint (&val, g_array_index (array, guint, i));
            break;

          case G_TYPE_LONG:
            g_value_set_long (&val, g_array_index (array, glong, i));
            break;

          case G_TYPE_ULONG:
            g_value_set_ulong (&val, g_array_index (array, gulong, i));
            break;

          case G_TYPE_INT64:
            g_value_set_int64 (&val, g_array_index (array, gint64, i));
            break;

          case G_TYPE_UINT64:
            g_value_set_uint64 (&val, g_array_index (array, guint64, i));
            break;

          default:
            g_assert_not_reached ();
        }

      iterator (&val, user_data);
    }
}

static void
array_append (DBusGTypeSpecializedAppendContext *ctx,
    GValue *value)
{
  GArray *array = g_value_get_boxed (ctx->val);
  GType elt_gtype;
  union {
      guint64 u64;
      gint64 i64;
      gulong ul;
      glong l;
      guint u;
      gint i;
      guchar uc;
      gchar c;
      gboolean b;
      gfloat f;
      gdouble d;
  } tmp;

  elt_gtype = dbus_g_type_get_collection_specialization (
      G_VALUE_TYPE (ctx->val));

  switch (elt_gtype)
    {
      case G_TYPE_BOOLEAN:
        tmp.b = g_value_get_boolean (value);
        g_array_append_val (array, tmp.b);
        break;

      case G_TYPE_FLOAT:
        tmp.f = g_value_get_float (value);
        g_array_append_val (array, tmp.f);
        break;

      case G_TYPE_DOUBLE:
        tmp.d = g_value_get_double (value);
        g_array_append_val (array, tmp.d);
        break;

      case G_TYPE_CHAR:
        tmp.c = g_value_get_char (value);
        g_array_append_val (array, tmp.c);
        break;

      case G_TYPE_UCHAR:
        tmp.uc = g_value_get_uchar (value);
        g_array_append_val (array, tmp.uc);
        break;

      case G_TYPE_INT:
        tmp.i = g_value_get_int (value);
        g_array_append_val (array, tmp.i);
        break;

      case G_TYPE_UINT:
        tmp.u = g_value_get_uint (value);
        g_array_append_val (array, tmp.u);
        break;

      case G_TYPE_LONG:
        tmp.l = g_value_get_long (value);
        g_array_append_val (array, tmp.l);
        break;

      case G_TYPE_ULONG:
        tmp.ul = g_value_get_ulong (value);
        g_array_append_val (array, tmp.ul);
        break;

      case G_TYPE_INT64:
        tmp.i64 = g_value_get_int64 (value);
        g_array_append_val (array, tmp.i64);
        break;

      case G_TYPE_UINT64:
        tmp.u64 = g_value_get_uint64 (value);
        g_array_append_val (array, tmp.u64);
        break;

      default:
        g_assert_not_reached ();
    }
}

static gpointer
array_copy (GType type, gpointer src)
{
  GArray *garray;
  GArray *new;

  garray = src;

  new = array_constructor (type);
  g_array_append_vals (new, garray->data, garray->len);

  return new;
}

static void
array_simple_free (gpointer val)
{
  GArray *array;
  array = val;
  g_array_free (array, TRUE);
}

static gboolean
array_fixed_accessor (GType type, gpointer instance, gpointer *values, guint *len)
{
  GType elt_type;
  GArray *array = instance;

  elt_type = dbus_g_type_get_collection_specialization (type);
  if (!_dbus_g_type_is_fixed (elt_type))
    return FALSE;

  *values = array->data;
  *len = array->len;
  return TRUE;
}

static gpointer
ptrarray_constructor (GType type)
{
  /* Later we should determine a destructor, need g_ptr_array_destroy */
  return g_ptr_array_new ();
}

static void
gvalue_take_ptrarray_value (GValue *value, gpointer instance)
{
  switch (g_type_fundamental (G_VALUE_TYPE (value)))
    {
    case G_TYPE_STRING:
      g_value_take_string (value, instance);
      break;
    case G_TYPE_BOXED:
      g_value_take_boxed (value, instance);
      break;
    case G_TYPE_OBJECT:
      g_value_take_object (value, instance);
      break;
    default:
      g_assert_not_reached ();
      break;
    }
}

static gpointer
ptrarray_value_from_gvalue (const GValue *value)
{
  GValue tmp = {0, };

  /* if the NOCOPY flag is set, then value was created via set_static and hence
   * is not owned by us. in order to preserve the "take" semantics that the API
   * has in general (which avoids copying in the common case), we must copy any
   * static values so that we can indiscriminately free the entire collection
   * later. */
  if (value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS)
    {
      g_value_init (&tmp, G_VALUE_TYPE (value));
      g_value_copy (value, &tmp);
      value = &tmp;
    }

  switch (g_type_fundamental (G_VALUE_TYPE (value)))
    {
    case G_TYPE_STRING:
      return (gpointer) g_value_get_string (value);
      break;
    case G_TYPE_BOXED:
      return g_value_get_boxed (value);
      break;
    case G_TYPE_OBJECT:
      return g_value_get_object (value);
      break;
    default:
      g_assert_not_reached ();
      return NULL;
    }
}

static void
ptrarray_iterator (GType                                   ptrarray_type,
		   gpointer                                instance,
		   DBusGTypeSpecializedCollectionIterator  iterator,
		   gpointer                                user_data)
{
  GPtrArray *ptrarray;
  GType elt_gtype;
  guint i;

  ptrarray = instance;

  elt_gtype = dbus_g_type_get_collection_specialization (ptrarray_type);

  for (i = 0; i < ptrarray->len; i++)
    {
      GValue val = {0, };
      g_value_init (&val, elt_gtype);
      gvalue_take_ptrarray_value (&val, g_ptr_array_index (ptrarray, i));
      iterator (&val, user_data);
    }
}

static void
ptrarray_copy_elt (const GValue *val, gpointer user_data)
{
  GPtrArray *dest = user_data;
  GValue val_copy = {0, };

  g_value_init (&val_copy, G_VALUE_TYPE (val));
  g_value_copy (val, &val_copy);

  g_ptr_array_add (dest, ptrarray_value_from_gvalue (&val_copy));
}

static gpointer
ptrarray_copy (GType type, gpointer src)
{
  GPtrArray *new;
  GValue array_val = {0, };

  g_value_init (&array_val, type);
  g_value_set_static_boxed (&array_val, src);

  new = ptrarray_constructor (type);
  dbus_g_type_collection_value_iterate (&array_val, ptrarray_copy_elt, new);

  return new;
}

static void
ptrarray_append (DBusGTypeSpecializedAppendContext *ctx, GValue *value)
{
  GPtrArray *array;

  array = g_value_get_boxed (ctx->val);

  g_ptr_array_add (array, ptrarray_value_from_gvalue (value));
}

static void
ptrarray_free (GType type, gpointer val)
{
  GPtrArray *array;
  GValue elt_val = {0, };
  GType elt_gtype;
  unsigned int i;

  array = val;

  elt_gtype = dbus_g_type_get_collection_specialization (type);

  for (i = 0; i < array->len; i++)
    {
      g_value_init (&elt_val, elt_gtype);
      gvalue_take_ptrarray_value (&elt_val, g_ptr_array_index (array, i));
      g_value_unset (&elt_val);
    }

  g_ptr_array_free (array, TRUE);
}

static gpointer
slist_constructor (GType type)
{
  return NULL;
}

static void
slist_iterator (GType                                   list_type,
		gpointer                                instance,
		DBusGTypeSpecializedCollectionIterator  iterator,
		gpointer                                user_data)
{
  GSList *slist;
  GType elt_gtype;

  slist = instance;

  elt_gtype = dbus_g_type_get_collection_specialization (list_type);

  for (slist = instance; slist != NULL; slist = slist->next)
    {
      GValue val = {0, };
      g_value_init (&val, elt_gtype);
      gvalue_take_ptrarray_value (&val, slist->data);
      iterator (&val, user_data);
    }
}

static void
slist_copy_elt (const GValue *val, gpointer user_data)
{
  GSList **dest = user_data;
  GValue val_copy = {0, };

  g_value_init (&val_copy, G_VALUE_TYPE (val));
  g_value_copy (val, &val_copy);

  *dest = g_slist_append (*dest, ptrarray_value_from_gvalue (&val_copy));
}

static gpointer
slist_copy (GType type, gpointer src)
{
  GSList *new;
  GValue slist_val = {0, };

  g_value_init (&slist_val, type);
  g_value_set_static_boxed (&slist_val, src);

  new = slist_constructor (type);
  dbus_g_type_collection_value_iterate (&slist_val, slist_copy_elt, &new);

  return new;
}

static void
slist_append (DBusGTypeSpecializedAppendContext *ctx, GValue *value)
{
  GSList *list;

  list = g_value_get_boxed (ctx->val);
  list = g_slist_prepend (list, ptrarray_value_from_gvalue (value));
  g_value_set_static_boxed (ctx->val, list);
}

static void
slist_end_append (DBusGTypeSpecializedAppendContext *ctx)
{
  GSList *list;

  /* if you append multiple times to the slist, this will reverse the existing
   * elements... we need an init_append function */
  list = g_value_get_boxed (ctx->val);
  list = g_slist_reverse (list);

  g_value_take_boxed (ctx->val, list);
}

static void
slist_free (GType type, gpointer val)
{
  GSList *list;
  GType elt_gtype;
  list = val;

  elt_gtype = dbus_g_type_get_collection_specialization (type);

  while (list != NULL)
    {
      GValue elt_val = {0, };
      g_value_init (&elt_val, elt_gtype);
      gvalue_take_ptrarray_value (&elt_val, list->data);
      g_value_unset (&elt_val);
      list = g_slist_next(list);
    }
  list=val;
  g_slist_free (list);
}

void
_dbus_g_type_specialized_builtins_init (void)
{
  /* types with a simple_free function can be freed at run-time without
   * the destroy function needing to know the type, so they can be
   * stored in hash tables */

  static const DBusGTypeSpecializedCollectionVtable array_vtable = {
    {
      array_constructor,
      NULL,
      array_copy,
      array_simple_free,
      NULL,
      NULL,
    },
    array_fixed_accessor,
    array_iterator,
    array_append,
    NULL
  };


  static const DBusGTypeSpecializedCollectionVtable ptrarray_vtable = {
    {
      ptrarray_constructor,
      ptrarray_free,
      ptrarray_copy,
      NULL,
      NULL,
      NULL,
    },
    NULL,
    ptrarray_iterator,
    ptrarray_append,
    NULL,
  };


  static const DBusGTypeSpecializedCollectionVtable slist_vtable = {
    {
      slist_constructor,
      slist_free,
      slist_copy,
      NULL,
      NULL,
      NULL,
    },
    NULL,
    slist_iterator,
    slist_append,
    slist_end_append,
  };

  static const DBusGTypeSpecializedMapVtable hashtable_vtable = {
    {
      hashtable_constructor,
      hashtable_free,
      hashtable_copy,
      hashtable_simple_free,
      NULL,
      NULL
    },
    hashtable_iterator,
    hashtable_append
  };

  static const DBusGTypeSpecializedStructVtable valuearray_vtable = {
    {
      valuearray_constructor,
      NULL,
      valuearray_copy,
      valuearray_simple_free,
      NULL,
      NULL
    },
    valuearray_get_member,
    valuearray_set_member
  };

  _dbus_g_type_register_collection ("GSList", &slist_vtable, 0);
  _dbus_g_type_register_collection ("GArray", &array_vtable, 0);
  _dbus_g_type_register_collection ("GPtrArray", &ptrarray_vtable, 0);
  _dbus_g_type_register_map ("GHashTable", &hashtable_vtable, 0);
  _dbus_g_type_register_struct ("GValueArray", &valuearray_vtable, 0);
}

#ifdef DBUS_BUILD_TESTS

typedef struct
{
  gboolean seen_foo;
  gboolean seen_baz;
} TestSpecializedHashData;

static void
test_specialized_hash (const GValue *key, const GValue *val, gpointer user_data)
{
  TestSpecializedHashData *data = user_data;

  g_assert (G_VALUE_HOLDS_STRING (key));
  g_assert (G_VALUE_HOLDS_STRING (val));

  if (!strcmp (g_value_get_string (key), "foo"))
    {
      data->seen_foo = TRUE;
      g_assert (!strcmp (g_value_get_string (val), "bar"));
    }
  else if (!strcmp (g_value_get_string (key), "baz"))
    {
      data->seen_baz = TRUE;
      g_assert (!strcmp (g_value_get_string (val), "moo"));
    }
  else
    {
      g_assert_not_reached ();
    }
}

static void
test_specialized_hash_2 (const GValue *key, const GValue *val, gpointer user_data)
{
  TestSpecializedHashData *data = user_data;
  const GValue *realval;

  g_assert (G_VALUE_HOLDS_STRING (key));
  g_assert (G_VALUE_TYPE (val) == G_TYPE_VALUE);

  realval = g_value_get_boxed (val);

  if (!strcmp (g_value_get_string (key), "foo"))
    {
      data->seen_foo = TRUE;
      g_assert (G_VALUE_HOLDS_UINT (realval));
      g_assert (g_value_get_uint (realval) == 20);
    }
  else if (!strcmp (g_value_get_string (key), "baz"))
    {
      data->seen_baz = TRUE;
      g_assert (G_VALUE_HOLDS_STRING (realval));
      g_assert (!strcmp ("bar", g_value_get_string (realval)));
    }
  else
    {
      g_assert_not_reached ();
    }
}

gboolean
_dbus_gvalue_utils_test (const char *datadir)
{
  GType type;

  dbus_g_type_specialized_init ();
 _dbus_g_type_specialized_builtins_init ();

  type = dbus_g_type_get_collection ("GArray", G_TYPE_UINT);
  g_assert (dbus_g_type_is_collection (type));
  g_assert (dbus_g_type_get_collection_specialization (type) == G_TYPE_UINT);
  {
    GArray *instance;

    instance = dbus_g_type_specialized_construct (type);

    g_assert (instance->len == 0);

    g_array_free (instance, TRUE);
  }

  type = dbus_g_type_get_map ("GHashTable", G_TYPE_STRING, G_TYPE_STRING);
  g_assert (dbus_g_type_is_map (type));
  g_assert (dbus_g_type_get_map_key_specialization (type) == G_TYPE_STRING);
  g_assert (dbus_g_type_get_map_value_specialization (type) == G_TYPE_STRING);
  {
    GHashTable *instance;
    GValue val = { 0, };
    TestSpecializedHashData hashdata;

    instance = dbus_g_type_specialized_construct (type);

    g_assert (g_hash_table_size (instance) == 0);
    g_hash_table_insert (instance, g_strdup ("foo"), g_strdup ("bar"));
    g_hash_table_insert (instance, g_strdup ("baz"), g_strdup ("moo"));
    g_assert (g_hash_table_size (instance) == 2);

    g_value_init (&val, type);
    g_value_take_boxed (&val, instance);
    hashdata.seen_foo = FALSE;
    hashdata.seen_baz = FALSE;
    dbus_g_type_map_value_iterate (&val,
				   test_specialized_hash, 
				   &hashdata);
    
    g_assert (hashdata.seen_foo);
    g_assert (hashdata.seen_baz);

    g_value_unset (&val);
  }

  type = dbus_g_type_get_map ("GHashTable", G_TYPE_STRING, G_TYPE_VALUE);
  g_assert (dbus_g_type_is_map (type));
  g_assert (dbus_g_type_get_map_key_specialization (type) == G_TYPE_STRING);
  g_assert (dbus_g_type_get_map_value_specialization (type) == G_TYPE_VALUE);
  {
    GHashTable *instance;
    GValue val = { 0, };
    TestSpecializedHashData hashdata;
    DBusGTypeSpecializedAppendContext ctx;
    GValue *eltval;

    instance = dbus_g_type_specialized_construct (type);
    g_value_init (&val, type);
    g_value_take_boxed (&val, instance);

    dbus_g_type_specialized_init_append (&val, &ctx);

    {
      GValue keyval = { 0, };
      GValue valval = { 0, };
      g_value_init (&keyval, G_TYPE_STRING);
      g_value_set_string (&keyval, "foo"); 

      g_value_init (&valval, G_TYPE_VALUE);
      eltval = g_new0 (GValue, 1);
      g_value_init (eltval, G_TYPE_UINT);
      g_value_set_uint (eltval, 20);
      g_value_take_boxed (&valval, eltval);
      dbus_g_type_specialized_map_append (&ctx, &keyval, &valval);
    }

    {
      GValue keyval = { 0, };
      GValue valval = { 0, };
      g_value_init (&keyval, G_TYPE_STRING);
      g_value_set_string (&keyval, "baz"); 
      g_value_init (&valval, G_TYPE_VALUE);
      eltval = g_new0 (GValue, 1);
      g_value_init (eltval, G_TYPE_STRING);
      g_value_set_string (eltval, "bar");
      g_value_take_boxed (&valval, eltval);
      dbus_g_type_specialized_map_append (&ctx, &keyval, &valval);
    }

    hashdata.seen_foo = FALSE;
    hashdata.seen_baz = FALSE;
    dbus_g_type_map_value_iterate (&val,
				   test_specialized_hash_2, 
				   &hashdata);
    
    g_assert (hashdata.seen_foo);
    g_assert (hashdata.seen_baz);

    g_value_unset (&val);
  }

  type = dbus_g_type_get_collection ("GSList", G_TYPE_OBJECT);
  g_assert (dbus_g_type_is_collection (type));
  g_assert (dbus_g_type_get_collection_specialization (type) == G_TYPE_OBJECT);
  {
    GSList *instance, *tmp, *copy;
    GValue val = {0, };
    GValue copyval = {0, };
    DBusGTypeSpecializedAppendContext ctx;
    GObject *objects[3];
    int i;

    instance = dbus_g_type_specialized_construct (type);
    g_assert (instance == NULL);

    g_value_init (&val, type);
    g_value_take_boxed (&val, instance);

    dbus_g_type_specialized_init_append (&val, &ctx);

    for (i = 0; i < 3; i++)
      {
        GValue eltval = { 0, };
        GObject *obj = g_object_new (G_TYPE_OBJECT, NULL);

        g_assert (obj != NULL);
        objects[i] = obj;
        g_object_add_weak_pointer (obj, (gpointer) (objects + i));

        g_value_init (&eltval, G_TYPE_OBJECT);
        g_value_take_object (&eltval, obj);
        dbus_g_type_specialized_collection_append (&ctx, &eltval);
      }

    dbus_g_type_specialized_collection_end_append (&ctx);

    instance = g_value_get_boxed (&val);
    g_assert (g_slist_length (instance) == 3);

    for (tmp = instance; tmp; tmp = tmp->next)
      {
        GObject *obj = tmp->data;
        g_assert (G_IS_OBJECT (obj));
        g_assert (obj->ref_count == 1);
      }

    g_value_init (&copyval, type);
    g_value_copy (&val, &copyval);

    copy = g_value_get_boxed (&copyval);
    g_assert (g_slist_length (copy) == 3);

    for (tmp = copy; tmp; tmp = tmp->next)
      {
        GObject *obj = tmp->data;
        g_assert (G_IS_OBJECT (obj));
        g_assert (obj->ref_count == 2);
      }

    g_value_unset (&copyval);

    for (i = 0; i < 3; i++)
      {
        g_assert (objects[i] != NULL);
      }

    for (tmp = instance; tmp; tmp = tmp->next)
      {
        GObject *obj = tmp->data;
        g_assert (G_IS_OBJECT (obj));
        g_assert (obj->ref_count == 1);
      }

    g_value_unset (&val);

    for (i = 0; i < 3; i++)
      {
        g_assert (objects[i] == NULL);
      }
  }

  type = dbus_g_type_get_collection ("GPtrArray", G_TYPE_STRING);
  g_assert (dbus_g_type_is_collection (type));
  g_assert (dbus_g_type_get_collection_specialization (type) == G_TYPE_STRING);
  {
    GPtrArray *instance;
    DBusGTypeSpecializedAppendContext ctx;
    GValue val = {0, };
    GValue eltval = {0, };

    instance = dbus_g_type_specialized_construct (type);

    g_assert (instance->len == 0);

    g_value_init (&val, type);
    g_value_take_boxed (&val, instance);

    dbus_g_type_specialized_init_append (&val, &ctx);

    g_value_init (&eltval, G_TYPE_STRING);
    g_value_set_static_string (&eltval, "foo");
    dbus_g_type_specialized_collection_append (&ctx, &eltval);

    g_value_reset (&eltval);
    g_value_set_static_string (&eltval, "bar");
    dbus_g_type_specialized_collection_append (&ctx, &eltval);

    g_value_reset (&eltval);
    g_value_set_static_string (&eltval, "baz");
    dbus_g_type_specialized_collection_append (&ctx, &eltval);

    dbus_g_type_specialized_collection_end_append (&ctx);

    g_assert (instance->len == 3);

    g_assert (!strcmp ("foo", g_ptr_array_index (instance, 0)));
    g_assert (!strcmp ("bar", g_ptr_array_index (instance, 1)));
    g_assert (!strcmp ("baz", g_ptr_array_index (instance, 2)));

    g_value_unset (&val);
  }

  type = dbus_g_type_get_struct ("GValueArray", G_TYPE_STRING, G_TYPE_UINT, DBUS_TYPE_G_OBJECT_PATH, G_TYPE_INVALID);
  g_assert (dbus_g_type_is_struct (type));
  g_assert (dbus_g_type_get_struct_size (type) == 3);
  g_assert (dbus_g_type_get_struct_member_type (type, 0) == G_TYPE_STRING);
  g_assert (dbus_g_type_get_struct_member_type (type, 1) == G_TYPE_UINT);
  g_assert (dbus_g_type_get_struct_member_type (type, 2) == DBUS_TYPE_G_OBJECT_PATH);
  {
    GValueArray *instance;
    GValue val = {0, };
    GValue memval = {0, };

    instance = dbus_g_type_specialized_construct (type);

    g_assert (instance->n_values == 3);

    g_value_init (&val, type);
    g_value_take_boxed (&val, instance);

    g_value_init (&memval, G_TYPE_STRING);
    g_value_set_static_string (&memval, "foo");
    dbus_g_type_struct_set_member (&val, 0, &memval);
    g_value_unset (&memval);

    g_value_init (&memval, G_TYPE_UINT);
    g_value_set_uint (&memval, 42);
    dbus_g_type_struct_set_member (&val, 1, &memval);
    g_value_unset (&memval);

    g_value_init (&memval, DBUS_TYPE_G_OBJECT_PATH);
    g_value_set_static_boxed (&memval, "/bar/moo/foo/baz");
    dbus_g_type_struct_set_member (&val, 2, &memval);
    g_value_unset (&memval);

    g_assert (instance->n_values == 3);

    g_value_init (&memval, G_TYPE_STRING);
    dbus_g_type_struct_get_member (&val, 0, &memval);
    g_assert (0 == strcmp (g_value_get_string (&memval), "foo"));
    g_value_unset (&memval);

    g_value_init (&memval, G_TYPE_UINT);
    dbus_g_type_struct_get_member (&val, 1, &memval);
    g_assert (g_value_get_uint (&memval) == 42);
    g_value_unset (&memval);

    g_value_init (&memval, DBUS_TYPE_G_OBJECT_PATH);
    dbus_g_type_struct_get_member (&val, 2, &memval);
    g_assert (0 == strcmp ((gchar*) g_value_get_boxed (&memval),
                           "/bar/moo/foo/baz"));
    g_value_unset (&memval);

    g_value_unset (&val);
  }

  type = dbus_g_type_get_struct ("GValueArray", G_TYPE_STRING, G_TYPE_UINT, DBUS_TYPE_G_OBJECT_PATH, G_TYPE_INVALID);
  g_assert (dbus_g_type_is_struct (type));
  g_assert (dbus_g_type_get_struct_size (type) == 3);
  g_assert (dbus_g_type_get_struct_member_type (type, 0) == G_TYPE_STRING);
  g_assert (dbus_g_type_get_struct_member_type (type, 1) == G_TYPE_UINT);
  g_assert (dbus_g_type_get_struct_member_type (type, 2) == DBUS_TYPE_G_OBJECT_PATH);
  {
    GValueArray *instance;
    GValue val = {0, };

    instance = dbus_g_type_specialized_construct (type);

    g_assert (instance->n_values == 3);

    g_value_init (&val, type);
    g_value_take_boxed (&val, instance);

    dbus_g_type_struct_set (&val,
                            0,"foo",
                            1, 42,
                            2, "/bar/moo/foo/baz",
                            G_MAXUINT);

    g_assert (instance->n_values == 3);

    {
      gchar *string;
      guint intval;
      gchar *path;

      dbus_g_type_struct_get (&val,
                              0, &string,
                              1, &intval,
                              2, &path,
                              G_MAXUINT);

      g_assert (0 == strcmp (string, "foo"));
      g_assert (intval == 42);
      g_assert (0 == strcmp (path, "/bar/moo/foo/baz"));
    }

    g_value_unset (&val);
  }


  return TRUE;
}



#endif /* DBUS_BUILD_TESTS */
