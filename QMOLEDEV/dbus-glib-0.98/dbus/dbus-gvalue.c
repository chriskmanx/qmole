/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gvalue.c GValue to-from DBusMessageIter
 *
 * Copyright (C) 2004 Ximian, Inc.
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

#include "config.h"
#include "dbus-gtest.h"
#include "dbus-gvalue.h"
#include "dbus-gsignature.h"
#include "dbus-gobject.h"
#include "dbus-gvalue-utils.h"
#include "dbus/dbus-glib.h"
#include <string.h>
#include <glib.h>
#include "dbus/dbus-signature.h"

/* Seems reasonable, but this should probably be part of the standard protocol */
#define DBUS_GLIB_MAX_VARIANT_RECURSION 32

static gboolean demarshal_static_variant (DBusGValueMarshalCtx    *context,
					  DBusMessageIter         *iter,
					  GValue                  *value,
					  GError                 **error);


static gboolean marshal_basic                   (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_basic                 (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_strv                    (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_strv                  (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_valuearray              (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_valuearray            (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_variant                 (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_variant               (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_proxy                   (DBusMessageIter           *iter,
						 const GValue             *value);
static gboolean demarshal_proxy                 (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_object_path             (DBusMessageIter           *iter,
						 const GValue             *value);
static gboolean demarshal_object_path           (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_object                  (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_object                (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_signature		(DBusMessageIter           *iter,
						 const GValue             *value);
static gboolean demarshal_signature		(DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_map                     (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_map                   (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);

static gboolean marshal_collection              (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean marshal_collection_ptrarray     (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean marshal_collection_array        (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_collection            (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean demarshal_collection_ptrarray   (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean demarshal_collection_array      (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);
static gboolean marshal_struct                  (DBusMessageIter           *iter,
						 const GValue              *value);
static gboolean demarshal_struct                (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);


typedef gboolean (*DBusGValueMarshalFunc)       (DBusMessageIter           *iter,
						 const GValue              *value);
typedef gboolean (*DBusGValueDemarshalFunc)     (DBusGValueMarshalCtx      *context,
						 DBusMessageIter           *iter,
						 GValue                    *value,
						 GError                   **error);

typedef struct {
  DBusGValueMarshalFunc       marshaller;
  DBusGValueDemarshalFunc     demarshaller;
} DBusGTypeMarshalVtable;

typedef struct {
  const char                       *sig;
  const DBusGTypeMarshalVtable     *vtable;
} DBusGTypeMarshalData;

static GQuark
dbus_g_type_metadata_data_quark ()
{
  static GQuark quark;
  if (!quark)
    quark = g_quark_from_static_string ("DBusGTypeMetaData");
  
  return quark;
}

static void
set_type_metadata (GType type, const DBusGTypeMarshalData *data)
{
  g_type_set_qdata (type, dbus_g_type_metadata_data_quark (), (gpointer) data);
}

static void
register_basic (int typecode, const DBusGTypeMarshalData *typedata)
{
  set_type_metadata (_dbus_gtype_from_basic_typecode (typecode), typedata);
}

void
_dbus_g_value_types_init (void)
{
  static gboolean types_initialized;

  static const DBusGTypeMarshalVtable basic_vtable = {
    marshal_basic,
    demarshal_basic
  };

  if (types_initialized)
    return;

  dbus_g_type_specialized_init ();

  /* Register basic types */
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_BOOLEAN_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_BOOLEAN, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_BYTE_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_BYTE, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_INT16_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_INT16, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_UINT16_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_UINT16, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_UINT32_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_UINT32, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_INT32_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_INT32, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_UINT64_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_UINT64, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_INT64_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_INT64, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_DOUBLE_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_DOUBLE, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_STRING_AS_STRING,
      &basic_vtable,
    };
    register_basic (DBUS_TYPE_STRING, &typedata);
  }
  /* fundamental GTypes that don't map 1:1 with D-BUS types */
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_BYTE_AS_STRING,
      &basic_vtable,
    };
    set_type_metadata (G_TYPE_CHAR, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_INT32_AS_STRING,
      &basic_vtable,
    };
    set_type_metadata (G_TYPE_LONG, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_UINT32_AS_STRING,
      &basic_vtable,
    };
    set_type_metadata (G_TYPE_ULONG, &typedata);
  }
  {
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_DOUBLE_AS_STRING,
      &basic_vtable,
    };
    set_type_metadata (G_TYPE_FLOAT, &typedata);
  }

  /* Register complex types with builtin GType mappings */
  {
    static const DBusGTypeMarshalVtable vtable = {
      marshal_variant,
      demarshal_variant
    };
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_VARIANT_AS_STRING,
      &vtable
    };
    set_type_metadata (G_TYPE_VALUE, &typedata);
  };
  {
    static const DBusGTypeMarshalVtable vtable = {
      marshal_strv,
      demarshal_strv
    };
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_ARRAY_AS_STRING DBUS_TYPE_STRING_AS_STRING,
      &vtable
    };
    set_type_metadata (G_TYPE_STRV, &typedata);
  };


  /* Register some types specific to the D-BUS GLib bindings */
  {
    static const DBusGTypeMarshalVtable vtable = {
      marshal_proxy,
      demarshal_proxy
    };
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_OBJECT_PATH_AS_STRING,
      &vtable
    };
    set_type_metadata (DBUS_TYPE_G_PROXY, &typedata);
  }

  {
    static const DBusGTypeMarshalVtable vtable = {
      marshal_object_path,
      demarshal_object_path
    };
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_OBJECT_PATH_AS_STRING,
      &vtable
    };
    set_type_metadata (DBUS_TYPE_G_OBJECT_PATH, &typedata);
  }

  {
    static const DBusGTypeMarshalVtable vtable = {
      marshal_object,
      demarshal_object
    };
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_OBJECT_PATH_AS_STRING,
      &vtable
    };
    set_type_metadata (G_TYPE_OBJECT, &typedata);
  }

  {
    static const DBusGTypeMarshalVtable vtable = {
      marshal_signature,
      demarshal_signature
    };
    static const DBusGTypeMarshalData typedata = {
      DBUS_TYPE_SIGNATURE_AS_STRING,
      &vtable
    };
    set_type_metadata (DBUS_TYPE_G_SIGNATURE, &typedata);
  }

  types_initialized = TRUE;
}

/**
 * DBusGObjectPath:
 *
 * A typedef for a string used to represent D-Bus object paths.
 * Its GType is %DBUS_TYPE_G_OBJECT_PATH, derived from %G_TYPE_BOXED.
 *
 * Prior to version 0.FIXME this was used as the type name of
 * %DBUS_TYPE_G_OBJECT_PATH, but did not actually exist as a typedef.
 *
 * Since: 0.FIXME
 */

/**
 * DBUS_TYPE_G_OBJECT_PATH:
 *
 * The #GType of a #DBusGObjectPath, which is a boxed type containing a
 * D-Bus object path as a zero-terminated string. Object paths can be
 * copied with g_strdup() and freed with g_free(), just like %G_TYPE_STRING,
 * but have a distinct boxed type to allow them to be distinguished when
 * stored in a #GValue.
 *
 * Returns: a type derived from %G_TYPE_BOXED
 */
GType
dbus_g_object_path_get_g_type (void)
{
  static GType type_id = 0;

  if (!type_id)
    type_id = g_boxed_type_register_static ("DBusGObjectPath",
					    (GBoxedCopyFunc) g_strdup,
					    (GBoxedFreeFunc) g_free);
  return type_id;
}

/**
 * DBusGSignature:
 *
 * A typedef for a string used to represent D-Bus signatures.
 * Its GType is %DBUS_TYPE_G_SIGNATURE, derived from %G_TYPE_BOXED.
 *
 * Prior to version 0.FIXME this was used as the type name of
 * %DBUS_TYPE_G_SIGNATURE, but did not actually exist as a typedef.
 *
 * Since: 0.FIXME
 */

/**
 * DBUS_TYPE_G_SIGNATURE:
 *
 * The #GType of a #DBusGSignature, which is a boxed type containing a
 * D-Bus signature as a zero-terminated string. Signatures can be
 * copied with g_strdup() and freed with g_free(), just like %G_TYPE_STRING,
 * but have a distinct boxed type to allow them to be distinguished when
 * stored in a #GValue.
 *
 * Returns: a type derived from %G_TYPE_BOXED
 */
GType
dbus_g_signature_get_g_type (void)
{
  static GType type_id = 0;

  if (G_UNLIKELY (type_id == 0))
    type_id = g_boxed_type_register_static ("DBusGSignature",
					    (GBoxedCopyFunc) g_strdup,
					    (GBoxedFreeFunc) g_free);

  return type_id;
}


char *
_dbus_gtype_to_signature (GType gtype)
{
  char *ret;
  DBusGTypeMarshalData *typedata;

  if (dbus_g_type_is_collection (gtype))
    {
      GType elt_gtype;
      char *subsig;

      elt_gtype = dbus_g_type_get_collection_specialization (gtype);
      subsig = _dbus_gtype_to_signature (elt_gtype);
      ret = g_strconcat (DBUS_TYPE_ARRAY_AS_STRING, subsig, NULL);
      g_free (subsig);
    }
  else if (dbus_g_type_is_map (gtype))
    {
      GType key_gtype;
      GType val_gtype;
      char *key_subsig;
      char *val_subsig;

      key_gtype = dbus_g_type_get_map_key_specialization (gtype);
      val_gtype = dbus_g_type_get_map_value_specialization (gtype);
      key_subsig = _dbus_gtype_to_signature (key_gtype);
      val_subsig = _dbus_gtype_to_signature (val_gtype);
      ret = g_strconcat (DBUS_TYPE_ARRAY_AS_STRING DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING, key_subsig, val_subsig, DBUS_DICT_ENTRY_END_CHAR_AS_STRING, NULL);
      g_free (key_subsig);
      g_free (val_subsig);
    }
  else if (dbus_g_type_is_struct (gtype))
    {
      guint i, size;
      GString *sig;
      size = dbus_g_type_get_struct_size (gtype);
      sig = g_string_sized_new (size+2); /*some sensible starting size*/
      g_string_assign (sig, DBUS_STRUCT_BEGIN_CHAR_AS_STRING);
      for (i = 0; i < size; i++)
        {
          gchar *subsig;
          subsig = _dbus_gtype_to_signature (
              dbus_g_type_get_struct_member_type (gtype, i));
          g_string_append (sig, subsig);
          g_free (subsig);
        }
      g_string_append (sig, DBUS_STRUCT_END_CHAR_AS_STRING);
      ret = g_string_free (sig, FALSE);
    }
  else
    {
      typedata = g_type_get_qdata (gtype, dbus_g_type_metadata_data_quark ());
      if (typedata == NULL)
	return NULL;
      ret = g_strdup (typedata->sig);
    }
  return ret;
}

char *
_dbus_gvalue_to_signature (const GValue *val)
{
  GType gtype;

  gtype = G_VALUE_TYPE (val);
  if (g_type_is_a (gtype, G_TYPE_VALUE_ARRAY))
    {
      GString *str;
      guint i;
      GValueArray *array;

      array = g_value_get_boxed (val);
      
      str = g_string_new (DBUS_STRUCT_BEGIN_CHAR_AS_STRING);
      for (i = 0; i < array->n_values; i++)
	{
	  char *sig;
	  sig = _dbus_gvalue_to_signature (g_value_array_get_nth (array, i));
	  g_string_append (str, sig);
	  g_free (sig);
	}
      g_string_append (str, DBUS_STRUCT_END_CHAR_AS_STRING);
      
      return g_string_free (str, FALSE);
    }
  else
    return _dbus_gtype_to_signature (gtype);
}

static gboolean
demarshal_basic (DBusGValueMarshalCtx      *context,
		 DBusMessageIter           *iter,
		 GValue                    *value,
		 GError                   **error)
{
  int current_type;
  
  current_type = dbus_message_iter_get_arg_type (iter);
  g_assert (dbus_type_is_basic (current_type));

  switch (current_type)
    {
    case DBUS_TYPE_BOOLEAN:
      {
	dbus_bool_t bool;
        if (!G_VALUE_HOLDS (value, G_TYPE_BOOLEAN))
          goto invalid_type;
	dbus_message_iter_get_basic (iter, &bool);
	g_value_set_boolean (value, bool);
	return TRUE;
      }
    case DBUS_TYPE_BYTE:
      {
	unsigned char byte;
        if (!G_VALUE_HOLDS (value, G_TYPE_UCHAR))
          goto invalid_type;
	dbus_message_iter_get_basic (iter, &byte);
	g_value_set_uchar (value, byte);
	return TRUE;
      }
    case DBUS_TYPE_INT32:
      {
	dbus_int32_t intval;
        if (!G_VALUE_HOLDS (value, G_TYPE_INT))
          goto invalid_type;
	dbus_message_iter_get_basic (iter, &intval);
	g_value_set_int (value, intval);
	return TRUE;
      }
    case DBUS_TYPE_UINT32:
      {
	dbus_uint32_t intval;
        if (!G_VALUE_HOLDS (value, G_TYPE_UINT))
          goto invalid_type;
	dbus_message_iter_get_basic (iter, &intval);
	g_value_set_uint (value, intval);
	return TRUE;
      }
    case DBUS_TYPE_INT64:
      {
	dbus_int64_t intval;
        if (!G_VALUE_HOLDS (value, G_TYPE_INT64))
          goto invalid_type;
	dbus_message_iter_get_basic (iter, &intval);
	g_value_set_int64 (value, intval);
	return TRUE;
      }
    case DBUS_TYPE_UINT64:
      {
	dbus_uint64_t intval;
        if (!G_VALUE_HOLDS (value, G_TYPE_UINT64))
          goto invalid_type;
	dbus_message_iter_get_basic (iter, &intval);
	g_value_set_uint64 (value, intval);
	return TRUE;
      }
    case DBUS_TYPE_DOUBLE:
      {
	double dval;
        if (!G_VALUE_HOLDS (value, G_TYPE_DOUBLE))
          goto invalid_type;
	dbus_message_iter_get_basic (iter, &dval);
	g_value_set_double (value, dval);
	return TRUE;
      }
    case DBUS_TYPE_INT16:
      {
        dbus_int16_t v;
        if (!G_VALUE_HOLDS (value, G_TYPE_INT))
          goto invalid_type;
        dbus_message_iter_get_basic (iter, &v);
        g_value_set_int (value, v);
	return TRUE;
      }
    case DBUS_TYPE_UINT16:
      {
        dbus_uint16_t v;
        if (!G_VALUE_HOLDS (value, G_TYPE_UINT))
          goto invalid_type;
        dbus_message_iter_get_basic (iter, &v);
        g_value_set_uint (value, v);
	return TRUE;
      }
    case DBUS_TYPE_STRING:
      {
        const char *s;
        if (!G_VALUE_HOLDS (value, G_TYPE_STRING))
          goto invalid_type;
        dbus_message_iter_get_basic (iter, &s);
	g_value_set_string (value, s);
	return TRUE;
      }
    default:
      /* fall through to invalid_type */
      break;
    }
  invalid_type:
    g_set_error (error,
                 DBUS_GERROR,
                 DBUS_GERROR_INVALID_ARGS,
                 "Expected type %s, got type code \'%c\'",
                 g_type_name (G_VALUE_TYPE (value)),
                 (guchar) current_type);
    return FALSE;
}

static gboolean
demarshal_static_variant (DBusGValueMarshalCtx    *context,
			  DBusMessageIter         *iter,
			  GValue                  *value,
			  GError                 **error)
{
  char *sig;
  DBusMessageIter subiter;
  GType variant_type;

  dbus_message_iter_recurse (iter, &subiter);
  sig = dbus_message_iter_get_signature (&subiter);

  variant_type = _dbus_gtype_from_signature (sig, context->proxy != NULL);
  if (variant_type != G_TYPE_INVALID)
    {
      g_value_init (value, variant_type);

      if (!_dbus_gvalue_demarshal (context, &subiter, value, error))
	{
	  dbus_free (sig);
	  return FALSE;
	}
    }
  dbus_free (sig);
  return TRUE;
}

static gboolean
demarshal_variant (DBusGValueMarshalCtx    *context,
		   DBusMessageIter         *iter,
		   GValue                  *value,
		   GError                 **error)

{
  GValue *variant_val;
  variant_val = g_new0 (GValue, 1);

  if (!demarshal_static_variant (context, iter, variant_val, error))
    return FALSE;
  
  g_value_take_boxed (value, variant_val);
  return TRUE;
}

static gboolean
demarshal_proxy (DBusGValueMarshalCtx    *context,
		 DBusMessageIter         *iter,
		 GValue                  *value,
		 GError                 **error)
{
  DBusGProxy *new_proxy;
  const char *objpath;
  int current_type;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_OBJECT_PATH)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS object path, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  g_assert (context->proxy != NULL);
  
  dbus_message_iter_get_basic (iter, &objpath);

  new_proxy = dbus_g_proxy_new_from_proxy (context->proxy, NULL, objpath);
  g_value_take_object (value, new_proxy);

  return TRUE;
}

static gboolean
demarshal_object_path (DBusGValueMarshalCtx    *context,
		       DBusMessageIter         *iter,
		       GValue                  *value,
		       GError                 **error)
{
  const char *objpath;
  int current_type;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_OBJECT_PATH)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS object path, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  dbus_message_iter_get_basic (iter, &objpath);

  g_value_set_boxed (value, objpath);

  return TRUE;
}

static gboolean
demarshal_object (DBusGValueMarshalCtx    *context,
		  DBusMessageIter         *iter,
		  GValue                  *value,
		  GError                 **error)
{
  const char *objpath;
  int current_type;
  GObject *obj;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_OBJECT_PATH)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS object path, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }
  g_assert (context->proxy == NULL);

  dbus_message_iter_get_basic (iter, &objpath);

  obj = dbus_g_connection_lookup_g_object (context->gconnection, objpath);
  if (obj == NULL)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Unregistered object at path '%s'",
		   objpath);
      return FALSE;
    }
  g_value_set_object (value, obj);

  return TRUE;
}

static gboolean
demarshal_signature (DBusGValueMarshalCtx    *context,
		     DBusMessageIter         *iter,
		     GValue                  *value,
		     GError                 **error)
{
  const char *sig;
  int current_type;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_SIGNATURE)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS signature, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  dbus_message_iter_get_basic (iter, &sig);

  g_value_set_boxed (value, sig);

  return TRUE;
}

static gboolean
demarshal_strv (DBusGValueMarshalCtx    *context,
		DBusMessageIter         *iter,
		GValue                  *value,
		GError                 **error)
{
  DBusMessageIter subiter;
  int current_type;
  GArray *arr;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_ARRAY)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS array, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  dbus_message_iter_recurse (iter, &subiter);

  current_type = dbus_message_iter_get_arg_type (&subiter);
  if (current_type != DBUS_TYPE_INVALID
      && current_type != DBUS_TYPE_STRING)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS string, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  arr = g_array_new (TRUE, FALSE, sizeof (char *));

  while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID)
    {
      g_assert (current_type == DBUS_TYPE_STRING);
      const char *str;
      char *copy;
      
      dbus_message_iter_get_basic (&subiter, &str);
      copy = g_strdup (str);
      g_array_append_val (arr, copy);

      dbus_message_iter_next (&subiter);
    }

  g_value_take_boxed (value, arr->data);
  g_array_free (arr, FALSE);
  
  return TRUE;
}

static gboolean
demarshal_valuearray (DBusGValueMarshalCtx    *context,
		      DBusMessageIter         *iter,
		      GValue                  *value,
		      GError                 **error)
{
  int current_type;
  GValueArray *ret;
  DBusMessageIter subiter;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_STRUCT)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS struct, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  dbus_message_iter_recurse (iter, &subiter);

  ret = g_value_array_new (12);

  while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID)
    {
      GValue *val;
      GType elt_type; 
      char *current_sig;

      g_value_array_append (ret, NULL);
      val = g_value_array_get_nth (ret, ret->n_values - 1);

      current_sig = dbus_message_iter_get_signature (&subiter);
      elt_type = _dbus_gtype_from_signature (current_sig, TRUE);

      if (elt_type == G_TYPE_INVALID)
	{
	  g_value_array_free (ret);
	  g_set_error (error,
		       DBUS_GERROR,
		       DBUS_GERROR_INVALID_ARGS,
		       "Couldn't demarshal argument with signature \"%s\"", current_sig);
	  dbus_free (current_sig);
	  return FALSE;
	}
      dbus_free (current_sig);
      
      g_value_init (val, elt_type);

      if (!_dbus_gvalue_demarshal (context, &subiter, val, error))
	{
	  g_value_array_free (ret);
	  return FALSE;
	}

      dbus_message_iter_next (&subiter);
    }

  g_value_take_boxed (value, ret);
  
  return TRUE;
}

static gboolean
demarshal_map (DBusGValueMarshalCtx    *context,
	       DBusMessageIter         *iter,
	       GValue                  *value,
	       GError                 **error)
{
  GType gtype;
  DBusMessageIter subiter;
  int current_type;
  gpointer ret;
  GType key_gtype;
  GType value_gtype;
  DBusGTypeSpecializedAppendContext appendctx;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_ARRAY)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS array, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  gtype = G_VALUE_TYPE (value);

  dbus_message_iter_recurse (iter, &subiter);

  current_type = dbus_message_iter_get_arg_type (&subiter);
  if (current_type != DBUS_TYPE_INVALID
      && current_type != DBUS_TYPE_DICT_ENTRY)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS dict entry, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  key_gtype = dbus_g_type_get_map_key_specialization (gtype);
  value_gtype = dbus_g_type_get_map_value_specialization (gtype);

  ret = dbus_g_type_specialized_construct (gtype);
  g_value_take_boxed (value, ret);

  dbus_g_type_specialized_init_append (value, &appendctx);

  while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID)
    {
      DBusMessageIter entry_iter;
      GValue key_value = {0,};
      GValue value_value = {0,};

      current_type = dbus_message_iter_get_arg_type (&subiter);
      g_assert (current_type == DBUS_TYPE_DICT_ENTRY);

      dbus_message_iter_recurse (&subiter, &entry_iter);

      g_value_init (&key_value, key_gtype);
      if (!_dbus_gvalue_demarshal (context,
				  &entry_iter,
				  &key_value,
				  error))
	return FALSE;

      dbus_message_iter_next (&entry_iter);

      g_value_init (&value_value, value_gtype);
      if (!_dbus_gvalue_demarshal (context,
				  &entry_iter,
				  &value_value,
				  error))
	return FALSE;

      dbus_g_type_specialized_map_append (&appendctx, &key_value, &value_value);
      /* Ownership of values passes to map, don't unset */

      dbus_message_iter_next (&subiter);
    }
  
  return TRUE;
}

static gboolean
demarshal_struct (DBusGValueMarshalCtx    *context,
                  DBusMessageIter         *iter,
                  GValue                  *value,
                  GError                 **error)
{
  int current_type;
  DBusMessageIter subiter;
  guint i, size;
  GValue val = {0,};
  GType elt_type;

  current_type = dbus_message_iter_get_arg_type (iter);
  if (current_type != DBUS_TYPE_STRUCT)
    {
      g_set_error (error,
                   DBUS_GERROR,
                   DBUS_GERROR_INVALID_ARGS,
                   "Expected D-BUS struct, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  dbus_message_iter_recurse (iter, &subiter);

  g_value_take_boxed (value,
    dbus_g_type_specialized_construct (G_VALUE_TYPE (value)));

  size = dbus_g_type_get_struct_size (G_VALUE_TYPE (value));

  for (i=0; i < size; i++)
    {

      elt_type = dbus_g_type_get_struct_member_type (G_VALUE_TYPE(value), i);
      if (elt_type == G_TYPE_INVALID)
        {
          g_value_unset (value);
          g_set_error (error,
                       DBUS_GERROR,
                       DBUS_GERROR_INVALID_ARGS,
                       "Couldn't demarshal argument, "
                         "struct type %s has no member %d",
                       g_type_name (G_VALUE_TYPE(value)), i);
          return FALSE;
        }

      g_value_init (&val, elt_type);

      if (!_dbus_gvalue_demarshal (context, &subiter, &val, error))
        {
          g_value_unset (&val);
          g_value_unset (value);
          return FALSE;
        }
      if (!dbus_g_type_struct_set_member (value, i, &val))
        {
          g_value_unset (&val);
          g_value_unset (value);
          return FALSE;
        }

      dbus_message_iter_next (&subiter);
      g_value_unset (&val);
    }

  g_assert (dbus_message_iter_get_arg_type (&subiter) == DBUS_TYPE_INVALID);

  return TRUE;
}


static DBusGValueDemarshalFunc
get_type_demarshaller (GType type)
{
  DBusGTypeMarshalData *typedata;

  typedata = g_type_get_qdata (type, dbus_g_type_metadata_data_quark ());
  if (typedata == NULL)
    {
      if (g_type_is_a (type, G_TYPE_VALUE_ARRAY))
	return demarshal_valuearray;
      if (dbus_g_type_is_collection (type))
	return demarshal_collection;
      if (dbus_g_type_is_map (type))
	return demarshal_map;
      if (dbus_g_type_is_struct (type))
        return demarshal_struct;

      g_warning ("No demarshaller registered for type \"%s\"", g_type_name (type));
      return NULL;
    }
  g_assert (typedata->vtable);
  return typedata->vtable->demarshaller;
}

static gboolean
demarshal_collection (DBusGValueMarshalCtx    *context,
		      DBusMessageIter         *iter,
		      GValue                  *value,
		      GError                 **error)
{
  GType coltype;
  GType subtype;
  
  coltype = G_VALUE_TYPE (value);
  subtype = dbus_g_type_get_collection_specialization (coltype);

  if (_dbus_g_type_is_fixed (subtype))
    return demarshal_collection_array (context, iter, value, error);
  else
    return demarshal_collection_ptrarray (context, iter, value, error);
}

static gboolean
demarshal_collection_ptrarray (DBusGValueMarshalCtx    *context,
			       DBusMessageIter         *iter,
			       GValue                  *value,
			       GError                 **error)
{
  GType coltype;
  GType subtype;
  gpointer instance;
  DBusGTypeSpecializedAppendContext ctx;
  DBusGValueDemarshalFunc demarshaller;
  DBusMessageIter subiter;
  int current_type;

  current_type = dbus_message_iter_get_arg_type (iter);

  if (current_type != DBUS_TYPE_ARRAY)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Expected D-BUS array, got type code \'%c\'", (guchar) current_type);
      return FALSE;
    }

  dbus_message_iter_recurse (iter, &subiter);
  
  coltype = G_VALUE_TYPE (value);
  subtype = dbus_g_type_get_collection_specialization (coltype);

  demarshaller = get_type_demarshaller (subtype);

  if (!demarshaller)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "No demarshaller registered for type \"%s\" of collection \"%s\"",
		   g_type_name (coltype),
		   g_type_name (subtype));
      return FALSE;
    }

  instance = dbus_g_type_specialized_construct (coltype);
  g_value_take_boxed (value, instance);

  dbus_g_type_specialized_init_append (value, &ctx);

  while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID)
    {
      GValue eltval = {0, };

      g_value_init (&eltval, subtype);

      if (!demarshaller (context, &subiter, &eltval, error))
	{
	  dbus_g_type_specialized_collection_end_append (&ctx);
	  g_value_unset (value);
	  return FALSE;
	}
      dbus_g_type_specialized_collection_append (&ctx, &eltval);
      
      dbus_message_iter_next (&subiter);
    }
  dbus_g_type_specialized_collection_end_append (&ctx);
  
  return TRUE;
}

static gboolean
demarshal_collection_array (DBusGValueMarshalCtx    *context,
			    DBusMessageIter         *iter,
			    GValue                  *value,
			    GError                 **error)
{
  DBusMessageIter subiter;
  GArray *ret;
  GType elt_gtype;
  int elt_size;
  void *msgarray;
  int msgarray_len;

  dbus_message_iter_recurse (iter, &subiter);

  elt_gtype = dbus_g_type_get_collection_specialization (G_VALUE_TYPE (value));
  g_assert (elt_gtype != G_TYPE_INVALID);
  g_assert (_dbus_g_type_is_fixed (elt_gtype));

  elt_size = _dbus_g_type_fixed_get_size (elt_gtype);
  
  ret = g_array_new (FALSE, TRUE, elt_size);

  msgarray = NULL;
  dbus_message_iter_get_fixed_array (&subiter,
				     &msgarray,
				     &msgarray_len);
  g_assert (msgarray != NULL || msgarray_len == 0);

  if (msgarray_len)
    g_array_append_vals (ret, msgarray, (guint) msgarray_len);

  g_value_take_boxed (value, ret);
  
  return TRUE;
}

gboolean
_dbus_gvalue_demarshal (DBusGValueMarshalCtx    *context,
		       DBusMessageIter         *iter,
		       GValue                  *value,
		       GError                 **error)
{
  GType gtype;
  DBusGValueDemarshalFunc demarshaller;
  gboolean retcode = FALSE;

  if (context->recursion_depth > DBUS_GLIB_MAX_VARIANT_RECURSION)
    {
      g_set_error (error, DBUS_GERROR,
                   DBUS_GERROR_NO_MEMORY, 
                   "Variant recursion limit exceeded");
      return FALSE;
    }
  context->recursion_depth++;

  gtype = G_VALUE_TYPE (value);

  demarshaller = get_type_demarshaller (gtype);

  if (demarshaller == NULL)
    {
      g_set_error (error,
		   DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "No demarshaller registered for type \"%s\"",
		   g_type_name (gtype));
    
      goto out;
    }
  
  retcode = demarshaller (context, iter, value, error);
 out:
  context->recursion_depth--;  
  return retcode;
}

gboolean
_dbus_gvalue_demarshal_variant (DBusGValueMarshalCtx    *context,
			       DBusMessageIter         *iter,
			       GValue                  *value,
			       GError                 **error)
{
  return demarshal_static_variant (context, iter, value, error);
}

GValueArray *
_dbus_gvalue_demarshal_message  (DBusGValueMarshalCtx    *context,
				DBusMessage             *message,
				guint                    n_types,
				const GType             *types,
				GError                 **error)
{
  GValueArray *ret;
  DBusMessageIter iter;
  int current_type;
  guint index_;
  
  ret = g_value_array_new (6);  /* 6 is a typical maximum for arguments */

  dbus_message_iter_init (message, &iter);
  index_ = 0;
  while ((current_type = dbus_message_iter_get_arg_type (&iter)) != DBUS_TYPE_INVALID)
    {
      GValue *value;
      GType gtype;

      if (index_ >= n_types)
	{
	  g_set_error (error, DBUS_GERROR,
		       DBUS_GERROR_INVALID_ARGS,
		       "Too many arguments in message");
	  goto lose;
	}
      
      g_value_array_append (ret, NULL);
      value = g_value_array_get_nth (ret, index_);

      gtype = types[index_]; 
      g_value_init (value, gtype);

      if (!_dbus_gvalue_demarshal (context, &iter, value, error))
	goto lose;
      dbus_message_iter_next (&iter);
      index_++;
    }
  if (index_ < n_types)
    {
      g_set_error (error, DBUS_GERROR,
		   DBUS_GERROR_INVALID_ARGS,
		   "Too few arguments in message");
      goto lose;
    }

  return ret;
 lose:
  g_value_array_free (ret);
  return NULL;
}

static void oom (void) G_GNUC_NORETURN;
static void
oom (void)
{
  g_error ("no memory");
}

static gboolean
marshal_basic (DBusMessageIter *iter, const GValue *value)
{
  GType value_type;

  value_type = G_VALUE_TYPE (value);
  
  switch (value_type)
    {
    case G_TYPE_CHAR:
      {
        char b = g_value_get_char (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_BYTE,
                                             &b))
          oom ();
      }
      return TRUE;
    case G_TYPE_UCHAR:
      {
        unsigned char b = g_value_get_uchar (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_BYTE,
                                             &b))
          oom ();
      }
      return TRUE;
    case G_TYPE_BOOLEAN:
      {
        dbus_bool_t b = g_value_get_boolean (value);

        g_return_val_if_fail (b == TRUE || b == FALSE, FALSE);

        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_BOOLEAN,
                                             &b))
          oom ();
      }
      return TRUE;
    case G_TYPE_INT:
      {
        dbus_int32_t v = g_value_get_int (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_INT32,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_UINT:
      {
        dbus_uint32_t v = g_value_get_uint (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_UINT32,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_LONG:
      {
        dbus_int32_t v = g_value_get_long (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_INT32,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_ULONG:
      {
        dbus_uint32_t v = g_value_get_ulong (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_UINT32,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_INT64:
      {
        gint64 v = g_value_get_int64 (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_INT64,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_UINT64:
      {
        guint64 v = g_value_get_uint64 (value);
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_UINT64,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_FLOAT:
      {
        double v = g_value_get_float (value);
        
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_DOUBLE,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_DOUBLE:
      {
        double v = g_value_get_double (value);
        
        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_DOUBLE,
                                             &v))
          oom ();
      }
      return TRUE;
    case G_TYPE_STRING:
      {
        const char *v = g_value_get_string (value);
	if (!v)
	  v = "";

        /* FIXME: fd.o #16320: consider using g_return_if_fail to check UTF-8
         * validity */

        if (!dbus_message_iter_append_basic (iter,
                                             DBUS_TYPE_STRING,
                                             &v))
          {
            gchar *s = g_strdup_value_contents (value);

            g_critical ("Unable to marshal string (not UTF-8 or OOM?): %s",
                s);
            g_free (s);
            return FALSE;
          }
      }
      return TRUE;
      
    default:
      {
	g_assert_not_reached ();
	return FALSE;
      }
    }
}

static gboolean
marshal_strv (DBusMessageIter   *iter,
	      const GValue       *value)
{
  DBusMessageIter subiter;
  char **array;
  char **elt;
  gboolean ret = FALSE;

  g_assert (G_VALUE_TYPE (value) == g_strv_get_type ());

  array = g_value_get_boxed (value);

  if (!dbus_message_iter_open_container (iter,
					 DBUS_TYPE_ARRAY,
					 "s",
					 &subiter))
    goto out;

  if (array)
    {
      for (elt = array; *elt; elt++)
        {
          if (!dbus_message_iter_append_basic (&subiter,
					   DBUS_TYPE_STRING,
					   elt))
	        goto out;
        }
    }

  if (!dbus_message_iter_close_container (iter, &subiter))
    goto out;
  ret = TRUE;
 out:
  return ret;
}

static gboolean
marshal_valuearray (DBusMessageIter   *iter,
		    const GValue       *value)
{
  GValueArray *array;
  guint i;
  DBusMessageIter subiter;

  g_assert (G_VALUE_TYPE (value) == G_TYPE_VALUE_ARRAY);

  array = g_value_get_boxed (value);

  if (!dbus_message_iter_open_container (iter,
					 DBUS_TYPE_STRUCT,
					 NULL,
					 &subiter))
    oom ();

  if (array)
    {
      for (i = 0; i < array->n_values; i++)
        {
          if (!_dbus_gvalue_marshal (&subiter, g_value_array_get_nth (array, i)))
            {
              dbus_message_iter_abandon_container (iter, &subiter);
              return FALSE;
            }
        }
    }

  return dbus_message_iter_close_container (iter, &subiter);
}

static gboolean
marshal_proxy (DBusMessageIter         *iter,
	       const GValue            *value)
{
  const char *path;
  DBusGProxy *proxy;

  g_assert (G_VALUE_TYPE (value) == dbus_g_proxy_get_type ());

  proxy = g_value_get_object (value);
  g_return_val_if_fail (DBUS_IS_G_PROXY (proxy), FALSE);
  path = dbus_g_proxy_get_path (proxy);
  g_return_val_if_fail (g_variant_is_object_path (path), FALSE);

  if (!dbus_message_iter_append_basic (iter,
				       DBUS_TYPE_OBJECT_PATH,
				       &path))
    oom ();

  return TRUE;
}

static gboolean
marshal_object_path (DBusMessageIter         *iter,
		     const GValue            *value)
{
  const char *path;

  g_assert (G_VALUE_TYPE (value) == DBUS_TYPE_G_OBJECT_PATH);

  path = g_value_get_boxed (value);
  g_return_val_if_fail (g_variant_is_object_path (path), FALSE);

  if (!dbus_message_iter_append_basic (iter,
				       DBUS_TYPE_OBJECT_PATH,
				       &path))
    oom ();

  return TRUE;
}

static gboolean
marshal_object (DBusMessageIter         *iter,
		const GValue            *value)
{
  const char *path;
  GObject *obj;

  obj = g_value_get_object (value);
  g_return_val_if_fail (G_IS_OBJECT (obj), FALSE);

  path = _dbus_gobject_get_path (obj);
  g_return_val_if_fail (g_variant_is_object_path (path), FALSE);

  if (!dbus_message_iter_append_basic (iter,
				       DBUS_TYPE_OBJECT_PATH,
				       &path))
    oom ();

  return TRUE;
}

static gboolean
marshal_signature (DBusMessageIter         *iter,
		   const GValue            *value)
{
  const char *sig;

  g_assert (G_VALUE_TYPE (value) == DBUS_TYPE_G_SIGNATURE);

  sig = g_value_get_boxed (value);
  g_return_val_if_fail (g_variant_is_signature (sig), FALSE);

  /* failure here isn't strictly *guaranteed* to be OOM, since GDBus might
   * understand more type-codes than our libdbus */
  if (!dbus_message_iter_append_basic (iter,
				       DBUS_TYPE_SIGNATURE,
				       &sig))
    return FALSE;

  return TRUE;
}

struct DBusGLibHashMarshalData
{
  const char *entry_sig;
  DBusMessageIter *iter;
  gboolean err;
};

static void
marshal_map_entry (const GValue *key,
		   const GValue *value,
		   gpointer data)
{
  struct DBusGLibHashMarshalData *hashdata = data;
  DBusMessageIter subiter;

  if (hashdata->err)
    return;

  if (!dbus_message_iter_open_container (hashdata->iter,
					 DBUS_TYPE_DICT_ENTRY,
					 NULL,
					 &subiter))
    goto lose;

  if (!_dbus_gvalue_marshal (&subiter, key))
    goto lose;

  if (!_dbus_gvalue_marshal (&subiter, value))
    goto lose;

  if (!dbus_message_iter_close_container (hashdata->iter, &subiter))
    goto lose;
  
  return;
 lose:
  hashdata->err = TRUE;
}

static gboolean
marshal_map (DBusMessageIter   *iter,
	     const GValue      *value)
{
  GType gtype;
  DBusMessageIter arr_iter;
  struct DBusGLibHashMarshalData hashdata;
  char *key_sig;
  char *value_sig;
  GType key_type;
  GType value_type;
  char *entry_sig;
  char *array_sig;

  gtype = G_VALUE_TYPE (value);

  key_type = dbus_g_type_get_map_key_specialization (gtype);
  g_assert (_dbus_gtype_is_valid_hash_key (key_type));
  value_type = dbus_g_type_get_map_value_specialization (gtype);
  g_assert (_dbus_gtype_is_valid_hash_value (value_type));

  key_sig = _dbus_gtype_to_signature (key_type);
  if (!key_sig)
    {
      g_warning ("Cannot marshal type \"%s\" in map\n", g_type_name (key_type));
      return FALSE;
    }
  value_sig = _dbus_gtype_to_signature (value_type);
  if (!value_sig)
    {
      g_free (key_sig);
      g_warning ("Cannot marshal type \"%s\" in map\n", g_type_name (value_type));
      return FALSE;
    }
  entry_sig = g_strdup_printf ("%s%s", key_sig, value_sig);
  g_free (key_sig);
  g_free (value_sig);
  array_sig = g_strdup_printf ("%c%s%c",
			       DBUS_DICT_ENTRY_BEGIN_CHAR,
			       entry_sig,
			       DBUS_DICT_ENTRY_END_CHAR);
  if (!dbus_message_iter_open_container (iter,
					 DBUS_TYPE_ARRAY,
					 array_sig,
					 &arr_iter))
    goto lose;

  hashdata.iter = &arr_iter;
  hashdata.err = FALSE;
  hashdata.entry_sig = entry_sig;

  dbus_g_type_map_value_iterate (value,
				 marshal_map_entry,
				 &hashdata);

  if (hashdata.err)
    {
      dbus_message_iter_abandon_container (iter, &arr_iter);
      goto lose;
    }
  else if (!dbus_message_iter_close_container (iter, &arr_iter))
    {
      goto lose;
    }

 out:
  g_free (entry_sig);
  g_free (array_sig);
  return !hashdata.err;
 lose:
  hashdata.err = TRUE;
  goto out;
}

static gboolean
marshal_struct (DBusMessageIter   *iter,
                const GValue      *value)
{
  GType gtype;
  DBusMessageIter subiter;
  guint size, i;
  GValue val = {0,};

  gtype = G_VALUE_TYPE (value);

  size = dbus_g_type_get_struct_size (gtype);

  if (!dbus_message_iter_open_container (iter,
                                         DBUS_TYPE_STRUCT,
                                         NULL,
                                         &subiter))
    oom ();

  for (i = 0; i < size; i++)
    {
      g_value_init (&val, dbus_g_type_get_struct_member_type
          (G_VALUE_TYPE(value), i));

      if (!dbus_g_type_struct_get_member (value, i, &val))
        goto abandon;

      if (!_dbus_gvalue_marshal (&subiter, &val))
        goto abandon;

      g_value_unset(&val);
    }

  return dbus_message_iter_close_container (iter, &subiter);

abandon:
  dbus_message_iter_abandon_container (iter, &subiter);
  return FALSE;
}

static gboolean
marshal_variant (DBusMessageIter          *iter,
		 const GValue             *value)
{
  GType value_gtype;
  DBusMessageIter subiter;
  char *variant_sig;
  GValue *real_value;
  gboolean ret = FALSE;

  real_value = g_value_get_boxed (value);
  value_gtype = G_VALUE_TYPE (real_value);

  variant_sig = _dbus_gvalue_to_signature (real_value);
  if (variant_sig == NULL)
    {
      g_warning ("Cannot marshal type \"%s\" in variant", g_type_name (value_gtype));
      return FALSE;
    }

  if (!dbus_message_iter_open_container (iter,
					 DBUS_TYPE_VARIANT,
					 variant_sig,
					 &subiter))
    goto out;

  if (!_dbus_gvalue_marshal (&subiter, real_value))
    {
      dbus_message_iter_abandon_container (iter, &subiter);
      goto out;
    }

  if (!dbus_message_iter_close_container (iter, &subiter))
    goto out;

  ret = TRUE;
 out:
  g_free (variant_sig);
  return ret;
}

static DBusGValueMarshalFunc
get_type_marshaller (GType type)
{
  DBusGTypeMarshalData *typedata;

  typedata = g_type_get_qdata (type, dbus_g_type_metadata_data_quark ());
  if (typedata == NULL)
    {
      if (g_type_is_a (type, G_TYPE_VALUE_ARRAY))
	return marshal_valuearray;
      if (dbus_g_type_is_collection (type))
	return marshal_collection;
      if (dbus_g_type_is_map (type))
	return marshal_map;
      if (dbus_g_type_is_struct (type))
	return marshal_struct;

      g_warning ("No marshaller registered for type \"%s\"", g_type_name (type));
      return NULL;
    }
  g_assert (typedata->vtable);
  return typedata->vtable->marshaller;
}

typedef struct
{
  DBusMessageIter *iter;
  DBusGValueMarshalFunc marshaller;
  gboolean err;
} DBusGValueCollectionMarshalData;

static void
collection_marshal_iterator (const GValue *eltval,
			     gpointer      user_data)
{
  DBusGValueCollectionMarshalData *data = user_data;

  if (data->err)
    return;

  if (!data->marshaller (data->iter, eltval))
    data->err = TRUE;
}

static gboolean
marshal_collection (DBusMessageIter         *iter,
		    const GValue            *value)
{
  GType coltype;
  GType subtype;
  
  coltype = G_VALUE_TYPE (value);
  subtype = dbus_g_type_get_collection_specialization (coltype);

  if (_dbus_g_type_is_fixed (subtype))
    return marshal_collection_array (iter, value);
  else
    return marshal_collection_ptrarray (iter, value);
}

static gboolean
marshal_collection_ptrarray (DBusMessageIter         *iter,
			     const GValue            *value)
{
  GType coltype;
  GType elt_gtype;
  DBusGValueCollectionMarshalData data;
  DBusMessageIter subiter;
  char *elt_sig;
  
  coltype = G_VALUE_TYPE (value);
  elt_gtype = dbus_g_type_get_collection_specialization (coltype);
  data.marshaller = get_type_marshaller (elt_gtype);
  if (!data.marshaller)
    return FALSE;

  elt_sig = _dbus_gtype_to_signature (elt_gtype);
  if (!elt_sig)
    {
      g_warning ("Cannot marshal type \"%s\" in collection\n", g_type_name (elt_gtype));
      return FALSE;
    }
  g_assert (g_variant_is_signature (elt_sig));

  if (!dbus_message_iter_open_container (iter,
					 DBUS_TYPE_ARRAY,
					 elt_sig,
					 &subiter))
    oom ();

  g_free (elt_sig);

  data.iter = &subiter;
  data.err = FALSE;

  dbus_g_type_collection_value_iterate (value,
					collection_marshal_iterator,
					&data);

  if (data.err)
    {
      dbus_message_iter_abandon_container (iter, &subiter);
      return FALSE;
    }

  return dbus_message_iter_close_container (iter, &subiter);
}

/* If any of these assertions are violated, then marshal_collection_array
 * is buggy for that type. dbus_g_value_basic_array_parse_variant() has
 * general size-conversion code, if needed. */
G_STATIC_ASSERT (sizeof (dbus_bool_t) == sizeof (gboolean));
G_STATIC_ASSERT (sizeof (dbus_int32_t) == sizeof (gint));
G_STATIC_ASSERT (sizeof (dbus_uint32_t) == sizeof (guint));
/* These should be true regardless, but just for completeness... */
G_STATIC_ASSERT (sizeof (dbus_int64_t) == sizeof (gint64));
G_STATIC_ASSERT (sizeof (dbus_uint64_t) == sizeof (guint64));
G_STATIC_ASSERT (sizeof (double) == sizeof (gdouble));

static gboolean
marshal_collection_array (DBusMessageIter   *iter,
			  const GValue      *value)
{
  GType elt_gtype;
  DBusMessageIter subiter;
  GArray *array;
  char *subsignature_str;

  array = g_value_get_boxed (value);
  g_return_val_if_fail (array != NULL, FALSE);

  elt_gtype = dbus_g_type_get_collection_specialization (G_VALUE_TYPE (value));
  g_assert (_dbus_g_type_is_fixed (elt_gtype));
  subsignature_str = _dbus_gtype_to_signature (elt_gtype);
  if (!subsignature_str)
    {
      g_warning ("Cannot marshal type \"%s\" in collection\n", g_type_name (elt_gtype));
      return FALSE;
    }
  g_assert (g_variant_is_signature (subsignature_str));

  if (!dbus_message_iter_open_container (iter,
					 DBUS_TYPE_ARRAY,
					 subsignature_str,
					 &subiter))
    oom ();

  /* TODO - This assumes that basic values are the same size
   * is this always true?  If it is we can probably avoid
   * a lot of the overhead in _marshal_basic_instance...
   */
  if (!dbus_message_iter_append_fixed_array (&subiter,
					     subsignature_str[0],
					     &(array->data),
					     array->len))
    {
      g_critical ("Unable to serialize %u GArray members as signature %s "
          "(OOM or invalid boolean value?)", array->len, subsignature_str);

      g_free (subsignature_str);
      dbus_message_iter_abandon_container (iter, &subiter);
      return FALSE;
    }

  g_free (subsignature_str);
  return dbus_message_iter_close_container (iter, &subiter);
}

gboolean
_dbus_gvalue_marshal (DBusMessageIter         *iter,
		     const GValue       *value)
{
  GType gtype;
  DBusGValueMarshalFunc marshaller;

  gtype = G_VALUE_TYPE (value);

  marshaller = get_type_marshaller (gtype);
  if (marshaller == NULL)
    return FALSE;
  return marshaller (iter, value);
}

#ifdef DBUS_BUILD_TESTS

static void
assert_type_maps_to (GType gtype, const char *expected_sig)
{
  char *sig;
  sig = _dbus_gtype_to_signature (gtype);
  g_assert (sig != NULL);
  g_assert (!strcmp (expected_sig, sig));
  g_free (sig);
}

static void
assert_signature_maps_to (const char *sig, GType expected_gtype)
{
  g_assert (_dbus_gtype_from_signature (sig, TRUE) == expected_gtype);
}

static void
assert_bidirectional_mapping (GType gtype, const char *expected_sig)
{
  assert_type_maps_to (gtype, expected_sig);
  assert_signature_maps_to (expected_sig, gtype);
}

/*
 * Unit test for general glib stuff
 * Returns: %TRUE on success.
 */
gboolean
_dbus_gvalue_test (const char *test_data_dir)
{
  _dbus_g_value_types_init ();
  
  assert_bidirectional_mapping (G_TYPE_STRING, DBUS_TYPE_STRING_AS_STRING);
  assert_bidirectional_mapping (G_TYPE_UCHAR, DBUS_TYPE_BYTE_AS_STRING);
  assert_bidirectional_mapping (G_TYPE_UINT, DBUS_TYPE_UINT32_AS_STRING);

  assert_bidirectional_mapping (dbus_g_type_get_map ("GHashTable", G_TYPE_STRING, G_TYPE_VALUE),
			      DBUS_TYPE_ARRAY_AS_STRING DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING DBUS_TYPE_STRING_AS_STRING DBUS_TYPE_VARIANT_AS_STRING DBUS_DICT_ENTRY_END_CHAR_AS_STRING);
  assert_bidirectional_mapping (dbus_g_type_get_collection ("GPtrArray", DBUS_TYPE_G_OBJECT_PATH),
				DBUS_TYPE_ARRAY_AS_STRING DBUS_TYPE_OBJECT_PATH_AS_STRING);
  assert_bidirectional_mapping (dbus_g_type_get_collection ("GArray", G_TYPE_INT),
				DBUS_TYPE_ARRAY_AS_STRING DBUS_TYPE_INT32_AS_STRING);

  assert_bidirectional_mapping (dbus_g_type_get_struct ("GValueArray", G_TYPE_INT, G_TYPE_STRING, DBUS_TYPE_G_OBJECT_PATH, G_TYPE_INVALID),
  				DBUS_STRUCT_BEGIN_CHAR_AS_STRING DBUS_TYPE_INT32_AS_STRING DBUS_TYPE_STRING_AS_STRING DBUS_TYPE_OBJECT_PATH_AS_STRING DBUS_STRUCT_END_CHAR_AS_STRING );
  return TRUE;
}

#endif /* DBUS_BUILD_TESTS */
