/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gsignature.c Mapping from dbus type signatures to GType
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

#include "config.h"
#include "dbus-gtest.h"
#include "dbus-gsignature.h"
#include "dbus-gvalue-utils.h"
#include <string.h>
#include <glib.h>

#define MAP_BASIC(d_t, g_t)                     \
    case DBUS_TYPE_##d_t:                       \
      return G_TYPE_##g_t;
static GType
typecode_to_gtype (int type)
{
  switch (type)
    {
      MAP_BASIC (BOOLEAN, BOOLEAN);
      MAP_BASIC (BYTE,    UCHAR);
      MAP_BASIC (INT16,   INT);
      MAP_BASIC (INT32,   INT);
      MAP_BASIC (UINT16,  UINT);
      MAP_BASIC (UINT32,  UINT);
      MAP_BASIC (INT64,   INT64);
      MAP_BASIC (UINT64,  UINT64);
      MAP_BASIC (DOUBLE,  DOUBLE);
      MAP_BASIC (STRING,  STRING);
    default:
      return G_TYPE_INVALID;
    }
}
#undef MAP_BASIC

static gboolean
dbus_typecode_maps_to_basic (int typecode)
{
  return typecode_to_gtype (typecode) != G_TYPE_INVALID;
}

GType
_dbus_gtype_from_basic_typecode (int typecode)
{
  g_assert (dbus_type_is_basic (typecode));
  g_assert (dbus_typecode_maps_to_basic (typecode));
  return typecode_to_gtype (typecode);
}

static GType
signature_iter_to_g_type_dict (const DBusSignatureIter *subiter, gboolean is_client)
{
  DBusSignatureIter iter;
  GType key_gtype;
  GType value_gtype;

  g_assert (dbus_signature_iter_get_current_type (subiter) == DBUS_TYPE_DICT_ENTRY);

  dbus_signature_iter_recurse (subiter, &iter);

  key_gtype = _dbus_gtype_from_signature_iter (&iter, is_client); 
  if (key_gtype == G_TYPE_INVALID)
    return G_TYPE_INVALID;

  dbus_signature_iter_next (&iter);
  value_gtype = _dbus_gtype_from_signature_iter (&iter, is_client);
  if (value_gtype == G_TYPE_INVALID)
    return G_TYPE_INVALID;

  if (!_dbus_gtype_is_valid_hash_key (key_gtype)
      || !_dbus_gtype_is_valid_hash_value (value_gtype))
    /* Later we need to return DBUS_TYPE_G_VALUE */
    return G_TYPE_INVALID; 

  return dbus_g_type_get_map ("GHashTable", key_gtype, value_gtype);
}

static GType
signature_iter_to_g_type_array (DBusSignatureIter *iter, gboolean is_client)
{
  GType elt_gtype;

  elt_gtype = _dbus_gtype_from_signature_iter (iter, is_client);
  if (elt_gtype == G_TYPE_INVALID)
    return G_TYPE_INVALID;

  if (elt_gtype == G_TYPE_OBJECT)
    return DBUS_TYPE_G_OBJECT_ARRAY;
  if (elt_gtype == G_TYPE_STRING)
    return G_TYPE_STRV;
  if (_dbus_g_type_is_fixed (elt_gtype))
    return dbus_g_type_get_collection ("GArray", elt_gtype);
  else if (g_type_is_a (elt_gtype, G_TYPE_OBJECT)
	   || g_type_is_a (elt_gtype, G_TYPE_BOXED))
    return dbus_g_type_get_collection ("GPtrArray", elt_gtype);

  /* Later we need to return DBUS_TYPE_G_VALUE */
  return G_TYPE_INVALID; 
}

static GType
signature_iter_to_g_type_struct (DBusSignatureIter *iter, gboolean is_client)
{
  GArray *types;
  GType ret;
  types = g_array_new (FALSE, FALSE, sizeof (GType));
  do
    {
      GType curtype;
      curtype = _dbus_gtype_from_signature_iter (iter, is_client);
      g_array_append_val (types, curtype);
    }
  while (dbus_signature_iter_next (iter));

  ret = dbus_g_type_get_structv ("GValueArray", types->len, (GType*) types->data);
  g_array_free (types, TRUE);
  return ret;
}

GType
_dbus_gtype_from_signature_iter (DBusSignatureIter *iter, gboolean is_client)
{
  int current_type;

  current_type = dbus_signature_iter_get_current_type (iter);
  /* TODO: handle type 0? */
  if (dbus_typecode_maps_to_basic (current_type))
    return _dbus_gtype_from_basic_typecode (current_type);
  else if (current_type == DBUS_TYPE_OBJECT_PATH)
    return DBUS_TYPE_G_OBJECT_PATH;
  else if (current_type == DBUS_TYPE_SIGNATURE)
    return DBUS_TYPE_G_SIGNATURE;
  else
    {
      DBusSignatureIter subiter;

      g_assert (dbus_type_is_container (current_type));

      if (current_type == DBUS_TYPE_VARIANT)
	return G_TYPE_VALUE;
      
      dbus_signature_iter_recurse (iter, &subiter);

      if (current_type == DBUS_TYPE_ARRAY)
	{
	  int elt_type = dbus_signature_iter_get_current_type (&subiter);
	  if (elt_type == DBUS_TYPE_DICT_ENTRY)
	    return signature_iter_to_g_type_dict (&subiter, is_client);
	  else 
	    return signature_iter_to_g_type_array (&subiter, is_client);
	}
      else if (current_type == DBUS_TYPE_STRUCT)
        {
          return signature_iter_to_g_type_struct (&subiter, is_client);
        }
      else
	{
	  g_assert_not_reached ();
	  return G_TYPE_INVALID;
	}
    }
}

GType
_dbus_gtype_from_signature (const char *signature, gboolean is_client)
{
  DBusSignatureIter iter;

  dbus_signature_iter_init (&iter, signature);

  return _dbus_gtype_from_signature_iter (&iter, is_client);
}

GArray *
_dbus_gtypes_from_arg_signature (const char *argsig, gboolean is_client)
{
  GArray *ret;
  int current_type;
  DBusSignatureIter sigiter;

  ret = g_array_new (FALSE, FALSE, sizeof (GType));

  dbus_signature_iter_init (&sigiter, argsig);
  while ((current_type = dbus_signature_iter_get_current_type (&sigiter)) != DBUS_TYPE_INVALID)
    {
      GType curtype;

      curtype = _dbus_gtype_from_signature_iter (&sigiter, is_client);
      g_array_append_val (ret, curtype);
      dbus_signature_iter_next (&sigiter);
    }
  return ret;
}
