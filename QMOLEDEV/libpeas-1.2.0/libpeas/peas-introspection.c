/*
 * peas-introspection.c
 * This file is part of libpeas
 *
 * Copyright (C) 2010 Steve Frécinaux
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "peas-introspection.h"

void
peas_gi_valist_to_arguments (GICallableInfo *callable_info,
                             va_list         va_args,
                             GIArgument     *arguments,
                             gpointer       *return_value)
{
  gint i, n_args;
  GIArgInfo arg_info;
  GITypeInfo arg_type_info;
  GITypeInfo retval_info;
  GIArgument *cur_arg;

  g_return_if_fail (callable_info != NULL);

  n_args = g_callable_info_get_n_args (callable_info);

  for (i = 0; i < n_args; i++)
    {
      g_callable_info_load_arg (callable_info, i, &arg_info);
      g_arg_info_load_type (&arg_info, &arg_type_info);
      cur_arg = &arguments[i];

      switch (g_arg_info_get_direction (&arg_info))
        {
        case GI_DIRECTION_IN:
          {
            /* Notes: According to GCC 4.4,
             *  - int8, uint8, int16, uint16, short and ushort are promoted to int when passed through '...'
             *  - float is promoted to double when passed through '...'
             */
            switch (g_type_info_get_tag (&arg_type_info))
              {
              case GI_TYPE_TAG_VOID:
                cur_arg->v_pointer = va_arg (va_args, gpointer);
                break;
              case GI_TYPE_TAG_BOOLEAN:
                cur_arg->v_boolean = va_arg (va_args, gboolean);
                break;
              case GI_TYPE_TAG_INT8:
                cur_arg->v_int8 = va_arg (va_args, gint);
                break;
              case GI_TYPE_TAG_UINT8:
                cur_arg->v_uint8 = va_arg (va_args, gint);
                break;
              case GI_TYPE_TAG_INT16:
                cur_arg->v_int16 = va_arg (va_args, gint);
                break;
              case GI_TYPE_TAG_UINT16:
                cur_arg->v_uint16 = va_arg (va_args, gint);
                break;
              case GI_TYPE_TAG_INT32:
                cur_arg->v_int32 = va_arg (va_args, gint32);
                break;
              case GI_TYPE_TAG_UNICHAR:
              case GI_TYPE_TAG_UINT32:
                cur_arg->v_uint32 = va_arg (va_args, guint32);
                break;
              case GI_TYPE_TAG_INT64:
                cur_arg->v_int64 = va_arg (va_args, gint64);
                break;
              case GI_TYPE_TAG_UINT64:
                cur_arg->v_uint64 = va_arg (va_args, guint64);
                break;
              case GI_TYPE_TAG_FLOAT:
                cur_arg->v_float = va_arg (va_args, gdouble);
                break;
              case GI_TYPE_TAG_DOUBLE:
                cur_arg->v_double = va_arg (va_args, gdouble);
                break;
              case GI_TYPE_TAG_GTYPE:
                /* apparently, GType is meant to be a gsize, from gobject/gtype.h in glib */
                cur_arg->v_size = va_arg (va_args, GType);
                break;
              case GI_TYPE_TAG_UTF8:
              case GI_TYPE_TAG_FILENAME:
                cur_arg->v_string = va_arg (va_args, gchar *);
                break;
              case GI_TYPE_TAG_ARRAY:
              case GI_TYPE_TAG_INTERFACE:
              case GI_TYPE_TAG_GLIST:
              case GI_TYPE_TAG_GSLIST:
              case GI_TYPE_TAG_GHASH:
              case GI_TYPE_TAG_ERROR:
                cur_arg->v_pointer = va_arg (va_args, gpointer);
                break;
              default:
                g_warn_if_reached ();
                cur_arg->v_pointer = va_arg (va_args, gpointer);
                break;
              }
            break;
          }
        /* In the other cases, we expect we will always have a pointer. */
        case GI_DIRECTION_INOUT:
        case GI_DIRECTION_OUT:
          cur_arg->v_pointer = va_arg (va_args, gpointer);
          break;
        }
    }

  if (return_value != NULL)
    {
      g_callable_info_load_return_type (callable_info, &retval_info);

      if (g_type_info_get_tag (&retval_info) != GI_TYPE_TAG_VOID)
        *return_value = va_arg (va_args, gpointer);
      else
        *return_value = NULL;
    }
}

static void
peas_gi_split_in_and_out_arguments (GICallableInfo *callable_info,
                                    GIArgument     *args,
                                    GIArgument     *in_args,
                                    guint          *n_in_args,
                                    GIArgument     *out_args,
                                    guint          *n_out_args)
{
  gint n_args, i;
  GIArgInfo arg_info;

  g_return_if_fail (callable_info != NULL);

  n_args = g_callable_info_get_n_args (callable_info);

  for (i = 0; i < n_args; i++)
    {
      g_callable_info_load_arg (callable_info, i, &arg_info);

      switch (g_arg_info_get_direction (&arg_info))
        {
        case GI_DIRECTION_IN:
          in_args[(*n_in_args)++] = args[i];
          break;
        case GI_DIRECTION_INOUT:
          in_args[(*n_in_args)++] = args[i];
          out_args[(*n_out_args)++] = args[i];
          break;
        case GI_DIRECTION_OUT:
          out_args[(*n_out_args)++] = args[i];
          break;
        }
    }
}

void
peas_gi_argument_to_pointer (GITypeInfo     *type_info,
                             GIArgument     *arg,
                             gpointer        ptr)
{
  switch (g_type_info_get_tag (type_info))
    {
    case GI_TYPE_TAG_VOID:
      *((gpointer **) ptr) = arg->v_pointer;
      break;
    case GI_TYPE_TAG_BOOLEAN:
      *((gboolean *) ptr) = arg->v_boolean;
      break;
    case GI_TYPE_TAG_INT8:
      *((gint8 *) ptr) = arg->v_int8;
      break;
    case GI_TYPE_TAG_UINT8:
      *((guint8 *) ptr) = arg->v_uint8;
      break;
    case GI_TYPE_TAG_INT16:
      *((gint16 *) ptr) = arg->v_int16;
      break;
    case GI_TYPE_TAG_UINT16:
      *((guint16 *) ptr) = arg->v_uint16;
      break;
    case GI_TYPE_TAG_INT32:
      *((gint32 *) ptr) = arg->v_int32;
      break;
    case GI_TYPE_TAG_UNICHAR:
    case GI_TYPE_TAG_UINT32:
      *((guint32 *) ptr) = arg->v_uint32;
      break;
    case GI_TYPE_TAG_INT64:
      *((gint64 *) ptr) = arg->v_int64;
      break;
    case GI_TYPE_TAG_UINT64:
      *((guint64 *) ptr) = arg->v_uint64;
      break;
    case GI_TYPE_TAG_FLOAT:
      *((gfloat *) ptr) = arg->v_float;
      break;
    case GI_TYPE_TAG_DOUBLE:
      *((gdouble *) ptr) = arg->v_double;
      break;
    case GI_TYPE_TAG_GTYPE:
      /* apparently, GType is meant to be a gsize, from gobject/gtype.h in glib */
      *((gsize *) ptr) = arg->v_size;
      break;
    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
      *((gchar **) ptr) = arg->v_string;
      break;
    case GI_TYPE_TAG_ARRAY:
    case GI_TYPE_TAG_INTERFACE:
    case GI_TYPE_TAG_GLIST:
    case GI_TYPE_TAG_GSLIST:
    case GI_TYPE_TAG_GHASH:
    case GI_TYPE_TAG_ERROR:
      *((gpointer **) ptr) = arg->v_pointer;
      break;
    default:
      g_return_if_reached ();
    }
}

void
peas_gi_pointer_to_argument (GITypeInfo     *type_info,
                             gpointer        ptr,
                             GIArgument     *arg)
{
  g_return_if_fail (ptr != NULL);

  switch (g_type_info_get_tag (type_info))
    {
    case GI_TYPE_TAG_VOID:
      arg->v_pointer = *((gpointer **) ptr);
      break;
    case GI_TYPE_TAG_BOOLEAN:
      arg->v_boolean = *((gboolean *) ptr);
      break;
    case GI_TYPE_TAG_INT8:
      arg->v_int8 = *((gint8 *) ptr);
      break;
    case GI_TYPE_TAG_UINT8:
      arg->v_uint8 = *((guint8 *) ptr);
      break;
    case GI_TYPE_TAG_INT16:
      arg->v_int16 = *((gint16 *) ptr);
      break;
    case GI_TYPE_TAG_UINT16:
      arg->v_uint16 = *((guint16 *) ptr);
      break;
    case GI_TYPE_TAG_INT32:
      arg->v_int32 = *((gint32 *) ptr);
      break;
    case GI_TYPE_TAG_UNICHAR:
    case GI_TYPE_TAG_UINT32:
      arg->v_uint32 = *((guint32 *) ptr);
      break;
    case GI_TYPE_TAG_INT64:
      arg->v_int64 = *((gint64 *) ptr);
      break;
    case GI_TYPE_TAG_UINT64:
      arg->v_uint64 = *((guint64 *) ptr);
      break;
    case GI_TYPE_TAG_FLOAT:
      arg->v_float = *((gfloat *) ptr);
      break;
    case GI_TYPE_TAG_DOUBLE:
      arg->v_double = *((gdouble *) ptr);
      break;
    case GI_TYPE_TAG_GTYPE:
      /* apparently, GType is meant to be a gsize, from gobject/gtype.h in glib */
      arg->v_size = *((gsize *) ptr);
      break;
    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
      arg->v_string = *((gchar **) ptr);
      break;
    case GI_TYPE_TAG_ARRAY:
    case GI_TYPE_TAG_INTERFACE:
    case GI_TYPE_TAG_GLIST:
    case GI_TYPE_TAG_GSLIST:
    case GI_TYPE_TAG_GHASH:
    case GI_TYPE_TAG_ERROR:
      arg->v_pointer = *((gpointer **) ptr);
      break;
    default:
      g_return_if_reached ();
    }
}

GICallableInfo *
peas_gi_get_method_info (GType        iface_type,
                         const gchar *method_name)
{
  GIRepository *repo;
  GIBaseInfo *iface_info;
  GIFunctionInfo *func_info;

  repo = g_irepository_get_default ();
  iface_info = g_irepository_find_by_gtype (repo, iface_type);
  if (iface_info == NULL)
    {
      g_warning ("Type not found in introspection: '%s'",
                 g_type_name (iface_type));
      return NULL;
    }

  switch (g_base_info_get_type (iface_info))
    {
    case GI_INFO_TYPE_OBJECT:
      func_info = g_object_info_find_method ((GIObjectInfo *) iface_info,
                                             method_name);
      break;
    case GI_INFO_TYPE_INTERFACE:
      func_info = g_interface_info_find_method ((GIInterfaceInfo *) iface_info,
                                                method_name);
      break;
    default:
      func_info = NULL;
    }

  if (func_info == NULL)
    {
      g_warning ("Method '%s.%s' not found",
                 g_type_name (iface_type),
                 method_name);
    }

  g_base_info_unref (iface_info);
  return (GICallableInfo *) func_info;
}

gboolean
peas_method_apply (GObject     *instance,
                   GType        iface_type,
                   const gchar *method_name,
                   GIArgument  *args,
                   GIArgument  *return_value)
{
  GICallableInfo *func_info;
  gint n_args;
  guint n_in_args, n_out_args;
  GIArgument *in_args, *out_args;
  gboolean ret = TRUE;
  GError *error = NULL;

  func_info = peas_gi_get_method_info (iface_type, method_name);
  if (func_info == NULL)
    return FALSE;

  n_args = g_callable_info_get_n_args (func_info);
  g_return_val_if_fail (n_args >= 0, FALSE);
  n_in_args = 0;
  n_out_args = 0;

  in_args = g_newa (GIArgument, n_args + 1);
  out_args = g_newa (GIArgument, n_args);

  peas_gi_split_in_and_out_arguments (func_info, args,
                                      in_args+1, &n_in_args,
                                      out_args, &n_out_args);

  /* Set the object as the first argument for the method. */
  in_args[0].v_pointer = instance;
  n_in_args++;

  g_debug ("Calling '%s.%s' on '%p'",
           g_type_name (iface_type), method_name, instance);

  ret = g_function_info_invoke (func_info, in_args, n_in_args, out_args,
                                n_out_args, return_value, &error);
  if (!ret)
    {
      g_warning ("Error while calling '%s.%s': %s",
                 g_type_name (iface_type), method_name, error->message);
      g_error_free (error);
      goto out;
    }

out:
  g_base_info_unref ((GIBaseInfo *) func_info);

  return ret;
}
