/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-binding-tool-glib.c: Output C glue
 *
 * Copyright (C) 2003, 2004, 2005 Red Hat, Inc.
 * Copyright (C) 2005 Nokia
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
#include "dbus-gidl.h"
#include "dbus-gparser.h"
#include "dbus-gutils.h"
#include "dbus-gtype-specialized.h"
#include "dbus-gsignature.h"
#include "dbus-gvalue-utils.h"
#include "dbus-glib-tool.h"
#include "dbus-binding-tool-glib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Remember to grep for ->format_version in the code if you change this,
 * most changes should be in dbus-gobject.c. */
#define FORMAT_VERSION 1

#define MARSHAL_PREFIX "dbus_glib_marshal_"

typedef struct
{
  gboolean ignore_unsupported;
  const char* prefix;
  GIOChannel *channel;
  
  GError **error;
  
  GHashTable *generated;
  GString *blob;
  GString *signal_blob;
  GString *property_blob;
  guint count;
} DBusBindingToolCData;

static gboolean gather_marshallers (BaseInfo *base, DBusBindingToolCData *data, GError **error);
static gboolean generate_glue_toplevel (BaseInfo *base, DBusBindingToolCData *data, GError **error);
static gboolean generate_glue (BaseInfo *base, DBusBindingToolCData *data, GError **error);
static gboolean generate_client_glue (BaseInfo *base, DBusBindingToolCData *data, GError **error);

static const char *
dbus_g_type_get_marshal_name (GType gtype)
{
  switch (G_TYPE_FUNDAMENTAL (gtype))
    {
    case G_TYPE_NONE:
      return "NONE";
    case G_TYPE_BOOLEAN:
      return "BOOLEAN";
    case G_TYPE_UCHAR:
      return "UCHAR";
    case G_TYPE_INT:
      return "INT";
    case G_TYPE_UINT:
      return "UINT";
    case G_TYPE_INT64:
      return "INT64";
    case G_TYPE_UINT64:
      return "UINT64";
    case G_TYPE_DOUBLE:
      return "DOUBLE";
    case G_TYPE_STRING:
      return "STRING";
    case G_TYPE_POINTER:
      return "POINTER";
    case G_TYPE_BOXED:
      return "BOXED";
    case G_TYPE_OBJECT:
      return "OBJECT";
    default:
      return NULL;
    }
}

/* This entire function is kind of...ugh. */
static const char *
dbus_g_type_get_c_name (GType gtype)
{
  GType subtype;
  if (dbus_g_type_is_struct (gtype))
    {
      return "GValueArray";
    }
  if (dbus_g_type_is_collection (gtype))
    {
      subtype = dbus_g_type_get_collection_specialization(gtype);
      if (_dbus_g_type_is_fixed (subtype))
        return "GArray";
      else
        return "GPtrArray";
    }

  if (dbus_g_type_is_map (gtype))
    return "GHashTable";

  if (g_type_is_a (gtype, G_TYPE_STRING))
    return "char *";

  /* This one is even more hacky...we get an extra *
   * because G_TYPE_STRV is a G_TYPE_BOXED
   */
  if (g_type_is_a (gtype, G_TYPE_STRV))
    return "char *";

  if (g_type_is_a (gtype, DBUS_TYPE_G_OBJECT_PATH))
    return "char";

  if (g_type_is_a (gtype, DBUS_TYPE_G_SIGNATURE))
    return "char";

  return g_type_name (gtype);
}

static gboolean
compute_gsignature (MethodInfo *method, GType *rettype, GArray **params, GError **error)
{
  GSList *elt;
  GType retval_type;
  GArray *ret;
  gboolean is_async;
  const char *arg_type;
  gboolean retval_signals_error;
  
  is_async = method_info_get_annotation (method, DBUS_GLIB_ANNOTATION_ASYNC) != NULL;
  retval_signals_error = FALSE;

  ret = g_array_new (TRUE, TRUE, sizeof (GType));

  if (is_async)
    retval_type = G_TYPE_NONE;
  else
    {
      gboolean found_retval;

      /* Look for return value */
      found_retval = FALSE;
      for (elt = method_info_get_args (method); elt; elt = elt->next)
	{
	  ArgInfo *arg = elt->data;
	  const char *returnval_annotation;
      
	  returnval_annotation = arg_info_get_annotation (arg, DBUS_GLIB_ANNOTATION_RETURNVAL);
	  if (returnval_annotation != NULL)
	    {
	      arg_type = arg_info_get_type (arg);
	      retval_type = _dbus_gtype_from_signature (arg_type, FALSE);
	      if (retval_type == G_TYPE_INVALID)
		goto invalid_type;
	      found_retval = TRUE;
	      if (!strcmp (returnval_annotation, "error"))
		retval_signals_error = TRUE;
	      break;
	    }
	}
      if (!found_retval)
	{
	  retval_type = G_TYPE_BOOLEAN;
	  retval_signals_error = TRUE;
	}
    }

  *rettype = retval_type;

  /* Handle all input arguments */
  for (elt = method_info_get_args (method); elt; elt = elt->next)
    {
      ArgInfo *arg = elt->data;
      if (arg_info_get_direction (arg) == ARG_IN)
	{
	  GType gtype;
	  
	  arg_type = arg_info_get_type (arg);
	  gtype = _dbus_gtype_from_signature (arg_type, FALSE);
	  if (gtype == G_TYPE_INVALID)
	    goto invalid_type;
	  
	  g_array_append_val (ret, gtype);
	}
    }

  if (!is_async)
    {
      /* Append pointer for each out arg storage */
      for (elt = method_info_get_args (method); elt; elt = elt->next)
	{
	  ArgInfo *arg = elt->data;

	  /* Skip return value */
	  if (arg_info_get_annotation (arg, DBUS_GLIB_ANNOTATION_RETURNVAL) != NULL)
	    continue;
      
	  if (arg_info_get_direction (arg) == ARG_OUT)
	    {
	      GType gtype;
	      arg_type = arg_info_get_type (arg);
	      gtype = _dbus_gtype_from_signature (arg_type, FALSE);
	      if (gtype == G_TYPE_INVALID)
		goto invalid_type;
	      /* We actually just need a pointer for the return value
		 storage */
	      gtype = G_TYPE_POINTER;
	      g_array_append_val (ret, gtype);
	    }
	}

      if (retval_signals_error)
	{
	  /* Final GError parameter */
	  GType gtype = G_TYPE_POINTER;
	  g_array_append_val (ret, gtype);
	}
    }
  else
    {
      /* Context pointer */
      GType gtype = G_TYPE_POINTER;
      g_array_append_val (ret, gtype);
    }

  *params = ret;
  return TRUE;

 invalid_type:
  g_set_error (error,
	       DBUS_BINDING_TOOL_ERROR,
	       DBUS_BINDING_TOOL_ERROR_UNSUPPORTED_CONVERSION,
	       "Unsupported conversion from D-BUS type %s to glib-genmarshal type",
	       arg_type);
  return FALSE;
}
  

static char *
compute_marshaller (MethodInfo *method, GError **error)
{
  GArray *signature;
  GType rettype;
  const char *marshal_name;
  GString *ret;
  guint i;

  if (!compute_gsignature (method, &rettype, &signature, error))
    return NULL;

  ret = g_string_new ("");
  marshal_name = dbus_g_type_get_marshal_name (rettype);
  g_assert (marshal_name != NULL);
  g_string_append (ret, marshal_name);
  g_string_append_c (ret, ':');
  for (i = 0; i < signature->len; i++)
    {
      marshal_name = dbus_g_type_get_marshal_name (g_array_index (signature, GType, i));
      g_assert (marshal_name != NULL);
      g_string_append (ret, marshal_name);
      if (i < signature->len - 1)
	g_string_append_c (ret, ',');
    }
  if (signature->len == 0)
    {
      marshal_name = dbus_g_type_get_marshal_name (G_TYPE_NONE);
      g_assert (marshal_name != NULL);
      g_string_append (ret, marshal_name);
    }
  g_array_free (signature, TRUE);
  return g_string_free (ret, FALSE);
}

static char *
compute_marshaller_name (MethodInfo *method, const char *prefix, GError **error)
{
  GString *ret;
  GArray *signature;
  GType rettype;
  const char *marshal_name;
  guint i;

  if (!compute_gsignature (method, &rettype, &signature, error))
    return NULL;

  ret = g_string_new (MARSHAL_PREFIX);
  g_string_append (ret, prefix);
  g_string_append_c (ret, '_');

  marshal_name = dbus_g_type_get_marshal_name (rettype);
  g_assert (marshal_name != NULL);
  g_string_append (ret, marshal_name);
  g_string_append (ret, "__");
  for (i = 0; i < signature->len; i++)
    {
      marshal_name = dbus_g_type_get_marshal_name (g_array_index (signature, GType, i));
      g_assert (marshal_name != NULL);
      g_string_append (ret, marshal_name);
      if (i < signature->len - 1)
	g_string_append_c (ret, '_');
    }
  if (signature->len == 0)
    {
      marshal_name = dbus_g_type_get_marshal_name (G_TYPE_NONE);
      g_assert (marshal_name != NULL);
      g_string_append (ret, marshal_name);
    }
  g_array_free (signature, TRUE);
  return g_string_free (ret, FALSE);
}

static gboolean
gather_marshallers_list (GSList *list, DBusBindingToolCData *data, GError **error)
{
  GSList *tmp;

  tmp = list;
  while (tmp != NULL)
    {
      if (!gather_marshallers (tmp->data, data, error))
	return FALSE;
      tmp = tmp->next;
    }
  return TRUE;
}

static gboolean
gather_marshallers (BaseInfo *base, DBusBindingToolCData *data, GError **error)
{
  if (base_info_get_type (base) == INFO_TYPE_NODE)
    {
      if (!gather_marshallers_list (node_info_get_nodes ((NodeInfo *) base),
				    data, error))
	return FALSE;
      if (!gather_marshallers_list (node_info_get_interfaces ((NodeInfo *) base),
				    data, error))
	return FALSE;
    }
  else
    {
      InterfaceInfo *interface;
      GSList *methods;
      GSList *tmp;
      const char *interface_c_name;

      interface = (InterfaceInfo *) base;
      interface_c_name = interface_info_get_annotation (interface, DBUS_GLIB_ANNOTATION_C_SYMBOL);
      if (interface_c_name == NULL)
        {
	  if (!data->prefix)
	    return TRUE;
        }

      methods = interface_info_get_methods (interface);

      /* Generate the necessary marshallers for the methods. */

      for (tmp = methods; tmp != NULL; tmp = g_slist_next (tmp))
        {
          MethodInfo *method;
          char *marshaller_name;

          method = (MethodInfo *) tmp->data;

          marshaller_name = compute_marshaller (method, error);
	  if (!marshaller_name)
	    return FALSE;

	  if (g_hash_table_lookup (data->generated, marshaller_name))
	    {
	      g_free (marshaller_name);
	      continue;
	    }

	  g_hash_table_insert (data->generated, marshaller_name, NULL);
        }

    }
  return TRUE;
}

static gboolean
generate_glue_list (GSList *list, DBusBindingToolCData *data, GError **error)
{
  GSList *tmp;

  tmp = list;
  while (tmp != NULL)
    {
      if (!generate_glue (tmp->data, data, error))
	return FALSE;
      tmp = tmp->next;
    }
  return TRUE;
}

#define WRITE_OR_LOSE(x) do { gsize bytes_written; if (!g_io_channel_write_chars (channel, x, -1, &bytes_written, error)) goto io_lose; } while (0)

static gboolean
write_printf_to_iochannel (const char *fmt, GIOChannel *channel, GError **error, ...)
{
  char *str;
  va_list args;
  GIOStatus status;
  gsize written;
  gboolean ret;

  va_start (args, error);

  str = g_strdup_vprintf (fmt, args);
  if ((status = g_io_channel_write_chars (channel, str, -1, &written, error)) == G_IO_STATUS_NORMAL)
    ret = TRUE;
  else
    ret = FALSE;

  g_free (str);

  va_end (args);

  return ret;
}

static gboolean
write_quoted_string (GIOChannel *channel, GString *string, GError **error)
{
  guint i;

  WRITE_OR_LOSE ("\"");
  for (i = 0; i < string->len; i++)
    {
      if (string->str[i] != '\0')
	{
	  if (!g_io_channel_write_chars (channel, string->str + i, 1, NULL, error))
	    return FALSE;
	}
      else
	{
	  if (!g_io_channel_write_chars (channel, "\\0", -1, NULL, error))
	    return FALSE;
	}
    }
  WRITE_OR_LOSE ("\\0\"");
  return TRUE;
 io_lose:
  return FALSE;
}

static gboolean
generate_glue_toplevel (BaseInfo *base, DBusBindingToolCData *data, GError **error)
{
  GString *object_introspection_data_blob;
  GIOChannel *channel;

  channel = data->channel;

  object_introspection_data_blob = g_string_new_len ("", 0);
  data->blob = object_introspection_data_blob;
  data->count = 0;

  data->signal_blob = g_string_new_len ("", 0);
  data->property_blob = g_string_new_len ("", 0);

  if (!write_printf_to_iochannel ("static const DBusGMethodInfo dbus_glib_%s_methods[] = {\n", channel, error, data->prefix))
    goto io_lose;
  
  if (!generate_glue_list (node_info_get_nodes ((NodeInfo *) base),
                           data, error))
    return FALSE;
  if (!generate_glue_list (node_info_get_interfaces ((NodeInfo *) base),
                             data, error))
    return FALSE;

  WRITE_OR_LOSE ("};\n\n");
  /* Information about the object. */

  if (!write_printf_to_iochannel ("const DBusGObjectInfo dbus_glib_%s_object_info = {  %d,\n",
                                  channel, error, data->prefix, FORMAT_VERSION))
    goto io_lose;
  if (!write_printf_to_iochannel ("  dbus_glib_%s_methods,\n", channel, error, data->prefix))
    goto io_lose;
  if (!write_printf_to_iochannel ("  %d,\n", channel, error, data->count))
    goto io_lose;
  if (!write_quoted_string (channel, object_introspection_data_blob, error))
    goto io_lose;
  WRITE_OR_LOSE (",\n");
  if (!write_quoted_string (channel, data->signal_blob, error))
    goto io_lose;
  WRITE_OR_LOSE (",\n");
  if (!write_quoted_string (channel, data->property_blob, error))
    goto io_lose;
  WRITE_OR_LOSE ("\n};\n\n");
  g_string_free (object_introspection_data_blob, TRUE);
  g_string_free (data->signal_blob, TRUE);
  g_string_free (data->property_blob, TRUE);
  data->signal_blob = NULL;
  data->property_blob = NULL;
  return TRUE;
io_lose:
 return FALSE;  
}

static gboolean
generate_glue (BaseInfo *base, DBusBindingToolCData *data, GError **error)
{
  if (base_info_get_type (base) == INFO_TYPE_NODE)
    {
      if (!generate_glue_list (node_info_get_nodes ((NodeInfo *) base),
	                       data, error))
        return FALSE;
      if (!generate_glue_list (node_info_get_interfaces ((NodeInfo *) base),
	                       data, error))
	return FALSE;
    }
  else
    {
      GIOChannel *channel;
      InterfaceInfo *interface;
      GSList *methods;
      GSList *signals;
      GSList *properties;
      GSList *tmp;
      const char *interface_c_name;
      GString *object_introspection_data_blob;

      channel = data->channel;
      object_introspection_data_blob = data->blob;

      interface = (InterfaceInfo *) base;
      interface_c_name = interface_info_get_annotation (interface, DBUS_GLIB_ANNOTATION_C_SYMBOL);
      if (interface_c_name == NULL)
        {
	  if (data->prefix == NULL)
	    return TRUE;
	  interface_c_name = data->prefix;
        }

      methods = interface_info_get_methods (interface);

      /* Table of marshalled methods. */

      for (tmp = methods; tmp != NULL; tmp = g_slist_next (tmp))
        {
          MethodInfo *method;
          char *marshaller_name;
	  char *method_c_name;
          gboolean async = FALSE;
	  GSList *args;
	  gboolean found_retval = FALSE;
          guint found_out_args = 0;

          method = (MethodInfo *) tmp->data;
	  method_c_name = g_strdup (method_info_get_annotation (method, DBUS_GLIB_ANNOTATION_C_SYMBOL));
          if (method_c_name == NULL)
	    {
	      char *method_name_uscored;
	      method_name_uscored = _dbus_gutils_wincaps_to_uscore (method_info_get_name (method));
              method_c_name = g_strdup_printf ("%s_%s",
					       interface_c_name,
					       method_name_uscored);
	      g_free (method_name_uscored);
            }

          if (!write_printf_to_iochannel ("  { (GCallback) %s, ", channel, error,
					  method_c_name))
            {
	      g_free (method_c_name);
              goto io_lose;
            }
          g_free (method_c_name);

          marshaller_name = compute_marshaller_name (method, data->prefix, error);
	  if (!marshaller_name)
	    goto io_lose;

          if (!write_printf_to_iochannel ("%s, %d },\n", channel, error,
					  marshaller_name,
					  object_introspection_data_blob->len))
	    {
	      g_free (marshaller_name);
	      goto io_lose;
	    }

          if (method_info_get_annotation (method, DBUS_GLIB_ANNOTATION_ASYNC) != NULL)
            async = TRUE;

	  /* Object method data blob format:
	   * <iface>\0<name>\0(<argname>\0<argdirection>\0<argtype>\0)*\0
	   */

	  g_string_append (object_introspection_data_blob, interface_info_get_name (interface));
	  g_string_append_c (object_introspection_data_blob, '\0');

	  g_string_append (object_introspection_data_blob, method_info_get_name (method));
	  g_string_append_c (object_introspection_data_blob, '\0');

	  g_string_append_c (object_introspection_data_blob, async ? 'A' : 'S');
	  g_string_append_c (object_introspection_data_blob, '\0');

	  for (args = method_info_get_args (method); args; args = args->next)
	    {
	      ArgInfo *arg;
	      char direction;
	      const char *returnval_annotation;

	      arg = args->data;

	      g_string_append (object_introspection_data_blob, arg_info_get_name (arg));
	      g_string_append_c (object_introspection_data_blob, '\0');

	      switch (arg_info_get_direction (arg))
		{
		case ARG_IN:
		  direction = 'I';
		  break;
		case ARG_OUT:
		  direction = 'O';
                  found_out_args++;
		  break;
		case ARG_INVALID:
                default:
                  g_assert_not_reached ();
                  direction = 0; /* silence gcc */
		  break;
		}
	      g_string_append_c (object_introspection_data_blob, direction);
	      g_string_append_c (object_introspection_data_blob, '\0');

	      if (arg_info_get_annotation (arg, DBUS_GLIB_ANNOTATION_CONST) != NULL)
		{
		  if (arg_info_get_direction (arg) == ARG_IN)
		    {
		      g_set_error (error,
				   DBUS_BINDING_TOOL_ERROR,
				   DBUS_BINDING_TOOL_ERROR_INVALID_ANNOTATION,
				   "Input argument \"%s\" cannot have const annotation in method \"%s\" of interface \"%s\"\n",
				   arg_info_get_name (arg),
				   method_info_get_name (method),
				   interface_info_get_name (interface));
		      return FALSE;
		    }
		  g_string_append_c (object_introspection_data_blob, 'C');
		  g_string_append_c (object_introspection_data_blob, '\0');
		}
	      else if (arg_info_get_direction (arg) == ARG_OUT)
		{
		  g_string_append_c (object_introspection_data_blob, 'F');
		  g_string_append_c (object_introspection_data_blob, '\0');
		}

	      returnval_annotation = arg_info_get_annotation (arg, DBUS_GLIB_ANNOTATION_RETURNVAL);
	      if (returnval_annotation != NULL)
		{
		  GType gtype;

		  if (found_retval)
		    {
		      g_set_error (error,
				   DBUS_BINDING_TOOL_ERROR,
				   DBUS_BINDING_TOOL_ERROR_INVALID_ANNOTATION,
				   "Multiple arguments with return value annotation in method \"%s\" of interface \"%s\"\n",
				   method_info_get_name (method),
				   interface_info_get_name (interface));
		      return FALSE;
		    }
		  found_retval = TRUE;
		  if (arg_info_get_direction (arg) == ARG_IN)
		    {
		      g_set_error (error,
				   DBUS_BINDING_TOOL_ERROR,
				   DBUS_BINDING_TOOL_ERROR_INVALID_ANNOTATION,
				   "Input argument \"%s\" cannot have return value annotation in method \"%s\" of interface \"%s\"\n",
				   arg_info_get_name (arg),
				   method_info_get_name (method),
				   interface_info_get_name (interface));
		      return FALSE;
		    }

                  if (found_out_args != 1)
                    {
                      g_set_error (error,
                          DBUS_BINDING_TOOL_ERROR,
                          DBUS_BINDING_TOOL_ERROR_INVALID_ANNOTATION,
                          "An output <arg> after the first cannot have the ReturnVal annotation, in argument \"%s\" of method \"%s\" of interface \"%s\"\n",
                          arg_info_get_name (arg),
                          method_info_get_name (method),
                          interface_info_get_name (interface));
                      return FALSE;
                    }

		  if (!strcmp ("", returnval_annotation))
		    g_string_append_c (object_introspection_data_blob, 'R');
		  else if (!strcmp ("error", returnval_annotation))
		    {
		      gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
		      if (!_dbus_gtype_can_signal_error (gtype))
			{
			  g_set_error (error,
				       DBUS_BINDING_TOOL_ERROR,
				       DBUS_BINDING_TOOL_ERROR_INVALID_ANNOTATION,
				       "Output argument \"%s\" cannot signal error with type \"%s\" in method \"%s\" of interface \"%s\"\n",
				       arg_info_get_name (arg),
				       g_type_name (gtype),
				       method_info_get_name (method),
				       interface_info_get_name (interface));
			  return FALSE;
			}
		      g_string_append_c (object_introspection_data_blob, 'E');
		    }
		  else
		    {
		      g_set_error (error,
				   DBUS_BINDING_TOOL_ERROR,
				   DBUS_BINDING_TOOL_ERROR_INVALID_ANNOTATION,
				   "Invalid ReturnVal annotation for argument \"%s\" in method \"%s\" of interface \"%s\"\n",
				   arg_info_get_name (arg),
				   method_info_get_name (method),
				   interface_info_get_name (interface));
		      return FALSE;
		    }
		      
		  g_string_append_c (object_introspection_data_blob, '\0');
		}
	      else if (arg_info_get_direction (arg) == ARG_OUT)
		{
		  g_string_append_c (object_introspection_data_blob, 'N');
		  g_string_append_c (object_introspection_data_blob, '\0');
		}

	      g_string_append (object_introspection_data_blob, arg_info_get_type (arg));
	      g_string_append_c (object_introspection_data_blob, '\0');
	    }

	  g_string_append_c (object_introspection_data_blob, '\0');

          data->count++;
        }

      signals = interface_info_get_signals (interface);

      for (tmp = signals; tmp != NULL; tmp = g_slist_next (tmp))
        {
          SignalInfo *sig;
	  
	  sig = tmp->data;

	  g_string_append (data->signal_blob, interface_info_get_name (interface));
	  g_string_append_c (data->signal_blob, '\0');
	  g_string_append (data->signal_blob, signal_info_get_name (sig));
	  g_string_append_c (data->signal_blob, '\0');
	}

      properties = interface_info_get_properties (interface);

      for (tmp = properties; tmp != NULL; tmp = g_slist_next (tmp))
        {
          PropertyInfo *prop;
          PropertyAccessFlags access_flags;
          const char *access_string;
          char *uscored;

          prop = tmp->data;

          access_flags = property_info_get_access (prop);
          if ((access_flags & PROPERTY_READ) && (access_flags & PROPERTY_WRITE))
            access_string = "readwrite";
          else if (access_flags & PROPERTY_READ)
            access_string = "read";
          else if (access_flags & PROPERTY_WRITE)
            access_string = "write";
          else
            continue;

          /* We append both in the blob so we have to malloc() less when processing
           * properties */
          uscored = _dbus_gutils_wincaps_to_uscore (property_info_get_name (prop));

          g_string_append (data->property_blob, interface_info_get_name (interface));
          g_string_append_c (data->property_blob, '\0');
          g_string_append (data->property_blob, property_info_get_name (prop));
          g_string_append_c (data->property_blob, '\0');
          g_string_append (data->property_blob, uscored);
          g_string_append_c (data->property_blob, '\0');
          g_string_append (data->property_blob, access_string);
          g_string_append_c (data->property_blob, '\0');

          g_free (uscored);
	}
    }
  return TRUE;
 io_lose:
  return FALSE;
}

static void
write_marshaller (gpointer key, gpointer value, gpointer user_data)
{
  DBusBindingToolCData *data;
  const char *marshaller;
  gsize bytes_written;

  data = user_data;
  marshaller = key;

  if (data->error && *data->error)
    return;

  if (g_io_channel_write_chars (data->channel, marshaller, -1, &bytes_written, data->error) == G_IO_STATUS_NORMAL)
    g_io_channel_write_chars (data->channel, "\n", -1, &bytes_written, data->error);
}

gboolean
dbus_binding_tool_output_glib_server (BaseInfo *info, GIOChannel *channel, const char *prefix, GError **error)
{
  gboolean ret;
  GPtrArray *argv;
  gint child_stdout;
  GIOChannel *genmarshal_stdout;
  GPid child_pid;
  DBusBindingToolCData data;
  char *tempfile_name;
  gint tempfile_fd;
  GIOStatus iostatus;
  char buf[4096];
  gsize bytes_read, bytes_written;

  memset (&data, 0, sizeof (data));

  dbus_g_type_specialized_init ();
  _dbus_g_type_specialized_builtins_init ();

  data.prefix = prefix;
  data.generated = g_hash_table_new_full (g_str_hash, g_str_equal, (GDestroyNotify) g_free, NULL);
  data.error = error;
  genmarshal_stdout = NULL;
  tempfile_name = NULL;

  if (!gather_marshallers (info, &data, error))
    goto io_lose;

  tempfile_fd = g_file_open_tmp ("dbus-binding-tool-c-marshallers.XXXXXX",
				 &tempfile_name, error);
  if (tempfile_fd < 0)
    goto io_lose;

  data.channel = g_io_channel_unix_new (tempfile_fd);
  if (!g_io_channel_set_encoding (data.channel, NULL, error))
    goto io_lose;
  g_hash_table_foreach (data.generated, write_marshaller, &data); 
  if (error && *error != NULL)
    {
      ret = FALSE;
      g_io_channel_shutdown (data.channel, TRUE, error);
      g_io_channel_unref (data.channel);
      goto io_lose;
    }

  g_io_channel_shutdown (data.channel, TRUE, error);
  g_io_channel_unref (data.channel);
  
  /* Now spawn glib-genmarshal to insert all our required marshallers */
  argv = g_ptr_array_new ();
  g_ptr_array_add (argv, "glib-genmarshal");
  g_ptr_array_add (argv, "--header");
  g_ptr_array_add (argv, "--body");
  g_ptr_array_add (argv, "--skip-source");
  g_ptr_array_add (argv, g_strdup_printf ("--prefix=%s%s", MARSHAL_PREFIX, prefix));
  g_ptr_array_add (argv, tempfile_name);
  g_ptr_array_add (argv, NULL);
  if (!g_spawn_async_with_pipes (NULL, (char**)argv->pdata, NULL,
				 G_SPAWN_SEARCH_PATH,
				 NULL, NULL,
				 &child_pid,
				 NULL,
				 &child_stdout, NULL, error))
    {
      g_ptr_array_free (argv, TRUE);
      goto io_lose;
    }
  g_ptr_array_free (argv, TRUE);

  genmarshal_stdout = g_io_channel_unix_new (child_stdout);
  if (!g_io_channel_set_encoding (genmarshal_stdout, NULL, error))
    goto io_lose;

  WRITE_OR_LOSE ("/* Generated by dbus-binding-tool; do not edit! */\n\n");

  while ((iostatus = g_io_channel_read_chars (genmarshal_stdout, buf, sizeof (buf),
					      &bytes_read, error)) == G_IO_STATUS_NORMAL)
    if (g_io_channel_write_chars (channel, buf, bytes_read, &bytes_written, error) != G_IO_STATUS_NORMAL)
      goto io_lose;
  if (iostatus != G_IO_STATUS_EOF)
    goto io_lose;

  g_io_channel_shutdown (genmarshal_stdout, TRUE, error);

  WRITE_OR_LOSE ("#include <dbus/dbus-glib.h>\n");

  data.channel = channel;
  g_io_channel_ref (data.channel);
  if (!generate_glue_toplevel (info, &data, error))
    goto io_lose;
  
  ret = TRUE;
 cleanup:
  if (tempfile_name)
    unlink (tempfile_name);
  g_free (tempfile_name);
  if (genmarshal_stdout)
    g_io_channel_unref (genmarshal_stdout);
  if (data.channel)
    g_io_channel_unref (data.channel);
  g_hash_table_destroy (data.generated);

  return ret;
 io_lose:
  ret = FALSE;
  goto cleanup;
}

static char *
iface_to_c_prefix (const char *iface)
{
  char **components;
  char **component;
  GString *ret;
  gboolean first;
  
  components = g_strsplit (iface, ".", 0);

  first = TRUE;
  ret = g_string_new ("");
  for (component = components; *component; component++)
    {
      if (!first)
	g_string_append_c (ret, '_');
      else
	first = FALSE;
      g_string_append (ret, *component);
    }
  g_strfreev (components);
  return g_string_free (ret, FALSE);
}

static char *
compute_client_method_name (const char *iface_prefix, MethodInfo *method)
{
  char *method_name_uscored, *ret;

  method_name_uscored = _dbus_gutils_wincaps_to_uscore (method_info_get_name (method));
  ret = g_strdup_printf ("%s_%s", iface_prefix, method_name_uscored);
  g_free (method_name_uscored);

  return ret;
}

static gboolean
write_formal_parameters (InterfaceInfo *iface, MethodInfo *method, GIOChannel *channel, GError **error)
{
  GSList *args;

  for (args = method_info_get_args (method); args; args = args->next)
    {
      ArgInfo *arg;
      const char *type_str;
      const char *type_suffix;
      GType gtype;
      int direction;

      arg = args->data;

      WRITE_OR_LOSE (", ");

      direction = arg_info_get_direction (arg);

      gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
      if (gtype == G_TYPE_INVALID)
	{
	  g_set_error (error,
		       DBUS_BINDING_TOOL_ERROR,
		       DBUS_BINDING_TOOL_ERROR_UNSUPPORTED_CONVERSION,
		       "Unsupported conversion from D-BUS type signature \"%s\" to glib C type in method \"%s\" of interface \"%s\"",
		       arg_info_get_type (arg),
		       method_info_get_name (method),
		       interface_info_get_name (iface));
	  return FALSE;
	}
      type_str = dbus_g_type_get_c_name (gtype);
      g_assert (type_str);
      /* Variants are special...*/
      if (gtype == G_TYPE_VALUE)
	{
	  if (direction == ARG_IN)
	    type_suffix = "*";
	  else
	    type_suffix = "";
	}
      else if ((g_type_is_a (gtype, G_TYPE_BOXED)
	      || g_type_is_a (gtype, G_TYPE_OBJECT)
	   || g_type_is_a (gtype, G_TYPE_POINTER)))
	type_suffix = "*";
      else
	type_suffix = "";


      switch (direction)
	{
	case ARG_IN:
	  if (!write_printf_to_iochannel ("const %s%s IN_%s", channel, error,
					  type_str,
					  type_suffix,
					  arg_info_get_name (arg)))
	    goto io_lose;
	  break;
	case ARG_OUT:
	  if (!write_printf_to_iochannel ("%s%s* OUT_%s", channel, error,
					  type_str,
					  type_suffix,
					  arg_info_get_name (arg)))
	    goto io_lose;
	  break;
	case ARG_INVALID:
	  break;
	}
    }

  return TRUE;
 io_lose:
  return FALSE;
}

#define MAP_FUNDAMENTAL(NAME) \
   case G_TYPE_ ## NAME: \
     return g_strdup ("G_TYPE_" #NAME);
#define MAP_KNOWN(NAME) \
    if (gtype == NAME) \
      return g_strdup (#NAME)
static char *
dbus_g_type_get_lookup_function (GType gtype)
{
  char *type_lookup;
  switch (gtype)
    {
      MAP_FUNDAMENTAL(CHAR);
      MAP_FUNDAMENTAL(UCHAR);
      MAP_FUNDAMENTAL(BOOLEAN);
      MAP_FUNDAMENTAL(LONG);
      MAP_FUNDAMENTAL(ULONG);
      MAP_FUNDAMENTAL(INT);
      MAP_FUNDAMENTAL(UINT);
      MAP_FUNDAMENTAL(INT64);
      MAP_FUNDAMENTAL(UINT64);
      MAP_FUNDAMENTAL(FLOAT);
      MAP_FUNDAMENTAL(DOUBLE);
      MAP_FUNDAMENTAL(STRING);
    }
  if (dbus_g_type_is_collection (gtype))
    {
      GType elt_gtype;
      char *sublookup;

      elt_gtype = dbus_g_type_get_collection_specialization (gtype);
      sublookup = dbus_g_type_get_lookup_function (elt_gtype);
      g_assert (sublookup);

      if (_dbus_g_type_is_fixed (elt_gtype))
        {
          type_lookup = g_strdup_printf ("dbus_g_type_get_collection "
              "(\"GArray\", %s)", sublookup);
        }
      else
        {
          type_lookup = g_strdup_printf ("dbus_g_type_get_collection "
              "(\"GPtrArray\", %s)", sublookup);
        }

      g_free (sublookup);

      return type_lookup;
    }
  else if (dbus_g_type_is_map (gtype))
    {
      GType key_gtype;
      char *key_lookup;
      GType value_gtype;
      char *value_lookup;
      
      key_gtype = dbus_g_type_get_map_key_specialization (gtype);
      value_gtype = dbus_g_type_get_map_value_specialization (gtype);
      key_lookup = dbus_g_type_get_lookup_function (key_gtype);
      g_assert (key_lookup);
      value_lookup = dbus_g_type_get_lookup_function (value_gtype);
      g_assert (value_lookup);
      type_lookup = g_strdup_printf ("dbus_g_type_get_map (\"GHashTable\", %s, %s)",
				     key_lookup, value_lookup);
      g_free (key_lookup);
      g_free (value_lookup);
      return type_lookup;
    }
  else if (dbus_g_type_is_struct (gtype))
    {
      GType value_gtype;
      GString *string;
      char *value_lookup = NULL;
      guint size, i;

      string = g_string_new ("dbus_g_type_get_struct (\"GValueArray\"");

      size = dbus_g_type_get_struct_size (gtype);
      for (i=0; i < size; i++)
        {
          value_gtype = dbus_g_type_get_struct_member_type(gtype, i);
          value_lookup = dbus_g_type_get_lookup_function (value_gtype);
          g_assert (value_lookup);
          g_string_append_printf (string, ", %s", value_lookup);
          g_free (value_lookup);
        }
      g_string_append (string, ", G_TYPE_INVALID)");
      return g_string_free (string, FALSE);
    }

  MAP_KNOWN(G_TYPE_VALUE);
  MAP_KNOWN(G_TYPE_STRV);
  MAP_KNOWN(G_TYPE_VALUE_ARRAY);
  MAP_KNOWN(DBUS_TYPE_G_PROXY);
  MAP_KNOWN(DBUS_TYPE_G_OBJECT_PATH);
  MAP_KNOWN(DBUS_TYPE_G_SIGNATURE);
  return NULL;
}
#undef MAP_FUNDAMENTAL
#undef MAP_KNOWN

static gboolean
write_args_for_direction (InterfaceInfo *iface, MethodInfo *method, GIOChannel *channel, int direction, GError **error)
{
  GSList *args;
  char *type_lookup = NULL;

  for (args = method_info_get_args (method); args; args = args->next)
    {
      ArgInfo *arg;
      GType gtype;

      arg = args->data;

      if (direction != arg_info_get_direction (arg))
	continue;

      gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
      g_assert (gtype != G_TYPE_INVALID);
      type_lookup = dbus_g_type_get_lookup_function (gtype);
      g_assert (type_lookup != NULL);

      switch (direction)
	{

	case ARG_IN:
	  if (!write_printf_to_iochannel ("%s, IN_%s, ", channel, error,
					  type_lookup,
					  arg_info_get_name (arg)))
	    goto io_lose;
	  break;
	case ARG_OUT:
	  if (!write_printf_to_iochannel ("%s, OUT_%s, ", channel, error,
					  type_lookup,
					  arg_info_get_name (arg)))
	    goto io_lose;
	  break;
	case ARG_INVALID:
	  break;
	}
      g_free (type_lookup);
    }

  return TRUE;
 io_lose:
  g_free (type_lookup);
  return FALSE;
}

static gboolean
check_supported_parameters (MethodInfo *method)
{
  GSList *args;

  for (args = method_info_get_args (method); args; args = args->next)
    {
      ArgInfo *arg;
      GType gtype;

      arg = args->data;
      gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
      if (gtype == G_TYPE_INVALID)
	return FALSE;
    }
  return TRUE;
}

static gboolean
write_untyped_out_args (InterfaceInfo *iface, MethodInfo *method, GIOChannel *channel, GError **error)
{
  GSList *args;

  for (args = method_info_get_args (method); args; args = args->next)
    {
      ArgInfo *arg;

      arg = args->data;
      if (arg_info_get_direction (arg) != ARG_OUT)
        continue;
            
      if (!write_printf_to_iochannel ("OUT_%s, ", channel, error,
                                      arg_info_get_name (arg)))
        goto io_lose;
     }

   return TRUE;
 io_lose:
  return FALSE;
}

static gboolean
write_formal_declarations_for_direction (InterfaceInfo *iface, MethodInfo *method, GIOChannel *channel, const int direction, GError **error)
 {
   GSList *args;
 
   for (args = method_info_get_args (method); args; args = args->next)
     {
       ArgInfo *arg;
      GType gtype;
      const char *type_str, *type_suffix, *type_initializer = "";
      int dir;

       arg = args->data;

      dir = arg_info_get_direction (arg);

      gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
      type_str = dbus_g_type_get_c_name (gtype);

      if (!type_str)
       {
         g_set_error (error,
                      DBUS_BINDING_TOOL_ERROR,
                      DBUS_BINDING_TOOL_ERROR_UNSUPPORTED_CONVERSION,
                      "Unsupported conversion from D-BUS type signature \"%s\" to glib C type in method \"%s\" of interface \"%s\"",
                      arg_info_get_type (arg),
                      method_info_get_name (method),
                      interface_info_get_name (iface));
         return FALSE;
       }

      /* Variants are special...*/
      if (gtype == G_TYPE_VALUE)
	{
	  if (direction == ARG_IN)
	    type_suffix = "*";
	  else
	    {
	      type_suffix = "";
	      type_initializer = " = { 0, }";
	    }
	}
      else if ((g_type_is_a (gtype, G_TYPE_BOXED)
	      || g_type_is_a (gtype, G_TYPE_OBJECT)
	   || g_type_is_a (gtype, G_TYPE_POINTER)))
	type_suffix = "*";
      else
	type_suffix = "";

      if (direction != dir)
        continue;

          switch (dir)
       {
       case ARG_IN:
         if (!write_printf_to_iochannel ("  %s%s IN_%s;\n", channel, error,
                                         type_str, type_suffix,
                                         arg_info_get_name (arg)))
           goto io_lose;
         break;
       case ARG_OUT:
         if (!write_printf_to_iochannel ("  %s%s OUT_%s%s;\n", channel, error,
                                         type_str, type_suffix,
                                         arg_info_get_name (arg),
                                         type_initializer))
           goto io_lose;
         break;
       case ARG_INVALID:
         break;
       }
     }
   return TRUE;
 io_lose:
  return FALSE;
 }

static gboolean
write_formal_parameters_for_direction (InterfaceInfo *iface, MethodInfo *method, int dir, GIOChannel *channel, GError **error)
{
  GSList *args;

  for (args = method_info_get_args (method); args; args = args->next)
    {
      ArgInfo *arg;
      const char *type_str;
      const char *type_suffix;
      GType gtype;
      int direction;

      arg = args->data;

      direction = arg_info_get_direction (arg);
      if (dir != direction) continue;
      
      WRITE_OR_LOSE (", ");

      gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
      type_str = dbus_g_type_get_c_name (gtype);
      /* Variants are special...*/
      if (gtype == G_TYPE_VALUE)
	{
	  if (direction == ARG_IN)
	    type_suffix = "*";
	  else
	    type_suffix = "";
	}
      else if ((g_type_is_a (gtype, G_TYPE_BOXED)
	      || g_type_is_a (gtype, G_TYPE_OBJECT)
	   || g_type_is_a (gtype, G_TYPE_POINTER)))
	type_suffix = "*";
      else
	type_suffix = "";

      if (!type_str)
	{
	  g_set_error (error,
		       DBUS_BINDING_TOOL_ERROR,
		       DBUS_BINDING_TOOL_ERROR_UNSUPPORTED_CONVERSION,
		       "Unsupported conversion from D-BUS type signature \"%s\" to glib C type in method \"%s\" of interface \"%s\"",
		       arg_info_get_type (arg),
		       method_info_get_name (method),
		       interface_info_get_name (iface));
	  return FALSE;
	}
 
       switch (direction)
 	{
 	case ARG_IN:
	  if (!write_printf_to_iochannel ("const %s%s IN_%s", channel, error,
					  type_str,
					  type_suffix,
					  arg_info_get_name (arg)))
 	    goto io_lose;
 	  break;
 	case ARG_OUT:
	  if (!write_printf_to_iochannel ("%s%s* OUT_%s", channel, error,
					  type_str,
					  type_suffix,
					  arg_info_get_name (arg)))
 	    goto io_lose;
 	  break;
 	case ARG_INVALID:
	  break;
	}
    }
  return TRUE;
 io_lose:
  return FALSE;
}

static gboolean
write_typed_args_for_direction (InterfaceInfo *iface, MethodInfo *method, GIOChannel *channel, const int direction, GError **error)
{
  GSList *args;
  char *type_lookup = NULL;
  
  for (args = method_info_get_args (method); args; args = args->next)
    {
      ArgInfo *arg;
      int dir;
      GType gtype;
      
      arg = args->data;

      dir = arg_info_get_direction (arg);

      if (dir != direction)
        continue;

      gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
      type_lookup = dbus_g_type_get_lookup_function (gtype);

      if (!write_printf_to_iochannel ("%s, &%s_%s, ", channel, error, type_lookup, direction == ARG_IN ? "IN" : "OUT", arg_info_get_name (arg)))
        goto io_lose;
      g_free (type_lookup);
    }
  return TRUE;
 io_lose:
  g_free (type_lookup);
  return FALSE;
}

static gboolean
write_async_method_client (GIOChannel *channel, InterfaceInfo *interface, MethodInfo *method, GError **error)
{
  char *method_name, *iface_prefix;
  const char *interface_c_name;

  iface_prefix = iface_to_c_prefix (interface_info_get_name (interface));
  interface_c_name = interface_info_get_annotation (interface, DBUS_GLIB_ANNOTATION_CLIENT_C_SYMBOL);
  if (interface_c_name == NULL)
    {
      interface_c_name = (const char *) iface_prefix;
    }

  method_name = g_strdup (method_info_get_annotation (method, DBUS_GLIB_ANNOTATION_CLIENT_C_SYMBOL));
  if (method_name == NULL)
    {
      method_name = compute_client_method_name (interface_c_name, method);
    }
  g_free(iface_prefix);

  /* Write the typedef for the client callback */
  if (!write_printf_to_iochannel ("typedef void (*%s_reply) (DBusGProxy *proxy, ", channel, error, method_name))
    goto io_lose;
  {
    GSList *args;
    for (args = method_info_get_args (method); args; args = args->next)
      {
	ArgInfo *arg;
	const char *type_suffix, *type_str;
	GType gtype;
	
	arg = args->data;
	
	if (arg_info_get_direction (arg) != ARG_OUT)
	  continue;
	gtype = _dbus_gtype_from_signature (arg_info_get_type (arg), TRUE);
	if (gtype != G_TYPE_VALUE && (g_type_is_a (gtype, G_TYPE_BOXED)
	     || g_type_is_a (gtype, G_TYPE_OBJECT)
	     || g_type_is_a (gtype, G_TYPE_POINTER)))
	  type_suffix = "*";
	else
	  type_suffix = "";
	type_str = dbus_g_type_get_c_name (_dbus_gtype_from_signature (arg_info_get_type (arg), TRUE));
	if (!write_printf_to_iochannel ("%s %sOUT_%s, ", channel, error, type_str, type_suffix, arg_info_get_name (arg)))
	  goto io_lose;
      }
  }
  WRITE_OR_LOSE ("GError *error, gpointer userdata);\n\n");
  
  
  /* Write the callback when the call returns */
  WRITE_OR_LOSE ("static void\n");
  if (!write_printf_to_iochannel ("%s_async_callback (DBusGProxy *proxy, DBusGProxyCall *call, void *user_data)\n", channel, error, method_name))
    goto io_lose;
  WRITE_OR_LOSE ("{\n");
  WRITE_OR_LOSE ("  DBusGAsyncData *data = (DBusGAsyncData*) user_data;\n  GError *error = NULL;\n");
  if (!write_formal_declarations_for_direction (interface, method, channel, ARG_OUT, error))
    goto io_lose;
  /* TODO: handle return boolean of end_call */
  WRITE_OR_LOSE ("  dbus_g_proxy_end_call (proxy, call, &error, ");
  if (!write_typed_args_for_direction (interface, method, channel, ARG_OUT, error))
    goto io_lose;
  WRITE_OR_LOSE("G_TYPE_INVALID);\n");
  if (!write_printf_to_iochannel ("  (*(%s_reply)data->cb) (proxy, ", channel, error, method_name))
    goto io_lose;
  if (!write_untyped_out_args (interface, method, channel, error))
    goto io_lose;
  WRITE_OR_LOSE ("error, data->userdata);\n");
  WRITE_OR_LOSE ("  return;\n}\n\n");
  

  /* Write the main wrapper function */
  WRITE_OR_LOSE ("static\n#ifdef G_HAVE_INLINE\ninline\n#endif\nDBusGProxyCall*\n");
  if (!write_printf_to_iochannel ("%s_async (DBusGProxy *proxy", channel, error,
                                  method_name))
    goto io_lose;
  if (!write_formal_parameters_for_direction (interface, method, ARG_IN, channel, error))
    goto io_lose;
  
  if (!write_printf_to_iochannel (", %s_reply callback, gpointer userdata)\n\n", channel, error, method_name))
    goto io_lose;
  
  WRITE_OR_LOSE ("{\n");
  WRITE_OR_LOSE ("  DBusGAsyncData *stuff;\n  stuff = g_slice_new (DBusGAsyncData);\n  stuff->cb = G_CALLBACK (callback);\n  stuff->userdata = userdata;\n");
  if (!write_printf_to_iochannel ("  return dbus_g_proxy_begin_call (proxy, \"%s\", %s_async_callback, stuff, _dbus_glib_async_data_free, ", channel, error, method_info_get_name (method), method_name))
    goto io_lose;
  if (!write_args_for_direction (interface, method, channel, ARG_IN, error))
    goto io_lose;
  WRITE_OR_LOSE ("G_TYPE_INVALID);\n}\n");

  g_free (method_name);
  return TRUE;
 io_lose:
  g_free (method_name);
  return FALSE;
 }

static gboolean
generate_client_glue_list (GSList *list, DBusBindingToolCData *data, GError **error)
{
  GSList *tmp;

  tmp = list;
  while (tmp != NULL)
    {
      if (!generate_client_glue (tmp->data, data, error))
	return FALSE;
      tmp = tmp->next;
    }
  return TRUE;
}

static gboolean
generate_client_glue (BaseInfo *base, DBusBindingToolCData *data, GError **error)
{
  char *iface_prefix;
  char *method_c_name;
  iface_prefix = NULL;
  method_c_name = NULL;

  if (base_info_get_type (base) == INFO_TYPE_NODE)
    {
      if (!generate_client_glue_list (node_info_get_nodes ((NodeInfo *) base),
              data, error))
        return FALSE;
      if (!generate_client_glue_list (node_info_get_interfaces ((NodeInfo *) base),
              data, error))
        return FALSE;
    }
  else
    {
      GIOChannel *channel;
      InterfaceInfo *interface;
      GSList *methods;
      GSList *tmp;
      const char *interface_c_name;

      channel = data->channel;

      interface = (InterfaceInfo *) base;

      methods = interface_info_get_methods (interface);

      iface_prefix = iface_to_c_prefix (interface_info_get_name (interface));
      interface_c_name = interface_info_get_annotation (interface, DBUS_GLIB_ANNOTATION_CLIENT_C_SYMBOL);
      if (interface_c_name == NULL)
      {
          interface_c_name = (const char *) iface_prefix;
      }

      if (!write_printf_to_iochannel ("#ifndef DBUS_GLIB_CLIENT_WRAPPERS_%s\n"
              "#define DBUS_GLIB_CLIENT_WRAPPERS_%s\n\n",
              channel, error,
              iface_prefix, iface_prefix))
        goto io_lose;

      for (tmp = methods; tmp != NULL; tmp = g_slist_next (tmp))
        {
          MethodInfo *method;
          gboolean is_noreply;

          method = (MethodInfo *) tmp->data;
          method_c_name = g_strdup (method_info_get_annotation (method, DBUS_GLIB_ANNOTATION_CLIENT_C_SYMBOL));
          if (method_c_name == NULL)
            {
              method_c_name = compute_client_method_name (interface_c_name, method);
            }

          is_noreply = method_info_get_annotation (method, DBUS_GLIB_ANNOTATION_NOREPLY) != NULL;

          if (data->ignore_unsupported && !check_supported_parameters (method))
            {
              g_warning ("Ignoring unsupported signature in method \"%s\" of interface \"%s\"\n",
              method_info_get_name (method),
              interface_info_get_name (interface));
              g_free (method_c_name);
              method_c_name = NULL;
              continue;
            }


          WRITE_OR_LOSE ("static\n#ifdef G_HAVE_INLINE\ninline\n#endif\ngboolean\n");
          if (!write_printf_to_iochannel ("%s (DBusGProxy *proxy", channel, error,
                  method_c_name))
            goto io_lose;
          g_free (method_c_name);
          method_c_name = NULL;

          if (!write_formal_parameters (interface, method, channel, error))
            goto io_lose;

          WRITE_OR_LOSE (", GError **error)\n\n");

          WRITE_OR_LOSE ("{\n");

          if (is_noreply) {
            if (!write_printf_to_iochannel ("  dbus_g_proxy_call_no_reply (proxy, \"%s\", ", channel, error,
                    method_info_get_name (method)))
              goto io_lose;

            if (!write_args_for_direction (interface, method, channel, ARG_IN, error))
              goto io_lose;

            WRITE_OR_LOSE ("G_TYPE_INVALID, ");

            if (!write_args_for_direction (interface, method, channel, ARG_OUT, error))
              goto io_lose;

            WRITE_OR_LOSE ("G_TYPE_INVALID);\n");

            WRITE_OR_LOSE ("  return TRUE;\n}\n\n");
          } else {
            if (!write_printf_to_iochannel ("  return dbus_g_proxy_call (proxy, \"%s\", ", channel, error,
                    method_info_get_name (method)))
              goto io_lose;

            WRITE_OR_LOSE ("error, ");

            if (!write_args_for_direction (interface, method, channel, ARG_IN, error))
              goto io_lose;

            WRITE_OR_LOSE ("G_TYPE_INVALID, ");

            if (!write_args_for_direction (interface, method, channel, ARG_OUT, error))
              goto io_lose;

            WRITE_OR_LOSE ("G_TYPE_INVALID);\n}\n\n");
          }

          write_async_method_client (channel, interface, method, error);
        }

      if (!write_printf_to_iochannel ("#endif /* defined DBUS_GLIB_CLIENT_WRAPPERS_%s */\n\n", channel, error, iface_prefix))
        goto io_lose;

      g_free (iface_prefix);
    }
  return TRUE;
 io_lose:
  g_free (method_c_name);
  g_free (iface_prefix);
  return FALSE;
}


gboolean
dbus_binding_tool_output_glib_client (BaseInfo *info, GIOChannel *channel, gboolean ignore_unsupported, GError **error)
{
  DBusBindingToolCData data;
  gboolean ret;

  memset (&data, 0, sizeof (data));
  
  data.channel = channel;
  data.ignore_unsupported = ignore_unsupported;

  dbus_g_type_specialized_init ();
  _dbus_g_type_specialized_builtins_init ();

  WRITE_OR_LOSE ("/* Generated by dbus-binding-tool; do not edit! */\n\n");
  WRITE_OR_LOSE ("#include <glib.h>\n");
  WRITE_OR_LOSE ("#include <dbus/dbus-glib.h>\n\n");
  WRITE_OR_LOSE ("G_BEGIN_DECLS\n\n");

  WRITE_OR_LOSE ("#ifndef _DBUS_GLIB_ASYNC_DATA_FREE\n");
  WRITE_OR_LOSE ("#define _DBUS_GLIB_ASYNC_DATA_FREE\n");
  WRITE_OR_LOSE ("static\n#ifdef G_HAVE_INLINE\ninline\n#endif\nvoid\n");
  WRITE_OR_LOSE ("_dbus_glib_async_data_free (gpointer stuff)\n{\n\tg_slice_free (DBusGAsyncData, stuff);\n}\n");
  WRITE_OR_LOSE ("#endif\n\n");

  ret = generate_client_glue (info, &data, error);
  if (!ret)
    goto io_lose;
  
  WRITE_OR_LOSE ("G_END_DECLS\n");

  return ret;
 io_lose:
  return FALSE;
}
