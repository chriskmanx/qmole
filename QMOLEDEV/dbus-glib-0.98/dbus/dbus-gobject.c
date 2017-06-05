/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gobject.c Exporting a GObject remotely
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
#include <gobject/gvaluecollector.h>
#include <dbus/dbus-glib.h>
#include <dbus/dbus-glib-lowlevel.h>
#include "dbus-gtest.h"
#include "dbus-gutils.h"
#include "dbus-gobject.h"
#include "dbus-gsignature.h"
#include "dbus-gvalue.h"
#include "dbus-gmarshal.h"
#include "dbus-gvalue-utils.h"
#include <string.h>

#include <gio/gio.h>

G_GNUC_NORETURN static void
oom (const gchar *explanation)
{
  g_error (explanation == NULL ? "Out of memory" : explanation);
  g_assert_not_reached ();
}

static DBusMessage *
reply_or_die (DBusMessage *in_reply_to)
{
  DBusMessage *reply;

  g_return_val_if_fail (in_reply_to != NULL, NULL);

  reply = dbus_message_new_method_return (in_reply_to);

  if (reply == NULL)
    oom ("dbus_message_new_method_return failed: out of memory?");

  return reply;
}

static DBusMessage *
error_or_die (DBusMessage *in_reply_to,
    const gchar *error_name,
    const gchar *error_message)
{
  DBusMessage *reply;

  g_return_val_if_fail (in_reply_to != NULL, NULL);
  /* error names are syntactically the same as interface names */
  g_return_val_if_fail (g_dbus_is_interface_name (error_name), NULL);
  g_return_val_if_fail (g_utf8_validate (error_message, -1, NULL), NULL);

  reply = dbus_message_new_error (in_reply_to, error_name, error_message);

  if (reply == NULL)
    oom ("dbus_message_new_error failed: out of memory?");

  return reply;
}

static void
connection_send_or_die (DBusConnection *connection,
    DBusMessage *message)
{
  g_return_if_fail (connection != NULL);
  g_return_if_fail (message != NULL);

  if (!dbus_connection_send (connection, message, NULL))
    oom ("dbus_connection_send failed: out of memory?");
}

static char *lookup_property_name (GObject    *object,
                                   const char *wincaps_propiface,
                                   const char *requested_propname);

typedef struct
{
  char *default_iface;
  GType code_enum;
} DBusGErrorInfo;

static GStaticRWLock globals_lock = G_STATIC_RW_LOCK_INIT;
/* See comments in check_property_access */
static gboolean disable_legacy_property_access = FALSE;
static GHashTable *marshal_table = NULL;
static GData *error_metadata = NULL;

static char*
uscore_to_wincaps_full (const char *uscore,
                        gboolean    uppercase_first,
                        gboolean    strip_underscores)
{
  const char *p;
  GString *str;
  gboolean last_was_uscore;

  last_was_uscore = uppercase_first;
  
  str = g_string_new (NULL);
  p = uscore;
  while (p && *p)
    {
      if (*p == '-' || (strip_underscores && *p == '_'))
        {
          last_was_uscore = TRUE;
        }
      else
        {
          if (last_was_uscore)
            {
              g_string_append_c (str, g_ascii_toupper (*p));
              last_was_uscore = FALSE;
            }
          else
            g_string_append_c (str, *p);
        }
      ++p;
    }

  return g_string_free (str, FALSE);
}

/* Ugly yes - but we have to accept strings from both formats */
static gboolean
compare_strings_ignoring_uscore_vs_dash (const char *a, const char *b)
{
  guint i;

  for (i = 0; a[i] && b[i]; i++)
    {
      if ((a[i] == '-' && b[i] == '_')
          || (a[i] == '_' && b[i] == '-'))
        continue;
      if (a[i] != b[i])
        return FALSE;
    }
  return (a[i] == '\0') && (b[i] == '\0');
}

static char *
uscore_to_wincaps (const char *uscore)
{
  return uscore_to_wincaps_full (uscore, TRUE, TRUE);
}

static const char *
string_table_next (const char *table)
{
  return (table + (strlen (table) + 1));
}

static const char *
string_table_lookup (const char *table, int index)
{
  const char *ret;

  ret = table;

  while (index--)
    ret = string_table_next (ret);

  return ret;
}

static const char *
get_method_data (const DBusGObjectInfo *object,
		 const DBusGMethodInfo *method)
{
  return object->data + method->data_offset;
}

static char *
object_error_domain_prefix_from_object_info (const DBusGObjectInfo *info)
{
  /* FIXME */
  return NULL;
}

static char *
object_error_code_from_object_info (const DBusGObjectInfo *info, GQuark domain, gint code)
{
  /* FIXME */
  return NULL;
}

static const char *
method_interface_from_object_info (const DBusGObjectInfo *object,
			      const DBusGMethodInfo *method)
{
  return string_table_lookup (get_method_data (object, method), 0);
}

static const char *
method_name_from_object_info (const DBusGObjectInfo *object,
			      const DBusGMethodInfo *method)
{
  return string_table_lookup (get_method_data (object, method), 1);
}

static const char *
method_arg_info_from_object_info (const DBusGObjectInfo *object,
				  const DBusGMethodInfo *method)
{
  return string_table_lookup (get_method_data (object, method), 3);/*RB was 2*/
}

typedef enum
{
  RETVAL_NONE,    
  RETVAL_NOERROR,    
  RETVAL_ERROR
} RetvalType;

/*
 * arg_iterate:
 * @data: a pointer to the beginning of an argument entry in a string table
 * @name: (out) (allow-none): used to return the name of the next argument
 * @in: (out) (allow-none): used to return %TRUE for an "in" argument or
 *    %FALSE for an "out" argument
 * @constval: (out) (allow-none): used to return %TRUE if the argument is
 *    an "out" argument and has the "C" (const) flag indicating that it
 *    should not be freed after it is returned; normally, "out" arguments
 *    are freed
 * @retval: (out) (allow-none): used to return %RETVAL_NONE if this
 *    D-Bus argument is not an "out" argument or is obtained like a C "out"
 *    parameter, %RETVAL_ERROR if this argument is obtained from the C
 *    return value and is also used to signal errors, or %RETVAL_NOERROR
 *    if this argument is obtained from the C return value and the method
 *    can never raise an error
 * @type: (out) (allow-none): used to return the D-Bus signature of this
 *    argument
 *
 * The data format is:
 *
 * argument name
 * \0
 * direction: I or O
 * \0
 * if direction == "O":
 *     freeable? F or C
 *     \0
 *     retval? N, E or R
 *     \0
 * signature
 * \0
 *
 * If none of the arguments has @retval != %RETVAL_NONE, the method is
 * assumed to return a gboolean, which behaves like %RETVAL_ERROR but is
 * not sent over D-Bus at all.
 *
 * Returns: the value of @data to use for the next call, or a pointer to '\0'
 *    if this function must not be called again
 */
static const char *
arg_iterate (const char    *data,
	     const char   **name,
	     gboolean      *in,
	     gboolean      *constval,
	     RetvalType    *retval,
	     const char   **type)
{
  gboolean inarg;

  if (name)
    *name = data;

  data = string_table_next (data);
  switch (*data)
    {
    case 'I':
      inarg = TRUE;
      break;
    case 'O':
      inarg = FALSE;
      break;
    default:
      g_warning ("invalid arg direction '%c'", *data);
      inarg = FALSE;
      break;
    }
  if (in)
    *in = inarg;

  if (!inarg)
    {
      data = string_table_next (data);
      switch (*data)
	{
	case 'F':
	  if (constval)
	    *constval = FALSE;
	  break;
	case 'C':
	  if (constval)
	    *constval = TRUE;
	  break;
	default:
	  g_warning ("invalid arg const value '%c'", *data);
	  break;
	}
      data = string_table_next (data);
      switch (*data)
	{
	case 'N':
	  if (retval)
	    *retval = RETVAL_NONE;
	  break;
	case 'E':
	  if (retval)
	    *retval = RETVAL_ERROR;
	  break;
	case 'R':
	  if (retval)
	    *retval = RETVAL_NOERROR;
	  break;
	default:
	  g_warning ("invalid arg ret value '%c'", *data);
	  break;
	}
    }
  else
    {
      if (constval)
	*constval = FALSE;
      if (retval)
	*retval = FALSE;
    }
  
  data = string_table_next (data);
  if (type)
    *type = data;

  return string_table_next (data);
}

static char *
method_dir_signature_from_object_info (const DBusGObjectInfo *object,
				       const DBusGMethodInfo *method,
				       gboolean               in)
{
  const char *arg;
  GString *ret;

  arg = method_arg_info_from_object_info (object, method);

  ret = g_string_new (NULL);

  while (*arg)
    {
      const char *name;
      gboolean arg_in;
      const char *type;

      arg = arg_iterate (arg, &name, &arg_in, NULL, NULL, &type);

      if (arg_in == in)
	g_string_append (ret, type);
    }

  return g_string_free (ret, FALSE);
}

static char *
method_input_signature_from_object_info (const DBusGObjectInfo *object,
					 const DBusGMethodInfo *method)
{
  return method_dir_signature_from_object_info (object, method, TRUE);
}

static char *
method_output_signature_from_object_info (const DBusGObjectInfo *object,
					  const DBusGMethodInfo *method)
{
  return method_dir_signature_from_object_info (object, method, FALSE);
}

static const char *
signal_iterate (const char *data, const char **iface, const char **name)
{
  *iface = data;

  data = string_table_next (data);
  *name = data;

  return string_table_next (data);
}

static const char *
property_iterate (const char  *data,
                  int          format_version,
                  const char **iface,
                  const char **exported_name,
                  const char **name_uscored,
                  const char **access_type)
{
  *iface = data;

  data = string_table_next (data);
  *exported_name = data;

  data = string_table_next (data);
  if (format_version == 1)
    {
      *name_uscored = data;
      data = string_table_next (data);
      *access_type = data;
      return string_table_next (data);
    }
  else
    {
      /* This tells the caller they need to compute it */
      *name_uscored = NULL;
      /* We don't know here, however note that we will still check against the
       * readable/writable flags from GObject's metadata.
       */
      *access_type = "readwrite";
      return data;
    }
}

/**
 * property_info_from_object_info:
 * @object: introspection data
 * @interface_name: (allow-none): Expected interface name, or %NULL for any
 * @property_name: Expected property name (can use "-" or "_" as separator)
 * @access_type: (out): Can be one of "read", "write", "readwrite"
 *
 * Look up property introspection data for the given interface/name pair.
 *
 * Returns: %TRUE if property was found
 */
static gboolean
property_info_from_object_info (const DBusGObjectInfo  *object,
                                const char             *interface_name,
                                const char             *property_name,
                                const char            **access_type)
{
  const char *properties_iter;

  properties_iter = object->exported_properties;
  while (properties_iter != NULL && *properties_iter)
    {
      const char *cur_interface_name;
      const char *cur_property_name;
      const char *cur_uscore_property_name;
      const char *cur_access_type;


      properties_iter = property_iterate (properties_iter, object->format_version,
                                          &cur_interface_name, &cur_property_name,
                                          &cur_uscore_property_name, &cur_access_type);

      if (interface_name && strcmp (interface_name, cur_interface_name) != 0)
        continue;

      /* This big pile of ugly is necessary to support the matrix resulting from multiplying
       * (v0 data, v1 data) * (FooBar, foo-bar)
       * In v1 data we have both forms of string, so we do a comparison against both without
       * having to malloc.
       * For v0 data, we need to reconstruct the foo-bar form.
       *
       * Adding to the complexity is that we *also* have to ignore the distinction between
       * '-' and '_', because g_object_{get,set} does.
       */
      /* First, compare against the primary property name - no malloc required */
      if (!compare_strings_ignoring_uscore_vs_dash (property_name, cur_property_name))
        {
          if (cur_uscore_property_name != NULL
              && !compare_strings_ignoring_uscore_vs_dash (property_name, cur_uscore_property_name))
            continue;
          else
            {
              /* v0 metadata, construct uscore */
              char *tmp_uscored;
              gboolean matches;
              tmp_uscored = _dbus_gutils_wincaps_to_uscore (cur_property_name);
              matches = compare_strings_ignoring_uscore_vs_dash (property_name, tmp_uscored);
              g_free (tmp_uscored);
              if (!matches)
                continue;
            }
        }

      *access_type = cur_access_type;
      return TRUE;
    }
  return FALSE;
}

static GQuark
dbus_g_object_type_dbus_metadata_quark (void)
{
  static GQuark quark;

  if (!quark)
    quark = g_quark_from_static_string ("DBusGObjectTypeDBusMetadataQuark");
  return quark;
}

/* Iterator function should return FALSE to stop iteration, TRUE to continue */
typedef gboolean (*ForeachObjectInfoFn) (const DBusGObjectInfo *info,
                                         GType                 gtype,
                                         gpointer              user_data);

static void
foreach_object_info (GObject *object,
		     ForeachObjectInfoFn callback,
		     gpointer user_data)
{
  GType *interfaces, *p;
  const DBusGObjectInfo *info;
  GType classtype;

  interfaces = g_type_interfaces (G_TYPE_FROM_INSTANCE (object), NULL);

  for (p = interfaces; *p != 0; p++)
    {
      info = g_type_get_qdata (*p, dbus_g_object_type_dbus_metadata_quark ());
      if (info != NULL && info->format_version >= 0)
        {
	  if (!callback (info, *p, user_data))
	    break;
	}
    }

  g_free (interfaces);

  for (classtype = G_TYPE_FROM_INSTANCE (object); classtype != 0; classtype = g_type_parent (classtype))
    {
      info = g_type_get_qdata (classtype, dbus_g_object_type_dbus_metadata_quark ());
      if (info != NULL && info->format_version >= 0)
	{
	  if (!callback (info, classtype, user_data))
	    break;
	}
    }

}

static gboolean
lookup_object_info_cb (const DBusGObjectInfo *info,
                       GType gtype,
		       gpointer user_data)
{
  GList **list = (GList **) user_data;

  *list = g_list_prepend (*list, (gpointer) info);
  return TRUE;
}

static GList *
lookup_object_info (GObject *object)
{
  GList *info_list = NULL;

  foreach_object_info (object, lookup_object_info_cb, &info_list);

  return info_list;
}

typedef struct {
  const char *iface;
  const DBusGObjectInfo *info;
  gboolean fallback;
  GType iface_type;
} LookupObjectInfoByIfaceData;

static gboolean
lookup_object_info_by_iface_cb (const DBusGObjectInfo *info,
				GType gtype,
				gpointer user_data)
{
  LookupObjectInfoByIfaceData *lookup_data = (LookupObjectInfoByIfaceData *) user_data;

  /* If interface is not specified, choose the first info */
  if (lookup_data->fallback && (!lookup_data->iface || strlen (lookup_data->iface) == 0))
    {
      lookup_data->info = info;
      lookup_data->iface_type = gtype;
    }
  else if (info->exported_properties && !strcmp (info->exported_properties, lookup_data->iface))
    {
      lookup_data->info = info;
      lookup_data->iface_type = gtype;
    }

  return !lookup_data->info;
}

static const DBusGObjectInfo *
lookup_object_info_by_iface (GObject     *object,
			     const char  *iface,
			     gboolean     fallback,
			     GType       *out_iface_type)
{
  LookupObjectInfoByIfaceData data;

  data.iface = iface;
  data.info = NULL;
  data.fallback = fallback;
  data.iface_type = 0;

  foreach_object_info (object, lookup_object_info_by_iface_cb, &data);

  if (out_iface_type && data.info)
    *out_iface_type = data.iface_type;

  return data.info;
}

typedef struct {
    /* owned */
    GSList *registrations;
    /* weak ref, or NULL if the object has been disposed */
    GObject *object;
} ObjectExport;

typedef struct {
    /* pseudo-weak ref, never NULL */
    DBusGConnection *connection;
    /* owned, never NULL */
    gchar *object_path;
    /* borrowed pointer to parent, never NULL */
    ObjectExport *export;
} ObjectRegistration;

static void object_export_object_died (gpointer user_data, GObject *dead);

static void
object_export_unregister_all (ObjectExport *oe)
{
  while (oe->registrations != NULL)
    {
      GSList *old = oe->registrations;
      ObjectRegistration *o = oe->registrations->data;

      dbus_connection_unregister_object_path (
          DBUS_CONNECTION_FROM_G_CONNECTION (o->connection), o->object_path);

      /* the link should have been removed by doing that */
      g_assert (oe->registrations != old);
    }
}

static void
object_export_free (ObjectExport *oe)
{
  g_slice_free (ObjectExport, oe);
}

static ObjectExport *
object_export_new (void)
{
  return g_slice_new0 (ObjectExport);
}

static ObjectRegistration *
object_registration_new (DBusGConnection *connection,
                         const gchar *object_path,
                         ObjectExport *export)
{
  ObjectRegistration *o = g_slice_new0 (ObjectRegistration);

  o->connection = connection;
  o->object_path = g_strdup (object_path);
  o->export = export;

  return o;
}

static void
object_registration_free (ObjectRegistration *o)
{
  g_assert (o->export != NULL);
  o->export->registrations = g_slist_remove (o->export->registrations, o);

  g_free (o->object_path);

  g_slice_free (ObjectRegistration, o);
}

/* Called when the object falls off the bus (e.g. because connection just
 * closed) */
static void
object_registration_unregistered (DBusConnection *connection,
                                  void *user_data)
{
  object_registration_free (user_data);
}

typedef struct
{
  GObject *object;
  GString *xml;
  GType gtype;
  const DBusGObjectInfo *object_info;
} DBusGLibWriteIterfaceData;

typedef struct
{
  GSList *methods;
  GSList *signals;
  GSList *properties;
} DBusGLibWriteInterfaceValues;

static void
write_interface (gpointer key, gpointer val, gpointer user_data)
{
  const char *name;
  GSList *methods;
  GSList *signals;
  GSList *properties;
  GString *xml;
  const DBusGObjectInfo *object_info;
  DBusGLibWriteIterfaceData *data;
  DBusGLibWriteInterfaceValues *values;

  name = key;

  values = val;
  methods = values->methods;
  signals = values->signals;
  properties = values->properties;

  data = user_data;
  xml = data->xml;
  object_info = data->object_info;

  g_string_append_printf (xml, "  <interface name=\"%s\">\n", name);

  /* FIXME: recurse to parent types ? */
  for (; methods; methods = methods->next)
    {
      DBusGMethodInfo *method;
      const char *args;
      method = methods->data;

      g_string_append_printf (xml, "    <method name=\"%s\">\n",
			      method_name_from_object_info (object_info, method));

      args = method_arg_info_from_object_info (object_info, method);

      while (*args)
	{
	  const char *name;
	  gboolean arg_in;
	  const char *type;
	  
	  args = arg_iterate (args, &name, &arg_in, NULL, NULL, &type);

	  /* FIXME - handle container types */
	  g_string_append_printf (xml, "      <arg name=\"%s\" type=\"%s\" direction=\"%s\"/>\n",
				  name, type, arg_in ? "in" : "out");

	}
      g_string_append (xml, "    </method>\n");

    }
  g_slist_free (values->methods);

  for (; signals; signals = signals->next)
    {
      guint id;
      guint arg;
      const char *signame;
      GSignalQuery query;
      char *s;

      signame = signals->data;

      s = _dbus_gutils_wincaps_to_uscore (signame);
      
      id = g_signal_lookup (s, data->gtype);
      g_assert (id != 0);

      g_signal_query (id, &query);
      g_assert (query.return_type == G_TYPE_NONE);

      g_string_append_printf (xml, "    <signal name=\"%s\">\n", signame);

      for (arg = 0; arg < query.n_params; arg++)
	{
	  char *dbus_type = _dbus_gtype_to_signature (query.param_types[arg]);

	  g_assert (dbus_type != NULL);

          g_string_append (xml, "      <arg type=\"");
          g_string_append (xml, dbus_type);
          g_string_append (xml, "\"/>\n");
	  g_free (dbus_type);
	}

      g_string_append (xml, "    </signal>\n");
      g_free (s);
    }
  g_slist_free (values->signals);

  for (; properties; properties = properties->next)
    {
      const char *iface;
      const char *propname;
      const char *propname_uscore;
      const char *access_type;
      GParamSpec *spec;
      char *dbus_type;
      gboolean can_set;
      gboolean can_get;
      char *s;

      spec = NULL;

      property_iterate (properties->data, object_info->format_version, &iface, &propname, &propname_uscore, &access_type);

      s = lookup_property_name (data->object, name, propname);

      spec = g_object_class_find_property (g_type_class_peek (data->gtype), s);
      g_assert (spec != NULL);
      g_free (s);
      
      dbus_type = _dbus_gtype_to_signature (G_PARAM_SPEC_VALUE_TYPE (spec));
      g_assert (dbus_type != NULL);

      can_set = strcmp (access_type, "readwrite") == 0
                    && ((spec->flags & G_PARAM_WRITABLE) != 0
                    && (spec->flags & G_PARAM_CONSTRUCT_ONLY) == 0);

      can_get = (spec->flags & G_PARAM_READABLE) != 0;

      if (can_set || can_get)
	{
	  g_string_append_printf (xml, "    <property name=\"%s\" ", propname);
	  g_string_append (xml, "type=\"");
	  g_string_append (xml, dbus_type);
	  g_string_append (xml, "\" access=\"");

	  if (can_set && can_get)
	    g_string_append (xml, "readwrite");
	  else if (can_get)
	    g_string_append (xml, "read");
	  else
	    {
	      g_assert (can_set);
	      g_string_append (xml, "write");
	    }
          
	  g_string_append (xml, "\"/>\n");
	}
      
      g_free (dbus_type);
    }
  g_slist_free (values->properties);

  g_free (values);
  g_string_append (xml, "  </interface>\n");
}

static DBusGLibWriteInterfaceValues *
lookup_values (GHashTable *interfaces, const char *method_interface)
{
  DBusGLibWriteInterfaceValues *values;
  if ((values = g_hash_table_lookup (interfaces, (gpointer) method_interface)) == NULL)
    {
      values = g_new0 (DBusGLibWriteInterfaceValues, 1);
      g_hash_table_insert (interfaces, (gpointer) method_interface, values);
    }
  return values;
}

static void
introspect_interfaces (GObject *object, GString *xml)
{
  GList *info_list;
  const GList *info_list_walk;
  const DBusGObjectInfo *info;
  DBusGLibWriteIterfaceData data;
  int i;
  GHashTable *interfaces;
  DBusGLibWriteInterfaceValues *values;
  const char *propsig;

  info_list = lookup_object_info (object);

  g_assert (info_list != NULL);

  /* Gather a list of all interfaces, indexed into their methods */
  for (info_list_walk = info_list; info_list_walk != NULL; info_list_walk = g_list_next (info_list_walk))
    {
      info = (DBusGObjectInfo *) info_list_walk->data;
      interfaces = g_hash_table_new (g_str_hash, g_str_equal);
      
      g_assert (info != NULL);

      for (i = 0; i < info->n_method_infos; i++)
        {
          const char *method_interface;
          const DBusGMethodInfo *method;

          method = &(info->method_infos[i]);

          method_interface = method_interface_from_object_info (info, method);

          values = lookup_values (interfaces, method_interface);
          values->methods = g_slist_prepend (values->methods, (gpointer) method);
        }

      propsig = info->exported_signals;
      while (propsig != NULL && *propsig)
        {
          const char *iface;
          const char *signame;

          propsig = signal_iterate (propsig, &iface, &signame);

          values = lookup_values (interfaces, iface);
          values->signals = g_slist_prepend (values->signals, (gpointer) signame);
        }

      propsig = info->exported_properties;
      while (propsig != NULL && *propsig)
        {
          const char *iface;
          const char *propname;
          const char *propname_uscore;
          const char *access_type;

          propsig = property_iterate (propsig, info->format_version, &iface, &propname, &propname_uscore, &access_type);

          values = lookup_values (interfaces, iface);
          values->properties = g_slist_prepend (values->properties, (gpointer)iface);
        }

      memset (&data, 0, sizeof (data));
      data.xml = xml;
      data.gtype = G_TYPE_FROM_INSTANCE (object);
      data.object_info = info;
      data.object = object;

      g_hash_table_foreach (interfaces, write_interface, &data);
      g_hash_table_destroy (interfaces);
    }

  g_list_free (info_list);
}

static DBusHandlerResult
handle_introspect (DBusConnection *connection,
                   DBusMessage    *message,
                   GObject        *object)
{
  GString *xml;
  unsigned int i;
  DBusMessage *ret;
  char **children;
  
  if (!dbus_connection_list_registered (connection, 
                                        dbus_message_get_path (message),
                                        &children))
    oom (NULL);
  
  xml = g_string_new (NULL);

  g_string_append (xml, DBUS_INTROSPECT_1_0_XML_DOCTYPE_DECL_NODE);
  
  g_string_append (xml, "<node>\n");

  /* We are introspectable, though I guess that was pretty obvious */
  g_string_append_printf (xml, "  <interface name=\"%s\">\n", DBUS_INTERFACE_INTROSPECTABLE);
  g_string_append (xml, "    <method name=\"Introspect\">\n");
  g_string_append_printf (xml, "      <arg name=\"data\" direction=\"out\" type=\"%s\"/>\n", DBUS_TYPE_STRING_AS_STRING);
  g_string_append (xml, "    </method>\n");
  g_string_append (xml, "  </interface>\n");

  /* We support get/set/getall properties */
  g_string_append_printf (xml, "  <interface name=\"%s\">\n", DBUS_INTERFACE_PROPERTIES);
  g_string_append (xml, "    <method name=\"Get\">\n");
  g_string_append_printf (xml, "      <arg name=\"interface\" direction=\"in\" type=\"%s\"/>\n", DBUS_TYPE_STRING_AS_STRING);
  g_string_append_printf (xml, "      <arg name=\"propname\" direction=\"in\" type=\"%s\"/>\n", DBUS_TYPE_STRING_AS_STRING);
  g_string_append_printf (xml, "      <arg name=\"value\" direction=\"out\" type=\"%s\"/>\n", DBUS_TYPE_VARIANT_AS_STRING);
  g_string_append (xml, "    </method>\n");
  g_string_append (xml, "    <method name=\"Set\">\n");
  g_string_append_printf (xml, "      <arg name=\"interface\" direction=\"in\" type=\"%s\"/>\n", DBUS_TYPE_STRING_AS_STRING);
  g_string_append_printf (xml, "      <arg name=\"propname\" direction=\"in\" type=\"%s\"/>\n", DBUS_TYPE_STRING_AS_STRING);
  g_string_append_printf (xml, "      <arg name=\"value\" direction=\"in\" type=\"%s\"/>\n", DBUS_TYPE_VARIANT_AS_STRING);
  g_string_append (xml, "    </method>\n");
  g_string_append (xml, "    <method name=\"GetAll\">\n");
  g_string_append_printf (xml, "      <arg name=\"interface\" direction=\"in\" type=\"%s\"/>\n", DBUS_TYPE_STRING_AS_STRING);
  g_string_append_printf (xml, "      <arg name=\"props\" direction=\"out\" type=\"%s\"/>\n",
                          DBUS_TYPE_ARRAY_AS_STRING
                          DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING
                            DBUS_TYPE_STRING_AS_STRING
                            DBUS_TYPE_VARIANT_AS_STRING
                          DBUS_DICT_ENTRY_END_CHAR_AS_STRING
                          );

  g_string_append (xml, "    </method>\n");
  g_string_append (xml, "  </interface>\n");
  
  introspect_interfaces (object, xml);

  /* Append child nodes */
  for (i = 0; children[i]; i++)
      g_string_append_printf (xml, "  <node name=\"%s\"/>\n",
                              children[i]);
  
  /* Close the XML, and send it to the requesting app */
  g_string_append (xml, "</node>\n");

  ret = reply_or_die (message);

  dbus_message_append_args (ret,
                            DBUS_TYPE_STRING, &xml->str,
                            DBUS_TYPE_INVALID);

  connection_send_or_die (connection, ret);
  dbus_message_unref (ret);

  g_string_free (xml, TRUE);

  dbus_free_string_array (children);
  
  return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusMessage*
set_object_property (DBusConnection  *connection,
                     DBusMessage     *message,
                     DBusMessageIter *iter,
                     GObject         *object,
                     GParamSpec      *pspec)
{
  GValue value = { 0, };
  DBusMessage *ret;
  DBusMessageIter sub;
  DBusGValueMarshalCtx context;

  dbus_message_iter_recurse (iter, &sub);

  context.recursion_depth = 0;
  context.gconnection = DBUS_G_CONNECTION_FROM_CONNECTION (connection);
  context.proxy = NULL;

  g_value_init (&value, pspec->value_type);
  if (_dbus_gvalue_demarshal (&context, &sub, &value, NULL))
    {
      g_object_set_property (object,
                             pspec->name,
                             &value);

      g_value_unset (&value);

      ret = reply_or_die (message);
    }
  else
    {
      ret = error_or_die (message,
          DBUS_ERROR_INVALID_ARGS,
          "Argument's D-BUS type can't be converted to a GType");
    }

  return ret;
}

/*
 * @pspec: the paramspec for a D-Bus-exported property
 *
 * Returns: a reply for the Get() D-Bus method, either successful or error
 */
static DBusMessage*
get_object_property (DBusConnection *connection,
                     DBusMessage    *message,
                     GObject        *object,
                     GParamSpec     *pspec)
{
  GType value_gtype;
  GValue value = {0, };
  gchar *variant_sig;
  DBusMessage *ret;
  DBusMessageIter iter, subiter;
  gchar *error_message = NULL;

  ret = reply_or_die (message);

  g_value_init (&value, pspec->value_type);
  g_object_get_property (object, pspec->name, &value);

  variant_sig = _dbus_gvalue_to_signature (&value);
  if (variant_sig == NULL)
    {
      value_gtype = G_VALUE_TYPE (&value);
      error_message = g_strdup_printf (
          "Internal error: cannot marshal type \"%s\" in variant",
          g_type_name (value_gtype));
      goto out;
    }

  dbus_message_iter_init_append (ret, &iter);
  if (!dbus_message_iter_open_container (&iter,
					 DBUS_TYPE_VARIANT,
					 variant_sig,
					 &subiter))
    {
      error_message = g_strdup_printf (
          "Internal error: cannot open variant container for signature %s",
          variant_sig);
      goto out;
    }

  if (!_dbus_gvalue_marshal (&subiter, &value))
    {
      dbus_message_iter_abandon_container (&iter, &subiter);
      error_message = g_strdup_printf (
          "Internal error: could not marshal type \"%s\" in variant",
          G_VALUE_TYPE_NAME (&value));
      goto out;
    }

  dbus_message_iter_close_container (&iter, &subiter);

out:
  g_value_unset (&value);
  g_free (variant_sig);

  if (error_message != NULL)
    {
      dbus_message_unref (ret);
      ret = error_or_die (message, DBUS_ERROR_FAILED, error_message);
      g_critical ("%s", error_message);
      g_free (error_message);
    }

  return ret;
}

#define SHADOW_PROP_QUARK (dbus_g_object_type_dbus_shadow_property_quark ())

static GQuark
dbus_g_object_type_dbus_shadow_property_quark (void)
{
  static GQuark quark;

  if (!quark)
    quark = g_quark_from_static_string ("DBusGObjectTypeDBusShadowPropertyQuark");
  return quark;
}

/* Look for shadow properties on the given interface first, otherwise
 * just return the original property name.  This allows implementations to
 * get around the glib limitation of unique property names among all
 * GInterfaces by registering a "shadow" property name that the get/set
 * request will be redirected to.
 *
 * Shadow property data is stored as qdata on each GInterface.  If there
 * is no interface info, or there is no registered shadow property, just
 * return the original property name.
 */
static char *
lookup_property_name (GObject    *object,
                      const char *wincaps_propiface,
                      const char *requested_propname)
{
  const DBusGObjectInfo *object_info;
  GHashTable *shadow_props;
  char *shadow_prop_name = NULL, *uscore_name;
  GType iface_type = 0;

  g_assert (wincaps_propiface != NULL);
  g_assert (requested_propname != NULL);

  uscore_name = _dbus_gutils_wincaps_to_uscore (requested_propname);

  object_info = lookup_object_info_by_iface (object, wincaps_propiface, FALSE, &iface_type);
  if (!object_info)
    return uscore_name;

  shadow_props = (GHashTable *) g_type_get_qdata (iface_type, SHADOW_PROP_QUARK);
  if (shadow_props)
    {
      shadow_prop_name = g_strdup (g_hash_table_lookup (shadow_props, requested_propname));
      if (shadow_prop_name)
        g_free (uscore_name);
    }

  return shadow_prop_name ? shadow_prop_name : uscore_name;
}

/**
 * dbus_g_object_type_register_shadow_property:
 * @iface_type: #GType for the #GInterface
 * @dbus_prop_name: D-Bus property name (as specified in the introspection data)
 *  to override with the shadow property name (as specified in the GType's
 *  initialization function, ie glib-style)
 * @shadow_prop_name: property name which should override the shadow property
 *
 * Registers a new property name @shadow_prop_name that overrides the
 * @dbus_prop_name in D-Bus property get/set requests.  Since all properties for
 * all interfaces implemented by a GObject exist in the same namespace, this
 * allows implementations to use the same property name in two or more D-Bus
 * interfaces implemented by the same GObject, as long as one of those D-Bus
 * interface properties is registered with a shadow property name.
 *
 * For example, if both org.foobar.Baz.InterfaceA and org.foobar.Baz.InterfaceB
 * have a D-Bus property called "Bork", the developer assigns a shadow property
 * name to the conflicting property name in one or both of these GInterfaces to
 * resolve the conflict.  Assume the GInterface implementing
 * org.foobar.Baz.InterfaceA registers a shadow property called "a-bork", while
 * the GInterface implementing org.foobar.Baz.InterfaceB registers a shadow
 * property called "b-bork".  The GObject implementing both these GInterfaces
 * would then use #g_object_class_override_property() to implement both
 * "a-bork" and "b-bork" and D-Bus requests for "Bork" on either D-Bus interface
 * will not conflict.
 */
void
dbus_g_object_type_register_shadow_property (GType      iface_type,
                                             const char *dbus_prop_name,
                                             const char *shadow_prop_name)
{
  GHashTable *shadow_props;

  g_return_if_fail (G_TYPE_IS_CLASSED (iface_type) || G_TYPE_IS_INTERFACE (iface_type));
  g_return_if_fail (dbus_prop_name != NULL);
  g_return_if_fail (shadow_prop_name != NULL);

  shadow_props = (GHashTable *) g_type_get_qdata (iface_type, SHADOW_PROP_QUARK);
  if (!shadow_props)
    {
      shadow_props = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
      g_type_set_qdata (iface_type,
                        dbus_g_object_type_dbus_shadow_property_quark (),
                        shadow_props);
    }

  g_assert (shadow_props);
  g_hash_table_insert (shadow_props, g_strdup (dbus_prop_name), g_strdup (shadow_prop_name));
}

static DBusMessage*
get_all_object_properties (DBusConnection        *connection,
                           DBusMessage           *message,
                           const DBusGObjectInfo *object_info,
                           const char            *wincaps_propiface,
                           GObject               *object)
{
  DBusMessage *ret;
  DBusMessageIter iter_ret;
  DBusMessageIter iter_dict;
  DBusMessageIter iter_dict_entry;
  DBusMessageIter iter_dict_value;
  const char *p;
  char *uscore_propname;

  ret = reply_or_die (message);

  dbus_message_iter_init_append (ret, &iter_ret);

  /* the types are all hard-coded, so this can only fail via OOM */
  if (!dbus_message_iter_open_container (&iter_ret,
                                         DBUS_TYPE_ARRAY,
                                         DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING
                                         DBUS_TYPE_STRING_AS_STRING
                                         DBUS_TYPE_VARIANT_AS_STRING
                                         DBUS_DICT_ENTRY_END_CHAR_AS_STRING,
                                         &iter_dict))
    oom (NULL);

  p = object_info->exported_properties;
  while (p != NULL && *p != '\0')
    {
      const char *prop_ifname;
      const char *prop_name;
      const char *prop_uscored;
      const char *access_flags;
      GParamSpec *pspec;
      GType value_gtype;
      GValue value = {0, };
      gchar *variant_sig;

      p = property_iterate (p, object_info->format_version, &prop_ifname, &prop_name, &prop_uscored, &access_flags);

      /* Conventionally, property names are valid member names, but dbus-glib
       * doesn't enforce this, and some dbus-glib services use GObject-style
       * property names (e.g. "foo-bar"). */
      if (!g_utf8_validate (prop_name, -1, NULL))
        {
          g_critical ("property name isn't UTF-8: %s", prop_name);
          continue;
        }

      uscore_propname = lookup_property_name (object, wincaps_propiface, prop_name);

      pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (object), uscore_propname);
      if (pspec == NULL)
        {
          g_warning ("introspection data references non-existing property %s", uscore_propname);
          g_free (uscore_propname);
          continue;
        }

      g_free (uscore_propname);

      g_value_init (&value, pspec->value_type);
      g_object_get_property (object, pspec->name, &value);

      variant_sig = _dbus_gvalue_to_signature (&value);
      if (variant_sig == NULL)
        {
          value_gtype = G_VALUE_TYPE (&value);
          g_warning ("Cannot marshal type \"%s\" in variant", g_type_name (value_gtype));
          g_value_unset (&value);
          continue;
        }

      /* a signature returned by _dbus_gvalue_to_signature had better be
       * valid */
      g_assert (g_variant_is_signature (variant_sig));

      /* type is hard-coded, so this can't fail except by OOM */
      if (!dbus_message_iter_open_container (&iter_dict,
                                             DBUS_TYPE_DICT_ENTRY,
                                             NULL,
                                             &iter_dict_entry))
        oom (NULL);

      /* prop_name is valid UTF-8, so this can't fail except by OOM; no point
       * in abandoning @iter_dict_entry since we're about to crash out */
      if (!dbus_message_iter_append_basic (&iter_dict_entry, DBUS_TYPE_STRING, &prop_name))
        oom (NULL);

      /* variant_sig has been asserted to be valid, so this can't fail
       * except by OOM */
      if (!dbus_message_iter_open_container (&iter_dict_entry,
                                             DBUS_TYPE_VARIANT,
                                             variant_sig,
                                             &iter_dict_value))
        oom (NULL);

      g_free (variant_sig);

      /* this can fail via programming error: the GObject property was
       * malformed (non-UTF8 string or something) */
      if (!_dbus_gvalue_marshal (&iter_dict_value, &value))
        {
          gchar *contents = g_strdup_value_contents (&value);
          gchar *error_message = g_strdup_printf (
              "cannot GetAll(%s): failed to serialize %s value of type %s: %s",
              wincaps_propiface, prop_name, G_VALUE_TYPE_NAME (&value),
              contents);

          g_critical ("%s", error_message);

          /* abandon ship! */
          dbus_message_iter_abandon_container (&iter_dict_entry,
              &iter_dict_value);
          dbus_message_iter_abandon_container (&iter_dict, &iter_dict_entry);
          dbus_message_unref (ret);
          ret = error_or_die (message, DBUS_ERROR_FAILED, error_message);

          g_free (contents);
          g_free (error_message);
          g_value_unset (&value);
          return ret;
        }

      /* these shouldn't fail except by OOM now that we were successful */
      if (!dbus_message_iter_close_container (&iter_dict_entry,
                                              &iter_dict_value))
        oom (NULL);
      if (!dbus_message_iter_close_container (&iter_dict, &iter_dict_entry))
        oom (NULL);

      g_value_unset (&value);
  }

  if (!dbus_message_iter_close_container (&iter_ret, &iter_dict))
    oom (NULL);

  return ret;
}

static gboolean
lookup_object_and_method (GObject      *object,
			  DBusMessage  *message,
			  const DBusGObjectInfo **object_ret,
			  const DBusGMethodInfo **method_ret)
{
  const char *interface;
  const char *member;
  const char *signature;
  GList *info_list;
  const GList *info_list_walk;
  const DBusGObjectInfo *info;
  int i;

  interface = dbus_message_get_interface (message);
  member = dbus_message_get_member (message);
  signature = dbus_message_get_signature (message);

  info_list = lookup_object_info (object);
  
  for (info_list_walk = info_list; info_list_walk != NULL; info_list_walk = g_list_next (info_list_walk))
    {
      info = (DBusGObjectInfo *) info_list_walk->data;
      *object_ret = info;

      for (i = 0; i < info->n_method_infos; i++)
        {
          const char *expected_member;
          const char *expected_interface;
          char *expected_signature;
          const DBusGMethodInfo *method;

          method = &(info->method_infos[i]);

          /* Check method interface/name and input signature */ 
          expected_interface = method_interface_from_object_info (*object_ret, method);
          expected_member = method_name_from_object_info (*object_ret, method);
          expected_signature = method_input_signature_from_object_info (*object_ret, method);

          if ((interface == NULL
              || strcmp (expected_interface, interface) == 0)
              && strcmp (expected_member, member) == 0
              && strcmp (expected_signature, signature) == 0)
            {
              g_free (expected_signature);
              *method_ret = method;
              g_list_free (info_list);
              return TRUE;
            }
            g_free (expected_signature);
        }
    }

  if (info_list)
    g_list_free (info_list);

  return FALSE;
}

static char *
gerror_domaincode_to_dbus_error_name (const DBusGObjectInfo *object_info,
				      const char *msg_interface,
				      GQuark domain, gint code)
{
  const char *domain_str;
  const char *code_str;
  GString *dbus_error_name;

  domain_str = object_error_domain_prefix_from_object_info (object_info);
  code_str = object_error_code_from_object_info (object_info, domain, code);

  if (!domain_str || !code_str)
    {
      DBusGErrorInfo *info;

      g_static_rw_lock_reader_lock (&globals_lock);

      if (error_metadata != NULL)
	info = g_datalist_id_get_data (&error_metadata, domain);
      else
	info = NULL;

      g_static_rw_lock_reader_unlock (&globals_lock);

      if (info)
	{
	  GEnumValue *value;
	  GEnumClass *klass;

	  klass = g_type_class_ref (info->code_enum);
	  value = g_enum_get_value (klass, code);
	  g_type_class_unref (klass);

	  domain_str = info->default_iface;
	  code_str = value->value_nick;
	}
    }

  if (!domain_str)
    domain_str = msg_interface;

  if (!domain_str || !code_str)
    {
      const char *domain_string;
      /* If we can't map it sensibly, make up an error name */
      
      dbus_error_name = g_string_new ("org.freedesktop.DBus.GLib.UnmappedError.");

      domain_string = g_quark_to_string (domain);
      if (domain_string != NULL)
        {
          char *uscored = uscore_to_wincaps (domain_string);
          g_string_append (dbus_error_name, uscored);
          g_string_append_c (dbus_error_name, '.');
          g_free (uscored);
        }

      g_string_append_printf (dbus_error_name, "Code%d", code);
    }
  else
    {
      gchar *code_str_wincaps;
      dbus_error_name = g_string_new (domain_str);
      g_string_append_c (dbus_error_name, '.');
      /* We can't uppercase here for backwards compatibility
       * reasons; if someone had a lowercase enumeration value,
       * previously we'd just send it across unaltered.
       */
      code_str_wincaps = uscore_to_wincaps_full (code_str, FALSE, FALSE);
      g_string_append (dbus_error_name, code_str_wincaps);
      g_free (code_str_wincaps);
    }

  return g_string_free (dbus_error_name, FALSE);
}

static DBusMessage *
gerror_to_dbus_error_message (const DBusGObjectInfo *object_info,
			      DBusMessage           *message,
			      const GError          *error)
{
  DBusMessage *reply;

  if (!error)
    {
      char *error_msg;
      
      error_msg = g_strdup_printf ("Method invoked for %s returned FALSE but did not set error", dbus_message_get_member (message));
      reply = error_or_die (message, "org.freedesktop.DBus.GLib.ErrorError", error_msg);
      g_free (error_msg);
    }
  else
    {
      if (error->domain == DBUS_GERROR)
        {
          const gchar *name = DBUS_ERROR_FAILED;

          switch (error->code)
            {
            case DBUS_GERROR_FAILED:
              name = DBUS_ERROR_FAILED;
              break;
            case DBUS_GERROR_NO_MEMORY:
              name = DBUS_ERROR_NO_MEMORY;
              break;
            case DBUS_GERROR_SERVICE_UNKNOWN:
              name = DBUS_ERROR_SERVICE_UNKNOWN;
              break;
            case DBUS_GERROR_NAME_HAS_NO_OWNER:
              name = DBUS_ERROR_NAME_HAS_NO_OWNER;
              break;
            case DBUS_GERROR_NO_REPLY:
              name = DBUS_ERROR_NO_REPLY;
              break;
            case DBUS_GERROR_IO_ERROR:
              name = DBUS_ERROR_IO_ERROR;
              break;
            case DBUS_GERROR_BAD_ADDRESS:
              name = DBUS_ERROR_BAD_ADDRESS;
              break;
            case DBUS_GERROR_NOT_SUPPORTED:
              name = DBUS_ERROR_NOT_SUPPORTED;
              break;
            case DBUS_GERROR_LIMITS_EXCEEDED:
              name = DBUS_ERROR_LIMITS_EXCEEDED;
              break;
            case DBUS_GERROR_ACCESS_DENIED:
              name = DBUS_ERROR_ACCESS_DENIED;
              break;
            case DBUS_GERROR_AUTH_FAILED:
              name = DBUS_ERROR_AUTH_FAILED;
              break;
            case DBUS_GERROR_NO_SERVER:
              name = DBUS_ERROR_NO_SERVER;
              break;
            case DBUS_GERROR_TIMEOUT:
              name = DBUS_ERROR_TIMEOUT;
              break;
            case DBUS_GERROR_NO_NETWORK:
              name = DBUS_ERROR_NO_NETWORK;
              break;
            case DBUS_GERROR_ADDRESS_IN_USE:
              name = DBUS_ERROR_ADDRESS_IN_USE;
              break;
            case DBUS_GERROR_DISCONNECTED:
              name = DBUS_ERROR_DISCONNECTED;
              break;
            case DBUS_GERROR_INVALID_ARGS:
              name = DBUS_ERROR_INVALID_ARGS;
              break;
            case DBUS_GERROR_FILE_NOT_FOUND:
              name = DBUS_ERROR_FILE_NOT_FOUND;
              break;
            case DBUS_GERROR_REMOTE_EXCEPTION:
              name = dbus_g_error_get_name ((GError*) error);
              break;
            }

          reply = error_or_die (message, name, error->message);
        }
      else
	{
	  char *error_name;
	  error_name = gerror_domaincode_to_dbus_error_name (object_info,
							     dbus_message_get_interface (message),
							     error->domain, error->code);
          reply = error_or_die (message, error_name, error->message);
          g_free (error_name);
        }
    }

  return reply;
}

/**
 * SECTION:dbus-gmethod
 * @short_description: GMethod Info & Invocation
 * @see_also: #DBusGMessage
 * @stability: Stable
 *
 * These types are used to call methods on #GObject objects.
 */

/**
 * DBusGMethodInvocation:
 *
 * The context of an asynchronous method call.  See dbus_g_method_return() and
 * dbus_g_method_return_error().
 */
struct _DBusGMethodInvocation {
  DBusGConnection *connection; /**< The connection */
  DBusGMessage *message; /**< The message which generated the method call */
  const DBusGObjectInfo *object; /**< The object the method was called on */
  const DBusGMethodInfo *method; /**< The method called */
  gboolean send_reply;
};

static DBusHandlerResult
invoke_object_method (GObject         *object,
		      const DBusGObjectInfo *object_info,
		      const DBusGMethodInfo *method,
		      DBusConnection  *connection,
		      DBusMessage     *message)
{
  gboolean had_error, is_async, send_reply;
  GError *gerror;
  GValueArray *value_array;
  GValue return_value = {0,};
  GClosure closure;
  char *in_signature;
  GArray *out_param_values = NULL;
  GValueArray *out_param_gvalues = NULL;
  int out_param_count;
  int out_param_pos, out_param_gvalue_pos;
  DBusMessage *reply = NULL;
  gboolean have_retval;
  gboolean retval_signals_error;
  gboolean retval_is_synthetic;
  gboolean retval_is_constant;
  const char *arg_metadata;

  gerror = NULL;

  /* This flag says whether invokee is handed a special DBusGMethodInvocation structure,
   * instead of being required to fill out all return values in the context of the function.
   * Some additional data is also exposed, such as the message sender.
   */
  is_async = strcmp (string_table_lookup (get_method_data (object_info, method), 2), "A") == 0;
  
  /* Messages can be sent with a flag that says "I don't need a reply".  This is an optimization
   * normally, but in the context of the system bus it's important to not send a reply
   * to these kinds of messages, because they will be unrequested replies, and thus subject
   * to denial and logging.  We don't want to fill up logs.
   * http://bugs.freedesktop.org/show_bug.cgi?id=19441
   */
  send_reply = !dbus_message_get_no_reply (message); 

  have_retval = FALSE;
  retval_signals_error = FALSE;
  retval_is_synthetic = FALSE;
  retval_is_constant = FALSE;

  /* This is evil.  We do this to work around the fact that
   * the generated glib marshallers check a flag in the closure object
   * which we don't care about.  We don't need/want to create
   * a new closure for each invocation.
   */
  memset (&closure, 0, sizeof (closure));

  in_signature = method_input_signature_from_object_info (object_info, method); 
  
  /* Convert method IN parameters to GValueArray */
  {
    GArray *types_array;
    guint n_params;
    const GType *types;
    DBusGValueMarshalCtx context;
    GError *error = NULL;
    
    context.recursion_depth = 0;
    context.gconnection = DBUS_G_CONNECTION_FROM_CONNECTION (connection);
    context.proxy = NULL;

    types_array = _dbus_gtypes_from_arg_signature (in_signature, FALSE);
    n_params = types_array->len;
    types = (const GType*) types_array->data;

    value_array = _dbus_gvalue_demarshal_message (&context, message, n_params, types, &error);
    if (value_array == NULL)
      {
	g_free (in_signature); 
	g_array_free (types_array, TRUE);
        reply = error_or_die (message, "org.freedesktop.DBus.GLib.ErrorError", error->message);
        connection_send_or_die (connection, reply);
	dbus_message_unref (reply);
	g_error_free (error);
	return DBUS_HANDLER_RESULT_HANDLED;
      }
    g_array_free (types_array, TRUE);
  }

  /* Prepend object as first argument */ 
  g_value_array_prepend (value_array, NULL);
  g_value_init (g_value_array_get_nth (value_array, 0), G_TYPE_OBJECT);
  g_value_set_object (g_value_array_get_nth (value_array, 0), object);
  
  if (is_async)
    {
      GValue context_value = {0,};
      DBusGMethodInvocation *context;
      context = g_new (DBusGMethodInvocation, 1);
      context->connection = dbus_g_connection_ref (DBUS_G_CONNECTION_FROM_CONNECTION (connection));
      context->message = dbus_g_message_ref (DBUS_G_MESSAGE_FROM_MESSAGE (message));
      context->object = object_info;
      context->method = method;
      context->send_reply = send_reply;
      g_value_init (&context_value, G_TYPE_POINTER);
      g_value_set_pointer (&context_value, context);
      g_value_array_append (value_array, &context_value);
    }
  else
    {
      RetvalType retval;
      gboolean arg_in;
      gboolean arg_const;
      const char *argsig;

      arg_metadata = method_arg_info_from_object_info (object_info, method);
      
      /* Count number of output parameters, and look for a return value */
      out_param_count = 0;
      while (*arg_metadata)
	{
	  arg_metadata = arg_iterate (arg_metadata, NULL, &arg_in, &arg_const, &retval, &argsig);
	  if (arg_in)
	    continue;
	  if (retval != RETVAL_NONE)
	    {
	      DBusSignatureIter tmp_sigiter;
	      /* This is the function return value */
	      g_assert (!have_retval);
	      have_retval = TRUE;
	      retval_is_synthetic = FALSE;

	      switch (retval)
		{
		case RETVAL_NONE:
		  g_assert_not_reached ();
		  break;
		case RETVAL_NOERROR:
		  retval_signals_error = FALSE;
		  break;
		case RETVAL_ERROR:
		  retval_signals_error = TRUE;
		  break;
		}

	      retval_is_constant = arg_const;

	      /* Initialize our return GValue with the specified type */
	      dbus_signature_iter_init (&tmp_sigiter, argsig);
	      g_value_init (&return_value, _dbus_gtype_from_signature_iter (&tmp_sigiter, FALSE));
	    }
	  else
	    {
	      /* It's a regular output value */
	      out_param_count++;
	    }
	}

      /* For compatibility, if we haven't found a return value, we assume
       * the function returns a gboolean for signalling an error
       * (and therefore also takes a GError).  We also note that it
       * is a "synthetic" return value; i.e. we aren't going to be
       * sending it over the bus, it's just to signal an error.
       */
      if (!have_retval)
	{
	  have_retval = TRUE;
	  retval_is_synthetic = TRUE;
	  retval_signals_error = TRUE;
	  g_value_init (&return_value, G_TYPE_BOOLEAN);
	}

      /* Create an array to store the actual values of OUT parameters
       * (other than the real function return, if any).  Then, create
       * a GValue boxed POINTER to each of those values, and append to
       * the invocation, so the method can return the OUT parameters.
       */
      out_param_values = g_array_sized_new (FALSE, TRUE, sizeof (GTypeCValue), out_param_count);

      /* We have a special array of GValues for toplevel GValue return
       * types.
       */
      out_param_gvalues = g_value_array_new (out_param_count);
      out_param_pos = 0;
      out_param_gvalue_pos = 0;

      /* Reset argument metadata pointer */
      arg_metadata = method_arg_info_from_object_info (object_info, method);
      
      /* Iterate over output arguments again, this time allocating space for
       * them as appopriate.
       */
      while (*arg_metadata)
	{
	  GValue value = {0, };
	  GTypeCValue storage;
	  DBusSignatureIter tmp_sigiter;
	  GType current_gtype;

	  arg_metadata = arg_iterate (arg_metadata, NULL, &arg_in, NULL, &retval, &argsig);
	  /* Skip over input arguments and the return value, if any */
	  if (arg_in || retval != RETVAL_NONE)
	    continue;

	  dbus_signature_iter_init (&tmp_sigiter, argsig);
	  current_gtype = _dbus_gtype_from_signature_iter (&tmp_sigiter, FALSE);

	  g_value_init (&value, G_TYPE_POINTER);

	  /* We special case variants to make method invocation a bit nicer */
	  if (current_gtype != G_TYPE_VALUE)
	    {
	      memset (&storage, 0, sizeof (storage));
	      g_array_append_val (out_param_values, storage);
	      g_value_set_pointer (&value, &(g_array_index (out_param_values, GTypeCValue, out_param_pos)));
	      out_param_pos++;
	    }
	  else
	    {
	      g_value_array_append (out_param_gvalues, NULL);
	      g_value_set_pointer (&value, out_param_gvalues->values + out_param_gvalue_pos);
	      out_param_gvalue_pos++;
	    }
	  g_value_array_append (value_array, &value);
	}
    }

  /* Append GError as final argument if necessary */
  if (retval_signals_error)
    {
      g_assert (have_retval);
      g_value_array_append (value_array, NULL);
      g_value_init (g_value_array_get_nth (value_array, value_array->n_values - 1), G_TYPE_POINTER);
      g_value_set_pointer (g_value_array_get_nth (value_array, value_array->n_values - 1), &gerror);
    }
  
  /* Actually invoke method */
  method->marshaller (&closure, have_retval ? &return_value : NULL,
		      value_array->n_values,
		      value_array->values,
		      NULL, method->function);
  if (is_async)
    {
      goto done;
    }

  if (retval_signals_error)
    had_error = _dbus_gvalue_signals_error (&return_value);
  else
    had_error = FALSE;

  if (!had_error)
    {
      DBusMessageIter iter;

      /* Careful here - there are two major cases in this section of the code.
       * If send_reply is TRUE, we're constructing a dbus message and freeing
       * the return values.  If it's FALSE, then we just need to free the
       * values.
       */
      if (send_reply)
        {
          reply = reply_or_die (message);

          /* Append output arguments to reply */
          dbus_message_iter_init_append (reply, &iter);
        }

      /* First, append the return value, unless it's synthetic */
      if (have_retval && !retval_is_synthetic)
	{
          if (reply != NULL && !_dbus_gvalue_marshal (&iter, &return_value))
            {
              gchar *desc = g_strdup_value_contents (&return_value);

              g_critical ("unable to append retval of type %s for %s: %s",
                  G_VALUE_TYPE_NAME (&return_value),
                  method_name_from_object_info (object_info, method),
                  desc);
              g_free (desc);
              /* the reply is now unusable but we still need to free
               * everything */
              dbus_message_unref (reply);
              reply = NULL;
            }

	  if (!retval_is_constant)
	    g_value_unset (&return_value);
	}

      /* Grab the argument metadata and iterate over it */
      arg_metadata = method_arg_info_from_object_info (object_info, method);
      
      /* Now append any remaining return values */
      out_param_pos = 0;
      out_param_gvalue_pos = 0;
      while (*arg_metadata)
	{
	  GValue gvalue = {0, };
	  const char *arg_name;
	  gboolean arg_in;
	  gboolean constval;
	  RetvalType retval;
	  const char *arg_signature;
	  DBusSignatureIter argsigiter;

	  do
	    {
	      /* Iterate over only output values; skip over input
		 arguments and the return value */
	      arg_metadata = arg_iterate (arg_metadata, &arg_name, &arg_in, &constval, &retval, &arg_signature);
	    }
	  while ((arg_in || retval != RETVAL_NONE) && *arg_metadata);

	  /* If the last argument we saw was input or the return
  	   * value, we must be done iterating over output arguments.
	   */
	  if (arg_in || retval != RETVAL_NONE)
	    break;

	  dbus_signature_iter_init (&argsigiter, arg_signature);
	  
	  g_value_init (&gvalue, _dbus_gtype_from_signature_iter (&argsigiter, FALSE));
	  if (G_VALUE_TYPE (&gvalue) != G_TYPE_VALUE)
	    {
	      if (!_dbus_gvalue_take (&gvalue,
				     &(g_array_index (out_param_values, GTypeCValue, out_param_pos))))
		g_assert_not_reached ();
	      out_param_pos++;
	    }
	  else
	    {
	      g_value_set_static_boxed (&gvalue, out_param_gvalues->values + out_param_gvalue_pos);
	      out_param_gvalue_pos++;
	    }

          if (reply && !_dbus_gvalue_marshal (&iter, &gvalue))
            {
              gchar *desc = g_strdup_value_contents (&gvalue);

              g_critical ("unable to append OUT arg of type %s for %s: %s",
                  G_VALUE_TYPE_NAME (&gvalue),
                  method_name_from_object_info (object_info, method),
                  desc);
              g_free (desc);
              /* the reply is now unusable but we still need to free
               * everything */
              dbus_message_unref (reply);
              reply = NULL;
            }

	  /* Here we actually free the allocated value; we
	   * took ownership of it with _dbus_gvalue_take, unless
	   * an annotation has specified this value as constant.
	   */
	  if (!constval)
	    g_value_unset (&gvalue);
	}
    }
  else if (send_reply)
    reply = gerror_to_dbus_error_message (object_info, message, gerror);

  if (reply)
    {
      connection_send_or_die (connection, reply);
      dbus_message_unref (reply);
    }

done:
  g_free (in_signature);

  if (!is_async)
    {
      g_array_free (out_param_values, TRUE);
      g_value_array_free (out_param_gvalues);
    }

  if (gerror != NULL)
    g_clear_error (&gerror);

  g_value_array_free (value_array);
  return DBUS_HANDLER_RESULT_HANDLED;
}

/*
 * @wincaps_propiface: the D-Bus interface name, conventionally WindowsCaps
 * @requested_propname: the D-Bus property name, conventionally WindowsCaps
 * @uscore_propname: the GObject property name, conventionally
 *    words_with_underscores or words-with-dashes
 * @is_set: %TRUE if we're going to set the property, %FALSE if we're going
 *    to get it
 *
 * Check that the requested property exists and the requested access is
 * allowed. If not, reply with a D-Bus AccessDenied error message.
 *
 * Returns: %TRUE if property access can continue, or %FALSE if an error
 *    reply has been sent
 */
static gboolean
check_property_access (DBusConnection  *connection,
                       DBusMessage     *message,
                       GObject         *object,
                       const char      *wincaps_propiface,
                       const char      *requested_propname,
                       const char      *uscore_propname,
                       gboolean         is_set)
{
  const DBusGObjectInfo *object_info;
  const char *access_type;
  DBusMessage *ret;
  gchar *error_message;

  if (!is_set && !disable_legacy_property_access)
    return TRUE;

  object_info = lookup_object_info_by_iface (object, wincaps_propiface, TRUE, NULL);
  if (!object_info)
    {
      error_message = g_strdup_printf (
          "Interface \"%s\" isn't exported (or may not exist), can't access property \"%s\"",
          wincaps_propiface, requested_propname);

      goto error;
    }

  /* Try both forms of property names: "foo_bar" or "FooBar"; for historical
   * reasons we accept both.
   */
  if (object_info
      && !(property_info_from_object_info (object_info, wincaps_propiface, requested_propname, &access_type)
           || property_info_from_object_info (object_info, wincaps_propiface, uscore_propname, &access_type)))
    {
      error_message = g_strdup_printf (
          "Property \"%s\" of interface \"%s\" isn't exported (or may not exist)",
          requested_propname, wincaps_propiface);

      goto error;
    }

  if (strcmp (access_type, "readwrite") == 0)
    return TRUE;

  if (is_set ? strcmp (access_type, "read") == 0
             : strcmp (access_type, "write") == 0)
    {
      error_message = g_strdup_printf (
          "Property \"%s\" of interface \"%s\" is not %s",
          requested_propname,
          wincaps_propiface,
          is_set ? "settable" : "readable");

      goto error;
    }

  return TRUE;

error:
  ret = error_or_die (message, DBUS_ERROR_ACCESS_DENIED, error_message);
  g_free (error_message);

  connection_send_or_die (connection, ret);
  dbus_message_unref (ret);
  return FALSE;
}

static DBusHandlerResult
object_registration_message (DBusConnection  *connection,
                             DBusMessage     *message,
                             void            *user_data)
{
  GParamSpec *pspec;
  GObject *object;
  gboolean setter;
  gboolean getter;
  gboolean getall;
  char *s;
  const char *requested_propname;
  const char *wincaps_propiface;
  DBusMessageIter iter;
  const DBusGMethodInfo *method;
  const DBusGObjectInfo *object_info;
  DBusMessage *ret;
  ObjectRegistration *o;

  o = user_data;
  /* export is always non-NULL. If the object has been disposed, the weak-ref
   * callback removes all registrations from the DBusConnection, so this
   * should never be reached with object = NULL. */
  object = G_OBJECT (o->export->object);
  g_assert (object != NULL);

  if (dbus_message_is_method_call (message,
                                   DBUS_INTERFACE_INTROSPECTABLE,
                                   "Introspect"))
    return handle_introspect (connection, message, object);

  /* Try the metainfo, which lets us invoke methods */
  object_info = NULL;
  if (lookup_object_and_method (object, message, &object_info, &method))
    return invoke_object_method (object, object_info, method, connection, message);

  /* If no metainfo, we can still do properties and signals
   * via standard GLib introspection.  Note we do now check
   * property access against the metainfo if available.
   */
  getter = FALSE;
  setter = FALSE;
  getall = FALSE;
  if (dbus_message_is_method_call (message,
                                   DBUS_INTERFACE_PROPERTIES,
                                   "Get"))
    getter = TRUE;
  else if (dbus_message_is_method_call (message,
                                        DBUS_INTERFACE_PROPERTIES,
                                        "Set"))
    setter = TRUE;
  else if (dbus_message_is_method_call (message,
                                   DBUS_INTERFACE_PROPERTIES,
                                   "GetAll"))
    getall = TRUE;

  if (!(setter || getter || getall))
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;

  ret = NULL;

  dbus_message_iter_init (message, &iter);

  if (dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_STRING)
    {
      ret = error_or_die (message, DBUS_ERROR_INVALID_ARGS,
          "First argument to Get(), GetAll() or Set() must be an interface string");
      goto out;
    }

  dbus_message_iter_get_basic (&iter, &wincaps_propiface);
  dbus_message_iter_next (&iter);

  if (getall)
    {
      object_info = lookup_object_info_by_iface (object, wincaps_propiface, TRUE, NULL);
      if (object_info != NULL)
          ret = get_all_object_properties (connection, message, object_info, wincaps_propiface, object);
      else
          return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }
  else
    {
      g_assert (getter || setter);

      if (dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_STRING)
        {
          ret = error_or_die (message, DBUS_ERROR_INVALID_ARGS,
              "Second argument to Get() or Set() must be a property name string");
          goto out;
        }

      dbus_message_iter_get_basic (&iter, &requested_propname);
      dbus_message_iter_next (&iter);

      s = lookup_property_name (object, wincaps_propiface, requested_propname);

      if (!check_property_access (connection, message, object, wincaps_propiface, requested_propname, s, setter))
        {
          g_free (s);
          return DBUS_HANDLER_RESULT_HANDLED;
        }

      pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (object),
                                            s);

      g_free (s);

      if (pspec != NULL)
        {
          if (setter)
            {
              if (dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_VARIANT)
                {
                  ret = error_or_die (message, DBUS_ERROR_INVALID_ARGS,
                      "Third argument to Set() must be a variant");
                  goto out;
                }

              ret = set_object_property (connection, message, &iter,
                                         object, pspec);
              dbus_message_iter_next (&iter);
            }
          else
            {
              g_assert (getter);
              ret = get_object_property (connection, message,
                                         object, pspec);
            }
        }
      else
        {
          gchar *error_message = g_strdup_printf ("No such property %s",
              requested_propname);

          ret = error_or_die (message, DBUS_ERROR_INVALID_ARGS, error_message);
          g_free (error_message);
        }
    }

  g_assert (ret != NULL);

  /* FIXME: this should be returned as a D-Bus error, not spammed out
   * as a warning. This is too late to do that, though - we've already
   * had any side-effects we were going to have - and it would break
   * anything that's relying on ability to give us too many arguments. */
  if (dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_INVALID)
    g_warning ("Property get, set or set all had too many arguments\n");

out:
  connection_send_or_die (connection, ret);
  dbus_message_unref (ret);
  return DBUS_HANDLER_RESULT_HANDLED;
}

static const DBusObjectPathVTable gobject_dbus_vtable = {
  object_registration_unregistered,
  object_registration_message,
  NULL
};

typedef struct {
  GClosure         closure;
  DBusGConnection *connection;
  GObject         *object;
  const char      *signame;
  const char      *sigiface;
} DBusGSignalClosure;

static GClosure *
dbus_g_signal_closure_new (GObject         *object,
			   const char      *signame,
			   const char      *sigiface)
{
  DBusGSignalClosure *closure;
  
  closure = (DBusGSignalClosure*) g_closure_new_simple (sizeof (DBusGSignalClosure), NULL);

  closure->object = object;
  closure->signame = signame;
  closure->sigiface = sigiface;
  return (GClosure*) closure;
}

static void
emit_signal_for_registration (ObjectRegistration *o,
                              DBusGSignalClosure *sigclosure,
                              GValue             *retval,
                              guint               n_param_values,
                              const GValue       *param_values)
{
  DBusMessage *signal;
  DBusMessageIter iter;
  guint i;

  g_assert (g_variant_is_object_path (o->object_path));
  g_assert (g_dbus_is_interface_name (sigclosure->sigiface));
  g_assert (g_dbus_is_member_name (sigclosure->signame));

  signal = dbus_message_new_signal (o->object_path,
                                    sigclosure->sigiface,
                                    sigclosure->signame);
  if (!signal)
    oom (NULL);

  dbus_message_iter_init_append (signal, &iter);

  /* First argument is the object itself, and we can't marshall that */
  for (i = 1; i < n_param_values; i++)
    {
      if (!_dbus_gvalue_marshal (&iter,
                                (GValue *) (&(param_values[i]))))
        {
          g_warning ("failed to marshal parameter %d for signal %s",
                     i, sigclosure->signame);
          goto out;
        }
    }

  connection_send_or_die (DBUS_CONNECTION_FROM_G_CONNECTION (o->connection),
      signal);
out:
  dbus_message_unref (signal);
}

static void
signal_emitter_marshaller (GClosure        *closure,
			   GValue          *retval,
			   guint            n_param_values,
			   const GValue    *param_values,
			   gpointer         invocation_hint,
			   gpointer         marshal_data)
{
  DBusGSignalClosure *sigclosure;
  const ObjectExport *oe;
  const GSList *iter;

  sigclosure = (DBusGSignalClosure *) closure;

  g_assert (retval == NULL);

  oe = g_object_get_data (sigclosure->object, "dbus_glib_object_registrations");
  /* If the object has ever been exported, this should exist; it persists until
   * the object is actually freed. */
  g_assert (oe != NULL);

  for (iter = oe->registrations; iter; iter = iter->next)
    {
      ObjectRegistration *o = iter->data;

      emit_signal_for_registration (o, sigclosure, retval, n_param_values, param_values);
    }
}

static void
export_signals (const GList *info_list, GObject *object)
{
  GType gtype;
  const char *sigdata;
  const char *iface;
  const char *signame;
  const DBusGObjectInfo *info;

  gtype = G_TYPE_FROM_INSTANCE (object);

  for (; info_list != NULL; info_list = g_list_next (info_list))
    {
      info = (DBusGObjectInfo *) info_list->data;
      
      sigdata = info->exported_signals;
      
      while (*sigdata != '\0')
        {
          guint id;
          GSignalQuery query;
          GClosure *closure;
          char *s;

          sigdata = signal_iterate (sigdata, &iface, &signame);

          if (!g_dbus_is_interface_name (iface))
            {
              g_critical ("invalid interface name found in %s: %s",
                  g_type_name (gtype), iface);
              continue;
            }

          if (!g_dbus_is_member_name (signame))
            {
              g_critical ("invalid signal name found in %s: %s",
                  g_type_name (gtype), signame);
              continue;
            }

          s = _dbus_gutils_wincaps_to_uscore (signame);

          id = g_signal_lookup (s, gtype);
          if (id == 0)
            {
              g_warning ("signal \"%s\" (from \"%s\") exported but not found in object class \"%s\"",
                     s, signame, g_type_name (gtype));
              g_free (s);
              continue;
            }

          g_signal_query (id, &query);

          if (query.return_type != G_TYPE_NONE)
            {
              g_warning ("Not exporting signal \"%s\" for object class \"%s\" as it has a return type \"%s\"",
                     s, g_type_name (gtype), g_type_name (query.return_type));
              g_free (s);
              continue; /* FIXME: these could be listed as methods ? */
            }
          
          closure = dbus_g_signal_closure_new (object, signame, (char*) iface);
          g_closure_set_marshal (closure, signal_emitter_marshaller);

          g_signal_connect_closure_by_id (object,
                          id,
                          0,
                          closure,
                          FALSE);

          g_free (s);
        }
    }
}

static gint
dbus_error_to_gerror_code (const char *derr)
{
  if (0) ; 
  else if (!strcmp (derr,  DBUS_ERROR_FAILED  )) 
    return  DBUS_GERROR_FAILED ;
  else if (!strcmp (derr,  DBUS_ERROR_NO_MEMORY  )) 
    return  DBUS_GERROR_NO_MEMORY ;
  else if (!strcmp (derr,  DBUS_ERROR_SERVICE_UNKNOWN  )) 
    return  DBUS_GERROR_SERVICE_UNKNOWN ;
  else if (!strcmp (derr,  DBUS_ERROR_NAME_HAS_NO_OWNER  )) 
    return  DBUS_GERROR_NAME_HAS_NO_OWNER ;
  else if (!strcmp (derr,  DBUS_ERROR_NO_REPLY  )) 
    return  DBUS_GERROR_NO_REPLY ;
  else if (!strcmp (derr,  DBUS_ERROR_IO_ERROR  )) 
    return  DBUS_GERROR_IO_ERROR ;
  else if (!strcmp (derr,  DBUS_ERROR_BAD_ADDRESS  )) 
    return  DBUS_GERROR_BAD_ADDRESS ;
  else if (!strcmp (derr,  DBUS_ERROR_NOT_SUPPORTED  )) 
    return  DBUS_GERROR_NOT_SUPPORTED ;
  else if (!strcmp (derr,  DBUS_ERROR_LIMITS_EXCEEDED  )) 
    return  DBUS_GERROR_LIMITS_EXCEEDED ;
  else if (!strcmp (derr,  DBUS_ERROR_ACCESS_DENIED  )) 
    return  DBUS_GERROR_ACCESS_DENIED ;
  else if (!strcmp (derr,  DBUS_ERROR_AUTH_FAILED  )) 
    return  DBUS_GERROR_AUTH_FAILED ;
  else if (!strcmp (derr,  DBUS_ERROR_NO_SERVER  )) 
    return  DBUS_GERROR_NO_SERVER ;
  else if (!strcmp (derr,  DBUS_ERROR_TIMEOUT  )) 
    return  DBUS_GERROR_TIMEOUT ;
  else if (!strcmp (derr,  DBUS_ERROR_NO_NETWORK  )) 
    return  DBUS_GERROR_NO_NETWORK ;
  else if (!strcmp (derr,  DBUS_ERROR_ADDRESS_IN_USE  )) 
    return  DBUS_GERROR_ADDRESS_IN_USE ;
  else if (!strcmp (derr,  DBUS_ERROR_DISCONNECTED  )) 
    return  DBUS_GERROR_DISCONNECTED ;
  else if (!strcmp (derr,  DBUS_ERROR_INVALID_ARGS  )) 
    return  DBUS_GERROR_INVALID_ARGS ;
  else if (!strcmp (derr,  DBUS_ERROR_FILE_NOT_FOUND  )) 
    return  DBUS_GERROR_FILE_NOT_FOUND ;
  else if (!strcmp (derr,  DBUS_ERROR_FILE_EXISTS  )) 
    return  DBUS_GERROR_FILE_EXISTS ;
  else if (!strcmp (derr,  DBUS_ERROR_UNKNOWN_METHOD  )) 
    return  DBUS_GERROR_UNKNOWN_METHOD ;
  else if (!strcmp (derr,  DBUS_ERROR_TIMED_OUT  )) 
    return  DBUS_GERROR_TIMED_OUT ;
  else if (!strcmp (derr,  DBUS_ERROR_MATCH_RULE_NOT_FOUND  )) 
    return  DBUS_GERROR_MATCH_RULE_NOT_FOUND ;
  else if (!strcmp (derr,  DBUS_ERROR_MATCH_RULE_INVALID  )) 
    return  DBUS_GERROR_MATCH_RULE_INVALID ;
  else if (!strcmp (derr,  DBUS_ERROR_SPAWN_EXEC_FAILED  )) 
    return  DBUS_GERROR_SPAWN_EXEC_FAILED ;
  else if (!strcmp (derr,  DBUS_ERROR_SPAWN_FORK_FAILED  )) 
    return  DBUS_GERROR_SPAWN_FORK_FAILED ;
  else if (!strcmp (derr,  DBUS_ERROR_SPAWN_CHILD_EXITED  )) 
    return  DBUS_GERROR_SPAWN_CHILD_EXITED ;
  else if (!strcmp (derr,  DBUS_ERROR_SPAWN_CHILD_SIGNALED  )) 
    return  DBUS_GERROR_SPAWN_CHILD_SIGNALED ;
  else if (!strcmp (derr,  DBUS_ERROR_SPAWN_FAILED  )) 
    return  DBUS_GERROR_SPAWN_FAILED ;
  else if (!strcmp (derr,  DBUS_ERROR_UNIX_PROCESS_ID_UNKNOWN  )) 
    return  DBUS_GERROR_UNIX_PROCESS_ID_UNKNOWN ;
  else if (!strcmp (derr,  DBUS_ERROR_INVALID_SIGNATURE  )) 
    return  DBUS_GERROR_INVALID_SIGNATURE ;
  else if (!strcmp (derr,  DBUS_ERROR_INVALID_FILE_CONTENT  )) 
    return  DBUS_GERROR_INVALID_FILE_CONTENT ;
  else if (!strcmp (derr,  DBUS_ERROR_SELINUX_SECURITY_CONTEXT_UNKNOWN  )) 
    return  DBUS_GERROR_SELINUX_SECURITY_CONTEXT_UNKNOWN ;
  else
    return DBUS_GERROR_REMOTE_EXCEPTION;
}

/**
 * dbus_set_g_error:
 * @gerror: an error
 * @derror: a #DBusError
 *
 * Store the information from a DBus method error return into a
 * GError.  For the normal case of an arbitrary remote process,
 * the error code will be DBUS_GERROR_REMOTE_EXCEPTION.  Now,
 * DBus errors have two components; a message and a "name". 
 * The former is an arbitrary (normally American English) string.  
 * The second is a string like com.example.FooFailure which 
 * programs can use as a conditional source.  Because a GError
 * only has one string, we use a hack to encode both values:
 *
 * &lt;human readable string&gt;&lt;null&gt;&lt;error name&gt;&lt;null&gt;
 * 
 * You can use the following code to retrieve both values:
 * 
 * |[const char *msg = error->message;
 * size_t len = strlen(msg);
 * const char *error_name = msg+len+1;]|
 */
void
dbus_set_g_error (GError    **gerror,
                  DBusError  *derror)
{
  int code;

  g_return_if_fail (derror != NULL);
  g_return_if_fail (dbus_error_is_set (derror));
  g_return_if_fail (gerror == NULL || *gerror == NULL);

  code = dbus_error_to_gerror_code (derror->name);
  if (code != DBUS_GERROR_REMOTE_EXCEPTION)
    g_set_error (gerror, DBUS_GERROR,
		 code,
		 "%s",
		 derror->message);
  else
    g_set_error (gerror, DBUS_GERROR,
		 code,
		 "%s%c%s",
		 derror->message ? derror->message : "",
		 '\0',
		 derror->name);
}

static void
dbus_g_error_info_free (gpointer p)
{
  DBusGErrorInfo *info;

  info = p;

  g_free (info->default_iface);
  g_free (info);
}

/**
 * SECTION:dbus-gobject
 * @short_description: Exporting a #GObject remotely
 * @see_also: #GObject
 * @stability: Stable
 *
 * FIXME
 */

/**
 * dbus_glib_global_set_disable_legacy_property_access:
 *
 * For historical reasons, DBus-GLib will allow read-only
 * access to every GObject property of an object exported
 * to the bus, regardless of whether or not the property
 * is listed in the type info installed with
 * dbus_g_object_type_install_info().  (Write access is
 * denied however).
 *
 * If you wish to restrict even read-only access, you
 * can call this method to globally change the behavior
 * for the entire process.
 *
 * Since: 0.88
 */
void
dbus_glib_global_set_disable_legacy_property_access (void)
{
  disable_legacy_property_access = TRUE;
}

/**
 * dbus_g_object_type_install_info:
 * @object_type: #GType for the object
 * @info: introspection data generated by #dbus-glib-tool
 *
 * Install introspection information about the given object #GType
 * sufficient to allow methods on the object to be invoked by name.
 * The introspection information is normally generated by
 * dbus-glib-tool, then this function is called in the
 * class_init() for the object class.
 *
 * Once introspection information has been installed, instances of the
 * object registered with dbus_g_connection_register_g_object() can have
 * their methods invoked remotely.
 */
void
dbus_g_object_type_install_info (GType                  object_type,
				 const DBusGObjectInfo *info)
{
  g_return_if_fail (G_TYPE_IS_CLASSED (object_type) || G_TYPE_IS_INTERFACE (object_type));

  _dbus_g_value_types_init ();

  g_type_set_qdata (object_type,
		    dbus_g_object_type_dbus_metadata_quark (),
		    (gpointer) info);
}

/**
 * dbus_g_error_domain_register:
 * @domain: the #GError domain
 * @default_iface: the prefix used for error values, or %NULL
 * @code_enum: a #GType for a #GEnum of the error codes
 *
 * Register a #GError domain and set of codes with D-Bus. When an object
 * raises a #GError in the domain @domain from one of its D-Bus methods,
 * the D-Bus error name used will be @default_iface, followed by a dot,
 * followed by the #GEnumValue.value_nick corresponding to the #GError.code.
 * For D-Bus, it's conventional to use an error name (value_nick) that is
 * in CamelCase.
 *
 * (For instance, if a D-Bus method <code>com.example.MyObject.GetThings</code>
 * can raise a #GError with domain <code>MY_ERROR</code> and code
 * <code>MY_ERROR_NOT_HAPPY</code>, you could call
 * <code>dbus_g_error_domain_register (MY_ERROR, "com.example.MyError",
 * MY_TYPE_ERROR)</code>, and set up the value_nick for
 * <code>MY_ERROR_NOT_HAPPY</code> to be <code>NotHappy</code>,
 * resulting in the D-Bus error string
 * <code>com.example.MyError.NotHappy</code>.)
 *
 * If @default_iface is %NULL, the D-Bus interface of the method that failed
 * will be used.
 *
 * (For instance, if the above example had called
 * <code>dbus_g_error_domain_register (MY_ERROR, NULL, MY_TYPE_ERROR)</code>
 * instead, then the D-Bus error string would be
 * <code>com.example.MyObject.NotHappy</code>.)
 */
void
dbus_g_error_domain_register (GQuark                domain,
			      const char           *default_iface,
			      GType                 code_enum)
{
  DBusGErrorInfo *info;
  
  g_return_if_fail (g_quark_to_string (domain) != NULL);
  g_return_if_fail (code_enum != G_TYPE_INVALID);
  g_return_if_fail (G_TYPE_FUNDAMENTAL (code_enum) == G_TYPE_ENUM);

  g_static_rw_lock_writer_lock (&globals_lock);

  if (error_metadata == NULL)
    g_datalist_init (&error_metadata);

  info = g_datalist_id_get_data (&error_metadata, domain);

  if (info != NULL)
    {
      g_warning ("Metadata for error domain \"%s\" already registered\n",
		 g_quark_to_string (domain));
    }
  else
    {
      info = g_new0 (DBusGErrorInfo, 1);
      info->default_iface = g_strdup (default_iface);
      info->code_enum = code_enum;

      g_datalist_id_set_data_full (&error_metadata,
				   domain,
				   info,
				   dbus_g_error_info_free);
    }

  g_static_rw_lock_writer_unlock (&globals_lock);
}

/* Called when the object is destroyed */
static void
object_export_object_died (gpointer user_data, GObject *dead)
{
  ObjectExport *oe = user_data;

  g_assert (dead == oe->object);

  /* this prevents the weak unref from taking place, which would cause an
   * assertion failure since the object has already gone... */
  oe->object = NULL;

  /* ... while this results in a call to object_registration_unregistered
   * for each contained registration */
  object_export_unregister_all (oe);

  /* We deliberately don't remove the ObjectExport yet, in case the object is
   * resurrected and re-registered: if that happens, we wouldn't want to call
   * export_signals() again. */
}

/**
 * dbus_g_connection_unregister_g_object:
 * @connection: the D-BUS connection
 * @object: the object
 *
 * Removes @object from any object paths at which it is exported on
 * @connection. Properties, methods, and signals
 * of the object can no longer be accessed remotely.
 */
void
dbus_g_connection_unregister_g_object (DBusGConnection *connection,
                                       GObject *object)
{
  ObjectExport *oe;
  GSList *registrations;

  g_return_if_fail (connection != NULL);
  g_return_if_fail (G_IS_OBJECT (object));

  oe = g_object_get_data (object, "dbus_glib_object_registrations");

  g_return_if_fail (oe != NULL);
  g_return_if_fail (oe->registrations != NULL);

  /* Copy the list before iterating it: it will be modified in
   * object_registration_free() each time an object path is unregistered.
   */
  for (registrations = g_slist_copy (oe->registrations);
      registrations != NULL;
      registrations = g_slist_delete_link (registrations, registrations))
    {
      ObjectRegistration *o = registrations->data;

      if (o->connection != connection)
        continue;

      dbus_connection_unregister_object_path (DBUS_CONNECTION_FROM_G_CONNECTION (o->connection),
          o->object_path);
    }
}

/**
 * dbus_g_connection_register_g_object:
 * @connection: the D-BUS connection
 * @at_path: the path where the object will live (the object's name)
 * @object: the object
 *
 * Registers a #GObject at the given path. Properties, methods, and signals
 * of the object can then be accessed remotely. Methods are only available
 * if method introspection data has been added to the object's class
 * with dbus_g_object_type_install_info().
 *
 * The registration will be cancelled if either the #DBusConnection or
 * the #GObject gets finalized, or if dbus_g_connection_unregister_g_object()
 * is used.
 *
 * Note: If an object is registered multiple times, the first registration
 * takes priority for cases such as turning an object into an object path.
 */
void
dbus_g_connection_register_g_object (DBusGConnection       *connection,
                                     const char            *at_path,
                                     GObject               *object)
{
  ObjectExport *oe;
  GSList *iter;
  ObjectRegistration *o;
  DBusError error;

  g_return_if_fail (connection != NULL);
  g_return_if_fail (g_variant_is_object_path (at_path));
  g_return_if_fail (G_IS_OBJECT (object));

  oe = g_object_get_data (object, "dbus_glib_object_registrations");

  if (oe == NULL)
    {
      GList *info_list = lookup_object_info (object);

      if (info_list == NULL)
        {
          g_warning ("No introspection data registered for object class \"%s\"",
                     g_type_name (G_TYPE_FROM_INSTANCE (object)));
          return;
        }

      /* This adds a hook into every signal for the object.  Only do this
       * on the first registration, because inside the signal marshaller
       * we emit a signal for each registration.
       */
      export_signals (info_list, object);
      g_list_free (info_list);

      oe = object_export_new ();
      g_object_set_data_full (object, "dbus_glib_object_registrations", oe,
          (GDestroyNotify) object_export_free);
    }

  if (oe->object == NULL)
    {
      /* Either the ObjectExport is newly-created, or it already existed but
       * the object was disposed and resurrected, causing the weak ref to
       * fall off */
      oe->object = object;
      g_object_weak_ref (object, object_export_object_died, oe);
    }

  for (iter = oe->registrations; iter; iter = iter->next)
    {
      o = iter->data;

      /* Silently ignore duplicate registrations */
      if (strcmp (o->object_path, at_path) == 0 && o->connection == connection)
        return;
    }

  o = object_registration_new (connection, at_path, oe);

  dbus_error_init (&error);
  if (!dbus_connection_try_register_object_path (DBUS_CONNECTION_FROM_G_CONNECTION (connection),
                                                 at_path,
                                                 &gobject_dbus_vtable,
                                                 o,
                                                 &error))
    {
      g_error ("Failed to register GObject with DBusConnection: %s %s",
               error.name, error.message);
      dbus_error_free (&error);
      object_registration_free (o);
      return;
    }

  oe->registrations = g_slist_append (oe->registrations, o);
}

/**
 * dbus_g_connection_lookup_g_object:
 * @connection: a #DBusGConnection
 * @at_path: path
 *
 * FIXME 
 *
 * Returns: the object at path @at_path
 */
GObject *
dbus_g_connection_lookup_g_object (DBusGConnection       *connection,
				   const char            *at_path)
{
  gpointer p;
  ObjectRegistration *o;

  g_return_val_if_fail (connection != NULL, NULL);
  g_return_val_if_fail (g_variant_is_object_path (at_path), NULL);

  if (!dbus_connection_get_object_path_data (DBUS_CONNECTION_FROM_G_CONNECTION (connection), at_path, &p))
    return NULL;

  if (p == NULL)
    return NULL;

  o = p;

  if (o->export->object == NULL)
    return NULL;

  return G_OBJECT (o->export->object);
}

typedef struct {
  GType    rettype;
  guint    n_params;
  GType   *params;
} DBusGFuncSignature;

static guint
funcsig_hash (gconstpointer key)
{
  const DBusGFuncSignature *sig = key;
  GType *types;
  guint ret;
  guint i;

  ret = sig->rettype;
  types = sig->params;

  for (i = 0; i < sig->n_params; i++)
    {
      ret += (int) (*types);
      types++;
    }
      
  return ret;
}

static gboolean
funcsig_equal (gconstpointer aval,
	       gconstpointer bval)
{
  const DBusGFuncSignature *a = aval;
  const DBusGFuncSignature *b = bval;
  const GType *atypes;
  const GType *btypes;
  guint i;

  if (a->rettype != b->rettype
      || a->n_params != b->n_params)
    return FALSE;

  atypes = a->params;
  btypes = b->params;

  for (i = 0; i < a->n_params; i++)
    {
      if (*btypes != *atypes)
	return FALSE;
      atypes++;
      btypes++;
    }
      
  return TRUE;
}

static void
funcsig_free (DBusGFuncSignature *sig)
{
  g_free (sig->params);
  g_free (sig);
}

GClosureMarshal
_dbus_gobject_lookup_marshaller (GType        rettype,
				 guint        n_params,
				 const GType *param_types)
{
  GClosureMarshal ret;
  DBusGFuncSignature sig;
  GType *params;
  guint i;

  /* Convert to fundamental types */
  rettype = G_TYPE_FUNDAMENTAL (rettype);
  params = g_new (GType, n_params);
  for (i = 0; i < n_params; i++)
    params[i] = G_TYPE_FUNDAMENTAL (param_types[i]);

  sig.rettype = rettype;
  sig.n_params = n_params;
  sig.params = params;
  
  g_static_rw_lock_reader_lock (&globals_lock);

  if (marshal_table)
    ret = g_hash_table_lookup (marshal_table, &sig);
  else
    ret = NULL;

  g_static_rw_lock_reader_unlock (&globals_lock);

  if (ret == NULL)
    {
      if (rettype == G_TYPE_NONE)
	{
	  if (n_params == 0)
	    ret = g_cclosure_marshal_VOID__VOID;
	  else if (n_params == 1)
	    {
	      switch (params[0])
		{
		case G_TYPE_BOOLEAN:
		  ret = g_cclosure_marshal_VOID__BOOLEAN;
		  break;
		case G_TYPE_UCHAR:
		  ret = g_cclosure_marshal_VOID__UCHAR;
		  break;
		case G_TYPE_INT:
		  ret = g_cclosure_marshal_VOID__INT;
		  break;
		case G_TYPE_UINT:
		  ret = g_cclosure_marshal_VOID__UINT;
		  break;
		case G_TYPE_DOUBLE:
		  ret = g_cclosure_marshal_VOID__DOUBLE;
		  break;
		case G_TYPE_STRING:
		  ret = g_cclosure_marshal_VOID__STRING;
		  break;
		case G_TYPE_BOXED:
		  ret = g_cclosure_marshal_VOID__BOXED;
		  break;
		}
	    }
	  else if (n_params == 3
		   && params[0] == G_TYPE_STRING
		   && params[1] == G_TYPE_STRING
		   && params[2] == G_TYPE_STRING)
	    {
	      ret = _dbus_g_marshal_NONE__STRING_STRING_STRING;
	    }
	}
    }

  g_free (params);
  return ret;
}

/**
 * dbus_g_object_register_marshaller:
 * @marshaller: a GClosureMarshal to be used for invocation
 * @rettype: a GType for the return type of the function
 * @...: The parameter #GTypes, followed by %G_TYPE_INVALID
 *
 * Register a #GClosureMarshal to be used for signal invocations,
 * giving its return type and a list of parameter types,
 * followed by %G_TYPE_INVALID.
 *
 * This function will not be needed once GLib includes libffi.
 */
void
dbus_g_object_register_marshaller (GClosureMarshal  marshaller,
				   GType            rettype,
				   ...)
{
  va_list args;
  GArray *types;
  GType gtype;

  va_start (args, rettype);

  types = g_array_new (TRUE, TRUE, sizeof (GType));

  while ((gtype = va_arg (args, GType)) != G_TYPE_INVALID)
    g_array_append_val (types, gtype);

  dbus_g_object_register_marshaller_array (marshaller, rettype,
					   types->len, (GType*) types->data);

  g_array_free (types, TRUE);
  va_end (args);
}

/**
 * dbus_g_object_register_marshaller_array:
 * @marshaller: a #GClosureMarshal to be used for invocation
 * @rettype: a #GType for the return type of the function
 * @n_types: number of function parameters
 * @types: a C array of GTypes values
 *
 * Register a #GClosureMarshal to be used for signal invocations.
 * @see_also dbus_g_object_register_marshaller()
 */
void
dbus_g_object_register_marshaller_array (GClosureMarshal  marshaller,
					 GType            rettype,
					 guint            n_types,
					 const GType*     types)
{
  DBusGFuncSignature *sig;
  guint i;

  g_static_rw_lock_writer_lock (&globals_lock);

  if (marshal_table == NULL)
    marshal_table = g_hash_table_new_full (funcsig_hash,
					   funcsig_equal,
					   (GDestroyNotify) funcsig_free,
					   NULL);
  sig = g_new0 (DBusGFuncSignature, 1);
  sig->rettype = G_TYPE_FUNDAMENTAL (rettype);
  sig->n_params = n_types;
  sig->params = g_new (GType, n_types);
  for (i = 0; i < n_types; i++)
    sig->params[i] = G_TYPE_FUNDAMENTAL (types[i]);

  g_hash_table_insert (marshal_table, sig, marshaller);

  g_static_rw_lock_writer_unlock (&globals_lock);
}

/**
 * dbus_g_method_get_sender:
 * @context: the method context
 *
 * Get the sender of a message so we can send a
 * "reply" later (i.e. send a message directly
 * to a service which invoked the method at a
 * later time).
 *
 * Returns: the unique name of the sender. It
 * is up to the caller to free the returned string.
 */
gchar *
dbus_g_method_get_sender (DBusGMethodInvocation *context)
{
  const gchar *sender;

  g_return_val_if_fail (context != NULL, NULL);

  sender = dbus_message_get_sender (dbus_g_message_get_message (context->message));
  return g_strdup (sender);
}

/**
 * dbus_g_method_get_reply:
 * @context: the method context
 *
 * Get the reply message to append reply values
 * Used as a sidedoor when you can't generate dbus values
 * of the correct type due to glib binding limitations
 *
 * Returns: a #DBusMessage with the reply
 */
DBusMessage *
dbus_g_method_get_reply (DBusGMethodInvocation *context)
{
  g_return_val_if_fail (context != NULL, NULL);

  return reply_or_die (dbus_g_message_get_message (context->message));
}

/**
 * dbus_g_method_send_reply:
 * @context: the method context
 * @reply: the reply message, will be unreffed
 *
 * Send a manually created reply message.
 *
 * Used as a sidedoor when you can't generate dbus values
 * of the correct type due to glib binding limitations
 */
void
dbus_g_method_send_reply (DBusGMethodInvocation *context, DBusMessage *reply)
{
  g_return_if_fail (context != NULL);
  g_return_if_fail (reply != NULL);

  connection_send_or_die (dbus_g_connection_get_connection (context->connection),
      reply);
  dbus_message_unref (reply);

  dbus_g_connection_unref (context->connection);
  dbus_g_message_unref (context->message);
  g_free (context);
}


/**
 * dbus_g_method_return:
 * @context: the method context
 * @...: zero or more values to return from the method, with their number
 *    and types given by its #DBusGObjectInfo
 *
 * Send a return message for a given method invocation, with arguments.
 * This function also frees the sending context.
 */
void
dbus_g_method_return (DBusGMethodInvocation *context, ...)
{
  DBusMessage *reply;
  DBusMessageIter iter;
  va_list args;
  char *out_sig;
  GArray *argsig;
  guint i;

  g_return_if_fail (context != NULL);

  /* This field was initialized inside invoke_object_method; we
   * carry it over through the async invocation to here.
   */
  if (!context->send_reply)
    goto out;

  reply = dbus_g_method_get_reply (context);
  out_sig = method_output_signature_from_object_info (context->object, context->method);
  argsig = _dbus_gtypes_from_arg_signature (out_sig, FALSE);

  dbus_message_iter_init_append (reply, &iter);

  va_start (args, context);
  for (i = 0; i < argsig->len; i++)
    {
      GValue value = {0,};
      char *error;
      g_value_init (&value, g_array_index (argsig, GType, i));
      error = NULL;
      G_VALUE_COLLECT (&value, args, G_VALUE_NOCOPY_CONTENTS, &error);
      if (error)
	{
	  g_warning("%s", error);
	  g_free (error);
	}
      else
        {
          if (!_dbus_gvalue_marshal (&iter, &value))
            g_warning ("failed to marshal parameter %d for method %s",
                       i, dbus_message_get_member (
                         dbus_g_message_get_message (context->message)));
        }
    }
  va_end (args);

  connection_send_or_die (dbus_g_connection_get_connection (context->connection),
      reply);
  dbus_message_unref (reply);

  g_free (out_sig);
  g_array_free (argsig, TRUE);

out:
  dbus_g_connection_unref (context->connection);
  dbus_g_message_unref (context->message);
  g_free (context);
}

/**
 * dbus_g_method_return_error:
 * @context: the method context
 * @error: the error to send
 *
 * Send a error message for a given method invocation.
 * This function also frees the sending context.
 */
void
dbus_g_method_return_error (DBusGMethodInvocation *context, const GError *error)
{
  DBusMessage *reply;

  g_return_if_fail (context != NULL);
  g_return_if_fail (error != NULL);

  /* See comment in dbus_g_method_return */
  if (!context->send_reply)
    goto out;

  reply = gerror_to_dbus_error_message (context->object, dbus_g_message_get_message (context->message), error);
  connection_send_or_die (
      dbus_g_connection_get_connection (context->connection), reply);
  dbus_message_unref (reply);

out:
  dbus_g_connection_unref (context->connection);
  dbus_g_message_unref (context->message);
  g_free (context);
}

const char *
_dbus_gobject_get_path (GObject *obj)
{
  ObjectExport *oe;
  ObjectRegistration *o;

  oe = g_object_get_data (obj, "dbus_glib_object_registrations");

  if (oe == NULL || oe->registrations == NULL)
    return NULL;

  /* First one to have been registered wins */
  o = oe->registrations->data;

  return o->object_path;
}

#ifdef DBUS_BUILD_TESTS
#include <stdlib.h>

static void
_dummy_function (void)
{
}

/* Data structures copied from one generated by current dbus-binding-tool;
 * we need to support this layout forever
 */
static const DBusGMethodInfo dbus_glib_internal_test_methods[] = {
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 0 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 49 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 117 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 191 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 270 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 320 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 391 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 495 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 623 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 693 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 765 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 838 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 911 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 988 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1064 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1140 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1204 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1278 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1347 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1408 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1460 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1533 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1588 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1647 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1730 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1784 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1833 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1895 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1947 },
  { (GCallback) _dummy_function, g_cclosure_marshal_VOID__VOID, 1999 },
};

const DBusGObjectInfo dbus_glib_internal_test_object_info = {
  0,
  dbus_glib_internal_test_methods,
  30,
"org.freedesktop.DBus.Tests.MyObject\0DoNothing\0S\0\0org.freedesktop.DBus.Tests.MyObject\0Increment\0S\0x\0I\0u\0arg1\0O\0F\0N\0u\0\0org.freedesktop.DBus.Tests.MyObject\0IncrementRetval\0S\0x\0I\0u\0arg1\0O\0F\0R\0u\0\0org.freedesktop.DBus.Tests.MyObject\0IncrementRetvalError\0S\0x\0I\0u\0arg1\0O\0F\0E\0u\0\0org.freedesktop.DBus.Tests.MyObject\0ThrowError\0S\0\0org.freedesktop.DBus.Tests.MyObject\0Uppercase\0S\0arg0\0I\0s\0arg1\0O\0F\0N\0s\0\0org.freedesktop.DBus.Tests.MyObject\0ManyArgs\0S\0x\0I\0u\0str\0I\0s\0trouble\0I\0d\0d_ret\0O\0F\0N\0d\0str_ret\0O\0F\0N\0s\0\0org.freedesktop.DBus.Tests.MyObject\0ManyReturn\0S\0arg0\0O\0F\0N\0u\0arg1\0O\0F\0N\0s\0arg2\0O\0F\0N\0i\0arg3\0O\0F\0N\0u\0arg4\0O\0F\0N\0u\0arg5\0O\0C\0N\0s\0\0org.freedesktop.DBus.Tests.MyObject\0Stringify\0S\0val\0I\0v\0arg1\0O\0F\0N\0s\0\0org.freedesktop.DBus.Tests.MyObject\0Unstringify\0S\0val\0I\0s\0arg1\0O\0F\0N\0v\0\0org.freedesktop.DBus.Tests.MyObject\0Recursive1\0S\0arg0\0I\0au\0arg1\0O\0F\0N\0u\0\0org.freedesktop.DBus.Tests.MyObject\0Recursive2\0S\0arg0\0I\0u\0arg1\0O\0F\0N\0au\0\0org.freedesktop.DBus.Tests.MyObject\0ManyUppercase\0S\0arg0\0I\0as\0arg1\0O\0F\0N\0as\0\0org.freedesktop.DBus.Tests.MyObject\0StrHashLen\0S\0arg0\0I\0a{ss}\0arg1\0O\0F\0N\0u\0\0org.freedesktop.DBus.Tests.MyObject\0SendCar\0S\0arg0\0I\0(suv)\0arg1\0O\0F\0N\0(uo)\0\0org.freedesktop.DBus.Tests.MyObject\0GetHash\0S\0arg0\0O\0F\0N\0a{ss}\0\0org.freedesktop.DBus.Tests.MyObject\0RecArrays\0S\0val\0I\0aas\0arg1\0O\0F\0N\0aau\0\0org.freedesktop.DBus.Tests.MyObject\0Objpath\0S\0arg0\0I\0o\0arg1\0O\0C\0N\0o\0\0org.freedesktop.DBus.Tests.MyObject\0GetObjs\0S\0arg0\0O\0F\0N\0ao\0\0org.freedesktop.DBus.Tests.MyObject\0IncrementVal\0S\0\0org.freedesktop.DBus.Tests.MyObject\0AsyncIncrement\0A\0x\0I\0u\0arg1\0O\0F\0N\0u\0\0org.freedesktop.DBus.Tests.MyObject\0AsyncThrowError\0A\0\0org.freedesktop.DBus.Tests.MyObject\0GetVal\0S\0arg0\0O\0F\0N\0u\0\0org.freedesktop.DBus.Tests.MyObject\0ManyStringify\0S\0arg0\0I\0a{sv}\0arg1\0O\0F\0N\0a{sv}\0\0org.freedesktop.DBus.Tests.MyObject\0EmitFrobnicate\0S\0\0org.freedesktop.DBus.Tests.MyObject\0Terminate\0S\0\0org.freedesktop.DBus.Tests.FooObject\0GetValue\0S\0arg0\0O\0F\0N\0u\0\0org.freedesktop.DBus.Tests.FooObject\0EmitSignals\0S\0\0org.freedesktop.DBus.Tests.FooObject\0EmitSignal2\0S\0\0org.freedesktop.DBus.Tests.FooObject\0Terminate\0S\0\0\0",
"org.freedesktop.DBus.Tests.MyObject\0Frobnicate\0org.freedesktop.DBus.Tests.FooObject\0Sig0\0org.freedesktop.DBus.Tests.FooObject\0Sig1\0org.freedesktop.DBus.Tests.FooObject\0Sig2\0\0",
"\0"
};


/*
 * Unit test for GLib GObject integration ("skeletons")
 * Returns: %TRUE on success.
 */
gboolean
_dbus_gobject_test (const char *test_data_dir)
{
  int i;
  const char *arg;
  const char *arg_name;
  gboolean arg_in;
  gboolean constval;
  RetvalType retval;
  const char *arg_signature;
  const char *sigdata;
  const char *iface;
  const char *signame;
  
  static struct { const char *wincaps; const char *uscore; } name_pairs[] = {
    { "SetFoo", "set_foo" },
    { "Foo", "foo" },
    { "GetFooBar", "get_foo_bar" },
    { "Hello", "hello" }
    
    /* Impossible-to-handle cases */
    /* { "FrobateUIHandler", "frobate_ui_handler" } */
  };

  /* Test lookup in our hardcoded object info; if these tests fail
   * then it likely means you changed the generated object info in an
   * incompatible way and broke the lookup functions.  In that case
   * you need to bump the version and use a new structure instead. */
  /* DoNothing */
  arg = method_arg_info_from_object_info (&dbus_glib_internal_test_object_info,
					  &(dbus_glib_internal_test_methods[0]));
  g_assert (*arg == '\0');

  /* Increment */
  arg = method_arg_info_from_object_info (&dbus_glib_internal_test_object_info,
					  &(dbus_glib_internal_test_methods[1]));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (!strcmp (arg_name, "x"));
  g_assert (arg_in == TRUE);
  g_assert (!strcmp (arg_signature, "u"));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (arg_in == FALSE);
  g_assert (retval == RETVAL_NONE);
  g_assert (!strcmp (arg_signature, "u"));
  g_assert (*arg == '\0');

  /* IncrementRetval */
  arg = method_arg_info_from_object_info (&dbus_glib_internal_test_object_info,
					  &(dbus_glib_internal_test_methods[2]));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (!strcmp (arg_name, "x"));
  g_assert (arg_in == TRUE);
  g_assert (!strcmp (arg_signature, "u"));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (retval == RETVAL_NOERROR);
  g_assert (arg_in == FALSE);
  g_assert (!strcmp (arg_signature, "u"));
  g_assert (*arg == '\0');

  /* IncrementRetvalError */
  arg = method_arg_info_from_object_info (&dbus_glib_internal_test_object_info,
					  &(dbus_glib_internal_test_methods[3]));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (!strcmp (arg_name, "x"));
  g_assert (arg_in == TRUE);
  g_assert (!strcmp (arg_signature, "u"));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (retval == RETVAL_ERROR);
  g_assert (arg_in == FALSE);
  g_assert (!strcmp (arg_signature, "u"));
  g_assert (*arg == '\0');
  
  /* Stringify */
  arg = method_arg_info_from_object_info (&dbus_glib_internal_test_object_info,
					  &(dbus_glib_internal_test_methods[8]));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (!strcmp (arg_name, "val"));
  g_assert (arg_in == TRUE);
  g_assert (!strcmp (arg_signature, "v"));
  g_assert (*arg != '\0');
  arg = arg_iterate (arg, &arg_name, &arg_in, &constval, &retval, &arg_signature);
  g_assert (retval == RETVAL_NONE);
  g_assert (arg_in == FALSE);
  g_assert (!strcmp (arg_signature, "s"));
  g_assert (*arg == '\0');

  sigdata = dbus_glib_internal_test_object_info.exported_signals;
  g_assert (*sigdata != '\0');
  sigdata = signal_iterate (sigdata, &iface, &signame);
  g_assert (!strcmp (iface, "org.freedesktop.DBus.Tests.MyObject"));
  g_assert (!strcmp (signame, "Frobnicate"));
  g_assert (*sigdata != '\0');
  sigdata = signal_iterate (sigdata, &iface, &signame);
  g_assert (!strcmp (iface, "org.freedesktop.DBus.Tests.FooObject"));
  g_assert (!strcmp (signame, "Sig0"));
  g_assert (*sigdata != '\0');
  sigdata = signal_iterate (sigdata, &iface, &signame);
  g_assert (!strcmp (iface, "org.freedesktop.DBus.Tests.FooObject"));
  g_assert (!strcmp (signame, "Sig1"));
  g_assert (*sigdata != '\0');
  sigdata = signal_iterate (sigdata, &iface, &signame);
  g_assert (!strcmp (iface, "org.freedesktop.DBus.Tests.FooObject"));
  g_assert (!strcmp (signame, "Sig2"));
  g_assert (*sigdata == '\0');


  i = 0;
  while (i < (int) G_N_ELEMENTS (name_pairs))
    {
      char *uscore;
      char *wincaps;

      uscore = _dbus_gutils_wincaps_to_uscore (name_pairs[i].wincaps);
      wincaps = uscore_to_wincaps (name_pairs[i].uscore);

      if (strcmp (uscore, name_pairs[i].uscore) != 0)
        {
          g_printerr ("\"%s\" should have been converted to \"%s\" not \"%s\"\n",
                      name_pairs[i].wincaps, name_pairs[i].uscore,
                      uscore);
          exit (1);
        }
      
      if (strcmp (wincaps, name_pairs[i].wincaps) != 0)
        {
          g_printerr ("\"%s\" should have been converted to \"%s\" not \"%s\"\n",
                      name_pairs[i].uscore, name_pairs[i].wincaps,
                      wincaps);
          exit (1);
        }
      
      g_free (uscore);
      g_free (wincaps);

      ++i;
    }
  
  return TRUE;
}

#endif /* DBUS_BUILD_TESTS */
