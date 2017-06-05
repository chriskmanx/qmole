/* -*- mode: C; c-file-style: "gnu"; indent-tabs-mode: nil; -*- */
/* dbus-bash-completion-helper.c  Bash Completion helper routines
 *
 * Copyright (C) 2008 David Zeuthen <davidz@redhat.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 */

#include <config.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dbus/dbus.h>
#include <glib.h>
#include "dbus-gparser.h"

static void
print_services (DBusConnection *connection)
{
  DBusMessage *message;
  DBusMessage *reply;
  DBusError error;
  DBusMessageIter iter;
  DBusMessageIter iter_array;
  const char *name;

  /* list both active and activatable names (the shell will sort and
   * uniquify them) - also avoid names that are not well-known
   * (e.g. :1.42).
   */

  message = dbus_message_new_method_call (DBUS_SERVICE_DBUS,
                                          DBUS_PATH_DBUS,
                                          DBUS_INTERFACE_DBUS,
                                          "ListNames");
  dbus_error_init (&error);
  reply = dbus_connection_send_with_reply_and_block (connection,
                                                     message,
                                                     -1,
                                                     &error);
  dbus_message_unref (message);
  dbus_message_iter_init (reply, &iter);
  dbus_message_iter_recurse (&iter, &iter_array);
  while (dbus_message_iter_get_arg_type (&iter_array) != DBUS_TYPE_INVALID)
    {
      dbus_message_iter_get_basic (&iter_array, &name);
      if (name[0] != ':')
        printf ("%s \n", name);
      dbus_message_iter_next (&iter_array);
    }
  dbus_message_unref (reply);

  message = dbus_message_new_method_call (DBUS_SERVICE_DBUS,
                                          DBUS_PATH_DBUS,
                                          DBUS_INTERFACE_DBUS,
                                          "ListActivatableNames");
  dbus_error_init (&error);
  reply = dbus_connection_send_with_reply_and_block (connection,
                                                     message,
                                                     -1,
                                                     &error);
  dbus_message_unref (message);
  dbus_message_iter_init (reply, &iter);
  dbus_message_iter_recurse (&iter, &iter_array);
  while (dbus_message_iter_get_arg_type (&iter_array) != DBUS_TYPE_INVALID)
    {
      dbus_message_iter_get_basic (&iter_array, &name);
      printf ("%s \n", name);
      dbus_message_iter_next (&iter_array);
    }
  dbus_message_unref (reply);
}

static gboolean
have_option (char **tokens, const char *option)
{
  int n;
  for (n = 0; tokens[n] != NULL; n++)
    if (strcmp (tokens[n], option) == 0)
      return TRUE;
  return FALSE;
}

static gboolean
have_option_with_value (char **tokens, const char *option, const char **value)
{
  int n;
  for (n = 0; tokens[n] != NULL; n++)
    {
      if (g_str_has_prefix (tokens[n], option))
        {
          if (strlen (tokens[n]) > strlen (option))
            *value = tokens[n] + strlen (option);
          return TRUE;
        }
    }
  return FALSE;
}

static void
print_objects (DBusConnection *connection, const char *service_name, const char *cur)
{
  DBusMessage *message;
  DBusMessage *reply;
  DBusError error;
  DBusMessageIter iter;
  const char *introspection_xml;
  NodeInfo *root;
  GSList *nodes;
  GSList *l;

  if (cur == NULL)
    cur = "/";

  message = dbus_message_new_method_call (service_name,
                                          cur,
                                          DBUS_INTERFACE_INTROSPECTABLE,
                                          "Introspect");
  dbus_error_init (&error);
  reply = dbus_connection_send_with_reply_and_block (connection,
                                                     message,
                                                     -1,
                                                     &error);
  dbus_message_unref (message);
  dbus_message_iter_init (reply, &iter);
  dbus_message_iter_get_basic (&iter, &introspection_xml);

  root = description_load_from_string (introspection_xml, strlen (introspection_xml), NULL);
  nodes = node_info_get_nodes (root);

  if (g_slist_length (node_info_get_interfaces (root)) > 0)
    printf ("%s \n", cur);

  for (l = nodes; l != NULL; l = l->next)
    {
      NodeInfo *node = (NodeInfo *) l->data;
      const char *name;
      char *new_path;

      name = node_info_get_name (node);
      if (strcmp (cur, "/") == 0)
        new_path = g_strdup_printf ("/%s", name);
      else
        new_path = g_strdup_printf ("%s/%s", cur, name);

      print_objects (connection, service_name, new_path);

      g_free (new_path);
    }
  node_info_unref (root);

  dbus_message_unref (reply);
}

static gboolean
is_object_path_with_interfaces (DBusConnection *connection, const char *service_name, const char *object_path)
{
  DBusMessage *message;
  DBusMessage *reply;
  DBusError error;
  DBusMessageIter iter;
  const char *introspection_xml;
  NodeInfo *root;
  gboolean ret;

  ret = FALSE;

  message = dbus_message_new_method_call (service_name,
                                          object_path,
                                          DBUS_INTERFACE_INTROSPECTABLE,
                                          "Introspect");
  dbus_error_init (&error);
  reply = dbus_connection_send_with_reply_and_block (connection,
                                                     message,
                                                     -1,
                                                     &error);
  dbus_message_unref (message);
  dbus_message_iter_init (reply, &iter);
  dbus_message_iter_get_basic (&iter, &introspection_xml);

  root = description_load_from_string (introspection_xml, strlen (introspection_xml), NULL);

  if (g_slist_length (node_info_get_interfaces (root)) > 0)
    ret = TRUE;

  node_info_unref (root);
  dbus_message_unref (reply);

  return ret;
}

static void
print_methods (DBusConnection *connection, const char *service_name, const char *object_path)
{
  DBusMessage *message;
  DBusMessage *reply;
  DBusError error;
  DBusMessageIter iter;
  const char *introspection_xml;
  NodeInfo *root;
  GSList *interfaces;
  GSList *l;

  message = dbus_message_new_method_call (service_name,
                                          object_path,
                                          DBUS_INTERFACE_INTROSPECTABLE,
                                          "Introspect");
  dbus_error_init (&error);
  reply = dbus_connection_send_with_reply_and_block (connection,
                                                     message,
                                                     -1,
                                                     &error);
  dbus_message_unref (message);
  dbus_message_iter_init (reply, &iter);
  dbus_message_iter_get_basic (&iter, &introspection_xml);

  root = description_load_from_string (introspection_xml, strlen (introspection_xml), NULL);
  interfaces = node_info_get_interfaces (root);
  for (l = interfaces; l != NULL; l = l->next)
    {
      InterfaceInfo *interface = (InterfaceInfo *) l->data;
      GSList *methods;
      GSList *ll;
      methods = interface_info_get_methods (interface);
      for (ll = methods; ll != NULL; ll = ll->next)
        {
          MethodInfo *method = (MethodInfo *) ll->data;
          printf ("%s.%s \n", interface_info_get_name (interface), method_info_get_name (method));
        }
    }
  node_info_unref (root);
  dbus_message_unref (reply);
}

static void
print_signature (DBusConnection *connection, const char *service_name, const char *object_path, const char *method)
{
  DBusMessage *message;
  DBusMessage *reply;
  DBusError error;
  DBusMessageIter iter;
  const char *introspection_xml;
  NodeInfo *root;
  GSList *interfaces;
  GSList *l;
  char *s;
  char *method_name;
  char *interface_name;
  int n;

  method_name = NULL;
  interface_name = NULL;

  s = strrchr (method, '.');
  if (s == NULL || strlen (s) < 2 || s - method < 1)
    goto fail;
  method_name = g_strdup (s + 1);
  interface_name = g_strndup (method, s - method);
  printf (" \n");

  message = dbus_message_new_method_call (service_name,
                                          object_path,
                                          DBUS_INTERFACE_INTROSPECTABLE,
                                          "Introspect");
  dbus_error_init (&error);
  reply = dbus_connection_send_with_reply_and_block (connection,
                                                     message,
                                                     -1,
                                                     &error);
  dbus_message_unref (message);
  dbus_message_iter_init (reply, &iter);
  dbus_message_iter_get_basic (&iter, &introspection_xml);

  root = description_load_from_string (introspection_xml, strlen (introspection_xml), NULL);
  interfaces = node_info_get_interfaces (root);
  for (l = interfaces; l != NULL; l = l->next)
    {
      InterfaceInfo *interface = (InterfaceInfo *) l->data;

      if (strcmp (interface_name, interface_info_get_name (interface)) == 0)
        {
          GSList *methods;
          GSList *ll;
          methods = interface_info_get_methods (interface);
          for (ll = methods; ll != NULL; ll = ll->next)
            {
              MethodInfo *method = (MethodInfo *) ll->data;
              if (strcmp (method_name, method_info_get_name (method)) == 0)
                {
                  GSList *args;
                  GSList *lll;
                  args = method_info_get_args (method);
                  for (lll = args, n = 0; lll != NULL; lll = lll->next, n++)
                    {
                      ArgInfo *arg = (ArgInfo *) lll->data;
                      printf ("#    %s: arg %d: %s (%s)         \n",
                              arg_info_get_direction (arg) == ARG_IN ? " IN" : "OUT",
                              n,
                              arg_info_get_name (arg),
                              arg_info_get_type (arg));
                    }
                  break;
                }
            }
        }
    }
  node_info_unref (root);
  dbus_message_unref (reply);
 fail:
  g_free (method_name);
  g_free (interface_name);
}


static int
complete_dbus_send (char *str)
{
  int ret;
  char **tokens;
  int num_tokens;
  const char *cur;
  gboolean have_system;
  gboolean have_session;
  gboolean have_print_reply;
  gboolean have_dest;
  DBusConnection *connection;
  DBusBusType bus_type;
  DBusError error;
  const char *target_service;
  const char *object_path;
  const char *method;
  int n;
  int object_path_index;

  ret = 1;
  connection = NULL;
  target_service = NULL;

  tokens = g_strsplit (str, " ", 0);
  num_tokens = g_strv_length (tokens);
  if (num_tokens >= 1) {
    cur = tokens[num_tokens - 1];
  } else {
    cur = "";
  }

  have_system = have_option (tokens, "--system");
  have_session = have_option (tokens, "--session");
  have_print_reply = have_option (tokens, "--print-reply");
  have_dest = have_option_with_value (tokens, "--dest=", &target_service);

  if (!have_print_reply)
    printf ("--print-reply \n");

  if (!have_system && !have_session)
    {
      printf ("--system \n");
      printf ("--session \n");
      goto done;
    }

  if (!have_dest && !g_str_has_prefix (cur, "--dest="))
    {
      printf ("--dest=\n");
      goto done;
    }

  if (have_system || have_session)
    {
      bus_type = have_system ? DBUS_BUS_SYSTEM : DBUS_BUS_SESSION;

      dbus_error_init (&error);
      connection = dbus_bus_get (bus_type, &error);
      if (connection == NULL)
        {
          fprintf (stderr, "Failed to open connection to %s message bus: %s: %s\n",
                   (bus_type == DBUS_BUS_SYSTEM) ? "system" : "session",
                   error.name, error.message);
          dbus_error_free (&error);
          goto fail;
        }
    }

  if (connection != NULL && g_str_has_prefix (cur, "--dest="))
    {
      print_services (connection);
      goto done;
    }

  /* see if we have an object path */
  object_path = NULL;
  object_path_index = 0;
  if (connection != NULL && target_service != NULL)
    {
      for (n = 0; tokens[n] != NULL; n++)
        {
          if (tokens[n] == cur)
            continue;

          if (*(tokens[n]) == '/')
            {
              if (is_object_path_with_interfaces (connection, target_service, tokens[n]))
                {
                  object_path = tokens[n];
                  object_path_index = n;
                }
            }
        }
    }

  /* if we have a connection and a destination but no object path, go ahead and list the object paths */
  if (connection != NULL && target_service != NULL && object_path == NULL)
    {
      print_objects (connection, target_service, NULL);
      goto done;
    }

  /* see if we have a method; it's directly after the object_path */
  method = NULL;
  if (connection != NULL && target_service != NULL && object_path != NULL)
    {
      if ((object_path_index + 1 < num_tokens - 1) &&
          (strlen (tokens[object_path_index + 1]) > 0) &&
          !(strcmp (cur, tokens[object_path_index + 1]) == 0))
        method = tokens[object_path_index + 1];
    }

  /* if we have connection, destination and object path but no method yet, list the methods */
  if (connection != NULL && target_service != NULL && object_path != NULL && method == NULL)
    {
      print_methods (connection, target_service, object_path);
      goto done;
    }

  /* print signature as comment */
  if (connection != NULL && target_service != NULL && object_path != NULL && method != NULL)
    {
      print_signature (connection, target_service, object_path, method);
    }

 done:
  ret = 0;

 fail:

  g_strfreev (tokens);

  if (connection != NULL)
    dbus_connection_unref (connection);
  return ret;
}

int
main (int argc, char *argv[])
{
  int ret;
  char *cur;
  gboolean dbus_send;

  ret = 1;
  dbus_send = FALSE;

  if (argc != 3)
    {
      fprintf (stderr, "invalid use\n");
      goto out;
    }

  if (strcmp (argv[1], "dbus-send") == 0)
    {
      dbus_send = TRUE;
    }
  else
    {
      fprintf (stderr, "unknown program '%s'\n", argv[1]);
      goto out;
    }

  if (strlen (argv[2]) < strlen (argv[1]) + 1)
    {
      fprintf (stderr, "error");
      goto out;
    }

  cur = argv[2] + strlen (argv[1]) + 1;

  if (dbus_send)
    ret = complete_dbus_send (cur);

 out:
  return ret;
}
