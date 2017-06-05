#include <config.h>

/* -*- mode: C; c-file-style: "gnu" -*- */
#include <dbus/dbus-glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <glib-object.h>

static gboolean
make_recursive_stringify_call (int recursion_depth, 
                               DBusGProxy *proxy, 
                               GError **error)
{
  char *out_str;

  int i;
  GValue *vals = g_new0 (GValue, recursion_depth+1);

  for (i = recursion_depth-1; i >= 0; i--) 
    {
      GValue *curval = &(vals[i]);
      g_value_init (curval, G_TYPE_VALUE);
    }
  for (i = 0; i < recursion_depth; i++) 
    {
      GValue *curval = &(vals[i]);
      GValue *nextval = &(vals[i+1]);
      g_value_take_boxed (curval, nextval);
    }
  g_value_init (&(vals[recursion_depth]), G_TYPE_STRING);
  g_value_set_string (&(vals[recursion_depth]), "end of the line");
  return dbus_g_proxy_call (proxy, "Stringify", error,
                                G_TYPE_VALUE, &(vals[0]),
                                G_TYPE_INVALID,
                                G_TYPE_STRING, &out_str,
                            G_TYPE_INVALID);
}

int
main (int argc, char **argv)
{
  DBusGConnection *connection;
  GError *error = NULL;
  DBusGProxy *proxy;
  GMainLoop *loop;
    
  g_type_init ();

  g_log_set_always_fatal (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL);
  
  loop = g_main_loop_new (NULL, FALSE);

  connection = dbus_g_bus_get (DBUS_BUS_SESSION, &error);
  if (connection == NULL)
    g_error ("Failed to open connection to bus: %s", error->message);

  proxy = dbus_g_proxy_new_for_name (connection,
                                     "org.freedesktop.DBus.GLib.TestService",
                                     "/org/freedesktop/DBus/GLib/Tests/MyTestObject",
                                     "org.freedesktop.DBus.GLib.Tests.MyObject");
  
  if (proxy == NULL)
    g_error ("Failed to create proxy for name owner: %s", error->message);
  
  /* Do an echo to be sure it started */
  if (!dbus_g_proxy_call (proxy, "DoNothing", &error,
			  G_TYPE_INVALID,
			  G_TYPE_INVALID))
    g_error ("Failed to complete DoNothing call: %s", error->message);

  /* Fewer than the current internal limit (16) */
  if (make_recursive_stringify_call (10, proxy, &error))
    g_error ("Unexpected success code from 10 recursive variant call: %s", error->message);
  if (error->code != DBUS_GERROR_REMOTE_EXCEPTION)
    g_error ("Error code was not remote exception: %s", error->message);
  g_printerr ("Got expected error %d: \"%s\" from recursive variant call\n", error->code, error->message);
  g_clear_error (&error);
  /* More than the current internal limit (16) */
  if (make_recursive_stringify_call (50, proxy, &error))
    g_error ("Unexpected success code from 50 recursive variant call: %s", error->message);
  if (error->code != DBUS_GERROR_REMOTE_EXCEPTION)
    g_error ("Error code was not remote exception: %s", error->message);
  g_printerr ("Got expected error %d: \"%s\" from recursive variant call\n", error->code, error->message);
  g_clear_error (&error);

  g_object_unref (G_OBJECT (proxy));

  g_main_loop_unref (loop);

  return 0;
}
