#include <config.h>

/* -*- mode: C; c-file-style: "gnu" -*- */
#include <dbus/dbus-glib.h>
/* NOTE - outside of D-BUS core this would be
 * include <dbus/dbus-glib-bindings.h>
 */
#include "tools/dbus-glib-bindings.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib/gi18n.h>
#include <glib-object.h>
#include <glib/gquark.h>

#include "my-object.h"
#include "my-object-subclass.h"

static GObject *obj;
static GObject *obj2;
static GObject *subobj;
GMainLoop *loop;

#define TEST_SERVICE_NAME "org.freedesktop.DBus.GLib.TestService"

int
main (int argc, char **argv)
{
  DBusGConnection *connection;
  GError *error;
  DBusGProxy *driver_proxy;
  guint32 request_name_ret;

  g_type_init ();
  g_thread_init (NULL); dbus_g_thread_init ();

  dbus_g_error_domain_register (MY_OBJECT_ERROR,
				NULL,
				MY_TYPE_ERROR);

  g_printerr ("Launching test-service-glib\n");

  loop = g_main_loop_new (NULL, FALSE);

  {
    GLogLevelFlags fatal_mask;
    
    fatal_mask = g_log_set_always_fatal (G_LOG_FATAL_MASK);
    fatal_mask |= G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL;
    g_log_set_always_fatal (fatal_mask);
  }

  error = NULL;
  connection = dbus_g_bus_get (DBUS_BUS_STARTER,
                               &error);
  if (connection == NULL)
    {
      g_printerr ("Failed to open connection to bus: %s\n",
                  error->message);
      g_error_free (error);
      exit (1);
    }

  obj = g_object_new (MY_TYPE_OBJECT, NULL);
  obj2 = g_object_new (MY_TYPE_OBJECT, NULL);
  subobj = g_object_new (MY_TYPE_OBJECT_SUBCLASS, NULL);

  dbus_g_connection_register_g_object (connection,
                                       "/org/freedesktop/DBus/GLib/Tests/MyTestObject",
                                       obj);
  /* Register a second time; we want the object to also be reachable through this interface */
  dbus_g_connection_register_g_object (connection,
                                       "/org/freedesktop/DBus/GLib/Tests/Compat/MyTestObjectCompat",
                                       obj);
  dbus_g_connection_register_g_object (connection,
                                       "/org/freedesktop/DBus/GLib/Tests/MyTestObject2",
                                       obj2);

  dbus_g_connection_register_g_object (connection,
                                       "/org/freedesktop/DBus/GLib/Tests/MyTestObjectSubclass",
                                       subobj);

  driver_proxy = dbus_g_proxy_new_for_name (connection,
                                            DBUS_SERVICE_DBUS,
                                            DBUS_PATH_DBUS,
                                            DBUS_INTERFACE_DBUS);

  if (!org_freedesktop_DBus_request_name (driver_proxy,
					  TEST_SERVICE_NAME,
					  0, &request_name_ret, &error))
    {
      g_assert (error != NULL);
      g_printerr ("Failed to get name: %s\n",
		  error->message);
      g_clear_error (&error);
      exit (1);
    }

  if (!(request_name_ret == DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER))
    {
      g_printerr ("Got result code %u from requesting name\n", request_name_ret);
      exit (1);
    }

  g_printerr ("GLib test service has name '%s'\n", TEST_SERVICE_NAME);
  g_printerr ("GLib test service entering main loop\n");

  g_main_loop_run (loop);
  
  g_printerr ("Successfully completed %s\n", argv[0]);
  
  return 0;
}
