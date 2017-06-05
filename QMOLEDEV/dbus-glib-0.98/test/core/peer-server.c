#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#include <dbus/dbus.h>
#include <dbus/dbus-glib.h>
#include <dbus/dbus-glib-lowlevel.h>

#include "my-object.h"

GMainLoop *loop;

static void
new_connection_func (DBusServer *server, DBusConnection *conn, gpointer user_data)
{
  GObject *obj;

  obj = g_object_new (MY_TYPE_OBJECT, NULL);

  dbus_connection_ref (conn);
  dbus_connection_setup_with_g_main (conn, NULL);

  dbus_g_connection_register_g_object
    (dbus_connection_get_g_connection (conn), "/", obj);
}

int
main (int argc, char **argv)
{
  DBusError error;
  DBusServer *server;
  char *addr;

  dbus_error_init (&error);

  g_thread_init (NULL); dbus_g_thread_init ();
  g_type_init ();

  loop = g_main_loop_new (NULL, TRUE);

  server = dbus_server_listen ("unix:tmpdir=/tmp", &error);
  if (!server) 
    {
      g_warning ("Cannot create server: %s", error.message);
      return 1;
    }
  addr = dbus_server_get_address (server);
  fprintf (stdout, "%s\n", addr);
  fflush (stdout);
  free (addr);
  dbus_server_setup_with_g_main (server, NULL);
  dbus_server_set_new_connection_function (server, new_connection_func, NULL, NULL);
  
  g_main_loop_run (loop);
  
  g_main_loop_unref (loop);
  return 0;
}
