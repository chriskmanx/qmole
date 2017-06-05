#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <dbus/dbus.h>
#include <glib.h>
#include <dbus/dbus-glib-lowlevel.h>

DBusConnection *bus;
GMainContext *main_context;

typedef struct _SpiReentrantCallClosure
{
  GMainLoop   *loop;
  DBusMessage *reply;
} SpiReentrantCallClosure;

static void
set_reply (DBusPendingCall * pending, void *user_data)
{
  SpiReentrantCallClosure* closure = (SpiReentrantCallClosure *) user_data;

  closure->reply = dbus_pending_call_steal_reply (pending);
  dbus_connection_setup_with_g_main (bus, NULL);

  g_main_loop_quit (closure->loop);
}

static DBusMessage *
send_and_allow_reentry (DBusConnection * bus, DBusMessage * message)
{
  DBusPendingCall *pending;
  SpiReentrantCallClosure closure;

  closure.loop = g_main_loop_new (main_context, FALSE);
  dbus_connection_setup_with_g_main (bus, main_context);

  if (!dbus_connection_send_with_reply (bus, message, &pending, 3000))
    {
  dbus_connection_setup_with_g_main (bus, NULL);
      return NULL;
    }
  dbus_pending_call_set_notify (pending, set_reply, (void *) &closure, NULL);
  g_main_loop_run  (closure.loop);

  g_main_loop_unref (closure.loop);
  return closure.reply;
}

int
main(int argc, const char *argv[])
{
  DBusError error;
  DBusMessage *message = NULL, *reply = NULL;
  const char *str;

  main_context = g_main_context_new ();
  dbus_error_init (&error);
  bus = dbus_bus_get (DBUS_BUS_SESSION, &error);
  if (!bus)
  {
    fprintf(stderr, "Couldn't connect to bus: %s\n", error.name);
    return 1;
  }
  dbus_connection_setup_with_g_main (bus, NULL);
  message = dbus_message_new_method_call ("org.freedesktop.DBus", "/org/freedesktop/DBus", DBUS_INTERFACE_DBUS, "GetId");
  reply = send_and_allow_reentry (bus, message);
  if (dbus_message_get_type (reply) == DBUS_MESSAGE_TYPE_ERROR)
  {
    char *err;
    dbus_message_get_args (reply, NULL, DBUS_TYPE_STRING, &err, DBUS_TYPE_INVALID);
    fprintf (stderr, "Got error: %s\n", err);
    return 1;
  }
  dbus_message_unref (reply);
  dbus_message_unref (message);
  message = dbus_message_new_method_call ("org.freedesktop.DBus", "/org/freedesktop/DBus", DBUS_INTERFACE_DBUS, "GetId");
  reply = send_and_allow_reentry (bus, message);
  if (!reply)
  {
    fprintf(stderr, "Sorry; dbus wouldn't answer me: %s\n", error.message);
    exit(1);
  }
  if (dbus_message_get_type (reply) == DBUS_MESSAGE_TYPE_ERROR)
  {
    char *err;
    dbus_message_get_args (reply, NULL, DBUS_TYPE_STRING, &err, DBUS_TYPE_INVALID);
    fprintf (stderr, "Got error: %s\n", err);
    return 1;
  }
  if (!dbus_message_get_args (reply, &error, DBUS_TYPE_STRING, &str, DBUS_TYPE_INVALID))
  {
    fprintf(stderr, "Sorry; can't communicate: %s\n", error.message);
    exit(1);
  }

  return 0;
}
