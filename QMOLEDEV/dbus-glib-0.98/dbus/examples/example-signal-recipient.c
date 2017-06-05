#include <config.h>

#include <dbus/dbus-glib.h>
#include <stdio.h>
#include <stdlib.h>

static void lose (const char *fmt, ...) G_GNUC_NORETURN G_GNUC_PRINTF (1, 2);
static void lose_gerror (const char *prefix, GError *error) G_GNUC_NORETURN;

static void
lose (const char *str, ...)
{
  va_list args;

  va_start (args, str);

  vfprintf (stderr, str, args);
  fputc ('\n', stderr);

  va_end (args);

  exit (1);
}

static void
lose_gerror (const char *prefix, GError *error) 
{
  lose ("%s: %s", prefix, error->message);
}

static gboolean
emit_signal (gpointer arg)
{
  DBusGProxy *proxy = arg;
  
  dbus_g_proxy_call_no_reply (proxy, "emitHelloSignal", G_TYPE_INVALID);
  return TRUE;
}

static void
hello_signal_handler (DBusGProxy *proxy, const char *hello_string, gpointer user_data)
{
  printf ("Received signal and it says: %s\n", hello_string);
}

int
main (int argc, char **argv)
{
  DBusGConnection *bus;
  DBusGProxy *remote_object;
  GError *error = NULL;
  GMainLoop *mainloop;

  g_type_init ();

  mainloop = g_main_loop_new (NULL, FALSE);

  bus = dbus_g_bus_get (DBUS_BUS_SESSION, &error);
  if (!bus)
    lose_gerror ("Couldn't connect to session bus", error);
  
  /* We use _for_name_owner in order to track this particular service
   * instance, which lets us receive signals.
   */
  remote_object = dbus_g_proxy_new_for_name (bus,
					     "org.designfu.TestService",
					     "/org/designfu/TestService/object",
					     "org.designfu.TestService");
  if (!remote_object)
    lose_gerror ("Failed to get name owner", error);

  /* IMPORTANT:
   *
   * Note because this signal's signature is VOID__STRING, we do not
   * need to register a marshaller, since there is a builtin one.
   * However for other signatures, you must generate a marshaller,
   * then call dbus_g_object_register_marshaller.  It would look like
   * this:
   * 
   * dbus_g_object_register_marshaller (g_cclosure_marshal_VOID__STRING, G_TYPE_NONE, G_TYPE_STRING, G_TYPE_INVALID);
   *
   */

  /* Tell DBus what the type signature of the signal callback is; this
   * allows us to sanity-check incoming messages before invoking the
   * callback.  You need to do this once for each proxy you create,
   * not every time you want to connect to the signal.
   */
  dbus_g_proxy_add_signal (remote_object, "HelloSignal", G_TYPE_STRING, G_TYPE_INVALID);

  /* Actually connect to the signal.  Note you can call
   * dbus_g_proxy_connect_signal multiple times for one invocation of
   * dbus_g_proxy_add_signal.
   */
  dbus_g_proxy_connect_signal (remote_object, "HelloSignal", G_CALLBACK (hello_signal_handler),
			       NULL, NULL);
  

  g_timeout_add (2000, emit_signal, remote_object);

  g_main_loop_run (mainloop);

  exit (0);
}
