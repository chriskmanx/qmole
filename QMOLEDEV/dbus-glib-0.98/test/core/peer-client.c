#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dbus/dbus.h>
#include <dbus/dbus-glib.h>

static GMainLoop *loop;
static guint exit_timeout = 0;
static int n_times_frobnicate_received = 0;
static gboolean terminating = FALSE, terminated = FALSE;

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
  if (error->domain == DBUS_GERROR && error->code == DBUS_GERROR_REMOTE_EXCEPTION)
    lose ("%s (%s): %s", prefix, dbus_g_error_get_name (error),
	  error->message);
  else
    lose ("%s: %s", prefix, error->message);
}

static gboolean
timed_exit (gpointer loop)
{
  g_print ("timed exit!\n");
  g_main_loop_quit (loop);
  return TRUE;
}

static void
frobnicate_signal_handler (DBusGProxy *proxy, int val, void *user_data)
{
  n_times_frobnicate_received += 1;

  g_assert (val == 42);

  g_main_loop_quit (loop);
  g_source_remove (exit_timeout);
}

static void
destroy_cb (DBusGProxy *proxy, gpointer user_data)
{
  if (!terminating) {
    lose ("Proxy destroyed when it shouldn't have been");
  } else {
    terminated = TRUE;
    g_main_loop_quit (loop);
    g_source_remove (exit_timeout);
  }
}

int
main (int argc, char **argv)
{
  GError *error = NULL;
  DBusGConnection *conn;
  DBusGProxy *proxy;
  guint32 v_UINT32_2;
  char *addrbuf;
  gsize lineoffset;
  GIOChannel *io;

  g_thread_init (NULL); dbus_g_thread_init ();
  g_type_init ();

  io = g_io_channel_unix_new (0);
  if (!g_io_channel_read_line (io, &addrbuf, NULL, &lineoffset, &error))
    lose_gerror ("failed to read address from stdin", error);
  /* trim newline */
  addrbuf[lineoffset] = '\0';

  loop = g_main_loop_new (NULL, TRUE);

  conn = dbus_g_connection_open (addrbuf, &error);
  if (!conn)
    g_error ("Cannot open connection: %s", error->message);
  
  proxy = dbus_g_proxy_new_for_peer (conn, "/", "org.freedesktop.DBus.GLib.Tests.MyObject");
  g_assert (proxy);

  
  if (!dbus_g_proxy_call (proxy, "DoNothing", &error, G_TYPE_INVALID, G_TYPE_INVALID))
    lose_gerror ("Failed to complete DoNothing call", error);
  
  
  if (!dbus_g_proxy_call (proxy, "Increment", &error,
			  G_TYPE_UINT, 42,
			  G_TYPE_INVALID,
			  G_TYPE_UINT, &v_UINT32_2,
			  G_TYPE_INVALID))
    lose_gerror ("Failed to complete Increment call", error);
  if (v_UINT32_2 != 43)
    lose ("Increment call returned %d, should be 43", v_UINT32_2);
  

  n_times_frobnicate_received = 0;
  dbus_g_proxy_add_signal (proxy, "Frobnicate", G_TYPE_INT, G_TYPE_INVALID);
  dbus_g_proxy_connect_signal (proxy, "Frobnicate",
                               G_CALLBACK (frobnicate_signal_handler),
                               NULL, NULL);
  g_signal_connect (G_OBJECT (proxy), "destroy",
		    G_CALLBACK (destroy_cb),
		    NULL);
  
  if (!dbus_g_proxy_call (proxy, "EmitFrobnicate", &error, G_TYPE_INVALID, G_TYPE_INVALID))
    lose_gerror ("Failed to complete EmitFrobnicate call", error);
  exit_timeout = g_timeout_add (5000, timed_exit, loop);
  g_main_loop_run (loop);
  if (n_times_frobnicate_received != 1)
    lose ("Frobnicate signal received %d times, should have been 1", n_times_frobnicate_received);

  terminating = TRUE;
  if (!dbus_g_proxy_call (proxy, "Terminate", &error, G_TYPE_INVALID, G_TYPE_INVALID))
    lose_gerror ("Failed to complete Terminate call", error);
  exit_timeout = g_timeout_add (5000, timed_exit, loop);
  g_main_loop_run (loop);
  if (!terminated)
    lose ("Proxy didn't destroy when peer terminated");
  
  g_main_loop_unref (loop);

  return 0;
}
