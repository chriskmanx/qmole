#include <config.h>

#include <glib.h>
#include <dbus/dbus-glib-lowlevel.h>
#include <stdio.h>
#include <string.h>

#include "test-thread.h"

typedef struct {
  guint32 counters[N_TEST_THREADS];
} ThreadTestData;

static ThreadTestData *
thread_test_data_new (void)
{
  ThreadTestData *data;

  data = g_new0 (ThreadTestData, 1);
  
  return data;
}

static void
thread_test_data_free (ThreadTestData *data)
{
  g_free (data);
}

static DBusHandlerResult
filter_test_message (DBusConnection     *connection,
		     DBusMessage        *message,
		     void               *user_data)
{
  ThreadTestData *data = user_data;
  DBusMessageIter iter;
  gint32 threadnr;
  guint32 counter;
  const char *str;
  char *expected_str;
  GString *counter_str;
  int i;

  if (!dbus_message_is_method_call (message, "org.freedesktop.DBus.GLib.ThreadTest",
                                    "TestMethod"))
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
  
  dbus_message_iter_init (message, &iter);
  
  if (dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_INT32)
    {
      g_print ("First arg not right type\n");
      goto out;
    }
   dbus_message_iter_get_basic (&iter, &threadnr);
  if (threadnr < 0 || threadnr >= N_TEST_THREADS)
    {
      g_print ("Invalid thread nr\n");
      goto out;
    }

  if (! dbus_message_iter_next (&iter))
    {
      g_print ("Couldn't get second arg\n");
      goto out;
    }

  if (dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_INT32)
    {
      g_print ("Second arg not right type\n");
      goto out;
    }
  
   dbus_message_iter_get_basic (&iter, &counter);

  if (counter != data->counters[threadnr])
    {
      g_print ("Thread %d, counter %d, expected %d\n", threadnr, counter, data->counters[threadnr]);
      goto out;
    }
  data->counters[threadnr]++;
  
  if (! dbus_message_iter_next (&iter))
    {
      g_print ("Couldn't get third arg\n");
      goto out;
    }

  if (dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_STRING)
    {
      g_print ("Third arg not right type\n");
      goto out;
    }

  dbus_message_iter_get_basic (&iter, &str);

  if (str == NULL)
    {
      g_print ("No third arg\n");
      goto out;
    }

  expected_str = g_strdup_printf ("Thread %d-%d\n", threadnr, counter);
  if (strcmp (expected_str, str) != 0)
    {
      g_print ("Wrong string '%s', expected '%s'\n", str, expected_str);
      g_free (expected_str);
      goto out;
    }
  g_free (expected_str);

  if (dbus_message_iter_next (&iter))
    {
      g_print ("Extra args on end of message\n");
      goto out;
    }
  
  dbus_connection_flush (connection);

  counter_str = g_string_new ("");
  for (i = 0; i < N_TEST_THREADS; i++)
    {
      g_string_append_printf (counter_str, "%d ", data->counters[i]);
    }
  g_print ("%s\r", counter_str->str);
  g_string_free (counter_str, TRUE);
  
 out:
  return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
filter_disconnect (DBusConnection     *connection,
                   DBusMessage        *message,
                   void               *user_data)
{
  if (!dbus_message_is_signal (message, DBUS_INTERFACE_LOCAL,
                               "Disconnected"))
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;

  g_print ("connection disconnected\n");
  dbus_connection_unref (connection);
  
  return DBUS_HANDLER_RESULT_HANDLED;
}

static void
new_connection_callback (DBusServer     *server,
                         DBusConnection *new_connection,
                         void           *user_data)
{
  ThreadTestData * data;

  g_print ("new_connection_callback\n");
  
  dbus_connection_ref (new_connection);
  dbus_connection_setup_with_g_main (new_connection, NULL);

  data = thread_test_data_new ();
  
  if (!dbus_connection_add_filter (new_connection,
                                   filter_test_message, data,
                                   (DBusFreeFunction) thread_test_data_free))
    goto nomem;
  
  if (!dbus_connection_add_filter (new_connection,
                                   filter_disconnect, NULL, NULL))
    goto nomem;

  return;
  
 nomem:
  g_error ("no memory to setup new connection");
}

int
main (int argc, char *argv[])
{
  GMainLoop *loop;
  DBusServer *server;
  DBusError error;

  g_thread_init (NULL);
  dbus_g_thread_init ();
  
  if (argc < 2)
    {
      fprintf (stderr, "Give the server address as an argument\n");
      return 1;
    }

  dbus_error_init (&error);
  server = dbus_server_listen (argv[1], &error);
  if (server == NULL)
    {
      fprintf (stderr, "Failed to start server on %s: %s\n",
               argv[1], error.message);
      dbus_error_free (&error);
      return 1;
    }
  
  dbus_server_set_new_connection_function (server,
                                           new_connection_callback,
                                           NULL, NULL);

  dbus_server_setup_with_g_main (server, NULL);
  
  loop = g_main_loop_new (NULL, FALSE);
  g_main_loop_run (loop);  

  return 0;
}
