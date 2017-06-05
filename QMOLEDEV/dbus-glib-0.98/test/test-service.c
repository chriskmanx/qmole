#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dbus/dbus.h>

static dbus_bool_t already_quit = FALSE;
static dbus_bool_t hello_from_self_reply_recived = FALSE;

static void
quit (void)
{
  if (!already_quit)
    already_quit = TRUE;
}

static void
die (const char *message)
{
  fprintf (stderr, "*** test-service: %s", message);
  exit (1);
}

static void
check_hello_from_self_reply (DBusPendingCall *pcall, 
                             void *user_data)
{
  DBusMessage *reply;
  DBusMessage *echo_message, *echo_reply;
  DBusError error;
  DBusConnection *connection;
  
  int type;
  
  dbus_error_init (&error);
 
  connection = dbus_bus_get (DBUS_BUS_STARTER, &error);
  if (connection == NULL)
    {
      fprintf (stderr, "*** Failed to open connection to activating message bus: %s\n",
               error.message);
      dbus_error_free (&error);
      die("no memory");
    }

  
  echo_message = (DBusMessage *)user_data;
    
  reply = dbus_pending_call_steal_reply (pcall);
    
  type = dbus_message_get_type (reply);
    
  if (type == DBUS_MESSAGE_TYPE_METHOD_RETURN)
    {
      const char *s;
      printf ("Reply from HelloFromSelf recived\n");
     
      if (!dbus_message_get_args (echo_message,
                              &error,
                              DBUS_TYPE_STRING, &s,
                              DBUS_TYPE_INVALID))
        {
            echo_reply = dbus_message_new_error (echo_message,
                                      error.name,
                                      error.message);

            if (echo_reply == NULL)
              die ("No memory\n");

        } 
      else
        {  
          echo_reply = dbus_message_new_method_return (echo_message);
          if (echo_reply == NULL)
            die ("No memory\n");
  
          if (!dbus_message_append_args (echo_reply,
                                 DBUS_TYPE_STRING, &s,
                                 DBUS_TYPE_INVALID))
            die ("No memory");
        }
        
      if (!dbus_connection_send (connection, echo_reply, NULL))
        die ("No memory\n");
      
      dbus_message_unref (echo_reply);
    }
  else if (type == DBUS_MESSAGE_TYPE_ERROR)
    {
      dbus_set_error_from_message (&error, reply);
      printf ("Error type in reply: %s\n", error.message);

      if (strcmp (error.name, DBUS_ERROR_NO_MEMORY) != 0)
        {
            echo_reply = dbus_message_new_error (echo_message,
                                      error.name,
                                      error.message);

            if (echo_reply == NULL)
              die ("No memory\n");

            if (!dbus_connection_send (connection, echo_reply, NULL))
              die ("No memory\n");

            dbus_message_unref (echo_reply);
        }
      dbus_error_free (&error);
    }
  
  hello_from_self_reply_recived = TRUE;
  
  dbus_message_unref (reply);
  dbus_message_unref (echo_message);
  dbus_pending_call_unref (pcall);
}

static DBusHandlerResult
handle_run_hello_from_self (DBusConnection     *connection,
                                               DBusMessage        *message)
{
  DBusError error;
  DBusMessage *reply, *self_message;
  DBusPendingCall *pcall;
  char *s;

  dbus_error_init (&error);
  
  if (!dbus_message_get_args (message,
                              &error,
                              DBUS_TYPE_STRING, &s,
                              DBUS_TYPE_INVALID))
    {
      reply = dbus_message_new_error (message,
                                      error.name,
                                      error.message);

      if (reply == NULL)
        die ("No memory\n");

      if (!dbus_connection_send (connection, reply, NULL))
        die ("No memory\n");

      dbus_message_unref (reply);

      return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }
    printf ("Sending HelloFromSelf\n");

 self_message = dbus_message_new_method_call ("org.freedesktop.DBus.GLib.TestEchoService",
                                          "/org/freedesktop/DBus/GLib/TestSuite",
                                          "org.freedesktop.DBus.GLib.TestSuite",
                                          "HelloFromSelf");
  
  if (self_message == NULL)
    die ("No memory");
  
  if (!dbus_connection_send_with_reply (connection, self_message, &pcall, -1))
    die("No memory");
  
  dbus_message_ref (message);
  if (!dbus_pending_call_set_notify (pcall, check_hello_from_self_reply, (void *)message, NULL))
    die("No memory");
    
  printf ("Sent HelloFromSelf\n");
  return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
handle_echo (DBusConnection     *connection,
             DBusMessage        *message)
{
  DBusError error;
  DBusMessage *reply;
  char *s;

  dbus_error_init (&error);
  
  if (!dbus_message_get_args (message,
                              &error,
                              DBUS_TYPE_STRING, &s,
                              DBUS_TYPE_INVALID))
    {
      reply = dbus_message_new_error (message,
                                      error.name,
                                      error.message);

      if (reply == NULL)
        die ("No memory\n");

      if (!dbus_connection_send (connection, reply, NULL))
        die ("No memory\n");

      dbus_message_unref (reply);

      return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }

  reply = dbus_message_new_method_return (message);
  if (reply == NULL)
    die ("No memory\n");
  
  if (!dbus_message_append_args (reply,
                                 DBUS_TYPE_STRING, &s,
                                 DBUS_TYPE_INVALID))
    die ("No memory");
  
  if (!dbus_connection_send (connection, reply, NULL))
    die ("No memory\n");

  fprintf (stderr, "Echo service echoed string: \"%s\"\n", s);
  
  dbus_message_unref (reply);
    
  return DBUS_HANDLER_RESULT_HANDLED;
}

static void
path_unregistered_func (DBusConnection  *connection,
                        void            *user_data)
{
  /* connection was finalized */
}

static DBusHandlerResult
path_message_func (DBusConnection  *connection,
                   DBusMessage     *message,
                   void            *user_data)
{
  if (dbus_message_is_method_call (message,
                                   "org.freedesktop.DBus.GLib.TestSuite",
                                   "Echo"))
    return handle_echo (connection, message);
  else if (dbus_message_is_method_call (message,
                                        "org.freedesktop.DBus.GLib.TestSuite",
                                        "Exit"))
    {
      dbus_connection_close (connection);
      quit ();
      return DBUS_HANDLER_RESULT_HANDLED;
    }
  else if (dbus_message_is_method_call (message,
                                        "org.freedesktop.DBus.GLib.TestSuite",
                                        "EmitFoo"))
    {
      /* Emit the Foo signal */
      DBusMessage *signal;
      double v_DOUBLE;

      signal = dbus_message_new_signal ("/org/freedesktop/DBus/GLib/TestSuite",
                                        "org.freedesktop.DBus.GLib.TestSuite",
                                        "Foo");
      if (signal == NULL)
        die ("No memory\n");

      v_DOUBLE = 42.6;
      if (!dbus_message_append_args (signal,
                                     DBUS_TYPE_DOUBLE, &v_DOUBLE,
                                     DBUS_TYPE_INVALID))
        die ("No memory");
  
      if (!dbus_connection_send (connection, signal, NULL))
        die ("No memory\n");
      
      return DBUS_HANDLER_RESULT_HANDLED;
    }
    
  else if (dbus_message_is_method_call (message,
                                   "org.freedesktop.DBus.GLib.TestSuite",
                                   "RunHelloFromSelf"))
    {
      return handle_run_hello_from_self (connection, message);
    }
  else if (dbus_message_is_method_call (message,
                                        "org.freedesktop.DBus.GLib.TestSuite",
                                        "HelloFromSelf"))
    {
        DBusMessage *reply;
        printf ("Recived the HelloFromSelf message\n");
        
        reply = dbus_message_new_method_return (message);
        if (reply == NULL)
          die ("No memory");
        
        if (!dbus_connection_send (connection, reply, NULL))
          die ("No memory");
    }
  
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusObjectPathVTable
echo_vtable = {
  path_unregistered_func,
  path_message_func,
  NULL,
};


static const char* echo_path = "/org/freedesktop/DBus/GLib/TestSuite" ;

static DBusHandlerResult
filter_func (DBusConnection     *connection,
             DBusMessage        *message,
             void               *user_data)
{
  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    {
      dbus_connection_close (connection);
      quit ();
      return DBUS_HANDLER_RESULT_HANDLED;
    }
  else
    {
      return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }
}

int
main (int    argc,
      char **argv)
{
  DBusError error;
  DBusConnection *connection;
  
  dbus_error_init (&error);
  connection = dbus_bus_get (DBUS_BUS_STARTER, &error);
  if (connection == NULL)
    {
      fprintf (stderr, "*** Failed to open connection to activating message bus: %s\n",
               error.message);
      dbus_error_free (&error);
      return 1;
    }

  if (!dbus_connection_add_filter (connection,
                                   filter_func, NULL, NULL))
    die ("No memory");

  if (!dbus_connection_register_object_path (connection,
                                             echo_path,
                                             &echo_vtable,
                                             (void*) 0xdeadbeef))
    die ("No memory");

  {
    void *d;
    if (!dbus_connection_get_object_path_data (connection, echo_path, &d))
      die ("No memory");
    if (d != (void*) 0xdeadbeef)
      die ("dbus_connection_get_object_path_data() doesn't seem to work right\n");
  }
  
  dbus_bus_request_name (connection, "org.freedesktop.DBus.GLib.TestEchoService",
                                  0, &error);
  if (dbus_error_is_set (&error))
    {
      fprintf (stderr, "Error %s\n", error.message);
      dbus_error_free (&error);
      exit (1);
    }
  
  while (dbus_connection_read_write_dispatch (connection, -1) && !already_quit)
    ;  

  dbus_connection_remove_filter (connection, filter_func, NULL);
  
  dbus_connection_unref (connection);

  dbus_shutdown ();

  return 0;
}
