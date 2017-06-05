/* -*- mode: C; c-file-style: "gnu" -*- */
/* test-profile.c Program that does basic message-response for timing; doesn't really use glib bindings
 *
 * Copyright (C) 2003, 2004  Red Hat Inc.
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
#include <glib.h>

/* This test uses Unix-specific facilities */
#ifdef G_OS_WIN32
#define TEST_PROFILE_DISABLED
#endif

#ifndef TEST_PROFILE_DISABLED

#include <dbus/dbus-glib-lowlevel.h>
#include <stdlib.h>
#include <unistd.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#ifndef HAVE_SOCKLEN_T
#define socklen_t int
#endif

#define _DBUS_ZERO(object) (memset (&(object), '\0', sizeof ((object))))
#define _DBUS_MAX_SUN_PATH_LENGTH 99

/* Note that if you set threads > 1 you get a bogus profile since the
 * clients start blocking on the server, so the client write() will go
 * higher in the profile the larger the number of threads.
 */
#define N_CLIENT_THREADS 1
/* It seems like at least 750000 or so iterations reduces the variability to sane levels */
#define N_ITERATIONS 2000
#define N_PROGRESS_UPDATES 20
/* Don't make PAYLOAD_SIZE too huge because it gets used as a static buffer size */
#define PAYLOAD_SIZE 0

#define ECHO_SERVICE "org.freedesktop.DBus.GLib.EchoTestServer"
#define ECHO_PATH "/org/freedesktop/DBus/GLib/EchoTest"
#define ECHO_INTERFACE "org.freedesktop.DBus.GLib.EchoTest"
#define ECHO_PING_METHOD "Ping"

static const char *messages_address;
static const char *plain_sockets_address;
static unsigned char *payload;
static int echo_call_size;
static int echo_return_size;

typedef struct ProfileRunVTable ProfileRunVTable;

typedef struct
{
  const ProfileRunVTable *vtable;
  int iterations;
  GMainLoop *loop;
} ClientData;

typedef struct
{
  const ProfileRunVTable *vtable;
  int handled;
  GMainLoop *loop;
  int n_clients;
} ServerData;

struct ProfileRunVTable
{
  const char *name;
  gboolean fake_malloc_overhead;
  void* (* init_server)        (ServerData *sd);
  void  (* stop_server)        (ServerData *sd,
                                void       *server);
  void* (* client_thread_func) (void *data); /* Data has to be the vtable */

  /* this is so different runs show up in the profiler with
   * different backtrace
   */
  void  (* main_loop_run_func) (GMainLoop *loop);
};

/* Note, this is all crack-a-rific; it isn't using DBusGProxy and thus is
 * a major pain
 */
static void
send_echo_method_call (DBusConnection *connection)
{
  DBusMessage *message;
  const char *hello = "Hello World!";
  dbus_int32_t i32 = 123456;

  message = dbus_message_new_method_call (ECHO_SERVICE,
                                          ECHO_PATH,
                                          ECHO_INTERFACE,
                                          ECHO_PING_METHOD);
  dbus_message_append_args (message,
                            DBUS_TYPE_STRING, &hello,
                            DBUS_TYPE_INT32, &i32,
#if PAYLOAD_SIZE > 0
                            DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
                            &payload, PAYLOAD_SIZE,
#endif
                            DBUS_TYPE_INVALID);
  
  dbus_connection_send (connection, message, NULL);
  dbus_message_unref (message);
  dbus_connection_flush (connection);
}

static void
send_echo_method_return (DBusConnection *connection,
                         DBusMessage    *call_message)
{
  DBusMessage *message;

  message = dbus_message_new_method_return (call_message);
  
  dbus_connection_send (connection, message, NULL);
  dbus_message_unref (message);
  dbus_connection_flush (connection);
}

static DBusHandlerResult
with_or_without_bus_client_filter (DBusConnection     *connection,
                                   DBusMessage        *message,
                                   ClientData         *cd)
{
  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    {
      g_printerr ("Client thread disconnected\n");
      exit (1);
    }
  else if (dbus_message_get_type (message) == DBUS_MESSAGE_TYPE_METHOD_RETURN)
    {
      cd->iterations += 1;
      if (cd->iterations >= N_ITERATIONS)
        {
          g_printerr ("\nCompleted %d iterations\n", N_ITERATIONS);
          g_main_loop_quit (cd->loop);
        }
      else if (cd->iterations % (N_ITERATIONS/N_PROGRESS_UPDATES) == 0)
        {
          g_printerr ("%d%% ", (int) (cd->iterations/(double)N_ITERATIONS * 100.0));
        }
      
      send_echo_method_call (connection);
      return DBUS_HANDLER_RESULT_HANDLED;
    }
  
  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusHandlerResult
no_bus_client_filter (DBusConnection     *connection,
                      DBusMessage        *message,
                      void               *user_data)
{
  ClientData *cd = user_data;

  return with_or_without_bus_client_filter (connection, message, cd);
}

static void*
no_bus_thread_func (void *data)
{
  DBusError error;
  GMainContext *context;
  DBusConnection *connection;
  ClientData cd;
  
  g_printerr ("Starting client thread %p\n", g_thread_self());  
  
  dbus_error_init (&error);
  connection = dbus_connection_open_private (messages_address, &error);
  if (connection == NULL)
    {
      g_printerr ("could not open connection: %s\n", error.message);
      dbus_error_free (&error);
      exit (1);
    }

  context = g_main_context_new ();

  cd.iterations = 1;
  cd.loop = g_main_loop_new (context, FALSE);
  
  if (!dbus_connection_add_filter (connection,
				   no_bus_client_filter, &cd, NULL))
    g_error ("no memory");
  
  
  dbus_connection_setup_with_g_main (connection, context);

  g_printerr ("Client thread sending message to prime pingpong\n");
  send_echo_method_call (connection);
  g_printerr ("Client thread sent message\n");

  g_printerr ("Client thread entering main loop\n");
  g_main_loop_run (cd.loop);
  g_printerr ("Client thread %p exiting main loop\n",
              g_thread_self());

  dbus_connection_close (connection);
  
  g_main_loop_unref (cd.loop);
  g_main_context_unref (context);
  
  return NULL;
}

static DBusHandlerResult
no_bus_server_filter (DBusConnection     *connection,
                      DBusMessage        *message,
                      void               *user_data)
{
  ServerData *sd = user_data;
  
  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    {
      g_printerr ("Client disconnected from server\n");
      sd->n_clients -= 1;
      if (sd->n_clients == 0)
        g_main_loop_quit (sd->loop);
    }
  else if (dbus_message_is_method_call (message,
                                        ECHO_INTERFACE,
                                        ECHO_PING_METHOD))
    {
      sd->handled += 1;
      send_echo_method_return (connection, message);
      return DBUS_HANDLER_RESULT_HANDLED;
    }
  
  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static void
no_bus_new_connection_callback (DBusServer     *server,
                                DBusConnection *new_connection,
                                void           *user_data)
{
  ServerData *sd = user_data;
  
  dbus_connection_ref (new_connection);
  dbus_connection_setup_with_g_main (new_connection, NULL);  
  
  if (!dbus_connection_add_filter (new_connection,
                                   no_bus_server_filter, sd, NULL))
    g_error ("no memory");

  sd->n_clients += 1;

  /* FIXME we leak the handler */  
}

static void*
no_bus_init_server (ServerData       *sd)
{
  DBusServer *server;
  DBusError error;

  dbus_error_init (&error);
  server = dbus_server_listen ("unix:tmpdir="DBUS_TEST_SOCKET_DIR,
                               &error);
  if (server == NULL)
    {
      g_printerr ("Could not start server: %s\n",
                  error.message);
      exit (1);
    }

  messages_address = dbus_server_get_address (server);
  
  dbus_server_set_new_connection_function (server,
                                           no_bus_new_connection_callback,
                                           sd, NULL);

  dbus_server_setup_with_g_main (server, NULL);
  
  return server;
}

static void
no_bus_stop_server (ServerData *sd,
                    void       *server)
{
  dbus_server_disconnect (server);
  dbus_server_unref (server);
}

static void
no_bus_main_loop_run (GMainLoop *loop)
{
  g_main_loop_run (loop);
}

static const ProfileRunVTable no_bus_vtable = {
  "dbus direct without bus",
  FALSE,
  no_bus_init_server,
  no_bus_stop_server,
  no_bus_thread_func,
  no_bus_main_loop_run
};

typedef struct
{
  const ProfileRunVTable *vtable;
  ServerData *sd;
  GHashTable *client_names;
  DBusConnection *connection;
} WithBusServer;

static DBusHandlerResult
with_bus_client_filter (DBusConnection     *connection,
                        DBusMessage        *message,
                        void               *user_data)
{
  ClientData *cd = user_data;

  return with_or_without_bus_client_filter (connection, message, cd);
}

static void*
with_bus_thread_func (void *data)
{
  DBusError error;
  DBusConnection *connection;
  ClientData cd;
  const char *address;
  GMainContext *context;
  
  g_printerr ("Starting client thread %p\n", g_thread_self());  

  address = g_getenv ("DBUS_SESSION_BUS_ADDRESS");
  if (address == NULL)
    {
      g_printerr ("DBUS_SESSION_BUS_ADDRESS not set\n");
      exit (1);
    }
  
  dbus_error_init (&error);
  connection = dbus_connection_open_private (address, &error);
  if (connection == NULL)
    {
      g_printerr ("could not open connection to bus: %s\n", error.message);
      dbus_error_free (&error);
      exit (1);
    }

  if (!dbus_bus_register (connection, &error))
    {
      g_printerr ("could not register with bus: %s\n", error.message);
      dbus_error_free (&error);
      exit (1);
    }
  
  context = g_main_context_new ();

  cd.iterations = 1;
  cd.loop = g_main_loop_new (context, FALSE);
  
  if (!dbus_connection_add_filter (connection,
				   with_bus_client_filter, &cd, NULL))
    g_error ("no memory");
  
  dbus_connection_setup_with_g_main (connection, context);

  g_printerr ("Client thread sending message to prime pingpong\n");
  send_echo_method_call (connection);
  g_printerr ("Client thread sent message\n");

  g_printerr ("Client thread entering main loop\n");
  g_main_loop_run (cd.loop);
  g_printerr ("Client thread %p exiting main loop\n",
              g_thread_self());

  dbus_connection_close (connection);
  
  g_main_loop_unref (cd.loop);
  g_main_context_unref (context);
  
  return NULL;
}

static DBusHandlerResult
with_bus_server_filter (DBusConnection     *connection,
                        DBusMessage        *message,
                        void               *user_data)
{
  WithBusServer *server = user_data;
  
  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    {
      g_printerr ("Server disconnected from message bus\n");
      exit (1);
    }
  else if (dbus_message_has_sender (message,
                                    DBUS_SERVICE_DBUS) &&
           dbus_message_is_signal (message,
                                   DBUS_INTERFACE_DBUS,
                                   "NameOwnerChanged"))
    {
      const char *name, *old_owner, *new_owner;
      DBusError error;

      name = NULL;
      old_owner = NULL;
      new_owner = NULL;

      dbus_error_init (&error);
      if (!dbus_message_get_args (message,
                                  &error,
                                  DBUS_TYPE_STRING, &name,
                                  DBUS_TYPE_STRING, &old_owner,
                                  DBUS_TYPE_STRING, &new_owner,
                                  DBUS_TYPE_INVALID))
        {
          g_printerr ("dbus_message_get_args(): %s\n", error.message);
          exit (1);
        }

      if (g_hash_table_lookup (server->client_names,
                               name) &&
          *old_owner != '\0' &&
          *new_owner == '\0')
        {
          g_hash_table_remove (server->client_names,
                               name);
          server->sd->n_clients -= 1;
          if (server->sd->n_clients == 0)
            g_main_loop_quit (server->sd->loop);
        }
    }
  else if (dbus_message_is_method_call (message,
                                        ECHO_INTERFACE,
                                        ECHO_PING_METHOD))
    {
      const char *sender;

      sender = dbus_message_get_sender (message);

      if (!g_hash_table_lookup (server->client_names,
                                sender))
        {
          g_printerr ("First message from new client %s on bus\n", sender);
          
          g_hash_table_replace (server->client_names,
                                g_strdup (sender),
                                GINT_TO_POINTER (1));
          server->sd->n_clients += 1;
        }
      
      server->sd->handled += 1;
      send_echo_method_return (connection, message);
      return DBUS_HANDLER_RESULT_HANDLED;
    }
  
  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static void*
with_bus_init_server (ServerData       *sd)
{
  DBusGConnection *gconnection;
  DBusConnection *connection;
  GError *gerror;
  const char *s;
  WithBusServer *server;
  char *rule;

  server = g_new0 (WithBusServer, 1);

  server->vtable = sd->vtable;
  server->sd = sd;
  
  s = g_getenv ("DBUS_TEST_GLIB_RUN_TEST_SCRIPT");
  if (s == NULL ||
      *s != '1')
    {
      g_printerr ("You have to run with_bus mode with the run-test.sh script\n");
      exit (1);
    }

  /* Note that we use the standard global bus connection for the
   * server, and the clients open their own connections so they can
   * have their own main loops and because I'm not sure "talking to
   * yourself" really works yet
   */
  gerror = NULL;
  gconnection = dbus_g_bus_get (DBUS_BUS_SESSION, &gerror);
  if (gconnection == NULL)
    {
      g_printerr ("could not open connection to bus: %s\n", gerror->message);
      g_error_free (gerror);
      exit (1);
    }

  server->client_names = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                g_free, NULL);
  
  connection = dbus_g_connection_get_connection (gconnection);

  dbus_bus_request_name (connection,
                         ECHO_SERVICE,
                         0, NULL); /* ignore errors because we suck */
  
  rule = g_strdup_printf ("type='signal',sender='%s',member='%s'",
                          DBUS_SERVICE_DBUS,
                          "NameOwnerChanged");

  /* ignore errors because we suck */
  dbus_bus_add_match (connection, rule, NULL);

  g_free (rule);
  
  if (!dbus_connection_add_filter (connection,
                                   with_bus_server_filter, server, NULL))
    g_error ("no memory");

  server->connection = connection;
  server->client_names = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                g_free, NULL);
  
  return server;
}

static void
with_bus_stop_server (ServerData *sd,
                      void       *serverv)
{
  WithBusServer *server = serverv;
  
  dbus_connection_remove_filter (server->connection,
                                 with_bus_server_filter, server);

  g_hash_table_destroy (server->client_names);
  dbus_connection_unref (server->connection);
  
  g_free (server);
}

static void
with_bus_main_loop_run (GMainLoop *loop)
{
  g_main_loop_run (loop);
}

static const ProfileRunVTable with_bus_vtable = {
  "routing via a bus",
  FALSE,
  with_bus_init_server,
  with_bus_stop_server,
  with_bus_thread_func,
  with_bus_main_loop_run
};


typedef struct
{
  const ProfileRunVTable *vtable;
  int listen_fd;
  ServerData *sd;
  unsigned int source_id;
} PlainSocketServer;

static void
read_and_drop_on_floor (int fd,
                        int count,
                        gboolean fake_malloc_overhead)
{
  int bytes_read;
  int val;
  char *buf;
  char *allocated;
  char not_allocated[512+PAYLOAD_SIZE];

  g_assert (count < (int) sizeof(not_allocated));
  
  if (fake_malloc_overhead)
    {
      allocated = g_malloc (count);
      buf = allocated;
    }
  else
    {
      allocated = NULL;
      buf = not_allocated;
    }
  
  bytes_read = 0;

  while (bytes_read < count)
    {
    again:
      
      val = read (fd, buf + bytes_read, count - bytes_read);                  
      
      if (val < 0)
        {
          if (errno == EINTR)
            goto again;
          else
            {
              g_printerr ("read() failed thread %p: %s\n",
                          g_thread_self(), strerror (errno));
              exit (1);
            }
        }
      else
        {
          bytes_read += val;
        }
    }

  if (fake_malloc_overhead)
    g_free (allocated);

#if 0
  g_printerr ("%p read %d bytes from fd %d\n",
           g_thread_self(), bytes_read, fd);
#endif
}

static void
write_junk (int fd,
            int count,
            gboolean fake_malloc_overhead)
{
  int bytes_written;
  int val;
  char *buf;
  char *allocated;
  char not_allocated[512+PAYLOAD_SIZE] = { '\0', };

  g_assert (count < (int) sizeof(not_allocated));
  
  if (fake_malloc_overhead)
    {
      int i;
      
      allocated = g_malloc (count);
      buf = allocated;

      /* Write some stuff into the allocated buffer to simulate
       * creating some sort of data
       */
      i = 0;
      while (i < count)
        {
          allocated[i] = (char) i;
          ++i;
        }
    }
  else
    {
      allocated = NULL;
      buf = not_allocated;
    }
  
  bytes_written = 0;
  
  while (bytes_written < count)
    {
    again:
      
      val = write (fd, buf + bytes_written, count - bytes_written);
      
      if (val < 0)
        {
          if (errno == EINTR)
            goto again;
          else
            {
              g_printerr ("write() failed thread %p: %s\n",
                          g_thread_self(), strerror (errno));
              exit (1);
            }
        }
      else
        {
          bytes_written += val;
        }
    }

  if (fake_malloc_overhead)
    g_free (allocated);
  
#if 0
  g_printerr ("%p wrote %d bytes to fd %d\n",
           g_thread_self(), bytes_written, fd);
#endif
}

static gboolean
plain_sockets_talk_to_client_watch (GIOChannel   *source,
                                    GIOCondition  condition,
                                    gpointer      data)
{
  PlainSocketServer *server = data;
  int client_fd = g_io_channel_unix_get_fd (source);
  
  if (condition & G_IO_HUP)
    {
      g_printerr ("Client disconnected from server\n");
      server->sd->n_clients -= 1;
      if (server->sd->n_clients == 0)
        g_main_loop_quit (server->sd->loop);

      return FALSE; /* remove watch */
    }
  else if (condition & G_IO_IN)
    {
      server->sd->handled += 1;

      read_and_drop_on_floor (client_fd, echo_call_size, server->vtable->fake_malloc_overhead);
      write_junk (client_fd, echo_return_size, server->vtable->fake_malloc_overhead);
    }
  else
    {
      g_printerr ("Unexpected IO condition in server thread\n");
      exit (1);
    }

  return TRUE;
}

static gboolean
plain_sockets_new_client_watch (GIOChannel   *source,
                                GIOCondition  condition,
                                gpointer      data)
{
  int client_fd;
  struct sockaddr addr;
  socklen_t addrlen;
  GIOChannel *channel;
  PlainSocketServer *server = data;

  if (!(condition & G_IO_IN))
    {
      g_printerr ("Unexpected IO condition on server socket\n");
      exit (1);
    }
  
  addrlen = sizeof (addr);
  
 retry:
  client_fd = accept (server->listen_fd, &addr, &addrlen);
  
  if (client_fd < 0)
    {
      if (errno == EINTR)
        goto retry;
      else
        {
          g_printerr ("Failed to accept() connection from client: %s\n",
                      strerror (errno));
          exit (1);
        }
    }
  
  channel = g_io_channel_unix_new (client_fd);
  g_io_add_watch (channel,
                  G_IO_IN | G_IO_ERR | G_IO_HUP | G_IO_NVAL | G_IO_PRI,
                  plain_sockets_talk_to_client_watch,
                  server);
  g_io_channel_unref (channel);

  server->sd->n_clients += 1;
  
  return TRUE;
}

static void*
plain_sockets_init_server (ServerData *sd)
{
  PlainSocketServer *server;
  struct sockaddr_un addr;
  static char path[] = "/tmp/dbus-test-profile-XXXXXX";
  char *p;
  GIOChannel *channel;

  server = g_new0 (PlainSocketServer, 1);
  server->sd = sd;
  server->vtable = sd->vtable; /* for convenience */
  
  p = path;
  while (*p)
    {
      if (*p == 'X')
        *p = 'a' + (int) (26.0*rand()/(RAND_MAX+1.0));
      ++p;
    }

  g_printerr ("Socket is %s\n", path);
  
  server->listen_fd = socket (PF_UNIX, SOCK_STREAM, 0);
  
  if (server->listen_fd < 0)
    {
      g_printerr ("Failed to create socket: %s",
                  strerror (errno));
      exit (1);
    }

  _DBUS_ZERO (addr);
  addr.sun_family = AF_UNIX;
  
#ifdef HAVE_ABSTRACT_SOCKETS
  /* remember that abstract names aren't nul-terminated so we rely
   * on sun_path being filled in with zeroes above.
   */
  addr.sun_path[0] = '\0'; /* this is what says "use abstract" */
  strncpy (&addr.sun_path[1], path, _DBUS_MAX_SUN_PATH_LENGTH - 2);
  /* _dbus_verbose_bytes (addr.sun_path, sizeof (addr.sun_path)); */
#else /* HAVE_ABSTRACT_SOCKETS */
  {
    struct stat sb;
    
    if (stat (path, &sb) == 0 &&
        S_ISSOCK (sb.st_mode))
      unlink (path);
  }

  strncpy (addr.sun_path, path, _DBUS_MAX_SUN_PATH_LENGTH - 1);
#endif /* ! HAVE_ABSTRACT_SOCKETS */
  
  if (bind (server->listen_fd, (struct sockaddr*) &addr, sizeof (addr)) < 0)
    {
      g_printerr ("Failed to bind socket \"%s\": %s",
                  path, strerror (errno));
      exit (1);
    }

  if (listen (server->listen_fd, 30 /* backlog */) < 0)
    {
      g_printerr ("Failed to listen on socket \"%s\": %s",
                  path, strerror (errno));
      exit (1);
    }

  plain_sockets_address = path;

  channel = g_io_channel_unix_new (server->listen_fd);
  server->source_id =
    g_io_add_watch (channel,
                    G_IO_IN | G_IO_ERR | G_IO_HUP | G_IO_NVAL | G_IO_PRI,
                    plain_sockets_new_client_watch,
                    server);
  g_io_channel_unref (channel);
  
  return server;
}

static void
plain_sockets_stop_server (ServerData *sd,
                           void       *server_v)
{
  PlainSocketServer *server = server_v;

  g_source_remove (server->source_id);
  
  close (server->listen_fd);
  g_free (server);
  
  {
    struct stat sb;
    
    if (stat (plain_sockets_address, &sb) == 0 &&
        S_ISSOCK (sb.st_mode))
      unlink (plain_sockets_address);
  }
}

static gboolean
plain_sockets_client_side_watch (GIOChannel   *source,
                                 GIOCondition  condition,
                                 gpointer      data)
{
  ClientData *cd = data;
  int fd = g_io_channel_unix_get_fd (source);

  if (condition & G_IO_IN)
    {
      read_and_drop_on_floor (fd, echo_return_size, cd->vtable->fake_malloc_overhead);
    }
  else if (condition & G_IO_OUT)
    {
      cd->iterations += 1;
      if (cd->iterations >= N_ITERATIONS)
        {
          g_printerr ("\nCompleted %d iterations\n", N_ITERATIONS);
          g_main_loop_quit (cd->loop);
        }
      else if (cd->iterations % (N_ITERATIONS/N_PROGRESS_UPDATES) == 0)
        {
          g_printerr ("%d%% ", (int) (cd->iterations/(double)N_ITERATIONS * 100.0));
        }
      
      write_junk (fd, echo_call_size, cd->vtable->fake_malloc_overhead);
    }
  else
    {
      g_printerr ("Unexpected IO condition in client thread\n");
      exit (1);
    }

  return TRUE;
}

static void*
plain_sockets_thread_func (void *data)
{
  GMainContext *context;
  ClientData cd;
  int fd;
  struct sockaddr_un addr;
  GIOChannel *channel;
  GSource *gsource;
  
  g_printerr ("Starting client thread %p\n",
              g_thread_self());
  
  fd = socket (PF_UNIX, SOCK_STREAM, 0);
  
  if (fd < 0)
    {
      g_printerr ("Failed to create socket: %s",
                  strerror (errno)); 
      exit (1);
    }

  _DBUS_ZERO (addr);
  addr.sun_family = AF_UNIX;

#ifdef HAVE_ABSTRACT_SOCKETS
  /* remember that abstract names aren't nul-terminated so we rely
   * on sun_path being filled in with zeroes above.
   */
  addr.sun_path[0] = '\0'; /* this is what says "use abstract" */
  strncpy (&addr.sun_path[1], plain_sockets_address, _DBUS_MAX_SUN_PATH_LENGTH - 2);
  /* _dbus_verbose_bytes (addr.sun_path, sizeof (addr.sun_path)); */
#else /* HAVE_ABSTRACT_SOCKETS */
  strncpy (addr.sun_path, plain_sockets_address, _DBUS_MAX_SUN_PATH_LENGTH - 1);
#endif /* ! HAVE_ABSTRACT_SOCKETS */
  
  if (connect (fd, (struct sockaddr*) &addr, sizeof (addr)) < 0)
    {      
      g_printerr ("Failed to connect to socket %s: %s",
                  plain_sockets_address, strerror (errno));
      exit (1);
    }

  context = g_main_context_new ();

  cd.iterations = 1;
  cd.loop = g_main_loop_new (context, FALSE);
  cd.vtable = data;

  channel = g_io_channel_unix_new (fd);
  
  gsource = g_io_create_watch (channel,
                               G_IO_IN | G_IO_OUT |
                               G_IO_ERR | G_IO_HUP | G_IO_NVAL | G_IO_PRI);

  g_source_set_callback (gsource,
                         (GSourceFunc)plain_sockets_client_side_watch,
                         &cd, NULL);

  g_source_attach (gsource, context);

  g_io_channel_unref (channel);

  g_printerr ("Client thread writing to prime pingpong\n");
  write_junk (fd, echo_call_size, cd.vtable->fake_malloc_overhead);
  g_printerr ("Client thread done writing primer\n");

  g_printerr ("Client thread entering main loop\n");
  g_main_loop_run (cd.loop);
  g_printerr ("Client thread %p exiting main loop\n",
              g_thread_self());

  g_source_destroy (gsource);
  
  close (fd);
  
  g_main_loop_unref (cd.loop);
  g_main_context_unref (context);

  return NULL;
}

static void
plain_sockets_main_loop_run (GMainLoop *loop)
{
  g_main_loop_run (loop);
}

static const ProfileRunVTable plain_sockets_vtable = {
  "plain sockets",
  FALSE,
  plain_sockets_init_server,
  plain_sockets_stop_server,
  plain_sockets_thread_func,
  plain_sockets_main_loop_run
};

static const ProfileRunVTable plain_sockets_with_malloc_vtable = {
  "plain sockets with malloc overhead",
  TRUE,
  plain_sockets_init_server,
  plain_sockets_stop_server,
  plain_sockets_thread_func,
  plain_sockets_main_loop_run
};

static double
do_profile_run (const ProfileRunVTable *vtable)
{
  GTimer *timer;
  int i;
  double secs;
  ServerData sd;
  void *server;

  g_printerr ("Profiling %s\n", vtable->name);
  
  sd.handled = 0;
  sd.n_clients = 0;
  sd.loop = g_main_loop_new (NULL, FALSE);
  sd.vtable = vtable;

  server = (* vtable->init_server) (&sd);
  
  for (i = 0; i < N_CLIENT_THREADS; i++)
    {
      g_thread_create (vtable->client_thread_func, (void*) vtable, FALSE, NULL);
    }

  timer = g_timer_new ();
  
  g_printerr ("Server thread %p entering main loop\n",
              g_thread_self());
  (* vtable->main_loop_run_func) (sd.loop);
  g_printerr ("Server thread %p exiting main loop\n",
              g_thread_self());

  secs = g_timer_elapsed (timer, NULL);
  g_timer_destroy (timer);

  g_printerr ("%s: %g seconds, %d round trips, %f seconds per pingpong\n",
              vtable->name, secs, sd.handled, secs/sd.handled);

  (* vtable->stop_server) (&sd, server);
  
  g_main_loop_unref (sd.loop);

  return secs;
}

static void
print_result (const ProfileRunVTable *vtable,
              double            seconds,
              double            baseline)
{
  g_printerr (" %g times slower for %s (%g seconds, %f per iteration)\n",
              seconds/baseline, vtable->name,
              seconds, seconds / N_ITERATIONS);
}
#endif

int
main (int argc, char *argv[])
{
#ifndef TEST_PROFILE_DISABLED
  g_thread_init (NULL);
  dbus_g_thread_init ();

#ifndef DBUS_DISABLE_ASSERT
  g_printerr ("You should probably --disable-asserts before you profile as they have noticeable overhead\n");
#endif
  
#if DBUS_ENABLE_VERBOSE_MODE
  g_printerr ("You should probably --disable-verbose-mode before you profile as verbose has noticeable overhead\n");
#endif
  
  payload = g_malloc (PAYLOAD_SIZE);

  /* The actual size of the DBusMessage on the wire, as of Nov 23 2004,
   * without the payload
   */
  echo_call_size = 140 + PAYLOAD_SIZE;
  echo_return_size = 32;

  if (argc > 1 && strcmp (argv[1], "plain_sockets") == 0)
    do_profile_run (&plain_sockets_vtable);
  else if (argc > 1 && strcmp (argv[1], "plain_sockets_with_malloc") == 0)
    do_profile_run (&plain_sockets_with_malloc_vtable);
  else if (argc > 1 && strcmp (argv[1], "no_bus") == 0)
    do_profile_run (&no_bus_vtable);
  else if (argc > 1 && strcmp (argv[1], "with_bus") == 0)
    do_profile_run (&with_bus_vtable);
  else if (argc > 1 && strcmp (argv[1], "all") == 0)
    {
      double e1, e2, e3, e4;

      e1 = do_profile_run (&plain_sockets_vtable);
      e2 = do_profile_run (&plain_sockets_with_malloc_vtable);
      e3 = do_profile_run (&no_bus_vtable);
      e4 = do_profile_run (&with_bus_vtable);

      g_printerr ("Baseline plain sockets time %g seconds for %d iterations\n",
                  e1, N_ITERATIONS);
      print_result (&plain_sockets_vtable, e1, e1);
      print_result (&plain_sockets_with_malloc_vtable, e2, e1);
      print_result (&no_bus_vtable, e3, e1);
      print_result (&with_bus_vtable, e4, e1);
    }
  else
    {
      g_printerr ("Specify profile type plain_sockets, plain_sockets_with_malloc, no_bus, with_bus, all\n");
      exit (1);
    }

  /* Make valgrind happy */
  dbus_shutdown ();
#endif  /* TEST_PROFILE_DISABLED */
  return 0;
}
