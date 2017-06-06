/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
 * Developed by Havoc Pennington, some code in here borrowed from 
 * gnome-name-server and libgnorba (Elliot Lee)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */


/*
 * This is the per-user configuration daemon.
 * (has debug crap in it now)
 */

#include <config.h>

#include "gconf-internals.h"
#include "gconf-sources.h"
#include "gconf-listeners.h"
#include "gconf-locale.h"
#include "gconf-schema.h"
#include "gconf.h"
#include "gconfd.h"
#include "gconf-database.h"

#ifdef HAVE_DBUS
#include "gconf-database-dbus.h"
#include "gconfd-dbus.h"
#endif

#ifdef HAVE_CORBA
#include <orbit/orbit.h>

#include "GConfX.h"
#endif

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <locale.h>

#include <dbus/dbus-glib-lowlevel.h>

#ifdef G_OS_WIN32
#include <io.h>
#include <conio.h>
#define _WIN32_WINNT 0x0500 
#include <windows.h>

static int
fsync (int fd)
{
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  DWORD err;

  if (h == INVALID_HANDLE_VALUE)
    {
      errno = EBADF;
      return -1;
    }

  if (!FlushFileBuffers (h))
    {
      err = GetLastError ();
      switch (err)
        {
           case ERROR_INVALID_HANDLE:
             errno = EINVAL;
             break;

           default:
             errno = EIO;
        }
      return -1;
    }

  return 0;
}
#endif

/* This makes hash table safer when debugging */
#ifndef GCONF_ENABLE_DEBUG
#define safe_g_hash_table_insert g_hash_table_insert
#else
static void
safe_g_hash_table_insert(GHashTable* ht, gpointer key, gpointer value)
{
  gpointer oldkey = NULL, oldval = NULL;

  if (g_hash_table_lookup_extended(ht, key, &oldkey, &oldval))
    {
      gconf_log(GCL_WARNING, "Hash key `%s' is already in the table!",
                (gchar*) key);
      return;
    }
  else
    {
      g_hash_table_insert(ht, key, value);
    }
}
#endif

/*
 * Declarations
 */

static void     gconf_main            (void);
static gboolean gconf_main_is_running (void);

#ifdef HAVE_CORBA
static void logfile_save (void);
static void logfile_read (void);
static void log_client_add (const ConfigListener client);
static void log_client_remove (const ConfigListener client);

static void    add_client            (const ConfigListener  client);
static void    remove_client         (const ConfigListener  client);
static GSList *list_clients          (void);
static void    log_clients_to_string (GString              *str);
static void    drop_old_clients      (void);
static guint   client_count          (void);
#endif

static void    enter_shutdown          (void);

static void                 init_databases (void);
static void                 shutdown_databases (void);
#ifdef HAVE_DBUS
static void                 reload_databases (void);
#endif
static void                 set_default_database (GConfDatabase* db);
static void                 register_database (GConfDatabase* db);
static void                 unregister_database (GConfDatabase* db);
static GConfDatabase*       lookup_database (GSList *addresses);
static void                 drop_old_databases (void);
static gboolean             no_databases_in_use (void);

/*
 * Flag indicating that we are shutting down, so return errors
 * on any attempted operation. We do this instead of unregistering with
 * OAF or deactivating the server object, because we want to avoid
 * another gconfd starting up before we finish shutting down.
 */

static gboolean in_shutdown = FALSE;

/*
 * Flag indicating we received a SIGHUP and we should reaload
 * all sources during the next periodic_cleanup()
 */
static gboolean need_db_reload = FALSE;

#ifdef HAVE_CORBA
/* 
 * CORBA goo
 */

static ConfigServer2 server = CORBA_OBJECT_NIL;
static PortableServer_POA the_poa;
static GConfLock *daemon_lock = NULL;

static ConfigDatabase
gconfd_get_default_database(PortableServer_Servant servant,
                            CORBA_Environment* ev);

static ConfigDatabase
gconfd_get_database(PortableServer_Servant servant,
                    const CORBA_char* address,
                    CORBA_Environment* ev);

static ConfigDatabase
gconfd_get_database_for_addresses (PortableServer_Servant           servant,
				   const ConfigServer2_AddressList *addresses,
				   CORBA_Environment               *ev);

static void
gconfd_add_client (PortableServer_Servant servant,
                   const ConfigListener client,
                   CORBA_Environment *ev);

static void
gconfd_remove_client (PortableServer_Servant servant,
                      const ConfigListener client,
                      CORBA_Environment *ev);

static CORBA_long
gconfd_ping(PortableServer_Servant servant, CORBA_Environment *ev);

static void
gconfd_shutdown(PortableServer_Servant servant, CORBA_Environment *ev);

static PortableServer_ServantBase__epv base_epv = {
  NULL,
  NULL,
  NULL
};

static POA_ConfigServer__epv server_epv = { 
  NULL,
  gconfd_get_default_database,
  gconfd_get_database,
  gconfd_add_client,
  gconfd_remove_client,
  gconfd_ping,
  gconfd_shutdown
};

static POA_ConfigServer2__epv server2_epv = { 
  NULL,
  gconfd_get_database_for_addresses
};

static POA_ConfigServer2__vepv poa_server_vepv = { &base_epv, &server_epv, &server2_epv };
static POA_ConfigServer2 poa_server_servant = { NULL, &poa_server_vepv };

static ConfigDatabase
gconfd_get_default_database(PortableServer_Servant servant,
                            CORBA_Environment* ev)
{
  GConfDatabase *db;

  if (gconfd_check_in_shutdown (ev))
    return CORBA_OBJECT_NIL;
  
  db = lookup_database (NULL);

  if (db)
    return CORBA_Object_duplicate (db->objref, ev);
  else
    return CORBA_OBJECT_NIL;
}

static ConfigDatabase
gconfd_get_database(PortableServer_Servant servant,
                    const CORBA_char* address,
                    CORBA_Environment* ev)
{
  GConfDatabase *db;
  GSList *addresses;
  GError* error = NULL;  

  if (gconfd_check_in_shutdown (ev))
    return CORBA_OBJECT_NIL;
  
  addresses = g_slist_append (NULL, (char *) address);
  db = gconfd_obtain_database (addresses, &error);
  g_slist_free (addresses);

  if (db != NULL)
    return CORBA_Object_duplicate (db->objref, ev);

  gconf_set_exception (&error, ev);

  return CORBA_OBJECT_NIL;
}

static ConfigDatabase
gconfd_get_database_for_addresses (PortableServer_Servant           servant,
				   const ConfigServer2_AddressList *seq,
				   CORBA_Environment               *ev)
{
  GConfDatabase  *db;
  GSList         *addresses = NULL;
  GError         *error = NULL;  
  int             i;

  if (gconfd_check_in_shutdown (ev))
    return CORBA_OBJECT_NIL;

  i = 0;
  while (i < seq->_length)
    addresses = g_slist_append (addresses, seq->_buffer [i++]);

  db = gconfd_obtain_database (addresses, &error);

  g_slist_free (addresses);

  if (db != NULL)
    return CORBA_Object_duplicate (db->objref, ev);

  gconf_set_exception (&error, ev);

  return CORBA_OBJECT_NIL;
}

static void
gconfd_add_client (PortableServer_Servant servant,
                   const ConfigListener client,
                   CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
  
  add_client (client);
}

static void
gconfd_remove_client (PortableServer_Servant servant,
                      const ConfigListener client,
                      CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
  
  remove_client (client);
}

static CORBA_long
gconfd_ping(PortableServer_Servant servant, CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return 0;
  
  return getpid();
}

static void
gconfd_shutdown(PortableServer_Servant servant, CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_log(GCL_DEBUG, _("Shutdown request received"));

  gconfd_main_quit();
}
#endif /* HAVE_CORBA */

/*
 * Main code
 */

/* This needs to be called before we register with OAF
 */
static GConfSources *
gconf_server_get_default_sources(void)
{
  GSList* addresses;
  GList* tmp;
  gboolean have_writable = FALSE;
  gchar* conffile;
  GConfSources* sources = NULL;
  GError* error = NULL;
  
  conffile = g_strconcat(GCONF_CONFDIR, "/path", NULL);

  addresses = gconf_load_source_path(conffile, NULL);

  g_free(conffile);

#ifdef GCONF_ENABLE_DEBUG
  /* -- Debug only */
  
  if (addresses == NULL)
    {
      gconf_log(GCL_DEBUG, _("gconfd compiled with debugging; trying to load gconf.path from the source directory"));
      conffile = g_strconcat(GCONF_SRCDIR, "/gconf/gconf.path", NULL);
      addresses = gconf_load_source_path(conffile, NULL);
      g_free(conffile);
    }

  /* -- End of Debug Only */
#endif

  if (addresses == NULL)
    {      
#ifndef G_OS_WIN32
      const char *home = g_get_home_dir ();
#else
      const char *home = _gconf_win32_get_home_dir ();
#endif

      /* Try using the default address xml:readwrite:$(HOME)/.gconf */
      addresses = g_slist_append(addresses, g_strconcat("xml:readwrite:", home, "/.gconf", NULL));

      gconf_log(GCL_DEBUG, _("No configuration files found. Trying to use the default configuration source `%s'"), (char *)addresses->data);
    }
  
  if (addresses == NULL)
    {
      /* We want to stay alive but do nothing, because otherwise every
         request would result in another failed gconfd being spawned.  
      */
      gconf_log(GCL_ERR, _("No configuration sources in the source path. Configuration won't be saved; edit %s%s"), GCONF_CONFDIR, "/path");
      /* don't request error since there aren't any addresses */
      sources = gconf_sources_new_from_addresses(NULL, NULL);

      return sources;
    }
  else
    {
      sources = gconf_sources_new_from_addresses(addresses, &error);

      if (error != NULL)
        {
          gconf_log(GCL_ERR, _("Error loading some configuration sources: %s"),
                    error->message);

          g_error_free(error);
          error = NULL;
        }
      
      gconf_address_list_free(addresses);

      g_assert(sources != NULL);

      if (sources->sources == NULL)
        gconf_log(GCL_ERR, _("No configuration source addresses successfully resolved. Can't load or store configuration data"));
    
      tmp = sources->sources;

      while (tmp != NULL)
        {
          if (((GConfSource*)tmp->data)->flags & GCONF_SOURCE_ALL_WRITEABLE)
            {
              have_writable = TRUE;
              break;
            }

          tmp = g_list_next(tmp);
        }

      /* In this case, some sources may still return TRUE from their writable() function */
      if (!have_writable)
        gconf_log(GCL_WARNING, _("No writable configuration sources successfully resolved. May be unable to save some configuration changes"));

      return sources;
    }
}

static void
gconf_server_load_sources(void)
{
  GConfSources* sources;

  sources = gconf_server_get_default_sources();

  /* Install the sources as the default database */
  set_default_database (gconf_database_new(sources));
}

/*
 * Signal handlers should not log debug messages as this code is non-reentrant.
 * Please avoid calling gconf_log in this function.
 */
static void
signal_handler (int signo)
{
  static gint in_fatal = 0;

  /* avoid loops */
  if (in_fatal > 0)
    return;
  
  ++in_fatal;
  
  switch (signo) {
  case SIGFPE:
#ifdef SIGPIPE
  case SIGPIPE:
#endif
    /* Go ahead and try the full cleanup on these,
     * though it could well not work out very well.
     */
    enter_shutdown ();

    /* let the fatal signals interrupt us */
    --in_fatal;
    
    if (gconf_main_is_running ())
      gconfd_main_quit ();
    
    break;

  case SIGTERM:
    enter_shutdown ();

    /* let the fatal signals interrupt us */
    --in_fatal;
    
    if (gconf_main_is_running ())
      gconfd_main_quit ();
    break;

#ifdef SIGHUP
  case SIGHUP:
    --in_fatal;

    /* reload sources during next periodic_cleanup() */
    need_db_reload = TRUE;
    break;
#endif

#ifdef SIGUSR1
  case SIGUSR1:
    --in_fatal;
    
    /* it'd be nice to log a message here but it's not very safe, so */
    gconf_log_debug_messages = !gconf_log_debug_messages;
    break;
#endif
    
  default:
#ifndef HAVE_SIGACTION
    signal (signo, signal_handler);
#endif
    break;
  }
}

#ifdef HAVE_CORBA
PortableServer_POA
gconf_get_poa (void)
{
  return the_poa;
}
#endif

#ifdef HAVE_CORBA
static const char *
get_introspection_xml (void)
{
  return "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"\n"
         "\"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">\n"
         "<node>\n"
         "  <interface name=\"org.freedesktop.DBus.Introspectable\">\n"
         "    <method name=\"Introspect\">\n"
         "      <arg name=\"introspection_xml\" direction=\"out\" type=\"s\"/>\n"
         "    </method>\n"
         "  </interface>\n"
         "  <interface name=\"org.gnome.GConf\">\n"
         "    <method name=\"GetIOR\">\n"
         "      <arg name=\"ior\" direction=\"out\" type=\"s\"/>\n"
         "    </method>\n"
         "  </interface>\n"
         "</node>\n";
}

static DBusHandlerResult
bus_message_handler (DBusConnection *connection,
                     DBusMessage    *message,
                     void           *user_data)
{
  DBusMessage *reply;

  reply = NULL;

  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    {
      gconfd_main_quit ();
      return DBUS_HANDLER_RESULT_HANDLED;
    }
  else if (dbus_message_is_method_call (message,
                                        "org.freedesktop.DBus.Introspectable",
                                        "Introspect"))
    {
      const char *introspection_xml;

      introspection_xml = get_introspection_xml ();

      reply = dbus_message_new_method_return (message);
      dbus_message_append_args (reply, DBUS_TYPE_STRING, &introspection_xml,
                                DBUS_TYPE_INVALID);

    }
  else if (dbus_message_is_method_call (message,
                                        "org.gnome.GConf",
                                        "GetIOR"))
    {
      const char *ior;

      ior = gconf_get_daemon_ior ();

      reply = dbus_message_new_method_return (message);
      dbus_message_append_args (reply, DBUS_TYPE_STRING, &ior, DBUS_TYPE_INVALID);
    }

  if (reply != NULL)
    {
      dbus_connection_send (connection, reply, NULL);
      dbus_message_unref (reply);
      return DBUS_HANDLER_RESULT_HANDLED;
    }

  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusConnection *
get_on_d_bus (void)
{
  DBusConnection *connection;
  DBusError bus_error;
  int result;

  dbus_error_init (&bus_error);
  connection = dbus_bus_get (DBUS_BUS_SESSION, &bus_error);

  if (dbus_error_is_set (&bus_error))
    {
      gconf_log (GCL_ERR, _("Could not connect to session bus: %s"), bus_error.message);
      dbus_error_free (&bus_error);
      return NULL;
    }

  dbus_connection_setup_with_g_main (connection, NULL);

  if (!dbus_connection_add_filter (connection, (DBusHandleMessageFunction)
                                  bus_message_handler, NULL, NULL))
    {
      dbus_connection_unref (connection);
      return NULL;
    }

  dbus_connection_set_exit_on_disconnect (connection, FALSE);

  result = dbus_bus_request_name (connection, "org.gnome.GConf",
                                  0, &bus_error);

  if (dbus_error_is_set (&bus_error))
    {
      gconf_log (GCL_WARNING,
                 _("Failed to get bus name for daemon, exiting: %s"),
                 bus_error.message);
      dbus_error_free (&bus_error);
    }

  if (result != DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER)
    {
      dbus_connection_unref (connection);
      return NULL;
    }

  return connection;
}
#endif

#ifdef ENABLE_DEFAULTS_SERVICE
/* listen on system bus for defaults changes */

static DBusHandlerResult
system_bus_message_handler (DBusConnection *connection,
			    DBusMessage    *message,
			    void           *user_data)
{
  DBusMessage *reply;

  reply = NULL;

  if (dbus_message_is_signal (message,
			      "org.gnome.GConf.Defaults",
                              "SystemSet"))
    {
      DBusError bus_error;
      char **keys;
      int n_keys;

      dbus_error_init (&bus_error);
      if (dbus_message_get_args (message, &bus_error,
				 DBUS_TYPE_ARRAY, DBUS_TYPE_STRING, &keys, &n_keys,
				 DBUS_TYPE_INVALID))
	{
	  char **key;
	  GConfSources *system_sources;
	  GSList addresses;

	  gconf_log (GCL_DEBUG, "System defaults changed.  Notifying.");

	  addresses.data = "xml:merged:" GCONF_ETCDIR "/gconf.xml.system";
	  addresses.next = NULL;
	  system_sources = gconf_sources_new_from_addresses (&addresses, NULL);

	  gconfd_clear_cache_for_sources (system_sources);

	  for (key = keys; *key; key++)
	    gconfd_notify_other_listeners (NULL, system_sources, *key);

	  gconf_sources_free (system_sources);

	  dbus_free_string_array (keys);
	}
      else
        {
	  gconf_log (GCL_DEBUG, "SystemSet signal received, but error getting message: %s", bus_error.message);
	}
      dbus_error_free (&bus_error);

      return DBUS_HANDLER_RESULT_HANDLED;
    }

  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusConnection *
get_on_system_bus (void)
{
  DBusConnection *connection;
  DBusError bus_error;

  dbus_error_init (&bus_error);
  connection = dbus_bus_get (DBUS_BUS_SYSTEM, &bus_error);

  if (dbus_error_is_set (&bus_error))
    {
      gconf_log (GCL_ERR, _("Could not connect to system bus: %s"), bus_error.message);
      dbus_error_free (&bus_error);
      return NULL;
    }

  dbus_connection_setup_with_g_main (connection, NULL);

  dbus_bus_add_match (connection, "type='signal',interface='org.gnome.GConf.Defaults'", &bus_error);
  dbus_connection_flush(connection);
  if (dbus_error_is_set (&bus_error))
    {
      gconf_log (GCL_DEBUG, "Failed to add signal match to system bus: %s", bus_error.message);
      dbus_connection_unref (connection);
      return NULL;
    }

  if (!dbus_connection_add_filter (connection, (DBusHandleMessageFunction)
                                   system_bus_message_handler, NULL, NULL))
    {
      gconf_log (GCL_DEBUG, "Failed to add message filter to system bus.");
      dbus_connection_unref (connection);
      return NULL;
    }

  return connection;
}
#endif  /* ENABLE_DEFAULTS_SERVICE */

#ifdef G_OS_WIN32

static void
wait_console_window (void)
{
  SetConsoleTitle ("GConf daemon exiting. Type any character to close this window.");
  printf ("\n"
	  "(GConf daemon exiting. Type any character to close this window)\n");
  _getch ();
}

#endif

int 
main(int argc, char** argv)
{
#ifdef HAVE_SIGACTION
  struct sigaction act;
  sigset_t empty_mask;
  sigset_t full_mask;
#endif

#ifdef HAVE_CORBA
  CORBA_Environment ev;
  CORBA_ORB orb;
  gchar* ior;
  DBusConnection *connection;
#endif

  int dev_null_fd;
  int exit_code = 0;
  int write_byte_fd;

  _gconf_init_i18n ();
  setlocale (LC_ALL, "");
  textdomain (GETTEXT_PACKAGE);
  
  /* Now this is an argument parser */
  if (argc > 1)
    write_byte_fd = atoi (argv[1]);
  else
    write_byte_fd = -1;
  
  /* This is so we don't prevent unmounting of devices. We divert
   * all messages to syslog
   */
  if (g_chdir ("/") < 0)
    {
       g_printerr ("Could not change to root directory: %s\n",
		   g_strerror (errno));
       exit (1);
    }

  if (!g_getenv ("GCONF_DEBUG_OUTPUT"))
    {
      dev_null_fd = open (DEV_NULL, O_RDWR);
      if (dev_null_fd >= 0)
        {
	  dup2 (dev_null_fd, 0);
	  dup2 (dev_null_fd, 1);
	  dup2 (dev_null_fd, 2);
	}
    }
  else
    {
      gconf_log_debug_messages = TRUE;
#ifdef G_OS_WIN32
      if (fileno (stdout) != -1 &&
	  _get_osfhandle (fileno (stdout)) != -1)
	{
	  /* stdout is fine, presumably redirected to a file or pipe */
	}
      else
	{
	  int allocated_new_console = FALSE;

	  typedef BOOL (* WINAPI AttachConsole_t) (DWORD);

	  AttachConsole_t p_AttachConsole =
	    (AttachConsole_t) GetProcAddress (GetModuleHandle ("kernel32.dll"), "AttachConsole");

	  if (p_AttachConsole != NULL)
	    {
	      if (!AttachConsole (ATTACH_PARENT_PROCESS))
		{
		  if (AllocConsole ())
		    allocated_new_console = TRUE;
		}

	      freopen ("CONOUT$", "w", stdout);
	      dup2 (fileno (stdout), 1);
	      freopen ("CONOUT$", "w", stderr);
	      dup2 (fileno (stderr), 2);

	      if (allocated_new_console)
		{
		  SetConsoleTitle ("GConf daemon debugging output. You can minimize this window, but don't close it.");
		  printf ("You asked for debugging output by setting the GCONF_DEBUG_OUTPUT\n"
			  "environment variable, so here it is.\n"
			  "\n");
		  atexit (wait_console_window);
		}
	    }
	}
#endif
    }
  
  umask (022);
  
  gconf_set_daemon_mode(TRUE);
  
  gconf_log (GCL_DEBUG, _("starting (version %s), pid %u user '%s'"), 
             VERSION, (guint)getpid(), g_get_user_name());

#ifdef GCONF_ENABLE_DEBUG
  gconf_log (GCL_DEBUG, "GConf was built with debugging features enabled");
#endif
  
  /* Session setup */
#ifdef HAVE_SIGACTION
  sigfillset (&full_mask);
  sigprocmask (SIG_UNBLOCK, &full_mask, NULL);

  sigemptyset (&empty_mask);
  act.sa_handler = signal_handler;
  act.sa_mask    = empty_mask;
  act.sa_flags   = 0;
  sigaction (SIGTERM,  &act, NULL);
  sigaction (SIGHUP,  &act, NULL);
  sigaction (SIGUSR1,  &act, NULL);

  act.sa_handler = SIG_IGN;
  sigaction (SIGINT, &act, NULL);
#else
  signal (SIGTERM, signal_handler);
#ifdef SIGHUP
  signal (SIGHUP,  signal_handler);
#endif
#ifdef SIGUSR1
  signal (SIGUSR1,  signal_handler);
#endif
#endif

#ifdef HAVE_CORBA
  CORBA_exception_init(&ev);
#endif

  init_databases ();

#ifdef HAVE_DBUS
  if (!gconfd_dbus_init ())
    return 1;
#endif

#ifdef HAVE_CORBA
  orb = gconf_orb_get ();
  
  POA_ConfigServer2__init (&poa_server_servant, &ev);
  
  the_poa = (PortableServer_POA)CORBA_ORB_resolve_initial_references(orb, "RootPOA", &ev);
  PortableServer_POAManager_activate(PortableServer_POA__get_the_POAManager(the_poa, &ev), &ev);

  server = PortableServer_POA_servant_to_reference(the_poa,
                                                   &poa_server_servant,
                                                   &ev);
  if (CORBA_Object_is_nil(server, &ev)) 
    {
      gconf_log(GCL_ERR, _("Failed to get object reference for ConfigServer"));
      return 1;
    }

  /* Needs to be done before loading sources */
  ior = CORBA_ORB_object_to_string (orb, server, &ev);
  gconf_set_daemon_ior (ior);
  CORBA_free (ior);

  connection = get_on_d_bus ();

  if (connection != NULL)
    {
      /* This loads backends and so on. It needs to be done before
       * we can handle any requests, so before we hit the
       * main loop. if daemon_lock == NULL we won't hit the
       * main loop.
       */
      gconf_server_load_sources ();
    }
#endif

#ifdef HAVE_DBUS
  gconf_server_load_sources ();
#endif

#ifdef HAVE_CORBA
  /* notify caller that we're done either getting the lock
   * or not getting it
   */
  if (write_byte_fd >= 0)
    {
      char buf[1] = { 'g' };
      if (write (write_byte_fd, buf, 1) != 1)
        {
          gconf_log (GCL_ERR, _("Failed to write byte to pipe file descriptor %d so client program may hang: %s"), write_byte_fd, g_strerror (errno));
        }
      
      close (write_byte_fd);
    }
  
  if (connection == NULL)
    {
      enter_shutdown ();
      shutdown_databases ();
      
      return 1;
    }  

  /* Read saved log file, if any */
  logfile_read ();
#endif

#ifdef ENABLE_DEFAULTS_SERVICE 
  get_on_system_bus ();
#endif

  gconf_main ();

  if (in_shutdown)
    exit_code = 1; /* means someone already called enter_shutdown() */
  
  /* This starts bouncing all incoming requests (and we aren't running
   * the main loop anyway, so they won't get processed)
   */
  enter_shutdown ();

#ifdef HAVE_CORBA
  /* Save current state in logfile (may compress the logfile a good
   * bit)
   */
  logfile_save ();
#endif
  
  shutdown_databases ();

  gconfd_locale_cache_drop ();

#ifdef HAVE_CORBA
  if (daemon_lock)
    {
      GError *err;

      err = NULL;
      gconf_release_lock (daemon_lock, &err);
      if (err != NULL)
        {
          gconf_log (GCL_WARNING, _("Error releasing lockfile: %s"),
                     err->message);
          g_error_free (err);
        }
    }

  daemon_lock = NULL;
#endif
  
  gconf_log (GCL_DEBUG, _("Exiting"));

  return exit_code;
}

/*
 * Main loop
 */

static GSList* main_loops = NULL;
static guint timeout_id = 0;
static gboolean need_log_cleanup = FALSE;

static gboolean
periodic_cleanup_timeout(gpointer data)
{  
  if (need_db_reload)
    {
      gconf_log (GCL_INFO, _("SIGHUP received, reloading all databases"));

      need_db_reload = FALSE;
#ifdef HAVE_CORBA
      logfile_save ();
      shutdown_databases ();
      init_databases ();
      gconf_server_load_sources ();
      logfile_read ();
#endif
#ifdef HAVE_DBUS
      reload_databases ();
#endif
    }
  
  gconf_log (GCL_DEBUG, "Performing periodic cleanup, expiring cache cruft");
  
#ifdef HAVE_CORBA
  drop_old_clients ();
#endif
  drop_old_databases ();

#ifdef HAVE_DBUS
  if (no_databases_in_use () && gconfd_dbus_client_count () == 0)
#else
  if (no_databases_in_use () && client_count () == 0)
#endif
    {
      gconf_log (GCL_INFO, _("GConf server is not in use, shutting down."));
      gconfd_main_quit ();
      return FALSE;
    }
  
  /* expire old locale cache entries */
  gconfd_locale_cache_expire ();

#ifdef HAVE_CORBA
  if (!need_log_cleanup)
    {
      gconf_log (GCL_DEBUG, "No log file saving needed in periodic cleanup handler");
      return TRUE;
    }
  
  /* Compress the running state file */
  logfile_save ();
#endif

  need_log_cleanup = FALSE;
  
  return TRUE;
}

void
gconfd_need_log_cleanup (void)
{
  need_log_cleanup = TRUE;
}

static void
gconf_main(void)
{
  GMainLoop* loop;

  loop = g_main_loop_new (NULL, TRUE);

  if (main_loops == NULL)
    {
      gulong timeout_len = 60*0.5; /* 60 s/min * .5 min */
      
      g_assert(timeout_id == 0);
      timeout_id = g_timeout_add_seconds (timeout_len,
                                          periodic_cleanup_timeout,
                                          NULL);

    }
  
  main_loops = g_slist_prepend(main_loops, loop);

  g_main_loop_run (loop);

  main_loops = g_slist_remove(main_loops, loop);

  if (main_loops == NULL)
    {
      g_assert(timeout_id != 0);
      g_source_remove(timeout_id);
      timeout_id = 0;
    }
  
  g_main_loop_unref (loop);
}

void
gconfd_main_quit(void)
{
  g_return_if_fail(main_loops != NULL);

  g_main_loop_quit (main_loops->data);
}

static gboolean
gconf_main_is_running (void)
{
  return main_loops != NULL;
}

/*
 * Database storage
 */

static GList* db_list = NULL;
static GHashTable* dbs_by_addresses = NULL;
static GConfDatabase *default_db = NULL;

static void
init_databases (void)
{
  gconfd_need_log_cleanup ();
  
  g_assert(db_list == NULL);
  g_assert(dbs_by_addresses == NULL);
  
  dbs_by_addresses = g_hash_table_new (g_str_hash, g_str_equal);
}

static void
set_default_database (GConfDatabase* db)
{
  gconfd_need_log_cleanup ();
  
  default_db = db;

  register_database (db);
}

static void
register_database (GConfDatabase *db)
{
  gconfd_need_log_cleanup ();
  
  if (db->sources->sources)
    safe_g_hash_table_insert (dbs_by_addresses,
			      (char *) gconf_database_get_persistent_name (db),
			      db);
  
  db_list = g_list_prepend (db_list, db);
}

static void
unregister_database (GConfDatabase *db)
{
  gconfd_need_log_cleanup ();
  
  if (db->sources->sources)
    {
      g_hash_table_remove (dbs_by_addresses,
			   gconf_database_get_persistent_name (db));
    }

  db_list = g_list_remove (db_list, db);

  gconf_database_free (db);
}

static GConfDatabase*
lookup_database (GSList *addresses)
{
  GConfDatabase *retval;
  char          *key;

  if (addresses == NULL)
    return default_db;

  key = gconf_address_list_get_persistent_name (addresses);

  retval = g_hash_table_lookup (dbs_by_addresses, key);

  g_free (key);

  return retval;
}

GConfDatabase *
gconfd_obtain_database (GSList  *addresses,
                        GError **err)
{
  GConfSources* sources;
  GError* error = NULL;
  GConfDatabase *db;

  db = lookup_database (addresses);

  if (db)
    return db;

  sources = gconf_sources_new_from_addresses(addresses, &error);

  if (error != NULL)
    {
      if (err)
        *err = error;
      else
        g_error_free (error);

      return NULL;
    }
  
  if (sources == NULL)
    return NULL;

  db = gconf_database_new (sources);

  register_database (db);

  return db;
}

static void
drop_old_databases(void)
{
  GList *tmp_list;
  GList *dead = NULL;
  GTime now;
  
  now = time(NULL);

  gconf_database_drop_dead_listeners (default_db);
  
  tmp_list = db_list;
  while (tmp_list)
    {
      GConfDatabase* db = tmp_list->data;

      if (db == default_db)
	{
	  tmp_list = g_list_next (tmp_list);
	  continue;
	}

      /* Drop any listeners whose clients are gone. */
      gconf_database_drop_dead_listeners (db);
      
      if (db->listeners &&                             /* not already hibernating */
          gconf_listeners_count(db->listeners) == 0 && /* Can hibernate */
#ifdef HAVE_DBUS
          db->listening_clients &&
          g_hash_table_size (db->listening_clients) == 0 &&
#endif
          (now - db->last_access) > (60*20))           /* 20 minutes without access */
        {
          dead = g_list_prepend (dead, db);
        }
      
      tmp_list = g_list_next (tmp_list);
    }

  tmp_list = dead;
  while (tmp_list)
    {
      GConfDatabase* db = tmp_list->data;

      unregister_database (db);
            
      tmp_list = g_list_next (tmp_list);
    }

  g_list_free (dead);
}

static void
shutdown_databases (void)
{
  GList *tmp_list;  

  /* This may be called before we init fully,
   * so check that everything != NULL
   */
  
  tmp_list = db_list;

  while (tmp_list)
    {
      GConfDatabase *db = tmp_list->data;

      gconf_database_free (db);
      
      tmp_list = g_list_next (tmp_list);
    }

  g_list_free (db_list);
  db_list = NULL;

  if (dbs_by_addresses)
    g_hash_table_destroy(dbs_by_addresses);

  dbs_by_addresses = NULL;
  default_db = NULL;
}

#ifdef HAVE_DBUS
static void
reload_databases (void)
{
  GConfSources* sources;
  GList *tmp_list;

  sources = gconf_server_get_default_sources ();
  gconf_database_set_sources (default_db, sources);

  tmp_list = db_list;
  while (tmp_list)
    {
      GConfDatabase* db = tmp_list->data;
      GList *l;
      GConfSource *source;
      GSList *addresses = NULL;
      GError *error = NULL;

      if (db == default_db)
	{
	  tmp_list = g_list_next (tmp_list);
	  continue;
	}

      for (l = db->sources->sources; l != NULL; l = l->next)
        {
          source = l->data;
          addresses = g_slist_prepend (addresses, source->address);
        }

      addresses = g_slist_reverse (addresses);
      sources = gconf_sources_new_from_addresses (addresses, &error);

      if (error == NULL)
        {
          gconf_database_set_sources (db, sources);
        }
      else
        {
          /* if we got an error, keep our old sources -- that's better than
           * nothing */
          g_error_free (error);
        }

      tmp_list = g_list_next (tmp_list);
    }
}
#endif

static gboolean
no_databases_in_use (void)
{
  /* Only the default database still open, and
   * it has no listeners
   */

  if (db_list == NULL)
    return TRUE;

  if (db_list->next == NULL &&
      db_list->data == default_db)
    return gconf_listeners_count (default_db->listeners) == 0;

  return FALSE;
}

void
gconfd_notify_other_listeners (GConfDatabase *modified_db,
			       GConfSources  *modified_sources,
                               const char    *key)
{
  GList *tmp;

  if (!modified_sources)
    return;
  
  tmp = db_list;
  while (tmp != NULL)
    {
      GConfDatabase *db = tmp->data;

      if (db != modified_db)
	{
	  GList *tmp2;

	  tmp2 = modified_sources->sources;
	  while (tmp2)
	    {
	      GConfSource *modified_source = tmp2->data;

	      if (gconf_sources_is_affected (db->sources, modified_source, key))
		{
		  GConfValue  *value;
#ifdef HAVE_CORBA
		  ConfigValue *cvalue;
#endif
		  GError      *error;
		  gboolean     is_default;
		  gboolean     is_writable;

		  error = NULL;
		  value = gconf_database_query_value (db,
						      key,
						      NULL,
						      TRUE,
						      NULL,
						      &is_default,
						      &is_writable,
						      &error);
		  if (error != NULL)
		    {
		      gconf_log (GCL_WARNING,
				 _("Error obtaining new value for `%s': %s"),
				 key, error->message);
		      g_error_free (error);
		      return;
		    }

#if HAVE_CORBA
		  if (value != NULL)
		    {
		      cvalue = gconf_corba_value_from_gconf_value (value);
		      gconf_value_free (value);
		    }
		  else
		    {
		      cvalue = gconf_invalid_corba_value ();
		    }

		  gconf_database_notify_listeners (db,
						   NULL,
						   key,
						   cvalue,
						   is_default,
						   is_writable,
						   FALSE);
		  CORBA_free (cvalue);
#endif
#ifdef HAVE_DBUS
		  gconf_database_dbus_notify_listeners (db,
							NULL,
							key,
							value,
							is_default,
							is_writable,
							FALSE);
#endif
		}

	      tmp2 = tmp2->next;
	    }
	}

      tmp = tmp->next;
    }
}

void
gconfd_clear_cache_for_sources (GConfSources *sources)
{
  GList *tmp;

  tmp = db_list;
  while (tmp != NULL)
    {
      GConfDatabase *db = tmp->data;

      gconf_database_clear_cache_for_sources (db, sources, NULL);

      tmp = tmp->next;
    }
}

/*
 * Cleanup
 */

static void 
enter_shutdown(void)
{
  in_shutdown = TRUE;
}


#ifdef HAVE_CORBA
/* Exceptions */

gboolean
gconf_set_exception(GError** error,
                    CORBA_Environment* ev)
{
  GConfError en;

  if (error == NULL)
    return FALSE;

  if (*error == NULL)
    return FALSE;
  
  en = (*error)->code;

  /* success is not supposed to get set */
  g_return_val_if_fail(en != GCONF_ERROR_SUCCESS, FALSE);
  
  {
    ConfigException* ce;

    ce = ConfigException__alloc();
    g_assert(error != NULL);
    g_assert(*error != NULL);
    g_assert((*error)->message != NULL);
    ce->message = CORBA_string_dup((gchar*)(*error)->message); /* cast const */
      
    switch (en)
      {
      case GCONF_ERROR_FAILED:
        ce->err_no = ConfigFailed;
        break;
      case GCONF_ERROR_NO_PERMISSION:
        ce->err_no = ConfigNoPermission;
        break;
      case GCONF_ERROR_BAD_ADDRESS:
        ce->err_no = ConfigBadAddress;
        break;
      case GCONF_ERROR_BAD_KEY:
        ce->err_no = ConfigBadKey;
        break;
      case GCONF_ERROR_PARSE_ERROR:
        ce->err_no = ConfigParseError;
        break;
      case GCONF_ERROR_CORRUPT:
        ce->err_no = ConfigCorrupt;
        break;
      case GCONF_ERROR_TYPE_MISMATCH:
        ce->err_no = ConfigTypeMismatch;
        break;
      case GCONF_ERROR_IS_DIR:
        ce->err_no = ConfigIsDir;
        break;
      case GCONF_ERROR_IS_KEY:
        ce->err_no = ConfigIsKey;
        break;
      case GCONF_ERROR_NO_WRITABLE_DATABASE:
        ce->err_no = ConfigNoWritableDatabase;        
        break;
      case GCONF_ERROR_IN_SHUTDOWN:
        ce->err_no = ConfigInShutdown;
        break;
      case GCONF_ERROR_OVERRIDDEN:
        ce->err_no = ConfigOverridden;
        break;
      case GCONF_ERROR_LOCK_FAILED:
        ce->err_no = ConfigLockFailed;
        break;

      case GCONF_ERROR_OAF_ERROR:
      case GCONF_ERROR_LOCAL_ENGINE:
      case GCONF_ERROR_NO_SERVER:
      case GCONF_ERROR_SUCCESS:
      default:
        gconf_log (GCL_ERR, "Unhandled error code %d", en);
        g_assert_not_reached();
        break;
      }

    CORBA_exception_set(ev, CORBA_USER_EXCEPTION,
                        ex_ConfigException, ce);

    gconf_log(GCL_DEBUG, _("Returning exception: %s"), (*error)->message);
      
    g_error_free(*error);
    *error = NULL;
      
    return TRUE;
  }
}

gboolean
gconfd_check_in_shutdown (CORBA_Environment *ev)
{
  if (in_shutdown)
    {
      ConfigException* ce;
      
      ce = ConfigException__alloc();
      ce->message = CORBA_string_dup("Configuration server is currently shutting down");
      ce->err_no = ConfigInShutdown;

      CORBA_exception_set(ev, CORBA_USER_EXCEPTION,
                          ex_ConfigException, ce);

      return TRUE;
    }
  else
    return FALSE;
}

/*
 * Logging
 */

/*
   The log file records the current listeners we have registered,
   so we can restore them if we exit and restart.

   Basically:

   1) On startup, we parse any logfile and try to restore the
      listeners contained therein. As we restore each listener (give
      clients a new listener ID) we append a removal of the previous
      daemon's listener and the addition of our own listener to the
      logfile; this means that if we crash and have to restore a
      client's listener a second time, we'll have the client's current
      listener ID. If all goes well we then atomically rewrite the
      parsed logfile with the resulting current state, to keep the logfile
      compact.

   2) While running, we keep a FILE* open and whenever we add/remove
      a listener we write a line to the logfile recording it,
      to keep the logfile always up-to-date.

   3) On normal exit, and also periodically (every hour or so, say) we
      atomically write over the running log with our complete current
      state, to keep the running log from growing without bound.
*/

static void
get_log_names (gchar **logdir, gchar **logfile)
{
#ifndef G_OS_WIN32
      const char *home = g_get_home_dir ();
#else
      const char *home = _gconf_win32_get_home_dir ();
#endif

  *logdir = g_build_filename (home, ".gconfd", NULL);
  *logfile = g_build_filename (*logdir, "saved_state", NULL);
}

static void close_append_handle (void);

static FILE* append_handle = NULL;
static guint append_handle_timeout = 0;

static gboolean
close_append_handle_timeout(gpointer data)
{
  close_append_handle ();

  /* uninstall the timeout */
  append_handle_timeout = 0;
  return FALSE;
}

static gboolean
open_append_handle (GError **err)
{
  if (append_handle == NULL)
    {
      gchar *logdir;
      gchar *logfile;

      get_log_names (&logdir, &logfile);
      
      g_mkdir (logdir, 0700); /* ignore failure, we'll catch failures
			       * that matter on open()
			       */
      
      append_handle = g_fopen (logfile, "a");

      if (append_handle == NULL)
        {
          gconf_set_error (err,
                           GCONF_ERROR_FAILED,
                           _("Failed to open gconfd logfile; won't be able to restore listeners after gconfd shutdown (%s)"),
                           g_strerror (errno));
          
          g_free (logdir);
          g_free (logfile);

          return FALSE;
        }
      
      g_free (logdir);
      g_free (logfile);


      {
        const gulong timeout_len = 60*0.5; /* 60 s/min * 0.5 min */

        if (append_handle_timeout != 0)
          g_source_remove (append_handle_timeout);
        
        append_handle_timeout = g_timeout_add_seconds (timeout_len,
                                                       close_append_handle_timeout,
                                                       NULL);
      }
    }

  return TRUE;
}

static void
close_append_handle (void)
{
  if (append_handle)
    {
      if (fclose (append_handle) < 0)
        gconf_log (GCL_WARNING,
                   _("Failed to close gconfd logfile; data may not have been properly saved (%s)"),
                   g_strerror (errno));

      append_handle = NULL;

      if (append_handle_timeout != 0)
        {
          g_source_remove (append_handle_timeout);
          append_handle_timeout = 0;
        }
    }
}

/* Atomically save our current state, if possible; otherwise
 * leave the running log in place.
 */
static void
logfile_save (void)
{
  GList *tmp_list;
  gchar *logdir = NULL;
  gchar *logfile = NULL;
  gchar *tmpfile = NULL;
  gchar *tmpfile2 = NULL;
  GString *saveme = NULL;
  gint fd = -1;
  
  /* Close the running log */
  close_append_handle ();
  
  get_log_names (&logdir, &logfile);

  g_mkdir (logdir, 0700); /* ignore failure, we'll catch failures
			   * that matter on open()
			   */

  saveme = g_string_new (NULL);

  /* Clients */
  log_clients_to_string (saveme);
  
  /* Databases */
  tmp_list = db_list;
  while (tmp_list)
    {
      GConfDatabase *db = tmp_list->data;

      gconf_database_log_listeners_to_string (db,
                                              db == default_db ? TRUE : FALSE,
                                              saveme);
      
      tmp_list = g_list_next (tmp_list);
    }
  
  /* Now try saving the string to a temporary file */
  tmpfile = g_strconcat (logfile, ".tmp", NULL);
  
  fd = g_open (tmpfile, O_WRONLY | O_CREAT | O_TRUNC, 0700);

  if (fd < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Could not open saved state file '%s' for writing: %s"),
                 tmpfile, g_strerror (errno));
      
      goto out;
    }

 again:
  
  if (write (fd, saveme->str, saveme->len) < 0)
    {
      if (errno == EINTR)
        goto again;
      
      gconf_log (GCL_WARNING,
                 _("Could not write saved state file '%s' fd: %d: %s"),
                 tmpfile, fd, g_strerror (errno));

      goto out;
    }

  if (fsync (fd) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Could not flush saved state file '%s' to disk: %s"),
                 tmpfile, g_strerror (errno));
    }

  if (close (fd) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to close new saved state file '%s': %s"),
                 tmpfile, g_strerror (errno));
      goto out;
    }

  fd = -1;
  
  /* Move the main saved state file aside, if it exists */
  if (g_file_test(logfile, G_FILE_TEST_EXISTS))
    {
      tmpfile2 = g_strconcat (logfile, ".orig", NULL);
      if (g_rename (logfile, tmpfile2) < 0)
        {
          gconf_log (GCL_WARNING,
                     _("Could not move aside old saved state file '%s': %s"),
                     logfile, g_strerror (errno));
          goto out;
        }
    }

  /* Move the new saved state file into place */
  if (g_rename (tmpfile, logfile) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to move new saved state file into place: %s"),
                 g_strerror (errno));

      /* Try to restore old file */
      if (tmpfile2)
        {
          if (g_rename (tmpfile2, logfile) < 0)
            {
              gconf_log (GCL_WARNING,
                         _("Failed to restore original saved state file that had been moved to '%s': %s"),
                         tmpfile2, g_strerror (errno));

            }
        }
      
      goto out;
    }

  /* Get rid of original saved state file if everything succeeded */
  if (tmpfile2)
    g_unlink (tmpfile2);
  
 out:
  if (saveme)
    g_string_free (saveme, TRUE);
  g_free (logdir);
  g_free (logfile);
  g_free (tmpfile);
  g_free (tmpfile2);

  if (fd >= 0)
    close (fd);
}

typedef struct _ListenerLogEntry ListenerLogEntry;

struct _ListenerLogEntry
{
  guint connection_id;
  gchar *ior;
  gchar *address;
  gchar *location;
};

static guint
listener_logentry_hash (gconstpointer v)
{
  const ListenerLogEntry *lle = v;

  return
    (lle->connection_id         & 0xff000000) |
    (g_str_hash (lle->ior)      & 0x00ff0000) |
    (g_str_hash (lle->address)  & 0x0000ff00) |
    (g_str_hash (lle->location) & 0x000000ff);
}

static gboolean
listener_logentry_equal (gconstpointer ap, gconstpointer bp)
{
  const ListenerLogEntry *a = ap;
  const ListenerLogEntry *b = bp;

  return
    a->connection_id == b->connection_id &&
    strcmp (a->location, b->location) == 0 &&
    strcmp (a->ior, b->ior) == 0 &&
    strcmp (a->address, b->address) == 0;
}

/* Return value indicates whether we "handled" this line of text */
static gboolean
parse_listener_entry (GHashTable *entries,
                      gchar      *text)
{
  gboolean add;
  gchar *p;
  gchar *ior;
  gchar *address;
  gchar *location;
  gchar *end;
  guint connection_id;
  GError *err;
  ListenerLogEntry *lle;
  ListenerLogEntry *old;
  
  if (strncmp (text, "ADD", 3) == 0)
    {
      add = TRUE;
      p = text + 3;
    }
  else if (strncmp (text, "REMOVE", 6) == 0)
    {
      add = FALSE;
      p = text + 6;
    }
  else
    {
      return FALSE;
    }
  
  while (*p && g_ascii_isspace (*p))
    ++p;

  errno = 0;
  end = NULL;
  connection_id = strtoul (p, &end, 10);
  if (end == p || errno != 0)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to parse connection ID in saved state file");
      
      return TRUE;
    }

  if (connection_id == 0)
    {
      gconf_log (GCL_DEBUG,
                 "Connection ID 0 in saved state file is not valid");
      return TRUE;
    }
  
  p = end;

  while (*p && g_ascii_isspace (*p))
    ++p;

  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote configuration source address from saved state file: %s",
                 err->message);

      g_error_free (err);
      
      return TRUE;
    }

  address = p;
  p = end;

  while (*p && g_ascii_isspace (*p))
    ++p;
  
  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote listener location from saved state file: %s",
                 err->message);

      g_error_free (err);
      
      return TRUE;
    }

  location = p;
  p = end;

  while (*p && g_ascii_isspace (*p))
    ++p;
  
  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote IOR from saved state file: %s",
                 err->message);
      
      g_error_free (err);
      
      return TRUE;
    }
  
  ior = p;
  p = end;    

  lle = g_new (ListenerLogEntry, 1);
  lle->connection_id = connection_id;
  lle->address = address;
  lle->ior = ior;
  lle->location = location;

  if (*(lle->address) == '\0' ||
      *(lle->ior) == '\0' ||
      *(lle->location) == '\0')
    {
      gconf_log (GCL_DEBUG,
                 "Saved state file listener entry didn't contain all the fields; ignoring.");

      g_free (lle);

      return TRUE;
    }
  
  old = g_hash_table_lookup (entries, lle);

  if (old)
    {
      if (add)
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file records the same listener added twice; ignoring the second instance");
          goto quit;
        }
      else
        {
          /* This entry was added, then removed. */
          g_hash_table_remove (entries, lle);
          goto quit;
        }
    }
  else
    {
      if (add)
        {
          g_hash_table_insert (entries, lle, lle);
          
          return TRUE;
        }
      else
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file had a removal of a listener that wasn't added; ignoring the removal.");
          goto quit;
        }
    }
  
 quit:
  g_free (lle);

  return TRUE;
}                

/* Return value indicates whether we "handled" this line of text */
static gboolean
parse_client_entry (GHashTable *clients,
                    gchar      *text)
{
  gboolean add;
  gchar *ior;
  GError *err;
  gchar *old;
  gchar *p;
  gchar *end;
  
  if (strncmp (text, "CLIENTADD", 9) == 0)
    {
      add = TRUE;
      p = text + 9;
    }
  else if (strncmp (text, "CLIENTREMOVE", 12) == 0)
    {
      add = FALSE;
      p = text + 12;
    }
  else
    {
      return FALSE;
    }
  
  while (*p && g_ascii_isspace (*p))
    ++p;
  
  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote IOR from saved state file: %s",
                 err->message);
      
      g_error_free (err);
      
      return TRUE;
    }
  
  ior = p;
  p = end;    
  
  old = g_hash_table_lookup (clients, ior);

  if (old)
    {
      if (add)
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file records the same client added twice; ignoring the second instance");
          goto quit;
        }
      else
        {
          /* This entry was added, then removed. */
          g_hash_table_remove (clients, ior);
          goto quit;
        }
    }
  else
    {
      if (add)
        {
          g_hash_table_insert (clients, ior, ior);
          
          return TRUE;
        }
      else
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file had a removal of a client that wasn't added; ignoring the removal.");
          goto quit;
        }
    }
  
 quit:

  return TRUE;
}

static void
restore_client (const gchar *ior)
{
  ConfigListener cl;
  CORBA_Environment ev;
  
  CORBA_exception_init (&ev);
  
  cl = CORBA_ORB_string_to_object (gconf_orb_get (), (gchar*)ior, &ev);

  CORBA_exception_free (&ev);
  
  if (CORBA_Object_is_nil (cl, &ev))
    {
      CORBA_exception_free (&ev);

      gconf_log (GCL_DEBUG,
                 "Client in saved state file no longer exists, not restoring it as a client");
      
      return;
    }

  ConfigListener_drop_all_caches (cl, &ev);
  
  if (ev._major != CORBA_NO_EXCEPTION)
    {
      gconf_log (GCL_DEBUG, "Failed to update client in saved state file, the client probably no longer exists");

      goto finished;
    }

  /* Add the client, since it still exists. Note that the client still
   * has the wrong server object reference, so next time it tries to
   * contact the server it will re-add itself; we just live with that,
   * it's not a problem.
   */
  add_client (cl);
  
 finished:
  CORBA_Object_release (cl, &ev);

  CORBA_exception_free (&ev);
}

static void
restore_listener (GConfDatabase* db,
                  ListenerLogEntry *lle)
{
  ConfigListener cl;
  CORBA_Environment ev;
  guint new_cnxn;
  GError *err;
  
  CORBA_exception_init (&ev);
  
  cl = CORBA_ORB_string_to_object (gconf_orb_get (), lle->ior, &ev);

  CORBA_exception_free (&ev);
  
  if (CORBA_Object_is_nil (cl, &ev))
    {
      CORBA_exception_free (&ev);

      gconf_log (GCL_DEBUG,
                 "Client in saved state file no longer exists, not updating its listener connections");
      
      return;
    }

  /* "Cancel" the addition of the listener in the saved state file,
   * so that if we reload the saved state file a second time
   * for some reason, we don't try to add this listener that time.
   */

  err = NULL;  
  if (!gconfd_logfile_change_listener (db,
                                       FALSE, /* remove */
                                       lle->connection_id,
                                       cl,
                                       lle->location,
                                       &err))
    {
      gconf_log (GCL_DEBUG,
                 "Failed to cancel previous daemon's listener in saved state file: %s",
                 err->message);
      g_error_free (err);
    }
  
  new_cnxn = gconf_database_readd_listener (db, cl, "from-saved-state", lle->location);

  gconf_log (GCL_DEBUG, "Attempting to update listener from saved state file, old connection %u, new connection %u", (guint) lle->connection_id, (guint) new_cnxn);
  
  ConfigListener_update_listener (cl,
                                  db->objref,
                                  lle->address,
                                  lle->connection_id,
                                  lle->location,
                                  new_cnxn,
                                  &ev);
  
  if (ev._major != CORBA_NO_EXCEPTION)
    {
      gconf_log (GCL_DEBUG, "Failed to update listener in saved state file, probably the client no longer exists");

      /* listener will get removed next time we try to notify -
       * we already appended a cancel of the listener to the
       * saved state file.
       */
      goto finished;
    }

  /* Successfully notified client of new connection ID, so put that
   * connection ID in the saved state file.
   */
  err = NULL;  
  if (!gconfd_logfile_change_listener (db,
                                       TRUE, /* add */
                                       new_cnxn,
                                       cl,
                                       lle->location,
                                       &err))
    {
      gconf_log (GCL_DEBUG,
                 "Failed to re-add this daemon's listener ID in saved state file: %s",
                 err->message);
      g_error_free (err);
    }

  /* We updated the listener, and logged that to the saved state
   * file. Yay!
   */
  
 finished:
  
  CORBA_Object_release (cl, &ev);

  CORBA_exception_free (&ev);
}

static void
listener_logentry_restore_and_destroy_foreach (gpointer key,
                                               gpointer value,
                                               gpointer data)
{
  ListenerLogEntry *lle = key;
  GConfDatabase *db = NULL;
  
  if (strcmp (lle->address, "def") == 0)
    db = default_db;
  else
    {
      GSList *addresses;

      addresses = gconf_persistent_name_get_address_list (lle->address);

      db = gconfd_obtain_database (addresses, NULL);

      gconf_address_list_free (addresses);
    }
  
  if (db == NULL)
    {
      gconf_log (GCL_WARNING,
                 _("Unable to restore a listener on address '%s', couldn't resolve the database"),
                 lle->address);
      return;
    }

  restore_listener (db, lle);

  /* We don't need it anymore */
  g_free (lle);
}

static void
restore_client_foreach (gpointer key,
                        gpointer value,
                        gpointer data)
{
  restore_client (key);
}


static gchar*
read_line (FILE *f)
{
#define BUF_SIZE 2048

  char  buf[BUF_SIZE] = { '\0' };
  char *retval = NULL;
  int   len = 0;

  do
    {
      if (fgets (buf, BUF_SIZE, f) == NULL)
        {
          if (ferror (f))
            {
              gconf_log (GCL_ERR,
                         _("Error reading saved state file: %s"),
                         g_strerror (errno));
            }
          break;
        }

      len = strlen (buf);
      if (len > 0 && buf[len - 1] == '\n')
	buf[--len] = '\0';

      if (retval == NULL)
	{
	  retval = g_strndup (buf, len);
	}
      else
	{
	  char *freeme = retval;

	  retval = g_strconcat (retval, buf, NULL);
	  g_free (freeme);
	}
    }
  while (len == BUF_SIZE - 1);

  return retval;

#undef BUF_SIZE
}

static void
logfile_read (void)
{
  gchar *logfile;
  gchar *logdir;
  GHashTable *entries;
  GHashTable *clients;
  FILE *f;
  gchar *line;
  GSList *lines = NULL;
  
  /* Just for good form */
  close_append_handle ();
  
  get_log_names (&logdir, &logfile);

  f = g_fopen (logfile, "r");
  
  if (f == NULL)
    {
      if (errno != ENOENT)
          gconf_log (GCL_ERR, _("Unable to open saved state file '%s': %s"),
                     logfile, g_strerror (errno));

      goto finished;
    }

  entries = g_hash_table_new (listener_logentry_hash, listener_logentry_equal);
  clients = g_hash_table_new (g_str_hash, g_str_equal);

  line = read_line (f);
  while (line)
    {
      if (!parse_listener_entry (entries, line))
        {
          if (!parse_client_entry (clients, line))
            {
              gconf_log (GCL_DEBUG,
                         "Didn't understand line in saved state file: '%s'", 
                         line);
              g_free (line);
              line = NULL;
            }
        }

      if (line)
        lines = g_slist_prepend (lines, line);
      
      line = read_line (f);
    }
  
  /* Restore clients first */
  g_hash_table_foreach (clients,
                        restore_client_foreach,
                        NULL);
  
  /* Entries that still remain in the listener hash table were added
   * but not removed, so add them in this daemon instantiation and
   * update their listeners with the new connection ID etc.
   */
  g_hash_table_foreach (entries, 
                        listener_logentry_restore_and_destroy_foreach,
                        NULL);

  g_hash_table_destroy (entries);
  g_hash_table_destroy (clients);

  /* Note that we need the strings to remain valid until we are totally
   * finished, because we store pointers to them in the log entry
   * hash.
   */
  g_slist_foreach (lines, (GFunc)g_free, NULL);
  g_slist_free (lines);
  
 finished:
  if (f != NULL)
    fclose (f);
  
  g_free (logfile);
  g_free (logdir);
}

gboolean
gconfd_logfile_change_listener (GConfDatabase *db,
                                gboolean add,
                                guint connection_id,
                                ConfigListener listener,
                                const gchar *where,
                                GError **err)
{
  gchar *ior = NULL;
  gchar *quoted_db_name;
  gchar *quoted_where;
  gchar *quoted_ior;
  
  if (!open_append_handle (err))
    return FALSE;
  
  ior = gconf_object_to_string (listener, err);
  
  if (ior == NULL)
    return FALSE;

  quoted_ior = gconf_quote_string (ior);
  g_free (ior);
  ior = NULL;
  
  if (db == default_db)
    quoted_db_name = gconf_quote_string ("def");
  else
    {
      const gchar *db_name;
      
      db_name = gconf_database_get_persistent_name (db);
      
      quoted_db_name = gconf_quote_string (db_name);
    }

  quoted_where = gconf_quote_string (where);

  /* KEEP IN SYNC with gconf-database.c log to string function */
  if (fprintf (append_handle, "%s %u %s %s %s\n",
               add ? "ADD" : "REMOVE", connection_id,
               quoted_db_name, quoted_where, quoted_ior) < 0)
    goto error;

  if (fflush (append_handle) < 0)
    goto error;

  g_free (quoted_db_name);
  g_free (quoted_ior);
  g_free (quoted_where);
  
  return TRUE;

 error:

  if (add)
    gconf_set_error (err,
                     GCONF_ERROR_FAILED,
                     _("Failed to log addition of listener to gconfd logfile; won't be able to re-add the listener if gconfd exits or shuts down (%s)"),
                     g_strerror (errno));
  else
    gconf_set_error (err,
                     GCONF_ERROR_FAILED,
                     _("Failed to log removal of listener to gconfd logfile; might erroneously re-add the listener if gconfd exits or shuts down (%s)"),
                     g_strerror (errno));

  g_free (quoted_db_name);
  g_free (quoted_ior);
  g_free (quoted_where);

  return FALSE;
}

static void
log_client_change (const ConfigListener client,
                   gboolean add)
{
  gchar *ior = NULL;
  gchar *quoted_ior = NULL;
  GError *err;
  
  err = NULL;
  ior = gconf_object_to_string (client, &err);

  if (err != NULL)
    {
      gconf_log (GCL_WARNING, _("Failed to get IOR for client: %s"),
                 err->message);
      g_error_free (err);
      return;
    }
      
  if (ior == NULL)
    return;

  quoted_ior = gconf_quote_string (ior);
  g_free (ior);
  ior = NULL;
  
  if (!open_append_handle (&err))
    {
      gconf_log (GCL_WARNING, _("Failed to open saved state file: %s"),
                 err->message);

      g_error_free (err);
      
      goto error;
    }

  /* KEEP IN SYNC with log to string function */
  if (fprintf (append_handle, "%s %s\n",
               add ? "CLIENTADD" : "CLIENTREMOVE", quoted_ior) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to write client add to saved state file: %s"),
                 g_strerror (errno));
      goto error;
    }

  if (fflush (append_handle) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to flush client add to saved state file: %s"),
                 g_strerror (errno));
      goto error;
    }

 error:
  g_free (ior);
  g_free (quoted_ior);
}

static void
log_client_add (const ConfigListener client)
{
  log_client_change (client, TRUE);
}

static void
log_client_remove (const ConfigListener client)
{
  log_client_change (client, FALSE);
}

/*
 * Client handling
 */

static GHashTable *client_table = NULL;

static void
add_client (const ConfigListener client)
{
  gconfd_need_log_cleanup ();
  
  if (client_table == NULL)
    client_table = g_hash_table_new ((GHashFunc) gconf_CORBA_Object_hash,
                                     (GCompareFunc) gconf_CORBA_Object_equal);

  if (g_hash_table_lookup (client_table, client))
    {
      /* Ignore this case; it happens normally when we added a client
       * from the logfile, and the client also adds itself
       * when it gets a new server objref.
       */
      return;
    }
  else
    {
      CORBA_Environment ev;
      ConfigListener copy;
      ORBitConnection *connection;
      
      CORBA_exception_init (&ev);
      copy = CORBA_Object_duplicate (client, &ev);
      g_hash_table_insert (client_table, copy, copy);
      CORBA_exception_free (&ev);

      /* Set maximum buffer size, which makes the connection nonblocking
       * if the kernel buffers are full and keeps gconfd from
       * locking up. Set the max to a pretty high number to avoid
       * dropping clients that are just stuck for a while.
       */
      connection = ORBit_small_get_connection (copy);
      ORBit_connection_set_max_buffer (connection, 1024 * 128);
      
      log_client_add (client);

      gconf_log (GCL_DEBUG, "Added a new client");
    }
}

static void
remove_client (const ConfigListener client)
{
  ConfigListener old_client;
  CORBA_Environment ev;

  gconfd_need_log_cleanup ();
  
  if (client_table == NULL)
    goto notfound;
  
  old_client = g_hash_table_lookup (client_table, 
                                    client);

  if (old_client == NULL)
    goto notfound;

  g_hash_table_remove (client_table,
                       old_client);

  log_client_remove (old_client);
  
  CORBA_exception_init (&ev);
  CORBA_Object_release (old_client, &ev);
  CORBA_exception_free (&ev);

  return;
  
 notfound:
  gconf_log (GCL_WARNING, _("Some client removed itself from the GConf server when it hadn't been added."));  
}

static void
hash_listify_func(gpointer key, gpointer value, gpointer user_data)
{
  GSList** list_p = user_data;

  *list_p = g_slist_prepend(*list_p, value);
}

static GSList*
list_clients (void)
{
  GSList *clients = NULL;

  if (client_table == NULL)
    return NULL;

  g_hash_table_foreach (client_table, hash_listify_func, &clients);

  return clients;
}

static void
log_clients_foreach (gpointer key, gpointer value, gpointer data)
{
  ConfigListener client;
  gchar *ior = NULL;
  gchar *quoted_ior = NULL;
  GError *err;

  client = value;
  
  err = NULL;
  ior = gconf_object_to_string (client, &err);

  if (err != NULL)
    {
      gconf_log (GCL_WARNING, _("Failed to get IOR for client: %s"),
                 err->message);
      g_error_free (err);
      return;
    }
      
  if (ior == NULL)
    return;

  quoted_ior = gconf_quote_string (ior);
  g_free (ior);
  ior = NULL;

  g_string_append (data, "CLIENTADD ");
  g_string_append (data, quoted_ior);
  g_string_append_c (data, '\n');
  g_free (quoted_ior);
}

static void
log_clients_to_string (GString *str)
{
  if (client_table == NULL)
    return;

  g_hash_table_foreach (client_table, log_clients_foreach, str);
}

static void
drop_old_clients (void)
{
  GSList *clients;
  GSList *tmp;
  
  clients = list_clients ();

  if (clients)
    {
      CORBA_Environment ev;

      CORBA_exception_init (&ev);
      
      tmp = clients;
      while (tmp != NULL)
        {
          ConfigListener cl = tmp->data;
          CORBA_boolean result;

          result = CORBA_Object_non_existent (cl, &ev);
          
          if (ev._major != CORBA_NO_EXCEPTION)
            {
              gconf_log (GCL_WARNING, "Exception from CORBA_Object_non_existent(), assuming stale listener");
              CORBA_exception_free (&ev);
              CORBA_exception_init (&ev);
              result = TRUE;
            }

          if (result)
            {
              gconf_log (GCL_DEBUG, "removing stale client in drop_old_clients");
              
              remove_client (cl);
            }
          
          tmp = g_slist_next (tmp);
        }

      g_slist_free (clients);

      CORBA_exception_free (&ev);
    }
}

static guint
client_count (void)
{
  if (client_table == NULL)
    return 0;
  else
    return g_hash_table_size (client_table);
}
#endif

gboolean
gconfd_in_shutdown (void)
{
  return in_shutdown;
}
