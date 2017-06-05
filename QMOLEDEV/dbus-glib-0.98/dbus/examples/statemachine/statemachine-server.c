#include <config.h>

#include <dbus/dbus-glib.h>
#include <stdio.h>
#include <stdlib.h>

#include "statemachine.h"
#include "sm-marshal.h"
#include "statemachine-server.h"

enum
{
  PROP_O,
  PROP_BUS
};

enum
{
  MACHINE_CREATED,
  LAST_SIGNAL
};

static guint sm_server_signals[LAST_SIGNAL] = { 0 };

static void     sm_server_set_property       (GObject               *object,
					      guint                  prop_id,
					      const GValue          *value,
					      GParamSpec            *pspec);
static void     sm_server_get_property       (GObject               *object,
					      guint                  prop_id,
					      GValue                *value,
					      GParamSpec            *pspec);

G_DEFINE_TYPE(SMServer, sm_server, G_TYPE_OBJECT)

#include "statemachine-server-glue.h"
#include "statemachine-glue.h"

static void
sm_server_init (SMServer *obj)
{
  obj->machines = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_object_unref);
}

static void
sm_server_class_init (SMServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = sm_server_set_property;
  object_class->get_property = sm_server_get_property;

  g_object_class_install_property (object_class,
				   PROP_BUS,
				   g_param_spec_boxed ("bus",
						       "bus",
						       "bus",
						       DBUS_TYPE_G_CONNECTION,
						       G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  sm_server_signals[MACHINE_CREATED] =
    g_signal_new ("machine-created",
		  G_OBJECT_CLASS_TYPE (klass),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  sm_marshal_VOID__STRING_BOXED,
                  G_TYPE_NONE, 2, G_TYPE_STRING, DBUS_TYPE_G_OBJECT_PATH);
}

static void
sm_server_set_property (GObject *object,
			guint prop_id,
			const GValue *value,
			GParamSpec *pspec)
{
  SMServer *server = SM_SERVER (object);

  switch (prop_id)
    {
    case PROP_BUS:
      server->bus = g_value_get_boxed (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void 
sm_server_get_property (GObject *object,
			guint prop_id,
			GValue *value,
			GParamSpec *pspec)
{
  SMServer *server = SM_SERVER (object);

  switch (prop_id)
    {
    case PROP_BUS:
      g_value_set_boxed (value, server->bus);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
machine_state_changed_cb (SMObject *obj, const char *state, gpointer data)
{
  char *name;

  g_object_get (obj, "name", &name, NULL);
  g_print ("Machine %s switching to state %s\n", name, state);
  g_free (name);
}

static void
machine_acquisition_changed_cb (SMObject *obj, gdouble progress, gpointer data)
{
  char *name;

  g_object_get (obj, "name", &name, NULL);
  g_print ("Machine %s got progress %f\n", name, progress);
  g_free (name);
}

gboolean
sm_server_create_machine (SMServer *server, const char *name, GError **error)
{
  SMObject *machine;
  char *path;

  machine = g_hash_table_lookup (server->machines, name);
  if (machine != NULL)
    {
      g_set_error (error,
		   SM_ERROR,
		   SM_ERROR_NAME_IN_USE,
		   "Statemachine name \"%s\" is already in use",
		   name);
      return FALSE;
    }

  machine = g_object_new (SM_TYPE_OBJECT, "name", name, NULL);

  path = g_strdup_printf ("/com/example/StateMachines/%s", name);
  dbus_g_connection_register_g_object (server->bus, path, G_OBJECT (machine));

  g_hash_table_insert (server->machines, g_strdup (name), machine);

  g_print ("Created state machine with name %s at %s\n", name, path);

  g_signal_connect_object (machine, "state-changed",
			   G_CALLBACK (machine_state_changed_cb),
			   NULL, 0);
  g_signal_connect_object (machine, "acquisition-progress",
			   G_CALLBACK (machine_acquisition_changed_cb),
			   NULL, 0);

  g_signal_emit (server, sm_server_signals[MACHINE_CREATED], 0, name, path);
  g_free (path);
  
  return TRUE;
}

static void
add_machine_to_ptr_array (gpointer key, gpointer val, gpointer data)
{
  const char *name = key;
  /* SMObject *sm = val; */
  GPtrArray *ptrarray = data;
  
  g_ptr_array_add (ptrarray, g_strdup_printf ("/com/example/StateMachines/%s",
					      name));
}

gboolean
sm_server_get_machines (SMServer *server, GPtrArray **machines, GError **error)
{
  *machines = g_ptr_array_new ();

  g_hash_table_foreach (server->machines, add_machine_to_ptr_array, *machines);

  return TRUE;
}

int
main (int argc, char **argv)
{
  DBusGConnection *bus;
  DBusGProxy *bus_proxy;
  GError *error = NULL;
  SMServer *server;
  GMainLoop *mainloop;
  guint request_name_result;

  g_type_init ();
  
  dbus_g_object_type_install_info (SM_TYPE_SERVER, &dbus_glib_sm_server_object_info);
  dbus_g_object_type_install_info (SM_TYPE_OBJECT, &dbus_glib_sm_object_object_info);
  dbus_g_error_domain_register (SM_ERROR, NULL, SM_TYPE_ERROR);

  mainloop = g_main_loop_new (NULL, FALSE);

  bus = dbus_g_bus_get (DBUS_BUS_SESSION, &error);
  if (!bus)
    g_critical ("Couldn't connect to session bus: %s\n", error->message);

  bus_proxy = dbus_g_proxy_new_for_name (bus, "org.freedesktop.DBus",
					 "/org/freedesktop/DBus",
					 "org.freedesktop.DBus");

  if (!dbus_g_proxy_call (bus_proxy, "RequestName", &error,
			  G_TYPE_STRING, "com.example.StateServer",
			  G_TYPE_UINT, 0,
			  G_TYPE_INVALID,
			  G_TYPE_UINT, &request_name_result,
			  G_TYPE_INVALID))
    g_critical ("Couldn't acquire com.example.StateServer: %s\n", error->message);

  server = g_object_new (SM_TYPE_SERVER, "bus", bus, NULL);

  dbus_g_connection_register_g_object (bus, "/com/example/StateServer", G_OBJECT (server));

  g_print ("StateMachine server initialized\n");

  g_main_loop_run (mainloop);

  exit (0);
}
