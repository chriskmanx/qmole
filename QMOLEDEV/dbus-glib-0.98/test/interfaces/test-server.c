#include <config.h>

#include <stdlib.h>
#include "dbus/dbus-glib.h"
#include "tools/dbus-glib-bindings.h"
#include "test-objects.h"
#include "test-dup-prop.h"

#define TEST_NAMESPACE "org.freedesktop.DBus.GLib.Test.Interfaces"
#define TEST_OBJECT_PATH "/org/freedesktop/DBus/GLib/Test/Interfaces"
#define TEST_DP_OBJECT_PATH "/org/freedesktop/DBus/GLib/Test/DupPropInterfaces"

static GMainLoop *loop = NULL;

int
main (int    argc, 
      char **argv)
{
	DBusGConnection *connection;
	DBusGProxy *proxy;
	GError *error = NULL;
	guint32 ret;
	TestBeatlesSong *song;
	TestDpObj *dp_obj;

	g_type_init ();

	/* Get the connection and ensure the name is not used yet */
	connection = dbus_g_bus_get (DBUS_BUS_SESSION, &error);
	if (connection == NULL) {
		g_warning ("Failed to make connection to session bus: %s",
			   error->message);
		g_error_free (error);
		exit(1);
	}
		
	proxy = dbus_g_proxy_new_for_name (connection, DBUS_SERVICE_DBUS,
					   DBUS_PATH_DBUS, DBUS_INTERFACE_DBUS);
	if (!org_freedesktop_DBus_request_name (proxy, TEST_NAMESPACE,
						0, &ret, &error)) {
		g_warning ("There was an error requesting the name: %s",
			   error->message);
		g_error_free (error);
		exit(1);
	}
	
	if (ret != DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER) {
		/* Someone else registered the name before us */
		exit(1);
	}
		
	song = test_beatles_song_new ();
		
	/* Register the app on the bus */
	dbus_g_connection_register_g_object (connection,
					     TEST_OBJECT_PATH,
					     G_OBJECT (song));

	dp_obj = test_dp_obj_new ();
	dbus_g_connection_register_g_object (connection,
					     TEST_DP_OBJECT_PATH,
					     G_OBJECT (dp_obj));

	loop = g_main_loop_new (NULL, FALSE);
	g_main_loop_run (loop);

	return 0;
}
