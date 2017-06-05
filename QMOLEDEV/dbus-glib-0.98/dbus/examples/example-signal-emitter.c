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

typedef struct TestObject TestObject;
typedef struct TestObjectClass TestObjectClass;

GType test_object_get_type (void);

struct TestObject
{
  GObject parent;
};

struct TestObjectClass
{
  GObjectClass parent;
};

enum
{
  HELLO_SIGNAL,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

#define TEST_TYPE_OBJECT              (test_object_get_type ())
#define TEST_OBJECT(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), TEST_TYPE_OBJECT, TestObject))
#define TEST_OBJECT_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), TEST_TYPE_OBJECT, TestObjectClass))
#define TEST_IS_OBJECT(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), TEST_TYPE_OBJECT))
#define TEST_IS_OBJECT_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), TEST_TYPE_OBJECT))
#define TEST_OBJECT_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), TEST_TYPE_OBJECT, TestObjectClass))

G_DEFINE_TYPE(TestObject, test_object, G_TYPE_OBJECT)

gboolean test_object_emit_hello_signal (TestObject *obj, GError **error);

#include "example-signal-emitter-glue.h"

static void
test_object_init (TestObject *obj)
{
}

static void
test_object_class_init (TestObjectClass *klass)
{
  signals[HELLO_SIGNAL] =
    g_signal_new ("hello_signal",
		  G_OBJECT_CLASS_TYPE (klass),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  g_cclosure_marshal_VOID__STRING,
                  G_TYPE_NONE, 1, G_TYPE_STRING);
}

gboolean
test_object_emit_hello_signal (TestObject *obj, GError **error)
{
  g_signal_emit (obj, signals[HELLO_SIGNAL], 0, "Hello");
  return TRUE;
}


int
main (int argc, char **argv)
{
  DBusGConnection *bus;
  DBusGProxy *bus_proxy;
  GError *error = NULL;
  TestObject *obj;
  GMainLoop *mainloop;
  guint request_name_result;

  g_type_init ();

  dbus_g_object_type_install_info (TEST_TYPE_OBJECT, &dbus_glib_test_object_object_info);
  
  mainloop = g_main_loop_new (NULL, FALSE);

  bus = dbus_g_bus_get (DBUS_BUS_SESSION, &error);
  if (!bus)
    lose_gerror ("Couldn't connect to session bus", error);

  bus_proxy = dbus_g_proxy_new_for_name (bus, "org.freedesktop.DBus",
					 "/org/freedesktop/DBus",
					 "org.freedesktop.DBus");

  if (!dbus_g_proxy_call (bus_proxy, "RequestName", &error,
			  G_TYPE_STRING, "org.designfu.TestService",
			  G_TYPE_UINT, 0,
			  G_TYPE_INVALID,
			  G_TYPE_UINT, &request_name_result,
			  G_TYPE_INVALID))
    lose_gerror ("Failed to acquire org.designfu.TestService", error);

  obj = g_object_new (TEST_TYPE_OBJECT, NULL);

  dbus_g_connection_register_g_object (bus, "/org/designfu/TestService/object", G_OBJECT (obj));

  printf ("test service running\n");

  g_main_loop_run (mainloop);

  exit (0);
}
