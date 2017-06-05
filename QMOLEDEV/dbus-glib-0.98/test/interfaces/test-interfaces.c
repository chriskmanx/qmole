#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "test-interfaces.h"

static gboolean
test_hello_dbus_say_hello (TestHello  *hello,
			   gchar     **message,
			   GError    **error)
{
	*message = test_hello_say_hello (hello);
	return TRUE;
}

static gboolean
test_goodbye_dbus_say_goodbye (TestGoodbye  *goodbye,
			       gchar       **message,
			       GError      **error)
{
	*message = test_goodbye_say_goodbye (goodbye);
	return TRUE;
}

#include "test-hello-glue.h"
#include "test-goodbye-glue.h"

enum {
	GREETINGS,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void
test_hello_class_init (gpointer g_iface)
{
	GType iface_type = G_TYPE_FROM_INTERFACE (g_iface);

	signals[GREETINGS] =
		g_signal_new ("greetings",
			      iface_type,
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (TestHelloIface, greetings),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	dbus_g_object_type_install_info (iface_type,
					 &dbus_glib_test_hello_object_info);
}

GType
test_hello_get_type (void)
{
	static GType the_type = 0;
	
	if (G_UNLIKELY (the_type == 0)) {
		static const GTypeInfo info = {
			sizeof (TestHelloIface),
			NULL, NULL,
			(GClassInitFunc) test_hello_class_init,
			NULL, NULL, 0, 0, NULL
		};
		
		the_type = g_type_register_static (G_TYPE_INTERFACE,
						   "TestHello",
						   &info, 0);
		g_type_interface_add_prerequisite (the_type, G_TYPE_OBJECT);
	}
	return the_type;
}

gchar *
test_hello_say_hello (TestHello *hello)
{
	g_return_val_if_fail (TEST_IS_HELLO (hello), NULL);

	return (* TEST_HELLO_GET_IFACE (hello)->say_hello) (hello);
}

void
test_hello_greetings (TestHello *hello)
{
	g_return_if_fail (TEST_IS_HELLO (hello));

	g_signal_emit (hello, signals[GREETINGS], 0);
}

static void
test_goodbye_class_init (gpointer g_iface)
{
	GType iface_type = G_TYPE_FROM_INTERFACE (g_iface);

	dbus_g_object_type_install_info (iface_type,
					 &dbus_glib_test_goodbye_object_info);
}

GType
test_goodbye_get_type (void)
{
	static GType the_type = 0;
	
	if (G_UNLIKELY (the_type == 0)) {
		static const GTypeInfo info = {
			sizeof (TestGoodbyeIface),
			NULL, NULL,
			(GClassInitFunc) test_goodbye_class_init,
			NULL, NULL, 0, 0, NULL
		};
		
		the_type = g_type_register_static (G_TYPE_INTERFACE,
						   "TestGoodbye",
						   &info, 0);
		g_type_interface_add_prerequisite (the_type, G_TYPE_OBJECT);
	}
	return the_type;
}

gchar *
test_goodbye_say_goodbye (TestGoodbye *goodbye)
{
	g_return_val_if_fail (TEST_IS_GOODBYE (goodbye), NULL);

	return (* TEST_GOODBYE_GET_IFACE (goodbye)->say_goodbye) (goodbye);
}
