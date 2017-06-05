#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "test-dup-prop.h"

#include "test-dup-prop-a-glue.h"
#include "test-dup-prop-b-glue.h"

#define TEST_A_FOOBAR "a-foobar"
#define TEST_B_FOOBAR "b-foobar"

static void
test_a_class_init (gpointer g_iface)
{
	GType iface_type = G_TYPE_FROM_INTERFACE (g_iface);

	g_object_interface_install_property (g_iface,
	    g_param_spec_uint (TEST_A_FOOBAR,
	                       "A Foobar",
	                       "A description of something",
	                       0, G_MAXUINT, 0,
	                       G_PARAM_READWRITE));

	dbus_g_object_type_install_info (iface_type,
					 &dbus_glib_test_dup_prop_a_object_info);
	dbus_g_object_type_register_shadow_property (iface_type,
	                                             "Foobar",
	                                             TEST_A_FOOBAR);
}

GType
test_a_get_type (void)
{
	static GType the_type = 0;

	if (G_UNLIKELY (the_type == 0)) {
		static const GTypeInfo info = {
			sizeof (TestAIface),
			NULL, NULL,
			(GClassInitFunc) test_a_class_init,
			NULL, NULL, 0, 0, NULL
		};

		the_type = g_type_register_static (G_TYPE_INTERFACE,
						   "TestA",
						   &info, 0);
		g_type_interface_add_prerequisite (the_type, G_TYPE_OBJECT);
	}
	return the_type;
}


static void
test_b_class_init (gpointer g_iface)
{
	GType iface_type = G_TYPE_FROM_INTERFACE (g_iface);

	g_object_interface_install_property (g_iface,
	    g_param_spec_uint (TEST_B_FOOBAR,
	                       "B Foobar",
	                       "A description of something",
	                       0, G_MAXUINT, 0,
	                       G_PARAM_READWRITE));

	dbus_g_object_type_install_info (iface_type,
					 &dbus_glib_test_dup_prop_b_object_info);
	dbus_g_object_type_register_shadow_property (iface_type,
	                                             "Foobar",
	                                             TEST_B_FOOBAR);
}

GType
test_b_get_type (void)
{
	static GType the_type = 0;

	if (G_UNLIKELY (the_type == 0)) {
		static const GTypeInfo info = {
			sizeof (TestBIface),
			NULL, NULL,
			(GClassInitFunc) test_b_class_init,
			NULL, NULL, 0, 0, NULL
		};

		the_type = g_type_register_static (G_TYPE_INTERFACE,
						   "TestB",
						   &info, 0);
		g_type_interface_add_prerequisite (the_type, G_TYPE_OBJECT);
	}
	return the_type;
}



static void test_a_init (TestAIface *a_class);
static void test_b_init (TestBIface *b_class);

G_DEFINE_TYPE_EXTENDED (TestDpObj, test_dp_obj, G_TYPE_OBJECT, 0,
                        G_IMPLEMENT_INTERFACE (TEST_TYPE_A, test_a_init)
                        G_IMPLEMENT_INTERFACE (TEST_TYPE_B, test_b_init))

#define TEST_DP_OBJ_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), TEST_TYPE_DP_OBJ, TestDpObjPrivate))

enum {
	PROP_0,
	PROP_A_FOOBAR,
	PROP_B_FOOBAR
};

typedef struct {
	guint32 a_foobar;
	guint32 b_foobar;
} TestDpObjPrivate;

TestDpObj *
test_dp_obj_new (void)
{
	return TEST_DP_OBJ (g_object_new (TEST_TYPE_DP_OBJ, NULL));
}

static void
test_a_init (TestAIface *a_class)
{
}

static void
test_b_init (TestBIface *b_class)
{
}

static void
test_dp_obj_init (TestDpObj *self)
{
}

static void
set_property (GObject *object, guint prop_id,
              const GValue *value, GParamSpec *pspec)
{
	TestDpObjPrivate *priv = TEST_DP_OBJ_GET_PRIVATE (object);

	switch (prop_id) {
	case PROP_A_FOOBAR:
		priv->a_foobar = g_value_get_uint (value);
		break;
	case PROP_B_FOOBAR:
		priv->b_foobar = g_value_get_uint (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
get_property (GObject *object, guint prop_id,
              GValue *value, GParamSpec *pspec)
{
	TestDpObjPrivate *priv = TEST_DP_OBJ_GET_PRIVATE (object);

	switch (prop_id) {
	case PROP_A_FOOBAR:
		g_value_set_uint (value, priv->a_foobar);
		break;
	case PROP_B_FOOBAR:
		g_value_set_uint (value, priv->b_foobar);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
test_dp_obj_class_init (TestDpObjClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	g_type_class_add_private (object_class, sizeof (TestDpObjPrivate));

	object_class->get_property = get_property;
	object_class->set_property = set_property;

	/* Properties */
	g_object_class_override_property (object_class,
	                                  PROP_A_FOOBAR,
	                                  TEST_A_FOOBAR);

	g_object_class_override_property (object_class,
	                                  PROP_B_FOOBAR,
	                                  TEST_B_FOOBAR);
}
