#include "test-unknown.h"

enum {
  PROP_SOME_PROPERTY = 1,
};


static void
test_interface_base_init (gpointer g_iface)
{
  static gboolean initialized = FALSE;
  
  if (!initialized)
    {
      g_object_interface_install_property (g_iface,
					   g_param_spec_string ("some-property",
								"some-property",
								"A simple test property",
								NULL,
								G_PARAM_READWRITE));
      initialized = TRUE;
    }
}


GType
test_interface_get_type (void)
{
  static GType gtype = 0;

  if (!gtype)
    {
      static const GTypeInfo info =
      {
        sizeof (TestInterfaceIface), /* class_size */
	test_interface_base_init,   /* base_init */
        NULL,           /* base_finalize */
        NULL,
        NULL,           /* class_finalize */
        NULL,           /* class_data */
        0,
        0,              /* n_preallocs */
        NULL
      };

      gtype =
        g_type_register_static (G_TYPE_INTERFACE, "TestInterface",
                                &info, 0);

      g_type_interface_add_prerequisite (gtype, G_TYPE_OBJECT);
    }

  return gtype;
}

void test_unknown_iface_method (TestInterface *iface)
{
}

static void
test_unknown_test_interface_init (TestInterfaceIface *iface)
{
  iface->iface_method = test_unknown_iface_method;
}

G_DEFINE_TYPE_WITH_CODE (TestUnknown, test_unknown, G_TYPE_OBJECT,
			 G_IMPLEMENT_INTERFACE (TEST_TYPE_INTERFACE,
						test_unknown_test_interface_init));

static void test_unknown_init (TestUnknown *self) {}


static void
test_unknown_get_property (GObject              *object,
                           guint                 prop_id,
                           GValue               *value,
                           GParamSpec           *pspec)
{

}

static void
test_unknown_set_property (GObject              *object,
                           guint                 prop_id,
                           const GValue         *value,
                           GParamSpec           *pspec)
{

}

static void test_unknown_class_init (TestUnknownClass *klass)
{
  GObjectClass *gobject_class = (GObjectClass*) klass;

  gobject_class->get_property = test_unknown_get_property;
  gobject_class->set_property = test_unknown_set_property;


  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SOME_PROPERTY,
                                   g_param_spec_string ("some-property",
                                                        "some-property",
                                                        "A simple test property",
                                                        NULL,
                                                        G_PARAM_READWRITE));
}

void test_interface_iface_method (TestInterface *instance)
{
  TestInterfaceIface *iface = TEST_INTERFACE_GET_IFACE (instance);

  (* iface->iface_method) (instance);
}
