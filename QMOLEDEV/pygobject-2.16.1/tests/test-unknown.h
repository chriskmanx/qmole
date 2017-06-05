#include <glib-object.h>

/* TestUnknown */

typedef struct {
  GObject parent;
} TestUnknown;

typedef struct {
  GObjectClass parent_class;
} TestUnknownClass;

#define TEST_TYPE_UNKNOWN            (test_unknown_get_type())
#define TEST_UNKNOWN(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_UNKNOWN, TestUnknown))
#define TEST_UNKNOWN_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), TEST_TYPE_UNKNOWN, TestUnknownClass))
#define TEST_IS_UNKNOWN(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_UNKNOWN))
#define TEST_IS_UNKNOWN_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), TEST_TYPE_UNKNOWN))
#define TEST_UNKNOWN_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), TEST_TYPE_UNKNOWN, TestUnknownClass))

GType test_unknown_get_type (void);

/* TestInterface */
typedef struct _TestInterface TestInterface;
typedef struct _TestInterfaceIface TestInterfaceIface;

struct _TestInterfaceIface
{
  GTypeInterface g_iface;
  /* VTable */
  void (* iface_method) (TestInterface *iface);
};

#define TEST_TYPE_INTERFACE            (test_interface_get_type ())
#define TEST_INTERFACE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_INTERFACE, TestInterface))
#define TEST_IS_INTERFACE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_INTERFACE))
#define TEST_INTERFACE_GET_IFACE(obj)  (G_TYPE_INSTANCE_GET_INTERFACE ((obj), TEST_TYPE_INTERFACE, TestInterfaceIface))

GType test_interface_get_type (void);

void test_interface_iface_method (TestInterface *iface);
