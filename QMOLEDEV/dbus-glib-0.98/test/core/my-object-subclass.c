#include <config.h>
#include <string.h>
#include <glib/gi18n.h>
#include <glib-object.h>
#include "my-object-subclass.h"

#include "test-service-glib-subclass-glue.h"

/* Properties */
enum
{
  PROP_0,
  PROP_THIS_IS_A_SUBCLASS_STRING,
  PROP_THIS_IS_A_SUBCLASS_UINT
};

G_DEFINE_TYPE(MyObjectSubclass, my_object_subclass, MY_TYPE_OBJECT)

static void
my_object_subclass_finalize (GObject *object)
{
  MyObjectSubclass *mobject = MY_OBJECT_SUBCLASS (object);

  g_free (mobject->this_is_a_subclass_string);

  (G_OBJECT_CLASS (my_object_subclass_parent_class)->finalize) (object);
}

static void
my_object_subclass_set_property (GObject      *object,
                                 guint         prop_id,
                                 const GValue *value,
                                 GParamSpec   *pspec)
{
  MyObjectSubclass *mobject;

  mobject = MY_OBJECT_SUBCLASS (object);
  
  switch (prop_id)
    {
    case PROP_THIS_IS_A_SUBCLASS_STRING:
      g_free (mobject->this_is_a_subclass_string);
      mobject->this_is_a_subclass_string = g_value_dup_string (value);
      break;
      
    case PROP_THIS_IS_A_SUBCLASS_UINT:
      mobject->this_is_a_subclass_uint = g_value_get_uint (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
my_object_subclass_get_property (GObject      *object,
                                 guint         prop_id,
                                 GValue       *value,
                                 GParamSpec   *pspec)
{
  MyObjectSubclass *mobject;

  mobject = MY_OBJECT_SUBCLASS (object);
  
  switch (prop_id)
    {
    case PROP_THIS_IS_A_SUBCLASS_STRING:
      g_value_set_string (value, mobject->this_is_a_subclass_string);
      break;

    case PROP_THIS_IS_A_SUBCLASS_UINT:
      g_value_set_uint (value, mobject->this_is_a_subclass_uint);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
my_object_subclass_init (MyObjectSubclass *obj)
{
}

static void
my_object_subclass_class_init (MyObjectSubclassClass *mobject_class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (mobject_class);

  dbus_g_object_type_install_info (MY_TYPE_OBJECT_SUBCLASS,
				   &dbus_glib_my_object_subclass_object_info);

  gobject_class->finalize = my_object_subclass_finalize;
  gobject_class->set_property = my_object_subclass_set_property;
  gobject_class->get_property = my_object_subclass_get_property;
  
  g_object_class_install_property (gobject_class,
				   PROP_THIS_IS_A_SUBCLASS_STRING,
				   g_param_spec_string ("this_is_a_subclass_string",
                                                        _("Sample string"),
                                                        _("Example of a string property"),
                                                        "default subclass value",
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (gobject_class,
				   PROP_THIS_IS_A_SUBCLASS_UINT,
				   g_param_spec_uint ("this_is_a_subclass_uint",
                                                        _("Sample uint"),
                                                        _("Example of a uint property"),
                                                        0, G_MAXUINT32, 1234567,
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

