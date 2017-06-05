#ifndef __MY_OBJECT_SUBCLASS_H__
#define __MY_OBJECT_SUBCLASS_H__

#include <glib-object.h>
#include <dbus/dbus-glib.h>

#include "my-object.h"

typedef struct MyObjectSubclass MyObjectSubclass;
typedef struct MyObjectSubclassClass MyObjectSubclassClass;

GType my_object_subclass_get_type (void);

struct MyObjectSubclass
{
  MyObject parent;
  char *this_is_a_subclass_string;
  guint this_is_a_subclass_uint;
};

struct MyObjectSubclassClass
{
  MyObjectClass parent;
};

#define MY_TYPE_OBJECT_SUBCLASS              (my_object_subclass_get_type ())
#define MY_OBJECT_SUBCLASS(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), MY_TYPE_OBJECT_SUBCLASS, MyObjectSubclass))
#define MY_OBJECT_SUBCLASS_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), MY_TYPE_OBJECT_SUBCLASS, MyObjectSubclassClass))
#define MY_IS_OBJECT_SUBCLASS(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), MY_TYPE_OBJECT_SUBCLASS))
#define MY_IS_OBJECT_SUBCLASS_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), MY_TYPE_OBJECT_SUBCLASS))
#define MY_OBJECT_SUBCLASS_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), MY_TYPE_OBJECT_SUBCLASS, MyObjectSubclassClass))

#endif
