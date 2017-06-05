#ifndef __MY_OBJECT_H__
#define __MY_OBJECT_H__

#include <glib-object.h>
#include <dbus/dbus-glib.h>

typedef struct MyObject MyObject;
typedef struct MyObjectClass MyObjectClass;

void my_object_register_marshallers (void);
GType my_object_get_type (void);

struct MyObject
{
  GObject parent;
  char *this_is_a_string;
  guint notouching;
  guint val;
  gdouble super_studly;
  gboolean should_be_hidden;
};

struct MyObjectClass
{
  GObjectClass parent;
};

#define MY_TYPE_OBJECT              (my_object_get_type ())
#define MY_OBJECT(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), MY_TYPE_OBJECT, MyObject))
#define MY_OBJECT_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), MY_TYPE_OBJECT, MyObjectClass))
#define MY_IS_OBJECT(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), MY_TYPE_OBJECT))
#define MY_IS_OBJECT_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), MY_TYPE_OBJECT))
#define MY_OBJECT_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), MY_TYPE_OBJECT, MyObjectClass))

typedef enum
{
  MY_OBJECT_ERROR_FOO,
  MY_OBJECT_ERROR_BAR,
  MY_OBJECT_ERROR_MULTI_WORD,
  MY_OBJECT_ERROR_UNDER_SCORE
} MyObjectError;

#define MY_OBJECT_ERROR (my_object_error_quark ())
#define MY_TYPE_ERROR (my_object_error_get_type ()) 

GQuark my_object_error_quark (void);
GType my_object_error_get_type (void);

gboolean my_object_do_nothing (MyObject *obj, GError **error);

gboolean my_object_increment (MyObject *obj, gint32 x, gint32 *ret, GError **error);

gint32   my_object_increment_retval (MyObject *obj, gint32 x);

gint32   my_object_increment_retval_error (MyObject *obj, gint32 x, GError **error);

gboolean my_object_throw_error (MyObject *obj, GError **error);
gboolean my_object_throw_not_supported (MyObject *obj, GError **error);
gboolean my_object_throw_error_multi_word (MyObject *obj, GError **error);
gboolean my_object_throw_unregistered_error (MyObject *obj, GError **error);

gboolean my_object_uppercase (MyObject *obj, const char *str, char **ret, GError **error);

gboolean my_object_many_args (MyObject *obj, guint32 x, const char *str, double trouble, double *d_ret, char **str_ret, GError **error);

gboolean my_object_many_return (MyObject *obj, guint32 *arg0, char **arg1, gint32 *arg2, guint32 *arg3, guint32 *arg4, const char **arg5, GError **error);

gboolean my_object_recursive1 (MyObject *obj, GArray *array, guint32 *len_ret, GError **error);
gboolean my_object_recursive2 (MyObject *obj, guint32 reqlen, GArray **array, GError **error);

gboolean my_object_many_stringify (MyObject *obj, GHashTable *vals, GHashTable **ret, GError **error);

gboolean my_object_rec_arrays (MyObject *obj, GPtrArray *in, GPtrArray **ret, GError **error);

gboolean my_object_objpath (MyObject *obj, const char *in, const char **arg1, GError **error);

gboolean my_object_get_objs (MyObject *obj, GPtrArray **objs, GError **error);

gboolean my_object_stringify (MyObject *obj, GValue *value, char **ret, GError **error);
gboolean my_object_unstringify (MyObject *obj, const char *str, GValue *value, GError **error);

gboolean my_object_many_uppercase (MyObject *obj, const char * const *in, char ***out, GError **error);

gboolean my_object_str_hash_len (MyObject *obj, GHashTable *table, guint *len, GError **error);

gboolean my_object_send_car (MyObject *obj, GValueArray *invals, GValueArray **outvals, GError **error);

gboolean my_object_get_hash (MyObject *obj, GHashTable **table, GError **error);

gboolean my_object_increment_val (MyObject *obj, GError **error);

gboolean my_object_get_val (MyObject *obj, guint *ret, GError **error);

gboolean my_object_get_value (MyObject *obj, guint *ret, GError **error);

gboolean my_object_emit_signals (MyObject *obj, GError **error);
gboolean my_object_emit_signal2 (MyObject *obj, GError **error);

gboolean my_object_emit_frobnicate (MyObject *obj, GError **error);

gboolean my_object_echo_variant (MyObject *obj, GValue *variant, GValue *ret, GError **error);
gboolean my_object_echo_signature (MyObject *obj, const gchar *in, gchar **out, GError **error);

gboolean my_object_process_variant_of_array_of_ints123 (MyObject *obj, GValue *variant, GError **error);

gboolean my_object_dict_of_dicts (MyObject *obj, GHashTable *dict, GHashTable **ret, GError **error);

void my_object_dict_of_sigs (MyObject *obj, GHashTable *dict, DBusGMethodInvocation *ctx);

void my_object_dict_of_objs (MyObject *obj, GHashTable *dict, DBusGMethodInvocation *ctx);

gboolean my_object_terminate (MyObject *obj, GError **error);

void my_object_async_increment (MyObject *obj, gint32 x, DBusGMethodInvocation *context);

void my_object_async_throw_error (MyObject *obj, DBusGMethodInvocation *context);

void my_object_unsafe_disable_legacy_property_access (MyObject *obj);

void my_object_emit_objectified (MyObject *obj, GObject *other);

#endif
