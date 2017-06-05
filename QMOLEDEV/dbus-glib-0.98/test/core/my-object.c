#include <config.h>
#include <string.h>
#include <glib/gi18n.h>
#include <glib-object.h>
#include "my-object.h"
#include "my-object-marshal.h"

static gboolean my_object_throw_error_under_score (MyObject *obj,
    GError **error);

#include "test-service-glib-glue.h"

void
my_object_register_marshallers (void)
{
  dbus_g_object_register_marshaller (my_object_marshal_VOID__STRING_INT_STRING,
      G_TYPE_NONE, G_TYPE_STRING, G_TYPE_INT, G_TYPE_STRING, G_TYPE_INVALID);

  dbus_g_object_register_marshaller (my_object_marshal_VOID__STRING_BOXED,
      G_TYPE_NONE, G_TYPE_STRING, G_TYPE_VALUE, G_TYPE_INVALID);
}

/* Properties */
enum
{
  PROP_0,
  PROP_THIS_IS_A_STRING,
  PROP_NO_TOUCHING,
  PROP_SUPER_STUDLY,
  PROP_SHOULD_BE_HIDDEN
};

enum
{
  FROBNICATE,
  OBJECTIFIED,
  SIG0,
  SIG1,
  SIG2,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

G_DEFINE_TYPE(MyObject, my_object, G_TYPE_OBJECT)

static void
my_object_finalize (GObject *object)
{
  MyObject *mobject = MY_OBJECT (object);

  g_free (mobject->this_is_a_string);

  (G_OBJECT_CLASS (my_object_parent_class)->finalize) (object);
}

static void
my_object_set_property (GObject      *object,
                        guint         prop_id,
                        const GValue *value,
                        GParamSpec   *pspec)
{
  MyObject *mobject;

  mobject = MY_OBJECT (object);
  
  switch (prop_id)
    {
    case PROP_THIS_IS_A_STRING:
      g_free (mobject->this_is_a_string);
      mobject->this_is_a_string = g_value_dup_string (value);
      break;

    case PROP_NO_TOUCHING:
      mobject->notouching = g_value_get_uint (value);
      break;

    case PROP_SUPER_STUDLY:
      mobject->super_studly = g_value_get_double (value);
      break;

    case PROP_SHOULD_BE_HIDDEN:
      mobject->should_be_hidden = g_value_get_boolean (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
my_object_get_property (GObject      *object,
                        guint         prop_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  MyObject *mobject;

  mobject = MY_OBJECT (object);
  
  switch (prop_id)
    {
    case PROP_THIS_IS_A_STRING:
      g_value_set_string (value, mobject->this_is_a_string);
      break;

    case PROP_NO_TOUCHING:
      g_value_set_uint (value, mobject->notouching);
      break;

    case PROP_SUPER_STUDLY:
      g_value_set_double (value, mobject->super_studly);
      break;

    case PROP_SHOULD_BE_HIDDEN:
      g_value_set_boolean (value, mobject->should_be_hidden);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
my_object_init (MyObject *obj)
{
  obj->val = 0;
  obj->notouching = 42;
}

static void
my_object_class_init (MyObjectClass *mobject_class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (mobject_class);

  my_object_register_marshallers ();

  dbus_g_object_type_install_info (MY_TYPE_OBJECT,
				   &dbus_glib_my_object_object_info);

  gobject_class->finalize = my_object_finalize;
  gobject_class->set_property = my_object_set_property;
  gobject_class->get_property = my_object_get_property;
  
  g_object_class_install_property (gobject_class,
				   PROP_THIS_IS_A_STRING,
				   g_param_spec_string ("this_is_a_string",
                                                        _("Sample string"),
                                                        _("Example of a string property"),
                                                        "default value",
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
                                   PROP_NO_TOUCHING,
                                   g_param_spec_uint ("no_touching",
                                                      _("Don't touch"),
                                                      _("Example of a readonly property (for export)"),
                                                      0, 100, 42,
                                                      G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                				   PROP_SUPER_STUDLY,
				                   g_param_spec_double ("super-studly",
                                                        _("In Studly Caps"),
                                                        _("Example of a StudlyCaps property"),
                                                        0, 256, 128,
                                                        G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                				   PROP_SHOULD_BE_HIDDEN,
				                   g_param_spec_boolean ("should-be-hidden",
                                                        _("A non-exported property"),
                                                        _("Example of a property we don't want exported"),
                                                        FALSE,
                                                        G_PARAM_READWRITE));

  signals[FROBNICATE] =
    g_signal_new ("frobnicate",
		  G_OBJECT_CLASS_TYPE (mobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  g_cclosure_marshal_VOID__INT,
                  G_TYPE_NONE, 1, G_TYPE_INT);

  signals[OBJECTIFIED] =
    g_signal_new ("objectified",
                  G_OBJECT_CLASS_TYPE (mobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  g_cclosure_marshal_VOID__OBJECT,
                  G_TYPE_NONE, 1, G_TYPE_OBJECT);

  signals[SIG0] =
    g_signal_new ("sig0",
		  G_OBJECT_CLASS_TYPE (mobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  my_object_marshal_VOID__STRING_INT_STRING,
                  G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_STRING);

  signals[SIG1] =
    g_signal_new ("sig1",
		  G_OBJECT_CLASS_TYPE (mobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  my_object_marshal_VOID__STRING_BOXED,
                  G_TYPE_NONE, 2, G_TYPE_STRING, G_TYPE_VALUE);

  signals[SIG2] =
    g_signal_new ("sig2",
		  G_OBJECT_CLASS_TYPE (mobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  g_cclosure_marshal_VOID__BOXED,
                  G_TYPE_NONE, 1, DBUS_TYPE_G_STRING_STRING_HASHTABLE);
}

GQuark
my_object_error_quark (void)
{
  static GQuark quark = 0;
  if (!quark)
    quark = g_quark_from_static_string ("my_object_error");

  return quark;
}

/* This should really be standard. */
#define ENUM_ENTRY(NAME, DESC) { NAME, "" #NAME "", DESC }

GType
my_object_error_get_type (void)
{
	static GType etype = 0;

	if (etype == 0)
	{
		static const GEnumValue values[] =
		{

			ENUM_ENTRY (MY_OBJECT_ERROR_FOO, "Foo"),
			ENUM_ENTRY (MY_OBJECT_ERROR_BAR, "Bar"),
			ENUM_ENTRY (MY_OBJECT_ERROR_MULTI_WORD, "Multi-word"),
			ENUM_ENTRY (MY_OBJECT_ERROR_UNDER_SCORE, "Under_score"),
			{ 0, 0, 0 }
		};

		etype = g_enum_register_static ("MyObjectError", values);
	}

	return etype;
}

gboolean
my_object_do_nothing (MyObject *obj, GError **error)
{
  return TRUE;
}

gboolean
my_object_increment (MyObject *obj, gint32 x, gint32 *ret, GError **error)
{
  *ret = x +1;
  return TRUE;
}

gint32
my_object_increment_retval (MyObject *obj, gint32 x)
{
  return x + 1;
}

gint32
my_object_increment_retval_error (MyObject *obj, gint32 x, GError **error)
{
  if (x + 1 > 10)
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "%s",
		   "x is bigger than 9");    
      return FALSE;
    }
  return x + 1;
}

gboolean
my_object_throw_error (MyObject *obj, GError **error)
{
  g_set_error (error,
	       MY_OBJECT_ERROR,
	       MY_OBJECT_ERROR_FOO,
	       "%s",
	       "this method always loses");    
  return FALSE;
}

gboolean
my_object_throw_not_supported (MyObject *obj, GError **error)
{
  g_set_error (error,
	       DBUS_GERROR,
	       DBUS_GERROR_NOT_SUPPORTED,
	       "%s",
	       "this method always loses");
  return FALSE;
}

gboolean
my_object_throw_error_multi_word (MyObject *obj, GError **error)
{
  g_set_error (error,
	       MY_OBJECT_ERROR,
	       MY_OBJECT_ERROR_MULTI_WORD,
	       "%s",
	       "this method's error has a hyphen");    
  return FALSE;
}

static gboolean
my_object_throw_error_under_score (MyObject *obj, GError **error)
{
  g_set_error (error,
	       MY_OBJECT_ERROR,
	       MY_OBJECT_ERROR_UNDER_SCORE,
	       "%s",
	       "this method's error has an underscore");
  return FALSE;
}

gboolean
my_object_throw_unregistered_error (MyObject *obj, GError **error)
{
  /* Unregistered errors shouldn't cause a dbus abort.  See
   * https://bugzilla.redhat.com/show_bug.cgi?id=581794
   */
  g_set_error (error, 0, 0,
	       "%s",
	       "this method always loses more");
  return FALSE;
}


gboolean
my_object_uppercase (MyObject *obj, const char *str, char **ret, GError **error)
{
  *ret = g_ascii_strup (str, -1);
  return TRUE;
}

gboolean
my_object_many_args (MyObject *obj, guint32 x, const char *str, double trouble, double *d_ret, char **str_ret, GError **error)
{
  *d_ret = trouble + (x * 2);
  *str_ret = g_ascii_strup (str, -1);
  return TRUE;
}

gboolean
my_object_many_return (MyObject *obj, guint32 *arg0, char **arg1, gint32 *arg2, guint32 *arg3, guint32 *arg4, const char **arg5, GError **error)
{
  *arg0 = 42;
  *arg1 = g_strdup ("42");
  *arg2 = -67;
  *arg3 = 2;
  *arg4 = 26;
  *arg5 = "hello world"; /* Annotation specifies as const */
  return TRUE;
}

gboolean
my_object_stringify (MyObject *obj, GValue *value, char **ret, GError **error)
{
  GValue valstr = {0, };

  g_value_init (&valstr, G_TYPE_STRING);
  if (!g_value_transform (value, &valstr))
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "couldn't transform value");
      return FALSE;
    }
  *ret = g_value_dup_string (&valstr);
  g_value_unset (&valstr);
  return TRUE;
}

gboolean
my_object_unstringify (MyObject *obj, const char *str, GValue *value, GError **error)
{
  if (str[0] == '\0' || !g_ascii_isdigit (str[0])) {
    g_value_init (value, G_TYPE_STRING);
    g_value_set_string (value, str);
  } else {
    g_value_init (value, G_TYPE_INT);
    g_value_set_int (value, (int) g_ascii_strtoull (str, NULL, 10));
  } 
  return TRUE;
}

gboolean
my_object_recursive1 (MyObject *obj, GArray *array, guint32 *len_ret, GError **error)
{
  *len_ret = array->len;
  return TRUE;
}

gboolean
my_object_recursive2 (MyObject *obj, guint32 reqlen, GArray **ret, GError **error)
{
  guint32 val;
  GArray *array;
  
  array = g_array_new (FALSE, TRUE, sizeof (guint32));

  while (reqlen > 0) {
    val = 42;
    g_array_append_val (array, val);
    val = 26;
    g_array_append_val (array, val);
    reqlen--;
  }
  val = 2;
  g_array_append_val (array, val);
  *ret = array;
  return TRUE;
}

gboolean
my_object_many_uppercase (MyObject *obj, const char * const *in, char ***out, GError **error)
{
  int len;
  int i;

  len = g_strv_length ((char**) in);

  *out = g_new0 (char *, len + 1);
  for (i = 0; i < len; i++)
    {
      (*out)[i] = g_ascii_strup (in[i], -1);
    }
  (*out)[i] = NULL;
  
  return TRUE;
}

static void
hash_foreach_stringify (gpointer key, gpointer val, gpointer user_data)
{
  const char *keystr = key;
  const GValue *value = val;
  GValue *sval;
  GHashTable *ret = user_data;

  sval = g_new0 (GValue, 1);
  g_value_init (sval, G_TYPE_STRING);
  if (!g_value_transform (value, sval))
    g_assert_not_reached ();

  g_hash_table_insert (ret, g_strdup (keystr), sval);
}

static void
unset_and_free_gvalue (gpointer val)
{
  g_value_unset (val);
  g_free (val);
}

gboolean
my_object_many_stringify (MyObject *obj, GHashTable /* char * -> GValue * */ *vals, GHashTable /* char * -> GValue * */ **ret, GError **error)
{
  *ret = g_hash_table_new_full (g_str_hash, g_str_equal,
				g_free, unset_and_free_gvalue);
  g_hash_table_foreach (vals, hash_foreach_stringify, *ret);
  return TRUE;
}

gboolean
my_object_rec_arrays (MyObject *obj, GPtrArray *in, GPtrArray **ret, GError **error)
{
  char **strs;
  GArray *ints;
  guint v_UINT;
  
  if (in->len != 2)
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid array len");
      return FALSE;
    }
  
  strs = g_ptr_array_index (in, 0);
  if (!*strs || strcmp (*strs, "foo"))
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid string 0");
      return FALSE;
    }
  strs++;
  if (!*strs || strcmp (*strs, "bar"))
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid string 1");
      return FALSE;
    }
  strs++;
  if (*strs)
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid string array len in pos 0");
      return FALSE;
    }
  strs = g_ptr_array_index (in, 1);
  if (!*strs || strcmp (*strs, "baz"))
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid string 0");
      return FALSE;
    }
  strs++;
  if (!*strs || strcmp (*strs, "whee"))
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid string 1");
      return FALSE;
    }
  strs++;
  if (!*strs || strcmp (*strs, "moo"))
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid string 2");
      return FALSE;
    }
  strs++;
  if (*strs)
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid string array len in pos 1");
      return FALSE;
    }

  *ret = g_ptr_array_new ();

  ints = g_array_new (TRUE, TRUE, sizeof (guint));
  v_UINT = 10;
  g_array_append_val (ints, v_UINT);
  v_UINT = 42;
  g_array_append_val (ints, v_UINT);
  v_UINT = 27;
  g_array_append_val (ints, v_UINT);
  g_ptr_array_add (*ret, ints);

  ints = g_array_new (TRUE, TRUE, sizeof (guint));
  v_UINT = 30;
  g_array_append_val (ints, v_UINT);
  g_ptr_array_add (*ret, ints);
  return TRUE;
}

gboolean
my_object_objpath (MyObject *obj, const char *incoming, const char **outgoing, GError **error)
{
  if (strcmp (incoming, "/org/freedesktop/DBus/GLib/Tests/MyTestObject"))
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid incoming object");
      return FALSE;
    }
  *outgoing = "/org/freedesktop/DBus/GLib/Tests/MyTestObject2";
  return TRUE;
}

gboolean
my_object_get_objs (MyObject *obj, GPtrArray **objs, GError **error)
{
  *objs = g_ptr_array_new ();

  g_ptr_array_add (*objs, g_strdup ("/org/freedesktop/DBus/GLib/Tests/MyTestObject"));
  g_ptr_array_add (*objs, g_strdup ("/org/freedesktop/DBus/GLib/Tests/MyTestObject2"));

  return TRUE;
}

static void
hash_foreach (gpointer key, gpointer val, gpointer user_data)
{
  const char *keystr = key;
  const char *valstr = val;
  guint *count = user_data;

  *count += (strlen (keystr) + strlen (valstr));
  g_print ("%s -> %s\n", keystr, valstr);
}

gboolean
my_object_str_hash_len (MyObject *obj, GHashTable *table, guint *len, GError **error)
{
  *len = 0;
  g_hash_table_foreach (table, hash_foreach, len);
  return TRUE;
}

gboolean
my_object_send_car (MyObject *obj, GValueArray *invals, GValueArray **outvals, GError **error)
{
  if (invals->n_values != 3
      || G_VALUE_TYPE (g_value_array_get_nth (invals, 0)) != G_TYPE_STRING
      || G_VALUE_TYPE (g_value_array_get_nth (invals, 1)) != G_TYPE_UINT
      || G_VALUE_TYPE (g_value_array_get_nth (invals, 2)) != G_TYPE_VALUE)
    {
      g_set_error (error,
		   MY_OBJECT_ERROR,
		   MY_OBJECT_ERROR_FOO,
		   "invalid incoming values");
      return FALSE;
    }
  *outvals = g_value_array_new (2);
  g_value_array_append (*outvals, NULL);
  g_value_init (g_value_array_get_nth (*outvals, (*outvals)->n_values - 1), G_TYPE_UINT);
  g_value_set_uint (g_value_array_get_nth (*outvals, (*outvals)->n_values - 1),
		    g_value_get_uint (g_value_array_get_nth (invals, 1)) + 1);
  g_value_array_append (*outvals, NULL);
  g_value_init (g_value_array_get_nth (*outvals, (*outvals)->n_values - 1), DBUS_TYPE_G_OBJECT_PATH);
  g_value_set_boxed (g_value_array_get_nth (*outvals, (*outvals)->n_values - 1),
		     g_strdup ("/org/freedesktop/DBus/GLib/Tests/MyTestObject2"));
  return TRUE;
}

gboolean
my_object_get_hash (MyObject *obj, GHashTable **ret, GError **error)
{
  GHashTable *table;

  table = g_hash_table_new (g_str_hash, g_str_equal);
  g_hash_table_insert (table, "foo", "bar");
  g_hash_table_insert (table, "baz", "whee");
  g_hash_table_insert (table, "cow", "crack");
  *ret = table;
  return TRUE;
}

gboolean
my_object_increment_val (MyObject *obj, GError **error)
{
  obj->val++;
  return TRUE;
}

gboolean
my_object_get_val (MyObject *obj, guint *ret, GError **error)
{
  *ret = obj->val;
  return TRUE;
}

gboolean
my_object_get_value (MyObject *obj, guint *ret, GError **error)
{
  *ret = obj->val;
  return TRUE;
}

gboolean
my_object_echo_variant (MyObject *obj, GValue *variant, GValue *ret, GError **error)
{
    GType t;
    t = G_VALUE_TYPE(variant);
    g_value_init (ret, t);
    g_value_copy (variant, ret);

    return TRUE;
}

gboolean
my_object_echo_signature (MyObject *obj, const gchar *in, gchar **out, GError **error)
{
  *out = g_strdup (in);
  return TRUE;
}

gboolean 
my_object_process_variant_of_array_of_ints123 (MyObject *obj, GValue *variant, GError **error)
{
  GArray *array;
  int i;
  int j;

  j = 0;

  array = (GArray *)g_value_get_boxed (variant);

  for (i = 0; i <= 2; i++)
    {
      j = g_array_index (array, int, i);
      if (j != i + 1)
        goto error;
    }

  return TRUE;

error:
  *error = g_error_new (MY_OBJECT_ERROR,
		       MY_OBJECT_ERROR_FOO,
		       "Error decoding a variant of type ai (i + 1 = %i, j = %i)",
		       i, j + 1);
  return FALSE;
}


typedef struct _HashAndString HashAndString;

struct _HashAndString
{
  GHashTable *hash;
  gchar* string;
};

static void
hash_foreach_prepend_string (gpointer key, gpointer val, gpointer user_data)
{
  HashAndString *data = (HashAndString*) user_data;
  gchar *in = (gchar*) val;
  g_hash_table_insert (data->hash, g_strdup ((gchar*) key),
                       g_strjoin (" ", data->string, in, NULL));
}


static void
hash_foreach_mangle_dict_of_strings (gpointer key, gpointer val, gpointer user_data)
{
  GHashTable *out = (GHashTable*) user_data;
  GHashTable *in_dict = (GHashTable *) val;
  HashAndString *data = g_new0 (HashAndString, 1);

  data->string = (gchar*) key;
  data->hash = g_hash_table_new_full (g_str_hash, g_str_equal,
                                            g_free, g_free);
  g_hash_table_foreach (in_dict, hash_foreach_prepend_string, data);

  g_hash_table_insert(out, g_strdup ((gchar*) key), data->hash);
}

gboolean
my_object_dict_of_dicts (MyObject *obj, GHashTable *in,
                                GHashTable **out, GError **error)
{
  *out = g_hash_table_new_full (g_str_hash, g_str_equal,
				(GDestroyNotify) g_free,
                                (GDestroyNotify) g_hash_table_destroy);
  g_hash_table_foreach (in, hash_foreach_mangle_dict_of_strings, *out);
  return TRUE;
}

void
my_object_dict_of_sigs (MyObject *obj,
                        GHashTable *dict,
                        DBusGMethodInvocation *context)
{
  dbus_g_method_return (context, dict);
}

void
my_object_dict_of_objs (MyObject *obj,
                        GHashTable *dict,
                        DBusGMethodInvocation *context)
{
  dbus_g_method_return (context, dict);
}

gboolean
my_object_emit_frobnicate (MyObject *obj, GError **error)
{
  g_signal_emit (obj, signals[FROBNICATE], 0, 42);
  return TRUE;
}

gboolean
my_object_emit_signals (MyObject *obj, GError **error)
{
  GValue val = {0, };

  g_signal_emit (obj, signals[SIG0], 0, "foo", 22, "moo");

  g_value_init (&val, G_TYPE_STRING);
  g_value_set_string (&val, "bar");
  g_signal_emit (obj, signals[SIG1], 0, "baz", &val);
  g_value_unset (&val);

  return TRUE;
}

gboolean
my_object_emit_signal2 (MyObject *obj, GError **error)
{
  GHashTable *table;

  table = g_hash_table_new (g_str_hash, g_str_equal);
  g_hash_table_insert (table, "baz", "cow");
  g_hash_table_insert (table, "bar", "foo");
  g_signal_emit (obj, signals[SIG2], 0, table);
  g_hash_table_destroy (table);
  return TRUE;
}

typedef struct {
  gint32 x;
  DBusGMethodInvocation *context;
} IncrementData;

static gboolean
do_async_increment (IncrementData *data)
{
  gint32 newx = data->x + 1;
  dbus_g_method_return (data->context, newx);
  g_free (data);
  return FALSE;
}

void
my_object_async_increment (MyObject *obj, gint32 x, DBusGMethodInvocation *context)
{
  IncrementData *data = g_new0 (IncrementData, 1);
  data->x = x;
  data->context = context;
  g_idle_add ((GSourceFunc)do_async_increment, data);
}

static gboolean
do_async_error (IncrementData *data)
{
  GError *error;
  error = g_error_new (MY_OBJECT_ERROR,
		       MY_OBJECT_ERROR_FOO,
		       "%s",
		       "this method always loses");
  dbus_g_method_return_error (data->context, error);
  g_free (data);
  return FALSE;
}

void
my_object_async_throw_error (MyObject *obj, DBusGMethodInvocation *context)
{
  IncrementData *data = g_new0(IncrementData, 1);
  data->context = context;
  g_idle_add ((GSourceFunc)do_async_error,  data);
}

void
my_object_unsafe_disable_legacy_property_access (MyObject *obj)
{
  dbus_glib_global_set_disable_legacy_property_access ();
}

extern GMainLoop *loop;

gboolean
my_object_terminate (MyObject *obj, GError **error)
{
  g_main_loop_quit (loop);
  return TRUE;
}

void
my_object_emit_objectified (MyObject *obj,
    GObject *other)
{
  g_signal_emit (obj, signals[OBJECTIFIED], 0, other);
}
