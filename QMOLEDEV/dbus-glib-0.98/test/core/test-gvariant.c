/* GVariant to dbus-glib escape hatch
 *
 * Copyright © 2010 Collabora Ltd. <http://www.collabora.co.uk/>
 *
 * Licensed under the Academic Free License version 2.1
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Alternatively, at your option, you can redistribute and/or modify
 * this single file under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * that license, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <config.h>

#include <dbus/dbus-glib.h>
#include <gio/gio.h>

/**
 * test_g_variant_equivalent:
 *
 * The function g_variant_equal() cannot be used for dictionaries because it
 * cares about the ordering of dictionaries, which breaks our tests.
 */
static gboolean
test_g_variant_equivalent (GVariant *one,
    GVariant *two)
{
  if (!g_variant_type_equal (
        g_variant_get_type (one),
        g_variant_get_type (two)))
    {
      return FALSE;
    }
  else if (g_variant_is_of_type (one, G_VARIANT_TYPE_DICTIONARY) &&
           g_variant_is_of_type (two, G_VARIANT_TYPE_DICTIONARY))
    {
      GHashTable *hash;
      GVariantIter iter;
      GVariant *child;
      gboolean equal = TRUE;

      if (g_variant_n_children (one) != g_variant_n_children (two))
        return FALSE;

      /* pack @one into a hash table */
      hash = g_hash_table_new_full (g_variant_hash, g_variant_equal,
          (GDestroyNotify) g_variant_unref, (GDestroyNotify) g_variant_unref);

      g_variant_iter_init (&iter, one);
      while ((child = g_variant_iter_next_value (&iter)))
        {
          g_hash_table_insert (hash,
              g_variant_get_child_value (child, 0),
              g_variant_get_child_value (child, 1));
          g_variant_unref (child);
        }

      /* now iterate @two to check for the keys in @hash */
      g_variant_iter_init (&iter, two);
      while (equal && (child = g_variant_iter_next_value (&iter)))
        {
          GVariant *k, *v1, *v2;

          k = g_variant_get_child_value (child, 0);
          v1 = g_variant_get_child_value (child, 1);

          v2 = g_hash_table_lookup (hash, k);

          if (v2 == NULL || !test_g_variant_equivalent (v1, v2))
            equal = FALSE;
          else
            g_hash_table_remove (hash, k);

          g_variant_unref (k);
          g_variant_unref (v1);
          g_variant_unref (child);
        }

      if (g_hash_table_size (hash) > 0)
        equal = FALSE;

      g_hash_table_destroy (hash);

      return equal;
    }
  else if (g_variant_is_container (one) &&
           g_variant_is_container (two))
    {
      guint i, size;
      gboolean equal = TRUE;

      if (g_variant_n_children (one) != g_variant_n_children (two))
        return FALSE;

      size = g_variant_n_children (one);
      for (i = 0; equal && i < size; i++)
        {
          GVariant *child1, *child2;

          child1 = g_variant_get_child_value (one, i);
          child2 = g_variant_get_child_value (two, i);

          equal = test_g_variant_equivalent (child1, child2);

          g_variant_unref (child1);
          g_variant_unref (child2);
        }

      return equal;
    }
  else
    {
      return g_variant_equal (one, two);
    }
}

#define assert_g_variant_equivalent(a,e) \
  assert_g_variant_equivalent_internal (__FILE__, __LINE__, \
      #a, a, #e, e)

static void
assert_g_variant_equivalent_internal (
    const gchar *file, gint line,
    const gchar *actual_name, GVariant *actual,
    const gchar *expected_name, GVariant *expected);

static void
assert_g_variant_equivalent_internal (const gchar *file,
    gint line,
    const gchar *actual_name,
    GVariant *actual,
    const gchar *expected_name,
    GVariant *expected)
{
  if (!test_g_variant_equivalent (actual, expected))
    {
      gchar *a = g_variant_print (actual, TRUE);
      gchar *e = g_variant_print (expected, TRUE);

      g_error ("%s:%d: Variants should have been equal:\n"
          "%s = %s\n"
          "%s = %s", file, line, actual_name, a, expected_name, e);
      /* no point in freeing the strings, we've just crashed anyway */
    }
}

/* test_g_variant_equivalent tests */
static void
test_simple_equiv (void)
{
  GVariant *v1, *v2;

  v1 = g_variant_new_int32 (1984);
  v2 = g_variant_new_int32 (1984);

  g_assert (test_g_variant_equivalent (v1, v2));

  g_variant_unref (v1);
  g_variant_unref (v2);
}

static void
test_simple_not_equiv (void)
{
  GVariant *v1, *v2;

  v1 = g_variant_new_int32 (1982);
  v2 = g_variant_new_int32 (1984);

  g_assert (!test_g_variant_equivalent (v1, v2));

  g_variant_unref (v1);
  g_variant_unref (v2);
}

static void
test_array_not_equiv (void)
{
  GVariantBuilder b;
  GVariant *v1, *v2;

  g_variant_builder_init (&b, G_VARIANT_TYPE ("av"));
  g_variant_builder_add (&b, "v", g_variant_new_int32 (1984));
  g_variant_builder_add (&b, "v", g_variant_new_string ("Orwell"));
  g_variant_builder_add (&b, "v", g_variant_new_object_path ("/cats/escher"));
  v1 = g_variant_builder_end (&b);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("av"));
  /* note the order has changed */
  g_variant_builder_add (&b, "v", g_variant_new_string ("Orwell"));
  g_variant_builder_add (&b, "v", g_variant_new_int32 (1984));
  g_variant_builder_add (&b, "v", g_variant_new_object_path ("/cats/escher"));
  v2 = g_variant_builder_end (&b);

  g_assert (!test_g_variant_equivalent (v1, v2));

  g_variant_unref (v1);
  g_variant_unref (v2);
}

static void
test_map_equiv (void)
{
  GVariantBuilder b;
  GVariant *v1, *v2;

  g_variant_builder_init (&b, G_VARIANT_TYPE ("a{os}"));
  g_variant_builder_add (&b, "{os}", "/cats/escher", "Escher Moonbeam");
  g_variant_builder_add (&b, "{os}", "/cats/harvey", "Harvey Nomcat");
  g_variant_builder_add (&b, "{os}", "/cats/josh", "Josh Smith");
  v1 = g_variant_builder_end (&b);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("a{os}"));
  /* note the order has changed */
  g_variant_builder_add (&b, "{os}", "/cats/harvey", "Harvey Nomcat");
  g_variant_builder_add (&b, "{os}", "/cats/escher", "Escher Moonbeam");
  g_variant_builder_add (&b, "{os}", "/cats/josh", "Josh Smith");
  v2 = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (v1, v2));

  g_variant_unref (v1);
  g_variant_unref (v2);
}

static void
test_map_not_equiv1 (void)
{
  GVariantBuilder b;
  GVariant *v1, *v2;

  g_variant_builder_init (&b, G_VARIANT_TYPE ("a{os}"));
  g_variant_builder_add (&b, "{os}", "/cats/escher", "Escher Moonbeam");
  g_variant_builder_add (&b, "{os}", "/cats/harvey", "Harvey Nomcat");
  g_variant_builder_add (&b, "{os}", "/cats/josh", "Josh Smith");
  v1 = g_variant_builder_end (&b);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("a{os}"));
  g_variant_builder_add (&b, "{os}", "/cats/escher", "Escher Moonbeam");
  g_variant_builder_add (&b, "{os}", "/cats/harvey", "Harvey Nomcat");
  g_variant_builder_add (&b, "{os}", "/cats/josh", "Josh Smith");
  g_variant_builder_add (&b, "{os}", "/cats/rory", "Rory Cat");
  v2 = g_variant_builder_end (&b);

  g_assert (!test_g_variant_equivalent (v1, v2));

  g_variant_unref (v1);
  g_variant_unref (v2);
}

static void
test_map_not_equiv2 (void)
{
  GVariantBuilder b;
  GVariant *v1, *v2;

  g_variant_builder_init (&b, G_VARIANT_TYPE ("a{os}"));
  g_variant_builder_add (&b, "{os}", "/cats/escher", "Escher Moonbeam");
  g_variant_builder_add (&b, "{os}", "/cats/harvey", "Harvey Nomcat");
  g_variant_builder_add (&b, "{os}", "/cats/josh", "Josh Smith");
  v1 = g_variant_builder_end (&b);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("a{os}"));
  g_variant_builder_add (&b, "{os}", "/cats/escher", "Escher Moonbeam");
  g_variant_builder_add (&b, "{os}", "/cats/harvey", "Harvey Nomcat");
  g_variant_builder_add (&b, "{os}", "/cats/josh", "Josh Cat");
  v2 = g_variant_builder_end (&b);

  g_assert (!test_g_variant_equivalent (v1, v2));

  g_variant_unref (v1);
  g_variant_unref (v2);
}

/* dbus_g_value_build_g_variant tests */
static void
test_i (void)
{
  GValue v = { 0, };
  GVariant *var, *varc;

  g_value_init (&v, G_TYPE_INT);
  g_value_set_int (&v, 1984);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  varc = g_variant_new_int32 (1984);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_s (void)
{
  GValue v = { 0, };
  GVariant *var, *varc;

  g_value_init (&v, G_TYPE_STRING);
  g_value_set_static_string (&v, "Orwell");

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  varc = g_variant_new_string ("Orwell");

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_o (void)
{
  GValue v = { 0, };
  GVariant *var, *varc;

  g_value_init (&v, DBUS_TYPE_G_OBJECT_PATH);
  g_value_set_boxed (&v, "/cats/escher");

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  varc = g_variant_new_object_path ("/cats/escher");

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_us (void)
{
  GValue v = { 0, };
  GVariant *var, *varc;
  GType us = dbus_g_type_get_struct ("GValueArray",
      G_TYPE_UINT,
      G_TYPE_STRING,
      G_TYPE_INVALID);

  g_value_init (&v, us);
  g_value_take_boxed (&v, dbus_g_type_specialized_construct (us));
  dbus_g_type_struct_set (&v,
      0, 1984,
      1, "Orwell",
      G_MAXUINT);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  varc = g_variant_new ("(us)", 1984, "Orwell");

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_a_os (void)
{
  GValue v = { 0, };
  GHashTable *map;
  GVariantBuilder b;
  GVariant *var, *varc;
  GType a_os = dbus_g_type_get_map ("GHashTable",
      DBUS_TYPE_G_OBJECT_PATH,
      G_TYPE_STRING);

  g_value_init (&v, a_os);
  map = dbus_g_type_specialized_construct (a_os);

  g_hash_table_insert (map,
      g_strdup ("/cats/escher"), g_strdup ("Escher Moonbeam"));
  g_hash_table_insert (map,
      g_strdup ("/cats/harvey"), g_strdup ("Harvey Nomcat"));
  g_hash_table_insert (map,
      g_strdup ("/cats/josh"), g_strdup ("Josh Smith"));
  g_value_take_boxed (&v, map);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("a{os}"));
  g_variant_builder_add (&b, "{os}", "/cats/escher", "Escher Moonbeam");
  g_variant_builder_add (&b, "{os}", "/cats/harvey", "Harvey Nomcat");
  g_variant_builder_add (&b, "{os}", "/cats/josh", "Josh Smith");
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_av (void)
{
  GValue v = { 0, }, *v2;
  GVariantBuilder b;
  GVariant *var, *varc;
  GType av = dbus_g_type_get_collection ("GPtrArray", G_TYPE_VALUE);
  GPtrArray *array;

  g_value_init (&v, av);
  array = dbus_g_type_specialized_construct (av);

  v2 = g_new0 (GValue, 1);
  g_value_init (v2, G_TYPE_INT);
  g_value_set_int (v2, 1984);
  g_ptr_array_add (array, v2);

  v2 = g_new0 (GValue, 1);
  g_value_init (v2, G_TYPE_STRING);
  g_value_set_static_string (v2, "Orwell");
  g_ptr_array_add (array, v2);

  v2 = g_new0 (GValue, 1);
  g_value_init (v2, DBUS_TYPE_G_OBJECT_PATH);
  g_value_set_boxed (v2, "/cats/escher");
  g_ptr_array_add (array, v2);

  g_value_take_boxed (&v, array);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("av"));
  g_variant_builder_add (&b, "v", g_variant_new_int32 (1984));
  g_variant_builder_add (&b, "v", g_variant_new_string ("Orwell"));
  g_variant_builder_add (&b, "v", g_variant_new_object_path ("/cats/escher"));
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_ab (void)
{
  GValue v = { 0, };
  gboolean bools[] = { TRUE, FALSE };
  GVariantBuilder b;
  GVariant *var, *varc;
  GType ab = dbus_g_type_get_collection ("GArray", G_TYPE_BOOLEAN);
  GArray *array;

  g_value_init (&v, ab);
  array = dbus_g_type_specialized_construct (ab);

  g_array_append_vals (array, bools, 2);
  g_assert_cmpint (g_array_index (array, gboolean, 0), ==, TRUE);
  g_assert_cmpint (g_array_index (array, gboolean, 1), ==, FALSE);

  g_value_take_boxed (&v, array);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("ab"));
  g_variant_builder_add (&b, "b", TRUE);
  g_variant_builder_add (&b, "b", FALSE);
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_ai (void)
{
  GValue v = { 0, };
  gint ints[] = { 1984, 1066 };
  GVariantBuilder b;
  GVariant *var, *varc;
  GType ai = dbus_g_type_get_collection ("GArray", G_TYPE_INT);
  GArray *array;

  g_value_init (&v, ai);
  array = dbus_g_type_specialized_construct (ai);

  g_array_append_vals (array, ints, 2);
  g_assert_cmpint (g_array_index (array, gint, 0), ==, 1984);
  g_assert_cmpint (g_array_index (array, gint, 1), ==, 1066);

  g_value_take_boxed (&v, array);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("ai"));
  g_variant_builder_add (&b, "i", 1984);
  g_variant_builder_add (&b, "i", 1066);
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_au (void)
{
  GValue v = { 0, };
  guint uints[] = { 1984, 1066 };
  GVariantBuilder b;
  GVariant *var, *varc;
  GType au = dbus_g_type_get_collection ("GArray", G_TYPE_UINT);
  GArray *array;

  g_value_init (&v, au);
  array = dbus_g_type_specialized_construct (au);

  g_array_append_vals (array, uints, 2);
  g_assert_cmpuint (g_array_index (array, guint, 0), ==, 1984);
  g_assert_cmpuint (g_array_index (array, guint, 1), ==, 1066);

  g_value_take_boxed (&v, array);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("au"));
  g_variant_builder_add (&b, "u", 1984);
  g_variant_builder_add (&b, "u", 1066);
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_ax (void)
{
  GValue v = { 0, };
  gint64 ints[] = { G_GINT64_CONSTANT (-0xAAABBBBCCCCDDDD), 1066 };
  GVariantBuilder b;
  GVariant *var, *varc;
  GType ax = dbus_g_type_get_collection ("GArray", G_TYPE_INT64);
  GArray *array;

  g_value_init (&v, ax);
  array = dbus_g_type_specialized_construct (ax);

  g_array_append_vals (array, ints, 2);
  g_assert_cmpint ((g_array_index (array, gint64, 0)
        / G_GINT64_CONSTANT (0x100000000)), ==,
      -0xAAABBBB);
  g_assert_cmpuint ((-(g_array_index (array, gint64, 0)))
      % G_GINT64_CONSTANT (0x100000000), ==, 0xCCCCDDDDu);
  g_assert_cmpint ((g_array_index (array, gint64, 1)
        / G_GINT64_CONSTANT (0x100000000)), ==, 0);
  g_assert_cmpuint ((g_array_index (array, gint64, 1))
      % G_GINT64_CONSTANT (0x100000000), ==, 1066);

  g_value_take_boxed (&v, array);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("ax"));
  g_variant_builder_add (&b, "x", G_GINT64_CONSTANT (-0xAAABBBBCCCCDDDD));
  g_variant_builder_add (&b, "x", G_GINT64_CONSTANT (1066));
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_at (void)
{
  GValue v = { 0, };
  guint64 uints[] = { G_GUINT64_CONSTANT (0xAAAABBBBCCCCDDDD), 1066 };
  GVariantBuilder b;
  GVariant *var, *varc;
  GType at = dbus_g_type_get_collection ("GArray", G_TYPE_UINT64);
  GArray *array;

  g_value_init (&v, at);
  array = dbus_g_type_specialized_construct (at);

  g_array_append_vals (array, uints, 2);
  g_assert_cmpuint ((g_array_index (array, guint64, 0)
        / G_GUINT64_CONSTANT (0x100000000)), ==, 0xAAAABBBBu);
  g_assert_cmpuint ((g_array_index (array, guint64, 0)
        % G_GUINT64_CONSTANT (0x100000000)), ==, 0xCCCCDDDDu);
  g_assert_cmpuint ((g_array_index (array, guint64, 1)
        / G_GUINT64_CONSTANT (0x100000000)), ==, 0);
  g_assert_cmpuint ((g_array_index (array, guint64, 1)
        % G_GUINT64_CONSTANT (0x100000000)), ==, 1066);

  g_value_take_boxed (&v, array);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("at"));
  g_variant_builder_add (&b, "t", G_GUINT64_CONSTANT (0xAAAABBBBCCCCDDDD));
  g_variant_builder_add (&b, "t", G_GUINT64_CONSTANT (1066));
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_ay (void)
{
  GValue v = { 0, };
  guchar bytes[] = { 23, 42 };
  GVariantBuilder b;
  GVariant *var, *varc;
  GType ay = dbus_g_type_get_collection ("GArray", G_TYPE_UCHAR);
  GArray *array;

  g_value_init (&v, ay);
  array = dbus_g_type_specialized_construct (ay);

  g_array_append_vals (array, bytes, 2);
  g_assert_cmpint (g_array_index (array, guchar, 0), ==, 23);
  g_assert_cmpint (g_array_index (array, guchar, 1), ==, 42);

  g_value_take_boxed (&v, array);

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  g_variant_builder_init (&b, G_VARIANT_TYPE ("ay"));
  g_variant_builder_add (&b, "y", 23);
  g_variant_builder_add (&b, "y", 42);
  varc = g_variant_builder_end (&b);

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_g (void)
{
  GValue v = { 0, };
  GVariant *var, *varc;

  g_value_init (&v, DBUS_TYPE_G_SIGNATURE);
  g_value_set_boxed (&v, "a{u(ua{sa{sv}})}");

  var = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);

  varc = g_variant_new_signature ("a{u(ua{sa{sv}})}");

  g_assert (test_g_variant_equivalent (var, varc));

  g_variant_unref (var);
  g_variant_unref (varc);
}

static void
test_roundtrip (gconstpointer user_data)
{
  const gchar *text = user_data;
  GVariant *before, *after;
  GValue v = { 0 };

  before = g_variant_new_parsed (text);
  dbus_g_value_parse_g_variant (before, &v);
  after = dbus_g_value_build_g_variant (&v);
  g_value_unset (&v);
  assert_g_variant_equivalent (before, after);
  g_variant_unref (before);
  g_variant_unref (after);
}

static void
test_parse_basic (void)
{
  GVariant *variant;
  GValue v = { 0 };

  variant = g_variant_new_parsed ("'o hai'");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_STRING));
  g_assert_cmpstr (g_value_get_string (&v), ==, "o hai");
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("objectpath '/hello/world'");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==,
      g_type_name (DBUS_TYPE_G_OBJECT_PATH));
  g_assert_cmpstr ((gchar *) g_value_get_boxed (&v), ==, "/hello/world");
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("signature 'a{sv}'");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==,
      g_type_name (DBUS_TYPE_G_SIGNATURE));
  g_assert_cmpstr ((gchar *) g_value_get_boxed (&v), ==, "a{sv}");
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("23.5");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_DOUBLE));
  /* this is chosen to be exactly representable in binary; we use inequalities
   * to work around -Wfloat-equal */
  g_assert_cmpfloat (g_value_get_double (&v), >=, 23.5);
  g_assert_cmpfloat (g_value_get_double (&v), <=, 23.5);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("byte 42");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_UCHAR));
  g_assert_cmpuint (g_value_get_uchar (&v), ==, 42);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("uint16 16");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_UINT));
  g_assert_cmpuint (g_value_get_uint (&v), ==, 16);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("uint32 32");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_UINT));
  g_assert_cmpuint (g_value_get_uint (&v), ==, 32);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("uint64 64");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_UINT64));
  g_assert_cmpuint ((guint) g_value_get_uint64 (&v), ==, 64);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("int16 -16");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_INT));
  g_assert_cmpint (g_value_get_int (&v), ==, -16);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("int32 -32");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_INT));
  g_assert_cmpint (g_value_get_int (&v), ==, -32);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("int64 -64");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==, g_type_name (G_TYPE_INT64));
  g_assert_cmpint ((gint) g_value_get_int64 (&v), ==, -64);
  g_value_unset (&v);
  g_variant_unref (variant);
}

static void
test_parse_array (void)
{
  GVariant *variant;
  GValue v = { 0 };
  GArray *a;

  /* We can't test the 16-bit cases via a round-trip, because information is
   * lost. */

  variant = g_variant_new_parsed ("[uint16 16, uint16 1600]");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==,
      g_type_name (DBUS_TYPE_G_UINT_ARRAY));
  a = g_value_get_boxed (&v);
  g_assert_cmpuint (a->len, ==, 2);
  g_assert_cmpuint (g_array_index (a, guint, 0), ==, 16);
  g_assert_cmpuint (g_array_index (a, guint, 1), ==, 1600);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("[int16 -16, int16 -1600]");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==,
      g_type_name (DBUS_TYPE_G_INT_ARRAY));
  a = g_value_get_boxed (&v);
  g_assert_cmpuint (a->len, ==, 2);
  g_assert_cmpint (g_array_index (a, gint, 0), ==, -16);
  g_assert_cmpint (g_array_index (a, gint, 1), ==, -1600);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("@aq []");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==,
      g_type_name (DBUS_TYPE_G_UINT_ARRAY));
  a = g_value_get_boxed (&v);
  g_assert_cmpuint (a->len, ==, 0);
  g_value_unset (&v);
  g_variant_unref (variant);

  variant = g_variant_new_parsed ("@an []");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==,
      g_type_name (DBUS_TYPE_G_INT_ARRAY));
  a = g_value_get_boxed (&v);
  g_assert_cmpuint (a->len, ==, 0);
  g_value_unset (&v);
  g_variant_unref (variant);
}

static void
test_parse_string_hash (void)
{
  GVariant *variant;
  GHashTable *h;
  GValue v = { 0 };

  variant = g_variant_new_parsed ("@a{ss} {'foo': 'bar'}");
  dbus_g_value_parse_g_variant (variant, &v);
  g_assert_cmpstr (G_VALUE_TYPE_NAME (&v), ==,
      g_type_name (DBUS_TYPE_G_STRING_STRING_HASHTABLE));
  h = g_value_get_boxed (&v);
  g_assert_cmpuint (g_hash_table_size (h), ==, 1);
  g_assert_cmpstr (g_hash_table_lookup (h, "foo"), ==, "bar");
  g_value_unset (&v);
  g_variant_unref (variant);
}

int
main (int argc,
    char **argv)
{
  g_type_init ();
  dbus_g_type_specialized_init ();

  g_test_init (&argc, &argv, NULL);

  /* test_g_variant_equivalent tests */
  g_test_add_func ("/test_g_variant_equivalent/test_simple_equiv",
      test_simple_equiv);
  g_test_add_func ("/test_g_variant_equivalent/test_simple_not_equiv",
      test_simple_not_equiv);
  g_test_add_func ("/test_g_variant_equivalent/test_array_not_equiv",
      test_array_not_equiv);
  g_test_add_func ("/test_g_variant_equivalent/test_map_equiv",
      test_map_equiv);
  g_test_add_func ("/test_g_variant_equivalent/test_map_not_equiv1",
      test_map_not_equiv1);
  g_test_add_func ("/test_g_variant_equivalent/test_map_not_equiv2",
      test_map_not_equiv2);

  /* dbus_g_value_build_g_variant tests */
  g_test_add_func ("/gvalue-to-gvariant/i", test_i);
  g_test_add_func ("/gvalue-to-gvariant/s", test_s);
  g_test_add_func ("/gvalue-to-gvariant/o", test_o);
  g_test_add_func ("/gvalue-to-gvariant/us", test_us);
  g_test_add_func ("/gvalue-to-gvariant/a{os}", test_a_os);
  g_test_add_func ("/gvalue-to-gvariant/av", test_av);
  g_test_add_func ("/gvalue-to-gvariant/ab", test_ab);
  g_test_add_func ("/gvalue-to-gvariant/ai", test_ai);
  g_test_add_func ("/gvalue-to-gvariant/au", test_au);
  g_test_add_func ("/gvalue-to-gvariant/ax", test_ax);
  g_test_add_func ("/gvalue-to-gvariant/at", test_at);
  g_test_add_func ("/gvalue-to-gvariant/ay", test_ay);
  g_test_add_func ("/gvalue-to-gvariant/g", test_g);

  /* dbus_g_value_parse_g_variant tests */
  g_test_add_func ("/parse-gvariant/basic", test_parse_basic);
  g_test_add_func ("/parse-gvariant/array", test_parse_array);
  g_test_add_func ("/parse-gvariant/string_hash", test_parse_string_hash);

  /* round-trips */
  g_test_add_data_func ("/parse-gvariant/roundtrip/u",
      "@u 42", test_roundtrip);
  g_test_add_data_func ("/parse-gvariant/roundtrip/non_empty_array",
      "@ai [23, 42]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_array", "@ai []", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aav", "[[<'bees'>]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aau", "[[uint32 666]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aax", "[[int64 666]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aat", "[[uint64 666]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aas", "[['a', 'b']]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aao",
      "[[objectpath '/a', objectpath '/b']]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aag", "[[signature 'ab', signature 'g']]",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/aad", "[[5.25, 3.5]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aay", "@aay [[1, 2]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aay", "@aay [[]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aab", "[[true, false]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aab", "@aab [[]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/aa_asv", "[[@a{sv} {'x': <'y'>}]]",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_av", "@av []", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_hash", "@a{uu} {}", test_roundtrip);
  g_test_add_data_func ("/roundtrip/easy_string_hash",
      "@a{ss} {'foo': 'bar'}", test_roundtrip);
  g_test_add_data_func ("/roundtrip/non_empty_asv",
      "@a{sv} {'badger': <42>, 'mushroom': <objectpath '/'>, 'snake': <''>}",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/variant_nesting", "<<<42>>>",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/tuple", "(23, 42, true)",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/nested", "[[[(1, 2)]]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aaa", "@aaav [[]]",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aa_asv", "@aaa{sv} [[]]",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aa_struct", "@aa(us) [[]]",
      test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aaas", "@aaas [[]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aax", "@aax [[]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aat", "@aat [[]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aad", "@aad [[]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aao", "@aao [[]]", test_roundtrip);
  g_test_add_data_func ("/roundtrip/empty_aag", "@aag [[]]", test_roundtrip);

  return g_test_run ();
}
