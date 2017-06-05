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

#include <dbus/dbus-gvalue-parse-variant.h>

#include <string.h>

#include <dbus/dbus-glib.h>

/* Static functions in this file are a bit weird: they take a GVariant as first
 * argument, but it can be NULL. If it is, @value will be initialized with the
 * right type for that GVariant, but not filled in, so it'll contain 0 or NULL
 * or whatever. */

static void dbus_g_value_parse_variant_by_type (GVariant *variant,
    const GVariantType *variant_type,
    GValue *value);

static void
dbus_g_value_dict_parse_variant (GVariant *variant,
    const GVariantType *member_type,
    GValue *value)
{
  const GVariantType *key_type, *value_type;
  GValue key_parsed = { 0 }, value_parsed = { 0 };

  g_assert (g_variant_type_is_dict_entry (member_type));
  key_type = g_variant_type_key (member_type);
  value_type = g_variant_type_value (member_type);

  /* first get the GTypes, without getting actual values */
  dbus_g_value_parse_variant_by_type (NULL, key_type, &key_parsed);
  dbus_g_value_parse_variant_by_type (NULL, value_type, &value_parsed);

  g_value_init (value, dbus_g_type_get_map ("GHashTable",
        G_VALUE_TYPE (&key_parsed),
        G_VALUE_TYPE (&value_parsed)));
  g_value_unset (&key_parsed);
  g_value_unset (&value_parsed);

  if (variant != NULL)
    {
      GVariantIter iter;
      GVariant *child;
      DBusGTypeSpecializedAppendContext ctx;

      g_value_take_boxed (value, dbus_g_type_specialized_construct (
            G_VALUE_TYPE (value)));
      dbus_g_type_specialized_init_append (value, &ctx);

      g_variant_iter_init (&iter, variant);

      for (child = g_variant_iter_next_value (&iter);
          child != NULL;
          child = g_variant_iter_next_value (&iter))
        {
          dbus_g_value_parse_variant_by_type (
              g_variant_get_child_value (child, 0), key_type, &key_parsed);
          dbus_g_value_parse_variant_by_type (
              g_variant_get_child_value (child, 1), value_type, &value_parsed);

          /* Here be dragons: this steals the *contents of* key_parsed and
           * value_parsed, so we can't g_value_unset() them. */
          dbus_g_type_specialized_map_append (&ctx, &key_parsed,
              &value_parsed);
          memset (&key_parsed, '\0', sizeof (key_parsed));
          memset (&value_parsed, '\0', sizeof (value_parsed));
          g_variant_unref (child);
        }
    }
}

static void
dbus_g_value_basic_array_parse_variant (GVariant *variant,
    gchar type_char,
    GValue *value)
{
  GType gtype = G_TYPE_INVALID;
  guint dg_size = 0, gv_size = 0;

  switch ((GVariantClass) type_char)
    {
      case G_VARIANT_CLASS_STRING:
        g_value_init (value, G_TYPE_STRV);

        if (variant != NULL)
          g_value_take_boxed (value, g_variant_dup_strv (variant, NULL));
        return;

      case G_VARIANT_CLASS_OBJECT_PATH:
      case G_VARIANT_CLASS_SIGNATURE:
          {
            if (type_char == G_VARIANT_CLASS_OBJECT_PATH)
              gtype = DBUS_TYPE_G_OBJECT_PATH;
            else
              gtype = DBUS_TYPE_G_SIGNATURE;

            g_value_init (value,
                dbus_g_type_get_collection ("GPtrArray", gtype));

            if (variant != NULL)
              {
                gsize n = g_variant_n_children (variant);
                gsize i;
                GPtrArray *pa = g_ptr_array_sized_new (n);

                for (i = 0; i < n; i++)
                  {
                    gchar *s = g_variant_dup_string (
                        g_variant_get_child_value (variant, i), NULL);

                    g_ptr_array_add (pa, s);
                  }

                g_value_take_boxed (value, pa);
              }
          }
        return;

      /* From here down handles fixed-size types. */

      case G_VARIANT_CLASS_BYTE:
        gtype = G_TYPE_UCHAR;
        gv_size = dg_size = sizeof (guchar);
        break;

      case G_VARIANT_CLASS_BOOLEAN:
        gtype = G_TYPE_BOOLEAN;
        dg_size = sizeof (gboolean);
        gv_size = sizeof (guchar);
        break;

      case G_VARIANT_CLASS_INT16:
        gtype = G_TYPE_INT;
        dg_size = sizeof (gint);
        gv_size = sizeof (gint16);
        break;

      case G_VARIANT_CLASS_INT32:
        gtype = G_TYPE_INT;
        dg_size = sizeof (gint);
        gv_size = sizeof (gint32);
        break;

      case G_VARIANT_CLASS_UINT16:
        gtype = G_TYPE_UINT;
        dg_size = sizeof (guint);
        gv_size = sizeof (guint16);
        break;

      case G_VARIANT_CLASS_UINT32:
        gtype = G_TYPE_UINT;
        dg_size = sizeof (guint);
        gv_size = sizeof (guint32);
        break;

      case G_VARIANT_CLASS_INT64:
        gtype = G_TYPE_INT64;
        dg_size = gv_size = sizeof (gint64);
        break;

      case G_VARIANT_CLASS_UINT64:
        gtype = G_TYPE_UINT64;
        dg_size = gv_size = sizeof (guint64);
        break;

      case G_VARIANT_CLASS_DOUBLE:
        gtype = G_TYPE_DOUBLE;
        dg_size = gv_size = sizeof (gdouble);
        break;

      case G_VARIANT_CLASS_HANDLE:
      case G_VARIANT_CLASS_VARIANT:
      case G_VARIANT_CLASS_MAYBE:
      case G_VARIANT_CLASS_ARRAY:
      case G_VARIANT_CLASS_TUPLE:
      case G_VARIANT_CLASS_DICT_ENTRY:
        g_return_if_reached ();
    }

  g_assert (gtype != G_TYPE_INVALID);
  g_assert (dg_size != 0);
  g_assert (gv_size != 0);

  g_value_init (value, dbus_g_type_get_collection ("GArray", gtype));

  if (variant != NULL)
    {
      GArray *arr;
      gsize n, i;
      gconstpointer blob = g_variant_get_fixed_array (variant, &n, gv_size);

      arr = g_array_sized_new (FALSE, FALSE, dg_size, n);
      g_value_take_boxed (value, arr);

      if (dg_size == gv_size)
        {
          /* fast path: we can just memcpy them in */
          g_array_append_vals (arr, blob, n);
        }
      else
        {
          DBusGTypeSpecializedAppendContext ctx;

          dbus_g_type_specialized_init_append (value, &ctx);

          for (i = 0; i < n; i++)
            {
              GValue v = { 0 };

              dbus_g_value_parse_g_variant (
                  g_variant_get_child_value (variant, i), &v);
              dbus_g_type_specialized_collection_append (&ctx, &v);
            }

          dbus_g_type_specialized_collection_end_append (&ctx);
        }
    }
}

static void
dbus_g_value_tuple_parse_variant (GVariant *variant,
    const GVariantType *variant_type,
    GValue *value)
{
  gsize n = g_variant_type_n_items (variant_type);
  GType *types;
  gsize i;
  GValueArray *va = g_value_array_new (n);
  const GVariantType *inner_type;

  types = g_new0 (GType, n);

  for (i = 0, inner_type = g_variant_type_first (variant_type);
      i < n;
      i++, inner_type = g_variant_type_next (inner_type))
    {
      g_value_array_append (va, NULL);
      dbus_g_value_parse_variant_by_type (
          variant == NULL ? NULL : g_variant_get_child_value (variant, i),
          inner_type, &va->values[i]);
      types[i] = G_VALUE_TYPE (&va->values[i]);
    }

  g_value_init (value, dbus_g_type_get_structv ("GValueArray", n, types));

  if (variant == NULL)
    g_value_array_free (va);
  else
    g_value_take_boxed (value, va);

  g_free (types);
}

static void
dbus_g_value_array_parse_variant (GVariant *variant,
    const GVariantType *variant_type,
    GValue *value)
{
  const GVariantType *member_type;
  gchar type_char;
  GPtrArray *pa = NULL;
  gsize n = 0, i;

  g_assert (g_variant_type_is_array (variant_type));

  member_type = g_variant_type_element (variant_type);
  type_char = g_variant_type_peek_string (member_type)[0];

  if (g_variant_type_is_dict_entry (member_type))
    {
      dbus_g_value_dict_parse_variant (variant, member_type, value);
      return;
    }

  if (g_variant_type_is_basic (member_type))
    {
      dbus_g_value_basic_array_parse_variant (variant, type_char, value);
      return;
    }

  /* all the non-basic types end up as a GPtrArray of boxed */
  if (variant != NULL)
    {
      n = g_variant_n_children (variant);
      pa = g_ptr_array_sized_new (n);
    }

  switch ((GVariantClass) type_char)
    {
      case G_VARIANT_CLASS_VARIANT:
          {
            g_value_init (value,
                dbus_g_type_get_collection ("GPtrArray", G_TYPE_VALUE));
          }
        break;

      case G_VARIANT_CLASS_ARRAY:
          {
            GValue v = { 0 };

            dbus_g_value_array_parse_variant (NULL, member_type, &v);
            g_value_init (value, dbus_g_type_get_collection ("GPtrArray",
                  G_VALUE_TYPE (&v)));
          }
        break;

      case G_VARIANT_CLASS_TUPLE:
          {
            GValue v = { 0 };

            dbus_g_value_tuple_parse_variant (NULL, member_type, &v);
            g_value_init (value, dbus_g_type_get_collection ("GPtrArray",
                  G_VALUE_TYPE (&v)));
          }
        break;

      case G_VARIANT_CLASS_DICT_ENTRY:
      case G_VARIANT_CLASS_MAYBE:
      default:
        g_critical ("unhandled GVariantClass array<%d>", type_char);
        g_return_if_reached ();
    }

  if (variant != NULL)
    {
      for (i = 0; i < n; i++)
        {
          GValue tmp = { 0 };

          dbus_g_value_parse_g_variant (g_variant_get_child_value (variant, i),
              &tmp);
          g_ptr_array_add (pa, g_value_dup_boxed (&tmp));
          g_value_unset (&tmp);
        }

      g_value_take_boxed (value, pa);
    }
}

static void
dbus_g_value_parse_variant_by_type (GVariant *variant,
    const GVariantType *variant_type,
    GValue *value)
{
  gchar type_char = g_variant_type_peek_string (variant_type)[0];

  switch ((GVariantClass) type_char)
    {
      case G_VARIANT_CLASS_BOOLEAN:
        g_value_init (value, G_TYPE_BOOLEAN);

        if (variant != NULL)
          g_value_set_boolean (value, !!g_variant_get_boolean (variant));

        break;

      case G_VARIANT_CLASS_BYTE:
        g_value_init (value, G_TYPE_UCHAR);

        if (variant != NULL)
          g_value_set_uchar (value, g_variant_get_byte (variant));

        break;

      case G_VARIANT_CLASS_UINT16:
        /* there is no G_TYPE_UINT16 */
        g_value_init (value, G_TYPE_UINT);

        if (variant != NULL)
          g_value_set_uint (value, g_variant_get_uint16 (variant));

        break;

      case G_VARIANT_CLASS_UINT32:
        g_value_init (value, G_TYPE_UINT);

        if (variant != NULL)
          g_value_set_uint (value, g_variant_get_uint32 (variant));

        break;

      case G_VARIANT_CLASS_UINT64:
        g_value_init (value, G_TYPE_UINT64);

        if (variant != NULL)
          g_value_set_uint64 (value, g_variant_get_uint64 (variant));

        break;

      case G_VARIANT_CLASS_INT16:
        /* there is no G_TYPE_INT16 */
        g_value_init (value, G_TYPE_INT);

        if (variant != NULL)
          g_value_set_int (value, g_variant_get_int16 (variant));

        break;

      case G_VARIANT_CLASS_INT32:
        g_value_init (value, G_TYPE_INT);

        if (variant != NULL)
          g_value_set_int (value, g_variant_get_int32 (variant));

        break;

      case G_VARIANT_CLASS_INT64:
        g_value_init (value, G_TYPE_INT64);

        if (variant != NULL)
          g_value_set_int64 (value, g_variant_get_int64 (variant));

        break;

      case G_VARIANT_CLASS_DOUBLE:
        g_value_init (value, G_TYPE_DOUBLE);

        if (variant != NULL)
          g_value_set_double (value, g_variant_get_double (variant));

        break;

      case G_VARIANT_CLASS_STRING:
        g_value_init (value, G_TYPE_STRING);

        if (variant != NULL)
          g_value_set_string (value, g_variant_get_string (variant, NULL));

        break;

      case G_VARIANT_CLASS_OBJECT_PATH:
        g_value_init (value, DBUS_TYPE_G_OBJECT_PATH);

        if (variant != NULL)
          g_value_set_boxed (value, g_variant_get_string (variant, NULL));

        break;

      case G_VARIANT_CLASS_SIGNATURE:
        g_value_init (value, DBUS_TYPE_G_SIGNATURE);

        if (variant != NULL)
          g_value_set_boxed (value, g_variant_get_string (variant, NULL));

        break;

      case G_VARIANT_CLASS_VARIANT:
        g_value_init (value, G_TYPE_VALUE);

        if (variant != NULL)
          {
            GValue *inner_variant = g_new0 (GValue, 1);

            dbus_g_value_parse_g_variant (g_variant_get_variant (variant),
                inner_variant);

            g_value_take_boxed (value, inner_variant);
          }
        break;

      case G_VARIANT_CLASS_ARRAY:
        dbus_g_value_array_parse_variant (variant, variant_type, value);
        break;

      case G_VARIANT_CLASS_TUPLE:
        dbus_g_value_tuple_parse_variant (variant, variant_type, value);
        break;

      case G_VARIANT_CLASS_DICT_ENTRY:
        g_critical ("found a dict entry not in a dict");
        break;

      case G_VARIANT_CLASS_HANDLE:
      case G_VARIANT_CLASS_MAYBE:
        g_critical ("unhandled GVariantClass '%c' (%d)",
            CLAMP (type_char, ' ', '~'),
            type_char);
        break;
    }
}

/**
 * dbus_g_value_parse_g_variant:
 * @variant: a #GVariant
 * @value: a zero-filled #GValue
 *
 * Deserialize @variant and put an equivalent dbus-glib data structure in
 * @value.
 *
 * It is an error if @variant contains any #GVariant extensions not supported
 * by dbus-glib, including handles (file descriptor passing) and 'maybe' types.
 */
void
dbus_g_value_parse_g_variant (GVariant *variant,
    GValue *value)
{
  g_return_if_fail (variant != NULL);
  dbus_g_value_parse_variant_by_type (variant, g_variant_get_type (variant),
      value);
}
