/* LIBGTK - The GTK Library
 * Copyright (C) 1995-1997 Peter Mattis and Spencer Kimball
 *
 * gimpunit.c
 * Copyright (C) 2003 Michael Natterer <mitch@gimp.org>
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include "claws-features.h"

#include "gtkunit.h"

#include <string.h>

#include <glib-object.h>


GtkCMUnitVtable _gtk_unit_vtable = { NULL, };


void
gtk_base_init (GtkCMUnitVtable *vtable)
{
  static gboolean gtk_base_initialized = FALSE;

  g_return_if_fail (vtable != NULL);

  if (gtk_base_initialized)
    g_error ("gtk_base_init() must only be called once!");

  _gtk_unit_vtable = *vtable;

  gtk_base_initialized = TRUE;
}


/**
 * SECTION: gimpunit
 * @title: gimpunit
 * @short_description: Provides a collection of predefined units and
 *                     functions for creating user-defined units.
 * @see_also: #GtkCMUnitMenu, #GtkSizeEntry.
 *
 * Provides a collection of predefined units and functions for
 * creating user-defined units.
 **/


static void   unit_to_string (const GValue *src_value,
                              GValue       *dest_value);
static void   string_to_unit (const GValue *src_value,
                              GValue       *dest_value);

GType
gtk_unit_get_type (void)
{
  static GType unit_type = 0;

  if (! unit_type)
    {
      const GTypeInfo type_info = { 0, };

      unit_type = g_type_register_static (G_TYPE_INT, "GtkCMUnit",
                                          &type_info, 0);

      g_value_register_transform_func (unit_type, G_TYPE_STRING,
                                       unit_to_string);
      g_value_register_transform_func (G_TYPE_STRING, unit_type,
                                       string_to_unit);
    }

  return unit_type;
}

static void
unit_to_string (const GValue *src_value,
                GValue       *dest_value)
{
  GtkCMUnit unit = (GtkCMUnit) g_value_get_int (src_value);

  g_value_set_string (dest_value, gtk_unit_get_identifier (unit));
}

static void
string_to_unit (const GValue *src_value,
                GValue       *dest_value)
{
  const gchar *str;
  gint         num_units;
  gint         i;

  str = g_value_get_string (src_value);

  if (!str || !*str)
    goto error;

  num_units = gtk_unit_get_number_of_units ();

  for (i = CM_UNIT_PIXEL; i < num_units; i++)
    if (strcmp (str, gtk_unit_get_identifier (i)) == 0)
      break;

  if (i == num_units)
    {
      if (strcmp (str, gtk_unit_get_identifier (CM_UNIT_PERCENT)) == 0)
        i = CM_UNIT_PERCENT;
      else
        goto error;
    }

  g_value_set_int (dest_value, i);
  return;

 error:
  g_warning ("Can't convert string to GtkCMUnit.");
}


/**
 * gtk_unit_get_number_of_units:
 *
 * Returns the number of units which are known to the #GtkCMUnit system.
 *
 * Returns: The number of defined units.
 **/
gint
gtk_unit_get_number_of_units (void)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_number_of_units != NULL,
                        CM_UNIT_END);

  return _gtk_unit_vtable.unit_get_number_of_units ();
}

/**
 * gtk_unit_get_number_of_built_in_units:
 *
 * Returns the number of #GtkCMUnit's which are hardcoded in the unit system
 * (UNIT_INCH, UNIT_MM, UNIT_POINT, UNIT_PICA and the two "pseudo unit"
 *  UNIT_PIXEL).
 *
 * Returns: The number of built-in units.
 **/
gint
gtk_unit_get_number_of_built_in_units (void)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_number_of_built_in_units
                        != NULL, CM_UNIT_END);

  return _gtk_unit_vtable.unit_get_number_of_built_in_units ();
}

/**
 * gtk_unit_new:
 * @identifier: The unit's identifier string.
 * @factor: The unit's factor (how many units are in one inch).
 * @digits: The unit's suggested number of digits (see gtk_unit_get_digits()).
 * @symbol: The symbol of the unit (e.g. "''" for inch).
 * @abbreviation: The abbreviation of the unit.
 * @singular: The singular form of the unit.
 * @plural: The plural form of the unit.
 *
 * Returns the integer ID of the new #GtkCMUnit.
 *
 * Note that a new unit is always created with it's deletion flag
 * set to %TRUE. You will have to set it to %FALSE with
 * gtk_unit_set_deletion_flag() to make the unit definition persistent.
 *
 * Returns: The ID of the new unit.
 **/
GtkCMUnit
gtk_unit_new (gchar   *identifier,
               gdouble  factor,
               gint     digits,
               gchar   *symbol,
               gchar   *abbreviation,
               gchar   *singular,
               gchar   *plural)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_new != NULL, CM_UNIT_INCH);

  return _gtk_unit_vtable.unit_new (identifier, factor, digits,
                                     symbol, abbreviation, singular, plural);
}

/**
 * gtk_unit_get_deletion_flag:
 * @unit: The unit you want to know the @deletion_flag of.
 *
 * Returns: The unit's @deletion_flag.
 **/
gboolean
gtk_unit_get_deletion_flag (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_deletion_flag != NULL, FALSE);

  return _gtk_unit_vtable.unit_get_deletion_flag (unit);
}

/**
 * gtk_unit_set_deletion_flag:
 * @unit: The unit you want to set the @deletion_flag for.
 * @deletion_flag: The new deletion_flag.
 *
 * Sets a #GtkCMUnit's @deletion_flag. If the @deletion_flag of a unit is
 * %TRUE when GTK exits, this unit will not be saved in the users's
 * "unitrc" file.
 *
 * Trying to change the @deletion_flag of a built-in unit will be silently
 * ignored.
 **/
void
gtk_unit_set_deletion_flag (GtkCMUnit unit,
                             gboolean deletion_flag)
{
  g_return_if_fail (_gtk_unit_vtable.unit_set_deletion_flag != NULL);

  _gtk_unit_vtable.unit_set_deletion_flag (unit, deletion_flag);
}

/**
 * gtk_unit_get_factor:
 * @unit: The unit you want to know the factor of.
 *
 * A #GtkCMUnit's @factor is defined to be:
 *
 * distance_in_units == (@factor * distance_in_inches)
 *
 * Returns 0 for @unit == CM_UNIT_PIXEL.
 *
 * Returns: The unit's factor.
 **/
gdouble
gtk_unit_get_factor (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_factor != NULL, 1.0);

  return _gtk_unit_vtable.unit_get_factor (unit);
}

/**
 * gtk_unit_get_digits:
 * @unit: The unit you want to know the digits.
 *
 * Returns the number of digits an entry field should provide to get
 * approximately the same accuracy as an inch input field with two digits.
 *
 * Returns 0 for @unit == CM_UNIT_PIXEL.
 *
 * Returns: The suggested number of digits.
 **/
gint
gtk_unit_get_digits (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_digits != NULL, 2);

  return _gtk_unit_vtable.unit_get_digits (unit);
}

/**
 * gtk_unit_get_identifier:
 * @unit: The unit you want to know the identifier of.
 *
 * This is an unstranslated string and must not be changed or freed.
 *
 * Returns: The unit's identifier.
 **/
const gchar *
gtk_unit_get_identifier (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_identifier != NULL, NULL);

  return _gtk_unit_vtable.unit_get_identifier (unit);
}

/**
 * gtk_unit_get_symbol:
 * @unit: The unit you want to know the symbol of.
 *
 * This is e.g. "''" for UNIT_INCH.
 *
 * NOTE: This string must not be changed or freed.
 *
 * Returns: The unit's symbol.
 **/
const gchar *
gtk_unit_get_symbol (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_symbol != NULL, NULL);

  return _gtk_unit_vtable.unit_get_symbol (unit);
}

/**
 * gtk_unit_get_abbreviation:
 * @unit: The unit you want to know the abbreviation of.
 *
 * For built-in units, this function returns the translated abbreviation
 * of the unit.
 *
 * NOTE: This string must not be changed or freed.
 *
 * Returns: The unit's abbreviation.
 **/
const gchar *
gtk_unit_get_abbreviation (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_abbreviation != NULL, NULL);

  return _gtk_unit_vtable.unit_get_abbreviation (unit);
}

/**
 * gtk_unit_get_singular:
 * @unit: The unit you want to know the singular form of.
 *
 * For built-in units, this function returns the translated singular form
 * of the unit's name.
 *
 * NOTE: This string must not be changed or freed.
 *
 * Returns: The unit's singular form.
 **/
const gchar *
gtk_unit_get_singular (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_singular != NULL, NULL);

  return _gtk_unit_vtable.unit_get_singular (unit);
}

/**
 * gtk_unit_get_plural:
 * @unit: The unit you want to know the plural form of.
 *
 * For built-in units, this function returns the translated plural form
 * of the unit's name.
 *
 * NOTE: This string must not be changed or freed.
 *
 * Returns: The unit's plural form.
 **/
const gchar *
gtk_unit_get_plural (GtkCMUnit unit)
{
  g_return_val_if_fail (_gtk_unit_vtable.unit_get_plural != NULL, NULL);

  return _gtk_unit_vtable.unit_get_plural (unit);
}

static gint
print (gchar       *buf,
       gint         len,
       gint         start,
       const gchar *fmt,
       ...)
{
  va_list args;
  gint printed;

  va_start (args, fmt);

  printed = g_vsnprintf (buf + start, len - start, fmt, args);
  if (printed < 0)
    printed = len - start;

  va_end (args);

  return printed;
}

/**
 * gtk_unit_format_string:
 * @format: A printf-like format string which is used to create the unit
 *          string.
 * @unit:   A unit.
 *
 * The @format string supports the following percent expansions:
 *
 * <informaltable pgwide="1" frame="none" role="enum">
 *   <tgroup cols="2"><colspec colwidth="1*"/><colspec colwidth="8*"/>
 *     <tbody>
 *       <row>
 *         <entry>% f</entry>
 *         <entry>Factor (how many units make up an inch)</entry>
 *        </row>
 *       <row>
 *         <entry>% y</entry>
 *         <entry>Symbol (e.g. "''" for CM_UNIT_INCH)</entry>
 *       </row>
 *       <row>
 *         <entry>% a</entry>
 *         <entry>Abbreviation</entry>
 *       </row>
 *       <row>
 *         <entry>% s</entry>
 *         <entry>Singular</entry>
 *       </row>
 *       <row>
 *         <entry>% p</entry>
 *         <entry>Plural</entry>
 *       </row>
 *       <row>
 *         <entry>%%</entry>
 *         <entry>Literal percent</entry>
 *       </row>
 *     </tbody>
 *   </tgroup>
 * </informaltable>
 *
 * Returns: A newly allocated string with above percent expressions
 *          replaced with the resp. strings for @unit.
 *
 * Since: GTK 2.8
 **/
gchar *
gtk_unit_format_string (const gchar *format,
                         GtkCMUnit     unit)
{
  gchar buffer[1024];
  gint  i = 0;

  g_return_val_if_fail (format != NULL, NULL);
  g_return_val_if_fail (unit == CM_UNIT_PERCENT ||
                        (unit >= CM_UNIT_PIXEL &&
                         unit < gtk_unit_get_number_of_units ()), NULL);

  while (i < (sizeof (buffer) - 1) && *format)
    {
      switch (*format)
        {
        case '%':
          format++;
          switch (*format)
            {
            case 0:
              g_warning ("%s: unit-menu-format string ended within %%-sequence",
                         G_STRFUNC);
              break;

            case '%':
              buffer[i++] = '%';
              break;

            case 'f': /* factor (how many units make up an inch) */
              i += print (buffer, sizeof (buffer), i, "%f",
                          gtk_unit_get_factor (unit));
              break;

            case 'y': /* symbol ("''" for inch) */
              i += print (buffer, sizeof (buffer), i, "%s",
                          gtk_unit_get_symbol (unit));
              break;

            case 'a': /* abbreviation */
              i += print (buffer, sizeof (buffer), i, "%s",
                          gtk_unit_get_abbreviation (unit));
              break;

            case 's': /* singular */
              i += print (buffer, sizeof (buffer), i, "%s",
                          gtk_unit_get_singular (unit));
              break;

            case 'p': /* plural */
              i += print (buffer, sizeof (buffer), i, "%s",
                          gtk_unit_get_plural (unit));
              break;

            default:
              g_warning ("%s: unit-menu-format contains unknown format "
                         "sequence '%%%c'", G_STRFUNC, *format);
              break;
            }
          break;

        default:
          buffer[i++] = *format;
          break;
        }

      format++;
    }

  buffer[MIN (i, sizeof (buffer) - 1)] = 0;

  return g_strdup (buffer);
}

/*
 * GTK_TYPE_PARAM_UNIT
 */

#define GTK_PARAM_SPEC_UNIT(pspec) (G_TYPE_CHECK_INSTANCE_CAST ((pspec), GTK_TYPE_PARAM_UNIT, GtkParamSpecUnit))

typedef struct _GtkParamSpecUnit GtkParamSpecUnit;

struct _GtkParamSpecUnit
{
  GParamSpecInt parent_instance;

  gboolean      allow_percent;
};

static void      gtk_param_unit_class_init     (GParamSpecClass *class);
static gboolean  gtk_param_unit_value_validate (GParamSpec      *pspec,
                                                 GValue          *value);

/**
 * gtk_param_unit_get_type:
 *
 * Reveals the object type
 *
 * Returns: the #GType for a unit param object
 *
 * Since: GTK 2.4
 **/
GType
gtk_param_unit_get_type (void)
{
  static GType spec_type = 0;

  if (! spec_type)
    {
      const GTypeInfo type_info =
      {
        sizeof (GParamSpecClass),
        NULL, NULL,
        (GClassInitFunc) gtk_param_unit_class_init,
        NULL, NULL,
        sizeof (GtkParamSpecUnit),
        0, NULL, NULL
      };

      spec_type = g_type_register_static (G_TYPE_PARAM_INT,
                                          "GtkParamUnit",
                                          &type_info, 0);
    }

  return spec_type;
}

static void
gtk_param_unit_class_init (GParamSpecClass *class)
{
  class->value_type     = GTK_TYPE_UNIT;
  class->value_validate = gtk_param_unit_value_validate;
}

static gboolean
gtk_param_unit_value_validate (GParamSpec *pspec,
                                GValue     *value)
{
  GParamSpecInt     *ispec = G_PARAM_SPEC_INT (pspec);
  GtkParamSpecUnit *uspec = GTK_PARAM_SPEC_UNIT (pspec);
  gint               oval  = value->data[0].v_int;

  if (uspec->allow_percent && value->data[0].v_int == CM_UNIT_PERCENT)
    {
      value->data[0].v_int = value->data[0].v_int;
    }
  else
    {
      value->data[0].v_int = CLAMP (value->data[0].v_int,
                                    ispec->minimum,
                                    gtk_unit_get_number_of_units () - 1);
    }

  return value->data[0].v_int != oval;
}

/**
 * gtk_param_spec_unit:
 * @name:          Canonical name of the param
 * @nick:          Nickname of the param
 * @blurb:         Brief desciption of param.
 * @allow_pixels:  Whether "pixels" is an allowed unit.
 * @allow_percent: Whether "perecent" is an allowed unit.
 * @default_value: Unit to use if none is assigned.
 * @flags:         a combination of #GParamFlags
 *
 * Creates a param spec to hold a units param.
 * See g_param_spec_internal() for more information.
 *
 * Returns: a newly allocated #GParamSpec instance
 *
 * Since: GTK 2.4
 **/
GParamSpec *
gtk_param_spec_unit (const gchar *name,
                      const gchar *nick,
                      const gchar *blurb,
                      gboolean     allow_pixels,
                      gboolean     allow_percent,
                      GtkCMUnit     default_value,
                      GParamFlags  flags)
{
  GtkParamSpecUnit *pspec;
  GParamSpecInt     *ispec;

  pspec = g_param_spec_internal (GTK_TYPE_PARAM_UNIT,
                                 name, nick, blurb, flags);

  ispec = G_PARAM_SPEC_INT (pspec);

  ispec->default_value = default_value;
  ispec->minimum       = allow_pixels ? CM_UNIT_PIXEL : CM_UNIT_INCH;
  ispec->maximum       = CM_UNIT_PERCENT - 1;

  pspec->allow_percent = allow_percent;

  return G_PARAM_SPEC (pspec);
}

/**
 * gtk_pixels_to_units:
 * @pixels:     value in pixels
 * @unit:       unit to convert to
 * @resolution: resloution in DPI
 *
 * Converts a @value specified in pixels to @unit.
 *
 * Returns: @pixels converted to units.
 *
 * Since: GTK 2.8
 **/
gdouble
gtk_pixels_to_units (gdouble  pixels,
                      GtkCMUnit unit,
                      gdouble  resolution)
{
  if (unit == CM_UNIT_PIXEL)
    return pixels;

  return pixels * gtk_unit_get_factor (unit) / resolution;
}

/**
 * gtk_units_to_pixels:
 * @value:      value in units
 * @unit:       unit of @value
 * @resolution: resloution in DPI
 *
 * Converts a @value specified in @unit to pixels.
 *
 * Returns: @value converted to pixels.
 *
 * Since: GTK 2.8
 **/
gdouble
gtk_units_to_pixels (gdouble  value,
                      GtkCMUnit unit,
                      gdouble  resolution)
{
  if (unit == CM_UNIT_PIXEL)
    return value;

  return value * resolution / gtk_unit_get_factor (unit);
}

/**
 * gtk_units_to_points:
 * @value:      value in units
 * @unit:       unit of @value
 * @resolution: resloution in DPI
 *
 * Converts a @value specified in @unit to points.
 *
 * Returns: @value converted to points.
 *
 * Since: GTK 2.8
 **/
gdouble
gtk_units_to_points (gdouble  value,
                      GtkCMUnit unit,
                      gdouble  resolution)
{
  if (unit == CM_UNIT_POINT)
    return value;

  if (unit == CM_UNIT_PIXEL)
    return (value * gtk_unit_get_factor (CM_UNIT_POINT) / resolution);

  return (value *
          gtk_unit_get_factor (CM_UNIT_POINT) / gtk_unit_get_factor (unit));
}
