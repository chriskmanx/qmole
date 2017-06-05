/* ATK -  Accessibility Toolkit
 * Copyright 2001, 2002, 2003 Sun Microsystems Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <string.h>
#include "atkvalue.h"

GType
atk_value_get_type (void)
{
  static GType type = 0;

  if (!type) {
    GTypeInfo tinfo =
    {
      sizeof (AtkValueIface),
      (GBaseInitFunc) NULL,
      (GBaseFinalizeFunc) NULL,

    };

    type = g_type_register_static (G_TYPE_INTERFACE, "AtkValue", &tinfo, 0);
  }

  return type;
}

/**
 * atk_value_get_current_value:
 * @obj: a GObject instance that implements AtkValueIface
 * @value: a #GValue representing the current accessible value
 *
 * Gets the value of this object.
 **/
void
atk_value_get_current_value (AtkValue *obj,
                             GValue   *value)
{
  AtkValueIface *iface;

  g_return_if_fail (value != NULL);
  g_return_if_fail (ATK_IS_VALUE (obj));

  iface = ATK_VALUE_GET_IFACE (obj);

  if (iface->get_current_value)
    {
      if (G_IS_VALUE (value))
        g_value_unset (value);
      else
        memset (value, 0, sizeof (*value));

      (iface->get_current_value) (obj, value);
    }
}

/**
 * atk_value_get_maximum_value:
 * @obj: a GObject instance that implements AtkValueIface
 * @value: a #GValue representing the maximum accessible value
 *
 * Gets the maximum value of this object.
 **/
void
atk_value_get_maximum_value  (AtkValue *obj,
                              GValue   *value)
{
  AtkValueIface *iface;

  g_return_if_fail (value != NULL);
  g_return_if_fail (ATK_IS_VALUE (obj));

  iface = ATK_VALUE_GET_IFACE (obj);

  if (iface->get_maximum_value)
    {
      if (G_IS_VALUE (value))
        g_value_unset (value);
      else
        memset (value, 0, sizeof (*value));

      (iface->get_maximum_value) (obj, value);
    }
}

/**
 * atk_value_get_minimum_value:
 * @obj: a GObject instance that implements AtkValueIface
 * @value: a #GValue representing the minimum accessible value
 *
 * Gets the minimum value of this object.
 **/
void
atk_value_get_minimum_value (AtkValue *obj,
                             GValue   *value)
{
  AtkValueIface *iface;

  g_return_if_fail (value != NULL);
  g_return_if_fail (ATK_IS_VALUE (obj));

  iface = ATK_VALUE_GET_IFACE (obj);

  if (iface->get_minimum_value)
    {
      if (G_IS_VALUE (value))
        g_value_unset (value);
      else
        memset (value, 0, sizeof (*value));

      (iface->get_minimum_value) (obj, value);
    }
}

/**
 * atk_value_get_minimum_increment:
 * @obj: a GObject instance that implements AtkValueIface
 * @value: a #GValue representing the minimum increment by which the accessible value may be changed
 *
 * Gets the minimum increment by which the value of this object may be changed.  If zero,
 * the minimum increment is undefined, which may mean that it is limited only by the 
 * floating point precision of the platform.
 *
 * Since: 1.12
 **/
void
atk_value_get_minimum_increment (AtkValue *obj,
                             GValue   *value)
{
  AtkValueIface *iface;

  g_return_if_fail (value != NULL);
  g_return_if_fail (ATK_IS_VALUE (obj));

  iface = ATK_VALUE_GET_IFACE (obj);

  if (iface->get_minimum_increment)
    {
      if (G_IS_VALUE (value))
        g_value_unset (value);
      else
        memset (value, 0, sizeof (*value));

      (iface->get_minimum_increment) (obj, value);
    }
}

/**
 * atk_value_set_current_value:
 * @obj: a GObject instance that implements AtkValueIface
 * @value: a #GValue which is the desired new accessible value.
 *
 * Sets the value of this object.
 *
 * Returns: %TRUE if new value is successfully set, %FALSE otherwise.
 **/
gboolean
atk_value_set_current_value (AtkValue       *obj, 
                             const GValue   *value)
{
  AtkValueIface *iface;

  g_return_val_if_fail (ATK_IS_VALUE (obj), FALSE);
  g_return_val_if_fail (G_IS_VALUE (value), FALSE);

  iface = ATK_VALUE_GET_IFACE (obj);

  if (iface->set_current_value)
    return (iface->set_current_value) (obj, value);
  else
    return FALSE;
}
