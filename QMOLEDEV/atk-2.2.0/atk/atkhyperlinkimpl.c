/* ATK -  Accessibility Toolkit
 * Copyright 2006 Sun Microsystems Inc.
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
#include "atkhyperlinkimpl.h"

GType
atk_hyperlink_impl_get_type (void)
{
  static GType type = 0;

  if (!type) {
    GTypeInfo tinfo =
    {
      sizeof (AtkHyperlinkImplIface),
      (GBaseInitFunc) NULL,
      (GBaseFinalizeFunc) NULL,

    };

    type = g_type_register_static (G_TYPE_INTERFACE, "AtkHyperlinkImpl", &tinfo, 0);
  }

  return type;
}

/**
 * atk_hyperlink_impl_get_hyperlink:
 * @obj: a GObject instance that implements AtkHyperlinkImplIface
 *
 * Gets the hyperlink associated with this object.
 *
 * Returns: (transfer full):  an AtkHyperlink object which points to this
 * implementing AtkObject.
 *
 * Since: 1.12
 **/
AtkHyperlink *
atk_hyperlink_impl_get_hyperlink (AtkHyperlinkImpl *obj)
{
  AtkHyperlinkImplIface *iface;

  g_return_val_if_fail (obj != NULL, NULL);
  g_return_val_if_fail (ATK_IS_HYPERLINK_IMPL (obj), NULL);

  iface = ATK_HYPERLINK_IMPL_GET_IFACE (obj);

  if (iface->get_hyperlink)
    {
      return (iface->get_hyperlink) (obj);
    }
  return NULL;
}

