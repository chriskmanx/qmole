/* ATK -  Accessibility Toolkit
 * Copyright (C) 2009 Novell, Inc.
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

#include "atk.h"
#include "atksocket.h"

static void atk_socket_class_init (AtkSocketClass *klass);

static void atk_component_interface_init (AtkComponentIface *iface);

G_DEFINE_TYPE_WITH_CODE (AtkSocket, atk_socket, ATK_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (ATK_TYPE_COMPONENT, atk_component_interface_init))

static void
atk_socket_init (AtkSocket* obj)
{
  obj->embedded_plug_id = NULL;
}

static void
atk_socket_class_init (AtkSocketClass* klass)
{
  klass->embed = NULL;
}

static void atk_component_interface_init (AtkComponentIface *iface)
{
}

AtkObject*
atk_socket_new (void)
{
  AtkObject* accessible;
  
  accessible = g_object_new (ATK_TYPE_SOCKET, NULL);
  g_return_val_if_fail (accessible != NULL, NULL);

  accessible->role = ATK_ROLE_FILLER;
  accessible->layer = ATK_LAYER_WIDGET;
  
  return accessible;
}

/**
 * atk_socket_embed:
 * @obj: an #AtkSocket
 * @plug_id: the ID of an #AtkPlug
 *
 * Embeds the children of an #AtkPlug as the children of the #AtkSocket.  The
 * plug may be in the same process or in a different process.
 * THe class item used by this function should be filled in by the IPC layer
 * (ie, at-spi2-atk).  The implementor of the AtkSocket should call this
 * function and pass the id for the plug as returned by atk_plug_get_id.
 * It is the responsibility of the application to pass the plug id on to
 * the process implementing the AtkSocket as needed.
 *
 * Since: 1.30
 **/
void
atk_socket_embed (AtkSocket* obj, gchar* plug_id)
{
  AtkSocketClass *klass;

  g_return_if_fail (plug_id != NULL);
  g_return_if_fail (ATK_IS_SOCKET (obj));

  klass = g_type_class_peek (ATK_TYPE_SOCKET);
  if (klass && klass->embed)
    {
      if (obj->embedded_plug_id)
        g_free (obj->embedded_plug_id);
      obj->embedded_plug_id = g_strdup (plug_id);
      (klass->embed) (obj, plug_id);
    }
}

/**
 * atk_socket_is_occupied:
 * @obj: an #AtkSocket
 *
 * Determines whether or not the socket has an embedded plug.
 *
 * Returns: TRUE if a plug is embedded in the socket
 *
 * Since: 1.30
 **/
gboolean
atk_socket_is_occupied (AtkSocket* obj)
{
  g_return_val_if_fail (ATK_IS_SOCKET (obj), FALSE);

  return (obj->embedded_plug_id != NULL);
}
