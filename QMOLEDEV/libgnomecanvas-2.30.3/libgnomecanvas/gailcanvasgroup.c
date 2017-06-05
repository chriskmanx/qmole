/* GAIL - The GNOME Accessibility Implementation Library
 * Copyright 2001 Sun Microsystems Inc.
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

#include <gtk/gtk.h>
#include <libgnomecanvas/gnome-canvas.h>
#include "gailcanvasgroup.h"

static void       gail_canvas_group_class_init      (GailCanvasGroupClass *klass);
static gint       gail_canvas_group_get_n_children  (AtkObject            *obj);
static AtkObject* gail_canvas_group_ref_child       (AtkObject            *obj,
                                                     gint                 i);

G_DEFINE_TYPE(GailCanvasGroup,
	      gail_canvas_group,
	      GAIL_TYPE_CANVAS_ITEM);

static void
gail_canvas_group_init (GailCanvasGroup *foo)
{
  ;
}

AtkObject*
gail_canvas_group_new (GObject *obj)
{
  gpointer object;
  AtkObject *atk_object;

  g_return_val_if_fail (GNOME_IS_CANVAS_GROUP (obj), NULL);
  object = g_object_new (GAIL_TYPE_CANVAS_GROUP, NULL);
  atk_object = ATK_OBJECT (object);
  atk_object_initialize (atk_object, obj);
  atk_object->role = ATK_ROLE_PANEL;
  return atk_object;
}

static void
gail_canvas_group_class_init (GailCanvasGroupClass *klass)
{
  AtkObjectClass *class = ATK_OBJECT_CLASS (klass);

  class->get_n_children = gail_canvas_group_get_n_children;
  class->ref_child = gail_canvas_group_ref_child;
}

static gint
gail_canvas_group_get_n_children (AtkObject *obj)
{
  AtkGObjectAccessible *atk_gobject;
  GnomeCanvasGroup *group;
  GObject *g_obj;

  g_return_val_if_fail (GAIL_IS_CANVAS_ITEM (obj), 0);
  atk_gobject = ATK_GOBJECT_ACCESSIBLE (obj);
  g_obj = atk_gobject_accessible_get_object (atk_gobject);
  g_return_val_if_fail (GNOME_IS_CANVAS_GROUP (g_obj), 0);
  group = GNOME_CANVAS_GROUP (g_obj);
  return g_list_length (group->item_list);
}


static AtkObject *
gail_canvas_group_ref_child (AtkObject *obj,
                             gint      i)
{
  AtkGObjectAccessible *atk_gobject;
  GnomeCanvasGroup *group;
  GnomeCanvasItem *item;
  AtkObject *accessible;
  GObject *g_obj;
  GList *list_item;

  g_return_val_if_fail (GAIL_IS_CANVAS_ITEM (obj), NULL);
  atk_gobject = ATK_GOBJECT_ACCESSIBLE (obj);
  g_obj = atk_gobject_accessible_get_object (atk_gobject);
  g_return_val_if_fail (GNOME_IS_CANVAS_GROUP (g_obj), NULL);
  group = GNOME_CANVAS_GROUP (g_obj);

  list_item = g_list_nth (group->item_list, i);
  if (!list_item)
	  return NULL;
  g_return_val_if_fail (list_item->data, NULL);
  item = GNOME_CANVAS_ITEM (list_item->data);
  accessible = atk_gobject_accessible_for_object (G_OBJECT (item));
  g_object_ref (accessible);
  return accessible;
}
