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
#include <libgnomecanvas/gnome-canvas-widget.h>
#include "gailcanvaswidget.h"

static void       gail_canvas_widget_class_init               (GailCanvasWidgetClass *klass);

static gint       gail_canvas_widget_get_n_children           (AtkObject            *obj);
static AtkObject* gail_canvas_widget_ref_child                (AtkObject            *obj,
                                                               gint                 i);

G_DEFINE_TYPE (GailCanvasWidget,
	       gail_canvas_widget,
	       GAIL_TYPE_CANVAS_ITEM);

static void
gail_canvas_widget_init (GailCanvasWidget *foo)
{
  ;
}

AtkObject*
gail_canvas_widget_new (GObject *obj)
{
  gpointer object;
  AtkObject *atk_object;

  g_return_val_if_fail (GNOME_IS_CANVAS_WIDGET (obj), NULL);
  object = g_object_new (GAIL_TYPE_CANVAS_WIDGET, NULL);
  atk_object = ATK_OBJECT (object);
  atk_object_initialize (atk_object, obj);
  atk_object->role = ATK_ROLE_PANEL;
  return atk_object;
}

static void
gail_canvas_widget_class_init (GailCanvasWidgetClass *klass)
{
  AtkObjectClass *class = ATK_OBJECT_CLASS (klass);

  class->get_n_children = gail_canvas_widget_get_n_children;
  class->ref_child = gail_canvas_widget_ref_child;
}

static gint 
gail_canvas_widget_get_n_children (AtkObject *obj)
{
  AtkGObjectAccessible *atk_gobj;
  GnomeCanvasWidget *canvas_widget;
  GObject *g_obj;

  g_return_val_if_fail (GAIL_IS_CANVAS_WIDGET (obj), 0);

  atk_gobj = ATK_GOBJECT_ACCESSIBLE (obj);
  g_obj = atk_gobject_accessible_get_object (atk_gobj);
  if (g_obj == NULL)
    /* State is defunct */
    return 0;

  g_return_val_if_fail (GNOME_IS_CANVAS_WIDGET (g_obj), 0);

  canvas_widget = GNOME_CANVAS_WIDGET (g_obj);
  g_return_val_if_fail (canvas_widget->widget, 0);
  return 1;
}

static AtkObject *
gail_canvas_widget_ref_child (AtkObject *obj,
                              gint      i)
{
  AtkGObjectAccessible *atk_gobj;
  GnomeCanvasWidget *canvas_widget;
  GObject *g_obj;
  AtkObject *atk_child;

  g_return_val_if_fail (GAIL_IS_CANVAS_WIDGET (obj), NULL);

  if (i != 0)
    return NULL;

  atk_gobj = ATK_GOBJECT_ACCESSIBLE (obj);
  g_obj = atk_gobject_accessible_get_object (atk_gobj);
  if (g_obj == NULL)
    /* State is defunct */
    return NULL;

  g_return_val_if_fail (GNOME_IS_CANVAS_WIDGET (g_obj), NULL);

  canvas_widget = GNOME_CANVAS_WIDGET (g_obj);
  g_return_val_if_fail (canvas_widget->widget, NULL);

  atk_child = gtk_widget_get_accessible (canvas_widget->widget);
  g_object_ref (atk_child);
  atk_object_set_parent (atk_child, obj);
  return atk_child;
}
