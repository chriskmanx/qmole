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

#include <math.h>
#include <gtk/gtk.h>
#include <libgnomecanvas/gnome-canvas.h>
#include <libgnomecanvas/gnome-canvas-util.h>
#include "gailcanvasitem.h"
#include <libgail-util/gailmisc.h>

static void       gail_canvas_item_class_init               (GailCanvasItemClass *klass);
static void       gail_canvas_item_initialize               (AtkObject         *obj,
		                                             gpointer          data);
static AtkObject* gail_canvas_item_get_parent               (AtkObject         *obj);
static gint       gail_canvas_item_get_index_in_parent      (AtkObject         *obj);
static AtkStateSet* gail_canvas_item_ref_state_set          (AtkObject         *obj);

static void       gail_canvas_item_component_interface_init (AtkComponentIface *iface);
static guint      gail_canvas_item_add_focus_handler        (AtkComponent      *component,
					                     AtkFocusHandler   handler);
static void       gail_canvas_item_get_extents              (AtkComponent      *component,
					                     gint              *x,
					                     gint              *y,
					                     gint              *width,
					                     gint              *height,
					                     AtkCoordType      coord_type);
static gint       gail_canvas_item_get_mdi_zorder           (AtkComponent      *component);
static gboolean   gail_canvas_item_grab_focus               (AtkComponent      *component);
static void       gail_canvas_item_remove_focus_handler     (AtkComponent      *component,
					                     guint             handler_id);
static gboolean   is_item_on_screen                         (GnomeCanvasItem   *item);
static void       get_item_extents                          (GnomeCanvasItem   *item,
					                     gint              *x,
					                     gint              *y,
					                     gint              *width,
					                     gint              *height);
static gboolean   is_item_in_window                         (GnomeCanvasItem   *item,
					                     gint              x,
					                     gint              y,
					                     gint              width,
					                     gint              height);

static AtkGObjectAccessibleClass *parent_class = NULL;

G_DEFINE_TYPE_WITH_CODE (GailCanvasItem,
			 gail_canvas_item,
			 ATK_TYPE_GOBJECT_ACCESSIBLE,
			 G_IMPLEMENT_INTERFACE (ATK_TYPE_COMPONENT,
						gail_canvas_item_component_interface_init));

static void
gail_canvas_item_init (GailCanvasItem *foo)
{
  ;
}

AtkObject*
gail_canvas_item_new (GObject *obj)
{
  gpointer object;
  AtkObject *atk_object;

  g_return_val_if_fail (GNOME_IS_CANVAS_ITEM (obj), NULL);
  object = g_object_new (GAIL_TYPE_CANVAS_ITEM, NULL);
  atk_object = ATK_OBJECT (object);
  atk_object_initialize (atk_object, obj);
  atk_object->role = ATK_ROLE_UNKNOWN;
  return atk_object;
}

static void
gail_canvas_item_initialize (AtkObject   *obj,
		             gpointer    data)
{
  ATK_OBJECT_CLASS (parent_class)->initialize (obj, data);

  g_object_set_data (G_OBJECT (obj), "atk-component-layer",
                     GINT_TO_POINTER (ATK_LAYER_MDI));
}

static void
gail_canvas_item_class_init (GailCanvasItemClass *klass)
{
  AtkObjectClass *class = ATK_OBJECT_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);

  class->get_parent = gail_canvas_item_get_parent;
  class->get_index_in_parent = gail_canvas_item_get_index_in_parent;
  class->ref_state_set = gail_canvas_item_ref_state_set;
  class->initialize = gail_canvas_item_initialize;
}

static AtkObject *
gail_canvas_item_get_parent (AtkObject *obj)
{
  AtkGObjectAccessible *atk_gobj;
  GObject *g_obj;
  GnomeCanvasItem *item;

  g_return_val_if_fail (GAIL_IS_CANVAS_ITEM (obj), NULL);
  if (obj->accessible_parent)
    return obj->accessible_parent;
  atk_gobj = ATK_GOBJECT_ACCESSIBLE (obj);
  g_obj = atk_gobject_accessible_get_object (atk_gobj);
  if (g_obj == NULL)
    /* Object is defunct */
    return NULL;

  item = GNOME_CANVAS_ITEM (g_obj);
  if (item->parent)
    return atk_gobject_accessible_for_object (G_OBJECT (item->parent));
  else
    return gtk_widget_get_accessible (GTK_WIDGET (item->canvas));
}

static gint
gail_canvas_item_get_index_in_parent (AtkObject *obj)
{
  AtkGObjectAccessible *atk_gobj;
  GObject *g_obj;
  GnomeCanvasItem *item;

  g_return_val_if_fail (GAIL_IS_CANVAS_ITEM (obj), -1);
  if (obj->accessible_parent)
    {
      gint n_children, i;
      gboolean found = FALSE;

      n_children = atk_object_get_n_accessible_children (obj->accessible_parent);
      for (i = 0; i < n_children; i++)
        {
          AtkObject *child;

          child = atk_object_ref_accessible_child (obj->accessible_parent, i);
          if (child == obj)
            found = TRUE;

          g_object_unref (child);
          if (found)
            return i;
        }
      return -1;
    }

  atk_gobj = ATK_GOBJECT_ACCESSIBLE (obj);
  g_obj = atk_gobject_accessible_get_object (atk_gobj);
  if (g_obj == NULL)
    /* Object is defunct */
    return -1;

  item = GNOME_CANVAS_ITEM (g_obj);
  if (item->parent)
    {
      return g_list_index (GNOME_CANVAS_GROUP (item->parent)->item_list, item);
    }
  else
    {
      g_return_val_if_fail (item->canvas->root == item, -1);
      return 0;
    }
}

static AtkStateSet*
gail_canvas_item_ref_state_set (AtkObject         *obj)
{
  AtkGObjectAccessible *atk_gobj;
  GObject *g_obj;
  GnomeCanvasItem *item;
  AtkStateSet *state_set;

  g_return_val_if_fail (GAIL_IS_CANVAS_ITEM (obj), NULL);
  atk_gobj = ATK_GOBJECT_ACCESSIBLE (obj);

  state_set = ATK_OBJECT_CLASS (parent_class)->ref_state_set (obj);

  g_obj = atk_gobject_accessible_get_object (atk_gobj);
  if (g_obj == NULL)
    {
    /* Object is defunct */
      atk_state_set_add_state (state_set, ATK_STATE_DEFUNCT);
    }
  else
    {
      item = GNOME_CANVAS_ITEM (g_obj);

      if (item->object.flags & GNOME_CANVAS_ITEM_VISIBLE)
        {
          atk_state_set_add_state (state_set, ATK_STATE_VISIBLE);
          if (is_item_on_screen (item))
            {
              atk_state_set_add_state (state_set, ATK_STATE_SHOWING);
            }
        }
      if (GTK_WIDGET_CAN_FOCUS (GTK_WIDGET (item->canvas)))
        {
          atk_state_set_add_state (state_set, ATK_STATE_FOCUSABLE);

          if (item->canvas->focused_item == item)
            {
              atk_state_set_add_state (state_set, ATK_STATE_FOCUSED);
            }
        }
    }

  return state_set;
}

static void
gail_canvas_item_component_interface_init (AtkComponentIface *iface)
{
  g_return_if_fail (iface != NULL);

  iface->add_focus_handler = gail_canvas_item_add_focus_handler;
  iface->get_extents = gail_canvas_item_get_extents;
  iface->get_mdi_zorder = gail_canvas_item_get_mdi_zorder;
  iface->grab_focus = gail_canvas_item_grab_focus;
  iface->remove_focus_handler = gail_canvas_item_remove_focus_handler;
}

static guint
gail_canvas_item_add_focus_handler (AtkComponent    *component,
                                    AtkFocusHandler handler)
{
  GSignalMatchType match_type;
  gulong ret;
  guint signal_id;

  match_type = G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_FUNC;
  signal_id = g_signal_lookup ("focus-event", ATK_TYPE_OBJECT);

  ret = g_signal_handler_find (component, match_type, signal_id, 0, NULL,
                               (gpointer) handler, NULL);
  if (!ret)
    {
      return g_signal_connect_closure_by_id (component,
                                             signal_id, 0,
                                             g_cclosure_new (
                                             G_CALLBACK (handler), NULL,
                                             (GClosureNotify) NULL),
                                             FALSE);
    }
  else
    {
      return 0;
    }
}

static void
gail_canvas_item_get_extents (AtkComponent *component,
                              gint         *x,
                              gint         *y,
                              gint         *width,
                              gint         *height,
                              AtkCoordType coord_type)
{
  AtkGObjectAccessible *atk_gobj;
  GObject *obj;
  GnomeCanvasItem *item;
  gint window_x, window_y;
  gint toplevel_x, toplevel_y;
  gint local_x, local_y;

  g_return_if_fail (GAIL_IS_CANVAS_ITEM (component));
  atk_gobj = ATK_GOBJECT_ACCESSIBLE (component);
  obj = atk_gobject_accessible_get_object (atk_gobj);

  if (obj == NULL) 
    /* item is defunct */
    return;

  /* Get the GnomeCanvasItem */
  item = GNOME_CANVAS_ITEM (obj);

  /* If this item has no parent canvas, something's broken */
  g_return_if_fail (GTK_IS_WIDGET (item->canvas));

  get_item_extents (item, &local_x, &local_y, width, height);  
  if (!is_item_in_window (item, local_x, local_y, *width, *height))
    {
      *x = G_MININT;
      *y = G_MININT;
      return;
    }

  gail_misc_get_origins (GTK_WIDGET (item->canvas), &window_x, &window_y,
                         &toplevel_x, &toplevel_y);
  *x = local_x + window_x - toplevel_x;
  *y = local_y + window_y - toplevel_y;

  /* If screen coordinates are requested, modify x and y appropriately */
  if (coord_type == ATK_XY_SCREEN)
    {
      *x += toplevel_x;
      *y += toplevel_y;
    }
  return;
}

static gint
gail_canvas_item_get_mdi_zorder (AtkComponent *component)
{
  g_return_val_if_fail (ATK_OBJECT (component), -1); 

  return gail_canvas_item_get_index_in_parent (ATK_OBJECT (component));
}

static gboolean
gail_canvas_item_grab_focus (AtkComponent    *component)
{
  AtkGObjectAccessible *atk_gobj;
  GObject *obj;
  GnomeCanvasItem *item;
  GtkWidget *toplevel;

  g_return_val_if_fail (GAIL_IS_CANVAS_ITEM (component), FALSE);
  atk_gobj = ATK_GOBJECT_ACCESSIBLE (component);
  obj = atk_gobject_accessible_get_object (atk_gobj);

  /* Get the GnomeCanvasItem */
  item = GNOME_CANVAS_ITEM (obj);
  if (item == NULL)
    /* item is defunct */
    return FALSE;

  gnome_canvas_item_grab_focus (item);
  toplevel = gtk_widget_get_toplevel (GTK_WIDGET (item->canvas));
  if (GTK_WIDGET_TOPLEVEL (toplevel))
    gtk_window_present (GTK_WINDOW (toplevel));

  return TRUE;
}

static void
gail_canvas_item_remove_focus_handler (AtkComponent *component,
                                       guint        handler_id)
{
  g_signal_handler_disconnect (ATK_OBJECT (component), handler_id);
}

static gboolean 
is_item_on_screen (GnomeCanvasItem *item)
{
  gint x, y, width, height;
 
  get_item_extents (item, &x, &y, &width, &height);
  return is_item_in_window (item, x, y, width, height);
}

static void
get_item_extents (GnomeCanvasItem   *item,
                  gint              *x,
                  gint              *y,
                  gint              *width,
                  gint              *height)
{
  double bx1, by1, bx2, by2;
  double i2c[6];
  ArtPoint p1, p2, p3, p4;
  ArtPoint q1, q2, q3, q4;
  double min_x1, min_y1, min_x2, min_y2;
  double max_x1, max_y1, max_x2, max_y2;
  int x1, y1, x2, y2;
  int scroll_x, scroll_y;

  /* Get the bounding box in item-relative coordinates */

  bx1 = by1 = bx2 = by2 = 0.0;

  if (GNOME_CANVAS_ITEM_CLASS (G_OBJECT_GET_CLASS (item))->bounds)
    (* GNOME_CANVAS_ITEM_CLASS (G_OBJECT_GET_CLASS (item))->bounds) (item, &bx1, &by1, &bx2, &by2);

  /* Get the item coordinates -> canvas pixel coordinates affine */

  gnome_canvas_item_i2c_affine (item, i2c);

  /* Convert the bounding box to canvas pixel coordinates and find its minimum
   * surrounding rectangle.
   */
  
  p1.x = p2.x = bx1;
  p1.y = p4.y = by1;
  p3.x = p4.x = bx2;
  p2.y = p3.y = by2;

  art_affine_point (&q1, &p1, i2c);
  art_affine_point (&q2, &p2, i2c);
  art_affine_point (&q3, &p3, i2c);
  art_affine_point (&q4, &p4, i2c);

  if (q1.x < q2.x)
    {
      min_x1 = q1.x;
      max_x1 = q2.x;
    }
  else
    {
      min_x1 = q2.x;
      max_x1 = q1.x;
    }

  if (q1.y < q2.y)
    {
      min_y1 = q1.y;
      max_y1 = q2.y;
    }
  else
    {
      min_y1 = q2.y;
      max_y1 = q1.y;
    }

  if (q3.x < q4.x)
    {
      min_x2 = q3.x;
      max_x2 = q4.x;
    }
  else
    {
      min_x2 = q4.x;
      max_x2 = q3.x;
    }

  if (q3.y < q4.y)
    {
      min_y2 = q3.y;
      max_y2 = q4.y;
    }
  else
    {
      min_y2 = q4.y;
      max_y2 = q3.y;
    }

  bx1 = MIN (min_x1, min_x2);
  by1 = MIN (min_y1, min_y2);
  bx2 = MAX (max_x1, max_x2);
  by2 = MAX (max_y1, max_y2);

  /* Convert to integer coordinates */

  x1 = floor (bx1);
  y1 = floor (by1);
  x2 = ceil (bx2);
  y2 = ceil (by2);

  gnome_canvas_get_scroll_offsets (item->canvas, &scroll_x, &scroll_y);

  if (x)
    *x = x1 - scroll_x;

  if (y)
    *y = y1 - scroll_y;

  if (width)
    *width = x2 - x1;

  if (height)
    *height = y2 - y1;
}

static gboolean
is_item_in_window (GnomeCanvasItem   *item,
                   gint              x,
                   gint              y,
                   gint              width,
                   gint              height)
{
  GtkWidget *widget;
  gboolean retval;

  widget = GTK_WIDGET (item->canvas);
  if (widget->window)
    {
      int window_width, window_height;
 
      gdk_window_get_geometry (widget->window, NULL, NULL, 
                               &window_width, &window_height, NULL);
      /*
       * Check whether rectangles intersect
       */
      if (x + width <= 0  ||
          y + height <= 0 ||
          x > window_width  ||
          y > window_height)
        {
          retval = FALSE;
        }
      else
        {
          retval = TRUE;
        }
    }
  else
    {
      retval = FALSE;
    }
  return retval;
}
