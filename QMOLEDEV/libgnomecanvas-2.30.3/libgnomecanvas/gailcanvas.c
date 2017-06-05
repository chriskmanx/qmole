/* GAIL - The GNOME Accessibility Implementation Library
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

#include <gtk/gtk.h>
#include <libgnomecanvas/gnome-canvas.h>
#include <libgnomecanvas/gnome-canvas-text.h>
#include <libgnomecanvas/gnome-canvas-rich-text.h>
#include <libgnomecanvas/gnome-canvas-widget.h>
#include "gailcanvas.h"
#include "gailcanvasitem.h"
#include "gailcanvasgroupfactory.h"
#include "gailcanvastextfactory.h"
#include "gailcanvasitemfactory.h"
#include "gailcanvaswidgetfactory.h"

static void       gail_canvas_class_init          (GailCanvasClass *klass);
static void       gail_canvas_real_initialize     (AtkObject       *obj,
                                                   gpointer        data);

static gint       gail_canvas_get_n_children      (AtkObject       *obj);
static AtkObject* gail_canvas_ref_child           (AtkObject       *obj,
                                                   gint            i);

static void       adjustment_changed              (GtkAdjustment   *adjustment,
                                                   GnomeCanvas     *canvas);

static void 	  gail_canvas_factory_class_init (GailCanvasFactoryClass *klass);

static AtkObject* gail_canvas_factory_create_accessible (GObject *obj);

static GType      gail_canvas_factory_get_accessible_type (void);

G_DEFINE_TYPE (GailCanvasFactory,
               gail_canvas_factory,
               ATK_TYPE_OBJECT_FACTORY);

static void
gail_canvas_factory_init (GailCanvasFactory *foo)
{
  ;
}

static void
gail_canvas_factory_class_init (GailCanvasFactoryClass *klass)
{
  AtkObjectFactoryClass *class = ATK_OBJECT_FACTORY_CLASS (klass);

  class->create_accessible = gail_canvas_factory_create_accessible;
  class->get_accessible_type = gail_canvas_factory_get_accessible_type;
}

static AtkObject*
gail_canvas_factory_create_accessible (GObject   *obj)
{
  return gail_canvas_new (GTK_WIDGET (obj));
}

static GType
gail_canvas_factory_get_accessible_type (void)
{
  return GAIL_TYPE_CANVAS;
}

GType
gail_canvas_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      GType parent_type = g_type_parent (GNOME_TYPE_CANVAS);      
      AtkObjectFactory *factory = atk_registry_get_factory (
				   atk_get_default_registry (), 
				   parent_type);
      GType atkobject_parent_type = atk_object_factory_get_accessible_type (factory);
      GTypeQuery query;
      static GTypeInfo tinfo =
      {
        0, /* class size */
        (GBaseInitFunc) NULL, /* base init */
        (GBaseFinalizeFunc) NULL, /* base finalize */
        (GClassInitFunc) gail_canvas_class_init, /* class init */
        (GClassFinalizeFunc) NULL, /* class finalize */
        NULL, /* class data */
        0, /* instance size */
        0, /* nb preallocs */
        (GInstanceInitFunc) NULL, /* instance init */
        NULL /* value table */
      };
      g_type_query (atkobject_parent_type, &query);
      tinfo.class_size = query.class_size;
      tinfo.instance_size = query.instance_size;

      /* use the size obtained from the parent type factory */
      type = g_type_register_static (atkobject_parent_type,
                                     "GailCanvas", &tinfo, 0);
    }

  return type;
}

static AtkObjectClass *parent_atk_object_class;

/**
 * Tell ATK how to create the appropriate AtkObject peers 
 **/
void
gail_canvas_init (void)
{
  atk_registry_set_factory_type (atk_get_default_registry (),
				 GNOME_TYPE_CANVAS, 
				 gail_canvas_factory_get_type ());
  atk_registry_set_factory_type (atk_get_default_registry (),
				 GNOME_TYPE_CANVAS_GROUP, 
				 gail_canvas_group_factory_get_type ());
  atk_registry_set_factory_type (atk_get_default_registry (),
				 GNOME_TYPE_CANVAS_TEXT, 
				 gail_canvas_text_factory_get_type ());
  atk_registry_set_factory_type (atk_get_default_registry (),
				 GNOME_TYPE_CANVAS_RICH_TEXT, 
				 gail_canvas_text_factory_get_type ());
  atk_registry_set_factory_type (atk_get_default_registry (),
				 GNOME_TYPE_CANVAS_WIDGET,
				 gail_canvas_widget_factory_get_type());
  atk_registry_set_factory_type (atk_get_default_registry (),
				 GNOME_TYPE_CANVAS_ITEM, 
				 gail_canvas_item_factory_get_type ());
}

static void
gail_canvas_class_init (GailCanvasClass *klass)
{
  AtkObjectClass *class = ATK_OBJECT_CLASS (klass);
  parent_atk_object_class = g_type_class_peek_parent (klass);

  class->get_n_children = gail_canvas_get_n_children;
  class->ref_child = gail_canvas_ref_child;
  class->initialize = gail_canvas_real_initialize;
}

AtkObject* 
gail_canvas_new (GtkWidget *widget)
{
  GObject *object;
  AtkObject *accessible;

  g_return_val_if_fail (GNOME_IS_CANVAS (widget), NULL);

  object = g_object_new (GAIL_TYPE_CANVAS, NULL);

  accessible = ATK_OBJECT (object);
  atk_object_initialize (accessible, widget);

  return accessible;
}


static void
gail_canvas_real_initialize (AtkObject *obj,
                             gpointer  data)
{
  GnomeCanvas *canvas;
  GtkAdjustment *adj;

  parent_atk_object_class->initialize (obj, data);

  canvas = GNOME_CANVAS (data);

  adj = canvas->layout.hadjustment;
  g_signal_connect (adj,
                    "value_changed",
                    G_CALLBACK (adjustment_changed),
                    canvas);

  adj = canvas->layout.vadjustment;
  g_signal_connect (adj,
                    "value_changed",
                    G_CALLBACK (adjustment_changed),
                    canvas);

  obj->role =  ATK_ROLE_LAYERED_PANE;
}

static gint 
gail_canvas_get_n_children (AtkObject* obj)
{
  GtkAccessible *accessible;
  GtkWidget *widget;
  GnomeCanvas *canvas;
  GnomeCanvasGroup *root_group;

  g_return_val_if_fail (GAIL_IS_CANVAS (obj), 0);

  accessible = GTK_ACCESSIBLE (obj);
  widget = accessible->widget;
  if (widget == NULL)
    /* State is defunct */
    return 0;

  g_return_val_if_fail (GNOME_IS_CANVAS (widget), 0);

  canvas = GNOME_CANVAS (widget);
  root_group = gnome_canvas_root (canvas);
  g_return_val_if_fail (root_group, 0);
  return 1;
}

static AtkObject* 
gail_canvas_ref_child (AtkObject *obj,
		       gint       i)
{
  GtkAccessible *accessible;
  GtkWidget *widget;
  GnomeCanvas *canvas;
  GnomeCanvasGroup *root_group;
  AtkObject *atk_object;

  /* Canvas only has one child, so return NULL if anything else is requested */
  if (i != 0)
    return NULL;
  g_return_val_if_fail (GAIL_IS_CANVAS (obj), NULL);

  accessible = GTK_ACCESSIBLE (obj);
  widget = accessible->widget;
  if (widget == NULL)
    /* State is defunct */
    return NULL;
  g_return_val_if_fail (GNOME_IS_CANVAS (widget), NULL);

  canvas = GNOME_CANVAS (widget);
  root_group = gnome_canvas_root (canvas);
  g_return_val_if_fail (root_group, NULL);

  atk_object = atk_gobject_accessible_for_object (G_OBJECT (root_group));
  g_object_ref (atk_object);
  return atk_object;
}

static void
adjustment_changed (GtkAdjustment *adjustment,
                    GnomeCanvas   *canvas)
{
  AtkObject *atk_obj;

  /*
   * The scrollbars have changed
   */
  atk_obj = gtk_widget_get_accessible (GTK_WIDGET (canvas));

  g_signal_emit_by_name (atk_obj, "visible_data_changed");
}

