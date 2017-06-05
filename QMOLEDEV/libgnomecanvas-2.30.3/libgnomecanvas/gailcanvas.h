/* gailcanvas.h - code from GAIL, the 
 * Gnome Accessibility Implementation Library
 * Copyright 2001-2006 Sun Microsystems Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __GAIL_CANVAS_H__
#define __GAIL_CANVAS_H__

#include <gtk/gtk.h>

/* This code provides the ATK implementation for gnome-canvas widgets. */

G_BEGIN_DECLS

#define GAIL_TYPE_CANVAS                  (gail_canvas_get_type ())
#define GAIL_CANVAS(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAIL_TYPE_CANVAS, GailCanvas))
#define GAIL_CANVAS_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), GAIL_TYPE_CANVAS, GailCanvasClass))
#define GAIL_IS_CANVAS(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAIL_TYPE_CANVAS))
#define GAIL_IS_CANVAS_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), GAIL_TYPE_CANVAS))
#define GAIL_CANVAS_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), GAIL_TYPE_CANVAS, GailCanvasClass))

#define GAIL_TYPE_CANVAS_FACTORY            (gail_canvas_factory_get_type ())
#define GAIL_CANVAS_FACTORY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAIL_TYPE_CANVAS_FACTORY, GailCanvasFactory))
#define GAIL_CANVAS_FACTORY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAIL_TYPE_CANVAS_FACTORY, GailCanvasFactoryClass))
#define GAIL_IS_CANVAS_FACTORY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAIL_TYPE_CANVAS_FACTORY))
#define GAIL_IS_CANVAS_FACTORY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAIL_TYPE_CANVAS_FACTORY))
#define GAIL_CANVAS_FACTORY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAIL_TYPE_CANVAS_FACTORY, GailCanvasFactoryClass))

typedef struct _GailCanvas                 GailCanvas;
typedef struct _GailCanvasClass            GailCanvasClass;
typedef struct _GailCanvasFactory          GailCanvasFactory;
typedef struct _GailCanvasFactoryClass     GailCanvasFactoryClass;

struct _GailCanvasFactory
{
  AtkObjectFactory parent;
};

struct _GailCanvasFactoryClass
{
  AtkObjectFactoryClass parent_class;
};


GType gail_canvas_get_type (void);

struct _GailCanvas;

struct _GailCanvasClass;

AtkObject* gail_canvas_new (GtkWidget *widget);

void gail_canvas_init (void);

GType gail_canvas_factory_get_type(void);

G_END_DECLS

#endif /* __GAIL_CANVAS_H__ */
