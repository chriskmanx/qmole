/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   pygtkcellrenderer.h: stub class to help implement cell renderers.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
 * USA
 */

#include <gtk/gtk.h>

#define PYGTK_TYPE_GENERIC_CELL_RENDERER            (pygtk_generic_cell_renderer_get_type())
#define PYGTK_GENERIC_CELL_RENDERER(object)         (G_TYPE_CHECK_INSTANCE_CAST((object), PYGTK_TYPE_GENERIC_CELL_RENDERER, PyGtkGenericCellRenderer))
#define PYGTK_GENERIC_CELL_RENDERER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), PYGTK_TYPE_GENERIC_CELL_RENDERER, PyGtkGenericCellRendererClass))
#define PYGTK_IS_GENERIC_CELL_RENDERER(object)      (G_TYPE_CHECK_INSTANCE_TYPE((object), PYGTK_TYPE_GENERIC_CELL_RENDERER))
#define PYGTK_IS_GENERIC_CELL_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), PYGTK_TYPE_GENERIC_CELL_RENDERER))
#define PYGTK_GENERIC_CELL_RENDERER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), PYGTK_TYPE_GENERIC_CELL_RENDERER, PyGtkGenericCellRendererClass))

typedef struct _PyGtkGenericCellRenderer PyGtkGenericCellRenderer;
typedef struct _PyGtkGenericCellRendererClass PyGtkGenericCellRendererClass;

struct _PyGtkGenericCellRenderer {
    GtkCellRenderer parent_instance;
};

struct _PyGtkGenericCellRendererClass {
    GtkCellRendererClass parent_class;
};

GType            pygtk_generic_cell_renderer_get_type (void);
GtkCellRenderer *pygtk_generic_cell_renderer_new      (void);
