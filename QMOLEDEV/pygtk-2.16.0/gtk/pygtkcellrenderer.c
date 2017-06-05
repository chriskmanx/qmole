/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   pygtkcellrenderer.c: stub class to help implement cell renderers.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "pygtkcellrenderer.h"
#include <Python.h>
#include "pygtk-private.h"

/* define this to print out debug messages */
#undef DEBUG_CELL_RENDERER

#ifndef _
#  define _(s) (s)
#endif

static void            pygtk_generic_cell_renderer_class_init    (PyGtkGenericCellRendererClass *klass);
static void            pygtk_generic_cell_renderer_get_size      (GtkCellRenderer               *cell,
								  GtkWidget                     *widget,
								  GdkRectangle                  *cell_area,
								  gint                          *x_offset,
								  gint                          *y_offset,
								  gint                          *width,
								  gint                          *height);
static void            pygtk_generic_cell_renderer_render        (GtkCellRenderer               *cell,
								  GdkWindow                     *window,
								  GtkWidget                     *widget,
								  GdkRectangle                  *background_area,
								  GdkRectangle                  *cell_area,
								  GdkRectangle                  *expose_area,
								  GtkCellRendererState           flags);
static gboolean        pygtk_generic_cell_renderer_activate      (GtkCellRenderer               *cell,
								  GdkEvent                      *event,
								  GtkWidget                     *widget,
								  const gchar                   *path,
								  GdkRectangle                  *background_area,
								  GdkRectangle                  *cell_area,
								  GtkCellRendererState           flags);
static GtkCellEditable *pygtk_generic_cell_renderer_start_editing (GtkCellRenderer               *cell,
								  GdkEvent                      *event,
								  GtkWidget                     *widget,
								  const gchar                   *path,
								  GdkRectangle                  *background_area,
								  GdkRectangle                  *cell_area,
								  GtkCellRendererState           flags);



GType
pygtk_generic_cell_renderer_get_type(void)
{
    static GType object_type = 0;

    if (!object_type) {
	static const GTypeInfo object_info = {
	    sizeof(PyGtkGenericCellRendererClass),
	    (GBaseInitFunc) NULL,
	    (GBaseFinalizeFunc) NULL,
	    (GClassInitFunc) pygtk_generic_cell_renderer_class_init,
	    NULL, /* class_finalize */
	    NULL, /* class_data */
	    sizeof(PyGtkGenericCellRenderer),
	    0, /* n_preallocs */
	    (GInstanceInitFunc) NULL,
	};
	object_type = g_type_register_static(GTK_TYPE_CELL_RENDERER,
					     "PyGtkGenericCellRenderer",
					     &object_info, 0);
    }

    return object_type;
}

static void
pygtk_generic_cell_renderer_class_init(PyGtkGenericCellRendererClass *klass)
{
    GtkCellRendererClass *cell_renderer_class = (GtkCellRendererClass*) klass;
    cell_renderer_class->get_size = pygtk_generic_cell_renderer_get_size;
    cell_renderer_class->render = pygtk_generic_cell_renderer_render;
    cell_renderer_class->activate = pygtk_generic_cell_renderer_activate;
    cell_renderer_class->start_editing = pygtk_generic_cell_renderer_start_editing;
}

#define METHOD_PREFIX "on_"

static void
pygtk_generic_cell_renderer_get_size (GtkCellRenderer *cell,
				      GtkWidget       *widget,
				      GdkRectangle    *cell_area,
				      gint            *x_offset,
				      gint            *y_offset,
				      gint            *width,
				      gint            *height)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_widget, *py_cell_area;
    gint my_x, my_y, my_width, my_height;

    g_return_if_fail(PYGTK_IS_GENERIC_CELL_RENDERER (cell));

    state = pyg_gil_state_ensure();

    self = pygobject_new((GObject *)cell);

#ifdef DEBUG_CELL_RENDERER
    g_message ("get_size()");
#endif
    py_widget = pygobject_new((GObject *)widget);
    py_cell_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, cell_area, TRUE, TRUE);

    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "get_size", "OO",
				 py_widget, py_cell_area);
    if (!py_ret) {
	PyErr_Print();
	Py_DECREF(py_widget);
	Py_DECREF(py_cell_area);
	pyg_gil_state_release(state);
	return;
    }
    Py_DECREF(py_widget);
    Py_DECREF(py_cell_area);

    if (!PyArg_ParseTuple(py_ret, "iiii",
			  &my_x, &my_y, &my_width, &my_height)) {
	PyErr_Clear();
	Py_DECREF(py_ret);
	pyg_gil_state_release(state);
	g_warning("could not parse return value of get_size() method.  "
		  "Should be of form (x_offset, y_offset, width, height)");
	return;
    }

    pyg_gil_state_release(state);

    /* success */
    if (x_offset)
	*x_offset = my_x;

    if (y_offset)
	*y_offset = my_y;

    if (width)
	*width = my_width;

    if (height)
	*height = my_height;

}

static void
pygtk_generic_cell_renderer_render (GtkCellRenderer      *cell,
				    GdkWindow            *window,
				    GtkWidget            *widget,
				    GdkRectangle         *background_area,
				    GdkRectangle         *cell_area,
				    GdkRectangle         *expose_area,
				    GtkCellRendererState  flags)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_window, *py_widget;
    PyObject *py_background_area, *py_cell_area, *py_expose_area;

    g_return_if_fail(PYGTK_IS_GENERIC_CELL_RENDERER (cell));

    state = pyg_gil_state_ensure();

    self = pygobject_new((GObject *)cell);

#ifdef DEBUG_CELL_RENDERER
    g_message ("render()");
#endif
    py_window = pygobject_new((GObject *)window);
    py_widget = pygobject_new((GObject *)widget);
    py_background_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, background_area, TRUE, TRUE);
    py_cell_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, cell_area, TRUE, TRUE);
    py_expose_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, expose_area, TRUE, TRUE);
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "render", "OOOOOi",
				 py_window, py_widget, py_background_area,
				 py_cell_area, py_expose_area, flags);
    if (!py_ret) {
	PyErr_Print();
    }

    Py_DECREF(py_window);
    Py_DECREF(py_widget);
    Py_DECREF(py_background_area);
    Py_DECREF(py_cell_area);
    Py_DECREF(py_expose_area);

    pyg_gil_state_release(state);
}

static gboolean
pygtk_generic_cell_renderer_activate (GtkCellRenderer      *cell,
				      GdkEvent             *event,
				      GtkWidget            *widget,
				      const gchar          *path,
				      GdkRectangle         *background_area,
				      GdkRectangle         *cell_area,
				      GtkCellRendererState  flags)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_event, *py_widget;
    PyObject *py_background_area, *py_cell_area;
    gboolean ret = FALSE;

    g_return_val_if_fail(PYGTK_IS_GENERIC_CELL_RENDERER (cell), FALSE);

    state = pyg_gil_state_ensure();

    self = pygobject_new((GObject *)cell);

#ifdef DEBUG_CELL_RENDERER
    g_message ("activate()");
#endif
    py_event = pyg_boxed_new(GDK_TYPE_EVENT, event, FALSE, FALSE);
    py_widget = pygobject_new((GObject *)widget);
    py_background_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, background_area, TRUE, TRUE);
    py_cell_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, cell_area, TRUE, TRUE);

    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "activate", "OOzOOi",
				 py_event, py_widget, path, py_background_area,
				 py_cell_area, flags);
    if (!py_ret) {
	PyErr_Print();
	goto out;
    }
    
    ret = PyObject_IsTrue(py_ret);
    Py_DECREF(py_ret);

out:
    pygtk_boxed_unref_shared(py_event);
    Py_DECREF(py_widget);
    Py_DECREF(py_background_area);
    Py_DECREF(py_cell_area);
    pyg_gil_state_release(state);
    return ret;
}

static GtkCellEditable *
pygtk_generic_cell_renderer_start_editing (GtkCellRenderer      *cell,
					   GdkEvent             *event,
					   GtkWidget            *widget,
					   const gchar          *path,
					   GdkRectangle         *background_area,
					   GdkRectangle         *cell_area,
					   GtkCellRendererState  flags)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_event, *py_widget;
    PyObject *py_background_area, *py_cell_area;
    GtkCellEditable *ret = NULL;
    extern PyTypeObject PyGtkCellEditable_Type;

    g_return_val_if_fail(PYGTK_IS_GENERIC_CELL_RENDERER (cell), NULL);

    state = pyg_gil_state_ensure();

    self = pygobject_new((GObject *)cell);

#ifdef DEBUG_CELL_RENDERER
    g_message ("start_editing()");
#endif
    py_event = pyg_boxed_new(GDK_TYPE_EVENT, event, FALSE, FALSE);
    py_widget = pygobject_new((GObject *)widget);
    py_background_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, background_area, TRUE, TRUE);
    py_cell_area = pyg_boxed_new(GDK_TYPE_RECTANGLE, cell_area, TRUE, TRUE);

    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "start_editing", "OOzOOi",
				 py_event, py_widget, path, py_background_area,
				 py_cell_area, flags);
    if (!py_ret) {
	PyErr_Print();
	Py_DECREF(py_event);
	Py_DECREF(py_widget);
	Py_DECREF(py_background_area);
	Py_DECREF(py_cell_area);
	pyg_gil_state_release(state);
	return NULL;
    }
    Py_DECREF(py_event);
    Py_DECREF(py_widget);
    Py_DECREF(py_background_area);
    Py_DECREF(py_cell_area);
    if (py_ret == Py_None) {
	ret = NULL;
    }
    else if (pygobject_check(py_ret, &PyGtkCellEditable_Type)) {
	ret = GTK_CELL_EDITABLE(g_object_ref(pygobject_get(py_ret)));
    }
    else {
	g_warning("return of start_editing() was not a GtkCellEditable");
    }
    Py_DECREF(py_ret);
    pyg_gil_state_release(state);
    return ret;
}

GtkCellRenderer *
pygtk_generic_cell_renderer_new(void)
{
    if (PyErr_Warn(PyExc_DeprecationWarning,
                   "subclass gtk.CellRenderer and override do_xxx methods") < 0)
        return NULL;
    return GTK_CELL_RENDERER(
        g_object_new(PYGTK_TYPE_GENERIC_CELL_RENDERER, NULL));
}
