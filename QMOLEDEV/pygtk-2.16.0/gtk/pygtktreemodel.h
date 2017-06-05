/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   pygtktreemodel.h: stub class to help implement tree models.
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

#include <Python.h>

#include <gtk/gtk.h>

#define PYGTK_TYPE_GENERIC_TREE_MODEL            (pygtk_generic_tree_model_get_type())
#define PYGTK_GENERIC_TREE_MODEL(object)         (G_TYPE_CHECK_INSTANCE_CAST((object), PYGTK_TYPE_GENERIC_TREE_MODEL, PyGtkGenericTreeModel))
#define PYGTK_GENERIC_TREE_MODEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), PYGTK_TYPE_GENERIC_TREE_MODEL, PyGtkGenericTreeModelClass))
#define PYGTK_IS_GENERIC_TREE_MODEL(object)      (G_TYPE_CHECK_INSTANCE_TYPE((object), PYGTK_TYPE_GENERIC_TREE_MODEL))
#define PYGTK_IS_GENERIC_TREE_MODEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), PYGTK_TYPE_GENERIC_TREE_MODEL))
#define PYGTK_GENERIC_TREE_MODEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), PYGTK_TYPE_GENERIC_TREE_MODEL, PyGtkGenericTreeModelClass))

typedef struct _PyGtkGenericTreeModel PyGtkGenericTreeModel;
typedef struct _PyGtkGenericTreeModelClass PyGtkGenericTreeModelClass;

struct _PyGtkGenericTreeModel {
    GObject parent_instance;

    gboolean leak_references;
    gint stamp;
};

struct _PyGtkGenericTreeModelClass {
    GObjectClass parent_class;
};

GType                   pygtk_generic_tree_model_get_type (void);
PyGtkGenericTreeModel * pygtk_generic_tree_model_new      (void);
void                    pygtk_generic_tree_model_invalidate_iters(PyGtkGenericTreeModel *);
gboolean                pygtk_generic_tree_model_iter_is_valid(PyGtkGenericTreeModel *,
                                                               GtkTreeIter *);
PyObject *              pygtk_generic_tree_model_get_user_data(PyGtkGenericTreeModel *,
                                                               GtkTreeIter *);
GtkTreeIter             pygtk_generic_tree_model_create_tree_iter(PyGtkGenericTreeModel *,
                                                                  PyObject *);
