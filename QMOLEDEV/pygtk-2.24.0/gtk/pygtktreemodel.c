/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   pygtktreemodel.c: stub class to help implement tree models.
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

#include "pygtktreemodel.h"
#include "pygtk-private.h"

/* define this to print out debug messages */
#undef DEBUG_TREE_MODEL

#ifndef _
#  define _(s) (s)
#endif

#define VALID_ITER(iter, tree_model) \
 (iter != NULL && iter->stamp == PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp)

enum {
    PROP_LEAK_REFERENCES = 1
};

static void pygtk_generic_tree_model_class_init(PyGtkGenericTreeModelClass *klass);
static void pygtk_generic_tree_model_init(PyGtkGenericTreeModel *self);
static void pygtk_generic_tree_model_iface_init(GtkTreeModelIface *iface);
static void pygtk_generic_tree_model_set_property (GObject *object,
						   guint property_id,
						   const GValue *value,
						   GParamSpec *pspec);
static void pygtk_generic_tree_model_get_property (GObject *object,
						   guint property_id,
						   GValue *value,
						   GParamSpec *pspec);


GType
pygtk_generic_tree_model_get_type(void)
{
    static GType object_type = 0;

    if (!object_type) {
	static const GTypeInfo object_info = {
	    sizeof(PyGtkGenericTreeModelClass),
	    (GBaseInitFunc) NULL,
	    (GBaseFinalizeFunc) NULL,
	    (GClassInitFunc) pygtk_generic_tree_model_class_init,
	    NULL, /* class_finalize */
	    NULL, /* class_data */
	    sizeof(PyGtkGenericTreeModel),
	    0, /* n_preallocs */
	    (GInstanceInitFunc) pygtk_generic_tree_model_init,
	};
	static const GInterfaceInfo tree_model_info = {
	    (GInterfaceInitFunc) pygtk_generic_tree_model_iface_init,
	    NULL,
	    NULL,
	};

	object_type = g_type_register_static(G_TYPE_OBJECT,
					     "PyGtkGenericTreeModel",
					     &object_info, 0);
	g_type_add_interface_static(object_type,
				    GTK_TYPE_TREE_MODEL,
				    &tree_model_info);
    }
    return object_type;
}

static void
pygtk_generic_tree_model_class_init(PyGtkGenericTreeModelClass *klass)
{
    GObjectClass *object_class = (GObjectClass*) klass;

    object_class->get_property = pygtk_generic_tree_model_get_property;
    object_class->set_property = pygtk_generic_tree_model_set_property;
 
    g_object_class_install_property (object_class,
				     PROP_LEAK_REFERENCES,
				     g_param_spec_boolean ("leak_references",
					_("Leak references"),
					_("Enable referencing iterator "
	"objects (this will cause a memory leak or at least a reference "
	"counting leak). You might need it though, if you return newly "
	"created objects."),
					TRUE,
					G_PARAM_READWRITE));
}

static guint pygtk_generic_tree_model_get_flags(GtkTreeModel *tree_model);
static gint pygtk_generic_tree_model_get_n_columns(GtkTreeModel *tree_model);
static GType pygtk_generic_tree_model_get_column_type(GtkTreeModel *tree_model,
					      gint index);
static gboolean pygtk_generic_tree_model_get_iter(GtkTreeModel *tree_model,
					  GtkTreeIter *iter,
					  GtkTreePath *path);
static GtkTreePath *pygtk_generic_tree_model_get_path(GtkTreeModel *tree_model,
					      GtkTreeIter *iter);
static void pygtk_generic_tree_model_get_value(GtkTreeModel*tree_model,
				       GtkTreeIter *iter,
				       gint column, GValue *value);
static gboolean pygtk_generic_tree_model_iter_next(GtkTreeModel *tree_model,
					   GtkTreeIter *iter);
static gboolean pygtk_generic_tree_model_iter_children(GtkTreeModel *tree_model,
					       GtkTreeIter *iter,
					       GtkTreeIter *parent);
static gboolean pygtk_generic_tree_model_iter_has_child(GtkTreeModel *tree_model,
						GtkTreeIter *iter);
static gint pygtk_generic_tree_model_iter_n_children(GtkTreeModel *tree_model,
					     GtkTreeIter *iter);
static gboolean pygtk_generic_tree_model_iter_nth_child(GtkTreeModel *tree_model,
						GtkTreeIter  *iter,
						GtkTreeIter  *parent,
						gint n);
static gboolean pygtk_generic_tree_model_iter_parent(GtkTreeModel *tree_model,
					     GtkTreeIter *iter,
					     GtkTreeIter *child);
static void pygtk_generic_tree_model_unref_node(GtkTreeModel *tree_model,
					     GtkTreeIter *iter);
static void pygtk_generic_tree_model_ref_node(GtkTreeModel *tree_model,
					     GtkTreeIter *iter);

static void
pygtk_generic_tree_model_iface_init(GtkTreeModelIface *iface)
{
  iface->get_flags = pygtk_generic_tree_model_get_flags;
  iface->get_n_columns = pygtk_generic_tree_model_get_n_columns;
  iface->get_column_type = pygtk_generic_tree_model_get_column_type;
  iface->get_iter = pygtk_generic_tree_model_get_iter;
  iface->get_path = pygtk_generic_tree_model_get_path;
  iface->get_value = pygtk_generic_tree_model_get_value;
  iface->iter_next = pygtk_generic_tree_model_iter_next;
  iface->iter_children = pygtk_generic_tree_model_iter_children;
  iface->iter_has_child = pygtk_generic_tree_model_iter_has_child;
  iface->iter_n_children = pygtk_generic_tree_model_iter_n_children;
  iface->iter_nth_child = pygtk_generic_tree_model_iter_nth_child;
  iface->iter_parent = pygtk_generic_tree_model_iter_parent;
  iface->ref_node = pygtk_generic_tree_model_ref_node;
  iface->unref_node = pygtk_generic_tree_model_unref_node;

}

static void
pygtk_generic_tree_model_init(PyGtkGenericTreeModel *self)
{
    self->leak_references = TRUE;
    do {
        self->stamp = g_random_int();
    } while (self->stamp == 0);
}

static void
pygtk_generic_tree_model_set_property (GObject *object, guint property_id,
				       const GValue *value, GParamSpec *pspec)
{
    switch (property_id) {
    case PROP_LEAK_REFERENCES:
	PYGTK_GENERIC_TREE_MODEL (object)->leak_references = g_value_get_boolean (value);
	break;
    default:
	G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
	break;
    }
 }
 
static void
pygtk_generic_tree_model_get_property (GObject *object, guint property_id,
				       GValue *value, GParamSpec *pspec)
{
    switch (property_id) {
    case PROP_LEAK_REFERENCES:
	g_value_set_boolean (value,
			    PYGTK_GENERIC_TREE_MODEL (object)->leak_references);
	break;
    default:
	G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
	break;
    }
}


PyGtkGenericTreeModel *
pygtk_generic_tree_model_new(void)
{
    return PYGTK_GENERIC_TREE_MODEL(
	g_object_new(PYGTK_TYPE_GENERIC_TREE_MODEL, NULL));
}


/* format of GtkTreeIter's for PyGtkGenericTreeModel:
 *  user_data == python object
 *  user_data2 == floating reference?
 *
 * I haven't worked out how everything should work.  For now I will
 * leak references.
 */

#define METHOD_PREFIX "on_"

static guint
pygtk_generic_tree_model_get_flags(GtkTreeModel *tree_model)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret;
    guint ret = 0;
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), 0);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_flags()");
#endif
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "get_flags", "");
    Py_DECREF(self);
    if (py_ret) {
	ret = PyInt_AsLong(py_ret);
	Py_DECREF(py_ret);
    } else {
	PyErr_Print();
    }
    
    pyg_gil_state_release(state);
    return ret;
}

static gint
pygtk_generic_tree_model_get_n_columns(GtkTreeModel *tree_model)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret;
    gint ret = 0;
    
    g_return_val_if_fail(tree_model != NULL, 0);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), 0);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_n_columns()");
#endif
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "get_n_columns", "");
    Py_DECREF(self);
    if (py_ret) {
	ret = PyInt_AsLong(py_ret);
	Py_DECREF(py_ret);
    } else {
	PyErr_Print();
    }
    pyg_gil_state_release(state);
    return ret;
}

static GType
pygtk_generic_tree_model_get_column_type(GtkTreeModel *tree_model, gint index)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret;
    GType ret = G_TYPE_INVALID;
    
    g_return_val_if_fail(tree_model != NULL, G_TYPE_INVALID);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), G_TYPE_INVALID);
    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_column_type(%d)", index);
#endif
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "get_column_type",
				 "(i)", index);
    Py_DECREF(self);
    if (py_ret) {
	ret = pyg_type_from_object(py_ret);
	Py_DECREF(py_ret);
    } else {
	PyErr_Print();
    }
    pyg_gil_state_release(state);
    return ret;
}

static gboolean
pygtk_generic_tree_model_get_iter(GtkTreeModel *tree_model,
                                  GtkTreeIter *iter, GtkTreePath *path)
{
    PyGILState_STATE state;
    PyObject *self, *py_path, *py_ret;
    gboolean ret = FALSE;
    
    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);
    g_return_val_if_fail(path != NULL, FALSE);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_iter(%p)", path);
#endif
    py_path = pygtk_tree_path_to_pyobject(path);
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "get_iter",
				 "(O)", py_path);
    Py_DECREF(self);
    Py_DECREF(py_path);

    if (py_ret) {
	if (py_ret != Py_None) {
	    iter->user_data = py_ret;
            iter->stamp = PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp;
	    if (!PYGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Py_DECREF((PyObject *)iter->user_data);
	    }
	    ret = TRUE;
	} else {
	    iter->user_data = NULL;
	    Py_DECREF(py_ret);
	}
    } else {
	PyErr_Print();
	iter->user_data = NULL;
    }

    pyg_gil_state_release(state);
    return ret;
}

static GtkTreePath *
pygtk_generic_tree_model_get_path(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_iter;
    GtkTreePath *path = NULL;
    
    g_return_val_if_fail(tree_model != NULL, NULL);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), NULL);
    g_return_val_if_fail(VALID_ITER(iter, tree_model), NULL);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_path(%p)", iter);
#endif
    py_iter = (PyObject *)iter->user_data;
    if (py_iter == NULL)
	py_iter = Py_None;

    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "get_path", "(O)",
                                 py_iter);
    Py_DECREF(self);
    if (py_ret) {
	path = pygtk_tree_path_from_pyobject(py_ret);

	if (!path)
	    g_warning("could not convert return value of get_path() to "
		      "a GtkTreePath");
	Py_DECREF(py_ret);
    } else {
	PyErr_Print();
    }
    
    pyg_gil_state_release(state);
    return path;
}

static void
pygtk_generic_tree_model_get_value(GtkTreeModel*tree_model, GtkTreeIter *iter,
                                   gint column, GValue *value)
{
    PyGILState_STATE state;
    PyObject *self, *py_value, *py_iter;

    g_return_if_fail(tree_model != NULL);
    g_return_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model));
    g_return_if_fail(VALID_ITER(iter, tree_model));

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_value(%p, %d)", iter, column);
    _PyObject_Dump (iter->user_data);
#endif
    /* init value to column type */
    g_value_init(value, pygtk_generic_tree_model_get_column_type(tree_model,
                                                                 column));

    py_iter = (PyObject *)iter->user_data;
    if (py_iter == NULL)
	py_iter = Py_None;
    py_value = PyObject_CallMethod(self, METHOD_PREFIX "get_value",
				   "(Oi)", py_iter,column);
    Py_DECREF(self);

    if (py_value) {
        if (py_value != Py_None)
            pyg_value_from_pyobject(value, py_value);
        Py_DECREF(py_value);
    } else {
	PyErr_Print();
    }
    pyg_gil_state_release(state);
}

static gboolean
pygtk_generic_tree_model_iter_next(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_iter;
    gboolean ret = FALSE;
    
    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(VALID_ITER(iter, tree_model), FALSE);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_next(%p)", iter);
#endif
    py_iter = (PyObject *)iter->user_data;
    if (py_iter == NULL)
	py_iter = Py_None;

    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "iter_next", "(O)",
                                 py_iter);
    Py_DECREF(self);
    if (py_ret) {
	if (py_ret != Py_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = py_ret;
	    if (!PYGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Py_DECREF((PyObject *)iter->user_data);
	    }
	    ret = TRUE;
	} else {
	    iter->user_data = NULL;
	    Py_DECREF(py_ret);
	}
    } else {
        iter->user_data = NULL;
	PyErr_Print();
    }

    pyg_gil_state_release(state);
    return ret;
}

static gboolean
pygtk_generic_tree_model_iter_children(GtkTreeModel *tree_model,
                                       GtkTreeIter *iter,
                                       GtkTreeIter *parent)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_parent = Py_None;
    gboolean ret = FALSE;
    
    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);
    g_return_val_if_fail(parent == NULL || parent->stamp == PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp, FALSE);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_children(%p, %p)", iter, parent);
#endif
    if (parent && parent->user_data != NULL)
	py_parent = (PyObject *)parent->user_data;
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "iter_children",
				 "(O)", py_parent);
    Py_DECREF(self);
    if (py_ret) {
	if (py_ret != Py_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = py_ret;
            iter->stamp = PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp;
	    if (!PYGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Py_DECREF((PyObject *)iter->user_data);
	    }
	    ret = TRUE;
	} else {
	    iter->user_data = NULL;
	    Py_DECREF(py_ret);
	}
    } else {
	iter->user_data = NULL;
	PyErr_Print();
    }
    
    pyg_gil_state_release(state);
    return ret;
}

static gboolean
pygtk_generic_tree_model_iter_has_child(GtkTreeModel *tree_model,
                                        GtkTreeIter *iter)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_iter;
    gboolean ret = FALSE;
    
    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(VALID_ITER(iter, tree_model), FALSE);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_has_child(%p)", iter);
#endif
    py_iter = (PyObject *)iter->user_data;
    if (py_iter == NULL)
	py_iter = Py_None;

    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "iter_has_child",
				 "(O)", py_iter);
    Py_DECREF(self);
    if (py_ret) {
	ret = PyObject_IsTrue(py_ret);

	Py_DECREF(py_ret);
    } else {
	PyErr_Print();
    }
    
    pyg_gil_state_release(state);
    return ret;
}

static gint
pygtk_generic_tree_model_iter_n_children(GtkTreeModel *tree_model,
                                         GtkTreeIter *iter)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_iter;
    guint ret = FALSE;
    
    g_return_val_if_fail(tree_model != NULL, 0);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), 0);
    g_return_val_if_fail(iter == NULL || iter->stamp == PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp, 0);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_n_children(%p)", iter);
#endif

    py_iter = iter != NULL ? (PyObject *)iter->user_data : Py_None;

    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "iter_n_children",
				 "(O)", py_iter);
    Py_DECREF(self);
    if (py_ret) {
	ret = PyInt_AsLong(py_ret);

	Py_DECREF(py_ret);
    } else {
	PyErr_Print();
    }
    
    pyg_gil_state_release(state);
    return ret;
}

static gboolean
pygtk_generic_tree_model_iter_nth_child(GtkTreeModel *tree_model,
                                        GtkTreeIter  *iter,
                                        GtkTreeIter  *parent, gint n)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_parent = Py_None;
    gboolean ret = FALSE;
    
    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);
    g_return_val_if_fail(parent == NULL || parent->stamp == PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp, FALSE);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_nth_child(%p, %p, %d)", iter, parent, n);
#endif
    if (parent && parent->user_data != NULL)
	py_parent = (PyObject *)parent->user_data;
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "iter_nth_child",
				 "(Oi)", py_parent, n);
    Py_DECREF(self);
    if (py_ret) {
	if (py_ret != Py_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = py_ret;
            iter->stamp = PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp;
	    if (!PYGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Py_DECREF((PyObject *)iter->user_data);
	    }
	    ret = TRUE;
	} else {
	    iter->user_data = NULL;
	    Py_DECREF(py_ret);
	}
    } else {
	iter->user_data = NULL;
	PyErr_Print();
    }
    
    pyg_gil_state_release(state);
    return ret;
}

static gboolean
pygtk_generic_tree_model_iter_parent(GtkTreeModel *tree_model,
                                     GtkTreeIter *iter,
                                     GtkTreeIter *child)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_child = Py_None;
    gboolean ret = FALSE;
    
    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);
    g_return_val_if_fail(VALID_ITER(child, tree_model), FALSE);

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_parent(%p, %p)", iter, child);
#endif
    if (child && child->user_data != NULL)
	py_child = (PyObject *)child->user_data;
    py_ret = PyObject_CallMethod(self, METHOD_PREFIX "iter_parent",
				 "(O)", py_child);
    Py_DECREF(self);
    if (py_ret) {
	if (py_ret != Py_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = py_ret;
            iter->stamp = PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp;
	    if (!PYGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Py_DECREF((PyObject *)iter->user_data);
	    }
	    ret = TRUE;
	} else {
	    iter->user_data = NULL;
	    Py_DECREF(py_ret);
	}
    } else {
	iter->user_data = NULL;
	PyErr_Print();
    }
    
    pyg_gil_state_release(state);
    return ret;
}

static void
pygtk_generic_tree_model_unref_node(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_iter, *method;

    g_return_if_fail(tree_model != NULL);
    g_return_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model));
    g_return_if_fail(VALID_ITER(iter, tree_model));

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("unref_node(%p)", iter);
#endif
    py_iter = (PyObject *)iter->user_data;
    if (py_iter == NULL)
	py_iter = Py_None;

    method = PyObject_GetAttrString(self, METHOD_PREFIX "unref_node");
    if (method == NULL)
	PyErr_Clear();
    else {
	py_ret = PyObject_CallFunction(method, "(O)", py_iter);
	if (py_ret) {
	    Py_DECREF(py_ret);
	} else {
	    PyErr_Print();
	}
    }
    Py_DECREF(self);

    pyg_gil_state_release(state);
}

static void
pygtk_generic_tree_model_ref_node(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    PyGILState_STATE state;
    PyObject *self, *py_ret, *py_iter, *method;

    g_return_if_fail(tree_model != NULL);
    g_return_if_fail(PYGTK_IS_GENERIC_TREE_MODEL(tree_model));
    g_return_if_fail(VALID_ITER(iter, tree_model));

    state = pyg_gil_state_ensure();

    /* this call finds the wrapper for this GObject */
    self = pygobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("ref_node(%p)", iter);
#endif
    py_iter = (PyObject *)iter->user_data;
    if (py_iter == NULL)
	py_iter = Py_None;

    method = PyObject_GetAttrString(self, METHOD_PREFIX "ref_node");
    if (method == NULL)
	PyErr_Clear();
    else {
	py_ret = PyObject_CallFunction(method, "(O)", py_iter);
	if (py_ret) {
	    Py_DECREF(py_ret);
	} else {
	    PyErr_Print();
	}
    }
    Py_DECREF(self);

    pyg_gil_state_release(state);
}

void
pygtk_generic_tree_model_invalidate_iters(PyGtkGenericTreeModel *tree_model)
{
    g_return_if_fail(tree_model != NULL);

    do {
        tree_model->stamp++;
    } while (tree_model->stamp == 0);
}

gboolean
pygtk_generic_tree_model_iter_is_valid(PyGtkGenericTreeModel *tree_model,
                                       GtkTreeIter *iter)
{
    g_return_val_if_fail(tree_model != NULL, FALSE);

    return VALID_ITER(iter, tree_model);
}

PyObject *
pygtk_generic_tree_model_get_user_data(PyGtkGenericTreeModel *tree_model,
                                       GtkTreeIter *iter)
{
    g_return_val_if_fail(tree_model != NULL, NULL);

    if (VALID_ITER(iter, tree_model)) {
        /* Py_INCREF and NULL checking is done at _wrap_*() level. */
        return iter->user_data;
    }
    else {
        g_warning("iter is not valid for the tree model");
        return NULL;
    }
}

GtkTreeIter
pygtk_generic_tree_model_create_tree_iter(PyGtkGenericTreeModel *tree_model,
                                          PyObject *user_data)
{
    GtkTreeIter  iter = {0,};

    if (tree_model != NULL) {
	iter.user_data = user_data;
	iter.stamp = PYGTK_GENERIC_TREE_MODEL(tree_model)->stamp;

	/* Otherwise, caller is supposed to hold a reference somewhere. */
	if (PYGTK_GENERIC_TREE_MODEL(tree_model)->leak_references)
	    Py_INCREF(user_data);
    }
    else {
	/* FIXME: I guess this is still not enough. */
	iter.user_data = NULL;
	iter.stamp = 0;
    }

    return iter;
}
