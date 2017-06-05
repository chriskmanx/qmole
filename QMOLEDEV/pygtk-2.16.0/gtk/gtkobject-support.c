/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2006  James Henstridge
 *
 *   gtkobject-support.c: some helper routines for the GTK module.
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

/* this module provides some of the base functionality of the GtkObject
 * wrapper system */

#include "pygtk-private.h"

/* ------------------- object support */

void
pygtk_custom_destroy_notify(gpointer user_data)
{
    PyGtkCustomNotify *cunote = user_data;
    PyGILState_STATE state;

    g_return_if_fail(user_data);
    state = pyg_gil_state_ensure();
    Py_XDECREF(cunote->func);
    Py_XDECREF(cunote->data);
    pyg_gil_state_release(state);
    
    g_free(cunote);
}

GdkAtom*
pygdk_atom_vector_from_sequence(PyObject *py_targets, gint *n_targets)
{
    gint i;
    GdkAtom *targets;

    if (!(py_targets = PySequence_Fast(py_targets,
                                       "targets must be a sequence")))
        return NULL;

    *n_targets = PySequence_Fast_GET_SIZE(py_targets);
    targets = g_new(GdkAtom, *n_targets);
    for (i = 0; i < *n_targets; i++) {
        PyObject *trgt = PySequence_Fast_GET_ITEM(py_targets, i);
        targets[i] = pygdk_atom_from_pyobject(trgt);
        if (PyErr_Occurred()) {
            PyErr_Clear();
            PyErr_SetString(PyExc_TypeError,
                            "each 'targets' item must be a GdkAtom or string");
            g_free(targets);
            Py_DECREF(py_targets);
            return NULL;
        }
    }
    Py_DECREF(py_targets);
    return targets;
}

GtkTargetList *
pygtk_target_list_from_sequence(PyObject *py_targets)
{
    gint n_targets, i;
    GtkTargetEntry *targets;
    GtkTargetList *target_list;

    if (!(py_targets = PySequence_Fast(py_targets,
                                       "target list must be a sequence")))
	return NULL;
    n_targets = PySequence_Fast_GET_SIZE(py_targets);
    targets = g_new(GtkTargetEntry, n_targets);
    for (i = 0; i < n_targets; i++) {
        PyObject *item = PySequence_Fast_GET_ITEM(py_targets, i);
        if (!PyArg_ParseTuple(item, "sii", &targets[i].target,
                              &targets[i].flags, &targets[i].info)) {
            PyErr_Clear();
            PyErr_SetString(PyExc_TypeError,
                            "target list items should be of form (string,int,int)");
            g_free(targets);
	    Py_DECREF(py_targets);
            return NULL;
        }
    }
    target_list = gtk_target_list_new(targets, n_targets);
    g_free(targets);
    Py_DECREF(py_targets);
    return target_list;
}

PyObject *
pygtk_target_list_to_list(GtkTargetList *targets)
{
    GList *tmp;
    PyObject *list = PyList_New(0);

    for (tmp = targets->list; tmp != NULL; tmp = tmp->next) {
        GtkTargetPair *pair = tmp->data;
        PyObject *item;
        gchar * name = gdk_atom_name(pair->target);
        item = Py_BuildValue("(Nii)",
                             PyString_FromString(name),
                             pair->flags, pair->info);
        PyList_Append(list, item);
        g_free(name);
        Py_DECREF(item);
    }
    return list;
}

void
pygtk_boxed_unref_shared(PyObject *boxed)
{
    if (boxed == Py_None) {
        Py_DECREF(Py_None);
        return;
    }
    PyGBoxed *pyboxed;
    g_return_if_fail(boxed != NULL && PyObject_TypeCheck(boxed, &PyGBoxed_Type));
    pyboxed = (PyGBoxed *) boxed;
    if (pyboxed->ob_refcnt != 1) {
        if (!pyboxed->free_on_dealloc) {
            pyboxed->boxed = g_boxed_copy(pyboxed->gtype,
                                          pyboxed->boxed);
            pyboxed->free_on_dealloc = TRUE;
        }
    }
    Py_DECREF(boxed);
}

