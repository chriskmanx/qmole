/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 2008  Johan Dahlin
 * Copyright (C) 2008  Gian Mario Tagliaretti
 *
 *   giomodule.c: module wrapping the GIO library
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include "pygio-utils.h"

/**
 * pygio_check_cancellable:
 * @pycancellable:
 * @cancellable:
 *
 * Returns:
 */
gboolean
pygio_check_cancellable(PyGObject *pycancellable,
			GCancellable **cancellable)
{
  if (pycancellable == NULL || (PyObject*)pycancellable == Py_None)
      *cancellable = NULL;
  else if (pygobject_check(pycancellable, &PyGCancellable_Type))
      *cancellable = G_CANCELLABLE(pycancellable->obj);
  else
    {
      PyErr_SetString(PyExc_TypeError,
		      "cancellable should be a gio.Cancellable");
      return FALSE;
    }
  return TRUE;
}

/**
 * pygio_check_launch_context:
 * @pycontext:
 * @context:
 *
 * Returns:
 */
gboolean
pygio_check_launch_context(PyGObject *pycontext,
                           GAppLaunchContext **context)
{
  if (pycontext == NULL || (PyObject*)pycontext == Py_None)
      *context = NULL;
  else if (pygobject_check(pycontext, &PyGAppLaunchContext_Type))
      *context = G_APP_LAUNCH_CONTEXT(pycontext->obj);
  else
    {
      PyErr_SetString(PyExc_TypeError,
		      "launch_context should be a GAppLaunchContext or None");
      return FALSE;
    }
  return TRUE;
}

/**
 * pygio_pylist_to_gfile_glist:
 * @pyfile_list:
 *
 * Returns:
 */
GList *
pygio_pylist_to_gfile_glist(PyObject *pyfile_list)
{
    GList *file_list = NULL;
    PyObject *item;
    int len, i;

    len = PySequence_Size(pyfile_list);
    for (i = 0; i < len; i++) {
        item = PySequence_GetItem(pyfile_list, i);
        if (!PyObject_TypeCheck(item, &PyGFile_Type)) {
            PyErr_SetString(PyExc_TypeError,
                            "files must be a list or tuple of GFile");
            g_list_free(file_list);
            return NULL;
        }
        file_list = g_list_prepend(file_list, ((PyGObject *)item)->obj);
    }
    file_list = g_list_reverse(file_list);

    return file_list;
}

/**
 * pygio_pylist_to_uri_glist:
 * @pyfile_list:
 *
 * Returns:
 */
GList *
pygio_pylist_to_uri_glist(PyObject *pyfile_list)
{
    GList *file_list = NULL;
    PyObject *item;
    int len, i;

    len = PySequence_Size(pyfile_list);
    for (i = 0; i < len; i++) {
    item = PySequence_GetItem(pyfile_list, i);
        if (!PyString_Check(item)) {
            PyErr_SetString(PyExc_TypeError,
                            "files must be strings");
            g_list_free(file_list);
            return NULL;
        }
        file_list = g_list_prepend(file_list, PyString_AsString(item));
    }
    file_list = g_list_reverse(file_list);

    return file_list;
}
