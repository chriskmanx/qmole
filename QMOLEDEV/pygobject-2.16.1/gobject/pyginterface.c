/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *               2004-2008  Johan Dahlin
 *   pyginterface.c: wrapper for the gobject library.
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <Python.h>
#include "pyglib.h"
#include "pygobject-private.h"

GQuark pyginterface_type_key;
GQuark pyginterface_info_key;

PYGLIB_DEFINE_TYPE("gobject.GInterface", PyGInterface_Type, PyObject)

static int
pyg_interface_init(PyObject *self, PyObject *args, PyObject *kwargs)
{
    gchar buf[512];

    if (!PyArg_ParseTuple(args, ":GInterface.__init__"))
	return -1;

    g_snprintf(buf, sizeof(buf), "%s can not be constructed",
	       Py_TYPE(self)->tp_name);
    PyErr_SetString(PyExc_NotImplementedError, buf);
    return -1;
}

static void
pyg_interface_free(PyObject *op)
{
    PyObject_FREE(op);
}

/**
 * pyg_register_interface:
 * @dict: a module dictionary.
 * @class_name: the class name for the wrapper class.
 * @gtype: the GType of the interface.
 * @type: the wrapper class for the interface.
 *
 * Registers a Python class as the wrapper for a GInterface.  As a
 * convenience it will also place a reference to the wrapper class in
 * the provided module dictionary.
 */
void
pyg_register_interface(PyObject *dict, const gchar *class_name,
                       GType gtype, PyTypeObject *type)
{
    PyObject *o;

    Py_TYPE(type) = &PyType_Type;
    type->tp_base = &PyGInterface_Type;

    if (PyType_Ready(type) < 0) {
        g_warning("could not ready `%s'", type->tp_name);
        return;
    }

    if (gtype) {
        o = pyg_type_wrapper_new(gtype);
        PyDict_SetItemString(type->tp_dict, "__gtype__", o);
        Py_DECREF(o);
    }

    g_type_set_qdata(gtype, pyginterface_type_key, type);
    
    PyDict_SetItemString(dict, (char *)class_name, (PyObject *)type);
    
}

void
pyg_register_interface_info(GType gtype, const GInterfaceInfo *info)
{
    g_type_set_qdata(gtype, pyginterface_info_key, (gpointer) info);
}

const GInterfaceInfo *
pyg_lookup_interface_info(GType gtype)
{
    return g_type_get_qdata(gtype, pyginterface_info_key);
}

void
pygobject_interface_register_types(PyObject *d)
{
  pyginterface_type_key = g_quark_from_static_string("PyGInterface::type");
  pyginterface_info_key = g_quark_from_static_string("PyGInterface::info");

  PyGInterface_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  PyGInterface_Type.tp_init = (initproc)pyg_interface_init;
  PyGInterface_Type.tp_free = (freefunc)pyg_interface_free;

  PYGOBJECT_REGISTER_GTYPE(d, PyGInterface_Type, "GInterface", G_TYPE_INTERFACE)

  PyDict_SetItemString(PyGInterface_Type.tp_dict, "__doc__",
		       pyg_object_descr_doc_get());
  PyDict_SetItemString(PyGInterface_Type.tp_dict, "__gdoc__",
		       pyg_object_descr_doc_get());
  
}
