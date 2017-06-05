/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   pangomodule.c: module wrapping the Pango library
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
#include <Python.h>
#include <pygobject.h>

#include <pango/pangocairo.h>
#include <pycairo.h>

/* include any extra headers needed here */

void pypangocairo_register_classes(PyObject *d);
void pypangocairo_add_constants(PyObject *module, const gchar *strip_prefix);

extern PyMethodDef pypangocairo_functions[];
extern PyTypeObject PyPangoCairoContext_Type;
extern GType pypango_layout_line_type;

Pycairo_CAPI_t *Pycairo_CAPI;


DL_EXPORT(void)
initpangocairo(void)
{
    PyObject *m, *d;

    /* perform any initialisation required by the library here */

    m = Py_InitModule("pangocairo", pypangocairo_functions);
    d = PyModule_GetDict(m);

    Pycairo_IMPORT;
    if (Pycairo_CAPI == NULL)
        return;

    PyPangoCairoContext_Type.tp_base = &PycairoContext_Type;
    if (PyType_Ready(&PyPangoCairoContext_Type) < 0) {
        g_return_if_reached();
    }
    init_pygobject();

    pypangocairo_register_classes(d);

    Py_INCREF(&PyPangoCairoContext_Type);
    PyModule_AddObject(m, "CairoContext", (PyObject *)&PyPangoCairoContext_Type);

    pypango_layout_line_type = g_type_from_name("PangoLayoutLine");
}

