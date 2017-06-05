/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 2006  John Finlay
 *
 *   gtkunixprintmodule.c: wrapper for the gtkunixprint library.
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
/* include this first, before NO_IMPORT_PYGOBJECT is defined */
#include <pygobject.h>
#include <pygtk.h>
#include <gtk/gtk.h>
#include <gtk/gtkunixprint.h>

# include <pycairo.h>
Pycairo_CAPI_t *Pycairo_CAPI;
 
void pygtkunixprint_register_classes(PyObject *d);
void pygtkunixprint_add_constants(PyObject *module, const gchar *strip_prefix);
extern PyMethodDef pygtkunixprint_functions[];

DL_EXPORT(void)
initgtkunixprint(void)
{
    PyObject *m, *d;

    m = Py_InitModule("gtkunixprint", pygtkunixprint_functions);
    d = PyModule_GetDict(m);

    init_pygobject();
    Pycairo_IMPORT;
    init_pygtk();

    pygtkunixprint_register_classes(d);
    pygtkunixprint_add_constants(m, "GTK_");
}
