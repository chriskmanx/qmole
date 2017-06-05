/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygobject - Python bindings for GObject
 * Copyright (C) 2008  Johan Dahlin
 *
 *   unixmodule.c: module wrapping the GIO UNIX library
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
#  include "config.h"
#endif
#include <Python.h>
#include <pygobject.h>

#include <gio/gio.h>

/* include any extra headers needed here */

void pyunix_register_classes(PyObject *d);
void pyunix_add_constants(PyObject *module, const gchar *strip_prefix);

extern PyMethodDef pyunix_functions[];

DL_EXPORT(void)
initunix(void)
{
    PyObject *m, *d;

    /* perform any initialisation required by the library here */

    m = Py_InitModule("gio.unix", pyunix_functions);
    d = PyModule_GetDict(m);

    init_pygobject();

    pyunix_register_classes(d);

}

