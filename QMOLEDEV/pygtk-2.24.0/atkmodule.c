/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   atkmodule.c: module wrapping the ATK library.
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
#include "config.h"
#endif

/* include this first, before NO_IMPORT_PYGOBJECT is defined */
#include <pygobject.h>

void pyatk_register_classes (PyObject *d);
void pyatk_add_constants(PyObject *module, const gchar *strip_prefix);
void _pyatk_register_boxed_types(void);	

extern PyMethodDef pyatk_functions[];

DL_EXPORT(void)
initatk(void)
{
    PyObject *m, *d;
	
    init_pygobject ();

    m = Py_InitModule ("atk", pyatk_functions);
    d = PyModule_GetDict (m);
    _pyatk_register_boxed_types();	
    pyatk_register_classes (d);
    pyatk_add_constants(m, "ATK_");    
}
