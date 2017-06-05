/* -*- Mode: C; c-basic-offset: 4 -*-
 * pyglib - Python bindings for GLib toolkit.
 * Copyright (C) 2008  Johan Dahlin
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

#ifndef __PYGLIB_PYTHON_COMPAT_H__
#define __PYGLIB_PYTHON_COMPAT_H__

/* Python 2.3 does not define Py_CLEAR */
#ifndef Py_CLEAR
#define Py_CLEAR(op)                \
        do {                                \
                if (op) {           \
                        PyObject *tmp = (PyObject *)(op);   \
                        (op) = NULL;        \
                        Py_DECREF(tmp);     \
                }               \
        } while (0)
#endif

/* Compilation on Python 2.4 */
#if PY_VERSION_HEX < 0x02050000
typedef int Py_ssize_t;
#define PY_SSIZE_T_MAX INT_MAX
#define PY_SSIZE_T_MIN INT_MIN
typedef inquiry lenfunc;
#endif

/* Compilation on Python 2.x */
#if PY_VERSION_HEX < 0x03000000
#define RO READONLY
#define _PyUnicode_Check PyString_Check
#define _PyUnicode_AsString PyString_AsString
#define _PyUnicode_AsStringAndSize PyString_AsStringAndSize
#define _PyUnicode_FromString PyString_FromString
#define _PyUnicode_FromStringAndSize PyString_FromStringAndSize
#define _PyUnicode_FromFormat PyString_FromFormat
#define _PyUnicode_AS_STRING PyString_AS_STRING
#define _PyUnicode_GET_SIZE PyString_GET_SIZE
#define _PyUnicode_Resize _PyString_Resize
#define _PyUnicode_Type PyString_Type
#define _PyLong_Check PyInt_Check
#define _PyLong_FromLong PyInt_FromLong
#define _PyLong_AsLong  PyInt_AsLong
#define _PyLongObject PyIntObject
#define _PyLong_Type PyInt_Type
#define _PyLong_AS_LONG PyInt_AS_LONG
#define Py_TYPE(ob) (((PyObject*)(ob))->ob_type)
#else
#undef PYGLIB_MODULE_START
#undef PYGLIB_MODULE_END
#undef PYGLIB_DEFINE_TYPE
#undef PYGLIB_REGISTER_TYPE

#define PYGLIB_MODULE_START(symbol, modname)	        \
    static struct PyModuleDef _##symbol##module = {     \
    PyModuleDef_HEAD_INIT,                              \
    modname,                                            \
    NULL,                                               \
    -1,                                                 \
    symbol##_functions,                                 \
    NULL,                                               \
    NULL,                                               \
    NULL,                                               \
    NULL                                                \
};                                                      \
PyMODINIT_FUNC PyInit_##symbol(void)                    \
{                                                       \
    PyObject *module;                                   \
    module = PyModule_Create(&_##symbol##module);
#define PYGLIB_MODULE_END return module; }
#define PYGLIB_DEFINE_TYPE(typename, symbol, csymbol)	\
PyTypeObject symbol = {                                 \
    PyVarObject_HEAD_INIT(NULL, 0)                      \
    typename,                                           \
    sizeof(csymbol)                                     \
};
#define PYGLIB_REGISTER_TYPE(d, type, name)	            \
    if (!type.tp_alloc)                                 \
	    type.tp_alloc = PyType_GenericAlloc;            \
    if (!type.tp_new)                                   \
	    type.tp_new = PyType_GenericNew;                \
    if (PyType_Ready(&type))                            \
	    return;                                         \
    PyDict_SetItemString(d, name, (PyObject *)&type);

#define _PyUnicode_Check PyUnicode_Check
#define _PyUnicode_AsString PyUnicode_AsString
#define _PyUnicode_AsStringAndSize(obj, buf, size) PyUnicode_AsStringAndSize(obj, size)
#define _PyUnicode_FromString PyUnicode_FromString
#define _PyUnicode_FromStringAndSize PyUnicode_FromStringAndSize
#define _PyUnicode_FromFormat PyUnicode_FromFormat
#define _PyUnicode_AS_STRING _PyUnicode_AsString
#define _PyUnicode_GET_SIZE PyUnicode_GET_SIZE
#define _PyUnicode_Resize PyUnicode_Resize
#define _PyUnicode_Type PyUnicode_Type
#define _PyLong_Check PyLong_Check
#define _PyLong_FromLong PyLong_FromLong
#define _PyLong_AsLong PyLong_AsLong
#define _PyLong_AS_LONG PyLong_AS_LONG
#define _PyLongObject PyLongObject
#define _PyLong_Type PyLong_Type
#endif

#endif /* __PYGLIB_PYTHON_COMPAT_H__ */
