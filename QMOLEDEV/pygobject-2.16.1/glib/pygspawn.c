/* -*- Mode: C; c-basic-offset: 4 -*-
 * pyglib - Python bindings for GLib toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *               2004-2008  Johan Dahlin
 *
 *   pygspawn.c: wrapper for the glib library.
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

#include <Python.h>
#include <glib/gspawn.h>

#include "pyglib.h"
#include "pyglib-private.h"

struct _PyGChildSetupData {
    PyObject *func;
    PyObject *data;
};

PYGLIB_DEFINE_TYPE("glib.Pid", PyGPid_Type, _PyLongObject)

static PyObject *
pyg_pid_close(PyObject *self, PyObject *args, PyObject *kwargs)
{
    g_spawn_close_pid(_PyLong_AsLong(self));
    Py_INCREF(Py_None);
    return Py_None;
}

static PyMethodDef pyg_pid_methods[] = {
    { "close", (PyCFunction)pyg_pid_close, METH_NOARGS },
    { NULL, NULL, 0 }
};

static void
pyg_pid_free(PyObject *gpid)
{
    g_spawn_close_pid((GPid) _PyLong_AsLong(gpid));
    _PyLong_Type.tp_free((void *) gpid);
}

static int
pyg_pid_tp_init(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyErr_SetString(PyExc_TypeError, "glib.Pid cannot be manually instantiated");
    return -1;
}

PyObject *
pyg_pid_new(GPid pid)
{
    _PyLongObject *pygpid;
    pygpid = PyObject_NEW(_PyLongObject, &PyGPid_Type);

#if PY_VERSION_HEX >= 0x03000000
#   warning "FIXME: figure out how to subclass long"    
#else
    pygpid->ob_ival = pid;
#endif    
    return (PyObject *) pygpid;
}

static void
_pyg_spawn_async_callback(gpointer user_data)
{
    struct _PyGChildSetupData *data;
    PyObject *retval;
    PyGILState_STATE gil;

    data = (struct _PyGChildSetupData *) user_data;
    gil = pyglib_gil_state_ensure();
    if (data->data)
        retval = PyObject_CallFunction(data->func, "O", data->data);
    else
        retval = PyObject_CallFunction(data->func, NULL);
    if (retval)
	Py_DECREF(retval);
    else
	PyErr_Print();
    Py_DECREF(data->func);
    Py_XDECREF(data->data);
    g_slice_free(struct _PyGChildSetupData, data);
    pyglib_gil_state_release(gil);
}

PyObject *
pyglib_spawn_async(PyObject *object, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "argv", "envp", "working_directory", "flags",
                              "child_setup", "user_data", "standard_input",
                              "standard_output", "standard_error", NULL };
    PyObject *pyargv, *pyenvp = NULL;
    char **argv, **envp = NULL;
    PyObject *func = Py_None, *user_data = NULL;
    char *working_directory = NULL;
    int flags = 0, _stdin = -1, _stdout = -1, _stderr = -1;
    PyObject *pystdin = NULL, *pystdout = NULL, *pystderr = NULL;
    gint *standard_input, *standard_output, *standard_error;
    struct _PyGChildSetupData *callback_data = NULL;
    GError *error = NULL;
    GPid child_pid = -1;
    Py_ssize_t len, i;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O|OsiOOOOO:glib.spawn_async",
                                     kwlist,
                                     &pyargv, &pyenvp, &working_directory, &flags,
                                     &func, &user_data,
                                     &pystdin, &pystdout, &pystderr))
        return NULL;

    if (pystdin && PyObject_IsTrue(pystdin))
        standard_input = &_stdin;
    else
        standard_input = NULL;

    if (pystdout && PyObject_IsTrue(pystdout))
        standard_output = &_stdout;
    else
        standard_output = NULL;

    if (pystderr && PyObject_IsTrue(pystderr))
        standard_error = &_stderr;
    else
        standard_error = NULL;

      /* parse argv */
    if (!PySequence_Check(pyargv)) {
        PyErr_SetString(PyExc_TypeError,
                        "glib.spawn_async: "
			"first argument must be a sequence of strings");
        return NULL;
    }
    len = PySequence_Length(pyargv);
    argv = g_new0(char *, len + 1);
    for (i = 0; i < len; ++i) {
        PyObject *tmp = PySequence_ITEM(pyargv, i);
        if (!_PyUnicode_Check(tmp)) {
            PyErr_SetString(PyExc_TypeError,
                            "glib.spawn_async: "
			    "first argument must be a sequence of strings");
            g_free(argv);
            Py_XDECREF(tmp);
            return NULL;
        }
        argv[i] = _PyUnicode_AsString(tmp);
        Py_DECREF(tmp);
    }

      /* parse envp */
    if (pyenvp) {
        if (!PySequence_Check(pyenvp)) {
            PyErr_SetString(PyExc_TypeError,
                            "glib.spawn_async: "
			    "second argument must be a sequence of strings");
            g_free(argv);
            return NULL;
        }
        len = PySequence_Length(pyenvp);
        envp = g_new0(char *, len + 1);
        for (i = 0; i < len; ++i) {
            PyObject *tmp = PySequence_ITEM(pyenvp, i);
            if (!_PyUnicode_Check(tmp)) {
                PyErr_SetString(PyExc_TypeError,
                                "glib.spawn_async: "
				"second argument must be a sequence of strings");
                g_free(envp);
                Py_XDECREF(tmp);
		g_free(argv);
                return NULL;
            }
            envp[i] = _PyUnicode_AsString(tmp);
            Py_DECREF(tmp);
        }
    }

    if (func != Py_None) {
        if (!PyCallable_Check(func)) {
            PyErr_SetString(PyExc_TypeError, "child_setup parameter must be callable or None");
            g_free(argv);
            if (envp)
                g_free(envp);
            return NULL;
        }
        callback_data = g_slice_new(struct _PyGChildSetupData);
        callback_data->func = func;
        callback_data->data = user_data;
        Py_INCREF(callback_data->func);
        if (callback_data->data)
            Py_INCREF(callback_data->data);
    }

    if (!g_spawn_async_with_pipes(working_directory, argv, envp, flags,
                                  (func != Py_None ? _pyg_spawn_async_callback : NULL),
                                  callback_data, &child_pid,
                                  standard_input,
                                  standard_output,
                                  standard_error,
                                  &error))


    {
        g_free(argv);
        if (envp) g_free(envp);
        if (callback_data) {
            Py_DECREF(callback_data->func);
            Py_XDECREF(callback_data->data);
            g_slice_free(struct _PyGChildSetupData, callback_data);
        }
        pyglib_error_check(&error);
        return NULL;
    }
    g_free(argv);
    if (envp) g_free(envp);

    if (standard_input)
        pystdin = _PyLong_FromLong(*standard_input);
    else {
        Py_INCREF(Py_None);
        pystdin = Py_None;
    }

    if (standard_output)
        pystdout = _PyLong_FromLong(*standard_output);
    else {
        Py_INCREF(Py_None);
        pystdout = Py_None;
    }

    if (standard_error)
        pystderr = _PyLong_FromLong(*standard_error);
    else {
        Py_INCREF(Py_None);
        pystderr = Py_None;
    }

    return Py_BuildValue("NNNN", pyg_pid_new(child_pid), pystdin, pystdout, pystderr);
}

void
pyglib_spawn_register_types(PyObject *d)
{
    PyGPid_Type.tp_base = &_PyLong_Type;
    PyGPid_Type.tp_flags = Py_TPFLAGS_DEFAULT;
    PyGPid_Type.tp_methods = pyg_pid_methods;
    PyGPid_Type.tp_init = pyg_pid_tp_init;
    PyGPid_Type.tp_free = (freefunc)pyg_pid_free;
    PYGLIB_REGISTER_TYPE(d, PyGPid_Type, "Pid");
}
