/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 2006  Johannes Hoelzl
 *
 *   pygoptioncontext.c: GOptionContext wrapper
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

#include <pyglib.h>
#include "pyglib-private.h"
#include "pygoptioncontext.h"

PYGLIB_DEFINE_TYPE("glib.OptionContext", PyGOptionContext_Type, PyGOptionContext)

static int
pyg_option_context_init(PyGOptionContext *self,
                        PyObject *args,
                        PyObject *kwargs)
{
    char *parameter_string;

    if (!PyArg_ParseTuple(args, "s:glib.GOptionContext.__init__",
                          &parameter_string))
        return -1;

    self->context = g_option_context_new(parameter_string);
    return 0;
}

static void
pyg_option_context_dealloc(PyGOptionContext *self)
{
    Py_CLEAR(self->main_group);

    if (self->context != NULL)
    {
        GOptionContext *tmp = self->context;
        self->context = NULL;
        g_option_context_free(tmp);
    }

    PyObject_Del(self);
}

static PyObject *
pyg_option_context_parse(PyGOptionContext *self,
                         PyObject *args,
                         PyObject *kwargs)
{
    static char *kwlist[] = { "argv", NULL };
    PyObject *arg;
    PyObject *new_argv, *argv;
    Py_ssize_t argv_length, pos;
    gint argv_length_int;
    char **argv_content, **original;
    GError *error = NULL;
    gboolean result;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O:GOptionContext.parse",
                                     kwlist, &argv))
        return NULL;

    if (!PyList_Check(argv))
    {
        PyErr_SetString(PyExc_TypeError,
                        "GOptionContext.parse expects a list of strings.");
        return NULL;
    }

    argv_length = PyList_Size(argv);
    if (argv_length == -1)
    {
        PyErr_SetString(PyExc_TypeError,
                        "GOptionContext.parse expects a list of strings.");
        return NULL;
    }

    argv_content = g_new(char*, argv_length + 1);
    argv_content[argv_length] = NULL;
    for (pos = 0; pos < argv_length; pos++)
    {
        arg = PyList_GetItem(argv, pos);
        argv_content[pos] = g_strdup(_PyUnicode_AsString(arg));
        if (argv_content[pos] == NULL)
        {
            g_strfreev(argv_content);
            return NULL;
        }
    }
    original = g_strdupv(argv_content);

    g_assert(argv_length <= G_MAXINT);
    argv_length_int = argv_length;
    pyglib_begin_allow_threads;
    result = g_option_context_parse(self->context, &argv_length_int, &argv_content,
                                    &error);
    pyglib_end_allow_threads;
    argv_length = argv_length_int;

    if (!result)
    {
        g_strfreev(argv_content);
        g_strfreev(original);
        pyglib_error_check(&error);
        return NULL;
    }

    new_argv = PyList_New(g_strv_length(argv_content));
    for (pos = 0; pos < argv_length; pos++)
    {
        arg = _PyUnicode_FromString(argv_content[pos]);
        PyList_SetItem(new_argv, pos, arg);
    }

    g_strfreev(original);
    g_strfreev(argv_content);
    return new_argv;
}

static PyObject *
pyg_option_context_set_help_enabled(PyGOptionContext *self,
                                    PyObject *args,
                                    PyObject *kwargs)
{
    static char *kwlist[] = { "help_enable", NULL };

    PyObject *help_enabled;
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "O:GOptionContext.set_help_enabled",
                                      kwlist, &help_enabled))
        return NULL;

    g_option_context_set_help_enabled(self->context, PyObject_IsTrue(help_enabled));

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_option_context_get_help_enabled(PyGOptionContext *self)
{
    return PyBool_FromLong(g_option_context_get_help_enabled(self->context));
}

static PyObject *
pyg_option_context_set_ignore_unknown_options(PyGOptionContext *self,
                                              PyObject *args,
                                              PyObject *kwargs)
{
    static char *kwlist[] = { "ignore_unknown_options", NULL };
    PyObject *ignore_unknown_options;

    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                "O:GOptionContext.set_ignore_unknown_options",
                                kwlist, &ignore_unknown_options))
        return NULL;

    g_option_context_set_ignore_unknown_options(self->context,
						PyObject_IsTrue(ignore_unknown_options));
    

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_option_context_get_ignore_unknown_options(PyGOptionContext *self)
{
    return PyBool_FromLong(
        g_option_context_get_ignore_unknown_options(self->context));
}

static PyObject *
pyg_option_context_set_main_group(PyGOptionContext *self,
                                  PyObject *args,
                                  PyObject *kwargs)
{
    static char *kwlist[] = { "group", NULL };
    GOptionGroup *g_group;
    PyObject *group;

    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "O:GOptionContext.set_main_group",
                                      kwlist, &group))
        return NULL;

    if (PyObject_IsInstance(group, (PyObject*) &PyGOptionGroup_Type) != 1) {
        PyErr_SetString(PyExc_TypeError,
                        "GOptionContext.set_main_group expects a GOptionGroup.");
        return NULL;
    }

    g_group = pyglib_option_group_transfer_group(group);
    if (g_group == NULL)
    {
        PyErr_SetString(PyExc_RuntimeError,
			"Group is already in a OptionContext.");
        return NULL;
    }

    g_option_context_set_main_group(self->context, g_group);

    Py_INCREF(group);
    self->main_group = (PyGOptionGroup*) group;

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_option_context_get_main_group(PyGOptionContext *self)
{
    if (self->main_group == NULL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }
    Py_INCREF(self->main_group);
    return (PyObject*) self->main_group;
}

static PyObject *
pyg_option_context_add_group(PyGOptionContext *self,
                             PyObject *args,
                             PyObject *kwargs)
{
    static char *kwlist[] = { "group", NULL };
    GOptionGroup *g_group;
    PyObject *group;

    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "O:GOptionContext.add_group",
                                      kwlist, &group))
        return NULL;

    if (PyObject_IsInstance(group, (PyObject*) &PyGOptionGroup_Type) != 1) {
        PyErr_SetString(PyExc_TypeError,
                        "GOptionContext.add_group expects a GOptionGroup.");
        return NULL;
    }

    g_group = pyglib_option_group_transfer_group(group);
    if (g_group == NULL)
    {
        PyErr_SetString(PyExc_RuntimeError,
                        "Group is already in a OptionContext.");
        return NULL;
    }
    Py_INCREF(group);

    g_option_context_add_group(self->context, g_group);

    Py_INCREF(Py_None);
    return Py_None;
}

static int
pyg_option_context_compare(PyGOptionContext *self, PyGOptionContext *context)
{
    if (self->context == context->context) return 0;
    if (self->context > context->context)
        return 1;
    return -1;
}

static PyMethodDef pyg_option_context_methods[] = {
    { "parse", (PyCFunction)pyg_option_context_parse, METH_VARARGS | METH_KEYWORDS },
    { "set_help_enabled", (PyCFunction)pyg_option_context_set_help_enabled, METH_VARARGS | METH_KEYWORDS },
    { "get_help_enabled", (PyCFunction)pyg_option_context_get_help_enabled, METH_NOARGS },
    { "set_ignore_unknown_options", (PyCFunction)pyg_option_context_set_ignore_unknown_options, METH_VARARGS | METH_KEYWORDS },
    { "get_ignore_unknown_options", (PyCFunction)pyg_option_context_get_ignore_unknown_options, METH_NOARGS },
    { "set_main_group", (PyCFunction)pyg_option_context_set_main_group, METH_VARARGS | METH_KEYWORDS },
    { "get_main_group", (PyCFunction)pyg_option_context_get_main_group, METH_NOARGS },
    { "add_group", (PyCFunction)pyg_option_context_add_group, METH_VARARGS | METH_KEYWORDS },
    { NULL, NULL, 0 },
};

void
pyglib_option_context_register_types(PyObject *d)
{
    PyGOptionContext_Type.tp_dealloc = (destructor)pyg_option_context_dealloc;
    PyGOptionContext_Type.tp_compare = (cmpfunc)pyg_option_context_compare;
    PyGOptionContext_Type.tp_flags = Py_TPFLAGS_DEFAULT;
    PyGOptionContext_Type.tp_methods = pyg_option_context_methods;
    PyGOptionContext_Type.tp_init = (initproc)pyg_option_context_init;
    PYGLIB_REGISTER_TYPE(d, PyGOptionContext_Type, "OptionContext");
}
