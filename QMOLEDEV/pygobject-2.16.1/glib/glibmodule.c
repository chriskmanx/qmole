/* -*- Mode: C; c-set-style: python; c-basic-offset: 4  -*-
 * pyglib - Python bindings for GLib toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *               2004-2008  Johan Dahlin
 *
 *   glibmodule.c: wrapper for the glib library.
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
#include <glib.h>
#include "pyglib.h"
#include "pyglib-private.h"
#include "pygiochannel.h"
#include "pygmaincontext.h"
#include "pygmainloop.h"
#include "pygoptioncontext.h"
#include "pygoptiongroup.h"
#include "pygsource.h"
#include "pygspawn.h"

#define PYGLIB_MAJOR_VERSION PYGOBJECT_MAJOR_VERSION
#define PYGLIB_MINOR_VERSION PYGOBJECT_MINOR_VERSION
#define PYGLIB_MICRO_VERSION PYGOBJECT_MICRO_VERSION


/* ---------------- glib module functions -------------------- */

struct _PyGChildData {
    PyObject *func;
    PyObject *data;
};

static gint
get_handler_priority(gint *priority, PyObject *kwargs)
{
    Py_ssize_t len, pos;
    PyObject *key, *val;

    /* no keyword args? leave as default */
    if (kwargs == NULL)	return 0;

    len = PyDict_Size(kwargs);
    if (len == 0) return 0;

    if (len != 1) {
	PyErr_SetString(PyExc_TypeError,
			"expecting at most one keyword argument");
	return -1;
    }
    pos = 0;
    PyDict_Next(kwargs, &pos, &key, &val);
    if (!_PyUnicode_Check(key)) {
	PyErr_SetString(PyExc_TypeError,
			"keyword argument name is not a string");
	return -1;
    }

    if (strcmp(_PyUnicode_AsString(key), "priority") != 0) {
	PyErr_SetString(PyExc_TypeError,
			"only 'priority' keyword argument accepted");
	return -1;
    }

    *priority = _PyLong_AsLong(val);
    if (PyErr_Occurred()) {
	PyErr_Clear();
	PyErr_SetString(PyExc_ValueError, "could not get priority value");
	return -1;
    }
    return 0;
}

static PyObject *
pyglib_idle_add(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *first, *callback, *cbargs = NULL, *data;
    gint len, priority = G_PRIORITY_DEFAULT_IDLE;
    guint handler_id;

    len = PyTuple_Size(args);
    if (len < 1) {
	PyErr_SetString(PyExc_TypeError,
			"idle_add requires at least 1 argument");
	return NULL;
    }
    first = PySequence_GetSlice(args, 0, 1);
    if (!PyArg_ParseTuple(first, "O:idle_add", &callback)) {
	Py_DECREF(first);
        return NULL;
    }
    Py_DECREF(first);
    if (!PyCallable_Check(callback)) {
        PyErr_SetString(PyExc_TypeError, "first argument not callable");
        return NULL;
    }
    if (get_handler_priority(&priority, kwargs) < 0)
	return NULL;

    cbargs = PySequence_GetSlice(args, 1, len);
    if (cbargs == NULL)
	return NULL;

    data = Py_BuildValue("(ON)", callback, cbargs);
    if (data == NULL)
	return NULL;
    handler_id = g_idle_add_full(priority,
				 _pyglib_handler_marshal, data,
				 _pyglib_destroy_notify);
    return _PyLong_FromLong(handler_id);
}


static PyObject *
pyglib_timeout_add(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *first, *callback, *cbargs = NULL, *data;
    gint len, priority = G_PRIORITY_DEFAULT;
    guint interval, handler_id;

    len = PyTuple_Size(args);
    if (len < 2) {
	PyErr_SetString(PyExc_TypeError,
			"timeout_add requires at least 2 args");
	return NULL;
    }
    first = PySequence_GetSlice(args, 0, 2);
    if (!PyArg_ParseTuple(first, "IO:timeout_add", &interval, &callback)) {
	Py_DECREF(first);
        return NULL;
    }
    Py_DECREF(first);
    if (!PyCallable_Check(callback)) {
        PyErr_SetString(PyExc_TypeError, "second argument not callable");
        return NULL;
    }
    if (get_handler_priority(&priority, kwargs) < 0)
	return NULL;

    cbargs = PySequence_GetSlice(args, 2, len);
    if (cbargs == NULL)
	return NULL;

    data = Py_BuildValue("(ON)", callback, cbargs);
    if (data == NULL)
	return NULL;
    handler_id = g_timeout_add_full(priority, interval,
				    _pyglib_handler_marshal, data,
				    _pyglib_destroy_notify);
    return _PyLong_FromLong(handler_id);
}

static PyObject *
pyglib_timeout_add_seconds(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *first, *callback, *cbargs = NULL, *data;
    gint len, priority = G_PRIORITY_DEFAULT;
    guint interval, handler_id;

    len = PyTuple_Size(args);
    if (len < 2) {
	PyErr_SetString(PyExc_TypeError,
			"timeout_add_seconds requires at least 2 args");
	return NULL;
    }
    first = PySequence_GetSlice(args, 0, 2);
    if (!PyArg_ParseTuple(first, "IO:timeout_add_seconds", &interval, &callback)) {
	Py_DECREF(first);
        return NULL;
    }
    Py_DECREF(first);
    if (!PyCallable_Check(callback)) {
        PyErr_SetString(PyExc_TypeError, "second argument not callable");
        return NULL;
    }
    if (get_handler_priority(&priority, kwargs) < 0)
	return NULL;

    cbargs = PySequence_GetSlice(args, 2, len);
    if (cbargs == NULL)
	return NULL;

    data = Py_BuildValue("(ON)", callback, cbargs);
    if (data == NULL)
	return NULL;
    handler_id = g_timeout_add_seconds_full(priority, interval,
                                            _pyglib_handler_marshal, data,
                                            _pyglib_destroy_notify);
    return _PyLong_FromLong(handler_id);
}

static gboolean
iowatch_marshal(GIOChannel *source,
		GIOCondition condition,
		gpointer user_data)
{
    PyGILState_STATE state;
    PyObject *tuple, *func, *firstargs, *args, *ret;
    gboolean res;

    g_return_val_if_fail(user_data != NULL, FALSE);

    state = pyglib_gil_state_ensure();

    tuple = (PyObject *)user_data;
    func = PyTuple_GetItem(tuple, 0);

    /* arg vector is (fd, condtion, *args) */
    firstargs = Py_BuildValue("(Oi)", PyTuple_GetItem(tuple, 1), condition);
    args = PySequence_Concat(firstargs, PyTuple_GetItem(tuple, 2));
    Py_DECREF(firstargs);

    ret = PyObject_CallObject(func, args);
    Py_DECREF(args);
    if (!ret) {
	PyErr_Print();
	res = FALSE;
    } else {
        if (ret == Py_None) {
            if (PyErr_Warn(PyExc_Warning,
			   "glib.io_add_watch callback returned None; "
                           "should return True/False")) {
                PyErr_Print();
            }
        }
	res = PyObject_IsTrue(ret);
	Py_DECREF(ret);
    }

    pyglib_gil_state_release(state);

    return res;
}

static PyObject *
pyglib_io_add_watch(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *first, *pyfd, *callback, *cbargs = NULL, *data;
    gint fd, priority = G_PRIORITY_DEFAULT, condition;
    Py_ssize_t len;
    GIOChannel *iochannel;
    guint handler_id;

    len = PyTuple_Size(args);
    if (len < 3) {
	PyErr_SetString(PyExc_TypeError,
			"io_add_watch requires at least 3 args");
	return NULL;
    }
    first = PySequence_GetSlice(args, 0, 3);
    if (!PyArg_ParseTuple(first, "OiO:io_add_watch", &pyfd, &condition,
			  &callback)) {
	Py_DECREF(first);
        return NULL;
    }
    Py_DECREF(first);
    fd = PyObject_AsFileDescriptor(pyfd);
    if (fd < 0) {
	return NULL;
    }
    if (!PyCallable_Check(callback)) {
        PyErr_SetString(PyExc_TypeError, "third argument not callable");
        return NULL;
    }
    if (get_handler_priority(&priority, kwargs) < 0)
	return NULL;

    cbargs = PySequence_GetSlice(args, 3, len);
    if (cbargs == NULL)
      return NULL;
    data = Py_BuildValue("(OON)", callback, pyfd, cbargs);
    if (data == NULL)
      return NULL;
    iochannel = g_io_channel_unix_new(fd);
    handler_id = g_io_add_watch_full(iochannel, priority, condition,
				     iowatch_marshal, data,
				     (GDestroyNotify)_pyglib_destroy_notify);
    g_io_channel_unref(iochannel);
    
    return _PyLong_FromLong(handler_id);
}

static PyObject *
pyglib_source_remove(PyObject *self, PyObject *args)
{
    guint tag;

    if (!PyArg_ParseTuple(args, "i:source_remove", &tag))
	return NULL;

    return PyBool_FromLong(g_source_remove(tag));
}

static PyObject *
pyglib_main_context_default(PyObject *unused)
{
    return pyglib_main_context_new(g_main_context_default());
}

static void
child_watch_func(GPid pid, gint status, gpointer data)
{
    struct _PyGChildData *child_data = (struct _PyGChildData *) data;
    PyObject *retval;
    PyGILState_STATE gil;

    gil = pyglib_gil_state_ensure();
    if (child_data->data)
        retval = PyObject_CallFunction(child_data->func, "iiO", pid, status,
                                       child_data->data);
    else
        retval = PyObject_CallFunction(child_data->func, "ii", pid, status);

    if (retval)
	Py_DECREF(retval);
    else
	PyErr_Print();

    pyglib_gil_state_release(gil);
}

static void
child_watch_dnotify(gpointer data)
{
    struct _PyGChildData *child_data = (struct _PyGChildData *) data;
    Py_DECREF(child_data->func);
    Py_XDECREF(child_data->data);
    g_slice_free(struct _PyGChildData, child_data);
}


static PyObject *
pyglib_child_watch_add(PyObject *unused, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "pid", "function", "data", "priority", NULL };
    guint id;
    gint priority = G_PRIORITY_DEFAULT;
    int pid;
    PyObject *func, *user_data = NULL;
    struct _PyGChildData *child_data;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "iO|Oi:glib.child_watch_add", kwlist,
                                     &pid, &func, &user_data, &priority))
        return NULL;
    if (!PyCallable_Check(func)) {
        PyErr_SetString(PyExc_TypeError,
                        "glib.child_watch_add: second argument must be callable");
        return NULL;
    }

    child_data = g_slice_new(struct _PyGChildData);
    child_data->func = func;
    child_data->data = user_data;
    Py_INCREF(child_data->func);
    if (child_data->data)
        Py_INCREF(child_data->data);
    id = g_child_watch_add_full(priority, pid, child_watch_func,
                                child_data, child_watch_dnotify);
    return _PyLong_FromLong(id);
}

static PyObject *
pyglib_markup_escape_text(PyObject *unused, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "text", NULL };
    char *text_in, *text_out;
    Py_ssize_t text_size;
    PyObject *retval;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "s#:glib.markup_escape_text", kwlist,
                                     &text_in, &text_size))
        return NULL;

    text_out = g_markup_escape_text(text_in, text_size);
    retval = _PyUnicode_FromString(text_out);
    g_free(text_out);
    return retval;
}

static PyObject *
pyglib_get_current_time(PyObject *unused)
{
    GTimeVal timeval;

    g_get_current_time(&timeval);
    return pyglib_float_from_timeval(timeval);
}

static PyObject *
pyglib_main_depth(PyObject *unused)
{
    return _PyLong_FromLong(g_main_depth());
}

static PyObject *
pyglib_filename_display_name(PyObject *self, PyObject *args)
{
    PyObject *py_display_name;
    char *filename, *display_name;
    
    if (!PyArg_ParseTuple(args, "s:glib.filename_display_name",
			  &filename))
	return NULL;

    display_name = g_filename_display_name(filename);
    py_display_name = PyUnicode_DecodeUTF8(display_name,
					   strlen(display_name), NULL);
    g_free(display_name);
    return py_display_name;
}

static PyObject *
pyglib_filename_display_basename(PyObject *self, PyObject *args)
{
    PyObject *py_display_basename;
    char *filename, *display_basename;
    
    if (!PyArg_ParseTuple(args, "s:glib.filename_display_basename",
			  &filename))
	return NULL;

    display_basename = g_filename_display_basename(filename);
    py_display_basename = PyUnicode_DecodeUTF8(display_basename,
					       strlen(display_basename), NULL);
    g_free(display_basename);
    return py_display_basename;
}

static PyObject *
pyglib_filename_from_utf8(PyObject *self, PyObject *args)
{
    char *filename, *utf8string;
    Py_ssize_t utf8string_len;
    gsize bytes_written;
    GError *error = NULL;
    PyObject *py_filename;
    
    if (!PyArg_ParseTuple(args, "s#:glib.filename_from_utf8",
			  &utf8string, &utf8string_len))
	return NULL;

    filename = g_filename_from_utf8(utf8string, utf8string_len,
				    NULL, &bytes_written, &error);
    if (pyglib_error_check(&error)) {
        g_free(filename);
        return NULL;
    }
    py_filename = _PyUnicode_FromStringAndSize(filename, bytes_written);
    g_free(filename);
    return py_filename;
}


static PyObject*
pyglib_get_application_name(PyObject *self)
{
    const char *name;

    name = g_get_application_name();
    if (!name) {
        Py_INCREF(Py_None);
        return Py_None;
    }
    return _PyUnicode_FromString(name);
}

static PyObject*
pyglib_set_application_name(PyObject *self, PyObject *arg)
{
    if (!PyString_Check(arg)) {
	PyErr_Format(PyExc_TypeError,
		     "first argument must be a string, not '%s'",
		     PyString_AS_STRING(PyObject_Repr(arg)));
	return NULL;
    }
    g_set_application_name(PyString_AS_STRING(arg));
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject*
pyglib_get_prgname(PyObject *self)
{
    char *name;

    name = g_get_prgname();
    if (!name) {
        Py_INCREF(Py_None);
        return Py_None;
    }
    return _PyUnicode_FromString(name);
}

static PyObject*
pyglib_set_prgname(PyObject *self, PyObject *arg)
{
    if (!PyString_Check(arg)) {
	PyErr_Format(PyExc_TypeError,
		     "first argument must be a string, not '%s'",
		     PyString_AS_STRING(PyObject_Repr(arg)));
	return NULL;
    }
    g_set_prgname(PyString_AS_STRING(arg));
    Py_INCREF(Py_None);
    return Py_None;
}

static PyMethodDef _glib_functions[] = {
    { "idle_add",
      (PyCFunction)pyglib_idle_add, METH_VARARGS|METH_KEYWORDS,
      "idle_add(callable, user_data=None, priority=None) -> source id\n"
      "  callable receives (user_data)\n"
      "Adds a callable to be called whenever there are no higher priority\n"
      "events pending to the default main loop." },
    { "timeout_add",
      (PyCFunction)pyglib_timeout_add, METH_VARARGS|METH_KEYWORDS,
      "timeout_add(interval, callable, user_data=None,\n"
      "            priority=None) -> source id\n"
      "  callable receives (user_data)\n"
      "Sets a callable be called repeatedly until it returns False." },
    { "timeout_add_seconds",
      (PyCFunction)pyglib_timeout_add_seconds, METH_VARARGS|METH_KEYWORDS,
      "timeout_add(interval, callable, user_data=None,\n"
      "            priority=None) -> source_id\n"
      "  callable receives (user_data)\n"
      "Sets a callable be called repeatedly until it returns False.\n"
      "Use this if you want to have a timer in the \"seconds\" range\n"
      "and do not care about the exact time of the first call of the\n"
      "timer, use this for more efficient system power usage." },
    { "io_add_watch",
      (PyCFunction)pyglib_io_add_watch, METH_VARARGS|METH_KEYWORDS,
      "io_add_watch(fd, condition, callback, user_data=None) -> source id\n"
      "  callable receives (fd, condition, user_data)\n"
      "Arranges for the fd to be monitored by the main loop for the\n"
      "specified condition. Condition is a combination of glib.IO_IN,\n"
      "glib.IO_OUT, glib.IO_PRI, gio.IO_ERR and gio.IO_HUB.\n" },
    { "child_watch_add",
      (PyCFunction)pyglib_child_watch_add, METH_VARARGS|METH_KEYWORDS,
      "child_watch_add(pid, callable, user_data=None,\n"
                       "priority=None) -> source id\n"
      "  callable receives (pid, condition, user_data)\n"
      "Sets the function specified by function to be called with the user\n"
      "data specified by data when the child indicated by pid exits.\n"
      "Condition is a combination of glib.IO_IN, glib.IO_OUT, glib.IO_PRI,\n"
      "gio.IO_ERR and gio.IO_HUB." },
    { "source_remove",
      (PyCFunction)pyglib_source_remove, METH_VARARGS,
      "source_remove(source_id) -> True if removed\n"
      "Removes the event source specified by source id as returned by the\n"
      "glib.idle_add(), glib.timeout_add() or glib.io_add_watch()\n"
      "functions." },
    { "spawn_async",
      (PyCFunction)pyglib_spawn_async, METH_VARARGS|METH_KEYWORDS,
      "spawn_async(argv, envp=None, working_directory=None,\n"
      "            flags=0, child_setup=None, user_data=None,\n"
      "            standard_input=None, standard_output=None,\n"
      "            standard_error=None) -> (pid, stdin, stdout, stderr)\n"
      "Execute a child program asynchronously within a glib.MainLoop()\n"
      "See the reference manual for a complete reference." },
    { "main_context_default",
      (PyCFunction)pyglib_main_context_default, METH_NOARGS,
      "main_context_default() -> a main context\n"
      "Returns the default main context. This is the main context used\n"
      "for main loop functions when a main loop is not explicitly specified." },
    { "main_depth",
      (PyCFunction)pyglib_main_depth, METH_NOARGS,
      "main_depth() -> stack depth\n"
      "Returns the depth of the stack of calls in the main context." },
    { "filename_display_name",
      (PyCFunction)pyglib_filename_display_name, METH_VARARGS },
    { "filename_display_basename",
      (PyCFunction)pyglib_filename_display_basename, METH_VARARGS },
    { "filename_from_utf8",
      (PyCFunction)pyglib_filename_from_utf8, METH_VARARGS },
    { "get_application_name",
      (PyCFunction)pyglib_get_application_name, METH_NOARGS },
    { "set_application_name",
      (PyCFunction)pyglib_set_application_name, METH_O },
    { "get_prgname",
      (PyCFunction)pyglib_get_prgname, METH_NOARGS },
    { "set_prgname",
      (PyCFunction)pyglib_set_prgname, METH_O },
    { "get_current_time",
      (PyCFunction)pyglib_get_current_time, METH_NOARGS },
    { "markup_escape_text",
      (PyCFunction)pyglib_markup_escape_text, METH_VARARGS|METH_KEYWORDS },
    { NULL, NULL, 0 }
};

/* ----------------- glib module initialisation -------------- */

static struct _PyGLib_Functions pyglib_api = {
    FALSE, /* threads_enabled */
    NULL,  /* gerror_exception */
    NULL,  /* block_threads */
    NULL  /* unblock_threads */
};

static void
pyglib_register_api(PyObject *d)
{
    PyObject *o;

    /* for addon libraries ... */
    PyDict_SetItemString(d, "_PyGLib_API",
			 o=PyCObject_FromVoidPtr(&pyglib_api,NULL));
    Py_DECREF(o);
    
    pyglib_init_internal(o);
}

static void
pyglib_register_error(PyObject *d)
{
    PyObject *dict;
    PyObject *gerror_class;
    dict = PyDict_New();
    /* This is a hack to work around the deprecation warning of
     * BaseException.message in Python 2.6+.
     * GError has also an "message" attribute.
     */
    PyDict_SetItemString(dict, "message", Py_None);
    gerror_class = PyErr_NewException("glib.GError", PyExc_RuntimeError, dict);
    Py_DECREF(dict);

    PyDict_SetItemString(d, "GError", gerror_class);
    pyglib_api.gerror_exception = gerror_class;
}

static void
pyglib_register_version_tuples(PyObject *d)
{
    PyObject *o;

    /* glib version */
    o = Py_BuildValue("(iii)", glib_major_version, glib_minor_version,
		      glib_micro_version);
    PyDict_SetItemString(d, "glib_version", o);
    Py_DECREF(o);

    /* pyglib version */
    o = Py_BuildValue("(iii)",
		      PYGLIB_MAJOR_VERSION,
		      PYGLIB_MINOR_VERSION,
		      PYGLIB_MICRO_VERSION);
    PyDict_SetItemString(d, "pyglib_version", o);
    Py_DECREF(o);
}

static void
pyglib_register_constants(PyObject *m)
{
    PyModule_AddIntConstant(m, "SPAWN_LEAVE_DESCRIPTORS_OPEN",
			    G_SPAWN_LEAVE_DESCRIPTORS_OPEN);
    PyModule_AddIntConstant(m, "SPAWN_DO_NOT_REAP_CHILD",
			    G_SPAWN_DO_NOT_REAP_CHILD);
    PyModule_AddIntConstant(m, "SPAWN_SEARCH_PATH",
			    G_SPAWN_SEARCH_PATH);
    PyModule_AddIntConstant(m, "SPAWN_STDOUT_TO_DEV_NULL",
			    G_SPAWN_STDOUT_TO_DEV_NULL);
    PyModule_AddIntConstant(m, "SPAWN_STDERR_TO_DEV_NULL",
			    G_SPAWN_STDERR_TO_DEV_NULL);
    PyModule_AddIntConstant(m, "SPAWN_CHILD_INHERITS_STDIN",
			    G_SPAWN_CHILD_INHERITS_STDIN);
    PyModule_AddIntConstant(m, "SPAWN_FILE_AND_ARGV_ZERO",
			    G_SPAWN_FILE_AND_ARGV_ZERO);

    PyModule_AddIntConstant(m, "PRIORITY_HIGH",
			    G_PRIORITY_HIGH);
    PyModule_AddIntConstant(m, "PRIORITY_DEFAULT",
			    G_PRIORITY_DEFAULT);
    PyModule_AddIntConstant(m, "PRIORITY_HIGH_IDLE",
			    G_PRIORITY_HIGH_IDLE);
    PyModule_AddIntConstant(m, "PRIORITY_DEFAULT_IDLE",
			    G_PRIORITY_DEFAULT_IDLE);
    PyModule_AddIntConstant(m, "PRIORITY_LOW",
			    G_PRIORITY_LOW);

    PyModule_AddIntConstant(m, "IO_IN",   G_IO_IN);
    PyModule_AddIntConstant(m, "IO_OUT",  G_IO_OUT);
    PyModule_AddIntConstant(m, "IO_PRI",  G_IO_PRI);
    PyModule_AddIntConstant(m, "IO_ERR",  G_IO_ERR);
    PyModule_AddIntConstant(m, "IO_HUP",  G_IO_HUP);
    PyModule_AddIntConstant(m, "IO_NVAL", G_IO_NVAL);

    PyModule_AddIntConstant(m, "IO_STATUS_ERROR",
			    G_IO_STATUS_ERROR);
    PyModule_AddIntConstant(m, "IO_STATUS_NORMAL",
			    G_IO_STATUS_NORMAL);
    PyModule_AddIntConstant(m, "IO_STATUS_EOF",
			    G_IO_STATUS_EOF);
    PyModule_AddIntConstant(m, "IO_STATUS_AGAIN",
			    G_IO_STATUS_AGAIN);
    PyModule_AddIntConstant(m, "IO_FLAG_APPEND",
			    G_IO_FLAG_APPEND);
    PyModule_AddIntConstant(m, "IO_FLAG_NONBLOCK",
			    G_IO_FLAG_NONBLOCK);
    PyModule_AddIntConstant(m, "IO_FLAG_IS_READABLE",
			    G_IO_FLAG_IS_READABLE);
    PyModule_AddIntConstant(m, "IO_FLAG_IS_WRITEABLE",
			    G_IO_FLAG_IS_WRITEABLE);
    PyModule_AddIntConstant(m, "IO_FLAG_IS_SEEKABLE",
			    G_IO_FLAG_IS_SEEKABLE);
    PyModule_AddIntConstant(m, "IO_FLAG_MASK",
			    G_IO_FLAG_MASK);
    PyModule_AddIntConstant(m, "IO_FLAG_GET_MASK",
			    G_IO_FLAG_GET_MASK);
    PyModule_AddIntConstant(m, "IO_FLAG_SET_MASK",
			    G_IO_FLAG_SET_MASK);

    PyModule_AddIntConstant(m, "OPTION_FLAG_HIDDEN",
			    G_OPTION_FLAG_HIDDEN);
    PyModule_AddIntConstant(m, "OPTION_FLAG_IN_MAIN",
			    G_OPTION_FLAG_IN_MAIN);
    PyModule_AddIntConstant(m, "OPTION_FLAG_REVERSE",
			    G_OPTION_FLAG_REVERSE);
    PyModule_AddIntConstant(m, "OPTION_FLAG_NO_ARG",
			    G_OPTION_FLAG_NO_ARG);
    PyModule_AddIntConstant(m, "OPTION_FLAG_FILENAME",
			    G_OPTION_FLAG_FILENAME);
    PyModule_AddIntConstant(m, "OPTION_FLAG_OPTIONAL_ARG",
			    G_OPTION_FLAG_OPTIONAL_ARG);
    PyModule_AddIntConstant(m, "OPTION_FLAG_NOALIAS",
			    G_OPTION_FLAG_NOALIAS); 

    PyModule_AddIntConstant(m, "OPTION_ERROR_UNKNOWN_OPTION",
			    G_OPTION_ERROR_UNKNOWN_OPTION);
    PyModule_AddIntConstant(m, "OPTION_ERROR_BAD_VALUE",
			    G_OPTION_ERROR_BAD_VALUE);
    PyModule_AddIntConstant(m, "OPTION_ERROR_FAILED",
			    G_OPTION_ERROR_FAILED);

    PyModule_AddStringConstant(m, "OPTION_REMAINING",
			       G_OPTION_REMAINING);
    PyModule_AddStringConstant(m, "OPTION_ERROR",
			       (char*) g_quark_to_string(G_OPTION_ERROR));
}

PYGLIB_MODULE_START(_glib, "glib._glib")
{
    PyObject *d = PyModule_GetDict(module);

    pyglib_register_constants(module);
    pyglib_register_api(d);
    pyglib_register_error(d);
    pyglib_register_version_tuples(d);
    pyglib_iochannel_register_types(d);
    pyglib_mainloop_register_types(d);
    pyglib_maincontext_register_types(d);
    pyglib_source_register_types(d);
    pyglib_spawn_register_types(d);
    pyglib_option_context_register_types(d);
    pyglib_option_group_register_types(d);
}
PYGLIB_MODULE_END
