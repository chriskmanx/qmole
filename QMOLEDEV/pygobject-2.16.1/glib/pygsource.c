/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 * Copyright (C) 2005       Oracle
 *
 * Author: Manish Singh <manish.singh@oracle.com>
 *
 *   pygsource.c: GSource wrapper
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
#include <pythread.h>
#include <structmember.h> /* for PyMemberDef */
#include "pyglib.h"
#include "pyglib-private.h"
#include "pygmaincontext.h"
#include "pygsource.h"

#define CHECK_DESTROYED(self, ret)			G_STMT_START {	\
    if ((self)->source == NULL) {					\
	PyErr_SetString(PyExc_RuntimeError, "source is destroyed");	\
	return (ret);							\
    }									\
} G_STMT_END


typedef struct {
    PyObject_HEAD
    GSource *source;
    PyObject *inst_dict;
    PyObject *weakreflist;
    gboolean python_source;
} PyGSource;

typedef struct
{
    GSource source;
    PyObject *obj;
} PyGRealSource;

/* glib.Source */

PYGLIB_DEFINE_TYPE("glib.Source", PyGSource_Type, PyGSource)

static PyObject *
source_repr(PyGSource *self, const char *type)
{
    gchar buf[256], *desc;
 
    if (self->source) {
	if (g_source_get_context(self->source))
	    desc = "attached";
	else
	    desc = "unattached";
    } else {
	desc = "destroyed";
    }

    if (type)
	g_snprintf(buf, sizeof(buf), "<%s glib %s source at 0x%lx>",
		   desc, type, (long) self);
    else
	g_snprintf(buf, sizeof(buf), "<%s glib source at 0x%lx>",
		   desc, (long) self);

    return _PyUnicode_FromString(buf);
}

static PyObject *
pyg_source_attach(PyGSource *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "context", NULL };
    PyGMainContext *py_context = NULL;
    GMainContext *context = NULL;
    guint id;

    if (!PyArg_ParseTupleAndKeywords (args, kwargs,
				      "|O!:attach", kwlist,
				      &PyGMainContext_Type, &py_context))
	return NULL;

    if (py_context)
	context = py_context->context;

    CHECK_DESTROYED(self, NULL);

    if (self->python_source) {
	PyGRealSource *pysource = (PyGRealSource *)self->source;
	Py_INCREF(pysource->obj);
    }

    id = g_source_attach(self->source, context);
    return _PyLong_FromLong(id);
}

static PyObject *
pyg_source_destroy(PyGSource *self)
{
    CHECK_DESTROYED(self, NULL);

    if (self->python_source && self->source->context) {
	PyGRealSource *pysource = (PyGRealSource *)self->source;
	Py_DECREF(pysource->obj);
    }

    g_source_destroy(self->source);
    self->source = NULL;

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_source_set_callback(PyGSource *self, PyObject *args)
{
    PyObject *first, *callback, *cbargs = NULL, *data;
    gint len;

    CHECK_DESTROYED(self, NULL);

    len = PyTuple_Size (args);
    if (len < 1) {
	PyErr_SetString(PyExc_TypeError,
			"set_callback requires at least 1 argument");
	return NULL;
    }

    first = PySequence_GetSlice(args, 0, 1);
    if (!PyArg_ParseTuple(first, "O:set_callback", &callback)) {
	Py_DECREF (first);
	return NULL;
    }
    Py_DECREF(first);

    if (!PyCallable_Check(callback)) {
	PyErr_SetString(PyExc_TypeError, "first argument not callable");
	return NULL;
    }

    cbargs = PySequence_GetSlice(args, 1, len);
    if (cbargs == NULL)
	return NULL;

    data = Py_BuildValue("(ON)", callback, cbargs);
    if (data == NULL)
	return NULL;

    g_source_set_callback(self->source,
			  _pyglib_handler_marshal, data,
			  _pyglib_destroy_notify);

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_source_get_context(PyGSource *self)
{
    GMainContext *context;

    CHECK_DESTROYED(self, NULL);

    context = g_source_get_context(self->source);

    if (context) {
	return pyglib_main_context_new(context);
    } else {
	Py_INCREF(Py_None);
	return Py_None;
    }
}

static PyObject *
pyg_source_add_poll(PyGSource *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "fd", NULL };
    PyGPollFD *fd;

    if (!self->python_source) {
	PyErr_SetString(PyExc_TypeError,
			"add_poll can only be used with sources "
			"implemented in python");
	return NULL;
    }

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "O!:add_poll", kwlist,
				     &PyGPollFD_Type, &fd))
	return NULL;

    CHECK_DESTROYED(self, NULL);

    g_source_add_poll(self->source, &fd->pollfd);

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_source_remove_poll(PyGSource *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "fd", NULL };
    PyGPollFD *fd;

    if (!self->python_source) {
	PyErr_SetString(PyExc_TypeError,
			"remove_poll can only be used with sources "
			"implemented in python");
	return NULL;
    }

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "O!:remove_poll", kwlist,
				     &PyGPollFD_Type, &fd))
	return NULL;

    CHECK_DESTROYED(self, NULL);

    g_source_remove_poll(self->source, &fd->pollfd);

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_source_get_current_time(PyGSource *self)
{
    GTimeVal timeval;
    double   ret;

    CHECK_DESTROYED(self, NULL);

    g_source_get_current_time(self->source, &timeval);
    ret = (double)timeval.tv_sec + (double)timeval.tv_usec * 0.000001;
    return PyFloat_FromDouble(ret);
}

static PyMethodDef pyg_source_methods[] = {
    { "attach", (PyCFunction)pyg_source_attach, METH_KEYWORDS },
    { "destroy", (PyCFunction)pyg_source_destroy, METH_NOARGS },
    { "set_callback", (PyCFunction)pyg_source_set_callback, METH_VARARGS },
    { "get_context", (PyCFunction)pyg_source_get_context, METH_NOARGS },
    { "add_poll", (PyCFunction)pyg_source_add_poll, METH_KEYWORDS },
    { "remove_poll", (PyCFunction)pyg_source_remove_poll, METH_KEYWORDS },
    { "get_current_time", (PyCFunction)pyg_source_get_current_time, METH_NOARGS },
    { NULL, NULL, 0 }
};

static PyObject *
pyg_source_get_dict(PyGSource *self, void *closure)
{
    if (self->inst_dict == NULL) {
	self->inst_dict = PyDict_New();
	if (self->inst_dict == NULL)
	    return NULL;
    }

    Py_INCREF(self->inst_dict);
    return self->inst_dict;
}

static PyObject *
pyg_source_get_priority(PyGSource *self, void *closure)
{
    CHECK_DESTROYED(self, NULL);

    return _PyLong_FromLong(g_source_get_priority(self->source));
}

static int
pyg_source_set_priority(PyGSource *self, PyObject *value, void *closure)
{
    CHECK_DESTROYED(self, -1);

    if (value == NULL) {
	PyErr_SetString(PyExc_TypeError, "cannot delete priority");
	return -1;
    }

    if (!_PyLong_Check(value)) {
	PyErr_SetString(PyExc_TypeError, "type mismatch");
	return -1;
    }

    g_source_set_priority(self->source, _PyLong_AsLong(value));

    return 0;
}

static PyObject *
pyg_source_get_can_recurse(PyGSource *self, void *closure)
{
    CHECK_DESTROYED(self, NULL);

    return PyBool_FromLong(g_source_get_can_recurse(self->source));
}

static int
pyg_source_set_can_recurse(PyGSource *self, PyObject *value, void *closure)
{
    CHECK_DESTROYED(self, -1);

    if (value == NULL) {
	PyErr_SetString(PyExc_TypeError, "cannot delete can_recurse");
	return -1;
    }

    g_source_set_can_recurse(self->source, PyObject_IsTrue(value));

    return 0;
}

static PyObject *
pyg_source_get_id(PyGSource *self, void *closure)
{
    CHECK_DESTROYED(self, NULL);

    if (g_source_get_context(self->source) == NULL) {
	PyErr_SetString(PyExc_RuntimeError, "source is not attached");
	return NULL;
    }

    return _PyLong_FromLong(g_source_get_id(self->source));
}

static PyGetSetDef pyg_source_getsets[] = {
    { "__dict__", (getter)pyg_source_get_dict,  (setter)0 },
    {"priority", (getter)pyg_source_get_priority, (setter)pyg_source_set_priority },
    {"can_recurse", (getter)pyg_source_get_can_recurse, (setter)pyg_source_set_can_recurse },
    {"id", (getter)pyg_source_get_id, (setter)0 },
    {NULL, 0, 0}
};

static PyObject *
pyg_source_repr(PyGSource *self)
{
    return source_repr(self, NULL);
}

static int
pyg_source_traverse(PyGSource *self, visitproc visit, void *arg)
{
    int ret = 0;

    if (self->inst_dict) ret = visit(self->inst_dict, arg);
    if (ret != 0) return ret;

    return 0;
}

static int
pyg_source_clear(PyGSource *self)
{
    PyObject *tmp;

    tmp = self->inst_dict;
    self->inst_dict = NULL;
    Py_XDECREF(tmp);

    if (self->source) {
	g_source_unref(self->source);
	self->source = NULL;
    }

    return 0;
}

static void
pyg_source_dealloc(PyGSource *self)
{
    PyObject_ClearWeakRefs((PyObject *)self);

    PyObject_GC_UnTrack((PyObject *)self);

    pyg_source_clear(self);

    PyObject_GC_Del(self);
}

static gboolean
pyg_source_prepare(GSource *source, gint *timeout)
{
    PyGRealSource *pysource = (PyGRealSource *)source;
    PyObject *t;
    gboolean ret = FALSE;
    gboolean got_err = TRUE;
    PyGILState_STATE state;

    state = pyglib_gil_state_ensure();

    t = PyObject_CallMethod(pysource->obj, "prepare", NULL);

    if (t == NULL) {
	goto bail;
    } else if (!PyObject_IsTrue(t)) {
	got_err = FALSE;
	goto bail;
    } else if (!PyTuple_Check(t)) {
	PyErr_SetString(PyExc_TypeError,
			"source prepare function must return a tuple or False");
	goto bail;
    } else if (PyTuple_Size(t) != 2) {
	PyErr_SetString(PyExc_TypeError,
			"source prepare function return tuple must be exactly "
			"2 elements long");
	goto bail;
    }

    ret = PyObject_IsTrue(PyTuple_GET_ITEM(t, 0));
	*timeout = _PyLong_AsLong(PyTuple_GET_ITEM(t, 1));

	if (*timeout == -1 && PyErr_Occurred()) {
	    ret = FALSE;
	    goto bail;
	}

    got_err = FALSE;

bail:
    if (got_err)
	PyErr_Print();

    Py_XDECREF(t);

    pyglib_gil_state_release(state);

    return ret;
}

static gboolean
pyg_source_check(GSource *source)
{
    PyGRealSource *pysource = (PyGRealSource *)source;
    PyObject *t;
    gboolean ret;
    PyGILState_STATE state;

    state = pyglib_gil_state_ensure();

    t = PyObject_CallMethod(pysource->obj, "check", NULL);

    if (t == NULL) {
	PyErr_Print();
	ret = FALSE;
    } else {
	ret = PyObject_IsTrue(t);
	Py_DECREF(t);
    }

    pyglib_gil_state_release(state);

    return ret;
}

static gboolean
pyg_source_dispatch(GSource *source, GSourceFunc callback, gpointer user_data)
{
    PyGRealSource *pysource = (PyGRealSource *)source;
    PyObject *func, *args, *tuple, *t;
    gboolean ret;
    PyGILState_STATE state;

    state = pyglib_gil_state_ensure();

    if (callback) {
	tuple = user_data;

	func = PyTuple_GetItem(tuple, 0);
        args = PyTuple_GetItem(tuple, 1);
    } else {
	func = Py_None;
	args = Py_None;
    }

    t = PyObject_CallMethod(pysource->obj, "dispatch", "OO", func, args);

    if (t == NULL) {
	PyErr_Print();
	ret = FALSE;
    } else {
	ret = PyObject_IsTrue(t);
	Py_DECREF(t);
    }

    pyglib_gil_state_release(state);

    return ret;
}

static void
pyg_source_finalize(GSource *source)
{
    PyGRealSource *pysource = (PyGRealSource *)source;
    PyObject *func, *t;
    PyGILState_STATE state;

    state = pyglib_gil_state_ensure();

    func = PyObject_GetAttrString(pysource->obj, "finalize");
    if (func) {
	t = PyObject_CallObject(func, NULL);
	Py_DECREF(func);

	if (t == NULL) {
	    PyErr_Print();
	} else {
	    Py_DECREF(t);
	}
    }

    pyglib_gil_state_release(state);
}

static GSourceFuncs pyg_source_funcs =
{
    pyg_source_prepare,
    pyg_source_check,
    pyg_source_dispatch,
    pyg_source_finalize
};

static int
pyg_source_init(PyGSource *self, PyObject *args, PyObject *kwargs)
{
    PyGRealSource *pysource;

    self->source = g_source_new(&pyg_source_funcs, sizeof(PyGRealSource));

    pysource = (PyGRealSource *)self->source;
    pysource->obj = (PyObject*)self;

    self->inst_dict = NULL;
    self->weakreflist = NULL;

    self->python_source = TRUE;

    return 0;
}

static void
pyg_source_free(PyObject *op)
{
    PyObject_GC_Del(op);
}

/* glib.Idle */

PYGLIB_DEFINE_TYPE("glib.Idle", PyGIdle_Type, PyGSource)

static PyObject *
pyg_idle_repr(PyGSource *self)
{
    return source_repr(self, "idle");
}

static int
pyg_idle_init(PyGSource *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "priority", NULL };
    gint priority = G_PRIORITY_DEFAULT_IDLE;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "|i:glib.Idle.__init__", kwlist,
				     &priority))
	return -1;

    self->source = g_idle_source_new ();

    if (priority != G_PRIORITY_DEFAULT_IDLE)
	g_source_set_priority(self->source, priority);

    self->inst_dict = NULL;
    self->weakreflist = NULL;

    self->python_source = FALSE;

    return 0;
}

/* glib.Timeout */

PYGLIB_DEFINE_TYPE("glib.Timeout", PyGTimeout_Type, PyGSource)

static PyObject *
pyg_timeout_repr(PyGSource *self)
{
    return source_repr(self, "timeout");
}

static int
pyg_timeout_init(PyGSource *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "interval", "priority", NULL };
    gint priority = G_PRIORITY_DEFAULT;
    guint interval;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "I|i:glib.Timeout.__init__", kwlist,
				     &interval, &priority))
	return -1;

    self->source = g_timeout_source_new(interval);

    if (priority != G_PRIORITY_DEFAULT)
	g_source_set_priority(self->source, priority);

    self->inst_dict = NULL;
    self->weakreflist = NULL;

    self->python_source = FALSE;

    return 0;
}

/* glib.PollFD */

PYGLIB_DEFINE_TYPE("glib.PollFD", PyGPollFD_Type, PyGPollFD)

static PyMemberDef pyg_poll_fd_members[] = {
    { "fd",      T_INT,    offsetof(PyGPollFD, pollfd.fd),      READONLY },
    { "events",  T_USHORT, offsetof(PyGPollFD, pollfd.events),  READONLY },
    { "revents", T_USHORT, offsetof(PyGPollFD, pollfd.revents), READONLY },
    { NULL, 0, 0, 0 }
};

static void
pyg_poll_fd_dealloc(PyGPollFD *self)
{
    Py_XDECREF(self->fd_obj);
    PyObject_DEL(self);
}

static PyObject *
pyg_poll_fd_repr(PyGPollFD *self)
{
    return _PyUnicode_FromFormat("<GPollFD %d (%d) at 0x%lx>",
				 self->pollfd.fd, self->pollfd.events,
				 (long)self);
}

static int
pyg_poll_fd_init(PyGPollFD *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "fd", "events", NULL };
    PyObject *o;
    gint fd;
    gushort events;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "OH:glib.PollFD.__init__", kwlist,
				     &o, &events))
	return -1;

    fd = PyObject_AsFileDescriptor(o);
    if (fd == -1)
	return -1;

    self->pollfd.fd = fd;
    self->pollfd.events = events;
    self->pollfd.revents = 0;

    Py_INCREF(o);
    self->fd_obj = o;

    return 0;
}

void
pyglib_source_register_types(PyObject *d)
{
    PyGSource_Type.tp_flags = (Py_TPFLAGS_DEFAULT |
			       Py_TPFLAGS_BASETYPE |
			       Py_TPFLAGS_HAVE_GC);
    PyGSource_Type.tp_init = (initproc)pyg_source_init;
    PyGSource_Type.tp_free = (freefunc)pyg_source_free;
    PyGSource_Type.tp_dealloc = (destructor)pyg_source_dealloc;
    PyGSource_Type.tp_methods = pyg_source_methods;
    PyGSource_Type.tp_repr = (reprfunc)pyg_source_repr;
    PyGSource_Type.tp_traverse = (traverseproc)pyg_source_traverse;
    PyGSource_Type.tp_clear = (inquiry)pyg_source_clear;
    PyGSource_Type.tp_getset = pyg_source_getsets;
    PyGSource_Type.tp_weaklistoffset = offsetof(PyGSource, weakreflist);
    PyGSource_Type.tp_dictoffset = offsetof(PyGSource, inst_dict);
    PYGLIB_REGISTER_TYPE(d, PyGSource_Type, "Source");

    PyGIdle_Type.tp_repr = (reprfunc)pyg_idle_repr;
    PyGIdle_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
    PyGIdle_Type.tp_base = (PyTypeObject *)&PyGSource_Type;
    PyGIdle_Type.tp_init = (initproc)pyg_idle_init;
    PYGLIB_REGISTER_TYPE(d, PyGIdle_Type, "Idle");

    PyGTimeout_Type.tp_repr = (reprfunc)pyg_timeout_repr;
    PyGTimeout_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
    PyGTimeout_Type.tp_base = (PyTypeObject *)&PyGSource_Type;
    PyGTimeout_Type.tp_init = (initproc)pyg_timeout_init;
    PYGLIB_REGISTER_TYPE(d, PyGTimeout_Type, "Timeout");

    PyGPollFD_Type.tp_dealloc = (destructor)pyg_poll_fd_dealloc;
    PyGPollFD_Type.tp_repr = (reprfunc)pyg_poll_fd_repr;
    PyGPollFD_Type.tp_flags = Py_TPFLAGS_DEFAULT;
    PyGPollFD_Type.tp_members = pyg_poll_fd_members;
    PyGPollFD_Type.tp_init = (initproc)pyg_poll_fd_init;
    PYGLIB_REGISTER_TYPE(d, PyGPollFD_Type, "PollFD");
}
