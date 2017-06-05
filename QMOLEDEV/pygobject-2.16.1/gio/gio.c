/* -- THIS FILE IS GENERATED - DO NOT EDIT *//* -*- Mode: C; c-basic-offset: 4 -*- */

#define PY_SSIZE_T_CLEAN
#include <Python.h>




#if PY_VERSION_HEX < 0x02050000
typedef int Py_ssize_t;
#define PY_SSIZE_T_MAX INT_MAX
#define PY_SSIZE_T_MIN INT_MIN
typedef inquiry lenfunc;
typedef intargfunc ssizeargfunc;
typedef intobjargproc ssizeobjargproc;
#endif


#line 28 "gio.override"
#define NO_IMPORT_PYGOBJECT
#include <pygobject.h>
#include <gio/gio.h>
#include "pygio-utils.h"
#include "pyglib.h"

#define BUFSIZE 8192

typedef struct {
    gboolean  referenced;
    PyObject *callback;
    PyObject *data;
    gboolean  attach_self;
    gpointer  buffer;
    gsize     buffer_size;
} PyGIONotify;

static GQuark
pygio_notify_get_internal_quark(void)
{
    static GQuark quark = 0;
    if (!quark)
        quark = g_quark_from_string("pygio::notify");
    return quark;
}

static PyGIONotify *
pygio_notify_new(void)
{
    return g_slice_new0(PyGIONotify);
}

static gboolean
pygio_notify_using_optional_callback(PyGIONotify *notify)
{
    if (notify->callback)
        return TRUE;
    else {
        notify->data = NULL;
        return FALSE;
    }
}

static gboolean
pygio_notify_callback_is_valid_full(PyGIONotify *notify, const gchar *name)
{
    if (!notify->callback) {
        PyErr_SetString(PyExc_RuntimeError, "internal error: callback is not set");
        return FALSE;
    }

    if (!PyCallable_Check(notify->callback)) {
        gchar *error_message = g_strdup_printf("%s argument not callable", name);

	PyErr_SetString(PyExc_TypeError, error_message);
        g_free(error_message);
	return FALSE;
    }

    return TRUE;
}

static gboolean
pygio_notify_callback_is_valid(PyGIONotify *notify)
{
    return pygio_notify_callback_is_valid_full(notify, "callback");
}

static void
pygio_notify_reference_callback(PyGIONotify *notify)
{
    if (!notify->referenced) {
        notify->referenced = TRUE;
        Py_XINCREF(notify->callback);
        Py_XINCREF(notify->data);
    }
}

static void
pygio_notify_copy_buffer(PyGIONotify *notify, gpointer buffer, gsize buffer_size)
{
    if (buffer_size > 0) {
	notify->buffer = g_slice_copy(buffer_size, buffer);
	notify->buffer_size = buffer_size;
    }
}

static gboolean
pygio_notify_allocate_buffer(PyGIONotify *notify, gsize buffer_size)
{
    if (buffer_size > 0) {
        notify->buffer = g_slice_alloc(buffer_size);
        if (!notify->buffer) {
            PyErr_Format(PyExc_MemoryError, "failed to allocate %d bytes", buffer_size);
            return FALSE;
        }

        notify->buffer_size = buffer_size;
    }

    return TRUE;
}

static void
pygio_notify_attach_to_result(PyGIONotify *notify)
{
    notify->attach_self = TRUE;
}

static PyGIONotify *
pygio_notify_get_attached(PyGObject *result)
{
    return g_object_get_qdata(G_OBJECT(result->obj), pygio_notify_get_internal_quark());
}

static void
pygio_notify_free(PyGIONotify *notify)
{
    if (notify) {
        if (notify->referenced) {
            Py_XDECREF(notify->callback);
            Py_XDECREF(notify->data);
        }

        if (notify->buffer)
            g_slice_free1(notify->buffer_size, notify->buffer);

        g_slice_free(PyGIONotify, notify);
    }
}

static void
async_result_callback_marshal(GObject *source_object,
			      GAsyncResult *result,
			      PyGIONotify *notify)
{
    PyObject *ret;
    PyGILState_STATE state;

    state = pyg_gil_state_ensure();

    if (!notify->referenced)
        g_warning("pygio_notify_reference_callback() hasn't been called before using the structure");

    if (notify->attach_self) {
        g_object_set_qdata_full(G_OBJECT(result), pygio_notify_get_internal_quark(),
                                notify, (GDestroyNotify) pygio_notify_free);
    }

    if (notify->data)
	ret = PyEval_CallFunction(notify->callback, "NNO",
				  pygobject_new(source_object),
				  pygobject_new((GObject *)result),
				  notify->data);
    else
	ret = PyObject_CallFunction(notify->callback, "NN",
				    pygobject_new(source_object),
				    pygobject_new((GObject *)result));

    if (ret == NULL) {
	PyErr_Print();
	PyErr_Clear();
    }

    Py_XDECREF(ret);

    /* Otherwise the structure is attached to 'result' and will be
     * freed when that object dies. */
    if (!notify->attach_self)
        pygio_notify_free(notify);

    pyg_gil_state_release(state);
}

#line 24 "gfile.override"

static void
file_progress_callback_marshal(goffset current_num_bytes,
			       goffset total_num_bytes,
			       PyGIONotify *notify)
{
    PyObject *ret;
    PyGILState_STATE state;

    state = pyg_gil_state_ensure();

    if (notify->data)
	ret = PyObject_CallFunction(notify->callback, "(KKO)",
				    current_num_bytes,
				    total_num_bytes,
				    notify->data);
    else
	ret = PyObject_CallFunction(notify->callback, "(KK)",
				    current_num_bytes,
				    total_num_bytes);

    if (ret == NULL)
      {
	PyErr_Print();
	PyErr_Clear();
      }

    Py_XDECREF(ret);
    pyg_gil_state_release(state);
}

#line 24 "gfileattribute.override"

extern PyTypeObject PyGFileAttributeInfo_Type;

typedef struct {
    PyObject_HEAD
    const GFileAttributeInfo *info;
} PyGFileAttributeInfo;

static PyObject *
pygio_file_attribute_info_tp_new(PyTypeObject *type)
{
    PyGFileAttributeInfo *self;
    GFileAttributeInfo *info = NULL;

    self = (PyGFileAttributeInfo *) PyObject_NEW(PyGFileAttributeInfo,
                                              &PyGFileAttributeInfo_Type);
    self->info = info;
    return (PyObject *) self;
}

static PyMethodDef pyg_file_attribute_info_methods[] = {
    { NULL,  0, 0 }
};

static PyObject *
pyg_file_attribute_info__get_name(PyObject *self, void *closure)
{
    const gchar *ret;

    ret = ((PyGFileAttributeInfo*)self)->info->name;
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_file_attribute_info__get_type(PyObject *self, void *closure)
{
    gint ret;

    ret = ((PyGFileAttributeInfo*)self)->info->type;
    return pyg_enum_from_gtype(G_TYPE_FILE_ATTRIBUTE_TYPE, ret);
}

static PyObject *
pyg_file_attribute_info__get_flags(PyObject *self, void *closure)
{
    guint ret;

    ret = ((PyGFileAttributeInfo*)self)->info->flags;
    return pyg_flags_from_gtype(G_TYPE_FILE_ATTRIBUTE_INFO_FLAGS, ret);
}

static const PyGetSetDef pyg_file_attribute_info_getsets[] = {
    { "name", (getter)pyg_file_attribute_info__get_name, (setter)0 },
    { "type", (getter)pyg_file_attribute_info__get_type, (setter)0 },
    { "flags", (getter)pyg_file_attribute_info__get_flags, (setter)0 },
    { NULL, (getter)0, (setter)0 },
};

PyTypeObject PyGFileAttributeInfo_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                  /* ob_size */
    "gio.FileAttributeInfo",            /* tp_name */
    sizeof(PyGFileAttributeInfo),      /* tp_basicsize */
    0,                                  /* tp_itemsize */
    /* methods */
    (destructor)0,                      /* tp_dealloc */
    (printfunc)0,                       /* tp_print */
    (getattrfunc)0,                     /* tp_getattr */
    (setattrfunc)0,                     /* tp_setattr */
    (cmpfunc)0,                         /* tp_compare */
    (reprfunc)0,                        /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)0,                        /* tp_hash */
    (ternaryfunc)0,                     /* tp_call */
    (reprfunc)0,                        /* tp_str */
    (getattrofunc)0,                    /* tp_getattro */
    (setattrofunc)0,                    /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "Holds information about an attribute", /* Documentation string */
    (traverseproc)0,                    /* tp_traverse */
    (inquiry)0,                         /* tp_clear */
    (richcmpfunc)0,                     /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    (getiterfunc)0,                     /* tp_iter */
    (iternextfunc)0,                    /* tp_iternext */
    (struct PyMethodDef*)pyg_file_attribute_info_methods,    /* tp_methods */
    0,                                  /* tp_members */
    (struct PyGetSetDef*)pyg_file_attribute_info_getsets,    /* tp_getset */
    (PyTypeObject *)0,                  /* tp_base */
    (PyObject *)0,                      /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)0,                        /* tp_init */
    0,                                  /* tp_alloc */
    (newfunc)pygio_file_attribute_info_tp_new,   /* tp_new */
    0,                                  /* tp_free */
    (inquiry)0,                         /* tp_is_gc */
    (PyObject *)0,                      /* tp_bases */
};

PyObject*
pyg_file_attribute_info_new(const GFileAttributeInfo *info)
{
    PyGFileAttributeInfo *self;

    self = (PyGFileAttributeInfo *)PyObject_NEW(PyGFileAttributeInfo,
                                             &PyGFileAttributeInfo_Type);
    if (G_UNLIKELY(self == NULL))
        return NULL;
    if (info)
        self->info = info;
    return (PyObject *)self;
}


#line 24 "ginputstream.override"
#define BUFSIZE 8192

#line 353 "gio.c"


/* ---------- types from other modules ---------- */
static PyTypeObject *_PyGObject_Type;
#define PyGObject_Type (*_PyGObject_Type)


/* ---------- forward type declarations ---------- */
PyTypeObject G_GNUC_INTERNAL PyGAppLaunchContext_Type;
PyTypeObject G_GNUC_INTERNAL PyGCancellable_Type;
PyTypeObject G_GNUC_INTERNAL PyGFileEnumerator_Type;
PyTypeObject G_GNUC_INTERNAL PyGFileInfo_Type;
PyTypeObject G_GNUC_INTERNAL PyGFileMonitor_Type;
PyTypeObject G_GNUC_INTERNAL PyGInputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGFileInputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGFilterInputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGBufferedInputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGDataInputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGMemoryInputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGMountOperation_Type;
PyTypeObject G_GNUC_INTERNAL PyGOutputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGMemoryOutputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGFilterOutputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGDataOutputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGFileOutputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGSimpleAsyncResult_Type;
PyTypeObject G_GNUC_INTERNAL PyGVfs_Type;
PyTypeObject G_GNUC_INTERNAL PyGVolumeMonitor_Type;
PyTypeObject G_GNUC_INTERNAL PyGNativeVolumeMonitor_Type;
PyTypeObject G_GNUC_INTERNAL PyGFileIcon_Type;
PyTypeObject G_GNUC_INTERNAL PyGThemedIcon_Type;
PyTypeObject G_GNUC_INTERNAL PyGAppInfo_Type;
PyTypeObject G_GNUC_INTERNAL PyGAsyncResult_Type;
PyTypeObject G_GNUC_INTERNAL PyGDrive_Type;
PyTypeObject G_GNUC_INTERNAL PyGFile_Type;
PyTypeObject G_GNUC_INTERNAL PyGIcon_Type;
PyTypeObject G_GNUC_INTERNAL PyGLoadableIcon_Type;
PyTypeObject G_GNUC_INTERNAL PyGMount_Type;
PyTypeObject G_GNUC_INTERNAL PyGSeekable_Type;
PyTypeObject G_GNUC_INTERNAL PyGVolume_Type;

#line 395 "gio.c"



/* ----------- GAppLaunchContext ----------- */

static int
_wrap_g_app_launch_context_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char* kwlist[] = { NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     ":gio.AppLaunchContext.__init__",
                                     kwlist))
        return -1;

    pygobject_constructv(self, 0, NULL);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.AppLaunchContext object");
        return -1;
    }
    return 0;
}

#line 25 "gapplaunchcontext.override"
static PyObject *
_wrap_g_app_launch_context_get_display(PyGObject *self,
                                       PyObject *args,
                                       PyObject *kwargs)
{
    static char *kwlist[] = { "info", "files", NULL };

    GList *file_list = NULL;
    PyGObject *py_info;
    PyObject *pyfile_list;
    gchar *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
			    "O!O:gio.AppLaunchContext.get_display",
			    kwlist,
			    &PyGAppInfo_Type, &py_info, &pyfile_list))
        return NULL;

    if (!PySequence_Check (pyfile_list)) {
        PyErr_Format (PyExc_TypeError,
                      "argument must be a list or tuple of GFile objects");
        return NULL;
    }

    file_list = pygio_pylist_to_gfile_glist(pyfile_list);

    ret = g_app_launch_context_get_display(G_APP_LAUNCH_CONTEXT(self->obj),
                                           G_APP_INFO(py_info->obj), file_list);
    g_list_free(file_list);

    if (ret)
        return PyString_FromString(ret);

    Py_INCREF(Py_None);
    return Py_None;
}
#line 458 "gio.c"


#line 63 "gapplaunchcontext.override"
static PyObject *
_wrap_g_app_launch_context_get_startup_notify_id(PyGObject *self,
                                                 PyObject *args,
                                                 PyObject *kwargs)
{
    static char *kwlist[] = { "info", "files", NULL };

    GList       *file_list = NULL;
    PyGObject   *py_info;
    PyObject    *pyfile_list;
    gchar       *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
			    "O!O:gio.AppLaunchContext.get_startup_notify_id",
			    kwlist,
			    &PyGAppInfo_Type, &py_info, &pyfile_list))
        return NULL;

    if (!PySequence_Check (pyfile_list)) {
        PyErr_Format (PyExc_TypeError,
                      "argument must be a list or tuple of GFile objects");
        return NULL;
    }

    file_list = pygio_pylist_to_gfile_glist(pyfile_list);

    ret = g_app_launch_context_get_startup_notify_id(
                                        G_APP_LAUNCH_CONTEXT(self->obj),
                                        G_APP_INFO(py_info->obj), file_list);
    g_list_free(file_list);

    if (ret)
        return PyString_FromString(ret);

    Py_INCREF(Py_None);
    return Py_None;
}
#line 499 "gio.c"


static PyObject *
_wrap_g_app_launch_context_launch_failed(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "startup_notify_id", NULL };
    char *startup_notify_id;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GAppLaunchContext.launch_failed", kwlist, &startup_notify_id))
        return NULL;
    
    g_app_launch_context_launch_failed(G_APP_LAUNCH_CONTEXT(self->obj), startup_notify_id);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGAppLaunchContext_methods[] = {
    { "get_display", (PyCFunction)_wrap_g_app_launch_context_get_display, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_startup_notify_id", (PyCFunction)_wrap_g_app_launch_context_get_startup_notify_id, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "launch_failed", (PyCFunction)_wrap_g_app_launch_context_launch_failed, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGAppLaunchContext_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.AppLaunchContext",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGAppLaunchContext_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_app_launch_context_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GCancellable ----------- */

static int
_wrap_g_cancellable_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char* kwlist[] = { NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     ":gio.Cancellable.__init__",
                                     kwlist))
        return -1;

    pygobject_constructv(self, 0, NULL);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.Cancellable object");
        return -1;
    }
    return 0;
}

static PyObject *
_wrap_g_cancellable_is_cancelled(PyGObject *self)
{
    int ret;

    
    ret = g_cancellable_is_cancelled(G_CANCELLABLE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_cancellable_set_error_if_cancelled(PyGObject *self)
{
    int ret;
    GError *error = NULL;

    
    ret = g_cancellable_set_error_if_cancelled(G_CANCELLABLE(self->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_cancellable_get_fd(PyGObject *self)
{
    int ret;

    
    ret = g_cancellable_get_fd(G_CANCELLABLE(self->obj));
    
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_cancellable_push_current(PyGObject *self)
{
    
    g_cancellable_push_current(G_CANCELLABLE(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_cancellable_pop_current(PyGObject *self)
{
    
    g_cancellable_pop_current(G_CANCELLABLE(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_cancellable_reset(PyGObject *self)
{
    
    g_cancellable_reset(G_CANCELLABLE(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_cancellable_cancel(PyGObject *self)
{
    
    g_cancellable_cancel(G_CANCELLABLE(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGCancellable_methods[] = {
    { "is_cancelled", (PyCFunction)_wrap_g_cancellable_is_cancelled, METH_NOARGS,
      NULL },
    { "set_error_if_cancelled", (PyCFunction)_wrap_g_cancellable_set_error_if_cancelled, METH_NOARGS,
      NULL },
    { "get_fd", (PyCFunction)_wrap_g_cancellable_get_fd, METH_NOARGS,
      NULL },
    { "push_current", (PyCFunction)_wrap_g_cancellable_push_current, METH_NOARGS,
      NULL },
    { "pop_current", (PyCFunction)_wrap_g_cancellable_pop_current, METH_NOARGS,
      NULL },
    { "reset", (PyCFunction)_wrap_g_cancellable_reset, METH_NOARGS,
      NULL },
    { "cancel", (PyCFunction)_wrap_g_cancellable_cancel, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGCancellable_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.Cancellable",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGCancellable_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_cancellable_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFileEnumerator ----------- */

static PyObject *
_wrap_g_file_enumerator_next_file(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    PyObject *py_ret;
    GCancellable *cancellable = NULL;
    GFileInfo *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFileEnumerator.next_file", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_enumerator_next_file(G_FILE_ENUMERATOR(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_enumerator_close(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFileEnumerator.close", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_enumerator_close(G_FILE_ENUMERATOR(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 59 "gfileenumerator.override"
static PyObject *
_wrap_g_file_enumerator_next_files_async(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "num_files", "callback",
			      "io_priority", "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    int num_files;
    int io_priority = G_PRIORITY_DEFAULT;
    GCancellable *cancellable = NULL;
    PyGObject *py_cancellable = NULL;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "iO|iOO:GFileEnumerator.enumerate_children_async",
				     kwlist,
				     &num_files,
				     &notify->callback,
				     &io_priority,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
	goto error;

    pygio_notify_reference_callback(notify);  
    
    g_file_enumerator_next_files_async(G_FILE_ENUMERATOR(self->obj),
				       num_files,
				       io_priority,
				       (GCancellable *) cancellable,
				       (GAsyncReadyCallback)async_result_callback_marshal,
				       notify);
    
    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 846 "gio.c"


#line 106 "gfileenumerator.override"
static PyObject *
_wrap_g_file_enumerator_next_files_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GList *next_files, *l;
    GError *error = NULL;
    PyObject *ret;
    
    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "O!:GFileEnumerator.next_files_finish",
				     kwlist,
				     &PyGAsyncResult_Type, &result))
        return NULL;
    
    next_files = g_file_enumerator_next_files_finish(G_FILE_ENUMERATOR(self->obj),
						     G_ASYNC_RESULT(result->obj),
						     &error);
    if (pyg_error_check(&error))
        return NULL;

    ret = PyList_New(0);
    for (l = next_files; l; l = l->next) {
	GFileInfo *file_info = l->data;
	PyObject *item = pygobject_new((GObject *)file_info);
	PyList_Append(ret, item);
	Py_DECREF(item);
	g_object_unref(file_info);
    }
    g_list_free(next_files);

    return ret;
}
#line 883 "gio.c"


static PyObject *
_wrap_g_file_enumerator_close_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFileEnumerator.close_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_file_enumerator_close_finish(G_FILE_ENUMERATOR(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_enumerator_is_closed(PyGObject *self)
{
    int ret;

    
    ret = g_file_enumerator_is_closed(G_FILE_ENUMERATOR(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_enumerator_has_pending(PyGObject *self)
{
    int ret;

    
    ret = g_file_enumerator_has_pending(G_FILE_ENUMERATOR(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_enumerator_set_pending(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "pending", NULL };
    int pending;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GFileEnumerator.set_pending", kwlist, &pending))
        return NULL;
    
    g_file_enumerator_set_pending(G_FILE_ENUMERATOR(self->obj), pending);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGFileEnumerator_methods[] = {
    { "next_file", (PyCFunction)_wrap_g_file_enumerator_next_file, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "close", (PyCFunction)_wrap_g_file_enumerator_close, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "next_files_async", (PyCFunction)_wrap_g_file_enumerator_next_files_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "FE.next_files_async(num_files, callback, [io_priority, cancellable,\n"
"                    user_data])\n"
"Request information for a number of files from the enumerator\n"
"asynchronously. When all i/o for the operation is finished the callback\n"
"will be called with the requested information.\n"
"\n"
"The callback can be called with less than num_files files in case of error\n"
"or at the end of the enumerator. In case of a partial error the callback\n"
"will be called with any succeeding items and no error, and on the next\n"
"request the error will be reported. If a request is cancelled the callback\n"
"will be called with gio.ERROR_CANCELLED.\n"
"\n"
"During an async request no other sync and async calls are allowed, and will\n"
"result in gio.ERROR_PENDING errors.\n"
"\n"
"Any outstanding i/o request with higher priority (lower numerical value)\n"
"will be executed before an outstanding request with lower priority.\n"
"Default priority is gobject.PRIORITY_DEFAULT." },
    { "next_files_finish", (PyCFunction)_wrap_g_file_enumerator_next_files_finish, METH_VARARGS|METH_KEYWORDS,
      (char *) "FE.next_files_finish(result) -> a list of gio.FileInfos\n"
"Finishes the asynchronous operation started with\n"
"gio.FileEnumerator.next_files_async()." },
    { "close_finish", (PyCFunction)_wrap_g_file_enumerator_close_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "is_closed", (PyCFunction)_wrap_g_file_enumerator_is_closed, METH_NOARGS,
      NULL },
    { "has_pending", (PyCFunction)_wrap_g_file_enumerator_has_pending, METH_NOARGS,
      NULL },
    { "set_pending", (PyCFunction)_wrap_g_file_enumerator_set_pending, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 24 "gfileenumerator.override"
static PyObject*
_wrap_g_file_enumerator_tp_iter(PyGObject *self)
{
    Py_INCREF (self);
    return (PyObject *) self;
}
#line 990 "gio.c"


#line 32 "gfileenumerator.override"
static PyObject*
_wrap_g_file_enumerator_tp_iternext(PyGObject *iter)
{
    GFileInfo *file_info;
    GError *error = NULL;

    if (!iter->obj) {
	PyErr_SetNone(PyExc_StopIteration);
	return NULL;
    }

    file_info = g_file_enumerator_next_file(G_FILE_ENUMERATOR(iter->obj),
					    NULL,
					    &error);
    if (pyg_error_check(&error)) {
        return NULL;
    }
    
    if (!file_info) {
	PyErr_SetNone(PyExc_StopIteration);
	return NULL;
    }

    return pygobject_new((GObject*)file_info);
}
#line 1019 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGFileEnumerator_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FileEnumerator",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)_wrap_g_file_enumerator_tp_iter,          /* tp_iter */
    (iternextfunc)_wrap_g_file_enumerator_tp_iternext,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFileEnumerator_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFileInfo ----------- */

static int
_wrap_g_file_info_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char* kwlist[] = { NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     ":gio.FileInfo.__init__",
                                     kwlist))
        return -1;

    pygobject_constructv(self, 0, NULL);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.FileInfo object");
        return -1;
    }
    return 0;
}

static PyObject *
_wrap_g_file_info_dup(PyGObject *self)
{
    PyObject *py_ret;
    GFileInfo *ret;

    
    ret = g_file_info_dup(G_FILE_INFO(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_info_copy_into(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "dest_info", NULL };
    PyGObject *dest_info;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFileInfo.copy_into", kwlist, &PyGFileInfo_Type, &dest_info))
        return NULL;
    
    g_file_info_copy_into(G_FILE_INFO(self->obj), G_FILE_INFO(dest_info->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_has_attribute(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.has_attribute", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_has_attribute(G_FILE_INFO(self->obj), attribute);
    
    return PyBool_FromLong(ret);

}

#line 24 "gfileinfo.override"
static PyObject *
_wrap_g_file_info_list_attributes(PyGObject *self, 
                                  PyObject  *args, 
				  PyObject  *kwargs)
{
    char *kwlist[] = { "name_space", NULL};
    gchar *name_space;
    gchar **names;
    gchar **n;
    PyObject *ret;
    
    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "s:gio.FileInfo.list_attributes",
				     kwlist, &name_space))
	return NULL;

    names = g_file_info_list_attributes(G_FILE_INFO(self->obj),
					name_space);

    ret = PyList_New(0);
    n = names;
    while (n && *n) {
        PyObject *item = PyString_FromString(n[0]);
        PyList_Append(ret, item);
        Py_DECREF(item);

        n++;
    }
    
    g_strfreev(names);
    return ret;
}
#line 1170 "gio.c"


static PyObject *
_wrap_g_file_info_get_attribute_type(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    gint ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_type", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_type(G_FILE_INFO(self->obj), attribute);
    
    return pyg_enum_from_gtype(G_TYPE_FILE_ATTRIBUTE_TYPE, ret);
}

static PyObject *
_wrap_g_file_info_remove_attribute(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.remove_attribute", kwlist, &attribute))
        return NULL;
    
    g_file_info_remove_attribute(G_FILE_INFO(self->obj), attribute);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_attribute_status(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    gint ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_status", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_status(G_FILE_INFO(self->obj), attribute);
    
    return pyg_enum_from_gtype(G_TYPE_FILE_ATTRIBUTE_STATUS, ret);
}

static PyObject *
_wrap_g_file_info_get_attribute_as_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    gchar *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_as_string", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_as_string(G_FILE_INFO(self->obj), attribute);
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_attribute_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    const gchar *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_string", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_string(G_FILE_INFO(self->obj), attribute);
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_attribute_byte_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    const gchar *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_byte_string", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_byte_string(G_FILE_INFO(self->obj), attribute);
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_attribute_boolean(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_boolean", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_boolean(G_FILE_INFO(self->obj), attribute);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_info_get_attribute_uint32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    guint32 ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_uint32", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_uint32(G_FILE_INFO(self->obj), attribute);
    
    return PyLong_FromUnsignedLong(ret);

}

static PyObject *
_wrap_g_file_info_get_attribute_int32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_int32", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_int32(G_FILE_INFO(self->obj), attribute);
    
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_file_info_get_attribute_uint64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    guint64 ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_uint64", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_uint64(G_FILE_INFO(self->obj), attribute);
    
    return PyLong_FromUnsignedLongLong(ret);
}

static PyObject *
_wrap_g_file_info_get_attribute_int64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    gint64 ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_int64", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_int64(G_FILE_INFO(self->obj), attribute);
    
    return PyLong_FromLongLong(ret);
}

static PyObject *
_wrap_g_file_info_get_attribute_object(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", NULL };
    char *attribute;
    GObject *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.get_attribute_object", kwlist, &attribute))
        return NULL;
    
    ret = g_file_info_get_attribute_object(G_FILE_INFO(self->obj), attribute);
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_file_info_set_attribute_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute, *attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ss:GFileInfo.set_attribute_string", kwlist, &attribute, &attr_value))
        return NULL;
    
    g_file_info_set_attribute_string(G_FILE_INFO(self->obj), attribute, attr_value);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_attribute_byte_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute, *attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ss:GFileInfo.set_attribute_byte_string", kwlist, &attribute, &attr_value))
        return NULL;
    
    g_file_info_set_attribute_byte_string(G_FILE_INFO(self->obj), attribute, attr_value);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_attribute_boolean(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute;
    int attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"si:GFileInfo.set_attribute_boolean", kwlist, &attribute, &attr_value))
        return NULL;
    
    g_file_info_set_attribute_boolean(G_FILE_INFO(self->obj), attribute, attr_value);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_attribute_uint32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute;
    unsigned long attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sk:GFileInfo.set_attribute_uint32", kwlist, &attribute, &attr_value))
        return NULL;
    
    g_file_info_set_attribute_uint32(G_FILE_INFO(self->obj), attribute, attr_value);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_attribute_int32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute;
    int attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"si:GFileInfo.set_attribute_int32", kwlist, &attribute, &attr_value))
        return NULL;
    
    g_file_info_set_attribute_int32(G_FILE_INFO(self->obj), attribute, attr_value);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_attribute_uint64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute;
    PyObject *py_attr_value = NULL;
    guint64 attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sO!:GFileInfo.set_attribute_uint64", kwlist, &attribute, &PyLong_Type, &py_attr_value))
        return NULL;
    attr_value = PyLong_AsUnsignedLongLong(py_attr_value);
    
    g_file_info_set_attribute_uint64(G_FILE_INFO(self->obj), attribute, attr_value);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_attribute_int64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute;
    gint64 attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sL:GFileInfo.set_attribute_int64", kwlist, &attribute, &attr_value))
        return NULL;
    
    g_file_info_set_attribute_int64(G_FILE_INFO(self->obj), attribute, attr_value);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_attribute_object(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "attr_value", NULL };
    char *attribute;
    PyGObject *attr_value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sO!:GFileInfo.set_attribute_object", kwlist, &attribute, &PyGObject_Type, &attr_value))
        return NULL;
    
    g_file_info_set_attribute_object(G_FILE_INFO(self->obj), attribute, G_OBJECT(attr_value->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_clear_status(PyGObject *self)
{
    
    g_file_info_clear_status(G_FILE_INFO(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_file_type(PyGObject *self)
{
    gint ret;

    
    ret = g_file_info_get_file_type(G_FILE_INFO(self->obj));
    
    return pyg_enum_from_gtype(G_TYPE_FILE_TYPE, ret);
}

static PyObject *
_wrap_g_file_info_get_is_hidden(PyGObject *self)
{
    int ret;

    
    ret = g_file_info_get_is_hidden(G_FILE_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_info_get_is_backup(PyGObject *self)
{
    int ret;

    
    ret = g_file_info_get_is_backup(G_FILE_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_info_get_is_symlink(PyGObject *self)
{
    int ret;

    
    ret = g_file_info_get_is_symlink(G_FILE_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_info_get_name(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_file_info_get_name(G_FILE_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_display_name(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_file_info_get_display_name(G_FILE_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_edit_name(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_file_info_get_edit_name(G_FILE_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_icon(PyGObject *self)
{
    GIcon *ret;

    
    ret = g_file_info_get_icon(G_FILE_INFO(self->obj));
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_file_info_get_content_type(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_file_info_get_content_type(G_FILE_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_size(PyGObject *self)
{
    gint64 ret;

    
    ret = g_file_info_get_size(G_FILE_INFO(self->obj));
    
    return PyLong_FromLongLong(ret);
}

#line 58 "gfileinfo.override"
static PyObject *
_wrap_g_file_info_get_modification_time(PyGObject *self, PyObject *unused)
{
    GTimeVal timeval;

    g_file_info_get_modification_time(G_FILE_INFO(self->obj), &timeval);
    return pyglib_float_from_timeval(timeval);
}

/* GFileInfo.get_attribute_data: No ArgType for GFileAttributeType* */
/* GFileInfo.set_attribute: No ArgType for gpointer */
/* GFileInfo.set_attribute_mask: No ArgType for GFileAttributeMatcher* */
/* GFileInfo.set_modification_time: No ArgType for GTimeVal* */
#line 1646 "gio.c"


static PyObject *
_wrap_g_file_info_get_symlink_target(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_file_info_get_symlink_target(G_FILE_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_etag(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_file_info_get_etag(G_FILE_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_get_sort_order(PyGObject *self)
{
    int ret;

    
    ret = g_file_info_get_sort_order(G_FILE_INFO(self->obj));
    
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_file_info_unset_attribute_mask(PyGObject *self)
{
    
    g_file_info_unset_attribute_mask(G_FILE_INFO(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_file_type(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", NULL };
    GFileType type;
    PyObject *py_type = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:GFileInfo.set_file_type", kwlist, &py_type))
        return NULL;
    if (pyg_enum_get_value(G_TYPE_FILE_TYPE, py_type, (gpointer)&type))
        return NULL;
    
    g_file_info_set_file_type(G_FILE_INFO(self->obj), type);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_is_hidden(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "is_hidden", NULL };
    int is_hidden;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GFileInfo.set_is_hidden", kwlist, &is_hidden))
        return NULL;
    
    g_file_info_set_is_hidden(G_FILE_INFO(self->obj), is_hidden);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_is_symlink(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "is_symlink", NULL };
    int is_symlink;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GFileInfo.set_is_symlink", kwlist, &is_symlink))
        return NULL;
    
    g_file_info_set_is_symlink(G_FILE_INFO(self->obj), is_symlink);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_name(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "name", NULL };
    char *name;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.set_name", kwlist, &name))
        return NULL;
    
    g_file_info_set_name(G_FILE_INFO(self->obj), name);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_display_name(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "display_name", NULL };
    char *display_name;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.set_display_name", kwlist, &display_name))
        return NULL;
    
    g_file_info_set_display_name(G_FILE_INFO(self->obj), display_name);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_edit_name(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "edit_name", NULL };
    char *edit_name;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.set_edit_name", kwlist, &edit_name))
        return NULL;
    
    g_file_info_set_edit_name(G_FILE_INFO(self->obj), edit_name);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_icon(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "icon", NULL };
    PyGObject *icon;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFileInfo.set_icon", kwlist, &PyGIcon_Type, &icon))
        return NULL;
    
    g_file_info_set_icon(G_FILE_INFO(self->obj), G_ICON(icon->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_content_type(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "content_type", NULL };
    char *content_type;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.set_content_type", kwlist, &content_type))
        return NULL;
    
    g_file_info_set_content_type(G_FILE_INFO(self->obj), content_type);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_size(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "size", NULL };
    gint64 size;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"L:GFileInfo.set_size", kwlist, &size))
        return NULL;
    
    g_file_info_set_size(G_FILE_INFO(self->obj), size);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_symlink_target(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "symlink_target", NULL };
    char *symlink_target;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFileInfo.set_symlink_target", kwlist, &symlink_target))
        return NULL;
    
    g_file_info_set_symlink_target(G_FILE_INFO(self->obj), symlink_target);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_info_set_sort_order(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "sort_order", NULL };
    int sort_order;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GFileInfo.set_sort_order", kwlist, &sort_order))
        return NULL;
    
    g_file_info_set_sort_order(G_FILE_INFO(self->obj), sort_order);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGFileInfo_methods[] = {
    { "dup", (PyCFunction)_wrap_g_file_info_dup, METH_NOARGS,
      NULL },
    { "copy_into", (PyCFunction)_wrap_g_file_info_copy_into, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "has_attribute", (PyCFunction)_wrap_g_file_info_has_attribute, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "list_attributes", (PyCFunction)_wrap_g_file_info_list_attributes, METH_VARARGS|METH_KEYWORDS,
      (char *) "INFO.list_attributes(name_space) -> Attribute list\n\n"
"Lists the file info structure's attributes." },
    { "get_attribute_type", (PyCFunction)_wrap_g_file_info_get_attribute_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "remove_attribute", (PyCFunction)_wrap_g_file_info_remove_attribute, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_status", (PyCFunction)_wrap_g_file_info_get_attribute_status, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_as_string", (PyCFunction)_wrap_g_file_info_get_attribute_as_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_string", (PyCFunction)_wrap_g_file_info_get_attribute_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_byte_string", (PyCFunction)_wrap_g_file_info_get_attribute_byte_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_boolean", (PyCFunction)_wrap_g_file_info_get_attribute_boolean, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_uint32", (PyCFunction)_wrap_g_file_info_get_attribute_uint32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_int32", (PyCFunction)_wrap_g_file_info_get_attribute_int32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_uint64", (PyCFunction)_wrap_g_file_info_get_attribute_uint64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_int64", (PyCFunction)_wrap_g_file_info_get_attribute_int64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_attribute_object", (PyCFunction)_wrap_g_file_info_get_attribute_object, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_string", (PyCFunction)_wrap_g_file_info_set_attribute_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_byte_string", (PyCFunction)_wrap_g_file_info_set_attribute_byte_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_boolean", (PyCFunction)_wrap_g_file_info_set_attribute_boolean, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_uint32", (PyCFunction)_wrap_g_file_info_set_attribute_uint32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_int32", (PyCFunction)_wrap_g_file_info_set_attribute_int32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_uint64", (PyCFunction)_wrap_g_file_info_set_attribute_uint64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_int64", (PyCFunction)_wrap_g_file_info_set_attribute_int64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_object", (PyCFunction)_wrap_g_file_info_set_attribute_object, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "clear_status", (PyCFunction)_wrap_g_file_info_clear_status, METH_NOARGS,
      NULL },
    { "get_file_type", (PyCFunction)_wrap_g_file_info_get_file_type, METH_NOARGS,
      NULL },
    { "get_is_hidden", (PyCFunction)_wrap_g_file_info_get_is_hidden, METH_NOARGS,
      NULL },
    { "get_is_backup", (PyCFunction)_wrap_g_file_info_get_is_backup, METH_NOARGS,
      NULL },
    { "get_is_symlink", (PyCFunction)_wrap_g_file_info_get_is_symlink, METH_NOARGS,
      NULL },
    { "get_name", (PyCFunction)_wrap_g_file_info_get_name, METH_NOARGS,
      NULL },
    { "get_display_name", (PyCFunction)_wrap_g_file_info_get_display_name, METH_NOARGS,
      NULL },
    { "get_edit_name", (PyCFunction)_wrap_g_file_info_get_edit_name, METH_NOARGS,
      NULL },
    { "get_icon", (PyCFunction)_wrap_g_file_info_get_icon, METH_NOARGS,
      NULL },
    { "get_content_type", (PyCFunction)_wrap_g_file_info_get_content_type, METH_NOARGS,
      NULL },
    { "get_size", (PyCFunction)_wrap_g_file_info_get_size, METH_NOARGS,
      NULL },
    { "get_modification_time", (PyCFunction)_wrap_g_file_info_get_modification_time, METH_NOARGS,
      (char *) "INFO.get_modification_time() -> modification time\n"
"Returns the modification time, in UNIX time format\n" },
    { "get_symlink_target", (PyCFunction)_wrap_g_file_info_get_symlink_target, METH_NOARGS,
      NULL },
    { "get_etag", (PyCFunction)_wrap_g_file_info_get_etag, METH_NOARGS,
      NULL },
    { "get_sort_order", (PyCFunction)_wrap_g_file_info_get_sort_order, METH_NOARGS,
      NULL },
    { "unset_attribute_mask", (PyCFunction)_wrap_g_file_info_unset_attribute_mask, METH_NOARGS,
      NULL },
    { "set_file_type", (PyCFunction)_wrap_g_file_info_set_file_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_is_hidden", (PyCFunction)_wrap_g_file_info_set_is_hidden, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_is_symlink", (PyCFunction)_wrap_g_file_info_set_is_symlink, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_name", (PyCFunction)_wrap_g_file_info_set_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_display_name", (PyCFunction)_wrap_g_file_info_set_display_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_edit_name", (PyCFunction)_wrap_g_file_info_set_edit_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_icon", (PyCFunction)_wrap_g_file_info_set_icon, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_content_type", (PyCFunction)_wrap_g_file_info_set_content_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_size", (PyCFunction)_wrap_g_file_info_set_size, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_symlink_target", (PyCFunction)_wrap_g_file_info_set_symlink_target, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_sort_order", (PyCFunction)_wrap_g_file_info_set_sort_order, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGFileInfo_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FileInfo",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFileInfo_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_file_info_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFileMonitor ----------- */

static PyObject *
_wrap_g_file_monitor_cancel(PyGObject *self)
{
    int ret;

    
    ret = g_file_monitor_cancel(G_FILE_MONITOR(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_monitor_is_cancelled(PyGObject *self)
{
    int ret;

    
    ret = g_file_monitor_is_cancelled(G_FILE_MONITOR(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_monitor_set_rate_limit(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "limit_msecs", NULL };
    int limit_msecs;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GFileMonitor.set_rate_limit", kwlist, &limit_msecs))
        return NULL;
    
    g_file_monitor_set_rate_limit(G_FILE_MONITOR(self->obj), limit_msecs);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_monitor_emit_event(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "file", "other_file", "event_type", NULL };
    PyGObject *file, *other_file;
    PyObject *py_event_type = NULL;
    GFileMonitorEvent event_type;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!O!O:GFileMonitor.emit_event", kwlist, &PyGFile_Type, &file, &PyGFile_Type, &other_file, &py_event_type))
        return NULL;
    if (pyg_enum_get_value(G_TYPE_FILE_MONITOR_EVENT, py_event_type, (gpointer)&event_type))
        return NULL;
    
    g_file_monitor_emit_event(G_FILE_MONITOR(self->obj), G_FILE(file->obj), G_FILE(other_file->obj), event_type);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGFileMonitor_methods[] = {
    { "cancel", (PyCFunction)_wrap_g_file_monitor_cancel, METH_NOARGS,
      NULL },
    { "is_cancelled", (PyCFunction)_wrap_g_file_monitor_is_cancelled, METH_NOARGS,
      NULL },
    { "set_rate_limit", (PyCFunction)_wrap_g_file_monitor_set_rate_limit, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "emit_event", (PyCFunction)_wrap_g_file_monitor_emit_event, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGFileMonitor_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FileMonitor",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFileMonitor_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GInputStream ----------- */

#line 28 "ginputstream.override"
static PyObject *
_wrap_g_input_stream_read(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "count", "cancellable", NULL };
    PyGObject *pycancellable = NULL;
    PyObject *v;
    GCancellable *cancellable;
    long count = -1;
    GError *error = NULL;
    size_t bytesread, buffersize, chunksize;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "|lO:InputStream.read",
                                     kwlist, &count,
                                     &pycancellable))
        return NULL;

    buffersize = (count < 0 ? BUFSIZE : count);

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        return NULL;

    v = PyString_FromStringAndSize((char *)NULL, buffersize);
    if (v == NULL)
        return NULL;

    bytesread = 0;
    for (;;)
        {
            pyg_begin_allow_threads;
            errno = 0;
            chunksize = g_input_stream_read(G_INPUT_STREAM(self->obj),
                                            PyString_AS_STRING((PyStringObject *)v) + bytesread,
                                            buffersize - bytesread, cancellable,
                                            &error);
            pyg_end_allow_threads;

            if (pyg_error_check(&error)) {
		Py_DECREF(v);
		return NULL;
	    }
	    if (chunksize == 0) {
		/* End of file. */
                break;
	    }

            bytesread += chunksize;
            if (bytesread < buffersize) {
		/* g_input_stream_read() decided to not read full buffer.  We
		 * then return early too, even if 'count' is less than 0.
		 */
                break;
	    }

            if (count < 0) {
		buffersize += BUFSIZE;
		if (_PyString_Resize(&v, buffersize) < 0)
		    return NULL;
	    }
            else {
                /* Got what was requested. */
                break;
	    }
        }

    if (bytesread != buffersize)
        _PyString_Resize(&v, bytesread);

    return v;
}
#line 2213 "gio.c"


#line 100 "ginputstream.override"
static PyObject *
_wrap_g_input_stream_read_all(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "count", "cancellable", NULL };
    PyGObject *pycancellable = NULL;
    PyObject *v;
    GCancellable *cancellable;
    long count = -1;
    GError *error = NULL;
    size_t bytesread, buffersize, chunksize;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "|lO:InputStream.read",
                                     kwlist, &count,
                                     &pycancellable))
        return NULL;

    buffersize = (count < 0 ? BUFSIZE : count);

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        return NULL;

    v = PyString_FromStringAndSize((char *)NULL, buffersize);
    if (v == NULL)
        return NULL;

    bytesread = 0;
    for (;;)
        {
            pyg_begin_allow_threads;
            errno = 0;
            g_input_stream_read_all(G_INPUT_STREAM(self->obj),
				    PyString_AS_STRING((PyStringObject *)v) + bytesread,
				    buffersize - bytesread,
				    &chunksize,
				    cancellable, &error);
            pyg_end_allow_threads;

            if (pyg_error_check(&error)) {
		Py_DECREF(v);
		return NULL;
	    }

            bytesread += chunksize;
            if (bytesread < buffersize || chunksize == 0) {
		/* End of file. */
                break;
	    }

            if (count < 0) {
		buffersize += BUFSIZE;
		if (_PyString_Resize(&v, buffersize) < 0)
		    return NULL;
	    }
            else {
                /* Got what was requested. */
                break;
	    }
        }

    if (bytesread != buffersize)
        _PyString_Resize(&v, bytesread);

    return v;
}
#line 2282 "gio.c"


static PyObject *
_wrap_g_input_stream_skip(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "count", "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    gsize count;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    gssize ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"k|O:GInputStream.skip", kwlist, &count, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_input_stream_skip(G_INPUT_STREAM(self->obj), count, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);

}

static PyObject *
_wrap_g_input_stream_close(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GInputStream.close", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_input_stream_close(G_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 167 "ginputstream.override"
static PyObject *
_wrap_g_input_stream_read_async(PyGObject *self,
                                PyObject *args,
                                PyObject *kwargs)
{
    static char *kwlist[] = { "count", "callback", "io_priority",
                              "cancellable", "user_data", NULL };
    long count = -1;
    int io_priority = G_PRIORITY_DEFAULT;
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "lO|iOO:InputStream.read_async",
                                     kwlist,
                                     &count,
                                     &notify->callback,
                                     &io_priority,
                                     &pycancellable,
                                     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    if (!pygio_notify_allocate_buffer(notify, count))
        goto error;

    pygio_notify_reference_callback(notify);
    pygio_notify_attach_to_result(notify);

    g_input_stream_read_async(G_INPUT_STREAM(self->obj),
                              notify->buffer,
                              notify->buffer_size,
                              io_priority,
                              cancellable,
                              (GAsyncReadyCallback) async_result_callback_marshal,
                              notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 2395 "gio.c"


#line 221 "ginputstream.override"
static PyObject *
_wrap_g_input_stream_read_finish(PyGObject *self,
                                 PyObject *args,
                                 PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GError *error = NULL;
    Py_ssize_t bytesread;
    PyGIONotify *notify;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!:GInputStream.read_finish",
                                     kwlist, &PyGAsyncResult_Type, &result))
        return NULL;

    bytesread = g_input_stream_read_finish(G_INPUT_STREAM(self->obj),
                                           G_ASYNC_RESULT(result->obj), &error);

    if (pyg_error_check(&error))
        return NULL;

    if (bytesread == 0)
        return PyString_FromString("");

    notify = pygio_notify_get_attached(result);
    return PyString_FromStringAndSize(notify->buffer, bytesread);
}
#line 2427 "gio.c"


static PyObject *
_wrap_g_input_stream_skip_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GError *error = NULL;
    gssize ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GInputStream.skip_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_input_stream_skip_finish(G_INPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);

}

#line 251 "ginputstream.override"
static PyObject *
_wrap_g_input_stream_close_async(PyGObject *self,
                                 PyObject *args,
                                 PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "io_priority", "cancellable",
                              "user_data", NULL };
    int io_priority = G_PRIORITY_DEFAULT;
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|iOO:InputStream.close_async",
                                     kwlist,
                                     &notify->callback,
                                     &io_priority,
                                     &pycancellable,
                                     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_input_stream_close_async(G_INPUT_STREAM(self->obj),
                               io_priority,
                               cancellable,
                               (GAsyncReadyCallback)async_result_callback_marshal,
                               notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 2494 "gio.c"


static PyObject *
_wrap_g_input_stream_close_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GInputStream.close_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_input_stream_close_finish(G_INPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_input_stream_is_closed(PyGObject *self)
{
    int ret;

    
    ret = g_input_stream_is_closed(G_INPUT_STREAM(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_input_stream_has_pending(PyGObject *self)
{
    int ret;

    
    ret = g_input_stream_has_pending(G_INPUT_STREAM(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_input_stream_set_pending(PyGObject *self)
{
    int ret;
    GError *error = NULL;

    
    ret = g_input_stream_set_pending(G_INPUT_STREAM(self->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_input_stream_clear_pending(PyGObject *self)
{
    
    g_input_stream_clear_pending(G_INPUT_STREAM(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGInputStream_methods[] = {
    { "read_part", (PyCFunction)_wrap_g_input_stream_read, METH_VARARGS|METH_KEYWORDS,
      (char *) "STREAM.read_part([count, [cancellable]]) -> string\n"
"\n"
"Read 'count' bytes from the stream. If 'count' is not specified or is\n"
"omitted, read until the end of the stream. This method is allowed to\n"
"stop at any time after reading at least 1 byte from the stream. E.g.\n"
"when reading over a (relatively slow) HTTP connection, it will often\n"
"stop after receiving one packet. Therefore, to reliably read requested\n"
"number of bytes, you need to use a loop. See also gio.InputStream.read\n"
"for easier to use (though less efficient) method.\n"
"\n"
"Note: this method roughly corresponds to C GIO g_input_stream_read." },
    { "read", (PyCFunction)_wrap_g_input_stream_read_all, METH_VARARGS|METH_KEYWORDS,
      (char *) "STREAM.read([count, [cancellable]]) -> string\n"
"\n"
"Read 'count' bytes from the stream. If 'count' is not specified or is\n"
"omitted, read until the end of the stream. This method will stop only\n"
"after reading requested number of bytes, reaching end of stream or\n"
"triggering an I/O error. See also gio.InputStream.read_part for more\n"
"efficient, but more cumbersome to use method.\n"
"\n"
"Note: this method roughly corresponds to C GIO g_input_stream_read_all.\n"
"It was renamed for consistency with Python standard file.read." },
    { "skip", (PyCFunction)_wrap_g_input_stream_skip, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "close", (PyCFunction)_wrap_g_input_stream_close, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_async", (PyCFunction)_wrap_g_input_stream_read_async, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_finish", (PyCFunction)_wrap_g_input_stream_read_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "skip_finish", (PyCFunction)_wrap_g_input_stream_skip_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "close_async", (PyCFunction)_wrap_g_input_stream_close_async, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "close_finish", (PyCFunction)_wrap_g_input_stream_close_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "is_closed", (PyCFunction)_wrap_g_input_stream_is_closed, METH_NOARGS,
      NULL },
    { "has_pending", (PyCFunction)_wrap_g_input_stream_has_pending, METH_NOARGS,
      NULL },
    { "set_pending", (PyCFunction)_wrap_g_input_stream_set_pending, METH_NOARGS,
      NULL },
    { "clear_pending", (PyCFunction)_wrap_g_input_stream_clear_pending, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGInputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.InputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGInputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFileInputStream ----------- */

static PyObject *
_wrap_g_file_input_stream_query_info(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attributes", "cancellable", NULL };
    char *attributes;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GFileInfo *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|O:GFileInputStream.query_info", kwlist, &attributes, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_input_stream_query_info(G_FILE_INPUT_STREAM(self->obj), attributes, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_file_input_stream_query_info_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GFileInfo *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFileInputStream.query_info_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_file_input_stream_query_info_finish(G_FILE_INPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static const PyMethodDef _PyGFileInputStream_methods[] = {
    { "query_info", (PyCFunction)_wrap_g_file_input_stream_query_info, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_info_finish", (PyCFunction)_wrap_g_file_input_stream_query_info_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGFileInputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FileInputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFileInputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFilterInputStream ----------- */

static PyObject *
_wrap_g_filter_input_stream_get_base_stream(PyGObject *self)
{
    GInputStream *ret;

    
    ret = g_filter_input_stream_get_base_stream(G_FILTER_INPUT_STREAM(self->obj));
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static const PyMethodDef _PyGFilterInputStream_methods[] = {
    { "get_base_stream", (PyCFunction)_wrap_g_filter_input_stream_get_base_stream, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGFilterInputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FilterInputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFilterInputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GBufferedInputStream ----------- */

static int
_wrap_g_buffered_input_stream_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    GType obj_type = pyg_type_from_object((PyObject *) self);
    GParameter params[1];
    PyObject *parsed_args[1] = {NULL, };
    char *arg_names[] = {"base_stream", NULL };
    char *prop_names[] = {"base_stream", NULL };
    guint nparams, i;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O:gio.BufferedInputStream.__init__" , arg_names , &parsed_args[0]))
        return -1;

    memset(params, 0, sizeof(GParameter)*1);
    if (!pyg_parse_constructor_args(obj_type, arg_names,
                                    prop_names, params, 
                                    &nparams, parsed_args))
        return -1;
    pygobject_constructv(self, nparams, params);
    for (i = 0; i < nparams; ++i)
        g_value_unset(&params[i].value);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.BufferedInputStream object");
        return -1;
    }
    return 0;
}

static PyObject *
_wrap_g_buffered_input_stream_get_buffer_size(PyGObject *self)
{
    gsize ret;

    
    ret = g_buffered_input_stream_get_buffer_size(G_BUFFERED_INPUT_STREAM(self->obj));
    
    return PyLong_FromUnsignedLongLong(ret);

}

static PyObject *
_wrap_g_buffered_input_stream_set_buffer_size(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "size", NULL };
    gsize size;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"k:GBufferedInputStream.set_buffer_size", kwlist, &size))
        return NULL;
    
    g_buffered_input_stream_set_buffer_size(G_BUFFERED_INPUT_STREAM(self->obj), size);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_buffered_input_stream_get_available(PyGObject *self)
{
    gsize ret;

    
    ret = g_buffered_input_stream_get_available(G_BUFFERED_INPUT_STREAM(self->obj));
    
    return PyLong_FromUnsignedLongLong(ret);

}

static PyObject *
_wrap_g_buffered_input_stream_fill(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "count", "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    gssize count, ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"l|O:GBufferedInputStream.fill", kwlist, &count, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_buffered_input_stream_fill(G_BUFFERED_INPUT_STREAM(self->obj), count, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);

}

static PyObject *
_wrap_g_buffered_input_stream_fill_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GError *error = NULL;
    gssize ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GBufferedInputStream.fill_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_buffered_input_stream_fill_finish(G_BUFFERED_INPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);

}

static PyObject *
_wrap_g_buffered_input_stream_read_byte(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GBufferedInputStream.read_byte", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_buffered_input_stream_read_byte(G_BUFFERED_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyInt_FromLong(ret);
}

static const PyMethodDef _PyGBufferedInputStream_methods[] = {
    { "get_buffer_size", (PyCFunction)_wrap_g_buffered_input_stream_get_buffer_size, METH_NOARGS,
      NULL },
    { "set_buffer_size", (PyCFunction)_wrap_g_buffered_input_stream_set_buffer_size, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_available", (PyCFunction)_wrap_g_buffered_input_stream_get_available, METH_NOARGS,
      NULL },
    { "fill", (PyCFunction)_wrap_g_buffered_input_stream_fill, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "fill_finish", (PyCFunction)_wrap_g_buffered_input_stream_fill_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_byte", (PyCFunction)_wrap_g_buffered_input_stream_read_byte, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGBufferedInputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.BufferedInputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGBufferedInputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_buffered_input_stream_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GDataInputStream ----------- */

 static int
_wrap_g_data_input_stream_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    GType obj_type = pyg_type_from_object((PyObject *) self);
    GParameter params[1];
    PyObject *parsed_args[1] = {NULL, };
    char *arg_names[] = {"base_stream", NULL };
    char *prop_names[] = {"base_stream", NULL };
    guint nparams, i;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O:gio.DataInputStream.__init__" , arg_names , &parsed_args[0]))
        return -1;

    memset(params, 0, sizeof(GParameter)*1);
    if (!pyg_parse_constructor_args(obj_type, arg_names,
                                    prop_names, params, 
                                    &nparams, parsed_args))
        return -1;
    pygobject_constructv(self, nparams, params);
    for (i = 0; i < nparams; ++i)
        g_value_unset(&params[i].value);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.DataInputStream object");
        return -1;
    }
    return 0;
}

static PyObject *
_wrap_g_data_input_stream_set_byte_order(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "order", NULL };
    PyObject *py_order = NULL;
    GDataStreamByteOrder order;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:GDataInputStream.set_byte_order", kwlist, &py_order))
        return NULL;
    if (pyg_enum_get_value(G_TYPE_DATA_STREAM_BYTE_ORDER, py_order, (gpointer)&order))
        return NULL;
    
    g_data_input_stream_set_byte_order(G_DATA_INPUT_STREAM(self->obj), order);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_data_input_stream_get_byte_order(PyGObject *self)
{
    gint ret;

    
    ret = g_data_input_stream_get_byte_order(G_DATA_INPUT_STREAM(self->obj));
    
    return pyg_enum_from_gtype(G_TYPE_DATA_STREAM_BYTE_ORDER, ret);
}

static PyObject *
_wrap_g_data_input_stream_set_newline_type(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", NULL };
    PyObject *py_type = NULL;
    GDataStreamNewlineType type;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:GDataInputStream.set_newline_type", kwlist, &py_type))
        return NULL;
    if (pyg_enum_get_value(G_TYPE_DATA_STREAM_NEWLINE_TYPE, py_type, (gpointer)&type))
        return NULL;
    
    g_data_input_stream_set_newline_type(G_DATA_INPUT_STREAM(self->obj), type);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_data_input_stream_get_newline_type(PyGObject *self)
{
    gint ret;

    
    ret = g_data_input_stream_get_newline_type(G_DATA_INPUT_STREAM(self->obj));
    
    return pyg_enum_from_gtype(G_TYPE_DATA_STREAM_NEWLINE_TYPE, ret);
}

static PyObject *
_wrap_g_data_input_stream_read_byte(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    gchar ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GDataInputStream.read_byte", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_input_stream_read_byte(G_DATA_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyString_FromStringAndSize(&ret, 1);
}

static PyObject *
_wrap_g_data_input_stream_read_int16(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GDataInputStream.read_int16", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_input_stream_read_int16(G_DATA_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_data_input_stream_read_uint16(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GDataInputStream.read_uint16", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_input_stream_read_uint16(G_DATA_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_data_input_stream_read_int32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GDataInputStream.read_int32", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_input_stream_read_int32(G_DATA_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_data_input_stream_read_uint32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    guint32 ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GDataInputStream.read_uint32", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_input_stream_read_uint32(G_DATA_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromUnsignedLong(ret);

}

static PyObject *
_wrap_g_data_input_stream_read_int64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    gint64 ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GDataInputStream.read_int64", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_input_stream_read_int64(G_DATA_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);
}

static PyObject *
_wrap_g_data_input_stream_read_uint64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    guint64 ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GDataInputStream.read_uint64", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_input_stream_read_uint64(G_DATA_INPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromUnsignedLongLong(ret);
}

#line 297 "ginputstream.override"
static PyObject *
_wrap_g_data_input_stream_read_line(PyGObject *self,
				    PyObject *args,
				    PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable;
    char *line;
    gsize length;
    PyObject *py_line;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "|O:gio.DataInputStream.read_line",
                                     kwlist, &pycancellable))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
	return NULL;

    line = g_data_input_stream_read_line(G_DATA_INPUT_STREAM(self->obj),
					 &length, cancellable, &error);
    if (pyg_error_check(&error))
        return NULL;

    py_line = PyString_FromStringAndSize(line, length);
    g_free(line);
    return py_line;
}
#line 3352 "gio.c"


#line 329 "ginputstream.override"
static PyObject *
_wrap_g_data_input_stream_read_until(PyGObject *self,
				     PyObject *args,
				     PyObject *kwargs)
{
    static char *kwlist[] = { "stop_chars", "cancellable", NULL };
    const char *stop_chars;
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable;
    char *line;
    gsize length;
    PyObject *py_line;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "s|O:gio.DataInputStream.read_line",
                                     kwlist, &stop_chars, &pycancellable))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
	return NULL;

    line = g_data_input_stream_read_until(G_DATA_INPUT_STREAM(self->obj),
					  stop_chars, &length, cancellable, &error);
    if (pyg_error_check(&error))
        return NULL;

    py_line = PyString_FromStringAndSize(line, length);
    g_free(line);
    return py_line;
}
#line 3387 "gio.c"


static const PyMethodDef _PyGDataInputStream_methods[] = {
    { "set_byte_order", (PyCFunction)_wrap_g_data_input_stream_set_byte_order, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_byte_order", (PyCFunction)_wrap_g_data_input_stream_get_byte_order, METH_NOARGS,
      NULL },
    { "set_newline_type", (PyCFunction)_wrap_g_data_input_stream_set_newline_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_newline_type", (PyCFunction)_wrap_g_data_input_stream_get_newline_type, METH_NOARGS,
      NULL },
    { "read_byte", (PyCFunction)_wrap_g_data_input_stream_read_byte, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_int16", (PyCFunction)_wrap_g_data_input_stream_read_int16, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_uint16", (PyCFunction)_wrap_g_data_input_stream_read_uint16, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_int32", (PyCFunction)_wrap_g_data_input_stream_read_int32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_uint32", (PyCFunction)_wrap_g_data_input_stream_read_uint32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_int64", (PyCFunction)_wrap_g_data_input_stream_read_int64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_uint64", (PyCFunction)_wrap_g_data_input_stream_read_uint64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "read_line", (PyCFunction)_wrap_g_data_input_stream_read_line, METH_VARARGS|METH_KEYWORDS,
      (char *) "S.read_line([cancellable]) -> str\n"
"Read a line from the stream. Return value includes ending newline\n"
"character." },
    { "read_until", (PyCFunction)_wrap_g_data_input_stream_read_until, METH_VARARGS|METH_KEYWORDS,
      (char *) "S.read_until(stop_chars, [cancellable]) -> str\n"
"Read characters from the string, stopping at the end or upon reading\n"
"any character in stop_chars. Return value does not include the stopping\n"
"character." },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGDataInputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.DataInputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGDataInputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_data_input_stream_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GMemoryInputStream ----------- */

 static int
_wrap_g_memory_input_stream_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char* kwlist[] = { NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     ":gio.MemoryInputStream.__init__",
                                     kwlist))
        return -1;

    pygobject_constructv(self, 0, NULL);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.MemoryInputStream object");
        return -1;
    }
    return 0;
}

#line 362 "ginputstream.override"
static PyObject *
_wrap_g_memory_input_stream_add_data(PyGObject *self,
                                     PyObject *args,
                                     PyObject *kwargs)
{
    static char *kwlist[] = { "data", NULL };
    PyObject *data;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O:gio.MemoryInputStream.add_data",
                                     kwlist, &data))
        return NULL;

    if (data != Py_None) {
        char *copy;
        int length;

        if (!PyString_Check(data)) {
            PyErr_SetString(PyExc_TypeError, "data must be a string or None");
            return NULL;
        }

        length = PyString_Size(data);
        copy = g_malloc(length);
        memcpy(copy, PyString_AsString(data), length);

        g_memory_input_stream_add_data(G_MEMORY_INPUT_STREAM(self->obj),
                                       copy, length, (GDestroyNotify) g_free);
    }

    Py_INCREF(Py_None);
    return Py_None;
}
/* GInputStream.skip_async: No ArgType for GAsyncReadyCallback */
#line 3528 "gio.c"


static const PyMethodDef _PyGMemoryInputStream_methods[] = {
    { "add_data", (PyCFunction)_wrap_g_memory_input_stream_add_data, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGMemoryInputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.MemoryInputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGMemoryInputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_memory_input_stream_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GMountOperation ----------- */

static int
_wrap_g_mount_operation_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char* kwlist[] = { NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     ":gio.MountOperation.__init__",
                                     kwlist))
        return -1;

    pygobject_constructv(self, 0, NULL);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.MountOperation object");
        return -1;
    }
    return 0;
}

static PyObject *
_wrap_g_mount_operation_get_username(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_mount_operation_get_username(G_MOUNT_OPERATION(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_set_username(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "username", NULL };
    char *username;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GMountOperation.set_username", kwlist, &username))
        return NULL;
    
    g_mount_operation_set_username(G_MOUNT_OPERATION(self->obj), username);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_get_password(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_mount_operation_get_password(G_MOUNT_OPERATION(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_set_password(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "password", NULL };
    char *password;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GMountOperation.set_password", kwlist, &password))
        return NULL;
    
    g_mount_operation_set_password(G_MOUNT_OPERATION(self->obj), password);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_get_anonymous(PyGObject *self)
{
    int ret;

    
    ret = g_mount_operation_get_anonymous(G_MOUNT_OPERATION(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_mount_operation_set_anonymous(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "anonymous", NULL };
    int anonymous;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GMountOperation.set_anonymous", kwlist, &anonymous))
        return NULL;
    
    g_mount_operation_set_anonymous(G_MOUNT_OPERATION(self->obj), anonymous);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_get_domain(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_mount_operation_get_domain(G_MOUNT_OPERATION(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_set_domain(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "domain", NULL };
    char *domain;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GMountOperation.set_domain", kwlist, &domain))
        return NULL;
    
    g_mount_operation_set_domain(G_MOUNT_OPERATION(self->obj), domain);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_get_password_save(PyGObject *self)
{
    gint ret;

    
    ret = g_mount_operation_get_password_save(G_MOUNT_OPERATION(self->obj));
    
    return pyg_enum_from_gtype(G_TYPE_PASSWORD_SAVE, ret);
}

static PyObject *
_wrap_g_mount_operation_set_password_save(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "save", NULL };
    PyObject *py_save = NULL;
    GPasswordSave save;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:GMountOperation.set_password_save", kwlist, &py_save))
        return NULL;
    if (pyg_enum_get_value(G_TYPE_PASSWORD_SAVE, py_save, (gpointer)&save))
        return NULL;
    
    g_mount_operation_set_password_save(G_MOUNT_OPERATION(self->obj), save);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_get_choice(PyGObject *self)
{
    int ret;

    
    ret = g_mount_operation_get_choice(G_MOUNT_OPERATION(self->obj));
    
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_mount_operation_set_choice(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "choice", NULL };
    int choice;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GMountOperation.set_choice", kwlist, &choice))
        return NULL;
    
    g_mount_operation_set_choice(G_MOUNT_OPERATION(self->obj), choice);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_operation_reply(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyObject *py_result = NULL;
    GMountOperationResult result;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:GMountOperation.reply", kwlist, &py_result))
        return NULL;
    if (pyg_enum_get_value(G_TYPE_MOUNT_OPERATION_RESULT, py_result, (gpointer)&result))
        return NULL;
    
    g_mount_operation_reply(G_MOUNT_OPERATION(self->obj), result);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGMountOperation_methods[] = {
    { "get_username", (PyCFunction)_wrap_g_mount_operation_get_username, METH_NOARGS,
      NULL },
    { "set_username", (PyCFunction)_wrap_g_mount_operation_set_username, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_password", (PyCFunction)_wrap_g_mount_operation_get_password, METH_NOARGS,
      NULL },
    { "set_password", (PyCFunction)_wrap_g_mount_operation_set_password, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_anonymous", (PyCFunction)_wrap_g_mount_operation_get_anonymous, METH_NOARGS,
      NULL },
    { "set_anonymous", (PyCFunction)_wrap_g_mount_operation_set_anonymous, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_domain", (PyCFunction)_wrap_g_mount_operation_get_domain, METH_NOARGS,
      NULL },
    { "set_domain", (PyCFunction)_wrap_g_mount_operation_set_domain, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_password_save", (PyCFunction)_wrap_g_mount_operation_get_password_save, METH_NOARGS,
      NULL },
    { "set_password_save", (PyCFunction)_wrap_g_mount_operation_set_password_save, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_choice", (PyCFunction)_wrap_g_mount_operation_get_choice, METH_NOARGS,
      NULL },
    { "set_choice", (PyCFunction)_wrap_g_mount_operation_set_choice, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "reply", (PyCFunction)_wrap_g_mount_operation_reply, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGMountOperation_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.MountOperation",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGMountOperation_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_mount_operation_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GOutputStream ----------- */

#line 24 "goutputstream.override"
static PyObject *
_wrap_g_output_stream_write(PyGObject *self,
			    PyObject *args,
			    PyObject *kwargs)
{
  static char *kwlist[] = { "buffer", "cancellable", NULL };
  PyGObject *pycancellable = NULL;
  gchar *buffer;
  long count = 0; 
  GCancellable *cancellable;
  GError *error = NULL;
  gssize written;
  
  if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				   "s#|O!:OutputStream.write",
				   kwlist, &buffer, &count,
				   &PyGCancellable_Type, &pycancellable))
    return NULL;
  
  if (!pygio_check_cancellable(pycancellable, &cancellable))
      return NULL;

  pyg_begin_allow_threads;
  written = g_output_stream_write(G_OUTPUT_STREAM(self->obj),
				  buffer, count, cancellable, &error);
  pyg_end_allow_threads;

  if (pyg_error_check(&error))
    return NULL;
      
  return PyInt_FromLong(written);
}
#line 3905 "gio.c"


#line 58 "goutputstream.override"
static PyObject *
_wrap_g_output_stream_write_all(PyGObject *self,
				PyObject *args,
				PyObject *kwargs)
{
  static char *kwlist[] = { "buffer", "cancellable", NULL };
  PyGObject *pycancellable = NULL;
  gchar *buffer;
  long count = 0; 
  GCancellable *cancellable;
  GError *error = NULL;
  gsize written;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				   "s#|O!:OutputStream.write",
				   kwlist, &buffer, &count,
				   &PyGCancellable_Type, &pycancellable))
    return NULL;

  if (!pygio_check_cancellable(pycancellable, &cancellable))
      return NULL;

  pyg_begin_allow_threads;
  g_output_stream_write_all(G_OUTPUT_STREAM(self->obj),
			    buffer, count, &written, cancellable, &error);
  pyg_end_allow_threads;

  if (pyg_error_check(&error))
    return NULL;
      
  return PyInt_FromLong(written);
}
#line 3941 "gio.c"


static PyObject *
_wrap_g_output_stream_splice(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "source", "flags", "cancellable", NULL };
    GError *error = NULL;
    PyObject *py_flags = NULL;
    GCancellable *cancellable = NULL;
    GOutputStreamSpliceFlags flags = G_OUTPUT_STREAM_SPLICE_NONE;
    gssize ret;
    PyGObject *source, *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!|OO:GOutputStream.splice", kwlist, &PyGInputStream_Type, &source, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_OUTPUT_STREAM_SPLICE_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_output_stream_splice(G_OUTPUT_STREAM(self->obj), G_INPUT_STREAM(source->obj), flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);

}

static PyObject *
_wrap_g_output_stream_flush(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GOutputStream.flush", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_output_stream_flush(G_OUTPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_output_stream_close(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GOutputStream.close", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_output_stream_close(G_OUTPUT_STREAM(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 92 "goutputstream.override"
static PyObject *
_wrap_g_output_stream_write_async(PyGObject *self,
				  PyObject *args,
				  PyObject *kwargs)
{
  static char *kwlist[] = { "buffer", "callback", "io_priority", "cancellable",
			    "user_data", NULL };
  gchar *buffer;
  long count = -1;
  int io_priority = G_PRIORITY_DEFAULT;
  PyGObject *pycancellable = NULL;
  GCancellable *cancellable;
  PyGIONotify *notify;

  notify = pygio_notify_new();

  if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				   "s#O|iOO:OutputStream.write_async",
				   kwlist, &buffer,
				   &count,
				   &notify->callback,
				   &io_priority,
				   &pycancellable,
				   &notify->data))
      goto error;

  if (!pygio_notify_callback_is_valid(notify))
      goto error;
  
  if (!pygio_check_cancellable(pycancellable, &cancellable))
      goto error;

  pygio_notify_reference_callback(notify);
  pygio_notify_copy_buffer(notify, buffer, count);

  g_output_stream_write_async(G_OUTPUT_STREAM(self->obj),
			      notify->buffer,
			      notify->buffer_size,
			      io_priority,
			      cancellable,
			      (GAsyncReadyCallback)async_result_callback_marshal,
			      notify);
  
  Py_INCREF(Py_None);
  return Py_None;

 error:
  pygio_notify_free(notify);
  return NULL;
}
#line 4083 "gio.c"


static PyObject *
_wrap_g_output_stream_write_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GError *error = NULL;
    gssize ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GOutputStream.write_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_output_stream_write_finish(G_OUTPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);

}

static PyObject *
_wrap_g_output_stream_splice_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GError *error = NULL;
    gssize ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GOutputStream.splice_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_output_stream_splice_finish(G_OUTPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyLong_FromLongLong(ret);

}

static PyObject *
_wrap_g_output_stream_flush_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GOutputStream.flush_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_output_stream_flush_finish(G_OUTPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 144 "goutputstream.override"
static PyObject *
_wrap_g_output_stream_close_async(PyGObject *self,
				  PyObject *args,
				  PyObject *kwargs)
{
  static char *kwlist[] = { "callback", "io_priority",
			    "cancellable", "user_data", NULL };
  int io_priority = G_PRIORITY_DEFAULT;
  PyGObject *pycancellable = NULL;
  GCancellable *cancellable;
  PyGIONotify *notify;

  notify = pygio_notify_new();

  if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				   "O|iOO:OutputStream.close_async",
				   kwlist,
				   &notify->callback,
				   &io_priority,
				   &pycancellable,
				   &notify->data))
      goto error;

  if (!pygio_notify_callback_is_valid(notify))
      goto error;

  if (!pygio_check_cancellable(pycancellable, &cancellable))
      goto error;

  pygio_notify_reference_callback(notify);
  
  g_output_stream_close_async(G_OUTPUT_STREAM(self->obj),
			      io_priority,
			      cancellable,
			      (GAsyncReadyCallback)async_result_callback_marshal,
			      notify);
  
  Py_INCREF(Py_None);
  return Py_None;

 error:
  pygio_notify_free(notify);
  return NULL;
}
#line 4188 "gio.c"


static PyObject *
_wrap_g_output_stream_close_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GOutputStream.close_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_output_stream_close_finish(G_OUTPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_output_stream_is_closed(PyGObject *self)
{
    int ret;

    
    ret = g_output_stream_is_closed(G_OUTPUT_STREAM(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_output_stream_has_pending(PyGObject *self)
{
    int ret;

    
    ret = g_output_stream_has_pending(G_OUTPUT_STREAM(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_output_stream_set_pending(PyGObject *self)
{
    int ret;
    GError *error = NULL;

    
    ret = g_output_stream_set_pending(G_OUTPUT_STREAM(self->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_output_stream_clear_pending(PyGObject *self)
{
    
    g_output_stream_clear_pending(G_OUTPUT_STREAM(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGOutputStream_methods[] = {
    { "write_part", (PyCFunction)_wrap_g_output_stream_write, METH_VARARGS|METH_KEYWORDS,
      (char *) "STREAM.write_part(buffer, [cancellable]) -> int\n"
"\n"
"Write the bytes in 'buffer' to the stream. Return the number of bytes\n"
"successfully written. This method is allowed to stop at any time after\n"
"writing at least 1 byte. Therefore, to reliably write the whole buffer,\n"
"you need to use a loop. See also gio.OutputStream.write for easier to\n"
"use (though less efficient) method.\n"
"\n"
"Note: this method roughly corresponds to C GIO g_output_stream_write." },
    { "write", (PyCFunction)_wrap_g_output_stream_write_all, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "splice", (PyCFunction)_wrap_g_output_stream_splice, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "flush", (PyCFunction)_wrap_g_output_stream_flush, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "close", (PyCFunction)_wrap_g_output_stream_close, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "write_async", (PyCFunction)_wrap_g_output_stream_write_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "S.write_async(buffer, callback [,io_priority] [,cancellable] [,user_data])\n"
"\n"
"Request an asynchronous write of count bytes from buffer into the stream.\n"
"When the operation is finished callback will be called. You can then call\n"
"gio.OutputStream.write_finish() to get the result of the operation.\n"
"On success, the number of bytes written will be passed to the callback.\n"
"It is not an error if this is not the same as the requested size, as it can\n"
"happen e.g. on a partial I/O error, but generally tries to write as many \n"
"bytes as requested.\n"
"For the synchronous, blocking version of this function, see\n"
"gio.OutputStream.write().\n" },
    { "write_finish", (PyCFunction)_wrap_g_output_stream_write_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "splice_finish", (PyCFunction)_wrap_g_output_stream_splice_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "flush_finish", (PyCFunction)_wrap_g_output_stream_flush_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "close_async", (PyCFunction)_wrap_g_output_stream_close_async, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "close_finish", (PyCFunction)_wrap_g_output_stream_close_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "is_closed", (PyCFunction)_wrap_g_output_stream_is_closed, METH_NOARGS,
      NULL },
    { "has_pending", (PyCFunction)_wrap_g_output_stream_has_pending, METH_NOARGS,
      NULL },
    { "set_pending", (PyCFunction)_wrap_g_output_stream_set_pending, METH_NOARGS,
      NULL },
    { "clear_pending", (PyCFunction)_wrap_g_output_stream_clear_pending, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGOutputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.OutputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGOutputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GMemoryOutputStream ----------- */

#line 190 "goutputstream.override"
static int
_wrap_g_memory_output_stream_new(PyGObject *self)
{
    self->obj = (GObject *)g_memory_output_stream_new(NULL, 0, g_realloc, g_free);

    if (!self->obj) {
        PyErr_SetString(PyExc_RuntimeError, "could not create gio.MemoryOutputStream object");
        return -1;
    }

    pygobject_register_wrapper((PyObject *)self);
    return 0;
}
#line 4374 "gio.c"


#line 205 "goutputstream.override"
static PyObject *
_wrap_g_memory_output_stream_get_data(PyGObject *self)
{
    GMemoryOutputStream *stream = G_MEMORY_OUTPUT_STREAM(self->obj);
    return PyString_FromStringAndSize(g_memory_output_stream_get_data(stream),
				      g_seekable_tell(G_SEEKABLE(stream)));
}

/* GOutputStream.write_all: No ArgType for const-void* */
/* GOutputStream.splice_async: No ArgType for GAsyncReadyCallback */
/* GOutputStream.flush_async: No ArgType for GAsyncReadyCallback */
#line 4389 "gio.c"


static PyObject *
_wrap_g_memory_output_stream_get_size(PyGObject *self)
{
    gsize ret;

    
    ret = g_memory_output_stream_get_size(G_MEMORY_OUTPUT_STREAM(self->obj));
    
    return PyLong_FromUnsignedLongLong(ret);

}

static const PyMethodDef _PyGMemoryOutputStream_methods[] = {
    { "get_contents", (PyCFunction)_wrap_g_memory_output_stream_get_data, METH_NOARGS,
      NULL },
    { "get_size", (PyCFunction)_wrap_g_memory_output_stream_get_size, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGMemoryOutputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.MemoryOutputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGMemoryOutputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_memory_output_stream_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFilterOutputStream ----------- */

static PyObject *
_wrap_g_filter_output_stream_get_base_stream(PyGObject *self)
{
    GOutputStream *ret;

    
    ret = g_filter_output_stream_get_base_stream(G_FILTER_OUTPUT_STREAM(self->obj));
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static const PyMethodDef _PyGFilterOutputStream_methods[] = {
    { "get_base_stream", (PyCFunction)_wrap_g_filter_output_stream_get_base_stream, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGFilterOutputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FilterOutputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFilterOutputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GDataOutputStream ----------- */

static int
_wrap_g_data_output_stream_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    GType obj_type = pyg_type_from_object((PyObject *) self);
    GParameter params[1];
    PyObject *parsed_args[1] = {NULL, };
    char *arg_names[] = {"base_stream", NULL };
    char *prop_names[] = {"base_stream", NULL };
    guint nparams, i;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O:gio.DataOutputStream.__init__" , arg_names , &parsed_args[0]))
        return -1;

    memset(params, 0, sizeof(GParameter)*1);
    if (!pyg_parse_constructor_args(obj_type, arg_names,
                                    prop_names, params, 
                                    &nparams, parsed_args))
        return -1;
    pygobject_constructv(self, nparams, params);
    for (i = 0; i < nparams; ++i)
        g_value_unset(&params[i].value);
    if (!self->obj) {
        PyErr_SetString(
            PyExc_RuntimeError, 
            "could not create gio.DataOutputStream object");
        return -1;
    }
    return 0;
}

static PyObject *
_wrap_g_data_output_stream_set_byte_order(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "order", NULL };
    PyObject *py_order = NULL;
    GDataStreamByteOrder order;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:GDataOutputStream.set_byte_order", kwlist, &py_order))
        return NULL;
    if (pyg_enum_get_value(G_TYPE_DATA_STREAM_BYTE_ORDER, py_order, (gpointer)&order))
        return NULL;
    
    g_data_output_stream_set_byte_order(G_DATA_OUTPUT_STREAM(self->obj), order);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_data_output_stream_get_byte_order(PyGObject *self)
{
    gint ret;

    
    ret = g_data_output_stream_get_byte_order(G_DATA_OUTPUT_STREAM(self->obj));
    
    return pyg_enum_from_gtype(G_TYPE_DATA_STREAM_BYTE_ORDER, ret);
}

static PyObject *
_wrap_g_data_output_stream_put_byte(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "data", "cancellable", NULL };
    char data;
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"c|O:GDataOutputStream.put_byte", kwlist, &data, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_byte(G_DATA_OUTPUT_STREAM(self->obj), data, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_data_output_stream_put_int16(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "data", "cancellable", NULL };
    int data, ret;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i|O:GDataOutputStream.put_int16", kwlist, &data, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_int16(G_DATA_OUTPUT_STREAM(self->obj), data, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_data_output_stream_put_uint16(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "data", "cancellable", NULL };
    int data, ret;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i|O:GDataOutputStream.put_uint16", kwlist, &data, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_uint16(G_DATA_OUTPUT_STREAM(self->obj), data, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_data_output_stream_put_int32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "data", "cancellable", NULL };
    int data, ret;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i|O:GDataOutputStream.put_int32", kwlist, &data, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_int32(G_DATA_OUTPUT_STREAM(self->obj), data, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_data_output_stream_put_uint32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "data", "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    unsigned long data;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"k|O:GDataOutputStream.put_uint32", kwlist, &data, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_uint32(G_DATA_OUTPUT_STREAM(self->obj), data, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_data_output_stream_put_int64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "data", "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    gint64 data;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"L|O:GDataOutputStream.put_int64", kwlist, &data, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_int64(G_DATA_OUTPUT_STREAM(self->obj), data, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_data_output_stream_put_uint64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "data", "cancellable", NULL };
    PyObject *py_data = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    int ret;
    guint64 data;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!|O:GDataOutputStream.put_uint64", kwlist, &PyLong_Type, &py_data, &py_cancellable))
        return NULL;
    data = PyLong_AsUnsignedLongLong(py_data);
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_uint64(G_DATA_OUTPUT_STREAM(self->obj), data, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_data_output_stream_put_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "str", "cancellable", NULL };
    char *str;
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|O:GDataOutputStream.put_string", kwlist, &str, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_data_output_stream_put_string(G_DATA_OUTPUT_STREAM(self->obj), str, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGDataOutputStream_methods[] = {
    { "set_byte_order", (PyCFunction)_wrap_g_data_output_stream_set_byte_order, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_byte_order", (PyCFunction)_wrap_g_data_output_stream_get_byte_order, METH_NOARGS,
      NULL },
    { "put_byte", (PyCFunction)_wrap_g_data_output_stream_put_byte, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "put_int16", (PyCFunction)_wrap_g_data_output_stream_put_int16, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "put_uint16", (PyCFunction)_wrap_g_data_output_stream_put_uint16, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "put_int32", (PyCFunction)_wrap_g_data_output_stream_put_int32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "put_uint32", (PyCFunction)_wrap_g_data_output_stream_put_uint32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "put_int64", (PyCFunction)_wrap_g_data_output_stream_put_int64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "put_uint64", (PyCFunction)_wrap_g_data_output_stream_put_uint64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "put_string", (PyCFunction)_wrap_g_data_output_stream_put_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGDataOutputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.DataOutputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGDataOutputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_data_output_stream_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFileOutputStream ----------- */

static PyObject *
_wrap_g_file_output_stream_query_info(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attributes", "cancellable", NULL };
    char *attributes;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable = NULL;
    GFileInfo *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|O:GFileOutputStream.query_info", kwlist, &attributes, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_output_stream_query_info(G_FILE_OUTPUT_STREAM(self->obj), attributes, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_file_output_stream_query_info_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    GFileInfo *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFileOutputStream.query_info_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_file_output_stream_query_info_finish(G_FILE_OUTPUT_STREAM(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_file_output_stream_get_etag(PyGObject *self)
{
    gchar *ret;

    
    ret = g_file_output_stream_get_etag(G_FILE_OUTPUT_STREAM(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGFileOutputStream_methods[] = {
    { "query_info", (PyCFunction)_wrap_g_file_output_stream_query_info, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_info_finish", (PyCFunction)_wrap_g_file_output_stream_query_info_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_etag", (PyCFunction)_wrap_g_file_output_stream_get_etag, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGFileOutputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FileOutputStream",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFileOutputStream_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GSimpleAsyncResult ----------- */

static int
pygobject_no_constructor(PyObject *self, PyObject *args, PyObject *kwargs)
{
    gchar buf[512];

    g_snprintf(buf, sizeof(buf), "%s is an abstract widget", self->ob_type->tp_name);
    PyErr_SetString(PyExc_NotImplementedError, buf);
    return -1;
}

static PyObject *
_wrap_g_simple_async_result_set_op_res_gssize(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "op_res", NULL };
    gssize op_res;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"l:GSimpleAsyncResult.set_op_res_gssize", kwlist, &op_res))
        return NULL;
    
    g_simple_async_result_set_op_res_gssize(G_SIMPLE_ASYNC_RESULT(self->obj), op_res);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_simple_async_result_get_op_res_gssize(PyGObject *self)
{
    gssize ret;

    
    ret = g_simple_async_result_get_op_res_gssize(G_SIMPLE_ASYNC_RESULT(self->obj));
    
    return PyLong_FromLongLong(ret);

}

static PyObject *
_wrap_g_simple_async_result_set_op_res_gboolean(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "op_res", NULL };
    int op_res;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GSimpleAsyncResult.set_op_res_gboolean", kwlist, &op_res))
        return NULL;
    
    g_simple_async_result_set_op_res_gboolean(G_SIMPLE_ASYNC_RESULT(self->obj), op_res);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_simple_async_result_get_op_res_gboolean(PyGObject *self)
{
    int ret;

    
    ret = g_simple_async_result_get_op_res_gboolean(G_SIMPLE_ASYNC_RESULT(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_simple_async_result_set_handle_cancellation(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "handle_cancellation", NULL };
    int handle_cancellation;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:GSimpleAsyncResult.set_handle_cancellation", kwlist, &handle_cancellation))
        return NULL;
    
    g_simple_async_result_set_handle_cancellation(G_SIMPLE_ASYNC_RESULT(self->obj), handle_cancellation);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_simple_async_result_complete(PyGObject *self)
{
    
    g_simple_async_result_complete(G_SIMPLE_ASYNC_RESULT(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_simple_async_result_complete_in_idle(PyGObject *self)
{
    
    g_simple_async_result_complete_in_idle(G_SIMPLE_ASYNC_RESULT(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_simple_async_result_propagate_error(PyGObject *self)
{
    int ret;
    GError *dest = NULL;

    
    ret = g_simple_async_result_propagate_error(G_SIMPLE_ASYNC_RESULT(self->obj), &dest);
    
    if (pyg_error_check(&dest))
        return NULL;
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGSimpleAsyncResult_methods[] = {
    { "set_op_res_gssize", (PyCFunction)_wrap_g_simple_async_result_set_op_res_gssize, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_op_res_gssize", (PyCFunction)_wrap_g_simple_async_result_get_op_res_gssize, METH_NOARGS,
      NULL },
    { "set_op_res_gboolean", (PyCFunction)_wrap_g_simple_async_result_set_op_res_gboolean, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_op_res_gboolean", (PyCFunction)_wrap_g_simple_async_result_get_op_res_gboolean, METH_NOARGS,
      NULL },
    { "set_handle_cancellation", (PyCFunction)_wrap_g_simple_async_result_set_handle_cancellation, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "complete", (PyCFunction)_wrap_g_simple_async_result_complete, METH_NOARGS,
      NULL },
    { "complete_in_idle", (PyCFunction)_wrap_g_simple_async_result_complete_in_idle, METH_NOARGS,
      NULL },
    { "propagate_error", (PyCFunction)_wrap_g_simple_async_result_propagate_error, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGSimpleAsyncResult_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.SimpleAsyncResult",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGSimpleAsyncResult_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)pygobject_no_constructor,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GVfs ----------- */

static PyObject *
_wrap_g_vfs_is_active(PyGObject *self)
{
    int ret;

    
    ret = g_vfs_is_active(G_VFS(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_vfs_get_file_for_path(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "path", NULL };
    char *path;
    PyObject *py_ret;
    GFile *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GVfs.get_file_for_path", kwlist, &path))
        return NULL;
    
    ret = g_vfs_get_file_for_path(G_VFS(self->obj), path);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_vfs_get_file_for_uri(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "uri", NULL };
    char *uri;
    PyObject *py_ret;
    GFile *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GVfs.get_file_for_uri", kwlist, &uri))
        return NULL;
    
    ret = g_vfs_get_file_for_uri(G_VFS(self->obj), uri);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_vfs_parse_name(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "parse_name", NULL };
    char *parse_name;
    PyObject *py_ret;
    GFile *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GVfs.parse_name", kwlist, &parse_name))
        return NULL;
    
    ret = g_vfs_parse_name(G_VFS(self->obj), parse_name);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 661 "gio.override"
static PyObject *
_wrap_g_vfs_get_supported_uri_schemes(PyGObject *self)
{
    const char * const *names;
    PyObject *ret;

    names = g_vfs_get_supported_uri_schemes(G_VFS(self->obj));

    ret = PyList_New(0);
    while (names && *names) {
        PyObject *item = PyString_FromString(names[0]);
        PyList_Append(ret, item);
        Py_DECREF(item);

        names++;
    }

    return ret;
}
#line 5287 "gio.c"


static const PyMethodDef _PyGVfs_methods[] = {
    { "is_active", (PyCFunction)_wrap_g_vfs_is_active, METH_NOARGS,
      NULL },
    { "get_file_for_path", (PyCFunction)_wrap_g_vfs_get_file_for_path, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_file_for_uri", (PyCFunction)_wrap_g_vfs_get_file_for_uri, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "parse_name", (PyCFunction)_wrap_g_vfs_parse_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_supported_uri_schemes", (PyCFunction)_wrap_g_vfs_get_supported_uri_schemes, METH_NOARGS,
      (char *) "VFS.get_supported_uri_schemes() -> [uri, ..]\n"
"Gets a list of URI schemes supported by vfs." },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGVfs_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.Vfs",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGVfs_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GVolumeMonitor ----------- */

#line 33 "gvolumemonitor.override"
static PyObject *
_wrap_g_volume_monitor_get_connected_drives (PyGObject *self)
{
  GList *list, *l;
  PyObject *ret;
  
  list = g_volume_monitor_get_connected_drives (G_VOLUME_MONITOR (self->obj));

  ret = PyList_New(0);
  for (l = list; l; l = l->next) {
    GDrive *drive = l->data;
    PyObject *item = pygobject_new((GObject *)drive);
    PyList_Append(ret, item);
    Py_DECREF(item);
    g_object_unref(drive);
  }
  g_list_free(list);
  
  return ret;
}
#line 5375 "gio.c"


#line 55 "gvolumemonitor.override"
static PyObject *
_wrap_g_volume_monitor_get_volumes (PyGObject *self)
{
  GList *list, *l;
  PyObject *ret;
  
  list = g_volume_monitor_get_volumes (G_VOLUME_MONITOR (self->obj));

  ret = PyList_New(0);
  for (l = list; l; l = l->next) {
    GVolume *volume = l->data;
    PyObject *item = pygobject_new((GObject *)volume);
    PyList_Append(ret, item);
    Py_DECREF(item);
    g_object_unref(volume);
  }
  g_list_free(list);
  
  return ret;
}
#line 5399 "gio.c"


#line 77 "gvolumemonitor.override"
static PyObject *
_wrap_g_volume_monitor_get_mounts (PyGObject *self)
{
  GList *list, *l;
  PyObject *ret;
  
  list = g_volume_monitor_get_mounts (G_VOLUME_MONITOR (self->obj));

  ret = PyList_New(0);
  for (l = list; l; l = l->next) {
    GMount *mount = l->data;
    PyObject *item = pygobject_new((GObject *)mount);
    PyList_Append(ret, item);
    Py_DECREF(item);
    g_object_unref(mount);
  }
  g_list_free(list);
  
  return ret;
}
#line 5423 "gio.c"


static PyObject *
_wrap_g_volume_monitor_get_volume_for_uuid(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "uuid", NULL };
    char *uuid;
    PyObject *py_ret;
    GVolume *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GVolumeMonitor.get_volume_for_uuid", kwlist, &uuid))
        return NULL;
    
    ret = g_volume_monitor_get_volume_for_uuid(G_VOLUME_MONITOR(self->obj), uuid);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_volume_monitor_get_mount_for_uuid(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "uuid", NULL };
    char *uuid;
    GMount *ret;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GVolumeMonitor.get_mount_for_uuid", kwlist, &uuid))
        return NULL;
    
    ret = g_volume_monitor_get_mount_for_uuid(G_VOLUME_MONITOR(self->obj), uuid);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static const PyMethodDef _PyGVolumeMonitor_methods[] = {
    { "get_connected_drives", (PyCFunction)_wrap_g_volume_monitor_get_connected_drives, METH_NOARGS,
      NULL },
    { "get_volumes", (PyCFunction)_wrap_g_volume_monitor_get_volumes, METH_NOARGS,
      NULL },
    { "get_mounts", (PyCFunction)_wrap_g_volume_monitor_get_mounts, METH_NOARGS,
      NULL },
    { "get_volume_for_uuid", (PyCFunction)_wrap_g_volume_monitor_get_volume_for_uuid, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_mount_for_uuid", (PyCFunction)_wrap_g_volume_monitor_get_mount_for_uuid, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 24 "gvolumemonitor.override"
static PyObject *
_wrap_g_volume_monitor_tp_new(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyErr_SetString(PyExc_TypeError,
		    "cannot create instance of type `GVolumeMonitor'");
    return NULL;
}
#line 5486 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGVolumeMonitor_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.VolumeMonitor",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGVolumeMonitor_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)_wrap_g_volume_monitor_tp_new,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GNativeVolumeMonitor ----------- */

PyTypeObject G_GNUC_INTERNAL PyGNativeVolumeMonitor_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.NativeVolumeMonitor",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)NULL, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFileIcon ----------- */

static int
_wrap_g_file_icon_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "file", NULL };
    PyGObject *file;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFileIcon.__init__", kwlist, &PyGFile_Type, &file))
        return -1;
    self->obj = (GObject *)g_file_icon_new(G_FILE(file->obj));

    if (!self->obj) {
        PyErr_SetString(PyExc_RuntimeError, "could not create GFileIcon object");
        return -1;
    }
    pygobject_register_wrapper((PyObject *)self);
    return 0;
}

static PyObject *
_wrap_g_file_icon_get_file(PyGObject *self)
{
    GFile *ret;

    
    ret = g_file_icon_get_file(G_FILE_ICON(self->obj));
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static const PyMethodDef _PyGFileIcon_methods[] = {
    { "get_file", (PyCFunction)_wrap_g_file_icon_get_file, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 171 "gicon.override"
static PyObject *
_wrap_g_file_icon_tp_repr(PyGObject *self)
{
    GFile *file = g_file_icon_get_file(G_FILE_ICON(self->obj));
    char *uri = (file ? g_file_get_uri(file) : NULL);
    gchar *representation;
    PyObject *result;

    if (uri) {
	representation = g_strdup_printf("<%s at %p: %s>", self->ob_type->tp_name, self, uri);
	g_free(uri);
    }
    else
	representation = g_strdup_printf("<%s at %p: UNKNOWN URI>", self->ob_type->tp_name, self);

    result = PyString_FromString(representation);
    g_free(representation);
    return result;
}
#line 5643 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGFileIcon_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.FileIcon",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)_wrap_g_file_icon_tp_repr,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFileIcon_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_file_icon_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GThemedIcon ----------- */

#line 194 "gicon.override"
static int
_wrap_g_themed_icon_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "name", "use_default_fallbacks", NULL };
    PyObject *name;
    gboolean use_default_fallbacks = FALSE;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O|i:gio.ThemedIcon.__init__",
				     kwlist, &name, &use_default_fallbacks))
	return -1;

    if (PyString_Check(name)) {
	pygobject_construct(self,
			    "name", PyString_AsString(name),
			    "use-default-fallbacks", use_default_fallbacks, NULL);
	return 0;
    }
    else if (PySequence_Check(name)) {
	PyObject *tuple = PySequence_Tuple(name);

	if (tuple) {
	    int k;
	    int length = PyTuple_Size(tuple);
	    char **names = g_new(char *, length + 1);

	    for (k = 0; k < length; k++) {
		PyObject *str = PyTuple_GetItem(tuple, k);
		if (str && PyString_Check(str))
		    names[k] = PyString_AsString(str);
		else {
		    Py_DECREF(tuple);
		    g_free(names);
		    goto error;
		}
	    }

	    names[length] = NULL;
	    pygobject_construct(self,
				"names", names,
				"use-default-fallbacks", use_default_fallbacks, NULL);
	    Py_DECREF(tuple);
	    g_free(names);
	    return 0;
	}
    }

 error:
    if (!PyErr_Occurred()) {
	PyErr_SetString(PyExc_TypeError,
			"argument 1 of gio.ThemedIcon.__init__ "
			"must be either a string or a sequence of strings");
    }
    return -1;
}
#line 5750 "gio.c"


#line 250 "gicon.override"
static PyObject *
_wrap_g_themed_icon_get_names(PyGObject *self)
{
    const char * const *names;
    PyObject *ret;

    names = g_themed_icon_get_names(G_THEMED_ICON(self->obj));

    ret = PyList_New(0);
    while (names && *names) {
        PyObject *item = PyString_FromString(names[0]);
        PyList_Append(ret, item);
        Py_DECREF(item);

        names++;
    }

    return ret;
}
#line 5773 "gio.c"


static PyObject *
_wrap_g_themed_icon_append_name(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "iconname", NULL };
    char *iconname;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GThemedIcon.append_name", kwlist, &iconname))
        return NULL;
    
    g_themed_icon_append_name(G_THEMED_ICON(self->obj), iconname);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyGThemedIcon_methods[] = {
    { "get_names", (PyCFunction)_wrap_g_themed_icon_get_names, METH_NOARGS,
      NULL },
    { "append_name", (PyCFunction)_wrap_g_themed_icon_append_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 271 "gicon.override"
static PyObject *
_wrap_g_themed_icon_tp_repr(PyGObject *self)
{
    const char * const *names = g_themed_icon_get_names(G_THEMED_ICON(self->obj));
    GString *representation = g_string_new(NULL);
    PyObject *result;

    g_string_append_printf(representation, "<%s at %p: ", self->ob_type->tp_name, self);

    if (names) {
	gboolean first_name = TRUE;
	while (*names) {
	    if (!first_name)
		g_string_append(representation, ", ");
	    else
		first_name = FALSE;

	    g_string_append(representation, *names++);
	}
    }

    g_string_append(representation, ">");
    result = PyString_FromString(representation->str);
    g_string_free(representation, TRUE);
    return result;
}
#line 5826 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGThemedIcon_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.ThemedIcon",                   /* tp_name */
    sizeof(PyGObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)_wrap_g_themed_icon_tp_repr,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGThemedIcon_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_themed_icon_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GAppInfo ----------- */

static PyObject *
_wrap_g_app_info_dup(PyGObject *self)
{
    GAppInfo *ret;
    PyObject *py_ret;

    
    ret = g_app_info_dup(G_APP_INFO(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_app_info_equal(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "appinfo2", NULL };
    PyGObject *appinfo2;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GAppInfo.equal", kwlist, &PyGAppInfo_Type, &appinfo2))
        return NULL;
    
    ret = g_app_info_equal(G_APP_INFO(self->obj), G_APP_INFO(appinfo2->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_app_info_get_id(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_app_info_get_id(G_APP_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_app_info_get_name(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_app_info_get_name(G_APP_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_app_info_get_description(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_app_info_get_description(G_APP_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_app_info_get_executable(PyGObject *self)
{
    const gchar *ret;

    
    ret = g_app_info_get_executable(G_APP_INFO(self->obj));
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_app_info_get_icon(PyGObject *self)
{
    GIcon *ret;

    
    ret = g_app_info_get_icon(G_APP_INFO(self->obj));
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

#line 118 "gappinfo.override"
static PyObject *
_wrap_g_app_info_launch(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "files", "launch_context", NULL };

    GList *file_list = NULL;
    PyGObject *pycontext = NULL;
    GAppLaunchContext *ctx;
    PyObject *pyfile_list = Py_None;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "|OO:gio.AppInfo.launch",
				     kwlist,
				     &pyfile_list, &pycontext))
        return NULL;

    if (!pygio_check_launch_context(pycontext, &ctx))
	return NULL;

    if (pyfile_list == Py_None)
        file_list = NULL;

    else if (PySequence_Check (pyfile_list))
        file_list = pygio_pylist_to_gfile_glist(pyfile_list);

    else {
        PyErr_SetString(PyExc_TypeError,
                        "file_list should be a list of strings or None");
        return NULL;
    }

    ret = g_app_info_launch(G_APP_INFO(self->obj),
                            file_list, ctx, &error);

    g_list_free(file_list);

    if (pyg_error_check(&error))
        return NULL;

    return PyBool_FromLong(ret);
}
#line 6021 "gio.c"


static PyObject *
_wrap_g_app_info_supports_uris(PyGObject *self)
{
    int ret;

    
    ret = g_app_info_supports_uris(G_APP_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_app_info_supports_files(PyGObject *self)
{
    int ret;

    
    ret = g_app_info_supports_files(G_APP_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

#line 73 "gappinfo.override"
static PyObject *
_wrap_g_app_info_launch_uris(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "files", "launch_context", NULL };

    GList *file_list = NULL;
    PyGObject *pycontext = NULL;
    GAppLaunchContext *ctx;
    PyObject *pyfile_list = Py_None;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "|OO:gio.AppInfo.launch_uris",
				     kwlist,
				     &pyfile_list, &pycontext))
        return NULL;

    if (!pygio_check_launch_context(pycontext, &ctx))
	return NULL;

    if (pyfile_list == Py_None)
        file_list = NULL;

    else if (PySequence_Check (pyfile_list))
        file_list = pygio_pylist_to_uri_glist(pyfile_list);

    else {
        PyErr_SetString(PyExc_TypeError,
                        "file_list should be a list of strings or None");
        return NULL;
    }

    ret = g_app_info_launch_uris(G_APP_INFO(self->obj),
                                 file_list, ctx, &error);

    g_list_free(file_list);

    if (pyg_error_check(&error))
        return NULL;

    return PyBool_FromLong(ret);
}
#line 6092 "gio.c"


static PyObject *
_wrap_g_app_info_should_show(PyGObject *self)
{
    int ret;

    
    ret = g_app_info_should_show(G_APP_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_app_info_set_as_default_for_type(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "content_type", NULL };
    char *content_type;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GAppInfo.set_as_default_for_type", kwlist, &content_type))
        return NULL;
    
    ret = g_app_info_set_as_default_for_type(G_APP_INFO(self->obj), content_type, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_app_info_set_as_default_for_extension(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "extension", NULL };
    char *extension;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GAppInfo.set_as_default_for_extension", kwlist, &extension))
        return NULL;
    
    ret = g_app_info_set_as_default_for_extension(G_APP_INFO(self->obj), extension, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_app_info_add_supports_type(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "content_type", NULL };
    char *content_type;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GAppInfo.add_supports_type", kwlist, &content_type))
        return NULL;
    
    ret = g_app_info_add_supports_type(G_APP_INFO(self->obj), content_type, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_app_info_can_remove_supports_type(PyGObject *self)
{
    int ret;

    
    ret = g_app_info_can_remove_supports_type(G_APP_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_app_info_remove_supports_type(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "content_type", NULL };
    char *content_type;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GAppInfo.remove_supports_type", kwlist, &content_type))
        return NULL;
    
    ret = g_app_info_remove_supports_type(G_APP_INFO(self->obj), content_type, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGAppInfo_methods[] = {
    { "dup", (PyCFunction)_wrap_g_app_info_dup, METH_NOARGS,
      NULL },
    { "equal", (PyCFunction)_wrap_g_app_info_equal, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_id", (PyCFunction)_wrap_g_app_info_get_id, METH_NOARGS,
      NULL },
    { "get_name", (PyCFunction)_wrap_g_app_info_get_name, METH_NOARGS,
      NULL },
    { "get_description", (PyCFunction)_wrap_g_app_info_get_description, METH_NOARGS,
      NULL },
    { "get_executable", (PyCFunction)_wrap_g_app_info_get_executable, METH_NOARGS,
      NULL },
    { "get_icon", (PyCFunction)_wrap_g_app_info_get_icon, METH_NOARGS,
      NULL },
    { "launch", (PyCFunction)_wrap_g_app_info_launch, METH_VARARGS|METH_KEYWORDS,
      (char *) "launch (files=None, launch_context=None) -> gboolean\n"
"\n"
"Launches the application. Passes files to the launched application\n"
"as arguments, using the optional launch_context to get information\n"
"about the details of the launcher (like what screen it is on).\n"
"On error, error will be set accordingly.\n\n"
"Note that even if the launch is successful the application launched\n"
"can fail to start if it runs into problems during startup.\n"
"There is no way to detect this.\n\n"
"Some URIs can be changed when passed through a gio.File\n"
"(for instance unsupported uris with strange formats like mailto:),\n"
"so if you have a textual uri you want to pass in as argument,\n"
"consider using gio.AppInfo.launch_uris() instead." },
    { "supports_uris", (PyCFunction)_wrap_g_app_info_supports_uris, METH_NOARGS,
      NULL },
    { "supports_files", (PyCFunction)_wrap_g_app_info_supports_files, METH_NOARGS,
      NULL },
    { "launch_uris", (PyCFunction)_wrap_g_app_info_launch_uris, METH_VARARGS|METH_KEYWORDS,
      (char *) "launch_uris (files=None, launch_context=None) -> gboolean\n"
"\n"
"Launches the application. Passes files to the launched application\n"
"as arguments, using the optional launch_context to get information\n"
"about the details of the launcher (like what screen it is on).\n"
"On error, error will be set accordingly.\n\n"
"Note that even if the launch is successful the application launched\n"
"can fail to start if it runs into problems during startup.\n"
"There is no way to detect this.\n\n" },
    { "should_show", (PyCFunction)_wrap_g_app_info_should_show, METH_NOARGS,
      NULL },
    { "set_as_default_for_type", (PyCFunction)_wrap_g_app_info_set_as_default_for_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_as_default_for_extension", (PyCFunction)_wrap_g_app_info_set_as_default_for_extension, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "add_supports_type", (PyCFunction)_wrap_g_app_info_add_supports_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "can_remove_supports_type", (PyCFunction)_wrap_g_app_info_can_remove_supports_type, METH_NOARGS,
      NULL },
    { "remove_supports_type", (PyCFunction)_wrap_g_app_info_remove_supports_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 194 "gappinfo.override"
static PyObject *
_wrap_g_app_info_tp_repr(PyGObject *self)
{
    const char *name = g_app_info_get_name(G_APP_INFO(self->obj));
    gchar *representation;
    PyObject *result;

    representation = g_strdup_printf("<%s at %p: %s>",
				     self->ob_type->tp_name, self,
				     name ? name : "UNKNOWN NAME");

    result = PyString_FromString(representation);
    g_free(representation);
    return result;
}
#line 6269 "gio.c"


#line 163 "gappinfo.override"
static PyObject *
_wrap_g_app_info_tp_richcompare(PyGObject *self, PyGObject *other, int op)
{
    PyObject *result;

    if (PyObject_TypeCheck(self, &PyGAppInfo_Type)
        && PyObject_TypeCheck(other, &PyGAppInfo_Type)) {
        GAppInfo *info1 = G_APP_INFO(self->obj);
        GAppInfo *info2 = G_APP_INFO(other->obj);

        switch (op) {
        case Py_EQ:
            result = (g_app_info_equal(info1, info2)
                      ? Py_True : Py_False);
            break;
        case Py_NE:
            result = (!g_app_info_equal(info1, info2)
                      ? Py_True : Py_False);
            break;
        default:
            result = Py_NotImplemented;
        }
    }
    else
        result = Py_NotImplemented;

    Py_INCREF(result);
    return result;
}
#line 6302 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGAppInfo_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.AppInfo",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)_wrap_g_app_info_tp_repr,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)_wrap_g_app_info_tp_richcompare,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGAppInfo_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GAsyncResult ----------- */

static PyObject *
_wrap_g_async_result_get_source_object(PyGObject *self)
{
    GObject *ret;

    
    ret = g_async_result_get_source_object(G_ASYNC_RESULT(self->obj));
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static const PyMethodDef _PyGAsyncResult_methods[] = {
    { "get_source_object", (PyCFunction)_wrap_g_async_result_get_source_object, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGAsyncResult_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.AsyncResult",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGAsyncResult_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GDrive ----------- */

static PyObject *
_wrap_g_drive_get_name(PyGObject *self)
{
    gchar *ret;

    
    ret = g_drive_get_name(G_DRIVE(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_drive_get_icon(PyGObject *self)
{
    PyObject *py_ret;
    GIcon *ret;

    
    ret = g_drive_get_icon(G_DRIVE(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_drive_has_volumes(PyGObject *self)
{
    int ret;

    
    ret = g_drive_has_volumes(G_DRIVE(self->obj));
    
    return PyBool_FromLong(ret);

}

#line 237 "gio.override"
static PyObject *
_wrap_g_drive_get_volumes (PyGObject *self)
{
  GList *list, *l;
  PyObject *ret;

  pyg_begin_allow_threads;

  list = g_drive_get_volumes (G_DRIVE (self->obj));

  pyg_end_allow_threads;

  ret = PyList_New(0);
  for (l = list; l; l = l->next) {
    GVolume *volume = l->data;
    PyObject *item = pygobject_new((GObject *)volume);
    PyList_Append(ret, item);
    Py_DECREF(item);
    g_object_unref(volume);
  }
  g_list_free(list);

  return ret;
}
#line 6490 "gio.c"


static PyObject *
_wrap_g_drive_is_media_removable(PyGObject *self)
{
    int ret;

    
    ret = g_drive_is_media_removable(G_DRIVE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_drive_has_media(PyGObject *self)
{
    int ret;

    
    ret = g_drive_has_media(G_DRIVE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_drive_is_media_check_automatic(PyGObject *self)
{
    int ret;

    
    ret = g_drive_is_media_check_automatic(G_DRIVE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_drive_can_poll_for_media(PyGObject *self)
{
    int ret;

    
    ret = g_drive_can_poll_for_media(G_DRIVE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_drive_can_eject(PyGObject *self)
{
    int ret;

    
    ret = g_drive_can_eject(G_DRIVE(self->obj));
    
    return PyBool_FromLong(ret);

}

#line 263 "gio.override"
static PyObject *
_wrap_g_drive_eject(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    GMountUnmountFlags flags = G_MOUNT_UNMOUNT_NONE;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OOO:gio.Drive.eject",
				     kwlist,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_UNMOUNT_FLAGS,
					py_flags, (gpointer) &flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_drive_eject(G_DRIVE(self->obj),
		  flags,
		  cancellable,
		  (GAsyncReadyCallback) async_result_callback_marshal,
		  notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 6600 "gio.c"


static PyObject *
_wrap_g_drive_eject_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GDrive.eject_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_drive_eject_finish(G_DRIVE(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 311 "gio.override"
static PyObject *
_wrap_g_drive_poll_for_media(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OO:gio.Drive.eject",
				     kwlist,
				     &notify->callback,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    pyg_begin_allow_threads;

    g_drive_poll_for_media(G_DRIVE(self->obj),
			   cancellable,
			   (GAsyncReadyCallback) async_result_callback_marshal,
			   notify);
    
    pyg_end_allow_threads;

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 6665 "gio.c"


static PyObject *
_wrap_g_drive_poll_for_media_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GDrive.poll_for_media_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_drive_poll_for_media_finish(G_DRIVE(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGDrive_methods[] = {
    { "get_name", (PyCFunction)_wrap_g_drive_get_name, METH_NOARGS,
      NULL },
    { "get_icon", (PyCFunction)_wrap_g_drive_get_icon, METH_NOARGS,
      NULL },
    { "has_volumes", (PyCFunction)_wrap_g_drive_has_volumes, METH_NOARGS,
      NULL },
    { "get_volumes", (PyCFunction)_wrap_g_drive_get_volumes, METH_NOARGS,
      NULL },
    { "is_media_removable", (PyCFunction)_wrap_g_drive_is_media_removable, METH_NOARGS,
      NULL },
    { "has_media", (PyCFunction)_wrap_g_drive_has_media, METH_NOARGS,
      NULL },
    { "is_media_check_automatic", (PyCFunction)_wrap_g_drive_is_media_check_automatic, METH_NOARGS,
      NULL },
    { "can_poll_for_media", (PyCFunction)_wrap_g_drive_can_poll_for_media, METH_NOARGS,
      NULL },
    { "can_eject", (PyCFunction)_wrap_g_drive_can_eject, METH_NOARGS,
      NULL },
    { "eject", (PyCFunction)_wrap_g_drive_eject, METH_VARARGS|METH_KEYWORDS,
      (char *) "D.eject(callback, [flags, [cancellable, [user_data]]]) -> start ejecting\n"
"Asynchronously ejects a drive. When the operation is finished, callback\n"
"will be called. You can then call gio.Drive.eject_finish to obtain the\n"
"result of the operation." },
    { "eject_finish", (PyCFunction)_wrap_g_drive_eject_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "poll_for_media", (PyCFunction)_wrap_g_drive_poll_for_media, METH_VARARGS|METH_KEYWORDS,
      (char *) "D.poll_for_media(callback, [cancellable, [user_data]]) -> start polling\n"
"Asynchronously polls drive to see if media has been inserted or removed.\n"
"When the operation is finished, callback will be called. You can then\n"
"call gio.Drive.poll_for_media_finish to obtain the result of the\n"
"operation." },
    { "poll_for_media_finish", (PyCFunction)_wrap_g_drive_poll_for_media_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 355 "gio.override"
static PyObject *
_wrap_g_drive_tp_repr(PyGObject *self)
{
    char *name = g_drive_get_name(G_DRIVE(self->obj));
    gchar *representation;
    PyObject *result;

    if (name) {
	representation = g_strdup_printf("<%s at %p: %s>", self->ob_type->tp_name, self, name);
	g_free(name);
    }
    else
	representation = g_strdup_printf("<%s at %p: UNKNOWN NAME>", self->ob_type->tp_name, self);

    result = PyString_FromString(representation);
    g_free(representation);
    return result;
}
#line 6743 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGDrive_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.Drive",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)_wrap_g_drive_tp_repr,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGDrive_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GFile ----------- */

static PyObject *
_wrap_g_file_dup(PyGObject *self)
{
    PyObject *py_ret;
    GFile *ret;

    
    ret = g_file_dup(G_FILE(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_equal(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "file2", NULL };
    PyGObject *file2;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.equal", kwlist, &PyGFile_Type, &file2))
        return NULL;
    
    ret = g_file_equal(G_FILE(self->obj), G_FILE(file2->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_get_basename(PyGObject *self)
{
    gchar *ret;

    
    ret = g_file_get_basename(G_FILE(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_get_path(PyGObject *self)
{
    gchar *ret;

    
    ret = g_file_get_path(G_FILE(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_get_uri(PyGObject *self)
{
    gchar *ret;

    
    ret = g_file_get_uri(G_FILE(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_get_parse_name(PyGObject *self)
{
    gchar *ret;

    
    ret = g_file_get_parse_name(G_FILE(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_get_parent(PyGObject *self)
{
    PyObject *py_ret;
    GFile *ret;

    
    ret = g_file_get_parent(G_FILE(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_get_child(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "name", NULL };
    char *name;
    PyObject *py_ret;
    GFile *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFile.get_child", kwlist, &name))
        return NULL;
    
    ret = g_file_get_child(G_FILE(self->obj), name);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_get_child_for_display_name(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "display_name", NULL };
    char *display_name;
    PyObject *py_ret;
    GFile *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFile.get_child_for_display_name", kwlist, &display_name))
        return NULL;
    
    ret = g_file_get_child_for_display_name(G_FILE(self->obj), display_name, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_has_prefix(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "descendant", NULL };
    PyGObject *descendant;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.has_prefix", kwlist, &PyGFile_Type, &descendant))
        return NULL;
    
    ret = g_file_has_prefix(G_FILE(self->obj), G_FILE(descendant->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_get_relative_path(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "descendant", NULL };
    PyGObject *descendant;
    gchar *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.get_relative_path", kwlist, &PyGFile_Type, &descendant))
        return NULL;
    
    ret = g_file_get_relative_path(G_FILE(self->obj), G_FILE(descendant->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_resolve_relative_path(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "relative_path", NULL };
    char *relative_path;
    PyObject *py_ret;
    GFile *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFile.resolve_relative_path", kwlist, &relative_path))
        return NULL;
    
    ret = g_file_resolve_relative_path(G_FILE(self->obj), relative_path);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_is_native(PyGObject *self)
{
    int ret;

    
    ret = g_file_is_native(G_FILE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_has_uri_scheme(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "uri_scheme", NULL };
    char *uri_scheme;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GFile.has_uri_scheme", kwlist, &uri_scheme))
        return NULL;
    
    ret = g_file_has_uri_scheme(G_FILE(self->obj), uri_scheme);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_get_uri_scheme(PyGObject *self)
{
    gchar *ret;

    
    ret = g_file_get_uri_scheme(G_FILE(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_file_read(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    GFileInputStream *ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFile.read", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_read(G_FILE(self->obj), (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 126 "gfile.override"
static PyObject *
_wrap_g_file_read_async(PyGObject *self,
			PyObject *args,
			PyObject *kwargs)
{
  static char *kwlist[] = { "callback", "io_priority",
			    "cancellable", "user_data", NULL };
  int io_priority = G_PRIORITY_DEFAULT;
  PyGObject *pycancellable = NULL;
  GCancellable *cancellable;
  PyGIONotify *notify;

  notify = pygio_notify_new();

  if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                   "O|iOO:File.read_async",
                                   kwlist,
                                   &notify->callback,
                                   &io_priority,
                                   &pycancellable,
                                   &notify->data))
      goto error;

  if (!pygio_notify_callback_is_valid(notify))
      goto error;

  if (!pygio_check_cancellable(pycancellable, &cancellable))
      goto error;

  pygio_notify_reference_callback(notify);

  g_file_read_async(G_FILE(self->obj),
                    io_priority,
                    cancellable,
                    (GAsyncReadyCallback)async_result_callback_marshal,
                    notify);

  Py_INCREF(Py_None);
  return Py_None;

 error:
  pygio_notify_free(notify);
  return NULL;
}
#line 7127 "gio.c"


static PyObject *
_wrap_g_file_read_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    PyObject *py_ret;
    GFileInputStream *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.read_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_read_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_append_to(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "flags", "cancellable", NULL };
    PyObject *py_flags = NULL, *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    PyGObject *py_cancellable = NULL;
    GFileOutputStream *ret;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|OO:GFile.append_to", kwlist, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_append_to(G_FILE(self->obj), flags, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_create(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "flags", "cancellable", NULL };
    PyObject *py_flags = NULL, *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    PyGObject *py_cancellable = NULL;
    GFileOutputStream *ret;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|OO:GFile.create", kwlist, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_create(G_FILE(self->obj), flags, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_replace(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "etag", "make_backup", "flags", "cancellable", NULL };
    int make_backup;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *etag;
    PyObject *py_flags = NULL, *py_ret;
    PyGObject *py_cancellable = NULL;
    GFileOutputStream *ret;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"si|OO:GFile.replace", kwlist, &etag, &make_backup, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_replace(G_FILE(self->obj), etag, make_backup, flags, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 791 "gfile.override"
static PyObject *
_wrap_g_file_append_to_async(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "io_priority",
                              "cancellable", "user_data", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;
    PyObject *py_flags = NULL;
    int io_priority = G_PRIORITY_DEFAULT;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OiOO:File.append_to_async",
                                      kwlist,
                                      &notify->callback,
                                      &flags, &io_priority,
                                      &pycancellable,
                                      &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS,
                                        py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_append_to_async(G_FILE(self->obj), flags, io_priority, cancellable,
                           (GAsyncReadyCallback)async_result_callback_marshal,
                           notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 7303 "gio.c"


static PyObject *
_wrap_g_file_append_to_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    PyObject *py_ret;
    GFileOutputStream *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.append_to_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_append_to_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 839 "gfile.override"
static PyObject *
_wrap_g_file_create_async(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "io_priority",
                              "cancellable", "user_data", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;
    PyObject *py_flags = NULL;
    int io_priority = G_PRIORITY_DEFAULT;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OiOO:File.create_async",
                                      kwlist,
                                      &notify->callback,
                                      &flags, &io_priority,
                                      &pycancellable,
                                      &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS,
                                        py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_create_async(G_FILE(self->obj), flags, io_priority, cancellable,
                        (GAsyncReadyCallback)async_result_callback_marshal,
                        notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 7375 "gio.c"


static PyObject *
_wrap_g_file_create_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    PyObject *py_ret;
    GFileOutputStream *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.create_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_create_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 887 "gfile.override"
static PyObject *
_wrap_g_file_replace_async(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "etag", "make_backup", "flags",
                              "io_priority", "cancellable", "user_data", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;
    PyObject *py_flags = NULL;
    int io_priority = G_PRIORITY_DEFAULT;
    char *etag = NULL;
    gboolean make_backup = TRUE;
    PyObject *py_backup = Py_True;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|zOOiOO:File.replace_async",
                                      kwlist,
                                      &notify->callback,
                                      &etag, &py_backup,
                                      &flags, &io_priority,
                                      &pycancellable,
                                      &notify->data))
        goto error;

    make_backup = PyObject_IsTrue(py_backup) ? TRUE : FALSE;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS,
                                        py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_replace_async(G_FILE(self->obj), etag, make_backup, flags,
                         io_priority, cancellable,
                         (GAsyncReadyCallback)async_result_callback_marshal,
                         notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 7454 "gio.c"


static PyObject *
_wrap_g_file_replace_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    PyObject *py_ret;
    GFileOutputStream *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.replace_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_replace_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_query_exists(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFile.query_exists", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_query_exists(G_FILE(self->obj), (GCancellable *) cancellable);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_query_info(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attributes", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL, *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attributes;
    GFileInfo *ret;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|OO:GFile.query_info", kwlist, &attributes, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_query_info(G_FILE(self->obj), attributes, flags, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 942 "gfile.override"
static PyObject *
_wrap_g_file_query_info_async(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "attributes", "flags",
                              "io_priority", "cancellable", "user_data", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL;
    int io_priority = G_PRIORITY_DEFAULT;
    char *attributes;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "Os|OiOO:File.query_info_async",
                                      kwlist,
                                      &notify->callback,
                                      &attributes,
                                      &flags, &io_priority,
                                      &pycancellable,
                                      &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS,
                                        py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_query_info_async(G_FILE(self->obj), attributes, flags,
                         io_priority, cancellable,
                         (GAsyncReadyCallback)async_result_callback_marshal,
                         notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 7589 "gio.c"


static PyObject *
_wrap_g_file_query_info_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    PyObject *py_ret;
    GFileInfo *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.query_info_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_query_info_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_query_filesystem_info(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attributes", "cancellable", NULL };
    PyObject *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attributes;
    GFileInfo *ret;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|O:GFile.query_filesystem_info", kwlist, &attributes, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_query_filesystem_info(G_FILE(self->obj), attributes, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_find_enclosing_mount(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    GMount *ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFile.find_enclosing_mount", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_find_enclosing_mount(G_FILE(self->obj), (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_find_enclosing_mount_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    GMount *ret;
    GError *error = NULL;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.find_enclosing_mount_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_find_enclosing_mount_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_enumerate_children(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attributes", "flags", "cancellable", NULL };
    GFileEnumerator *ret;
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL, *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attributes;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|OO:GFile.enumerate_children", kwlist, &attributes, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_enumerate_children(G_FILE(self->obj), attributes, flags, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 300 "gfile.override"
static PyObject *
_wrap_g_file_enumerate_children_async(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attributes", "callback", "flags",
			      "io_priority", "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    char *attributes;
    PyObject *py_flags = NULL;
    int io_priority = G_PRIORITY_DEFAULT;
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    GCancellable *cancellable = NULL;
    PyGObject *py_cancellable = NULL;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "sO|OiOO:GFile.enumerate_children_async",
				     kwlist,
				     &attributes,
				     &notify->callback,
				     &py_flags,
				     &io_priority,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
	goto error;

    pygio_notify_reference_callback(notify);

    g_file_enumerate_children_async(G_FILE(self->obj),
				    attributes,
				    flags,
				    io_priority,
				    (GCancellable *) cancellable,
				    (GAsyncReadyCallback)async_result_callback_marshal,
				    notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 7788 "gio.c"


static PyObject *
_wrap_g_file_enumerate_children_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    GFileEnumerator *ret;
    GError *error = NULL;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.enumerate_children_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_enumerate_children_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_set_display_name(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "display_name", "cancellable", NULL };
    PyObject *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *display_name;
    GFile *ret;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|O:GFile.set_display_name", kwlist, &display_name, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_set_display_name(G_FILE(self->obj), display_name, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_set_display_name_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    PyObject *py_ret;
    GFile *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.set_display_name_finish", kwlist, &PyGAsyncResult_Type, &res))
        return NULL;
    
    ret = g_file_set_display_name_finish(G_FILE(self->obj), G_ASYNC_RESULT(res->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_delete(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFile.delete", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_delete(G_FILE(self->obj), (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_trash(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFile.trash", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_trash(G_FILE(self->obj), (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 516 "gfile.override"
static PyObject *
_wrap_g_file_copy(PyGObject *self,
		  PyObject *args,
		  PyObject *kwargs)
{
    static char *kwlist[] = { "destination", "progress_callback",
			      "flags", "cancellable", 
			      "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *destination = NULL;
    PyGObject *py_cancellable = NULL;
    GFileCopyFlags flags = G_FILE_COPY_NONE;
    GCancellable *cancellable;
    int ret;
    GError *error = NULL;
    GFileProgressCallback callback = NULL;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!|OOOO:File.copy",
				     kwlist,
				     &PyGFile_Type,
				     &destination,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (pygio_notify_using_optional_callback(notify)) {
        callback = (GFileProgressCallback)file_progress_callback_marshal;
        if (!pygio_notify_callback_is_valid(notify))
            goto error;
    }

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_COPY_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    /* No need to reference callback here, because it will be used
     * only while this function is in progress. */

    pyg_begin_allow_threads;

    ret = g_file_copy(G_FILE(self->obj),
		      G_FILE(destination->obj),
		      flags,
		      cancellable,
		      callback,
		      notify,
		      &error);

    pyg_end_allow_threads;

    if (pyg_error_check(&error))
        goto error;

    pygio_notify_free(notify);
    return PyBool_FromLong(ret);

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 7993 "gio.c"


#line 587 "gfile.override"
static PyObject *
_wrap_g_file_move(PyGObject *self,
		  PyObject *args,
		  PyObject *kwargs)
{
    static char *kwlist[] = { "destination", "progress_callback",
			      "flags", "cancellable", 
			      "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *destination = NULL;
    PyGObject *py_cancellable = NULL;
    GFileCopyFlags flags = G_FILE_COPY_NONE;
    GCancellable *cancellable;
    int ret;
    GError *error = NULL;
    GFileProgressCallback callback = NULL;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!|OOOO:File.move",
				     kwlist,
				     &PyGFile_Type,
				     &destination,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (pygio_notify_using_optional_callback(notify)) {
        callback = (GFileProgressCallback)file_progress_callback_marshal;
        if (!pygio_notify_callback_is_valid(notify))
            goto error;
    }

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_COPY_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    /* No need to reference callback here, because it will be used
     * only while this function is in progress. */

    pyg_begin_allow_threads;

    ret = g_file_move(G_FILE(self->obj),
		      G_FILE(destination->obj),
		      flags,
		      cancellable,
		      callback,
		      notify,
		      &error);
    
    pyg_end_allow_threads;

    if (pyg_error_check(&error))
        goto error;

    pygio_notify_free(notify);
    return PyBool_FromLong(ret);

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 8066 "gio.c"


static PyObject *
_wrap_g_file_make_directory(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFile.make_directory", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_make_directory(G_FILE(self->obj), (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_make_symbolic_link(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "symlink_value", "cancellable", NULL };
    char *symlink_value;
    PyGObject *py_cancellable = NULL;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s|O:GFile.make_symbolic_link", kwlist, &symlink_value, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    pyg_begin_allow_threads;
    ret = g_file_make_symbolic_link(G_FILE(self->obj), symlink_value, (GCancellable *) cancellable, &error);
    pyg_end_allow_threads;
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 699 "gfile.override"
static PyObject *
_wrap_g_file_query_settable_attributes(PyGObject *self,
                                       PyObject *args,
                                       PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable = NULL;
    GFileAttributeInfoList *ret;
    GError *error = NULL;
    gint i, n_infos;
    GFileAttributeInfo *infos;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "|O:GFile.query_settable_attributes",
                                     kwlist, &pycancellable))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        return NULL;

    ret = g_file_query_settable_attributes(G_FILE(self->obj),
                                           (GCancellable *) cancellable,
                                           &error);
    if (pyg_error_check(&error))
        return NULL;

    n_infos = ret->n_infos;
    infos = ret->infos;

    if (n_infos > 0) {
        py_ret = PyList_New(n_infos);
        for (i = 0; i < n_infos; i++) {
            PyList_SetItem(py_ret, i, pyg_file_attribute_info_new(&infos[i]));
        }
        g_file_attribute_info_list_unref(ret);
        return py_ret;

    } else {
        Py_INCREF(Py_None);
        return Py_None;
    }
}
#line 8171 "gio.c"


#line 745 "gfile.override"
static PyObject *
_wrap_g_file_query_writable_namespaces(PyGObject *self,
                                       PyObject *args,
                                       PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable = NULL;
    GFileAttributeInfoList *ret;
    GError *error = NULL;
    gint i, n_infos;
    GFileAttributeInfo *infos;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "|O:GFile.query_writable_namespaces",
                                     kwlist, &pycancellable))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        return NULL;

    ret = g_file_query_writable_namespaces(G_FILE(self->obj),
                                           (GCancellable *) cancellable,
                                           &error);
    if (pyg_error_check(&error))
        return NULL;

    n_infos = ret->n_infos;
    infos = ret->infos;

    if (n_infos > 0) {
        py_ret = PyList_New(n_infos);
        for (i = 0; i < n_infos; i++) {
            PyList_SetItem(py_ret, i, pyg_file_attribute_info_new(&infos[i]));
        }
        g_file_attribute_info_list_unref(ret);
        return py_ret;

    } else {
        Py_INCREF(Py_None);
        return Py_None;
    }
}
#line 8219 "gio.c"


#line 658 "gfile.override"
static PyObject *
_wrap_g_file_set_attribute(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "type", "value_p",
                              "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    int ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attribute;
    PyObject *py_type = NULL, *py_flags = NULL, *value_p;
    PyGObject *pycancellable = NULL;
    GFileAttributeType type;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sOO|OO:GFile.set_attribute",
                                     kwlist, &attribute, &py_type, &value_p,
                                     &py_flags, &pycancellable))
        return NULL;

    if (pyg_enum_get_value(G_TYPE_FILE_ATTRIBUTE_TYPE, py_type,
                            (gpointer)&type))
        return NULL;

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags,
                                        (gpointer)&flags))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        return NULL;

    ret = g_file_set_attribute(G_FILE(self->obj), attribute, type,
                               (gpointer)value_p, flags, (GCancellable *)
                               cancellable, &error);

    if (pyg_error_check(&error))
        return NULL;

    return PyBool_FromLong(ret);
}
#line 8262 "gio.c"


static PyObject *
_wrap_g_file_set_attributes_from_info(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "info", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    int ret;
    PyGObject *info, *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!|OO:GFile.set_attributes_from_info", kwlist, &PyGFileInfo_Type, &info, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_set_attributes_from_info(G_FILE(self->obj), G_FILE_INFO(info->obj), flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_set_attribute_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "value", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attribute, *value;
    int ret;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ss|OO:GFile.set_attribute_string", kwlist, &attribute, &value, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_set_attribute_string(G_FILE(self->obj), attribute, value, flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_set_attribute_byte_string(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "value", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attribute, *value;
    int ret;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ss|OO:GFile.set_attribute_byte_string", kwlist, &attribute, &value, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_set_attribute_byte_string(G_FILE(self->obj), attribute, value, flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_set_attribute_uint32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "value", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attribute;
    int ret;
    unsigned long value;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sk|OO:GFile.set_attribute_uint32", kwlist, &attribute, &value, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_set_attribute_uint32(G_FILE(self->obj), attribute, value, flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_set_attribute_int32(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "value", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    int value, ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attribute;
    PyObject *py_flags = NULL;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"si|OO:GFile.set_attribute_int32", kwlist, &attribute, &value, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_set_attribute_int32(G_FILE(self->obj), attribute, value, flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_set_attribute_uint64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "value", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_value = NULL, *py_flags = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attribute;
    int ret;
    guint64 value;
    PyGObject *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sO!|OO:GFile.set_attribute_uint64", kwlist, &attribute, &PyLong_Type, &py_value, &py_flags, &py_cancellable))
        return NULL;
    value = PyLong_AsUnsignedLongLong(py_value);
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_set_attribute_uint64(G_FILE(self->obj), attribute, value, flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_set_attribute_int64(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "attribute", "value", "flags", "cancellable", NULL };
    GFileQueryInfoFlags flags = G_FILE_QUERY_INFO_NONE;
    PyObject *py_flags = NULL;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    char *attribute;
    int ret;
    PyGObject *py_cancellable = NULL;
    gint64 value;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"sL|OO:GFile.set_attribute_int64", kwlist, &attribute, &value, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_set_attribute_int64(G_FILE(self->obj), attribute, value, flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 461 "gfile.override"
static PyObject *
_wrap_g_file_mount_enclosing_volume(PyGObject *self,
				    PyObject *args,
				    PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "mount_operation",
			      "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *mount_operation;
    PyGObject *py_cancellable = NULL;
    GMountMountFlags flags = G_MOUNT_MOUNT_NONE;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!O|OOO:File.mount_enclosing_volume",
				     kwlist,
				     &PyGMountOperation_Type,
				     &mount_operation,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_MOUNT_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_mount_enclosing_volume(G_FILE(self->obj),
				  flags,
				  G_MOUNT_OPERATION(mount_operation->obj),
				  cancellable,
				  (GAsyncReadyCallback)async_result_callback_marshal,
				  notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 8553 "gio.c"


static PyObject *
_wrap_g_file_mount_enclosing_volume_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.mount_enclosing_volume_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_file_mount_enclosing_volume_finish(G_FILE(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 355 "gfile.override"
static PyObject *
_wrap_g_file_mount_mountable(PyGObject *self,
			     PyObject *args,
			     PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "mount_operation",
			      "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *mount_operation;
    PyGObject *py_cancellable = NULL;
    GMountMountFlags flags = G_MOUNT_MOUNT_NONE;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!O|OOO:File.mount_mountable",
				     kwlist,
				     &PyGMountOperation_Type,
				     &mount_operation,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_MOUNT_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_mount_mountable(G_FILE(self->obj),
			   flags,
			   G_MOUNT_OPERATION(mount_operation->obj),
			   cancellable,
			   (GAsyncReadyCallback)async_result_callback_marshal,
			   notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 8629 "gio.c"


static PyObject *
_wrap_g_file_mount_mountable_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    PyObject *py_ret;
    GFile *ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.mount_mountable_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_file_mount_mountable_finish(G_FILE(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 410 "gfile.override"
static PyObject *
_wrap_g_file_unmount_mountable(PyGObject *self,
			     PyObject *args,
			     PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags",
			      "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *py_cancellable = NULL;
    GMountUnmountFlags flags = G_MOUNT_UNMOUNT_NONE;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OOO:File.unmount_mountable",
				     kwlist,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_UNMOUNT_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_unmount_mountable(G_FILE(self->obj),
			     flags,
			     cancellable,
			     (GAsyncReadyCallback)async_result_callback_marshal,
			     notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 8704 "gio.c"


static PyObject *
_wrap_g_file_unmount_mountable_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.unmount_mountable_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_file_unmount_mountable_finish(G_FILE(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_eject_mountable_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GFile.eject_mountable_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_file_eject_mountable_finish(G_FILE(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_copy_attributes(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "destination", "flags", "cancellable", NULL };
    int ret;
    GFileCopyFlags flags = G_FILE_COPY_NONE;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    PyObject *py_flags = NULL;
    PyGObject *destination, *py_cancellable = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!|OO:GFile.copy_attributes", kwlist, &PyGFile_Type, &destination, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_COPY_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_copy_attributes(G_FILE(self->obj), G_FILE(destination->obj), flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_file_monitor_directory(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "flags", "cancellable", NULL };
    PyObject *py_flags = NULL, *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    GFileMonitorFlags flags = G_FILE_MONITOR_NONE;
    PyGObject *py_cancellable = NULL;
    GFileMonitor *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|OO:GFile.monitor_directory", kwlist, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_MONITOR_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_monitor_directory(G_FILE(self->obj), flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_monitor_file(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "flags", "cancellable", NULL };
    PyObject *py_flags = NULL, *py_ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    GFileMonitorFlags flags = G_FILE_MONITOR_NONE;
    PyGObject *py_cancellable = NULL;
    GFileMonitor *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|OO:GFile.monitor_file", kwlist, &py_flags, &py_cancellable))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_MONITOR_FLAGS, py_flags, (gpointer)&flags))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_monitor_file(G_FILE(self->obj), flags, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_file_query_default_handler(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    GAppInfo *ret;
    GCancellable *cancellable = NULL;
    GError *error = NULL;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"|O:GFile.query_default_handler", kwlist, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_file_query_default_handler(G_FILE(self->obj), (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

#line 172 "gfile.override"
static PyObject *
_wrap_g_file_load_contents(PyGObject *self,
                           PyObject *args,
                           PyObject *kwargs)
{
    static char *kwlist[] = { "cancellable", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    gchar *contents, *etag_out;
    gsize length;
    GError *error = NULL;
    gboolean ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "|O:File.load_contents",
                                      kwlist,
                                      &pycancellable))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
	return NULL;

    pyg_begin_allow_threads;

    ret = g_file_load_contents(G_FILE(self->obj), cancellable,
                               &contents, &length, &etag_out, &error);

    pyg_end_allow_threads;

    if (pyg_error_check(&error))
        return NULL;

    if (ret) {
        PyObject *pyret;

        pyret = Py_BuildValue("(s#ks)", contents, length, length, etag_out);
        g_free(contents);
	g_free(etag_out);
        return pyret;
    } else {
        Py_INCREF(Py_None);
        return Py_None;
    }
}
#line 8921 "gio.c"


#line 218 "gfile.override"
static PyObject *
_wrap_g_file_load_contents_async(PyGObject *self,
                                 PyObject *args,
                                 PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "cancellable", "user_data", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OO:File.load_contents_async",
                                      kwlist,
                                      &notify->callback,
                                      &pycancellable,
                                      &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_file_load_contents_async(G_FILE(self->obj),
			       cancellable,
			       (GAsyncReadyCallback)async_result_callback_marshal,
			       notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 8965 "gio.c"


#line 260 "gfile.override"
static PyObject *
_wrap_g_file_load_contents_finish(PyGObject *self,
                           PyObject *args,
                           PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    gchar *contents, *etag_out;
    gsize length;
    GError *error = NULL;
    gboolean ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!:File.load_contents_finish",
                                      kwlist,
                                      &PyGAsyncResult_Type,
                                      &res))
        return NULL;

    ret = g_file_load_contents_finish(G_FILE(self->obj),
                                      G_ASYNC_RESULT(res->obj), &contents,
                                      &length, &etag_out, &error);

    if (pyg_error_check(&error))
        return NULL;

    if (ret) {
        PyObject *pyret;

        pyret = Py_BuildValue("(s#ks)", contents, length, length, etag_out);
        g_free(contents);
	g_free(etag_out);
        return pyret;
    } else {
        Py_INCREF(Py_None);
        return Py_None;
    }
}
#line 9007 "gio.c"


#line 993 "gfile.override"
static PyObject *
_wrap_g_file_replace_contents(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "contents", "etag", "make_backup",
                              "flags", "cancellable", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;
    PyObject *py_flags = NULL;
    gsize length;
    gboolean make_backup = FALSE;
    char *contents;
    char *etag = NULL;
    char *new_etag = NULL;
    GError *error = NULL;
    gboolean ret;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "s#|zbOO:File.replace_contents",
                                     kwlist,
                                     &contents,
                                     &length,
                                     &etag,
                                     &make_backup,
                                     &flags,
                                     &cancellable))
    {
        return NULL;
    }

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS,
                                        py_flags, (gpointer)&flags))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        return NULL;

    pyg_begin_allow_threads;

    ret = g_file_replace_contents(G_FILE(self->obj), contents, length, etag,
                                  make_backup, flags, &new_etag, cancellable,
                                  &error);

    pyg_end_allow_threads;

    if (pyg_error_check(&error))
        return NULL;

    if (ret) {
        py_ret = PyString_FromString(new_etag);
    } else {
        py_ret = Py_None;
        Py_INCREF(py_ret);
    }

    g_free(new_etag);
    return py_ret;
}
#line 9070 "gio.c"


#line 1091 "gfile.override"
static PyObject *
_wrap_g_file_replace_contents_async(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "contents", "callback", "etag", "make_backup",
                              "flags", "cancellable", "user_data", NULL };
    GCancellable *cancellable;
    PyGObject *pycancellable = NULL;
    PyGIONotify *notify;
    GFileCreateFlags flags = G_FILE_CREATE_NONE;
    PyObject *py_flags = NULL;
    gsize length;
    gboolean make_backup = FALSE;
    char *contents;
    char *etag = NULL;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "s#O|zbOOO:File.replace_contents_async",
                                      kwlist,
                                      &contents,
                                      &length,
                                      &notify->callback,
                                      &etag,
                                      &make_backup,
                                      &py_flags,
                                      &pycancellable,
                                      &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS,
                                        py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);
    pygio_notify_copy_buffer(notify, contents, length);

    g_file_replace_contents_async(G_FILE(self->obj),
                                  notify->buffer,
                                  notify->buffer_size,
                                  etag,
                                  make_backup,
                                  flags,
                                  cancellable,
                                  (GAsyncReadyCallback)async_result_callback_marshal,
                                  notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 9134 "gio.c"


#line 1054 "gfile.override"
static PyObject *
_wrap_g_file_replace_contents_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *res;
    gchar *etag_out = NULL;
    GError *error = NULL;
    gboolean ret;
    PyObject *py_ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!:File.replace_contents_finish",
                                      kwlist,
                                      &PyGAsyncResult_Type,
                                      &res))
        return NULL;

    ret = g_file_replace_contents_finish(G_FILE(self->obj),
                                         G_ASYNC_RESULT(res->obj), &etag_out,
                                         &error);

    if (pyg_error_check(&error))
        return NULL;

    if (ret) {
        py_ret = PyString_FromString(etag_out);
        return py_ret;
    } else {
        py_ret = Py_None;
        Py_INCREF(py_ret);
    }

    g_free(etag_out);
    return py_ret;
}
#line 9173 "gio.c"


static const PyMethodDef _PyGFile_methods[] = {
    { "dup", (PyCFunction)_wrap_g_file_dup, METH_NOARGS,
      NULL },
    { "equal", (PyCFunction)_wrap_g_file_equal, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_basename", (PyCFunction)_wrap_g_file_get_basename, METH_NOARGS,
      NULL },
    { "get_path", (PyCFunction)_wrap_g_file_get_path, METH_NOARGS,
      NULL },
    { "get_uri", (PyCFunction)_wrap_g_file_get_uri, METH_NOARGS,
      NULL },
    { "get_parse_name", (PyCFunction)_wrap_g_file_get_parse_name, METH_NOARGS,
      NULL },
    { "get_parent", (PyCFunction)_wrap_g_file_get_parent, METH_NOARGS,
      NULL },
    { "get_child", (PyCFunction)_wrap_g_file_get_child, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_child_for_display_name", (PyCFunction)_wrap_g_file_get_child_for_display_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "has_prefix", (PyCFunction)_wrap_g_file_has_prefix, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_relative_path", (PyCFunction)_wrap_g_file_get_relative_path, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "resolve_relative_path", (PyCFunction)_wrap_g_file_resolve_relative_path, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "is_native", (PyCFunction)_wrap_g_file_is_native, METH_NOARGS,
      NULL },
    { "has_uri_scheme", (PyCFunction)_wrap_g_file_has_uri_scheme, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "get_uri_scheme", (PyCFunction)_wrap_g_file_get_uri_scheme, METH_NOARGS,
      NULL },
    { "read", (PyCFunction)_wrap_g_file_read, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.read([cancellable]) -> input stream\n"
"Opens a file for reading. The result is a GFileInputStream that\n"
"can be used to read the contents of the file.\n"
"\n"
"If cancellable is specified, then the operation can be cancelled\n"
"by triggering the cancellable object from another thread. If the\n"
"operation was cancelled, the error gio.IO_ERROR_CANCELLED will\n"
"be returned. If the file does not exist, the gio.IO_ERROR_NOT_FOUND\n"
"error will be returned. If the file is a directory, the\n"
"gio.IO_ERROR_IS_DIRECTORY error will be returned. Other errors\n"
"are possible too, and depend on what kind of filesystem the file is on." },
    { "read_async", (PyCFunction)_wrap_g_file_read_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.read_async(callback [,io_priority [,cancellable [,user_data]]])\n"
"-> start read\n"
"\n"
"For more details, see gio.File.read() which is the synchronous\n"
"version of this call. Asynchronously opens file for reading.\n"
"When the operation is finished, callback will be called.\n"
"You can then call g_file_read_finish() to get the result of the\n"
"operation.\n" },
    { "read_finish", (PyCFunction)_wrap_g_file_read_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "append_to", (PyCFunction)_wrap_g_file_append_to, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "create", (PyCFunction)_wrap_g_file_create, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "replace", (PyCFunction)_wrap_g_file_replace, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "append_to_async", (PyCFunction)_wrap_g_file_append_to_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.append_to_async(callback [flags, [,io_priority [,cancellable\n"
"                  [,user_data]]]]) -> open for append\n"
"\n"
"Asynchronously opens file for appending.\n"
"For more details, see gio.File.append_to() which is the synchronous\n"
"version of this call. When the operation is finished, callback will\n"
"be called. You can then call F.append_to_finish() to get the result\n"
"of the operation." },
    { "append_to_finish", (PyCFunction)_wrap_g_file_append_to_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "create_async", (PyCFunction)_wrap_g_file_create_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.create_async(callback [flags, [,io_priority [,cancellable\n"
"               [,user_data]]]]) -> file created\n"
"\n"
"Asynchronously creates a new file and returns an output stream for\n"
"writing to it. The file must not already exist.\n"
"For more details, see F.create() which is the synchronous\n"
"version of this call.\n"
"When the operation is finished, callback will be called. You can\n"
"then call F.create_finish() to get the result of the operation." },
    { "create_finish", (PyCFunction)_wrap_g_file_create_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "replace_async", (PyCFunction)_wrap_g_file_replace_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.replace_async(callback [etag, [make_backup, [flags, [io_priority,\n"
"                [cancellable, [user_data]]]]]]) -> file replace\n"
"\n"
"Asynchronously overwrites the file, replacing the contents, possibly\n"
"creating a backup copy of the file first.\n"
"For more details, see F.replace() which is the synchronous\n"
"version of this call.\n"
"When the operation is finished, callback will be called. You can\n"
"then call F.replace_finish() to get the result of the operation." },
    { "replace_finish", (PyCFunction)_wrap_g_file_replace_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_exists", (PyCFunction)_wrap_g_file_query_exists, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_info", (PyCFunction)_wrap_g_file_query_info, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_info_async", (PyCFunction)_wrap_g_file_query_info_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.query_info_async(callback, attributes, [flags, [io_priority,\n"
"                   [cancellable, [user_data]]]]) -> query attributes\n"
"\n"
"Asynchronously gets the requested information about specified file.\n"
"The result is a GFileInfo object that contains key-value attributes\n"
"(such as type or size for the file).\n"
"For more details, see F.query_info() which is the synchronous\n"
"version of this call. \n"
"When the operation is finished, callback will be called. You can\n"
"then call F.query_info_finish() to get the result of the operation.\n" },
    { "query_info_finish", (PyCFunction)_wrap_g_file_query_info_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_filesystem_info", (PyCFunction)_wrap_g_file_query_filesystem_info, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "find_enclosing_mount", (PyCFunction)_wrap_g_file_find_enclosing_mount, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "find_enclosing_mount_finish", (PyCFunction)_wrap_g_file_find_enclosing_mount_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "enumerate_children", (PyCFunction)_wrap_g_file_enumerate_children, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.enumerate_children(attributes, [flags, cancellable]) -> enumerator\n"
"Gets the requested information about the files in a directory.\n"
"The result is a gio.FileEnumerator object that will give out gio.FileInfo\n"
"objects for all the files in the directory.\n"
"The attribute value is a string that specifies the file attributes that\n"
"should be gathered. It is not an error if it's not possible to read a \n"
"particular requested attribute from a file - it just won't be set.\n"
"attribute should be a comma-separated list of attribute or attribute\n"
"wildcards. The wildcard \"*\" means all attributes, and a wildcard like\n"
"\"standard::*\" means all attributes in the standard namespace.\n"
"An example attribute query be \"standard::*,owner::user\". The standard\n"
"attributes are available as defines, like gio.FILE_ATTRIBUTE_STANDARD_NAME.\n"
"\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the operation was\n"
"cancelled, the error gio.ERROR_CANCELLED will be returned.\n"
"\n"
"If the file does not exist, the gio.ERROR_NOT_FOUND error will be returned.\n"
"If the file is not a directory, the gio.FILE_ERROR_NOTDIR error will\n"
"be returned. Other errors are possible too." },
    { "enumerate_children_async", (PyCFunction)_wrap_g_file_enumerate_children_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.enumerate_children_async(attributes, callback,\n"
"                           [flags, io_priority, cancellable, user_data])\n"
"Asynchronously gets the requested information about the files in a\n"
"directory. The result is a GFileEnumerator object that will give out\n"
"GFileInfo objects for all the files in the directory.\n"
"\n"
"For more details, see gio.File.enumerate_children() which is the synchronous\n"
"version of this call.\n"
"\n"
"When the operation is finished, callback will be called. You can then call\n"
"gio.File.enumerate_children_finish() to get the result of the operation." },
    { "enumerate_children_finish", (PyCFunction)_wrap_g_file_enumerate_children_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_display_name", (PyCFunction)_wrap_g_file_set_display_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_display_name_finish", (PyCFunction)_wrap_g_file_set_display_name_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "delete", (PyCFunction)_wrap_g_file_delete, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "trash", (PyCFunction)_wrap_g_file_trash, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "copy", (PyCFunction)_wrap_g_file_copy, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.copy(destination, [callback, flags, cancellable, user_data])\n"
"Copies the file source to the location specified by destination.\n"
"Can not handle recursive copies of directories.\n"
"\n"
"If the flag gio.FILE_COPY_OVERWRITE is specified an already existing\n"
"destination file is overwritten.\n"
"\n"
"If the flag gio.FILE_COPY_NOFOLLOW_SYMLINKS is specified then symlink\n"
"will be copied as symlinks, otherwise the target of the source symlink\n"
"will be copied.\n"
"\n"
"If cancellable is not None, then the operation can be cancelled b\n"
"triggering the cancellable object from another thread.\n"
"If the operation was cancelled, the error gio.ERROR_CANCELLED\n"
"will be returned.\n"
"\n"
"If progress_callback is not None, then the operation can be monitored\n"
"by setting this to a callable. if specified progress_callback_data will\n"
"be passed to this function. It is guaranteed that this callback\n"
"will be called after all data has been transferred with the total number\n"
"of bytes copied during the operation.\n"
"\n"
"If the source file does not exist then the gio.ERROR_NOT_FOUND\n"
"error is returned, independent on the status of the destination.\n"
"\n"
"If gio.FILE_COPY_OVERWRITE is not specified and the target exists\n"
"then the error gio.ERROR_EXISTS is returned.\n"
"\n"
"If trying to overwrite a file over a directory the gio.ERROR_IS_DIRECTORY\n"
"error is returned. If trying to overwrite a directory with a directory\n"
"the gio.ERROR_WOULD_MERGE error is returned.\n"
"\n"
"If the source is a directory and the target does not exist\n"
"or gio.FILE_COPY_OVERWRITE is specified and the target is a file\n"
"then the gio.ERROR_WOULD_RECURSE error is returned.\n"
"\n"
"If you are interested in copying the GFile object itself\n"
"(not the on-disk file), see gio.File.dup()." },
    { "move", (PyCFunction)_wrap_g_file_move, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.move(destination, [callback, flags, cancellable, user_data])\n"
"Tries to move the file or directory source to the location\n"
"specified by destination. If native move operations are\n"
"supported then this is used, otherwise a copy + delete fallback\n"
"is used. The native implementation may support moving directories\n"
"(for instance on moves inside the same filesystem), but the \n"
"fallback code does not.\n"
"\n"
"If the flag gio.FILE_COPY_OVERWRITE is specified an already existing\n"
"destination file is overwritten.\n"
"\n"
"If the flag gio.FILE_COPY_NOFOLLOW_SYMLINKS is specified then symlink\n"
"will be copied as symlinks, otherwise the target of the source symlink\n"
"will be copied.\n"
"\n"
"If cancellable is not None, then the operation can be cancelled b\n"
"triggering the cancellable object from another thread.\n"
"If the operation was cancelled, the error gio.ERROR_CANCELLED\n"
"will be returned.\n"
"\n"
"If progress_callback is not None, then the operation can be monitored\n"
"by setting this to a callable. if specified progress_callback_data will\n"
"be passed to this function. It is guaranteed that this callback\n"
"will be called after all data has been transferred with the total number\n"
"of bytes copied during the operation.\n"
"\n"
"If the source file does not exist then the gio.ERROR_NOT_FOUND\n"
"error is returned, independent on the status of the destination.\n"
"\n"
"If gio.FILE_COPY_OVERWRITE is not specified and the target exists\n"
"then the error gio.ERROR_EXISTS is returned.\n"
"\n"
"If trying to overwrite a file over a directory the gio.ERROR_IS_DIRECTORY\n"
"error is returned. If trying to overwrite a directory with a directory\n"
"the gio.ERROR_WOULD_MERGE error is returned.\n"
"\n"
"If the source is a directory and the target does not exist\n"
"or gio.FILE_COPY_OVERWRITE is specified and the target is a file\n"
"then the gio.ERROR_WOULD_RECURSE error is returned." },
    { "make_directory", (PyCFunction)_wrap_g_file_make_directory, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "make_symbolic_link", (PyCFunction)_wrap_g_file_make_symbolic_link, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_settable_attributes", (PyCFunction)_wrap_g_file_query_settable_attributes, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.query_settable_attributes([cancellable]) -> list\n\n"
"Obtain the list of settable attributes for the file.\n"
"Returns the type and full attribute name of all the attributes that\n"
"can be set on this file. This doesn't mean setting it will always\n"
"succeed though, you might get an access failure, or some specific\n"
"file may not support a specific attribute.\n\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the operation\n"
"was cancelled, the error gio.IO_ERROR_CANCELLED will be returned." },
    { "query_writable_namespaces", (PyCFunction)_wrap_g_file_query_writable_namespaces, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.query_writable_namespaces([cancellable]) -> list\n\n"
"Obtain the list of attribute namespaces where new attributes can\n"
"be created by a user. An example of this is extended attributes\n"
"(in the "
"xattr"
" namespace).\n"
"If cancellable is not None, then the operation can be cancelled\n"
"by triggering the cancellable object from another thread. If the\n"
"operation was cancelled, the error gio.IO_ERROR_CANCELLED\n"
"will be returned." },
    { "set_attribute", (PyCFunction)_wrap_g_file_set_attribute, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.set_attribute(attribute, type, value_p [,flags [,cancellable ]])->bool\n"
"\n"
"Sets an attribute in the file with attribute name attribute to value_p.\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the operation\n"
"was cancelled, the error gio.IO_ERROR_CANCELLED will be returned." },
    { "set_attributes_from_info", (PyCFunction)_wrap_g_file_set_attributes_from_info, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_string", (PyCFunction)_wrap_g_file_set_attribute_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_byte_string", (PyCFunction)_wrap_g_file_set_attribute_byte_string, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_uint32", (PyCFunction)_wrap_g_file_set_attribute_uint32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_int32", (PyCFunction)_wrap_g_file_set_attribute_int32, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_uint64", (PyCFunction)_wrap_g_file_set_attribute_uint64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "set_attribute_int64", (PyCFunction)_wrap_g_file_set_attribute_int64, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "mount_enclosing_volume", (PyCFunction)_wrap_g_file_mount_enclosing_volume, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.mount_enclosing_volume(mount_operation, callback, [cancellable,\n"
"                         user_data])\n"
"Starts a mount_operation, mounting the volume that contains\n"
"the file location.\n"
"\n"
"When this operation has completed, callback will be called with\n"
"user_user data, and the operation can be finalized with\n"
"gio.File.mount_enclosing_volume_finish().\n"
"\n"
"If cancellable is not None, then the operation can be cancelled\n"
"by triggering the cancellable object from another thread.\n"
"If the operation was cancelled, the error gio.ERROR_CANCELLED\n"
"will be returned." },
    { "mount_enclosing_volume_finish", (PyCFunction)_wrap_g_file_mount_enclosing_volume_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "mount_mountable", (PyCFunction)_wrap_g_file_mount_mountable, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.mount_mountable(mount_operation, callback, [flags, cancellable,\n"
"                  user_data])\n"
"Mounts a file of type gio.FILE_TYPE_MOUNTABLE. Using mount_operation,\n"
"you can request callbacks when, for instance, passwords are needed\n"
"during authentication.\n"
"\n"
"If cancellable is not None, then the operation can be cancelled by\n"
" triggering the cancellable object from another thread. If the\n"
"operation was cancelled, the error gio.ERROR_CANCELLED will be returned.\n"
"\n"
"When the operation is finished, callback will be called. You can then\n"
"call g_file_mount_mountable_finish() to get the result of the operation.\n" },
    { "mount_mountable_finish", (PyCFunction)_wrap_g_file_mount_mountable_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unmount_mountable", (PyCFunction)_wrap_g_file_unmount_mountable, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.unmount_mountable(callback, [flags, cancellable, user_data])\n"
"Unmounts a file of type gio.FILE_TYPE_MOUNTABLE.\n"
"\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the\n"
"operation was cancelled, the error gio.ERROR_CANCELLED will be returned.\n"
"\n"
"When the operation is finished, callback will be called. You can\n"
"then call gio.File.unmount_mountable_finish() to get the\n"
"result of the operation.\n" },
    { "unmount_mountable_finish", (PyCFunction)_wrap_g_file_unmount_mountable_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "eject_mountable_finish", (PyCFunction)_wrap_g_file_eject_mountable_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "copy_attributes", (PyCFunction)_wrap_g_file_copy_attributes, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "monitor_directory", (PyCFunction)_wrap_g_file_monitor_directory, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "monitor_file", (PyCFunction)_wrap_g_file_monitor_file, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "query_default_handler", (PyCFunction)_wrap_g_file_query_default_handler, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "load_contents", (PyCFunction)_wrap_g_file_load_contents, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.load_contents([cancellable]) -> contents, length, etag_out\n\n"
"Loads the content of the file into memory, returning the size of the\n"
"data. The data is always zero terminated, but this is not included\n"
"in the resultant length.\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the operation\n"
"was cancelled, the error gio.IO_ERROR_CANCELLED will be returned.\n" },
    { "load_contents_async", (PyCFunction)_wrap_g_file_load_contents_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.load_contents_async(callback, [cancellable, [user_data]])->start loading\n\n"
"Starts an asynchronous load of the file's contents.\n\n"
"For more details, see F.load_contents() which is the synchronous\n"
"version of this call.\n\n"
"When the load operation has completed, callback will be called with\n"
"user data. To finish the operation, call F.load_contents_finish() with\n"
"the parameter 'res' returned by the callback.\n\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the operation\n"
"was cancelled, the error gio.IO_ERROR_CANCELLED will be returned.\n" },
    { "load_contents_finish", (PyCFunction)_wrap_g_file_load_contents_finish, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.load_contents_finish(res) -> contents, length, etag_out\n\n"
"Finishes an asynchronous load of the file's contents. The contents are\n"
"placed in contents, and length is set to the size of the contents\n"
"string. If etag_out is present, it will be set to the new entity\n"
"tag for the file.\n" },
    { "replace_contents", (PyCFunction)_wrap_g_file_replace_contents, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.replace_contents(contents, [etag, [make_backup, [flags, [cancellable]]]])\n"
"-> etag_out\n"
"\n"
"Replaces the content of the file, returning the new etag value for the\n"
"file. If an etag is specified, any existing file must have that etag, or\n"
"the error gio.IO_ERROR_WRONG_ETAG will be returned.\n"
"If make_backup is True, this method will attempt to make a backup of the\n"
"file. If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the operation\n"
"was cancelled, the error gio.IO_ERROR_CANCELLED will be returned.\n" },
    { "replace_contents_async", (PyCFunction)_wrap_g_file_replace_contents_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.replace_contents_async(contents, callback, [etag, [make_backup, [flags,\n"
"                         [cancellable]]]]) -> etag_out\n"
"\n"
"Starts an asynchronous replacement of the file with the given contents.\n"
"For more details, see F.replace_contents() which is the synchronous\n"
"version of this call.\n\n"
"When the load operation has completed, callback will be called with\n"
"user data. To finish the operation, call F.replace_contents_finish() with\n"
"the parameter 'res' returned by the callback.\n\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the operation\n"
"was cancelled, the error gio.IO_ERROR_CANCELLED will be returned.\n" },
    { "replace_contents_finish", (PyCFunction)_wrap_g_file_replace_contents_finish, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.replace_contents_finish(res) -> etag_out\n\n"
"Finishes an asynchronous replacement of the file's contents.\n"
"The new entity tag for the file is returned.\n" },
    { NULL, NULL, 0, NULL }
};

#line 1191 "gfile.override"
static PyObject *
_wrap_g_file_tp_repr(PyGObject *self)
{
    char *uri = g_file_get_uri(G_FILE(self->obj));
    gchar *representation;
    PyObject *result;

    if (uri) {
	representation = g_strdup_printf("<%s at %p: %s>", self->ob_type->tp_name, self, uri);
	g_free(uri);
    }
    else
	representation = g_strdup_printf("<%s at %p: UNKNOWN URI>", self->ob_type->tp_name, self);

    result = PyString_FromString(representation);
    g_free(representation);
    return result;
}

/* GFile.eject_mountable */
/* GFile.find_enclosing_mount_async */
/* GFile.set_attributes_async */
/* GFile.set_display_name_async */
/* GFile.load_partial_contents_async: No ArgType for GFileReadMoreCallback */
/* GFile.move: No ArgType for GFileProgressCallback */
/* GFile.set_attributes_finish: No ArgType for GFileInfo** */
/* GFile.load_partial_contents_finish: No ArgType for char** */
#line 9600 "gio.c"


#line 1184 "gfile.override"
static long
_wrap_g_file_tp_hash(PyGObject *self)
{
    return g_file_hash(G_FILE(self->obj));
}
#line 9609 "gio.c"


#line 1153 "gfile.override"
static PyObject *
_wrap_g_file_tp_richcompare(PyGObject *self, PyGObject *other, int op)
{
    PyObject *result;

    if (PyObject_TypeCheck(self, &PyGFile_Type)
        && PyObject_TypeCheck(other, &PyGFile_Type)) {
        GFile *file1 = G_FILE(self->obj);
        GFile *file2 = G_FILE(other->obj);

        switch (op) {
        case Py_EQ:
            result = (g_file_equal(file1, file2)
                      ? Py_True : Py_False);
            break;
        case Py_NE:
            result = (!g_file_equal(file1, file2)
                      ? Py_True : Py_False);
            break;
        default:
            result = Py_NotImplemented;
        }
    }
    else
        result = Py_NotImplemented;

    Py_INCREF(result);
    return result;
}
#line 9642 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGFile_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.File",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)_wrap_g_file_tp_repr,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)_wrap_g_file_tp_hash,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    (char *) "File(arg, path=None, uri=None) -> gio.File subclass\n"
"\n"
"If arg is specified; creates a GFile with the given argument from the\n"
"command line.  The value of arg can be either a URI, an absolute path\n"
"or a relative path resolved relative to the current working directory.\n"
"If path is specified, create a file from an absolute or relative path.\n"
"If uri is specified, create a file from a URI.\n\n"
"This operation never fails, but the returned object might not \n"
"support any I/O operation if arg points to a malformed path.",                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)_wrap_g_file_tp_richcompare,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGFile_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GIcon ----------- */

static PyObject *
_wrap_g_icon_equal(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "icon2", NULL };
    PyGObject *icon2;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GIcon.equal", kwlist, &PyGIcon_Type, &icon2))
        return NULL;
    
    ret = g_icon_equal(G_ICON(self->obj), G_ICON(icon2->obj));
    
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGIcon_methods[] = {
    { "equal", (PyCFunction)_wrap_g_icon_equal, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 60 "gicon.override"
static long
_wrap_g_icon_tp_hash(PyGObject *self)
{
    return g_icon_hash(G_ICON(self->obj));
}
#line 9730 "gio.c"


#line 29 "gicon.override"
static PyObject *
_wrap_g_icon_tp_richcompare(PyGObject *self, PyGObject *other, int op)
{
    PyObject *result;

    if (PyObject_TypeCheck(self, &PyGIcon_Type)
        && PyObject_TypeCheck(other, &PyGIcon_Type)) {
        GIcon *icon1 = G_ICON(self->obj);
        GIcon *icon2 = G_ICON(other->obj);

        switch (op) {
        case Py_EQ:
            result = (g_icon_equal(icon1, icon2)
                      ? Py_True : Py_False);
            break;
        case Py_NE:
            result = (!g_icon_equal(icon1, icon2)
                      ? Py_True : Py_False);
            break;
        default:
            result = Py_NotImplemented;
        }
    }
    else
        result = Py_NotImplemented;

    Py_INCREF(result);
    return result;
}
#line 9763 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGIcon_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.Icon",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)_wrap_g_icon_tp_hash,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)_wrap_g_icon_tp_richcompare,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGIcon_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GLoadableIcon ----------- */

#line 67 "gicon.override"
static PyObject *
_wrap_g_loadable_icon_load(PyGObject *self,
                           PyObject *args,
                           PyObject *kwargs)
{
    static char *kwlist[] = { "size", "cancellable", NULL };
    int size = 0;
    char *type = NULL;
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable;
    GError *error = NULL;
    GInputStream *stream;
    PyObject *result;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "|iO:gio.LoadableIcon.load",
				     kwlist,
				     &size, &pycancellable))
        return NULL;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
	return NULL;

    stream = g_loadable_icon_load(G_LOADABLE_ICON(self->obj), size, &type,
				  cancellable, &error);
    if (pyg_error_check(&error))
        return NULL;

    result = Py_BuildValue("Ns", pygobject_new((GObject *) stream), type);
    g_free(type);
    return result;
}
#line 9848 "gio.c"


#line 101 "gicon.override"
static PyObject *
_wrap_g_loadable_icon_load_async(PyGObject *self,
				 PyObject *args,
				 PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "size", "cancellable", "user_data", NULL };
    int size = 0;
    PyGObject *pycancellable = NULL;
    GCancellable *cancellable;
    PyGIONotify *notify;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|iOO:gio.LoadableIcon.load_async",
				     kwlist,
				     &notify->callback, &size, &pycancellable, &notify->data))
	goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (!pygio_check_cancellable(pycancellable, &cancellable))
	goto error;

    pygio_notify_reference_callback(notify);

    g_loadable_icon_load_async(G_LOADABLE_ICON(self->obj),
			       size,
			       cancellable,
			       (GAsyncReadyCallback) async_result_callback_marshal,
			       notify);
    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 9891 "gio.c"


#line 142 "gicon.override"
static PyObject *
_wrap_g_loadable_icon_load_finish(PyGObject *self,
				  PyObject *args,
				  PyObject *kwargs)
{
    static char *kwlist[] = { "res", NULL };
    PyGObject *res;
    char *type = NULL;
    GError *error = NULL;
    GInputStream *stream;
    PyObject *result;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!:gio.LoadableIcon.load_finish",
				     kwlist,
				     &PyGAsyncResult_Type, &res))
        return NULL;

    stream = g_loadable_icon_load_finish(G_LOADABLE_ICON(self->obj),
					 G_ASYNC_RESULT(res->obj), &type, &error);
    if (pyg_error_check(&error))
        return NULL;

    result = Py_BuildValue("Ns", pygobject_new((GObject *) stream), type);
    g_free(type);
    return result;
}
#line 9922 "gio.c"


static const PyMethodDef _PyGLoadableIcon_methods[] = {
    { "load", (PyCFunction)_wrap_g_loadable_icon_load, METH_VARARGS|METH_KEYWORDS,
      (char *) "ICON.load([size, [cancellable]]) -> input stream, type\n"
"\n"
"Opens a stream of icon data for reading. The result is a tuple of\n"
"gio.InputStream and type (either a string or None). The stream can\n"
"be read to retrieve icon data.\n"
"\n"
"Optional size is a hint at desired icon size. Not all implementations\n"
"support it and the hint will be just ignored in such cases.\n"
"If cancellable is specified, then the operation can be cancelled\n"
"by triggering the cancellable object from another thread. See\n"
"gio.File.read for details." },
    { "load_async", (PyCFunction)_wrap_g_loadable_icon_load_async, METH_VARARGS|METH_KEYWORDS,
      (char *) "ICON.load_async(callback, [size, [cancellable, [user_data]]])\n"
"-> start loading\n"
"\n"
"For more information, see gio.LoadableIcon.load() which is the\n"
"synchronous version of this call. Asynchronously opens icon data for\n"
"reading. When the operation is finished, callback will be called.\n"
"You can then call gio.LoadableIcon.load_finish() to get the result of\n"
"the operation.\n" },
    { "load_finish", (PyCFunction)_wrap_g_loadable_icon_load_finish, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.load_finish(res) -> start loading\n"
"\n"
"Finish asynchronous icon loading operation. Must be called from callback\n"
"as specified to gio.LoadableIcon.load_async. Returns a tuple of\n"
"gio.InputStream and type, just as gio.LoadableIcon.load." },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGLoadableIcon_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.LoadableIcon",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGLoadableIcon_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GMount ----------- */

static PyObject *
_wrap_g_mount_get_root(PyGObject *self)
{
    PyObject *py_ret;
    GFile *ret;

    
    ret = g_mount_get_root(G_MOUNT(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_mount_get_name(PyGObject *self)
{
    gchar *ret;

    
    ret = g_mount_get_name(G_MOUNT(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_get_icon(PyGObject *self)
{
    PyObject *py_ret;
    GIcon *ret;

    
    ret = g_mount_get_icon(G_MOUNT(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_mount_get_uuid(PyGObject *self)
{
    gchar *ret;

    
    ret = g_mount_get_uuid(G_MOUNT(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_mount_get_volume(PyGObject *self)
{
    PyObject *py_ret;
    GVolume *ret;

    
    ret = g_mount_get_volume(G_MOUNT(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_mount_get_drive(PyGObject *self)
{
    PyObject *py_ret;
    GDrive *ret;

    
    ret = g_mount_get_drive(G_MOUNT(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_mount_can_unmount(PyGObject *self)
{
    int ret;

    
    ret = g_mount_can_unmount(G_MOUNT(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_mount_can_eject(PyGObject *self)
{
    int ret;

    
    ret = g_mount_can_eject(G_MOUNT(self->obj));
    
    return PyBool_FromLong(ret);

}

#line 457 "gio.override"
static PyObject *
_wrap_g_mount_unmount(PyGObject *self,
		      PyObject *args,
		      PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags",
			      "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *py_cancellable = NULL;
    GMountUnmountFlags flags = G_MOUNT_UNMOUNT_NONE;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OOO:GMount.unmount",
				     kwlist,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_UNMOUNT_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    pyg_begin_allow_threads;

    g_mount_unmount(G_MOUNT(self->obj),
		    flags,
		    cancellable,
		    (GAsyncReadyCallback)async_result_callback_marshal,
		    notify);

    pyg_end_allow_threads;

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 10177 "gio.c"


static PyObject *
_wrap_g_mount_unmount_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GMount.unmount_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_mount_unmount_finish(G_MOUNT(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 512 "gio.override"
static PyObject *
_wrap_g_mount_eject(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    GMountUnmountFlags flags = G_MOUNT_UNMOUNT_NONE;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OOO:gio.Mount.eject",
				     kwlist,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_UNMOUNT_FLAGS,
					py_flags, (gpointer) &flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    pyg_begin_allow_threads;

    g_mount_eject(G_MOUNT(self->obj),
		  flags,
		  cancellable,
		  (GAsyncReadyCallback) async_result_callback_marshal,
		  notify);

    pyg_end_allow_threads;

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 10250 "gio.c"


static PyObject *
_wrap_g_mount_eject_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GMount.eject_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_mount_eject_finish(G_MOUNT(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 564 "gio.override"
static PyObject *
_wrap_g_mount_remount(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "mount_operation",
			      "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    GMountUnmountFlags flags = G_MOUNT_UNMOUNT_NONE;
    PyObject *py_mount_operation = Py_None;
    GMountOperation *mount_operation = NULL;
    PyGObject *py_cancellable = NULL;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OOOO:gio.Mount.remount",
				     kwlist,
				     &notify->callback,
				     &py_flags,
				     &py_mount_operation,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_mount_operation != Py_None) {
	if (!pygobject_check(py_mount_operation, &PyGMountOperation_Type)) {
	    PyErr_SetString(PyExc_TypeError,
			    "mount_operation must be a gio.MountOperation or None");
            goto error;
	}

	mount_operation = G_MOUNT_OPERATION(pygobject_get(py_mount_operation));
    }

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_UNMOUNT_FLAGS,
					py_flags, (gpointer) &flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    pyg_begin_allow_threads;

    g_mount_remount(G_MOUNT(self->obj),
		    flags,
		    mount_operation,
		    cancellable,
		    (GAsyncReadyCallback) async_result_callback_marshal,
		    notify);

    pyg_end_allow_threads;

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 10338 "gio.c"


static PyObject *
_wrap_g_mount_remount_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GMount.remount_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_mount_remount_finish(G_MOUNT(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGMount_methods[] = {
    { "get_root", (PyCFunction)_wrap_g_mount_get_root, METH_NOARGS,
      NULL },
    { "get_name", (PyCFunction)_wrap_g_mount_get_name, METH_NOARGS,
      NULL },
    { "get_icon", (PyCFunction)_wrap_g_mount_get_icon, METH_NOARGS,
      NULL },
    { "get_uuid", (PyCFunction)_wrap_g_mount_get_uuid, METH_NOARGS,
      NULL },
    { "get_volume", (PyCFunction)_wrap_g_mount_get_volume, METH_NOARGS,
      NULL },
    { "get_drive", (PyCFunction)_wrap_g_mount_get_drive, METH_NOARGS,
      NULL },
    { "can_unmount", (PyCFunction)_wrap_g_mount_can_unmount, METH_NOARGS,
      NULL },
    { "can_eject", (PyCFunction)_wrap_g_mount_can_eject, METH_NOARGS,
      NULL },
    { "unmount", (PyCFunction)_wrap_g_mount_unmount, METH_VARARGS|METH_KEYWORDS,
      (char *) "M.unmount(callback, [flags, cancellable, user_data])\n"
"Unmounts a mount. This is an asynchronous operation, and is finished\n"
"by calling gio.Mount.unmount_finish() with the mount and gio.AsyncResults\n"
"data returned in the callback." },
    { "unmount_finish", (PyCFunction)_wrap_g_mount_unmount_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "eject", (PyCFunction)_wrap_g_mount_eject, METH_VARARGS|METH_KEYWORDS,
      (char *) "F.eject(callback, [flags, cancellable, user_data])\n"
"Ejects a volume.\n"
"\n"
"If cancellable is not None, then the operation can be cancelled by\n"
"triggering the cancellable object from another thread. If the\n"
"operation was cancelled, the error gio.ERROR_CANCELLED will be returned.\n"
"\n"
"When the operation is finished, callback will be called. You can\n"
"then call gio.Volume.eject_finish() to get the result of the operation.\n" },
    { "eject_finish", (PyCFunction)_wrap_g_mount_eject_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "remount", (PyCFunction)_wrap_g_mount_remount, METH_VARARGS|METH_KEYWORDS,
      (char *) "M.remount(callback, [flags, [mount_operation, [cancellable, [user_data]]]])\n"
"Remounts a mount. This is an asynchronous operation, and is finished by\n"
"calling gio.Mount.remount_finish with the mount and gio.AsyncResults data\n"
"returned in the callback." },
    { "remount_finish", (PyCFunction)_wrap_g_mount_remount_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 631 "gio.override"
static PyObject *
_wrap_g_mount_tp_repr(PyGObject *self)
{
    char *name = g_mount_get_name(G_MOUNT(self->obj));
    char *uuid = g_mount_get_uuid(G_MOUNT(self->obj));
    gchar *representation;
    PyObject *result;

    if (name) {
	if (uuid) {
	    representation = g_strdup_printf("<%s at %p: %s (%s)>",
					     self->ob_type->tp_name, self, name, uuid);
	}
	else {
	    representation = g_strdup_printf("<%s at %p: %s>",
					     self->ob_type->tp_name, self, name);
	}
    }
    else
	representation = g_strdup_printf("<%s at %p: UNKNOWN NAME>", self->ob_type->tp_name, self);

    g_free(name);
    g_free(uuid);

    result = PyString_FromString(representation);
    g_free(representation);
    return result;
}
#line 10435 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGMount_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.Mount",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)_wrap_g_mount_tp_repr,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGMount_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GSeekable ----------- */

static PyObject *
_wrap_g_seekable_tell(PyGObject *self)
{
    gint64 ret;

    
    ret = g_seekable_tell(G_SEEKABLE(self->obj));
    
    return PyLong_FromLongLong(ret);
}

static PyObject *
_wrap_g_seekable_can_seek(PyGObject *self)
{
    int ret;

    
    ret = g_seekable_can_seek(G_SEEKABLE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_seekable_seek(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "offset", "type", "cancellable", NULL };
    int type = G_SEEK_SET, ret;
    PyGObject *py_cancellable = NULL;
    gint64 offset;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"L|iO:GSeekable.seek", kwlist, &offset, &type, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_seekable_seek(G_SEEKABLE(self->obj), offset, type, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_seekable_can_truncate(PyGObject *self)
{
    int ret;

    
    ret = g_seekable_can_truncate(G_SEEKABLE(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_seekable_truncate(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "offset", "cancellable", NULL };
    PyGObject *py_cancellable = NULL;
    int ret;
    gint64 offset;
    GCancellable *cancellable = NULL;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"L|O:GSeekable.truncate", kwlist, &offset, &py_cancellable))
        return NULL;
    if ((PyObject *)py_cancellable == Py_None)
        cancellable = NULL;
    else if (py_cancellable && pygobject_check(py_cancellable, &PyGCancellable_Type))
        cancellable = G_CANCELLABLE(py_cancellable->obj);
    else if (py_cancellable) {
        PyErr_SetString(PyExc_TypeError, "cancellable should be a GCancellable or None");
        return NULL;
    }
    
    ret = g_seekable_truncate(G_SEEKABLE(self->obj), offset, (GCancellable *) cancellable, &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGSeekable_methods[] = {
    { "tell", (PyCFunction)_wrap_g_seekable_tell, METH_NOARGS,
      NULL },
    { "can_seek", (PyCFunction)_wrap_g_seekable_can_seek, METH_NOARGS,
      NULL },
    { "seek", (PyCFunction)_wrap_g_seekable_seek, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "can_truncate", (PyCFunction)_wrap_g_seekable_can_truncate, METH_NOARGS,
      NULL },
    { "truncate", (PyCFunction)_wrap_g_seekable_truncate, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGSeekable_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.Seekable",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)0,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGSeekable_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GVolume ----------- */

static PyObject *
_wrap_g_volume_get_name(PyGObject *self)
{
    gchar *ret;

    
    ret = g_volume_get_name(G_VOLUME(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_volume_get_icon(PyGObject *self)
{
    PyObject *py_ret;
    GIcon *ret;

    
    ret = g_volume_get_icon(G_VOLUME(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_volume_get_uuid(PyGObject *self)
{
    gchar *ret;

    
    ret = g_volume_get_uuid(G_VOLUME(self->obj));
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_volume_get_drive(PyGObject *self)
{
    PyObject *py_ret;
    GDrive *ret;

    
    ret = g_volume_get_drive(G_VOLUME(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_volume_get_mount(PyGObject *self)
{
    GMount *ret;
    PyObject *py_ret;

    
    ret = g_volume_get_mount(G_VOLUME(self->obj));
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_volume_can_mount(PyGObject *self)
{
    int ret;

    
    ret = g_volume_can_mount(G_VOLUME(self->obj));
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_volume_can_eject(PyGObject *self)
{
    int ret;

    
    ret = g_volume_can_eject(G_VOLUME(self->obj));
    
    return PyBool_FromLong(ret);

}

#line 24 "gvolume.override"
static PyObject *
_wrap_g_volume_mount(PyGObject *self,
		     PyObject *args,
		     PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags", "mount_operation",
			      "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *mount_operation;
    PyGObject *py_cancellable = NULL;
    GMountMountFlags flags = G_MOUNT_MOUNT_NONE;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O!O|OOO:Volume.mount",
				     kwlist,
				     &PyGMountOperation_Type,
				     &mount_operation,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_MOUNT_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_volume_mount(G_VOLUME(self->obj),
		   flags,
		   G_MOUNT_OPERATION(mount_operation->obj),
		   cancellable,
		   (GAsyncReadyCallback)async_result_callback_marshal,
		   notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 10800 "gio.c"


static PyObject *
_wrap_g_volume_mount_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GVolume.mount_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_volume_mount_finish(G_VOLUME(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

#line 79 "gvolume.override"
static PyObject *
_wrap_g_volume_eject(PyGObject *self,
		     PyObject *args,
		     PyObject *kwargs)
{
    static char *kwlist[] = { "callback", "flags",
			      "cancellable", "user_data", NULL };
    PyGIONotify *notify;
    PyObject *py_flags = NULL;
    PyGObject *py_cancellable = NULL;
    GMountUnmountFlags flags = G_MOUNT_UNMOUNT_NONE;
    GCancellable *cancellable;

    notify = pygio_notify_new();

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                     "O|OOO:Volume.eject",
				     kwlist,
				     &notify->callback,
				     &py_flags,
				     &py_cancellable,
				     &notify->data))
        goto error;

    if (!pygio_notify_callback_is_valid(notify))
        goto error;

    if (py_flags && pyg_flags_get_value(G_TYPE_MOUNT_UNMOUNT_FLAGS,
					py_flags, (gpointer)&flags))
        goto error;

    if (!pygio_check_cancellable(py_cancellable, &cancellable))
        goto error;

    pygio_notify_reference_callback(notify);

    g_volume_eject(G_VOLUME(self->obj),
		   flags,
		   cancellable,
		   (GAsyncReadyCallback)async_result_callback_marshal,
		   notify);

    Py_INCREF(Py_None);
    return Py_None;

 error:
    pygio_notify_free(notify);
    return NULL;
}
#line 10872 "gio.c"


static PyObject *
_wrap_g_volume_eject_finish(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "result", NULL };
    PyGObject *result;
    int ret;
    GError *error = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:GVolume.eject_finish", kwlist, &PyGAsyncResult_Type, &result))
        return NULL;
    
    ret = g_volume_eject_finish(G_VOLUME(self->obj), G_ASYNC_RESULT(result->obj), &error);
    
    if (pyg_error_check(&error))
        return NULL;
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGVolume_methods[] = {
    { "get_name", (PyCFunction)_wrap_g_volume_get_name, METH_NOARGS,
      NULL },
    { "get_icon", (PyCFunction)_wrap_g_volume_get_icon, METH_NOARGS,
      NULL },
    { "get_uuid", (PyCFunction)_wrap_g_volume_get_uuid, METH_NOARGS,
      NULL },
    { "get_drive", (PyCFunction)_wrap_g_volume_get_drive, METH_NOARGS,
      NULL },
    { "get_mount", (PyCFunction)_wrap_g_volume_get_mount, METH_NOARGS,
      NULL },
    { "can_mount", (PyCFunction)_wrap_g_volume_can_mount, METH_NOARGS,
      NULL },
    { "can_eject", (PyCFunction)_wrap_g_volume_can_eject, METH_NOARGS,
      NULL },
    { "mount", (PyCFunction)_wrap_g_volume_mount, METH_VARARGS|METH_KEYWORDS,
      (char *) "V.mount(mount_operation, callback, [flags, cancellable, user_data])\n"
"Mounts a volume. Using mount_operation, you can request callbacks\n"
"when, for instance, passwords are needed during authentication.\n"
"\n"
"If cancellable is not None, then the operation can be cancelled by\n"
" triggering the cancellable object from another thread. If the\n"
"operation was cancelled, the error gio.ERROR_CANCELLED will be returned.\n"
"\n"
"When the operation is finished, callback will be called. You can then\n"
"call gio.Volume.mount_finish() to get the result of the operation.\n" },
    { "mount_finish", (PyCFunction)_wrap_g_volume_mount_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "eject", (PyCFunction)_wrap_g_volume_eject, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "eject_finish", (PyCFunction)_wrap_g_volume_eject_finish, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

#line 130 "gvolume.override"
static PyObject *
_wrap_g_volume_tp_repr(PyGObject *self)
{
    char *name = g_volume_get_name(G_VOLUME(self->obj));
    gchar *representation;
    PyObject *result;

    if (name) {
	representation = g_strdup_printf("<%s at %p: %s>", self->ob_type->tp_name, self, name);
	g_free(name);
    }
    else
	representation = g_strdup_printf("<%s at %p: UNKNOWN NAME>", self->ob_type->tp_name, self);

    result = PyString_FromString(representation);
    g_free(representation);
    return result;
}
#line 10948 "gio.c"


PyTypeObject G_GNUC_INTERNAL PyGVolume_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.Volume",                   /* tp_name */
    sizeof(PyObject),          /* tp_basicsize */
    0,                                 /* tp_itemsize */
    /* methods */
    (destructor)0,        /* tp_dealloc */
    (printfunc)0,                      /* tp_print */
    (getattrfunc)0,       /* tp_getattr */
    (setattrfunc)0,       /* tp_setattr */
    (cmpfunc)0,           /* tp_compare */
    (reprfunc)_wrap_g_volume_tp_repr,             /* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,             /* tp_hash */
    (ternaryfunc)0,          /* tp_call */
    (reprfunc)0,              /* tp_str */
    (getattrofunc)0,     /* tp_getattro */
    (setattrofunc)0,     /* tp_setattro */
    (PyBufferProcs*)0,  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,                      /* tp_flags */
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGVolume_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)0,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- functions ----------- */

#line 375 "gio.override"
static PyObject *
_wrap_g_app_info_get_all (PyGObject *self)
{
  GList *list, *l;
  PyObject *ret;

  list = g_app_info_get_all ();

  ret = PyList_New(0);
  for (l = list; l; l = l->next) {
    GObject *obj = l->data;
    PyObject *item = pygobject_new(obj);
    PyList_Append(ret, item);
    Py_DECREF(item);
  }
  g_list_free(list);

  return ret;
}
#line 11020 "gio.c"


#line 396 "gio.override"
static PyObject *
_wrap_g_app_info_get_all_for_type (PyGObject *self, PyObject *args)
{
  GList *list, *l;
  PyObject *ret;
  gchar *type;

  if (!PyArg_ParseTuple (args, "s:app_info_get_all_for_type", &type))
    return NULL;

  list = g_app_info_get_all_for_type (type);

  ret = PyList_New(0);
  for (l = list; l; l = l->next) {
    GObject *obj = l->data;
    PyObject *item = pygobject_new(obj);
    PyList_Append(ret, item);
    Py_DECREF(item);
  }
  g_list_free(list);

  return ret;
}
#line 11047 "gio.c"


static PyObject *
_wrap_g_app_info_get_default_for_type(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "content_type", "must_support_uris", NULL };
    char *content_type;
    int must_support_uris;
    GAppInfo *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"si:app_info_get_default_for_type", kwlist, &content_type, &must_support_uris))
        return NULL;
    
    ret = g_app_info_get_default_for_type(content_type, must_support_uris);
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_app_info_get_default_for_uri_scheme(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "uri_scheme", NULL };
    char *uri_scheme;
    GAppInfo *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:app_info_get_default_for_uri_scheme", kwlist, &uri_scheme))
        return NULL;
    
    ret = g_app_info_get_default_for_uri_scheme(uri_scheme);
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_buffered_input_stream_new_sized(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "base_stream", "size", NULL };
    PyGObject *base_stream;
    gsize size;
    GInputStream *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!k:buffered_input_stream_new_sized", kwlist, &PyGInputStream_Type, &base_stream, &size))
        return NULL;
    
    ret = g_buffered_input_stream_new_sized(G_INPUT_STREAM(base_stream->obj), size);
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_buffered_output_stream_new_sized(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "base_stream", "size", NULL };
    PyGObject *base_stream;
    PyObject *py_size = NULL;
    guint size = 0;
    GOutputStream *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!O:buffered_output_stream_new_sized", kwlist, &PyGOutputStream_Type, &base_stream, &py_size))
        return NULL;
    if (py_size) {
        if (PyLong_Check(py_size))
            size = PyLong_AsUnsignedLong(py_size);
        else if (PyInt_Check(py_size))
            size = PyInt_AsLong(py_size);
        else
            PyErr_SetString(PyExc_TypeError, "Parameter 'size' must be an int or a long");
        if (PyErr_Occurred())
            return NULL;
    }
    
    ret = g_buffered_output_stream_new_sized(G_OUTPUT_STREAM(base_stream->obj), size);
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_cancellable_get_current(PyObject *self)
{
    GCancellable *ret;

    
    ret = g_cancellable_get_current();
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_content_type_equals(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type1", "type2", NULL };
    char *type1, *type2;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ss:content_type_equals", kwlist, &type1, &type2))
        return NULL;
    
    ret = g_content_type_equals(type1, type2);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_content_type_is_a(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", "supertype", NULL };
    char *type, *supertype;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ss:content_type_is_a", kwlist, &type, &supertype))
        return NULL;
    
    ret = g_content_type_is_a(type, supertype);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_content_type_is_unknown(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", NULL };
    char *type;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:content_type_is_unknown", kwlist, &type))
        return NULL;
    
    ret = g_content_type_is_unknown(type);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_content_type_get_description(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", NULL };
    char *type;
    gchar *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:content_type_get_description", kwlist, &type))
        return NULL;
    
    ret = g_content_type_get_description(type);
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_content_type_get_mime_type(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", NULL };
    char *type;
    gchar *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:content_type_get_mime_type", kwlist, &type))
        return NULL;
    
    ret = g_content_type_get_mime_type(type);
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_content_type_get_icon(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", NULL };
    char *type;
    PyObject *py_ret;
    GIcon *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:content_type_get_icon", kwlist, &type))
        return NULL;
    
    ret = g_content_type_get_icon(type);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_content_type_can_be_executable(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "type", NULL };
    char *type;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:content_type_can_be_executable", kwlist, &type))
        return NULL;
    
    ret = g_content_type_can_be_executable(type);
    
    return PyBool_FromLong(ret);

}

#line 421 "gio.override"
static PyObject *
_wrap_g_content_type_guess(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    char *kwlist[] = {"filename", "data", "want_uncertain", NULL};
    char *filename = NULL, *data = NULL, *type;
    int data_size = 0;
    gboolean result_uncertain, want_uncertain = FALSE;
    PyObject *ret;

    if (!PyArg_ParseTupleAndKeywords (args, kwargs,
				      "|zz#i:g_content_type_guess",
				      kwlist,
				      &filename, &data, &data_size,
				      &want_uncertain))
      return NULL;

    if (!filename && !data) {
      PyErr_SetString(PyExc_TypeError, "need at least one argument");
      return NULL;
    }

    type = g_content_type_guess(filename, (guchar *) data,
				data_size, &result_uncertain);

    if (want_uncertain) {
	ret = Py_BuildValue("zN", type, PyBool_FromLong(result_uncertain));
    
    } else {
        ret = PyString_FromString(type);
    }

    g_free(type);
    return ret;
}
#line 11300 "gio.c"


#line 682 "gio.override"
static PyObject *
_wrap_g_content_types_get_registered(PyObject *self)
{
    GList *list, *l;
    PyObject *ret;

    list = g_content_types_get_registered();

    ret = PyList_New(0);
    for (l = list; l; l = l->next) {
	char *content_type = l->data;
	PyObject *string = PyString_FromString(content_type);
	PyList_Append(ret, string);
	Py_DECREF(string);
	g_free(content_type);
    }
    g_list_free(list);

    return ret;
}
#line 11324 "gio.c"


static PyObject *
_wrap_g_file_parse_name(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "parse_name", NULL };
    char *parse_name;
    PyObject *py_ret;
    GFile *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:file_parse_name", kwlist, &parse_name))
        return NULL;
    
    ret = g_file_parse_name(parse_name);
    
    py_ret = pygobject_new((GObject *)ret);
    if (ret != NULL)
        g_object_unref(ret);
    return py_ret;
}

static PyObject *
_wrap_g_io_error_from_errno(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "err_no", NULL };
    int err_no;
    gint ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"i:io_error_from_errno", kwlist, &err_no))
        return NULL;
    
    ret = g_io_error_from_errno(err_no);
    
    return pyg_enum_from_gtype(G_TYPE_IO_ERROR_ENUM, ret);
}

static PyObject *
_wrap_g_vfs_get_default(PyObject *self)
{
    GVfs *ret;

    
    ret = g_vfs_get_default();
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_vfs_get_local(PyObject *self)
{
    GVfs *ret;

    
    ret = g_vfs_get_local();
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_volume_monitor_get(PyObject *self)
{
    GVolumeMonitor *ret;

    
    ret = g_volume_monitor_get();
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_volume_monitor_adopt_orphan_mount(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount", NULL };
    PyGObject *mount;
    GVolume *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:volume_monitor_adopt_orphan_mount", kwlist, &PyGMount_Type, &mount))
        return NULL;
    
    ret = g_volume_monitor_adopt_orphan_mount(G_MOUNT(mount->obj));
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

#line 72 "gfile.override"
static PyObject*
_wrap__file_init(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    GFile *file;
    Py_ssize_t n_args, n_kwargs;
    char *arg;
    PyObject *py_ret;

    n_args = PyTuple_Size(args);
    n_kwargs = kwargs != NULL ? PyDict_Size(kwargs) : 0;

    if (n_args == 1 && n_kwargs == 0) {
	if (!PyArg_ParseTuple(args, "s:GFile", &arg))
	    return NULL;
	file = g_file_new_for_commandline_arg(arg);
    } else if (n_args == 0 && n_kwargs == 1) {
	if (PyDict_GetItemString(kwargs, "path")) {
	    char *kwlist[] = { "path", NULL };
	    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
					     "s:gio.File", kwlist, &arg))
		return NULL;
	    file = g_file_new_for_path(arg);
	} else if (PyDict_GetItemString(kwargs, "uri")) {
	    char *kwlist[] = { "uri", NULL };
	    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
					     "s:gio.File", kwlist, &arg))
		return NULL;
	    file = g_file_new_for_uri(arg);
	} else {
	    PyErr_Format(PyExc_TypeError,
			 "gio.File() got an unexpected keyword argument '%s'",
			 "unknown");
	    return NULL;
	}
    } else {
	PyErr_Format(PyExc_TypeError,
		     "gio.File() takes exactly 1 argument (%zd given)",
		     n_args + n_kwargs);
	return NULL;
    }

    if (!file) {
        PyErr_SetString(PyExc_RuntimeError,
			"could not create GFile object");
        return NULL;
    }

    py_ret = pygobject_new((GObject *)file);
    g_object_unref(file);

    return py_ret;
}
#line 11466 "gio.c"


#line 56 "gfile.override"
static PyObject *
_wrap__install_file_meta(PyObject *self, PyObject *args)
{
    PyObject *metaclass;

    if (!PyArg_ParseTuple(args, "O", &metaclass))
	return NULL;

    Py_INCREF(metaclass);
    PyGFile_Type.ob_type = (PyTypeObject*)metaclass;

    Py_INCREF(Py_None);
    return Py_None;
}
#line 11484 "gio.c"


#line 25 "gappinfo.override"
static PyObject *
_wrap__install_app_info_meta(PyObject *self, PyObject *args)
{
    PyObject *metaclass;

    if (!PyArg_ParseTuple(args, "O", &metaclass))
	return NULL;

    Py_INCREF(metaclass);
    PyGAppInfo_Type.ob_type = (PyTypeObject*)metaclass;

    Py_INCREF(Py_None);
    return Py_None;
}
#line 11502 "gio.c"


#line 41 "gappinfo.override"
static PyObject *
_wrap__app_info_init(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "commandline", "application_name",
			      "flags", NULL };
    char *commandline, *application_name = NULL;
    PyObject *py_flags = NULL;
    GAppInfo *ret;
    GError *error = NULL;
    GAppInfoCreateFlags flags = G_APP_INFO_CREATE_NONE;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "s|zO:gio.AppInfo",
				     kwlist,
				     &commandline, &application_name,
				     &py_flags))
        return NULL;
    if (py_flags && pyg_flags_get_value(G_TYPE_APP_INFO_CREATE_FLAGS,
					py_flags, (gpointer)&flags))
        return NULL;

    ret = g_app_info_create_from_commandline(commandline,
					     application_name, flags, &error);

    if (pyg_error_check(&error))
        return NULL;

    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}
#line 11536 "gio.c"


const PyMethodDef pygio_functions[] = {
    { "app_info_get_all", (PyCFunction)_wrap_g_app_info_get_all, METH_NOARGS,
      NULL },
    { "app_info_get_all_for_type", (PyCFunction)_wrap_g_app_info_get_all_for_type, METH_VARARGS,
      NULL },
    { "app_info_get_default_for_type", (PyCFunction)_wrap_g_app_info_get_default_for_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "app_info_get_default_for_uri_scheme", (PyCFunction)_wrap_g_app_info_get_default_for_uri_scheme, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "buffered_input_stream_new_sized", (PyCFunction)_wrap_g_buffered_input_stream_new_sized, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "buffered_output_stream_new_sized", (PyCFunction)_wrap_g_buffered_output_stream_new_sized, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "cancellable_get_current", (PyCFunction)_wrap_g_cancellable_get_current, METH_NOARGS,
      NULL },
    { "content_type_equals", (PyCFunction)_wrap_g_content_type_equals, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "content_type_is_a", (PyCFunction)_wrap_g_content_type_is_a, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "content_type_is_unknown", (PyCFunction)_wrap_g_content_type_is_unknown, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "content_type_get_description", (PyCFunction)_wrap_g_content_type_get_description, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "content_type_get_mime_type", (PyCFunction)_wrap_g_content_type_get_mime_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "content_type_get_icon", (PyCFunction)_wrap_g_content_type_get_icon, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "content_type_can_be_executable", (PyCFunction)_wrap_g_content_type_can_be_executable, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "content_type_guess", (PyCFunction)_wrap_g_content_type_guess, METH_VARARGS|METH_KEYWORDS,
      (char *) "content_type_guess([filename, data, want_uncertain]) -> mime type\n"
"\n"
"Guesses the content type based on the parameters passed.\n"
"Either filename or data must be specified\n"
"Returns a string containing the mime type.\n"
"If want_uncertain is set to True, return a tuple with the mime type and \n"
"True/False if the type guess was uncertain or not." },
    { "content_types_get_registered", (PyCFunction)_wrap_g_content_types_get_registered, METH_NOARGS,
      NULL },
    { "file_parse_name", (PyCFunction)_wrap_g_file_parse_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "io_error_from_errno", (PyCFunction)_wrap_g_io_error_from_errno, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "vfs_get_default", (PyCFunction)_wrap_g_vfs_get_default, METH_NOARGS,
      NULL },
    { "vfs_get_local", (PyCFunction)_wrap_g_vfs_get_local, METH_NOARGS,
      NULL },
    { "volume_monitor_get", (PyCFunction)_wrap_g_volume_monitor_get, METH_NOARGS,
      NULL },
    { "volume_monitor_adopt_orphan_mount", (PyCFunction)_wrap_g_volume_monitor_adopt_orphan_mount, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "_file_init", (PyCFunction)_wrap__file_init, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "_install_file_meta", (PyCFunction)_wrap__install_file_meta, METH_VARARGS,
      NULL },
    { "_install_app_info_meta", (PyCFunction)_wrap__install_app_info_meta, METH_VARARGS,
      NULL },
    { "_app_info_init", (PyCFunction)_wrap__app_info_init, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};


/* ----------- enums and flags ----------- */

void
pygio_add_constants(PyObject *module, const gchar *strip_prefix)
{
#ifdef VERSION
    PyModule_AddStringConstant(module, "__version__", VERSION);
#endif
  pyg_flags_add(module, "AppInfoCreateFlags", strip_prefix, G_TYPE_APP_INFO_CREATE_FLAGS);
  pyg_enum_add(module, "DataStreamByteOrder", strip_prefix, G_TYPE_DATA_STREAM_BYTE_ORDER);
  pyg_enum_add(module, "DataStreamNewlineType", strip_prefix, G_TYPE_DATA_STREAM_NEWLINE_TYPE);
  pyg_enum_add(module, "FileAttributeType", strip_prefix, G_TYPE_FILE_ATTRIBUTE_TYPE);
  pyg_flags_add(module, "FileAttributeInfoFlags", strip_prefix, G_TYPE_FILE_ATTRIBUTE_INFO_FLAGS);
  pyg_enum_add(module, "FileAttributeStatus", strip_prefix, G_TYPE_FILE_ATTRIBUTE_STATUS);
  pyg_flags_add(module, "FileQueryInfoFlags", strip_prefix, G_TYPE_FILE_QUERY_INFO_FLAGS);
  pyg_flags_add(module, "FileCreateFlags", strip_prefix, G_TYPE_FILE_CREATE_FLAGS);
  pyg_flags_add(module, "MountUnmountFlags", strip_prefix, G_TYPE_MOUNT_UNMOUNT_FLAGS);
  pyg_flags_add(module, "FileCopyFlags", strip_prefix, G_TYPE_FILE_COPY_FLAGS);
  pyg_flags_add(module, "FileMonitorFlags", strip_prefix, G_TYPE_FILE_MONITOR_FLAGS);
  pyg_enum_add(module, "FileType", strip_prefix, G_TYPE_FILE_TYPE);
  pyg_enum_add(module, "FileMonitorEvent", strip_prefix, G_TYPE_FILE_MONITOR_EVENT);
  pyg_enum_add(module, "ErrorEnum", strip_prefix, G_TYPE_IO_ERROR_ENUM);
  pyg_flags_add(module, "AskPasswordFlags", strip_prefix, G_TYPE_ASK_PASSWORD_FLAGS);
  pyg_enum_add(module, "PasswordSave", strip_prefix, G_TYPE_PASSWORD_SAVE);
  pyg_enum_add(module, "MountOperationResult", strip_prefix, G_TYPE_MOUNT_OPERATION_RESULT);
  pyg_flags_add(module, "OutputStreamSpliceFlags", strip_prefix, G_TYPE_OUTPUT_STREAM_SPLICE_FLAGS);

  if (PyErr_Occurred())
    PyErr_Print();
}

/* initialise stuff extension classes */
void
pygio_register_classes(PyObject *d)
{
    PyObject *module;

    if ((module = PyImport_ImportModule("gobject")) != NULL) {
        _PyGObject_Type = (PyTypeObject *)PyObject_GetAttrString(module, "GObject");
        if (_PyGObject_Type == NULL) {
            PyErr_SetString(PyExc_ImportError,
                "cannot import name GObject from gobject");
            return ;
        }
    } else {
        PyErr_SetString(PyExc_ImportError,
            "could not import gobject");
        return ;
    }


#line 147 "gfileattribute.override"
if (PyType_Ready(&PyGFileAttributeInfo_Type) < 0) {
    g_return_if_reached();
}
if (PyDict_SetItemString(d, "FileAttributeInfo",
                         (PyObject *)&PyGFileAttributeInfo_Type) < 0) {
    g_return_if_reached();
}

#line 11662 "gio.c"
    pyg_register_interface(d, "AppInfo", G_TYPE_APP_INFO, &PyGAppInfo_Type);
    pyg_register_interface(d, "AsyncResult", G_TYPE_ASYNC_RESULT, &PyGAsyncResult_Type);
    pyg_register_interface(d, "Drive", G_TYPE_DRIVE, &PyGDrive_Type);
    pyg_register_interface(d, "File", G_TYPE_FILE, &PyGFile_Type);
    pyg_register_interface(d, "Icon", G_TYPE_ICON, &PyGIcon_Type);
    pyg_register_interface(d, "LoadableIcon", G_TYPE_LOADABLE_ICON, &PyGLoadableIcon_Type);
    pyg_register_interface(d, "Mount", G_TYPE_MOUNT, &PyGMount_Type);
    pyg_register_interface(d, "Seekable", G_TYPE_SEEKABLE, &PyGSeekable_Type);
    pyg_register_interface(d, "Volume", G_TYPE_VOLUME, &PyGVolume_Type);
    pygobject_register_class(d, "GAppLaunchContext", G_TYPE_APP_LAUNCH_CONTEXT, &PyGAppLaunchContext_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_APP_LAUNCH_CONTEXT);
    pygobject_register_class(d, "GCancellable", G_TYPE_CANCELLABLE, &PyGCancellable_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_CANCELLABLE);
    pygobject_register_class(d, "GFileEnumerator", G_TYPE_FILE_ENUMERATOR, &PyGFileEnumerator_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_FILE_ENUMERATOR);
    pygobject_register_class(d, "GFileInfo", G_TYPE_FILE_INFO, &PyGFileInfo_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_FILE_INFO);
    pygobject_register_class(d, "GFileMonitor", G_TYPE_FILE_MONITOR, &PyGFileMonitor_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_FILE_MONITOR);
    pygobject_register_class(d, "GInputStream", G_TYPE_INPUT_STREAM, &PyGInputStream_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_INPUT_STREAM);
    pygobject_register_class(d, "GFileInputStream", G_TYPE_FILE_INPUT_STREAM, &PyGFileInputStream_Type, Py_BuildValue("(O)", &PyGInputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_FILE_INPUT_STREAM);
    pygobject_register_class(d, "GFilterInputStream", G_TYPE_FILTER_INPUT_STREAM, &PyGFilterInputStream_Type, Py_BuildValue("(O)", &PyGInputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_FILTER_INPUT_STREAM);
    pygobject_register_class(d, "GBufferedInputStream", G_TYPE_BUFFERED_INPUT_STREAM, &PyGBufferedInputStream_Type, Py_BuildValue("(O)", &PyGFilterInputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_BUFFERED_INPUT_STREAM);
    pygobject_register_class(d, "GDataInputStream", G_TYPE_DATA_INPUT_STREAM, &PyGDataInputStream_Type, Py_BuildValue("(O)", &PyGFilterInputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_DATA_INPUT_STREAM);
    pygobject_register_class(d, "GMemoryInputStream", G_TYPE_MEMORY_INPUT_STREAM, &PyGMemoryInputStream_Type, Py_BuildValue("(O)", &PyGInputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_MEMORY_INPUT_STREAM);
    pygobject_register_class(d, "GMountOperation", G_TYPE_MOUNT_OPERATION, &PyGMountOperation_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_MOUNT_OPERATION);
    pygobject_register_class(d, "GOutputStream", G_TYPE_OUTPUT_STREAM, &PyGOutputStream_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_OUTPUT_STREAM);
    pygobject_register_class(d, "GMemoryOutputStream", G_TYPE_MEMORY_OUTPUT_STREAM, &PyGMemoryOutputStream_Type, Py_BuildValue("(O)", &PyGOutputStream_Type));
    pygobject_register_class(d, "GFilterOutputStream", G_TYPE_FILTER_OUTPUT_STREAM, &PyGFilterOutputStream_Type, Py_BuildValue("(O)", &PyGOutputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_FILTER_OUTPUT_STREAM);
    pygobject_register_class(d, "GDataOutputStream", G_TYPE_DATA_OUTPUT_STREAM, &PyGDataOutputStream_Type, Py_BuildValue("(O)", &PyGFilterOutputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_DATA_OUTPUT_STREAM);
    pygobject_register_class(d, "GFileOutputStream", G_TYPE_FILE_OUTPUT_STREAM, &PyGFileOutputStream_Type, Py_BuildValue("(O)", &PyGOutputStream_Type));
    pyg_set_object_has_new_constructor(G_TYPE_FILE_OUTPUT_STREAM);
    pygobject_register_class(d, "GSimpleAsyncResult", G_TYPE_SIMPLE_ASYNC_RESULT, &PyGSimpleAsyncResult_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pygobject_register_class(d, "GVfs", G_TYPE_VFS, &PyGVfs_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_VFS);
    pygobject_register_class(d, "GVolumeMonitor", G_TYPE_VOLUME_MONITOR, &PyGVolumeMonitor_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pyg_set_object_has_new_constructor(G_TYPE_VOLUME_MONITOR);
    pygobject_register_class(d, "GNativeVolumeMonitor", G_TYPE_NATIVE_VOLUME_MONITOR, &PyGNativeVolumeMonitor_Type, Py_BuildValue("(O)", &PyGVolumeMonitor_Type));
    pyg_set_object_has_new_constructor(G_TYPE_NATIVE_VOLUME_MONITOR);
    pygobject_register_class(d, "GFileIcon", G_TYPE_FILE_ICON, &PyGFileIcon_Type, Py_BuildValue("(OOO)", &PyGObject_Type, &PyGIcon_Type, &PyGLoadableIcon_Type));
    pygobject_register_class(d, "GThemedIcon", G_TYPE_THEMED_ICON, &PyGThemedIcon_Type, Py_BuildValue("(OO)", &PyGObject_Type, &PyGIcon_Type));
    pyg_set_object_has_new_constructor(G_TYPE_THEMED_ICON);
}
