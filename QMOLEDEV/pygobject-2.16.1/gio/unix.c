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


#line 24 "unix.override"
#define NO_IMPORT_PYGOBJECT
#include <pygobject.h>
#include <gio/gdesktopappinfo.h>
#include <gio/gunixinputstream.h>
#include <gio/gunixmounts.h>
#include <gio/gunixoutputstream.h>

#define GIO_UNIX_MOUNT_ENTRY_TYPE (_gio_unix_mount_entry_get_type ())

static GType _gio_unix_mount_entry_get_type (void)
{
  static GType our_type = 0;
  
  if (our_type == 0)
    our_type = g_pointer_type_register_static ("GUnixMountEntry");

  return our_type;
}

#line 40 "unix.c"


/* ---------- types from other modules ---------- */
static PyTypeObject *_PyGObject_Type;
#define PyGObject_Type (*_PyGObject_Type)
static PyTypeObject *_PyGInputStream_Type;
#define PyGInputStream_Type (*_PyGInputStream_Type)
static PyTypeObject *_PyGOutputStream_Type;
#define PyGOutputStream_Type (*_PyGOutputStream_Type)


/* ---------- forward type declarations ---------- */
PyTypeObject G_GNUC_INTERNAL PyGDesktopAppInfo_Type;
PyTypeObject G_GNUC_INTERNAL PyGUnixInputStream_Type;
PyTypeObject G_GNUC_INTERNAL PyGUnixOutputStream_Type;

#line 57 "unix.c"



/* ----------- GUnixMountEntry ----------- */

static int
pygobject_no_constructor(PyObject *self, PyObject *args, PyObject *kwargs)
{
    gchar buf[512];

    g_snprintf(buf, sizeof(buf), "%s is an abstract widget", self->ob_type->tp_name);
    PyErr_SetString(PyExc_NotImplementedError, buf);
    return -1;
}

PyTypeObject G_GNUC_INTERNAL PyGUnixMountEntry_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.unix.MountEntry",                   /* tp_name */
    sizeof(PyGPointer),          /* tp_basicsize */
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
    (struct PyMethodDef*)NULL, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    0,                 /* tp_dictoffset */
    (initproc)pygobject_no_constructor,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GDesktopAppInfo ----------- */

static int
_wrap_g_desktop_app_info_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "desktop_id", NULL };
    char *desktop_id;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:GDesktopAppInfo.__init__", kwlist, &desktop_id))
        return -1;
    self->obj = (GObject *)g_desktop_app_info_new(desktop_id);

    if (!self->obj) {
        PyErr_SetString(PyExc_RuntimeError, "could not create GDesktopAppInfo object");
        return -1;
    }
    pygobject_register_wrapper((PyObject *)self);
    return 0;
}

static PyObject *
_wrap_g_desktop_app_info_get_is_hidden(PyGObject *self)
{
    int ret;

    
    ret = g_desktop_app_info_get_is_hidden(G_DESKTOP_APP_INFO(self->obj));
    
    return PyBool_FromLong(ret);

}

static const PyMethodDef _PyGDesktopAppInfo_methods[] = {
    { "get_is_hidden", (PyCFunction)_wrap_g_desktop_app_info_get_is_hidden, METH_NOARGS,
      NULL },
    { NULL, NULL, 0, NULL }
};

PyTypeObject G_GNUC_INTERNAL PyGDesktopAppInfo_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.unix.DesktopAppInfo",                   /* tp_name */
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
    (char *) "DesktopAppInfo(desktop_id) -> gio.unix.DesktopAppInfo\n\n"
"gio.Unix.DesktopAppInfo is an implementation of gio.AppInfo\n"
"based on desktop files.",                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    offsetof(PyGObject, weakreflist),             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyGDesktopAppInfo_methods, /* tp_methods */
    (struct PyMemberDef*)0,              /* tp_members */
    (struct PyGetSetDef*)0,  /* tp_getset */
    NULL,                              /* tp_base */
    NULL,                              /* tp_dict */
    (descrgetfunc)0,    /* tp_descr_get */
    (descrsetfunc)0,    /* tp_descr_set */
    offsetof(PyGObject, inst_dict),                 /* tp_dictoffset */
    (initproc)_wrap_g_desktop_app_info_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GUnixInputStream ----------- */

static int
_wrap_g_unix_input_stream_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "fd", "close_fd_at_close", NULL };
    int fd, close_fd_at_close;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ii:GUnixInputStream.__init__", kwlist, &fd, &close_fd_at_close))
        return -1;
    self->obj = (GObject *)g_unix_input_stream_new(fd, close_fd_at_close);

    if (!self->obj) {
        PyErr_SetString(PyExc_RuntimeError, "could not create GUnixInputStream object");
        return -1;
    }
    pygobject_register_wrapper((PyObject *)self);
    return 0;
}

PyTypeObject G_GNUC_INTERNAL PyGUnixInputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.unix.InputStream",                   /* tp_name */
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
    (initproc)_wrap_g_unix_input_stream_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- GUnixOutputStream ----------- */

static int
_wrap_g_unix_output_stream_new(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "fd", "close_fd_at_close", NULL };
    int fd, close_fd_at_close;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"ii:GUnixOutputStream.__init__", kwlist, &fd, &close_fd_at_close))
        return -1;
    self->obj = (GObject *)g_unix_output_stream_new(fd, close_fd_at_close);

    if (!self->obj) {
        PyErr_SetString(PyExc_RuntimeError, "could not create GUnixOutputStream object");
        return -1;
    }
    pygobject_register_wrapper((PyObject *)self);
    return 0;
}

PyTypeObject G_GNUC_INTERNAL PyGUnixOutputStream_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "gio.unix.OutputStream",                   /* tp_name */
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
    (initproc)_wrap_g_unix_output_stream_new,             /* tp_init */
    (allocfunc)0,           /* tp_alloc */
    (newfunc)0,               /* tp_new */
    (freefunc)0,             /* tp_free */
    (inquiry)0              /* tp_is_gc */
};



/* ----------- functions ----------- */

static PyObject *
_wrap_g_desktop_app_info_new_from_filename(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "filename", NULL };
    char *filename;
    GDesktopAppInfo *ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:desktop_app_info_new_from_filename", kwlist, &filename))
        return NULL;
    
    ret = g_desktop_app_info_new_from_filename(filename);
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_desktop_app_info_set_desktop_env(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "desktop_env", NULL };
    char *desktop_env;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:desktop_app_info_set_desktop_env", kwlist, &desktop_env))
        return NULL;
    
    g_desktop_app_info_set_desktop_env(desktop_env);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_unix_mount_free(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_free", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    g_unix_mount_free(mount_entry);
    
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_unix_mount_compare(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount1", "mount2", NULL };
    PyObject *py_mount1, *py_mount2;
    int ret;
    GUnixMountEntry *mount1 = NULL, *mount2 = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"OO:unix_mount_compare", kwlist, &py_mount1, &py_mount2))
        return NULL;
    if (pyg_pointer_check(py_mount1, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount1 = pyg_pointer_get(py_mount1, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount1 should be a GUnixMountEntry");
        return NULL;
    }
    if (pyg_pointer_check(py_mount2, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount2 = pyg_pointer_get(py_mount2, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount2 should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_compare(mount1, mount2);
    
    return PyInt_FromLong(ret);
}

static PyObject *
_wrap_g_unix_mount_get_mount_path(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    const gchar *ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_get_mount_path", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_get_mount_path(mount_entry);
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_unix_mount_get_device_path(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    const gchar *ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_get_device_path", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_get_device_path(mount_entry);
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_unix_mount_get_fs_type(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    const gchar *ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_get_fs_type", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_get_fs_type(mount_entry);
    
    if (ret)
        return PyString_FromString(ret);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_unix_mount_is_readonly(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    int ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_is_readonly", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_is_readonly(mount_entry);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_unix_mount_is_system_internal(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    int ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_is_system_internal", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_is_system_internal(mount_entry);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_unix_mount_guess_can_eject(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    int ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_guess_can_eject", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_guess_can_eject(mount_entry);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_unix_mount_guess_should_display(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    int ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_guess_should_display", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_guess_should_display(mount_entry);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_unix_mount_guess_name(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    gchar *ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_guess_name", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_guess_name(mount_entry);
    
    if (ret) {
        PyObject *py_ret = PyString_FromString(ret);
        g_free(ret);
        return py_ret;
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_g_unix_mount_guess_icon(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_entry", NULL };
    PyObject *py_mount_entry;
    GIcon *ret;
    GUnixMountEntry *mount_entry = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O:unix_mount_guess_icon", kwlist, &py_mount_entry))
        return NULL;
    if (pyg_pointer_check(py_mount_entry, GIO_UNIX_MOUNT_ENTRY_TYPE))
        mount_entry = pyg_pointer_get(py_mount_entry, GUnixMountEntry);
    else {
        PyErr_SetString(PyExc_TypeError, "mount_entry should be a GUnixMountEntry");
        return NULL;
    }
    
    ret = g_unix_mount_guess_icon(mount_entry);
    
    /* pygobject_new handles NULL checking */
    return pygobject_new((GObject *)ret);
}

static PyObject *
_wrap_g_unix_mounts_changed_since(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "time", NULL };
    PyObject *py_time = NULL;
    int ret;
    guint64 time;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:unix_mounts_changed_since", kwlist, &PyLong_Type, &py_time))
        return NULL;
    time = PyLong_AsUnsignedLongLong(py_time);
    
    ret = g_unix_mounts_changed_since(time);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_unix_mount_points_changed_since(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "time", NULL };
    PyObject *py_time = NULL;
    int ret;
    guint64 time;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:unix_mount_points_changed_since", kwlist, &PyLong_Type, &py_time))
        return NULL;
    time = PyLong_AsUnsignedLongLong(py_time);
    
    ret = g_unix_mount_points_changed_since(time);
    
    return PyBool_FromLong(ret);

}

static PyObject *
_wrap_g_unix_is_mount_path_system_internal(PyObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "mount_path", NULL };
    char *mount_path;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,"s:unix_is_mount_path_system_internal", kwlist, &mount_path))
        return NULL;
    
    ret = g_unix_is_mount_path_system_internal(mount_path);
    
    return PyBool_FromLong(ret);

}

const PyMethodDef pyunix_functions[] = {
    { "desktop_app_info_new_from_filename", (PyCFunction)_wrap_g_desktop_app_info_new_from_filename, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "desktop_app_info_set_desktop_env", (PyCFunction)_wrap_g_desktop_app_info_set_desktop_env, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_free", (PyCFunction)_wrap_g_unix_mount_free, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_compare", (PyCFunction)_wrap_g_unix_mount_compare, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_get_mount_path", (PyCFunction)_wrap_g_unix_mount_get_mount_path, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_get_device_path", (PyCFunction)_wrap_g_unix_mount_get_device_path, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_get_fs_type", (PyCFunction)_wrap_g_unix_mount_get_fs_type, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_is_readonly", (PyCFunction)_wrap_g_unix_mount_is_readonly, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_is_system_internal", (PyCFunction)_wrap_g_unix_mount_is_system_internal, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_guess_can_eject", (PyCFunction)_wrap_g_unix_mount_guess_can_eject, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_guess_should_display", (PyCFunction)_wrap_g_unix_mount_guess_should_display, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_guess_name", (PyCFunction)_wrap_g_unix_mount_guess_name, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_guess_icon", (PyCFunction)_wrap_g_unix_mount_guess_icon, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mounts_changed_since", (PyCFunction)_wrap_g_unix_mounts_changed_since, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_mount_points_changed_since", (PyCFunction)_wrap_g_unix_mount_points_changed_since, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "unix_is_mount_path_system_internal", (PyCFunction)_wrap_g_unix_is_mount_path_system_internal, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { NULL, NULL, 0, NULL }
};

/* initialise stuff extension classes */
void
pyunix_register_classes(PyObject *d)
{
    PyObject *module;

    if ((module = PyImport_ImportModule("gio")) != NULL) {
        _PyGInputStream_Type = (PyTypeObject *)PyObject_GetAttrString(module, "InputStream");
        if (_PyGInputStream_Type == NULL) {
            PyErr_SetString(PyExc_ImportError,
                "cannot import name InputStream from gio");
            return ;
        }
        _PyGOutputStream_Type = (PyTypeObject *)PyObject_GetAttrString(module, "OutputStream");
        if (_PyGOutputStream_Type == NULL) {
            PyErr_SetString(PyExc_ImportError,
                "cannot import name OutputStream from gio");
            return ;
        }
    } else {
        PyErr_SetString(PyExc_ImportError,
            "could not import gio");
        return ;
    }
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


#line 768 "unix.c"
    pyg_register_pointer(d, "MountEntry", GIO_UNIX_MOUNT_ENTRY_TYPE, &PyGUnixMountEntry_Type);
    pygobject_register_class(d, "GDesktopAppInfo", G_TYPE_DESKTOP_APP_INFO, &PyGDesktopAppInfo_Type, Py_BuildValue("(O)", &PyGObject_Type));
    pygobject_register_class(d, "GUnixInputStream", G_TYPE_UNIX_INPUT_STREAM, &PyGUnixInputStream_Type, Py_BuildValue("(O)", &PyGInputStream_Type));
    pygobject_register_class(d, "GUnixOutputStream", G_TYPE_UNIX_OUTPUT_STREAM, &PyGUnixOutputStream_Type, Py_BuildValue("(O)", &PyGOutputStream_Type));
}
