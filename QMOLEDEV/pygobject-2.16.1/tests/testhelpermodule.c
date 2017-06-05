#include "pygobject.h"
#include <gobject/gmarshal.h>

#include "test-thread.h"
#include "test-unknown.h"

static PyTypeObject *_PyGObject_Type;
#define PyGObject_Type (*_PyGObject_Type)

static PyObject * _wrap_TestInterface__do_iface_method(PyObject *cls,
						       PyObject *args,
						       PyObject *kwargs);

GType
test_type_get_type(void)
{
    static GType gtype = 0;
    GType parent_type;
    
    if (gtype == 0)
    {
        GTypeInfo *type_info;
        GTypeQuery query;
	
	parent_type = g_type_from_name("PyGObject");
	if (parent_type == 0)
	     g_error("could not get PyGObject from testmodule");

	type_info = (GTypeInfo *)g_new0(GTypeInfo, 1);
	
        g_type_query(parent_type, &query);
        type_info->class_size = query.class_size;
        type_info->instance_size = query.instance_size;
	
        gtype = g_type_register_static(parent_type,
				       "TestType", type_info, 0);
	if (!gtype)
	     g_error("Could not register TestType");
    }
    
    return gtype;
}

#define TYPE_TEST (test_type_get_type())

static PyObject *
_wrap_get_test_thread (PyObject * self)
{
  GObject *obj;

  test_thread_get_type();
  g_assert (g_type_is_a (TEST_TYPE_THREAD, G_TYPE_OBJECT));
  obj = g_object_new (TEST_TYPE_THREAD, NULL);
  g_assert (obj != NULL);
  
  return pygobject_new(obj);
}

static PyObject *
_wrap_get_unknown (PyObject * self)
{
  GObject *obj;
  obj = g_object_new (TEST_TYPE_UNKNOWN, NULL);
  return pygobject_new(obj);
}

static PyObject *
_wrap_create_test_type (PyObject * self)
{
    GObject *obj;
    PyObject *rv;
    obj = g_object_new(TYPE_TEST, NULL);
    rv = pygobject_new(obj);
    g_object_unref(obj);
    return rv;
}

static PyObject *
_wrap_test_g_object_new (PyObject * self)
{
    GObject *obj;
    PyObject *rv;

    obj = g_object_new(g_type_from_name("PyGObject"), NULL);
    rv = PyInt_FromLong(obj->ref_count); /* should be == 2 at this point */
    g_object_unref(obj);
    return rv;
}

/* TestUnknown */
static PyObject *
_wrap_test_interface_iface_method(PyGObject *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,":", kwlist))
        return NULL;
    
    test_interface_iface_method(TEST_INTERFACE(self->obj));
    
    Py_INCREF(Py_None);
    return Py_None;
}

static const PyMethodDef _PyTestInterface_methods[] = {
    { "iface_method", (PyCFunction)_wrap_test_interface_iface_method, METH_VARARGS|METH_KEYWORDS,
      NULL },
    { "do_iface_method", (PyCFunction)_wrap_TestInterface__do_iface_method, METH_VARARGS|METH_KEYWORDS|METH_CLASS,
      NULL },
    { NULL, NULL, 0, NULL }
};

/* TestInterface */
PyTypeObject PyTestInterface_Type = {
    PyObject_HEAD_INIT(NULL)
    0,					/* ob_size */
    "test.Interface",			/* tp_name */
    sizeof(PyObject),	        /* tp_basicsize */
    0,					/* tp_itemsize */
    /* methods */
    (destructor)0,	/* tp_dealloc */
    (printfunc)0,			/* tp_print */
    (getattrfunc)0,	/* tp_getattr */
    (setattrfunc)0,	/* tp_setattr */
    (cmpfunc)0,		/* tp_compare */
    (reprfunc)0,		/* tp_repr */
    (PyNumberMethods*)0,     /* tp_as_number */
    (PySequenceMethods*)0, /* tp_as_sequence */
    (PyMappingMethods*)0,   /* tp_as_mapping */
    (hashfunc)0,		/* tp_hash */
    (ternaryfunc)0,		/* tp_call */
    (reprfunc)0,		/* tp_str */
    (getattrofunc)0,			/* tp_getattro */
    (setattrofunc)0,			/* tp_setattro */
    (PyBufferProcs*)0,	/* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    NULL,                        /* Documentation string */
    (traverseproc)0,     /* tp_traverse */
    (inquiry)0,             /* tp_clear */
    (richcmpfunc)0,   /* tp_richcompare */
    0,             /* tp_weaklistoffset */
    (getiterfunc)0,          /* tp_iter */
    (iternextfunc)0,     /* tp_iternext */
    (struct PyMethodDef*)_PyTestInterface_methods, /* tp_methods */
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

static PyObject *
_wrap_TestInterface__do_iface_method(PyObject *cls, PyObject *args, PyObject *kwargs)
{
  TestInterfaceIface *iface;
  static char *kwlist[] = { "self", NULL };
  PyGObject *self;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs,"O!:TestInterface.iface_method", kwlist, &PyTestInterface_Type, &self))
    return NULL;
  
  iface = g_type_interface_peek(g_type_class_peek(pyg_type_from_object(cls)),
				TEST_TYPE_INTERFACE);
  if (iface->iface_method)
    iface->iface_method(TEST_INTERFACE(self->obj));
  else {
    PyErr_SetString(PyExc_NotImplementedError,
		    "interface method TestInterface.iface_method not implemented");
    return NULL;
  }
  Py_INCREF(Py_None);
  return Py_None;
}

PyTypeObject PyTestUnknown_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size */
    "testhelper.Unknown",            /* tp_name */
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
    (struct PyMethodDef*)0, /* tp_methods */
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


static void
_wrap_TestInterface__proxy_do_iface_method(TestInterface *self)
{
    PyGILState_STATE __py_state;
    PyObject *py_self;
    PyObject *py_retval;
    PyObject *py_args;
    PyObject *py_method;
    
    __py_state = pyg_gil_state_ensure();
    py_self = pygobject_new((GObject *) self);
    if (!py_self) {
        if (PyErr_Occurred())
            PyErr_Print();
        pyg_gil_state_release(__py_state);
        return;
    }
    py_args = PyTuple_New(0);
    py_method = PyObject_GetAttrString(py_self, "do_iface_method");
    if (!py_method) {
        if (PyErr_Occurred())
            PyErr_Print();
        Py_DECREF(py_args);
        Py_DECREF(py_self);
        pyg_gil_state_release(__py_state);
        return;
    }
    py_retval = PyObject_CallObject(py_method, py_args);
    if (!py_retval) {
        if (PyErr_Occurred())
            PyErr_Print();
        Py_DECREF(py_method);
        Py_DECREF(py_args);
        Py_DECREF(py_self);
        pyg_gil_state_release(__py_state);
        return;
    }
    if (py_retval != Py_None) {
        if (PyErr_Occurred())
            PyErr_Print();
        PyErr_SetString(PyExc_TypeError, "retval should be None");
        Py_DECREF(py_retval);
        Py_DECREF(py_method);
        Py_DECREF(py_args);
        Py_DECREF(py_self);
        pyg_gil_state_release(__py_state);
        return;
    }
    
    Py_DECREF(py_retval);
    Py_DECREF(py_method);
    Py_DECREF(py_args);
    Py_DECREF(py_self);
    pyg_gil_state_release(__py_state);
}

static void
__TestInterface__interface_init(TestInterfaceIface *iface,
				PyTypeObject *pytype)
{
    TestInterfaceIface *parent_iface = g_type_interface_peek_parent(iface);
    PyObject *py_method;

    py_method = pytype ? PyObject_GetAttrString((PyObject *) pytype,
						"do_iface_method") : NULL;

    if (py_method && !PyObject_TypeCheck(py_method, &PyCFunction_Type)) {
        iface->iface_method = _wrap_TestInterface__proxy_do_iface_method;
    } else {
        PyErr_Clear();
        if (parent_iface) {
            iface->iface_method = parent_iface->iface_method;
        }
	Py_XDECREF(py_method);
    }
}

static const GInterfaceInfo __TestInterface__iinfo = {
    (GInterfaceInitFunc) __TestInterface__interface_init,
    NULL,
    NULL
};

#include <string.h>
#include <glib-object.h>

static void
test1_callback (GObject *object, char *data)
{
  g_return_if_fail (G_IS_OBJECT (object));
  g_return_if_fail (!strcmp (data, "user-data"));
}

static void
test1_callback_swapped (char *data, GObject *object)
{
  g_return_if_fail (G_IS_OBJECT (object));
  g_return_if_fail (!strcmp (data, "user-data"));
}

static void
test2_callback (GObject *object, char *string)
{
  g_return_if_fail (G_IS_OBJECT (object));
  g_return_if_fail (!strcmp (string, "string"));
}

static int
test3_callback (GObject *object, double d)
{
  g_return_val_if_fail (G_IS_OBJECT (object), -1);
  g_return_val_if_fail (d == 42.0, -1);

  return 20;
}

static void
test4_callback (GObject *object,
                gboolean b, long l, float f, double d, guint uint, gulong ulong,
                gpointer user_data)
{
  g_return_if_fail (b == TRUE);
  g_return_if_fail (l == 10L);
  g_return_if_fail (f <= 3.14001 && f >= 3.13999);
  g_return_if_fail (d == 1.78);
  g_return_if_fail (uint == 20);
  g_return_if_fail (ulong == 30L);
}

static float
test_float_callback (GObject *object, float f)
{
  g_return_val_if_fail (G_IS_OBJECT (object), -1);
  g_return_val_if_fail (f <= 1.234001 && f >= 1.123999, -1);

  return f;
}

static double
test_double_callback (GObject *object, double d)
{
  g_return_val_if_fail (G_IS_OBJECT (object), -1);
  g_return_val_if_fail (d <= 1.234001 && d >= 1.123999, -1);

  return d;
}

static char *
test_string_callback (GObject *object, char *s)
{
  g_return_val_if_fail (G_IS_OBJECT (object), NULL);
  g_return_val_if_fail (!strcmp(s, "str"), NULL);

  return g_strdup (s);
}

static GObject *
test_object_callback (GObject *object, GObject *o)
{
  g_return_val_if_fail (G_IS_OBJECT (object), NULL);

  return o;
}

void
connectcallbacks (GObject *object)
{

  gchar *data = "user-data";
  
  g_signal_connect (G_OBJECT (object),
                    "test1",
                    G_CALLBACK (test1_callback), 
                    data);
  g_signal_connect_swapped (G_OBJECT (object),
                    "test1",
                    G_CALLBACK (test1_callback_swapped), 
                    data);
  g_signal_connect (G_OBJECT (object),
                    "test2",
                    G_CALLBACK (test2_callback), 
                    NULL);
  g_signal_connect (G_OBJECT (object),
                    "test3",
                    G_CALLBACK (test3_callback), 
                    NULL);
  g_signal_connect (G_OBJECT (object),
                    "test4",
                    G_CALLBACK (test4_callback), 
                    NULL);
  g_signal_connect (G_OBJECT (object),
                    "test_float",
                    G_CALLBACK (test_float_callback), 
                    NULL);
  g_signal_connect (G_OBJECT (object),
                    "test_double",
                    G_CALLBACK (test_double_callback), 
                    NULL);
  g_signal_connect (G_OBJECT (object),
                    "test_string",
                    G_CALLBACK (test_string_callback), 
                    NULL);
  g_signal_connect (G_OBJECT (object),
                    "test_object",
                    G_CALLBACK (test_object_callback), 
                    NULL);
}

static PyObject *
_wrap_connectcallbacks(PyObject * self, PyObject *args)
{
    PyGObject *obj;

    if (!PyArg_ParseTuple(args, "O", &obj))
      return NULL;

    connectcallbacks (G_OBJECT (obj->obj));

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
_wrap_test_value(PyObject *self, PyObject *args)
{
  GValue tvalue = {0,}, *value = &tvalue;
  PyObject *obj;

  if (!PyArg_ParseTuple(args, "O", &obj))
    return NULL;

  g_value_init(value, G_TYPE_VALUE);
  if (pyg_value_from_pyobject(value, obj)) {
    PyErr_SetString(PyExc_TypeError, "Could not convert to GValue");
    return NULL;
  }
  
  return pyg_value_as_pyobject(value, FALSE);
}

static PyObject *
_wrap_test_value_array(PyObject *self, PyObject *args)
{
  GValue tvalue = {0,}, *value = &tvalue;
  PyObject *obj;

  if (!PyArg_ParseTuple(args, "O", &obj))
    return NULL;

  g_value_init(value, G_TYPE_VALUE_ARRAY);
  if (pyg_value_from_pyobject(value, obj)) {
    PyErr_SetString(PyExc_TypeError, "Could not convert to GValueArray");
    return NULL;
  }
  
  return pyg_value_as_pyobject(value, FALSE);
}

static PyObject *
_wrap_test_gerror_exception(PyObject *self, PyObject *args)
{
    PyObject *py_method;
    PyObject *py_args;
    PyObject *py_ret;
    GError *err = NULL;

    if (!PyArg_ParseTuple(args, "O", &py_method))
        return NULL;

    py_args = PyTuple_New(0);
    py_ret = PyObject_CallObject(py_method, py_args);
    if (pyg_gerror_exception_check(&err)) {
        pyg_error_check(&err);
        return NULL;
    }	    

    Py_DECREF(py_method);
    Py_DECREF(py_args);
    Py_DECREF(py_ret);

    Py_INCREF(Py_None);
    return Py_None;
}

static PyMethodDef testhelper_functions[] = {
    { "get_test_thread", (PyCFunction)_wrap_get_test_thread, METH_NOARGS },
    { "get_unknown", (PyCFunction)_wrap_get_unknown, METH_NOARGS },
    { "create_test_type", (PyCFunction)_wrap_create_test_type, METH_NOARGS },
    { "test_g_object_new", (PyCFunction)_wrap_test_g_object_new, METH_NOARGS },
    { "connectcallbacks", (PyCFunction)_wrap_connectcallbacks, METH_VARARGS },
    { "test_value", (PyCFunction)_wrap_test_value, METH_VARARGS },      
    { "test_value_array", (PyCFunction)_wrap_test_value_array, METH_VARARGS },
    { "test_gerror_exception", (PyCFunction)_wrap_test_gerror_exception, METH_VARARGS },
    { NULL, NULL }
};

void 
inittesthelper ()
{
  PyObject *m, *d;
  PyObject *module;
  
  g_thread_init(NULL);
  init_pygobject();
  m = Py_InitModule ("testhelper", testhelper_functions);

  d = PyModule_GetDict(m);

  if ((module = PyImport_ImportModule("gobject")) != NULL) {
    PyObject *moddict = PyModule_GetDict(module);
    
    _PyGObject_Type = (PyTypeObject *)PyDict_GetItemString(moddict, "GObject");
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

  /* TestInterface */
  pyg_register_interface(d, "Interface", TEST_TYPE_INTERFACE,
			 &PyTestInterface_Type);
  pyg_register_interface_info(TEST_TYPE_INTERFACE, &__TestInterface__iinfo);


  /* TestUnknown */
  pygobject_register_class(d, "Unknown", TEST_TYPE_UNKNOWN,
			   &PyTestUnknown_Type,
			   Py_BuildValue("(O)",
					 &PyGObject_Type,
					 &PyTestInterface_Type));
  pyg_set_object_has_new_constructor(TEST_TYPE_UNKNOWN);
  //pyg_register_class_init(TEST_TYPE_UNKNOWN, __GtkUIManager_class_init);

}

