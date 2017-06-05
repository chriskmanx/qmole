/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   pygtype.c: glue code to wrap the GType code.
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

#include "pygobject-private.h"
#include "pygparamspec.h"
#include "pygtype.h"

/* -------------- __gtype__ objects ---------------------------- */

typedef struct {
    PyObject_HEAD
    GType type;
} PyGTypeWrapper;

PYGLIB_DEFINE_TYPE("gobject.GType", PyGTypeWrapper_Type, PyGTypeWrapper);

static int
pyg_type_wrapper_compare(PyGTypeWrapper *self, PyGTypeWrapper *v)
{
    if (self->type == v->type) return 0;
    if (self->type > v->type) return -1;
    return 1;
}

static long
pyg_type_wrapper_hash(PyGTypeWrapper *self)
{
    return (long)self->type;
}

static PyObject *
pyg_type_wrapper_repr(PyGTypeWrapper *self)
{
    char buf[80];
    const gchar *name = g_type_name(self->type);

    g_snprintf(buf, sizeof(buf), "<GType %s (%lu)>",
	       name?name:"invalid", (unsigned long int) self->type);
    return _PyUnicode_FromString(buf);
}

static void
pyg_type_wrapper_dealloc(PyGTypeWrapper *self)
{
    PyObject_DEL(self);
}

static PyObject *
_wrap_g_type_wrapper__get_pytype(PyGTypeWrapper *self, void *closure)
{
    PyObject *py_type;
    
    py_type = g_type_get_qdata(self->type, pygobject_class_key);
    if (!py_type)
      py_type = Py_None;

    Py_INCREF(py_type);
    return py_type;
}

static PyObject *
_wrap_g_type_wrapper__get_name(PyGTypeWrapper *self, void *closure)
{
   const char *name = g_type_name(self->type);
   return _PyUnicode_FromString(name ? name : "invalid");
}

static PyObject *
_wrap_g_type_wrapper__get_parent(PyGTypeWrapper *self, void *closure)
{
   return pyg_type_wrapper_new(g_type_parent(self->type));
}

static PyObject *
_wrap_g_type_wrapper__get_fundamental(PyGTypeWrapper *self, void *closure)
{
   return pyg_type_wrapper_new(g_type_fundamental(self->type));
}

static PyObject *
_wrap_g_type_wrapper__get_children(PyGTypeWrapper *self, void *closure)
{
  guint n_children, i;
  GType *children;
  PyObject *retval;
    
  children = g_type_children(self->type, &n_children);

  retval = PyList_New(n_children);
  for (i = 0; i < n_children; i++)
      PyList_SetItem(retval, i, pyg_type_wrapper_new(children[i]));
  g_free(children);
  
  return retval;
}

static PyObject *
_wrap_g_type_wrapper__get_interfaces(PyGTypeWrapper *self, void *closure)
{
  guint n_interfaces, i;
  GType *interfaces;
  PyObject *retval;
    
  interfaces = g_type_interfaces(self->type, &n_interfaces);

  retval = PyList_New(n_interfaces);
  for (i = 0; i < n_interfaces; i++)
      PyList_SetItem(retval, i, pyg_type_wrapper_new(interfaces[i]));
  g_free(interfaces);
  
  return retval;
}

static PyObject *
_wrap_g_type_wrapper__get_depth(PyGTypeWrapper *self, void *closure)
{
  return _PyLong_FromLong(g_type_depth(self->type));
}

static PyGetSetDef _PyGTypeWrapper_getsets[] = {
    { "pytype", (getter)_wrap_g_type_wrapper__get_pytype, (setter)0 },
    { "name",  (getter)_wrap_g_type_wrapper__get_name, (setter)0 },
    { "fundamental",  (getter)_wrap_g_type_wrapper__get_fundamental, (setter)0 },
    { "parent",  (getter)_wrap_g_type_wrapper__get_parent, (setter)0 },
    { "children",  (getter)_wrap_g_type_wrapper__get_children, (setter)0 },
    { "interfaces",  (getter)_wrap_g_type_wrapper__get_interfaces, (setter)0 },
    { "depth",  (getter)_wrap_g_type_wrapper__get_depth, (setter)0 },
    { NULL, (getter)0, (setter)0 }
};

static PyObject*
_wrap_g_type_is_interface(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_INTERFACE(self->type));
}

static PyObject*
_wrap_g_type_is_classed(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_CLASSED(self->type));
}

static PyObject*
_wrap_g_type_is_instantiatable(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_INSTANTIATABLE(self->type));
}

static PyObject*
_wrap_g_type_is_derivable(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_DERIVABLE(self->type));
}

static PyObject*
_wrap_g_type_is_deep_derivable(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_DEEP_DERIVABLE(self->type));
}

static PyObject*
_wrap_g_type_is_abstract(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_ABSTRACT(self->type));
}

static PyObject*
_wrap_g_type_is_value_abstract(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_VALUE_ABSTRACT(self->type));
}

static PyObject*
_wrap_g_type_is_value_type(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_IS_VALUE_TYPE(self->type));
}

static PyObject*
_wrap_g_type_has_value_table(PyGTypeWrapper *self)
{
    return PyBool_FromLong(G_TYPE_HAS_VALUE_TABLE(self->type));
}

static PyObject*
_wrap_g_type_from_name(PyGTypeWrapper *_, PyObject *args)
{
    char *type_name;
    GType type;
    
    if (!PyArg_ParseTuple(args, "s:GType.from_name", &type_name))
	return NULL;

    type = _pyg_type_from_name(type_name);
    if (type == 0) {
	PyErr_SetString(PyExc_RuntimeError, "unknown type name");
	return NULL;
    }

    return pyg_type_wrapper_new(type);
}

static PyObject*
_wrap_g_type_is_a(PyGTypeWrapper *self, PyObject *args)
{
    PyObject *gparent;
    GType parent;

    if (!PyArg_ParseTuple(args, "O:GType.is_a", &gparent))
	return NULL;
    else if ((parent = pyg_type_from_object(gparent)) == 0)
	return NULL;
    
    return PyBool_FromLong(g_type_is_a(self->type, parent));
}

static PyMethodDef _PyGTypeWrapper_methods[] = {
    { "is_interface", (PyCFunction)_wrap_g_type_is_interface, METH_NOARGS },
    { "is_classed", (PyCFunction)_wrap_g_type_is_classed, METH_NOARGS },
    { "is_instantiatable", (PyCFunction)_wrap_g_type_is_instantiatable, METH_NOARGS },
    { "is_derivable", (PyCFunction)_wrap_g_type_is_derivable, METH_NOARGS },
    { "is_deep_derivable", (PyCFunction)_wrap_g_type_is_deep_derivable, METH_NOARGS },
    { "is_abstract", (PyCFunction)_wrap_g_type_is_abstract, METH_NOARGS },
    { "is_value_abstract", (PyCFunction)_wrap_g_type_is_value_abstract, METH_NOARGS },
    { "is_value_type", (PyCFunction)_wrap_g_type_is_value_type, METH_NOARGS },
    { "has_value_table", (PyCFunction)_wrap_g_type_has_value_table, METH_NOARGS },
    { "from_name", (PyCFunction)_wrap_g_type_from_name, METH_VARARGS | METH_STATIC },
    { "is_a", (PyCFunction)_wrap_g_type_is_a, METH_VARARGS },
    { NULL,  0, 0 }
};
    
static int
pyg_type_wrapper_init(PyGTypeWrapper *self, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "object", NULL };
    PyObject *py_object;
    GType type;
    
    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				     "O:GType.__init__",
				     kwlist, &py_object))
        return -1;

    if (!(type = pyg_type_from_object(py_object)))
	return -1;

    self->type = type;

    return 0;
}

/**
 * pyg_type_wrapper_new:
 * type: a GType
 *
 * Creates a Python wrapper for a GType.
 *
 * Returns: the Python wrapper.
 */
PyObject *
pyg_type_wrapper_new(GType type)
{
    PyGTypeWrapper *self;

    self = (PyGTypeWrapper *)PyObject_NEW(PyGTypeWrapper,
					  &PyGTypeWrapper_Type);
    if (self == NULL)
	return NULL;

    self->type = type;
    return (PyObject *)self;
}

/**
 * pyg_type_from_object:
 * obj: a Python object
 *
 * converts a python object to a GType.  Raises an exception if it
 * can't perform the conversion.
 *
 * Returns: the corresponding GType, or 0 on error.
 */
GType
pyg_type_from_object(PyObject *obj)
{
    PyObject *gtype;
    GType type;

    /* NULL check */
    if (!obj) {
	PyErr_SetString(PyExc_TypeError, "can't get type from NULL object");
	return 0;
    }

    /* map some standard types to primitive GTypes ... */
    if (obj == Py_None)
	return G_TYPE_NONE;
    if (PyType_Check(obj)) {
	PyTypeObject *tp = (PyTypeObject *)obj;

	if (tp == &_PyLong_Type)
	    return G_TYPE_INT;
	else if (tp == &PyBool_Type)
	    return G_TYPE_BOOLEAN;
	else if (tp == &PyLong_Type)
	    return G_TYPE_LONG;
	else if (tp == &PyFloat_Type)
	    return G_TYPE_DOUBLE;
	else if (tp == &_PyUnicode_Type)
	    return G_TYPE_STRING;
	else if (tp == &PyBaseObject_Type)
	    return PY_TYPE_OBJECT;
    }

    if (Py_TYPE(obj) == &PyGTypeWrapper_Type) {
	return ((PyGTypeWrapper *)obj)->type;
    }

    /* handle strings */
    if (_PyUnicode_Check(obj)) {
	gchar *name = _PyUnicode_AsString(obj);
	
	type = _pyg_type_from_name(name);
	if (type != 0) {
	    return type;
	}
    }

    /* finally, look for a __gtype__ attribute on the object */
    gtype = PyObject_GetAttrString(obj, "__gtype__");

    if (gtype) {
	if (Py_TYPE(gtype) == &PyGTypeWrapper_Type) {
	    type = ((PyGTypeWrapper *)gtype)->type;
	    Py_DECREF(gtype);
	    return type;
	}
	Py_DECREF(gtype);
    }

    PyErr_Clear();
    PyErr_SetString(PyExc_TypeError, "could not get typecode from object");
    return 0;
}

/* -------------- GValue marshalling ------------------ */

/**
 * pyg_enum_get_value:
 * @enum_type: the GType of the flag.
 * @obj: a Python object representing the flag value
 * @val: a pointer to the location to store the integer representation of the flag.
 *
 * Converts a Python object to the integer equivalent.  The conversion
 * will depend on the type of the Python object.  If the object is an
 * integer, it is passed through directly.  If it is a string, it will
 * be treated as a full or short enum name as defined in the GType.
 *
 * Returns: 0 on success or -1 on failure
 */
gint
pyg_enum_get_value(GType enum_type, PyObject *obj, gint *val)
{
    GEnumClass *eclass = NULL;
    gint res = -1;

    g_return_val_if_fail(val != NULL, -1);
    if (!obj) {
	*val = 0;
	res = 0;
    } else if (_PyLong_Check(obj)) {
	*val = _PyLong_AsLong(obj);
	res = 0;

	if (PyObject_TypeCheck(obj, &PyGEnum_Type) && ((PyGEnum *) obj)->gtype != enum_type) {
	    g_warning("expected enumeration type %s, but got %s instead",
		      g_type_name(enum_type),
		      g_type_name(((PyGEnum *) obj)->gtype));
	}
    } else if (_PyUnicode_Check(obj)) {
	GEnumValue *info;
	char *str = _PyUnicode_AsString(obj);
	
	if (enum_type != G_TYPE_NONE)
	    eclass = G_ENUM_CLASS(g_type_class_ref(enum_type));
	else {
	    PyErr_SetString(PyExc_TypeError, "could not convert string to enum because there is no GType associated to look up the value");
	    res = -1;
	}
	info = g_enum_get_value_by_name(eclass, str);
	g_type_class_unref(eclass);

	if (!info)
	    info = g_enum_get_value_by_nick(eclass, str);
	if (info) {
	    *val = info->value;
	    res = 0;
	} else {
	    PyErr_SetString(PyExc_TypeError, "could not convert string");
	    res = -1;
	}
    } else {
	PyErr_SetString(PyExc_TypeError,"enum values must be strings or ints");
	res = -1;
    }
    return res;
}

/**
 * pyg_flags_get_value:
 * @flag_type: the GType of the flag.
 * @obj: a Python object representing the flag value
 * @val: a pointer to the location to store the integer representation of the flag.
 *
 * Converts a Python object to the integer equivalent.  The conversion
 * will depend on the type of the Python object.  If the object is an
 * integer, it is passed through directly.  If it is a string, it will
 * be treated as a full or short flag name as defined in the GType.
 * If it is a tuple, then the items are treated as strings and ORed
 * together.
 *
 * Returns: 0 on success or -1 on failure
 */
gint
pyg_flags_get_value(GType flag_type, PyObject *obj, gint *val)
{
    GFlagsClass *fclass = NULL;
    gint res = -1;

    g_return_val_if_fail(val != NULL, -1);
    if (!obj) {
	*val = 0;
	res = 0;
    } else if (_PyLong_Check(obj)) {
	*val = _PyLong_AsLong(obj);
	res = 0;
    } else if (PyLong_Check(obj)) {
        *val = PyLong_AsLongLong(obj);
        res = 0;
    } else if (_PyUnicode_Check(obj)) {
	GFlagsValue *info;
	char *str = _PyUnicode_AsString(obj);

	if (flag_type != G_TYPE_NONE)
	    fclass = G_FLAGS_CLASS(g_type_class_ref(flag_type));
	else {
	    PyErr_SetString(PyExc_TypeError, "could not convert string to flag because there is no GType associated to look up the value");
	    res = -1;
	}
	info = g_flags_get_value_by_name(fclass, str);
	g_type_class_unref(fclass);
	
	if (!info)
	    info = g_flags_get_value_by_nick(fclass, str);
	if (info) {
	    *val = info->value;
	    res = 0;
	} else {
	    PyErr_SetString(PyExc_TypeError, "could not convert string");
	    res = -1;
	}
    } else if (PyTuple_Check(obj)) {
	int i, len;

	len = PyTuple_Size(obj);
	*val = 0;
	res = 0;

	if (flag_type != G_TYPE_NONE)
	    fclass = G_FLAGS_CLASS(g_type_class_ref(flag_type));
	else {
	    PyErr_SetString(PyExc_TypeError, "could not convert string to flag because there is no GType associated to look up the value");
	    res = -1;
	}

	for (i = 0; i < len; i++) {
	    PyObject *item = PyTuple_GetItem(obj, i);
	    char *str = _PyUnicode_AsString(item);
	    GFlagsValue *info = g_flags_get_value_by_name(fclass, str);

	    if (!info)
		info = g_flags_get_value_by_nick(fclass, str);
	    if (info) {
		*val |= info->value;
	    } else {
		PyErr_SetString(PyExc_TypeError, "could not convert string");
		res = -1;
		break;
	    }
	}
	g_type_class_unref(fclass);
    } else {
	PyErr_SetString(PyExc_TypeError,
			"flag values must be strings, ints, longs, or tuples");
	res = -1;
    }
    return res;
}

typedef struct {
    fromvaluefunc fromvalue;
    tovaluefunc tovalue;
} PyGTypeMarshal;
static GQuark pyg_type_marshal_key = 0;

static PyGTypeMarshal *
pyg_type_lookup(GType type)
{
    GType	ptype = type;
    PyGTypeMarshal	*tm = NULL;

    /* recursively lookup types */
    while (ptype) {
	if ((tm = g_type_get_qdata(ptype, pyg_type_marshal_key)) != NULL)
	    break;
	ptype = g_type_parent(ptype);
    }
    return tm;
}

/**
 * pyg_register_gtype_custom:
 * @gtype: the GType for the new type
 * @from_func: a function to convert GValues to Python objects
 * @to_func: a function to convert Python objects to GValues
 *
 * In order to handle specific conversion of gboxed types or new
 * fundamental types, you may use this function to register conversion
 * handlers.
 */

void
pyg_register_gtype_custom(GType gtype,
			  fromvaluefunc from_func,
                          tovaluefunc to_func)
{
    PyGTypeMarshal *tm;

    if (!pyg_type_marshal_key)
        pyg_type_marshal_key = g_quark_from_static_string("PyGType::marshal");

    tm = g_new(PyGTypeMarshal, 1);
    tm->fromvalue = from_func;
    tm->tovalue = to_func;
    g_type_set_qdata(gtype, pyg_type_marshal_key, tm);
}

static int
pyg_value_array_from_pyobject(GValue *value,
			      PyObject *obj,
			      const GParamSpecValueArray *pspec)
{
    int len;
    GValueArray *value_array;
    int i;

    len = PySequence_Length(obj);
    if (len == -1) {
	PyErr_Clear();
	return -1;
    }

    if (pspec && pspec->fixed_n_elements > 0 && len != pspec->fixed_n_elements)
	return -1;
	    
    value_array = g_value_array_new(len);

    for (i = 0; i < len; ++i) {
	PyObject *item = PySequence_GetItem(obj, i);
	GType type;
	GValue item_value = { 0, };
	int status;

	if (! item) {
	    PyErr_Clear();
	    g_value_array_free(value_array);
	    return -1;
	}

	if (pspec && pspec->element_spec)
	    type = G_PARAM_SPEC_VALUE_TYPE(pspec->element_spec);
	else if (item == Py_None)
	    type = G_TYPE_POINTER; /* store None as NULL */
	else {
	    type = pyg_type_from_object((PyObject*)Py_TYPE(item));
	    if (! type) {
		PyErr_Clear();
		g_value_array_free(value_array);
		Py_DECREF(item);
		return -1;
	    }
	}

	g_value_init(&item_value, type);
	status = (pspec && pspec->element_spec)
	    ? pyg_param_gvalue_from_pyobject(&item_value, item, pspec->element_spec)
	    : pyg_value_from_pyobject(&item_value, item);
	Py_DECREF(item);

	if (status == -1) {
	    g_value_array_free(value_array);
	    g_value_unset(&item_value);
	    return -1;
	}

	g_value_array_append(value_array, &item_value);
	g_value_unset(&item_value);
    }

    g_value_take_boxed(value, value_array);
    return 0;
}

/**
 * pyg_value_from_pyobject:
 * @value: the GValue object to store the converted value in.
 * @obj: the Python object to convert.
 *
 * This function converts a Python object and stores the result in a
 * GValue.  The GValue must be initialised in advance with
 * g_value_init().  If the Python object can't be converted to the
 * type of the GValue, then an error is returned.
 *
 * Returns: 0 on success, -1 on error.
 */
int
pyg_value_from_pyobject(GValue *value, PyObject *obj)
{
    PyObject *tmp;

    switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value))) {
    case G_TYPE_INTERFACE:
	/* we only handle interface types that have a GObject prereq */
	if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT)) {
	    if (obj == Py_None)
		g_value_set_object(value, NULL);
	    else {
		if (!PyObject_TypeCheck(obj, &PyGObject_Type)) {
		    return -1;
		}
		if (!G_TYPE_CHECK_INSTANCE_TYPE(pygobject_get(obj),
						G_VALUE_TYPE(value))) {
		    return -1;
		}
		g_value_set_object(value, pygobject_get(obj));
	    }
	} else {
	    return -1;
	}
	break;
    case G_TYPE_CHAR:
	if ((tmp = PyObject_Str(obj)))
	    g_value_set_char(value, _PyUnicode_AsString(tmp)[0]);
	else {
	    PyErr_Clear();
	    return -1;
	}
	Py_DECREF(tmp);
	break;
    case G_TYPE_UCHAR:
	if ((tmp = PyObject_Str(obj)))
	    g_value_set_char(value, _PyUnicode_AsString(tmp)[0]);
	else {
	    PyErr_Clear();
	    return -1;
	}
	Py_DECREF(tmp);
	break;
    case G_TYPE_BOOLEAN:
	g_value_set_boolean(value, PyObject_IsTrue(obj));
	break;
    case G_TYPE_INT:
	g_value_set_int(value, _PyLong_AsLong(obj));
	if (PyErr_Occurred()) {
	    g_value_unset(value);
	    PyErr_Clear();
	    return -1;
	}
	break;
    case G_TYPE_UINT:
	{
	    if (_PyLong_Check(obj)) {
		glong val;

		val = _PyLong_AsLong(obj);
		if (val >= 0 && val <= G_MAXUINT)
		    g_value_set_uint(value, (guint)val);
		else
		    return -1;
	    } else {
		g_value_set_uint(value, PyLong_AsUnsignedLong(obj));
		if (PyErr_Occurred()) {
		    g_value_unset(value);
		    PyErr_Clear();
		    return -1;
		}
	    }
	}
	break;
    case G_TYPE_LONG:
	g_value_set_long(value, _PyLong_AsLong(obj));
	if (PyErr_Occurred()) {
	    g_value_unset(value);
	    PyErr_Clear();
	    return -1;
	}
	break;
    case G_TYPE_ULONG:
	{
	    if (_PyLong_Check(obj)) {
		glong val;

		val = _PyLong_AsLong(obj);
		if (val >= 0)
		    g_value_set_ulong(value, (gulong)val);
		else
		    return -1;
	    } else {
		g_value_set_ulong(value, PyLong_AsUnsignedLong(obj));
		if (PyErr_Occurred()) {
		    g_value_unset(value);
		    PyErr_Clear();
		    return -1;
		}
	    }
	}
	break;
    case G_TYPE_INT64:
	g_value_set_int64(value, PyLong_AsLongLong(obj));
	if (PyErr_Occurred()) {
	    g_value_unset(value);
	    PyErr_Clear();
	    return -1;
	}
	break;
    case G_TYPE_UINT64:
        if (_PyLong_Check(obj))
            g_value_set_uint64(value, _PyLong_AsLong(obj));
        else if (PyLong_Check(obj))
            g_value_set_uint64(value, PyLong_AsUnsignedLongLong(obj));
        else
            return -1;
	break;
    case G_TYPE_ENUM:
	{
	    gint val = 0;
	    if (pyg_enum_get_value(G_VALUE_TYPE(value), obj, &val) < 0) {
		PyErr_Clear();
		return -1;
	    }
	    g_value_set_enum(value, val);
	}
	break;
    case G_TYPE_FLAGS:
	{
	    gint val = 0;
	    if (pyg_flags_get_value(G_VALUE_TYPE(value), obj, &val) < 0) {
		PyErr_Clear();
		return -1;
	    }
	    g_value_set_flags(value, val);
	}
	break;
    case G_TYPE_FLOAT:
	g_value_set_float(value, PyFloat_AsDouble(obj));
	if (PyErr_Occurred()) {
	    g_value_unset(value);
	    PyErr_Clear();
	    return -1;
	}
	break;
    case G_TYPE_DOUBLE:
	g_value_set_double(value, PyFloat_AsDouble(obj));
	if (PyErr_Occurred()) {
	    g_value_unset(value);
	    PyErr_Clear();
	    return -1;
	}
	break;
    case G_TYPE_STRING:
	if (obj == Py_None)
	    g_value_set_string(value, NULL);
	else if ((tmp = PyObject_Str(obj))) {
	    g_value_set_string(value, _PyUnicode_AsString(tmp));
	    Py_DECREF(tmp);
	} else {
	    PyErr_Clear();
	    return -1;
	}
	break;
    case G_TYPE_POINTER:
	if (obj == Py_None)
	    g_value_set_pointer(value, NULL);
	else if (PyObject_TypeCheck(obj, &PyGPointer_Type) &&
		   G_VALUE_HOLDS(value, ((PyGPointer *)obj)->gtype))
	    g_value_set_pointer(value, pyg_pointer_get(obj, gpointer));
	else if (PyCObject_Check(obj))
	    g_value_set_pointer(value, PyCObject_AsVoidPtr(obj));
	else
	    return -1;
	break;
    case G_TYPE_BOXED: {
	PyGTypeMarshal *bm;

	if (obj == Py_None)
	    g_value_set_boxed(value, NULL);
	else if (G_VALUE_HOLDS(value, PY_TYPE_OBJECT))
	    g_value_set_boxed(value, obj);
	else if (PyObject_TypeCheck(obj, &PyGBoxed_Type) &&
		   G_VALUE_HOLDS(value, ((PyGBoxed *)obj)->gtype))
	    g_value_set_boxed(value, pyg_boxed_get(obj, gpointer));
        else if (G_VALUE_HOLDS(value, G_TYPE_VALUE)) {
            GType type;
            GValue *n_value; 
            
            type = pyg_type_from_object((PyObject*)Py_TYPE(obj));
            if (G_UNLIKELY (! type)) {
                PyErr_Clear();
                return -1;
            }
            n_value = g_new0 (GValue, 1);
            g_value_init (n_value, type);
            g_value_take_boxed (value, n_value);
            return pyg_value_from_pyobject (n_value, obj);
        }
        else if (PySequence_Check(obj) &&
		   G_VALUE_HOLDS(value, G_TYPE_VALUE_ARRAY))
	    return pyg_value_array_from_pyobject(value, obj, NULL);
	else if (_PyUnicode_Check(obj) &&
                 G_VALUE_HOLDS(value, G_TYPE_GSTRING)) {
            GString *string;
            char *buffer;
            Py_ssize_t len;
            if (_PyUnicode_AsStringAndSize(obj, &buffer, &len))
                return -1;
            string = g_string_new_len(buffer, len);
	    g_value_set_boxed(value, string);
	    g_string_free (string, TRUE);
            break;
        }
	else if ((bm = pyg_type_lookup(G_VALUE_TYPE(value))) != NULL)
	    return bm->tovalue(value, obj);
	else if (PyCObject_Check(obj))
	    g_value_set_boxed(value, PyCObject_AsVoidPtr(obj));
	else
	    return -1;
	break;
    }
    case G_TYPE_PARAM:
	if (PyGParamSpec_Check(obj))
	    g_value_set_param(value, PyCObject_AsVoidPtr(obj));
	else
	    return -1;
	break;
    case G_TYPE_OBJECT:
	if (obj == Py_None) {
	    g_value_set_object(value, NULL);
	} else if (PyObject_TypeCheck(obj, &PyGObject_Type) &&
		   G_TYPE_CHECK_INSTANCE_TYPE(pygobject_get(obj),
					      G_VALUE_TYPE(value))) {
	    g_value_set_object(value, pygobject_get(obj));
	} else
	    return -1;
	break;
    default:
	{
	    PyGTypeMarshal *bm;
	    if ((bm = pyg_type_lookup(G_VALUE_TYPE(value))) != NULL)
		return bm->tovalue(value, obj);
	    break;
	}
    }
    return 0;
}

/**
 * pyg_value_as_pyobject:
 * @value: the GValue object.
 * @copy_boxed: true if boxed values should be copied.
 *
 * This function creates/returns a Python wrapper object that
 * represents the GValue passed as an argument.
 *
 * Returns: a PyObject representing the value.
 */
PyObject *
pyg_value_as_pyobject(const GValue *value, gboolean copy_boxed)
{
    gchar buf[128];

    switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value))) {
    case G_TYPE_INTERFACE:
	if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT))
	    return pygobject_new(g_value_get_object(value));
	else
	    break;
    case G_TYPE_CHAR: {
	gint8 val = g_value_get_char(value);
	return _PyUnicode_FromStringAndSize((char *)&val, 1);
    }
    case G_TYPE_UCHAR: {
	guint8 val = g_value_get_uchar(value);
	return _PyUnicode_FromStringAndSize((char *)&val, 1);
    }
    case G_TYPE_BOOLEAN: {
	return PyBool_FromLong(g_value_get_boolean(value));
    }
    case G_TYPE_INT:
	return _PyLong_FromLong(g_value_get_int(value));
    case G_TYPE_UINT:
	{
	    /* in Python, the Int object is backed by a long.  If a
	       long can hold the whole value of an unsigned int, use
	       an Int.  Otherwise, use a Long object to avoid overflow.
	       This matches the ULongArg behavior in codegen/argtypes.h */
#if (G_MAXUINT <= G_MAXLONG)
	    return _PyLong_FromLong((glong) g_value_get_uint(value));
#else
	    return PyLong_FromUnsignedLong((gulong) g_value_get_uint(value));
#endif
	}
    case G_TYPE_LONG:
	return _PyLong_FromLong(g_value_get_long(value));
    case G_TYPE_ULONG:
	{
	    gulong val = g_value_get_ulong(value);

	    if (val <= G_MAXLONG)
		return _PyLong_FromLong((glong) val);
	    else
		return PyLong_FromUnsignedLong(val);
	}
    case G_TYPE_INT64:
	{
	    gint64 val = g_value_get_int64(value);

	    if (G_MINLONG <= val && val <= G_MAXLONG)
		return _PyLong_FromLong((glong) val);
	    else
		return PyLong_FromLongLong(val);
	}
    case G_TYPE_UINT64:
	{
	    guint64 val = g_value_get_uint64(value);

	    if (val <= G_MAXLONG)
		return _PyLong_FromLong((glong) val);
	    else
		return PyLong_FromUnsignedLongLong(val);
	}
    case G_TYPE_ENUM:
	return pyg_enum_from_gtype(G_VALUE_TYPE(value), g_value_get_enum(value));
    case G_TYPE_FLAGS:
	return pyg_flags_from_gtype(G_VALUE_TYPE(value), g_value_get_flags(value));
    case G_TYPE_FLOAT:
	return PyFloat_FromDouble(g_value_get_float(value));
    case G_TYPE_DOUBLE:
	return PyFloat_FromDouble(g_value_get_double(value));
    case G_TYPE_STRING:
	{
	    const gchar *str = g_value_get_string(value);

	    if (str)
		return _PyUnicode_FromString(str);
	    Py_INCREF(Py_None);
	    return Py_None;
	}
    case G_TYPE_POINTER:
	return pyg_pointer_new(G_VALUE_TYPE(value),
			       g_value_get_pointer(value));
    case G_TYPE_BOXED: {
	PyGTypeMarshal *bm;

	if (G_VALUE_HOLDS(value, PY_TYPE_OBJECT)) {
	    PyObject *ret = (PyObject *)g_value_dup_boxed(value);
	    if (ret == NULL) {
		Py_INCREF(Py_None);
		return Py_None;
	    }
	    return ret;
        } else if (G_VALUE_HOLDS(value, G_TYPE_VALUE)) {
            GValue *n_value = g_value_get_boxed (value);
            return pyg_value_as_pyobject(n_value, copy_boxed);
        } else if (G_VALUE_HOLDS(value, G_TYPE_VALUE_ARRAY)) {
	    GValueArray *array = (GValueArray *) g_value_get_boxed(value);
	    PyObject *ret = PyList_New(array->n_values);
	    int i;
	    for (i = 0; i < array->n_values; ++i)
		PyList_SET_ITEM(ret, i, pyg_value_as_pyobject
                                (array->values + i, copy_boxed));
	    return ret;
	} else if (G_VALUE_HOLDS(value, G_TYPE_GSTRING)) {
	    GString *string = (GString *) g_value_get_boxed(value);
	    PyObject *ret = _PyUnicode_FromStringAndSize(string->str, string->len);
	    return ret;
	}	    
	bm = pyg_type_lookup(G_VALUE_TYPE(value));
	if (bm) {
	    return bm->fromvalue(value);
	} else {
	    if (copy_boxed)
		return pyg_boxed_new(G_VALUE_TYPE(value),
				     g_value_get_boxed(value), TRUE, TRUE);
	    else
		return pyg_boxed_new(G_VALUE_TYPE(value),
				     g_value_get_boxed(value),FALSE,FALSE);
	}
    }
    case G_TYPE_PARAM:
	return pyg_param_spec_new(g_value_get_param(value));
    case G_TYPE_OBJECT:
	return pygobject_new(g_value_get_object(value));
    default:
	{
	    PyGTypeMarshal *bm;
	    if ((bm = pyg_type_lookup(G_VALUE_TYPE(value))))
		return bm->fromvalue(value);
	    break;
	}
    }
    g_snprintf(buf, sizeof(buf), "unknown type %s",
	       g_type_name(G_VALUE_TYPE(value)));
    PyErr_SetString(PyExc_TypeError, buf);
    return NULL;
}

/* -------------- PyGClosure ----------------- */

static void
pyg_closure_invalidate(gpointer data, GClosure *closure)
{
    PyGClosure *pc = (PyGClosure *)closure;
    PyGILState_STATE state;

    state = pyglib_gil_state_ensure();
    Py_XDECREF(pc->callback);
    Py_XDECREF(pc->extra_args);
    Py_XDECREF(pc->swap_data);
    pyglib_gil_state_release(state);

    pc->callback = NULL;
    pc->extra_args = NULL;
    pc->swap_data = NULL;
}

static void
pyg_closure_marshal(GClosure *closure,
		    GValue *return_value,
		    guint n_param_values,
		    const GValue *param_values,
		    gpointer invocation_hint,
		    gpointer marshal_data)
{
    PyGILState_STATE state;
    PyGClosure *pc = (PyGClosure *)closure;
    PyObject *params, *ret;
    guint i;

    state = pyglib_gil_state_ensure();

    /* construct Python tuple for the parameter values */
    params = PyTuple_New(n_param_values);
    for (i = 0; i < n_param_values; i++) {
	/* swap in a different initial data for connect_object() */
	if (i == 0 && G_CCLOSURE_SWAP_DATA(closure)) {
	    g_return_if_fail(pc->swap_data != NULL);
	    Py_INCREF(pc->swap_data);
	    PyTuple_SetItem(params, 0, pc->swap_data);
	} else {
	    PyObject *item = pyg_value_as_pyobject(&param_values[i], FALSE);

	    /* error condition */
	    if (!item) {
		goto out;
	    }
	    PyTuple_SetItem(params, i, item);
	}
    }
    /* params passed to function may have extra arguments */
    if (pc->extra_args) {
	PyObject *tuple = params;
	params = PySequence_Concat(tuple, pc->extra_args);
	Py_DECREF(tuple);
    }
    ret = PyObject_CallObject(pc->callback, params);
    if (ret == NULL) {
	if (pc->exception_handler)
	    pc->exception_handler(return_value, n_param_values, param_values);
	else
	    PyErr_Print();
	goto out;
    }
    
    if (return_value && pyg_value_from_pyobject(return_value, ret) != 0) {
	PyErr_SetString(PyExc_TypeError,
			"can't convert return value to desired type");
	
	if (pc->exception_handler)
	    pc->exception_handler(return_value, n_param_values, param_values);
	else
	    PyErr_Print();
    }
    Py_DECREF(ret);
    
 out:
    Py_DECREF(params);
    pyglib_gil_state_release(state);
}

/**
 * pyg_closure_new:
 * callback: a Python callable object
 * extra_args: a tuple of extra arguments, or None/NULL.
 * swap_data: an alternative python object to pass first.
 *
 * Creates a GClosure wrapping a Python callable and optionally a set
 * of additional function arguments.  This is needed to attach python
 * handlers to signals, for instance.
 *
 * Returns: the new closure.
 */
GClosure *
pyg_closure_new(PyObject *callback, PyObject *extra_args, PyObject *swap_data)
{
    GClosure *closure;

    g_return_val_if_fail(callback != NULL, NULL);
    closure = g_closure_new_simple(sizeof(PyGClosure), NULL);
    g_closure_add_invalidate_notifier(closure, NULL, pyg_closure_invalidate);
    g_closure_set_marshal(closure, pyg_closure_marshal);
    Py_INCREF(callback);
    ((PyGClosure *)closure)->callback = callback;
    if (extra_args && extra_args != Py_None) {
	Py_INCREF(extra_args);
	if (!PyTuple_Check(extra_args)) {
	    PyObject *tmp = PyTuple_New(1);
	    PyTuple_SetItem(tmp, 0, extra_args);
	    extra_args = tmp;
	}
	((PyGClosure *)closure)->extra_args = extra_args;
    }
    if (swap_data) {
	Py_INCREF(swap_data);
	((PyGClosure *)closure)->swap_data = swap_data;
	closure->derivative_flag = TRUE;
    }
    return closure;
}

/**
 * pyg_closure_set_exception_handler:
 * @closure: a closure created with pyg_closure_new()
 * @handler: the handler to call when an exception occurs or NULL for none
 *
 * Sets the handler to call when an exception occurs during closure invocation.
 * The handler is responsible for providing a proper return value to the 
 * closure invocation. If @handler is %NULL, the default handler will be used.
 * The default handler prints the exception to stderr and doesn't touch the
 * closure's return value.
 */
void
pyg_closure_set_exception_handler(GClosure *closure,
				  PyClosureExceptionHandler handler)
{
    PyGClosure *pygclosure;
    
    g_return_if_fail(closure != NULL);

    pygclosure = (PyGClosure *)closure;
    pygclosure->exception_handler = handler;
}
/* -------------- PySignalClassClosure ----------------- */
/* a closure used for the `class closure' of a signal.  As this gets
 * all the info from the first argument to the closure and the
 * invocation hint, we can have a single closure that handles all
 * class closure cases.  We call a method by the name of the signal
 * with "do_" prepended.
 *
 *  We also remove the first argument from the * param list, as it is
 *  the instance object, which is passed * implicitly to the method
 *  object. */

static void
pyg_signal_class_closure_marshal(GClosure *closure,
				 GValue *return_value,
				 guint n_param_values,
				 const GValue *param_values,
				 gpointer invocation_hint,
				 gpointer marshal_data)
{
    PyGILState_STATE state;
    GObject *object;
    PyObject *object_wrapper;
    GSignalInvocationHint *hint = (GSignalInvocationHint *)invocation_hint;
    gchar *method_name, *tmp;
    PyObject *method;
    PyObject *params, *ret;
    guint i, len;

    state = pyglib_gil_state_ensure();
    
    g_return_if_fail(invocation_hint != NULL);
    /* get the object passed as the first argument to the closure */
    object = g_value_get_object(&param_values[0]);
    g_return_if_fail(object != NULL && G_IS_OBJECT(object));

    /* get the wrapper for this object */
    object_wrapper = pygobject_new(object);
    g_return_if_fail(object_wrapper != NULL);

    /* construct method name for this class closure */
    method_name = g_strconcat("do_", g_signal_name(hint->signal_id), NULL);

    /* convert dashes to underscores.  For some reason, g_signal_name
     * seems to convert all the underscores in the signal name to
       dashes??? */
    for (tmp = method_name; *tmp != '\0'; tmp++)
	if (*tmp == '-') *tmp = '_';

    method = PyObject_GetAttrString(object_wrapper, method_name);
    g_free(method_name);

    if (!method) {
	PyErr_Clear();
	Py_DECREF(object_wrapper);
	pyglib_gil_state_release(state);
	return;
    }
    Py_DECREF(object_wrapper);

    /* construct Python tuple for the parameter values; don't copy boxed values
       initially because we'll check after the call to see if a copy is needed. */
    params = PyTuple_New(n_param_values - 1);
    for (i = 1; i < n_param_values; i++) {
	PyObject *item = pyg_value_as_pyobject(&param_values[i], FALSE);

	/* error condition */
	if (!item) {
	    Py_DECREF(params);
	    pyglib_gil_state_release(state);
	    return;
	}
	PyTuple_SetItem(params, i - 1, item);
    }

    ret = PyObject_CallObject(method, params);
    
    /* Copy boxed values if others ref them, this needs to be done regardless of
       exception status. */
    len = PyTuple_Size(params);    
    for (i = 0; i < len; i++) {
	PyObject *item = PyTuple_GetItem(params, i);
	if (item != NULL && PyObject_TypeCheck(item, &PyGBoxed_Type)
	    && item->ob_refcnt != 1) {
	    PyGBoxed* boxed_item = (PyGBoxed*)item;
	    if (!boxed_item->free_on_dealloc) {
		boxed_item->boxed = g_boxed_copy(boxed_item->gtype, boxed_item->boxed);
		boxed_item->free_on_dealloc = TRUE;
	    }
	}
    }

    if (ret == NULL) {
	PyErr_Print();
	Py_DECREF(method);
	Py_DECREF(params);
	pyglib_gil_state_release(state);
	return;
    }
    Py_DECREF(method);
    Py_DECREF(params);
    if (return_value)
	pyg_value_from_pyobject(return_value, ret);
    Py_DECREF(ret);
    pyglib_gil_state_release(state);
}

/**
 * pyg_signal_class_closure_get:
 *
 * Returns the GClosure used for the class closure of signals.  When
 * called, it will invoke the method do_signalname (for the signal
 * "signalname").
 *
 * Returns: the closure.
 */
GClosure *
pyg_signal_class_closure_get(void)
{
    static GClosure *closure;

    if (closure == NULL) {
	closure = g_closure_new_simple(sizeof(GClosure), NULL);
	g_closure_set_marshal(closure, pyg_signal_class_closure_marshal);

	g_closure_ref(closure);
	g_closure_sink(closure);
    }
    return closure;
}

GClosure *
gclosure_from_pyfunc(PyGObject *object, PyObject *func)
{
    GSList *l;
    PyGObjectData *inst_data;
    inst_data = pyg_object_peek_inst_data(object->obj);
    if (inst_data)
    {
        for (l = inst_data->closures; l; l = l->next) {
            PyGClosure *pyclosure = l->data;
            int res;
            PyObject_Cmp(pyclosure->callback, func, &res);
            if (!res) {
                return (GClosure*)pyclosure;
            }
        }
    }
    return NULL;
}

/* ----- __doc__ descriptor for GObject and GInterface ----- */

static void
object_doc_dealloc(PyObject *self)
{
    PyObject_FREE(self);
}

/* append information about signals of a particular gtype */
static void
add_signal_docs(GType gtype, GString *string)
{
    GTypeClass *class = NULL;
    guint *signal_ids, n_ids = 0, i;

    if (G_TYPE_IS_CLASSED(gtype))
	class = g_type_class_ref(gtype);
    signal_ids = g_signal_list_ids(gtype, &n_ids);

    if (n_ids > 0) {
	g_string_append_printf(string, "Signals from %s:\n",
			       g_type_name(gtype));

	for (i = 0; i < n_ids; i++) {
	    GSignalQuery query;
	    guint j;

	    g_signal_query(signal_ids[i], &query);

	    g_string_append(string, "  ");
	    g_string_append(string, query.signal_name);
	    g_string_append(string, " (");
	    for (j = 0; j < query.n_params; j++) {
		g_string_append(string, g_type_name(query.param_types[j]));
		if (j != query.n_params - 1)
		    g_string_append(string, ", ");
	    }
	    g_string_append(string, ")");
	    if (query.return_type && query.return_type != G_TYPE_NONE) {
		g_string_append(string, " -> ");
		g_string_append(string, g_type_name(query.return_type));
	    }
	    g_string_append(string, "\n");
	}
	g_free(signal_ids);
	g_string_append(string, "\n");
    }
    if (class)
	g_type_class_unref(class);
}

static void
add_property_docs(GType gtype, GString *string)
{
    GObjectClass *class;
    GParamSpec **props;
    guint n_props = 0, i;
    gboolean has_prop = FALSE;
    G_CONST_RETURN gchar *blurb=NULL;

    class = g_type_class_ref(gtype);
    props = g_object_class_list_properties(class, &n_props);

    for (i = 0; i < n_props; i++) {
	if (props[i]->owner_type != gtype)
	    continue; /* these are from a parent type */

	/* print out the heading first */
	if (!has_prop) {
	    g_string_append_printf(string, "Properties from %s:\n",
				   g_type_name(gtype));
	    has_prop = TRUE;
	}
	g_string_append_printf(string, "  %s -> %s: %s\n",
			       g_param_spec_get_name(props[i]),
			       g_type_name(props[i]->value_type),
			       g_param_spec_get_nick(props[i]));

	/* g_string_append_printf crashes on win32 if the third 
	   argument is NULL. */
	blurb=g_param_spec_get_blurb(props[i]);
	if (blurb)
	    g_string_append_printf(string, "    %s\n",blurb);
    }
    g_free(props);
    if (has_prop)
	g_string_append(string, "\n");
    g_type_class_unref(class);
}

static PyObject *
object_doc_descr_get(PyObject *self, PyObject *obj, PyObject *type)
{
    GType gtype = 0;
    GString *string;
    PyObject *pystring;

    if (obj && pygobject_check(obj, &PyGObject_Type)) {
	gtype = G_OBJECT_TYPE(pygobject_get(obj));
	if (!gtype)
	    PyErr_SetString(PyExc_RuntimeError, "could not get object type");
    } else {
	gtype = pyg_type_from_object(type);
    }
    if (!gtype)
	return NULL;

    string = g_string_new_len(NULL, 512);

    if (g_type_is_a(gtype, G_TYPE_INTERFACE))
	g_string_append_printf(string, "Interface %s\n\n", g_type_name(gtype));
    else if (g_type_is_a(gtype, G_TYPE_OBJECT))
	g_string_append_printf(string, "Object %s\n\n", g_type_name(gtype));
    else
	g_string_append_printf(string, "%s\n\n", g_type_name(gtype));

    if (((PyTypeObject *) type)->tp_doc)
        g_string_append_printf(string, "%s\n\n", ((PyTypeObject *) type)->tp_doc);

    if (g_type_is_a(gtype, G_TYPE_OBJECT)) {
	GType parent = G_TYPE_OBJECT;
        GArray *parents = g_array_new(FALSE, FALSE, sizeof(GType));
        int iparent;

        while (parent) {
            g_array_append_val(parents, parent);
            parent = g_type_next_base(gtype, parent);
        }
        
        for (iparent = parents->len - 1; iparent >= 0; --iparent) {
	    GType *interfaces;
	    guint n_interfaces, i;

            parent = g_array_index(parents, GType, iparent);
	    add_signal_docs(parent, string);
	    add_property_docs(parent, string);

	    /* add docs for implemented interfaces */
	    interfaces = g_type_interfaces(parent, &n_interfaces);
	    for (i = 0; i < n_interfaces; i++)
		add_signal_docs(interfaces[i], string);
	    g_free(interfaces);
	}
        g_array_free(parents, TRUE);
    }

    pystring = _PyUnicode_FromStringAndSize(string->str, string->len);
    g_string_free(string, TRUE);
    return pystring;
}

PYGLIB_DEFINE_TYPE("gobject.GObject.__doc__", PyGObjectDoc_Type, PyObject);

/**
 * pyg_object_descr_doc_get:
 *
 * Returns an object intended to be the __doc__ attribute of GObject
 * wrappers.  When read in the context of the object it will return
 * some documentation about the signals and properties of the object.
 *
 * Returns: the descriptor.
 */
PyObject *
pyg_object_descr_doc_get(void)
{
    static PyObject *doc_descr = NULL;

    if (!doc_descr) {
	Py_TYPE(&PyGObjectDoc_Type) = &PyType_Type;
	if (PyType_Ready(&PyGObjectDoc_Type))
	    return NULL;

	doc_descr = PyObject_NEW(PyObject, &PyGObjectDoc_Type);
	if (doc_descr == NULL)
	    return NULL;
    }
    return doc_descr;
}


/**
 * pyg_pyobj_to_unichar_conv:
 *
 * Converts PyObject value to a unichar and write result to memory
 * pointed to by ptr.  Follows the calling convention of a ParseArgs
 * converter (O& format specifier) so it may be used to convert function
 * arguments.
 *
 * Returns: 1 if the conversion succeeds and 0 otherwise.  If the conversion
 *          did not succeesd, a Python exception is raised
 */
int pyg_pyobj_to_unichar_conv(PyObject* py_obj, void* ptr)
{
    gunichar* u = ptr;
    const Py_UNICODE* uni_buffer;
    PyObject* tmp_uni = NULL;
    
    if (PyUnicode_Check(py_obj)) {
	tmp_uni = py_obj;
	Py_INCREF(tmp_uni);
    }
    else {
	tmp_uni = PyUnicode_FromObject(py_obj);
	if (tmp_uni == NULL)
	    goto failure;
    }
    
    if ( PyUnicode_GetSize(tmp_uni) != 1) {
	PyErr_SetString(PyExc_ValueError, "unicode character value must be 1 character uniode string");
	goto failure;
    }
    uni_buffer = PyUnicode_AsUnicode(tmp_uni);
    if ( uni_buffer == NULL)
	goto failure;
    *u = uni_buffer[0];
    
    Py_DECREF(tmp_uni);
    return 1;
    
  failure:
    Py_XDECREF(tmp_uni);
    return 0;
}


int 
pyg_param_gvalue_from_pyobject(GValue* value, 
                               PyObject* py_obj, 
			       const GParamSpec* pspec)
{
    if (G_IS_PARAM_SPEC_UNICHAR(pspec)) {
	gunichar u;
	
	if (!pyg_pyobj_to_unichar_conv(py_obj, &u)) {
	    PyErr_Clear();
	    return -1;
	}
        g_value_set_uint(value, u);
	return 0;
    }
    else if (G_IS_PARAM_SPEC_VALUE_ARRAY(pspec))
	return pyg_value_array_from_pyobject(value, py_obj,
					     G_PARAM_SPEC_VALUE_ARRAY(pspec));
    else {
	return pyg_value_from_pyobject(value, py_obj);
    }
}

PyObject* 
pyg_param_gvalue_as_pyobject(const GValue* gvalue,
                             gboolean copy_boxed, 
			     const GParamSpec* pspec)
{
    if (G_IS_PARAM_SPEC_UNICHAR(pspec)) {
	gunichar u;
	Py_UNICODE uni_buffer[2] = { 0, 0 };
	
	u = g_value_get_uint(gvalue);
	uni_buffer[0] = u;
	return PyUnicode_FromUnicode(uni_buffer, 1);
    }
    else {
	return pyg_value_as_pyobject(gvalue, copy_boxed);
    }
}

/**
 * pyg_type_registration_callback
 * @gtypename: type name
 * @callback: function to run
 *
 */
typedef struct {
    PyGTypeRegistrationFunction callback;
    gpointer data;
} CustomTypeData;

void 
pyg_type_register_custom_callback(const gchar *typename,
				  PyGTypeRegistrationFunction callback,
				  gpointer user_data)
{
    CustomTypeData *data;

    if (!custom_type_registration)
	custom_type_registration = g_hash_table_new_full (g_str_hash, g_str_equal,
							  g_free, g_free);

    data = g_new (CustomTypeData, 1);
    data->callback = callback;
    data->data = user_data;

    g_hash_table_insert(custom_type_registration,
			g_strdup(typename),
			data);
}

PyTypeObject *
pyg_type_get_custom(const gchar *name)
{
    CustomTypeData *data;
    PyTypeObject *retval;
    
    if (!custom_type_registration)
	return NULL;

    data = g_hash_table_lookup(custom_type_registration, name);
    if (!data)
	return NULL;

    retval = data->callback(name, data->data);

    g_hash_table_remove(custom_type_registration, name);

    return retval;
}

GType
_pyg_type_from_name(const gchar *name)
{
    GType type;

    type = g_type_from_name(name);
    if (type == G_TYPE_INVALID) {
	pyg_type_get_custom(name);
	type = g_type_from_name(name);
    }
    
    return type;
}

static PyObject *
_pyg_strv_from_gvalue(const GValue *value)
{
    gchar    **argv = (gchar **) g_value_get_boxed(value);
    int        argc = 0, i;
    PyObject  *py_argv;

    if (argv) {
        while (argv[argc])
            argc++;
    }
    py_argv = PyList_New(argc);
    for (i = 0; i < argc; ++i)
	PyList_SET_ITEM(py_argv, i, _PyUnicode_FromString(argv[i]));
    return py_argv;
}

static int
_pyg_strv_to_gvalue(GValue *value, PyObject *obj)
{
    Py_ssize_t argc, i;
    gchar **argv;

    if (!(PyTuple_Check(obj) || PyList_Check(obj)))
        return -1;

    argc = PySequence_Length(obj);
    for (i = 0; i < argc; ++i)
	if (!_PyUnicode_Check(PySequence_Fast_GET_ITEM(obj, i)))
	    return -1;
    argv = g_new(gchar *, argc + 1);
    for (i = 0; i < argc; ++i)
	argv[i] = g_strdup(_PyUnicode_AsString(PySequence_Fast_GET_ITEM(obj, i)));
    argv[i] = NULL;
    g_value_take_boxed(value, argv);
    return 0;
}

void
pygobject_type_register_types(PyObject *d)
{
    PyGTypeWrapper_Type.tp_dealloc = (destructor)pyg_type_wrapper_dealloc;
    PyGTypeWrapper_Type.tp_compare = (cmpfunc)pyg_type_wrapper_compare;
    PyGTypeWrapper_Type.tp_repr = (reprfunc)pyg_type_wrapper_repr;
    PyGTypeWrapper_Type.tp_hash = (hashfunc)pyg_type_wrapper_hash;
    PyGTypeWrapper_Type.tp_flags = Py_TPFLAGS_DEFAULT;
    PyGTypeWrapper_Type.tp_methods = _PyGTypeWrapper_methods;
    PyGTypeWrapper_Type.tp_getset = _PyGTypeWrapper_getsets;
    PyGTypeWrapper_Type.tp_init = (initproc)pyg_type_wrapper_init;
    PYGLIB_REGISTER_TYPE(d, PyGTypeWrapper_Type, "GType");

    /* This type lazily registered in pyg_object_descr_doc_get */
    PyGObjectDoc_Type.tp_dealloc = (destructor)object_doc_dealloc;
    PyGObjectDoc_Type.tp_flags = Py_TPFLAGS_DEFAULT;
    PyGObjectDoc_Type.tp_descr_get = (descrgetfunc)object_doc_descr_get;
    
    pyg_register_gtype_custom(G_TYPE_STRV,
			      _pyg_strv_from_gvalue,
			      _pyg_strv_to_gvalue);
}
