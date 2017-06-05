/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 * Copyright (C) 2004       Johan Dahlin
 *
 *   pygenum.c: GEnum wrapper
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

GQuark pygenum_class_key;

PYGLIB_DEFINE_TYPE("gobject.GEnum", PyGEnum_Type, PyGEnum);

static PyObject *
pyg_enum_richcompare(PyGEnum *self, PyObject *other, int op)
{
    static char warning[256];

    if (!_PyLong_Check(other)) {
	Py_INCREF(Py_NotImplemented);
	return Py_NotImplemented;
    }

    if (PyObject_TypeCheck(other, &PyGEnum_Type) && ((PyGEnum*)other)->gtype != self->gtype) {
	g_snprintf(warning, sizeof(warning), "comparing different enum types: %s and %s",
		   g_type_name(self->gtype), g_type_name(((PyGEnum*)other)->gtype));
	if (PyErr_Warn(PyExc_Warning, warning))
	    return NULL;
    }

    return pyg_integer_richcompare((PyObject *)self, other, op);
}

static PyObject *
pyg_enum_repr(PyGEnum *self)
{
  GEnumClass *enum_class;
  const char *value;
  guint index;
  static char tmp[256];
  
  enum_class = g_type_class_ref(self->gtype);
  g_assert(G_IS_ENUM_CLASS(enum_class));

  for (index = 0; index < enum_class->n_values; index++)
      if (_PyLong_AS_LONG(self) == enum_class->values[index].value)
          break;
  value = enum_class->values[index].value_name;
  if (value)
      sprintf(tmp, "<enum %s of type %s>", value, g_type_name(self->gtype));
  else
      sprintf(tmp, "<enum %ld of type %s>", _PyLong_AS_LONG(self), g_type_name(self->gtype));

  g_type_class_unref(enum_class);

  return _PyUnicode_FromString(tmp);
}

static PyObject *
pyg_enum_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "value", NULL };
    long value;
    PyObject *pytc, *values, *ret, *intvalue;
    GType gtype;
    GEnumClass *eclass;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "l", kwlist, &value))
	return NULL;
    
    pytc = PyObject_GetAttrString((PyObject *)type, "__gtype__");
    if (!pytc)
	return NULL;
    
    if (!PyObject_TypeCheck(pytc, &PyGTypeWrapper_Type)) {
	Py_DECREF(pytc);
	PyErr_SetString(PyExc_TypeError,
			"__gtype__ attribute not a typecode");
	return NULL;
    }
    
    gtype = pyg_type_from_object(pytc);
    Py_DECREF(pytc);

    eclass = G_ENUM_CLASS(g_type_class_ref(gtype));

    /* A check that 0 < value < eclass->n_values was here but got
     * removed: enumeration values do not need to be consequitive,
     * e.g. GtkPathPriorityType values are not.
     */

    values = PyObject_GetAttrString((PyObject *)type, "__enum_values__");
    if (!values) {
	g_type_class_unref(eclass);
	return NULL;
    }

    /* Note that size of __enum_values__ dictionary can easily be less
     * than 'n_values'.  This happens if some values of the enum are
     * numerically equal, e.g. gtk.ANCHOR_N == gtk.ANCHOR_NORTH.
     * Johan said that "In retrospect, using a dictionary to store the
     * values might not have been that good", but we need to keep
     * backward compatibility.
     */
    if (!PyDict_Check(values) || PyDict_Size(values) > eclass->n_values) {
	PyErr_SetString(PyExc_TypeError, "__enum_values__ badly formed");
	Py_DECREF(values);
	g_type_class_unref(eclass);
	return NULL;
    }

    g_type_class_unref(eclass);
    
    intvalue = _PyLong_FromLong(value);
    ret = PyDict_GetItem(values, intvalue);
    Py_DECREF(intvalue);
    Py_DECREF(values);
    if (ret)
        Py_INCREF(ret);
    else
	PyErr_Format(PyExc_ValueError, "invalid enum value: %ld", value);
    
    return ret;
}

PyObject*
pyg_enum_from_gtype (GType gtype, int value)
{
    PyObject *pyclass, *values, *retval, *intvalue;

    g_return_val_if_fail(gtype != G_TYPE_INVALID, NULL);
    
    pyclass = (PyObject*)g_type_get_qdata(gtype, pygenum_class_key);
    if (pyclass == NULL) {
	pyclass = pyg_enum_add(NULL, g_type_name(gtype), NULL, gtype);
	if (!pyclass)
	    return _PyLong_FromLong(value);
    }
    
    values = PyDict_GetItemString(((PyTypeObject *)pyclass)->tp_dict,
				  "__enum_values__");
    intvalue = _PyLong_FromLong(value);
    retval = PyDict_GetItem(values, intvalue);
    Py_DECREF(intvalue);
    if (!retval) {
	PyErr_Clear();
	retval = ((PyTypeObject *)pyclass)->tp_alloc((PyTypeObject *)pyclass, 0);
	g_assert(retval != NULL);
	
#if PY_VERSION_HEX >= 0x03000000
#   warning "FIXME: figure out how to subclass long"    
#else
	((_PyLongObject*)retval)->ob_ival = value;
#endif    
	((PyGFlags*)retval)->gtype = gtype;
	//return _PyLong_FromLong(value);
    }
    
    Py_INCREF(retval);
    return retval;
}

PyObject *
pyg_enum_add (PyObject *   module,
	      const char * typename,
	      const char * strip_prefix,
	      GType        gtype)
{
    PyGILState_STATE state;
    PyObject *instance_dict, *stub, *values, *o;
    GEnumClass *eclass;
    int i;
    
    g_return_val_if_fail(typename != NULL, NULL);
    if (!g_type_is_a(gtype, G_TYPE_ENUM)) {
        g_warning("Trying to register gtype '%s' as enum when in fact it is of type '%s'",
                  g_type_name(gtype), g_type_name(G_TYPE_FUNDAMENTAL(gtype)));
        return NULL;
    }
    
    state = pyglib_gil_state_ensure();

    instance_dict = PyDict_New();
    stub = PyObject_CallFunction((PyObject *)&PyType_Type, "s(O)O",
                                 typename, (PyObject *)&PyGEnum_Type,
                                 instance_dict);
    Py_DECREF(instance_dict);
    if (!stub) {
	PyErr_SetString(PyExc_RuntimeError, "can't create const");
	pyglib_gil_state_release(state);
	return NULL;
    }

    ((PyTypeObject *)stub)->tp_flags &= ~Py_TPFLAGS_BASETYPE;
    ((PyTypeObject *)stub)->tp_new = pyg_enum_new;

    if (module)
	PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict,
			     "__module__",
			     _PyUnicode_FromString(PyModule_GetName(module)));
    
    g_type_set_qdata(gtype, pygenum_class_key, stub);

    o = pyg_type_wrapper_new(gtype);
    PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict, "__gtype__", o);
    Py_DECREF(o);

    if (module) {
	/* Add it to the module name space */
	PyModule_AddObject(module, (char*)typename, stub);
	Py_INCREF(stub);
    }

    /* Register enum values */
    eclass = G_ENUM_CLASS(g_type_class_ref(gtype));

    values = PyDict_New();
    for (i = 0; i < eclass->n_values; i++) {
	PyObject *item, *intval;
      
	item = ((PyTypeObject *)stub)->tp_alloc((PyTypeObject *)stub, 0);
#if PY_VERSION_HEX >= 0x03000000
#   warning "FIXME: figure out how to subclass long"    
#else
	((_PyLongObject*)item)->ob_ival = eclass->values[i].value;
#endif    
	((PyGEnum*)item)->gtype = gtype;
	
        intval = _PyLong_FromLong(eclass->values[i].value);
	PyDict_SetItem(values, intval, item);
        Py_DECREF(intval);
	
	if (module) {
	    char *prefix;

	    prefix = g_strdup(pyg_constant_strip_prefix(eclass->values[i].value_name, strip_prefix));
	    PyModule_AddObject(module, prefix, item);
	    g_free(prefix);

	    Py_INCREF(item);
	}
    }
    
    PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict,
			 "__enum_values__", values);
    Py_DECREF(values);

    g_type_class_unref(eclass);
    
    pyglib_gil_state_release(state);
    return stub;
}

static PyObject *
pyg_enum_reduce(PyObject *self, PyObject *args)
{
    if (!PyArg_ParseTuple(args, ":GEnum.__reduce__"))
        return NULL;

    return Py_BuildValue("(O(i)O)", Py_TYPE(self), _PyLong_AsLong(self),
                         PyObject_GetAttrString(self, "__dict__"));
}

static PyObject *
pyg_enum_get_value_name(PyGEnum *self, void *closure)
{
  GEnumClass *enum_class;
  GEnumValue *enum_value;
  PyObject *retval;
  
  enum_class = g_type_class_ref(self->gtype);
  g_assert(G_IS_ENUM_CLASS(enum_class));
  
  enum_value = g_enum_get_value(enum_class, _PyLong_AS_LONG(self));

  retval = _PyUnicode_FromString(enum_value->value_name);
  g_type_class_unref(enum_class);

  return retval;
}

static PyObject *
pyg_enum_get_value_nick(PyGEnum *self, void *closure)
{
  GEnumClass *enum_class;
  GEnumValue *enum_value;
  PyObject *retval;
  
  enum_class = g_type_class_ref(self->gtype);
  g_assert(G_IS_ENUM_CLASS(enum_class));
  
  enum_value = g_enum_get_value(enum_class, _PyLong_AS_LONG(self));

  retval = _PyUnicode_FromString(enum_value->value_nick);
  g_type_class_unref(enum_class);

  return retval;
}


static PyMethodDef pyg_enum_methods[] = {
    { "__reduce__", (PyCFunction)pyg_enum_reduce, METH_VARARGS },
    { NULL, NULL, 0 }
};

static PyGetSetDef pyg_enum_getsets[] = {
    { "value_name", (getter)pyg_enum_get_value_name, (setter)0 },
    { "value_nick", (getter)pyg_enum_get_value_nick, (setter)0 },
    { NULL, 0, 0 }
};

void
pygobject_enum_register_types(PyObject *d)
{
    pygenum_class_key        = g_quark_from_static_string("PyGEnum::class");

    PyGEnum_Type.tp_base = &_PyLong_Type;
    PyGEnum_Type.tp_repr = (reprfunc)pyg_enum_repr;
    PyGEnum_Type.tp_str = (reprfunc)pyg_enum_repr;
    PyGEnum_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
    PyGEnum_Type.tp_richcompare = (richcmpfunc)pyg_enum_richcompare;
    PyGEnum_Type.tp_methods = pyg_enum_methods;
    PyGEnum_Type.tp_getset = pyg_enum_getsets;
    PyGEnum_Type.tp_new = pyg_enum_new;
    PYGOBJECT_REGISTER_GTYPE(d, PyGEnum_Type, "GEnum", G_TYPE_ENUM);

}
