/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 * Copyright (C) 2004       Johan Dahlin
 *
 *   pygenum.c: GFlags wrapper
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
#include "pygflags.h"

GQuark pygflags_class_key;

PYGLIB_DEFINE_TYPE("gobject.GFlags", PyGFlags_Type, PyGFlags);

static PyObject *
pyg_flags_richcompare(PyGFlags *self, PyObject *other, int op)
{
    static char warning[256];

    if (!_PyLong_Check(other)) {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    if (PyObject_TypeCheck(other, &PyGFlags_Type) && ((PyGFlags*)other)->gtype != self->gtype) {
	g_snprintf(warning, sizeof(warning), "comparing different flags types: %s and %s",
		   g_type_name(self->gtype), g_type_name(((PyGFlags*)other)->gtype));
 	if (PyErr_Warn(PyExc_Warning, warning))
 	    return NULL;
    }

    return pyg_integer_richcompare((PyObject *)self, other, op);
}

static char *
generate_repr(GType gtype, int value)
{
    GFlagsClass *flags_class;
    char *retval = NULL, *tmp;
    int i;

    flags_class = g_type_class_ref(gtype);
    g_assert(G_IS_FLAGS_CLASS(flags_class));
  
    for (i = 0; i < flags_class->n_values; i++) {
	/* Some types (eg GstElementState in GStreamer 0.8) has flags with 0 values,
         * we're just ignore them for now otherwise they'll always show up
         */
        if (flags_class->values[i].value == 0)
            continue;

        if ((value & flags_class->values[i].value) == flags_class->values[i].value) {
	    if (retval) {
		tmp = g_strdup_printf("%s | %s", retval, flags_class->values[i].value_name);
		g_free(retval);
		retval = tmp;
	    } else {
		retval = g_strdup_printf("%s", flags_class->values[i].value_name);
	    }
	}
    }
    
    g_type_class_unref(flags_class);

    return retval;
}
	    
static PyObject *
pyg_flags_repr(PyGFlags *self)
{
    char *tmp, *retval;
    PyObject *pyretval;
  
    tmp = generate_repr(self->gtype, _PyLong_AS_LONG(self));

    if (tmp)
        retval = g_strdup_printf("<flags %s of type %s>", tmp,
                                 g_type_name(self->gtype));
    else
        retval = g_strdup_printf("<flags %ld of type %s>", _PyLong_AS_LONG(self),
                                 g_type_name(self->gtype));
    g_free(tmp);
    
    pyretval = _PyUnicode_FromString(retval);
    g_free(retval);
  
    return pyretval;
}

static PyObject *
pyg_flags_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    static char *kwlist[] = { "value", NULL };
    long value;
    PyObject *pytc, *values, *ret, *pyint;
    GType gtype;
    GFlagsClass *eclass;
    
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

    eclass = G_FLAGS_CLASS(g_type_class_ref(gtype));

    values = PyObject_GetAttrString((PyObject *)type, "__flags_values__");
    if (!values) {
	g_type_class_unref(eclass);
	return NULL;
    }
    
    if (!PyDict_Check(values) || PyDict_Size(values) != eclass->n_values) {
	PyErr_SetString(PyExc_TypeError, "__flags_values__ badly formed");
	Py_DECREF(values);
	g_type_class_unref(eclass);
	return NULL;
    }

    g_type_class_unref(eclass);
    
    pyint = _PyLong_FromLong(value);
    ret = PyDict_GetItem(values, pyint);
    Py_DECREF(pyint);
    Py_DECREF(values);

    if (ret)
        Py_INCREF(ret);
    else
        PyErr_Format(PyExc_ValueError, "invalid flag value: %ld", value);
    return ret;
}

PyObject*
pyg_flags_from_gtype (GType gtype, int value)
{
    PyObject *pyclass, *values, *retval, *pyint;

    g_return_val_if_fail(gtype != G_TYPE_INVALID, NULL);
    
    pyclass = (PyObject*)g_type_get_qdata(gtype, pygflags_class_key);
    if (pyclass == NULL) {
	pyclass = pyg_flags_add(NULL, g_type_name(gtype), NULL, gtype);
	if (!pyclass)
	    return _PyLong_FromLong(value);
    }

    
    values = PyDict_GetItemString(((PyTypeObject *)pyclass)->tp_dict,
				  "__flags_values__");
    pyint = _PyLong_FromLong(value);
    retval = PyDict_GetItem(values, pyint);
    Py_DECREF(pyint);

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
    } else {
	Py_INCREF(retval);
    }
    return retval;
}

PyObject *
pyg_flags_add (PyObject *   module,
	       const char * typename,
	       const char * strip_prefix,
	       GType        gtype)
{
    PyGILState_STATE state;
    PyObject *instance_dict, *stub, *values, *o;
    GFlagsClass *eclass;
    int i;

    g_return_val_if_fail(typename != NULL, NULL);
    if (!g_type_is_a(gtype, G_TYPE_FLAGS)) {
        g_warning("Trying to register gtype '%s' as flags when in fact it is of type '%s'",
                  g_type_name(gtype), g_type_name(G_TYPE_FUNDAMENTAL(gtype)));
        return NULL;
    }
    
    state = pyglib_gil_state_ensure();

    instance_dict = PyDict_New();
    stub = PyObject_CallFunction((PyObject *)&PyType_Type, "s(O)O",
                                 typename, (PyObject *)&PyGFlags_Type,
                                 instance_dict);
    Py_DECREF(instance_dict);
    if (!stub) {
	PyErr_SetString(PyExc_RuntimeError, "can't create const");
	pyglib_gil_state_release(state);
        return NULL;
    }
    
    ((PyTypeObject *)stub)->tp_flags &= ~Py_TPFLAGS_BASETYPE;
    ((PyTypeObject *)stub)->tp_new = pyg_flags_new;

    if (module) {
        PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict,
                             "__module__",
                             _PyUnicode_FromString(PyModule_GetName(module)));
    
          /* Add it to the module name space */
        PyModule_AddObject(module, (char*)typename, stub);
        Py_INCREF(stub);
    }
    g_type_set_qdata(gtype, pygflags_class_key, stub);

    o = pyg_type_wrapper_new(gtype);
    PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict, "__gtype__", o);
    Py_DECREF(o);
    
    /* Register flag values */
    eclass = G_FLAGS_CLASS(g_type_class_ref(gtype));

    values = PyDict_New();
    for (i = 0; i < eclass->n_values; i++) {
      PyObject *item, *intval;
      
      item = ((PyTypeObject *)stub)->tp_alloc((PyTypeObject *)stub, 0);
#if PY_VERSION_HEX >= 0x03000000
#   warning "FIXME: figure out how to subclass long"    
#else
      ((_PyLongObject*)item)->ob_ival = eclass->values[i].value;
#endif    
      ((PyGFlags*)item)->gtype = gtype;
            
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
			 "__flags_values__", values);
    Py_DECREF(values);

    g_type_class_unref(eclass);

    pyglib_gil_state_release(state);

    return stub;
}

static PyObject *
pyg_flags_and(PyGFlags *a, PyGFlags *b)
{
	if (!PyGFlags_Check(a) || !PyGFlags_Check(b))
		return _PyLong_Type.tp_as_number->nb_and((PyObject*)a,
						       (PyObject*)b);
	
	return pyg_flags_from_gtype(a->gtype,
				    _PyLong_AS_LONG(a) & _PyLong_AS_LONG(b));
}

static PyObject *
pyg_flags_or(PyGFlags *a, PyGFlags *b)
{
	if (!PyGFlags_Check(a) || !PyGFlags_Check(b))
		return _PyLong_Type.tp_as_number->nb_or((PyObject*)a,
						      (PyObject*)b);

	return pyg_flags_from_gtype(a->gtype, _PyLong_AS_LONG(a) | _PyLong_AS_LONG(b));
}

static PyObject *
pyg_flags_xor(PyGFlags *a, PyGFlags *b)
{
	if (!PyGFlags_Check(a) || !PyGFlags_Check(b))
		return _PyLong_Type.tp_as_number->nb_xor((PyObject*)a,
						       (PyObject*)b);

	return pyg_flags_from_gtype(a->gtype,
				    _PyLong_AS_LONG(a) ^ _PyLong_AS_LONG(b));

}

static PyObject *
pyg_flags_warn (PyObject *self, PyObject *args)
{
    if (PyErr_Warn(PyExc_Warning, "unsupported arithmetic operation for flags type"))
	return NULL;

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pyg_flags_get_first_value_name(PyGFlags *self, void *closure)
{
  GFlagsClass *flags_class;
  GFlagsValue *flags_value;
  PyObject *retval;
  
  flags_class = g_type_class_ref(self->gtype);
  g_assert(G_IS_FLAGS_CLASS(flags_class));
  flags_value = g_flags_get_first_value(flags_class, _PyLong_AS_LONG(self));
  if (flags_value)
      retval = _PyUnicode_FromString(flags_value->value_name);
  else {
      retval = Py_None;
      Py_INCREF(Py_None);
  }
  g_type_class_unref(flags_class);

  return retval;
}

static PyObject *
pyg_flags_get_first_value_nick(PyGFlags *self, void *closure)
{
  GFlagsClass *flags_class;
  GFlagsValue *flags_value;
  PyObject *retval;
  
  flags_class = g_type_class_ref(self->gtype);
  g_assert(G_IS_FLAGS_CLASS(flags_class));

  flags_value = g_flags_get_first_value(flags_class, _PyLong_AS_LONG(self));
  if (flags_value)
      retval = _PyUnicode_FromString(flags_value->value_nick);
  else {
      retval = Py_None;
      Py_INCREF(Py_None);
  }
  g_type_class_unref(flags_class);

  return retval;
}

static PyObject *
pyg_flags_get_value_names(PyGFlags *self, void *closure)
{
  GFlagsClass *flags_class;
  PyObject *retval;
  int i;
  
  flags_class = g_type_class_ref(self->gtype);
  g_assert(G_IS_FLAGS_CLASS(flags_class));
  
  retval = PyList_New(0);
  for (i = 0; i < flags_class->n_values; i++)
      if ((_PyLong_AS_LONG(self) & flags_class->values[i].value) == flags_class->values[i].value)
	  PyList_Append(retval, _PyUnicode_FromString(flags_class->values[i].value_name));

  g_type_class_unref(flags_class);

  return retval;
}

static PyObject *
pyg_flags_get_value_nicks(PyGFlags *self, void *closure)
{
  GFlagsClass *flags_class;
  PyObject *retval;
  int i;
  
  flags_class = g_type_class_ref(self->gtype);
  g_assert(G_IS_FLAGS_CLASS(flags_class));
  
  retval = PyList_New(0);
  for (i = 0; i < flags_class->n_values; i++)
      if ((_PyLong_AS_LONG(self) & flags_class->values[i].value) == flags_class->values[i].value)
	  PyList_Append(retval, _PyUnicode_FromString(flags_class->values[i].value_nick));

  g_type_class_unref(flags_class);

  return retval;
}

static PyGetSetDef pyg_flags_getsets[] = {
    { "first_value_name", (getter)pyg_flags_get_first_value_name, (setter)0 },
    { "first_value_nick", (getter)pyg_flags_get_first_value_nick, (setter)0 },
    { "value_names", (getter)pyg_flags_get_value_names, (setter)0 },
    { "value_nicks", (getter)pyg_flags_get_value_nicks, (setter)0 },
    { NULL, 0, 0 }
};

static PyNumberMethods pyg_flags_as_number = {
	(binaryfunc)pyg_flags_warn,		/* nb_add */
	(binaryfunc)pyg_flags_warn,		/* nb_subtract */
	(binaryfunc)pyg_flags_warn,		/* nb_multiply */
	(binaryfunc)pyg_flags_warn,		/* nb_divide */
	(binaryfunc)pyg_flags_warn,		/* nb_remainder */
	(binaryfunc)pyg_flags_warn,		/* nb_divmod */
	(ternaryfunc)pyg_flags_warn,		/* nb_power */
	0,					/* nb_negative */
	0,					/* nb_positive */
	0,					/* nb_absolute */
	0,					/* nb_nonzero */
	0,					/* nb_invert */
	0,					/* nb_lshift */
	0,					/* nb_rshift */
	(binaryfunc)pyg_flags_and,		/* nb_and */
	(binaryfunc)pyg_flags_xor,		/* nb_xor */
	(binaryfunc)pyg_flags_or,		/* nb_or */
	0,					/* nb_coerce */
	0,					/* nb_int */
	0,					/* nb_long */
	0,					/* nb_float */
	0,					/* nb_oct */
	0,		 			/* nb_hex */
	0,					/* nb_inplace_add */
	0,					/* nb_inplace_subtract */
	0,					/* nb_inplace_multiply */
	0,					/* nb_inplace_divide */
	0,					/* nb_inplace_remainder */
	0,					/* nb_inplace_power */
	0,					/* nb_inplace_lshift */
	0,					/* nb_inplace_rshift */
	0,					/* nb_inplace_and */
	0,					/* nb_inplace_xor */
	0,					/* nb_inplace_or */
	0,					/* nb_floor_divide */
	0,					/* nb_true_divide */
	0,					/* nb_inplace_floor_divide */
	0,					/* nb_inplace_true_divide */
};

void
pygobject_flags_register_types(PyObject *d)
{
    pygflags_class_key = g_quark_from_static_string("PyGFlags::class");

    PyGFlags_Type.tp_base = &_PyLong_Type;
    PyGFlags_Type.tp_repr = (reprfunc)pyg_flags_repr;
    PyGFlags_Type.tp_as_number = &pyg_flags_as_number;
    PyGFlags_Type.tp_str = (reprfunc)pyg_flags_repr;
    PyGFlags_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
    PyGFlags_Type.tp_richcompare = (richcmpfunc)pyg_flags_richcompare;
    PyGFlags_Type.tp_getset = pyg_flags_getsets;
    PyGFlags_Type.tp_new = pyg_flags_new;
    PYGOBJECT_REGISTER_GTYPE(d, PyGFlags_Type, "GFlags", G_TYPE_FLAGS);
}
