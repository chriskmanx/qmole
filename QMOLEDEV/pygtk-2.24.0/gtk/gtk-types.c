/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   gtk-types.c: wrappers for some specialised GTK types.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
 * USA
 */
#define PY_SSIZE_T_CLEAN

#include <gtk/gtk.h>
#include "pygtk-private.h"
#include <structmember.h>

#if PY_VERSION_HEX < 0x02050000
typedef int Py_ssize_t;
#define PY_SSIZE_T_MAX INT_MAX
#define PY_SSIZE_T_MIN INT_MIN
typedef inquiry lenfunc;
typedef intargfunc ssizeargfunc;
typedef intobjargproc ssizeobjargproc;
#endif

#if 0
PyObject *
PyGdkWindow_New(GdkWindow *win)
{
    PyGdkWindow_Object *self;

    self = (PyGdkWindow_Object *)PyObject_NEW(PyGdkWindow_Object,
					      &PyGdkWindow_Type);
    if (self == NULL)
	return NULL;
    self->obj = win;
    g_object_ref(self->obj);
    return (PyObject *)self;
}
#endif

PyObject *
PyGdkAtom_New(GdkAtom atom)
{
    PyGdkAtom_Object *self;

    self = (PyGdkAtom_Object *)PyObject_NEW(PyGdkAtom_Object, &PyGdkAtom_Type);
    if (self == NULL)
	return NULL;
    self->atom = atom;
    self->name = NULL;
    return (PyObject *)self;
}


/* style & rc-style helper code */
#define NUM_STATES 5
staticforward PyTypeObject PyGtkStyleHelper_Type;
staticforward PyTypeObject PyGtkRcStyleHelper_Type;

PyObject *
_pygtk_style_helper_new(GtkStyle *style, int type, gpointer array)
{
    PyGtkStyleHelper_Object *self;

    self = (PyGtkStyleHelper_Object *)PyObject_NEW(PyGtkStyleHelper_Object,
						   &PyGtkStyleHelper_Type);
    if (self == NULL)
	return NULL;

    self->style = g_object_ref(style);
    self->type = type;
    self->array = array;
    return (PyObject *)self;
}

static void
pygtk_style_helper_dealloc(PyGtkStyleHelper_Object *self)
{
    g_object_unref(self->style);
    PyObject_DEL(self);
}

static Py_ssize_t
pygtk_style_helper_length(PyGtkStyleHelper_Object *self)
{
    return NUM_STATES;
}

static PyObject *
pygtk_style_helper_getitem(PyGtkStyleHelper_Object *self, Py_ssize_t pos)
{
    if (pos < 0) pos += NUM_STATES;
    if (pos < 0 || pos >= NUM_STATES) {
	PyErr_SetString(PyExc_IndexError, "index out of range");
	return NULL;
    }
    switch (self->type) {
    case STYLE_COLOUR_ARRAY:
	{
	    GdkColor *array = (GdkColor *)self->array;
	    return pyg_boxed_new(GDK_TYPE_COLOR, &array[pos], TRUE, TRUE);
	}
    case STYLE_GC_ARRAY:
	{
	    GdkGC **array = (GdkGC **)self->array;
	    return pygobject_new((GObject *)array[pos]);
	}
    case STYLE_PIXMAP_ARRAY:
	{
	    GdkPixmap **array = (GdkPixmap **)self->array;
	    if ((long)array[pos] == GDK_PARENT_RELATIVE)
		return PyLong_FromLong(GDK_PARENT_RELATIVE);
	    return pygobject_new((GObject *)array[pos]);
	}
    }
    g_assert_not_reached();
    return NULL;
}

static int
pygtk_style_helper_setitem(PyGtkStyleHelper_Object *self, Py_ssize_t pos,
			   PyObject *value)
{
    extern PyTypeObject PyGdkGC_Type;
    extern PyTypeObject PyGdkPixmap_Type;

    if (pos < 0) pos += NUM_STATES;
    if (pos < 0 || pos >= NUM_STATES) {
	PyErr_SetString(PyExc_IndexError, "index out of range");
	return -1;
    }
    switch (self->type) {
    case STYLE_COLOUR_ARRAY:
	{
	    GdkColor *array = (GdkColor *)self->array;

	    if (!pyg_boxed_check(value, GDK_TYPE_COLOR)) {
		PyErr_SetString(PyExc_TypeError, "can only assign a GdkColor");
		return -1;
	    }
	    array[pos] = *pyg_boxed_get(value, GdkColor);
	    return 0;
	}
    case STYLE_GC_ARRAY:
	{
	    GdkGC **array = (GdkGC **)self->array;

	    if (!pygobject_check(value, &PyGdkGC_Type)) {
		PyErr_SetString(PyExc_TypeError, "can only assign a GdkGC");
		return -1;
	    }
	    if (array[pos]) {
		g_object_unref(array[pos]);
	    }
	    array[pos] = GDK_GC(g_object_ref(pygobject_get(value)));
	    return 0;
	}
    case STYLE_PIXMAP_ARRAY:
	{
	    GdkPixmap **array = (GdkPixmap **)self->array;
	    GdkPixmap *cvalue = NULL;

	    if (pygobject_check(value, &PyGdkPixmap_Type))
		cvalue = GDK_PIXMAP(g_object_ref(pygobject_get(value)));
	    else if (PyLong_Check(value)) {
		if (PyLong_AsLong(value) != GDK_PARENT_RELATIVE) {
		    PyErr_SetString(PyExc_TypeError,
				    "can only assign a GdkPixmap, None or "
				    "GDK_PARENT_RELATIVE");
		    return -1;
		}
		cvalue = (GdkPixmap*)GDK_PARENT_RELATIVE;
	    } else if (value != Py_None) {
		PyErr_SetString(PyExc_TypeError,
				"can only assign a GdkPixmap, None or "
				"GDK_PARENT_RELATIVE");
		return -1;
	    }
	    
	    if (array[pos] && (long)array[pos] != GDK_PARENT_RELATIVE) {
		g_object_unref(array[pos]);
	    }
	    array[pos] = cvalue;
	    return 0;
	}
    }
    g_assert_not_reached();
    return -1;
}

static PySequenceMethods pygtk_style_helper_seqmethods = {
    (lenfunc)pygtk_style_helper_length,
    0,
    0,
    (ssizeargfunc)pygtk_style_helper_getitem,
    0,
    (ssizeobjargproc)pygtk_style_helper_setitem,
    0,
};
static PyTypeObject PyGtkStyleHelper_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "gtk.GtkStyleHelper",
    sizeof(PyGtkStyleHelper_Object),
    0,
    (destructor)pygtk_style_helper_dealloc,
    (printfunc)0,
    (getattrfunc)0,
    (setattrfunc)0,
    (cmpfunc)0,
    (reprfunc)0,
    0,
    &pygtk_style_helper_seqmethods,
    0,
    (hashfunc)0,
    (ternaryfunc)0,
    (reprfunc)0,
    (getattrofunc)0,
    (setattrofunc)0,
    0,
    Py_TPFLAGS_DEFAULT,
    NULL
};

PyObject *
_pygtk_rc_style_helper_new(GtkRcStyle *rc_style, int type, gpointer array, GtkRcFlags is_set_flag)
{
    PyGtkRcStyleHelper_Object *self;

    self = (PyGtkRcStyleHelper_Object *)PyObject_NEW(PyGtkRcStyleHelper_Object,
                                                     &PyGtkRcStyleHelper_Type);
    if (self == NULL)
	return NULL;

    self->rc_style = g_object_ref(rc_style);
    self->type = type;
    self->array = array;
    self->is_set_flag = is_set_flag;
    return (PyObject *)self;
}

static void
pygtk_rc_style_helper_dealloc(PyGtkRcStyleHelper_Object *self)
{
    g_object_unref(self->rc_style);
    PyObject_DEL(self);
}

static Py_ssize_t
pygtk_rc_style_helper_length(PyGtkRcStyleHelper_Object *self)
{
    return NUM_STATES;
}

static PyObject *
pygtk_rc_style_helper_getitem(PyGtkRcStyleHelper_Object *self, Py_ssize_t pos)
{
    if (pos < 0) pos += NUM_STATES;
    if (pos < 0 || pos >= NUM_STATES) {
	PyErr_SetString(PyExc_IndexError, "index out of range");
	return NULL;
    }
    switch (self->type) {
    case RC_STYLE_STRING_ARRAY:
	{
	    gchar **array = (gchar **)self->array;
	    if (array[pos])
		return PyString_FromString(array[pos]);
	    else {
		Py_INCREF(Py_None);
		return Py_None;
	    }
	}
    case RC_STYLE_COLOUR_ARRAY:
        if (self->rc_style->color_flags[pos] & self->is_set_flag) {
            GdkColor *array = (GdkColor *)self->array;
            return pyg_boxed_new(GDK_TYPE_COLOR, &array[pos], TRUE, TRUE);
        }
        else {
            Py_INCREF(Py_None);
            return Py_None;
        }
    }
    g_assert_not_reached();
    return NULL;
}

static int
pygtk_rc_style_helper_setitem(PyGtkRcStyleHelper_Object *self, Py_ssize_t pos,
                              PyObject *value)
{
    extern PyTypeObject PyGdkGC_Type;
    extern PyTypeObject PyGdkPixmap_Type;

    if (pos < 0) pos += NUM_STATES;
    if (pos < 0 || pos >= NUM_STATES) {
	PyErr_SetString(PyExc_IndexError, "index out of range");
	return -1;
    }
    switch (self->type) {
    case RC_STYLE_STRING_ARRAY:
	{
            gchar **array = (gchar **)self->array;
            gchar *string;
	    PyObject *as_string;

	    if (value == Py_None)
                string = NULL;
	    else if ((as_string = PyObject_Str(value)) != NULL) {
		string = g_strdup(PyString_AsString(as_string));
		Py_DECREF(as_string);
	    }
            else
                return -1;

            g_free(array[pos]);
            array[pos] = string;
	    return 0;
	}
    case RC_STYLE_COLOUR_ARRAY:
        if (value == Py_None) {
            self->rc_style->color_flags[pos] &= ~self->is_set_flag;
            return 0;
        }
        if (pyg_boxed_check(value, GDK_TYPE_COLOR)) {
	    GdkColor *array = (GdkColor *)self->array;
	    array[pos] = *pyg_boxed_get(value, GdkColor);
            self->rc_style->color_flags[pos] |= self->is_set_flag;
	    return 0;
        }
        else {
            PyErr_SetString(PyExc_TypeError, "can only assign a gtk.gdk.Color or None");
            return -1;
	}
    }
    g_assert_not_reached();
    return -1;
}

static PySequenceMethods pygtk_rc_style_helper_seqmethods = {
    (lenfunc)pygtk_rc_style_helper_length,
    0,
    0,
    (ssizeargfunc)pygtk_rc_style_helper_getitem,
    0,
    (ssizeobjargproc)pygtk_rc_style_helper_setitem,
    0,
};
static PyTypeObject PyGtkRcStyleHelper_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "gtk.GtkRcStyleHelper",
    sizeof(PyGtkRcStyleHelper_Object),
    0,
    (destructor)pygtk_rc_style_helper_dealloc,
    (printfunc)0,
    (getattrfunc)0,
    (setattrfunc)0,
    (cmpfunc)0,
    (reprfunc)0,
    0,
    &pygtk_rc_style_helper_seqmethods,
    0,
    (hashfunc)0,
    (ternaryfunc)0,
    (reprfunc)0,
    (getattrofunc)0,
    (setattrofunc)0,
    0,
    Py_TPFLAGS_DEFAULT,
    NULL
};

#if 0
static void
PyGdkWindow_Dealloc(PyGdkWindow_Object *self)
{
    if (gdk_drawable_get_type(self->obj) == GDK_WINDOW_PIXMAP)
	g_object_unref(self->obj);
    else
	g_object_unref(self->obj);
    PyObject_DEL(self);
}

static int
PyGdkWindow_Compare(PyGdkWindow_Object *self, PyGdkWindow_Object *v)
{
    if (self->obj == v->obj) return 0;
    if (self->obj > v->obj) return -1;
    return 1;
}

static long
PyGdkWindow_Hash(PyGdkWindow_Object *self)
{
    return (long)self->obj;
}

static PyObject *
PyGdkWindow_Repr(PyGdkWindow_Object *self)
{
    char buf[100];
    if (gdk_drawable_get_type(self->obj) == GDK_WINDOW_PIXMAP)
	sprintf(buf, "<GdkPixmap at %lx>", (long)PyGdkWindow_Get(self));
    else
	sprintf(buf, "<GdkWindow at %lx>", (long)PyGdkWindow_Get(self));
    return PyString_FromString(buf);
}

static PyObject *
PyGdkWindow_NewGC(PyGdkWindow_Object *self, PyObject *args, PyObject *kws)
{
    int i = 0;
    PyObject *key, *value;
    char *strkey;
    GdkGCValues values;
    GdkGCValuesMask mask = 0;
    GdkGC *gc;

    if (kws != NULL)
	while (PyDict_Next(kws, &i, &key, &value)) {
	    strkey = PyString_AsString(key);
	    if (!strcmp(strkey, "foreground")) {
		if (!PyGdkColor_Check(value)) {
		    PyErr_SetString(PyExc_TypeError,
				    "foreground argument takes a GdkColor");
		    return NULL;
		}
		mask |= GDK_GC_FOREGROUND;
		values.foreground.red = PyGdkColor_Get(value)->red;
		values.foreground.green = PyGdkColor_Get(value)->green;
		values.foreground.blue = PyGdkColor_Get(value)->blue;
		values.foreground.pixel = PyGdkColor_Get(value)->pixel;
	    } else if (!strcmp(strkey, "background")) {
		if (!PyGdkColor_Check(value)) {
		    PyErr_SetString(PyExc_TypeError,
				    "background argument takes a GdkColor");
		    return NULL;
		}
		mask |= GDK_GC_BACKGROUND;
		values.background.red = PyGdkColor_Get(value)->red;
		values.background.green = PyGdkColor_Get(value)->green;
		values.background.blue = PyGdkColor_Get(value)->blue;
		values.background.pixel = PyGdkColor_Get(value)->pixel;
	    } else if (!strcmp(strkey, "font")) {
		if (!PyGdkFont_Check(value)) {
		    PyErr_SetString(PyExc_TypeError,
				    "font argument takes a GdkFont");
		    return NULL;
		}
		mask |= GDK_GC_FONT;
		values.font = PyGdkFont_Get(value);
	    } else if (!strcmp(strkey, "tile")) {
		if (!PyGdkWindow_Check(value)) {
		    PyErr_SetString(PyExc_TypeError,
				    "tile argument takes a GdkPixmap");
		    return NULL;
		}
		mask |= GDK_GC_TILE;
		values.tile = PyGdkWindow_Get(value);
	    } else if (!strcmp(strkey, "stipple")) {
		if (!PyGdkWindow_Check(value)) {
		    PyErr_SetString(PyExc_TypeError,
				    "stipple argument takes a GdkPixmap");
		    return NULL;
		}
		mask |= GDK_GC_STIPPLE;
		values.stipple = PyGdkWindow_Get(value);
	    } else if (!strcmp(strkey, "clip_mask")) {
		if (!PyGdkWindow_Check(value)) {
		    PyErr_SetString(PyExc_TypeError,
				    "clip_mask argument takes a GdkPixmap");
		    return NULL;
		}
		mask |= GDK_GC_CLIP_MASK;
		values.clip_mask = PyGdkWindow_Get(value);
	    } else {
		int i = 0;
#ifndef offsetof
#define offsetof(type, member) ( (int) &((type*)0)->member)
#endif
#define OFF(x) offsetof(GdkGCValues, x)
		static struct {char *name;GdkGCValuesMask mask;int offs; } others[] = {
		    {"function", GDK_GC_FUNCTION, OFF(function)},
		    {"fill",     GDK_GC_FILL,     OFF(fill)},
		    {"subwindow_mode", GDK_GC_SUBWINDOW, OFF(subwindow_mode)},
		    {"ts_x_origin", GDK_GC_TS_X_ORIGIN, OFF(ts_x_origin)},
		    {"ts_y_origin", GDK_GC_TS_Y_ORIGIN, OFF(ts_y_origin)},
		    {"clip_x_origin", GDK_GC_CLIP_X_ORIGIN, OFF(clip_x_origin)},
		    {"clip_y_origin", GDK_GC_CLIP_Y_ORIGIN, OFF(clip_y_origin)},
		    {"graphics_exposures", GDK_GC_EXPOSURES, OFF(graphics_exposures)},
		    {"line_width", GDK_GC_LINE_WIDTH, OFF(line_width)},
		    {"line_style", GDK_GC_LINE_STYLE, OFF(line_style)},
		    {"cap_style", GDK_GC_CAP_STYLE, OFF(cap_style)},
		    {"join_style", GDK_GC_JOIN_STYLE, OFF(join_style)},
		    {NULL, 0, 0}
		};
#undef OFF
		while (others[i].name != NULL) {
		    if (!strcmp(strkey, others[i].name)) {
			if (!PyInt_Check(value)) {
			    char buf[80];
			    g_snprintf(buf, sizeof(buf),
				       "%s argument expects an integer",
				       others[i].name);
			    PyErr_SetString(PyExc_TypeError, buf);
			    return NULL;
			}
			mask |= others[i].mask;
			*((int *)((char *)&values + others[i].offs)) =
			    PyInt_AsLong(value);
			break;
		    }
		    i++;
		}
		if (others[i].name == NULL) {
		    PyErr_SetString(PyExc_TypeError, "unknown argument");
		    return NULL;
		}
	    }
	}
    if (!PyArg_ParseTuple(args, ":GdkWindow.new_gc"))
	return NULL;
    gc = gdk_gc_new_with_values(PyGdkWindow_Get(self), &values, mask);
    value = PyGdkGC_New(gc);
    g_object_unref(gc);
    return value;
}

static PyObject *
PyGdkWindow_SetCursor(PyGdkWindow_Object *self, PyObject *args)
{
    PyObject *cursor;

    if (!PyArg_ParseTuple(args, "O!:GdkWindow.set_cursor", &PyGdkCursor_Type,
			  &cursor))
	return NULL;
    gdk_window_set_cursor(self->obj, PyGdkCursor_Get(cursor));
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
PyGdkWindow_PropertyGet(PyGdkWindow_Object *self, PyObject *args)
{
    PyObject *py_property, py_type = NULL;
    gint pdelete = FALSE;

    GdkAtom atype, property, type;
    gint aformat, alength;
    guchar *data;

    if (!PyArg_ParseTuple(args, "O|Oi:GdkWindow.property_get", &py_property,
			  &py_type, &pdelete)) {
	return NULL;
    }

    property = pygdk_atom_from_pyobject(py_property);
    if (Pyerr_Occurred())
	return NULL;

    type = pygdk_atom_from_pyobject(py_type);
    if (Pyerr_Occurred())
	return NULL;
	
    if (gdk_property_get(self->obj, property, type, 0, G_MAXLONG,
			 pdelete, &atype, &aformat, &alength, &data)) {
	/* success */
	PyObject *pdata = NULL;
	gint i;
	guint16 *data16;
	guint32 *data32;
	switch (aformat) {
	case 8:
	    if ((pdata = PyString_FromStringAndSize(data, alength)) == NULL)
	        return NULL;
	    break;
	case 16:
	    data16 = (guint16 *)data;
	    if ((pdata = PyTuple_New(alength)) == NULL)
	        return NULL;
	    for (i = 0; i < alength; i++)
		PyTuple_SetItem(pdata, i, PyInt_FromLong(data16[i]));
	    break;
	case 32:
	    data32 = (guint32 *)data;
	    if ((pdata = PyTuple_New(alength)) == NULL)
	        return NULL;
	    for (i = 0; i < alength; i++)
		PyTuple_SetItem(pdata, i, PyInt_FromLong(data32[i]));
	    break;
	default:
	    g_warning("got a property format != 8, 16 or 32");
	    g_assert_not_reached();
	}
	g_free(data);
	return Py_BuildValue("(NiN)", PyGdkAtom_New(atype), aformat, pdata);
    } else {
	Py_INCREF(Py_None);
	return Py_None;
    }
}

static PyObject *
PyGdkWindow_PropertyChange(PyGdkWindow_Object *self, PyObject *args)
{
    PyObject *py_property, *py_type;
    GdkAtom property, type;
    gint format;
    PyObject *py_mode, *pdata;
    GdkPropMode mode;
    guchar *data = NULL;
    gint nelements;

    if (!PyArg_ParseTuple(args, "OOiOO:GdkWindow.property_change",
			  &py_property, &py_type, &format, &py_mode, &pdata)) {
	return NULL;
    }

    property = pygdk_atom_from_pyobject(py_property);
    if (Pyerr_Occurred())
	return NULL;

    type = pygdk_atom_from_pyobject(py_type);
    if (Pyerr_Occurred())
	return NULL;
    
    if (pygtk_enum_get_value(GDK_TYPE_PROP_MODE, py_mode, (gint *)&mode))
	return NULL;
    switch (format) {
    case 8:
	if (!PyString_Check(pdata)) {
	    PyErr_SetString(PyExc_TypeError, "data not a string and format=8");
	    return NULL;
	}
	data = PyString_AsString(pdata);
	nelements = PyString_Size(pdata);
	break;
    case 16:
	{
	    guint16 *data16;
	    gint i;

	    if (!PySequence_Check(pdata)) {
		PyErr_SetString(PyExc_TypeError,
				"data not a sequence and format=16");
		return NULL;
	    }
	    nelements = PySequence_Length(pdata);
	    data16 = g_new(guint16, nelements);
	    data = (guchar *)data16;
	    for (i = 0; i < nelements; i++) {
		PyObject *item = PySequence_GetItem(pdata, i);
		Py_DECREF(item);
		item = PyNumber_Int(item);
		if (!item) {
		    g_free(data16);
		    PyErr_Clear();
		    PyErr_SetString(PyExc_TypeError,"data element not an int");
		    return NULL;
		}
		data16[i] = PyInt_AsLong(item);
		Py_DECREF(item);
	    }
	}
	break;
    case 32:
	{
	    guint32 *data32;
	    gint i;

	    if (!PySequence_Check(pdata)) {
		PyErr_SetString(PyExc_TypeError,
				"data not a sequence and format=32");
		return NULL;
	    }
	    nelements = PySequence_Length(pdata);
	    data32 = g_new(guint32, nelements);
	    data = (guchar *)data32;
	    for (i = 0; i < nelements; i++) {
		PyObject *item = PySequence_GetItem(pdata, i);
		Py_DECREF(item);
		item = PyNumber_Int(item);
		if (!item) {
		    g_free(data32);
		    PyErr_Clear();
		    PyErr_SetString(PyExc_TypeError,"data element not an int");
		    return NULL;
		}
		data32[i] = PyInt_AsLong(item);
		Py_DECREF(item);
	    }
	}
	break;
    default:
	PyErr_SetString(PyExc_TypeError, "format must be 8, 16 or 32");
	return NULL;
	break;
    }
    gdk_property_change(self->obj, property, type, format, mode, data,
			nelements);
    if (format != 8)
	g_free(data);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
PyGdkWindow_PropertyDelete(PyGdkWindow_Object *self, PyObject *args)
{
    PyObject py_property;
    GdkAtom property;

    if (!PyArg_ParseTuple(args, "O:GdkWindow.property_delete", &property)) {
	return NULL;
    }

    property = pygdk_atom_from_pyobject(py_property);
    if (Pyerr_Occurred())
	return NULL;
    
    gdk_property_delete(self->obj, property);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
PyGdkWindow_Raise(PyGdkWindow_Object *self, PyObject *args)
{
    if (!PyArg_ParseTuple(args, ":GdkWindow._raise"))
	return NULL;
    gdk_window_raise(self->obj);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
PyGdkWindow_Lower(PyGdkWindow_Object *self, PyObject *args)
{
    if (!PyArg_ParseTuple(args, ":GdkWindow.lower"))
	return NULL;
    gdk_window_lower(self->obj);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
PyGdkWindow_InputGetPointer(PyGdkWindow_Object *self, PyObject *args)
{
    guint32 deviceid;
    gdouble x = 0.0, y = 0.0, pressure = 0.0, xtilt = 0.0, ytilt = 0.0;
    GdkModifierType mask = 0;

    if (!PyArg_ParseTuple(args, "i:GdkWindow.input_get_pointer", &deviceid))
	return NULL;
    gdk_input_window_get_pointer(self->obj, deviceid, &x, &y, &pressure,
				 &xtilt, &ytilt, &mask);
    return Py_BuildValue("(dddddi)", x, y, pressure, xtilt, ytilt, mask);
}

static PyMethodDef PyGdkWindow_methods[] = {
    {"new_gc", (PyCFunction)PyGdkWindow_NewGC, METH_VARARGS|METH_KEYWORDS, NULL},
    {"set_cursor", (PyCFunction)PyGdkWindow_SetCursor, METH_VARARGS, NULL},
    {"property_get", (PyCFunction)PyGdkWindow_PropertyGet, METH_VARARGS, NULL},
    {"property_change", (PyCFunction)PyGdkWindow_PropertyChange, METH_VARARGS, NULL},
    {"property_delete", (PyCFunction)PyGdkWindow_PropertyDelete, METH_VARARGS, NULL},
    {"_raise", (PyCFunction)PyGdkWindow_Raise, METH_VARARGS, NULL},
    {"lower", (PyCFunction)PyGdkWindow_Lower, METH_VARARGS, NULL},
    {"input_get_pointer", (PyCFunction)PyGdkWindow_InputGetPointer, METH_VARARGS, NULL},
    {NULL, 0, 0, NULL}
};

static PyObject *
PyGdkWindow_GetAttr(PyGdkWindow_Object *self, char *key)
{
    GdkWindow *win = PyGdkWindow_Get(self);
    gint x, y;
    GdkModifierType p_mask;

    if (!strcmp(key, "__members__"))
	return Py_BuildValue("[sssssssssssss]", "children", "colormap", "depth",
			     "height", "parent", "pointer", "pointer_state",
			     "toplevel", "type", "width", "x", "xid", "y");
    if (!strcmp(key, "width")) {
	gdk_drawable_get_size(win, &x, NULL);
	return PyInt_FromLong(x);
    }
    if (!strcmp(key, "height")) {
	gdk_drawable_get_size(win, NULL, &y);
	return PyInt_FromLong(y);
    }
    if (!strcmp(key, "x")) {
	gdk_window_get_position(win, &x, NULL);
	return PyInt_FromLong(x);
    }
    if (!strcmp(key, "y")) {
	gdk_window_get_position(win, NULL, &y);
	return PyInt_FromLong(y);
    }
    if (!strcmp(key, "colormap"))
	return PyGdkColormap_New(gdk_drawable_get_colormap(win));
    if (!strcmp(key, "pointer")) {
	gdk_window_get_pointer(win, &x, &y, NULL);
	return Py_BuildValue("(ii)", x, y);
    }
    if (!strcmp(key, "pointer_state")) {
	gdk_window_get_pointer(win, NULL, NULL, &p_mask);
	return PyInt_FromLong(p_mask);
    }
    if (!strcmp(key, "parent")) {
	GdkWindow *par = gdk_window_get_parent(win);
	if (par)
	    return PyGdkWindow_New(par);
	Py_INCREF(Py_None);
	return Py_None;
    }
    if (!strcmp(key, "toplevel"))
	return PyGdkWindow_New(gdk_window_get_toplevel(win));
    if (!strcmp(key, "children")) {
	GList *children, *tmp;
	PyObject *ret;
	children = gdk_window_get_children(win);
	if ((ret = PyList_New(0)) == NULL)
	    return NULL;
	for (tmp = children; tmp != NULL; tmp = tmp->next) {
	    PyObject *win = PyGdkWindow_New(tmp->data);
	    if (win == NULL) {
		Py_DECREF(ret);
		return NULL;
	    }
	    PyList_Append(ret, win);
	    Py_DECREF(win);
	}
	g_list_free(children);
	return ret;
    }
    if (!strcmp(key, "type"))
	return PyInt_FromLong(gdk_drawable_get_type(win));
    if (!strcmp(key, "depth")) {
	gdk_window_get_geometry(win, NULL, NULL, NULL, NULL, &x);
	return PyInt_FromLong(x);
    }
#ifdef WITH_XSTUFF
    if (!strcmp(key, "xid"))
	return PyInt_FromLong(GDK_WINDOW_XWINDOW(win));
#endif

    return Py_FindMethod(PyGdkWindow_methods, (PyObject *)self, key);
}

PyTypeObject PyGdkWindow_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "GdkWindow",
    sizeof(PyGdkWindow_Object),
    0,
    (destructor)PyGdkWindow_Dealloc,
    (printfunc)0,
    (getattrfunc)PyGdkWindow_GetAttr,
    (setattrfunc)0,
    (cmpfunc)PyGdkWindow_Compare,
    (reprfunc)PyGdkWindow_Repr,
    0,
    0,
    0,
    (hashfunc)PyGdkWindow_Hash,
    (ternaryfunc)0,
    (reprfunc)0,
    (getattrofunc)0,
    (setattrofunc)0,
    0,
    Py_TPFLAGS_DEFAULT,
    NULL
};
#endif

GdkAtom
pygdk_atom_from_pyobject(PyObject *object)
{
    if (object == NULL)
	return NULL;
    if (PyString_Check(object))
	return gdk_atom_intern(PyString_AsString(object), FALSE);
    if (PyGdkAtom_Check(object))
	return PyGdkAtom_Get(object);
    PyErr_SetString(PyExc_TypeError, "unable to convert argument to GdkAtom");
    return NULL;
}

static void
pygdk_atom_dealloc(PyGdkAtom_Object *self)
{
    if (self->name) g_free(self->name);
    PyObject_DEL(self);
}

static long
pygdk_atom_hash(PyGdkAtom_Object *self)
{
    return (long)self->atom;
}

static PyObject *
pygdk_atom_repr(PyGdkAtom_Object *self)
{
    char buf[256];
    if (!self->name) self->name = gdk_atom_name(self->atom);
    g_snprintf(buf, 256, "<GdkAtom 0x%lx = '%s'>", (unsigned long)self->atom,
	       self->name?self->name:"(null)");
    return PyString_FromString(buf);
}

static PyObject *
pygdk_atom_str(PyGdkAtom_Object *self)
{
    if (!self->name) self->name = gdk_atom_name(self->atom);
    if (self->name)
	return PyString_FromString(self->name);
    return pygdk_atom_repr(self);
}

static PyObject *
pygdk_atom_richcompare(PyGdkAtom_Object *self, PyGdkAtom_Object *v, int op)
{
    PyObject *result = Py_NotImplemented;
 
    if (PyString_Check(v)) {
        PyObject *str = pygdk_atom_str(self);
        result =  PyObject_RichCompare(str, (PyObject *)v, op);
        Py_DECREF(str);
        return result;
    }
    if (PyGdkAtom_Check(v)) {
        switch (op) {
        case Py_LT:
            result = (self->atom < v->atom) ? Py_True : Py_False;
            break;
        case Py_LE:
            result = (self->atom <= v->atom) ? Py_True : Py_False;
            break;
        case Py_EQ:
            result = (self->atom == v->atom) ? Py_True : Py_False;
            break;
        case Py_NE:
            result = (self->atom != v->atom) ? Py_True : Py_False;
            break;
        case Py_GE:
            result = (self->atom >= v->atom) ? Py_True : Py_False;
            break;
        case Py_GT:
            result = (self->atom > v->atom) ? Py_True : Py_False;
            break;
        default:
            break;
        }
    }
    Py_INCREF(result);
    return result;
}

PyTypeObject PyGdkAtom_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "gtk.gdk.GdkAtom",
    sizeof(PyGdkAtom_Object),
    0,
    (destructor)pygdk_atom_dealloc,
    (printfunc)0,
    (getattrfunc)0,
    (setattrfunc)0,
    (cmpfunc)0,
    (reprfunc)pygdk_atom_repr,
    0,
    0,
    0,
    (hashfunc)pygdk_atom_hash,
    (ternaryfunc)0,
    (reprfunc)pygdk_atom_str,
    (getattrofunc)0,
    (setattrofunc)0,
    0,
    Py_TPFLAGS_DEFAULT,
    NULL,
    0,
    0,
    (richcmpfunc)pygdk_atom_richcompare,
};

typedef struct {
    PyObject_HEAD
    GtkTreeModel *model;
    GtkTreeIter iter;
} PyGtkTreeModelRow;
staticforward PyTypeObject PyGtkTreeModelRow_Type;

PyObject *
_pygtk_tree_model_row_new(GtkTreeModel *model, GtkTreeIter *iter)
{
    PyGtkTreeModelRow *self;

    self = (PyGtkTreeModelRow *) PyObject_NEW(PyGtkTreeModelRow,
					      &PyGtkTreeModelRow_Type);
    if (self == NULL)
	return NULL;
    self->model = g_object_ref(model);
    self->iter = *iter;
    return (PyObject *)self;
}

static void
pygtk_tree_model_row_dealloc(PyGtkTreeModelRow *self)
{
    g_object_unref(self->model);
    PyObject_DEL(self);
}

static Py_ssize_t
pygtk_tree_model_row_length(PyGtkTreeModelRow *self)
{
    return gtk_tree_model_get_n_columns(self->model);
}

static PyObject *
pygtk_tree_model_row_getitem(PyGtkTreeModelRow *self, Py_ssize_t column)
{
    gint n_columns;
    GValue value = { 0, };
    PyObject *ret;

    n_columns = gtk_tree_model_get_n_columns(self->model);
    if (column < 0 || column >= n_columns) {
	PyErr_SetString(PyExc_IndexError, "column index out of range");
        return NULL;
    }
    gtk_tree_model_get_value(self->model, &self->iter, column, &value);
    ret = pyg_value_as_pyobject(&value, TRUE);
    g_value_unset(&value);
    return ret;
}

static int
pygtk_tree_model_row_setitem(PyGtkTreeModelRow *self, Py_ssize_t column,
			     PyObject *pyvalue)
{
    gint n_columns;
    GValue value = { 0, };

    if (!GTK_IS_LIST_STORE(self->model) && !GTK_IS_TREE_STORE(self->model)) {
	PyErr_SetString(PyExc_TypeError,
			"can not set cells in this tree model");
	return -1;
    }

    n_columns = gtk_tree_model_get_n_columns(self->model);
    if (column < 0 || column >= n_columns) {
	PyErr_SetString(PyExc_IndexError, "column index out of range");
        return -1;
    }
    g_value_init(&value, gtk_tree_model_get_column_type(self->model, column));
    if (pyg_value_from_pyobject(&value, pyvalue)) {
	PyErr_SetString(PyExc_TypeError,
			"value is of wrong type for this column");
	return -1;
    }
    if (GTK_IS_LIST_STORE(self->model))
	gtk_list_store_set_value(GTK_LIST_STORE(self->model), &self->iter,
				 column, &value);
    else if (GTK_IS_TREE_STORE(self->model))
	gtk_tree_store_set_value(GTK_TREE_STORE(self->model), &self->iter,
				 column, &value);
    g_value_unset(&value);
    return 0;
}

static PySequenceMethods pygtk_tree_model_row_seqmethods = {
    (lenfunc)pygtk_tree_model_row_length,
    0,
    0,
    (ssizeargfunc)pygtk_tree_model_row_getitem,
    0,
    (ssizeobjargproc)pygtk_tree_model_row_setitem,
    0
};

static PyObject *
pygtk_tree_model_row_iterchildren(PyGtkTreeModelRow *self)
{
    return _pygtk_tree_model_row_iter_new(self->model, &self->iter);
}

static PyMethodDef pygtk_tree_model_row_methods[] = {
    { "iterchildren", (PyCFunction)pygtk_tree_model_row_iterchildren, METH_NOARGS },
    { NULL, NULL, 0 }
};

static PyObject *
pygtk_tree_model_row_get_next(PyGtkTreeModelRow *self, void *closure)
{
    GtkTreeIter iter;

    iter = self->iter;
    if (gtk_tree_model_iter_next(self->model, &iter))
	return _pygtk_tree_model_row_new(self->model, &iter);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pygtk_tree_model_row_get_parent(PyGtkTreeModelRow *self, void *closure)
{
    GtkTreeIter parent;

    if (gtk_tree_model_iter_parent(self->model, &parent, &self->iter))
	return _pygtk_tree_model_row_new(self->model, &parent);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
pygtk_tree_model_row_get_model(PyGtkTreeModelRow *self, void *closure)
{
    return pygobject_new((GObject *)self->model);
}

static PyObject *
pygtk_tree_model_row_get_path(PyGtkTreeModelRow *self, void *closure)
{
    GtkTreePath *path;
    PyObject *ret;

    path = gtk_tree_model_get_path(self->model, &self->iter);
    if (!path) {
	PyErr_SetString(PyExc_RuntimeError, "could not get tree path");
	return NULL;
    }
    ret = pygtk_tree_path_to_pyobject(path);
    gtk_tree_path_free(path);
    return ret;
}

static PyObject *
pygtk_tree_model_row_get_iter(PyGtkTreeModelRow *self, void *closure)
{
    return pyg_boxed_new(GTK_TYPE_TREE_ITER, &self->iter, TRUE, TRUE);
}

static PyGetSetDef pygtk_tree_model_row_getsets[] = {
    { "next", (getter)pygtk_tree_model_row_get_next, (setter)0 },
    { "parent", (getter)pygtk_tree_model_row_get_parent, (setter)0 },
    { "model", (getter)pygtk_tree_model_row_get_model, (setter)0 },
    { "path", (getter)pygtk_tree_model_row_get_path, (setter)0 },
    { "iter", (getter)pygtk_tree_model_row_get_iter, (setter)0 },
    { NULL, (getter)0, (setter)0 }
};

static PyTypeObject PyGtkTreeModelRow_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "gtk.TreeModelRow",
    sizeof(PyGtkTreeModelRow),
    0,
    (destructor)pygtk_tree_model_row_dealloc,
    (printfunc)0,
    (getattrfunc)0,
    (setattrfunc)0,
    (cmpfunc)0,
    (reprfunc)0,
    0,
    &pygtk_tree_model_row_seqmethods,
    0,
    (hashfunc)0,
    (ternaryfunc)0,
    (reprfunc)0,
    (getattrofunc)0,
    (setattrofunc)0,
    0,
    Py_TPFLAGS_DEFAULT,
    NULL,
    (traverseproc)0,
    (inquiry)0,
    (richcmpfunc)0,
    0,
    (getiterfunc)0,
    (iternextfunc)0,
    pygtk_tree_model_row_methods,
    0,
    pygtk_tree_model_row_getsets
};

typedef struct {
    PyObject_HEAD
    GtkTreeModel *model;
    gboolean has_more;
    GtkTreeIter iter;
} PyGtkTreeModelRowIter;
staticforward PyTypeObject PyGtkTreeModelRowIter_Type;

PyObject *
_pygtk_tree_model_row_iter_new(GtkTreeModel *model, GtkTreeIter *parent_iter)
{
    PyGtkTreeModelRowIter *self;

    self = (PyGtkTreeModelRowIter *) PyObject_NEW(PyGtkTreeModelRowIter,
						  &PyGtkTreeModelRowIter_Type);
    if (self == NULL)
	return NULL;
    self->model = g_object_ref(model);
    /* iterate through child nodes */
    self->has_more = gtk_tree_model_iter_children(self->model, &self->iter,
						  parent_iter);
    return (PyObject *)self;
}

static void
pygtk_tree_model_row_iter_dealloc(PyGtkTreeModelRowIter *self)
{
    g_object_unref(self->model);
    PyObject_DEL(self);
}

static PyObject *
pygtk_tree_model_row_iter_getiter(PyGtkTreeModelRowIter *self)
{
    Py_INCREF(self);
    return (PyObject *)self;
}

static PyObject *
pygtk_tree_model_row_iter_next(PyGtkTreeModelRowIter *self)
{
    PyObject *row;

    if (!self->has_more) {
	PyErr_SetNone(PyExc_StopIteration);
	return NULL;
    }

    row = _pygtk_tree_model_row_new(self->model, &self->iter);

    /* move to next iter */
    self->has_more = gtk_tree_model_iter_next(self->model, &self->iter);

    return row;
}

static PyTypeObject PyGtkTreeModelRowIter_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "gtk.TreeModelRowIter",
    sizeof(PyGtkTreeModelRowIter),
    0,
    (destructor)pygtk_tree_model_row_iter_dealloc,
    (printfunc)0,
    (getattrfunc)0,
    (setattrfunc)0,
    (cmpfunc)0,
    (reprfunc)0,
    0,
    0,
    0,
    (hashfunc)0,
    (ternaryfunc)0,
    (reprfunc)0,
    (getattrofunc)0,
    (setattrofunc)0,
    0,
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_HAVE_ITER,
    NULL,
    (traverseproc)0,
    (inquiry)0,
    (richcmpfunc)0,
    0,
    (getiterfunc)pygtk_tree_model_row_iter_getiter,
    (iternextfunc)pygtk_tree_model_row_iter_next
};

int
_pygtk_tree_model_remove_row(GtkTreeModel *model, GtkTreeIter *iter)
{
    GtkTreeModel *child;
    GtkTreeIter citer;

    if (GTK_IS_LIST_STORE(model)) {
        gtk_list_store_remove(GTK_LIST_STORE(model), iter);
        return 0;
    }

    if (GTK_IS_TREE_STORE(model)) {
        gtk_tree_store_remove(GTK_TREE_STORE(model), iter);
        return 0;
    }

    if (GTK_IS_TREE_MODEL_SORT(model)) {
        child = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
        gtk_tree_model_sort_convert_iter_to_child_iter(
            GTK_TREE_MODEL_SORT(model), &citer, iter);
        return _pygtk_tree_model_remove_row(child, &citer);
    }

    if (GTK_IS_TREE_MODEL_FILTER(model)) {
        child = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
        gtk_tree_model_filter_convert_iter_to_child_iter(
            GTK_TREE_MODEL_FILTER(model), &citer, iter);
        return _pygtk_tree_model_remove_row(child, &citer);
    }

    PyErr_SetString(PyExc_TypeError,
                    "cannot remove rows in this tree model");
    return -1;
}

int
_pygtk_tree_model_set_row(GtkTreeModel *model, GtkTreeIter *iter,
			  PyObject *items)
{
    gint n_columns, i;
    GtkTreeModel *child;
    GtkTreeIter citer;

    if (!GTK_IS_LIST_STORE(model) && !GTK_IS_TREE_STORE(model) &&
        !GTK_IS_TREE_MODEL_SORT(model) && !GTK_IS_TREE_MODEL_FILTER(model)) {
	PyErr_SetString(PyExc_TypeError,
			"cannot set cells in this tree model");
	return -1;
    }

    if (GTK_IS_TREE_MODEL_SORT(model)) {
        child = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
        gtk_tree_model_sort_convert_iter_to_child_iter(
            GTK_TREE_MODEL_SORT(model), &citer, iter);
        return _pygtk_tree_model_set_row(child, &citer, items);
    }

    if (GTK_IS_TREE_MODEL_FILTER(model)) {
        child = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
        gtk_tree_model_filter_convert_iter_to_child_iter(
            GTK_TREE_MODEL_FILTER(model), &citer, iter);
        return _pygtk_tree_model_set_row(child, &citer, items);
    }

    if (!PySequence_Check(items)) {
	PyErr_SetString(PyExc_TypeError, "expecting a sequence");
	return -1;
    }
    n_columns = gtk_tree_model_get_n_columns(model);
    if (PySequence_Length(items) != n_columns) {
	PyErr_SetString(PyExc_ValueError, "row sequence has wrong length");
	return -1;
    }
    for (i = 0; i < n_columns; i++) {
	GValue value = { 0, };
	PyObject *item;

	item = PySequence_GetItem(items, i);
	if (!item)
	    return -1;
	g_value_init(&value, gtk_tree_model_get_column_type(model, i));
	if (pyg_value_from_pyobject(&value, item)) {
	    Py_DECREF(item);
	    PyErr_SetString(PyExc_TypeError,
			    "value is of wrong type for this column");
	    return -1;
	}

	if (GTK_IS_LIST_STORE(model))
	    gtk_list_store_set_value(GTK_LIST_STORE(model), iter, i, &value);
	else if (GTK_IS_TREE_STORE(model))
	    gtk_tree_store_set_value(GTK_TREE_STORE(model), iter, i, &value);

	g_value_unset(&value);
	Py_DECREF(item);
    }
    return 0;
}

PyObject *
pygtk_tree_path_to_pyobject(GtkTreePath *path)
{
    gint len, i, *indices;
    PyObject *ret;

    len = gtk_tree_path_get_depth(path);
    indices = gtk_tree_path_get_indices(path);
    ret = PyTuple_New(len);
    for (i = 0; i < len; i++)
	PyTuple_SetItem(ret, i, PyInt_FromLong(indices[i]));
    return ret;
}

GtkTreePath *
pygtk_tree_path_from_pyobject(PyObject *object)
{
    if (PyString_Check(object)) {
	GtkTreePath *path;

	path = gtk_tree_path_new_from_string(PyString_AsString(object));
	return path;
    } else if (PyInt_Check(object)) {
	GtkTreePath *path;

	path = gtk_tree_path_new();
	gtk_tree_path_append_index(path, PyInt_AsLong(object));
	return path;
    } else if (PyTuple_Check(object)) {
	GtkTreePath *path;
	guint len, i;

	len = PyTuple_Size(object);
	if (len < 1)
	    return NULL;
	path = gtk_tree_path_new();
	for (i = 0; i < len; i++) {
	    PyObject *item = PyTuple_GetItem(object, i);
	    gint index = PyInt_AsLong(item);
	    if (PyErr_Occurred()) {
		gtk_tree_path_free(path);
		PyErr_Clear();
		return NULL;
	    }
	    gtk_tree_path_append_index(path, index);
	}
	return path;
    }
    return NULL;
}

/* marshalers for the boxed types.  Uses uppercase notation so that
 * the macro below can automatically install them. */
static PyObject *
PyGtkTreePath_from_value(const GValue *value)
{
    GtkTreePath *path = (GtkTreePath *)g_value_get_boxed(value);

    return pygtk_tree_path_to_pyobject(path);
}
static int
PyGtkTreePath_to_value(GValue *value, PyObject *object)
{
    GtkTreePath *path = pygtk_tree_path_from_pyobject(object);

    if (path) {
	g_value_set_boxed(value, path);
	gtk_tree_path_free(path);
	return 0;
    }
    return -1;
}

gboolean
pygdk_rectangle_from_pyobject(PyObject *object, GdkRectangle *rectangle)
{
    g_return_val_if_fail(rectangle != NULL, FALSE);

    if (pyg_boxed_check(object, GDK_TYPE_RECTANGLE)) {
	*rectangle = *pyg_boxed_get(object, GdkRectangle);
	return TRUE;
    }
    if (PyArg_ParseTuple(object, "iiii", &rectangle->x, &rectangle->y,
				&rectangle->width, &rectangle->height)) {
	return TRUE;
    }
    PyErr_Clear();
    PyErr_SetString(PyExc_TypeError, "could not convert to GdkRectangle");
    return FALSE;
}

static PyObject *
PyGdkRectangle_from_value(const GValue *value)
{
    GdkRectangle *rect = (GdkRectangle *)g_value_get_boxed(value);

    return pyg_boxed_new(GDK_TYPE_RECTANGLE, rect, TRUE, TRUE);
}
static int
PyGdkRectangle_to_value(GValue *value, PyObject *object)
{
    GdkRectangle rect;

    if (!pygdk_rectangle_from_pyobject(object, &rect))
	return -1;

    g_value_set_boxed(value, &rect);
    return 0;
}

/* We have to set ob_type here because stupid win32 does not allow you
 * to use variables from another dll in a global variable initialisation.
 */
void
_pygtk_register_boxed_types(PyObject *moddict)
{
    PyGtkStyleHelper_Type.ob_type = &PyType_Type;
    PyGtkRcStyleHelper_Type.ob_type = &PyType_Type;
    PyGdkAtom_Type.ob_type = &PyType_Type;
    PyGtkTreeModelRow_Type.ob_type = &PyType_Type;
    PyGtkTreeModelRowIter_Type.ob_type = &PyType_Type;

    PyType_Ready(&PyGtkStyleHelper_Type);
    PyType_Ready(&PyGtkRcStyleHelper_Type);
    PyType_Ready(&PyGdkAtom_Type);
    PyType_Ready(&PyGtkTreeModelRow_Type);
    PyType_Ready(&PyGtkTreeModelRowIter_Type);

    PyDict_SetItemString(moddict, "GdkAtomType", (PyObject *)&PyGdkAtom_Type);

    pyg_register_boxed_custom(GTK_TYPE_TREE_PATH,
			      PyGtkTreePath_from_value,
			      PyGtkTreePath_to_value);
    pyg_register_boxed_custom(GDK_TYPE_RECTANGLE,
			      PyGdkRectangle_from_value,
			      PyGdkRectangle_to_value);
}
