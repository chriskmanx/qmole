/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *
 *   gtkmodule.c: module wrapping the GTK library
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/* include this first, before NO_IMPORT_PYGOBJECT is defined */
#include <pygobject.h>
#include "pygtk-private.h"
#include <pyerrors.h>

#ifdef HAVE_PYCAIRO
# include <pycairo.h>
Pycairo_CAPI_t *Pycairo_CAPI;
#endif

void _pygtk_register_boxed_types(PyObject *moddict);
void pygtk_register_classes(PyObject *d);
void pygdk_register_classes(PyObject *d);
void pygtk_add_constants(PyObject *module, const gchar *strip_prefix);
void pygdk_add_constants(PyObject *module, const gchar *strip_prefix);

extern PyMethodDef pygtk_functions[];
extern PyMethodDef pygdk_functions[];

static PyObject *PyGtkDeprecationWarning;
PyObject *PyGtkWarning;

static struct _PyGtk_FunctionStruct functions = {
    VERSION,

    &PyGdkAtom_Type,  PyGdkAtom_New,
    pygdk_rectangle_from_pyobject,
    pygtk_tree_path_to_pyobject,
    pygtk_tree_path_from_pyobject,
};

static void
pygtk_add_stock_items(PyObject *d)
{
    GSList *stock_ids, *cur;
    char buf[128];

    stock_ids = gtk_stock_list_ids();
    strcpy(buf, "STOCK_");
    for (cur = stock_ids; cur; cur = stock_ids) {
	char *ctmp;
	PyObject *obj;
	int i;
	
	ctmp = cur->data;
	if(strncmp(ctmp, "gtk-", 4)) {
		g_free(cur->data);
		stock_ids = cur->next;
		g_slist_free_1(cur);
		continue;
	}
	ctmp += 4;
	
	strcpy(buf + sizeof("STOCK"), ctmp);
	ctmp = buf + sizeof("STOCK");
	for (i = 0; ctmp[i]; i++) {
		if (ctmp[i] == '-')
		    ctmp[i] = '_';
		else if (ctmp[i] >= 'a' && ctmp[i] <= 'z')
		    ctmp[i] -= 'a'-'A';
	}
	
	obj = PyString_FromString(cur->data);
	PyDict_SetItemString(d, buf, obj);
	Py_DECREF(obj);
	g_free(cur->data);
	stock_ids = cur->next;
	g_slist_free_1(cur);
    }
    
}

static void
pygdk_add_extra_constants(PyObject *m)
{
    gchar * aname;

    PyModule_AddObject(m, "CURRENT_TIME",
		       PyLong_FromLong(GDK_CURRENT_TIME));
    PyModule_AddObject(m, "PARENT_RELATIVE",
		       PyLong_FromLong(GDK_PARENT_RELATIVE));

      /* Add predefined atoms */
#define add_atom(name) { aname = gdk_atom_name((GDK_##name)); \
PyModule_AddObject(m, #name, PyString_FromString(aname)); \
g_free(aname); }

    add_atom(SELECTION_PRIMARY);
    add_atom(SELECTION_SECONDARY);
    add_atom(SELECTION_CLIPBOARD);
    add_atom(TARGET_BITMAP);
    add_atom(TARGET_COLORMAP);
    add_atom(TARGET_DRAWABLE);
    add_atom(TARGET_PIXMAP);
    add_atom(TARGET_STRING);
    add_atom(SELECTION_TYPE_ATOM);
    add_atom(SELECTION_TYPE_BITMAP);
    add_atom(SELECTION_TYPE_COLORMAP);
    add_atom(SELECTION_TYPE_DRAWABLE);
    add_atom(SELECTION_TYPE_INTEGER);
    add_atom(SELECTION_TYPE_PIXMAP);
    add_atom(SELECTION_TYPE_WINDOW);
    add_atom(SELECTION_TYPE_STRING);
#undef add_atom
}

static void
pygtk_add_extra_constants(PyObject *m)
{
#if GTK_CHECK_VERSION(2, 9, 3)
    PyModule_AddObject(m, "PAPER_NAME_A3",
		       PyString_FromString(GTK_PAPER_NAME_A3));
    PyModule_AddObject(m, "PAPER_NAME_A4",
		       PyString_FromString(GTK_PAPER_NAME_A4));
    PyModule_AddObject(m, "PAPER_NAME_A5",
		       PyString_FromString(GTK_PAPER_NAME_A5));
    PyModule_AddObject(m, "PAPER_NAME_B5",
		       PyString_FromString(GTK_PAPER_NAME_B5));
    PyModule_AddObject(m, "PAPER_NAME_LETTER",
		       PyString_FromString(GTK_PAPER_NAME_LETTER));
    PyModule_AddObject(m, "PAPER_NAME_EXECUTIVE",
		       PyString_FromString(GTK_PAPER_NAME_EXECUTIVE));
    PyModule_AddObject(m, "PAPER_NAME_LEGAL",
		       PyString_FromString(GTK_PAPER_NAME_LEGAL));
#endif
}

static gboolean
init_pycairo(void)
{
#ifdef HAVE_PYCAIRO
    Pycairo_IMPORT;
    if (Pycairo_CAPI == NULL)
        return FALSE;
#endif
    return TRUE;
}

DL_EXPORT(void)
init_gtk(void)
{
    PyObject *m, *d, *tuple, *o;

    /* initialise pygobject */
    init_pygobject_check(2, 12, 0);
    g_assert(pygobject_register_class != NULL);
    
    /* initialise pycairo */
    if (!init_pycairo())
	return;
    
    /* initialise pygtk */
    gtk_type_init(0);

    m = Py_InitModule("gtk._gtk", pygtk_functions);
    d = PyModule_GetDict(m);

    /* gtk+ version */
    tuple = Py_BuildValue("(iii)", gtk_major_version, gtk_minor_version,
			  gtk_micro_version);
    PyDict_SetItemString(d, "gtk_version", tuple);    
    Py_DECREF(tuple);
	
    /* pygtk version */
    tuple = Py_BuildValue("(iii)", PYGTK_MAJOR_VERSION, PYGTK_MINOR_VERSION,
			  PYGTK_MICRO_VERSION);
    PyDict_SetItemString(d, "pygtk_version", tuple);
    Py_DECREF(tuple);
	
    _pygtk_register_boxed_types(d);
    pygtk_register_classes(d);
    pygtk_add_constants(m, "GTK_");
    pygtk_add_extra_constants(m);
    pygtk_add_stock_items(d);
    
    /* extension API */
    PyDict_SetItemString(d, "_PyGtk_API",
			 o=PyCObject_FromVoidPtr(&functions, NULL));
    Py_DECREF(o);
	
    PyGtkDeprecationWarning = PyErr_NewException("gtk.GtkDeprecationWarning",
						 PyExc_DeprecationWarning, NULL);
    PyDict_SetItemString(d, "DeprecationWarning", PyGtkDeprecationWarning);

    PyGtkWarning = PyErr_NewException("gtk.GtkWarning", PyExc_Warning, NULL);
    PyDict_SetItemString(d, "Warning", PyGtkWarning);

    /* namespace all the gdk stuff in gtk.gdk ... */
    m = Py_InitModule("gtk.gdk", pygdk_functions);
    d = PyModule_GetDict(m);

    pygdk_register_classes(d);
    pygdk_add_constants(m, "GDK_");
    pygdk_add_extra_constants(m);

#if defined(GDK_WINDOWING_X11)
    PyModule_AddStringConstant(m, "WINDOWING", "x11");
#elif defined(GDK_WINDOWING_WIN32)
    PyModule_AddStringConstant(m, "WINDOWING", "win32");
#elif defined(GDK_WINDOWING_QUARTZ)
    PyModule_AddStringConstant(m, "WINDOWING", "quartz");
#elif defined(GDK_WINDOWING_DIRECTFB)
    PyModule_AddStringConstant(m, "WINDOWING", "directfb");
#else
    PyModule_AddStringConstant(m, "WINDOWING", "?");
#endif
}
