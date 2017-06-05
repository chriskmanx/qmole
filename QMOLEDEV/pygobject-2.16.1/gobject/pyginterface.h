/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *               2004-2008  Johan Dahlin
 *   pyginterface.c: wrapper for the gobject library.
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

#ifndef __PYGOBJECT_INTERFACE_H__ 
#define __PYGOBJECT_INTERFACE_H__

extern GQuark pyginterface_type_key;
extern GQuark pyginterface_info_key;

extern PyTypeObject PyGInterface_Type;

void pyg_register_interface(PyObject *dict,
			    const gchar *class_name,
			    GType gtype,
			    PyTypeObject *type);
const GInterfaceInfo * pyg_lookup_interface_info(GType gtype);
void pyg_register_interface_info(GType gtype, const
				 GInterfaceInfo *info);
void pygobject_interface_register_types(PyObject *d);

#endif /* __PYGOBJECT_INTERFACE_H__ */
