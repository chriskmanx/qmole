/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gobject.h: common functions used to map between D-BUS and GObject
 *
 * Copyright (C) 2005  Red Hat, Inc.
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */
#ifndef DBUS_GLIB_OBJECT_H
#define DBUS_GLIB_OBJECT_H

#include <dbus/dbus.h>
#include <dbus/dbus-signature.h>
#include <glib.h>
#include "dbus/dbus-glib.h"

G_BEGIN_DECLS

const char *       _dbus_gobject_get_path (GObject *obj);

GClosureMarshal    _dbus_gobject_lookup_marshaller (GType        rettype,
						    guint        n_params,
						    const GType *param_types);
  
						    

G_END_DECLS

#endif
