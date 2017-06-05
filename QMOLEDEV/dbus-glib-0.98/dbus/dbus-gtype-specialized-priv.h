/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gtype-specialized-priv.h: internals of specialized GTypes
 *
 * Copyright (C) 2009 Collabora Ltd.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 */

#ifndef DBUS_GOBJECT_TYPE_SPECIALIZED_PRIV_H
#define DBUS_GOBJECT_TYPE_SPECIALIZED_PRIV_H

#include "dbus-gtype-specialized.h"

G_BEGIN_DECLS

G_GNUC_INTERNAL
void           _dbus_g_type_register_collection      (const char                                   *name,
						       const DBusGTypeSpecializedCollectionVtable   *vtable,
						       guint                                         flags);

G_GNUC_INTERNAL
void           _dbus_g_type_register_map             (const char                                   *name,
						       const DBusGTypeSpecializedMapVtable          *vtable,
						       guint                                         flags);

G_GNUC_INTERNAL
void           _dbus_g_type_register_struct            (const char                                   *name,
						       const DBusGTypeSpecializedStructVtable        *vtable,
						       guint                                          flags);

G_END_DECLS

#endif
