/* -*- mode: C; c-file-style: "gnu" -*- */
/* egg-dbus.h GLib main loop integration
 *
 * Copyright (C) 2002, 2003 CodeFactory AB
 * Copyright (C) 2005 Red Hat, Inc.
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifndef EGGDBUS_H_
#define EGGDBUS_H_

#include <glib.h>
#include <dbus/dbus.h>

void egg_dbus_connect_with_mainloop (DBusConnection *connection, GMainContext *context);

void egg_dbus_disconnect_from_mainloop (DBusConnection *connection, GMainContext *context);

#endif /*EGGDBUS_H_*/
