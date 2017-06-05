/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-binding-tool-output-glib.h prototypes for glib output
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
 * This program is distributed in the hope that it bwill be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */
#ifndef DBUS_BINDING_TOOL_OUTPUT_GLIB_H
#define DBUS_BINDING_TOOL_OUTPUT_GLIB_H

G_BEGIN_DECLS

#define DBUS_GLIB_ANNOTATION_C_SYMBOL "org.freedesktop.DBus.GLib.CSymbol"
#define DBUS_GLIB_ANNOTATION_CLIENT_C_SYMBOL "org.freedesktop.DBus.GLib.ClientCSymbol"
#define DBUS_GLIB_ANNOTATION_ASYNC "org.freedesktop.DBus.GLib.Async"
#define DBUS_GLIB_ANNOTATION_CONST "org.freedesktop.DBus.GLib.Const"
#define DBUS_GLIB_ANNOTATION_RETURNVAL "org.freedesktop.DBus.GLib.ReturnVal"
#define DBUS_GLIB_ANNOTATION_NOREPLY "org.freedesktop.DBus.Method.NoReply"

gboolean dbus_binding_tool_output_glib_client (BaseInfo *info, GIOChannel *channel, gboolean ignore_unsupported, GError **error);
gboolean dbus_binding_tool_output_glib_server (BaseInfo *info, GIOChannel *channel, const char *prefix, GError **error);

G_END_DECLS

#endif
