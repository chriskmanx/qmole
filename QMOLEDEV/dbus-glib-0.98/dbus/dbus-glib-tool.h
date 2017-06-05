/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-glib-tool.h: Definitions used internally by binding tool
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
#ifndef DBUS_BINDING_TOOL_H
#define DBUS_BINDING_TOOL_H

#include <glib.h>

typedef enum
{
  DBUS_BINDING_TOOL_ERROR_UNSUPPORTED_CONVERSION,
  DBUS_BINDING_TOOL_ERROR_INVALID_ANNOTATION
} DBusBindingToolError;

#define DBUS_BINDING_TOOL_ERROR dbus_binding_tool_error_quark ()

GQuark dbus_binding_tool_error_quark (void);

#endif
