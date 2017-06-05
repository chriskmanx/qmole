/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gthread.c  GThread integration
 *
 * Copyright (C) 2002  CodeFactory AB
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

#include <config.h>

/* #define G_DEBUG_LOCKS 1 */

#include <glib.h>
#include <dbus/dbus.h>
#include "dbus-glib.h"

/**
 * dbus_g_thread_init:
 *
 * Initializes the D-BUS thread system.
 * This function may only be called
 * once and must be called prior to calling any
 * other function in the D-BUS API.
 */
void
dbus_g_thread_init (void)
{
  if (!g_thread_supported ())
    g_error ("g_thread_init() must be called before dbus_threads_init()");

  dbus_threads_init_default ();
}
