/*
 * extension-c-plugin.c
 * This file is part of libpeas
 *
 * Copyright (C) 2011 - Garrett Regier
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "extension-c-plugin.h"

#include "introspection-callable.h"

#include "callable-plugin.h"

G_MODULE_EXPORT void
peas_register_types (PeasObjectModule *module)
{
  testing_callable_plugin_register (G_TYPE_MODULE (module));

  peas_object_module_register_extension_type (module,
                                              INTROSPECTION_TYPE_CALLABLE,
                                              TESTING_TYPE_CALLABLE_PLUGIN);
}
