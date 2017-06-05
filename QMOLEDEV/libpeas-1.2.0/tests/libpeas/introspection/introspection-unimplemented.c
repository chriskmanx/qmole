/*
 * introspection-unimplemented.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 Garrett Regier
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

#include "introspection-unimplemented.h"

#include <glib.h>

G_DEFINE_INTERFACE(IntrospectionUnimplemented, introspection_unimplemented, G_TYPE_OBJECT)

void
introspection_unimplemented_default_init (IntrospectionUnimplementedInterface *iface)
{
  /* AFAIK there is no way to make this unimplemented but
   * in the tests no one will implement it so its fine.
   */
}

