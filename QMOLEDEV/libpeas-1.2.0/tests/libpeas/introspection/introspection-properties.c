/*
 * introspection-properties.h
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

#include "introspection-properties.h"

G_DEFINE_INTERFACE(IntrospectionProperties, introspection_properties, G_TYPE_OBJECT)

void
introspection_properties_default_init (IntrospectionPropertiesInterface *iface)
{
  static gboolean initialized = FALSE;

#define DEFINE_PROP(name, flags) \
  g_object_interface_install_property (iface, \
                                       g_param_spec_string (name, name, \
                                                            name, name, \
                                                            G_PARAM_STATIC_STRINGS | \
                                                            (flags)))

  if (!initialized)
    {
      DEFINE_PROP ("construct-only", G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
      DEFINE_PROP ("read-only", G_PARAM_READABLE);
      DEFINE_PROP ("write-only", G_PARAM_WRITABLE);
      DEFINE_PROP ("readwrite", G_PARAM_READWRITE | G_PARAM_CONSTRUCT);

      initialized = TRUE;
    }

#undef DEFINE_PROP
}
