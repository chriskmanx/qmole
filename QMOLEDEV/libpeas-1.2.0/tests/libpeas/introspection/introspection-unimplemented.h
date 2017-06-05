/*
 * introspection-unimplemented.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Garrett Regier
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

#ifndef __INTROSPECTION_UNIMPLEMENTED_H__
#define __INTROSPECTION_UNIMPLEMENTED_H__

#include <glib-object.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define INTROSPECTION_TYPE_UNIMPLEMENTED             (introspection_unimplemented_get_type ())
#define INTROSPECTION_UNIMPLEMENTED(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), INTROSPECTION_TYPE_UNIMPLEMENTED, IntrospectionUnimplemented))
#define INTROSPECTION_UNIMPLEMENTED_IFACE(obj)       (G_TYPE_CHECK_CLASS_CAST ((obj), INTROSPECTION_TYPE_UNIMPLEMENTED, IntrospectionUnimplementedInterface))
#define INTROSPECTION_IS_UNIMPLEMENTED(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), INTROSPECTION_TYPE_UNIMPLEMENTED))
#define INTROSPECTION_UNIMPLEMENTED_GET_IFACE(obj)   (G_TYPE_INSTANCE_GET_INTERFACE ((obj), INTROSPECTION_TYPE_UNIMPLEMENTED, IntrospectionUnimplementedInterface))

typedef struct _IntrospectionUnimplemented           IntrospectionUnimplemented; /* dummy typedef */
typedef struct _IntrospectionUnimplementedInterface  IntrospectionUnimplementedInterface;

struct _IntrospectionUnimplementedInterface {
  GTypeInterface g_iface;
};

/*
 * Public methods
 */
GType introspection_unimplemented_get_type (void)  G_GNUC_CONST;

G_END_DECLS

#endif /* __INTROSPECTION_UNIMPLEMENTED_H__ */
