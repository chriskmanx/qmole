/*
 * peas-helpers.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 Steve Frécinaux
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

#ifndef __PEAS_HELPERS_H__
#define __PEAS_HELPERS_H__

#include <glib-object.h>

gpointer  _g_type_struct_ref        (GType         the_type);
void      _g_type_struct_unref      (GType         the_type,
                                     gpointer      type_struct);

gboolean  _valist_to_parameter_list (GType         the_type,
                                     gpointer      type_struct,
                                     const gchar  *first_property_name,
                                     va_list       var_args,
                                     GParameter  **params,
                                     guint        *n_params);

#endif /* __PEAS_HELPERS_H__ */
