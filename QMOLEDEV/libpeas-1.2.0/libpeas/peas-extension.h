/*
 * peas-extension.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Steve Frécinaux
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

#ifndef __PEAS_EXTENSION_H__
#define __PEAS_EXTENSION_H__

#include <glib-object.h>
#include <girepository.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define PEAS_TYPE_EXTENSION            (G_TYPE_OBJECT)
#define PEAS_EXTENSION(obj)            (G_OBJECT(obj))
#define PEAS_IS_EXTENSION(obj)         (G_IS_OBJECT(obj))

/**
 * PeasExtension:
 *
 * A proxy class to access the actual plugin.
 */
typedef GObject PeasExtension;

/*
 * All the public methods of PeasExtension are deprecated and should not be
 * used. Due to gi-scanner's touchiness, we also hide these legacy API from 
 * GI to avoid hairy issues.
 */
#if !defined(PEAS_DISABLE_DEPRECATED) && !defined(__GI_SCANNER__)
GType        peas_extension_get_type        (void)  G_GNUC_CONST;

GType        peas_extension_get_extension_type
                                            (PeasExtension *exten);

gboolean     peas_extension_call            (PeasExtension *exten,
                                             const gchar   *method_name,
                                             ...);
gboolean     peas_extension_call_valist     (PeasExtension *exten,
                                             const gchar   *method_name,
                                             va_list        args);
gboolean     peas_extension_callv           (PeasExtension *exten,
                                             const gchar   *method_name,
                                             GIArgument    *args,
                                             GIArgument    *return_value);
#endif

G_END_DECLS

#endif /* __PEAS_EXTENSION_H__ */
