/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 *  gtksourceviewutils.h
 *
 *  Copyright (C) 2007 - Gustavo Giráldez and Paolo Maggi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef __GTK_SOURCE_VIEW_UTILS_H__
#define __GTK_SOURCE_VIEW_UTILS_H__

#include <glib.h>

G_BEGIN_DECLS

gchar 	**_gtk_source_view_get_default_dirs (const gchar  *basename,
					     gboolean      compat);

GSList 	 *_gtk_source_view_get_file_list    (gchar       **path,
					     const gchar  *suffix,
					     gboolean      only_dirs);

G_END_DECLS

#endif /* __GTK_SOURCE_VIEW_UTILS_H__ */
