/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcestyle.h
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
 *
 * GtkSourceView is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * GtkSourceView is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef __GTK_SOURCE_STYLE_H__
#define __GTK_SOURCE_STYLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define GTK_SOURCE_TYPE_STYLE		(gtk_source_style_get_type ())
#define GTK_SOURCE_STYLE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_SOURCE_TYPE_STYLE, GtkSourceStyle))
#define GTK_SOURCE_IS_STYLE(obj)	(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_SOURCE_TYPE_STYLE))

typedef struct _GtkSourceStyle GtkSourceStyle;

GType		 gtk_source_style_get_type	(void) G_GNUC_CONST;

GtkSourceStyle	*gtk_source_style_copy		(const GtkSourceStyle *style);


G_END_DECLS

#endif  /* __GTK_SOURCE_STYLE_H__ */
