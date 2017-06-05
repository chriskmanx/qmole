/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcestyle-private.h
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

#ifndef __GTK_SOURCE_STYLE_PRIVATE_H__
#define __GTK_SOURCE_STYLE_PRIVATE_H__

#include "gtksourcestyle.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GTK_SOURCE_STYLE_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_SOURCE_TYPE_STYLE, GtkSourceStyleClass))
#define GTK_SOURCE_IS_STYLE_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_SOURCE_TYPE_STYLE))
#define GTK_SOURCE_STYLE_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_SOURCE_TYPE_STYLE, GtkSourceStyleClass))

enum {
	GTK_SOURCE_STYLE_USE_LINE_BACKGROUND = 1 << 0,	/*< nick=use_line_background >*/
	GTK_SOURCE_STYLE_USE_BACKGROUND      = 1 << 1,	/*< nick=use_background >*/
	GTK_SOURCE_STYLE_USE_FOREGROUND      = 1 << 2,	/*< nick=use_foreground >*/
	GTK_SOURCE_STYLE_USE_ITALIC          = 1 << 3,	/*< nick=use_italic >*/
	GTK_SOURCE_STYLE_USE_BOLD            = 1 << 4,	/*< nick=use_bold >*/
	GTK_SOURCE_STYLE_USE_UNDERLINE       = 1 << 5,	/*< nick=use_underline >*/
	GTK_SOURCE_STYLE_USE_STRIKETHROUGH   = 1 << 6	/*< nick=use_strikethrough >*/
};

struct _GtkSourceStyle
{
	GObject base_instance;
	/* foreground and background are strings interned with
	 * with g_intern_string(), so we don't need to copy/free
	 * them. */
	const gchar *foreground;
	const gchar *background;
	const gchar *line_background;
	guint italic : 1;
	guint bold : 1;
	guint underline : 1;
	guint strikethrough : 1;
	guint mask : 12;
};

void		 _gtk_source_style_apply	(const GtkSourceStyle *style,
						 GtkTextTag           *tag);


G_END_DECLS

#endif  /* __GTK_SOURCE_STYLE_PRIVATE_H__ */
