/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcegutterrenderertext.h
 * This file is part of GtkSourceView 
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
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

#ifndef __GTK_SOURCE_GUTTER_RENDERER_PIXBUF_H__
#define __GTK_SOURCE_GUTTER_RENDERER_PIXBUF_H__

#include <gtksourceview/gtksourcegutterrenderer.h>

G_BEGIN_DECLS

#define GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF			(gtk_source_gutter_renderer_pixbuf_get_type ())
#define GTK_SOURCE_GUTTER_RENDERER_PIXBUF(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF, GtkSourceGutterRendererPixbuf))
#define GTK_SOURCE_GUTTER_RENDERER_PIXBUF_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF, GtkSourceGutterRendererPixbuf const))
#define GTK_SOURCE_GUTTER_RENDERER_PIXBUF_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF, GtkSourceGutterRendererPixbufClass))
#define GTK_SOURCE_IS_GUTTER_RENDERER_PIXBUF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF))
#define GTK_SOURCE_IS_GUTTER_RENDERER_PIXBUF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF))
#define GTK_SOURCE_GUTTER_RENDERER_PIXBUF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF, GtkSourceGutterRendererPixbufClass))

typedef struct _GtkSourceGutterRendererPixbuf		GtkSourceGutterRendererPixbuf;
typedef struct _GtkSourceGutterRendererPixbufClass	GtkSourceGutterRendererPixbufClass;
typedef struct _GtkSourceGutterRendererPixbufPrivate	GtkSourceGutterRendererPixbufPrivate;

struct _GtkSourceGutterRendererPixbuf
{
	/*< private >*/
	GtkSourceGutterRenderer parent;

	GtkSourceGutterRendererPixbufPrivate *priv;

	/*< public >*/
};

struct _GtkSourceGutterRendererPixbufClass
{
	/*< private >*/
	GtkSourceGutterRendererClass parent_class;

	/*< public >*/
};

GType gtk_source_gutter_renderer_pixbuf_get_type (void) G_GNUC_CONST;

GtkSourceGutterRenderer *gtk_source_gutter_renderer_pixbuf_new (void);

void         gtk_source_gutter_renderer_pixbuf_set_pixbuf       (GtkSourceGutterRendererPixbuf *renderer,
                                                                 GdkPixbuf                     *pixbuf);

GdkPixbuf   *gtk_source_gutter_renderer_pixbuf_get_pixbuf       (GtkSourceGutterRendererPixbuf *renderer);

void         gtk_source_gutter_renderer_pixbuf_set_stock_id     (GtkSourceGutterRendererPixbuf *renderer,
                                                                 const gchar                   *stock_id);

const gchar *gtk_source_gutter_renderer_pixbuf_get_stock_id     (GtkSourceGutterRendererPixbuf *renderer);

void         gtk_source_gutter_renderer_pixbuf_set_gicon        (GtkSourceGutterRendererPixbuf *renderer,
                                                                 GIcon                         *icon);

GIcon       *gtk_source_gutter_renderer_pixbuf_get_gicon        (GtkSourceGutterRendererPixbuf *renderer);

void         gtk_source_gutter_renderer_pixbuf_set_icon_name    (GtkSourceGutterRendererPixbuf *renderer,
                                                                 const gchar                   *icon_name);

const gchar *gtk_source_gutter_renderer_pixbuf_get_icon_name    (GtkSourceGutterRendererPixbuf *renderer);

G_END_DECLS

#endif /* __GTK_SOURCE_GUTTER_RENDERER_TEXT_H__ */
