/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcemarkattributes.h
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 * Copyright (C) 2010 - Krzesimir Nowak
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

#ifndef __GTK_SOURCE_MARK_ATTRIBUTES_H__
#define __GTK_SOURCE_MARK_ATTRIBUTES_H__

#include <gtk/gtk.h>
#include <gtksourceview/gtksourcemark.h>

G_BEGIN_DECLS

#define GTK_SOURCE_TYPE_MARK_ATTRIBUTES			(gtk_source_mark_attributes_get_type ())
#define GTK_SOURCE_MARK_ATTRIBUTES(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_SOURCE_TYPE_MARK_ATTRIBUTES, GtkSourceMarkAttributes))
#define GTK_SOURCE_MARK_ATTRIBUTES_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_SOURCE_TYPE_MARK_ATTRIBUTES, GtkSourceMarkAttributes const))
#define GTK_SOURCE_MARK_ATTRIBUTES_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GTK_SOURCE_TYPE_MARK_ATTRIBUTES, GtkSourceMarkAttributesClass))
#define GTK_SOURCE_IS_MARK_ATTRIBUTES(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_SOURCE_TYPE_MARK_ATTRIBUTES))
#define GTK_SOURCE_IS_MARK_ATTRIBUTES_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_SOURCE_TYPE_MARK_ATTRIBUTES))
#define GTK_SOURCE_MARK_ATTRIBUTES_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_SOURCE_TYPE_MARK_ATTRIBUTES, GtkSourceMarkAttributesClass))

typedef struct _GtkSourceMarkAttributes		GtkSourceMarkAttributes;
typedef struct _GtkSourceMarkAttributesClass	GtkSourceMarkAttributesClass;
typedef struct _GtkSourceMarkAttributesPrivate	GtkSourceMarkAttributesPrivate;

struct _GtkSourceMarkAttributes
{
	/*< private >*/
	GObject parent;

	GtkSourceMarkAttributesPrivate *priv;

	/*< public >*/
};

struct _GtkSourceMarkAttributesClass
{
	/*< private >*/
	GObjectClass parent_class;

	/*< public >*/
};

GType gtk_source_mark_attributes_get_type (void) G_GNUC_CONST;

GtkSourceMarkAttributes *gtk_source_mark_attributes_new (void);

void             gtk_source_mark_attributes_set_background      (GtkSourceMarkAttributes *attributes,
                                                                 const GdkRGBA           *background);

gboolean         gtk_source_mark_attributes_get_background      (GtkSourceMarkAttributes *attributes,
                                                                 GdkRGBA                 *background);

void             gtk_source_mark_attributes_set_stock_id        (GtkSourceMarkAttributes *attributes,
                                                                 const gchar             *stock_id);
const gchar     *gtk_source_mark_attributes_get_stock_id        (GtkSourceMarkAttributes *attributes);

void             gtk_source_mark_attributes_set_icon_name       (GtkSourceMarkAttributes *attributes,
                                                                 const gchar             *icon_name);
const gchar     *gtk_source_mark_attributes_get_icon_name       (GtkSourceMarkAttributes *attributes);

void             gtk_source_mark_attributes_set_gicon           (GtkSourceMarkAttributes *attributes,
                                                                 GIcon                   *gicon);
GIcon           *gtk_source_mark_attributes_get_gicon           (GtkSourceMarkAttributes *attributes);

void             gtk_source_mark_attributes_set_pixbuf          (GtkSourceMarkAttributes *attributes,
                                                                 const GdkPixbuf         *pixbuf);
const GdkPixbuf *gtk_source_mark_attributes_get_pixbuf          (GtkSourceMarkAttributes *attributes);

const GdkPixbuf *gtk_source_mark_attributes_render_icon         (GtkSourceMarkAttributes *attributes,
                                                                 GtkWidget               *widget,
                                                                 gint                   size);

gchar           *gtk_source_mark_attributes_get_tooltip_text    (GtkSourceMarkAttributes *attributes,
                                                                 GtkSourceMark           *mark);

gchar           *gtk_source_mark_attributes_get_tooltip_markup  (GtkSourceMarkAttributes *attributes,
                                                                 GtkSourceMark           *mark);

G_END_DECLS

#endif /* __GTK_SOURCE_MARK_ATTRIBUTES_H__ */
