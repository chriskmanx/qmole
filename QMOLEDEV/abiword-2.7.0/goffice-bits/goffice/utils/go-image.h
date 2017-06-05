/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-image.h - Image formats
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License, version 2, as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */
#ifndef GO_IMAGE_H
#define GO_IMAGE_H

#include <glib-object.h>
#include <goffice/utils/goffice-utils.h>
#ifdef GOFFICE_WITH_CAIRO
#	include <cairo.h>
#endif
#ifdef GOFFICE_WITH_GTK
#	include <gdk-pixbuf/gdk-pixbuf.h>
#endif

G_BEGIN_DECLS

typedef enum {
	GO_IMAGE_FORMAT_SVG,
	GO_IMAGE_FORMAT_PNG,
	GO_IMAGE_FORMAT_JPG,
	GO_IMAGE_FORMAT_PDF,
	GO_IMAGE_FORMAT_PS,
	GO_IMAGE_FORMAT_EMF,
	GO_IMAGE_FORMAT_WMF,
	GO_IMAGE_FORMAT_UNKNOWN
} GOImageFormat;

typedef struct {
	GOImageFormat format;
	char *name;
	char *desc;
	char *ext;
	gboolean has_pixbuf_saver;
	gboolean is_dpi_useful; 
	gboolean alpha_support;
} GOImageFormatInfo;

char 	  *go_mime_to_image_format      (char const *mime_type);
char 	  *go_image_format_to_mime      (char const *format);

GOImageFormatInfo const *go_image_get_format_info       	(GOImageFormat format);
GOImageFormat            go_image_get_format_from_name  	(char const *name);
GSList 			*go_image_get_formats_with_pixbuf_saver (void);

/******************
 * GOImage object *
 ******************/

#define GO_IMAGE_TYPE	(go_image_get_type ())
#define GO_IMAGE(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_IMAGE_TYPE, GOImage))
#define IS_GO_IMAGE(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_IMAGE_TYPE))

GType go_image_get_type (void);

#ifdef GOFFICE_WITH_CAIRO
cairo_t *go_image_get_cairo (GOImage *image);
cairo_pattern_t *go_image_create_cairo_pattern (GOImage *image);
#endif

#ifdef GOFFICE_WITH_GTK
GOImage *go_image_new_from_pixbuf (GdkPixbuf *pixbuf);
GdkPixbuf *go_image_get_pixbuf (GOImage *image);
#endif

GOImage *go_image_new_from_file (const char *filename, GError **error);
guint8 *go_image_get_pixels (GOImage *image);
int go_image_get_rowstride (GOImage *image);
void go_image_fill (GOImage *image, GOColor color);

G_END_DECLS

#endif /* GO_IMAGE_H */
