/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- */
/*
 * gnome-paint
 * Copyright (C) Rogério Ferro do Nascimento 2010 <rogerioferro@gmail.com>
 * 
 * gnome-paint is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * gnome-paint is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _GP_IMAGE_H_
#define _GP_IMAGE_H_

#include <glib-object.h>
 #include <gtk/gtk.h>


G_BEGIN_DECLS

#define GP_TYPE_IMAGE             (gp_image_get_type ())
#define GP_IMAGE(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GP_TYPE_IMAGE, GpImage))
#define GP_IMAGE_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), GP_TYPE_IMAGE, GpImageClass))
#define GP_IS_IMAGE(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GP_TYPE_IMAGE))
#define GP_IS_IMAGE_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), GP_TYPE_IMAGE))
#define GP_IMAGE_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GP_TYPE_IMAGE, GpImageClass))

typedef struct _GpImageClass	GpImageClass;
typedef struct _GpImage			GpImage;
typedef struct _GpImagePrivate  GpImagePrivate;
typedef struct _GpImageData		GpImageData;

struct _GpImageClass
{
	GObjectClass parent_class;
};

struct _GpImage
{
	GObject parent_instance;

	GpImagePrivate  *priv;
};

GType			gp_image_get_type			( void ) G_GNUC_CONST;
GpImage *		gp_image_new				( gint width, gint height, 
						                      gboolean has_alpha );
GpImage *		gp_image_new_from_pixmap	( GdkPixmap* pixmap, 
			                                  GdkRectangle *rect, 
			                                  gboolean has_alpha );
GpImage *		gp_image_new_from_data		( GpImageData *data );
void			gp_image_set_mask			( GpImage *image, GdkBitmap *mask );
GdkPixbuf *		gp_image_get_pixbuf			( GpImage *image );
GpImageData *   gp_image_get_data			( GpImage *image );
void			gp_image_data_free			( GpImageData *data );
void			gp_image_draw				( GpImage *image, 
							                  GdkDrawable *drawable,
							                  GdkGC *gc,
							                  gint x, gint y );
gint			gp_image_get_width			( GpImage *image );
gint			gp_image_get_height			( GpImage *image );
gboolean		gp_image_get_has_alpha		( GpImage *image );
GdkBitmap *		gp_image_get_mask			( GpImage *image );

void			gp_image_set_diff_pixmap	( GpImage *image, 
				                              GdkPixmap* pixmap, 
				                              guint x_offset, 
				                              guint y_offset );


G_END_DECLS

#endif /* _GP_IMAGE_H_ */
