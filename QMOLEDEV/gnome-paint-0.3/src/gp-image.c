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


#include "gp-image.h"
#include <string.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <glib/gi18n.h>



#define GP_IMAGE_GET_PRIVATE(object)	\
	(G_TYPE_INSTANCE_GET_PRIVATE ((object), GP_TYPE_IMAGE, GpImagePrivate))


const int BITS_PER_SAMPLE = 8;


struct _GpImageData
{
    guint8  *buffer;
    gsize   len;
};


struct _GpImagePrivate
{
	GdkPixbuf *pixbuf;
};


G_DEFINE_TYPE (GpImage, gp_image, G_TYPE_OBJECT);

static void
gp_image_init (GpImage *object)
{
	object->priv = GP_IMAGE_GET_PRIVATE (object);
	object->priv->pixbuf = NULL;
}

static void
gp_image_finalize (GObject *object)
{
	GpImagePrivate *priv;
	priv	=	GP_IMAGE (object)->priv;
	if ( priv->pixbuf != NULL )
	{
		g_object_unref ( priv->pixbuf );
	}
	G_OBJECT_CLASS (gp_image_parent_class)->finalize (object);	
}

static void
gp_image_class_init (GpImageClass *klass)
{
	GObjectClass* object_class = G_OBJECT_CLASS (klass);
	GObjectClass* parent_class = G_OBJECT_CLASS (klass);

	object_class->finalize = gp_image_finalize;

	g_type_class_add_private (object_class, sizeof (GpImagePrivate));
}


GpImage * 
gp_image_new ( gint width, gint height, gboolean has_alpha  )
{
	GpImage *image;

	g_return_val_if_fail (width > 0, NULL);
	g_return_val_if_fail (height > 0, NULL);
	
	image = g_object_new (GP_TYPE_IMAGE, NULL);
    image->priv->pixbuf = gdk_pixbuf_new (
            GDK_COLORSPACE_RGB, has_alpha, BITS_PER_SAMPLE, width, height);
    g_assert(image->priv->pixbuf);
	g_object_set_data ( G_OBJECT(image), "pixbuf", image->priv->pixbuf);

	return image;
}


GpImage * 
gp_image_new_from_pixmap ( GdkPixmap* pixmap, GdkRectangle *rect, gboolean has_alpha  )
{
    GpImage			*image;
	GdkRectangle	r;

	g_return_val_if_fail ( GDK_IS_PIXMAP (pixmap), NULL);

	if ( rect == NULL )
	{
		r.x = r.y = 0;
		gdk_drawable_get_size ( pixmap, &r.width, &r.height );
	}
	else
	{
		r.x		=   rect->x;
		r.y		=   rect->y;
		r.width =   rect->width;
		r.height=   rect->height;
	}
	
	image   = gp_image_new( r.width, r.height, has_alpha );
	g_return_val_if_fail ( image != NULL, NULL);
	
    gdk_pixbuf_get_from_drawable(
                image->priv->pixbuf,
                pixmap,
                gdk_drawable_get_colormap ( pixmap ), 
                r.x, r.y,
                0, 0,
                r.width, r.height);
    return image;
}

GpImage *
gp_image_new_from_data ( GpImageData *data )
{
	GpImage			*image;
    GInputStream	*stream;

	g_return_val_if_fail (data, NULL);

	image = g_object_new (GP_TYPE_IMAGE, NULL);

	stream  =	g_memory_input_stream_new_from_data ( data->buffer, data->len, NULL );
    image->priv->pixbuf  =   gdk_pixbuf_new_from_stream ( stream, NULL, NULL );
	g_object_unref ( stream );

	g_assert(image->priv->pixbuf);
	g_object_set_data ( G_OBJECT(image), "pixbuf", image->priv->pixbuf);


	return image;
}


struct SaveToBufferData {
	gchar *buffer;
	gsize len, max;
};


static gboolean            
save_to_buffer_callback ( const gchar *data,
             			  gsize count,
             			  GError **error,
            			  gpointer user_data )
{
	struct SaveToBufferData *sdata = user_data;
	gchar *new_buffer;
	gsize new_max;

	if (sdata->len + count > sdata->max) 
	{
		new_max = sdata->len + count + 16;
		new_buffer = g_try_realloc (sdata->buffer, new_max);
		if (!new_buffer) 
		{
            /*Insufficient memory to save image into a buffer*/
			return FALSE;
		}
		sdata->buffer = new_buffer;
		sdata->max = new_max;
	}
	memcpy (sdata->buffer + sdata->len, data, count);
	sdata->len += count;
	return TRUE;
}

GpImageData *   
gp_image_get_data ( GpImage *image )
{
	static const gint initial_max = 66;
	struct SaveToBufferData sdata;
	sdata.buffer = g_try_malloc (initial_max);
	sdata.max = initial_max;
	sdata.len = 0;
	if (!sdata.buffer) 
	{
		/*Insufficient memory to save image into a buffer*/
		return NULL;
	}
	if (!gdk_pixbuf_save_to_callback ( image->priv->pixbuf,
	                          		   save_to_buffer_callback,
	                                   &sdata,
	                                   "png", NULL, NULL ) ) 
	{
		g_free (sdata.buffer);
		return NULL;
	}
	else
	{
		GpImageData *data;
		g_print ("data size:%d\n",sdata.len);
		data			= g_slice_new ( GpImageData );
		data->buffer	= sdata.buffer;
		data->len		= sdata.len;
		return data;
	}
}

void
gp_image_data_free ( GpImageData *data )
{
	g_free (data->buffer);
	g_slice_free (GpImageData, data);
}

GdkPixbuf *
gp_image_get_pixbuf ( GpImage *image )
{
	g_return_val_if_fail ( GP_IS_IMAGE (image), NULL);

	return  gdk_pixbuf_copy ( image->priv->pixbuf );
}


typedef union 
{
	guint8  ui8[4];
	guint32 ui32;
} pixel_union;

void 
gp_image_set_diff_pixmap ( GpImage *image, GdkPixmap* pixmap, guint x_offset, guint y_offset )
{
	GdkPixbuf *pixbuf;
	GdkPixbuf *m_pixbuf;
	guchar *pixels, *m_pixels;
	guchar *p, *m_p;
	gint w, h;
	gint n_channels, rowstride;

	g_return_if_fail ( GP_IS_IMAGE (image) );
	

	pixbuf		=   image->priv->pixbuf;
	if(!gdk_pixbuf_get_has_alpha ( pixbuf ) )
	{  /*add alpha*/
		GdkPixbuf *tmp ;
		tmp = gdk_pixbuf_add_alpha(pixbuf, FALSE, 0, 0, 0);
		g_object_unref(pixbuf);
		pixbuf = tmp;
	}
	m_pixbuf	=   gdk_pixbuf_copy ( pixbuf );
	
	w			=   gdk_pixbuf_get_width		( pixbuf );
	h			=   gdk_pixbuf_get_height		( pixbuf );

	gdk_pixbuf_get_from_drawable (  m_pixbuf, 
               						pixmap,
               						gdk_drawable_get_colormap (pixmap),
               						x_offset,y_offset,
               						0,0,
              						w,h);
	
	n_channels  =   gdk_pixbuf_get_n_channels   ( pixbuf );
	rowstride   =   gdk_pixbuf_get_rowstride	( pixbuf );
	pixels		=   gdk_pixbuf_get_pixels		( pixbuf );
	m_pixels	=   gdk_pixbuf_get_pixels		( m_pixbuf );
	while (h--) 
	{
		guint   i = w;
		p   = pixels;
		m_p = m_pixels;
		while (i--) 
		{
			pixel_union *pu, *m_pu;

			pu = (pixel_union *)p;
			m_pu = (pixel_union *)m_p;
				
			if(pu->ui32 == m_pu->ui32)
			{
				p[0] = 0; 
				p[1] = 0; 
				p[2] = 0; 
				p[3] = 0; 
			}
			p   += n_channels;
			m_p += n_channels;
		}
		pixels		+= rowstride;
		m_pixels	+= rowstride;
	}
	g_object_unref (m_pixbuf);

	
}


void		
gp_image_set_mask ( GpImage *image, GdkBitmap *mask )
{
	GdkPixbuf *pixbuf;
	GdkPixbuf *m_pixbuf;
	guchar *pixels, *m_pixels;
	guchar *p, *m_p;
	gint w, h;
	gint n_channels, rowstride;

	g_return_if_fail ( GP_IS_IMAGE (image) );
	

	pixbuf		=   image->priv->pixbuf;
	if(!gdk_pixbuf_get_has_alpha ( pixbuf ) )
	{  /*add alpha*/
		GdkPixbuf *tmp ;
		tmp = gdk_pixbuf_add_alpha(pixbuf, FALSE, 0, 0, 0);
		g_object_unref(pixbuf);
		pixbuf = tmp;
	}
	m_pixbuf	=   gdk_pixbuf_copy ( pixbuf );
	
	gdk_pixbuf_get_from_drawable (  m_pixbuf, 
               						mask,
               						gdk_drawable_get_colormap (mask),
               						0,0,
               						0,0,
              						-1,-1);
	n_channels  =   gdk_pixbuf_get_n_channels   ( pixbuf );
	rowstride   =   gdk_pixbuf_get_rowstride	( pixbuf );
	w			=   gdk_pixbuf_get_width		( pixbuf );
	h			=   gdk_pixbuf_get_height		( pixbuf );
	pixels		=   gdk_pixbuf_get_pixels		( pixbuf );
	m_pixels	=   gdk_pixbuf_get_pixels		( m_pixbuf );
	while (h--) 
	{
		guint   i = w;
		p   = pixels;
		m_p = m_pixels;
		while (i--) 
		{
			if(m_p[0] == 0)
			{
				p[0] = 0; 
				p[1] = 0; 
				p[2] = 0; 
				p[3] = 0; 
			}
			p   += n_channels;
			m_p += n_channels;
		}
		pixels		+= rowstride;
		m_pixels	+= rowstride;
	}
	g_object_unref (m_pixbuf);
}

void
gp_image_draw ( GpImage *image, 
                GdkDrawable *drawable,
                GdkGC *gc,
				gint x, gint y )
{
	gdk_draw_pixbuf	( drawable,
			          gc,
			     	  image->priv->pixbuf,
			          0, 0,
			          x, y,
			          -1, -1,
			          GDK_RGB_DITHER_NORMAL, 
		              0, 0);
}

gint
gp_image_get_width ( GpImage *image )
{
	return (gint)gdk_pixbuf_get_width (image->priv->pixbuf);
}

gint
gp_image_get_height ( GpImage *image )
{
	return (gint)gdk_pixbuf_get_height (image->priv->pixbuf);
}

gboolean
gp_image_get_has_alpha ( GpImage *image )
{
	return (gint)gdk_pixbuf_get_has_alpha (image->priv->pixbuf);
}

GdkBitmap *
gp_image_get_mask ( GpImage *image )
{
	GdkBitmap   *mask;
	gdk_pixbuf_render_pixmap_and_mask ( image->priv->pixbuf, 
	                                    NULL, &mask, 255 );
	return mask;
}


