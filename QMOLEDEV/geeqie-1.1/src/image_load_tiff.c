/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2011 The Geeqie Team
 *
 * Author: Vladimir Nadvornik
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

/* based on code from GdkPixbuf library - TIFF image loader
 *
 * Copyright (C) 1999 Mark Crichton
 * Copyright (C) 1999 The Free Software Foundation
 *
 * Authors: Mark Crichton <crichton@gimp.org>
 *          Federico Mena-Quintero <federico@gimp.org>
 *          Jonathan Blandford <jrb@redhat.com>
 *          S�ren Sandmann <sandmann@daimi.au.dk>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "main.h"

#include "image-load.h"
#include "image_load_tiff.h"

#ifdef HAVE_TIFF

#include <tiffio.h>

typedef struct _ImageLoaderTiff ImageLoaderTiff;
struct _ImageLoaderTiff {
	ImageLoaderBackendCbAreaUpdated area_updated_cb;
	ImageLoaderBackendCbSize size_cb;
	ImageLoaderBackendCbAreaPrepared area_prepared_cb;
	
	gpointer data;

	GdkPixbuf *pixbuf;
	guint requested_width;
	guint requested_height;
	
	gboolean abort;

	const guchar *buffer;
	toff_t used;
	toff_t pos;
};

static void free_buffer (guchar *pixels, gpointer data)
{
	g_free (pixels);
}

static tsize_t
tiff_load_read (thandle_t handle, tdata_t buf, tsize_t size)
{
        ImageLoaderTiff *context = (ImageLoaderTiff *)handle;
        
        if (context->pos + size > context->used)
                return 0;
        
        memcpy (buf, context->buffer + context->pos, size);
        context->pos += size;
        return size;
}

static tsize_t
tiff_load_write (thandle_t handle, tdata_t buf, tsize_t size)
{
        return -1;
}

static toff_t
tiff_load_seek (thandle_t handle, toff_t offset, int whence)
{
        ImageLoaderTiff *context = (ImageLoaderTiff *)handle;
        
        switch (whence) {
        case SEEK_SET:
                if (offset > context->used)
                        return -1;
                context->pos = offset;
                break;
        case SEEK_CUR:
                if (offset + context->pos >= context->used)
                        return -1;
                context->pos += offset;
                break;
        case SEEK_END:
                if (offset + context->used > context->used)
                        return -1;
                context->pos = context->used + offset;
                break;
        default:
                return -1;
        }
        return context->pos;
}

static int
tiff_load_close (thandle_t context)
{
        return 0;
}

static toff_t
tiff_load_size (thandle_t handle)
{
        ImageLoaderTiff *context = (ImageLoaderTiff *)handle;
        
        return context->used;
}

static int
tiff_load_map_file (thandle_t handle, tdata_t *buf, toff_t *size)
{
        ImageLoaderTiff *context = (ImageLoaderTiff *)handle;
        
        *buf = (tdata_t *) context->buffer;
        *size = context->used;
        
        return 0;
}

static void
tiff_load_unmap_file (thandle_t handle, tdata_t data, toff_t offset)
{
}

static gboolean image_loader_tiff_load (gpointer loader, const guchar *buf, gsize count, GError **error)
{
	ImageLoaderTiff *lt = (ImageLoaderTiff *) loader;

        TIFF *tiff;
	guchar *pixels = NULL;
	gint width, height, rowstride, bytes;
	uint16 orientation = 0;
	uint16 transform = 0;
	uint32 rowsperstrip;

	lt->buffer = buf;
	lt->used = count;
	lt->pos = 0;

	TIFFSetWarningHandler(NULL);

        tiff = TIFFClientOpen ("libtiff-geeqie", "r", lt, 
                               tiff_load_read, tiff_load_write, 
                               tiff_load_seek, tiff_load_close, 
                               tiff_load_size, 
                               tiff_load_map_file, tiff_load_unmap_file);
        if (!tiff) 
        	{
        	DEBUG_1("Failed to open TIFF image");
        	return FALSE;
        	}



	if (!TIFFGetField (tiff, TIFFTAG_IMAGEWIDTH, &width)) {
		DEBUG_1("Could not get image width (bad TIFF file)");
		TIFFClose(tiff);
                return FALSE;
        }
        
        if (!TIFFGetField (tiff, TIFFTAG_IMAGELENGTH, &height)) {
        	DEBUG_1("Could not get image height (bad TIFF file)");
		TIFFClose(tiff);
                return FALSE;
        }

        if (width <= 0 || height <= 0) {
        	DEBUG_1("Width or height of TIFF image is zero");
		TIFFClose(tiff);
                return FALSE;                
        }
        
        rowstride = width * 4;
        if (rowstride / 4 != width) { /* overflow */
        	DEBUG_1("Dimensions of TIFF image too large");
		TIFFClose(tiff);
                return FALSE;                
        }
        
        bytes = height * rowstride;
        if (bytes / rowstride != height) { /* overflow */
        	DEBUG_1("Dimensions of TIFF image too large");
		TIFFClose(tiff);
                return FALSE;                
        }

	lt->requested_width = width;
	lt->requested_height = height;
	lt->size_cb(loader, lt->requested_width, lt->requested_height, lt->data);

        pixels = g_try_malloc (bytes);

        if (!pixels) {
        	DEBUG_1("Insufficient memory to open TIFF file");
		TIFFClose(tiff);
                return FALSE;
        }

	lt->pixbuf = gdk_pixbuf_new_from_data (pixels, GDK_COLORSPACE_RGB, TRUE, 8, 
                                           width, height, rowstride,
                                           free_buffer, NULL);
        if (!lt->pixbuf) {
                g_free (pixels);
                DEBUG_1("Insufficient memory to open TIFF file");
		TIFFClose(tiff);
                return FALSE;
        }

	/* Set the "orientation" key associated with this image. libtiff 
	   orientation handling is odd, so further processing is required
	   by higher-level functions based on this tag. If the embedded
	   orientation tag is 1-4, libtiff flips/mirrors the image as
	   required, and no client processing is required - so we report 
	   no orientation. Orientations 5-8 require rotations which would 
	   swap the width and height of the image. libtiff does not do this. 
	   Instead it interprets orientations 5-8 the same as 1-4. 
	   See http://bugzilla.remotesensing.org/show_bug.cgi?id=1548.
	   To correct for this, the client must apply the transform normally
	   used for orientation 5 to both orientations 5 and 7, and apply
	   the transform normally used for orientation 7 for both
	   orientations 6 and 8. Then everythings works out OK! */
	
	TIFFGetField (tiff, TIFFTAG_ORIENTATION, &orientation);

	switch (orientation) {
		case 5:
		case 7:
			transform = 5;
			break;
		case 6:
		case 8:
			transform = 7;
			break;
		default:
			transform = 0;
			break;
	}


	lt->area_prepared_cb(loader, lt->data);



	if( TIFFGetField(tiff, TIFFTAG_ROWSPERSTRIP, &rowsperstrip) ) 
		{
		/* read by strip */
		int row;
		guchar *wrk_line = (guchar *)g_malloc(width * sizeof (uint32));


		for( row = 0; row < height; row += rowsperstrip )
			{
			int rows_to_write, i_row;
			
			if (lt->abort) {
				break;
			}

			/* Read the strip into an RGBA array */
			if (!TIFFReadRGBAStrip(tiff, row, (uint32 *)(pixels + row * rowstride))) {
				break;
			}

			/*
			 * Figure out the number of scanlines actually in this strip.
			*/
			if( row + (int)rowsperstrip > height )
				rows_to_write = height - row;
			else
				rows_to_write = rowsperstrip;



			/*
			 * For some reason the TIFFReadRGBAStrip() function chooses the
			 * lower left corner as the origin.  Vertically mirror scanlines.
			 */
			for( i_row = 0; i_row < rows_to_write / 2; i_row++ )
				{
				guchar *top_line, *bottom_line;

				top_line = pixels + (row + i_row) * rowstride;
				bottom_line = pixels + (row + rows_to_write - i_row - 1) * rowstride; 

				memcpy(wrk_line, top_line, 4*width);
				memcpy(top_line, bottom_line, 4*width);
				memcpy(bottom_line, wrk_line, 4*width);
        		}
			lt->area_updated_cb(loader, 0, row, width, rows_to_write, lt->data);
			}
		g_free(wrk_line);
		}
	else
		{
		/* fallback, tiled tiff */
		if (!TIFFReadRGBAImageOriented (tiff, width, height, (uint32 *)pixels, ORIENTATION_TOPLEFT, 1)) 
			{
			TIFFClose(tiff);
			return FALSE;
			}

#if G_BYTE_ORDER == G_BIG_ENDIAN
		/* Turns out that the packing used by TIFFRGBAImage depends on 
        	 * the host byte order... 
	         */ 
		while (pixels < pixbuf->pixels + bytes) 
			{
			uint32 pixel = *(uint32 *)pixels;
			int r = TIFFGetR(pixel);
			int g = TIFFGetG(pixel);
			int b = TIFFGetB(pixel);
			int a = TIFFGetA(pixel);
			*pixels++ = r;
			*pixels++ = g;
			*pixels++ = b;
			*pixels++ = a;
			}
#endif

		lt->area_updated_cb(loader, 0, 0, width, height, lt->data);
		}
	TIFFClose(tiff);

	return TRUE;
}


static gpointer image_loader_tiff_new(ImageLoaderBackendCbAreaUpdated area_updated_cb, ImageLoaderBackendCbSize size_cb, ImageLoaderBackendCbAreaPrepared area_prepared_cb, gpointer data)
{
        ImageLoaderTiff *loader = g_new0(ImageLoaderTiff, 1);
        
	loader->area_updated_cb = area_updated_cb;
	loader->size_cb = size_cb;
	loader->area_prepared_cb = area_prepared_cb;
	loader->data = data;
	return (gpointer) loader;
}


static void image_loader_tiff_set_size(gpointer loader, int width, int height)
{
	ImageLoaderTiff *lt = (ImageLoaderTiff *) loader;
	lt->requested_width = width;
	lt->requested_height = height;
}

static GdkPixbuf* image_loader_tiff_get_pixbuf(gpointer loader)
{
	ImageLoaderTiff *lt = (ImageLoaderTiff *) loader;
	return lt->pixbuf;
}

static gchar* image_loader_tiff_get_format_name(gpointer loader)
{
	return g_strdup("tiff");
}
static gchar** image_loader_tiff_get_format_mime_types(gpointer loader)
{
	static gchar *mime[] = {"image/tiff", NULL};
	return g_strdupv(mime);
}

static gboolean image_loader_tiff_close(gpointer loader, GError **error)
{
	return TRUE;
}

static void image_loader_tiff_abort(gpointer loader)
{
	ImageLoaderTiff *lt = (ImageLoaderTiff *) loader;
	lt->abort = TRUE;
}

static void image_loader_tiff_free(gpointer loader)
{
	ImageLoaderTiff *lt = (ImageLoaderTiff *) loader;
	if (lt->pixbuf) g_object_unref(lt->pixbuf);
	g_free(lt);
}


void image_loader_backend_set_tiff(ImageLoaderBackend *funcs)
{
	funcs->loader_new = image_loader_tiff_new;
	funcs->set_size = image_loader_tiff_set_size;
	funcs->load = image_loader_tiff_load;
	funcs->write = NULL;
	funcs->get_pixbuf = image_loader_tiff_get_pixbuf;
	funcs->close = image_loader_tiff_close;
	funcs->abort = image_loader_tiff_abort;
	funcs->free = image_loader_tiff_free;
	
	funcs->get_format_name = image_loader_tiff_get_format_name;
	funcs->get_format_mime_types = image_loader_tiff_get_format_mime_types;
}



#endif