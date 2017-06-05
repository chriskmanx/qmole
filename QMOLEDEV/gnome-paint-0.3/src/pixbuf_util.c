/***************************************************************************
    Copyright (C) Rogério Ferro do Nascimento 2010 <rogerioferro@gmail.com>
    Contributed by Juan Balderas

    This file is part of gnome-paint.

    gnome-paint is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    gnome-paint is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with gnome-paint.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************/


#include <gtk/gtk.h>
#include "pixbuf_util.h"

struct fillinfo
{
   unsigned char *rgb; 
   int rowstride;
   int pixelsize; 
   int width;
   int height;
   unsigned char or, og, ob, r, g, b;
};


static void setpixel(guchar *pixels, gint rowstride, gint n_channels, gint x, gint y, guint color);
static gint getpixel(guchar *pixels, gint rowstride, gint n_channels, gint x, gint y);
static gint fill_shape(GdkPixbuf *pixbuf, guint x, guint y, guint nc);

static void
flood_fill_algo(struct fillinfo *info, int x, int y);


void fill_draw(GdkDrawable *drawable, GdkGC *gc, guint fill_color, guint x, guint y)
{
	GdkPixbuf *pixbuf;
	gint width, height;
	struct fillinfo fillinfo;
	guchar *p;
	
	printf("fill_draw fill_color: %.08X\n", fill_color);
	printf("fill_draw x: %d, y: %d\n", x, y);
	
	gdk_drawable_get_size(drawable, &width, &height);
	printf("fill_draw w: %d, h: %d\n", width, height);
	pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width, height);

	gdk_pixbuf_fill(pixbuf, 0);

	/* 1 get pixbuf from drawable */
	gdk_pixbuf_get_from_drawable(pixbuf, drawable, NULL, 0, 0, 0, 0, width, height);
	/* 2 draw on pixbuf */
	//fill_shape(pixbuf, x, y, fill_color);
	fillinfo.rgb = gdk_pixbuf_get_pixels (pixbuf);
    fillinfo.width = gdk_pixbuf_get_width (pixbuf);
    fillinfo.height = gdk_pixbuf_get_height (pixbuf);
    fillinfo.rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    fillinfo.pixelsize = gdk_pixbuf_get_n_channels (pixbuf);
    fillinfo.r = getr(fill_color);
    fillinfo.g = getg(fill_color);
    fillinfo.b = getb(fill_color);
    p = fillinfo.rgb + y * fillinfo.rowstride + x * fillinfo.pixelsize;
    fillinfo.or = *p;
    fillinfo.og = *(p + 1);
    fillinfo.ob = *(p + 2);
    flood_fill_algo(&fillinfo, x, y);

	/* 3 draw pixbuf back onto drawable. Notice xdest & ydest */
	gdk_draw_pixbuf(drawable, gc, pixbuf, 0, 0, 0, 0, gdk_pixbuf_get_width(pixbuf),
					gdk_pixbuf_get_height(pixbuf), GDK_RGB_DITHER_NONE, 0, 0);
	
	/* clean up */
	g_object_unref(pixbuf);
}

/* Copied from gpaint */
/* Get a pixel's value at (x,y)
 * For pixbufs with alpha channel only.
 * Sets 'color' as rgb value
 */
gboolean get_pixel_from_pixbuf(GdkPixbuf *pixbuf, guint *color, guint x, guint y)
{
	int width, height, rowstride, n_channels;
	guchar *pixels;

	if(!GDK_IS_PIXBUF(pixbuf)){
		printf("get_pixel_from_pixbuf: !pixbuf\n");
		return 0;
	}

	n_channels = gdk_pixbuf_get_n_channels (pixbuf);
	width = gdk_pixbuf_get_width (pixbuf);
	height = gdk_pixbuf_get_height (pixbuf);
	rowstride = gdk_pixbuf_get_rowstride (pixbuf);
	pixels = gdk_pixbuf_get_pixels (pixbuf);
	
	if (gdk_pixbuf_get_colorspace (pixbuf) != GDK_COLORSPACE_RGB){
		printf("get_pixel_from_pixbuf: color space  != GDK_COLORSPACE_RGB\n");
		return FALSE;
	}
	if (gdk_pixbuf_get_bits_per_sample (pixbuf) != 8){
		printf("get_pixel_from_pixbuf: bits per sample != 8\n");
		return FALSE;
	}
	if (!gdk_pixbuf_get_has_alpha (pixbuf)){
		printf("get_pixel_from_pixbuf: no alpha\n");
		return FALSE;
	}
	if (n_channels != 4){
		printf("get_pixel_from_pixbuf: n_channels != 4\n");
		return FALSE;
	}
	if (NULL == color){
		printf("get_pixel_from_pixbuf: NULL == color\n");
		return FALSE;
	}

	*color = getpixel(pixels, rowstride, n_channels, x, y);
	printf("get_pixel_from_pixbuf color: 0x%.08x %s %d\n", *color, __FILE__, __LINE__);

	return TRUE;
}


void set_pixel_in_pixbuf(GdkPixbuf *pixbuf, guint color, guint x, guint y)
{
	int width, height, rowstride, n_channels;
	guchar *pixels;

	if(!GDK_IS_PIXBUF(pixbuf)){
		printf("get_pixel_from_pixbuf: !pixbuf\n");
		return ;
	}

	n_channels = gdk_pixbuf_get_n_channels (pixbuf);
	width = gdk_pixbuf_get_width (pixbuf);
	height = gdk_pixbuf_get_height (pixbuf);
	rowstride = gdk_pixbuf_get_rowstride (pixbuf);
	pixels = gdk_pixbuf_get_pixels (pixbuf);
	
	if (gdk_pixbuf_get_colorspace (pixbuf) != GDK_COLORSPACE_RGB){
		printf("set_pixel_from_pixbuf: color space  != GDK_COLORSPACE_RGB\n");
		return ;
	}
	if (gdk_pixbuf_get_bits_per_sample (pixbuf) != 8){
		printf("set_pixel_from_pixbuf: bits per sample != 8\n");
		return ;
	}
	if (!gdk_pixbuf_get_has_alpha (pixbuf)){
		printf("set_pixel_from_pixbuf: no alpha\n");
		return ;
	}
	if (n_channels != 4){
		printf("set_pixel_from_pixbuf: n_channels != 4\n");
		return ;
	}
	
	setpixel(pixels, rowstride, n_channels, x, y, color);
}


static void
setpixel(guchar *pixels, gint rowstride, gint n_channels, gint x, gint y, guint color)
{
	guchar *p;
	
	//printf("pp %.08X ", color);

	p = pixels + (y * rowstride + x * n_channels);
	p[0] = getr(color);/*red*/
	p[1] = getg(color);/*green*/
	p[2] = getb(color);/*blue*/
	p[3] = 0xFF; /* alpha */

}



static gint
getpixel(guchar *pixels, gint rowstride, gint n_channels, gint x, gint y)
{
	guchar *p;
	gint color = 0;
	FILE *fp;
	
	p = pixels + (y * rowstride + x * n_channels);

	fp = fopen("flood.txt", "a");
	fprintf(fp, "x: %d, y: %d rs: %d, nc: %d, p: %p\n", x, y, rowstride, n_channels, p);
	fclose(fp);
	color = col(p[0], p[1], p[2]);

	return color;
}


struct fillpixelinfo
{
   int y, xl, xr, dy;
};

#define STACKSIZE 10000

#define PUSH(py, pxl, pxr, pdy) \
{ \
    struct fillpixelinfo *p = sp;\
    if (((py) + (pdy) >= 0) && ((py) + (pdy) < info->height))\
    {\
        p->y = (py);\
        p->xl = (pxl);\
        p->xr = (pxr);\
        p->dy = (pdy);\
        sp++; \
    }\
}
   
#define POP(py, pxl, pxr, pdy) \
{\
    sp--;\
    (py) = sp->y + sp->dy;\
    (pxl) = sp->xl;\
    (pxr) = sp->xr;\
    (pdy) = sp->dy;\
}

static __inline__ int
is_old_pixel_value(struct fillinfo *info, int x, int y)
{
    unsigned char *p = info->rgb + y * info->rowstride + x * info->pixelsize;
    unsigned char or, og, ob;
    or = *p;
    og = *(p + 1);
    ob = *(p + 2);
    if ((or == info->or) && (og == info->og) && (ob == info->ob))
    {
        return 1;
    }
    return 0;
}


static __inline__ void
set_new_pixel_value(struct fillinfo *info, int x, int y)
{
    unsigned char *p = info->rgb + y * info->rowstride + x * info->pixelsize;
    *p = info->r;
    *(p + 1) = info->g;
    *(p + 2) = info->b;
}

/*
 * algorithm based on SeedFill.c from GraphicsGems 1
 */
static void
flood_fill_algo(struct fillinfo *info, int x, int y)
{
    /* TODO: check for stack overflow? */
    /* TODO: that's a lot of memory! esp if we never use it */
    struct fillpixelinfo stack[STACKSIZE];
    struct fillpixelinfo * sp = stack;
    int l, x1, x2, dy;
      
    if ((x >= 0) && (x < info->width) && (y >= 0) && (y < info->height))
    {
        if ((info->or == info->r) && (info->og == info->g) && (info->ob == info->b))
        {
            return;
        }
        PUSH(y, x, x, 1);
        PUSH(y + 1, x, x, -1);
        while (sp > stack)  
        {
            POP(y, x1, x2, dy);
            for (x = x1; (x >= 0) && is_old_pixel_value(info, x, y); x--)
            { 
                set_new_pixel_value(info, x, y);
            }
            if (x >= x1)
            {
                goto skip;
            }
            l = x + 1;
            if (l < x1)
            {
                PUSH(y, l, x1 - 1, -dy);
            }
            x = x1 + 1;
            do
            {
                for (; (x < info->width) && is_old_pixel_value(info, x, y); x++)
                {
                    set_new_pixel_value(info, x, y);
                }
                PUSH(y, l, x - 1, dy);
                if (x > x2 + 1)
                {
                    PUSH(y, x2 + 1, x - 1, -dy);
                }
skip:
                for (x++; x <= x2 && !is_old_pixel_value(info, x, y); x++) 
                {
                    /* empty */ ;
                }
                l = x;
            } while (x <= x2);
        }
    }
}  

