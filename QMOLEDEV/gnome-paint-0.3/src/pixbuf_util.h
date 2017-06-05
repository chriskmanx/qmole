#ifndef _PIXBUF_UTIL_H_
#define _PIXBUF_UTIL_H_


#define col(r, g ,b)  ((unsigned int) (((unsigned char) (b) | \
					  ((unsigned short) (g) << 8)) | \
					  (((unsigned int) (unsigned char) (r)) << 16)))

#define col_rgba(r,g,b,a) ( (((guint)(guchar)(a))<<24) | col(r,g,b) )

#define getr(x) (((x >> 16) & 0x0FF))
#define getg(x) (((x >> 8) & 0x0FF))
#define getb(x) ((x & 0x0FF))

void fill_draw(GdkDrawable *drawable, GdkGC *gc, guint fill_color, guint x, guint y);
gboolean get_pixel_from_pixbuf(GdkPixbuf *pixbuf, guint *color, guint x, guint y);


#endif
