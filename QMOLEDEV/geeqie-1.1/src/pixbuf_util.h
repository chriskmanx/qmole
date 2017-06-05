/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef PIXBUF_UTIL_H
#define PIXBUF_UTIL_H


gboolean pixbuf_to_file_as_png (GdkPixbuf *pixbuf, const gchar *filename);
gboolean pixbuf_to_file_as_jpg(GdkPixbuf *pixbuf, const gchar *filename, gint quality);

void pixbuf_inline_register_stock_icons(void);
gboolean register_theme_icon_as_stock(const gchar *key, const gchar *icon);

GdkPixbuf *pixbuf_inline(const gchar *key);
GdkPixbuf *pixbuf_fallback(FileData *fd, gint requested_width, gint requested_height);

gboolean pixbuf_scale_aspect(gint req_w, gint req_h, gint old_w, gint old_h, gint *new_w, gint *new_h);

#define PIXBUF_INLINE_FOLDER_CLOSED	"folder_closed"
#define PIXBUF_INLINE_FOLDER_LOCKED	"folder_locked"
#define PIXBUF_INLINE_FOLDER_OPEN	"folder_open"
#define PIXBUF_INLINE_FOLDER_UP		"folder_up"
#define PIXBUF_INLINE_SCROLLER		"scroller"
#define PIXBUF_INLINE_BROKEN		"broken"
#define PIXBUF_INLINE_ICON		"icon"
#define PIXBUF_INLINE_LOGO		"logo"

#define PIXBUF_INLINE_ICON_FLOAT	"icon_float"
#define PIXBUF_INLINE_ICON_THUMB	"icon_thumb"

#define PIXBUF_INLINE_ICON_BOOK		"icon_book"
#define PIXBUF_INLINE_ICON_CONFIG	"icon_config"
#define PIXBUF_INLINE_ICON_TOOLS	"icon_tools"
#define PIXBUF_INLINE_ICON_VIEW		"icon_view"


GdkPixbuf *pixbuf_copy_rotate_90(GdkPixbuf *src, gboolean counter_clockwise);
GdkPixbuf *pixbuf_copy_mirror(GdkPixbuf *src, gboolean mirror, gboolean flip);
GdkPixbuf* pixbuf_apply_orientation(GdkPixbuf *pixbuf, gint orientation);

void pixbuf_draw_rect_fill(GdkPixbuf *pb,
			   gint x, gint y, gint w, gint h,
			   gint r, gint g, gint b, gint a);

void pixbuf_draw_rect(GdkPixbuf *pb,
		      gint x, gint y, gint w, gint h,
		      gint r, gint g, gint b, gint a,
		      gint left, gint right, gint top, gint bottom);

void pixbuf_set_rect_fill(GdkPixbuf *pb,
			  gint x, gint y, gint w, gint h,
			  gint r, gint g, gint b, gint a);

void pixbuf_set_rect(GdkPixbuf *pb,
		     gint x, gint y, gint w, gint h,
		     gint r, gint g, gint b, gint a,
		     gint left, gint right, gint top, gint bottom);

void pixbuf_pixel_set(GdkPixbuf *pb, gint x, gint y, gint r, gint g, gint b, gint a);


void pixbuf_draw_layout(GdkPixbuf *pixbuf, PangoLayout *layout, GtkWidget *widget,
			gint x, gint y,
			guint8 r, guint8 g, guint8 b, guint8 a);


void pixbuf_draw_triangle(GdkPixbuf *pb,
			  gint clip_x, gint clip_y, gint clip_w, gint clip_h,
			  gint x1, gint y1, gint x2, gint y2, gint x3, gint y3,
			  guint8 r, guint8 g, guint8 b, guint8 a);

void pixbuf_draw_line(GdkPixbuf *pb,
		      gint clip_x, gint clip_y, gint clip_w, gint clip_h,
		      gint x1, gint y1, gint x2, gint y2,
		      guint8 r, guint8 g, guint8 b, guint8 a);

void pixbuf_draw_shadow(GdkPixbuf *pb,
			gint clip_x, gint clip_y, gint clip_w, gint clip_h,
			gint x, gint y, gint w, gint h, gint border,
			guint8 r, guint8 g, guint8 b, guint8 a);

void pixbuf_desaturate_rect(GdkPixbuf *pb,
			    gint x, gint y, gint w, gint h);


/* clipping utils */

gboolean util_clip_region(gint x, gint y, gint w, gint h,
		          gint clip_x, gint clip_y, gint clip_w, gint clip_h,
		          gint *rx, gint *ry, gint *rw, gint *rh);
void util_clip_triangle(gint x1, gint y1, gint x2, gint y2, gint x3, gint y3,
			gint *rx, gint *ry, gint *rw, gint *rh);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
