
#include "gcr-shooter.h"

#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <stdio.h>
#include <errno.h>
#include <sys/wait.h>
#include <unistd.h>
#include <X11/extensions/shape.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <locale.h>
#include <math.h>

#define MAXIMUM_WM_REPARENTING_DEPTH 4

#define SMALL_WIDTH  240
#define SMALL_HEIGHT 75
#define MEDIUM_WIDTH 240
#define MEDIUM_HEIGHT 165
#define LARGE_WIDTH 240
#define LARGE_HEIGHT 240

static Window
find_toplevel_window (Window xid)
{
	Window root, parent, *children;
	guint nchildren;

	do {
		if (XQueryTree (GDK_DISPLAY_XDISPLAY (gdk_display_get_default ()), xid, &root,
		                &parent, &children, &nchildren) == 0) {
			g_warning ("Couldn't find window manager window");
			return None;
		}

		if (root == parent)
			return xid;

		xid = parent;
	} while (TRUE);
}


static gboolean
adjust_size_callback (GcrShooterInfo *info)
{
	Window toplevel;
	Window root;
	GdkWindow *window;
	gint tx;
	gint ty;
	guint twidth;
	guint theight;
	guint tborder_width;
	guint tdepth;
	gint target_width = 0;
	gint target_height = 0;

	window = gtk_widget_get_window (info->window);
	toplevel = find_toplevel_window (GDK_WINDOW_XID (window));
	XGetGeometry (GDK_WINDOW_XDISPLAY (window), toplevel,
	              &root, &tx, &ty, &twidth, &theight, &tborder_width, &tdepth);

	switch (info->size) {
	case GCR_SHOOTER_SMALL:
		target_width = SMALL_WIDTH;
		target_height = SMALL_HEIGHT;
		break;
	case GCR_SHOOTER_MEDIUM:
		target_width = MEDIUM_WIDTH;
		target_height = MEDIUM_HEIGHT;
		break;
	case GCR_SHOOTER_LARGE:
		target_width = LARGE_WIDTH;
		target_height = LARGE_HEIGHT;
		break;
	case GCR_SHOOTER_ASIS:
		target_width = twidth;
		target_height = theight;
		break;
	}

	if (twidth > target_width || theight > target_height) {
		gtk_widget_set_size_request (info->window,
		                             2 + target_width - (twidth - target_width), /* Dunno why I need the +2 fudge factor; */
		                             2 + target_height - (theight - target_height));
	}
	return FALSE;
}

static void
realize_callback (GtkWidget  *widget, GcrShooterInfo *info)
{
	gdk_threads_add_timeout (500, (GSourceFunc)adjust_size_callback, info);
}

GcrShooterInfo*
gcr_shooter_info_new (const gchar *name, GtkWidget *widget, GcrShooterSize size)
{
	GcrShooterInfo *info;

	info = g_new0 (GcrShooterInfo, 1);
	info->name = g_strdup (name);
	info->size = size;
	if (GTK_IS_WINDOW (widget)) {
		info->window = widget;
		gtk_window_set_resizable (GTK_WINDOW (info->window), FALSE);
		info->include_decorations = TRUE;
		g_signal_connect (info->window, "realize", G_CALLBACK (realize_callback), info);
	} else {
		info->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
		gtk_window_set_has_resize_grip (GTK_WINDOW (info->window), FALSE);
		info->include_decorations = FALSE;
		gtk_widget_show_all (widget);
		gtk_container_add (GTK_CONTAINER (info->window), widget);
	}
	info->no_focus = TRUE;

	gtk_widget_set_app_paintable (info->window, TRUE);
	g_signal_connect (info->window, "focus", G_CALLBACK (gtk_true), NULL);
	gtk_container_set_border_width (GTK_CONTAINER (info->window), 12);

	switch (size) {
	case GCR_SHOOTER_SMALL:
		gtk_widget_set_size_request (info->window, 240, 75);
		break;
	case GCR_SHOOTER_MEDIUM:
		gtk_widget_set_size_request (info->window, 240, 165);
		break;
	case GCR_SHOOTER_LARGE:
		gtk_widget_set_size_request (info->window, 240, 240);
		break;
	default:
		break;
	}

	return info;
}

#define BLUR_RADIUS 5
#define SHADOW_OFFSET (BLUR_RADIUS * 4 / 5)
#define SHADOW_OPACITY 0.75

typedef struct {
	int size;
	double *data;
} ConvFilter;

static double
gaussian (double x, double y, double r)
{
	return ((1 / (2 * M_PI * r)) *
	        exp ((- (x * x + y * y)) / (2 * r * r)));
}

static ConvFilter *
create_blur_filter (int radius)
{
	ConvFilter *filter;
	int x, y;
	double sum;

	filter = g_new0 (ConvFilter, 1);
	filter->size = radius * 2 + 1;
	filter->data = g_new (double, filter->size * filter->size);

	sum = 0.0;

	for (y = 0 ; y < filter->size; y++) {
		for (x = 0 ; x < filter->size; x++) {
			sum += filter->data[y * filter->size + x] = gaussian (x - (filter->size >> 1),
			       y - (filter->size >> 1),
			       radius);
		}
	}

	for (y = 0; y < filter->size; y++) {
		for (x = 0; x < filter->size; x++) {
			filter->data[y * filter->size + x] /= sum;
		}
	}

	return filter;

}

static GdkPixbuf *
create_shadow (GdkPixbuf *src)
{
	int x, y, i, j;
	int width, height;
	GdkPixbuf *dest;
	static ConvFilter *filter = NULL;
	int src_rowstride, dest_rowstride;
	int src_bpp, dest_bpp;
	guchar *src_pixels, *dest_pixels;

	if (!filter)
		filter = create_blur_filter (BLUR_RADIUS);

	width = gdk_pixbuf_get_width (src) + BLUR_RADIUS * 2 + SHADOW_OFFSET;
	height = gdk_pixbuf_get_height (src) + BLUR_RADIUS * 2 + SHADOW_OFFSET;

	dest = gdk_pixbuf_new (gdk_pixbuf_get_colorspace (src),
	                       gdk_pixbuf_get_has_alpha (src),
	                       gdk_pixbuf_get_bits_per_sample (src),
	                       width, height);
	gdk_pixbuf_fill (dest, 0);
	src_pixels = gdk_pixbuf_get_pixels (src);
	src_rowstride = gdk_pixbuf_get_rowstride (src);
	src_bpp = gdk_pixbuf_get_has_alpha (src) ? 4 : 3;

	dest_pixels = gdk_pixbuf_get_pixels (dest);
	dest_rowstride = gdk_pixbuf_get_rowstride (dest);
	dest_bpp = gdk_pixbuf_get_has_alpha (dest) ? 4 : 3;

	for (y = 0; y < height; y++) {
		for (x = 0; x < width; x++) {
			int sumr = 0, sumg = 0, sumb = 0, suma = 0;

			for (i = 0; i < filter->size; i++) {
				for (j = 0; j < filter->size; j++) {
					int src_x, src_y;

					src_y = -(BLUR_RADIUS + SHADOW_OFFSET) + y - (filter->size >> 1) + i;
					src_x = -(BLUR_RADIUS + SHADOW_OFFSET) + x - (filter->size >> 1) + j;

					if (src_y < 0 || src_y > gdk_pixbuf_get_height (src) ||
					    src_x < 0 || src_x > gdk_pixbuf_get_width (src))
						continue;

					sumr += src_pixels [src_y * src_rowstride +
					                    src_x * src_bpp + 0] *
					                    filter->data [i * filter->size + j];
					sumg += src_pixels [src_y * src_rowstride +
					                    src_x * src_bpp + 1] *
					                    filter->data [i * filter->size + j];

					sumb += src_pixels [src_y * src_rowstride +
					                    src_x * src_bpp + 2] *
					                    filter->data [i * filter->size + j];

					if (src_bpp == 4)
						suma += src_pixels [src_y * src_rowstride +
						                    src_x * src_bpp + 3] *
						                    filter->data [i * filter->size + j];
				}
			}

			if (dest_bpp == 4)
				dest_pixels [y * dest_rowstride +
				             x * dest_bpp + 3] = suma * SHADOW_OPACITY;
		}
	}

	return dest;
}

GdkPixbuf *
create_shadowed_pixbuf (GdkPixbuf *src)
{
	GdkPixbuf *dest;

	dest = create_shadow (src);

	gdk_pixbuf_composite (src, dest,
	                      BLUR_RADIUS, BLUR_RADIUS,
	                      gdk_pixbuf_get_width (src),
	                      gdk_pixbuf_get_height (src),
	                      BLUR_RADIUS, BLUR_RADIUS, 1.0, 1.0,
	                      GDK_INTERP_NEAREST, 255);
	return dest;
}
static GdkPixbuf *
add_border_to_shot (GdkPixbuf *pixbuf)
{
	GdkPixbuf *retval;

	retval = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8,
	                         gdk_pixbuf_get_width (pixbuf) + 2,
	                         gdk_pixbuf_get_height (pixbuf) + 2);

	/* Fill with solid black */
	gdk_pixbuf_fill (retval, 0xFF);
	gdk_pixbuf_copy_area (pixbuf,
	                      0, 0,
	                      gdk_pixbuf_get_width (pixbuf),
	                      gdk_pixbuf_get_height (pixbuf),
	                      retval, 1, 1);

	return retval;
}

static GdkPixbuf *
remove_shaped_area (GdkPixbuf *pixbuf, Window window)
{
	GdkPixbuf *retval;
	XRectangle *rectangles;
	int rectangle_count, rectangle_order;
	int i;

	retval = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8,
	                         gdk_pixbuf_get_width (pixbuf),
	                         gdk_pixbuf_get_height (pixbuf));

	gdk_pixbuf_fill (retval, 0);
	rectangles = XShapeGetRectangles (GDK_DISPLAY_XDISPLAY (gdk_display_get_default ()), window,
	                                  ShapeBounding, &rectangle_count, &rectangle_order);

	for (i = 0; i < rectangle_count; i++) {
		int y, x;

		for (y = rectangles[i].y; y < rectangles[i].y + rectangles[i].height; y++) {
			guchar *src_pixels, *dest_pixels;

			src_pixels = gdk_pixbuf_get_pixels (pixbuf) +
			             y * gdk_pixbuf_get_rowstride (pixbuf) +
			             rectangles[i].x * (gdk_pixbuf_get_has_alpha (pixbuf) ? 4 : 3);
			dest_pixels = gdk_pixbuf_get_pixels (retval) +
			              y * gdk_pixbuf_get_rowstride (retval) +
			              rectangles[i].x * 4;

			for (x = rectangles[i].x; x < rectangles[i].x + rectangles[i].width; x++) {
				*dest_pixels++ = *src_pixels ++;
				*dest_pixels++ = *src_pixels ++;
				*dest_pixels++ = *src_pixels ++;
				*dest_pixels++ = 255;

				if (gdk_pixbuf_get_has_alpha (pixbuf))
					src_pixels++;
			}
		}
	}

	return retval;
}

static GdkPixbuf *
take_window_shot (Window child, gboolean include_decoration)
{
	GdkWindow *window;
	Window xid;
	gint x_orig, y_orig;
	gint x = 0, y = 0;
	gint width, height;

	GdkPixbuf *tmp, *tmp2;
	GdkPixbuf *retval;

	if (include_decoration)
		xid = find_toplevel_window (child);
	else
		xid = child;

	window = gdk_x11_window_foreign_new_for_display (gdk_display_get_default (), xid);

	width = gdk_window_get_width (window);
	height = gdk_window_get_height (window);
	gdk_window_get_origin (window, &x_orig, &y_orig);

	if (x_orig < 0) {
		x = - x_orig;
		width = width + x_orig;
		x_orig = 0;
	}

	if (y_orig < 0) {
		y = - y_orig;
		height = height + y_orig;
		y_orig = 0;
	}

	if (x_orig + width > gdk_screen_width ())
		width = gdk_screen_width () - x_orig;

	if (y_orig + height > gdk_screen_height ())
		height = gdk_screen_height () - y_orig;

	tmp = gdk_pixbuf_get_from_window (window, x, y, width, height);

	if (include_decoration)
		tmp2 = remove_shaped_area (tmp, xid);
	else
		tmp2 = add_border_to_shot (tmp);

	retval = create_shadowed_pixbuf (tmp2);
	g_object_unref (tmp);
	g_object_unref (tmp2);

	return retval;
}

int
main (int argc, char **argv)
{
	GdkPixbuf *screenshot = NULL;
	GError *error = NULL;
	GtkAllocation allocation;
	GcrShooterInfo *info;
	GdkWindow *window;
	const gchar *outdir;
	XID id;
	char *filename;

	/*
	 * If there's no DISPLAY, we silently error out.  We don't want to break
	 * headless builds.
	 */

	if (!gtk_init_check (&argc, &argv))
		return 0;

	if (argc < 2 || !argv[1]) {
		g_printerr ("specify name of widget\n");
		return 2;
	}

	info = gcr_widgets_create (argv[1]);
	if (info == NULL) {
		g_printerr ("no such widget: %s\n", argv[1]);
		return 1;
	}

	gtk_widget_show (info->window);
	window = gtk_widget_get_window (info->window);
	gtk_widget_get_allocation (info->window, &allocation);

	gtk_widget_show_now (info->window);
	gtk_widget_queue_draw_area (info->window,
	                            allocation.x, allocation.y,
	                            allocation.width, allocation.height);
	gdk_window_process_updates (window, TRUE);

	while (gtk_events_pending ())
		gtk_main_iteration ();
	sleep (1);

	while (gtk_events_pending ())
		gtk_main_iteration ();

	id = gdk_x11_window_get_xid (window);
	screenshot = take_window_shot (id, info->include_decorations);

	outdir = (argc >= 3) ? argv[2] : ".";
	filename = g_strdup_printf ("%s/%s.png", outdir, info->name);
	if (!gdk_pixbuf_save (screenshot, filename, "png", &error, NULL)) {
		g_printerr ("couldn't write screenshot: %s: %s\n", filename, error->message);
		g_clear_error (&error);
		return 1;
	}
	g_free(filename);
	gtk_widget_hide (info->window);

	return 0;
}
