/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* tasklist.c - code for tracking windows
 *
 * Loosly based on code in GNOME's libwnck.
 */

#include "config.h"

#include <stdlib.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>

#include "global.h"

#include "tasklist.h"
#include "wrapped.h"
#include "options.h"
#include "gui_support.h"
#include "main.h"
#include "pinboard.h"
#include "pixmaps.h"
#include "support.h"

/* There is one of these for each window controlled by the window
 * manager (all tasks) in the _NET_CLIENT_LIST property.
 */
typedef struct _IconWindow IconWindow;

struct _IconWindow {
	GtkWidget *widget;	/* Widget used for icon when iconified */
	GtkWidget *label;
	gchar *text;
	Window xwindow;
	gboolean iconified;
	gint timeout_update;	/* Non-zero => timeout callback in use */
};

/* If TRUE, only iconfied windows with _NET_WM_STATE_HIDDEN are really icons */
static gboolean wm_supports_hidden = FALSE;

static GdkAtom xa__NET_SUPPORTED = GDK_NONE;
static GdkAtom xa_WM_STATE = GDK_NONE;
static GdkAtom xa_WM_NAME = GDK_NONE;
static GdkAtom xa_WM_ICON_NAME = GDK_NONE;
static GdkAtom xa_UTF8_STRING = GDK_NONE;
static GdkAtom xa_TEXT = GDK_NONE;
static GdkAtom xa__NET_WM_VISIBLE_NAME = GDK_NONE;
static GdkAtom xa__NET_WM_ICON_NAME = GDK_NONE;
static GdkAtom xa__NET_CLIENT_LIST = GDK_NONE;
static GdkAtom xa__NET_WM_ICON_GEOMETRY = GDK_NONE;
static GdkAtom xa__NET_WM_STATE = GDK_NONE;
static GdkAtom xa__NET_WM_STATE_HIDDEN = GDK_NONE;
static GdkAtom xa__NET_DESKTOP_GEOMETRY = GDK_NONE;
static GdkAtom xa__NET_DESKTOP_VIEWPORT = GDK_NONE;

/* We have selected destroy and property events on every window in
 * this table.
 */
static GHashTable *known = NULL;	/* XID -> IconWindow */

/* Static prototypes */
static void remove_window(Window win);
static void tasklist_update(gboolean to_empty);
static GdkFilterReturn window_filter(GdkXEvent *xevent,
				     GdkEvent *event,
				     gpointer data);
static guint xid_hash(XID *xid);
static gboolean xid_equal(XID *a, XID *b);
static void state_changed(IconWindow *win);
static void show_icon(IconWindow *win);
static void icon_win_free(IconWindow *win);
static void update_style(gpointer key, gpointer data, gpointer user_data);
static void update_supported(void);
static gboolean update_title(gpointer data);
static void update_current_desktop(void);

/* remember what desktop number (and viewport) is currently displayed */
static int cur_desktop = 0;
static GdkRectangle cur_viewport;

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void tasklist_set_active(gboolean active)
{
	static gboolean need_init = TRUE;
	static gboolean tasklist_active = FALSE;

	if (active == tasklist_active)
	{
		if (o_pinboard_tasklist_per_workspace.has_changed && active)
			tasklist_update(FALSE);
		return;
	}
	tasklist_active = active;
	
	if (need_init)
	{
		GdkWindow *root;

		root = gdk_get_default_root_window();

		xa__NET_SUPPORTED = gdk_atom_intern("_NET_SUPPORTED", FALSE);
		xa_WM_STATE = gdk_atom_intern("WM_STATE", FALSE);
		xa_WM_ICON_NAME = gdk_atom_intern("WM_ICON_NAME", FALSE);
		xa_WM_NAME = gdk_atom_intern("WM_NAME", FALSE);
		xa_UTF8_STRING = gdk_atom_intern("UTF8_STRING", FALSE);
		xa_TEXT = gdk_atom_intern("TEXT", FALSE);
		xa__NET_CLIENT_LIST =
				gdk_atom_intern("_NET_CLIENT_LIST", FALSE);
		xa__NET_WM_VISIBLE_NAME =
			gdk_atom_intern("_NET_WM_VISIBLE_NAME", FALSE);
		xa__NET_WM_ICON_NAME =
			gdk_atom_intern("_NET_WM_ICON_NAME", FALSE);
		xa__NET_WM_ICON_GEOMETRY =
			gdk_atom_intern("_NET_WM_ICON_GEOMETRY", FALSE);
		xa__NET_WM_STATE = gdk_atom_intern("_NET_WM_STATE", FALSE);
		xa__NET_WM_STATE_HIDDEN =
			gdk_atom_intern("_NET_WM_STATE_HIDDEN", FALSE);
		xa__NET_DESKTOP_GEOMETRY = gdk_atom_intern("_NET_DESKTOP_GEOMETRY", FALSE);
		xa__NET_DESKTOP_VIEWPORT = gdk_atom_intern("_NET_DESKTOP_VIEWPORT", FALSE);

		known = g_hash_table_new_full((GHashFunc) xid_hash,
					      (GEqualFunc) xid_equal,
					      NULL,
					      (GDestroyNotify) icon_win_free);
		gdk_window_set_events(root, gdk_window_get_events(root) |
					GDK_PROPERTY_CHANGE_MASK);
		need_init = FALSE;
	}
	
	if (active)
	{
		update_current_desktop();
		gdk_window_add_filter(NULL, window_filter, NULL);
		update_supported();
	}
	else
		gdk_window_remove_filter(NULL, window_filter, NULL);

	tasklist_update(!active);
}

/* User has changes the colours in the options box... */
void tasklist_style_changed(void)
{
	if (known)
		g_hash_table_foreach(known, update_style, NULL);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void icon_win_free(IconWindow *win)
{
	g_return_if_fail(win->widget == NULL);
	g_return_if_fail(win->label == NULL);

	if (win->timeout_update)
		g_source_remove(win->timeout_update);

	g_free(win->text);
	g_free(win);
}

/* From gdk */
static guint xid_hash(XID *xid)
{
	return *xid;
}

/* From gdk */
static gboolean xid_equal(XID *a, XID *b)
{
	return (*a == *b);
}

static int wincmp(const void *a, const void *b)
{
	const Window *aw = a;
	const Window *bw = b;

	if (*aw < *bw)
		return -1;
	else if (*aw > *bw)
		return 1;
	else
		return 0;
}

/* Read the list of WINDOWs from (xwindow,atom), returning them
 * in a (sorted) Array of Windows. On error, an empty array is
 * returned.
 * Free the array afterwards.
 */
static GArray *get_window_list(Window xwindow, GdkAtom atom)
{
	GArray *array;
	Atom type;
	int format;
	gulong nitems;
	gulong bytes_after;
	unsigned char *data;
	int err, result;
	int i;

	array = g_array_new(FALSE, FALSE, sizeof(Window));
	
	gdk_error_trap_push();
	type = None;
	result = XGetWindowProperty(gdk_display,
			xwindow,
			gdk_x11_atom_to_xatom(atom),
			0, G_MAXLONG,
			False, XA_WINDOW, &type, &format, &nitems,
			&bytes_after, &data);  
	err = gdk_error_trap_pop();

	if (err != Success || result != Success)
		return array;

	if (type == XA_WINDOW)
	{
		for (i = 0; i < nitems; i++)
			g_array_append_val(array, ((Window *) data)[i]);

		if (array->len)
			g_array_sort(array, wincmp);
	}

	XFree(data);

	return array;  
}

static guchar *get_str(IconWindow *win, GdkAtom atom)
{
	Atom rtype;
	int format;
	gulong nitems;
	gulong bytes_after;
	unsigned char *data, *str = NULL;
	int err, result;

	gdk_error_trap_push();

	result = XGetWindowProperty(gdk_display, win->xwindow,
			gdk_x11_atom_to_xatom(atom),
			0, G_MAXLONG, False,
			AnyPropertyType,
			&rtype, &format, &nitems,
			&bytes_after, &data);

	err = gdk_error_trap_pop();
	
	if (err == Success && result == Success && data)
	{
		if (*data)
			str = g_strdup(data);
		XFree(data);
	}

	return str;
}

static void get_icon_name(IconWindow *win)
{
	null_g_free(&win->text);

	/* Keep this list in sync with window_filter */

	win->text = get_str(win, xa__NET_WM_ICON_NAME);
	if (!win->text)
		win->text = get_str(win, xa__NET_WM_VISIBLE_NAME);
	if (!win->text)
		win->text = get_str(win, xa_WM_ICON_NAME);
	if (!win->text)
		win->text = get_str(win, xa_WM_NAME);
	if (!win->text)
		win->text = g_strdup(_("Window"));
}

static gboolean update_title(gpointer data)
{
	IconWindow *win = (IconWindow *) data;

	if (!win->widget)
		return FALSE;		/* No longer an icon */

	get_icon_name(win);
	wrapped_label_set_text(WRAPPED_LABEL(win->label), win->text);

	win->timeout_update = 0;

	return FALSE;
}

/* Call from within error_push/pop
 * See wnck_window_is_in_viewport */
static gboolean within_viewport(Window xwindow)
{
  int x, y;
  unsigned int width, height, bw, depth;
  Window root_window, child;
  GdkRectangle win_rect;

  XGetGeometry (gdk_display,
                xwindow,
                &root_window,
                &x, &y, &width, &height, &bw, &depth);
  XTranslateCoordinates (gdk_display,
                         xwindow,
			 root_window,
                         0, 0,
                         &x, &y, &child);

  win_rect.x = x + cur_viewport.x;
  win_rect.y = y + cur_viewport.y;
  win_rect.width = width;
  win_rect.height = height;

  return gdk_rectangle_intersect (&cur_viewport, &win_rect, &win_rect);
}

/* Call from within error_push/pop */
static void window_check_status(IconWindow *win)
{
	Atom type;
	int format;
	gulong nitems;
	gulong bytes_after;
	unsigned char *data;
	Window transient_for;
	gboolean iconic = FALSE;

	if (XGetTransientForHint(gdk_display, win->xwindow, &transient_for) && transient_for)
		iconic = FALSE;
	else if (wm_supports_hidden && XGetWindowProperty(gdk_display, win->xwindow,
			gdk_x11_atom_to_xatom(xa__NET_WM_STATE),
			0, G_MAXLONG, False,
			XA_ATOM,
			&type, &format, &nitems,
			&bytes_after, &data) == Success && data)
	{
		GdkAtom state;
		int i;
			
		for (i = 0; i < nitems; i++)
		{
			state = gdk_x11_xatom_to_atom(((Atom *) data)[i]);
			if (state == xa__NET_WM_STATE_HIDDEN)
			{
				iconic = TRUE;
				break;
			}
		}
		XFree(data);
	}
	else if (XGetWindowProperty(gdk_display, win->xwindow,
			gdk_x11_atom_to_xatom(xa_WM_STATE),
			0, 1, False,
			gdk_x11_atom_to_xatom(xa_WM_STATE),
			&type, &format, &nitems,
			&bytes_after, &data) == Success && data)
	{
		iconic = ((guint32 *) data)[0] == 3;
		XFree(data);
	}
	else
		iconic = FALSE;
	
	/* Iconified windows on another desktops are not shown */
	if (o_pinboard_tasklist_per_workspace.int_value &&
	    XGetWindowProperty(gdk_display, win->xwindow,
			gdk_x11_atom_to_xatom(xa__NET_WM_DESKTOP),
			0, G_MAXLONG, False, XA_CARDINAL, &type, &format, &nitems,
			&bytes_after, &data) == Success && data)
	{
		guint32 desk_on = ((guint32 *) data)[0];

		if ((desk_on != 0xffffffff) && ((desk_on != cur_desktop) ||
					       !within_viewport(win->xwindow)))
			iconic = FALSE;
		XFree(data);
	}

	if (win->iconified == iconic)
		return;

	win->iconified = iconic;

	state_changed(win);

	gdk_flush();
}

static void update_current_desktop(void)
{
	unsigned char *data;
	Atom type;
	int format;
	gulong nitems;
	gulong bytes_after;

	if (XGetWindowProperty(gdk_display, gdk_x11_get_default_root_xwindow(),
			gdk_x11_atom_to_xatom(xa__NET_CURRENT_DESKTOP),
			0, G_MAXLONG, False, XA_CARDINAL, &type, &format, &nitems,
			&bytes_after, (unsigned char **)(&data)) == Success && data) {
		cur_desktop = ((gint32*)data)[0];
		XFree(data);
	}

	if (XGetWindowProperty(gdk_display, gdk_x11_get_default_root_xwindow(),
			gdk_x11_atom_to_xatom(xa__NET_DESKTOP_VIEWPORT),
			0, G_MAXLONG, False, XA_CARDINAL, &type, &format,
			&nitems,
			&bytes_after, (unsigned char **)(&data)) == Success &&
	    data) {
		cur_viewport.x = ((gint32*)data)[0];
		cur_viewport.y = ((gint32*)data)[1];
		cur_viewport.width = screen_width;
		cur_viewport.height = screen_height;
		XFree(data);
	}
}

/* Called for all events on all windows */
static GdkFilterReturn window_filter(GdkXEvent *xevent,
				     GdkEvent *event,
				     gpointer data)
{
	XEvent *xev = (XEvent *) xevent;
	IconWindow *w;

	if (xev->type == PropertyNotify)
	{
		GdkAtom atom = gdk_x11_xatom_to_atom(xev->xproperty.atom);
		Window win = ((XPropertyEvent *) xev)->window;

		if (atom == xa_WM_STATE || atom == xa__NET_WM_STATE)
		{
			w = g_hash_table_lookup(known, &win);
			
			if (w)
			{
				gdk_error_trap_push();
				window_check_status(w);
				if (gdk_error_trap_pop() != Success)
					g_hash_table_remove(known, &win);
			}
		}
		else if (atom == xa__NET_CURRENT_DESKTOP ||
			 atom == xa__NET_DESKTOP_VIEWPORT ||
			 atom == xa__NET_DESKTOP_GEOMETRY)
		{
			update_current_desktop();
			tasklist_update(FALSE);
		}
		else if (atom == xa__NET_WM_ICON_NAME ||
			 atom == xa__NET_WM_VISIBLE_NAME ||
			 atom == xa_WM_ICON_NAME ||
			 atom == xa_WM_NAME)
		{
			/* Keep this list in sync with get_icon_name */
			w = g_hash_table_lookup(known, &win);

			if (w && w->widget && !w->timeout_update)
				w->timeout_update = g_timeout_add(100,
							update_title, w);
		}
		else if (atom == xa__NET_CLIENT_LIST)
			tasklist_update(FALSE);
		else if (atom == xa__NET_SUPPORTED)
			update_supported();
	}

	return GDK_FILTER_CONTINUE;
}

/* Window has been added to list of managed windows */
static void add_window(Window win)
{
	IconWindow *w;

	/* g_print("[ New window %ld ]\n", (long) win); */

	w = g_hash_table_lookup(known, &win);

	if (!w)
	{
		XWindowAttributes attr;
		
		gdk_error_trap_push();

		XGetWindowAttributes(gdk_display, win, &attr);

		if (gdk_error_trap_pop() != Success)
			return;
		gdk_error_trap_push();

		XSelectInput(gdk_display, win, attr.your_event_mask |
			PropertyChangeMask);

		gdk_flush();

		if (gdk_error_trap_pop() != Success)
			return;

		w = g_new(IconWindow, 1);
		w->widget = NULL;
		w->label = NULL;
		w->text = NULL;
		w->xwindow = win;
		w->iconified = FALSE;
		w->timeout_update = 0;

		g_hash_table_insert(known, &w->xwindow, w);
	}

	gdk_error_trap_push();

	window_check_status(w);

#if 0
	set_iconify_pos(w);
#endif

	if (gdk_error_trap_pop() != Success)
		g_hash_table_remove(known, &win);
}

/* Window is no longer managed, but hasn't been destroyed yet */
static void remove_window(Window win)
{
	IconWindow *w;

	/* g_print("[ Remove window %ld ]\n", (long) win); */

	w = g_hash_table_lookup(known, &win);
	if (w)
	{
		if (w->iconified)
		{
			w->iconified = FALSE;
			state_changed(w);
		}

		g_hash_table_remove(known, &win);
	}
}

/* Make sure the window list is up-to-date. Call once to start, and then
 * everytime _NET_CLIENT_LIST changes.
 * If 'to_empty' is set them pretend all windows have disappeared.
 */
static void tasklist_update(gboolean to_empty)
{
	static GArray *old_mapping = NULL;
	GArray *mapping = NULL;
	int new_i, old_i;

	if (!old_mapping)
	{
		old_mapping = g_array_new(FALSE, FALSE, sizeof(Window));
	}

	if (to_empty)
		mapping = g_array_new(FALSE, FALSE, sizeof(Window));
	else
		mapping = get_window_list(gdk_x11_get_default_root_xwindow(),
				gdk_atom_intern("_NET_CLIENT_LIST", FALSE));

	new_i = 0;
	old_i = 0;
	while (new_i < mapping->len && old_i < old_mapping->len)
	{
		Window new = g_array_index(mapping, Window, new_i);
		Window old = g_array_index(old_mapping, Window, old_i);

		if (new == old)
		{
			add_window(new);
			new_i++;
			old_i++;
		}
		else if (new < old)
		{
			add_window(new);
			new_i++;
		}
		else
		{
			remove_window(old);
			old_i++;
		}
	}
	while (new_i < mapping->len)
	{
		add_window(g_array_index(mapping, Window, new_i));
		new_i++;
	}
	while (old_i < old_mapping->len)
	{
		remove_window(g_array_index(old_mapping, Window, old_i));
		old_i++;
	}

	g_array_free(old_mapping, TRUE);
	old_mapping = mapping;
}

/* Called when the user clicks on the button */
static void uniconify(IconWindow *win, guint32 timestamp)
{
	XClientMessageEvent sev;

	sev.type = ClientMessage;
	sev.display = gdk_display;
	sev.format = 32;
	sev.window = win->xwindow;
	sev.message_type = gdk_x11_atom_to_xatom(
			gdk_atom_intern("_NET_ACTIVE_WINDOW", FALSE));
	sev.data.l[0] = 2;
	sev.data.l[1] = timestamp;
	sev.data.l[2] = 0;

	gdk_error_trap_push();

	XSendEvent(gdk_display, DefaultRootWindow(gdk_display), False,
			SubstructureNotifyMask | SubstructureRedirectMask,
			(XEvent *) &sev);
	XSync(gdk_display, False);

	gdk_error_trap_pop();
}

static gint drag_start_x = -1;
static gint drag_start_y = -1;
static gboolean drag_started = FALSE;
static gint drag_off_x = -1;
static gint drag_off_y = -1;

static void icon_button_press(GtkWidget *widget,
			      GdkEventButton *event,
			      IconWindow *win)
{
	if (event->button == 1)
	{
		drag_start_x = event->x_root;
		drag_start_y = event->y_root;
		drag_started = FALSE;

		drag_off_x = event->x;
		drag_off_y = event->y;
	}
}

static gboolean icon_motion_notify(GtkWidget *widget,
			       GdkEventMotion *event,
			       IconWindow *win)
{
	if (event->state & GDK_BUTTON1_MASK)
	{
		int dx = event->x_root - drag_start_x;
		int dy = event->y_root - drag_start_y;

		if (!drag_started)
		{
			if (abs(dx) < 5 && abs(dy) < 5)
				return FALSE;
			drag_started = TRUE;
		}

		fixed_move_fast(GTK_FIXED(win->widget->parent),
				win->widget,
				event->x_root - drag_off_x,
				event->y_root - drag_off_y);

		pinboard_moved_widget(win->widget, win->text,
				      event->x_root - drag_off_x,
				      event->y_root - drag_off_y);
	}

	return FALSE;
}

static void button_released(GtkWidget *widget, GdkEventButton *event,
		IconWindow *win)
{
	if (!drag_started && event->button == 1)
		uniconify(win, event->time);
}

static GdkColormap* get_cmap(GdkPixmap *pixmap)
{
	GdkColormap *cmap;

	cmap = gdk_drawable_get_colormap(pixmap);
	if (cmap)
		g_object_ref(G_OBJECT(cmap));

	if (cmap == NULL)
	{
		if (gdk_drawable_get_depth(pixmap) == 1)
		{
			/* Masks don't need colourmaps */
			cmap = NULL;
		}
		else
		{
			/* Try system cmap */
			cmap = gdk_colormap_get_system();
			g_object_ref(G_OBJECT(cmap));
		}
	}

	/* Be sure we aren't going to blow up due to visual mismatch */
	if (cmap && (gdk_colormap_get_visual(cmap)->depth !=
			 gdk_drawable_get_depth(pixmap)))
		cmap = NULL;

	return cmap;
}

/* Copy a pixmap from the server to a client-side pixbuf */
static GdkPixbuf* pixbuf_from_pixmap(Pixmap xpixmap)
{
	GdkDrawable *drawable;
	GdkPixbuf *retval;
	GdkColormap *cmap;
	int width, height;

	retval = NULL;

	drawable = gdk_xid_table_lookup(xpixmap);

	if (GDK_IS_DRAWABLE(drawable))
		g_object_ref(G_OBJECT(drawable));
	else
	{
		drawable = gdk_pixmap_foreign_new(xpixmap);
		if (!GDK_IS_DRAWABLE(drawable))
			return retval;
	}

	cmap = get_cmap(drawable);

	/* GDK is supposed to do this but doesn't in GTK 2.0.2,
	 * fixed in 2.0.3
	 */
	gdk_drawable_get_size(drawable, &width, &height);

	retval = gdk_pixbuf_get_from_drawable(NULL, drawable, cmap,
						0, 0, 0, 0, width, height);

	if (cmap)
		g_object_unref(G_OBJECT(cmap));
	g_object_unref(G_OBJECT(drawable));

	return retval;
}

/* Creates a new masked pixbuf from a non-masked pixbuf and a mask */
static GdkPixbuf* apply_mask(GdkPixbuf *pixbuf, GdkPixbuf *mask)
{
	int w, h;
	int i, j;
	GdkPixbuf *with_alpha;
	guchar *src;
	guchar *dest;
	int src_stride;
	int dest_stride;

	w = MIN(gdk_pixbuf_get_width(mask), gdk_pixbuf_get_width(pixbuf));
	h = MIN(gdk_pixbuf_get_height(mask), gdk_pixbuf_get_height(pixbuf));

	with_alpha = gdk_pixbuf_add_alpha(pixbuf, FALSE, 0, 0, 0);

	dest = gdk_pixbuf_get_pixels(with_alpha);
	src = gdk_pixbuf_get_pixels(mask);

	dest_stride = gdk_pixbuf_get_rowstride(with_alpha);
	src_stride = gdk_pixbuf_get_rowstride(mask);

	i = 0;
	while (i < h)
	{
		j = 0;
		while (j < w)
		{
			guchar *s = src + i * src_stride + j * 3;
			guchar *d = dest + i * dest_stride + j * 4;

			/* s[0] == s[1] == s[2], they are 255 if the bit was
			 * set, 0 otherwise
			 */
			if (s[0] == 0)
				d[3] = 0;   /* transparent */
			else
				d[3] = 255; /* opaque */

			++j;
		}

		++i;
	}

	return with_alpha;
}

#define BORDER_X 16
#define BORDER_Y 8
/* Take the icon that the iconified window has given us and modify it to make
 * it obvious what it is.
 */
static GdkPixbuf *apply_window_effect(GdkPixbuf *src)
{
	GdkPixbuf *new;
	int w, h;

	w = gdk_pixbuf_get_width(src);
	h = gdk_pixbuf_get_height(src);

	new = gdk_pixbuf_new(gdk_pixbuf_get_colorspace(src), TRUE,
			8, w + BORDER_X * 2, h + BORDER_Y * 2);
	gdk_pixbuf_fill(new, 0x88888888);

	gdk_pixbuf_composite(src, new,
			BORDER_X, BORDER_Y, w, h,
			BORDER_X, BORDER_Y, 1, 1,
			GDK_INTERP_NEAREST, 255);

	return new;
}

/* Return a suitable icon for this window. unref the result.
 * Never returns NULL.
 */
static GdkPixbuf *get_image_for(IconWindow *win)
{
	static MaskedPixmap *default_icon = NULL;
	Pixmap pixmap = None;
	Pixmap mask = None;
	XWMHints *hints;
	GdkPixbuf *retval = NULL;

	/* Try the pixmap and mask in the old WMHints... */
	gdk_error_trap_push();
	hints = XGetWMHints(gdk_display, win->xwindow);

	if (hints)
	{
		if (hints->flags & IconPixmapHint)
			pixmap = hints->icon_pixmap;
		if (hints->flags & IconMaskHint)
			mask = hints->icon_mask;

		XFree(hints);
		hints = NULL;
	}

	if (pixmap != None)
	{
		GdkPixbuf *mask_pb = NULL;
		
		retval = pixbuf_from_pixmap(pixmap);

		if (retval && mask != None)
			mask_pb = pixbuf_from_pixmap(mask);

		if (mask_pb)
		{
			GdkPixbuf *masked;

			masked = apply_mask(retval, mask_pb);
			g_object_unref(G_OBJECT(mask_pb));

			if (masked)
			{
				g_object_unref(G_OBJECT(retval));
				retval = masked;
			}
		}
	}

	gdk_flush();

	gdk_error_trap_pop();
	
	if (!retval)
	{
		if (!default_icon)
			default_icon = load_pixmap("iconified");

		retval = default_icon->pixbuf;
		g_object_ref(retval);
	}

	/* Apply a special effect to make this look different from normal
	 * pinboard icons.
	 */
	{
		GdkPixbuf *old = retval;
		GdkPixbuf *small;
		small = scale_pixbuf(old, ICON_WIDTH, ICON_HEIGHT);
		g_object_unref(old);
		retval = apply_window_effect(small);
		g_object_unref(small);
	}

	return retval;
}

/* Stop the button from highlighting */
static gint icon_expose(GtkWidget *widget, GdkEventExpose *event,
			IconWindow *win)
{
	static GtkWidgetClass *parent_class = NULL;

	g_return_val_if_fail(win != NULL, TRUE);
	g_return_val_if_fail(win->label != NULL, TRUE);

	if (!parent_class)
	{
		gpointer c = ((GTypeInstance *) widget)->g_class;
		parent_class = (GtkWidgetClass *) g_type_class_peek_parent(c);
	}

	draw_label_shadow((WrappedLabel *) win->label, event->region);

	gdk_gc_set_clip_region(win->label->style->fg_gc[win->label->state],
				event->region);
	(parent_class->expose_event)(widget, event);
	gdk_gc_set_clip_region(win->label->style->fg_gc[win->label->state],
				NULL);

	/* Stop the button effect */
	return TRUE;
}

/* A window has been iconified -- display it on the screen */
static void show_icon(IconWindow *win)
{
	GdkPixbuf *pixbuf;
	GtkWidget *vbox;

	g_return_if_fail(win->widget == NULL);
	g_return_if_fail(win->label == NULL);

	win->widget = gtk_button_new();
	gtk_button_set_relief(GTK_BUTTON(win->widget), GTK_RELIEF_NONE);
	g_signal_connect(win->widget, "expose-event",
			G_CALLBACK(icon_expose), win);
	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(win->widget), vbox);

	pixbuf = get_image_for(win);

	gtk_box_pack_start(GTK_BOX(vbox), simple_image_new(pixbuf),
			  FALSE, TRUE, 0);
	g_object_unref(pixbuf);

	win->label = wrapped_label_new(win->text, 180);

	update_style(NULL, win, NULL);

	gtk_box_pack_start(GTK_BOX(vbox), win->label, FALSE, TRUE, 0);

	gtk_widget_add_events(win->widget,
			GDK_BUTTON1_MOTION_MASK | GDK_BUTTON_RELEASE_MASK);
	g_signal_connect(win->widget, "button-press-event",
			G_CALLBACK(icon_button_press), win);
	g_signal_connect(win->widget, "motion-notify-event",
			G_CALLBACK(icon_motion_notify), win);
	g_signal_connect(win->widget, "button-release-event",
			G_CALLBACK(button_released), win);
	
	gtk_widget_show_all(vbox);	/* So the size comes out right */
	pinboard_add_widget(win->widget, win->text);
	gtk_widget_show(win->widget);
}

/* A window has been destroyed/expanded -- remove its icon */
static void hide_icon(IconWindow *win)
{
	g_return_if_fail(win->widget != NULL);

	gtk_widget_hide(win->widget);	/* Stops flicker - stupid GtkFixed! */
	gtk_widget_destroy(win->widget);
	win->widget = NULL;
	win->label = NULL;
}

static void state_changed(IconWindow *win)
{
	if (win->iconified)
	{
		get_icon_name(win);
		show_icon(win);
	}
	else
		hide_icon(win);
}

#if 0
/* Set the _NET_WM_ICON_GEOMETRY property, which indicates where this window
 * will be iconified to. Should be inside a push/pop.
 */
static void set_iconify_pos(IconWindow *win)
{
	gint32 data[4];

	data[0] = iconify_next_x;
	data[1] = iconify_next_y;
	data[2] = 100;
	data[3] = 32;

	XChangeProperty(gdk_display, win->xwindow,
			gdk_x11_atom_to_xatom(xa__NET_WM_ICON_GEOMETRY),
			XA_CARDINAL, 32, PropModeReplace, (guchar *) data, 4);
}
#endif

static void update_style(gpointer key, gpointer data, gpointer user_data)
{
	IconWindow *win = (IconWindow *) data;

	if (!win->widget)
		return;

	widget_modify_font(win->label, pinboard_font);
	gtk_widget_modify_fg(win->label, GTK_STATE_NORMAL, &pin_text_fg_col);
	gtk_widget_modify_bg(win->label, GTK_STATE_NORMAL, &pin_text_bg_col);
	gtk_widget_modify_fg(win->label, GTK_STATE_PRELIGHT, &pin_text_fg_col);
	gtk_widget_modify_bg(win->label, GTK_STATE_PRELIGHT, &pin_text_bg_col);
}

/* Find out what the new window manager can do... */
static void update_supported(void)
{
	Atom type;
	int format;
	gulong nitems;
	gulong bytes_after;
	unsigned char *data;
	int err, result;
	int i;
	gboolean old_supports_hidden = wm_supports_hidden;

	wm_supports_hidden = FALSE;

	gdk_error_trap_push();
	type = None;
	result = XGetWindowProperty(gdk_display,
			gdk_x11_get_default_root_xwindow(),
			gdk_x11_atom_to_xatom(xa__NET_SUPPORTED),
			0, G_MAXLONG,
			False, XA_ATOM, &type, &format, &nitems,
			&bytes_after, &data);  
	err = gdk_error_trap_pop();

	if (err != Success || result != Success)
		goto out;

	for (i = 0; i < nitems; i++)
	{
		GdkAtom atom = gdk_x11_xatom_to_atom(((Atom *) data)[i]);

		if (atom == xa__NET_WM_STATE_HIDDEN)
			wm_supports_hidden = TRUE;
	}

	XFree(data);
out:

	if (wm_supports_hidden != old_supports_hidden)
	{
		tasklist_update(TRUE);
		tasklist_update(FALSE);
	}
}
