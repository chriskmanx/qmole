#pragma once

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/extensions/shape.h>
#include "util.h"

enum x_atom {
	XATOM_WM_STATE,
	XATOM_NET_DESKTOP_NAMES,
	XATOM_NET_WM_STATE,
	XATOM_NET_ACTIVE_WINDOW,
	XATOM_NET_CLOSE_WINDOW,
	XATOM_NET_WM_NAME,
	XATOM_NET_WM_ICON_NAME,
	XATOM_NET_WM_VISIBLE_ICON_NAME,
	XATOM_NET_WORKAREA,
	XATOM_NET_WM_ICON,
	XATOM_NET_WM_ICON_GEOMETRY,
	XATOM_NET_WM_VISIBLE_NAME,
	XATOM_NET_WM_STATE_SKIP_TASKBAR,
	XATOM_NET_WM_STATE_SHADED,
	XATOM_NET_WM_STATE_HIDDEN,
	XATOM_NET_WM_STATE_DEMANDS_ATTENTION,
	XATOM_NET_WM_DESKTOP,
	XATOM_NET_MOVERESIZE_WINDOW,
	XATOM_NET_WM_WINDOW_TYPE,
	XATOM_NET_WM_WINDOW_TYPE_DOCK,
	XATOM_NET_WM_WINDOW_TYPE_DESKTOP,
	XATOM_NET_WM_STRUT,
	XATOM_NET_WM_STRUT_PARTIAL,
	XATOM_NET_CLIENT_LIST,
	XATOM_NET_NUMBER_OF_DESKTOPS,
	XATOM_NET_CURRENT_DESKTOP,
	XATOM_NET_SYSTEM_TRAY_OPCODE,
	XATOM_UTF8_STRING,
	XATOM_MOTIF_WM_HINTS,
	XATOM_XROOTPMAP_ID,

	XATOM_XDND_AWARE,
	XATOM_XDND_POSITION,
	XATOM_XDND_STATUS,
	XATOM_COUNT
};


struct x_connection {
	Display *dpy;

	int screen;
	int screen_width;
	int screen_height;

	int workarea_x;
	int workarea_y;
	int workarea_width;
	int workarea_height;

	Visual *default_visual;
	Colormap default_colormap;
	int default_depth;

	Visual *argb_visual;
	Colormap argb_colormap;

	Window root;
	Pixmap root_pixmap;
	
	Atom atoms[XATOM_COUNT];
};

void x_connect(struct x_connection *c, const char *display);
void x_disconnect(struct x_connection *c);

/*
 * default window is (ommiting 5 parameters):
 *  parent = c->root
 *  border_width = 0
 *  depth = c->default_depth
 *  class = InputOutput
 *  visual = c->default_visual
 */
Window x_create_default_window(struct x_connection *c, 
			       int x, int y, unsigned int w, unsigned int h, 
			       unsigned long valuemask, XSetWindowAttributes *attrs);

/* 
 * default pixmap is (ommiting 2 parameters):
 *  d = c->root
 *  depth = c->default_depth
 */
Pixmap x_create_default_pixmap(struct x_connection *c, 
			       unsigned int w, unsigned int h);

Window x_create_default_embedder(struct x_connection *c, Window parent, 
				 Window icon, unsigned int w, unsigned int h);

/* allocated by Xlib, should be released with XFree */
void *x_get_prop_data(struct x_connection *c, Window win, Atom prop, 
		      Atom type, int *items);

int x_get_prop_int(struct x_connection *c, Window win, Atom at);
Window x_get_prop_window(struct x_connection *c, Window win, Atom at);
Pixmap x_get_prop_pixmap(struct x_connection *c, Window win, Atom at);
int x_get_window_desktop(struct x_connection *c, Window win);

void x_set_prop_int(struct x_connection *c, Window win, Atom type, int value);
void x_set_prop_visualid(struct x_connection *c, Window win, 
			 Atom type, VisualID value);
void x_set_prop_atom(struct x_connection *c, Window win, Atom type, Atom at);
void x_set_prop_array(struct x_connection *c, Window win, Atom type, 
		      const long *values, size_t len);

int x_is_window_hidden(struct x_connection *c, Window win);
int x_is_window_iconified(struct x_connection *c, Window win);
int x_is_window_demands_attention(struct x_connection *c, Window win);

/* allocated by xstrdup, should be released with xfree */
void x_realloc_window_name(struct strbuf *sb, struct x_connection *c, 
			   Window win, Atom *atom, Atom *atype);
void x_send_netwm_message(struct x_connection *c, Window win, 
			  Atom a, long l0, long l1, long l2, long l3, long l4);
void x_send_dnd_message(struct x_connection *c, Window win, 
			Atom a, long l0, long l1, long l2, long l3, long l4);
void x_update_root_pmap(struct x_connection *c);

void x_set_error_trap();
int x_done_error_trap();
