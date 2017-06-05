/*
 * Copyright (C) 2008 nsf
 */

#ifndef BMPANEL_BMPANEL_H
#define BMPANEL_BMPANEL_H

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/extensions/Xrender.h>
#include <Imlib2.h>
#include "common.h"

struct task {
	struct task *next;
	char *name;
	Window win;
	Imlib_Image icon;
	int posx;
	int width;
	int desktop;
	uint focused;
	uint iconified;
};

struct desktop {
	struct desktop *next;
	char *name;
	int posx;
	int width;
	uint focused;
};

struct tray {
	struct tray *next;
	Window win;
	int x;
	int y;
};

struct panel {
	Window win;
	struct task *tasks;
	struct desktop *desktops;
	struct theme *theme;
	struct tray *trayicons;
	Window trayselowner;
	int width;
	int x;
	int y;
};

enum {
	XATOM_WM_STATE,
	XATOM_NET_DESKTOP_NAMES,
	XATOM_NET_WM_STATE,
	XATOM_NET_ACTIVE_WINDOW,
	XATOM_NET_WM_NAME,
	XATOM_NET_WORKAREA,
	XATOM_NET_WM_ICON,
	XATOM_NET_WM_VISIBLE_NAME,
	XATOM_NET_WM_STATE_SKIP_TASKBAR,
	XATOM_NET_WM_STATE_SHADED,
	XATOM_NET_WM_STATE_HIDDEN,
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
	XATOM_COUNT
};

struct xinfo {
	Display *display;
	int screen;
	int screen_width;
	int screen_height;
	
	int wa_x;
	int wa_y;
	int wa_w;
	int wa_h;

	Visual *visual;
	Colormap colmap;
	XSetWindowAttributes attrs;
	uint32_t amask;
	int depth;

	Window root;
	Pixmap rootpmap;
	Atom atoms[XATOM_COUNT];
	Atom trayselatom;
};

#endif
