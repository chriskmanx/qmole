#pragma once

#include "gui.h"
#include "widget-utils.h"
#include "array.h"

/* button states */
#define BUTTON_STATE_IDLE		0
#define BUTTON_STATE_IDLE_HIGHLIGHT	1
#define BUTTON_STATE_PRESSED		2
#define BUTTON_STATE_PRESSED_HIGHLIGHT	3

/**************************************************************************
  Taskbar
**************************************************************************/

struct taskbar_task {
	struct strbuf name;
	cairo_surface_t *icon;
	Window win;
	int desktop;
	int x;
	int w;
	int geom_x; /* for _NET_WM_ICON_GEOMETRY */
	int geom_w;
	int demands_attention;

	/* I'm using only one name source Atom and I'm watching it for
	 * updates. 
	 */
	Atom name_atom;
	Atom name_type_atom;
};

struct taskbar_state {
	struct triple_image background;
	struct text_info font;
	int icon_offset[2];
	int exists;
};

struct taskbar_theme {
	struct taskbar_state states[4];
	cairo_surface_t *default_icon;
	int task_max_width;

	cairo_surface_t *separator;
};

struct taskbar_widget {
	struct taskbar_theme theme;

	/* array */
	struct taskbar_task *tasks;
	size_t tasks_n;
	size_t tasks_alloc;

	Window active;
	int highlighted;
	int desktop;

	Window dnd_win;
	Window taken;

	Cursor dnd_cur;
	
	/* parameters from bmpanel2rc */
	int task_death_threshold;
	int task_urgency_hint;
};

extern struct widget_interface taskbar_interface;

/**************************************************************************
  Clock
**************************************************************************/

struct clock_theme {
	struct triple_image background;
	struct text_info font;
	char *time_format;
};

struct clock_widget {
	struct clock_theme theme;
	
	/* parameters from bmpanel2rc */
	char *clock_prog;
	int mouse_button;
};

extern struct widget_interface clock_interface;

/**************************************************************************
  Desktop Switcher
**************************************************************************/

struct desktops_state {
	cairo_surface_t *left_corner;
	struct triple_image background;
	cairo_surface_t *right_corner;
	struct text_info font;
	int exists;
};

struct desktops_desktop {
	char *name;
	int x;
	int w;
	int textw;
};

struct desktops_theme {
	struct desktops_state states[4];
	cairo_surface_t *separator;
};

struct desktops_widget {
	struct desktops_theme theme;

	/* array */
	struct desktops_desktop *desktops;
	size_t desktops_n;
	size_t desktops_alloc;

	int active;
	int highlighted;
};

extern struct widget_interface desktops_interface;

/**************************************************************************
  Decor
**************************************************************************/

struct decor_widget {
	cairo_surface_t *image;
};

extern struct widget_interface decor_interface;

/**************************************************************************
  Empty
**************************************************************************/

extern struct widget_interface empty_interface;

/**************************************************************************
  Systray
**************************************************************************/

struct systray_icon {
	Window icon;
	Window embedder;
	int mapped;
};

struct systray_theme {
	struct triple_image background;
	int icon_size[2];
	int icon_offset[2];
	int icon_spacing;
};

struct systray_widget {
	/* array */
	struct systray_icon *icons;
	size_t icons_n;
	size_t icons_alloc;

	Atom tray_selection_atom;
	Window selection_owner;
	struct systray_theme theme;
};

extern struct widget_interface systray_interface;

/**************************************************************************
  Launch Bar
**************************************************************************/

struct launchbar_item {
	cairo_surface_t *icon;
	char *execstr;
	int x;
	int w;
};

struct launchbar_widget {
	/* parameters from bmpanel2rc */
	/* array */
	struct launchbar_item *items;
	size_t items_n;
	size_t items_alloc;

	int icon_size[2];
	int active;
};

extern struct widget_interface launchbar_interface;
