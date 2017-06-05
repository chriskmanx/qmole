#include <stdio.h>
#include "gui.h"
#include "settings.h"
#include "widget-utils.h"

static int find_widget_in_stash(const char *interface, struct widget_stash *stash)
{
	size_t i;
	for (i = 0; i < stash->widgets_n; ++i) {
		if (!strcmp(stash->widgets[i].interface->theme_name, interface))
			return (int)i;
	}
	return -1;
}

/**************************************************************************
  Panel theme
**************************************************************************/

static int parse_position(const char *pos)
{
	if (strcmp("top", pos) == 0)
		return PANEL_POSITION_TOP;
	else if (strcmp("bottom", pos) == 0)
		return PANEL_POSITION_BOTTOM;
	XWARNING("Unknown position type: %s, back to default 'top'", pos);
	return PANEL_POSITION_TOP;
}

static int load_panel_theme(struct panel_theme *theme, struct config_format_tree *tree)
{
	CLEAR_STRUCT(theme);
	struct config_format_entry *e = find_config_format_entry(&tree->root, "panel");
	if (!e)
		return XERROR("Failed to find 'panel' section in theme format file");


	theme->position = PANEL_POSITION_TOP; /* default */
	const char *v = find_config_format_entry_value(e, "position");
	if (v)
		theme->position = parse_position(v);
	
	theme->background = parse_image_part_named("background", e, tree, 1);
	if (!theme->background)
		return -1;

	theme->separator = parse_image_part_named("separator", e, tree, 0);
	theme->transparent = parse_bool("transparent", e);
	theme->align = parse_align("align", e);
	theme->width = parse_int_or_percents("width", e, -1, 
					     &theme->width_in_percents);
	return 0;
}

static void free_panel_theme(struct panel_theme *theme)
{
	cairo_surface_destroy(theme->background);
	if (theme->separator)
		cairo_surface_destroy(theme->separator);
}

/**************************************************************************
  Panel
**************************************************************************/

static void select_render_interface(struct panel *p)
{
	/* TODO: composite manager detection and composite render */
	if (p->theme.transparent)
		p->render = &render_pseudo;
	else
		p->render = &render_normal;
}

static void get_position_and_strut(const struct x_connection *c, 
		const struct panel_theme *t, int *ox, int *oy, 
		int *ow, int *oh, long *strut)
{
	int x,y,w,h;
	x = c->workarea_x;
	y = c->workarea_y;
	h = image_height(t->background);
	w = c->workarea_width;

	strut[0] = strut[1] = strut[3] = 0;
	strut[2] = h + c->workarea_y;
	if (t->position == PANEL_POSITION_BOTTOM) {
		y = (c->workarea_height + c->workarea_y) - h;
		strut[2] = 0;
		strut[3] = h + c->screen_height - 
			(c->workarea_height + c->workarea_y);
	}

	/* variable width */
	if (t->width != -1) {
		if (t->width_in_percents)
			w = ((float)c->workarea_width / 100.0f) * t->width;
		else
			w = t->width;

		/* limit */
		if (w > c->workarea_width)
			w = c->workarea_width;
		
		/* X */
		switch (t->align) {
		case ALIGN_CENTER:
			x += (c->workarea_width - w) / 2;
			break;
		case ALIGN_RIGHT:
			x += c->workarea_width - w;
			break;
		default:
			/* skip */
			break;
		}
	}

	*ox = x; *oy = y; *oh = h; *ow = w;
	
	static const struct {
		int s, e;
	} where[] = {
		[PANEL_POSITION_TOP] = {8, 9},
		[PANEL_POSITION_BOTTOM] = {10, 11}
	};

	strut[where[t->position].s] = x;
	strut[where[t->position].e] = x+w;
}

static void get_strut_for_position(const struct x_connection *c,
				   const struct panel_theme *t,
				   int x, int y, int w, int h,
				   long *strut)
{
	strut[0] = strut[1] = strut[3] = 0;
	strut[2] = h;
	
	if (t->position == PANEL_POSITION_BOTTOM) {
		strut[2] = 0;
		strut[3] = c->screen_height - h;
	}
	
	static const struct {
		int s, e;
	} where[] = {
		[PANEL_POSITION_TOP] = {8, 9},
		[PANEL_POSITION_BOTTOM] = {10, 11}
	};

	strut[where[t->position].s] = x;
	strut[where[t->position].e] = x+w;
}

static void create_window(struct panel *panel, int ax, int ay, int aw)
{
	struct x_connection *c = &panel->connection;
	struct panel_theme *t = &panel->theme;

	int x,y,w,h;
	long strut[12] = {0};

	if (ax == -1)
		get_position_and_strut(c, t, &x, &y, &w, &h, strut);
	else {
		x = ax;
		y = ay;
		w = aw;
		h = image_height(t->background);
		get_strut_for_position(c, t, x, y, w, h, strut);
	}

	panel->bg = x_create_default_pixmap(c, w, h);

	XSetWindowAttributes attrs;
	attrs.background_pixmap = panel->bg;
	attrs.event_mask = ExposureMask | StructureNotifyMask | ButtonPressMask |
		ButtonReleaseMask | PointerMotionMask | EnterWindowMask |
		LeaveWindowMask;
	panel->win = x_create_default_window(c, x, y, w, h, 
					     CWBackPixmap | CWEventMask, &attrs);

	panel->x = x;
	panel->y = y;
	panel->width = w;
	panel->height = h;

	/* Xdnd awareness */
	x_set_prop_atom(c, panel->win, c->atoms[XATOM_XDND_AWARE], 5);

	/* XWMHints */
	XWMHints wmhints;
	wmhints.flags = InputHint;
	wmhints.input = 0;
	XSetWMHints(c->dpy, panel->win, &wmhints);

	/* NETWM struts */
	x_set_prop_array(c, panel->win, c->atoms[XATOM_NET_WM_STRUT], strut, 4);
	x_set_prop_array(c, panel->win, c->atoms[XATOM_NET_WM_STRUT_PARTIAL], 
			strut, 12);

	/* desktops and window type */
	x_set_prop_int(c, panel->win, c->atoms[XATOM_NET_WM_DESKTOP], -1);
	x_set_prop_atom(c, panel->win, c->atoms[XATOM_NET_WM_WINDOW_TYPE],
			c->atoms[XATOM_NET_WM_WINDOW_TYPE_DOCK]);
	
	/* also send desktop message to wm */
	x_send_netwm_message(c, panel->win, c->atoms[XATOM_NET_WM_DESKTOP], 
			     0xFFFFFFFF, 0, 0, 0, 0);
	
	/* place window on it's position */
	XSizeHints size_hints;

	size_hints.x = x;
	size_hints.y = y;
	size_hints.width = w;
	size_hints.height = h;

	size_hints.flags = PPosition | PMaxSize | PMinSize;
	size_hints.min_width = size_hints.max_width = w;
	size_hints.min_height = size_hints.max_height = h;
	XSetWMNormalHints(c->dpy, panel->win, &size_hints);

	/* motif hints */
	#define MWM_HINTS_DECORATIONS (1L << 1)
	struct mwmhints {
		uint32_t flags;
		uint32_t functions;
		uint32_t decorations;
		int32_t input_mode;
		uint32_t status;
	} mwm = {MWM_HINTS_DECORATIONS,0,0,0,0};
	XChangeProperty(c->dpy, panel->win, c->atoms[XATOM_MOTIF_WM_HINTS], 
			c->atoms[XATOM_MOTIF_WM_HINTS], 32, PropModeReplace, 
			(unsigned char*)&mwm, sizeof(struct mwmhints) / 4);
	#undef MWM_HINTS_DECORATIONS

	/* classhint */
	XClassHint ch;
	ch.res_name = "panel";
	ch.res_class = "bmpanel";
	XSetClassHint(c->dpy, panel->win, &ch);
}

static void parse_panel_widgets(struct panel *panel, struct config_format_tree *tree)
{
	size_t i;
	for (i = 0; i < tree->root.children_n; ++i) {
		struct config_format_entry *e = &tree->root.children[i];
		struct widget_interface *we = lookup_widget_interface(e->name);
		if (!we) 
			continue;

		if (panel->widgets_n == PANEL_MAX_WIDGETS)
			XDIE("error: Widgets limit reached");
		
		struct widget *w = &panel->widgets[panel->widgets_n];

		w->interface = we;
		w->panel = panel;
		w->needs_expose = 0;

		if ((*we->create_widget_private)(w, e, tree) == 0) {
			panel->widgets_n++;
			w->no_separator = parse_bool("no_separator", e);
			w->paint_replace = parse_bool("paint_replace", e);
		} else {
			XWARNING("Failed to create widget: \"%s\"", e->name);
		}
	}
}

static void retheme_reconfigure_panel_widgets(struct widget_stash *stash,
					      struct panel *panel, 
					      struct config_format_tree *tree)
{
	size_t i;
	for (i = 0; i < tree->root.children_n; ++i) {
		struct config_format_entry *e = &tree->root.children[i];
		struct widget_interface *we = lookup_widget_interface(e->name);
		if (!we)
			continue;
			
		if (panel->widgets_n == PANEL_MAX_WIDGETS)
			XDIE("error: Widgets limit reached");
		
		struct widget *w = &panel->widgets[panel->widgets_n];

		w->interface = we;
		w->panel = panel;
		w->needs_expose = 0;
		
		int stashwi = find_widget_in_stash(e->name, stash);
		if (stashwi != -1 && we->retheme_reconfigure) {
			/* pop widget from the stash */
			struct widget *sw = &stash->widgets[stashwi];
			*w = *sw;
			*sw = stash->widgets[stash->widgets_n-1];
			stash->widgets_n--;

			/* try retheme or destroy */
			if ((*we->retheme_reconfigure)(w, e, tree) == 0) {
				panel->widgets_n++;

				w->no_separator = parse_bool("no_separator", e);
				w->paint_replace = parse_bool("paint_replace", e);

				continue;
			} else 
				(*w->interface->destroy_widget_private)(w);
		}

		/* create new one if failed */
		if ((*we->create_widget_private)(w, e, tree) == 0) {
			panel->widgets_n++;
			w->no_separator = parse_bool("no_separator", e);
			w->paint_replace = parse_bool("paint_replace", e);
		} else {
			XWARNING("Failed to create widget: \"%s\"", e->name);
		}
	}
}

void recalculate_widgets_sizes(struct panel *panel)
{
        const int min_fill_size = 20; /* was 200 */
	int num_constant = 0;
	int num_fill = 0;
	int total_constants_width = 0;
	int x = 0;
	int x2 = panel->width;
	int separators = 0;
	int separator_width = image_width(panel->theme.separator);
	int total_separators_width = 0;
	size_t i;

	for (i = 0; i < panel->widgets_n; ++i) {
		struct widget *w = &panel->widgets[i];
		if (w->interface->size_type == WIDGET_SIZE_CONSTANT) {
			num_constant++;
			total_constants_width += w->width;
			if (w->width && !w->no_separator)
				separators++;
		} else
			num_fill++;
	}

	total_separators_width = separators * separator_width;

	if (num_fill != 1)
		XDIE("There always should be exactly one widget with a "
		     "SIZE_FILL size type (taskbar)");

	if (total_constants_width + total_separators_width > 
	    panel->width - min_fill_size)
	{
	  XDIE("Too many widgets here, try to remove one or more");
	  //XWARNING("Too many widgets here, try to remove one or more\n");
	}

	for (i = 0; i < panel->widgets_n; ++i) {
		struct widget *w = &panel->widgets[i];
		if (w->interface->size_type == WIDGET_SIZE_FILL)
			break;

		w->x = x;
		x += w->width;
		if (w->width && !w->no_separator)
			x += separator_width;
	}

	for (i = panel->widgets_n - 1;; --i) {
		struct widget *w = &panel->widgets[i];
		if (w->interface->size_type == WIDGET_SIZE_FILL)
			break;

		x2 -= w->width;
		w->x = x2;
		if (w->width && !w->no_separator)
			x2 -= separator_width;
	}

	panel->widgets[i].x = x;
	panel->widgets[i].width = x2 - x;

	/* request redraw */
	panel->needs_expose = 1;
}

static void expose_whole_panel(struct panel *panel)
{
	Display *dpy = panel->connection.dpy;

	int sepw = 0;
	sepw += image_width(panel->theme.separator);

	size_t i;
	for (i = 0; i < panel->widgets_n; ++i) {
		struct widget *wi = &panel->widgets[i];
		int x = wi->x;
		int w = wi->width;
		if (!w) /* skip empty */
			continue;

		/* background */
		pattern_image(panel->theme.background, panel->cr, x, 0, w, 0);

		cairo_save(panel->cr);
		if (wi->paint_replace)
			cairo_set_operator(panel->cr, CAIRO_OPERATOR_SOURCE);

		/* widget contents */
		if (wi->interface->draw)
			(*wi->interface->draw)(wi);
		cairo_restore(panel->cr);

		/* separator */
		x += w;
		if (panel->theme.separator && panel->widgets_n - 1 != i)
			blit_image(panel->theme.separator, panel->cr, x, 0);

		/* widget was drawn, clear "needs_expose" flag */
		wi->needs_expose = 0;
	}

	(*panel->render->blit)(panel, 0, 0, panel->width, panel->height);
	XFlush(dpy);
	panel->needs_expose = 0;

	/* after exposing panel actions, for those who need panel background
	 * (e.g. systray icons)
	 */
	for (i = 0; i < panel->widgets_n; ++i) {
		struct widget *wi = &panel->widgets[i];
		if (wi->interface->panel_exposed)
			(*wi->interface->panel_exposed)(wi);
	}
	XFlush(dpy);
}

static void expose_panel(struct panel *panel)
{
	Display *dpy = panel->connection.dpy;

	if (panel->needs_expose) {
		expose_whole_panel(panel);
		return;
	}

	size_t i;
	for (i = 0; i < panel->widgets_n; ++i) {
		struct widget *w = &panel->widgets[i];
		if (!w->needs_expose)
			continue;
		
		pattern_image(panel->theme.background, panel->cr, 
				w->x, 0, w->width, 0);
		cairo_save(panel->cr);
		if (w->paint_replace)
			cairo_set_operator(panel->cr, CAIRO_OPERATOR_SOURCE);
		if (w->interface->draw)
			(*w->interface->draw)(w);
		cairo_restore(panel->cr);

		(*panel->render->blit)(panel, w->x, 0, 
				       w->width, panel->height);
		w->needs_expose = 0;
	}
	XFlush(dpy);
}

void init_panel(struct panel *panel, struct config_format_tree *tree,
		int x, int y, int w)
{
	CLEAR_STRUCT(panel);

	/* connect to X server */
	x_connect(&panel->connection, 0);

	/* parse panel theme */
	if (load_panel_theme(&panel->theme, tree))
		XDIE("Failed to load theme format file");
	
	panel->drag_threshold = parse_int("drag_threshold",
					  &g_settings.root, 30);

	select_render_interface(panel);
	struct x_connection *c = &panel->connection;

	/* create window */
	create_window(panel, x, y, w);
	
	/* render private */
	if (panel->render->create_private)
		(*panel->render->create_private)(panel);

	/* rendering context */
	(*panel->render->create_dc)(panel);

	/* create text layout */
	panel->layout = pango_cairo_create_layout(panel->cr);

	/* parse panel widgets */
	parse_panel_widgets(panel, tree);
	recalculate_widgets_sizes(panel);

	/* all ok, map window */
	expose_panel(panel);
	XMapWindow(c->dpy, panel->win);
	XFlush(c->dpy);
	
	/* send desktop property again after mapping (fluxbox bug?) */
	x_send_netwm_message(c, panel->win, c->atoms[XATOM_NET_WM_DESKTOP], 
			     0xFFFFFFFF, 0, 0, 0, 0);
}

void free_panel(struct panel *panel)
{
	size_t i;

	if (panel->render->free_private)
		(*panel->render->free_private)(panel);

	for (i = 0; i < panel->widgets_n; ++i) {
		struct widget *w = &panel->widgets[i];
		(*w->interface->destroy_widget_private)(w);
	}
	panel->widgets_n = 0;

	g_object_unref(panel->layout);
	cairo_destroy(panel->cr);
	XDestroyWindow(panel->connection.dpy, panel->win);
	XFreePixmap(panel->connection.dpy, panel->bg);
	free_panel_theme(&panel->theme);
	x_disconnect(&panel->connection);
}

void reconfigure_free_panel(struct panel *panel, struct widget_stash *stash)
{
	/* free stuff */
	if (panel->render->free_private)
		(*panel->render->free_private)(panel);

	stash->widgets = xmalloc(sizeof(struct widget) * panel->widgets_n);
	stash->widgets_n = panel->widgets_n;
	memcpy(stash->widgets, panel->widgets, 
	       sizeof(struct widget) * panel->widgets_n);

	panel->widgets_n = 0;

	g_object_unref(panel->layout);
	cairo_destroy(panel->cr);
	free_panel_theme(&panel->theme);
}

void reconfigure_panel(struct panel *panel, struct config_format_tree *tree,
		       struct widget_stash *stash)
{
	/* reload theme */
	if (load_panel_theme(&panel->theme, tree))
		XDIE("Failed to load theme format file");
	
	/* reparse config values */
	reconfigure_panel_config(panel);
	
	/* check render interface */
	select_render_interface(panel);

	/* move panel */
	struct x_connection *c = &panel->connection;
	struct panel_theme *t = &panel->theme;

	int x,y,w,h;
	long strut[12] = {0};
	get_position_and_strut(c, t, &x, &y, &w, &h, strut);
	panel->x = x;
	panel->y = y;
	panel->width = w;
	panel->height = h;
	
	XFreePixmap(panel->connection.dpy, panel->bg);
	panel->bg = x_create_default_pixmap(c, w, h);
	
	/* render private */
	if (panel->render->create_private)
		(*panel->render->create_private)(panel);

	/* rendering context */
	(*panel->render->create_dc)(panel);

	/* create text layout */
	panel->layout = pango_cairo_create_layout(panel->cr);
	
	/* reparse panel widgets */
	retheme_reconfigure_panel_widgets(stash, panel, tree);
	size_t i;
	for (i = 0; i < stash->widgets_n; ++i) {
		struct widget *w = &stash->widgets[i];
		(*w->interface->destroy_widget_private)(w);
	}
	xfree(stash->widgets);
	recalculate_widgets_sizes(panel);

	/* all ok, update window */
	XSetWindowBackgroundPixmap(c->dpy, panel->win, panel->bg);
	XFlush(c->dpy);
	expose_panel(panel);

	XMoveResizeWindow(c->dpy, panel->win, x, y, w, h);
	x_set_prop_array(c, panel->win, c->atoms[XATOM_NET_WM_STRUT], strut, 4);
	x_set_prop_array(c, panel->win, c->atoms[XATOM_NET_WM_STRUT_PARTIAL], 
			 strut, 12);

	XSizeHints size_hints;
	size_hints.x = x;
	size_hints.y = y;
	size_hints.width = w;
	size_hints.height = h;

	size_hints.flags = PPosition | PMaxSize | PMinSize;
	size_hints.min_width = size_hints.max_width = w;
	size_hints.min_height = size_hints.max_height = h;
	XSetWMNormalHints(c->dpy, panel->win, &size_hints);
	XFlush(c->dpy);
}

void reconfigure_panel_config(struct panel *panel)
{
	panel->drag_threshold = parse_int("drag_threshold",
					  &g_settings.root, 30);
}

void reconfigure_widgets(struct panel *panel)
{
	size_t i;
	for (i = 0; i < panel->widgets_n; ++i) {
		struct widget *w = &panel->widgets[i];
		if (w->interface->reconfigure)
			(*w->interface->reconfigure)(w);
	}
	recalculate_widgets_sizes(panel);
}

static void panel_property_notify(struct panel *p, XPropertyEvent *e)
{
	if (e->atom == p->connection.atoms[XATOM_XROOTPMAP_ID] &&
	    p->render->update_bg) 
	{
		(*p->render->update_bg)(p);
	}
}

static void panel_configure_notify(struct panel *p, XConfigureEvent *e)
{
	struct x_connection *c = &p->connection;
	struct panel_theme *t = &p->theme;

	int x,y,w,h;
	long strut[12] = {0};

	if (e->window == c->root &&
	    (e->width != c->screen_width ||
	    e->height != c->screen_height)) 
	{
		/* resolution changed */
		c->screen_width = e->width;
		c->screen_height = e->height;

		c->workarea_x = 0;
		c->workarea_y = 0;
		c->workarea_width = e->width;
		c->workarea_height = e->height;
		
		get_position_and_strut(c, t, &x, &y, &w, &h, strut);
		XMoveResizeWindow(c->dpy, p->win, x, y, w, h);
		x_set_prop_array(c, p->win, c->atoms[XATOM_NET_WM_STRUT], strut, 4);
		x_set_prop_array(c, p->win, c->atoms[XATOM_NET_WM_STRUT_PARTIAL], 
				 strut, 12);

		p->x = x;
		p->y = y;
		p->width = w;
		p->height = h;
	
		XSizeHints size_hints;
		size_hints.x = x;
		size_hints.y = y;
		size_hints.width = w;
		size_hints.height = h;

		size_hints.flags = PPosition | PMaxSize | PMinSize;
		size_hints.min_width = size_hints.max_width = w;
		size_hints.min_height = size_hints.max_height = h;
		XSetWMNormalHints(c->dpy, p->win, &size_hints);

		if (p->render->panel_resize)
			(*p->render->panel_resize)(p);

		recalculate_widgets_sizes(p);
	}
}

static void panel_expose(struct panel *p, XExposeEvent *e)
{
	if (e->window == p->win && p->render->expose)
		(*p->render->expose)(p);
}

static int process_events(struct panel *p)
{
	Display *dpy = p->connection.dpy;
	int events_processed = 0;
	
	while (XPending(dpy)) {
		XEvent e;

		events_processed++;
		XNextEvent(dpy, &e);

		switch (e.type) {
		
		case NoExpose:
		case MapNotify:
		case UnmapNotify:
		case VisibilityNotify:
		case ReparentNotify:
		case SelectionClear:
			/* skip? */
			break;

		case Expose:
			panel_expose(p, &e.xexpose);
			break;
		
		case ButtonRelease:
		case ButtonPress:
			disp_button_press_release(p, &e.xbutton);
			break;

		case MotionNotify:
			disp_motion_notify(p, &e.xmotion);
			break;

		case EnterNotify:
		case LeaveNotify:
			disp_enter_leave_notify(p, &e.xcrossing);
			break;
		
		case PropertyNotify:
			panel_property_notify(p, &e.xproperty);
			disp_property_notify(p, &e.xproperty);
			break;

		case ClientMessage:
			disp_client_msg(p, &e.xclient);
			break;
		
		case ConfigureNotify:
			panel_configure_notify(p, &e.xconfigure);
			disp_configure(p, &e.xconfigure);
			break;

		case DestroyNotify:
			disp_win_destroy(p, &e.xdestroywindow);
			break;
		
		default:
			XWARNING("Unknown XEvent (type: %d, win: %d)", 
				 e.type, e.xany.window);
			break;
		}
	}
	if (events_processed)
		expose_panel(p);
	return events_processed;
}

static gboolean panel_second_timeout(gpointer data)
{
	struct panel *p = data;
	size_t i;
	struct widget *w;
	for (i = 0; i < p->widgets_n; ++i) {
		w = &p->widgets[i];
		if (w->interface->clock_tick)
			(*w->interface->clock_tick)(w);
	}
	expose_panel(p);
	/* just in case, actually it helps a lot */
	process_events(p);
	return 1;
}

static gboolean panel_x_in(GIOChannel *gio, GIOCondition condition, gpointer data)
{
	/* TODO: be aware of connection drop */
	/* ENSURE(condition == G_IO_IN, "Input condition failed"); */
	struct panel *p = data;

	/* we do here more greedy processing */
	while (process_events(p))
		;

	return 1;
}

void panel_main_loop(struct panel *panel)
{
	int fd = ConnectionNumber(panel->connection.dpy);
	panel->loop = g_main_loop_new(0, 0);
	
	GIOChannel *x = g_io_channel_unix_new(fd);
	g_io_add_watch(x, G_IO_IN | G_IO_HUP, panel_x_in, panel);
	g_io_channel_unref(x);

	g_timeout_add(1000, panel_second_timeout, panel);

	g_main_loop_run(panel->loop);
	g_main_loop_unref(panel->loop);
}

