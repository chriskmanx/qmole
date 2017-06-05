#include "builtin-widgets.h"

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree);
static void destroy_widget_private(struct widget *w);
static void draw(struct widget *w);
static void button_click(struct widget *w, XButtonEvent *e);
static void prop_change(struct widget *w, XPropertyEvent *e);
static void client_msg(struct widget *w, XClientMessageEvent *e);

static void dnd_drop(struct widget *w, struct drag_info *di);

static void mouse_motion(struct widget *w, XMotionEvent *e);
static void mouse_leave(struct widget *w);

struct widget_interface desktops_interface = {
	.theme_name 		= "desktop_switcher",
	.size_type 		= WIDGET_SIZE_CONSTANT,
	.create_widget_private 	= create_widget_private,
	.destroy_widget_private = destroy_widget_private,
	.draw 			= draw,
	.button_click 		= button_click,
	.prop_change 		= prop_change,
	.dnd_drop 		= dnd_drop,
	.client_msg		= client_msg,
	.mouse_motion		= mouse_motion,
	.mouse_leave		= mouse_leave
};

/**************************************************************************
  Desktop switcher theme
**************************************************************************/

static int parse_desktops_state(struct desktops_state *ds, const char *name,
				struct config_format_entry *e,
				struct config_format_tree *tree,
				int required)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (!ee) {
		if (required)
			required_entry_not_found(e, name);
		ds->exists = 0;
		return -1;
	}

	if (parse_triple_image(&ds->background, ee, tree, 1))
		return -1;

	parse_text_info_named(&ds->font, "font", ee, 0);
	ds->left_corner = parse_image_part_named("left_corner", ee, tree, 0);
	ds->right_corner = parse_image_part_named("right_corner", ee, tree, 0);

	ds->exists = 1;
	return 0;
}

static void free_desktops_state(struct desktops_state *ds)
{
	if (ds->exists) {
		free_triple_image(&ds->background);
		free_text_info(&ds->font);
		if (ds->left_corner)
			cairo_surface_destroy(ds->left_corner);
		if (ds->right_corner)
			cairo_surface_destroy(ds->right_corner);
	}
}

static int parse_desktops_theme(struct desktops_theme *dt,
				struct config_format_entry *e,
				struct config_format_tree *tree)
{
	if (parse_desktops_state(&dt->states[BUTTON_STATE_IDLE], "idle", e, tree, 1))
		goto parse_desktops_state_error_idle;

	if (parse_desktops_state(&dt->states[BUTTON_STATE_PRESSED], "pressed", e, tree, 1))
		goto parse_desktops_state_error_pressed;
	
	dt->separator = parse_image_part_named("separator", e, tree, 0);
	parse_desktops_state(&dt->states[BUTTON_STATE_IDLE_HIGHLIGHT], 
			     "idle_highlight", e, tree, 0);
	parse_desktops_state(&dt->states[BUTTON_STATE_PRESSED_HIGHLIGHT], 
			     "pressed_highlight", e, tree, 0);

	return 0;

parse_desktops_state_error_pressed:
	free_desktops_state(&dt->states[BUTTON_STATE_IDLE]);
parse_desktops_state_error_idle:
	return -1;
}

static void free_desktops_theme(struct desktops_theme *dt)
{
	unsigned int i;
	for (i = 0; i < 4; ++i)
		free_desktops_state(&dt->states[i]);
	if (dt->separator)
		cairo_surface_destroy(dt->separator);
}

/**************************************************************************
  Desktops management
**************************************************************************/

static void free_desktops(struct desktops_widget *dw)
{
	size_t i;
	for (i = 0; i < dw->desktops_n; ++i)
		xfree(dw->desktops[i].name);
	CLEAR_ARRAY(dw->desktops);
}

static void update_active_desktop(struct desktops_widget *dw, struct x_connection *c)
{
	dw->active = x_get_prop_int(c, c->root, 
				    c->atoms[XATOM_NET_CURRENT_DESKTOP]);
}

static void switch_desktop(int desktop, struct x_connection *c)
{
	int desktops = x_get_prop_int(c, c->root, 
				      c->atoms[XATOM_NET_NUMBER_OF_DESKTOPS]);
	if (desktop >= desktops)
		return;

	x_send_netwm_message(c, c->root, c->atoms[XATOM_NET_CURRENT_DESKTOP],
			     desktop, 0, 0, 0, 0);
}

static void update_desktops(struct desktops_widget *dw, struct x_connection *c)
{
	free_desktops(dw);
	update_active_desktop(dw, c);
	int desktops_n = x_get_prop_int(c, c->root, 
					c->atoms[XATOM_NET_NUMBER_OF_DESKTOPS]);
	char *name, *names;
	size_t i;
	names = name = x_get_prop_data(c, c->root, 
				       c->atoms[XATOM_NET_DESKTOP_NAMES],
				       c->atoms[XATOM_UTF8_STRING], 0);

	for (i = 0; i < desktops_n; ++i) {
		struct desktops_desktop d = {0,0,0,0};
		if (names) {
			d.name = xstrdup(name);
			name += strlen(name) + 1;
		} else {
			char buf[16];
			snprintf(buf, sizeof(buf), "%zu", i+1);
			d.name = xstrdup(buf);
		}
		ARRAY_APPEND(dw->desktops, d);
	}

	if (names)
		XFree(names);
}

static void resize_desktops(struct widget *w)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;
	struct desktops_state *ds = &dw->theme.states[BUTTON_STATE_IDLE];
	size_t i;

	int x = 0;
	
	int left_cornerw = image_width(ds->left_corner);
	int leftw = image_width(ds->background.left);
	int rightw = image_width(ds->background.right);
	int right_cornerw = image_width(ds->right_corner);
	int sepw = image_width(dw->theme.separator);

	for (i = 0; i < dw->desktops_n; ++i) {
		int basex = x;
		if (i == 0)
			x += left_cornerw;
		else 
			x += leftw;
		int maxwidth = 0;
		if (ds->font.pfd) {
			int j, width = 0;
			for (j = 0; j < 4; ++j) {
				struct desktops_state *lds = &dw->theme.states[j];
				if (!lds->exists)
					continue;
				text_extents(w->panel->layout, lds->font.pfd, 
					     dw->desktops[i].name, &width, 0);
				if (width > maxwidth)
					maxwidth = width;
			}
		}
		dw->desktops[i].textw = maxwidth;
		x += maxwidth;
		if (i == dw->desktops_n - 1)
			x += right_cornerw;
		else 
			x += rightw + sepw;
		dw->desktops[i].w = x - sepw - basex;
	}
	w->width = x;
}

static int get_desktop_at(struct widget *w, int x)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;

	size_t i;
	for (i = 0; i < dw->desktops_n; ++i) {
		struct desktops_desktop *d = &dw->desktops[i];
		if (x < (d->x + d->w) && x > d->x)
			return (int)i;
	}
	return -1;
}

/**************************************************************************
  Desktop switcher interface
**************************************************************************/

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree)
{
	struct desktops_widget *dw = xmallocz(sizeof(struct desktops_widget));
	if (parse_desktops_theme(&dw->theme, e, tree)) {
		xfree(dw);
		XWARNING("Failed to parse desktop switcher theme");
		return -1;
	}

	INIT_ARRAY(dw->desktops, 16);
	w->private = dw;

	struct x_connection *c = &w->panel->connection;
	update_desktops(dw, c);
	resize_desktops(w);
	dw->highlighted = -1;

	return 0;
}

static void destroy_widget_private(struct widget *w)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;
	free_desktops_theme(&dw->theme);
	free_desktops(dw);
	FREE_ARRAY(dw->desktops);
	xfree(dw);
}

static void draw(struct widget *w)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;
	struct desktops_state *idle = &dw->theme.states[BUTTON_STATE_IDLE];
	cairo_t *cr = w->panel->cr;
	size_t i;

	int x = w->x;
	
	int left_cornerw = image_width(idle->left_corner);
	int leftw = image_width(idle->background.left);
	int rightw = image_width(idle->background.right);
	int right_cornerw = image_width(idle->right_corner);
	int sepw = image_width(dw->theme.separator);
	int h = w->panel->height;

	for (i = 0; i < dw->desktops_n; ++i) {
		int state = (i == dw->active) << 1;
		int state_hl = ((i == dw->active) << 1) | (i == dw->highlighted);
		struct desktops_state *cur;

		if (dw->theme.states[state_hl].exists)
			cur = &dw->theme.states[state_hl];
		else
			cur = &dw->theme.states[state];
		
		dw->desktops[i].x = x;
		
		if (i == 0 && left_cornerw) {
			blit_image(cur->left_corner, cr, x, 0); 
			x += left_cornerw;
		} else if (leftw) {
			blit_image(cur->background.left, cr, x, 0);
			x += leftw;
		}
		int width = dw->desktops[i].textw;

		pattern_image(cur->background.center, cr, x, 0, width, 1);
		if (cur->font.pfd)
			draw_text(cr, w->panel->layout, &cur->font, 
				  dw->desktops[i].name, x, 0, width, h, 0);

		x += width;
		if (i == dw->desktops_n - 1 && right_cornerw) {
			blit_image(cur->right_corner, cr, x, 0);
			x += right_cornerw;
		} else {
			if (rightw) {
				blit_image(cur->background.right, cr, x, 0);
				x += rightw;
			}
			if (sepw) {
				blit_image(dw->theme.separator, cr, x, 0);
				x += sepw;
			}
		}
	}
}

static void button_click(struct widget *w, XButtonEvent *e)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;
	int di = get_desktop_at(w, e->x);
	if (di == -1)
		return;
	
	struct x_connection *c = &w->panel->connection;

	if (e->button == 1 && e->type == ButtonRelease && dw->active != di)
		switch_desktop(di, c);
}

static void prop_change(struct widget *w, XPropertyEvent *e)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;
	struct x_connection *c = &w->panel->connection;

	if (e->window == c->root) {
		if (e->atom == c->atoms[XATOM_NET_NUMBER_OF_DESKTOPS] ||
		    e->atom == c->atoms[XATOM_NET_DESKTOP_NAMES])
		{
			update_desktops(dw, c);
			resize_desktops(w);
			recalculate_widgets_sizes(w->panel);
			return;
		}

		if (e->atom == c->atoms[XATOM_NET_CURRENT_DESKTOP]) {
			update_active_desktop(dw, c);
			w->needs_expose = 1;
			return;
		}
	}
}

static void client_msg(struct widget *w, XClientMessageEvent *e)
{
	struct panel *p = w->panel;
	struct x_connection *c = &p->connection;
	struct desktops_widget *dw = (struct desktops_widget*)w->private;

	if (e->message_type == c->atoms[XATOM_XDND_POSITION]) {
		int x = (e->data.l[2] >> 16) & 0xFFFF;

		/* if it's not ours, skip.. */
		if ((x < (p->x + w->x)) || (x > (p->x + w->x + w->width)))
			return;

		int di = get_desktop_at(w, x - p->x);
		if (di != -1 && di != dw->active)
				switch_desktop(di, c);

		x_send_dnd_message(c, e->data.l[0], 
				   c->atoms[XATOM_XDND_STATUS],
				   p->win,
				   2, /* bits: 0 1 */
				   0, 0, 
				   None);
	}
}

static void dnd_drop(struct widget *w, struct drag_info *di)
{
	if (di->taken_on->interface != &taskbar_interface)
		return;

	struct taskbar_widget *tw = di->taken_on->private;
	if (tw->taken == None)
		return;

	int desktop = get_desktop_at(w, di->dropped_x);
	if (desktop == -1)
		return;

	struct x_connection *c = &w->panel->connection;
	x_send_netwm_message(c, tw->taken, 
			     c->atoms[XATOM_NET_WM_DESKTOP],
			     (long)desktop, 2, 0, 0, 0);
}

static void mouse_motion(struct widget *w, XMotionEvent *e)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;
	int i = get_desktop_at(w, e->x);
	if (i != dw->highlighted) {
		dw->highlighted = i;
		w->needs_expose = 1;
	}
}

static void mouse_leave(struct widget *w)
{
	struct desktops_widget *dw = (struct desktops_widget*)w->private;
	if (dw->highlighted != -1) {
		dw->highlighted = -1;
		w->needs_expose = 1;
	}
}
