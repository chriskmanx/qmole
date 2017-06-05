#include "settings.h"
#include "builtin-widgets.h"

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree);
static void destroy_widget_private(struct widget *w);
static void draw(struct widget *w);
static void button_click(struct widget *w, XButtonEvent *e);
static void prop_change(struct widget *w, XPropertyEvent *e);
static void client_msg(struct widget *w, XClientMessageEvent *e);

static void dnd_start(struct widget *w, struct drag_info *di);
static void dnd_drag(struct widget *w, struct drag_info *di);
static void dnd_drop(struct widget *w, struct drag_info *di);

static void mouse_motion(struct widget *w, XMotionEvent *e);
static void mouse_leave(struct widget *w);

static void clock_tick(struct widget *w);
static void reconfigure(struct widget *w);

struct widget_interface taskbar_interface = {
	.theme_name 		= "taskbar",
	.size_type 		= WIDGET_SIZE_FILL,
	.create_widget_private 	= create_widget_private,
	.destroy_widget_private = destroy_widget_private,
	.draw 			= draw,
	.button_click 		= button_click,
	.prop_change 		= prop_change,
	.dnd_start 		= dnd_start,
	.dnd_drag 		= dnd_drag,
	.dnd_drop 		= dnd_drop,
	.client_msg		= client_msg,
	.mouse_motion		= mouse_motion,
	.mouse_leave		= mouse_leave,
	.clock_tick		= clock_tick,
	.reconfigure		= reconfigure
};

/**************************************************************************
  Taskbar theme
**************************************************************************/

static int parse_taskbar_state(struct taskbar_state *ts, const char *name,
		struct config_format_entry *e, struct config_format_tree *tree,
		int required)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (!ee) {
		if (required)
			required_entry_not_found(e, name);
		ts->exists = 0;
		return -1;
	}

	if (parse_triple_image(&ts->background, ee, tree, 1))
		goto parse_taskbar_state_error_background;

	if (parse_text_info_named(&ts->font, "font", ee, 1))
		goto parse_taskbar_state_error_font;

	parse_2ints(ts->icon_offset, "icon_offset", ee);
	
	ts->exists = 1;
	return 0;

parse_taskbar_state_error_font:
	free_triple_image(&ts->background);
parse_taskbar_state_error_background:
	return -1;
}

static void free_taskbar_state(struct taskbar_state *ts)
{
	if (ts->exists) {
		free_triple_image(&ts->background);
		free_text_info(&ts->font);
	}
}

static int parse_taskbar_theme(struct taskbar_theme *tt, 
		struct config_format_entry *e, struct config_format_tree *tree)
{
	if (parse_taskbar_state(&tt->states[BUTTON_STATE_IDLE], "idle", e, tree, 1))
		goto parse_taskbar_button_theme_error_idle;

	if (parse_taskbar_state(&tt->states[BUTTON_STATE_PRESSED], "pressed", e, tree, 1))
		goto parse_taskbar_button_theme_error_pressed;

	struct config_format_entry *ee = find_config_format_entry(e, "default_icon");
	if (ee)
		tt->default_icon = parse_image_part(ee, tree, 0);

	parse_taskbar_state(&tt->states[BUTTON_STATE_IDLE_HIGHLIGHT], 
			    "idle_highlight", e, tree, 0);
	parse_taskbar_state(&tt->states[BUTTON_STATE_PRESSED_HIGHLIGHT], 
			    "pressed_highlight", e, tree, 0);

	tt->separator = parse_image_part_named("separator", e, tree, 0);
	tt->task_max_width = parse_int("task_max_width", e, 0);

	return 0;

parse_taskbar_button_theme_error_pressed:
	free_taskbar_state(&tt->states[BUTTON_STATE_IDLE]);
parse_taskbar_button_theme_error_idle:
	return -1;
}

static void free_taskbar_theme(struct taskbar_theme *tt)
{
	unsigned int i;
	for (i = 0; i < 4; ++i)
		free_taskbar_state(&tt->states[i]);
	if (tt->default_icon)
		cairo_surface_destroy(tt->default_icon);
	if (tt->separator)
		cairo_surface_destroy(tt->separator);
}

/**************************************************************************
  Taskbar task management
**************************************************************************/

static int find_task_by_window(struct taskbar_widget *tw, Window win)
{
	size_t i;
	for (i = 0; i < tw->tasks_n; ++i) {
		if (tw->tasks[i].win == win)
			return (int)i;
	}
	return -1;
}

static int find_last_task_by_desktop(struct taskbar_widget *tw, int desktop)
{
	int t = -1;
	size_t i;
	for (i = 0; i < tw->tasks_n; ++i) {
		if (tw->tasks[i].desktop <= desktop)
			t = (int)i;
	}
	return t;
}

static void add_task(struct widget *w, struct x_connection *c, Window win)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	struct taskbar_task t;

	x_set_error_trap();
	if (x_is_window_hidden(c, win)) {
		if (x_done_error_trap())
			return;
		// we need this if window will apear later
		if (w->panel->win != win)
			XSelectInput(c->dpy, win, PropertyChangeMask);
		return;
	}
	
	XSelectInput(c->dpy, win, PropertyChangeMask);

	CLEAR_STRUCT(&t);
	t.win = win;
	t.demands_attention = x_is_window_demands_attention(c, win);

	x_realloc_window_name(&t.name, c, win, &t.name_atom, &t.name_type_atom); 
	if (tw->theme.default_icon)
		t.icon = get_window_icon(c, win, tw->theme.default_icon);
	else
		t.icon = 0;
	t.desktop = x_get_window_desktop(c, win);

	int i = find_last_task_by_desktop(tw, t.desktop);
	if (i == -1)
		ARRAY_PREPEND(tw->tasks, t);
	else
		ARRAY_INSERT_AFTER(tw->tasks, (size_t)i, t);
}

static void free_task(struct taskbar_task *t)
{
	strbuf_free(&t->name);
	if (t->icon)
		cairo_surface_destroy(t->icon);
}

static void remove_task(struct taskbar_widget *tw, size_t i)
{
	free_task(&tw->tasks[i]);
	ARRAY_REMOVE(tw->tasks, i);
}

static void free_tasks(struct taskbar_widget *tw)
{
	size_t i;
	for (i = 0; i < tw->tasks_n; ++i)
		free_task(&tw->tasks[i]);
	FREE_ARRAY(tw->tasks);
}

static int count_tasks_on_desktop(struct taskbar_widget *tw, int desktop)
{
	int count = 0;
	size_t i;
	for (i = 0; i < tw->tasks_n; ++i) {
		if (tw->tasks[i].desktop == desktop ||
		    tw->tasks[i].desktop == -1) /* count "all desktops" too */
		{
			count++;
		}
	}
	return count;
}

static int highlighted_state_exists(struct taskbar_theme *theme, int active)
{
	int state_hl = (active << 1) | 1;
	if (theme->states[state_hl].exists)
		return 1;
	return 0;
}

static void draw_task(struct taskbar_task *task, struct taskbar_widget *tw,
		cairo_t *cr, PangoLayout *layout, int x, int w, int active,
		int highlighted)
{
	struct taskbar_theme *theme = &tw->theme;

	if (tw->task_urgency_hint) {
		if (active)
			task->demands_attention = 0;

		if (task->demands_attention > 0) {
			if (highlighted_state_exists(theme, active))
				highlighted = task->demands_attention - 1;
			else
				active = task->demands_attention - 1;
		}
	}

	/* calculations */
	int state = active << 1;
	int state_hl = (active << 1) | highlighted;

	struct triple_image *tbt;
	struct text_info *font;
	int *icon_offset;
	if (theme->states[state_hl].exists) {
		tbt = &theme->states[state_hl].background;
		font = &theme->states[state_hl].font;
		icon_offset = theme->states[state_hl].icon_offset;
	} else {
		tbt = &theme->states[state].background;
		font = &theme->states[state].font;
		icon_offset = theme->states[state].icon_offset;
	}

	int leftw = image_width(tbt->left);
	int rightw = image_width(tbt->right);
	int height = image_height(tbt->center);
	int centerw = w - leftw - rightw;
	
	int iconw = image_width(theme->default_icon);
	int iconh = image_height(theme->default_icon);
	int textw = centerw - (iconw + icon_offset[0]);

	/* background */
	int leftx = x;
	int centerx = x + leftw;
	int rightx = centerx + centerw;

	if (tbt->stretched_overlap)
		stretch_image(tbt->center, cr, 
			      leftx + tbt->center_offsets[0], 0, 
			      w - tbt->center_offsets[0] - tbt->center_offsets[1]);
	else if (tbt->stretched)
		stretch_image(tbt->center, cr, 
			      centerx + tbt->center_offsets[0], 0, 
			      centerw - tbt->center_offsets[0] - tbt->center_offsets[1]);
	else
		pattern_image(tbt->center, cr, centerx, 0, centerw, 1);

	if (leftw) blit_image(tbt->left, cr, leftx, 0);
	if (rightw) blit_image(tbt->right, cr, rightx, 0);

	/* icon */
	int xx = centerx;
	if (iconw && iconh) {
		int yy = (height - iconh) / 2;
		xx += icon_offset[0];
		yy += icon_offset[1];
		cairo_save(cr);
		cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
		blit_image(task->icon, cr, xx, yy);
		cairo_restore(cr);
	}
	xx += iconw; 
	
	/* text */
	draw_text(cr, layout, font, task->name.buf, xx, 0, textw, height, 1);
}

static inline void activate_task(struct x_connection *c, struct taskbar_task *t)
{
	x_send_netwm_message(c, t->win, c->atoms[XATOM_NET_ACTIVE_WINDOW], 
			2, CurrentTime, 0, 0, 0);

	XWindowChanges wc;
	wc.stack_mode = Above;
	XConfigureWindow(c->dpy, t->win, CWStackMode, &wc);
}

static inline void close_task(struct x_connection *c, struct taskbar_task *t)
{
	x_send_netwm_message(c, t->win, c->atoms[XATOM_NET_CLOSE_WINDOW],
			CurrentTime, 2, 0, 0, 0);
}

static void move_task(struct taskbar_widget *tw, int what, int where)
{
	struct taskbar_task t = tw->tasks[what];
	if (what == where)
		return;
	ARRAY_REMOVE(tw->tasks, (size_t)what);
	if (where > what) {
		where -= 1;
		ARRAY_INSERT_AFTER(tw->tasks, (size_t)where, t);
	} else {
		ARRAY_INSERT_BEFORE(tw->tasks, (size_t)where, t);
	}
}

/**************************************************************************
  Updates
**************************************************************************/

static void update_active(struct taskbar_widget *tw, struct x_connection *c)
{
	tw->active = x_get_prop_window(c, c->root, 
			c->atoms[XATOM_NET_ACTIVE_WINDOW]);
}

static void update_desktop(struct taskbar_widget *tw, struct x_connection *c)
{
	tw->desktop = x_get_prop_int(c, c->root, 
			c->atoms[XATOM_NET_CURRENT_DESKTOP]);
}

static void update_tasks(struct widget *w, struct x_connection *c)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	Window *wins;
	int num;

	wins = x_get_prop_data(c, c->root, c->atoms[XATOM_NET_CLIENT_LIST], 
			XA_WINDOW, &num);

	size_t i;
	int j;
	for (i = 0; i < tw->tasks_n; ++i) {
		struct taskbar_task *t = &tw->tasks[i];
		int delete = 1;
		for (j = 0; j < num; ++j) {
			if (wins[j] == t->win) {
				delete = 0;
				break;
			}
		}
		if (delete)
			remove_task(tw, i--);
	}

	for (j = 0; j < num; ++j) {
		if (find_task_by_window(tw, wins[j]) == -1)
			add_task(w, c, wins[j]);
	}
	
	XFree(wins);
}

static int get_taskbar_task_at(struct widget *w, int x)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;

	size_t i;
	for (i = 0; i < tw->tasks_n; ++i) {
		struct taskbar_task *t = &tw->tasks[i];
		if (t->desktop != tw->desktop &&
		    t->desktop != -1)
		{
			continue;
		}

		if (x < (t->x + t->w) && x > t->x)
			return (int)i;
	}
	return -1;
}

/**************************************************************************
  Taskbar interface
**************************************************************************/

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree)
{
	struct taskbar_widget *tw = xmallocz(sizeof(struct taskbar_widget));
	if (parse_taskbar_theme(&tw->theme, e, tree)) {
		xfree(tw);
		XWARNING("Failed to parse taskbar theme");
		return -1;
	}

	INIT_ARRAY(tw->tasks, 50);
	w->private = tw;

	struct x_connection *c = &w->panel->connection;
	update_desktop(tw, c);
	update_active(tw, c);
	update_tasks(w, c);
	tw->dnd_win = None;
	tw->taken = None;
	tw->task_death_threshold = parse_int("task_death_threshold", 
					     &g_settings.root, 50);
	tw->task_urgency_hint = parse_bool("task_urgency_hint",
					   &g_settings.root);
	tw->dnd_cur = XCreateFontCursor(c->dpy, XC_fleur);
	tw->highlighted = -1;

	return 0;
}

static void destroy_widget_private(struct widget *w)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	free_taskbar_theme(&tw->theme);
	free_tasks(tw);
	XFreeCursor(w->panel->connection.dpy, tw->dnd_cur);
	xfree(tw);
}

static void draw(struct widget *w)
{
	/* I think it's a good idea to calculate all buttons positions here, and
	 * cache these data for later use in other message handlers. User
	 * interacts with what he/she sees, right? 
	 */
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	struct panel *p = w->panel;
	struct x_connection *c = &p->connection;
	cairo_t *cr = p->cr;

	int count = count_tasks_on_desktop(tw, tw->desktop);
	if (!count)
		return;

	int taskw = w->width / count;
	if (tw->theme.task_max_width && taskw > tw->theme.task_max_width)
		taskw = tw->theme.task_max_width;

	int x = w->x;
	size_t i;

	for (i = 0; i < tw->tasks_n; ++i) {
		struct taskbar_task *t = &tw->tasks[i];
		
		if (t->desktop != tw->desktop &&
		    t->desktop != -1)
		{
			continue;
		}

#define TASKS_NEED_CORRECTION (taskw != tw->theme.task_max_width)
		/* last task width correction */
		if (TASKS_NEED_CORRECTION &&
		    (i == tw->tasks_n - 1 || 
		     (t->desktop != -1 && tw->tasks[i+1].desktop != t->desktop)))
		{
			taskw = (w->x + w->width) - x;
		}
		
		/* save position for other events */
		t->x = x;
		t->w = taskw;

		/* set icon geometry */
		if (t->geom_x != t->x || t->geom_w != t->w) {
			t->geom_x = t->x;
			t->geom_w = t->w;

			long icon_geometry[4] = {
				w->panel->x + t->x,
				w->panel->y,
				t->w,
				w->panel->width
			};
			x_set_prop_array(c, t->win, c->atoms[XATOM_NET_WM_ICON_GEOMETRY],
					 icon_geometry, 4);
		}


		draw_task(t, tw, cr, w->panel->layout,
			  x, taskw, t->win == tw->active, i == tw->highlighted);
		x += taskw;
	}
}

static void prop_change(struct widget *w, XPropertyEvent *e)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	struct x_connection *c = &w->panel->connection;

	/* root window props */
	if (e->window == c->root) {
		if (e->atom == c->atoms[XATOM_NET_ACTIVE_WINDOW]) {
			update_active(tw, c);
			w->needs_expose = 1;
			return;
		}
		if (e->atom == c->atoms[XATOM_NET_CURRENT_DESKTOP]) {
			update_desktop(tw, c);
			w->needs_expose = 1;
			return;
		}
		if (e->atom == c->atoms[XATOM_NET_CLIENT_LIST]) {
			update_tasks(w, c);
			w->needs_expose = 1;
			return;
		}
	}

	/* check if it's our task */
	int ti = find_task_by_window(tw, e->window);
	if (ti == -1) {
		if (e->atom == c->atoms[XATOM_NET_WM_STATE] ||
		    e->atom == c->atoms[XATOM_NET_WM_WINDOW_TYPE]) {
			add_task(w, c, e->window);
		}
		return;
	}

	/* desktop changed (task was moved to other desktop) */
	if (e->atom == c->atoms[XATOM_NET_WM_DESKTOP]) {
		struct taskbar_task t = tw->tasks[ti];
		t.desktop = x_get_window_desktop(c, t.win);

		ARRAY_REMOVE(tw->tasks, (size_t)ti);
		int insert_after = find_last_task_by_desktop(tw, t.desktop);
		if (insert_after == -1)
			ARRAY_PREPEND(tw->tasks, t);
		else
			ARRAY_INSERT_AFTER(tw->tasks, (size_t)insert_after, t);
		w->needs_expose = 1;
		return;
	}

	/* task name was changed */
	if (e->atom == tw->tasks[ti].name_atom)
	{
		struct taskbar_task *t = &tw->tasks[ti];
		x_realloc_window_name(&t->name, c, t->win, 
				    &t->name_atom, &t->name_type_atom);
		w->needs_expose = 1;
		return;
	}

	/* icon was changed */
	if (tw->theme.default_icon) {
		if (e->atom == c->atoms[XATOM_NET_WM_ICON] ||
		    e->atom == XA_WM_HINTS)
		{
			struct taskbar_task *t = &tw->tasks[ti];
			cairo_surface_destroy(t->icon);
			t->icon = get_window_icon(c, t->win, tw->theme.default_icon);
			w->needs_expose = 1;
			return;
		}
	}

	if (e->atom == c->atoms[XATOM_NET_WM_STATE]) {
		struct taskbar_task *t = &tw->tasks[ti];
		if (x_is_window_hidden(c, t->win))
			remove_task(tw, ti);
		t->demands_attention = x_is_window_demands_attention(c, t->win);
		w->needs_expose = 1;
		return;
	}
}

static void button_click(struct widget *w, XButtonEvent *e)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	int ti = get_taskbar_task_at(w, e->x);
	if (ti == -1)
		return;
	struct taskbar_task *t = &tw->tasks[ti];
	struct x_connection *c = &w->panel->connection;

	if (e->button == 1 && e->type == ButtonRelease) {
		if (tw->active == t->win)
			XIconifyWindow(c->dpy, t->win, c->screen);
		else
			activate_task(c, t);
	}
}

static Window create_window_for_dnd(struct x_connection *c, int x, int y,
				    cairo_surface_t *icon)
{
	int iconw = image_width(icon);
	int iconh = image_width(icon);

	/* background with an icon */
	Pixmap bg = x_create_default_pixmap(c, iconw, iconh);
	cairo_t *cr = create_cairo_for_pixmap(c, bg, iconw, iconh);

	cairo_set_source_rgba(cr, 0,0,0,1);
	cairo_paint(cr);
	blit_image(icon, cr, 0, 0);
	cairo_destroy(cr);

	XSetWindowAttributes attrs;
	attrs.background_pixmap = bg;
	attrs.override_redirect = True;

	/* the window */
	Window win = x_create_default_window(c, x, y, iconw, iconh, 
			CWOverrideRedirect | CWBackPixmap, &attrs);
	XFreePixmap(c->dpy, bg);

	/* create shape for a window */
	Pixmap mask = XCreatePixmap(c->dpy, c->root, iconw, iconh, 1);
	cr = create_cairo_for_bitmap(c, mask, iconw, iconh);

	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
	cairo_set_source_rgba(cr, 0,0,0,0);
	cairo_paint(cr);
	blit_image(icon, cr, 0, 0);
	cairo_destroy(cr);

	XShapeCombineMask(c->dpy, win, ShapeBounding, 0, 0, mask, ShapeSet);
	XFreePixmap(c->dpy, mask);

	return win;
}

static void client_msg(struct widget *w, XClientMessageEvent *e)
{
	struct panel *p = w->panel;
	struct x_connection *c = &p->connection;
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;

	if (e->message_type == c->atoms[XATOM_XDND_POSITION]) {
		int x = (e->data.l[2] >> 16) & 0xFFFF;

		/* if it's not ours, skip.. */
		if ((x < (p->x + w->x)) || (x > (p->x + w->x + w->width)))
			return;
		
		int ti = get_taskbar_task_at(w, x - p->x);
		if (ti != -1) {
			struct taskbar_task *t = &tw->tasks[ti];
			if (t->win != tw->active)
				activate_task(c, t);
		}

		x_send_dnd_message(c, e->data.l[0], 
				   c->atoms[XATOM_XDND_STATUS],
				   p->win,
				   2, /* bits: 0 1 */
				   0, 0,
				   None);
	}
}

static void dnd_start(struct widget *w, struct drag_info *di)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	struct x_connection *c = &w->panel->connection;

	int ti = get_taskbar_task_at(di->taken_on, di->taken_x);
	if (ti == -1)
		return;

	struct taskbar_task *t = &tw->tasks[ti];
	if (t->icon) {
		tw->dnd_win = create_window_for_dnd(c, 
						    di->cur_root_x, 
						    di->cur_root_y,
						    t->icon);
		XMapWindow(c->dpy, tw->dnd_win);
	}

	XDefineCursor(c->dpy, w->panel->win, tw->dnd_cur);
	tw->taken = t->win;
}

static void dnd_drag(struct widget *w, struct drag_info *di)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	struct x_connection *c = &w->panel->connection;
	if (tw->dnd_win != None)
		XMoveWindow(c->dpy, tw->dnd_win, di->cur_root_x, di->cur_root_y);
}

static void dnd_drop(struct widget *w, struct drag_info *di)
{
	/* ignore dragged data from other widgets */
	if (di->taken_on != w)
		return;

	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	struct x_connection *c = &w->panel->connection;

	/* check if we have something draggable */
	if (tw->taken != None) {
		int taken = find_task_by_window(tw, tw->taken);
		int dropped = get_taskbar_task_at(w, di->dropped_x);
		if (di->taken_on == di->dropped_on && 
		    taken != -1 && dropped != -1 &&
		    tw->tasks[taken].desktop == tw->tasks[dropped].desktop)
		{
			/* if the desktop is the same.. move task */
			move_task(tw, taken, dropped);
			w->needs_expose = 1;
		} else if (!di->dropped_on && taken != -1) {
			/* out of the panel */
			if (di->cur_y < -tw->task_death_threshold || 
			    di->cur_y > w->panel->height + tw->task_death_threshold)
			{
				close_task(c, &tw->tasks[taken]);
			}
		}
	}

	XUndefineCursor(c->dpy, w->panel->win);

	if (tw->dnd_win != None) {
		XDestroyWindow(c->dpy, tw->dnd_win);
		tw->dnd_win = None;
	}
	tw->taken = None;
}

static void mouse_motion(struct widget *w, XMotionEvent *e)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	int i = get_taskbar_task_at(w, e->x);
	if (i != tw->highlighted) {
		tw->highlighted = i;
		w->needs_expose = 1;
	}
}

static void mouse_leave(struct widget *w)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	if (tw->highlighted != -1) {
		tw->highlighted = -1;
		w->needs_expose = 1;
	}
}

static void clock_tick(struct widget *w)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;
	size_t i;
	time_t seconds = time(0);
	for (i = 0; i < tw->tasks_n; ++i) {
		struct taskbar_task *t = &tw->tasks[i];
		if (t->demands_attention > 0) {
			w->needs_expose = 1;
			t->demands_attention = 1 + (seconds % 2);
		}
	}
}

static void reconfigure(struct widget *w)
{
	struct taskbar_widget *tw = (struct taskbar_widget*)w->private;

	tw->task_death_threshold = parse_int("task_death_threshold", 
					     &g_settings.root, 50);
	tw->task_urgency_hint = parse_bool("task_urgency_hint",
					   &g_settings.root);
}
