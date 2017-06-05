#include "settings.h"
#include "builtin-widgets.h"

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
				 struct config_format_tree *tree);
static void destroy_widget_private(struct widget *w);
static void draw(struct widget *w);
static void mouse_motion(struct widget *w, XMotionEvent *e);
static void mouse_leave(struct widget *w);
static void button_click(struct widget *w, XButtonEvent *e);
static void reconfigure(struct widget *w);

struct widget_interface launchbar_interface = {
	.theme_name		= "launchbar",
	.size_type		= WIDGET_SIZE_CONSTANT,
	.create_widget_private	= create_widget_private,
	.destroy_widget_private = destroy_widget_private,
	.draw			= draw,
	.mouse_motion		= mouse_motion,
	.mouse_leave		= mouse_leave,
	.button_click		= button_click,
	.reconfigure		= reconfigure
};

static int get_item(struct launchbar_widget *lw, int x)
{
	size_t i;
	for (i = 0; i < lw->items_n; ++i) {
		struct launchbar_item *item = &lw->items[i];
		if (item->x < x && (item->x + item->w) > x)
			return (int)i;
	}
	return -1;
}

static int parse_items(struct launchbar_widget *lw)
{
	int items = 0;
	size_t i;
	struct config_format_entry *e = find_config_format_entry(&g_settings.root, 
								 "launchbar");
	if (!e)
		return 0;

	ENSURE_ARRAY_CAPACITY(lw->items, e->children_n);
	for (i = 0; i < e->children_n; ++i) {
		struct launchbar_item lbitem = {0,0,0,0};
		struct config_format_entry *ee = &e->children[i];

		/* if no exec path, skip */
		if (!ee->value)
			continue;

		/* if there is no icon, skip */
		const char *icon_path = find_config_format_entry_value(ee, "icon");
		if (!icon_path)
			continue;

		/* if can't load icon, skip */
		cairo_surface_t *icon = get_image(icon_path);
		if (!icon){

			continue;
		}

		lbitem.icon = copy_resized(icon, 
					   lw->icon_size[0], 
					   lw->icon_size[1]);
		cairo_surface_destroy(icon);
		lbitem.execstr = xstrdup(ee->value);

		ARRAY_APPEND(lw->items, lbitem);
		items++;
	}

	return items;
}

/**************************************************************************
  Launch Bar interface
**************************************************************************/

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
				 struct config_format_tree *tree)
{
	struct launchbar_widget *lw = xmallocz(sizeof(struct launchbar_widget));
	parse_2ints(lw->icon_size, "icon_size", e);

	INIT_EMPTY_ARRAY(lw->items);
	lw->active = -1;
	int items = parse_items(lw);

	w->width = items * (lw->icon_size[0] + 5) + 5;
	w->private = lw;

	return 0;
}

static void destroy_widget_private(struct widget *w)
{
	struct launchbar_widget *lw = (struct launchbar_widget*)w->private;
	size_t i;
	for (i = 0; i < lw->items_n; ++i) {
		cairo_surface_destroy(lw->items[i].icon);
		xfree(lw->items[i].execstr);
	}
	FREE_ARRAY(lw->items);
	xfree(lw);
}

static void reconfigure(struct widget *w)
{
	struct launchbar_widget *lw = (struct launchbar_widget*)w->private;

	/* free items */	
	size_t i;
	for (i = 0; i < lw->items_n; ++i) {
		cairo_surface_destroy(lw->items[i].icon);
		xfree(lw->items[i].execstr);
	}
	CLEAR_ARRAY(lw->items);
	lw->active = -1;
	int items = parse_items(lw);

	w->width = items * (lw->icon_size[0] + 5) + 5;
}

static void draw(struct widget *w)
{
	struct launchbar_widget *lw = (struct launchbar_widget*)w->private;
	size_t i;
	
	cairo_t *cr = w->panel->cr;
	int x = w->x + 5;
	int y = (w->panel->height - lw->icon_size[1]) / 2;
	for (i = 0; i < lw->items_n; ++i) {
		lw->items[i].x = x;
		lw->items[i].w = lw->icon_size[0];
		cairo_save(cr);
		cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
		blit_image(lw->items[i].icon, cr, x, y);
		if (i == lw->active) {
			cairo_save(cr);
			cairo_set_operator(cr, CAIRO_OPERATOR_ADD);
			cairo_set_source_rgba(cr, 1, 1, 1, 0.2);
			cairo_rectangle(cr, x, y, 
					lw->icon_size[0], lw->icon_size[1]);
			cairo_clip(cr);
			cairo_mask_surface(cr, lw->items[i].icon, x, y);
			cairo_restore(cr);
		}
		cairo_restore(cr);
		x += lw->icon_size[0] + 5; 
	}
}

static void mouse_motion(struct widget *w, XMotionEvent *e)
{
	struct launchbar_widget *lw = (struct launchbar_widget*)w->private;
	int cur = get_item(lw, e->x);
	if (cur != lw->active) {
		lw->active = cur;
		w->needs_expose = 1;
	}
}

static void mouse_leave(struct widget *w)
{
	struct launchbar_widget *lw = (struct launchbar_widget*)w->private;
	if (lw->active != -1) {
		lw->active = -1;
		w->needs_expose = 1;
	}
}

static void button_click(struct widget *w, XButtonEvent *e)
{
	struct launchbar_widget *lw = (struct launchbar_widget*)w->private;
	int cur = get_item(lw, e->x);
	if (cur == -1)
		return;

	if (e->button == 1 && e->type == ButtonRelease)
		g_spawn_command_line_async(lw->items[cur].execstr, 0);
}
