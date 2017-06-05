#include "builtin-widgets.h"

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree);
static void destroy_widget_private(struct widget *w);
static void draw(struct widget *w);

struct widget_interface decor_interface = {
	.theme_name 		= "decor",
	.size_type 		= WIDGET_SIZE_CONSTANT,
	.create_widget_private 	= create_widget_private,
	.destroy_widget_private = destroy_widget_private,
	.draw 			= draw
};

/**************************************************************************
  Decor interface
**************************************************************************/

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree)
{
	struct decor_widget *dw = xmallocz(sizeof(struct decor_widget));
	dw->image = parse_image_part(e, tree, 1);
	if (!dw->image) {
		xfree(dw);
		XWARNING("Failed to parse decor theme");
		return -1;
	}

	w->width = image_width(dw->image);
	w->private = dw;
	return 0;
}

static void destroy_widget_private(struct widget *w)
{
	struct decor_widget *dw = (struct decor_widget*)w->private;
	cairo_surface_destroy(dw->image);
	xfree(dw);
}

static void draw(struct widget *w)
{
	struct decor_widget *dw = (struct decor_widget*)w->private;
	cairo_t *cr = w->panel->cr;

	blit_image(dw->image, cr, w->x, 0);
}
