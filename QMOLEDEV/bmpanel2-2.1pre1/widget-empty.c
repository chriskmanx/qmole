#include "builtin-widgets.h"

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree);
static void destroy_widget_private(struct widget *w);

struct widget_interface empty_interface = {
	.theme_name 		= "empty",
	.size_type 		= WIDGET_SIZE_CONSTANT,
	.create_widget_private 	= create_widget_private,
	.destroy_widget_private = destroy_widget_private
};

/**************************************************************************
  Empty interface
**************************************************************************/

static int create_widget_private(struct widget *w, struct config_format_entry *e, 
		struct config_format_tree *tree)
{
	if (!e->value) {
		XWARNING("Failed to parse empty widget (width is required)");
		return -1;
	}

	w->width = 0;
	if (sscanf(e->value, "%d", &w->width) != 1) {
		XWARNING("Failed to parse empty widget width, format is: empty %%d");
		return -1;
	}
	w->private = 0;
	return 0;
}

static void destroy_widget_private(struct widget *w)
{
}
