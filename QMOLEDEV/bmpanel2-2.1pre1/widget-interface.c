#include "gui.h"

#define MAX_WIDGET_INTERFACES PANEL_MAX_WIDGETS

static size_t widget_interfaces_n;
static struct widget_interface *widget_interfaces[MAX_WIDGET_INTERFACES];

int register_widget_interface(struct widget_interface *wc)
{
	if (widget_interfaces_n == MAX_WIDGET_INTERFACES)
		return XERROR("Widget interfaces limit was reached");
	widget_interfaces[widget_interfaces_n++] = wc;
	return 0;
}

struct widget_interface *lookup_widget_interface(const char *themename)
{
	size_t i;
	for (i = 0; i < widget_interfaces_n; ++i) {
		if (strcmp(themename, widget_interfaces[i]->theme_name) == 0)
			return widget_interfaces[i];
	}
	return 0;
}
