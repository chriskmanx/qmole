/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _PANEL_H
#define _PANEL_H

typedef enum {
	PANEL_TOP,
	PANEL_BOTTOM,
	PANEL_LEFT,
	PANEL_RIGHT,

	PANEL_NUMBER_OF_SIDES,	/* (goes after valid sides) */

	PANEL_DEFAULT_SIDE	/* Read from file, or use free side */
} PanelSide;

#define MENU_MARGIN(side) ((side) == PANEL_BOTTOM ? 32 : 8)

struct _Panel {
	GtkWidget	*window;
	GtkAdjustment	*adj;		/* Scroll position of the bar */
	PanelSide	side;
	guchar		*name;		/* Leaf name */

	GtkWidget	*before;	/* Icons at the left/top end */
	GtkWidget	*after;		/* Icons at the right/bottom end */

	GtkWidget	*gap;		/* Event box between sides */

	int		autoscroll_to;	/* Timeout */
	int		autoscroll_speed; /* 0 => not scrolling */
	GdkRectangle	geometry;
	/* Options */
	int			style;		/* Possible values defined in panel.c */
	int			width;
	gboolean	xinerama;
	int			monitor;
	gboolean	avoid;
};

extern Panel *current_panel[PANEL_NUMBER_OF_SIDES];

void panel_init(void);
Panel *panel_new(const gchar *name, PanelSide side);
void panel_icon_may_update(Icon *icon);
void panel_save(Panel *panel);

gboolean panel_add(PanelSide side,
		   const gchar *path, const gchar *label, gboolean after, const gchar *shortcut, const gchar *args, 
		   gboolean locked);
gboolean panel_remove_item(PanelSide side, const gchar *path,
			   const gchar *label);
void panel_mark_used(GdkRegion *used);
void panel_update_size(void);

/* Side names here are English for implementation */
PanelSide panel_name_to_side(gchar *side);
const char *panel_side_to_name(PanelSide side);

GtkWidget *panel_new_panel_submenu(void);

#endif /* _PANEL_H */
