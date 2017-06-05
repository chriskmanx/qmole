/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _ICON_H
#define _ICON_H

#include <glib.h>
#include <X11/Xlib.h>

extern GList *icon_selection;
extern GtkWidget *icon_menu;		/* The popup icon menu */

typedef struct _IconClass IconClass;

typedef struct {
	KeyCode keycode;
	int modifier;
} MyKey;

struct _IconClass {
	GObjectClass parent;

	gboolean (*same_group)(Icon *icon, Icon *other);
	void (*destroy)(Icon *icon);
	void (*redraw)(Icon *icon);
	void (*update)(Icon *icon);
	void (*wink)(Icon *icon);

	/* Acts on selected items */
	void (*remove_items)(void);
};

struct _Icon {
	GObject		object;
	
	gboolean	selected;
	guchar		*src_path;	/* Eg: ~/Apps */
	guchar		*path;		/* Eg: /home/fred/Apps */
	DirItem		*item;
	gchar		*shortcut;	/* Eg: Control + x */
	MyKey		shortcut_key;	/* Parsed version of shortcut */
	gchar           *args;          /* Arguments, or NULL if none */
	gboolean	locked;		/* TRUE if the icon can't be removed */

	GtkWidget	*dialog;	/* Current rename box, if any */
};

GType icon_get_type(void);
gboolean icons_require(const gchar *path);
void icon_may_update(Icon *icon);
void icons_may_update(const gchar *path);

/* After options_item you may pass further MenuItems followed by NULL to
 * terminate list */
void icon_prepare_menu(Icon *icon, GtkWidget *options_item, ...);

void icon_set_selected(Icon *icon, gboolean selected);
void icon_select_only(Icon *select);
void icon_set_path(Icon *icon, const char *pathname, const char *name);
gchar *icon_create_uri_list(void);
void icon_destroy(Icon *icon);
void icon_set_shortcut(Icon *icon, const gchar *shortcut);
void icon_set_arguments(Icon *icon, const gchar *args);
void icon_run(Icon *icon);

#endif /* _ICON_H */
