/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _PINBOARD_H
#define _PINBOARD_H

extern Pinboard	*current_pinboard;

extern Icon *pinboard_drag_in_progress;

extern Option o_pinboard_tasklist_per_workspace;

extern PangoFontDescription *pinboard_font;
extern GdkColor pin_text_fg_col, pin_text_bg_col;

typedef enum {
	BACKDROP_NONE,
	BACKDROP_PROGRAM,
	BACKDROP_CENTRE, BACKDROP_SCALE, BACKDROP_STRETCH, BACKDROP_TILE, BACKDROP_FIT
} BackdropStyle;

void pinboard_init(void);
void pinboard_activate(const gchar *name);
void pinboard_pin(const gchar *path, const gchar *name, int x, int y,
		  const gchar *shortcut);
void pinboard_pin_with_args(const gchar *path, const gchar *name, int x, int y,
			   const gchar *shortcut, const char *arg, 
			   gboolean locked, gboolean update);
gboolean pinboard_remove(const gchar *path, const gchar *name);
void pinboard_move_icons(void);
const gchar *pinboard_get_name(void);
void pinboard_set_backdrop_app(const gchar *app);
GdkWindow *pinboard_get_window(void);
void pinboard_add_widget(GtkWidget *widget, const gchar *name);
void pinboard_moved_widget(GtkWidget *widget, const gchar *name,
			   int x, int y);
void pinboard_update_size(void);
void draw_label_shadow(WrappedLabel *wl, GdkRegion *region);
void pinboard_set_backdrop(const gchar *path, BackdropStyle style);

#endif /* _PINBOARD_H */
