/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef FULLSCREEN_H
#define FULLSCREEN_H

#define FULL_SCREEN_HIDE_MOUSE_DELAY 3000
#define FULL_SCREEN_BUSY_MOUSE_DELAY 200


FullScreenData *fullscreen_start(GtkWidget *window, ImageWindow *imd,
				 void (*stop_func)(FullScreenData *, gpointer), gpointer stop_data);
void fullscreen_stop(FullScreenData *fs);


/* screen numbers for fullscreen_prefs are as follows:
 *   0  use default display size
 * 101  screen 0, monitor 0
 * 102  screen 0, monitor 1
 * 201  screen 1, monitor 0
 */


typedef struct _ScreenData ScreenData;
struct _ScreenData {
	gint number;
	gchar *description;
	gint x;
	gint y;
	gint width;
	gint height;
};


GList *fullscreen_prefs_list(void);
void fullscreen_prefs_list_free(GList *list);

ScreenData *fullscreen_prefs_list_find(GList *list, gint screen);

void fullscreen_prefs_get_geometry(gint screen, GtkWidget *widget, gint *x, gint *y, gint *width, gint *height,
				   GdkScreen **dest_screen, gboolean *same_region);

gint fullscreen_prefs_find_screen_for_widget(GtkWidget *widget);

GtkWidget *fullscreen_prefs_selection_new(const gchar *text, gint *screen_value, gboolean *above_value);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
