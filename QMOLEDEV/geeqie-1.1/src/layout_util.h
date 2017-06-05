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


#ifndef LAYOUT_UTIL_H
#define LAYOUT_UTIL_H


#include "layout.h"

gboolean layout_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data);

void layout_util_sync_thumb(LayoutWindow *lw);
void layout_util_sync_color(LayoutWindow *lw);
void layout_util_sync(LayoutWindow *lw);

void layout_util_status_update_write(LayoutWindow *lw);
void layout_util_status_update_write_all(void);

//void layout_edit_update_all(void);

void layout_recent_update_all(void);
void layout_recent_add_path(const gchar *path);

void layout_copy_path_update_all(void);

void layout_editors_reload_start(void);
void layout_editors_reload_finish(void);
void layout_actions_setup(LayoutWindow *lw);
void layout_actions_add_window(LayoutWindow *lw, GtkWidget *window);
GtkWidget *layout_actions_menu_bar(LayoutWindow *lw);
void layout_toolbar_add_from_config(LayoutWindow *lw, ToolbarType type, const gchar **attribute_names, const gchar **attribute_values);

GtkWidget *layout_actions_toolbar(LayoutWindow *lw, ToolbarType type);

void layout_toolbar_clear(LayoutWindow *lw, ToolbarType type);
void layout_toolbar_add(LayoutWindow *lw, ToolbarType type, const gchar *action);
void layout_toolbar_add_default(LayoutWindow *lw, ToolbarType type);
void layout_toolbar_write_config(LayoutWindow *lw, ToolbarType type, GString *outstr, gint indent);


void layout_keyboard_init(LayoutWindow *lw, GtkWidget *window);


void layout_bar_toggle(LayoutWindow *lw);
void layout_bar_set(LayoutWindow *lw, GtkWidget *bar);

void layout_bar_sort_toggle(LayoutWindow *lw);
void layout_bar_sort_set(LayoutWindow *lw, GtkWidget *bar);

void layout_bars_new_image(LayoutWindow *lw);
void layout_bars_new_selection(LayoutWindow *lw, gint count);

GtkWidget *layout_bars_prepare(LayoutWindow *lw, GtkWidget *image);
void layout_bars_close(LayoutWindow *lw);

void layout_exif_window_new(LayoutWindow *lw);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
