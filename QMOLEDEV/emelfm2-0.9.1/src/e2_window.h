/* $Id: e2_window.h 3055 2014-02-14 23:18:22Z tpgww $

Copyright (C) 2004-2014 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_WINDOW_H__
#define __E2_WINDOW_H__

#include "emelfm2.h"

typedef struct _E2_WindowRuntime
{
	GtkWidget *panes_outer_box;
	GtkWidget *panes_inner_box;
	GtkWidget *panes_paned;
	gdouble panes_paned_ratio;
	gdouble panes_paned_ratio_last;
	GtkWidget *output_paned;
	gdouble output_paned_ratio;
	gdouble output_paned_ratio_last;
	gboolean panes_horizontal;
} E2_WindowRuntime;

#ifdef E2_COMPOSIT
void e2_window_set_opacity (GtkWidget *window, gint level);
#endif
gboolean e2_window_output_show (GtkWidget *widget, gpointer user_data);
gboolean e2_window_output_hide (GtkWidget *widget, GdkEventFocus *event,
	gpointer user_data);
#ifdef USE_GTK3_0
gboolean e2_window_key_cb (GtkWidget *widget, GdkEventKey *event,
	gpointer user_data);
#endif
void e2_window_set_title (GtkWidget *widget, const gchar *title);
void e2_window_set_title_path (GtkWidget *wid, ViewInfo *view);
void e2_window_set_cursor(GdkCursorType type);
gboolean e2_window_update_status_bar (gpointer userdata);
void e2_window_enable_status_update (gint interval);
void e2_window_disable_status_update (void);
inline void e2_window_show_status_message (const gchar *message
#ifdef USE_GTK2_20
	, gboolean with_spinner
#endif
);
//void e2_window_remove_status_message (void);
void e2_window_clear_status_message (void);
//UNUSED void e2_window_adjust_output_pane_ratio (const gchar *arg);
void e2_window_adjust_pane_ratio (const gchar *arg);
void e2_window_create (E2_WindowRuntime *rt);
void e2_window_recreate (E2_WindowRuntime *rt);
void e2_window_actions_register (void);

#endif //ndef __E2_WINDOW_H__
