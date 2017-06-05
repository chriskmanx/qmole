/*
 * (SLIK) SimpLIstic sKin functions
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef UI_PATHSEL_H
#define UI_PATHSEL_H


GtkWidget *path_selection_new_with_files(GtkWidget *entry, const gchar *path,
					 const gchar *filter, const gchar *filter_desc);
GtkWidget *path_selection_new(const gchar *path, GtkWidget *entry);

void path_selection_sync_to_entry(GtkWidget *entry);

void path_selection_add_select_func(GtkWidget *entry,
				    void (*func)(const gchar *, gpointer), gpointer data);
void path_selection_add_filter(GtkWidget *entry, const gchar *filter, const gchar *description, gint set);
void path_selection_clear_filter(GtkWidget *entry);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
