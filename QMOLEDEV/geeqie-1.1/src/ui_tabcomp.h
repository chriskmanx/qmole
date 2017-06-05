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

#ifndef UI_TABCOMP_H
#define UI_TABCOMP_H


GtkWidget *tab_completion_new_with_history(GtkWidget **entry, const gchar *text,
					   const gchar *history_key, gint max_levels,
					   void (*enter_func)(const gchar *, gpointer), gpointer data);
const gchar *tab_completion_set_to_last_history(GtkWidget *entry);
void tab_completion_append_to_history(GtkWidget *entry, const gchar *path);

GtkWidget *tab_completion_new(GtkWidget **entry, const gchar *text,
			      void (*enter_func)(const gchar *, gpointer), gpointer data);
void tab_completion_add_to_entry(GtkWidget *entry, void (*enter_func)(const gchar *, gpointer), gpointer data);
void tab_completion_add_tab_func(GtkWidget *entry, void (*tab_func)(const gchar *, gpointer), gpointer data);
gchar *remove_trailing_slash(const gchar *path);

void tab_completion_add_select_button(GtkWidget *entry, const gchar *title, gboolean folders_only);
void tab_completion_add_append_func(GtkWidget *entry, void (*tab_append_func)(const gchar *, gpointer, gint), gpointer data);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
