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


#ifndef UI_BOOKMARK_H
#define UI_BOOKMARK_H


/* bookmarks */

GtkWidget *bookmark_list_new(const gchar *key,
			     void (*select_func)(const gchar *path, gpointer data), gpointer select_data);
void bookmark_list_set_key(GtkWidget *list, const gchar *key);
void bookmark_list_set_no_defaults(GtkWidget *list, gint no_defaults);
void bookmark_list_set_editable(GtkWidget *list, gint editable);
void bookmark_list_set_only_directories(GtkWidget *list, gint only_directories);
void bookmark_list_add(GtkWidget *list, const gchar *name, const gchar *path);

/* allows apps to set up the defaults */
void bookmark_add_default(const gchar *name, const gchar *path);


/* history combo entry */

GtkWidget *history_combo_new(GtkWidget **entry, const gchar *text,
			     const gchar *history_key, gint max_levels);
void history_combo_append_history(GtkWidget *widget, const gchar *text);



#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
