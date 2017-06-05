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


#ifndef MENU_H
#define MENU_H


gpointer submenu_item_get_data(GtkWidget *menu);

GtkWidget *submenu_add_edit(GtkWidget *menu, GtkWidget **menu_item, GCallback func, gpointer data, GList *fd_list);

gchar *sort_type_get_text(SortType method);
GtkWidget *submenu_add_sort(GtkWidget *menu, GCallback func, gpointer data,
			    gboolean include_none, gboolean include_path,
			    gboolean show_current, SortType type);

gchar *alter_type_get_text(AlterType type);
GtkWidget *submenu_add_alter(GtkWidget *menu, GCallback func, gpointer data);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
