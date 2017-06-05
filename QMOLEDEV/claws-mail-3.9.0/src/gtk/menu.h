/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __MENU_H__
#define __MENU_H__

#include <glib.h>
#include <gtk/gtk.h>
#if !GTK_CHECK_VERSION(3, 0, 0)
#include "gtkcmoptionmenu.h"
#endif

#define MENU_VAL_ID "Claws::Menu::ValueID"
#define MENU_VAL_DATA "Claws::Menu::ValueDATA"

#define MENUITEM_ADD(menu, menuitem, label, data) 		 \
{ 								 \
 	if (label)						 \
 		menuitem = gtk_menu_item_new_with_label(label);	 \
 	else {							 \
 		menuitem = gtk_menu_item_new();			 \
 		gtk_widget_set_sensitive(menuitem, FALSE);	 \
 	}							 \
	gtk_widget_show(menuitem); 				 \
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem); 	 \
	if (data) 						 \
		g_object_set_data(G_OBJECT(menuitem), 		 \
				  MENU_VAL_ID, 			 \
				  GINT_TO_POINTER(data)); 	 \
}

#define MENUITEM_ADDUI(path, name, action, type)		 \
	gtk_ui_manager_add_ui(gtkut_ui_manager(),		 \
			gtk_ui_manager_new_merge_id(gtkut_ui_manager()),\
			path, name, action, type, FALSE);

#define MENUITEM_ADDUI_MANAGER(ui_manager, path, name, action, type)	\
	gtk_ui_manager_add_ui(ui_manager,			 \
			gtk_ui_manager_new_merge_id(ui_manager), \
			path, name, action, type, FALSE);

#define MENUITEM_ADDUI_ID(path, name, action, type,id)		 \
	id = gtk_ui_manager_new_merge_id(gtkut_ui_manager());	 \
	gtk_ui_manager_add_ui(gtkut_ui_manager(),		 \
			id,					 \
			path, name, action, type, FALSE);

#define MENUITEM_REMUI(action_group,name,id) {			\
	GtkAction *action = gtk_action_group_get_action(action_group, name); \
	if (action) gtk_action_group_remove_action(action_group, action);	\
	if (id) gtk_ui_manager_remove_ui(gtkut_ui_manager(), id);	\
}

#define MENUITEM_ADDUI_ID_MANAGER(manager,path,name,action,type,id)		 \
	id = gtk_ui_manager_new_merge_id(manager);	 \
	gtk_ui_manager_add_ui(manager,		 \
			id,					 \
			path, name, action, type, FALSE);

#define MENUITEM_REMUI_MANAGER(manager,action_group,name,id) {			\
	GtkAction *action = gtk_action_group_get_action(action_group, name); \
	if (action) gtk_action_group_remove_action(action_group, action);	\
	if (id) gtk_ui_manager_remove_ui(manager, id);	\
}

#define menu_set_insensitive_all(menu_shell) \
	menu_set_sensitive_all(menu_shell, FALSE);

gchar *menu_translate		(const gchar *path, gpointer data);

GtkActionGroup *cm_menu_create_action_group(const gchar *name, GtkActionEntry *entries,
					    gint num_entries, gpointer data);
GtkActionGroup *cm_menu_create_action_group_full(GtkUIManager *ui_manager,
					    const gchar *name, GtkActionEntry *entries,
					    gint num_entries, gpointer data);
void cm_menu_set_sensitive(gchar *menu, gboolean sensitive);
void cm_toggle_menu_set_active(gchar *menu, gboolean active);
void cm_menu_set_sensitive_full(GtkUIManager *gui_manager, gchar *menu, gboolean sensitive);
void cm_toggle_menu_set_active_full(GtkUIManager *gui_manager, gchar *menu, gboolean active);
gchar *cm_menu_item_get_shortcut(GtkUIManager *gui_manager, gchar *menu);
GtkWidget *cm_menu_item_new_label_from_url(gchar *label);

#if !GTK_CHECK_VERSION(3, 0, 0)
gint menu_find_option_menu_index(GtkCMOptionMenu *optmenu, gpointer data,
				 GCompareFunc func);
#endif

void menu_button_position	(GtkMenu		*menu,
				 gint			*x,
				 gint			*y,
				 gboolean		*push_in,
				 gpointer		 user_data);

void menu_set_sensitive_all(GtkMenuShell *menu_shell, gboolean sensitive);

#endif /* __MENU_H__ */
