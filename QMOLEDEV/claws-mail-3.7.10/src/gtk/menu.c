/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "gtkcmoptionmenu.h"
#include "menu.h"
#include "utils.h"
#include "gtkutils.h"

#ifdef MAEMO
#ifdef CHINOOK
#include <hildon/hildon-program.h>
#else
#include <hildon-widgets/hildon-program.h>
#endif
#endif

GtkActionGroup *cm_menu_create_action_group(const gchar *name, GtkActionEntry *entries,
					    gint num_entries, gpointer data)
{
	GtkActionGroup *group = gtk_action_group_new(name);
	gtk_action_group_set_translate_func(group, menu_translate, NULL, NULL);
	gtk_action_group_add_actions(group, entries, num_entries, data);
	gtk_ui_manager_insert_action_group(gtkut_ui_manager(), group, 0);
	return group;
}

GtkActionGroup *cm_menu_create_action_group_full(GtkUIManager *manager, const gchar *name, GtkActionEntry *entries,
					    gint num_entries, gpointer data)
{
	GtkActionGroup *group = gtk_action_group_new(name);
	gtk_action_group_set_translate_func(group, menu_translate, NULL, NULL);
	gtk_action_group_add_actions(group, entries, num_entries, data);
	gtk_ui_manager_insert_action_group(manager, group, 0);
	return group;
}

gchar *menu_translate(const gchar *path, gpointer data)
{
	gchar *retval;

	retval = gettext(path);

	return retval;
}

void cm_menu_set_sensitive(gchar *menu, gboolean sensitive)
{
	GtkUIManager *gui_manager = gtkut_ui_manager();
	gchar *path = g_strdup_printf("Menus/%s", menu);

	cm_menu_set_sensitive_full(gui_manager, path, sensitive);
	g_free(path);
}

void cm_toggle_menu_set_active(gchar *menu, gboolean active)
{
	GtkUIManager *gui_manager = gtkut_ui_manager();
	gchar *path = g_strdup_printf("Menus/%s", menu);

	cm_toggle_menu_set_active_full(gui_manager, path, active);
	g_free(path);
}

void cm_menu_set_sensitive_full(GtkUIManager *gui_manager, gchar *menu, gboolean sensitive)
{
	GtkWidget *widget;
	gchar *path = g_strdup_printf("/%s/", menu);

	widget = gtk_ui_manager_get_widget(gui_manager, path);
	if( !GTK_IS_WIDGET(widget) ) {
		g_message("Blah, '%s' is not a widget.\n", path);
	}

	if( !GTK_IS_MENU_ITEM(widget) ) {
		g_message("Blah, '%s' is not a menu item.\n", path);
	}

	gtk_widget_set_sensitive(widget, sensitive);
	g_free(path);
}

void cm_toggle_menu_set_active_full(GtkUIManager *gui_manager, gchar *menu, gboolean active)
{
	GtkWidget *widget;
	gchar *path = g_strdup_printf("/%s/", menu);

	widget = gtk_ui_manager_get_widget(gui_manager, path);
	if( !GTK_IS_WIDGET(widget) ) {
		g_message("Blah, '%s' is not a widget.\n", path);
	}

	if( !GTK_CHECK_MENU_ITEM(widget) ) {
		g_message("Blah, '%s' is not a check menu item.\n", path);
	}

	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), active);
	g_free(path);
}

void menu_set_sensitive_all(GtkMenuShell *menu_shell, gboolean sensitive)
{
	GList *cur;

	for (cur = menu_shell->children; cur != NULL; cur = cur->next)
		gtk_widget_set_sensitive(GTK_WIDGET(cur->data), sensitive);
}

void menu_button_position(GtkMenu *menu, gint *x, gint *y, gboolean *push_in,
			  gpointer user_data)
{
        GtkWidget *widget;
        gint wheight;
        gint wx, wy;
	GtkRequisition mreq;
	GdkScreen *screen;
	GdkRectangle monitor;
	gint monitor_num;

	cm_return_if_fail(x && y);
 	cm_return_if_fail(GTK_IS_BUTTON(user_data));

	widget = GTK_WIDGET(user_data);

        gdk_window_get_origin(widget->window, x, y);
        wheight = widget->requisition.height;
        wx = widget->allocation.x;
        wy = widget->allocation.y;
        
	gtk_widget_size_request(GTK_WIDGET(menu), &mreq);
	screen = gtk_widget_get_screen (widget);
	monitor_num = gdk_screen_get_monitor_at_point (screen, *x, *y);
	gdk_screen_get_monitor_geometry (screen, monitor_num, 
					 &monitor);

        *x = *x + wx;
        *y = *y + wy + wheight;
	
	if (*y + mreq.height >= monitor.height)
		*y -= mreq.height;
}

gint menu_find_option_menu_index(GtkCMOptionMenu *optmenu, gpointer data,
				 GCompareFunc func)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	gpointer menu_data;
	GList *cur;
	gint n;

	menu = gtk_cmoption_menu_get_menu(optmenu);

	for (cur = GTK_MENU_SHELL(menu)->children, n = 0;
	     cur != NULL; cur = cur->next, n++) {
		menuitem = GTK_WIDGET(cur->data);
		menu_data = g_object_get_data(G_OBJECT(menuitem),
					      MENU_VAL_ID);
		if (func) {
			if (func(menu_data, data) == 0)
				return n;
		} else if (menu_data == data)
			return n;
	}

	return -1;
}
