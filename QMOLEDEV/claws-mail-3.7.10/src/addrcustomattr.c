/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2007-2011 The Claws Mail Team
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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include <stdlib.h>

#include "menu.h"
#include "addrcustomattr.h"
#include "manage_window.h"
#include "prefs_common.h"
#include "alertpanel.h"
#include "addrbook.h"
#include "editaddress.h"

static GtkActionGroup *custom_attr_popup_action = NULL;
static GtkWidget *custom_attr_popup_menu = NULL;

static struct CustomAttrWindow
{
	GtkWidget *window;
	GtkWidget *attr_list;
	GtkWidget *hbox1;
	GtkWidget *hbox2;
	GtkWidget *vbox1;
	GtkWidget *label;
	GtkWidget *cancel_btn;
	GtkWidget *ok_btn;
	GtkWidget *add_entry;
	GtkWidget *add_btn;
} custom_attr_window;

enum {
	CUSTOM_ATTR_NAME,
	N_CUSTOM_ATTR
};

static gchar *default_addressbook_attributes_table[] = {
	N_("date of birth"),
	N_("address"),
	N_("phone"),
	N_("mobile phone"),
	N_("organization"),
	N_("office address"),
	N_("office phone"),
	N_("fax"),
	N_("website"),
	NULL
};

static gboolean dirty = FALSE;

static void custom_attr_window_create(void);
static void custom_attr_selected_attr_edited(GtkCellRendererText *widget,
		gchar *arg1, gchar *arg2,
		GtkWidget *list_view);
static void custom_attr_window_load_list(GList *list);
static void custom_attr_window_save_list (void);
static GList *custom_attr_default_list(void);

void addressbook_custom_attr_edit()
{
	if (!custom_attr_window.window)
		custom_attr_window_create();

	manage_window_set_transient(GTK_WINDOW(custom_attr_window.window));
	gtk_widget_grab_focus(custom_attr_window.ok_btn);
	
	custom_attr_window_load_list(prefs_common.addressbook_custom_attributes);

	gtk_widget_show(custom_attr_window.window);
	gtk_widget_grab_focus(custom_attr_window.attr_list);
	gtk_window_set_modal(GTK_WINDOW(custom_attr_window.window), TRUE);
}

static gint custom_attr_cmp_func (GtkTreeModel *model, GtkTreeIter *a,
								  GtkTreeIter *b, gpointer userdata)
 {
	gchar *name1, *name2;

	gtk_tree_model_get(model, a, CUSTOM_ATTR_NAME, &name1, -1);
	gtk_tree_model_get(model, b, CUSTOM_ATTR_NAME, &name2, -1);
	
	if (name1 == NULL)
		return name2 == NULL ? 0:1;
	
	if (name2 == NULL)
		return name1 == NULL ? 0:1;
	
	return g_utf8_collate(name1, name2);
}

static GtkListStore* custom_attr_window_create_data_store(void)
{
	GtkListStore *store = gtk_list_store_new(N_CUSTOM_ATTR,
				  G_TYPE_STRING,
				  -1);
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE(store);

	gtk_tree_sortable_set_sort_func(sortable, 0, custom_attr_cmp_func,
                                    NULL, NULL);

	return store;
}

static void custom_attr_window_create_list_view_columns(GtkWidget *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	g_object_set(G_OBJECT(renderer), "editable", TRUE, NULL);

	column = gtk_tree_view_column_new_with_attributes
		(_("Attribute name"),
		 renderer,
		 "text", CUSTOM_ATTR_NAME,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_set_search_column(GTK_TREE_VIEW(list_view),
					CUSTOM_ATTR_NAME);
	g_signal_connect(G_OBJECT(renderer), "edited",
			 G_CALLBACK(custom_attr_selected_attr_edited),
			 list_view);
}

static void custom_attr_window_list_view_clear_list(GtkWidget *list_view, gboolean warn)
{
	if (!warn || alertpanel(_("Delete all attribute names"),
		       _("Do you really want to delete all attribute names?"),
		       GTK_STOCK_CANCEL, GTK_STOCK_DELETE, NULL) == G_ALERTALTERNATE) {
		GtkListStore *list_store = GTK_LIST_STORE(gtk_tree_view_get_model
						(GTK_TREE_VIEW(list_view)));
		gtk_list_store_clear(list_store);
		dirty = TRUE;
	}
}

static void custom_attr_popup_clear_list (void *obj, void *data)
{
	custom_attr_window_list_view_clear_list(custom_attr_window.attr_list, TRUE);
}

static void custom_attr_popup_delete (void *obj, void *data)
{
	GtkTreeIter sel;
	GtkTreeModel *model;
	
	if (!gtk_tree_selection_get_selected(gtk_tree_view_get_selection
				(GTK_TREE_VIEW(custom_attr_window.attr_list)),
				&model, &sel))
		return;				

	if (alertpanel(_("Delete attribute name"),
		       _("Do you really want to delete this attribute name?"),
		       GTK_STOCK_CANCEL, GTK_STOCK_DELETE, NULL) == G_ALERTALTERNATE) {
		gtk_list_store_remove(GTK_LIST_STORE(model), &sel);
		dirty = TRUE;
	}
}

static void custom_attr_popup_factory_defaults (void *obj, void *data)
{
	if (alertpanel(_("Reset to default"),
		       _("Do you really want to replace all attribute names\nwith the default set?"),
		       GTK_STOCK_NO, GTK_STOCK_YES, NULL) == G_ALERTALTERNATE) {
		GList *tmp = custom_attr_default_list();
		custom_attr_window_load_list(tmp);
		if (tmp) {
			GList *cur;
			cur = tmp;
			while (cur) {
				g_free(cur->data);
				cur = cur->next;
			}
			g_list_free(tmp);
		}
		dirty = TRUE;
	}
}

static GtkActionEntry custom_attr_popup_entries[] =
{
	{"CustomAttrPopup",			NULL, "CustomAttrPopup" },
	{"CustomAttrPopup/Delete",		NULL, N_("_Delete"), NULL, NULL, G_CALLBACK(custom_attr_popup_delete) },
	{"CustomAttrPopup/DeleteAll",		NULL, N_("Delete _all"), NULL, NULL, G_CALLBACK(custom_attr_popup_clear_list) },
	{"CustomAttrPopup/Reset",		NULL, N_("_Reset to default"), NULL, NULL, G_CALLBACK(custom_attr_popup_factory_defaults) },
};

static gint custom_attr_list_btn_pressed(GtkWidget *widget, GdkEventButton *event,
				    GtkTreeView *list_view)
{
	if (event && event->button == 3) {
		GtkTreeModel *model = gtk_tree_view_get_model(list_view);
		GtkTreeIter iter;
		gboolean non_empty;

		if (!custom_attr_popup_menu) {
				custom_attr_popup_action = cm_menu_create_action_group("CustomAttrPopup", custom_attr_popup_entries,
					G_N_ELEMENTS(custom_attr_popup_entries), (gpointer)list_view);
				MENUITEM_ADDUI("/Menus", "CustomAttrPopup", "CustomAttrPopup", GTK_UI_MANAGER_MENU)
				MENUITEM_ADDUI("/Menus/CustomAttrPopup", "Delete", "CustomAttrPopup/Delete", GTK_UI_MANAGER_MENUITEM)
				MENUITEM_ADDUI("/Menus/CustomAttrPopup", "DeleteAll", "CustomAttrPopup/DeleteAll", GTK_UI_MANAGER_MENUITEM)
				MENUITEM_ADDUI("/Menus/CustomAttrPopup", "Reset", "CustomAttrPopup/Reset", GTK_UI_MANAGER_MENUITEM)
				custom_attr_popup_menu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
					gtk_ui_manager_get_widget(gtkut_ui_manager(), "/Menus/CustomAttrPopup")) );
		}

		/* grey out popup menu items if list is empty */
		non_empty = gtk_tree_model_get_iter_first(model, &iter);
		cm_menu_set_sensitive("CustomAttrPopup/Delete", non_empty);
		cm_menu_set_sensitive("CustomAttrPopup/DeleteAll", non_empty);

		gtk_menu_popup(GTK_MENU(custom_attr_popup_menu), 
			       NULL, NULL, NULL, NULL, 
			       event->button, event->time);

		return FALSE;
	}
	return FALSE;
}

static gboolean custom_attr_list_popup_menu(GtkWidget *widget, gpointer data)
{
	GtkTreeView *list_view = (GtkTreeView *)data;
	GdkEventButton event;
	
	event.button = 3;
	event.time = gtk_get_current_event_time();
	
	custom_attr_list_btn_pressed(NULL, &event, list_view);

	return TRUE;
}

static GtkWidget *custom_attr_window_list_view_create	(void)
{
	GtkTreeView *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(custom_attr_window_create_data_store());
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(model));
	g_object_unref(model);	
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model),
			CUSTOM_ATTR_NAME, GTK_SORT_ASCENDING);

	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	
	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);

	/* create the columns */
	custom_attr_window_create_list_view_columns(GTK_WIDGET(list_view));

#ifndef MAEMO
	g_signal_connect(G_OBJECT(list_view), "popup-menu",
			 G_CALLBACK(custom_attr_list_popup_menu), list_view);
#else
	gtk_widget_tap_and_hold_setup(GTK_WIDGET(list_view), NULL, NULL,
			GTK_TAP_AND_HOLD_NONE | GTK_TAP_AND_HOLD_NO_INTERNALS);
	g_signal_connect(G_OBJECT(list_view), "tap-and-hold",
			 G_CALLBACK(custom_attr_list_popup_menu), list_view);
#endif
	g_signal_connect(G_OBJECT(list_view), "button-press-event",
			G_CALLBACK(custom_attr_list_btn_pressed), list_view);
	return GTK_WIDGET(list_view);
}

static void custom_attr_window_close(void) 
{
	if (dirty)
		custom_attr_window_save_list();
	custom_attr_window_list_view_clear_list(custom_attr_window.attr_list, FALSE);
	gtk_widget_hide(custom_attr_window.window);
	gtk_window_set_modal(GTK_WINDOW(custom_attr_window.window), FALSE);
	if (dirty && !prefs_common.addressbook_use_editaddress_dialog)
		addressbook_edit_reload_attr_list();
}

static void custom_attr_window_cancel_cb(GtkWidget *widget,
			         gpointer data) 
{
	dirty = FALSE;
	custom_attr_window_close();
}

static void custom_attr_window_ok_cb(GtkWidget *widget,
			         gpointer data) 
{
	custom_attr_window_close();
}

static void custom_attr_selected_attr_edited(GtkCellRendererText *widget,
		gchar *path, gchar *new_text,
		GtkWidget *list_view)
{
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(list_view));
	SummaryView *summaryview = NULL;

	if (mainwindow_get_mainwindow() != NULL)
		summaryview = mainwindow_get_mainwindow()->summaryview;

	if (!gtk_tree_model_get_iter_from_string(model, &iter, path))
		return;

	if (!new_text || !*new_text)
		return;

	gtk_list_store_set(GTK_LIST_STORE(model), &iter,
			   CUSTOM_ATTR_NAME, new_text,
			   -1);
	dirty = TRUE;
}

typedef struct FindAttrInStore {
	gchar		*attr;
	GtkTreePath	*path;
	GtkTreeIter	 iter;
} FindAttrInStore;

static gboolean find_attr_in_store(GtkTreeModel *model,
				      GtkTreePath  *path,
				      GtkTreeIter  *iter,
				      FindAttrInStore *data)
{
	gchar *attr;
	gtk_tree_model_get(model, iter, CUSTOM_ATTR_NAME, &attr, -1);

	if (g_utf8_collate(data->attr, attr)==0) {
		data->path = path; /* signal we found it */
		data->iter = *iter;
		return TRUE;
	}
	return FALSE; 
}

static void custom_attr_window_add_attr(void)
{
	gchar *new_attr = gtk_editable_get_chars(GTK_EDITABLE(custom_attr_window.add_entry),
								0, -1);
	g_strstrip(new_attr);
	if (new_attr && *new_attr) {
		GtkListStore *list_store = GTK_LIST_STORE(gtk_tree_view_get_model
						(GTK_TREE_VIEW(custom_attr_window.attr_list)));
		FindAttrInStore fis;

		fis.attr = new_attr;
		fis.path = NULL;
		gtk_tree_model_foreach(gtk_tree_view_get_model
				(GTK_TREE_VIEW(custom_attr_window.attr_list)), 
				(GtkTreeModelForeachFunc) find_attr_in_store,
				&fis);

		if (fis.path) {
			/* activate existing one */
			GtkTreeSelection *selection;
			GtkTreePath* path;
			GtkTreeModel *model = gtk_tree_view_get_model(
				GTK_TREE_VIEW(custom_attr_window.attr_list));

			selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(custom_attr_window.attr_list));
			gtk_tree_selection_select_iter(selection, &fis.iter);

			path = gtk_tree_model_get_path(model, &fis.iter);
			/* XXX returned path may not be valid??? create new one to be sure */ 
			gtk_tree_view_set_cursor(GTK_TREE_VIEW(custom_attr_window.attr_list),
						path, NULL, FALSE);

			gtk_list_store_set(list_store, &fis.iter,
						CUSTOM_ATTR_NAME, new_attr,
						-1);

			gtk_tree_path_free(path);
		} else {
			/* append new */
			GtkTreeIter iter;

			gtk_list_store_append(list_store, &iter);
			gtk_list_store_set(list_store, &iter,
					   CUSTOM_ATTR_NAME, new_attr,
					   -1);
		}
		dirty = TRUE;
	} else {
		alertpanel_error(_("Attribute name is not set."));
	}
	g_free(new_attr);
}

static void custom_attr_window_add_attr_cb(GtkWidget *widget,
			         gpointer data) 
{
	custom_attr_window_add_attr();
	gtk_entry_set_text(GTK_ENTRY(custom_attr_window.add_entry), "");
	gtk_widget_grab_focus(custom_attr_window.attr_list);
}

static void custom_attr_window_del_attr_cb(GtkWidget *widget,
			         gpointer data) 
{
	custom_attr_popup_delete(NULL, NULL);
	gtk_widget_grab_focus(custom_attr_window.attr_list);
}

static gboolean custom_attr_window_key_pressed(GtkWidget *widget,
											   GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_Escape)
		custom_attr_window_close();
	else if (event && event->keyval == GDK_Delete)
		custom_attr_popup_delete(NULL, NULL);
	return FALSE;
}

static gboolean custom_attr_window_add_key_pressed(GtkWidget *widget,
												   GdkEventKey *event, gpointer data)
{
	if (event && (event->keyval == GDK_KP_Enter || event->keyval == GDK_Return)) {
		custom_attr_window_add_attr();
		gtk_entry_set_text(GTK_ENTRY(custom_attr_window.add_entry), "");
		gtk_widget_grab_focus(custom_attr_window.attr_list);
	}
	return FALSE;
}

static void custom_attr_window_create(void) 
{
	GtkWidget *window;
	GtkWidget *hbox1;
	GtkWidget *hbox2;
	GtkWidget *vbox1;
	GtkWidget *label;
	GtkWidget *attr_list;
	GtkWidget *cancel_btn;
	GtkWidget *ok_btn;
	GtkWidget *scrolledwin;
	GtkWidget *new_attr_label;
	GtkWidget *new_attr_entry;
	GtkWidget *add_btn;
	GtkWidget *del_btn;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "custom_attr_edit_window");
	gtk_window_set_title (GTK_WINDOW(window),
			      Q_("Dialog title|Edit attribute names"));

	gtk_container_set_border_width (GTK_CONTAINER (window), 8);
	gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW (window), TRUE);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(custom_attr_window_cancel_cb), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(custom_attr_window_key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT (window);

	vbox1 = gtk_vbox_new(FALSE, 6);
	hbox1 = gtk_hbox_new(FALSE, 6);
	
	new_attr_label = gtk_label_new(_("New attribute name:"));
	gtk_misc_set_alignment(GTK_MISC(new_attr_label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(hbox1), new_attr_label, FALSE, FALSE, 0);
	
	new_attr_entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox1), new_attr_entry, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(new_attr_entry), "key_press_event",
			 G_CALLBACK(custom_attr_window_add_key_pressed), NULL);
	
	add_btn = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_box_pack_start(GTK_BOX(hbox1), add_btn, FALSE, FALSE, 0);
	
	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_box_pack_start(GTK_BOX(hbox1), del_btn, FALSE, FALSE, 0);
	
	gtkut_stock_button_set_create(&hbox2, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);

	gtk_widget_show(new_attr_label);
	gtk_widget_show(new_attr_entry);
	gtk_widget_show(add_btn);
	gtk_widget_show(del_btn);
	gtk_widget_show(cancel_btn);
	gtk_widget_show(ok_btn);

	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(custom_attr_window_cancel_cb), NULL);
	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(custom_attr_window_ok_cb), NULL);
	g_signal_connect(G_OBJECT(add_btn), "clicked",
			 G_CALLBACK(custom_attr_window_add_attr_cb), NULL);
	g_signal_connect(G_OBJECT(del_btn), "clicked",
			 G_CALLBACK(custom_attr_window_del_attr_cb), NULL);

	attr_list = custom_attr_window_list_view_create();
	
	label = gtk_label_new(_("Adding or removing attribute names won't "
				"affect attributes already set for contacts."));
	gtk_widget_set_size_request(GTK_WIDGET(label), 380, -1);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox1), label, FALSE, TRUE, 0);
	
	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
				       
	gtk_widget_set_size_request(scrolledwin, 400, 250);

	gtk_container_add(GTK_CONTAINER(scrolledwin), attr_list);
	gtk_box_pack_start(GTK_BOX(vbox1), scrolledwin, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox1), hbox1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox1), hbox2, FALSE, FALSE, 0);
	
	gtk_widget_show(label);
	gtk_widget_show(scrolledwin);
	gtk_widget_show(attr_list);
	gtk_widget_show(hbox2);
	gtk_widget_show(hbox1);
	gtk_widget_show(vbox1);
	gtk_container_add(GTK_CONTAINER (window), vbox1);

	custom_attr_window.window = window;
	custom_attr_window.hbox1 = hbox1;
	custom_attr_window.hbox2 = hbox2;
	custom_attr_window.vbox1 = vbox1;
	custom_attr_window.label = label;
	custom_attr_window.attr_list = attr_list;
	custom_attr_window.cancel_btn = cancel_btn;
	custom_attr_window.ok_btn = ok_btn;
	custom_attr_window.add_btn = add_btn;
	custom_attr_window.add_entry = new_attr_entry;
}

static void custom_attr_window_load_list (GList *list) 
{
	/* copy attribute names list from prefs to store */
	GList *cur;
	GtkTreeIter iter;
	GtkListStore *list_store = GTK_LIST_STORE(gtk_tree_view_get_model
					(GTK_TREE_VIEW(custom_attr_window.attr_list)));

	custom_attr_window_list_view_clear_list(custom_attr_window.attr_list, FALSE);
	
	cur = list;
	while (cur) {
		gtk_list_store_append(list_store, &iter);
		gtk_list_store_set(list_store, &iter,
				   CUSTOM_ATTR_NAME, cur->data,
				   -1);
		cur = cur->next;
	}
}

static GList *store_to_glist = NULL;

static gboolean custom_attr_store_to_glist (GtkTreeModel *model,
				      GtkTreePath  *path,
				      GtkTreeIter  *iter,
				      gpointer     *data)
{
	gchar *attr;

	gtk_tree_model_get(model, iter, CUSTOM_ATTR_NAME, &attr, -1);
	if (attr) {
		store_to_glist = g_list_prepend(store_to_glist, g_strdup(attr));
	}
	return FALSE;
}

static void custom_attr_window_save_list (void) 
{
	GList *cur;

	/* clear existing attribute names list in prefs */
	cur = prefs_common.addressbook_custom_attributes;
	while (cur) {
		g_free(cur->data);
		cur = cur->next;
	}
	g_list_free(prefs_common.addressbook_custom_attributes);

	/* copy attribute names list from store to prefs */
	store_to_glist = store_to_glist;
	gtk_tree_model_foreach(gtk_tree_view_get_model
			(GTK_TREE_VIEW(custom_attr_window.attr_list)), 
			(GtkTreeModelForeachFunc) custom_attr_store_to_glist,
		    NULL);
	prefs_common.addressbook_custom_attributes = g_list_reverse(store_to_glist);
	store_to_glist = NULL;
}

static GList *custom_attr_default_list(void)
{
	/* returned GList must be deallocated by caller */
	GList *list;
	gint i;
	i = 0;

	list = NULL;
	while (default_addressbook_attributes_table[i]) {
		list = g_list_prepend(
			list, g_strdup(gettext(default_addressbook_attributes_table[i])));
		i++;
	}
	list = g_list_reverse(list);
	return list;
}

GList *addressbook_update_custom_attr_from_prefs(void)
{
	/* load addressbook custom attribute names list from file */
	/* use a list of default attribute names if storage file doesn't exist */
	GList *list;
	GList *default_attr_list;
	GList *cur;

	/* load table into glist */
	default_attr_list = custom_attr_default_list();

	list =	prefs_common_read_history_from_dir_with_defaults(ADDRBOOK_DIR,
				ADDRESSBOOK_CUSTOM_ATTRIBUTES,
				default_attr_list);

	/* free glist if it's the one we return (the default one) */
	if (list != default_attr_list) {
		cur = default_attr_list;
		while (cur) {
			g_free(cur->data);
			cur = cur->next;
		}
		g_list_free(default_attr_list);
	}
	return list;
}
