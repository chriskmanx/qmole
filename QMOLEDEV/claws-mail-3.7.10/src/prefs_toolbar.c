/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2011 Hiroyuki Yamamoto & the Claws Mail team
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

/*
 * General functions for accessing address book files.
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

#include "stock_pixmap.h"
#include "manage_window.h"
#include "combobox.h"
#include "gtkutils.h"
#include "mainwindow.h"
#include "alertpanel.h"
#include "prefs_common.h"

#include "utils.h"

#include "toolbar.h"
#include "prefs_toolbar.h"
#include "prefswindow.h"
#include "prefs_gtk.h"
#include "plugin.h"

enum
{
	SET_ICON	  = 0,
	SET_FILENAME	  = 1,
	SET_TEXT	  = 2,
	SET_EVENT	  = 3,
	SET_ICON_TEXT	  = 4,		/*!< "icon" text (separator) */ 
	SET_ICON_IS_TEXT  = 5,		/*!< icon is text representation */
	N_SET_COLUMNS
};

enum
{
	ITEM_FUNCTION	  = 0,
	ITEM_USER_ACTION  = 1,
	ITEM_PLUGIN       = 2,
	ITEM_SEPARATOR	  = 3,
};

static const gint ToolbarIcons[] =
{
	STOCK_PIXMAP_ADDRESS_BOOK,
	STOCK_PIXMAP_ADDRESS_SEARCH,
	STOCK_PIXMAP_BOOK,
	STOCK_PIXMAP_CATEGORY,
	STOCK_PIXMAP_CHECK_SPELLING,
	STOCK_PIXMAP_CLOSE,
	STOCK_PIXMAP_DOWN_ARROW,
	STOCK_PIXMAP_UP_ARROW,
	STOCK_PIXMAP_EDIT_EXTERN,
	STOCK_PIXMAP_ERROR,
	STOCK_PIXMAP_EXEC,
	STOCK_PIXMAP_GROUP,
	STOCK_PIXMAP_INSERT_FILE,
	STOCK_PIXMAP_INTERFACE,
	STOCK_PIXMAP_JPILOT,
	STOCK_PIXMAP_LDAP,
	STOCK_PIXMAP_LINEWRAP_CURRENT,
	STOCK_PIXMAP_LINEWRAP_ALL,
	STOCK_PIXMAP_MAIL,
	STOCK_PIXMAP_MAIL_ATTACH,
	STOCK_PIXMAP_MAIL_COMPOSE,
	STOCK_PIXMAP_MAIL_FORWARD,
	STOCK_PIXMAP_MAIL_RECEIVE,
	STOCK_PIXMAP_MAIL_RECEIVE_ALL,
	STOCK_PIXMAP_MAIL_REPLY,
	STOCK_PIXMAP_MAIL_REPLY_TO_ALL,
	STOCK_PIXMAP_MAIL_REPLY_TO_AUTHOR,
	STOCK_PIXMAP_MAIL_REPLY_TO_LIST,
	STOCK_PIXMAP_MAIL_SEND,
	STOCK_PIXMAP_MAIL_SEND_QUEUE,
	STOCK_PIXMAP_MAIL_SIGN,
	STOCK_PIXMAP_OPEN_MAIL,
	STOCK_PIXMAP_NEWS_COMPOSE,
	STOCK_PIXMAP_PASTE,
	STOCK_PIXMAP_PREFERENCES,
	STOCK_PIXMAP_PROPERTIES,
	STOCK_PIXMAP_VCARD,
	STOCK_PIXMAP_ONLINE,
	STOCK_PIXMAP_OFFLINE,
	STOCK_PIXMAP_NOTICE_WARN,		/* small warning */
	STOCK_PIXMAP_NOTICE_ERROR,		/* small error   */
	STOCK_PIXMAP_NOTICE_NOTE,		/* small message */
	STOCK_PIXMAP_GO_FOLDERS,
	STOCK_PIXMAP_MIME_TEXT_PLAIN,
	STOCK_PIXMAP_MIME_TEXT_HTML,
	STOCK_PIXMAP_MIME_TEXT_PATCH,
	STOCK_PIXMAP_MIME_APPLICATION,
	STOCK_PIXMAP_MIME_IMAGE,
	STOCK_PIXMAP_MIME_AUDIO,
	STOCK_PIXMAP_MIME_TEXT_ENRICHED,
	STOCK_PIXMAP_MIME_UNKNOWN,
	STOCK_PIXMAP_MIME_PDF,
	STOCK_PIXMAP_MIME_PS,
	STOCK_PIXMAP_MIME_TEXT_CALENDAR,
	STOCK_PIXMAP_MIME_PGP_SIG,
	STOCK_PIXMAP_PRINTER,
	STOCK_PIXMAP_PRIVACY_SIGNED,
	STOCK_PIXMAP_PRIVACY_PASSED,
	STOCK_PIXMAP_PRIVACY_FAILED,
	STOCK_PIXMAP_PRIVACY_UNKNOWN,
	STOCK_PIXMAP_PRIVACY_EXPIRED,
	STOCK_PIXMAP_PRIVACY_WARN,
	STOCK_PIXMAP_PRIVACY_EMBLEM_SIGNED,
	STOCK_PIXMAP_PRIVACY_EMBLEM_PASSED,
	STOCK_PIXMAP_PRIVACY_EMBLEM_FAILED,
	STOCK_PIXMAP_PRIVACY_EMBLEM_WARN,
	STOCK_PIXMAP_MIME_MESSAGE,
	STOCK_PIXMAP_SPAM_BTN,
	STOCK_PIXMAP_HAM_BTN,
	STOCK_PIXMAP_TRASH,
	STOCK_PIXMAP_DELETE,
	STOCK_PIXMAP_CANCEL,
	STOCK_PIXMAP_EMPTY,              /* last entry */
};

typedef struct _ToolbarPage
{
	PrefsPage  page;

	GtkWidget *window;		/* do not modify */

	ToolbarType source;
	GList     *combo_action_list;	/* list of internal functions    */

	GtkWidget *list_view_set;	/* toolbar items treeview        */
	GtkWidget *item_text_entry;	/* item name                     */
	GtkWidget *item_type_combo;	/* item type selection widget    */
	GtkWidget *item_func_combo;	/* item internal function widget */
	GtkWidget *item_action_combo;	/* item user action widget       */
	GtkWidget *item_plugin_combo;   /* items registered by plugins */
	GtkWidget *icon_button;		/* item icon chooser widget      */
	
	GtkWidget *icon_chooser_win;
	GtkWidget *icon_chooser_view;
	
	gchar *item_icon_file;		/* item icon file                */

} ToolbarPage;

#define ERROR_MSG _("Selected Action already set.\nPlease choose another Action from List")
#define ERROR_MSG_NO_ICON _("Item has no icon defined.")
#define ERROR_MSG_NO_TEXT _("Item has no text defined.")

typedef struct _ToolbarPluginItem ToolbarPluginItem;
struct _ToolbarPluginItem {
	gchar *plugin;
	gchar *item_name;
	ToolbarPluginCallback cb;
	gpointer cb_data;
};

/* items registered by plugins */
static GHashTable *plugin_items_mainwin = NULL;
static GHashTable *plugin_items_compose = NULL;
static GHashTable *plugin_items_msgview = NULL;

static void prefs_toolbar_populate               (ToolbarPage *prefs_toolbar);

static void get_action_name			 (const gchar *entry, 
						  gchar **menu);
						  
static gboolean is_duplicate                     (ToolbarPage *prefs_toolbar,
						  gchar            *chosen_action);
static void prefs_toolbar_save                   (PrefsPage 	   *_page);

static void prefs_toolbar_register               (GtkButton        *button,
						  ToolbarPage *prefs_toolbar);
static void prefs_toolbar_substitute             (GtkButton        *button,
						  ToolbarPage *prefs_toolbar);
static void prefs_toolbar_delete                 (GtkButton        *button,
						  ToolbarPage *prefs_toolbar);

static void prefs_toolbar_up                     (GtkButton        *button,
						  ToolbarPage *prefs_toolbar);

static void prefs_toolbar_down                   (GtkButton        *button,
						  ToolbarPage *prefs_toolbar);

static void action_selection_changed		 (GtkComboBox *action_combo,
						  ToolbarPage *prefs_toolbar);
static void plugin_selection_changed		 (GtkComboBox *action_combo,
						  ToolbarPage *prefs_toolbar);

static void func_selection_changed		 (GtkComboBox *action_combo,
						  ToolbarPage *prefs_toolbar);

static void prefs_toolbar_create                 (ToolbarPage *prefs_toolbar);

static GtkWidget *create_set_list_view		 (ToolbarPage *prefs_toolbar);

static gboolean set_list_selected		 (GtkTreeSelection *selector,
						  GtkTreeModel *model, 
						  GtkTreePath *path,
						  gboolean currently_selected,
						  ToolbarPage *prefs_toolbar);

static void icon_chooser_create			 (GtkButton *button,
						  ToolbarPage *prefs_toolbar);


static GHashTable** get_plugin_hash_from_toolbar_type(ToolbarType toolbar_type)
{
	if (toolbar_type == TOOLBAR_MAIN)
		return &plugin_items_mainwin;
	else if (toolbar_type == TOOLBAR_COMPOSE)
		return &plugin_items_compose;
	else if (toolbar_type == TOOLBAR_MSGVIEW)
		return &plugin_items_msgview;
	else
		return NULL;
}

static void prefs_toolbar_create_widget(PrefsPage *_page, GtkWindow *window, gpointer data)
{
	ToolbarPage *prefs_toolbar = (ToolbarPage *) _page;
	gchar *win_titles[3];
	win_titles[TOOLBAR_MAIN]    = _("Main toolbar configuration");
	win_titles[TOOLBAR_COMPOSE] = _("Compose toolbar configuration");  
	win_titles[TOOLBAR_MSGVIEW] = _("Message view toolbar configuration");  

	prefs_toolbar->window = GTK_WIDGET(window);

	toolbar_read_config_file(prefs_toolbar->source);

	prefs_toolbar_create(prefs_toolbar);
	prefs_toolbar_populate(prefs_toolbar);
}

static void prefs_toolbar_save(PrefsPage *_page)
{
	ToolbarPage *prefs_toolbar = (ToolbarPage *) _page;
	GtkTreeView *list_view = GTK_TREE_VIEW(prefs_toolbar->list_view_set);
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	GtkTreeIter iter;
	
	toolbar_clear_list(prefs_toolbar->source);

	if (!gtk_tree_model_iter_n_children(model, NULL)
	||  !gtk_tree_model_get_iter_first(model, &iter))
		toolbar_set_default(prefs_toolbar->source);
	else {
		do {
			ToolbarItem *item;
			gchar *fname, *text, *event; 
			
			item = g_new0(ToolbarItem, 1);

			gtk_tree_model_get(model, &iter,
					   SET_FILENAME, &fname,
					   SET_TEXT, &text,
					   SET_EVENT, &event,
					   -1);

			/* XXX: remember that G_TYPE_STRING returned by model
			 * is owned by caller of gtk_tree_model_get() */
			item->file  = fname;
			item->text  = text;
			item->index = toolbar_ret_val_from_descr(event);
			g_free(event);

			/* TODO: save A_CLAWS_ACTIONS only if they are still active */
			toolbar_set_list_item(item, prefs_toolbar->source);

			g_free(item->file);
			g_free(item->text);
			g_free(item);
		} while (gtk_tree_model_iter_next(model, &iter));
	}

	toolbar_save_config_file(prefs_toolbar->source);

	if (prefs_toolbar->source == TOOLBAR_MAIN) 
		toolbar_update(TOOLBAR_MAIN, mainwindow_get_mainwindow());
	else if (prefs_toolbar->source == TOOLBAR_COMPOSE)
		compose_reflect_prefs_pixmap_theme();
	else if (prefs_toolbar->source == TOOLBAR_MSGVIEW)
		messageview_reflect_prefs_pixmap_theme();
}

static void prefs_toolbar_destroy_widget(PrefsPage *_page)
{
	ToolbarPage *prefs_toolbar = (ToolbarPage *) _page;

	g_list_free(prefs_toolbar->combo_action_list);
	prefs_toolbar->combo_action_list = NULL;
}

static void prefs_toolbar_set_displayed(ToolbarPage *prefs_toolbar)
{
	GSList *cur;
	GtkTreeView *list_view_set = GTK_TREE_VIEW(prefs_toolbar->list_view_set);
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model
						(list_view_set));
	GSList *toolbar_list = toolbar_get_list(prefs_toolbar->source);
	GtkTreeIter iter;

	gtk_list_store_clear(store);

	/* set currently active toolbar entries */
	for (cur = toolbar_list; cur != NULL; cur = cur->next) {
		ToolbarItem *item = (ToolbarItem*) cur->data;

		gtk_list_store_append(store, &iter);
	
		if (item->index != A_SEPARATOR) {
			GdkPixbuf *pix;
			StockPixmap icon = stock_pixmap_get_icon(item->file);
			
			stock_pixbuf_gdk(prefs_toolbar->window, icon, &pix);

			gtk_list_store_set(store, &iter, 
					   SET_ICON, pix,
					   SET_FILENAME, item->file,
					   SET_TEXT, item->text,
					   SET_EVENT, toolbar_ret_descr_from_val(item->index),
					   SET_ICON_TEXT, NULL,	
					   SET_ICON_IS_TEXT, FALSE,
					   -1);
		} else {
			gtk_list_store_set(store, &iter,
					   SET_ICON, NULL,
					   SET_FILENAME, toolbar_ret_descr_from_val(A_SEPARATOR),
					   SET_TEXT, (const gchar *) "", 
					   SET_EVENT, toolbar_ret_descr_from_val(A_SEPARATOR),
					   SET_ICON_TEXT, (const gchar *) SEPARATOR_PIXMAP,
					   SET_ICON_IS_TEXT, TRUE,
					   -1);
		}
	}

	/* select first */
	gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store), &iter);
	gtk_tree_selection_select_iter(gtk_tree_view_get_selection
						(list_view_set),
				       &iter);	
}

static void add_item_to_plugin_combo(gpointer key, gpointer data, gpointer combo_box)
{
	gtk_combo_box_append_text(GTK_COMBO_BOX(combo_box), (const gchar*)key);
}

static void prefs_toolbar_populate(ToolbarPage *prefs_toolbar)
{
	GSList *cur;
	gchar *act, *act_name;
	GHashTable **hash;

	prefs_toolbar->combo_action_list = toolbar_get_action_items(prefs_toolbar->source);
	combobox_set_popdown_strings(GTK_COMBO_BOX(prefs_toolbar->item_func_combo),
				     prefs_toolbar->combo_action_list);
	
	/* get currently defined sylpheed actions */
	if (prefs_common.actions_list != NULL) {
		for (cur = prefs_common.actions_list; cur != NULL; cur = cur->next) {
			act = (gchar *)cur->data;
			get_action_name(act, &act_name);
			
			gtk_combo_box_append_text(
				GTK_COMBO_BOX(prefs_toolbar->item_action_combo),
				act_name);

			g_free(act_name);
		} 

	} else
		combobox_set_sensitive(GTK_COMBO_BOX(prefs_toolbar->item_type_combo),
					ITEM_USER_ACTION, FALSE);
	
	/* items registered by plugins */
	hash = get_plugin_hash_from_toolbar_type(prefs_toolbar->source);
	if (hash && *hash)
		g_hash_table_foreach(*hash, add_item_to_plugin_combo, 
				prefs_toolbar->item_plugin_combo);
	else
		combobox_set_sensitive(GTK_COMBO_BOX(prefs_toolbar->item_type_combo),
					ITEM_PLUGIN, FALSE);

	gtk_combo_box_set_active(GTK_COMBO_BOX(prefs_toolbar->item_func_combo), 0);
	gtk_combo_box_set_active(GTK_COMBO_BOX(prefs_toolbar->item_action_combo), 0);
	gtk_combo_box_set_active(GTK_COMBO_BOX(prefs_toolbar->item_plugin_combo), 0);
	
	prefs_toolbar_set_displayed(prefs_toolbar);

	toolbar_clear_list(prefs_toolbar->source);
}

static gboolean is_duplicate(ToolbarPage *prefs_toolbar, gchar *chosen_action)
{
	GtkTreeView *list_view_set = GTK_TREE_VIEW
					(prefs_toolbar->list_view_set);
	GtkTreeModel *model_set = gtk_tree_view_get_model(list_view_set);					
	gchar *entry;
	GtkTreeIter iter;
	gboolean result;

	cm_return_val_if_fail(chosen_action != NULL, TRUE);

	if (!gtk_tree_model_iter_n_children(model_set, NULL))
		return FALSE;
	
	if (!gtk_tree_model_get_iter_first(model_set, &iter))
		return FALSE;

	result = FALSE;
	do {
		gtk_tree_model_get(model_set, &iter,
				   SET_EVENT, &entry, 
				   -1);
		if (g_utf8_collate(chosen_action, entry) == 0) 
			result = TRUE;
		g_free(entry);			
	} while (!result && gtk_tree_model_iter_next(model_set, &iter));

	return result;
}

static void prefs_toolbar_default(GtkButton *button, ToolbarPage *prefs_toolbar)
{
	toolbar_clear_list(prefs_toolbar->source);
	toolbar_set_default(prefs_toolbar->source);
	prefs_toolbar_set_displayed(prefs_toolbar);
}

/*!
 *\return	String that should be freed by caller.
 */
static void get_action_name(const gchar *entry, gchar **menu)
{
	gchar *act, *act_p;
	
	*menu = NULL;

	if (prefs_common.actions_list != NULL) {
		
		act = g_strdup(entry);
		act_p = strstr(act, ": ");
		if (act_p != NULL)
			act_p[0] = 0x00;
		/* freed by calling func */
		*menu = act;
	}
}

static void prefs_toolbar_register(GtkButton *button, ToolbarPage *prefs_toolbar)
{
	GtkTreeView *list_view_set   = GTK_TREE_VIEW(prefs_toolbar->list_view_set);
	gint item_type = gtk_combo_box_get_active(GTK_COMBO_BOX(prefs_toolbar->item_type_combo));
	GtkListStore *store_set;
	GtkTreeIter iter;

	store_set = GTK_LIST_STORE(gtk_tree_view_get_model(list_view_set));

	/* SEPARATOR or other ? */
	if (item_type == ITEM_SEPARATOR) {
		gtk_list_store_append(store_set, &iter);
		gtk_list_store_set(store_set, &iter,
				   SET_ICON, NULL,
				   SET_FILENAME, prefs_toolbar->item_icon_file,
				   SET_TEXT, NULL,
				   SET_EVENT, toolbar_ret_descr_from_val(A_SEPARATOR),
				   SET_ICON_TEXT, (const gchar *) SEPARATOR_PIXMAP,
				   SET_ICON_IS_TEXT, TRUE,
				   -1);
	} else {
		GdkPixbuf *pixbuf;
		gchar *event, *text;

		if (prefs_toolbar->item_icon_file == NULL) {
			alertpanel_error(ERROR_MSG_NO_ICON);
			return;
		}
		stock_pixbuf_gdk(prefs_toolbar->window, 
				 stock_pixmap_get_icon(prefs_toolbar->item_icon_file),
				 &pixbuf);
		if(pixbuf == NULL) {
			alertpanel_error(ERROR_MSG_NO_ICON);
			return;
		}
				
		if (item_type == ITEM_FUNCTION) {
			event = gtk_combo_box_get_active_text(GTK_COMBO_BOX(
						prefs_toolbar->item_func_combo));
						
			if (is_duplicate(prefs_toolbar, event)) {
				alertpanel_error(ERROR_MSG);
				g_free(event);
				return;
			}
		} else if(item_type == ITEM_PLUGIN)
		  event = toolbar_ret_descr_from_val(A_CLAWS_PLUGINS);
		else
			event = toolbar_ret_descr_from_val(A_CLAWS_ACTIONS);
		
		text = gtk_editable_get_chars(
			GTK_EDITABLE(prefs_toolbar->item_text_entry), 0 , -1);

		if (text != NULL) {
			gtk_list_store_append(store_set, &iter);
			gtk_list_store_set(store_set, &iter,
					   SET_ICON, pixbuf,
					   SET_FILENAME, prefs_toolbar->item_icon_file,
					   SET_TEXT, text,
					   SET_EVENT, event,
					   SET_ICON_TEXT, NULL,
					   SET_ICON_IS_TEXT, FALSE,
					   -1);
		} else {
			alertpanel_error(ERROR_MSG_NO_TEXT);
			return;
		}
		
		g_free(text);
		if((item_type != ITEM_USER_ACTION) && (item_type != ITEM_PLUGIN))
			g_free(event);
	}
	
	gtk_tree_selection_select_iter(gtk_tree_view_get_selection
						(list_view_set),
				       &iter);
}

static void prefs_toolbar_substitute(GtkButton *button, ToolbarPage *prefs_toolbar)
{
	GtkTreeView *list_view_set   = GTK_TREE_VIEW(prefs_toolbar->list_view_set);
	GtkListStore *store_set   = GTK_LIST_STORE(gtk_tree_view_get_model(list_view_set));
	gint item_type = gtk_combo_box_get_active(GTK_COMBO_BOX(prefs_toolbar->item_type_combo));
	GtkTreeSelection *sel_set;
	GtkTreeIter iter_set;

	if (!gtk_tree_model_iter_n_children(GTK_TREE_MODEL(store_set), NULL))
		return;
		
	sel_set = gtk_tree_view_get_selection(list_view_set);		
	if (!gtk_tree_selection_get_selected(sel_set, NULL, &iter_set))
		return;

	if (item_type == ITEM_SEPARATOR) {
		gtk_list_store_set(store_set, &iter_set, 
				   SET_ICON, NULL,
				   SET_TEXT, NULL,
				   SET_EVENT, toolbar_ret_descr_from_val(A_SEPARATOR),
				   SET_FILENAME, prefs_toolbar->item_icon_file,
				   SET_ICON_TEXT, (const gchar *) SEPARATOR_PIXMAP,
				   SET_ICON_IS_TEXT, TRUE,
				   -1);
	} else {
		GdkPixbuf *pixbuf;
		gchar *icon_event, *set_event, *text;

		if (prefs_toolbar->item_icon_file == NULL) {
			alertpanel_error(ERROR_MSG_NO_ICON);
			return;
		}
		stock_pixbuf_gdk(prefs_toolbar->window, 
				 stock_pixmap_get_icon(prefs_toolbar->item_icon_file),
				 &pixbuf);
		if(pixbuf == NULL) {
			alertpanel_error(ERROR_MSG_NO_ICON);
			return;
		}

		gtk_tree_model_get(GTK_TREE_MODEL(store_set), &iter_set, 
						  SET_EVENT, &set_event,
						  -1);
		
		if (item_type == ITEM_FUNCTION) {
			icon_event = gtk_combo_box_get_active_text(GTK_COMBO_BOX(
						prefs_toolbar->item_func_combo));
						
			if (is_duplicate(prefs_toolbar, icon_event)
			&& g_utf8_collate(icon_event, set_event) != 0){
				alertpanel_error(ERROR_MSG);
				g_free(icon_event);
				g_free(set_event);
				return;
			}
		} else if(item_type == ITEM_PLUGIN)
			icon_event = toolbar_ret_descr_from_val(A_CLAWS_PLUGINS);
		else
			icon_event = toolbar_ret_descr_from_val(A_CLAWS_ACTIONS);
		
		text = gtk_editable_get_chars(
			GTK_EDITABLE(prefs_toolbar->item_text_entry), 0 , -1);

		/* change the row */
		if (text != NULL) {
			gtk_list_store_set(store_set, &iter_set,
					   SET_ICON, pixbuf,
					   SET_FILENAME, prefs_toolbar->item_icon_file,
					   SET_TEXT, text,
					   SET_EVENT, icon_event,
					   SET_ICON_TEXT, NULL,
					   SET_ICON_IS_TEXT, FALSE,
					   -1);
		} else 
			alertpanel_error(ERROR_MSG_NO_TEXT);
				
		g_free(text);
		g_free(set_event);
		if((item_type != ITEM_USER_ACTION) && (item_type != ITEM_PLUGIN))
			g_free(icon_event);
	}
}

static void prefs_toolbar_delete(GtkButton *button, ToolbarPage *prefs_toolbar)
{
	GtkTreeView *list_view_set = GTK_TREE_VIEW(prefs_toolbar->list_view_set);
	GtkTreeModel *store_set = gtk_tree_view_get_model(list_view_set);
	GtkTreeIter iter_set;
	GtkTreePath *path;							

	if (!gtk_tree_model_iter_n_children(GTK_TREE_MODEL(store_set), NULL))
		return;
	
	if (!gtk_tree_selection_get_selected(gtk_tree_view_get_selection
							(list_view_set),
					     NULL,
					     &iter_set))
		return;					     

	/* select prev list item, if deleted was first select next */
	path = gtk_tree_model_get_path(store_set, &iter_set);
	gtk_tree_path_prev(path);
	
	gtk_list_store_remove(GTK_LIST_STORE(store_set), &iter_set);

	gtk_tree_selection_select_path(
			gtk_tree_view_get_selection(list_view_set),
			path);

	gtk_tree_path_free(path);
}

static void prefs_toolbar_up(GtkButton *button, ToolbarPage *prefs_toolbar)
{
	GtkTreePath *prev, *sel;
	GtkTreeIter isel;
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter iprev;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(prefs_toolbar->list_view_set)),
		 &model,	
		 &isel))
		return;
	store = (GtkListStore *)model;

	sel = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &isel);
	if (!sel)
		return;
	
	/* no move if we're at row 0... */
	prev = gtk_tree_path_copy(sel);
	if (!gtk_tree_path_prev(prev)) {
		gtk_tree_path_free(prev);
		gtk_tree_path_free(sel);
		return;
	}

	gtk_tree_model_get_iter(GTK_TREE_MODEL(store),
				&iprev, prev);
	gtk_tree_path_free(sel);
	gtk_tree_path_free(prev);

	gtk_list_store_swap(store, &iprev, &isel);
}

static void prefs_toolbar_down(GtkButton *button, ToolbarPage *prefs_toolbar)
{
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter next, sel;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(prefs_toolbar->list_view_set)),
		 &model,
		 &sel))
		return;

	store = (GtkListStore *)model;
	next = sel;
	if (!gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &next)) 
		return;

	gtk_list_store_swap(store, &next, &sel);
}

static void item_type_changed(GtkComboBox *item_type_combo,
				ToolbarPage *prefs_toolbar)
{
	gint active = gtk_combo_box_get_active(item_type_combo);

	switch(active) {
	case ITEM_FUNCTION:
		gtk_widget_show(prefs_toolbar->item_func_combo);
		gtk_widget_hide(prefs_toolbar->item_action_combo);
		gtk_widget_hide(prefs_toolbar->item_plugin_combo);
		gtk_combo_box_set_active(
			GTK_COMBO_BOX(prefs_toolbar->item_func_combo), 0);
		gtk_button_set_label(GTK_BUTTON(prefs_toolbar->icon_button), "");
		gtk_widget_set_sensitive(prefs_toolbar->item_text_entry, TRUE);
		gtk_widget_set_sensitive(prefs_toolbar->item_func_combo, TRUE);
		gtk_widget_set_sensitive(prefs_toolbar->icon_button, TRUE);

		func_selection_changed(GTK_COMBO_BOX(prefs_toolbar->item_func_combo),
					prefs_toolbar);
		break;
	case ITEM_USER_ACTION:
		gtk_widget_show(prefs_toolbar->item_action_combo);
		gtk_widget_hide(prefs_toolbar->item_func_combo);
		gtk_widget_hide(prefs_toolbar->item_plugin_combo);
		gtk_combo_box_set_active(
			GTK_COMBO_BOX(prefs_toolbar->item_action_combo), 0);
		gtk_button_set_label(GTK_BUTTON(prefs_toolbar->icon_button), "");
		gtk_widget_set_sensitive(prefs_toolbar->item_text_entry, FALSE);
		gtk_widget_set_sensitive(prefs_toolbar->item_action_combo, TRUE);
		gtk_widget_set_sensitive(prefs_toolbar->icon_button, TRUE);
		
		action_selection_changed(GTK_COMBO_BOX(prefs_toolbar->item_action_combo),
					prefs_toolbar);		
		break;
	case ITEM_SEPARATOR:
		gtk_button_set_label(GTK_BUTTON(prefs_toolbar->icon_button), _("None"));
		gtk_button_set_image(GTK_BUTTON(prefs_toolbar->icon_button), NULL);
		g_free(prefs_toolbar->item_icon_file);
		prefs_toolbar->item_icon_file = NULL;
		gtk_combo_box_set_active(
			GTK_COMBO_BOX(prefs_toolbar->item_func_combo), -1);
		gtk_combo_box_set_active(
			GTK_COMBO_BOX(prefs_toolbar->item_action_combo), -1);
		gtk_combo_box_set_active(
			GTK_COMBO_BOX(prefs_toolbar->item_plugin_combo), -1);
		gtk_entry_set_text(GTK_ENTRY(prefs_toolbar->item_text_entry), "");
		gtk_widget_set_sensitive(prefs_toolbar->item_action_combo, FALSE);
		gtk_widget_set_sensitive(prefs_toolbar->item_text_entry, FALSE);
		gtk_widget_set_sensitive(prefs_toolbar->item_plugin_combo, FALSE);
		gtk_widget_set_sensitive(prefs_toolbar->item_func_combo, FALSE);
		gtk_widget_set_sensitive(prefs_toolbar->icon_button, FALSE);
		break;
	  case ITEM_PLUGIN:
	  	gtk_widget_show(prefs_toolbar->item_plugin_combo);
	  	gtk_widget_hide(prefs_toolbar->item_func_combo);
	  	gtk_widget_hide(prefs_toolbar->item_action_combo);
	  	gtk_combo_box_set_active(GTK_COMBO_BOX(prefs_toolbar->item_plugin_combo), 0);
	  	gtk_button_set_label(GTK_BUTTON(prefs_toolbar->icon_button), "");
	  	gtk_widget_set_sensitive(prefs_toolbar->item_text_entry, FALSE);
	  	gtk_widget_set_sensitive(prefs_toolbar->item_plugin_combo, TRUE);
	  	gtk_widget_set_sensitive(prefs_toolbar->icon_button, TRUE);
	  	plugin_selection_changed(GTK_COMBO_BOX(prefs_toolbar->item_plugin_combo), prefs_toolbar);
	  	break;
	}

}

static void action_selection_changed(GtkComboBox *action_combo,
				ToolbarPage *prefs_toolbar)
{
	gchar *text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(
			   prefs_toolbar->item_action_combo));

	if(text != NULL) { /* action */
		gtk_entry_set_text(GTK_ENTRY(prefs_toolbar->item_text_entry), text);
		g_free(text);
	} 
}

static void plugin_selection_changed(GtkComboBox *action_combo,
                ToolbarPage *prefs_toolbar)
{
	gchar *text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(prefs_toolbar->item_plugin_combo));

	if (text != NULL) { /* action */
		gtk_entry_set_text(GTK_ENTRY(prefs_toolbar->item_text_entry), text);
		g_free(text);
	}
}

static void func_selection_changed(GtkComboBox *action_combo,
				ToolbarPage *prefs_toolbar)
{
	gchar *text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(
			   prefs_toolbar->item_func_combo));

	if(text != NULL) { /* action */
		int action = -1;
		action = toolbar_ret_val_from_descr(text);
		if (action >= 0)
			gtk_entry_set_text(GTK_ENTRY(prefs_toolbar->item_text_entry), 
					toolbar_get_short_text(action));
		g_free(text);
		if (action >= 0) {
			StockPixmap stockp = toolbar_get_icon(action);
			if (stockp >= 0)  {
				g_free(prefs_toolbar->item_icon_file);
				prefs_toolbar->item_icon_file = g_strdup(stock_pixmap_get_name(stockp));

				gtk_button_set_image(GTK_BUTTON(prefs_toolbar->icon_button),
				     stock_pixmap_widget(prefs_toolbar->window, stockp));
			}
		}
	} 
}

static void prefs_toolbar_create(ToolbarPage *prefs_toolbar)
{
	GtkWidget *main_vbox;
	GtkWidget *toolbar_item_hbox;
	GtkWidget *icon_vbox;
	GtkWidget *icon_label;
	GtkWidget *icon_button;
	GtkWidget *icon_hbox;
	GtkWidget *item_type_combo;
	GtkListStore *item_type_model;
	GtkTreeIter iter;
	GtkWidget *item_action_combo;
	GtkWidget *item_plugin_combo;
	GtkWidget *item_func_combo;
	GtkWidget *reg_hbox;
	GtkWidget *arrow;
	GtkWidget *btn_hbox;
#ifdef GENERIC_UMPC
	GtkWidget *hbox;
#endif
	GtkWidget *reg_btn;
	GtkWidget *subst_btn;
	GtkWidget *del_btn;
	GtkWidget *default_btn;
	GtkWidget *vbox_frame;
	GtkWidget *table;
	GtkWidget *label_icon_text;
	GtkWidget *item_text_entry;
	GtkWidget *vbox_toolbar_items;
	GtkWidget *hbox_bottom;
	GtkWidget *scrolledwindow_list_view_set;
	GtkWidget *list_view_set;
	GtkWidget *label;

	GtkWidget *btn_vbox;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	debug_print("Creating custom toolbar window...\n");

	main_vbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_show(main_vbox);

	vbox_frame = gtk_frame_new(_("Toolbar item"));
	gtk_widget_show(vbox_frame);
	gtk_box_pack_start(GTK_BOX(main_vbox), vbox_frame, FALSE, TRUE, 0);

	toolbar_item_hbox = gtk_hbox_new (FALSE, 4);
	gtk_widget_show(toolbar_item_hbox);
	gtk_container_add(GTK_CONTAINER (vbox_frame), toolbar_item_hbox);
	
	table = gtk_table_new (3, 3, FALSE);
	gtk_box_pack_start(GTK_BOX(toolbar_item_hbox), table,
			   TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (table), 8);
	gtk_table_set_row_spacings (GTK_TABLE (table), 8);
	gtk_table_set_col_spacings (GTK_TABLE (table), 8);

	/* toolbar item type */
	label = gtk_label_new(_("Item type"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_widget_show(label);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 0, 1,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);	
	
	item_type_combo = gtkut_sc_combobox_create(NULL, TRUE);
	item_type_model = GTK_LIST_STORE(gtk_combo_box_get_model(
					 GTK_COMBO_BOX(item_type_combo)));
	COMBOBOX_ADD(item_type_model, _("Internal Function"), ITEM_FUNCTION);
	COMBOBOX_ADD(item_type_model, _("User Action"), ITEM_USER_ACTION);
	COMBOBOX_ADD(item_type_model, _("Plugins"), ITEM_PLUGIN);
	COMBOBOX_ADD(item_type_model, _("Separator"), ITEM_SEPARATOR);	
	gtk_widget_set_size_request(item_type_combo, 200, -1);
	gtk_table_attach(GTK_TABLE(table), item_type_combo, 1, 3, 0, 1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);

	/* available actions */
	label = gtk_label_new(_("Event executed on click"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
			  (GtkAttachOptions) (GTK_FILL),
			  (GtkAttachOptions) (0), 0, 0);

	item_action_combo = gtk_combo_box_new_text();
	gtk_widget_set_size_request(item_action_combo, 200, -1);
	gtk_table_attach (GTK_TABLE (table), item_action_combo, 1, 3, 1, 2,
			  (GtkAttachOptions) (GTK_FILL),
			  (GtkAttachOptions) (0), 0, 0);
			  
	/* available internal functions */
	item_func_combo = gtk_combo_box_new_text();
	gtk_widget_set_size_request(item_func_combo, 200, -1);
	gtk_table_attach (GTK_TABLE (table), item_func_combo, 1, 3, 1, 2,
			  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			  (GtkAttachOptions) (0), 0, 0);
	
	/* plugin-registered items */
	item_plugin_combo = gtk_combo_box_new_text();
	gtk_widget_set_size_request(item_plugin_combo, 200, -1);
	gtk_table_attach(GTK_TABLE(table), item_plugin_combo, 1, 3, 1, 2,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);

	/* toolbar item description */
	label_icon_text = gtk_label_new(_("Toolbar text"));
	gtk_misc_set_alignment(GTK_MISC(label_icon_text), 0, 0.5);
	gtk_widget_show (label_icon_text);
	gtk_table_attach (GTK_TABLE (table), label_icon_text, 0, 1, 2, 3,
			  (GtkAttachOptions) (GTK_FILL),
			  (GtkAttachOptions) (0), 0, 0);

	item_text_entry = gtk_entry_new();
	gtk_table_attach (GTK_TABLE (table), item_text_entry, 1, 3, 2, 3,
			  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			  (GtkAttachOptions) (0), 0, 0);

	icon_vbox = gtk_vbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show(icon_vbox);
	
	icon_label = gtk_label_new(_("Icon"));
	gtk_widget_set_size_request(icon_label, 100, -1);
	gtk_box_pack_start(GTK_BOX(icon_vbox), icon_label, FALSE, FALSE, 0);
	
	icon_hbox = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(icon_hbox);
	
	label = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(icon_hbox), label, TRUE, TRUE, 0);
	
	icon_button = gtk_button_new();
	gtk_widget_show(icon_button);
	gtk_widget_set_size_request(icon_button, 50, 50);
	g_signal_connect(G_OBJECT(icon_button), "clicked",
			 G_CALLBACK(icon_chooser_create), prefs_toolbar);
	gtk_box_pack_start(GTK_BOX(icon_hbox), icon_button, FALSE, FALSE, 8);

	label = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(icon_hbox), label, TRUE, TRUE, 0);	
	
	gtk_box_pack_start(GTK_BOX(icon_vbox), icon_hbox, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(toolbar_item_hbox), icon_vbox, FALSE, FALSE, 0);
		
	/* register / substitute / delete */
	reg_hbox = gtk_hbox_new(FALSE, 4);
	gtk_box_pack_start(GTK_BOX(main_vbox), reg_hbox, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(reg_hbox), 10);

	arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_box_pack_start(GTK_BOX(reg_hbox), arrow, FALSE, FALSE, 0);
	gtk_widget_set_size_request(arrow, -1, 16);

	btn_hbox = gtk_hbox_new(TRUE, 4);
	gtk_box_pack_start(GTK_BOX(reg_hbox), btn_hbox, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_box_pack_start(GTK_BOX(btn_hbox), reg_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(reg_btn), "clicked",
			 G_CALLBACK(prefs_toolbar_register), 
			 prefs_toolbar);

	subst_btn = gtkut_get_replace_btn(_("Replace"));
	gtk_box_pack_start(GTK_BOX(btn_hbox), subst_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(subst_btn), "clicked",
			 G_CALLBACK(prefs_toolbar_substitute),
			 prefs_toolbar);

	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_box_pack_start(GTK_BOX(btn_hbox), del_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(del_btn), "clicked",
			 G_CALLBACK(prefs_toolbar_delete), 
			  prefs_toolbar);

	default_btn = gtk_button_new_with_label(_(" Use default "));
#ifndef GENERIC_UMPC
	gtk_box_pack_end(GTK_BOX(reg_hbox), default_btn, FALSE, TRUE, 0);
#else
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(main_vbox), hbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), default_btn, FALSE, FALSE, 0);
#endif
	g_signal_connect(G_OBJECT(default_btn), "clicked",
			 G_CALLBACK(prefs_toolbar_default), 
			 prefs_toolbar);

	/* currently active toolbar items */
	vbox_toolbar_items = gtk_vbox_new(FALSE, VBOX_BORDER);
	gtk_box_pack_start(GTK_BOX(main_vbox), vbox_toolbar_items, TRUE, TRUE, 0);
	
	hbox_bottom = gtk_hbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(vbox_toolbar_items), hbox_bottom);
	
	scrolledwindow_list_view_set = gtk_scrolled_window_new(NULL, NULL);
	gtk_box_pack_start(GTK_BOX(hbox_bottom), scrolledwindow_list_view_set, TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(scrolledwindow_list_view_set), 1);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwindow_list_view_set), 
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwindow_list_view_set),
					    GTK_SHADOW_IN);

	list_view_set = create_set_list_view(prefs_toolbar); 
	gtk_widget_show(list_view_set);
	gtk_container_add(GTK_CONTAINER(scrolledwindow_list_view_set), list_view_set);
	gtk_widget_set_size_request(list_view_set, 225, 120);

	btn_vbox = gtk_vbox_new(FALSE, 8);
	gtk_widget_show(btn_vbox);
	gtk_box_pack_start(GTK_BOX(hbox_bottom), btn_vbox, FALSE, FALSE, 5);

	up_btn = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_widget_show(up_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), up_btn, FALSE, FALSE, 2);

	down_btn = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_widget_show(down_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), down_btn, FALSE, FALSE, 0);

	g_signal_connect(G_OBJECT(item_type_combo), "changed",
			 G_CALLBACK(item_type_changed), prefs_toolbar);
	g_signal_connect(G_OBJECT(item_action_combo), "changed",
			 G_CALLBACK(action_selection_changed), prefs_toolbar);
	g_signal_connect(G_OBJECT(item_plugin_combo), "changed",
			 G_CALLBACK(plugin_selection_changed), prefs_toolbar);
	g_signal_connect(G_OBJECT(item_func_combo), "changed",
			 G_CALLBACK(func_selection_changed), prefs_toolbar);
	g_signal_connect(G_OBJECT(up_btn), "clicked",
			 G_CALLBACK(prefs_toolbar_up), prefs_toolbar);
	g_signal_connect(G_OBJECT(down_btn), "clicked",
			 G_CALLBACK(prefs_toolbar_down), prefs_toolbar);
	
	gtk_widget_show_all(main_vbox);

	prefs_toolbar->list_view_set    = list_view_set;
	prefs_toolbar->item_text_entry  = item_text_entry;
	prefs_toolbar->item_type_combo	= item_type_combo;
	prefs_toolbar->item_func_combo	= item_func_combo;
	prefs_toolbar->item_action_combo= item_action_combo;
	prefs_toolbar->item_plugin_combo= item_plugin_combo;
	prefs_toolbar->icon_button	= icon_button;
	prefs_toolbar->item_icon_file	= NULL;
	
	prefs_toolbar->page.widget = main_vbox;
}

ToolbarPage *prefs_toolbar_mainwindow;
ToolbarPage *prefs_toolbar_composewindow;
ToolbarPage *prefs_toolbar_messageview;

static void toolbar_unregister_plugin_item_real(GHashTable *hash, 
					const gchar *plugin_name, 
					const gchar *item_name)
{
	gchar *key;

	if (!hash)
		return;

	key = g_strdup_printf(plugin_name, "/", item_name, NULL);
	g_hash_table_remove(hash, key);
	g_free(key);
}

void prefs_toolbar_unregister_plugin_item(ToolbarType toolbar_type, 
					const gchar *plugin_name, 
					const gchar *item_name)
{
	GHashTable **hash;
	hash = get_plugin_hash_from_toolbar_type(toolbar_type);
	if (hash)
		toolbar_unregister_plugin_item_real(*hash, plugin_name, item_name);
}

static void prefs_toolbar_execute_plugin_item_real(gpointer parent, 
				GHashTable *hash, const gchar *id)
{
	ToolbarPluginItem *value;
	GSList *walk;
	gboolean found;

	if (!hash) {
		debug_print("No plugin registered toolbar items yet\n");
		return;
	}

	value = g_hash_table_lookup(hash, id);
	if (!value) {
		debug_print("Could not find plugin toolbar item with id %s\n", id);
		return;
	}

	/* check if corresponding plugin is currently loaded */
	found = FALSE;
	for (walk = plugin_get_list(); walk; walk = walk->next) {
		const gchar *plugin_name;
		Plugin *plugin = walk->data;
		plugin_name = plugin_get_name(plugin);
		if (!strcmp(plugin_name, value->plugin)) {
			found = TRUE;
			break;
		}
	}
	if (!found) {
		debug_print("Plugin '%s' is currently not loaded, cannot execute toolbar action\n", value->plugin);
		return;
	}

	value->cb(parent, value->item_name, value->cb_data);
}

void prefs_toolbar_execute_plugin_item(gpointer parent, 
			ToolbarType toolbar_type, const gchar *id)
{
	GHashTable **hash;
	hash = get_plugin_hash_from_toolbar_type(toolbar_type);
	if (hash)
		prefs_toolbar_execute_plugin_item_real(parent, *hash, id);
}

static void destroy_plugin_item_hash_value(ToolbarPluginItem *item)
{
	g_free(item->plugin);
	g_free(item->item_name);
	g_free(item);
}

static void prefs_toolbar_register_plugin_item_real(GHashTable **hash, 
					const gchar *plugin_name, 
					const gchar *item_name, 
					ToolbarPluginCallback cb, 
					gpointer cb_data)
{
	gchar *key;
	ToolbarPluginItem *value;

	cm_return_if_fail(plugin_name && item_name);

	if (!*hash) {
		*hash = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, 
				(GDestroyNotify) destroy_plugin_item_hash_value);
		if (!*hash)
			return;
	}

	key = g_strconcat(plugin_name, "/", item_name, NULL);
	value = g_new0(ToolbarPluginItem, 1);
	value->plugin = g_strdup(plugin_name);
	value->item_name = g_strdup(item_name);
	value->cb = cb;
	value->cb_data = cb_data;
	g_hash_table_insert(*hash, key, value);
}

void prefs_toolbar_register_plugin_item(ToolbarType toolbar_type, 
					const gchar *plugin_name, 
					const gchar *item_name, 
					ToolbarPluginCallback cb, 
					gpointer cb_data)
{
	GHashTable **hash;
	hash = get_plugin_hash_from_toolbar_type(toolbar_type);
	if(hash)
		prefs_toolbar_register_plugin_item_real(hash, plugin_name, 
						item_name, cb, cb_data);
}

void prefs_toolbar_init(void)
{
	ToolbarPage *page;
	static gchar *mainpath[3], *messagepath[3], *composepath[3];

	mainpath[0] = _("Toolbars");
	mainpath[1] = _("Main Window");
	mainpath[2] = NULL;

	page = g_new0(ToolbarPage, 1);
	page->page.path = mainpath;
	page->page.create_widget = prefs_toolbar_create_widget;
	page->page.destroy_widget = prefs_toolbar_destroy_widget;
	page->page.save_page = prefs_toolbar_save;
	page->source = TOOLBAR_MAIN;
	page->page.weight = 50.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_toolbar_mainwindow = page;

	messagepath[0] = _("Toolbars");
	messagepath[1] = _("Message Window");
	messagepath[2] = NULL;

	page = g_new0(ToolbarPage, 1);
	page->page.path = messagepath;
	page->page.create_widget = prefs_toolbar_create_widget;
	page->page.destroy_widget = prefs_toolbar_destroy_widget;
	page->page.save_page = prefs_toolbar_save;
	page->source = TOOLBAR_MSGVIEW;
	page->page.weight = 45.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_toolbar_messageview = page;

	composepath[0] = _("Toolbars");
	composepath[1] = _("Compose Window");
	composepath[2] = NULL;

	page = g_new0(ToolbarPage, 1);
	page->page.path = composepath;
	page->page.create_widget = prefs_toolbar_create_widget;
	page->page.destroy_widget = prefs_toolbar_destroy_widget;
	page->page.save_page = prefs_toolbar_save;
	page->source = TOOLBAR_COMPOSE;
	page->page.weight = 40.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_toolbar_composewindow = page;
}

void prefs_toolbar_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_toolbar_mainwindow);
	g_free(prefs_toolbar_mainwindow->item_icon_file);
	g_free(prefs_toolbar_mainwindow);
	prefs_gtk_unregister_page((PrefsPage *) prefs_toolbar_composewindow);
	g_free(prefs_toolbar_composewindow->item_icon_file);
	g_free(prefs_toolbar_composewindow);
	prefs_gtk_unregister_page((PrefsPage *) prefs_toolbar_messageview);
	g_free(prefs_toolbar_messageview->item_icon_file);
	g_free(prefs_toolbar_messageview);
}

static void set_visible_if_not_text(GtkTreeViewColumn *col,
				    GtkCellRenderer   *renderer,
			            GtkTreeModel      *model,
				    GtkTreeIter       *iter,
				    gpointer           user_data)
{
	gboolean is_text;
	GdkPixbuf *pixbuf;

	gtk_tree_model_get(model, iter, SET_ICON_IS_TEXT, &is_text, -1);
	if (is_text) {
		g_object_set(renderer, "visible", FALSE, NULL); 
	} else {
		pixbuf = NULL;
		gtk_tree_model_get(model, iter, 
				   SET_ICON, &pixbuf,
				   -1);
		/* note getting a pixbuf from a tree model increases
		 * its refcount ... */
		g_object_unref(pixbuf);
		
		g_object_set(renderer, "visible", TRUE, NULL);
		g_object_set(renderer, "pixbuf",  pixbuf, NULL);
	}
}

static GtkWidget *create_set_list_view(ToolbarPage *prefs_toolbar)
{
	GtkTreeView *list_view;
	GtkListStore *store;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selector;

	store = gtk_list_store_new(N_SET_COLUMNS, 
				   GDK_TYPE_PIXBUF,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_BOOLEAN,
				   -1);
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
	g_object_unref(G_OBJECT(store));

	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, _("Icon"));
	renderer = gtk_cell_renderer_pixbuf_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	gtk_tree_view_set_reorderable(list_view, TRUE);
	/* tell pixbuf renderer it is only visible if 
	 * the icon is not represented by text */
	gtk_tree_view_column_set_cell_data_func(column, renderer,
						set_visible_if_not_text,
						NULL, NULL);
	
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	
	/* tell the text renderer it is only visible if the icon
	 * is represented by an image */
	gtk_tree_view_column_set_attributes(column, renderer,
					    "visible", SET_ICON_IS_TEXT,
					    "text", SET_ICON_TEXT,
					    NULL);

	gtk_tree_view_append_column(list_view, column);

	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, _("Icon text"));
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	gtk_tree_view_column_set_attributes(column, renderer,
					    "text", SET_TEXT,
					    NULL);
	gtk_tree_view_append_column(list_view, column);

	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, _("Mapped event"));
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	gtk_tree_view_column_set_attributes(column, renderer,
					    "text", SET_EVENT,
					    NULL);
	gtk_tree_view_append_column(list_view, column);

	/* various other tree view attributes */
	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	
	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);
	gtk_tree_selection_set_select_function
		(selector, (GtkTreeSelectionFunc) set_list_selected,
	         prefs_toolbar, NULL);

	return GTK_WIDGET(list_view);	

}

static gboolean set_list_selected(GtkTreeSelection *selector,
			          GtkTreeModel *model, 
				  GtkTreePath *path,
				  gboolean currently_selected,
				  ToolbarPage *prefs_toolbar)
{

	GtkTreeIter iter;
	gchar *icon_text, *icon_file, *descr;
	GList *cur;
	GSList *cur2;
	gint item_num;
	GdkPixbuf *pix;
	
	if (currently_selected || !gtk_tree_model_get_iter(model, &iter, path))
		return TRUE;
	
	gtk_tree_model_get(model, &iter,
			   SET_ICON, &pix,
			   SET_TEXT, &icon_text,
			   SET_EVENT, &descr,
			   SET_FILENAME, &icon_file,
			   -1);
	
	g_free(prefs_toolbar->item_icon_file);
	prefs_toolbar->item_icon_file = icon_file;
	gtk_button_set_image(GTK_BUTTON(prefs_toolbar->icon_button),
			     gtk_image_new_from_pixbuf(pix));
	
	if (g_utf8_collate(toolbar_ret_descr_from_val(A_SEPARATOR), descr) == 0) {
		gtk_button_set_label(GTK_BUTTON(prefs_toolbar->icon_button),
				    _("None"));
		g_free(prefs_toolbar->item_icon_file);
		prefs_toolbar->item_icon_file = NULL;
		gtk_combo_box_set_active(GTK_COMBO_BOX(prefs_toolbar->item_type_combo),
					ITEM_SEPARATOR);
		g_free(icon_text);
		g_free(descr);

		return TRUE;
	}
	
	gtk_button_set_label(GTK_BUTTON(prefs_toolbar->icon_button), "");
	gtk_entry_set_text(GTK_ENTRY(prefs_toolbar->item_text_entry), 
			   icon_text);

	if (g_utf8_collate(toolbar_ret_descr_from_val(A_CLAWS_ACTIONS), descr) == 0) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(
			prefs_toolbar->item_type_combo), ITEM_USER_ACTION);

		for(cur2 = prefs_common.actions_list, item_num = 0; cur2 != NULL;
		    cur2 = cur2->next) {
			gchar *item_string;
			get_action_name((gchar *)cur2->data, &item_string);
			
			if(g_utf8_collate(item_string, icon_text) == 0) {
				gtk_combo_box_set_active(
					GTK_COMBO_BOX(prefs_toolbar->item_action_combo),
					item_num);
				g_free(item_string);
				break;
			}
			else {
				item_num++;
				g_free(item_string);
			}
		}

		gtk_widget_show(prefs_toolbar->item_action_combo);
		gtk_widget_hide(prefs_toolbar->item_func_combo);
		gtk_widget_hide(prefs_toolbar->item_plugin_combo);
		
		g_free(icon_text);
		g_free(descr);

		return TRUE;
	}

	if (g_utf8_collate(toolbar_ret_descr_from_val(A_CLAWS_PLUGINS), descr) == 0) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(prefs_toolbar->item_type_combo), ITEM_PLUGIN);

		gtk_combo_box_set_active(GTK_COMBO_BOX(prefs_toolbar->item_plugin_combo), 0);

		gtk_widget_show(prefs_toolbar->item_plugin_combo);
		gtk_widget_hide(prefs_toolbar->item_func_combo);
		gtk_widget_hide(prefs_toolbar->item_action_combo);

		g_free(descr);
		g_free(icon_text);
		return TRUE;
	}
	
	/* scan combo list for selected description an set combo item accordingly */
	for (cur = prefs_toolbar->combo_action_list, item_num = 0; cur != NULL; 
	     cur = cur->next) {
		gchar *item_str = (gchar*)cur->data;
		if (g_utf8_collate(item_str, descr) == 0) {
			gtk_combo_box_set_active(
				GTK_COMBO_BOX(prefs_toolbar->item_func_combo),
				item_num);
			
			break;
		}
		else
			item_num++;
	}

	gtk_combo_box_set_active(GTK_COMBO_BOX(
			prefs_toolbar->item_type_combo),ITEM_FUNCTION);
	gtk_widget_hide(prefs_toolbar->item_action_combo);
	gtk_widget_show(prefs_toolbar->item_func_combo);

	g_free(icon_text);
	g_free(descr);

	return TRUE;
}

static void icon_chooser_ok_clicked(GtkButton *button,
					ToolbarPage *prefs_toolbar)
{
	GtkTreeModel *model;
	GtkTreeIter iter;
	GList *list;
	GdkPixbuf *pix;
	gchar *icon_file;
	
	cm_return_if_fail(prefs_toolbar != NULL);

	model = gtk_icon_view_get_model(GTK_ICON_VIEW(prefs_toolbar->icon_chooser_view));
	list = gtk_icon_view_get_selected_items(GTK_ICON_VIEW(prefs_toolbar->icon_chooser_view));
	if(list == NULL)
		return;
	
	if(!gtk_tree_model_get_iter(model, &iter, (GtkTreePath *)list->data)) {
		gtk_tree_path_free(list->data);
		g_list_free(list);
		return;
	}
	
	gtk_tree_model_get(model, &iter,
			   SET_ICON, &pix,
			   SET_FILENAME, &icon_file,
			   -1);

	g_free(prefs_toolbar->item_icon_file);
	prefs_toolbar->item_icon_file = icon_file;
	gtk_button_set_image(GTK_BUTTON(prefs_toolbar->icon_button),
			     gtk_image_new_from_pixbuf(pix));
	
	gtk_widget_destroy(prefs_toolbar->icon_chooser_win);
	prefs_toolbar->icon_chooser_win = NULL;
	prefs_toolbar->icon_chooser_view = NULL;
	
	gtk_tree_path_free(list->data);
	g_list_free(list);
}

static void icon_chooser_cancel_clicked(GtkButton *button,
					ToolbarPage *prefs_toolbar)
{
	cm_return_if_fail(prefs_toolbar != NULL);

	gtk_widget_destroy(prefs_toolbar->icon_chooser_win);
	prefs_toolbar->icon_chooser_win = NULL;
	prefs_toolbar->icon_chooser_view = NULL;
}

static gboolean icon_chooser_key_pressed(GtkWidget *widget, GdkEventKey *event,
			ToolbarPage *prefs_toolbar)
{
	if (event && event->keyval == GDK_Escape) {
		icon_chooser_cancel_clicked(NULL, prefs_toolbar);
		return TRUE;
	}

	return FALSE;
}

static gboolean icon_list_key_pressed(GtkWidget *widget, GdkEventKey *event,
			ToolbarPage *prefs_toolbar)
{
	if (event) {
		if (event->keyval == GDK_KP_Enter ||
		    event->keyval == GDK_Return ||
		    event->keyval == GDK_space) {
			icon_chooser_ok_clicked(NULL, prefs_toolbar);
			return TRUE;
		}
	}
	return FALSE;
}

static gboolean ok_cb(gpointer data)
{
	ToolbarPage *prefs_toolbar = (ToolbarPage *)data;
	icon_chooser_ok_clicked(NULL, prefs_toolbar);
	return FALSE;
}

static gboolean icon_list_button_release(GtkWidget *widget,
					       GdkEventButton *event,
					       ToolbarPage *prefs_toolbar )
{
	static guint id = -1;
	if (id >= 0) {
		g_source_remove(id);
		id = -1;
	}
	id = g_timeout_add(100, ok_cb, prefs_toolbar);
	return FALSE;
}

static gboolean icon_window_button_press(GtkWidget *widget,
					       GdkEventButton *event,
					       ToolbarPage *prefs_toolbar )
{
	GtkWidget *event_widget, *button;
	gboolean restore = TRUE;

	button = prefs_toolbar->icon_button;

	/* Test where mouse was clicked */
	event_widget = gtk_get_event_widget((GdkEvent *)event);
	if (event_widget != widget) {
		while (event_widget) {
			if (event_widget == widget)
				return FALSE;
			else if (event_widget == prefs_toolbar->icon_chooser_win) {
				restore = FALSE;
				break;
			}
			event_widget = event_widget->parent;
		}
	}

	if (restore) {
		icon_chooser_cancel_clicked(NULL, prefs_toolbar);
		return TRUE;
	}
	return FALSE;
}

static void icon_chooser_activated(GtkTreeView *treeview, GtkTreePath *path,
				ToolbarPage *prefs_toolbar)
{
	icon_chooser_ok_clicked(NULL, prefs_toolbar);
}

static void icon_chooser_create(GtkButton *button, ToolbarPage *prefs_toolbar)
{
	GtkWidget *icon_chooser_win;
	GtkWidget *scrollwin;
	GtkWidget *icon_view;
	GtkListStore *store;
	GtkTreeIter iter;
	gint i, x, y;
	
	store = gtk_list_store_new(2, 
				   GDK_TYPE_PIXBUF,
				   G_TYPE_STRING,
				   -1);
				   
	gtk_list_store_clear(store);

	for (i = 0; ToolbarIcons[i] != STOCK_PIXMAP_EMPTY; i++) {
		GdkPixbuf *pixbuf;
		stock_pixbuf_gdk(prefs_toolbar->window, ToolbarIcons[i], &pixbuf);
		
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
				   SET_ICON, pixbuf,
				   SET_FILENAME, stock_pixmap_get_name((StockPixmap) ToolbarIcons[i]),
				   -1);
 	}
	
	icon_chooser_win = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_toolbar");
	gtk_window_set_title(GTK_WINDOW(icon_chooser_win), _("Toolbar item icon"));
#ifndef MAEMO
	gtk_window_set_decorated(GTK_WINDOW(icon_chooser_win), FALSE);
#endif
	gdk_window_get_origin(GTK_WIDGET(prefs_toolbar->icon_button)->window, 
			&x, &y);
	x += GTK_WIDGET(prefs_toolbar->icon_button)->allocation.x;
	y += GTK_WIDGET(prefs_toolbar->icon_button)->allocation.y;
	y += 50;
	x -= 300-50;
	gtk_window_move(GTK_WINDOW(icon_chooser_win), x, y);
	gtk_window_set_resizable(GTK_WINDOW(icon_chooser_win), FALSE);
	gtk_widget_set_size_request(icon_chooser_win, 300, 320);
	
	scrollwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_container_add(GTK_CONTAINER(icon_chooser_win), scrollwin);
	gtk_widget_show(scrollwin);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollwin),
				GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrollwin),
				GTK_SHADOW_OUT);

	icon_view = gtk_icon_view_new_with_model(GTK_TREE_MODEL(store));
	gtk_icon_view_set_selection_mode(GTK_ICON_VIEW(icon_view), GTK_SELECTION_SINGLE);
	gtk_icon_view_set_pixbuf_column(GTK_ICON_VIEW(icon_view), SET_ICON);
	gtk_container_add(GTK_CONTAINER(scrollwin), GTK_WIDGET(icon_view));

	g_signal_connect(G_OBJECT(icon_chooser_win), "key_press_event",
			 G_CALLBACK(icon_chooser_key_pressed), prefs_toolbar);
	g_signal_connect(G_OBJECT(icon_view), "item-activated",
			 G_CALLBACK(icon_chooser_activated), prefs_toolbar);
	g_signal_connect(G_OBJECT(icon_chooser_win),
			 "button-press-event",
			 G_CALLBACK(icon_window_button_press),
			 prefs_toolbar );
	g_signal_connect(G_OBJECT(icon_view),
			 "button-release-event",
			 G_CALLBACK(icon_list_button_release),
			 prefs_toolbar );
	g_signal_connect(G_OBJECT(icon_view), "key_press_event",
			 G_CALLBACK(icon_list_key_pressed), prefs_toolbar);

	gtk_widget_show_all(icon_chooser_win);
	gtk_widget_grab_focus(GTK_WIDGET(icon_view));
	gtk_window_set_modal(GTK_WINDOW(icon_chooser_win), TRUE);
	
	prefs_toolbar->icon_chooser_win		= icon_chooser_win;
	prefs_toolbar->icon_chooser_view	= icon_view;
}
