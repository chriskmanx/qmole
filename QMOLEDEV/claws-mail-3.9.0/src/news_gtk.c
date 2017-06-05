/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & the Claws Mail Team
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
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>

#include <gtk/gtk.h>

#include "utils.h"
#include "folder.h"
#include "folderview.h"
#include "menu.h"
#include "account.h"
#include "alertpanel.h"
#include "grouplistdialog.h"
#include "prefs_common.h"
#include "news_gtk.h"
#include "common/hooks.h"
#include "inc.h"
#include "news.h"
#include "statusbar.h"
#include "inputdialog.h"

static void subscribe_newsgroup_cb(GtkAction *action, gpointer data);
static void unsubscribe_newsgroup_cb(GtkAction *action, gpointer data);
static void rename_newsgroup_cb(GtkAction *action, gpointer data);
static void update_tree_cb(GtkAction *action, gpointer data);
static void download_cb(GtkAction *action, gpointer data);
static void sync_cb(GtkAction *action, gpointer data);

static GtkActionEntry news_popup_entries[] =
{
	{"FolderViewPopup/Subscribe",		NULL, N_("_Subscribe to newsgroup..."), NULL, NULL, G_CALLBACK(subscribe_newsgroup_cb) },
	{"FolderViewPopup/Unsubscribe",		NULL, N_("_Unsubscribe newsgroup"), NULL, NULL, G_CALLBACK(unsubscribe_newsgroup_cb) },

	{"FolderViewPopup/Synchronise",		NULL, N_("Synchronise"), NULL, NULL, G_CALLBACK(sync_cb) },
	{"FolderViewPopup/DownloadMessages",	NULL, N_("Down_load messages"), NULL, NULL, G_CALLBACK(download_cb) },
	{"FolderViewPopup/RenameFolder",	NULL, N_("_Rename folder..."), NULL, NULL, G_CALLBACK(rename_newsgroup_cb) },

	{"FolderViewPopup/CheckNewMessages",	NULL, N_("_Check for new messages"), NULL, NULL, G_CALLBACK(update_tree_cb) },

};

static void set_sensitivity(GtkUIManager *ui_manager, FolderItem *item);
static void add_menuitems(GtkUIManager *ui_manager, FolderItem *item);

static FolderViewPopup news_popup =
{
	"news",
	"<NewsFolder>",
	news_popup_entries,
	G_N_ELEMENTS(news_popup_entries),
	NULL, 0,
	NULL, 0, 0, NULL,
	add_menuitems,
	set_sensitivity
};

void news_gtk_init(void)
{
	folderview_register_popup(&news_popup);
}

static void add_menuitems(GtkUIManager *ui_manager, FolderItem *item)
{
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "Subscribe", "FolderViewPopup/Subscribe", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "Unsubscribe", "FolderViewPopup/Unsubscribe", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorNews1", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "Synchronise", "FolderViewPopup/Synchronise", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "DownloadMessages", "FolderViewPopup/DownloadMessages", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "RenameFolder", "FolderViewPopup/RenameFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorNews2", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CheckNewMessages", "FolderViewPopup/CheckNewMessages", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorNews3", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
}
	
static void set_sensitivity(GtkUIManager *ui_manager, FolderItem *item)
{
	MainWindow *mainwin = mainwindow_get_mainwindow();
	
#define SET_SENS(name, sens) \
	cm_menu_set_sensitive_full(ui_manager, "Popup/"name, sens)

	SET_SENS("FolderViewPopup/Subscribe", 
			folder_item_parent(item) == NULL 
			&& mainwin->lock_count == 0
			&& news_folder_locked(item->folder) == 0);
	SET_SENS("FolderViewPopup/Unsubscribe",     
			folder_item_parent(item) != NULL 
			&& mainwin->lock_count == 0
			&& news_folder_locked(item->folder) == 0);
	SET_SENS("FolderViewPopup/CheckNewMessages",    
			folder_item_parent(item) == NULL 
			&& mainwin->lock_count == 0
			&& news_folder_locked(item->folder) == 0);
	SET_SENS("FolderViewPopup/Synchronise",    
			item ? (folder_item_parent(item) != NULL
			&& folder_want_synchronise(item->folder))
			: FALSE);
	SET_SENS("FolderViewPopup/DownloadMessages",
			item ? (folder_item_parent(item) != NULL
			&& !item->no_select)
			: FALSE);
	SET_SENS("FolderViewPopup/RenameFolder",
			folder_item_parent(item) != NULL 
			&& mainwin->lock_count == 0
			&& news_folder_locked(item->folder) == 0);
#undef SET_SENS
}

static FolderItem *news_find_child_item(FolderItem *item, const gchar *path)
{
	GNode *node;
	FolderItem *child;

	for (node = item->node->children; node != NULL; node = node->next) {
		child = FOLDER_ITEM(node->data);
		if (strcmp2(child->path, path) == 0) {
			return child;
		}
	}

	return NULL;
}

static void subscribe_newsgroup_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	GtkCMCTreeNode *servernode, *node;
	Folder *folder;
	FolderItem *item;
	FolderItem *rootitem;
	FolderItem *newitem;
	GSList *new_subscr;
	GSList *cur;
	GNode *gnode;
	MainWindow *mainwin = mainwindow_get_mainwindow();
	
	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	cm_return_if_fail(item != NULL);

	if (mainwin->lock_count || news_folder_locked(item->folder))
		return;

	folder = item->folder;
	cm_return_if_fail(folder != NULL);
	cm_return_if_fail(FOLDER_TYPE(folder) == F_NEWS);
	cm_return_if_fail(folder->account != NULL);

	if (GTK_CMCTREE_ROW(folderview->selected)->parent != NULL)
		servernode = GTK_CMCTREE_ROW(folderview->selected)->parent;
	else
		servernode = folderview->selected;

	rootitem = gtk_cmctree_node_get_row_data(ctree, servernode);

	new_subscr = grouplist_dialog(folder);

	/* remove unsubscribed newsgroups */
	for (gnode = folder->node->children; gnode != NULL; ) {
		GNode *next = gnode->next;

		item = FOLDER_ITEM(gnode->data);
		if (g_slist_find_custom(new_subscr, item->path,
					(GCompareFunc)g_ascii_strcasecmp) != NULL) {
			gnode = next;
			continue;
		}

		node = gtk_cmctree_find_by_row_data(ctree, servernode, item);
		if (!node) {
			gnode = next;
			continue;
		}

		if (folderview->opened == node) {
			summary_clear_all(folderview->summaryview);
			folderview->opened = NULL;
		}

		gtk_cmctree_remove_node(ctree, node);
		folder_item_remove(item);

		gnode = next;
	}

	gtk_cmclist_freeze(GTK_CMCLIST(ctree));

	/* add subscribed newsgroups */
	for (cur = new_subscr; cur != NULL; cur = cur->next) {
		gchar *name = (gchar *)cur->data;
		FolderUpdateData hookdata;

		if (news_find_child_item(rootitem, name) != NULL)
			continue;

		newitem = folder_item_new(folder, name, name);
		folder_item_append(rootitem, newitem);

		hookdata.folder = newitem->folder;
		hookdata.update_flags = FOLDER_TREE_CHANGED | FOLDER_ADD_FOLDERITEM;
		hookdata.item = newitem;
		hooks_invoke(FOLDER_UPDATE_HOOKLIST, &hookdata);
	}

	gtk_cmclist_thaw(GTK_CMCLIST(ctree));

	slist_free_strings_full(new_subscr);

	folder_write_list();
}

static void unsubscribe_newsgroup_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item;
	gchar *name;
	gchar *message;
	gchar *old_id;
	AlertValue avalue;
	MainWindow *mainwin = mainwindow_get_mainwindow();
	
	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	cm_return_if_fail(item != NULL);

	if (mainwin->lock_count || news_folder_locked(item->folder))
		return;

	cm_return_if_fail(item->folder != NULL);
	cm_return_if_fail(FOLDER_TYPE(item->folder) == F_NEWS);
	cm_return_if_fail(item->folder->account != NULL);

	old_id = folder_item_get_identifier(item);

	name = trim_string(item->path, 32);
	message = g_strdup_printf(_("Really unsubscribe newsgroup '%s'?"), name);
	avalue = alertpanel_full(_("Unsubscribe newsgroup"), message,
		 	         GTK_STOCK_CANCEL, _("_Unsubscribe"), NULL, FALSE,
			         NULL, ALERT_WARNING, G_ALERTDEFAULT);
	g_free(message);
	g_free(name);
	if (avalue != G_ALERTALTERNATE) return;

	if (folderview->opened == folderview->selected) {
		summary_clear_all(folderview->summaryview);
		folderview->opened = NULL;
	}

	if(item->folder->klass->remove_folder(item->folder, item) < 0) {
		folder_item_scan(item);
		alertpanel_error(_("Can't remove the folder '%s'."), name);
		g_free(old_id);
		return;
	}
	
	folder_write_list();
	
	prefs_filtering_delete_path(old_id);
	g_free(old_id);
}

static void rename_newsgroup_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *item;
	gchar *new_folder;
	gchar *name;
	gchar *message;

	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->path != NULL);
	cm_return_if_fail(item->folder != NULL);

	name = trim_string(item->name, 32);
	message = g_strdup_printf(_("Input new name for '%s':"), name);
	new_folder = input_dialog(_("Rename newsgroup folder"), message, item->name);
	g_free(message);
	g_free(name);

	if (!new_folder) return;
	AUTORELEASE_STR(new_folder, {g_free(new_folder); return;});

	if (strchr(new_folder, G_DIR_SEPARATOR) != NULL) {
		alertpanel_error(_("'%c' can't be included in folder name."),
				 G_DIR_SEPARATOR);
		return;
	}

	if (folder_find_child_item_by_name(folder_item_parent(item), new_folder)) {
		name = trim_string(new_folder, 32);
		alertpanel_error(_("The folder '%s' already exists."), name);
		g_free(name);
		return;
	}

	if (folder_item_rename(item, new_folder) < 0) {
		alertpanel_error(_("The folder could not be renamed.\n"
				   "The new folder name is not allowed."));
		return;
	}

	folder_write_list();
}

static void update_tree_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *item;
	MainWindow *mainwin = mainwindow_get_mainwindow();
	
	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);

	if (mainwin->lock_count || news_folder_locked(item->folder))
		return;

	summary_show(folderview->summaryview, NULL);

	cm_return_if_fail(item->folder != NULL);

	folderview_check_new(item->folder);
}

static void sync_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *item;

	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);
	folder_synchronise(item->folder);
}

void news_gtk_synchronise(FolderItem *item, gint days)
{
	MainWindow *mainwin = mainwindow_get_mainwindow();
	FolderView *folderview = mainwin->folderview;
	GSList *mlist;
	GSList *cur;
	gint num = 0;
	gint total = 0;
	time_t t = time(NULL);

	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->folder != NULL);

	if (mainwin->lock_count || news_folder_locked(item->folder))
		return;

	total = item->total_msgs;

	main_window_cursor_wait(mainwin);
	inc_lock();
	main_window_lock(mainwin);
	gtk_widget_set_sensitive(folderview->ctree, FALSE);
	main_window_progress_on(mainwin);
	GTK_EVENTS_FLUSH();

	mlist = folder_item_get_msg_list(item);
	for (cur = mlist; cur != NULL; cur = cur->next) {
		MsgInfo *msginfo = (MsgInfo *)cur->data;
		gint age = (t - msginfo->date_t) / (60*60*24);
		if (days == 0 || age <= days)
			folder_item_fetch_msg_full(msginfo->folder, msginfo->msgnum, TRUE, TRUE);
		statusbar_progress_all(num++,total, 100);
		if (num % 100 == 0)
			GTK_EVENTS_FLUSH();
	}

	statusbar_progress_all(0,0,0);
	procmsg_msg_list_free(mlist);
	folder_set_ui_func(item->folder, NULL, NULL);
	main_window_progress_off(mainwin);
	gtk_widget_set_sensitive(folderview->ctree, TRUE);
	main_window_unlock(mainwin);
	inc_unlock();
	main_window_cursor_normal(mainwin);
}

static void download_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item;

	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	news_gtk_synchronise(item, 0);
}
