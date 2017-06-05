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
#include <time.h>

#include "utils.h"
#include "folder.h"
#include "folder_item_prefs.h"
#include "folderview.h"
#include "menu.h"
#include "account.h"
#include "alertpanel.h"
#include "foldersel.h"
#include "inputdialog.h"
#include "imap.h"
#include "inc.h"
#include "prefs_common.h"
#include "statusbar.h"
#include "summaryview.h"
#include "prefs_actions.h"

static void new_folder_cb(GtkAction *action, gpointer data);
static void rename_folder_cb(GtkAction *action, gpointer data);
static void move_folder_cb(GtkAction *action, gpointer data);
static void copy_folder_cb(GtkAction *action, gpointer data);
static void delete_folder_cb(GtkAction *action, gpointer data);
static void update_tree_cb(GtkAction *action, gpointer data);
static void download_cb(GtkAction *action, gpointer data);
static void sync_cb(GtkAction *action, gpointer data);
static void subscribed_cb(GtkAction *action, gpointer data);
static void subscribe_cb(GtkAction *action, gpointer data);
static void unsubscribe_cb(GtkAction *action, gpointer data);

static GtkActionEntry imap_popup_entries[] =
{
	{"FolderViewPopup/CreateNewFolder",	NULL, N_("Create _new folder..."), NULL, NULL, G_CALLBACK(new_folder_cb) },

	{"FolderViewPopup/RenameFolder",	NULL, N_("_Rename folder..."), NULL, NULL, G_CALLBACK(rename_folder_cb) },
	{"FolderViewPopup/MoveFolder",		NULL, N_("M_ove folder..."), NULL, NULL, G_CALLBACK(move_folder_cb) },
	{"FolderViewPopup/CopyFolder",		NULL, N_("Cop_y folder..."), NULL, NULL, G_CALLBACK(copy_folder_cb) },

	{"FolderViewPopup/DeleteFolder",	NULL, N_("_Delete folder..."), NULL, NULL, G_CALLBACK(delete_folder_cb) },

	{"FolderViewPopup/Synchronise",		NULL, N_("Synchronise"), NULL, NULL, G_CALLBACK(sync_cb) },
	{"FolderViewPopup/DownloadMessages",	NULL, N_("Down_load messages"), NULL, NULL, G_CALLBACK(download_cb) },


	{"FolderViewPopup/Subscriptions",	NULL, N_("S_ubscriptions") },
	{"FolderViewPopup/Subscriptions/---",	NULL, "---", NULL, NULL, NULL }, 
	{"FolderViewPopup/Subscriptions/Subscribe",	NULL, N_("_Subscribe..."), NULL, NULL, G_CALLBACK(subscribe_cb) },
	{"FolderViewPopup/Subscriptions/Unsubscribe",	NULL, N_("_Unsubscribe..."), NULL, NULL, G_CALLBACK(unsubscribe_cb) },

	{"FolderViewPopup/CheckNewMessages",	NULL, N_("_Check for new messages"), NULL, NULL, G_CALLBACK(update_tree_cb) }, /*0*/
	{"FolderViewPopup/CheckNewFolders",	NULL, N_("C_heck for new folders"), NULL, NULL, G_CALLBACK(update_tree_cb) }, /*1*/
	{"FolderViewPopup/RebuildTree",		NULL, N_("R_ebuild folder tree"), NULL, NULL, G_CALLBACK(update_tree_cb) }, /*2*/
};

static GtkToggleActionEntry imap_toggle_popup_entries[] =
{
	{"FolderViewPopup/Subscriptions/ShowOnlySubs",	NULL, N_("Show only subscribed _folders"), NULL, NULL, G_CALLBACK(subscribed_cb) }, 
};

static void set_sensitivity(GtkUIManager *ui_manager, FolderItem *item);
static void add_menuitems(GtkUIManager *ui_manager, FolderItem *item);

static FolderViewPopup imap_popup =
{
	"imap",
	"<IMAPFolder>",
	imap_popup_entries,
	G_N_ELEMENTS(imap_popup_entries),
	imap_toggle_popup_entries,
	G_N_ELEMENTS(imap_toggle_popup_entries),
	NULL, 0, 0, NULL,
	add_menuitems,
	set_sensitivity
};

void imap_gtk_init(void)
{
	folderview_register_popup(&imap_popup);
}

static void add_menuitems(GtkUIManager *ui_manager, FolderItem *item)
{
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CreateNewFolder", "FolderViewPopup/CreateNewFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorIMAP1", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "RenameFolder", "FolderViewPopup/RenameFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "MoveFolder", "FolderViewPopup/MoveFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CopyFolder", "FolderViewPopup/CopyFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorIMAP2", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "DeleteFolder", "FolderViewPopup/DeleteFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorIMAP3", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "Synchronise", "FolderViewPopup/Synchronise", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "DownloadMessages", "FolderViewPopup/DownloadMessages", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorIMAP4", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "Subscriptions", "FolderViewPopup/Subscriptions", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup/Subscriptions", "ShowOnlySubs", "FolderViewPopup/Subscriptions/ShowOnlySubs", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup/Subscriptions", "SeparatorIMAP5", "FolderViewPopup/Subscriptions/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup/Subscriptions", "Subscribe", "FolderViewPopup/Subscriptions/Subscribe", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup/Subscriptions", "Unsubscribe", "FolderViewPopup/Subscriptions/Unsubscribe", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CheckNewMessages", "FolderViewPopup/CheckNewMessages", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CheckNewFolders", "FolderViewPopup/CheckNewFolders", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "RebuildTree", "FolderViewPopup/RebuildTree", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorIMAP6", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
}

static void set_sensitivity(GtkUIManager *ui_manager, FolderItem *item)
{
	gboolean folder_is_normal = 
			item != NULL &&
			item->stype == F_NORMAL &&
			!folder_has_parent_of_type(item, F_OUTBOX) &&
			!folder_has_parent_of_type(item, F_DRAFT) &&
			!folder_has_parent_of_type(item, F_QUEUE) &&
			!folder_has_parent_of_type(item, F_TRASH);

#define SET_SENS(name, sens) \
	cm_menu_set_sensitive_full(ui_manager, "Popup/"name, sens)

	SET_SENS("FolderViewPopup/CreateNewFolder",   item && item->no_sub == FALSE);
	SET_SENS("FolderViewPopup/RenameFolder",       item && item->stype == F_NORMAL && folder_item_parent(item) != NULL);
	SET_SENS("FolderViewPopup/MoveFolder", 	    item && folder_is_normal && folder_item_parent(item) != NULL);
	SET_SENS("FolderViewPopup/DeleteFolder", 	    item && item->stype == F_NORMAL && folder_item_parent(item) != NULL);

	SET_SENS("FolderViewPopup/CheckNewMessages", folder_item_parent(item) == NULL);
	SET_SENS("FolderViewPopup/CheckNewFolders",  folder_item_parent(item) == NULL);
	SET_SENS("FolderViewPopup/RebuildTree",    folder_item_parent(item) == NULL);

	SET_SENS("FolderViewPopup/Synchronise",    
			item ? (folder_item_parent(item) != NULL
			&& folder_want_synchronise(item->folder))
			: FALSE);
	SET_SENS("FolderViewPopup/DownloadMessages", item && !item->no_select);

	SET_SENS("FolderViewPopup/CheckNewMessages", folder_item_parent(item) == NULL);
	SET_SENS("FolderViewPopup/CheckNewFolders",  folder_item_parent(item) == NULL);
	SET_SENS("FolderViewPopup/RebuildTree",    folder_item_parent(item) == NULL);
	
	SET_SENS("FolderViewPopup/Subscriptions/Unsubscribe",    item && item->stype == F_NORMAL && folder_item_parent(item) != NULL);
	SET_SENS("FolderViewPopup/Subscriptions/Subscribe",    TRUE);
	if (item && item->folder && item->folder->account)
		cm_toggle_menu_set_active_full(ui_manager, "Popup/FolderViewPopup/Subscriptions/ShowOnlySubs",
			item->folder->account->imap_subsonly);

#undef SET_SENS
}

static void new_folder_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item;
	FolderItem *new_item;
	gchar *new_folder;
	gchar *name;
	gchar *p;
	gchar separator = '/';
	
	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->folder != NULL);
	cm_return_if_fail(item->folder->account != NULL);

	new_folder = input_dialog_with_checkbtn
		(_("New folder"),
		 _("Input the name of new folder:\n"
		   "(if you want to create a folder to store subfolders\n"
		   "only and no mail, append '/' to the folder name)"),
		 _("NewFolder"),
		 _("Inherit properties from parent folder"),
		 &(prefs_common.inherit_folder_props));

	if (!new_folder) return;
	AUTORELEASE_STR(new_folder, {g_free(new_folder); return;});

	separator = imap_get_path_separator_for_item(item);

	p = strchr(new_folder, separator);
	if (p && *(p + 1) != '\0') {
		alertpanel_error(_("'%c' can't be included in folder name."),
				 separator);
		return;
	}
	p = strchr(new_folder, '/');
	if (p && *(p + 1) != '\0') {
		alertpanel_error(_("'%c' can't be included in folder name."),
				 '/');
		return;
	}

	name = trim_string(new_folder, 32);
	AUTORELEASE_STR(name, {g_free(name); return;});

	/* find whether the directory already exists */
	if (folder_find_child_item_by_name(item, new_folder)) {
		alertpanel_error(_("The folder '%s' already exists."), name);
		return;
	}

	new_item = folder_create_folder(item, new_folder);
	if (!new_item) {
		alertpanel_error(_("Can't create the folder '%s'."), name);
		return;
	}

	if (prefs_common.inherit_folder_props) {
		folder_item_prefs_copy_prefs(item, new_item);
	}

	folder_write_list();
}

static void rename_folder_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *item;
	gchar *new_folder;
	gchar *name;
	gchar *message;
	gchar *old_id;
	gchar *new_id;
	gchar *base;
	gchar separator = '/';

	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->path != NULL);
	cm_return_if_fail(item->folder != NULL);

	name = trim_string(item->name, 32);
	message = g_strdup_printf(_("Input new name for '%s':"), name);
	base = g_path_get_basename(item->path);
	new_folder = input_dialog(_("Rename folder"), message, base);
	g_free(base);
	g_free(message);
	g_free(name);
	if (!new_folder) return;
	AUTORELEASE_STR(new_folder, {g_free(new_folder); return;});

	separator = imap_get_path_separator_for_item(item);
	if (strchr(new_folder, separator) != NULL) {
		alertpanel_error(_("'%c' can't be included in folder name."),
				 separator);
		return;
	}
	if (strchr(new_folder, '/') != NULL) {
		alertpanel_error(_("`%c' can't be included in folder name."),
				 '/');
		return;
	}

	if (folder_find_child_item_by_name(folder_item_parent(item), new_folder)) {
		name = trim_string(new_folder, 32);
		alertpanel_error(_("The folder '%s' already exists."), name);
		g_free(name);
		return;
	}

	old_id = folder_item_get_identifier(item);
	
	if (folder_item_rename(item, new_folder) < 0) {
		alertpanel_error(_("The folder could not be renamed.\n"
				   "The new folder name is not allowed."));
		g_free(old_id);
		return;
	}

	new_id = folder_item_get_identifier(item);
	prefs_filtering_rename_path(old_id, new_id);
	account_rename_path(old_id, new_id);
	prefs_actions_rename_path(old_id, new_id);
	g_free(old_id);
	g_free(new_id);

	folder_item_prefs_save_config_recursive(item);
	folder_write_list();
}

static void move_folder_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *from_folder = NULL, *to_folder = NULL;

	from_folder = folderview_get_selected_item(folderview);
	if (!from_folder || from_folder->folder->klass != imap_get_class())
		return;

	to_folder = foldersel_folder_sel(from_folder->folder, FOLDER_SEL_MOVE, NULL, TRUE);
	if (!to_folder)
		return;
	
	folderview_move_folder(folderview, from_folder, to_folder, 0);
}

static void copy_folder_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *from_folder = NULL, *to_folder = NULL;

	from_folder = folderview_get_selected_item(folderview);
	if (!from_folder || from_folder->folder->klass != imap_get_class())
		return;

	to_folder = foldersel_folder_sel(from_folder->folder, FOLDER_SEL_MOVE, NULL, TRUE);
	if (!to_folder)
		return;
	
	folderview_move_folder(folderview, from_folder, to_folder, 1);
}

static void delete_folder_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item;
	gchar *message, *name;
	AlertValue avalue;
	gchar *old_id;

	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->path != NULL);
	cm_return_if_fail(item->folder != NULL);

	name = trim_string(item->name, 32);
	AUTORELEASE_STR(name, {g_free(name); return;});
	message = g_markup_printf_escaped
		(_("All folders and messages under '%s' will be permanently deleted. "
		   "Recovery will not be possible.\n\n"
		   "Do you really want to delete?"), name);
	avalue = alertpanel_full(_("Delete folder"), message,
		 		 GTK_STOCK_CANCEL, GTK_STOCK_DELETE, NULL, FALSE,
				 NULL, ALERT_WARNING, G_ALERTDEFAULT);
	g_free(message);
	if (avalue != G_ALERTALTERNATE) return;

	old_id = folder_item_get_identifier(item);

	if (folderview->opened == folderview->selected ||
	    gtk_cmctree_is_ancestor(ctree,
				  folderview->selected,
				  folderview->opened)) {
		summary_clear_all(folderview->summaryview);
		folderview->opened = NULL;
	}

	if (item->folder->klass->remove_folder(item->folder, item) < 0) {
		folder_item_scan(item);
		alertpanel_error(_("Can't remove the folder '%s'."), name);
		g_free(old_id);
		return;
	}

	folder_write_list();

	prefs_filtering_delete_path(old_id);
	g_free(old_id);

}

#define DO_ACTION(name, act)	{ if (!strcmp(a_name, name)) act; }

static void update_tree_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *item;
	const gchar *a_name = gtk_action_get_name(action);

	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);

	summary_show(folderview->summaryview, NULL);

	cm_return_if_fail(item->folder != NULL);

	DO_ACTION("FolderViewPopup/CheckNewMessages", folderview_check_new(item->folder));
	DO_ACTION("FolderViewPopup/CheckNewFolders", folderview_rescan_tree(item->folder, FALSE));
	DO_ACTION("FolderViewPopup/RebuildTree", folderview_rescan_tree(item->folder, TRUE));
}

static void sync_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *item;

	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);
	folder_synchronise(item->folder);
}

void imap_gtk_synchronise(FolderItem *item, gint days)
{
	MainWindow *mainwin = mainwindow_get_mainwindow();
	FolderView *folderview = mainwin->folderview;
	
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->folder != NULL);

	main_window_cursor_wait(mainwin);
	inc_lock();
	main_window_lock(mainwin);
	gtk_widget_set_sensitive(folderview->ctree, FALSE);
	main_window_progress_on(mainwin);
	GTK_EVENTS_FLUSH();
	if (item->no_select == FALSE) {
		GSList *mlist;
		GSList *cur;
		gint num = 0;
		gint total = item->total_msgs;
		time_t t = time(NULL);

		mlist = folder_item_get_msg_list(item);
		for (cur = mlist; cur != NULL; cur = cur->next) {
			MsgInfo *msginfo = (MsgInfo *)cur->data;
			gint age = (t - msginfo->date_t) / (60*60*24);
			if (days == 0 || age <= days)
				imap_cache_msg(msginfo->folder, msginfo->msgnum);
			statusbar_progress_all(num++,total, 100);
			if (num % 100 == 0)
				GTK_EVENTS_FLUSH();
		}

		statusbar_progress_all(0,0,0);
		procmsg_msg_list_free(mlist);
	}

	folder_set_ui_func(item->folder, NULL, NULL);
	main_window_progress_off(mainwin);
	gtk_widget_set_sensitive(folderview->ctree, TRUE);
	main_window_unlock(mainwin);
	inc_unlock();
	main_window_cursor_normal(mainwin);
}

static void chk_update_val(GtkWidget *widget, gpointer data)
{
        gboolean *val = (gboolean *)data;
	*val = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
}

static gboolean imap_gtk_subscribe_func(GNode *node, gpointer data)
{
	FolderItem *item = node->data;
	gboolean action = GPOINTER_TO_INT(data);
	
	if (item->path)
		imap_subscribe(item->folder, item, NULL, action);

	return FALSE;
}

static void subscribe_cb_full(FolderView *folderview, guint action)
{
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item;
	gchar *message, *name;
	AlertValue avalue;
	GtkWidget *rec_chk;
	gboolean recurse = FALSE;

	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->folder != NULL);

	name = trim_string(item->name, 32);
	AUTORELEASE_STR(name, {g_free(name); return;});
	
	if (action && item->folder->account->imap_subsonly) {
		GList *child_list = NULL;
		GList *transc_list = NULL;

		message = g_markup_printf_escaped
			(_("Do you want to search for unsubscribed subfolders of '%s'?"),
			 name);

		rec_chk = gtk_check_button_new_with_label(_("Search recursively"));

		g_signal_connect(G_OBJECT(rec_chk), "toggled", 
				G_CALLBACK(chk_update_val), &recurse);

		avalue = alertpanel_full(_("Subscriptions"), message,
		 			 GTK_STOCK_CANCEL, _("+_Search"), NULL, FALSE,
					 rec_chk, ALERT_QUESTION, G_ALERTDEFAULT);
		g_free(message);
		if (avalue != G_ALERTALTERNATE) return;
		
		child_list = imap_scan_subtree(item->folder, item, TRUE, recurse);
		
		if (child_list) {
			GList *cur;
			int r = -1;
			gchar *msg = g_strdup_printf(_("Choose a subfolder of %s to subscribe to: "),
					item->name); 
			gchar *child_folder = NULL;
			
			for (cur = child_list; cur; cur = cur->next) {
				transc_list = g_list_append(transc_list, 
					imap_modified_utf7_to_utf8(cur->data, FALSE));
			}
			
			transc_list = g_list_sort(transc_list, g_str_equal);
			
			child_folder = input_dialog_combo(_("Subscribe"), 
					msg,
					transc_list->next?_("All of them"):transc_list->data, transc_list);
			g_free(msg);
			if (child_folder && strcmp(child_folder, _("All of them"))) {
				gchar *transc_folder = imap_utf8_to_modified_utf7(child_folder, FALSE);
				r = imap_subscribe(item->folder, NULL, transc_folder, TRUE);
				g_free(transc_folder);
			} else if (child_folder) {
				for (cur = child_list; cur; cur = cur->next) 
					r = imap_subscribe(item->folder, NULL, (gchar *)cur->data, TRUE);
			}
			g_free(child_folder);
			for (cur = child_list; cur; cur = cur->next) 
				g_free((gchar *)cur->data);
			for (cur = transc_list; cur; cur = cur->next) 
				g_free((gchar *)cur->data);
			if (r == 0)
				folderview_fast_rescan_tree(item->folder);
		} else {
			alertpanel_notice(_("This folder is already subscribed and "
				  "has no unsubscribed subfolders.\n\nIf there are new folders, "
				  "created and subscribed to from another client, use \"Check "
				  "for new folders\" at the mailbox's root folder."));
		}
		g_list_free(child_list);
		return;
	}
	message = g_markup_printf_escaped
		(_("Do you want to %s the '%s' folder?"),
		   action?_("subscribe"):_("unsubscribe"), name);
	
	rec_chk = gtk_check_button_new_with_label(_("Apply to subfolders"));
	
	g_signal_connect(G_OBJECT(rec_chk), "toggled", 
			G_CALLBACK(chk_update_val), &recurse);

	avalue = alertpanel_full(_("Subscriptions"), message,
		 		 GTK_STOCK_CANCEL, action?_("+_Subscribe"):_("+_Unsubscribe"), NULL, FALSE,
				 rec_chk, ALERT_QUESTION, G_ALERTDEFAULT);
	g_free(message);
	if (avalue != G_ALERTALTERNATE) return;
	
	
	if (!action) {
		if (folderview->opened == folderview->selected ||
		    gtk_cmctree_is_ancestor(ctree,
					  folderview->selected,
					  folderview->opened)) {
			summary_clear_all(folderview->summaryview);
			folderview->opened = NULL;
		}
	}

	if (recurse) {
		g_node_traverse(item->node, G_PRE_ORDER,
			G_TRAVERSE_ALL, -1, imap_gtk_subscribe_func, GINT_TO_POINTER(action));
	} else {
		imap_subscribe(item->folder, item, NULL, action);
	}

	if (!action && item->folder->account->imap_subsonly)
		folderview_fast_rescan_tree(item->folder);
}

static void subscribe_cb(GtkAction *action, gpointer data)
{
	subscribe_cb_full((FolderView *)data, 1);
}

static void unsubscribe_cb(GtkAction *action, gpointer data)
{
	subscribe_cb_full((FolderView *)data, 0);
}

static void subscribed_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	
	if (!item || !item->folder || !item->folder->account)
		return;
	if (item->folder->account->imap_subsonly == gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)))
		return;

	if (folderview->opened == folderview->selected ||
	    gtk_cmctree_is_ancestor(ctree,
				  folderview->selected,
				  folderview->opened)) {
		summary_clear_all(folderview->summaryview);
		folderview->opened = NULL;
	}

	item->folder->account->imap_subsonly = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));
	folderview_fast_rescan_tree(item->folder);
}

static void download_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item;

	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	imap_gtk_synchronise(item, 0);
}
