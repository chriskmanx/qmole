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
#include "folder_item_prefs.h"
#include "folderview.h"
#include "menu.h"
#include "account.h"
#include "alertpanel.h"
#include "inputdialog.h"
#include "mh.h"
#include "foldersel.h"
#include "prefs_common.h"
#include "prefs_actions.h"

static void new_folder_cb(GtkAction *action, gpointer data);
static void delete_folder_cb(GtkAction *action, gpointer data);
static void rename_folder_cb(GtkAction *action, gpointer data);
static void move_folder_cb(GtkAction *action, gpointer data);
static void copy_folder_cb(GtkAction *action, gpointer data);
static void update_tree_cb(GtkAction *action, gpointer data);
static void remove_mailbox_cb(GtkAction *action, gpointer data);

static GtkActionEntry mh_popup_entries[] = 
{
	{"FolderViewPopup/CreateNewFolder",	NULL, N_("Create _new folder..."), NULL, NULL, G_CALLBACK(new_folder_cb) },
	{"FolderViewPopup/RenameFolder",	NULL, N_("_Rename folder..."), NULL, NULL, G_CALLBACK(rename_folder_cb) },
	{"FolderViewPopup/MoveFolder",		NULL, N_("M_ove folder..."), NULL, NULL, G_CALLBACK(move_folder_cb) },
	{"FolderViewPopup/CopyFolder",		NULL, N_("Cop_y folder..."), NULL, NULL, G_CALLBACK(copy_folder_cb) },
	{"FolderViewPopup/DeleteFolder",	NULL, N_("_Delete folder..."), NULL, NULL, G_CALLBACK(delete_folder_cb) },
	{"FolderViewPopup/CheckNewMessages",	NULL, N_("_Check for new messages"), NULL, NULL, G_CALLBACK(update_tree_cb) }, /*0*/
	{"FolderViewPopup/CheckNewFolders",	NULL, N_("C_heck for new folders"), NULL, NULL, G_CALLBACK(update_tree_cb) }, /*1*/
	{"FolderViewPopup/RebuildTree",		NULL, N_("R_ebuild folder tree"), NULL, NULL, G_CALLBACK(update_tree_cb) }, /*2*/
	{"FolderViewPopup/RemoveMailbox",	NULL, N_("Remove _mailbox..."), NULL, NULL, G_CALLBACK(remove_mailbox_cb) },
};			
static void set_sensitivity(GtkUIManager *ui_manager, FolderItem *item);
static void add_menuitems(GtkUIManager *ui_manager, FolderItem *item);

static FolderViewPopup mh_popup =
{
	"mh",
	"<MHFolder>",
	mh_popup_entries,
	G_N_ELEMENTS(mh_popup_entries),
	NULL, 0,
	NULL, 0, 0, NULL,
	add_menuitems,
	set_sensitivity
};

void mh_gtk_init(void)
{
	folderview_register_popup(&mh_popup);
}

static void add_menuitems(GtkUIManager *ui_manager, FolderItem *item)
{
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CreateNewFolder", "FolderViewPopup/CreateNewFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorMH1", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "RenameFolder", "FolderViewPopup/RenameFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "MoveFolder", "FolderViewPopup/MoveFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CopyFolder", "FolderViewPopup/CopyFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorMH2", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "DeleteFolder", "FolderViewPopup/DeleteFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorMH3", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CheckNewMessages", "FolderViewPopup/CheckNewMessages", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "CheckNewFolders", "FolderViewPopup/CheckNewFolders", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "RebuildTree", "FolderViewPopup/RebuildTree", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorMH4", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "RemoveMailbox", "FolderViewPopup/RemoveMailbox", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popup/FolderViewPopup", "SeparatorMH5", "FolderViewPopup/---", GTK_UI_MANAGER_SEPARATOR)
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

	SET_SENS("FolderViewPopup/CreateNewFolder",   TRUE);
	SET_SENS("FolderViewPopup/RenameFolder",       item && item->stype == F_NORMAL && folder_item_parent(item) != NULL);
	SET_SENS("FolderViewPopup/MoveFolder", 	    folder_is_normal && folder_item_parent(item) != NULL);
	SET_SENS("FolderViewPopup/DeleteFolder", 	    item && item->stype == F_NORMAL && folder_item_parent(item) != NULL);

	SET_SENS("FolderViewPopup/CheckNewMessages", folder_item_parent(item) == NULL);
	SET_SENS("FolderViewPopup/CheckNewFolders",  folder_item_parent(item) == NULL);
	SET_SENS("FolderViewPopup/RebuildTree",    folder_item_parent(item) == NULL);

	SET_SENS("FolderViewPopup/RemoveMailbox",         folder_item_parent(item) == NULL);

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

	if (!folderview->selected) return;

	item = gtk_cmctree_node_get_row_data(ctree, folderview->selected);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->folder != NULL);

	new_folder = input_dialog_with_checkbtn(_("New folder"),
						_("Input the name of new folder:"),
						_("NewFolder"),
						_("Inherit properties from parent folder"),
						&(prefs_common.inherit_folder_props));
	if (!new_folder) return;
	AUTORELEASE_STR(new_folder, {g_free(new_folder); return;});

	p = strchr(new_folder, G_DIR_SEPARATOR);
	if (p) {
		alertpanel_error(_("'%c' can't be included in folder name."),
				 G_DIR_SEPARATOR);
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

static void delete_folder_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	GtkCMCTree *ctree = GTK_CMCTREE(folderview->ctree);
	FolderItem *item;
	gchar *message, *name;
	AlertValue avalue;
	gchar *old_id;

	item = folderview_get_selected_item(folderview);
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

	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->path != NULL);
	cm_return_if_fail(item->folder != NULL);

	name = trim_string(item->name, 32);
	message = g_strdup_printf(_("Input new name for '%s':"), name);
	base = g_path_get_basename(item->path);
	new_folder = input_dialog(_("Rename folder"), message, base);
	g_free(message);
	g_free(name);
	g_free(base);
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
	if (!from_folder || from_folder->folder->klass != mh_get_class())
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
	if (!from_folder || from_folder->folder->klass != mh_get_class())
		return;

	to_folder = foldersel_folder_sel(from_folder->folder, FOLDER_SEL_MOVE, NULL, TRUE);
	if (!to_folder)
		return;
	
	folderview_move_folder(folderview, from_folder, to_folder, 1);
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

static void remove_mailbox_cb(GtkAction *action, gpointer data)
{
	FolderView *folderview = (FolderView *)data;
	FolderItem *item;
	gchar *name;
	gchar *message;
	AlertValue avalue;

	item = folderview_get_selected_item(folderview);
	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->folder != NULL);
	if (folder_item_parent(item)) return;

	name = trim_string(item->folder->name, 32);
	message = g_markup_printf_escaped
		(_("Really remove the mailbox '%s' ?\n"
		   "(The messages are NOT deleted from the disk)"), name);
	avalue = alertpanel_full(_("Remove mailbox"), message,
		 		 GTK_STOCK_CANCEL, _("_Remove"), NULL, FALSE,
				 NULL, ALERT_WARNING, G_ALERTDEFAULT);
			    
	g_free(message);
	g_free(name);
	if (avalue != G_ALERTALTERNATE) return;

	folderview_unselect(folderview);
	summary_clear_all(folderview->summaryview);

	folder_destroy(item->folder);
}

