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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include <errno.h>

#include "main.h"
#include "mainwindow.h"
#include "folderview.h"
#include "folder.h"
#include "account.h"
#include "prefs_gtk.h"
#include "prefs_account.h"
#include "prefs_common.h"
#include "folder_item_prefs.h"
#include "compose.h"
#include "manage_window.h"
#include "stock_pixmap.h"
#include "inc.h"
#include "gtkutils.h"
#include "utils.h"
#include "alertpanel.h"
#include "procheader.h"
#include "customheader.h"
#include "remotefolder.h"
#include "manual.h"
#include "filtering.h"
#include "prefs_actions.h"

enum {
	ACCOUNT_IS_DEFAULT,
	ACCOUNT_ENABLE_GET_ALL,	
	ACCOUNT_NAME,
	ACCOUNT_PROTOCOL,
	ACCOUNT_SERVER,
	ACCOUNT_DATA,
	N_ACCOUNT_COLUMNS
};


typedef enum
{
	COL_DEFAULT	= 0,
	COL_GETALL	= 1,
	COL_NAME	= 2,
	COL_PROTOCOL	= 3,
	COL_SERVER	= 4
} EditAccountColumnPos;

# define N_EDIT_ACCOUNT_COLS	5

PrefsAccount *cur_account;

static GList *account_list = NULL;
static gboolean account_list_dirty = FALSE;

static struct EditAccount {
	GtkWidget *window;
	GtkWidget *list_view;
	GtkWidget *close_btn;
} edit_account;

static void account_edit_create		(void);
static void	      account_destroy		(PrefsAccount	*ac_prefs);
static void	      account_set_as_default	(PrefsAccount	*ac_prefs);
static void	      account_set_menu		(void);

static void account_edit_prefs		(GtkWidget *widget, gpointer data);
static void account_delete		(GtkWidget *widget, gpointer data);
static void account_clone		(GtkWidget *widget, gpointer data);

static void account_up			(GtkWidget *widget, gpointer data);
static void account_down		(GtkWidget *widget, gpointer data);

static void account_set_default		(GtkWidget *widget, gpointer data);

static void account_edit_close		(GtkWidget *widget, gpointer data);

static gint account_delete_event	(GtkWidget	*widget,
					 GdkEventAny	*event,
					 gpointer	 data);
static void account_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation);
#ifndef MAEMO
static gboolean account_key_pressed	(GtkWidget	*widget,
					 GdkEventKey	*event,
					 gpointer	 data);
#endif
static gboolean account_search_func_cb (GtkTreeModel *model, gint column, 
						const gchar *key, GtkTreeIter *iter, 
						gpointer search_data);
static void account_list_view_add	(PrefsAccount	*ac_prefs);
static void account_list_view_set	(void);

static void account_list_set		(void);

typedef struct FindAccountInStore {
	gint		 account_id;
	GtkTreePath	*path;
	GtkTreeIter	 iter;
} FindAccountInStore;

static GtkListStore* account_create_data_store	(void);

static void account_list_view_insert_account_item (GtkListStore	*list_store, 
						   const gchar	*account_name,
						   const gchar	*protocol, 
						   const gchar	*server_name,
						   gboolean	 is_default, 
						   gboolean	 is_get_all,
						   PrefsAccount *account_data);

static GtkWidget *account_list_view_create	(void);
static void account_create_list_view_columns	(GtkWidget *list_view);

static gint account_list_view_get_selected_account_id		(GtkWidget *list_view);
static GtkTreePath *account_list_view_get_selected_account_path	(GtkWidget *list_view);
static PrefsAccount *account_list_view_get_selected_account		(GtkWidget *list_view);
static gboolean account_list_view_select_account			(GtkWidget *list_view, 
								 gint	    account_id);

static void account_list_view_set_default_by_id(GtkWidget *list_view,
						gint account_id);

static gboolean set_new_default_account		(GtkTreeModel *model,
						 GtkTreePath  *path,
						 GtkTreeIter  *iter,
						 gint	      *account_id);

static gboolean find_account_in_store		(GtkTreeModel *model,
						 GtkTreePath  *path,
						 GtkTreeIter  *iter,
						 FindAccountInStore *data);

static void account_get_all_toggled		(GtkCellRendererToggle	*widget, 
						 gchar			*path, 
						 GtkWidget		*list_view);
						 
static void account_double_clicked		(GtkTreeView		*list_view,
						 GtkTreePath		*path,
						 GtkTreeViewColumn	*column,
						 gpointer		 data);
						 
static void drag_begin				(GtkTreeView *list_view,
						 GdkDragContext *context,
						 gpointer data);

static void drag_end				(GtkTreeView *list_view,
						 GdkDragContext *context,
						 gpointer data);
		      
static void account_row_changed_while_drag_drop	(GtkTreeModel *model, 
						 GtkTreePath  *path,
						 GtkTreeIter  *iter,
						 gpointer      arg3,
						 GtkTreeView  *list_view);

static void account_flush_state(void)
{
	account_set_menu();
	main_window_reflect_prefs_all();

	account_list_dirty = FALSE;
}

void account_read_config_all(void)
{
	GSList *ac_label_list = NULL, *cur;
	gchar *rcpath;
	FILE *fp;
	gchar buf[PREFSBUFSIZE];
	PrefsAccount *ac_prefs;

	debug_print("Reading all config for each account...\n");

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, ACCOUNT_RC, NULL);
	if ((fp = g_fopen(rcpath, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(rcpath, "fopen");
		g_free(rcpath);
		return;
	}
	g_free(rcpath);

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		if (!strncmp(buf, "[Account: ", 10)) {
			strretchomp(buf);
			memmove(buf, buf + 1, strlen(buf));
			buf[strlen(buf) - 1] = '\0';
			debug_print("Found label: %s\n", buf);
			ac_label_list = g_slist_append(ac_label_list,
						       g_strdup(buf));
		}
	}
	fclose(fp);

	/* read config data from file */
	cur_account = NULL;
	for (cur = ac_label_list; cur != NULL; cur = cur->next) {
		ac_prefs = prefs_account_new();
		prefs_account_read_config(ac_prefs, (gchar *)cur->data);
		account_list = g_list_append(account_list, ac_prefs);
		if (ac_prefs->is_default)
			cur_account = ac_prefs;
	}
	/* if default is not set, assume first account as default */
	if (!cur_account && account_list) {
		ac_prefs = (PrefsAccount *)account_list->data;
		account_set_as_default(ac_prefs);
		cur_account = ac_prefs;
	}

	account_set_menu();
	main_window_reflect_prefs_all_now();

	while (ac_label_list) {
		g_free(ac_label_list->data);
		ac_label_list = g_slist_remove(ac_label_list,
					       ac_label_list->data);
	}
}

void account_write_config_all(void)
{
	prefs_account_write_config_all(account_list);
}

/*
 * account_find_all_from_address:
 * @ac_list: initial list of accounts. NULL to create a new one.
 * Accounts found in the @address will be appended to this list.
 * @address: Email address string.
 *
 * Find all the mail (not news) accounts within the specified address.
 *
 * Return value: the original accounts list with the found accounts appended.
 */
GList *account_find_all_from_address(GList *ac_list, const gchar *address)
{
	GList *cur;
	PrefsAccount *ac;

	if (address == NULL)
		return ac_list;

	for (cur = account_list; cur != NULL; cur = cur->next) {
		ac = (PrefsAccount *)cur->data;
		if (ac->protocol != A_NNTP && ac->address &&
		    strcasestr(address, ac->address) != NULL)
			ac_list = g_list_append(ac_list, ac);
	}
	return ac_list;
}
	
PrefsAccount *account_find_from_smtp_server(const gchar *address,
					    const gchar *smtp_server)
{
	GList *cur;
	PrefsAccount *ac;

	cm_return_val_if_fail(address != NULL, NULL);
	cm_return_val_if_fail(smtp_server != NULL, NULL);

	for (cur = account_list; cur != NULL; cur = cur->next) {
		ac = (PrefsAccount *)cur->data;
		if (!strcmp2(address, ac->address) &&
		    !strcmp2(smtp_server, ac->smtp_server))
			return ac;
	}

	return NULL;
}

/*
 * account_find_from_address:
 * @address: Email address string.
 *
 * Find a mail (not news if newsgroups_ok is FALSE) account with the specified email address.
 *
 * Return value: The found account, or NULL if not found.
 */
PrefsAccount *account_find_from_address(const gchar *address, gboolean newsgroups_ok)
{
	GList *cur;
	PrefsAccount *ac;

	cm_return_val_if_fail(address != NULL, NULL);

	for (cur = account_list; cur != NULL; cur = cur->next) {
		ac = (PrefsAccount *)cur->data;
		if ((ac->protocol != A_NNTP || newsgroups_ok) && ac->address &&
		    g_ascii_strcasecmp(address, ac->address) == 0)
			return ac;
	}

	return NULL;
}

PrefsAccount *account_find_from_id(gint id)
{
	GList *cur;
	PrefsAccount *ac;

	for (cur = account_list; cur != NULL; cur = cur->next) {
		ac = (PrefsAccount *)cur->data;
		if (id == ac->account_id)
			return ac;
	}

	return NULL;
}

PrefsAccount *account_find_from_item(FolderItem *item)
{
	PrefsAccount *ac;

	cm_return_val_if_fail(item != NULL, NULL);

	ac = item->account;
	if (!ac) {
		FolderItem *cur_item = folder_item_parent(item);
		while (cur_item != NULL) {
			if (cur_item->account && cur_item->apply_sub) {
				ac = cur_item->account;
				break;
			}				
			cur_item = folder_item_parent(cur_item);
		}
	}
	if (!ac)
		ac = item->folder->account;

	return ac;
}

static void account_set_menu(void)
{
	main_window_set_account_menu(account_list);
}

void account_set_menu_only_toolbar(void)
{
	main_window_set_account_menu_only_toolbar(account_list);
}

GList *account_get_list(void)
{
	return account_list;
}

void account_edit_open(gpointer a, gpointer b)
{
	inc_lock();

	account_list_dirty = FALSE;

	if (compose_get_compose_list()) {
		alertpanel_error(_("Some composing windows are open.\n"
				   "Please close all the composing "
				   "windows before editing the accounts."));
		inc_unlock();
		return;
	}

	debug_print("Opening account edit window...\n");

	if (!edit_account.window)
		account_edit_create();

	account_list_view_set();

	manage_window_set_transient(GTK_WINDOW(edit_account.window));
	gtk_widget_grab_focus(edit_account.close_btn);
	gtk_widget_show(edit_account.window);
	gtk_window_set_modal(GTK_WINDOW(edit_account.window), TRUE);
	manage_window_focus_in(edit_account.window, NULL, NULL);
}

void account_add(void)
{
	PrefsAccount *ac_prefs;

	ac_prefs = prefs_account_open(NULL, &account_list_dirty);

	if (!ac_prefs) return;

	account_list = g_list_append(account_list, ac_prefs);

	if (ac_prefs->is_default)
		account_set_as_default(ac_prefs);

	account_list_view_set();

	if (ac_prefs->protocol == A_IMAP4 || ac_prefs->protocol == A_NNTP) {
		Folder *folder;

		if (ac_prefs->protocol == A_IMAP4) {
			folder = folder_new(folder_get_class_from_string("imap"), ac_prefs->account_name,
					    ac_prefs->recv_server);
		} else {
			folder = folder_new(folder_get_class_from_string("news"), ac_prefs->account_name,
					    ac_prefs->nntp_server);
		}
		if (folder == NULL) {
			alertpanel_error(_("Can't create folder."));
			return;
		}
		folder->account = ac_prefs;
		ac_prefs->folder = folder;
		folder_add(folder);
		if (ac_prefs->protocol == A_IMAP4)
			folder->klass->create_tree(folder);
		folderview_set_all();
		folder_write_list();
	}
}

void account_open(PrefsAccount *ac_prefs)
{
	gboolean prev_default;
	gchar *ac_name, *old_prefix, *new_prefix;
	gboolean account_dirty = FALSE;

	cm_return_if_fail(ac_prefs != NULL);

	prev_default = ac_prefs->is_default;
	Xstrdup_a(ac_name, ac_prefs->account_name ? ac_prefs->account_name : "",
		  return);

	prefs_account_open(ac_prefs, &account_dirty);

	if (account_dirty) {
		if (!prev_default && ac_prefs->is_default)
			account_set_as_default(ac_prefs);

		if (ac_prefs->folder && strcmp2(ac_name, ac_prefs->account_name) != 0) {
			old_prefix = folder_get_identifier(FOLDER(ac_prefs->folder));
			folder_set_name(FOLDER(ac_prefs->folder),
					ac_prefs->account_name);
			folderview_set_all();
			folder_prefs_save_config_recursive(FOLDER(ac_prefs->folder));
			new_prefix = folder_get_identifier(FOLDER(ac_prefs->folder));

			account_rename_path(old_prefix, new_prefix);
			prefs_filtering_rename_path(old_prefix, new_prefix);
			prefs_actions_rename_path(old_prefix, new_prefix);
			
			g_free(old_prefix);
			g_free(new_prefix);
		}

		account_write_config_all();

		account_flush_state();
	}
}

static void account_set_as_default(PrefsAccount *ac_prefs)
{
	PrefsAccount *ap;
	GList *cur;

	for (cur = account_list; cur != NULL; cur = cur->next) {
		ap = (PrefsAccount *)cur->data;
		if (ap->is_default)
			ap->is_default = FALSE;
	}

	ac_prefs->is_default = TRUE;
}

PrefsAccount *account_get_default(void)
{
	PrefsAccount *ap;
	GList *cur;

	for (cur = account_list; cur != NULL; cur = cur->next) {
		ap = (PrefsAccount *)cur->data;
		if (ap->is_default)
			return ap;
	}

	return NULL;
}

void account_set_missing_folder(void)
{
	PrefsAccount *ap;
	GList *cur;

	for (cur = account_list; cur != NULL; cur = cur->next) {
		ap = (PrefsAccount *)cur->data;
		if ((ap->protocol == A_IMAP4 || ap->protocol == A_NNTP) &&
		    !ap->folder) {
			Folder *folder;

			if (ap->protocol == A_IMAP4) {
				folder = folder_new(folder_get_class_from_string("imap"), ap->account_name,
						    ap->recv_server);
			} else {
				folder = folder_new(folder_get_class_from_string("news"), ap->account_name,
						    ap->nntp_server);
			}
			if (folder == NULL)
				return;
			folder->account = ap;
			ap->folder = folder;
			folder_add(folder);
			if (ap->protocol == A_IMAP4)
				folder->klass->create_tree(folder);
			folder_write_list();

		}
	}
}

#define CHECK_CHANGE_FOLDER(folder) {						\
	if (folder && !strncmp(folder, old_id, strlen(old_id))) {		\
		if (strlen(folder) == strlen(old_id)) {				\
			g_free(folder);						\
			folder = g_strdup(new_id);				\
		} else if (strlen(folder) > strlen(old_id)			\
		  && folder[strlen(old_id)] == G_DIR_SEPARATOR) {		\
			gchar *new_path = g_strdup_printf("%s%s",		\
					new_id, (folder + strlen(old_id)));	\
			g_free(folder);						\
			folder = new_path;					\
		} 								\
	}									\
}

void account_rename_path(const gchar *old_id, const gchar *new_id)
{
	GList *cur = account_list;
	for (; cur != NULL; cur = g_list_next(cur)) {
		PrefsAccount *ap = (PrefsAccount *)cur->data;
		CHECK_CHANGE_FOLDER(ap->inbox);
		CHECK_CHANGE_FOLDER(ap->local_inbox);
		CHECK_CHANGE_FOLDER(ap->queue_folder);
		CHECK_CHANGE_FOLDER(ap->sent_folder);
		CHECK_CHANGE_FOLDER(ap->draft_folder);
		CHECK_CHANGE_FOLDER(ap->trash_folder);
	}
}

FolderItem *account_get_special_folder(PrefsAccount *ac_prefs,
				       SpecialFolderItemType type)
{
	FolderItem *item = NULL;

	cm_return_val_if_fail(ac_prefs != NULL, NULL);

	switch (type) {
	case F_INBOX:
		if (ac_prefs->folder)
			item = FOLDER(ac_prefs->folder)->inbox;
		if (!item)
			item = folder_get_default_inbox();
		break;
	case F_OUTBOX:
		if (ac_prefs->set_sent_folder && ac_prefs->sent_folder) {
			item = folder_find_item_from_identifier
				(ac_prefs->sent_folder);
		}
		if (!item) {
			if (ac_prefs->folder)
				item = FOLDER(ac_prefs->folder)->outbox;
			if (!item)
				item = folder_get_default_outbox_for_class(F_MH);
			if (!item)
				item = folder_get_default_outbox();
		}
		break;
	case F_DRAFT:
		if (ac_prefs->set_draft_folder && ac_prefs->draft_folder) {
			item = folder_find_item_from_identifier
				(ac_prefs->draft_folder);
		}
		if (!item) {
			if (ac_prefs->folder)
				item = FOLDER(ac_prefs->folder)->draft;
			if (!item)
				item = folder_get_default_draft_for_class(F_MH);
			if (!item)
				item = folder_get_default_draft();
		}
		break;
	case F_QUEUE:
		if (ac_prefs->set_queue_folder && ac_prefs->queue_folder) {
			item = folder_find_item_from_identifier
				(ac_prefs->queue_folder);
		}
		if (!item) {
			if (ac_prefs->folder)
				item = FOLDER(ac_prefs->folder)->queue;
			if (!item)
				item = folder_get_default_queue_for_class(F_MH);
			if (!item)
				item = folder_get_default_queue();
		}
		break;
	case F_TRASH:
		if (ac_prefs->set_trash_folder && ac_prefs->trash_folder) {
			item = folder_find_item_from_identifier
				(ac_prefs->trash_folder);
		}
		if (!item) {
			if (ac_prefs->folder)
				item = FOLDER(ac_prefs->folder)->trash;
			if (!item)
				item = folder_get_default_trash_for_class(F_MH);
			if (!item)
				item = folder_get_default_trash();
		}
		break;
	default:
		break;
	}

	return item;
}

void account_destroy(PrefsAccount *ac_prefs)
{
	cm_return_if_fail(ac_prefs != NULL);

	folder_unref_account_all(ac_prefs);

	prefs_account_free(ac_prefs);
	account_list = g_list_remove(account_list, ac_prefs);

	if (cur_account == ac_prefs) cur_account = NULL;
	if (!cur_account && account_list) {
		cur_account = account_get_default();
		if (!cur_account) {
			ac_prefs = (PrefsAccount *)account_list->data;
			account_set_as_default(ac_prefs);
			cur_account = ac_prefs;
		}
	}
}

/*!
 *\brief	Save Gtk object size to prefs dataset
 */
static void account_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.accountswin_width = allocation->width;
	prefs_common.accountswin_height = allocation->height;
}

static void account_edit_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *label;
	GtkWidget *hbox;
	GtkWidget *scrolledwin;
	GtkWidget *list_view;

	GtkWidget *vbox2;
	GtkWidget *add_btn;
	GtkWidget *edit_btn;
	GtkWidget *del_btn;
	GtkWidget *clone_btn;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	GtkWidget *default_btn;

	GtkWidget *confirm_area;
	GtkWidget *help_btn;
	GtkWidget *close_btn;

	static GdkGeometry geometry;

	debug_print("Creating account edit window...\n");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "account");
	gtk_container_set_border_width (GTK_CONTAINER (window), 8);
	gtk_window_set_title (GTK_WINDOW (window), _("Edit accounts"));
	g_signal_connect (G_OBJECT (window), "delete_event",
			  G_CALLBACK (account_delete_event), NULL);
#ifdef MAEMO
	maemo_connect_key_press_to_mainwindow(GTK_WINDOW(window));
#else
	g_signal_connect (G_OBJECT (window), "key_press_event",
			  G_CALLBACK (account_key_pressed), NULL);
#endif			  
	MANAGE_WINDOW_SIGNALS_CONNECT (window);
	gtk_widget_realize(window);

	vbox = gtk_vbox_new (FALSE, 10);
	gtk_widget_show (vbox);
	gtk_container_add (GTK_CONTAINER (window), vbox);

	hbox = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

	label = gtk_label_new
		(_("Using 'Get Mail' will retrieve messages from your Accounts "
		   "in the order given, the checkbox indicates which accounts "
		   "will be included. Bold text indicates the default account."));
	gtk_widget_show (label);
	gtk_widget_set_size_request(GTK_WIDGET(label), 
				    prefs_common.accountswin_width-8, -1);
	gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 4);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);

	hbox = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox), 2);

	scrolledwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwin);
	gtk_box_pack_start (GTK_BOX (hbox), scrolledwin, TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwin),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);

	list_view = account_list_view_create();
	gtk_widget_show(list_view);
	gtk_container_add(GTK_CONTAINER(scrolledwin), list_view);

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (hbox), vbox2, FALSE, FALSE, 0);

	add_btn = gtk_button_new_from_stock(GTK_STOCK_NEW);
	gtk_widget_show (add_btn);
	gtk_box_pack_start (GTK_BOX (vbox2), add_btn, FALSE, FALSE, 4);
	g_signal_connect (G_OBJECT(add_btn), "clicked",
			  G_CALLBACK (account_add), NULL);

	edit_btn = gtk_button_new_from_stock (GTK_STOCK_EDIT);
	gtk_widget_show (edit_btn);
	gtk_box_pack_start (GTK_BOX (vbox2), edit_btn, FALSE, FALSE, 4);
	g_signal_connect (G_OBJECT(edit_btn), "clicked",
			  G_CALLBACK (account_edit_prefs), NULL);

	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_widget_show (del_btn);
	gtk_box_pack_start (GTK_BOX (vbox2), del_btn, FALSE, FALSE, 4);
	g_signal_connect (G_OBJECT(del_btn), "clicked",
			  G_CALLBACK (account_delete), NULL);

	clone_btn = gtk_button_new_from_stock(GTK_STOCK_COPY);
	gtk_widget_show (clone_btn);
	gtk_box_pack_start (GTK_BOX (vbox2), clone_btn, FALSE, FALSE, 4);
	g_signal_connect(G_OBJECT(clone_btn), "clicked",
			 G_CALLBACK(account_clone), NULL);
	
	down_btn = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_widget_show (down_btn);
	gtk_box_pack_end (GTK_BOX (vbox2), down_btn, FALSE, FALSE, 4);
	g_signal_connect (G_OBJECT(down_btn), "clicked",
			  G_CALLBACK (account_down), NULL);

	up_btn = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_widget_show (up_btn);
	gtk_box_pack_end (GTK_BOX (vbox2), up_btn, FALSE, FALSE, 4);
	g_signal_connect (G_OBJECT(up_btn), "clicked",
			  G_CALLBACK (account_up), NULL);

	hbox = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

	default_btn = gtk_button_new_with_mnemonic
		(_(" _Set as default account "));
	gtk_widget_show (default_btn);
	gtk_box_pack_start (GTK_BOX (hbox), default_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT(default_btn), "clicked",
			  G_CALLBACK (account_set_default), NULL);

	gtkut_stock_button_set_create_with_help(&confirm_area, &help_btn,
			&close_btn, GTK_STOCK_CLOSE,
			NULL, NULL, NULL, NULL);
	gtk_widget_show(confirm_area);

	gtk_box_pack_end (GTK_BOX (hbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default (close_btn);

	g_signal_connect (G_OBJECT (close_btn), "clicked",
			  G_CALLBACK (account_edit_close),
			  NULL);
	g_signal_connect(G_OBJECT(help_btn), "clicked",
			G_CALLBACK(manual_open_with_anchor_cb),
			MANUAL_ANCHOR_ACCOUNTPREFS);


	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(account_size_allocate_cb), NULL);

	if (!geometry.min_height) {
		geometry.min_width = 500;
		geometry.min_height = 380;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.accountswin_width,
				    prefs_common.accountswin_height);
#ifdef G_OS_WIN32
	gtk_window_move(GTK_WINDOW(window), 48, 48);
#endif

	edit_account.window    = window;
	edit_account.list_view = list_view;
	edit_account.close_btn = close_btn;
#ifdef MAEMO
	maemo_window_full_screen_if_needed(GTK_WINDOW(edit_account.window));
#endif
}

static void account_edit_prefs(GtkWidget *widget, gpointer data)
{
	PrefsAccount *ac_prefs;

	ac_prefs = account_list_view_get_selected_account(edit_account.list_view);
	
	if (ac_prefs) {
		account_open(ac_prefs);
		account_list_view_set();
	}		
}

static gboolean account_delete_references_func(GNode *node, gpointer data)
{
	FolderItem *item;
	gint account;

	cm_return_val_if_fail(node->data != NULL, FALSE);

	item = FOLDER_ITEM(node->data);
	account = GPOINTER_TO_INT(data);

	if(!item->prefs) /* && item->prefs->stype == F_NORMAL */
		return FALSE;
	if(item->prefs->default_account != account)
		return FALSE;
	
	item->prefs->enable_default_account = FALSE;
	item->prefs->default_account = 0;
	folder_item_prefs_save_config(item);

	return FALSE;
}

	
#define ACP_FDUP(fld) ac_clon->fld = ((ac_prefs->fld) != NULL)?\
				     g_strdup(ac_prefs->fld): NULL
#define ACP_FASSIGN(fld) ac_clon->fld = ac_prefs->fld
static void account_clone(GtkWidget *widget, gpointer data)
{
	PrefsAccount *ac_prefs, *ac_clon;
	GSList *hdrs = NULL;
	CustomHeader *cch = NULL, *ch = NULL;
	
	ac_prefs = account_list_view_get_selected_account(edit_account.list_view);
	if (ac_prefs == NULL)
		return;

	if (ac_prefs->protocol == A_IMAP4 || ac_prefs->protocol == A_NNTP) {
		alertpanel_error(_("Accounts with remote folders cannot be copied."));
		return;
	}
	account_list_dirty = TRUE;
	
	ac_clon = prefs_account_new();
	/* copy fields */
	ac_clon->account_name = g_strdup_printf(_("Copy of %s"),
						ac_prefs->account_name);
	/* personal */
	ACP_FDUP(name);
	ACP_FDUP(address);
	ACP_FDUP(organization);

	/* server */
	ACP_FASSIGN(protocol);
	ACP_FDUP(recv_server);
	ACP_FDUP(smtp_server);
	ACP_FDUP(nntp_server);
	ACP_FASSIGN(use_nntp_auth);
	ACP_FASSIGN(use_nntp_auth_onconnect);
	ACP_FDUP(userid);
	ACP_FDUP(passwd);

	ACP_FDUP(local_mbox);
	ACP_FASSIGN(use_mail_command);
	ACP_FDUP(mail_command);
	
	ACP_FASSIGN(ssl_pop);
	ACP_FASSIGN(ssl_imap);
	ACP_FASSIGN(ssl_nntp);
	ACP_FASSIGN(ssl_smtp);
	ACP_FASSIGN(use_nonblocking_ssl);
	
	/* receive */
	ACP_FASSIGN(use_apop_auth);
	ACP_FASSIGN(rmmail);
	ACP_FASSIGN(msg_leave_time);
	ACP_FASSIGN(msg_leave_hour);
	ACP_FASSIGN(recv_at_getall);
	ACP_FASSIGN(sd_rmmail_on_download);
	ACP_FASSIGN(enable_size_limit);
	ACP_FASSIGN(size_limit);
	ACP_FASSIGN(filter_on_recv);
	ACP_FASSIGN(filterhook_on_recv);
	ACP_FDUP(inbox);
	ACP_FDUP(local_inbox);
	ACP_FASSIGN(max_articles);

	ACP_FASSIGN(imap_auth_type);

	/* send */
	ACP_FASSIGN(gen_msgid);
	ACP_FASSIGN(gen_xmailer);
	ACP_FASSIGN(add_customhdr);
	ACP_FASSIGN(use_smtp_auth);
	ACP_FASSIGN(smtp_auth_type);
	ACP_FDUP(smtp_userid);
	ACP_FDUP(smtp_passwd);

	ACP_FASSIGN(pop_before_smtp);
	ACP_FASSIGN(pop_before_smtp_timeout);
	ACP_FASSIGN(last_pop_login_time);

	ac_clon->customhdr_list = NULL;
	hdrs = ac_prefs->customhdr_list;
	while (hdrs != NULL) {
		ch = (CustomHeader *)hdrs->data;

		cch = g_new0(CustomHeader, 1);
		cch->account_id = ac_clon->account_id;	
		cch->name = (ch->name != NULL) ? g_strdup(ch->name) : NULL;
		cch->value = (ch->value != NULL) ? g_strdup(ch->value) : NULL;
		
		ac_clon->customhdr_list = g_slist_append(ac_clon->customhdr_list, cch);
		
		hdrs = g_slist_next(hdrs);
	}

	/* compose */
        ACP_FASSIGN(sig_type);
        ACP_FDUP(sig_path);
        ACP_FASSIGN(auto_sig);
        ACP_FDUP(sig_sep);
        ACP_FASSIGN(set_autocc);
        ACP_FDUP(auto_cc);
        ACP_FASSIGN(set_autobcc);
        ACP_FDUP(auto_bcc);
        ACP_FASSIGN(set_autoreplyto);
        ACP_FDUP(auto_replyto);
	ACP_FASSIGN(enable_default_dictionary);
	ACP_FDUP(default_dictionary);
	ACP_FASSIGN(enable_default_alt_dictionary);
	ACP_FDUP(default_alt_dictionary);
	ACP_FASSIGN(compose_with_format);
	ACP_FDUP(compose_subject_format);
	ACP_FDUP(compose_body_format);
	ACP_FASSIGN(reply_with_format);
	ACP_FDUP(reply_quotemark);
	ACP_FDUP(reply_body_format);
	ACP_FASSIGN(forward_with_format);
	ACP_FDUP(forward_quotemark);
	ACP_FDUP(forward_body_format);

        /* privacy */
	ACP_FDUP(default_privacy_system);
        ACP_FASSIGN(default_encrypt);
	ACP_FASSIGN(default_encrypt_reply);
        ACP_FASSIGN(default_sign);
	ACP_FASSIGN(default_sign_reply);
	ACP_FASSIGN(save_encrypted_as_clear_text);
	ACP_FASSIGN(encrypt_to_self);
	
        /* advanced */
        ACP_FASSIGN(set_smtpport);
        ACP_FASSIGN(smtpport);
        ACP_FASSIGN(set_popport);
        ACP_FASSIGN(popport);
        ACP_FASSIGN(set_imapport);
        ACP_FASSIGN(imapport);
        ACP_FASSIGN(set_nntpport);
        ACP_FASSIGN(nntpport);
        ACP_FASSIGN(set_domain);
        ACP_FDUP(domain);
        ACP_FASSIGN(mark_crosspost_read);
        ACP_FASSIGN(crosspost_col);

#ifndef G_OS_WIN32
        ACP_FASSIGN(set_tunnelcmd);
        ACP_FDUP(tunnelcmd);
#endif

        ACP_FDUP(imap_dir);
	ACP_FASSIGN(imap_subsonly);
	ACP_FASSIGN(low_bandwidth);

        ACP_FASSIGN(set_sent_folder);
        ACP_FDUP(sent_folder);
	ACP_FASSIGN(set_queue_folder);
	ACP_FDUP(queue_folder);
        ACP_FASSIGN(set_draft_folder);
        ACP_FDUP(draft_folder);
        ACP_FASSIGN(set_trash_folder);
        ACP_FDUP(trash_folder);
	/* don't want two default accounts */
	ac_clon->is_default = FALSE;
	ACP_FASSIGN(folder);

	account_list = g_list_append(account_list, ac_clon);
	account_list_view_set();
}
#undef ACP_FDUP
#undef ACP_FASSIGN

static void account_delete(GtkWidget *widget, gpointer data)
{
	PrefsAccount *ac_prefs;
	gchar buf[BUFFSIZE];
	GList *list;
	Folder *folder;
	GSList *cur;
 
 	ac_prefs = account_list_view_get_selected_account(edit_account.list_view);
 	if (ac_prefs == NULL)
 		return;

	g_snprintf(buf, sizeof(buf),
		   _("Do you really want to delete the account '%s'?"),
		   ac_prefs->account_name ? ac_prefs->account_name :
		   _("(Untitled)"));
	if (alertpanel_full(_("Delete account"), buf,
		 	    GTK_STOCK_CANCEL, GTK_STOCK_DELETE, NULL, FALSE,
			    NULL, ALERT_WARNING, G_ALERTDEFAULT) != G_ALERTALTERNATE)
		return;
	account_list_dirty = TRUE;

	if (ac_prefs->folder) {
		FolderItem *item;

		item = mainwindow_get_mainwindow()->summaryview->folder_item;
		if (item && item->folder == FOLDER(ac_prefs->folder))
			summary_clear_all(mainwindow_get_mainwindow()->summaryview);
		folder_destroy(FOLDER(ac_prefs->folder));
		folderview_set_all();
	}
	account_destroy(ac_prefs);
	account_list_view_set();

	debug_print("Removing deleted account references for all the folders...\n");
	list = folder_get_list();
	for (; list != NULL; list = list->next) {
		folder = FOLDER(list->data);
		if (folder->node)  /* && folder->type == F_? */
			g_node_traverse(folder->node, G_PRE_ORDER,
				G_TRAVERSE_ALL, -1,
				account_delete_references_func,
				GINT_TO_POINTER(ac_prefs->account_id));
	}

	debug_print("Removing filter rules relative to this account...\n");
	for(cur = filtering_rules ; cur != NULL ;) {
		FilteringProp * prop = (FilteringProp *) cur->data;

		if (prop && (prop->account_id == ac_prefs->account_id)) {
			/* get next item before we kill the current one */
			cur = g_slist_next(cur);

			/* unallocate filteringprop and unchain it from the list */
			filteringprop_free(prop);
			filtering_rules = g_slist_remove(filtering_rules, prop);
		} else {
			cur = g_slist_next(cur);
		}
	}
	folder_write_list();
}

static void account_up(GtkWidget *widget, gpointer data)
{
	GtkTreePath *sel = account_list_view_get_selected_account_path
				(edit_account.list_view),
		    *up;
	GtkTreeIter isel, iup;
	GtkTreeModel *model = gtk_tree_view_get_model
				(GTK_TREE_VIEW(edit_account.list_view));
	
	if (!sel) 
		return;
	account_list_dirty = TRUE;

	up = gtk_tree_path_copy(sel);
	if (!up) {
		gtk_tree_path_free(sel);
		return;
	}

	if (!gtk_tree_path_prev(up)) {
		gtk_tree_path_free(up);
		gtk_tree_path_free(sel);
		return;
	}

	if (!gtk_tree_model_get_iter(model, &isel, sel)
	||  !gtk_tree_model_get_iter(model, &iup,  up)) {
		gtk_tree_path_free(up);
		gtk_tree_path_free(sel);
		return;
	}

	gtk_list_store_swap(GTK_LIST_STORE(model), &isel, &iup);

	account_list_set();
	
	gtk_tree_path_free(up);
	gtk_tree_path_free(sel);
}

static void account_down(GtkWidget *widget, gpointer data)
{
	GtkTreePath *sel = account_list_view_get_selected_account_path
				(edit_account.list_view),
		    *dn;
	GtkTreeIter isel, idn;
	GtkTreeModel *model = gtk_tree_view_get_model
				(GTK_TREE_VIEW(edit_account.list_view));
	
	if (!sel) 
		return;
	account_list_dirty = TRUE;

	dn = gtk_tree_path_copy(sel);
	if (!dn) {
		gtk_tree_path_free(sel);
		return;
	}

	/* XXX no check possible??? however, if down but at bottom, then 
	 * nothing seems to happen much anyway, so the following seems to 
	 * be okay */
	gtk_tree_path_next(dn);

	if (!gtk_tree_model_get_iter(model, &isel, sel)
	||  !gtk_tree_model_get_iter(model, &idn,  dn)) {
		gtk_tree_path_free(dn);
		gtk_tree_path_free(sel);
		return;
	}

	gtk_list_store_swap(GTK_LIST_STORE(model), &isel, &idn);

	account_list_set();
	
	gtk_tree_path_free(dn);
	gtk_tree_path_free(sel);
}

static void account_set_default(GtkWidget *widget, gpointer data)
{
	PrefsAccount *ac_prefs;

	if (NULL == (ac_prefs = account_list_view_get_selected_account
					(edit_account.list_view))) 
		return;	

	/* we need to change the store variables by resetting everything
	 * and setting the new default one */
	account_list_view_set_default_by_id(edit_account.list_view,
					    ac_prefs->account_id);		 
	
	account_set_as_default(ac_prefs);
	account_list_view_set();
	
	cur_account = ac_prefs;
	account_flush_state();
}

static void account_edit_close(GtkWidget *widget, gpointer data)
{
	account_list_set();
	account_write_config_all();

	if (!cur_account && account_list) {
		PrefsAccount *ac_prefs = (PrefsAccount *)account_list->data;
		account_set_as_default(ac_prefs);
		cur_account = ac_prefs;
	}

	if (account_list_dirty)
		account_flush_state();

	gtk_widget_hide(edit_account.window);
	gtk_window_set_modal(GTK_WINDOW(edit_account.window), FALSE);
	inc_unlock();
}

static gint account_delete_event(GtkWidget *widget, GdkEventAny *event,
				 gpointer data)
{
	account_edit_close(NULL, NULL);
	return TRUE;
}
#ifndef MAEMO
static gboolean account_key_pressed(GtkWidget *widget, GdkEventKey *event,
				    gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape)
		account_edit_close(NULL, NULL);
	return FALSE;
}
#endif

static gboolean account_search_func_cb (GtkTreeModel *model, gint column, const gchar *key, 
						GtkTreeIter *iter, gpointer search_data) 
{
	gboolean retval;
	PrefsAccount *ac;

	gtk_tree_model_get (model, iter, ACCOUNT_DATA, &ac, -1);

	if (!ac->name || !key) return FALSE;

	retval = (strncmp (key, ac->account_name, strlen(key)) != 0);

	debug_print("selecting row\n");
	account_list_view_select_account(edit_account.list_view, ac->account_id);

	return retval;
}
static void account_list_view_add(PrefsAccount *ac_prefs)
{
	GtkTreeView *list_view = GTK_TREE_VIEW(edit_account.list_view);
	GtkListStore *list_store = GTK_LIST_STORE(gtk_tree_view_get_model(list_view));
	gchar *name, *protocol, *server;
	gboolean has_getallbox;
	gboolean getall;

	name = ac_prefs->account_name;
#ifdef USE_GNUTLS
	protocol = ac_prefs->protocol == A_POP3 ?
		  (ac_prefs->ssl_pop == SSL_TUNNEL ?
		   "POP3 (SSL)" :
		   ac_prefs->ssl_pop == SSL_STARTTLS ?
		   "POP3 (TLS)" : "POP3") :
		   ac_prefs->protocol == A_IMAP4 ?
		  (ac_prefs->ssl_imap == SSL_TUNNEL ?
		   "IMAP4 (SSL)" :
		   ac_prefs->ssl_imap == SSL_STARTTLS ?
		   "IMAP4 (TLS)" : "IMAP4") :
		   ac_prefs->protocol == A_NNTP ?
		  (ac_prefs->ssl_nntp == SSL_TUNNEL ?
		   "NNTP (SSL)" : "NNTP") :
		   ac_prefs->protocol == A_LOCAL ? "Local" :
		   ac_prefs->protocol == A_NONE ?  "SMTP" : "-";
#else
	protocol = ac_prefs->protocol == A_POP3  ? "POP3" :
		   ac_prefs->protocol == A_IMAP4 ? "IMAP4" :
		   ac_prefs->protocol == A_LOCAL ? "Local" :
		   ac_prefs->protocol == A_NNTP  ? "NNTP" :
		   ac_prefs->protocol == A_NONE ?  "SMTP" : "-";
#endif
	server= ac_prefs->protocol == A_NNTP ? ac_prefs->nntp_server :
		   ac_prefs->protocol == A_LOCAL ?  "-" :
		   ac_prefs->protocol == A_NONE ? ac_prefs->smtp_server :
		   ac_prefs->recv_server;

	has_getallbox = (ac_prefs->protocol == A_POP3  ||
			 ac_prefs->protocol == A_IMAP4 ||
			 ac_prefs->protocol == A_NNTP  ||
			 ac_prefs->protocol == A_LOCAL);
	getall = has_getallbox && ac_prefs->recv_at_getall;

	account_list_view_insert_account_item(list_store,
					     name, protocol, server,
					     ac_prefs->is_default,
					     getall, ac_prefs);
	return;
}

static void account_list_view_set(void)
{
	GList *cur;
	gint prev_sel_account;
	GtkListStore *store;
	
	store = GTK_LIST_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(edit_account.list_view)));

	prev_sel_account = account_list_view_get_selected_account_id
		(edit_account.list_view); 

	gtk_list_store_clear(store);
	
	for (cur = account_list; cur != NULL; cur = cur->next) {
		account_list_view_add((PrefsAccount *)cur->data);
		if ((PrefsAccount *)cur->data == cur_account)
			account_list_view_select_account
				(edit_account.list_view, 
				 cur_account->account_id);
	}
	
	if (prev_sel_account >= 0)
		account_list_view_select_account(edit_account.list_view, 
						 prev_sel_account); 
}

/* set account list from CList */
static void account_list_set(void)
{
	/* want to make sure we iterate *IN ORDER*, so therefore using
	 * gtk_tree_model_XXXX_nth_child() */
	gint row, n_rows;
	PrefsAccount *ac_prefs;
	GtkTreeModel *model = gtk_tree_view_get_model
				(GTK_TREE_VIEW(edit_account.list_view));
	
	while (account_list)
		account_list = g_list_remove(account_list, account_list->data);

	n_rows = gtk_tree_model_iter_n_children(model, NULL);

	for (row = 0; row < n_rows; row++) {
		GtkTreeIter iter;

		if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, row)) {
			g_warning("%s(%d) - no iter found???\n", __FILE__, __LINE__); 					      
			continue;
		}
	
		ac_prefs = NULL;
		gtk_tree_model_get(model, &iter,
				   ACCOUNT_DATA, &ac_prefs,
				   -1);
		if (ac_prefs)
			account_list = g_list_append(account_list, ac_prefs);
	}
}

/*!
 *\brief	finds the PrefsAccounts which should be used to answer a mail
 *
 *\param	msginfo The message to be answered
 *\param	reply_autosel Indicates whether reply account autoselection is on
 *
 *\return	PrefsAccount * the correct account, NULL if not found
 */
PrefsAccount *account_get_reply_account(MsgInfo *msginfo, gboolean reply_autosel)
{
	PrefsAccount *account = NULL;
	/* select the account set in folderitem's property (if enabled) */
	if (msginfo->folder->prefs && msginfo->folder->prefs->enable_default_account)
		account = account_find_from_id(msginfo->folder->prefs->default_account);
	
	/* select account by to: and cc: header if enabled */
	if (reply_autosel) {
		gchar * field = NULL;
		int fieldno = 0;
		for (field = msginfo->to; fieldno++ < 2; field = msginfo->cc) {
			if (!account && field) {
				gchar *to = NULL;
				if (!strchr(field, ',')) {
					Xstrdup_a(to, field, return NULL);
					extract_address(to);
					account = account_find_from_address(to, FALSE);
				} else {
					gchar **split = g_strsplit(field, ",", -1);
					int i = -1;
					do {
						i++;
						if (!split[i])
							break;
						Xstrdup_a(to, split[i], return NULL);
						extract_address(to);
						account = account_find_from_address(to, FALSE);
					} while (!account);
					g_strfreev(split);
				}
			}
		}
		if (!account) {
			gchar deliveredto[BUFFSIZE];
			if (!procheader_get_header_from_msginfo
				(msginfo, deliveredto,sizeof deliveredto , "Delivered-To:")) { 
				gchar *buf = deliveredto + strlen("Delivered-To:");
		        	extract_address(buf);
		        	account = account_find_from_address(buf, FALSE);
                	}
		}
	}

	/* select the account for the whole folder (IMAP / NNTP) */
	if (!account) 
		/* FIXME: this is not right, because folder may be nested. we should
		 * ascend the tree until we find a parent with proper account 
		 * information */
		account = msginfo->folder->folder->account;

	/* select current account */
	if (!account) account = cur_account;
	
	return account;
}

/*!
 *\brief	Create data store
 */
static GtkListStore* account_create_data_store(void)
{
	return gtk_list_store_new(N_ACCOUNT_COLUMNS,
				 G_TYPE_INT,		/* ACCOUNT_IS_DEFAULT */
				 G_TYPE_BOOLEAN,	/* ACCOUNT_ENABLE_GET_ALL */
				 G_TYPE_STRING,		/* ACCOUNT_NAME */
				 G_TYPE_STRING,		/* ACCOUNT_PROTOCOL */
				 G_TYPE_STRING,		/* ACCOUNT_SERVER */
				 G_TYPE_POINTER,	/* ACCOUNT_DATA */
				 -1);
}

/*!
 *\brief	Insert an account item in the list. 
 *
 *\return	GtkTreeRowReference * A tree row reference, which is guaranteed to 
 *		stable whatever operations are performed on the list.
 */
static void account_list_view_insert_account_item(GtkListStore *list_store, 
						  const gchar *account_name,
						  const gchar *protocol, 
						  const gchar *server_name,
						  gboolean is_default, 
						  gboolean is_get_all,
						  PrefsAccount *account_data)
{
	GtkTreeIter iter;
	
	gtk_list_store_append(list_store, &iter);
	gtk_list_store_set(list_store, &iter, 
			   ACCOUNT_IS_DEFAULT,     is_default ? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL,
			   ACCOUNT_ENABLE_GET_ALL, is_get_all,
			   ACCOUNT_NAME,	   account_name,
			   ACCOUNT_PROTOCOL,	   protocol,
			   ACCOUNT_SERVER,	   server_name,
			   ACCOUNT_DATA,	   account_data,
			   -1);
}

/*!
 *\brief	Create and set up account list view, including tasks like
 *		creating the data store (\ref account_create_data_store()),
 *		and setting up the account list's individual columns (\ref 
 *		account_create_list_view_columns()).
 *
 *\return	GtkWidget * The created list view widget.
 */
static GtkWidget *account_list_view_create(void)
{
	GtkTreeView *list_view;
	GtkTreeSelection *selector;
	GtkListStore *store = account_create_data_store();

	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
	g_object_unref(G_OBJECT(store));
#ifdef GENERIC_UMPC
	g_object_set(list_view, "allow-checkbox-mode", FALSE, NULL);
#endif
	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	gtk_tree_view_set_reorderable(list_view, TRUE);
	
	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);

	/* create the columns */
	account_create_list_view_columns(GTK_WIDGET(list_view));

	/* set a double click listener */
	g_signal_connect(G_OBJECT(list_view), "row_activated",
	                 G_CALLBACK(account_double_clicked),
			 list_view);

	g_signal_connect(G_OBJECT(list_view), "drag_begin", 			 
			 G_CALLBACK(drag_begin),
			 list_view);
			 
	g_signal_connect(G_OBJECT(list_view), "drag_end", 			 
			 G_CALLBACK(drag_end),
			 list_view);
			 
	gtk_tree_view_set_reorderable(list_view, TRUE);
	return GTK_WIDGET(list_view);
}

static void account_create_list_view_columns(GtkWidget *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_toggle_new();
	g_object_set(renderer, 
		     "radio", FALSE, 
		     "activatable", TRUE,
		      NULL);
	column = gtk_tree_view_column_new_with_attributes
		(Q_("Accounts List Get Column Name|G"), renderer,
		 "active", ACCOUNT_ENABLE_GET_ALL,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
	gtk_tree_view_column_set_alignment (column, 0.5);
	CLAWS_SET_TIP(gtk_tree_view_column_get_widget(column),
			_("'Get Mail' retrieves mail from the checked accounts"));
	g_signal_connect(G_OBJECT(renderer), "toggled", 		     
			 G_CALLBACK(account_get_all_toggled),
			 list_view);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Name"), renderer,
		 "text", ACCOUNT_NAME,
		 "weight", ACCOUNT_IS_DEFAULT,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
	
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Protocol"), renderer,
		 "text", ACCOUNT_PROTOCOL,
		 "weight", ACCOUNT_IS_DEFAULT,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		 
	
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Server"), renderer,
		 "text", ACCOUNT_SERVER,
		 "weight", ACCOUNT_IS_DEFAULT,
		 NULL);
	gtk_tree_view_set_search_column(GTK_TREE_VIEW(list_view), ACCOUNT_NAME);
	gtk_tree_view_set_search_equal_func(GTK_TREE_VIEW(list_view), account_search_func_cb , NULL, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		 
}

/*!
 *\brief	Get currently selected account (by its unique ID)
 */
static gint account_list_view_get_selected_account_id(GtkWidget *list_view)
{
	GtkTreeSelection *selector;
	GtkTreeModel *model;
	GtkTreeIter iter;
	PrefsAccount *res = NULL;

	selector = gtk_tree_view_get_selection(GTK_TREE_VIEW(list_view));
	
	if (!gtk_tree_selection_get_selected(selector, &model, &iter))
		return -1;

	gtk_tree_model_get(model, &iter, ACCOUNT_DATA, &res, -1);

	return res->account_id;			   
}

/*!
 *\brief	Get the tree path of the currently selected account
 */
static GtkTreePath *account_list_view_get_selected_account_path(GtkWidget *list_view)
{
	GtkTreeSelection *selector;
	GtkTreeModel *model;
	GtkTreeIter iter;

	selector = gtk_tree_view_get_selection(GTK_TREE_VIEW(list_view));
	
	if (!gtk_tree_selection_get_selected(selector, &model, &iter))
		return NULL;

	return gtk_tree_model_get_path(gtk_tree_view_get_model
		(GTK_TREE_VIEW(list_view)), &iter);
}

/*!
 *\brief	Get the account data of the currently selected account
 */
static PrefsAccount *account_list_view_get_selected_account(GtkWidget *list_view)
{
	GtkTreeSelection *selector;
	GtkTreeModel *model;
	GtkTreeIter iter;
	PrefsAccount *res = NULL;

	selector = gtk_tree_view_get_selection(GTK_TREE_VIEW(list_view));
	
	if (!gtk_tree_selection_get_selected(selector, &model, &iter))
		return NULL;

	gtk_tree_model_get(model, &iter, ACCOUNT_DATA, &res, -1);

	return res;			   
}

/*!
 *\brief	Select a row by the account it represents
 *
 *\return	gboolean TRUE if found and selected, FALSE if not.
 */
static gboolean account_list_view_select_account(GtkWidget *list_view, gint account_id)
{
	FindAccountInStore fis;
	GtkTreeModel *model;
	
	fis.account_id = account_id;
	fis.path = NULL;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(list_view));

	gtk_tree_model_foreach(model, (GtkTreeModelForeachFunc) find_account_in_store,
			       &fis);
			       
	if (fis.path) {
		GtkTreeSelection *selection;
		GtkTreePath* path;

		selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(list_view));
		gtk_tree_selection_select_iter(selection, &fis.iter);
		path = gtk_tree_model_get_path(model, &fis.iter);
		/* XXX returned path may not be valid??? create new one to be sure */ 
		gtk_tree_view_set_cursor(GTK_TREE_VIEW(list_view), path, NULL, FALSE);
		gtk_tree_path_free(path);
	}

	return fis.path != NULL;
}

/*!
 *\brief	Set a new default account by its ID. (There is only one
 *		default account.)
 */
static void account_list_view_set_default_by_id(GtkWidget *list_view,
						gint account_id)
{
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(list_view));
	
	gtk_tree_model_foreach
		(model, (GtkTreeModelForeachFunc) set_new_default_account,
		 &account_id);
}

static gboolean set_new_default_account(GtkTreeModel *model,
					GtkTreePath  *path,
					GtkTreeIter  *iter,
					gint	     *account_id)
{
	PrefsAccount *account = NULL;
	PangoWeight weight;
	
	gtk_tree_model_get(model, iter, 
			   ACCOUNT_DATA, &account, 
			   -1);

	if (*account_id == account->account_id)
		weight = PANGO_WEIGHT_NORMAL;
	else
		weight = PANGO_WEIGHT_BOLD;

	gtk_list_store_set(GTK_LIST_STORE(model), iter, 
			   ACCOUNT_IS_DEFAULT, weight, -1);

	return FALSE;
}
					
static gboolean find_account_in_store(GtkTreeModel *model,
				      GtkTreePath  *path,
				      GtkTreeIter  *iter,
				      FindAccountInStore *data)
{
	PrefsAccount *account = NULL;
	gtk_tree_model_get(model, iter, ACCOUNT_DATA, &account, -1);

	if (data->account_id == account->account_id) {
		data->path = path; /* signal we found it */
		data->iter = *iter;
		return TRUE;
	}

	return FALSE; 
}

/*!
 *\brief	Triggered when "get all" column is activated or de-activated
 */
static void account_get_all_toggled(GtkCellRendererToggle *widget, 
				    gchar *path, 
				    GtkWidget *list_view)
{
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(list_view));
	PrefsAccount *ac = NULL;
	gboolean get_all;
	
	if (!gtk_tree_model_get_iter_from_string(model, &iter, path))
		return;

	gtk_tree_model_get(model, &iter, 
			   ACCOUNT_DATA, &ac,
			   ACCOUNT_ENABLE_GET_ALL, &get_all,
			   -1);

	/* check if the account has a selectable get all checkbox anyway... */
	if (!(ac->protocol == A_POP3  || 
	      ac->protocol == A_IMAP4 ||
	      ac->protocol == A_NNTP  ||
	      ac->protocol == A_LOCAL))
		return;	      

	/* set value in store */
	gtk_list_store_set(GTK_LIST_STORE(model), &iter,
			   ACCOUNT_ENABLE_GET_ALL, !get_all,
			   -1);

	/* set value in account */
	ac->recv_at_getall ^= TRUE;
}

static void account_double_clicked(GtkTreeView		*list_view,
				   GtkTreePath		*path,
				   GtkTreeViewColumn	*column,
				   gpointer		 data)
{
	account_edit_prefs(NULL, NULL);	
}

static void drag_begin(GtkTreeView *list_view,
		      GdkDragContext *context,
		      gpointer data)
{
	/* XXX unfortunately a completed drag & drop does not emit 
	 * a "rows_reordered" signal, but a "row_changed" signal.
	 * So during drag and drop, listen to "row_changed", and
	 * update the account list accordingly */

	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	g_signal_connect(G_OBJECT(model), "row_changed",
			 G_CALLBACK(account_row_changed_while_drag_drop),
			 list_view);
}

static void drag_end(GtkTreeView *list_view,
		    GdkDragContext *context,
		    gpointer data)
{
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	g_signal_handlers_disconnect_by_func(G_OBJECT(model),
					     G_CALLBACK(account_row_changed_while_drag_drop),
					     list_view);
}

static void account_row_changed_while_drag_drop(GtkTreeModel *model, 
				   GtkTreePath  *path,
				   GtkTreeIter  *iter,
				   gpointer      arg3,
				   GtkTreeView  *list_view)
{	
	account_list_set();	
}

gchar *account_get_signature_str(PrefsAccount *account)
{
	gchar *sig_body = NULL;
	gchar *sig_str = NULL;
	gchar *utf8_sig_str = NULL;

	cm_return_val_if_fail(account != NULL, NULL);

	if (!account->sig_path)
		return NULL;

	if (account->sig_type == SIG_FILE) {
		if (!is_file_or_fifo_exist(account->sig_path)) {
			g_warning("can't open signature file: %s\n",
				  account->sig_path);
			return NULL;
		}
	}

	if (account->sig_type == SIG_COMMAND)
		sig_body = get_command_output(account->sig_path);
	else {
		gchar *tmp;

		tmp = file_read_to_str(account->sig_path);
		if (!tmp)
			return NULL;
		sig_body = normalize_newlines(tmp);
		g_free(tmp);
	}

	if (account->sig_sep) {
		sig_str = g_strconcat("\n", account->sig_sep, "\n", sig_body,
				      NULL);
		g_free(sig_body);
	} else
		sig_str = g_strconcat("\n", sig_body, NULL);

	if (sig_str) {
		if (g_utf8_validate(sig_str, -1, NULL) == TRUE)
			utf8_sig_str = sig_str;
		else {
			utf8_sig_str = conv_codeset_strdup
				(sig_str, conv_get_locale_charset_str_no_utf8(),
				 CS_INTERNAL);
			g_free(sig_str);
		}
	}

	return utf8_sig_str;
}

PrefsAccount *account_get_cur_account (void)
{
	return cur_account;
}
