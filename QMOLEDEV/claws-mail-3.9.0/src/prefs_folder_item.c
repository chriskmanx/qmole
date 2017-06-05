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

/* alfons - all folder item specific settings should migrate into 
 * folderlist.xml!!! the old folderitemrc file will only serve for a few 
 * versions (for compatibility) */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include "folder.h"
#include "alertpanel.h"
#include "prefs_folder_item.h"
#include "folderview.h"
#include "folder.h"
#include "summaryview.h"
#include "menu.h"
#include "account.h"
#include "prefs_gtk.h"
#include "manage_window.h"
#include "utils.h"
#include "addr_compl.h"
#include "prefs_common.h"
#include "gtkutils.h"
#include "filtering.h"
#include "folder_item_prefs.h"
#include "gtk/colorsel.h"
#include "string_match.h"
#include "quote_fmt.h"
#include "combobox.h"

#if USE_ENCHANT
#include "gtkaspell.h"
#endif

#define ASSIGN_STRING(string, value) \
	{ \
		g_free(string); \
		string = (value); \
	}

static void prefs_folder_item_register_page	(PrefsPage 	*page);

typedef struct _FolderItemGeneralPage FolderItemGeneralPage;
typedef struct _FolderItemComposePage FolderItemComposePage;
typedef struct _FolderItemTemplatesPage FolderItemTemplatesPage;
static gboolean can_save = TRUE;

struct _FolderItemGeneralPage
{
	PrefsPage page;

	FolderItem *item;

	GtkWidget *table;
	GtkWidget *no_save_warning;
	GtkWidget *folder_type;
#ifndef G_OS_WIN32
	GtkWidget *checkbtn_simplify_subject;
	GtkWidget *entry_simplify_subject;
	GtkWidget *entry_regexp_test_string;
	GtkWidget *entry_regexp_test_result;
#endif
	GtkWidget *checkbtn_folder_chmod;
	GtkWidget *entry_folder_chmod;
	GtkWidget *folder_color_btn;
	GtkWidget *checkbtn_enable_processing;
	GtkWidget *checkbtn_enable_processing_when_opening;
	GtkWidget *checkbtn_newmailcheck;
	GtkWidget *checkbtn_offlinesync;
	GtkWidget *label_offlinesync;
	GtkWidget *entry_offlinesync;
	GtkWidget *label_end_offlinesync;
	GtkWidget *checkbtn_remove_old_offlinesync;
	GtkWidget *promote_html_part;
	
	/* apply to sub folders */
#ifndef G_OS_WIN32
	GtkWidget *simplify_subject_rec_checkbtn;
#endif
	GtkWidget *folder_chmod_rec_checkbtn;
	GtkWidget *folder_color_rec_checkbtn;
	GtkWidget *enable_processing_rec_checkbtn;
	GtkWidget *enable_processing_when_opening_rec_checkbtn;
	GtkWidget *newmailcheck_rec_checkbtn;
	GtkWidget *offlinesync_rec_checkbtn;
	GtkWidget *promote_html_part_rec_checkbtn;

	gint	   folder_color;
};

struct _FolderItemComposePage
{
	PrefsPage page;

	FolderItem *item;

	GtkWidget *window;
	GtkWidget *table;
	GtkWidget *no_save_warning;
	GtkWidget *checkbtn_request_return_receipt;
	GtkWidget *checkbtn_save_copy_to_folder;
	GtkWidget *checkbtn_default_to;
	GtkWidget *entry_default_to;
	GtkWidget *checkbtn_default_reply_to;
	GtkWidget *entry_default_reply_to;
	GtkWidget *checkbtn_default_cc;
	GtkWidget *entry_default_cc;
	GtkWidget *checkbtn_default_bcc;
	GtkWidget *entry_default_bcc;
	GtkWidget *checkbtn_default_replyto;
	GtkWidget *entry_default_replyto;
	GtkWidget *checkbtn_enable_default_account;
	GtkWidget *optmenu_default_account;
#if USE_ENCHANT
	GtkWidget *checkbtn_enable_default_dictionary;
	GtkWidget *checkbtn_enable_default_alt_dictionary;
	GtkWidget *combo_default_dictionary;
	GtkWidget *combo_default_alt_dictionary;
#endif

	/* apply to sub folders */
	GtkWidget *request_return_receipt_rec_checkbtn;
	GtkWidget *save_copy_to_folder_rec_checkbtn;
	GtkWidget *default_to_rec_checkbtn;
	GtkWidget *default_reply_to_rec_checkbtn;
	GtkWidget *default_cc_rec_checkbtn;
	GtkWidget *default_bcc_rec_checkbtn;
	GtkWidget *default_replyto_rec_checkbtn;
	GtkWidget *default_account_rec_checkbtn;
#if USE_ENCHANT
	GtkWidget *default_dictionary_rec_checkbtn;
	GtkWidget *default_alt_dictionary_rec_checkbtn;
#endif
};

struct _FolderItemTemplatesPage
{
	PrefsPage page;

	FolderItem *item;

	GtkWidget *window;
	GtkWidget *table;
	GtkWidget *checkbtn_compose_with_format;
	GtkWidget *compose_override_from_format;
	GtkWidget *compose_subject_format;
	GtkWidget *compose_body_format;
	GtkWidget *checkbtn_reply_with_format;
	GtkWidget *reply_quotemark;
	GtkWidget *reply_override_from_format;
	GtkWidget *reply_body_format;
	GtkWidget *checkbtn_forward_with_format;
	GtkWidget *forward_quotemark;
	GtkWidget *forward_override_from_format;
	GtkWidget *forward_body_format;

	/* apply to sub folders */
	GtkWidget *new_msg_format_rec_checkbtn;
	GtkWidget *reply_format_rec_checkbtn;
	GtkWidget *forward_format_rec_checkbtn;
};


static void general_save_folder_prefs(FolderItem *folder, FolderItemGeneralPage *page);
static void compose_save_folder_prefs(FolderItem *folder, FolderItemComposePage *page);
static void templates_save_folder_prefs(FolderItem *folder, FolderItemTemplatesPage *page);

static gboolean general_save_recurse_func(GNode *node, gpointer data);
static gboolean compose_save_recurse_func(GNode *node, gpointer data);
static gboolean templates_save_recurse_func(GNode *node, gpointer data);

static gint prefs_folder_item_chmod_mode		(gchar *folder_chmod);

static void folder_color_set_dialog(GtkWidget *widget, gpointer data);
static void clean_cache_cb(GtkWidget *widget, gpointer data);
#ifndef G_OS_WIN32
static void folder_regexp_test_cb(GtkWidget *widget, gpointer data);
static void folder_regexp_set_subject_example_cb(GtkWidget *widget, gpointer data);
#endif
#define SAFE_STRING(str) \
	(str) ? (str) : ""

static void prefs_folder_item_general_create_widget_func(PrefsPage * page_,
						   GtkWindow * window,
                                		   gpointer data)
{
	FolderItemGeneralPage *page = (FolderItemGeneralPage *) page_;
	FolderItem *item = (FolderItem *) data;
	guint rowcount;

	GtkWidget *table;
	GtkWidget *hbox, *hbox2, *hbox_spc;
	GtkWidget *label;
	
	GtkListStore *folder_type_menu;
	GtkWidget *folder_type;
	GtkTreeIter iter;
	GtkWidget *dummy_checkbtn, *clean_cache_btn;
	SpecialFolderItemType type;
	
	GtkWidget *no_save_warning = NULL;
#ifndef G_OS_WIN32
	GtkWidget *checkbtn_simplify_subject;
	GtkWidget *entry_simplify_subject;
	GtkWidget *label_regexp_test;
	GtkWidget *entry_regexp_test_string;
	GtkWidget *label_regexp_result;
	GtkWidget *entry_regexp_test_result;
#endif
	GtkWidget *checkbtn_folder_chmod;
	GtkWidget *entry_folder_chmod;
	GtkWidget *folder_color;
	GtkWidget *folder_color_btn;
	GtkWidget *checkbtn_enable_processing;
	GtkWidget *checkbtn_enable_processing_when_opening;
	GtkWidget *checkbtn_newmailcheck;
	GtkWidget *checkbtn_offlinesync;
	GtkWidget *label_offlinesync;
	GtkWidget *entry_offlinesync;
	GtkWidget *label_end_offlinesync;
	GtkWidget *checkbtn_remove_old_offlinesync;
	GtkWidget *promote_html_part;
	GtkListStore *promote_html_part_menu;

#ifndef G_OS_WIN32
	GtkWidget *simplify_subject_rec_checkbtn;
#endif
	GtkWidget *folder_chmod_rec_checkbtn;
	GtkWidget *folder_color_rec_checkbtn;
	GtkWidget *enable_processing_rec_checkbtn;
	GtkWidget *enable_processing_when_opening_rec_checkbtn;
	GtkWidget *newmailcheck_rec_checkbtn;
	GtkWidget *offlinesync_rec_checkbtn;
	GtkWidget *promote_html_part_rec_checkbtn;

	page->item	   = item;

	/* Table */
	table = gtk_table_new(12, 3, FALSE);
	gtk_container_set_border_width (GTK_CONTAINER (table), VBOX_BORDER);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 4);
	rowcount = 0;

	if (!can_save) {
		no_save_warning = gtk_label_new(
			_("<i>These preferences will not be saved as this folder "
			"is a top-level folder. However, you can set them for the "
			"whole mailbox tree by using \"Apply to subfolders\".</i>"));
		gtk_label_set_use_markup(GTK_LABEL(no_save_warning), TRUE);
		gtk_label_set_line_wrap(GTK_LABEL(no_save_warning), TRUE);
		gtk_misc_set_alignment(GTK_MISC(no_save_warning), 0.0, 0.5);
		gtk_table_attach(GTK_TABLE(table), no_save_warning, 0, 3,
			 rowcount, rowcount + 1, GTK_FILL, 0, 0, 0);
		rowcount++;
	}
	
	/* Apply to subfolders */
	label = gtk_label_new(_("Apply to\nsubfolders"));
	gtk_misc_set_alignment(GTK_MISC(label), 0.5, 0.5);
	gtk_table_attach(GTK_TABLE(table), label, 2, 3,
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	rowcount++;

	/* folder_type */
	folder_type = gtkut_sc_combobox_create(NULL, FALSE);
	gtk_widget_show (folder_type);

	type = F_NORMAL;
	if (item->stype == F_INBOX)
		type = F_INBOX;
	else if (folder_has_parent_of_type(item, F_OUTBOX))
		type = F_OUTBOX;
	else if (folder_has_parent_of_type(item, F_DRAFT))
		type = F_DRAFT;
	else if (folder_has_parent_of_type(item, F_QUEUE))
		type = F_QUEUE;
	else if (folder_has_parent_of_type(item, F_TRASH))
		type = F_TRASH;

	folder_type_menu = GTK_LIST_STORE(gtk_combo_box_get_model(
				GTK_COMBO_BOX(folder_type)));

	COMBOBOX_ADD (folder_type_menu, _("Normal"),  F_NORMAL);
	COMBOBOX_ADD (folder_type_menu, _("Inbox"),  F_INBOX);
	COMBOBOX_ADD (folder_type_menu, _("Outbox"),  F_OUTBOX);
	COMBOBOX_ADD (folder_type_menu, _("Drafts"),  F_DRAFT);
	COMBOBOX_ADD (folder_type_menu, _("Queue"),  F_QUEUE);
	COMBOBOX_ADD (folder_type_menu, _("Trash"),  F_TRASH);

	combobox_select_by_data(GTK_COMBO_BOX(folder_type), type);

	dummy_checkbtn = gtk_check_button_new();
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dummy_checkbtn), type != F_INBOX);
	gtk_widget_set_sensitive(dummy_checkbtn, FALSE);

	if (type == item->stype && type == F_NORMAL)
		gtk_widget_set_sensitive(folder_type, TRUE);
	else
		gtk_widget_set_sensitive(folder_type, FALSE);

	label = gtk_label_new(_("Folder type"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);
	gtk_table_attach(GTK_TABLE(table), folder_type, 1, 2, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);
	gtk_table_attach(GTK_TABLE(table), dummy_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

	rowcount++;

#ifndef G_OS_WIN32
	/* Simplify Subject */
	checkbtn_simplify_subject = gtk_check_button_new_with_label(_("Simplify Subject RegExp"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_simplify_subject, 0, 1, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_simplify_subject), 
				     item->prefs->enable_simplify_subject);

	g_signal_connect(G_OBJECT(checkbtn_simplify_subject), "toggled",
			G_CALLBACK(folder_regexp_set_subject_example_cb), page);

	entry_simplify_subject = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_simplify_subject, 1, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_simplify_subject, entry_simplify_subject);
	gtk_entry_set_text(GTK_ENTRY(entry_simplify_subject), 
			   SAFE_STRING(item->prefs->simplify_subject_regexp));

	g_signal_connect(G_OBJECT(entry_simplify_subject), "changed",
			G_CALLBACK(folder_regexp_test_cb), page);

	simplify_subject_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), simplify_subject_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

	rowcount++;

	/* Test string */
	label_regexp_test = gtk_label_new(_("Test string:"));
	gtk_misc_set_alignment(GTK_MISC(label_regexp_test), 1, 0.5);
	gtk_table_attach(GTK_TABLE(table), label_regexp_test, 0, 1, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_simplify_subject, label_regexp_test);

	entry_regexp_test_string = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_regexp_test_string, 1, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_simplify_subject, entry_regexp_test_string);

	g_signal_connect(G_OBJECT(entry_regexp_test_string), "changed",
			G_CALLBACK(folder_regexp_test_cb), page);

	rowcount++;
	
	/* Test result */
	label_regexp_result = gtk_label_new(_("Result:"));
	gtk_misc_set_alignment(GTK_MISC(label_regexp_result), 1, 0.5);
	gtk_table_attach(GTK_TABLE(table), label_regexp_result, 0, 1, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_simplify_subject, label_regexp_result);

	entry_regexp_test_result = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_regexp_test_result, 1, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_simplify_subject, entry_regexp_test_result);
	gtk_editable_set_editable(GTK_EDITABLE(entry_regexp_test_result), FALSE);

	rowcount++;
#endif
	/* Folder chmod */
	checkbtn_folder_chmod = gtk_check_button_new_with_label(_("Folder chmod"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_folder_chmod, 0, 1, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_folder_chmod), 
				     item->prefs->enable_folder_chmod);

	entry_folder_chmod = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_folder_chmod, 1, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	SET_TOGGLE_SENSITIVITY(checkbtn_folder_chmod, entry_folder_chmod);
	if (item->prefs->folder_chmod) {
		gchar *buf;

		buf = g_strdup_printf("%o", item->prefs->folder_chmod);
		gtk_entry_set_text(GTK_ENTRY(entry_folder_chmod), buf);
		g_free(buf);
	}
	
	folder_chmod_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), folder_chmod_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

	rowcount++;
	
	/* Folder color */
	folder_color = gtk_label_new(_("Folder color"));
	gtk_misc_set_alignment(GTK_MISC(folder_color), 0, 0.5);
	gtk_table_attach(GTK_TABLE(table), folder_color, 0, 1, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox, 1, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);

	folder_color_btn = gtk_button_new_with_label("");
	gtk_widget_set_size_request(folder_color_btn, 36, 26);
  	gtk_box_pack_start (GTK_BOX(hbox), folder_color_btn, FALSE, FALSE, 0);
	CLAWS_SET_TIP(folder_color_btn,
			     _("Pick color for folder"));

	page->folder_color = item->prefs->color;

	g_signal_connect(G_OBJECT(folder_color_btn), "clicked",
			 G_CALLBACK(folder_color_set_dialog),
			 page);

	gtkut_set_widget_bgcolor_rgb(folder_color_btn, item->prefs->color);

	folder_color_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), folder_color_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

	rowcount++;

	/* Enable processing at startup */
	checkbtn_enable_processing =
		gtk_check_button_new_with_label(_("Run Processing rules at start-up"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_enable_processing, 0, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_enable_processing), 
				     item->prefs->enable_processing);

	enable_processing_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), enable_processing_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	
	rowcount++;

	/* Enable processing rules when opening folder */
	checkbtn_enable_processing_when_opening =
		gtk_check_button_new_with_label(_("Run Processing rules when opening"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_enable_processing_when_opening, 0, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_enable_processing_when_opening), 
				     item->prefs->enable_processing_when_opening);

	enable_processing_when_opening_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), enable_processing_when_opening_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	
	rowcount++;

	/* Check folder for new mail */
	checkbtn_newmailcheck = gtk_check_button_new_with_label(_("Scan for new mail"));
	CLAWS_SET_TIP(checkbtn_newmailcheck,
			     _("Turn this option on if mail is delivered directly "
			       "to this folder by server side filtering on IMAP or "
			       "by an external application"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_newmailcheck, 0, 2,
			 rowcount, rowcount+1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_newmailcheck),
								 item->prefs->newmailcheck);
	newmailcheck_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), newmailcheck_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

	rowcount++;

	/* Select HTML part by default? */
	hbox = gtk_hbox_new (FALSE, 2);
	gtk_widget_show (hbox);
	gtk_table_attach (GTK_TABLE(table), hbox, 0, 2,
			rowcount, rowcount+1, GTK_FILL, GTK_FILL, 0, 0);

	label = gtk_label_new(_("Select the HTML part of multipart messages"));
	gtk_widget_show (label);
	gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);

	promote_html_part = gtkut_sc_combobox_create (NULL, FALSE);
	gtk_widget_show (promote_html_part);
	gtk_box_pack_start (GTK_BOX(hbox), promote_html_part, FALSE, FALSE, 0);

	promote_html_part_menu = GTK_LIST_STORE(gtk_combo_box_get_model(
				GTK_COMBO_BOX(promote_html_part)));
	COMBOBOX_ADD (promote_html_part_menu, _("Default"), HTML_PROMOTE_DEFAULT);
	COMBOBOX_ADD (promote_html_part_menu, _("No"), HTML_PROMOTE_NEVER);
	COMBOBOX_ADD (promote_html_part_menu, _("Yes"), HTML_PROMOTE_ALWAYS);

	combobox_select_by_data(GTK_COMBO_BOX(promote_html_part),
			item->prefs->promote_html_part);

	CLAWS_SET_TIP(hbox, _("\"Default\" will follow global preference (found in /Preferences/"
			      "Message View/Text Options)"));

	promote_html_part_rec_checkbtn = gtk_check_button_new();
	gtk_widget_show (promote_html_part_rec_checkbtn);
	gtk_table_attach(GTK_TABLE(table), promote_html_part_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	rowcount++;

	/* Synchronise folder for offline use */
	checkbtn_offlinesync = gtk_check_button_new_with_label(_("Synchronise for offline use"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_offlinesync, 0, 2,
			 rowcount, rowcount+1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	
	offlinesync_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), offlinesync_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

	rowcount++;

	hbox = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox);
	gtk_table_attach(GTK_TABLE(table), hbox, 0, 3,
			 rowcount, rowcount+1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	rowcount++;

	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox_spc);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);

	label_offlinesync = gtk_label_new(_("Fetch message bodies from the last"));
	gtk_widget_show (label_offlinesync);
	gtk_box_pack_start (GTK_BOX (hbox), label_offlinesync, FALSE, FALSE, 0);

	entry_offlinesync = gtk_entry_new();
	gtk_widget_set_size_request (entry_offlinesync, 64, -1);
	gtk_widget_show (entry_offlinesync);
	CLAWS_SET_TIP(entry_offlinesync, _("0: all bodies"));
	gtk_box_pack_start (GTK_BOX (hbox), entry_offlinesync, FALSE, FALSE, 0);

	label_end_offlinesync = gtk_label_new(_("days"));
	gtk_widget_show (label_end_offlinesync);
	gtk_box_pack_start (GTK_BOX (hbox), label_end_offlinesync, FALSE, FALSE, 0);

	checkbtn_remove_old_offlinesync = gtk_check_button_new_with_label(
						_("Remove older messages bodies"));

	hbox2 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox2);
	gtk_table_attach(GTK_TABLE(table), hbox2, 0, 3,
			 rowcount, rowcount+1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	rowcount++;

	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox_spc);
	gtk_box_pack_start (GTK_BOX (hbox2), hbox_spc, FALSE, FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox2), checkbtn_remove_old_offlinesync, FALSE, FALSE, 0);

	SET_TOGGLE_SENSITIVITY (checkbtn_offlinesync, hbox);
	SET_TOGGLE_SENSITIVITY (checkbtn_offlinesync, hbox2);
	
	clean_cache_btn = gtk_button_new_with_label(_("Discard folder cache"));
	gtk_widget_show (clean_cache_btn);
	gtk_table_attach(GTK_TABLE(table), clean_cache_btn, 0, 1,
			 rowcount, rowcount+1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	g_signal_connect(G_OBJECT(clean_cache_btn), "clicked",
			 G_CALLBACK(clean_cache_cb),
			 page);

	gtk_widget_show_all(table);

	if (item->folder && (item->folder->klass->type != F_IMAP && 
	    item->folder->klass->type != F_NEWS)) {
		item->prefs->offlinesync = TRUE;
		item->prefs->offlinesync_days = 0;
		item->prefs->remove_old_bodies = FALSE;

		gtk_widget_set_sensitive(GTK_WIDGET(checkbtn_offlinesync),
								 FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(offlinesync_rec_checkbtn),
								 FALSE);
		gtk_widget_hide(GTK_WIDGET(checkbtn_offlinesync));
		gtk_widget_hide(GTK_WIDGET(hbox));
		gtk_widget_hide(GTK_WIDGET(hbox2));
		gtk_widget_hide(GTK_WIDGET(offlinesync_rec_checkbtn));
		gtk_widget_hide(GTK_WIDGET(clean_cache_btn));
	
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_offlinesync),
								 item->prefs->offlinesync);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_remove_old_offlinesync),
								 item->prefs->remove_old_bodies);
	gtk_entry_set_text(GTK_ENTRY(entry_offlinesync), itos(item->prefs->offlinesync_days));

	page->table = table;
	page->folder_type = folder_type;
	page->no_save_warning = no_save_warning;
#ifndef G_OS_WIN32
	page->checkbtn_simplify_subject = checkbtn_simplify_subject;
	page->entry_simplify_subject = entry_simplify_subject;
	page->entry_regexp_test_string = entry_regexp_test_string;
	page->entry_regexp_test_result = entry_regexp_test_result;
#endif
	page->checkbtn_folder_chmod = checkbtn_folder_chmod;
	page->entry_folder_chmod = entry_folder_chmod;
	page->folder_color_btn = folder_color_btn;
	page->checkbtn_enable_processing = checkbtn_enable_processing;
	page->checkbtn_enable_processing_when_opening = checkbtn_enable_processing_when_opening;
	page->checkbtn_newmailcheck = checkbtn_newmailcheck;
	page->checkbtn_offlinesync = checkbtn_offlinesync;
	page->label_offlinesync = label_offlinesync;
	page->entry_offlinesync = entry_offlinesync;
	page->label_end_offlinesync = label_end_offlinesync;
	page->checkbtn_remove_old_offlinesync = checkbtn_remove_old_offlinesync;
	page->promote_html_part = promote_html_part;

#ifndef G_OS_WIN32
	page->simplify_subject_rec_checkbtn  = simplify_subject_rec_checkbtn;
#endif
	page->folder_chmod_rec_checkbtn	     = folder_chmod_rec_checkbtn;
	page->folder_color_rec_checkbtn	     = folder_color_rec_checkbtn;
	page->enable_processing_rec_checkbtn = enable_processing_rec_checkbtn;
	page->enable_processing_when_opening_rec_checkbtn = enable_processing_when_opening_rec_checkbtn;
	page->newmailcheck_rec_checkbtn	     = newmailcheck_rec_checkbtn;
	page->offlinesync_rec_checkbtn	     = offlinesync_rec_checkbtn;
	page->promote_html_part_rec_checkbtn = promote_html_part_rec_checkbtn;

	page->page.widget = table;

#ifndef G_OS_WIN32
	folder_regexp_set_subject_example_cb(NULL, page);
#endif
}

static void prefs_folder_item_general_destroy_widget_func(PrefsPage *page_) 
{
	/* FolderItemGeneralPage *page = (FolderItemGeneralPage *) page_; */
}

/** \brief  Save the prefs in page to folder.
 *
 *  If the folder is not the one  specified in page->item, then only those properties 
 *  that have the relevant 'apply to sub folders' button checked are saved
 */
static void general_save_folder_prefs(FolderItem *folder, FolderItemGeneralPage *page)
{
	FolderItemPrefs *prefs = folder->prefs;
	gchar *buf;
	gboolean all = FALSE, summary_update_needed = FALSE;
	SpecialFolderItemType type = F_NORMAL;
	FolderView *folderview = mainwindow_get_mainwindow()->folderview;
	HTMLPromoteType promote_html_part = HTML_PROMOTE_DEFAULT;

	if (folder->path == NULL)
		return;

	cm_return_if_fail(prefs != NULL);

	if (page->item == folder) 
		all = TRUE;

	type = combobox_get_active_data(GTK_COMBO_BOX(page->folder_type));
	if (all && folder->stype != type && page->item->parent_stype == F_NORMAL) {
		folder_item_change_type(folder, type);
		summary_update_needed = TRUE;
	}

	promote_html_part =
		combobox_get_active_data(GTK_COMBO_BOX(page->promote_html_part));
	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->promote_html_part_rec_checkbtn)))
		prefs->promote_html_part = promote_html_part;

#ifndef G_OS_WIN32
	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->simplify_subject_rec_checkbtn))) {
		gboolean old_simplify_subject = prefs->enable_simplify_subject;
		int regexp_diffs = strcmp2(prefs->simplify_subject_regexp, gtk_editable_get_chars(
					GTK_EDITABLE(page->entry_simplify_subject), 0, -1));
		prefs->enable_simplify_subject =
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_simplify_subject));
		ASSIGN_STRING(prefs->simplify_subject_regexp,
			      gtk_editable_get_chars(GTK_EDITABLE(page->entry_simplify_subject), 0, -1));
		if (old_simplify_subject != prefs->enable_simplify_subject || regexp_diffs != 0)
			summary_update_needed = TRUE;
	}
#endif	
	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->folder_chmod_rec_checkbtn))) {
		prefs->enable_folder_chmod = 
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_folder_chmod));
		buf = gtk_editable_get_chars(GTK_EDITABLE(page->entry_folder_chmod), 0, -1);
		prefs->folder_chmod = prefs_folder_item_chmod_mode(buf);
		g_free(buf);
	}

	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->folder_color_rec_checkbtn))) {
		int old_color = prefs->color;
		prefs->color = page->folder_color;
	
		/* update folder view */
		if (prefs->color != old_color)
			folder_item_update(folder, F_ITEM_UPDATE_MSGCNT);
	}

	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->enable_processing_rec_checkbtn))) {
		prefs->enable_processing = 
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_enable_processing));
	}

	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->enable_processing_when_opening_rec_checkbtn))) {
		prefs->enable_processing_when_opening = 
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_enable_processing_when_opening));
	}

	if (all ||  gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->newmailcheck_rec_checkbtn))) {
		prefs->newmailcheck = 
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_newmailcheck));
	}

	if (all ||  gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->offlinesync_rec_checkbtn))) {
		prefs->offlinesync = 
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_offlinesync));
		prefs->offlinesync_days = 
			atoi(gtk_entry_get_text(GTK_ENTRY(page->entry_offlinesync)));
		prefs->remove_old_bodies =
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_remove_old_offlinesync));
	}

	folder_item_prefs_save_config(folder);

	if (folder->opened && summary_update_needed) {
		summary_set_prefs_from_folderitem(folderview->summaryview, folder);
		summary_show(folderview->summaryview, folder);
	}	
}	

static gboolean general_save_recurse_func(GNode *node, gpointer data)
{
	FolderItem *item = (FolderItem *) node->data;
	FolderItemGeneralPage *page = (FolderItemGeneralPage *) data;

	cm_return_val_if_fail(item != NULL, TRUE);
	cm_return_val_if_fail(page != NULL, TRUE);

	general_save_folder_prefs(item, page);

	/* optimise by not continuing if none of the 'apply to sub folders'
	   check boxes are selected - and optimise the checking by only doing
	   it once */
	if ((node == page->item->node) &&
	    !(
#ifndef G_OS_WIN32
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->simplify_subject_rec_checkbtn)) ||
#endif
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->folder_chmod_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->folder_color_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->enable_processing_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->enable_processing_when_opening_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->newmailcheck_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->offlinesync_rec_checkbtn)) ||
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->promote_html_part_rec_checkbtn))
			))
		return TRUE;
	else 
		return FALSE;
}

static void prefs_folder_item_general_save_func(PrefsPage *page_) 
{
	FolderItemGeneralPage *page = (FolderItemGeneralPage *) page_;

	g_node_traverse(page->item->node, G_PRE_ORDER, G_TRAVERSE_ALL,
			-1, general_save_recurse_func, page);
	
	main_window_set_menu_sensitive(mainwindow_get_mainwindow());

}

static RecvProtocol item_protocol(FolderItem *item)
{
	if (!item)
		return A_NONE;
	if (!item->folder)
		return A_NONE;
	if (!item->folder->account)
		return A_NONE;
	return item->folder->account->protocol;
}

static void prefs_folder_item_compose_create_widget_func(PrefsPage * page_,
						   GtkWindow * window,
                                		   gpointer data)
{
	FolderItemComposePage *page = (FolderItemComposePage *) page_;
	FolderItem *item = (FolderItem *) data;
	guint rowcount;
	gchar *text = NULL;

	GtkWidget *table;
	GtkWidget *label;
	
	GtkWidget *no_save_warning = NULL;
	GtkWidget *checkbtn_request_return_receipt = NULL;
	GtkWidget *checkbtn_save_copy_to_folder = NULL;
	GtkWidget *checkbtn_default_to = NULL;
	GtkWidget *entry_default_to = NULL;
	GtkWidget *checkbtn_default_reply_to = NULL;
	GtkWidget *entry_default_reply_to = NULL;
	GtkWidget *checkbtn_default_cc = NULL;
	GtkWidget *entry_default_cc = NULL;
	GtkWidget *checkbtn_default_bcc = NULL;
	GtkWidget *entry_default_bcc = NULL;
	GtkWidget *checkbtn_default_replyto = NULL;
	GtkWidget *entry_default_replyto = NULL;
	GtkWidget *checkbtn_enable_default_account = NULL;
	GtkWidget *optmenu_default_account = NULL;
	GtkListStore *optmenu_default_account_menu = NULL;
	GtkTreeIter iter;
#if USE_ENCHANT
	GtkWidget *checkbtn_enable_default_dictionary = NULL;
	GtkWidget *combo_default_dictionary = NULL;
	GtkWidget *checkbtn_enable_default_alt_dictionary = NULL;
	GtkWidget *combo_default_alt_dictionary = NULL;
	GtkWidget *default_dictionary_rec_checkbtn = NULL;
	GtkWidget *default_alt_dictionary_rec_checkbtn = NULL;
	gchar *dictionary;
#endif
	GtkWidget *request_return_receipt_rec_checkbtn = NULL;
	GtkWidget *save_copy_to_folder_rec_checkbtn = NULL;
	GtkWidget *default_to_rec_checkbtn = NULL;
	GtkWidget *default_reply_to_rec_checkbtn = NULL;
	GtkWidget *default_cc_rec_checkbtn = NULL;
	GtkWidget *default_bcc_rec_checkbtn = NULL;
	GtkWidget *default_replyto_rec_checkbtn = NULL;
	GtkWidget *default_account_rec_checkbtn = NULL;

	GList *cur_ac;
	GList *account_list;
	PrefsAccount *ac_prefs;
	gboolean default_account_set = FALSE;

	page->item	   = item;

	/* Table */
#if USE_ENCHANT
# define TABLEHEIGHT 7
#else
# define TABLEHEIGHT 6
#endif
	table = gtk_table_new(TABLEHEIGHT, 3, FALSE);
	gtk_container_set_border_width (GTK_CONTAINER (table), VBOX_BORDER);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 4);
	rowcount = 0;

	if (!can_save) {
		no_save_warning = gtk_label_new(
			_("<i>These preferences will not be saved as this folder "
			"is a top-level folder. However, you can set them for the "
			"whole mailbox tree by using \"Apply to subfolders\".</i>"));
		gtk_label_set_use_markup(GTK_LABEL(no_save_warning), TRUE);
		gtk_label_set_line_wrap(GTK_LABEL(no_save_warning), TRUE);
		gtk_misc_set_alignment(GTK_MISC(no_save_warning), 0.0, 0.5);
		gtk_table_attach(GTK_TABLE(table), no_save_warning, 0, 3,
			 rowcount, rowcount + 1, GTK_FILL, 0, 0, 0);
		rowcount++;
	}
	
	/* Apply to subfolders */
	label = gtk_label_new(_("Apply to\nsubfolders"));
	gtk_misc_set_alignment(GTK_MISC(label), 0.5, 0.5);
	gtk_table_attach(GTK_TABLE(table), label, 2, 3,
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	rowcount++;

	if (item_protocol(item) != A_NNTP) {
		/* Request Return Receipt */
		checkbtn_request_return_receipt = gtk_check_button_new_with_label
			(_("Request Return Receipt"));
		gtk_table_attach(GTK_TABLE(table), checkbtn_request_return_receipt, 
				 0, 2, rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, 
				 GTK_FILL, 0, 0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_request_return_receipt),
					     item->ret_rcpt ? TRUE : FALSE);

		request_return_receipt_rec_checkbtn = gtk_check_button_new();
		gtk_table_attach(GTK_TABLE(table), request_return_receipt_rec_checkbtn, 2, 3, 
				 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

		rowcount++;

		/* Save Copy to Folder */
		checkbtn_save_copy_to_folder = gtk_check_button_new_with_label
			(_("Save copy of outgoing messages to this folder instead of Sent"));
		gtk_table_attach(GTK_TABLE(table), checkbtn_save_copy_to_folder, 0, 2, 
				 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_FILL, 0, 0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_save_copy_to_folder),
					     item->prefs->save_copy_to_folder ? TRUE : FALSE);

		save_copy_to_folder_rec_checkbtn = gtk_check_button_new();
		gtk_table_attach(GTK_TABLE(table), save_copy_to_folder_rec_checkbtn, 2, 3, 
				 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

		rowcount++;

		/* Default To */
		text = g_strconcat(_("Default "), prefs_common_translated_header_name("To:"), NULL);
		checkbtn_default_to = gtk_check_button_new_with_label(text);
		gtk_table_attach(GTK_TABLE(table), checkbtn_default_to, 0, 1, 
				 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_default_to), 
					     item->prefs->enable_default_to);
		g_free(text);

		entry_default_to = gtk_entry_new();
		gtk_table_attach(GTK_TABLE(table), entry_default_to, 1, 2,
				 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);
		SET_TOGGLE_SENSITIVITY(checkbtn_default_to, entry_default_to);
		gtk_entry_set_text(GTK_ENTRY(entry_default_to), SAFE_STRING(item->prefs->default_to));
		address_completion_register_entry(GTK_ENTRY(entry_default_to),
				TRUE);

		default_to_rec_checkbtn = gtk_check_button_new();
		gtk_table_attach(GTK_TABLE(table), default_to_rec_checkbtn, 2, 3, 
				 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

		rowcount++;

		/* Default address to reply to */
		text = g_strconcat(_("Default "), prefs_common_translated_header_name("To:"),
				 _(" for replies"), NULL);
		checkbtn_default_reply_to = gtk_check_button_new_with_label(text);
		gtk_table_attach(GTK_TABLE(table), checkbtn_default_reply_to, 0, 1, 
				 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_default_reply_to), 
					     item->prefs->enable_default_reply_to);
		g_free(text);

		entry_default_reply_to = gtk_entry_new();
		gtk_table_attach(GTK_TABLE(table), entry_default_reply_to, 1, 2,
				 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);
		SET_TOGGLE_SENSITIVITY(checkbtn_default_reply_to, entry_default_reply_to);
		gtk_entry_set_text(GTK_ENTRY(entry_default_reply_to), SAFE_STRING(item->prefs->default_reply_to));
		address_completion_register_entry(
				GTK_ENTRY(entry_default_reply_to), TRUE);

		default_reply_to_rec_checkbtn = gtk_check_button_new();
		gtk_table_attach(GTK_TABLE(table), default_reply_to_rec_checkbtn, 2, 3, 
				 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

		rowcount++;

		/* Default Cc */
		text = g_strconcat(_("Default "), prefs_common_translated_header_name("Cc:"), NULL);
		checkbtn_default_cc = gtk_check_button_new_with_label(text);
		gtk_table_attach(GTK_TABLE(table), checkbtn_default_cc, 0, 1, 
				 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_default_cc), 
					     item->prefs->enable_default_cc);
		g_free(text);

		entry_default_cc = gtk_entry_new();
		gtk_table_attach(GTK_TABLE(table), entry_default_cc, 1, 2,
				 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);
		SET_TOGGLE_SENSITIVITY(checkbtn_default_cc, entry_default_cc);
		gtk_entry_set_text(GTK_ENTRY(entry_default_cc), SAFE_STRING(item->prefs->default_cc));
		address_completion_register_entry(GTK_ENTRY(entry_default_cc),
				TRUE);

		default_cc_rec_checkbtn = gtk_check_button_new();
		gtk_table_attach(GTK_TABLE(table), default_cc_rec_checkbtn, 2, 3, 
				 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

		rowcount++;

		/* Default Bcc */
		text = g_strconcat(_("Default "), prefs_common_translated_header_name("Bcc:"), NULL);
		checkbtn_default_bcc = gtk_check_button_new_with_label(text);
		gtk_table_attach(GTK_TABLE(table), checkbtn_default_bcc, 0, 1, 
				 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_default_bcc), 
					     item->prefs->enable_default_bcc);
		g_free(text);

		entry_default_bcc = gtk_entry_new();
		gtk_table_attach(GTK_TABLE(table), entry_default_bcc, 1, 2,
				 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);
		SET_TOGGLE_SENSITIVITY(checkbtn_default_bcc, entry_default_bcc);
		gtk_entry_set_text(GTK_ENTRY(entry_default_bcc), SAFE_STRING(item->prefs->default_bcc));
		address_completion_register_entry(GTK_ENTRY(entry_default_bcc),
				TRUE);

		default_bcc_rec_checkbtn = gtk_check_button_new();
		gtk_table_attach(GTK_TABLE(table), default_bcc_rec_checkbtn, 2, 3, 
				 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

		rowcount++;

		/* Default Reply-to */
		text = g_strconcat(_("Default "), prefs_common_translated_header_name("Reply-To:"), NULL);
		checkbtn_default_replyto = gtk_check_button_new_with_label(text);
		gtk_table_attach(GTK_TABLE(table), checkbtn_default_replyto, 0, 1, 
				 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_default_replyto), 
					     item->prefs->enable_default_replyto);
		g_free(text);

		entry_default_replyto = gtk_entry_new();
		gtk_table_attach(GTK_TABLE(table), entry_default_replyto, 1, 2,
				 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);
		SET_TOGGLE_SENSITIVITY(checkbtn_default_replyto, entry_default_replyto);
		gtk_entry_set_text(GTK_ENTRY(entry_default_replyto), SAFE_STRING(item->prefs->default_replyto));
		address_completion_register_entry(GTK_ENTRY(entry_default_replyto),
				TRUE);

		default_replyto_rec_checkbtn = gtk_check_button_new();
		gtk_table_attach(GTK_TABLE(table), default_replyto_rec_checkbtn, 2, 3, 
				 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);

		rowcount++;
	}
	/* Default account */
	checkbtn_enable_default_account = gtk_check_button_new_with_label(_("Default account"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_enable_default_account, 0, 1, 
			 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_enable_default_account), 
				     item->prefs->enable_default_account);

 	optmenu_default_account = gtkut_sc_combobox_create(NULL, FALSE);
	gtk_table_attach(GTK_TABLE(table), optmenu_default_account, 1, 2, 
			 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);
 	optmenu_default_account_menu = GTK_LIST_STORE(
			gtk_combo_box_get_model(GTK_COMBO_BOX(optmenu_default_account)));

	account_list = account_get_list();
	for (cur_ac = account_list; cur_ac != NULL; cur_ac = cur_ac->next) {
		ac_prefs = (PrefsAccount *)cur_ac->data;
		if (item->folder->account &&
	    	    ( (item_protocol(item) == A_NNTP && ac_prefs->protocol != A_NNTP)
		    ||(item_protocol(item) != A_NNTP && ac_prefs->protocol == A_NNTP))) 
			continue;

		if (item->folder->klass->type != F_NEWS && ac_prefs->protocol == A_NNTP)
			continue;

		COMBOBOX_ADD_ESCAPED (optmenu_default_account_menu,
					ac_prefs->account_name?ac_prefs->account_name : _("Untitled"),
					ac_prefs->account_id);

		/* Set combobox to current default account id */
		if (ac_prefs->account_id == item->prefs->default_account) {
			combobox_select_by_data(GTK_COMBO_BOX(optmenu_default_account),
					ac_prefs->account_id);
			default_account_set = TRUE;
		}
	}

	/* If nothing has been set (folder doesn't have a default account set),
	 * pre-select global default account, since that's what actually used
	 * anyway. We don't want nothing selected in combobox. */
	if( !default_account_set )
		combobox_select_by_data(GTK_COMBO_BOX(optmenu_default_account),
				account_get_default()->account_id);

	SET_TOGGLE_SENSITIVITY(checkbtn_enable_default_account, optmenu_default_account);

	default_account_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), default_account_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	rowcount++;

#if USE_ENCHANT
	/* Default dictionary */
	checkbtn_enable_default_dictionary = gtk_check_button_new_with_label(_("Default dictionary"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_enable_default_dictionary, 0, 1,
	    		 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_enable_default_dictionary),
	    			     item->prefs->enable_default_dictionary);

	combo_default_dictionary = gtkaspell_dictionary_combo_new(TRUE);
	gtk_table_attach(GTK_TABLE(table), combo_default_dictionary, 1, 2,
	    		 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);

	dictionary = item->prefs->default_dictionary;
	if (dictionary && strrchr(dictionary, '/')) {
		gchar *tmp = g_strdup(strrchr(dictionary, '/')+1);
		g_free(item->prefs->default_dictionary);
		item->prefs->default_dictionary = tmp;
		dictionary = item->prefs->default_dictionary;
	}
	if (item->prefs->default_dictionary &&
	    strchr(item->prefs->default_dictionary, '-')) {
		*(strchr(item->prefs->default_dictionary, '-')) = '\0';
	}
	if (dictionary)
		gtkaspell_set_dictionary_menu_active_item(
			GTK_COMBO_BOX(combo_default_dictionary), dictionary);

	SET_TOGGLE_SENSITIVITY(checkbtn_enable_default_dictionary, combo_default_dictionary);

	default_dictionary_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), default_dictionary_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	
	rowcount++;

	/* Default dictionary */
	checkbtn_enable_default_alt_dictionary = gtk_check_button_new_with_label(_("Default alternate dictionary"));
	gtk_table_attach(GTK_TABLE(table), checkbtn_enable_default_alt_dictionary, 0, 1,
	    		 rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_enable_default_alt_dictionary),
	    			     item->prefs->enable_default_alt_dictionary);

	combo_default_alt_dictionary = gtkaspell_dictionary_combo_new(FALSE);
	gtk_table_attach(GTK_TABLE(table), combo_default_alt_dictionary, 1, 2,
	    		 rowcount, rowcount + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);

	dictionary = item->prefs->default_alt_dictionary;
	if (dictionary && strrchr(dictionary, '/')) {
		gchar *tmp = g_strdup(strrchr(dictionary, '/')+1);
		g_free(item->prefs->default_alt_dictionary);
		item->prefs->default_alt_dictionary = tmp;
		dictionary = item->prefs->default_alt_dictionary;
	}
	if (item->prefs->default_alt_dictionary &&
	    strchr(item->prefs->default_alt_dictionary, '-')) {
		*(strchr(item->prefs->default_alt_dictionary, '-')) = '\0';
	}
	if (dictionary)
		gtkaspell_set_dictionary_menu_active_item(
			GTK_COMBO_BOX(combo_default_alt_dictionary), dictionary);

	SET_TOGGLE_SENSITIVITY(checkbtn_enable_default_alt_dictionary, combo_default_alt_dictionary);

	default_alt_dictionary_rec_checkbtn = gtk_check_button_new();
	gtk_table_attach(GTK_TABLE(table), default_alt_dictionary_rec_checkbtn, 2, 3, 
			 rowcount, rowcount + 1, GTK_SHRINK, GTK_SHRINK, 0, 0);
	
	rowcount++;
#endif

	gtk_widget_show_all(table);

	page->window = GTK_WIDGET(window);
	page->table = table;
	page->no_save_warning = no_save_warning;
	page->checkbtn_request_return_receipt = checkbtn_request_return_receipt;
	page->checkbtn_save_copy_to_folder = checkbtn_save_copy_to_folder;
	page->checkbtn_default_to = checkbtn_default_to;
	page->entry_default_to = entry_default_to;
	page->checkbtn_default_reply_to = checkbtn_default_reply_to;
	page->entry_default_reply_to = entry_default_reply_to;
	page->checkbtn_default_cc = checkbtn_default_cc;
	page->entry_default_cc = entry_default_cc;
	page->checkbtn_default_bcc = checkbtn_default_bcc;
	page->entry_default_bcc = entry_default_bcc;
	page->checkbtn_default_replyto = checkbtn_default_replyto;
	page->entry_default_replyto = entry_default_replyto;
	page->checkbtn_enable_default_account = checkbtn_enable_default_account;
	page->optmenu_default_account = optmenu_default_account;
#ifdef USE_ENCHANT
	page->checkbtn_enable_default_dictionary = checkbtn_enable_default_dictionary;
	page->combo_default_dictionary = combo_default_dictionary;
	page->checkbtn_enable_default_alt_dictionary = checkbtn_enable_default_alt_dictionary;
	page->combo_default_alt_dictionary = combo_default_alt_dictionary;
#endif

	page->request_return_receipt_rec_checkbtn = request_return_receipt_rec_checkbtn;
	page->save_copy_to_folder_rec_checkbtn	  = save_copy_to_folder_rec_checkbtn;
	page->default_to_rec_checkbtn		  = default_to_rec_checkbtn;
	page->default_reply_to_rec_checkbtn	  = default_reply_to_rec_checkbtn;
	page->default_cc_rec_checkbtn		  = default_cc_rec_checkbtn;
	page->default_bcc_rec_checkbtn		  = default_bcc_rec_checkbtn;
	page->default_replyto_rec_checkbtn		  = default_replyto_rec_checkbtn;
	page->default_account_rec_checkbtn	  = default_account_rec_checkbtn;
#if USE_ENCHANT
	page->default_dictionary_rec_checkbtn = default_dictionary_rec_checkbtn;
	page->default_alt_dictionary_rec_checkbtn = default_alt_dictionary_rec_checkbtn;
#endif

	page->page.widget = table;
}

static void prefs_folder_item_compose_destroy_widget_func(PrefsPage *page_) 
{
	FolderItemComposePage *page = (FolderItemComposePage *) page_;

	if (page->entry_default_to)
		address_completion_unregister_entry(GTK_ENTRY(page->entry_default_to));
	if (page->entry_default_reply_to)
		address_completion_unregister_entry(GTK_ENTRY(page->entry_default_reply_to));
	if (page->entry_default_cc)
		address_completion_unregister_entry(GTK_ENTRY(page->entry_default_cc));
	if (page->entry_default_bcc)
		address_completion_unregister_entry(GTK_ENTRY(page->entry_default_bcc));
	if (page->entry_default_replyto)
		address_completion_unregister_entry(GTK_ENTRY(page->entry_default_replyto));
}

/** \brief  Save the prefs in page to folder.
 *
 *  If the folder is not the one  specified in page->item, then only those properties 
 *  that have the relevant 'apply to sub folders' button checked are saved
 */
static void compose_save_folder_prefs(FolderItem *folder, FolderItemComposePage *page)
{
	FolderItemPrefs *prefs = folder->prefs;

	gboolean all = FALSE;

	if (folder->path == NULL)
		return;

	if (page->item == folder) 
		all = TRUE;

	cm_return_if_fail(prefs != NULL);

	if (item_protocol(folder) != A_NNTP) {
		if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->request_return_receipt_rec_checkbtn))) {
			prefs->request_return_receipt = 
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_request_return_receipt));
			/* MIGRATION */    
			folder->ret_rcpt = prefs->request_return_receipt;
		}

		if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->save_copy_to_folder_rec_checkbtn))) {
			prefs->save_copy_to_folder = 
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_save_copy_to_folder));
		}

		if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_to_rec_checkbtn))) {

			prefs->enable_default_to = 
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_default_to));
			ASSIGN_STRING(prefs->default_to,
				      gtk_editable_get_chars(GTK_EDITABLE(page->entry_default_to), 0, -1));
		}

		if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_reply_to_rec_checkbtn))) {
			prefs->enable_default_reply_to = 
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_default_reply_to));
			ASSIGN_STRING(prefs->default_reply_to,
				      gtk_editable_get_chars(GTK_EDITABLE(page->entry_default_reply_to), 0, -1));
		}

		if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_cc_rec_checkbtn))) {

			prefs->enable_default_cc = 
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_default_cc));
			ASSIGN_STRING(prefs->default_cc,
				      gtk_editable_get_chars(GTK_EDITABLE(page->entry_default_cc), 0, -1));
		}

		if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_bcc_rec_checkbtn))) {

			prefs->enable_default_bcc = 
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_default_bcc));
			ASSIGN_STRING(prefs->default_bcc,
				      gtk_editable_get_chars(GTK_EDITABLE(page->entry_default_bcc), 0, -1));
		}

		if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_replyto_rec_checkbtn))) {

			prefs->enable_default_replyto = 
				gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_default_replyto));
			ASSIGN_STRING(prefs->default_replyto,
				      gtk_editable_get_chars(GTK_EDITABLE(page->entry_default_replyto), 0, -1));
		}

	} else {
		prefs->request_return_receipt = FALSE;
		prefs->save_copy_to_folder = FALSE;
		prefs->enable_default_to = FALSE;
		prefs->enable_default_reply_to = FALSE;
		prefs->enable_default_cc = FALSE;
		prefs->enable_default_bcc = FALSE;
		prefs->enable_default_replyto = FALSE;
	}

	if (all || gtk_toggle_button_get_active(
				GTK_TOGGLE_BUTTON(page->default_account_rec_checkbtn))) {
		prefs->enable_default_account = 
			gtk_toggle_button_get_active(
					GTK_TOGGLE_BUTTON(page->checkbtn_enable_default_account));
		prefs->default_account = combobox_get_active_data(
				GTK_COMBO_BOX(page->optmenu_default_account));
	}

#if USE_ENCHANT
	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_dictionary_rec_checkbtn))) {
		prefs->enable_default_dictionary =
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_enable_default_dictionary));
		ASSIGN_STRING(prefs->default_dictionary,
			      gtkaspell_get_dictionary_menu_active_item(
			      		GTK_COMBO_BOX(page->combo_default_dictionary)));
	}
	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_alt_dictionary_rec_checkbtn))) {
		prefs->enable_default_alt_dictionary =
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_enable_default_alt_dictionary));
		ASSIGN_STRING(prefs->default_alt_dictionary,
			      gtkaspell_get_dictionary_menu_active_item(
				      GTK_COMBO_BOX(page->combo_default_alt_dictionary)));
	}
#endif

	folder_item_prefs_save_config(folder);
}	

static gboolean compose_save_recurse_func(GNode *node, gpointer data)
{
	FolderItem *item = (FolderItem *) node->data;
	FolderItemComposePage *page = (FolderItemComposePage *) data;

	cm_return_val_if_fail(item != NULL, TRUE);
	cm_return_val_if_fail(page != NULL, TRUE);

	compose_save_folder_prefs(item, page);

	/* optimise by not continuing if none of the 'apply to sub folders'
	   check boxes are selected - and optimise the checking by only doing
	   it once */
	if ((node == page->item->node) && item_protocol(item) != A_NNTP &&
	    !(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->request_return_receipt_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->save_copy_to_folder_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_to_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_account_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_cc_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_bcc_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_replyto_rec_checkbtn)) ||
#if USE_ENCHANT
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_dictionary_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_alt_dictionary_rec_checkbtn)) ||
#endif
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_reply_to_rec_checkbtn))
			))
		return TRUE;
	else if ((node == page->item->node) && item_protocol(item) == A_NNTP &&
	    !(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_account_rec_checkbtn)) 
#if USE_ENCHANT
	      || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_dictionary_rec_checkbtn))
	      || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->default_alt_dictionary_rec_checkbtn))
#endif
		    ))
		return TRUE;
	else 
		return FALSE;
}

static void prefs_folder_item_compose_save_func(PrefsPage *page_) 
{
	FolderItemComposePage *page = (FolderItemComposePage *) page_;

	g_node_traverse(page->item->node, G_PRE_ORDER, G_TRAVERSE_ALL,
			-1, compose_save_recurse_func, page);

}

static void prefs_folder_item_templates_create_widget_func(PrefsPage * page_,
						   GtkWindow * window,
                                		   gpointer data)
{
	FolderItemTemplatesPage *page = (FolderItemTemplatesPage *) page_;
	FolderItem *item = (FolderItem *) data;

	GtkWidget *notebook;
	GtkWidget *vbox;
	GtkWidget *page_vbox;
	GtkWidget *no_save_warning;
	GtkWidget *new_msg_format_rec_checkbtn;
	GtkWidget *reply_format_rec_checkbtn;
	GtkWidget *forward_format_rec_checkbtn;
	GtkWidget *hbox;
	GtkWidget *vbox_format;
	
	page->item = item;

	page_vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (page_vbox);
	
	if (!can_save) {
		no_save_warning = gtk_label_new(
			_("<i>These preferences will not be saved as this folder "
			"is a top-level folder. However, you can set them for the "
			"whole mailbox tree by using \"Apply to subfolders\".</i>"));
		gtk_label_set_use_markup(GTK_LABEL(no_save_warning), TRUE);
		gtk_label_set_line_wrap(GTK_LABEL(no_save_warning), TRUE);
		gtk_misc_set_alignment(GTK_MISC(no_save_warning), 0.0, 0.5);
		gtk_box_pack_start(GTK_BOX(page_vbox), no_save_warning, FALSE, FALSE, 4);
	}

	/* Notebook */
	notebook = gtk_notebook_new();
	gtk_widget_show(notebook);
	gtk_box_pack_start(GTK_BOX(page_vbox), notebook, TRUE, TRUE, 0);

	/* compose format */
	vbox = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), VBOX_BORDER);

	quotefmt_create_new_msg_fmt_widgets(
				window,
				vbox,
				&page->checkbtn_compose_with_format,
				&page->compose_override_from_format,
				&page->compose_subject_format,
				&page->compose_body_format,
				FALSE, FALSE);
	address_completion_register_entry(GTK_ENTRY(page->compose_override_from_format),
			TRUE);
	
	vbox_format = gtk_widget_get_parent(
			gtk_widget_get_parent(page->compose_body_format));

	hbox = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_end (GTK_BOX(vbox_format), hbox, FALSE, FALSE, 0);
	quotefmt_add_info_button(window, hbox);
	
	new_msg_format_rec_checkbtn = gtk_check_button_new_with_label(
			_("Apply to subfolders"));
	gtk_box_pack_end (GTK_BOX(hbox), new_msg_format_rec_checkbtn, FALSE, FALSE, 0);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, gtk_label_new(_("Compose")));

	/* reply format */
	vbox = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), VBOX_BORDER);

	quotefmt_create_reply_fmt_widgets(
				window,
				vbox,
				&page->checkbtn_reply_with_format,
				&page->reply_override_from_format,
				&page->reply_quotemark,
				&page->reply_body_format,
				FALSE, FALSE);
	address_completion_register_entry(GTK_ENTRY(page->reply_override_from_format),
			TRUE);

	vbox_format = gtk_widget_get_parent(
			gtk_widget_get_parent(page->reply_body_format));

	hbox = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_end (GTK_BOX(vbox_format), hbox, FALSE, FALSE, 0);
	quotefmt_add_info_button(window, hbox);

	reply_format_rec_checkbtn = gtk_check_button_new_with_label(
			_("Apply to subfolders"));
	gtk_box_pack_end (GTK_BOX(hbox), reply_format_rec_checkbtn, FALSE, FALSE, 0);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, gtk_label_new(_("Reply")));

	/* forward format */
	vbox = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), VBOX_BORDER);

	quotefmt_create_forward_fmt_widgets(
				window,
				vbox,
				&page->checkbtn_forward_with_format,
				&page->forward_override_from_format,
				&page->forward_quotemark,
				&page->forward_body_format,
				FALSE, FALSE);
	address_completion_register_entry(GTK_ENTRY(page->forward_override_from_format),
			TRUE);

	vbox_format = gtk_widget_get_parent(
			gtk_widget_get_parent(page->forward_body_format));

	hbox = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_end (GTK_BOX(vbox_format), hbox, FALSE, FALSE, 0);
	quotefmt_add_info_button(window, hbox);

	forward_format_rec_checkbtn = gtk_check_button_new_with_label(
			_("Apply to subfolders"));
	gtk_box_pack_end (GTK_BOX(hbox), forward_format_rec_checkbtn, FALSE, FALSE, 0);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, gtk_label_new(_("Forward")));

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(page->checkbtn_compose_with_format),
			item->prefs->compose_with_format);
	pref_set_entry_from_pref(GTK_ENTRY(page->compose_override_from_format),
			item->prefs->compose_override_from_format);
	pref_set_entry_from_pref(GTK_ENTRY(page->compose_subject_format),
			item->prefs->compose_subject_format);
	pref_set_textview_from_pref(GTK_TEXT_VIEW(page->compose_body_format),
			item->prefs->compose_body_format);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(page->checkbtn_reply_with_format),
			item->prefs->reply_with_format);
	pref_set_entry_from_pref(GTK_ENTRY(page->reply_quotemark),
			item->prefs->reply_quotemark);
	pref_set_entry_from_pref(GTK_ENTRY(page->reply_override_from_format),
			item->prefs->reply_override_from_format);
	pref_set_textview_from_pref(GTK_TEXT_VIEW(page->reply_body_format),
			item->prefs->reply_body_format);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(page->checkbtn_forward_with_format),
			item->prefs->forward_with_format);
	pref_set_entry_from_pref(GTK_ENTRY(page->forward_quotemark),
			item->prefs->forward_quotemark);
	pref_set_entry_from_pref(GTK_ENTRY(page->forward_override_from_format),
			item->prefs->forward_override_from_format);
	pref_set_textview_from_pref(GTK_TEXT_VIEW(page->forward_body_format),
			item->prefs->forward_body_format);

	gtk_widget_show_all(page_vbox);

	page->window = GTK_WIDGET(window);

	page->new_msg_format_rec_checkbtn = new_msg_format_rec_checkbtn;
	page->reply_format_rec_checkbtn = reply_format_rec_checkbtn;
	page->forward_format_rec_checkbtn = forward_format_rec_checkbtn;

	page->page.widget = page_vbox;
}

static void prefs_folder_item_templates_destroy_widget_func(PrefsPage *page_) 
{
	FolderItemTemplatesPage *page = (FolderItemTemplatesPage *) page_;

	if (page->compose_override_from_format)
		address_completion_unregister_entry(GTK_ENTRY(page->compose_override_from_format));		
	if (page->reply_override_from_format)
		address_completion_unregister_entry(GTK_ENTRY(page->reply_override_from_format));		
	if (page->forward_override_from_format)
		address_completion_unregister_entry(GTK_ENTRY(page->forward_override_from_format));		
}

/** \brief  Save the prefs in page to folder.
 *
 *  If the folder is not the one  specified in page->item, then only those properties 
 *  that have the relevant 'apply to sub folders' button checked are saved
 */
static void templates_save_folder_prefs(FolderItem *folder, FolderItemTemplatesPage *page)
{
	FolderItemPrefs *prefs = folder->prefs;
	gboolean all = FALSE;

	if (folder->path == NULL)
		return;

	if (page->item == folder) 
		all = TRUE;

	cm_return_if_fail(prefs != NULL);

	/* save and check formats */

	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->new_msg_format_rec_checkbtn))) {
		
		prefs->compose_with_format =
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_compose_with_format));
		prefs->compose_override_from_format = pref_get_pref_from_entry(
				GTK_ENTRY(page->compose_override_from_format));
		prefs->compose_subject_format = pref_get_pref_from_entry(
				GTK_ENTRY(page->compose_subject_format));
		prefs->compose_body_format = pref_get_pref_from_textview(
				GTK_TEXT_VIEW(page->compose_body_format));
		quotefmt_check_new_msg_formats(prefs->compose_with_format,
						prefs->compose_override_from_format,
						prefs->compose_subject_format,
						prefs->compose_body_format);
	}

	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->reply_format_rec_checkbtn))) {
		prefs->reply_with_format =
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_reply_with_format));
		prefs->reply_quotemark = gtk_editable_get_chars(
				GTK_EDITABLE(page->reply_quotemark), 0, -1);
		prefs->reply_override_from_format = pref_get_pref_from_entry(
				GTK_ENTRY(page->reply_override_from_format));
		prefs->reply_body_format = pref_get_pref_from_textview(
				GTK_TEXT_VIEW(page->reply_body_format));
		quotefmt_check_reply_formats(prefs->reply_with_format,
										prefs->reply_override_from_format,
										prefs->reply_quotemark,
										prefs->reply_body_format);
	}

	if (all || gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->forward_format_rec_checkbtn))) {
		prefs->forward_with_format =
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_forward_with_format));
		prefs->forward_quotemark = gtk_editable_get_chars(
				GTK_EDITABLE(page->forward_quotemark), 0, -1);
		prefs->forward_override_from_format = pref_get_pref_from_entry(
				GTK_ENTRY(page->forward_override_from_format));
		prefs->forward_body_format = pref_get_pref_from_textview(
				GTK_TEXT_VIEW(page->forward_body_format));
		quotefmt_check_forward_formats(prefs->forward_with_format,
										prefs->forward_override_from_format,
										prefs->forward_quotemark,
										prefs->forward_body_format);
	}

	folder_item_prefs_save_config(folder);
}	

static gboolean templates_save_recurse_func(GNode *node, gpointer data)
{
	FolderItem *item = (FolderItem *) node->data;
	FolderItemTemplatesPage *page = (FolderItemTemplatesPage *) data;

	cm_return_val_if_fail(item != NULL, TRUE);
	cm_return_val_if_fail(page != NULL, TRUE);

	templates_save_folder_prefs(item, page);

	/* optimise by not continuing if none of the 'apply to sub folders'
	   check boxes are selected - and optimise the checking by only doing
	   it once */
	if ((node == page->item->node) &&
	    !(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->new_msg_format_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->reply_format_rec_checkbtn)) ||
	      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->forward_format_rec_checkbtn))))
		return TRUE;
	else 
		return FALSE;

	return FALSE;
}

static void prefs_folder_item_templates_save_func(PrefsPage *page_) 
{
	FolderItemTemplatesPage *page = (FolderItemTemplatesPage *) page_;

	g_node_traverse(page->item->node, G_PRE_ORDER, G_TRAVERSE_ALL,
			-1, templates_save_recurse_func, page);

}

static gint prefs_folder_item_chmod_mode(gchar *folder_chmod) 
{
	gint newmode = 0;
	gchar *tmp;

	if (folder_chmod) {
		newmode = strtol(folder_chmod, &tmp, 8);
		if (!(*(folder_chmod) && !(*tmp)))
			newmode = 0;
	}

	return newmode;
}

static void folder_color_set_dialog(GtkWidget *widget, gpointer data)
{
	FolderItemGeneralPage *page = (FolderItemGeneralPage *) data;
	gint rgbcolor;

	rgbcolor = colorsel_select_color_rgb(_("Pick color for folder"), 
					     page->folder_color);
	gtkut_set_widget_bgcolor_rgb(page->folder_color_btn, rgbcolor);
	page->folder_color = rgbcolor;
}

static void clean_cache_cb(GtkWidget *widget, gpointer data)
{
	FolderItemGeneralPage *page = (FolderItemGeneralPage *) data;
	FolderItem *item = page->item;
	gboolean was_open = FALSE;
	FolderView *folderview = NULL;

	if (alertpanel_full(_("Discard cache"), 
			    _("Do you really want to discard the local cached "
			      "data for this folder?"),
		 		 GTK_STOCK_CANCEL, _("+Discard"), NULL, FALSE,
				 NULL, ALERT_WARNING, G_ALERTDEFAULT)
		!= G_ALERTALTERNATE)
		return;
	
	if (mainwindow_get_mainwindow())
		folderview = mainwindow_get_mainwindow()->folderview;

	if (folderview && item->opened) {
		folderview_close_opened(folderview);
		was_open = TRUE;
	}
	folder_item_discard_cache(item);
	if (was_open)
		folderview_select(folderview,item);
}

#ifndef G_OS_WIN32
static regex_t *summary_compile_simplify_regexp(gchar *simplify_subject_regexp)
{
	int err;
	gchar buf[BUFFSIZE];
	regex_t *preg = NULL;

	preg = g_new0(regex_t, 1);

	err = string_match_precompile(simplify_subject_regexp, 
				      preg, REG_EXTENDED);
	if (err) {
		regerror(err, preg, buf, BUFFSIZE);
		g_free(preg);
		preg = NULL;
	}

	return preg;
}

static void folder_regexp_test_cb(GtkWidget *widget, gpointer data)
{
#if !GTK_CHECK_VERSION(3, 0, 0)
	static GdkColor red;
	static gboolean colors_initialised = FALSE;
#else
	static GdkColor red = { (guint32)0, (guint16)0xff, (guint16)0x70, (guint16)0x70 };
#endif
	static gchar buf[BUFFSIZE];
	FolderItemGeneralPage *page = (FolderItemGeneralPage *)data;
	gchar *test_string, *regexp;
	regex_t *preg;

	regexp = g_strdup(gtk_entry_get_text(GTK_ENTRY(page->entry_simplify_subject)));
	test_string = g_strdup(gtk_entry_get_text(GTK_ENTRY(page->entry_regexp_test_string)));

	if (!regexp || !regexp[0]) {
		gtk_widget_modify_base(page->entry_simplify_subject,
				GTK_STATE_NORMAL, NULL);
		if (test_string)
			gtk_entry_set_text(GTK_ENTRY(page->entry_regexp_test_result), test_string);
			
		g_free(test_string);
		g_free(regexp);
		return;
	}

	if (!test_string || !test_string[0]) {
		g_free(test_string);
		g_free(regexp);
		return;
	}

#if !GTK_CHECK_VERSION(3, 0, 0)
	if (!colors_initialised) {
		gdk_color_parse("#ff7070", &red);
		colors_initialised = gdk_colormap_alloc_color(
			gdk_colormap_get_system(), &red, FALSE, TRUE);
	}
#endif

	preg = summary_compile_simplify_regexp(regexp);
#if !GTK_CHECK_VERSION(3, 0, 0)
	if (colors_initialised)
		gtk_widget_modify_base(page->entry_simplify_subject,
				GTK_STATE_NORMAL, preg ? NULL : &red);
#endif

	if (preg != NULL) {
		string_remove_match(buf, BUFFSIZE, test_string, preg);

		gtk_entry_set_text(GTK_ENTRY(page->entry_regexp_test_result), buf);

		regfree(preg);
		g_free(preg);
	}

	g_free(test_string);
	g_free(regexp);
}

static gchar *folder_regexp_get_subject_example(void)
{
	MsgInfo *msginfo_selected;
	SummaryView *summaryview = NULL;

	if (!mainwindow_get_mainwindow())
		return NULL;
	summaryview = mainwindow_get_mainwindow()->summaryview;

	msginfo_selected = summary_get_selected_msg(summaryview);
	return msginfo_selected ? g_strdup(msginfo_selected->subject) : NULL;
}

static void folder_regexp_set_subject_example_cb(GtkWidget *widget, gpointer data)
{
	FolderItemGeneralPage *page = (FolderItemGeneralPage *)data;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_simplify_subject))) {
		gchar *subject = folder_regexp_get_subject_example();
		if (subject) {
			gtk_entry_set_text(GTK_ENTRY(page->entry_regexp_test_string), subject);
			g_free(subject);
		}
	}
}
#endif
static void register_general_page()
{
	static gchar *pfi_general_path[3];
	static FolderItemGeneralPage folder_item_general_page;

	pfi_general_path[0] = _("Folder");
	pfi_general_path[1] = _("General");
	pfi_general_path[2] = NULL;

        folder_item_general_page.page.path = pfi_general_path;
        folder_item_general_page.page.create_widget = prefs_folder_item_general_create_widget_func;
        folder_item_general_page.page.destroy_widget = prefs_folder_item_general_destroy_widget_func;
        folder_item_general_page.page.save_page = prefs_folder_item_general_save_func;
        
	prefs_folder_item_register_page((PrefsPage *) &folder_item_general_page);
}


static void register_compose_page(void)
{
	static gchar *pfi_compose_path[3];
	static FolderItemComposePage folder_item_compose_page;

	pfi_compose_path[0] = _("Folder");
	pfi_compose_path[1] = _("Compose");
	pfi_compose_path[2] = NULL;

        folder_item_compose_page.page.path = pfi_compose_path;
        folder_item_compose_page.page.create_widget = prefs_folder_item_compose_create_widget_func;
        folder_item_compose_page.page.destroy_widget = prefs_folder_item_compose_destroy_widget_func;
        folder_item_compose_page.page.save_page = prefs_folder_item_compose_save_func;
        
	prefs_folder_item_register_page((PrefsPage *) &folder_item_compose_page);
}

static void register_templates_page(void)
{
	static gchar *pfi_templates_path[3];
	static FolderItemTemplatesPage folder_item_templates_page;

	pfi_templates_path[0] = _("Folder");
	pfi_templates_path[1] = _("Templates");
	pfi_templates_path[2] = NULL;

        folder_item_templates_page.page.path = pfi_templates_path;
        folder_item_templates_page.page.create_widget = prefs_folder_item_templates_create_widget_func;
        folder_item_templates_page.page.destroy_widget = prefs_folder_item_templates_destroy_widget_func;
        folder_item_templates_page.page.save_page = prefs_folder_item_templates_save_func;
        
	prefs_folder_item_register_page((PrefsPage *) &folder_item_templates_page);
}

static GSList *prefs_pages = NULL;

static void prefs_folder_item_address_completion_start(GtkWindow * window)
{
	address_completion_start(GTK_WIDGET(window));
}

static void prefs_folder_item_address_completion_end(GtkWindow * window)
{
	address_completion_end(GTK_WIDGET(window));
}

void prefs_folder_item_open(FolderItem *item)
{
	gchar *id, *title;
	if (prefs_pages == NULL) {
		register_general_page();
		register_compose_page();
		register_templates_page();
	}

	if (item->path) {
		id = folder_item_get_identifier (item);
		can_save = TRUE;
	} else {
		id = g_strdup(item->name);
		can_save = FALSE;
	}
	title = g_strdup_printf (_("Properties for folder %s"), id);
	g_free (id);
	prefswindow_open(title, prefs_pages, item,
			&prefs_common.folderitemwin_width, &prefs_common.folderitemwin_height,
			prefs_folder_item_address_completion_start,
			prefs_folder_item_address_completion_end);

	g_free (title);
}

static void prefs_folder_item_register_page(PrefsPage *page)
{
	prefs_pages = g_slist_append(prefs_pages, page);
}
