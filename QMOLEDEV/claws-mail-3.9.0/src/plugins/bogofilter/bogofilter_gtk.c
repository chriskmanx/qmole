/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Colin Leroy <colin@colino.net> and 
 * the Claws Mail team
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
#include "gtk/gtkutils.h"

#include "common/claws.h"
#include "common/version.h"
#include "plugin.h"
#include "common/utils.h"
#include "prefs.h"
#include "folder.h"
#include "prefs_gtk.h"
#include "foldersel.h"
#include "statusbar.h"
#include "bogofilter.h"
#include "menu.h"
#include "addressbook.h"
#include "combobox.h"

struct BogofilterPage
{
	PrefsPage page;

	GtkWidget *process_emails;
	GtkWidget *receive_spam;
	GtkWidget *save_folder;
	GtkWidget *save_folder_select;
	GtkWidget *save_unsure;
	GtkWidget *save_unsure_folder;
	GtkWidget *save_unsure_folder_select;
	GtkWidget *insert_header;
	GtkWidget *max_size;
	GtkWidget *bogopath;
	GtkWidget *whitelist_ab;
	GtkWidget *whitelist_ab_folder_combo;
	GtkWidget *learn_from_whitelist_chkbtn;
	GtkWidget *mark_as_read;
};

static void foldersel_cb(GtkWidget *widget, gpointer data)
{
	GtkWidget *entry = (GtkWidget *) data;
	FolderItem *item;
	gchar *item_id;
	gint newpos = 0;
	
	item = foldersel_folder_sel(NULL, FOLDER_SEL_MOVE, NULL, FALSE);
	if (item && (item_id = folder_item_get_identifier(item)) != NULL) {
		gtk_editable_delete_text(GTK_EDITABLE(entry), 0, -1);
		gtk_editable_insert_text(GTK_EDITABLE(entry), item_id, strlen(item_id), &newpos);
		g_free(item_id);
	}
}

#ifndef USE_NEW_ADDRBOOK
static void bogofilter_whitelist_ab_select_cb(GtkWidget *widget, gpointer data)
{
	struct BogofilterPage *page = (struct BogofilterPage *) data;
	const gchar *folderpath = NULL;
	gchar *new_path = NULL;

	folderpath = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((page->whitelist_ab_folder_combo)))));
	new_path = addressbook_folder_selection(folderpath);
	if (new_path) {
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((page->whitelist_ab_folder_combo)))), new_path);
		g_free(new_path);
	} 
}
#endif

static void bogofilter_create_widget_func(PrefsPage * _page,
					    GtkWindow * window,
					    gpointer data)
{
	struct BogofilterPage *page = (struct BogofilterPage *) _page;
	BogofilterConfig *config;

	GtkWidget *vbox1, *vbox2;
	GtkWidget *hbox_max_size;
	GtkWidget *hbox_process_emails, *hbox_save_spam, *hbox_save_unsure;
	GtkWidget *hbox_bogopath, *hbox_whitelist;
	GtkWidget *hbox_mark_as_read;

	GtkWidget *max_size_label;
	GtkAdjustment *max_size_spinbtn_adj;
	GtkWidget *max_size_spinbtn;
	GtkWidget *max_size_kb_label;

	GtkWidget *process_emails_checkbtn;

	GtkWidget *save_spam_checkbtn;
	GtkWidget *save_spam_folder_entry;
	GtkWidget *save_spam_folder_select;

	GtkWidget *save_unsure_checkbtn;
	GtkWidget *save_unsure_folder_entry;
	GtkWidget *save_unsure_folder_select;

	GtkWidget *insert_header_checkbtn;
	GtkWidget *whitelist_ab_checkbtn;
	GtkWidget *learn_from_whitelist_chkbtn;
	GtkWidget *bogopath_label;
	GtkWidget *bogopath_entry;

	GtkWidget *mark_as_read_checkbtn;

	GtkWidget *whitelist_ab_folder_combo;
	GtkWidget *whitelist_ab_select_btn;

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);

	vbox2 = gtk_vbox_new (FALSE, 4);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (vbox1), vbox2, FALSE, FALSE, 0);

	hbox_process_emails = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_process_emails);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox_process_emails, TRUE, TRUE, 0);

	process_emails_checkbtn = gtk_check_button_new_with_label(
			_("Process messages on receiving"));
	gtk_widget_show(process_emails_checkbtn);
	gtk_box_pack_start(GTK_BOX(hbox_process_emails), process_emails_checkbtn, TRUE, TRUE, 0);

	hbox_max_size = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_max_size);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox_max_size, TRUE, TRUE, 0);

	max_size_label = gtk_label_new(_("Maximum size"));
	gtk_widget_show(max_size_label);
	gtk_box_pack_start(GTK_BOX(hbox_max_size), max_size_label, FALSE, FALSE, 0);

	max_size_spinbtn_adj = GTK_ADJUSTMENT(gtk_adjustment_new(250, 0, 10000, 10, 10, 0));
	max_size_spinbtn = gtk_spin_button_new(GTK_ADJUSTMENT(max_size_spinbtn_adj), 1, 0);
	gtk_widget_show(max_size_spinbtn);
	gtk_box_pack_start(GTK_BOX(hbox_max_size), max_size_spinbtn, FALSE, FALSE, 0);
	CLAWS_SET_TIP(max_size_spinbtn,
			_("Messages larger than this will not be checked"));
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(max_size_spinbtn), TRUE);

	max_size_kb_label = gtk_label_new(_("KB"));
	gtk_widget_show(max_size_kb_label);
	gtk_box_pack_start(GTK_BOX(hbox_max_size), max_size_kb_label, FALSE, FALSE, 0);

	hbox_save_spam = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_save_spam);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox_save_spam, TRUE, TRUE, 0);

	save_spam_checkbtn = gtk_check_button_new_with_label(_("Save spam in"));
	gtk_widget_show(save_spam_checkbtn);
	gtk_box_pack_start(GTK_BOX(hbox_save_spam), save_spam_checkbtn, FALSE, FALSE, 0);

	save_spam_folder_entry = gtk_entry_new();
	gtk_widget_show (save_spam_folder_entry);
	gtk_box_pack_start (GTK_BOX (hbox_save_spam), save_spam_folder_entry, TRUE, TRUE, 0);
	CLAWS_SET_TIP(save_spam_folder_entry,
			_("Folder for storing identified spam. Leave empty to use the trash folder."));

	save_spam_folder_select = gtkut_get_browse_directory_btn(_("_Browse"));
	gtk_widget_show (save_spam_folder_select);
	gtk_box_pack_start (GTK_BOX (hbox_save_spam), save_spam_folder_select, FALSE, FALSE, 0);
	CLAWS_SET_TIP(save_spam_folder_select,
			_("Click this button to select a folder for storing spam"));

	hbox_save_unsure = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_save_unsure);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox_save_unsure, TRUE, TRUE, 0);

	save_unsure_checkbtn = gtk_check_button_new_with_label(_("When unsure, move to"));
	gtk_widget_show(save_unsure_checkbtn);
	gtk_box_pack_start(GTK_BOX(hbox_save_unsure), save_unsure_checkbtn, FALSE, FALSE, 0);

	save_unsure_folder_entry = gtk_entry_new();
	gtk_widget_show (save_unsure_folder_entry);
	gtk_box_pack_start (GTK_BOX (hbox_save_unsure), save_unsure_folder_entry, TRUE, TRUE, 0);
	CLAWS_SET_TIP(save_unsure_folder_entry,
			_("Folder for storing mail for which spam status is Unsure. Leave empty to use the Inbox folder."));

	save_unsure_folder_select = gtkut_get_browse_directory_btn(_("_Browse"));
	gtk_widget_show (save_unsure_folder_select);
	gtk_box_pack_start (GTK_BOX (hbox_save_unsure), save_unsure_folder_select, FALSE, FALSE, 0);
	CLAWS_SET_TIP(save_unsure_folder_select,
			_("Click this button to select a folder for storing Unsure mails."));

	insert_header_checkbtn = gtk_check_button_new_with_label(_("Insert X-Bogosity header"));
	gtk_widget_show(insert_header_checkbtn);
	gtk_box_pack_start(GTK_BOX(vbox2), insert_header_checkbtn, FALSE, FALSE, 0);
	CLAWS_SET_TIP(insert_header_checkbtn,
			_("Only done for messages in MH folders"));

	hbox_whitelist = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_whitelist);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox_whitelist, TRUE, TRUE, 0);

	whitelist_ab_checkbtn = gtk_check_button_new_with_label(_("Whitelist senders found in address book/folder"));
	gtk_widget_show(whitelist_ab_checkbtn);
	gtk_box_pack_start(GTK_BOX(hbox_whitelist), whitelist_ab_checkbtn, FALSE, FALSE, 0);
	CLAWS_SET_TIP(whitelist_ab_checkbtn,
			_("Messages coming from your address book contacts will be received in the normal folder even if detected as spam"));

	whitelist_ab_folder_combo = combobox_text_new(TRUE, _("Any"), NULL);
	gtk_widget_set_size_request(whitelist_ab_folder_combo, 100, -1);
	gtk_box_pack_start (GTK_BOX (hbox_whitelist), whitelist_ab_folder_combo, TRUE, TRUE, 0);

	whitelist_ab_select_btn = gtk_button_new_with_label(_("Select ..."));
	gtk_widget_show (whitelist_ab_select_btn);
	gtk_box_pack_start (GTK_BOX (hbox_whitelist), whitelist_ab_select_btn, FALSE, FALSE, 0);
	CLAWS_SET_TIP(whitelist_ab_select_btn,
			_("Click this button to select a book or folder in the address book"));

	learn_from_whitelist_chkbtn = gtk_check_button_new_with_label(_("Learn whitelisted emails as ham"));
	CLAWS_SET_TIP(learn_from_whitelist_chkbtn,
			_("If Bogofilter thought an email was spam or unsure, but it was whitelisted, "
			  "learn it as ham."));
	gtk_widget_show(learn_from_whitelist_chkbtn);
	gtk_box_pack_start (GTK_BOX (vbox2), learn_from_whitelist_chkbtn, TRUE, TRUE, 0);

	hbox_bogopath = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_bogopath);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox_bogopath, FALSE, FALSE, 0);

	bogopath_label = gtk_label_new(_("Bogofilter call"));
	gtk_widget_show(bogopath_label);
	gtk_box_pack_start(GTK_BOX(hbox_bogopath), bogopath_label, FALSE, FALSE, 0);

	bogopath_entry = gtk_entry_new();
	gtk_widget_show(bogopath_entry);
	gtk_box_pack_start(GTK_BOX(hbox_bogopath), bogopath_entry, FALSE, FALSE, 0);
	CLAWS_SET_TIP(bogopath_entry,
			_("Path to bogofilter executable"));

	hbox_mark_as_read = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_mark_as_read);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox_mark_as_read, TRUE, TRUE, 0);

	mark_as_read_checkbtn = gtk_check_button_new_with_label(_("Mark spam as read"));
	gtk_widget_show(mark_as_read_checkbtn);
	gtk_box_pack_start(GTK_BOX(hbox_mark_as_read), mark_as_read_checkbtn, FALSE, FALSE, 0);

	SET_TOGGLE_SENSITIVITY(save_spam_checkbtn, save_spam_folder_entry);
	SET_TOGGLE_SENSITIVITY(save_spam_checkbtn, save_spam_folder_select);
	SET_TOGGLE_SENSITIVITY(save_unsure_checkbtn, save_unsure_folder_entry);
	SET_TOGGLE_SENSITIVITY(save_unsure_checkbtn, save_unsure_folder_select);
	SET_TOGGLE_SENSITIVITY(whitelist_ab_checkbtn, whitelist_ab_folder_combo);
#ifndef USE_NEW_ADDRBOOK
	SET_TOGGLE_SENSITIVITY(whitelist_ab_checkbtn, whitelist_ab_select_btn);
#endif
	SET_TOGGLE_SENSITIVITY(whitelist_ab_checkbtn, learn_from_whitelist_chkbtn);
	SET_TOGGLE_SENSITIVITY(save_spam_checkbtn, mark_as_read_checkbtn);

	config = bogofilter_get_config();

	g_signal_connect(G_OBJECT(save_spam_folder_select), "clicked",
			G_CALLBACK(foldersel_cb), save_spam_folder_entry);
	g_signal_connect(G_OBJECT(save_unsure_folder_select), "clicked",
			G_CALLBACK(foldersel_cb), save_unsure_folder_entry);
#ifndef USE_NEW_ADDRBOOK
	g_signal_connect(G_OBJECT (whitelist_ab_select_btn), "clicked",
			 G_CALLBACK(bogofilter_whitelist_ab_select_cb), page);
#else
	gtk_widget_set_sensitive(GTK_WIDGET(whitelist_ab_select_btn), FALSE);
#endif
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(max_size_spinbtn), (float) config->max_size);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(process_emails_checkbtn), config->process_emails);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(save_spam_checkbtn), config->receive_spam);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(save_unsure_checkbtn), config->save_unsure);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(insert_header_checkbtn), config->insert_header);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(whitelist_ab_checkbtn), config->whitelist_ab);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(learn_from_whitelist_chkbtn), config->learn_from_whitelist);
	if (config->whitelist_ab_folder != NULL) {
		/* translate "Any" (stored UNtranslated) */
		if (strcasecmp(config->whitelist_ab_folder, "Any") == 0)
			gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((whitelist_ab_folder_combo)))),
					config->whitelist_ab_folder);
		else
		/* backward compatibility (when translated "Any" was stored) */
		if (g_utf8_collate(config->whitelist_ab_folder, _("Any")) == 0)
			gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((whitelist_ab_folder_combo)))),
					config->whitelist_ab_folder);
		else
			gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((whitelist_ab_folder_combo)))),
					config->whitelist_ab_folder);
	}
	if (config->save_folder != NULL)
		gtk_entry_set_text(GTK_ENTRY(save_spam_folder_entry), config->save_folder);
	if (config->save_unsure_folder != NULL)
		gtk_entry_set_text(GTK_ENTRY(save_unsure_folder_entry), config->save_unsure_folder);
	if (config->bogopath != NULL)
		gtk_entry_set_text(GTK_ENTRY(bogopath_entry), config->bogopath);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(mark_as_read_checkbtn), config->mark_as_read);

	page->max_size = max_size_spinbtn;
	page->process_emails = process_emails_checkbtn;

	page->receive_spam = save_spam_checkbtn;
	page->save_folder = save_spam_folder_entry;
	page->save_folder_select = save_spam_folder_select;

	page->save_unsure = save_unsure_checkbtn;
	page->save_unsure_folder = save_unsure_folder_entry;
	page->save_unsure_folder_select = save_unsure_folder_select;

	page->insert_header = insert_header_checkbtn;
	page->whitelist_ab = whitelist_ab_checkbtn;
	page->whitelist_ab_folder_combo = whitelist_ab_folder_combo;
	page->learn_from_whitelist_chkbtn = learn_from_whitelist_chkbtn;
	page->bogopath = bogopath_entry;

	page->mark_as_read = mark_as_read_checkbtn;

	page->page.widget = vbox1;
}

static void bogofilter_destroy_widget_func(PrefsPage *_page)
{
	debug_print("Destroying Bogofilter widget\n");
}

static void bogofilter_save_func(PrefsPage *_page)
{
	struct BogofilterPage *page = (struct BogofilterPage *) _page;
	BogofilterConfig *config;

	debug_print("Saving Bogofilter Page\n");

	config = bogofilter_get_config();

	/* process_emails */
	config->process_emails = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->process_emails));

	/* receive_spam */
	config->receive_spam = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->receive_spam));

	/* save_folder */
	g_free(config->save_folder);
	config->save_folder = gtk_editable_get_chars(GTK_EDITABLE(page->save_folder), 0, -1);

	/* save_unsure */
	config->save_unsure = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->save_unsure));

	/* save_unsure_folder */
	g_free(config->save_unsure_folder);
	config->save_unsure_folder = gtk_editable_get_chars(GTK_EDITABLE(page->save_unsure_folder), 0, -1);

	/* insert_header */
	config->insert_header = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->insert_header));

	/* whitelist_ab */
	config->whitelist_ab = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->whitelist_ab));
	g_free(config->whitelist_ab_folder);
	config->whitelist_ab_folder = gtk_editable_get_chars(
				GTK_EDITABLE(gtk_bin_get_child(GTK_BIN((page->whitelist_ab_folder_combo)))), 0, -1);
	/* store UNtranslated "Any" */
	if (g_utf8_collate(config->whitelist_ab_folder, _("Any")) == 0) {
		g_free(config->whitelist_ab_folder);
		config->whitelist_ab_folder = g_strdup("Any");
	}

	/* learn_from_whitelist_chkbtn */
	config->learn_from_whitelist = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->learn_from_whitelist_chkbtn));

	/* bogopath */
	g_free(config->bogopath);
	config->bogopath = gtk_editable_get_chars(GTK_EDITABLE(page->bogopath), 0, -1);

	/* max_size */
	config->max_size = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(page->max_size));

	/* mark_as_read */
	config->mark_as_read = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->mark_as_read));

	if (config->process_emails) {
		bogofilter_register_hook();
	} else {
		bogofilter_unregister_hook();
	}

	procmsg_register_spam_learner(bogofilter_learn);
	procmsg_spam_set_folder(config->save_folder, bogofilter_get_spam_folder);

	bogofilter_save_config();
}

typedef struct _BogoCbData {
	gchar *message;
	gint total;
	gint done;
} BogoCbData;

static gboolean gtk_message_callback(gpointer data)
{
	BogoCbData *cbdata = (BogoCbData *)data;

	if (cbdata->message)
		statusbar_print_all("%s", cbdata->message);
	else if (cbdata->total == 0) {
		statusbar_pop_all();
	}
	if (cbdata->total && cbdata->done)
		statusbar_progress_all(cbdata->done, cbdata->total, 10);
	else
		statusbar_progress_all(0,0,0);
	g_free(cbdata->message);
	g_free(cbdata);
	GTK_EVENTS_FLUSH();
	return FALSE;
}

static void gtk_safe_message_callback(gchar *message, gint total, gint done, gboolean thread_safe)
{
	BogoCbData *cbdata = g_new0(BogoCbData, 1);
	if (message)
		cbdata->message = g_strdup(message);
	cbdata->total = total;
	cbdata->done = done;
	if (thread_safe)
		g_timeout_add(0, gtk_message_callback, cbdata);
	else
		gtk_message_callback(cbdata);
}

static struct BogofilterPage bogofilter_page;

gint bogofilter_gtk_init(void)
{
	static gchar *path[3];

	path[0] = _("Plugins");
	path[1] = _("Bogofilter");
	path[2] = NULL;

	bogofilter_page.page.path = path;
	bogofilter_page.page.create_widget = bogofilter_create_widget_func;
	bogofilter_page.page.destroy_widget = bogofilter_destroy_widget_func;
	bogofilter_page.page.save_page = bogofilter_save_func;
	bogofilter_page.page.weight = 35.0;

	prefs_gtk_register_page((PrefsPage *) &bogofilter_page);
	bogofilter_set_message_callback(gtk_safe_message_callback);

	debug_print("Bogofilter GTK plugin loaded\n");
	return 0;	
}

void bogofilter_gtk_done(void)
{
        prefs_gtk_unregister_page((PrefsPage *) &bogofilter_page);
}
