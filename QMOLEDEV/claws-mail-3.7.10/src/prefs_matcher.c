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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "gtkcmoptionmenu.h"
#include "main.h"
#include "prefs_gtk.h"
#include "prefs_matcher.h"
#include "prefs_common.h"
#include "mainwindow.h"
#include "foldersel.h"
#include "manage_window.h"
#include "inc.h"
#include "matcher.h"
#include "utils.h"
#include "gtkutils.h"
#include "alertpanel.h"
#include "folder.h"
#include "description_window.h"
#include "combobox.h"

#include "matcher_parser.h"
#include "colorlabel.h"
#include "tags.h"

static void prefs_matcher_addressbook_select(void);
static void prefs_matcher_test_info(GtkWidget *widget, GtkWidget *parent);

enum {
	PREFS_MATCHER_COND,
	PREFS_MATCHER_COND_VALID,
	N_PREFS_MATCHER_COLUMNS
};

/*!
 *\brief	UI data for matcher dialog
 */
static struct Matcher {
	GtkWidget *window;

	GtkWidget *ok_btn;

	GtkWidget *match_combo;
	GtkWidget *header_addr_combo;
	GtkWidget *bool_op_combo;
	GtkWidget *criteria_label2;
	GtkWidget *criteria_combo;
	GtkWidget *criteria_combo2;
	GtkWidget *match_combo2;
	GtkWidget *match_label;
	GtkWidget *match_label2;
	GtkWidget *headers_combo;
	GtkWidget *upper_filler;
	GtkWidget *lower_filler;

	GtkWidget *header_entry;
	GtkWidget *header_addr_entry;
	GtkWidget *string_entry;
	GtkWidget *numeric_entry;
	GtkWidget *numeric_label;
	GtkWidget *addressbook_folder_combo;
	GtkWidget *case_checkbtn;
#ifndef G_OS_WIN32
	GtkWidget *regexp_checkbtn;
#endif
	GtkWidget *color_optmenu;

	GtkWidget *test_btn;
	GtkWidget *addressbook_select_btn;

	GtkTreeModel *model_age;
	GtkTreeModel *model_age_units;
	GtkTreeModel *model_contain;
	GtkTreeModel *model_found;
	GtkTreeModel *model_flags;
	GtkTreeModel *model_headers;
	GtkTreeModel *model_partial;
	GtkTreeModel *model_phrase;
	GtkTreeModel *model_score;
	GtkTreeModel *model_set;
	GtkTreeModel *model_size;
	GtkTreeModel *model_size_units;
	GtkTreeModel *model_tags;
	GtkTreeModel *model_test;
	GtkTreeModel *model_thread;
	
	GtkWidget *cond_list_view;

	gint selected_criteria; /*!< selected criteria in combobox */ 
} matcher;

/*!
 *\brief	Conditions with a negate counterpart (like unread and ~unread)
 *		have the same CRITERIA_XXX id). I.e. both unread and ~unread
 *		have criteria id CRITERIA_UNREAD. This id is passed as the
 *		first parameter to #matcherprop_new and #matcherprop_unquote_new.
 */		
enum {
	CRITERIA_ALL = 0,

	CRITERIA_SUBJECT = 1,
	CRITERIA_FROM = 2,
	CRITERIA_TO = 3,
	CRITERIA_CC = 4,
	CRITERIA_TO_OR_CC = 5,
	CRITERIA_NEWSGROUPS = 6,
	CRITERIA_INREPLYTO = 7,
	CRITERIA_REFERENCES = 8,
	CRITERIA_AGE_GREATER = 9,
	CRITERIA_AGE_LOWER = 10,
	CRITERIA_HEADER = 11,
	CRITERIA_HEADERS_PART = 12,
	CRITERIA_BODY_PART = 13,
	CRITERIA_MESSAGE = 14,

	CRITERIA_UNREAD = 15,
	CRITERIA_NEW = 16,
	CRITERIA_MARKED = 17,
	CRITERIA_DELETED = 18,
	CRITERIA_REPLIED = 19,
	CRITERIA_FORWARDED = 20,
	CRITERIA_LOCKED = 21,
	CRITERIA_SPAM = 22,
	CRITERIA_COLORLABEL = 23,
	CRITERIA_IGNORE_THREAD = 24,
	CRITERIA_WATCH_THREAD = 25,

	CRITERIA_SCORE_GREATER = 26,
	CRITERIA_SCORE_LOWER = 27,
	CRITERIA_SCORE_EQUAL = 28,

	CRITERIA_TEST = 29,

	CRITERIA_SIZE_GREATER = 30,
	CRITERIA_SIZE_SMALLER = 31,
	CRITERIA_SIZE_EQUAL   = 32,
	
	CRITERIA_PARTIAL = 33,

	CRITERIA_FOUND_IN_ADDRESSBOOK = 34,
	
	CRITERIA_TAG = 35,
	CRITERIA_TAGGED = 36,

	CRITERIA_HAS_ATTACHMENT = 37,
	CRITERIA_SIGNED = 38
};

enum {
	MATCH_ALL	= 0,
	MATCH_HEADER	= 1,
	MATCH_AGE	= 2,
	MATCH_PHRASE	= 3,
	MATCH_FLAG	= 4,
	MATCH_LABEL	= 5,
	MATCH_THREAD	= 6,
	MATCH_SCORE	= 7,
	MATCH_SIZE	= 8,
	MATCH_PARTIAL	= 9,
	MATCH_ABOOK	= 10,
	MATCH_TAGS	= 11,
	MATCH_TEST	= 12
};

enum {
	AGE_DAYS  = 0,
	AGE_WEEKS = 1
};

enum {
	SIZE_UNIT_BYTES  = 0,
	SIZE_UNIT_KBYTES = 1,
	SIZE_UNIT_MBYTES = 2
};

#define MB_SIZE 0x100000
#define KB_SIZE 0x000400

enum {
	THREAD_IGNORED = 0,
	THREAD_NOT_IGNORED = 1,
	THREAD_WATCHED = 2,
	THREAD_NOT_WATCHED = 3
};

/*!
 *\brief	Contains predicate	
 */
enum {
	PREDICATE_CONTAINS = 0,
	PREDICATE_DOES_NOT_CONTAIN = 1
};

/*!
 *\brief	Enabled predicate
 */
enum {
	PREDICATE_FLAG_ENABLED = 0,
	PREDICATE_FLAG_DISABLED = 1
};

/*!
 *\brief	Hooks
 */
static PrefsMatcherSignal *matchers_callback;

/* widget creating functions */
static void prefs_matcher_create	(void);

static void prefs_matcher_set_dialog	(MatcherList *matchers);
static void prefs_matcher_list_view_set_row	(GtkTreeIter *row, 
						 MatcherProp *prop);

/* callback functions */

static void prefs_matcher_register_cb	(void);
static void prefs_matcher_substitute_cb	(void);
static void prefs_matcher_delete_cb	(void);
static void prefs_matcher_up		(void);
static void prefs_matcher_down		(void);
static gboolean prefs_matcher_key_pressed(GtkWidget	*widget,
					 GdkEventKey	*event,
					 gpointer	 data);
static void prefs_matcher_ok		(void);
static void prefs_matcher_cancel	(void);
static gint prefs_matcher_deleted	(GtkWidget *widget, GdkEventAny *event,
					 gpointer data);
static void prefs_matcher_criteria_select	(GtkWidget *widget,
						 gpointer   user_data);
static void prefs_matcher_second_criteria_sel	(GtkWidget *widget,
						 gpointer   user_data);
static void prefs_matcher_set_model		(GtkWidget *wiget,
						 GtkTreeModel *model);
static MatcherList *prefs_matcher_get_list	(void);

static GtkListStore* prefs_matcher_create_data_store	(void);

static void prefs_matcher_list_view_insert_matcher	(GtkWidget *list_view,
							 GtkTreeIter *row_iter,
							 const gchar *matcher,
							 gboolean is_valid);

static GtkWidget *prefs_matcher_list_view_create	(void);

static void prefs_matcher_create_list_view_columns	(GtkWidget *list_view);

static gboolean prefs_matcher_selected			(GtkTreeSelection *selector,
							 GtkTreeModel *model, 
							 GtkTreePath *path,
							 gboolean currently_selected,
							 gpointer data);

static int header_name_to_crit(const gchar *header)
{
	if (header == NULL)
		return CRITERIA_HEADER;

	if (!strcasecmp(header, "Subject"))
		return CRITERIA_SUBJECT;
	if (!strcasecmp(header, "From"))
		return CRITERIA_FROM;
	if (!strcasecmp(header, "To"))
		return CRITERIA_TO;
	if (!strcasecmp(header, "Cc"))
		return CRITERIA_CC;
	if (!strcasecmp(header, "To or Cc"))
		return CRITERIA_TO_OR_CC;
	if (!strcasecmp(header, "In-Reply-To"))
		return CRITERIA_INREPLYTO;
	if (!strcasecmp(header, "Newsgroups"))
		return CRITERIA_NEWSGROUPS;
	if (!strcasecmp(header, "References"))
		return CRITERIA_REFERENCES;

	return CRITERIA_HEADER;
}

static void prefs_matcher_models_create(void)
{
	GtkListStore *store;
	GtkTreeIter iter;
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("more than"), CRITERIA_AGE_GREATER);
	COMBOBOX_ADD(store, _("less than"), CRITERIA_AGE_LOWER);
	matcher.model_age = GTK_TREE_MODEL(store);

	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("days"), AGE_DAYS);
	COMBOBOX_ADD(store, _("weeks"), AGE_WEEKS);
	matcher.model_age_units = GTK_TREE_MODEL(store);

	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("higher than"), CRITERIA_SCORE_GREATER);
	COMBOBOX_ADD(store, _("lower than"), CRITERIA_SCORE_LOWER);
	COMBOBOX_ADD(store, _("exactly"), CRITERIA_SCORE_EQUAL);
	matcher.model_score = GTK_TREE_MODEL(store);
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("greater than"), CRITERIA_SIZE_GREATER);
	COMBOBOX_ADD(store, _("smaller than"), CRITERIA_SIZE_SMALLER);
	COMBOBOX_ADD(store, _("exactly"), CRITERIA_SIZE_EQUAL);
	matcher.model_size = GTK_TREE_MODEL(store);

	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("bytes"), SIZE_UNIT_BYTES);
	COMBOBOX_ADD(store, _("kilobytes"), SIZE_UNIT_KBYTES);
	COMBOBOX_ADD(store, _("megabytes"), SIZE_UNIT_MBYTES);
	matcher.model_size_units = GTK_TREE_MODEL(store);
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("contains"), 0);
	COMBOBOX_ADD(store, _("doesn't contain"), 0);
	matcher.model_contain = GTK_TREE_MODEL(store);
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, "Subject", CRITERIA_SUBJECT);
	COMBOBOX_ADD(store, "From", CRITERIA_FROM);
	COMBOBOX_ADD(store, "To", CRITERIA_TO);
	COMBOBOX_ADD(store, "Cc", CRITERIA_CC);
	COMBOBOX_ADD(store, "To or Cc", CRITERIA_TO_OR_CC);
	COMBOBOX_ADD(store, "In-Reply-To", CRITERIA_INREPLYTO);
	COMBOBOX_ADD(store, "Newsgroups", CRITERIA_NEWSGROUPS);
	COMBOBOX_ADD(store, "References", CRITERIA_REFERENCES);
	COMBOBOX_ADD(store, "Sender", CRITERIA_HEADER);
	COMBOBOX_ADD(store, "X-ML-Name", CRITERIA_HEADER);
	COMBOBOX_ADD(store, "X-List", CRITERIA_HEADER);
	COMBOBOX_ADD(store, "X-Sequence", CRITERIA_HEADER);
	COMBOBOX_ADD(store, "X-Mailer", CRITERIA_HEADER);
	COMBOBOX_ADD(store, "X-BeenThere", CRITERIA_HEADER);
	COMBOBOX_ADD(store, "List-Post", CRITERIA_HEADER);
	COMBOBOX_ADD(store, "List-Id", CRITERIA_HEADER);
	matcher.model_headers = GTK_TREE_MODEL(store);
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("headers part"), CRITERIA_HEADERS_PART);
	COMBOBOX_ADD(store, _("body part"), CRITERIA_BODY_PART);
	COMBOBOX_ADD(store, _("whole message"), CRITERIA_MESSAGE);
	matcher.model_phrase = GTK_TREE_MODEL(store);
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("Unread"), CRITERIA_UNREAD);
	COMBOBOX_ADD(store, _("New"), CRITERIA_NEW);
	COMBOBOX_ADD(store, _("Marked"), CRITERIA_MARKED);
	COMBOBOX_ADD(store, _("Deleted"), CRITERIA_DELETED);
	COMBOBOX_ADD(store, _("Replied"), CRITERIA_REPLIED);
	COMBOBOX_ADD(store, _("Forwarded"), CRITERIA_FORWARDED);
	COMBOBOX_ADD(store, _("Locked"), CRITERIA_LOCKED);
	COMBOBOX_ADD(store, _("Spam"), CRITERIA_SPAM);
	COMBOBOX_ADD(store, _("Has attachment"), CRITERIA_HAS_ATTACHMENT);
	COMBOBOX_ADD(store, _("Signed"), CRITERIA_SIGNED);
	matcher.model_flags = GTK_TREE_MODEL(store);
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("set"), 0);
	COMBOBOX_ADD(store, _("not set"), 1);
	matcher.model_set = GTK_TREE_MODEL(store);

	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("yes"), CRITERIA_PARTIAL);
	COMBOBOX_ADD(store, _("no"), CRITERIA_PARTIAL);
	matcher.model_partial = GTK_TREE_MODEL(store);

	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("Any tags"), CRITERIA_TAGGED);
	COMBOBOX_ADD(store, _("Specific tag"), CRITERIA_TAG);
	matcher.model_tags = GTK_TREE_MODEL(store);

	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("ignored"), CRITERIA_IGNORE_THREAD);
	COMBOBOX_ADD(store, _("not ignored"), CRITERIA_IGNORE_THREAD);
	COMBOBOX_ADD(store, _("watched"), CRITERIA_WATCH_THREAD);
	COMBOBOX_ADD(store, _("not watched"), CRITERIA_WATCH_THREAD);
	matcher.model_thread = GTK_TREE_MODEL(store);
	
	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("found"), 0);
	COMBOBOX_ADD(store, _("not found"), 1);
	matcher.model_found = GTK_TREE_MODEL(store);

	store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	COMBOBOX_ADD(store, _("0 (Passed)"), 0);
	COMBOBOX_ADD(store, _("non-0 (Failed)"), 1);
	matcher.model_test = GTK_TREE_MODEL(store);
}

/*!
 *\brief	Opens the matcher dialog with a list of conditions
 *
 *\param	matchers List of conditions
 *\param	cb Callback
 *
 */
void prefs_matcher_open(MatcherList *matchers, PrefsMatcherSignal *cb)
{
	inc_lock();

	if (!matcher.window) {
		prefs_matcher_models_create();
		prefs_matcher_create();
	} else {
		/* update color label menu */
		gtk_cmoption_menu_set_menu(GTK_CMOPTION_MENU(matcher.color_optmenu),
				colorlabel_create_color_menu());
	}

	manage_window_set_transient(GTK_WINDOW(matcher.window));
	gtk_widget_grab_focus(matcher.ok_btn);

	matchers_callback = cb;

	prefs_matcher_set_dialog(matchers);

	gtk_widget_show(matcher.window);
	gtk_window_set_modal(GTK_WINDOW(matcher.window), TRUE);
}

/*!
 *\brief	Save Gtk object size to prefs dataset
 */
static void prefs_matcher_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.matcherwin_width = allocation->width;
	prefs_common.matcherwin_height = allocation->height;
}

/*!
 *\brief	Create the matcher dialog
 */
static void prefs_matcher_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *confirm_area;

	GtkWidget *vbox1;
	GtkWidget *frame;
	GtkWidget *table;
	GtkWidget *upper_hbox;
	GtkWidget *lower_hbox;
	GtkWidget *match_hbox;
	GtkWidget *criteria_combo;
	GtkWidget *criteria_label;
	GtkWidget *match_label;
	GtkWidget *criteria_label2;
	GtkWidget *headers_combo;
	GtkWidget *match_combo2;
	GtkWidget *match_label2;

	GtkWidget *hbox;
	GtkWidget *upper_filler;
	GtkWidget *lower_filler;
	
	GtkWidget *criteria_combo2;
	GtkWidget *header_entry;
	GtkWidget *header_addr_combo;
	GtkWidget *header_addr_entry;
	GtkWidget *string_entry;
	GtkWidget *addressbook_folder_combo;
	GtkWidget *match_combo;
	GtkWidget *bool_op_combo;
	GtkWidget *bool_op_label;

	GtkWidget *numeric_hbox;
	GtkWidget *numeric_entry;
	GtkWidget *numeric_label;
	
#ifndef G_OS_WIN32
	GtkWidget *regexp_checkbtn;
#endif
	GtkWidget *case_checkbtn;

	GtkWidget *reg_hbox;
	GtkWidget *btn_hbox;
	GtkWidget *arrow;
	GtkWidget *reg_btn;
	GtkWidget *subst_btn;
	GtkWidget *del_btn;

	GtkWidget *cond_hbox;
	GtkWidget *cond_scrolledwin;
	GtkWidget *cond_list_view;

	GtkWidget *btn_vbox;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	GtkWidget *test_btn;
	GtkWidget *addressbook_select_btn;

	GtkWidget *color_optmenu;

	static GdkGeometry geometry;
	GtkSizeGroup *size_group;
	GtkListStore *store;
	GtkTreeIter iter;

	debug_print("Creating matcher configuration window...\n");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_matcher");
	gtk_container_set_border_width(GTK_CONTAINER(window), 4);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);

	vbox = gtk_vbox_new(FALSE, 6);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	gtkut_stock_button_set_create(&confirm_area, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK, NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_btn);

	gtk_window_set_title(GTK_WINDOW(window),
			     _("Condition configuration"));
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(prefs_matcher_deleted), NULL);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(prefs_matcher_size_allocate_cb), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(prefs_matcher_key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(prefs_matcher_ok), NULL);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(prefs_matcher_cancel), NULL);

	vbox1 = gtk_vbox_new(FALSE, VSPACING);
	gtk_box_pack_start(GTK_BOX(vbox), vbox1, TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER (vbox1), 2);

	frame = gtk_frame_new(_("Rule"));
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.01, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox1), frame, FALSE, FALSE, 0);
	
	table = gtk_table_new(3, 3, FALSE);
	gtk_container_add(GTK_CONTAINER(frame), table);
	gtk_widget_set_size_request(frame, -1, 105);
	
	upper_hbox = gtk_hbox_new(FALSE, HSPACING_NARROW);
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), upper_hbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(""), TRUE, TRUE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox, 2, 3, 0, 1, 
			GTK_FILL, GTK_SHRINK, 2, 2);
	
	lower_hbox = gtk_hbox_new(FALSE, HSPACING_NARROW);
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), lower_hbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(""), TRUE, TRUE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox,2, 3, 1, 2, 
			 GTK_FILL, GTK_SHRINK, 2, 2);
	
	size_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget(size_group, upper_hbox);
	gtk_size_group_add_widget(size_group, lower_hbox);
	
	/* criteria combo box */
	criteria_label = gtk_label_new(_("Match criteria:"));
	gtk_misc_set_alignment(GTK_MISC(criteria_label), 1, 0.5);
	gtk_widget_set_size_request(criteria_label, 100, -1);
	gtk_table_attach(GTK_TABLE(table), criteria_label, 0, 1, 0, 1, 
			 GTK_FILL, GTK_SHRINK, 2, 2);

	criteria_combo = gtkut_sc_combobox_create(NULL, FALSE);
	store = GTK_LIST_STORE(gtk_combo_box_get_model(
				GTK_COMBO_BOX(criteria_combo)));
	COMBOBOX_ADD(store, _("All messages"), 0);
	COMBOBOX_ADD(store, _("Header"), 1);
	COMBOBOX_ADD(store, _("Age"), 2);
	COMBOBOX_ADD(store, _("Phrase"), 3);
	COMBOBOX_ADD(store, _("Flags"), 4);
	COMBOBOX_ADD(store, _("Color labels"), 5);
	COMBOBOX_ADD(store, _("Thread"), 6);
	COMBOBOX_ADD(store, _("Score"), 7);
	COMBOBOX_ADD(store, _("Size"), 8);
	COMBOBOX_ADD(store, _("Partially downloaded"), 9);
	COMBOBOX_ADD(store, _("Address book"), 10);
	COMBOBOX_ADD(store, _("Tags"), 11);
	COMBOBOX_ADD(store, _("External program test"), 12);

	gtk_widget_set_size_request(criteria_combo, 150, -1);
	gtk_combo_box_set_active(GTK_COMBO_BOX(criteria_combo), MATCH_ALL);
	gtk_table_attach(GTK_TABLE(table), criteria_combo, 1, 2, 0, 1,
			 GTK_FILL, GTK_SHRINK, 2, 2);
	g_signal_connect(G_OBJECT(criteria_combo), "changed",
			 G_CALLBACK(prefs_matcher_criteria_select),
			 NULL);
	
	upper_filler = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(upper_hbox), upper_filler, TRUE, TRUE, 0); 
	
	lower_filler = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(lower_hbox), lower_filler, TRUE, TRUE, 0);
			 
	criteria_label2 = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(upper_hbox), criteria_label2, FALSE, FALSE, 0);

	/* headers combo box entry */
	headers_combo = gtk_combo_box_entry_new_with_model(matcher.model_headers, 0);
	gtk_widget_set_size_request(headers_combo, 100, -1);
	gtk_box_pack_start(GTK_BOX(upper_hbox), headers_combo, TRUE, TRUE, 0);
	header_entry = gtk_bin_get_child(GTK_BIN((headers_combo)));
	
	criteria_combo2 = gtkut_sc_combobox_create(NULL, TRUE);
	prefs_matcher_set_model(criteria_combo2, matcher.model_phrase);
	gtk_box_pack_start(GTK_BOX(upper_hbox), criteria_combo2, TRUE, TRUE, 0);
	g_signal_connect(G_OBJECT(criteria_combo2), "changed",
			 G_CALLBACK(prefs_matcher_second_criteria_sel),
			 NULL);

	/* book/folder value */
	addressbook_folder_combo = combobox_text_new(TRUE, _("Any"), NULL);
	gtk_widget_set_size_request(addressbook_folder_combo, 150, -1);
	gtk_box_pack_start(GTK_BOX(upper_hbox), addressbook_folder_combo, TRUE, TRUE, 0);			 

	addressbook_select_btn = gtk_button_new_with_label(_("Select ..."));
	gtk_box_pack_start(GTK_BOX(upper_hbox), addressbook_select_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT (addressbook_select_btn), "clicked",
			 G_CALLBACK(prefs_matcher_addressbook_select),
			 NULL);

	match_label = gtk_label_new("");
	gtk_misc_set_alignment(GTK_MISC(match_label), 1, 0.5);
	gtk_table_attach(GTK_TABLE(table), match_label, 0, 1, 1, 2,
			 GTK_FILL, GTK_SHRINK, 2, 2);

	match_hbox = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(table), match_hbox, 1, 2, 1, 2,
			 GTK_FILL, GTK_SHRINK, 2, 2); 

	match_combo = gtkut_sc_combobox_create(NULL, TRUE);
	gtk_box_pack_start(GTK_BOX(match_hbox), match_combo, TRUE, TRUE, 0);
	
	/* color labels combo */
	color_optmenu = gtk_cmoption_menu_new();
	gtk_cmoption_menu_set_menu(GTK_CMOPTION_MENU(color_optmenu),
				 colorlabel_create_color_menu());
	gtk_box_pack_start(GTK_BOX(match_hbox), color_optmenu, FALSE, FALSE, 0);
	
	/* address header name */
	header_addr_combo = combobox_text_new(TRUE,
			      Q_("Filtering Matcher Menu|All"), _("Any"),
			      "From", "To", "Cc", "Reply-To", "Sender", NULL);
	gtk_box_pack_start(GTK_BOX(match_hbox), header_addr_combo, FALSE, FALSE, 0);
	header_addr_entry = gtk_bin_get_child(GTK_BIN((header_addr_combo)));
	gtk_widget_set_size_request(header_addr_combo, 150, -1);
	
	match_label2 = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(lower_hbox), match_label2, FALSE, FALSE, 0);

	/* numeric value */
	numeric_hbox = gtk_hbox_new(FALSE, HSPACING_NARROW);
	gtk_box_pack_start(GTK_BOX(lower_hbox), numeric_hbox, FALSE, FALSE, 0);

	numeric_entry = gtk_spin_button_new_with_range(0, 1000, 1);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(numeric_entry), 0);
	gtk_box_pack_start(GTK_BOX(numeric_hbox), numeric_entry, FALSE, FALSE, 0);
	
	numeric_label = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(numeric_hbox), numeric_label, FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(numeric_hbox), gtk_label_new(""), TRUE, TRUE, 0);

	match_combo2 = gtkut_sc_combobox_create(NULL, TRUE);
	gtk_box_pack_start(GTK_BOX(lower_hbox), match_combo2, TRUE, TRUE, 0);
	
	/* string value */
	string_entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(lower_hbox), string_entry, TRUE, TRUE, 0);

	hbox = gtk_hbox_new(FALSE, HSPACING_NARROW);
	gtk_size_group_add_widget(size_group, hbox);
	PACK_CHECK_BUTTON(hbox, case_checkbtn, _("Case sensitive"));
#ifndef G_OS_WIN32
	PACK_CHECK_BUTTON(hbox, regexp_checkbtn, _("Use regexp"));
#endif
	gtk_box_pack_end(GTK_BOX(hbox), gtk_label_new(""), TRUE, TRUE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox, 2, 3, 2, 3,
			 GTK_FILL, GTK_SHRINK, 4, 0);

	/* test info button */
	test_btn = gtk_button_new_from_stock(GTK_STOCK_INFO);
	gtk_box_pack_start(GTK_BOX(lower_hbox), test_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT (test_btn), "clicked",
			 G_CALLBACK(prefs_matcher_test_info),
			 window);

	/* register / substitute / delete */
	reg_hbox = gtk_hbox_new(FALSE, HSPACING_NARROW);
	gtk_box_pack_start(GTK_BOX(vbox1), reg_hbox, FALSE, FALSE, 0);

	arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_box_pack_start(GTK_BOX(reg_hbox), arrow, FALSE, FALSE, 0);
	gtk_widget_set_size_request(arrow, -1, 16);

	btn_hbox = gtk_hbox_new(FALSE, HSPACING_NARROW);
	gtk_box_pack_start(GTK_BOX(reg_hbox), btn_hbox, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_box_pack_start(GTK_BOX(btn_hbox), reg_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(reg_btn), "clicked",
			 G_CALLBACK(prefs_matcher_register_cb), NULL);

	subst_btn = gtkut_get_replace_btn(_("Replace"));
	gtk_box_pack_start(GTK_BOX(btn_hbox), subst_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(subst_btn), "clicked",
			 G_CALLBACK(prefs_matcher_substitute_cb),
			 NULL);

	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_box_pack_start(GTK_BOX(btn_hbox), del_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(del_btn), "clicked",
			 G_CALLBACK(prefs_matcher_delete_cb), NULL);

	cond_hbox = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_box_pack_start(GTK_BOX(vbox1), cond_hbox, TRUE, TRUE, 0);

	cond_scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(cond_scrolledwin, -1, 150);
	gtk_box_pack_start(GTK_BOX(cond_hbox), cond_scrolledwin,
			   TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cond_scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	cond_list_view = prefs_matcher_list_view_create(); 				       
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(cond_scrolledwin),
					    GTK_SHADOW_ETCHED_IN);
	gtk_container_add(GTK_CONTAINER(cond_scrolledwin), cond_list_view);

	btn_vbox = gtk_vbox_new(FALSE, VBOX_BORDER);
	gtk_box_pack_start(GTK_BOX(cond_hbox), btn_vbox, FALSE, FALSE, 0);

	up_btn = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_box_pack_start(GTK_BOX(btn_vbox), up_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(up_btn), "clicked",
			 G_CALLBACK(prefs_matcher_up), NULL);

	down_btn = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_box_pack_start(GTK_BOX(btn_vbox), down_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(down_btn), "clicked",
			 G_CALLBACK(prefs_matcher_down), NULL);

	/* boolean operation */
	GtkWidget *hbox_bool = gtk_hbox_new(FALSE, HSPACING_NARROW);
	gtk_box_pack_start(GTK_BOX(vbox1), hbox_bool, FALSE, FALSE, 0);

	bool_op_label = gtk_label_new(_("Message must match"));
	gtk_box_pack_start(GTK_BOX(hbox_bool), bool_op_label,
			   FALSE, FALSE, 0);

	bool_op_combo = combobox_text_new(FALSE, _("at least one"), 
					  _("all"), NULL);
	gtk_box_pack_start(GTK_BOX(hbox_bool), bool_op_combo,
			   FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_bool), gtk_label_new(_("of above rules")),
			   FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_bool), gtk_label_new(""),
			   TRUE, TRUE, 0);
	
	if (!geometry.min_height) {
		geometry.min_width = 550;
		geometry.min_height = 368;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.matcherwin_width,
				    prefs_common.matcherwin_height);

	gtk_widget_show_all(window);

	matcher.window    = window;

	matcher.ok_btn = ok_btn;

	matcher.criteria_combo = criteria_combo;
	matcher.criteria_combo2 = criteria_combo2;
	matcher.header_entry = header_entry;
	matcher.header_addr_combo = header_addr_combo;
	matcher.header_addr_entry = header_addr_entry;
	matcher.string_entry = string_entry;
	matcher.numeric_entry = numeric_entry;
	matcher.numeric_label = numeric_label;
	matcher.addressbook_folder_combo = addressbook_folder_combo;
	matcher.match_combo = match_combo;
	matcher.case_checkbtn = case_checkbtn;
#ifndef G_OS_WIN32
	matcher.regexp_checkbtn = regexp_checkbtn;
#endif
	matcher.bool_op_combo = bool_op_combo;
	matcher.test_btn = test_btn;
	matcher.addressbook_select_btn = addressbook_select_btn;
	matcher.color_optmenu = color_optmenu;
	matcher.match_label = match_label;
	matcher.criteria_label2 = criteria_label2;
	matcher.headers_combo = headers_combo;
	matcher.match_combo2 = match_combo2;
	matcher.match_label2 = match_label2;
	matcher.upper_filler = upper_filler;
	matcher.lower_filler = lower_filler;
	
	matcher.cond_list_view = cond_list_view;

	matcher.selected_criteria = -1;
	prefs_matcher_criteria_select(criteria_combo, NULL);
}

/*!
 *\brief	Set the contents of a row
 *
 *\param	row Index of row to set
 *\param	prop Condition to set
 *
 *\return	gint Row index \a prop has been added
 */
static void prefs_matcher_list_view_set_row(GtkTreeIter *row, MatcherProp *prop)
{
	gchar *matcher_str;

	if (prop == NULL) {
		prefs_matcher_list_view_insert_matcher(matcher.cond_list_view,
						       NULL, _("(New)"), FALSE);
		return;						       
	}

	matcher_str = matcherprop_to_string(prop);
	if (!row)
		prefs_matcher_list_view_insert_matcher(matcher.cond_list_view,
						       NULL, matcher_str,
						       TRUE);
	else
		prefs_matcher_list_view_insert_matcher(matcher.cond_list_view,
						       row, matcher_str, 
						       TRUE);
	g_free(matcher_str);
}

static gboolean match_combo2_model_set(void)
{
	GtkTreeModel *model = gtk_combo_box_get_model(GTK_COMBO_BOX(matcher.match_combo2));
	if (model == matcher.model_age_units ||
	    model == matcher.model_found ||
	    model == matcher.model_partial ||
	    model == matcher.model_phrase ||
	    model == matcher.model_set ||
	    model == matcher.model_size_units ||
	    model == matcher.model_tags ||
	    model == matcher.model_thread)
		return TRUE;
	else
		debug_print("match_combo2 model unset.\n");

	return FALSE;
}

static gboolean match_combo_model_set(void)
{
	GtkTreeModel *model = gtk_combo_box_get_model(GTK_COMBO_BOX(matcher.match_combo));
	if (model == matcher.model_age ||
	    model == matcher.model_contain ||
	    model == matcher.model_flags ||
	    model == matcher.model_score ||
	    model == matcher.model_size ||
	    model == matcher.model_test)
		return TRUE;
	else
		debug_print("match_combo model unset.\n");

	return FALSE;
}

/*!
 *\brief	Clears a condition in the list widget
 */
static void prefs_matcher_reset_condition(void)
{
	gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.criteria_combo), MATCH_ALL);
	gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.criteria_combo2), 0);
	if (match_combo_model_set())
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo), 0);
	if (match_combo2_model_set())
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2), 0);
	gtk_cmoption_menu_set_history(GTK_CMOPTION_MENU(matcher.color_optmenu), 0);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(matcher.numeric_entry), 0);
	gtk_entry_set_text(GTK_ENTRY(matcher.header_entry), "");
	gtk_entry_set_text(GTK_ENTRY(matcher.header_addr_entry), "");
	gtk_entry_set_text(GTK_ENTRY(matcher.string_entry), "");
	gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((matcher.addressbook_folder_combo)))), "");
#ifndef G_OS_WIN32
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn), FALSE);
#endif
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn), FALSE);
}

/*!
 *\brief	Initializes dialog with a set of conditions
 *
 *\param	matchers List of conditions
 */
static void prefs_matcher_set_dialog(MatcherList *matchers)
{
	GSList *cur;
	gboolean bool_op = 1;
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model
				(GTK_TREE_VIEW(matcher.cond_list_view)));

	gtk_list_store_clear(store);				

	prefs_matcher_list_view_set_row(NULL, NULL);
	if (matchers != NULL) {
		for (cur = matchers->matchers; cur != NULL;
		     cur = g_slist_next(cur)) {
			MatcherProp *prop;
			prop = (MatcherProp *) cur->data;
			prefs_matcher_list_view_set_row(NULL, prop);
		}

		bool_op = matchers->bool_and;
	}
	
	gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.bool_op_combo), bool_op);

	prefs_matcher_reset_condition();
	
	combobox_set_sensitive(GTK_COMBO_BOX(matcher.criteria_combo), MATCH_TAGS,
			(tags_get_size() > 0) ? TRUE : FALSE);
}

/*!
 *\brief	Converts current conditions in list box in
 *		a matcher list used by the matcher.
 *
 *\return	MatcherList * List of conditions.
 */
static MatcherList *prefs_matcher_get_list(void)
{
	gchar *matcher_str;
	MatcherProp *prop;
	gboolean bool_and;
	GSList *matcher_list;
	MatcherList *matchers;
	GtkTreeModel *model;
	GtkTreeIter iter;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(matcher.cond_list_view));
	if (!gtk_tree_model_get_iter_first(model, &iter))
		return NULL;

	matcher_list = NULL;

	do {
		gboolean is_valid;
	
		gtk_tree_model_get(model, &iter,
				   PREFS_MATCHER_COND, &matcher_str,
				   PREFS_MATCHER_COND_VALID, &is_valid,
				   -1);
		
		if (is_valid) {
			/* tmp = matcher_str; */
			prop = matcher_parser_get_prop(matcher_str);
			g_free(matcher_str);
			if (prop == NULL)
				break;
			
			matcher_list = g_slist_append(matcher_list, prop);
		}
	} while (gtk_tree_model_iter_next(model, &iter));

	bool_and = gtk_combo_box_get_active(GTK_COMBO_BOX(matcher.bool_op_combo));

	matchers = matcherlist_new(matcher_list, bool_and);

	return matchers;
}

/*!
 *\brief	Maps a keyword id (see #get_matchparser_tab_id) to a 
 *		criteria type (see first parameter of #matcherprop_new
 *		or #matcherprop_unquote_new)
 *
 *\param	matching_id Id returned by the matcher parser.
 *
 *\return	gint One of the CRITERIA_xxx constants.
 */
static gint prefs_matcher_get_criteria_from_matching(gint matching_id)
{
	switch(matching_id) {
	case MATCHCRITERIA_ALL:
		return CRITERIA_ALL;
	case MATCHCRITERIA_NOT_UNREAD:
	case MATCHCRITERIA_UNREAD:
		return CRITERIA_UNREAD;
	case MATCHCRITERIA_NOT_NEW:
	case MATCHCRITERIA_NEW:
		return CRITERIA_NEW;
	case MATCHCRITERIA_NOT_MARKED:
	case MATCHCRITERIA_MARKED:
		return CRITERIA_MARKED;
	case MATCHCRITERIA_NOT_DELETED:
	case MATCHCRITERIA_DELETED:
		return CRITERIA_DELETED;
	case MATCHCRITERIA_NOT_REPLIED:
	case MATCHCRITERIA_REPLIED:
		return CRITERIA_REPLIED;
	case MATCHCRITERIA_NOT_FORWARDED:
	case MATCHCRITERIA_FORWARDED:
		return CRITERIA_FORWARDED;
	case MATCHCRITERIA_LOCKED:
	case MATCHCRITERIA_NOT_LOCKED:
		return CRITERIA_LOCKED;
	case MATCHCRITERIA_NOT_SPAM:
	case MATCHCRITERIA_SPAM:
		return CRITERIA_SPAM;
	case MATCHCRITERIA_HAS_ATTACHMENT:
	case MATCHCRITERIA_HAS_NO_ATTACHMENT:
		return CRITERIA_HAS_ATTACHMENT;
	case MATCHCRITERIA_SIGNED:
	case MATCHCRITERIA_NOT_SIGNED:
		return CRITERIA_SIGNED;
	case MATCHCRITERIA_PARTIAL:
	case MATCHCRITERIA_NOT_PARTIAL:
		return CRITERIA_PARTIAL;
	case MATCHCRITERIA_COLORLABEL:
	case MATCHCRITERIA_NOT_COLORLABEL:
		return CRITERIA_COLORLABEL;
	case MATCHCRITERIA_IGNORE_THREAD:
	case MATCHCRITERIA_NOT_IGNORE_THREAD:
		return CRITERIA_IGNORE_THREAD;
	case MATCHCRITERIA_WATCH_THREAD:
	case MATCHCRITERIA_NOT_WATCH_THREAD:
		return CRITERIA_WATCH_THREAD;
	case MATCHCRITERIA_NOT_SUBJECT:
	case MATCHCRITERIA_SUBJECT:
		return CRITERIA_SUBJECT;
	case MATCHCRITERIA_NOT_FROM:
	case MATCHCRITERIA_FROM:
		return CRITERIA_FROM;
	case MATCHCRITERIA_NOT_TO:
	case MATCHCRITERIA_TO:
		return CRITERIA_TO;
	case MATCHCRITERIA_NOT_CC:
	case MATCHCRITERIA_CC:
		return CRITERIA_CC;
	case MATCHCRITERIA_NOT_NEWSGROUPS:
	case MATCHCRITERIA_NEWSGROUPS:
		return CRITERIA_NEWSGROUPS;
	case MATCHCRITERIA_NOT_INREPLYTO:
	case MATCHCRITERIA_INREPLYTO:
		return CRITERIA_INREPLYTO;
	case MATCHCRITERIA_NOT_REFERENCES:
	case MATCHCRITERIA_REFERENCES:
		return CRITERIA_REFERENCES;
	case MATCHCRITERIA_NOT_TO_AND_NOT_CC:
	case MATCHCRITERIA_TO_OR_CC:
		return CRITERIA_TO_OR_CC;
	case MATCHCRITERIA_NOT_TAG:
	case MATCHCRITERIA_TAG:
		return CRITERIA_TAG;
	case MATCHCRITERIA_NOT_TAGGED:
	case MATCHCRITERIA_TAGGED:
		return CRITERIA_TAGGED;
	case MATCHCRITERIA_NOT_BODY_PART:
	case MATCHCRITERIA_BODY_PART:
		return CRITERIA_BODY_PART;
	case MATCHCRITERIA_NOT_MESSAGE:
	case MATCHCRITERIA_MESSAGE:
		return CRITERIA_MESSAGE;
	case MATCHCRITERIA_NOT_HEADERS_PART:
	case MATCHCRITERIA_HEADERS_PART:
		return CRITERIA_HEADERS_PART;
	case MATCHCRITERIA_NOT_HEADER:
	case MATCHCRITERIA_HEADER:
		return CRITERIA_HEADER;
	case MATCHCRITERIA_AGE_GREATER:
		return CRITERIA_AGE_GREATER;
	case MATCHCRITERIA_AGE_LOWER:
		return CRITERIA_AGE_LOWER;
	case MATCHCRITERIA_SCORE_GREATER:
		return CRITERIA_SCORE_GREATER;
	case MATCHCRITERIA_SCORE_LOWER:
		return CRITERIA_SCORE_LOWER;
	case MATCHCRITERIA_SCORE_EQUAL:
		return CRITERIA_SCORE_EQUAL;
	case MATCHCRITERIA_NOT_TEST:
	case MATCHCRITERIA_TEST:
		return CRITERIA_TEST;
	case MATCHCRITERIA_SIZE_GREATER:
		return CRITERIA_SIZE_GREATER;
	case MATCHCRITERIA_SIZE_SMALLER:
		return CRITERIA_SIZE_SMALLER;
	case MATCHCRITERIA_SIZE_EQUAL:
		return CRITERIA_SIZE_EQUAL;
	case MATCHCRITERIA_FOUND_IN_ADDRESSBOOK:
	case MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK:
		return CRITERIA_FOUND_IN_ADDRESSBOOK;
	default:
		return -1;
	}
}

/*!
 *\brief	Returns the matcher keyword id from a criteria id
 *
 *\param	criteria_id Criteria id (should not be the negate
 *		one)
 *
 *\return	gint A matcher keyword id. See #get_matchparser_tab_id.
 */
static gint prefs_matcher_get_matching_from_criteria(gint criteria_id)
{
	switch (criteria_id) {
	case CRITERIA_ALL:
		return MATCHCRITERIA_ALL;
	case CRITERIA_UNREAD:
		return MATCHCRITERIA_UNREAD;
	case CRITERIA_NEW:
		return MATCHCRITERIA_NEW;
	case CRITERIA_MARKED:
		return MATCHCRITERIA_MARKED;
	case CRITERIA_DELETED:
		return MATCHCRITERIA_DELETED;
	case CRITERIA_REPLIED:
		return MATCHCRITERIA_REPLIED;
	case CRITERIA_FORWARDED:
		return MATCHCRITERIA_FORWARDED;
	case CRITERIA_LOCKED:
		return MATCHCRITERIA_LOCKED;
	case CRITERIA_SPAM:
		return MATCHCRITERIA_SPAM;
	case CRITERIA_HAS_ATTACHMENT:
		return MATCHCRITERIA_HAS_ATTACHMENT;
	case CRITERIA_SIGNED:
		return MATCHCRITERIA_SIGNED;
	case CRITERIA_PARTIAL:
		return MATCHCRITERIA_PARTIAL;
	case CRITERIA_COLORLABEL:
		return MATCHCRITERIA_COLORLABEL;
	case CRITERIA_IGNORE_THREAD:
		return MATCHCRITERIA_IGNORE_THREAD;
	case CRITERIA_WATCH_THREAD:
		return MATCHCRITERIA_WATCH_THREAD;
	case CRITERIA_SUBJECT:
		return MATCHCRITERIA_SUBJECT;
	case CRITERIA_FROM:
		return MATCHCRITERIA_FROM;
	case CRITERIA_TO:
		return MATCHCRITERIA_TO;
	case CRITERIA_CC:
		return MATCHCRITERIA_CC;
	case CRITERIA_TO_OR_CC:
		return MATCHCRITERIA_TO_OR_CC;
	case CRITERIA_TAG:
		return MATCHCRITERIA_TAG;
	case CRITERIA_TAGGED:
		return MATCHCRITERIA_TAGGED;
	case CRITERIA_NEWSGROUPS:
		return MATCHCRITERIA_NEWSGROUPS;
	case CRITERIA_INREPLYTO:
		return MATCHCRITERIA_INREPLYTO;
	case CRITERIA_REFERENCES:
		return MATCHCRITERIA_REFERENCES;
	case CRITERIA_AGE_GREATER:
		return MATCHCRITERIA_AGE_GREATER;
	case CRITERIA_AGE_LOWER:
		return MATCHCRITERIA_AGE_LOWER;
	case CRITERIA_SCORE_GREATER:
		return MATCHCRITERIA_SCORE_GREATER;
	case CRITERIA_SCORE_LOWER:
		return MATCHCRITERIA_SCORE_LOWER;
	case CRITERIA_SCORE_EQUAL:
		return MATCHCRITERIA_SCORE_EQUAL;
	case CRITERIA_HEADER:
		return MATCHCRITERIA_HEADER;
	case CRITERIA_HEADERS_PART:
		return MATCHCRITERIA_HEADERS_PART;
	case CRITERIA_BODY_PART:
		return MATCHCRITERIA_BODY_PART;
	case CRITERIA_MESSAGE:
		return MATCHCRITERIA_MESSAGE;
	case CRITERIA_TEST:
		return MATCHCRITERIA_TEST;
	case CRITERIA_SIZE_GREATER:
		return MATCHCRITERIA_SIZE_GREATER;
	case CRITERIA_SIZE_SMALLER:
		return MATCHCRITERIA_SIZE_SMALLER;
	case CRITERIA_SIZE_EQUAL:
		return MATCHCRITERIA_SIZE_EQUAL;
	case CRITERIA_FOUND_IN_ADDRESSBOOK:
		return MATCHCRITERIA_FOUND_IN_ADDRESSBOOK;
	default:
		return -1;
	}
}

/*!
 *\brief	Returns the negate matcher keyword id from a matcher keyword
 *		id.
 *
 *\param	matcher_criteria Matcher keyword id. 
 *
 *\return	gint A matcher keyword id. See #get_matchparser_tab_id.
 */
static gint prefs_matcher_not_criteria(gint matcher_criteria)
{
	switch(matcher_criteria) {
	case MATCHCRITERIA_UNREAD:
		return MATCHCRITERIA_NOT_UNREAD;
	case MATCHCRITERIA_NEW:
		return MATCHCRITERIA_NOT_NEW;
	case MATCHCRITERIA_MARKED:
		return MATCHCRITERIA_NOT_MARKED;
	case MATCHCRITERIA_DELETED:
		return MATCHCRITERIA_NOT_DELETED;
	case MATCHCRITERIA_REPLIED:
		return MATCHCRITERIA_NOT_REPLIED;
	case MATCHCRITERIA_FORWARDED:
		return MATCHCRITERIA_NOT_FORWARDED;
	case MATCHCRITERIA_LOCKED:
		return MATCHCRITERIA_NOT_LOCKED;
	case MATCHCRITERIA_SPAM:
		return MATCHCRITERIA_NOT_SPAM;
	case MATCHCRITERIA_HAS_ATTACHMENT:
		return MATCHCRITERIA_HAS_NO_ATTACHMENT;
	case MATCHCRITERIA_SIGNED:
		return MATCHCRITERIA_NOT_SIGNED;
	case MATCHCRITERIA_PARTIAL:
		return MATCHCRITERIA_NOT_PARTIAL;
	case MATCHCRITERIA_COLORLABEL:
		return MATCHCRITERIA_NOT_COLORLABEL;
	case MATCHCRITERIA_IGNORE_THREAD:
		return MATCHCRITERIA_NOT_IGNORE_THREAD;
	case MATCHCRITERIA_WATCH_THREAD:
		return MATCHCRITERIA_NOT_WATCH_THREAD;
	case MATCHCRITERIA_SUBJECT:
		return MATCHCRITERIA_NOT_SUBJECT;
	case MATCHCRITERIA_FROM:
		return MATCHCRITERIA_NOT_FROM;
	case MATCHCRITERIA_TO:
		return MATCHCRITERIA_NOT_TO;
	case MATCHCRITERIA_CC:
		return MATCHCRITERIA_NOT_CC;
	case MATCHCRITERIA_TO_OR_CC:
		return MATCHCRITERIA_NOT_TO_AND_NOT_CC;
	case MATCHCRITERIA_TAG:
		return MATCHCRITERIA_NOT_TAG;
	case MATCHCRITERIA_TAGGED:
		return MATCHCRITERIA_NOT_TAGGED;
	case MATCHCRITERIA_NEWSGROUPS:
		return MATCHCRITERIA_NOT_NEWSGROUPS;
	case MATCHCRITERIA_INREPLYTO:
		return MATCHCRITERIA_NOT_INREPLYTO;
	case MATCHCRITERIA_REFERENCES:
		return MATCHCRITERIA_NOT_REFERENCES;
	case MATCHCRITERIA_HEADER:
		return MATCHCRITERIA_NOT_HEADER;
	case MATCHCRITERIA_HEADERS_PART:
		return MATCHCRITERIA_NOT_HEADERS_PART;
	case MATCHCRITERIA_MESSAGE:
		return MATCHCRITERIA_NOT_MESSAGE;
	case MATCHCRITERIA_TEST:
		return MATCHCRITERIA_NOT_TEST;
	case MATCHCRITERIA_BODY_PART:
		return MATCHCRITERIA_NOT_BODY_PART;
	case MATCHCRITERIA_FOUND_IN_ADDRESSBOOK:
		return MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK;
	default:
		return matcher_criteria;
	}
}

static gint prefs_matcher_get_criteria(void)
{
	gint match_criteria = gtk_combo_box_get_active(GTK_COMBO_BOX(
					matcher.criteria_combo));
	const gchar *header = NULL;
	  
	switch (match_criteria) {
	case MATCH_ABOOK:
		return CRITERIA_FOUND_IN_ADDRESSBOOK;	
	case MATCH_ALL:
		return CRITERIA_ALL;
	case MATCH_AGE:
	case MATCH_SCORE:
	case MATCH_SIZE:
	case MATCH_FLAG:
		return combobox_get_active_data(GTK_COMBO_BOX(
					matcher.match_combo));
	case MATCH_HEADER:
		header = gtk_entry_get_text(GTK_ENTRY(matcher.header_entry));
		return header_name_to_crit(header);
	case MATCH_LABEL:
		return CRITERIA_COLORLABEL;
	case MATCH_PARTIAL:
		return CRITERIA_PARTIAL;
	case MATCH_TEST:
		return CRITERIA_TEST;
	case MATCH_PHRASE:
	case MATCH_TAGS:
	case MATCH_THREAD:
		return combobox_get_active_data(GTK_COMBO_BOX(
					matcher.criteria_combo2));
	}
	
	return -1;
}

static gint prefs_matcher_get_pred(const gint criteria)
{
	switch(criteria) {
	case CRITERIA_SUBJECT:
	case CRITERIA_FROM:
	case CRITERIA_TO:
	case CRITERIA_CC:
	case CRITERIA_TO_OR_CC:
	case CRITERIA_NEWSGROUPS:
	case CRITERIA_INREPLYTO:
	case CRITERIA_REFERENCES:
	case CRITERIA_HEADER:
	case CRITERIA_HEADERS_PART:
	case CRITERIA_BODY_PART:
	case CRITERIA_MESSAGE:
	case CRITERIA_TAG:
	case CRITERIA_TAGGED:
	case CRITERIA_TEST:
		return gtk_combo_box_get_active(GTK_COMBO_BOX(matcher.match_combo));
	case CRITERIA_FOUND_IN_ADDRESSBOOK:
	case CRITERIA_UNREAD:
	case CRITERIA_NEW:
	case CRITERIA_MARKED:
	case CRITERIA_DELETED:
	case CRITERIA_REPLIED:
	case CRITERIA_FORWARDED:
	case CRITERIA_LOCKED:
	case CRITERIA_SPAM:
	case CRITERIA_HAS_ATTACHMENT:
	case CRITERIA_SIGNED:
	case CRITERIA_COLORLABEL:
		return gtk_combo_box_get_active(GTK_COMBO_BOX(matcher.match_combo2));
	case CRITERIA_WATCH_THREAD:
		return gtk_combo_box_get_active(GTK_COMBO_BOX(matcher.criteria_combo2)) - 2;
	case CRITERIA_IGNORE_THREAD:
	case CRITERIA_PARTIAL:
		return gtk_combo_box_get_active(GTK_COMBO_BOX(matcher.criteria_combo2));
	}
	
	return 0;
}

/*!
 *\brief	Converts the text in the selected row to a 
 *		matcher structure
 *
 *\return	MatcherProp * Newly allocated matcher structure.
 */
static MatcherProp *prefs_matcher_dialog_to_matcher(void)
{
	MatcherProp *matcherprop;
	gint criteria;
	gint matchtype;
	gint value_pred;
	gint value_criteria = prefs_matcher_get_criteria();
	gboolean use_regexp;
	gboolean case_sensitive;
	const gchar *header;
	const gchar *expr;
	gint value, sel;

	if (value_criteria == -1)
		return NULL;

	criteria = prefs_matcher_get_matching_from_criteria(value_criteria);

	value_pred = prefs_matcher_get_pred(value_criteria);
	if(value_pred)
		criteria = prefs_matcher_not_criteria(criteria);

#ifndef G_OS_WIN32
	use_regexp = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn));
#else
	use_regexp = FALSE;
#endif
	case_sensitive = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn));

	if (use_regexp) {
		if (case_sensitive)
			matchtype = MATCHTYPE_REGEXP;
		else
			matchtype = MATCHTYPE_REGEXPCASE;
	}
	else {
		if (case_sensitive)
			matchtype = MATCHTYPE_MATCH;
		else
			matchtype = MATCHTYPE_MATCHCASE;
	}

	header = NULL;
	expr = NULL;
	value = 0;

	switch (value_criteria) {
	case CRITERIA_ALL:
	case CRITERIA_UNREAD:
	case CRITERIA_NEW:
	case CRITERIA_MARKED:
	case CRITERIA_DELETED:
	case CRITERIA_REPLIED:
	case CRITERIA_FORWARDED:
	case CRITERIA_LOCKED:
	case CRITERIA_SPAM:
	case CRITERIA_HAS_ATTACHMENT:
	case CRITERIA_SIGNED:
	case CRITERIA_PARTIAL:
	case CRITERIA_IGNORE_THREAD:
	case CRITERIA_WATCH_THREAD:
	case CRITERIA_TAGGED:
		break;

	case CRITERIA_SUBJECT:
	case CRITERIA_FROM:
	case CRITERIA_TO:
	case CRITERIA_CC:
	case CRITERIA_TO_OR_CC:
	case CRITERIA_TAG:
	case CRITERIA_NEWSGROUPS:
	case CRITERIA_INREPLYTO:
	case CRITERIA_REFERENCES:
	case CRITERIA_HEADERS_PART:
	case CRITERIA_BODY_PART:
	case CRITERIA_MESSAGE:
		expr = gtk_entry_get_text(GTK_ENTRY(matcher.string_entry));
		
		if(*expr == '\0') {
			alertpanel_error(_("Search pattern is not set."));
			return NULL;
		}
		break;

	case CRITERIA_TEST:
		expr = gtk_entry_get_text(GTK_ENTRY(matcher.string_entry));
		
		if(*expr == '\0') {
			alertpanel_error(_("Test command is not set."));
			return NULL;
		}
		break;

	case CRITERIA_AGE_GREATER:
	case CRITERIA_AGE_LOWER:
		value = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(
							 matcher.numeric_entry));
		sel = gtk_combo_box_get_active(GTK_COMBO_BOX(matcher.match_combo2));
		if(sel == AGE_WEEKS)
			value *= 7;
		break;
			
	case CRITERIA_SCORE_GREATER:
	case CRITERIA_SCORE_LOWER:
	case CRITERIA_SCORE_EQUAL:
		value = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(
							 matcher.numeric_entry));
		break;
							 
	case CRITERIA_SIZE_GREATER:
	case CRITERIA_SIZE_SMALLER:
	case CRITERIA_SIZE_EQUAL:
		value = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(
							 matcher.numeric_entry));
		sel = gtk_combo_box_get_active(GTK_COMBO_BOX(matcher.match_combo2));
		if(sel == SIZE_UNIT_MBYTES)
			value *= MB_SIZE;
		if(sel == SIZE_UNIT_KBYTES)
			value *= KB_SIZE;
		break;
		
	case CRITERIA_COLORLABEL:
		value = colorlabel_get_color_menu_active_item
			(gtk_cmoption_menu_get_menu(GTK_CMOPTION_MENU
				(matcher.color_optmenu))); 
		break;

	case CRITERIA_HEADER:
		header = gtk_entry_get_text(GTK_ENTRY(matcher.header_entry));
		expr = gtk_entry_get_text(GTK_ENTRY(matcher.string_entry));

		if (*header == '\0') {
		    alertpanel_error(_("Header name is not set."));
		    return NULL;
		}
		
		if(*expr == '\0') {
			alertpanel_error(_("Search pattern is not set."));
			return NULL;
		} 
		break;

	case CRITERIA_FOUND_IN_ADDRESSBOOK:
		header = gtk_entry_get_text(GTK_ENTRY(matcher.header_addr_entry));
		expr = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((matcher.addressbook_folder_combo)))));

		if (*header == '\0') {
		    alertpanel_error(_("Header name is not set."));
		    return NULL;
		}
		if (*expr == '\0') {
			gchar *tmp;

			if (g_utf8_collate(header, Q_("Filtering Matcher Menu|All")) == 0)
				tmp = g_strdup(_("all addresses in all headers"));
			else
			if (g_utf8_collate(header, _("Any")) == 0)
				tmp = g_strdup(_("any address in any header"));
			else
				tmp = g_strdup_printf(_("the address(es) in header '%s'"), header);
			alertpanel_error(_("Book/folder path is not set.\n\n"
						"If you want to match %s against the whole address book, "
						"you have to select '%s' from the book/folder drop-down list."),
						tmp, _("Any"));
			g_free(tmp);
		    return NULL;
		}
		/* store UNtranslated "Any"/"All" in matcher expressions */
		if (g_utf8_collate(header, Q_("Filtering Matcher Menu|All")) == 0)
			header = "All";
		else
			if (g_utf8_collate(header, _("Any")) == 0)
				header = "Any";
		if (g_utf8_collate(expr, _("Any")) == 0)
			expr = "Any";
		break;
	}

	matcherprop = matcherprop_new_create(criteria, header, matchtype,
				      expr, value);

	return matcherprop;
}

/*!
 *\brief	Signal handler for register button
 */
static void prefs_matcher_register_cb(void)
{
	MatcherProp *matcherprop;
	
	matcherprop = prefs_matcher_dialog_to_matcher();
	if (matcherprop == NULL)
		return;

	prefs_matcher_list_view_set_row(NULL, matcherprop);

	matcherprop_free(matcherprop);
	
	prefs_matcher_reset_condition();
}

/*!
 *\brief	Signal handler for substitute button
 */
static void prefs_matcher_substitute_cb(void)
{
	MatcherProp *matcherprop;
	GtkTreeIter row;
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	gboolean is_valid;

	selection = gtk_tree_view_get_selection
			(GTK_TREE_VIEW(matcher.cond_list_view));
	
	if (!gtk_tree_selection_get_selected(selection, &model, &row))
		return;
	
	gtk_tree_model_get(model, &row, 
			   PREFS_MATCHER_COND_VALID, &is_valid,
			   -1);
	if (!is_valid)
		return;

	matcherprop = prefs_matcher_dialog_to_matcher();
	if (matcherprop == NULL)
		return;

	prefs_matcher_list_view_set_row(&row, matcherprop);

	matcherprop_free(matcherprop);
}

/*!
 *\brief	Signal handler for delete button
 */
static void prefs_matcher_delete_cb(void)
{
	GtkTreeIter row;
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	gboolean is_valid;

	selection = gtk_tree_view_get_selection
			(GTK_TREE_VIEW(matcher.cond_list_view));
	
	if (!gtk_tree_selection_get_selected(selection, &model, &row))
		return;
		
	gtk_tree_model_get(model, &row, 
			   PREFS_MATCHER_COND_VALID, &is_valid,
			   -1);

	if (!is_valid)
		return;

	gtk_list_store_remove(GTK_LIST_STORE(model), &row);		

	prefs_matcher_reset_condition();
}

/*!
 *\brief	Signal handler for 'move up' button
 */
static void prefs_matcher_up(void)
{
	GtkTreePath *prev, *sel, *try;
	GtkTreeIter isel;
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter iprev;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(matcher.cond_list_view)),
		 &model,	
		 &isel))
		return;
	store = (GtkListStore *)model;
	sel = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &isel);
	if (!sel)
		return;
	
	/* no move if we're at row 0 or 1, looks phony, but other
	 * solutions are more convoluted... */
	try = gtk_tree_path_copy(sel);
	if (!gtk_tree_path_prev(try) || !gtk_tree_path_prev(try)) {
		gtk_tree_path_free(try);
		gtk_tree_path_free(sel);
		return;
	}
	gtk_tree_path_free(try);

	prev = gtk_tree_path_copy(sel);		
	if (gtk_tree_path_prev(prev)) {
		gtk_tree_model_get_iter(GTK_TREE_MODEL(store),
					&iprev, prev);
		gtk_list_store_swap(store, &iprev, &isel);
		/* XXX: GTK2 select row?? */
	}

	gtk_tree_path_free(sel);
	gtk_tree_path_free(prev);
}

/*!
 *\brief	Signal handler for 'move down' button
 */
static void prefs_matcher_down(void)
{
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter next, sel;
	GtkTreePath *try;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(matcher.cond_list_view)),
		 &model,
		 &sel))
		return;
	store = (GtkListStore *)model;
	try = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &sel);
	if (!try) 
		return;
	
	/* move when not at row 0 ... */
	if (gtk_tree_path_prev(try)) {
		next = sel;
		if (gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &next))
			gtk_list_store_swap(store, &next, &sel);
	}
		
	gtk_tree_path_free(try);
}

static void prefs_matcher_enable_widget(GtkWidget* widget, const gboolean enable)
{
	cm_return_if_fail(widget != NULL);

	if(enable == TRUE) {
		gtk_widget_set_sensitive(widget, TRUE);
		gtk_widget_show(widget);	
	} else {
		gtk_widget_set_sensitive(widget, FALSE);
		gtk_widget_hide(widget);
	}
}

static void prefs_matcher_set_model(GtkWidget *widget, GtkTreeModel *model)
{
	cm_return_if_fail(widget != NULL);
	cm_return_if_fail(model != NULL);
	
	gtk_combo_box_set_model(GTK_COMBO_BOX(widget), model);
	gtk_combo_box_set_active(GTK_COMBO_BOX(widget), 0);
}

static void prefs_matcher_second_criteria_sel(GtkWidget *widget,
					      gpointer user_data)
{
	gint criteria = gtk_combo_box_get_active(GTK_COMBO_BOX(
						matcher.criteria_combo));
	gint criteria2 = combobox_get_active_data(GTK_COMBO_BOX(
						matcher.criteria_combo2));
	
	if(criteria != MATCH_PHRASE && criteria != MATCH_TAGS) return;
	
	if(criteria == MATCH_PHRASE) {
		switch(criteria2) {
		case CRITERIA_HEADERS_PART:
			gtk_label_set_text(GTK_LABEL(matcher.match_label),
					_("Headers part"));
			break;
		case CRITERIA_BODY_PART:
			gtk_label_set_text(GTK_LABEL(matcher.match_label),
					_("Body part"));
			break;	
		case CRITERIA_MESSAGE:
			gtk_label_set_text(GTK_LABEL(matcher.match_label),
					_("Whole message"));
			break;
		}
	}
	
	if(criteria == MATCH_TAGS) {
		if(criteria2 == CRITERIA_TAGGED) {
			prefs_matcher_enable_widget(matcher.upper_filler, FALSE);
			prefs_matcher_enable_widget(matcher.match_label2, TRUE);
			prefs_matcher_enable_widget(matcher.string_entry, FALSE);
			prefs_matcher_enable_widget(matcher.case_checkbtn, FALSE);
#ifndef G_OS_WIN32
			prefs_matcher_enable_widget(matcher.regexp_checkbtn, FALSE);
#endif
		} else {
			prefs_matcher_enable_widget(matcher.upper_filler, TRUE);
			prefs_matcher_enable_widget(matcher.match_label2, FALSE);
			prefs_matcher_enable_widget(matcher.string_entry, TRUE);
			prefs_matcher_enable_widget(matcher.case_checkbtn, TRUE);
#ifndef G_OS_WIN32
			prefs_matcher_enable_widget(matcher.regexp_checkbtn, TRUE);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
						matcher.regexp_checkbtn), FALSE);
#endif
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
						matcher.case_checkbtn), FALSE);
		}
	}
}

#define MATCH_COMBO_IS_ENABLED(x) (x != MATCH_ALL && x != MATCH_ABOOK && \
		x != MATCH_PARTIAL && x != MATCH_THREAD && x != MATCH_LABEL) ? TRUE : FALSE
#define MATCH_CASE_REGEXP(x) (x == MATCH_HEADER || x == MATCH_PHRASE) ? TRUE : FALSE 
#define MATCH_NUMERIC(x) (x == MATCH_AGE || x == MATCH_SCORE || \
			  x == MATCH_SIZE) ? TRUE : FALSE

/*!
 *\brief	Change widgets depending on the selected condition
 *
 *\param	criteria combo widget
 *\param	user_data Not used	
 */
static void prefs_matcher_criteria_select(GtkWidget *widget,
					  gpointer user_data)
{
	gint value, old_value;

	old_value = matcher.selected_criteria;
	matcher.selected_criteria = value = gtk_combo_box_get_active
		(GTK_COMBO_BOX(matcher.criteria_combo));

	if (old_value == matcher.selected_criteria)
		return;

	prefs_matcher_enable_widget(matcher.criteria_label2,
				    (value == MATCH_ABOOK   ||
				     value == MATCH_PHRASE  ||
				     value == MATCH_HEADER  ||
				     value == MATCH_PARTIAL ||
				     value == MATCH_TAGS    ||
				     value == MATCH_THREAD));
	prefs_matcher_enable_widget(matcher.headers_combo,
				    (value == MATCH_HEADER));
	prefs_matcher_enable_widget(matcher.criteria_combo2,
				    (value == MATCH_PHRASE  ||
				     value == MATCH_PARTIAL ||
				     value == MATCH_TAGS    ||
				     value == MATCH_THREAD));
	prefs_matcher_enable_widget(matcher.match_combo2,
				    (value == MATCH_ABOOK ||
				     value == MATCH_AGE   ||
				     value == MATCH_FLAG  ||
				     value == MATCH_LABEL ||
				     value == MATCH_SIZE));
	prefs_matcher_enable_widget(matcher.match_label2,
				    (value == MATCH_ABOOK ||
				     value == MATCH_FLAG  ||
				     value == MATCH_LABEL ||
				     value == MATCH_TAGS));
	prefs_matcher_enable_widget(matcher.header_addr_combo,
				    (value == MATCH_ABOOK));
	prefs_matcher_enable_widget(matcher.string_entry,
				    (MATCH_CASE_REGEXP(value) ||
				     value == MATCH_TEST));
	prefs_matcher_enable_widget(matcher.numeric_entry,
				    MATCH_NUMERIC(value));
	prefs_matcher_enable_widget(matcher.numeric_label,
				    (value == MATCH_SCORE));
	prefs_matcher_enable_widget(matcher.addressbook_folder_combo,
				    (value == MATCH_ABOOK));
	prefs_matcher_enable_widget(matcher.match_combo,
				    MATCH_COMBO_IS_ENABLED(value));
	prefs_matcher_enable_widget(matcher.case_checkbtn,
				    MATCH_CASE_REGEXP(value));
#ifndef G_OS_WIN32
	prefs_matcher_enable_widget(matcher.regexp_checkbtn,
				    MATCH_CASE_REGEXP(value));
#endif
	prefs_matcher_enable_widget(matcher.test_btn,
				    (value == MATCH_TEST));
	prefs_matcher_enable_widget(matcher.addressbook_select_btn,
				    (value == MATCH_ABOOK));
	prefs_matcher_enable_widget(matcher.color_optmenu,
				    (value == MATCH_LABEL));
	prefs_matcher_enable_widget(matcher.upper_filler,
				    MATCH_CASE_REGEXP(value));
	prefs_matcher_enable_widget(matcher.lower_filler,
				    (value == MATCH_ABOOK));
				
	gtk_label_set_text(GTK_LABEL(matcher.match_label), "");
	gtk_entry_set_text(GTK_ENTRY(matcher.string_entry), "");

	switch(value) {
	case MATCH_ABOOK:
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.header_addr_combo), 0);	
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.addressbook_folder_combo), 0);
		prefs_matcher_set_model(matcher.match_combo2, matcher.model_found);
		gtk_label_set_text(GTK_LABEL(matcher.criteria_label2), _("in"));
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Header"));
		gtk_label_set_text(GTK_LABEL(matcher.match_label2), _("content is"));
		break;
	case MATCH_AGE:
		prefs_matcher_set_model(matcher.match_combo, matcher.model_age);
		prefs_matcher_set_model(matcher.match_combo2, matcher.model_age_units);
		gtk_spin_button_set_range(GTK_SPIN_BUTTON(
				  matcher.numeric_entry), 0, 1000);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(matcher.numeric_entry), 0);
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Age is"));
		break;
	case MATCH_FLAG:
		prefs_matcher_set_model(matcher.match_combo, matcher.model_flags);
		prefs_matcher_set_model(matcher.match_combo2, matcher.model_set);
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Flag"));
		gtk_label_set_text(GTK_LABEL(matcher.match_label2), _("is"));
		break;
	case MATCH_HEADER:
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.headers_combo), 0);
		prefs_matcher_set_model(matcher.match_combo, matcher.model_contain);
		gtk_label_set_text(GTK_LABEL(matcher.criteria_label2), _("Name:"));
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Header"));
#ifndef G_OS_WIN32
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn), FALSE);
#endif
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn), FALSE);
		break;
	case MATCH_LABEL:
		gtk_cmoption_menu_set_history(GTK_CMOPTION_MENU(matcher.color_optmenu), 0);
		prefs_matcher_set_model(matcher.match_combo2, matcher.model_set);
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Label"));
		gtk_label_set_text(GTK_LABEL(matcher.match_label2), _("is"));
		break;
	case MATCH_PARTIAL:
		prefs_matcher_set_model(matcher.criteria_combo2, matcher.model_partial);
		gtk_label_set_text(GTK_LABEL(matcher.criteria_label2), _("Value:"));
		break;
	case MATCH_PHRASE:
		prefs_matcher_set_model(matcher.criteria_combo2, matcher.model_phrase);
		prefs_matcher_set_model(matcher.match_combo, matcher.model_contain);
		gtk_label_set_text(GTK_LABEL(matcher.criteria_label2), _("in"));
#ifndef G_OS_WIN32
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn), FALSE);
#endif
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn), FALSE);
		prefs_matcher_second_criteria_sel(NULL, NULL);
		break;	
	case MATCH_SCORE:
		prefs_matcher_set_model(matcher.match_combo, matcher.model_score);
		gtk_spin_button_set_range(GTK_SPIN_BUTTON(
				  	  matcher.numeric_entry), -1000, 1000);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(matcher.numeric_entry), 0);
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Score is"));
		gtk_label_set_text(GTK_LABEL(matcher.numeric_label), _("points"));
		break;	
	case MATCH_SIZE:
		prefs_matcher_set_model(matcher.match_combo, matcher.model_size);
		prefs_matcher_set_model(matcher.match_combo2, matcher.model_size_units);
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2),
					 SIZE_UNIT_KBYTES);
		gtk_spin_button_set_range(GTK_SPIN_BUTTON(
				  	  matcher.numeric_entry), 0, 100000);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(matcher.numeric_entry), 0);
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Size is"));
		break;
	case MATCH_TAGS:
		prefs_matcher_set_model(matcher.criteria_combo2, matcher.model_tags);
		prefs_matcher_set_model(matcher.match_combo, matcher.model_contain);
		gtk_label_set_text(GTK_LABEL(matcher.criteria_label2), _("Scope:"));
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Message"));
		gtk_label_set_text(GTK_LABEL(matcher.match_label2), _("tags"));
		prefs_matcher_second_criteria_sel(NULL, NULL);
		break;
	case MATCH_THREAD:
		prefs_matcher_set_model(matcher.criteria_combo2, matcher.model_thread);
		gtk_label_set_text(GTK_LABEL(matcher.criteria_label2), _("type is"));
		break;
	case MATCH_TEST:
		prefs_matcher_set_model(matcher.match_combo, matcher.model_test);
		gtk_label_set_text(GTK_LABEL(matcher.match_label), _("Program returns"));
		break;
	}	
}

/*!
 *\brief	Handle key press
 *
 *\param	widget Widget receiving key press
 *\param	event Key event
 *\param	data User data
 */
static gboolean prefs_matcher_key_pressed(GtkWidget *widget, GdkEventKey *event,
				     gpointer data)
{
	if (event && event->keyval == GDK_Escape) {
		prefs_matcher_cancel();
		return TRUE;		
	}
	return FALSE;
}

/*!
 *\brief	Cancel matcher dialog
 */
static void prefs_matcher_cancel(void)
{
	gtk_widget_hide(matcher.window);
	gtk_window_set_modal(GTK_WINDOW(matcher.window), FALSE);
	inc_unlock();
}

/*!
 *\brief	Accept current matchers
 */
static void prefs_matcher_ok(void)
{
	MatcherList *matchers;
	MatcherProp *matcherprop;
	AlertValue val;
	gchar *matcher_str = NULL;
	gchar *str = NULL;
	gint row = 1;
	GtkTreeModel *model;
	GtkTreeIter iter;

	matchers = prefs_matcher_get_list();

	if (matchers != NULL) {
		matcherprop = prefs_matcher_dialog_to_matcher();
		if (matcherprop != NULL) {
			str = matcherprop_to_string(matcherprop);
			matcherprop_free(matcherprop);
			if (strcmp(str, "all") != 0) {
				model = gtk_tree_view_get_model(GTK_TREE_VIEW
						(matcher.cond_list_view));

				while (gtk_tree_model_iter_nth_child(model, &iter, NULL, row)) {
					gtk_tree_model_get(model, &iter,
							   PREFS_MATCHER_COND, &matcher_str,
							   -1);
					if (matcher_str && strcmp(matcher_str, str) == 0) 
						break;
					row++;
					g_free(matcher_str);
					matcher_str = NULL;
				}

				if (!matcher_str || strcmp(matcher_str, str) != 0) {
	                        	val = alertpanel(_("Entry not saved"),
       		                        	 _("The entry was not saved.\nClose anyway?"),
               		                	 GTK_STOCK_CLOSE, _("+_Continue editing"), NULL);
					if (G_ALERTDEFAULT != val) {
						g_free(matcher_str);						 
	        	                        g_free(str);
						return;
					}
				}
				g_free(matcher_str);
			}
		}
                g_free(str);
		gtk_widget_hide(matcher.window);
		gtk_window_set_modal(GTK_WINDOW(matcher.window), FALSE);
		if (matchers_callback != NULL)
			matchers_callback(matchers);
		matcherlist_free(matchers);
	}
	inc_unlock();
}

/*!
 *\brief	Called when closing dialog box
 *
 *\param	widget Dialog widget
 *\param	event Event info
 *\param	data User data
 *
 *\return	gint TRUE
 */
static gint prefs_matcher_deleted(GtkWidget *widget, GdkEventAny *event,
				  gpointer data)
{
	prefs_matcher_cancel();
	return TRUE;
}

/*
 * Strings describing test format strings
 * 
 * When adding new lines, remember to put 2 strings for each line
 */
static gchar *test_desc_strings[] = {
	"%%",	N_("literal %"),
	"%s",	N_("Subject"),
	"%f",	N_("From"),
	"%t",	N_("To"),
	"%c",	N_("Cc"),
	"%d",	N_("Date"),
	"%i",	N_("Message-ID"),
	"%n",	N_("Newsgroups"),
	"%r",	N_("References"),
	"%F",	N_("filename (should not be modified)"),
	"\\n",	N_("new line"),
	"\\",	N_("escape character for quotes"),
	"\\\"", N_("quote character"),
	NULL,   NULL
};

static DescriptionWindow test_desc_win = { 
	NULL,
        NULL, 
        2,
        N_("Match Type: 'Test'"),
	N_("'Test' allows you to test a message or message element "
	   "using an external program or script. The program will "
	   "return either 0 or 1.\n\n"
	   "The following symbols can be used:"),
        test_desc_strings
};



/*!
 *\brief	Show Test action's info
 */
static void prefs_matcher_test_info(GtkWidget *widget, GtkWidget *parent)
{
	test_desc_win.parent = parent;
	description_window_create(&test_desc_win);
}

static void prefs_matcher_addressbook_select(void)
{
	const gchar *folderpath = NULL;
	gchar *new_path = NULL;

	folderpath = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((matcher.addressbook_folder_combo)))));
	new_path = addressbook_folder_selection(folderpath);
	if (new_path) {
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((matcher.addressbook_folder_combo)))), new_path);
		g_free(new_path);
	} 
}


/*
 * list view
 */

static GtkListStore* prefs_matcher_create_data_store(void)
{
	return gtk_list_store_new(N_PREFS_MATCHER_COLUMNS,
				  G_TYPE_STRING,	
				  G_TYPE_BOOLEAN,
				  -1);
}

static void prefs_matcher_list_view_insert_matcher(GtkWidget *list_view,
						   GtkTreeIter *row_iter,
						   const gchar *matcher,
						   gboolean is_valid) 
{
	GtkTreeIter iter;
	GtkListStore *list_store = GTK_LIST_STORE(gtk_tree_view_get_model
					(GTK_TREE_VIEW(list_view)));

	if (row_iter == NULL) {
		/* append new */
		gtk_list_store_append(list_store, &iter);
	} else {
		/* change existing */
		iter = *row_iter;
	}

	gtk_list_store_set(list_store, &iter,
			   PREFS_MATCHER_COND, matcher,
			   PREFS_MATCHER_COND_VALID, is_valid,
			   -1);
}

static GtkWidget *prefs_matcher_list_view_create(void)
{
	GtkTreeView *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(prefs_matcher_create_data_store());
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(model));
	g_object_unref(model);	
	
	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	gtk_tree_view_set_reorderable(list_view, TRUE);
	
	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);
	gtk_tree_selection_set_select_function(selector, prefs_matcher_selected,
					       NULL, NULL);

	/* create the columns */
	prefs_matcher_create_list_view_columns(GTK_WIDGET(list_view));

	return GTK_WIDGET(list_view);
}

static void prefs_matcher_create_list_view_columns(GtkWidget *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Current condition rules"),
		 renderer,
		 "text", PREFS_MATCHER_COND,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
}

static void prefs_matcher_set_criteria(const gint criteria)
{
	gint match_criteria = 0;
	
	switch (criteria) {
	case CRITERIA_FOUND_IN_ADDRESSBOOK:
		match_criteria = MATCH_ABOOK;
		break;	
	case CRITERIA_ALL:
		match_criteria = MATCH_ALL;
		break;
	case CRITERIA_AGE_GREATER:
	case CRITERIA_AGE_LOWER:
		match_criteria = MATCH_AGE;
		break;
	case CRITERIA_SCORE_GREATER:
	case CRITERIA_SCORE_LOWER:
	case CRITERIA_SCORE_EQUAL:
		match_criteria = MATCH_SCORE;
		break;
	case CRITERIA_SIZE_GREATER:
	case CRITERIA_SIZE_SMALLER:
	case CRITERIA_SIZE_EQUAL:
		match_criteria = MATCH_SIZE;
		break;
	case CRITERIA_SUBJECT:
	case CRITERIA_FROM:
	case CRITERIA_TO:
	case CRITERIA_CC:
	case CRITERIA_TO_OR_CC:
	case CRITERIA_NEWSGROUPS:
	case CRITERIA_INREPLYTO:
	case CRITERIA_REFERENCES:
	case CRITERIA_HEADER:
		match_criteria = MATCH_HEADER;
		break;
	case CRITERIA_HEADERS_PART:
	case CRITERIA_BODY_PART:
	case CRITERIA_MESSAGE:
		match_criteria = MATCH_PHRASE;
		break;
	case CRITERIA_TEST:
		match_criteria = MATCH_TEST;
		break;
	case CRITERIA_COLORLABEL:
		match_criteria = MATCH_LABEL;
		break;
	case CRITERIA_TAG:
	case CRITERIA_TAGGED:
		match_criteria = MATCH_TAGS;
		break;
	case CRITERIA_UNREAD:
	case CRITERIA_NEW:
	case CRITERIA_MARKED:
	case CRITERIA_DELETED:
	case CRITERIA_REPLIED:
	case CRITERIA_FORWARDED:
	case CRITERIA_LOCKED:
	case CRITERIA_SPAM:
	case CRITERIA_HAS_ATTACHMENT:
	case CRITERIA_SIGNED:
		match_criteria = MATCH_FLAG;
		break;
	case CRITERIA_PARTIAL:
		match_criteria = MATCH_PARTIAL;
		break;
	case CRITERIA_IGNORE_THREAD:
	case CRITERIA_WATCH_THREAD:
		match_criteria = MATCH_THREAD;
		break;
	}
	
	gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.criteria_combo),
				 match_criteria);
	
	switch(match_criteria) {
	case MATCH_HEADER:
		if(criteria != CRITERIA_HEADER)
			combobox_select_by_data(GTK_COMBO_BOX(
						matcher.headers_combo),
						criteria);
		break;
	case MATCH_AGE:
	case MATCH_SCORE:
	case MATCH_SIZE:
	case MATCH_FLAG:
		combobox_select_by_data(GTK_COMBO_BOX(
					matcher.match_combo), criteria);
		break;
	case MATCH_PHRASE:
	case MATCH_TAGS:
		combobox_select_by_data(GTK_COMBO_BOX(
					matcher.criteria_combo2), criteria);
		break;
	}
}

static gboolean prefs_matcher_selected(GtkTreeSelection *selector,
				       GtkTreeModel *model, 
				       GtkTreePath *path,
				       gboolean currently_selected,
				       gpointer data)
{
	gchar *matcher_str;
	MatcherProp *prop;
	gboolean negative_cond;
	gint criteria;
	GtkWidget *menu;
	GtkTreeIter iter;
	gboolean is_valid;

	if (currently_selected)
		return TRUE;

	if (!gtk_tree_model_get_iter(model, &iter, path))
		return TRUE;

	gtk_tree_model_get(model, &iter, 
			   PREFS_MATCHER_COND_VALID,  &is_valid,
			   PREFS_MATCHER_COND, &matcher_str,
			   -1);
	
	if (!is_valid) {
		g_free(matcher_str);
		prefs_matcher_reset_condition();
		return TRUE;
	}

	negative_cond = FALSE;

	prop = matcher_parser_get_prop(matcher_str);
	if (prop == NULL) {
		g_free(matcher_str);
		return TRUE;
	}		

	criteria = prefs_matcher_get_criteria_from_matching(prop->criteria);
	prefs_matcher_set_criteria(criteria);

	switch(prop->criteria) {
	case MATCHCRITERIA_NOT_UNREAD:
	case MATCHCRITERIA_NOT_NEW:
	case MATCHCRITERIA_NOT_MARKED:
	case MATCHCRITERIA_NOT_DELETED:
	case MATCHCRITERIA_NOT_REPLIED:
	case MATCHCRITERIA_NOT_FORWARDED:
	case MATCHCRITERIA_NOT_LOCKED:
	case MATCHCRITERIA_NOT_SPAM:
	case MATCHCRITERIA_HAS_NO_ATTACHMENT:
	case MATCHCRITERIA_NOT_SIGNED:
	case MATCHCRITERIA_NOT_PARTIAL:
	case MATCHCRITERIA_NOT_COLORLABEL:
	case MATCHCRITERIA_NOT_IGNORE_THREAD:
	case MATCHCRITERIA_NOT_WATCH_THREAD:
	case MATCHCRITERIA_NOT_SUBJECT:
	case MATCHCRITERIA_NOT_FROM:
	case MATCHCRITERIA_NOT_TO:
	case MATCHCRITERIA_NOT_CC:
	case MATCHCRITERIA_NOT_TO_AND_NOT_CC:
	case MATCHCRITERIA_NOT_TAG:
	case MATCHCRITERIA_NOT_TAGGED:
	case MATCHCRITERIA_NOT_NEWSGROUPS:
	case MATCHCRITERIA_NOT_INREPLYTO:
	case MATCHCRITERIA_NOT_REFERENCES:
	case MATCHCRITERIA_NOT_HEADER:
	case MATCHCRITERIA_NOT_HEADERS_PART:
	case MATCHCRITERIA_NOT_MESSAGE:
	case MATCHCRITERIA_NOT_BODY_PART:
	case MATCHCRITERIA_NOT_TEST:
	case MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK:
		negative_cond = TRUE;
		break;
	}
	
	switch(prop->criteria) {
	case MATCHCRITERIA_ALL:
		break;

	case MATCHCRITERIA_NOT_SUBJECT:
	case MATCHCRITERIA_NOT_FROM:
	case MATCHCRITERIA_NOT_TO:
	case MATCHCRITERIA_NOT_CC:
	case MATCHCRITERIA_NOT_TO_AND_NOT_CC:
	case MATCHCRITERIA_NOT_TAG:
	case MATCHCRITERIA_NOT_NEWSGROUPS:
	case MATCHCRITERIA_NOT_INREPLYTO:
	case MATCHCRITERIA_NOT_REFERENCES:
	case MATCHCRITERIA_NOT_HEADERS_PART:
	case MATCHCRITERIA_NOT_BODY_PART:
	case MATCHCRITERIA_NOT_MESSAGE:
	case MATCHCRITERIA_NOT_TEST:
	case MATCHCRITERIA_SUBJECT:
	case MATCHCRITERIA_FROM:
	case MATCHCRITERIA_TO:
	case MATCHCRITERIA_CC:
	case MATCHCRITERIA_TO_OR_CC:
	case MATCHCRITERIA_TAG:
	case MATCHCRITERIA_NEWSGROUPS:
	case MATCHCRITERIA_INREPLYTO:
	case MATCHCRITERIA_REFERENCES:
	case MATCHCRITERIA_HEADERS_PART:
	case MATCHCRITERIA_BODY_PART:
	case MATCHCRITERIA_MESSAGE:
	case MATCHCRITERIA_TEST:
		gtk_entry_set_text(GTK_ENTRY(matcher.string_entry), prop->expr);
		break;

	case MATCHCRITERIA_FOUND_IN_ADDRESSBOOK:
	case MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK:
	{
		gchar *header;
		gchar *expr;

		/* matcher expressions contain UNtranslated "Any"/"All",
		  select the relevant translated combo item */
		if (strcasecmp(prop->header, "All") == 0)
			header = (gchar*)Q_("Filtering Matcher Menu|All");
		else
			if (strcasecmp(prop->header, "Any") == 0)
				header = _("Any");
			else
				header = prop->header;
		if (strcasecmp(prop->expr, "Any") == 0)
			expr = _("Any");
		else
			expr = prop->expr;

		gtk_entry_set_text(GTK_ENTRY(matcher.header_addr_entry), header);
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((matcher.addressbook_folder_combo)))), expr);
		break;
	}

	case MATCHCRITERIA_AGE_GREATER:
	case MATCHCRITERIA_AGE_LOWER:
		if(prop->value >= 7 && !(prop->value % 7)) {
			gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2),
						 AGE_WEEKS);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(
					matcher.numeric_entry), prop->value/7);
		} else {
			gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2),
						 AGE_DAYS);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(
					matcher.numeric_entry), prop->value);
		}
		break;
		
	case MATCHCRITERIA_SCORE_GREATER:
	case MATCHCRITERIA_SCORE_LOWER:
	case MATCHCRITERIA_SCORE_EQUAL:
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(matcher.numeric_entry),
					  prop->value);
		break;

	case MATCHCRITERIA_SIZE_GREATER:
	case MATCHCRITERIA_SIZE_SMALLER:
	case MATCHCRITERIA_SIZE_EQUAL:
		if(prop->value >= MB_SIZE && !(prop->value % MB_SIZE)) {
			gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2),
						 SIZE_UNIT_MBYTES);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(
					matcher.numeric_entry), prop->value/MB_SIZE);
		} else if(prop->value >= KB_SIZE && !(prop->value % KB_SIZE)) {
			gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2),
						 SIZE_UNIT_KBYTES);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(
					matcher.numeric_entry), prop->value/KB_SIZE);
		} else {
			gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2),
						 SIZE_UNIT_BYTES);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(
					matcher.numeric_entry), prop->value);		
		}
		break;

	case MATCHCRITERIA_NOT_COLORLABEL:
	case MATCHCRITERIA_COLORLABEL:
		gtk_cmoption_menu_set_history(GTK_CMOPTION_MENU(matcher.color_optmenu),
					    prop->value + 1);
		menu = gtk_cmoption_menu_get_menu(GTK_CMOPTION_MENU(matcher.color_optmenu));
		g_signal_emit_by_name(G_OBJECT(menu), "selection-done", menu);
		break;

	case MATCHCRITERIA_NOT_HEADER:
	case MATCHCRITERIA_HEADER:
		gtk_entry_set_text(GTK_ENTRY(matcher.header_entry), prop->header);
		gtk_entry_set_text(GTK_ENTRY(matcher.string_entry), prop->expr);
		break;
	}

	switch(criteria) {
	case CRITERIA_SUBJECT:
	case CRITERIA_FROM:
	case CRITERIA_TO:
	case CRITERIA_CC:
	case CRITERIA_TO_OR_CC:
	case CRITERIA_NEWSGROUPS:
	case CRITERIA_INREPLYTO:
	case CRITERIA_REFERENCES:
	case CRITERIA_HEADER:
	case CRITERIA_HEADERS_PART:
	case CRITERIA_BODY_PART:
	case CRITERIA_MESSAGE:
	case CRITERIA_TAG:
	case CRITERIA_TAGGED:
	case CRITERIA_TEST:
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo),
					negative_cond ? PREDICATE_DOES_NOT_CONTAIN :
							PREDICATE_CONTAINS);
		break;
	case CRITERIA_FOUND_IN_ADDRESSBOOK:
	case CRITERIA_UNREAD:
	case CRITERIA_NEW:
	case CRITERIA_MARKED:
	case CRITERIA_DELETED:
	case CRITERIA_REPLIED:
	case CRITERIA_FORWARDED:
	case CRITERIA_LOCKED:
	case CRITERIA_SPAM:
	case CRITERIA_HAS_ATTACHMENT:
	case CRITERIA_SIGNED:
	case CRITERIA_COLORLABEL:
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.match_combo2),
					 negative_cond ? PREDICATE_FLAG_DISABLED :
					 		 PREDICATE_FLAG_ENABLED);
		break;
	case CRITERIA_WATCH_THREAD:
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.criteria_combo2),
					 negative_cond ? THREAD_NOT_WATCHED :
					 		 THREAD_WATCHED);
		break;
	case CRITERIA_IGNORE_THREAD:
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.criteria_combo2),
					 negative_cond ? THREAD_NOT_IGNORED :
					 		 THREAD_IGNORED);
		break;	
	case CRITERIA_PARTIAL:
		gtk_combo_box_set_active(GTK_COMBO_BOX(matcher.criteria_combo2),
					 negative_cond ? PREDICATE_FLAG_DISABLED :
					 		 PREDICATE_FLAG_ENABLED);
		break;
	}

	switch(prop->matchtype) {
	case MATCHTYPE_MATCH:
#ifndef G_OS_WIN32
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn), FALSE);
#endif
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn), TRUE);
		break;

	case MATCHTYPE_MATCHCASE:
#ifndef G_OS_WIN32
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn), FALSE);
#endif
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn), FALSE);
		break;

	case MATCHTYPE_REGEXP:
#ifndef G_OS_WIN32
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn), TRUE);
#endif
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn), TRUE);
		break;

	case MATCHTYPE_REGEXPCASE:
#ifndef G_OS_WIN32
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.regexp_checkbtn), TRUE);
#endif
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(matcher.case_checkbtn), FALSE);
		break;
	}

	g_free(matcher_str);
	return TRUE;
}

