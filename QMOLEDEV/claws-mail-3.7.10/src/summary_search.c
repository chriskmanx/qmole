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
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include "gtk/gtksctree.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "main.h"
#include "summary_search.h"
#include "summaryview.h"
#include "messageview.h"
#include "mainwindow.h"
#include "menu.h"
#include "utils.h"
#include "gtkutils.h"
#include "combobox.h"
#include "prefs_gtk.h"
#include "manage_window.h"
#include "alertpanel.h"
#include "matcher.h"
#include "matcher_parser.h"
#include "prefs_matcher.h"
#include "manual.h"
#include "prefs_common.h"

static struct SummarySearchWindow {
	GtkWidget *window;

	GtkWidget *bool_optmenu;

	GtkWidget *from_entry;
	GtkWidget *to_entry;
	GtkWidget *subject_entry;
	GtkWidget *body_entry;

	GtkWidget *adv_condition_entry;
	GtkWidget *adv_condition_btn;
	GtkWidget *adv_search_checkbtn;

	GtkWidget *case_checkbtn;

	GtkWidget *clear_btn;
	GtkWidget *help_btn;
	GtkWidget *all_btn;
	GtkWidget *prev_btn;
	GtkWidget *next_btn;
	GtkWidget *close_btn;
	GtkWidget *stop_btn;

	SummaryView *summaryview;

	MatcherList			*matcher_list;

	gboolean is_searching;
	gboolean from_entry_has_focus;
	gboolean to_entry_has_focus;
	gboolean subject_entry_has_focus;
	gboolean body_entry_has_focus;
	gboolean adv_condition_entry_has_focus;
} search_window;

static void summary_search_create	(void);

static void summary_search_execute	(gboolean	 backward,
					 gboolean	 search_all);

static void summary_search_clear	(GtkButton	*button,
					 gpointer	 data);
static void summary_search_prev_clicked	(GtkButton	*button,
					 gpointer	 data);
static void summary_search_next_clicked	(GtkButton	*button,
					 gpointer	 data);
static void summary_search_all_clicked	(GtkButton	*button,
					 gpointer	 data);
static void summary_search_stop_clicked	(GtkButton	*button,
					 gpointer	 data);
static void adv_condition_btn_clicked	(GtkButton	*button,
					 gpointer	 data);

static void from_changed			(void);
static void to_changed				(void);
static void subject_changed			(void);
static void body_changed			(void);
static void adv_condition_changed	(void);

static gboolean from_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean from_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean to_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean to_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean subject_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean subject_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean body_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean body_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean adv_condition_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean adv_condition_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
#ifndef MAEMO
static gboolean key_pressed		(GtkWidget	*widget,
					 GdkEventKey	*event,
					 gpointer	 data);
#endif

#if !GTK_CHECK_VERSION(2,14,0)
/* Work around http://bugzilla.gnome.org/show_bug.cgi?id=56070 */
#define GTK_BUTTON_SET_SENSITIVE(widget,sensitive) {					\
	gboolean in_btn = FALSE;							\
	if (GTK_IS_BUTTON(widget))							\
		in_btn = GTK_BUTTON(widget)->in_button;					\
	gtk_widget_set_sensitive(widget, sensitive);					\
	if (GTK_IS_BUTTON(widget))							\
		GTK_BUTTON(widget)->in_button = in_btn;					\
}
#else
#define GTK_BUTTON_SET_SENSITIVE(widget,sensitive) {					\
	gtk_widget_set_sensitive(widget, sensitive);					\
}
#endif

void summary_search(SummaryView *summaryview)
{
	if (!search_window.window) {
		summary_search_create();
	} else {
		gtk_widget_hide(search_window.window);
	}

	search_window.summaryview = summaryview;

	gtk_widget_grab_focus(search_window.next_btn);
	gtk_widget_grab_focus(search_window.subject_entry);
	gtk_widget_show(search_window.window);
}

static void summary_show_stop_button(void)
{
	gtk_widget_hide(search_window.close_btn);
	gtk_widget_show(search_window.stop_btn);
	GTK_BUTTON_SET_SENSITIVE(search_window.all_btn, FALSE)
	GTK_BUTTON_SET_SENSITIVE(search_window.prev_btn, FALSE)
	GTK_BUTTON_SET_SENSITIVE(search_window.next_btn, FALSE)
}

static void summary_hide_stop_button(void)
{
	gtk_widget_hide(search_window.stop_btn);
	gtk_widget_show(search_window.close_btn);
	gtk_widget_set_sensitive(search_window.all_btn, TRUE);
	gtk_widget_set_sensitive(search_window.prev_btn, TRUE);
	gtk_widget_set_sensitive(search_window.next_btn, TRUE);
}

static void summary_search_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox1;
	GtkWidget *bool_hbox;
	GtkWidget *bool_optmenu;
	GtkListStore *menu;
	GtkTreeIter iter;
	GtkWidget *clear_btn;

	GtkWidget *table1;
	GtkWidget *from_label;
	GtkWidget *from_entry;
	GtkWidget *to_label;
	GtkWidget *to_entry;
	GtkWidget *subject_label;
	GtkWidget *subject_entry;
	GtkWidget *body_label;
	GtkWidget *body_entry;
	GtkWidget *adv_condition_label;
	GtkWidget *adv_condition_entry;
	GtkWidget *adv_condition_btn;

	GtkWidget *checkbtn_hbox;
	GtkWidget *adv_search_checkbtn;
	GtkWidget *case_checkbtn;

	GtkWidget *confirm_area;
	GtkWidget *help_btn;
	GtkWidget *all_btn;
	GtkWidget *prev_btn;
	GtkWidget *next_btn;
	GtkWidget *close_btn;
	GtkWidget *stop_btn;
	gboolean is_searching = FALSE;
	CLAWS_TIP_DECL();

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "summary_search");
	gtk_window_set_title(GTK_WINDOW (window), _("Search messages"));
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);
	gtk_container_set_border_width(GTK_CONTAINER (window), 8);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(gtk_widget_hide_on_delete), NULL);
#ifdef MAEMO
	maemo_connect_key_press_to_mainwindow(GTK_WINDOW(window));
#else
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
#endif
	MANAGE_WINDOW_SIGNALS_CONNECT(window);

	vbox1 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox1);
	gtk_container_add (GTK_CONTAINER (window), vbox1);

	bool_hbox = gtk_hbox_new(FALSE, 4);
	gtk_widget_show(bool_hbox);
	gtk_box_pack_start(GTK_BOX(vbox1), bool_hbox, FALSE, FALSE, 0);

	bool_optmenu = gtkut_sc_combobox_create(NULL, FALSE);
	menu = GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(bool_optmenu)));
	gtk_widget_show(bool_optmenu);
	gtk_box_pack_start(GTK_BOX(bool_hbox), bool_optmenu, FALSE, FALSE, 0);

	COMBOBOX_ADD(menu, _("Match any of the following"), 0);
	gtk_combo_box_set_active_iter(GTK_COMBO_BOX(bool_optmenu), &iter);
	COMBOBOX_ADD(menu, _("Match all of the following"), 1);

	clear_btn = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
	gtk_widget_show(clear_btn);
	gtk_box_pack_end(GTK_BOX(bool_hbox), clear_btn, FALSE, FALSE, 0);

	table1 = gtk_table_new (5, 3, FALSE);
	gtk_widget_show (table1);
	gtk_box_pack_start (GTK_BOX (vbox1), table1, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (table1), 4);
	gtk_table_set_row_spacings (GTK_TABLE (table1), 8);
	gtk_table_set_col_spacings (GTK_TABLE (table1), 8);

	from_entry = gtk_combo_box_entry_new_text ();
	gtk_combo_box_set_active(GTK_COMBO_BOX(from_entry), -1);
	if (prefs_common.summary_search_from_history)
		combobox_set_popdown_strings(GTK_COMBO_BOX(from_entry),
				prefs_common.summary_search_from_history);
	gtk_widget_show (from_entry);
	gtk_table_attach (GTK_TABLE (table1), from_entry, 1, 3, 0, 1,
			  GTK_EXPAND|GTK_FILL, 0, 0, 0);
	g_signal_connect(G_OBJECT(from_entry), "changed",
			 G_CALLBACK(from_changed), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((from_entry)))),
			 "focus_in_event", G_CALLBACK(from_entry_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((from_entry)))),
			 "focus_out_event", G_CALLBACK(from_entry_focus_evt_out), NULL);

	to_entry = gtk_combo_box_entry_new_text ();
	gtk_combo_box_set_active(GTK_COMBO_BOX(to_entry), -1);
	if (prefs_common.summary_search_to_history)
		combobox_set_popdown_strings(GTK_COMBO_BOX(to_entry),
				prefs_common.summary_search_to_history);
	gtk_widget_show (to_entry);
	gtk_table_attach (GTK_TABLE (table1), to_entry, 1, 3, 1, 2,
			  GTK_EXPAND|GTK_FILL, 0, 0, 0);
	g_signal_connect(G_OBJECT(to_entry), "changed",
			 G_CALLBACK(to_changed), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((to_entry)))),
			 "focus_in_event", G_CALLBACK(to_entry_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((to_entry)))),
			 "focus_out_event", G_CALLBACK(to_entry_focus_evt_out), NULL);

	subject_entry = gtk_combo_box_entry_new_text ();
	gtk_combo_box_set_active(GTK_COMBO_BOX(subject_entry), -1);
	if (prefs_common.summary_search_subject_history)
		combobox_set_popdown_strings(GTK_COMBO_BOX(subject_entry),
				prefs_common.summary_search_subject_history);
	gtk_widget_show (subject_entry);
	gtk_table_attach (GTK_TABLE (table1), subject_entry, 1, 3, 2, 3,
			  GTK_EXPAND|GTK_FILL, 0, 0, 0);
	g_signal_connect(G_OBJECT(subject_entry), "changed",
			 G_CALLBACK(subject_changed), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((subject_entry)))),
			 "focus_in_event", G_CALLBACK(subject_entry_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((subject_entry)))),
			 "focus_out_event", G_CALLBACK(subject_entry_focus_evt_out), NULL);

	body_entry = gtk_combo_box_entry_new_text ();
	gtk_combo_box_set_active(GTK_COMBO_BOX(body_entry), -1);
	if (prefs_common.summary_search_body_history)
		combobox_set_popdown_strings(GTK_COMBO_BOX(body_entry),
				prefs_common.summary_search_body_history);
	gtk_widget_show (body_entry);
	gtk_table_attach (GTK_TABLE (table1), body_entry, 1, 3, 3, 4,
			  GTK_EXPAND|GTK_FILL, 0, 0, 0);
	g_signal_connect(G_OBJECT(body_entry), "changed",
			 G_CALLBACK(body_changed), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((body_entry)))),
			 "focus_in_event", G_CALLBACK(body_entry_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((body_entry)))),
			 "focus_out_event", G_CALLBACK(body_entry_focus_evt_out), NULL);

	adv_condition_entry = gtk_combo_box_entry_new_text ();
	gtk_combo_box_set_active(GTK_COMBO_BOX(adv_condition_entry), -1);
	if (prefs_common.summary_search_adv_condition_history)
		combobox_set_popdown_strings(GTK_COMBO_BOX(adv_condition_entry),
				prefs_common.summary_search_adv_condition_history);
	gtk_widget_show (adv_condition_entry);
	gtk_table_attach (GTK_TABLE (table1), adv_condition_entry, 1, 2, 4, 5,
			  GTK_EXPAND|GTK_FILL, 0, 0, 0);
	g_signal_connect(G_OBJECT(adv_condition_entry), "changed",
			 G_CALLBACK(adv_condition_changed), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((adv_condition_entry)))),
			 "focus_in_event", G_CALLBACK(adv_condition_entry_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((adv_condition_entry)))),
			 "focus_out_event", G_CALLBACK(adv_condition_entry_focus_evt_out), NULL);

	adv_condition_btn = gtk_button_new_with_label(" ... ");
	gtk_widget_show (adv_condition_btn);
	gtk_table_attach (GTK_TABLE (table1), adv_condition_btn, 2, 3, 4, 5,
			  GTK_FILL, 0, 0, 0);
	g_signal_connect(G_OBJECT (adv_condition_btn), "clicked",
			 G_CALLBACK(adv_condition_btn_clicked), search_window.window);

	CLAWS_SET_TIP(adv_condition_btn,
			     _("Edit search criteria"));

	from_label = gtk_label_new (_("From:"));
	gtk_widget_show (from_label);
	gtk_table_attach (GTK_TABLE (table1), from_label, 0, 1, 0, 1,
			  GTK_FILL, 0, 0, 0);
	gtk_label_set_justify (GTK_LABEL (from_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment (GTK_MISC (from_label), 1, 0.5);

	to_label = gtk_label_new (_("To:"));
	gtk_widget_show (to_label);
	gtk_table_attach (GTK_TABLE (table1), to_label, 0, 1, 1, 2,
			  GTK_FILL, 0, 0, 0);
	gtk_label_set_justify (GTK_LABEL (to_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment (GTK_MISC (to_label), 1, 0.5);

	subject_label = gtk_label_new (_("Subject:"));
	gtk_widget_show (subject_label);
	gtk_table_attach (GTK_TABLE (table1), subject_label, 0, 1, 2, 3,
			  GTK_FILL, 0, 0, 0);
	gtk_label_set_justify (GTK_LABEL (subject_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment (GTK_MISC (subject_label), 1, 0.5);

	body_label = gtk_label_new (_("Body:"));
	gtk_widget_show (body_label);
	gtk_table_attach (GTK_TABLE (table1), body_label, 0, 1, 3, 4,
			  GTK_FILL, 0, 0, 0);
	gtk_label_set_justify (GTK_LABEL (body_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment (GTK_MISC (body_label), 1, 0.5);

	adv_condition_label = gtk_label_new (_("Condition:"));
	gtk_widget_show (adv_condition_label);
	gtk_table_attach (GTK_TABLE (table1), adv_condition_label, 0, 1, 4, 5,
			  GTK_FILL, 0, 0, 0);
	gtk_label_set_justify (GTK_LABEL (adv_condition_label), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment (GTK_MISC (adv_condition_label), 1, 0.5);

	checkbtn_hbox = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (checkbtn_hbox);
	gtk_box_pack_start (GTK_BOX (vbox1), checkbtn_hbox, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (checkbtn_hbox), 8);

	case_checkbtn = gtk_check_button_new_with_label (_("Case sensitive"));
	gtk_widget_show (case_checkbtn);
	gtk_box_pack_start (GTK_BOX (checkbtn_hbox), case_checkbtn,
			    FALSE, FALSE, 0);

	adv_search_checkbtn = gtk_check_button_new_with_label (_("Extended Search"));
	gtk_widget_show (adv_search_checkbtn);
	gtk_box_pack_start (GTK_BOX (checkbtn_hbox), adv_search_checkbtn,
			    FALSE, FALSE, 0);

	confirm_area = gtk_hbutton_box_new();
	gtk_widget_show (confirm_area);
	gtk_button_box_set_layout(GTK_BUTTON_BOX(confirm_area),
				  GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(confirm_area), 5);

	gtkut_stock_button_add_help(confirm_area, &help_btn);

	all_btn = gtk_button_new_with_mnemonic(_("Find _all"));
	gtkut_widget_set_can_default(all_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(confirm_area), all_btn, TRUE, TRUE, 0);
	gtk_widget_show(all_btn);

	prev_btn = gtk_button_new_from_stock(GTK_STOCK_GO_BACK);
	gtkut_widget_set_can_default(prev_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(confirm_area), prev_btn, TRUE, TRUE, 0);
	gtk_widget_show(prev_btn);

	next_btn = gtk_button_new_from_stock(GTK_STOCK_GO_FORWARD);
	gtkut_widget_set_can_default(next_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(confirm_area), next_btn, TRUE, TRUE, 0);
	gtk_widget_show(next_btn);

	close_btn = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
	gtkut_widget_set_can_default(close_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(confirm_area), close_btn, TRUE, TRUE, 0);
	gtk_widget_show(close_btn);

	/* stop button hidden */
	stop_btn = gtk_button_new_from_stock(GTK_STOCK_STOP);
	gtkut_widget_set_can_default(stop_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(confirm_area), stop_btn, TRUE, TRUE, 0);

	gtk_box_pack_start (GTK_BOX (vbox1), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(next_btn);

	SET_TOGGLE_SENSITIVITY_REVERSE(adv_search_checkbtn, bool_optmenu)
	SET_TOGGLE_SENSITIVITY_REVERSE(adv_search_checkbtn, from_entry)
	SET_TOGGLE_SENSITIVITY_REVERSE(adv_search_checkbtn, to_entry)
	SET_TOGGLE_SENSITIVITY_REVERSE(adv_search_checkbtn, subject_entry)
	SET_TOGGLE_SENSITIVITY_REVERSE(adv_search_checkbtn, body_entry)
	SET_TOGGLE_SENSITIVITY(adv_search_checkbtn, adv_condition_label)
	SET_TOGGLE_SENSITIVITY(adv_search_checkbtn, adv_condition_entry)
	SET_TOGGLE_SENSITIVITY(adv_search_checkbtn, adv_condition_btn)
	SET_TOGGLE_SENSITIVITY_REVERSE(adv_search_checkbtn, case_checkbtn)

	g_signal_connect(G_OBJECT(help_btn), "clicked",
			 G_CALLBACK(manual_open_with_anchor_cb),
			 MANUAL_ANCHOR_SEARCHING);
	g_signal_connect(G_OBJECT(clear_btn), "clicked",
			 G_CALLBACK(summary_search_clear), NULL);
	g_signal_connect(G_OBJECT(all_btn), "clicked",
			 G_CALLBACK(summary_search_all_clicked), NULL);
	g_signal_connect(G_OBJECT(prev_btn), "clicked",
			 G_CALLBACK(summary_search_prev_clicked), NULL);
	g_signal_connect(G_OBJECT(next_btn), "clicked",
			 G_CALLBACK(summary_search_next_clicked), NULL);
	g_signal_connect_closure
		(G_OBJECT(close_btn), "clicked",
		 g_cclosure_new_swap(G_CALLBACK(gtk_widget_hide),
	     window, NULL), FALSE);
	g_signal_connect(G_OBJECT(stop_btn), "clicked",
			 G_CALLBACK(summary_search_stop_clicked), NULL);

	search_window.window = window;
	search_window.bool_optmenu = bool_optmenu;
	search_window.from_entry = from_entry;
	search_window.to_entry = to_entry;
	search_window.subject_entry = subject_entry;
	search_window.body_entry = body_entry;
	search_window.adv_condition_entry = adv_condition_entry;
	search_window.adv_condition_btn = adv_condition_btn;
	search_window.case_checkbtn = case_checkbtn;
	search_window.adv_search_checkbtn = adv_search_checkbtn;
	search_window.clear_btn = clear_btn;
	search_window.help_btn = help_btn;
	search_window.all_btn = all_btn;
	search_window.prev_btn = prev_btn;
	search_window.next_btn = next_btn;
	search_window.close_btn = close_btn;
	search_window.stop_btn = stop_btn;
	search_window.matcher_list = NULL;
	search_window.is_searching = is_searching;
#ifdef MAEMO
	maemo_window_full_screen_if_needed(GTK_WINDOW(search_window.window));
#endif
}

static void summary_search_execute(gboolean backward, gboolean search_all)
{
	SummaryView *summaryview = search_window.summaryview;
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	MsgInfo *msginfo;
	gboolean adv_search;
	gboolean bool_and = FALSE;
	gboolean case_sens = FALSE;
	gboolean all_searched = FALSE;
	gboolean matched = FALSE;
	gboolean body_matched = FALSE;
	gchar *from_str = NULL, *to_str = NULL, *subject_str = NULL;
	gchar *body_str = NULL;
	gchar *adv_condition = NULL;
	gboolean is_fast = TRUE;
	gint interval = 1000;
	gint i = 0;
	GSList *matchers = NULL;

	if (summary_is_locked(summaryview)) {
		return;
	}
	summary_lock(summaryview);

	adv_search = gtk_toggle_button_get_active
		(GTK_TOGGLE_BUTTON(search_window.adv_search_checkbtn));

	if (search_window.matcher_list != NULL) {
		matcherlist_free(search_window.matcher_list);
		search_window.matcher_list = NULL;
	}
	if (adv_search) {
		adv_condition = gtk_combo_box_get_active_text(GTK_COMBO_BOX(search_window.adv_condition_entry));
		if (!adv_condition)
			adv_condition = gtk_editable_get_chars(
					GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(search_window.adv_condition_entry))),0,-1);
		if (adv_condition && adv_condition[0] != '\0') {

			/* add to history */
			combobox_unset_popdown_strings(GTK_COMBO_BOX(search_window.adv_condition_entry));
			prefs_common.summary_search_adv_condition_history = add_history(
					prefs_common.summary_search_adv_condition_history, adv_condition);
			combobox_set_popdown_strings(GTK_COMBO_BOX(search_window.adv_condition_entry),
					prefs_common.summary_search_adv_condition_history);

			search_window.matcher_list = matcher_parser_get_cond((gchar*)adv_condition, &is_fast);
			if (!is_fast)
				interval = 100;
			/* TODO: check for condition parsing error and show an error dialog */
			g_free(adv_condition);
		} else {
			/* TODO: warn if no search condition? (or make buttons enabled only when
				at least one search condition has been set */
			summary_unlock(summaryview);
			return;
		}
	} else {
		bool_and = combobox_get_active_data(
				GTK_COMBO_BOX(search_window.bool_optmenu));
		case_sens = gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON(search_window.case_checkbtn));

		from_str    = gtk_combo_box_get_active_text(GTK_COMBO_BOX(search_window.from_entry));
		to_str      = gtk_combo_box_get_active_text(GTK_COMBO_BOX(search_window.to_entry));
		subject_str = gtk_combo_box_get_active_text(GTK_COMBO_BOX(search_window.subject_entry));
		body_str    = gtk_combo_box_get_active_text(GTK_COMBO_BOX(search_window.body_entry));

		if (!from_str)
			from_str = gtk_editable_get_chars(
					GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(search_window.from_entry))),0,-1);
		if (!to_str)
			to_str = gtk_editable_get_chars(
					GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(search_window.to_entry))),0,-1);
		if (!subject_str)
			subject_str = gtk_editable_get_chars(
					GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(search_window.subject_entry))),0,-1);
		if (!body_str)
			body_str = gtk_editable_get_chars(
					GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(search_window.body_entry))),0,-1);

		if (!from_str || !to_str || !subject_str || !body_str) {
			/* TODO: warn if no search criteria? (or make buttons enabled only when
 			 * at least one search criteria has been set */
			summary_unlock(summaryview);
			return;
		}
		if (	(from_str[0] == '\0') &&
				(to_str[0] == '\0') &&
				(subject_str[0] == '\0') &&
				(body_str[0] == '\0')) {
			/* TODO: warn if no search criteria? (or make buttons enabled only when
				at least one search criteria has been set */
			summary_unlock(summaryview);
			return;
		}

		/* add to history */
		if (from_str[0] != '\0') {
			MatcherProp *prop = matcherprop_new(MATCHCRITERIA_FROM,
						NULL, case_sens ? MATCHTYPE_MATCH:MATCHTYPE_MATCHCASE,
						from_str, 0);
			matchers = g_slist_append(matchers, prop);
			combobox_unset_popdown_strings(GTK_COMBO_BOX(search_window.from_entry));
			prefs_common.summary_search_from_history = add_history(
					prefs_common.summary_search_from_history, from_str);
			combobox_set_popdown_strings(GTK_COMBO_BOX(search_window.from_entry),
					prefs_common.summary_search_from_history);
		}
		if (to_str[0] != '\0') {
			MatcherProp *prop = matcherprop_new(MATCHCRITERIA_TO,
						NULL, case_sens ? MATCHTYPE_MATCH:MATCHTYPE_MATCHCASE,
						to_str, 0);
			matchers = g_slist_append(matchers, prop);
			combobox_unset_popdown_strings(GTK_COMBO_BOX(search_window.to_entry));
			prefs_common.summary_search_to_history = add_history(
					prefs_common.summary_search_to_history, to_str);
			combobox_set_popdown_strings(GTK_COMBO_BOX(search_window.to_entry),
					prefs_common.summary_search_to_history);
		}
		if (subject_str[0] != '\0') {
			MatcherProp *prop = matcherprop_new(MATCHCRITERIA_SUBJECT,
						NULL, case_sens ? MATCHTYPE_MATCH:MATCHTYPE_MATCHCASE,
						subject_str, 0);
			matchers = g_slist_append(matchers, prop);
			combobox_unset_popdown_strings(GTK_COMBO_BOX(search_window.subject_entry));
			prefs_common.summary_search_subject_history = add_history(
					prefs_common.summary_search_subject_history, subject_str);
			combobox_set_popdown_strings(GTK_COMBO_BOX(search_window.subject_entry),
					prefs_common.summary_search_subject_history);
		}
		if (body_str[0] != '\0') {
			MatcherProp *prop = matcherprop_new(MATCHCRITERIA_BODY_PART,
						NULL, case_sens ? MATCHTYPE_MATCH:MATCHTYPE_MATCHCASE,
						body_str, 0);
			matchers = g_slist_append(matchers, prop);
			combobox_unset_popdown_strings(GTK_COMBO_BOX(search_window.body_entry));
			prefs_common.summary_search_body_history = add_history(
					prefs_common.summary_search_body_history, body_str);
			combobox_set_popdown_strings(GTK_COMBO_BOX(search_window.body_entry),
					prefs_common.summary_search_body_history);
		}
		search_window.matcher_list = matcherlist_new(matchers, bool_and);
	}

	search_window.is_searching = TRUE;
	main_window_cursor_wait(summaryview->mainwin);
	summary_show_stop_button();

	if (search_all) {
		summary_freeze(summaryview);
		summary_unselect_all(summaryview);
		node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
		backward = FALSE;
	} else if (!summaryview->selected) {
		if (backward) {
			node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list_end);
		} else {
			node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
		}

		if (!node) {
			search_window.is_searching = FALSE;
			summary_hide_stop_button();
			main_window_cursor_normal(summaryview->mainwin);
			summary_unlock(summaryview);
			return;
		}
	} else {
		if (backward) {
			node = gtkut_ctree_node_prev
				(ctree, summaryview->selected);
		} else {
			node = gtkut_ctree_node_next
				(ctree, summaryview->selected);
		}
	}

	for (; search_window.is_searching; i++) {
		if (!node) {
			gchar *str;
			AlertValue val;

			if (search_all) {
				break;
			}

			if (all_searched) {
				alertpanel_full(_("Search failed"),
						_("Search string not found."),
				 		GTK_STOCK_CLOSE, NULL, NULL, FALSE,
				 		NULL, ALERT_WARNING, G_ALERTDEFAULT);
				break;
			}

			if (backward)
				str = _("Beginning of list reached; continue from end?");
			else
				str = _("End of list reached; continue from beginning?");

			val = alertpanel(_("Search finished"), str,
					 GTK_STOCK_NO, "+" GTK_STOCK_YES, NULL);
			if (G_ALERTALTERNATE == val) {
				if (backward) {
					node = GTK_CMCTREE_NODE
						(GTK_CMCLIST(ctree)->row_list_end);
				} else {
					node = GTK_CMCTREE_NODE
						(GTK_CMCLIST(ctree)->row_list);
				}

				all_searched = TRUE;

				manage_window_focus_in(search_window.window, NULL, NULL);
			} else {
				break;
			}
		}

		msginfo = gtk_cmctree_node_get_row_data(ctree, node);
		body_matched = FALSE;

		matched = matcherlist_match(search_window.matcher_list, msginfo);

		if (matched) {
			if (search_all) {
				gtk_cmctree_select(ctree, node);
			} else {
				if (messageview_is_visible
					(summaryview->messageview)) {
					summary_unlock(summaryview);
					summary_select_node
						(summaryview, node, TRUE, TRUE);
					summary_lock(summaryview);
					if (body_matched) {
						messageview_search_string
							(summaryview->messageview,
							 body_str, case_sens);
					}
				} else {
					summary_select_node
						(summaryview, node, FALSE, TRUE);
				}
				break;
			}
		}

		node = backward ? gtkut_ctree_node_prev(ctree, node)
				: gtkut_ctree_node_next(ctree, node);
		if (i % interval == 0)
			GTK_EVENTS_FLUSH();
	}

	g_free(from_str);
	g_free(to_str);
	g_free(subject_str);
	g_free(body_str);

	search_window.is_searching = FALSE;
	summary_hide_stop_button();
	main_window_cursor_normal(summaryview->mainwin);
	if (search_all) {
		summary_thaw(summaryview);
	}
	summary_unlock(summaryview);
}

static void summary_search_clear(GtkButton *button, gpointer data)
{
	if (gtk_toggle_button_get_active
		(GTK_TOGGLE_BUTTON(search_window.adv_search_checkbtn))) {
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((search_window.adv_condition_entry)))), "");
	} else {
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((search_window.from_entry)))), "");
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((search_window.to_entry)))), "");
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((search_window.subject_entry)))), "");
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((search_window.body_entry)))), "");
	}
	/* stop searching */
	if (search_window.is_searching) {
		search_window.is_searching = FALSE;
	}
}

static void summary_search_prev_clicked(GtkButton *button, gpointer data)
{
	summary_search_execute(TRUE, FALSE);
}

static void summary_search_next_clicked(GtkButton *button, gpointer data)
{
	summary_search_execute(FALSE, FALSE);
}

static void summary_search_all_clicked(GtkButton *button, gpointer data)
{
	summary_search_execute(FALSE, TRUE);
}

static void adv_condition_btn_done(MatcherList * matchers)
{
	gchar *str;

	cm_return_if_fail(
			mainwindow_get_mainwindow()->summaryview->quicksearch != NULL);

	if (matchers == NULL) {
		return;
	}

	str = matcherlist_to_string(matchers);

	if (str != NULL) {
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((search_window.adv_condition_entry)))), str);
		g_free(str);
	}
}

static void summary_search_stop_clicked(GtkButton *button, gpointer data)
{
	search_window.is_searching = FALSE;
}

static void adv_condition_btn_clicked(GtkButton *button, gpointer data)
{
	const gchar * cond_str;
	MatcherList * matchers = NULL;

	cm_return_if_fail( search_window.window != NULL );

	/* re-use the current search value if it's a condition expression,
	   otherwise ignore it silently */
	cond_str = gtk_combo_box_get_active_text(GTK_COMBO_BOX(search_window.adv_condition_entry));
	if (cond_str && *cond_str != '\0') {
		matchers = matcher_parser_get_cond((gchar*)cond_str, NULL);
	}

	prefs_matcher_open(matchers, adv_condition_btn_done);

	if (matchers != NULL) {
		matcherlist_free(matchers);
	}
};

static void from_changed(void)
{
	if (!search_window.from_entry_has_focus)
		gtk_widget_grab_focus(search_window.from_entry);
}

static void to_changed(void)
{
	if (!search_window.to_entry_has_focus)
		gtk_widget_grab_focus(search_window.to_entry);
}

static void subject_changed(void)
{
	if (!search_window.subject_entry_has_focus)
		gtk_widget_grab_focus(search_window.subject_entry);
}

static void body_changed(void)
{
	if (!search_window.body_entry_has_focus)
		gtk_widget_grab_focus(search_window.body_entry);
}

static void adv_condition_changed(void)
{
	if (!search_window.adv_condition_entry_has_focus)
		gtk_widget_grab_focus(search_window.adv_condition_entry);
}

static gboolean from_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.from_entry_has_focus = TRUE;
	return FALSE;
}

static gboolean from_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.from_entry_has_focus = FALSE;
	return FALSE;
}

static gboolean to_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.to_entry_has_focus = TRUE;
	return FALSE;
}

static gboolean to_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.to_entry_has_focus = FALSE;
	return FALSE;
}

static gboolean subject_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.subject_entry_has_focus = TRUE;
	return FALSE;
}

static gboolean subject_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.subject_entry_has_focus = FALSE;
	return FALSE;
}

static gboolean body_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.body_entry_has_focus = TRUE;
	return FALSE;
}

static gboolean body_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.body_entry_has_focus = FALSE;
	return FALSE;
}

static gboolean adv_condition_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.adv_condition_entry_has_focus = TRUE;
	return FALSE;
}

static gboolean adv_condition_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data)
{
	search_window.adv_condition_entry_has_focus = FALSE;
	return FALSE;
}
#ifndef MAEMO
static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event,
			    gpointer data)
{
	if (event && (event->keyval == GDK_Escape)) {
		/* ESC key will:
			- stop a running search
			- close the search window if no search is running
		*/
		if (!search_window.is_searching) {
			gtk_widget_hide(search_window.window);
		} else {
			search_window.is_searching = FALSE;
		}
	}

	if (event && (event->keyval == GDK_Return || event->keyval == GDK_KP_Enter)) {
		if (!search_window.is_searching) {
			summary_search_execute(FALSE, FALSE);
		}
	}

	if (event && (event->keyval == GDK_Down || event->keyval == GDK_Up)) {
		if (search_window.from_entry_has_focus) {
			combobox_set_value_from_arrow_key(
					GTK_COMBO_BOX(search_window.from_entry),
					event->keyval);
			return TRUE;
		}
		if (search_window.to_entry_has_focus) {
			combobox_set_value_from_arrow_key(
					GTK_COMBO_BOX(search_window.to_entry),
					event->keyval);
			return TRUE;
		}
		if (search_window.subject_entry_has_focus) {
			combobox_set_value_from_arrow_key(
					GTK_COMBO_BOX(search_window.subject_entry),
					event->keyval);
			return TRUE;
		}
		if (search_window.body_entry_has_focus) {
			combobox_set_value_from_arrow_key(
					GTK_COMBO_BOX(search_window.body_entry),
					event->keyval);
			return TRUE;
		}
		if (search_window.adv_condition_entry_has_focus) {
			combobox_set_value_from_arrow_key(
					GTK_COMBO_BOX(search_window.adv_condition_entry),
					event->keyval);
			return TRUE;
		}
	}

	return FALSE;
}
#endif
