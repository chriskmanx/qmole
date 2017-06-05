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
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "main.h"
#include "message_search.h"
#include "messageview.h"
#include "compose.h"
#include "utils.h"
#include "gtkutils.h"
#include "combobox.h"
#include "manage_window.h"
#include "alertpanel.h"
#include "manual.h"
#include "prefs_common.h"

static struct MessageSearchWindow {
	GtkWidget *window;
	GtkWidget *body_entry;
	GtkWidget *case_checkbtn;
	GtkWidget *help_btn;
	GtkWidget *prev_btn;
	GtkWidget *next_btn;
	GtkWidget *close_btn;
	GtkWidget *stop_btn;

	MessageView *messageview;

	Compose *compose;
	gboolean search_compose;
	gboolean is_searching;
	gboolean body_entry_has_focus;
} search_window;

static void message_search_create	(void);
static void message_search_execute	(gboolean	 backward);

static void message_search_prev_clicked	(GtkButton	*button,
					 gpointer	 data);
static void message_search_next_clicked	(GtkButton	*button,
					 gpointer	 data);
static void message_search_stop_clicked	(GtkButton	*button,
					 gpointer	 data);
static void body_changed		(void);
static gboolean body_entry_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean body_entry_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  gpointer data);
static gboolean key_pressed		(GtkWidget	*widget,
					 GdkEventKey	*event,
					 gpointer	 data);


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

static void message_show_stop_button(void)
{
	gtk_widget_hide(search_window.close_btn);
	gtk_widget_show(search_window.stop_btn);
	GTK_BUTTON_SET_SENSITIVE(search_window.prev_btn, FALSE)
	GTK_BUTTON_SET_SENSITIVE(search_window.next_btn, FALSE)
}

static void message_hide_stop_button(void)
{
	gtk_widget_hide(search_window.stop_btn);
	gtk_widget_show(search_window.close_btn);
	gtk_widget_set_sensitive(search_window.prev_btn, TRUE);
	gtk_widget_set_sensitive(search_window.next_btn, TRUE);
}

void message_search(MessageView *messageview)
{
	if (!search_window.window)
		message_search_create();
	else
		gtk_widget_hide(search_window.window);

	search_window.messageview = messageview;
	search_window.search_compose = FALSE;

	gtk_widget_grab_focus(search_window.next_btn);
	gtk_widget_grab_focus(search_window.body_entry);
	gtk_widget_show(search_window.window);
}

void message_search_compose(Compose *compose)
{
	if (!search_window.window)
		message_search_create();
	else
		gtk_widget_hide(search_window.window);

	search_window.compose = compose;
	search_window.search_compose = TRUE;

	gtk_widget_grab_focus(search_window.next_btn);
	gtk_widget_grab_focus(search_window.body_entry);
	gtk_widget_show(search_window.window);
}

static void message_search_create(void)
{
	GtkWidget *window;

	GtkWidget *vbox1;
	GtkWidget *hbox1;
	GtkWidget *body_label;
	GtkWidget *body_entry;

	GtkWidget *checkbtn_hbox;
	GtkWidget *case_checkbtn;

	GtkWidget *confirm_area;
	GtkWidget *help_btn;
	GtkWidget *prev_btn;
	GtkWidget *next_btn;
	GtkWidget *close_btn;
	GtkWidget *stop_btn;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "message_search");
	gtk_window_set_title (GTK_WINDOW (window),
			      _("Find in current message"));
	gtk_widget_set_size_request (window, 450, -1);
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);
	gtk_container_set_border_width (GTK_CONTAINER (window), 8);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(gtk_widget_hide_on_delete), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);

	vbox1 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox1);
	gtk_container_add (GTK_CONTAINER (window), vbox1);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, TRUE, TRUE, 0);

	body_label = gtk_label_new (_("Find text:"));
	gtk_widget_show (body_label);
	gtk_box_pack_start (GTK_BOX (hbox1), body_label, FALSE, FALSE, 0);

	body_entry = gtk_combo_box_entry_new_text ();
	gtk_combo_box_set_active(GTK_COMBO_BOX(body_entry), -1);
	if (prefs_common.message_search_history)
		combobox_set_popdown_strings(GTK_COMBO_BOX(body_entry),
				prefs_common.message_search_history);
	gtk_widget_show (body_entry);
	gtk_box_pack_start (GTK_BOX (hbox1), body_entry, TRUE, TRUE, 0);
	g_signal_connect(G_OBJECT(body_entry), "changed",
			 G_CALLBACK(body_changed), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((body_entry)))),
			 "focus_in_event", G_CALLBACK(body_entry_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((body_entry)))),
			 "focus_out_event", G_CALLBACK(body_entry_focus_evt_out), NULL);

	checkbtn_hbox = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (checkbtn_hbox);
	gtk_box_pack_start (GTK_BOX (vbox1), checkbtn_hbox, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (checkbtn_hbox), 8);

	case_checkbtn = gtk_check_button_new_with_label (_("Case sensitive"));
	gtk_widget_show (case_checkbtn);
	gtk_box_pack_start (GTK_BOX (checkbtn_hbox), case_checkbtn,
			    FALSE, FALSE, 0);

	confirm_area = gtk_hbutton_box_new();
	gtk_widget_show (confirm_area);
	gtk_button_box_set_layout(GTK_BUTTON_BOX(confirm_area),
				  GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(confirm_area), 5);

	gtkut_stock_button_add_help(confirm_area, &help_btn);

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

	gtk_widget_show (confirm_area);
	gtk_box_pack_start (GTK_BOX (vbox1), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(next_btn);

	g_signal_connect(G_OBJECT(help_btn), "clicked",
			 G_CALLBACK(manual_open_with_anchor_cb),
			 MANUAL_ANCHOR_SEARCHING);
	g_signal_connect(G_OBJECT(prev_btn), "clicked",
			 G_CALLBACK(message_search_prev_clicked), NULL);
	g_signal_connect(G_OBJECT(next_btn), "clicked",
			 G_CALLBACK(message_search_next_clicked), NULL);
	g_signal_connect_closure
		(G_OBJECT(close_btn), "clicked",
		 g_cclosure_new_swap(G_CALLBACK(gtk_widget_hide),
				     window, NULL),
		 FALSE);
	g_signal_connect(G_OBJECT(stop_btn), "clicked",
			 G_CALLBACK(message_search_stop_clicked), NULL);

	search_window.window = window;
	search_window.body_entry = body_entry;
	search_window.case_checkbtn = case_checkbtn;
	search_window.help_btn = help_btn;
	search_window.prev_btn = prev_btn;
	search_window.next_btn = next_btn;
	search_window.close_btn = close_btn;
	search_window.stop_btn = stop_btn;
}

static void message_search_execute(gboolean backward)
{
	MessageView *messageview = search_window.messageview;
	Compose *compose = search_window.compose;
	gboolean case_sens;
	gboolean all_searched = FALSE;
	gchar *body_str;

	body_str = gtk_combo_box_get_active_text(GTK_COMBO_BOX(search_window.body_entry));
	if (!body_str)
		body_str = gtk_editable_get_chars(
				GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(search_window.body_entry))),0,-1);
	if (!body_str || *body_str == '\0') return;

	/* add to history */
	combobox_unset_popdown_strings(GTK_COMBO_BOX(search_window.body_entry));
	prefs_common.message_search_history = add_history(
			prefs_common.message_search_history, body_str);
	combobox_set_popdown_strings(GTK_COMBO_BOX(search_window.body_entry),
			prefs_common.message_search_history);

	case_sens = gtk_toggle_button_get_active
		(GTK_TOGGLE_BUTTON(search_window.case_checkbtn));

	search_window.is_searching = TRUE;
	message_show_stop_button();

	for (; search_window.is_searching;) {
		gchar *str;
		AlertValue val;

		if (backward) {
			if (search_window.search_compose) {
				if (compose_search_string_backward
					(compose, body_str, case_sens) == TRUE)
					break;
			} else {
				if (messageview_search_string_backward
					(messageview, body_str, case_sens) == TRUE)
					break;
			}
		} else {
			if (search_window.search_compose) {
				if (compose_search_string
					(compose, body_str, case_sens) == TRUE)
					break;
			} else {
				if (messageview_search_string
					(messageview, body_str, case_sens) == TRUE)
					break;
			}
		}

		if (all_searched) {
			alertpanel_full(_("Search failed"),
					_("Search string not found."),
				       	 GTK_STOCK_CLOSE, NULL, NULL, FALSE,
				       	 NULL, ALERT_WARNING, G_ALERTDEFAULT);
			break;
		}

		all_searched = TRUE;

		if (backward)
			str = _("Beginning of message reached; "
				"continue from end?");
		else
			str = _("End of message reached; "
				"continue from beginning?");

		val = alertpanel(_("Search finished"), str,
				 GTK_STOCK_NO, "+" GTK_STOCK_YES, NULL);
		if (G_ALERTALTERNATE == val) {
			manage_window_focus_in(search_window.window,
					       NULL, NULL);
			if (search_window.search_compose) {
				compose_set_position(compose,
							 backward ? -1 : 0);
			} else {
				messageview_set_position(messageview,
							 backward ? -1 : 0);
			}
		} else
			break;
	}

	search_window.is_searching = FALSE;
	message_hide_stop_button();
	g_free(body_str);
}

static void body_changed(void)
{
	if (!search_window.body_entry_has_focus)
		gtk_widget_grab_focus(search_window.body_entry);
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

static void message_search_prev_clicked(GtkButton *button, gpointer data)
{
	message_search_execute(TRUE);
}

static void message_search_next_clicked(GtkButton *button, gpointer data)
{
	message_search_execute(FALSE);
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event,
			    gpointer data)
{
	if (event && (event->keyval == GDK_KEY_Escape)) {
		gtk_widget_hide(search_window.window);
	}

	if (event && (event->keyval == GDK_KEY_Return || event->keyval == GDK_KEY_KP_Enter)) {
		message_search_execute(FALSE);
	}

	if (event && (event->keyval == GDK_KEY_Down || event->keyval == GDK_KEY_Up)) {
		if (search_window.body_entry_has_focus) {
			combobox_set_value_from_arrow_key(
					GTK_COMBO_BOX(search_window.body_entry),
					event->keyval);
			return TRUE;
		}
	}

	return FALSE;
}

static void message_search_stop_clicked(GtkButton *button, gpointer data)
{
	search_window.is_searching = FALSE;
}
