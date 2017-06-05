/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Colin Leroy <colin@colino.net> 
 * and the Claws Mail team
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

#include <glib.h>
#include <glib/gi18n.h>
#include <ctype.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "gtkcmoptionmenu.h"
#include "utils.h"
#include "combobox.h"
#include "menu.h"
#include "prefs_common.h"
#include "description_window.h"
#include "matcher.h"
#include "matcher_parser.h"
#include "quicksearch.h"
#include "folderview.h"
#include "folder.h"
#include "prefs_matcher.h"
#include "claws.h"
#include "statusbar.h"

struct _QuickSearchRequest
{
	QuickSearchType			 type;
	gchar				*matchstring;
	FolderItem			*folderItem;
	gboolean			 recursive;
};
typedef struct _QuickSearchRequest QuickSearchRequest;

struct _QuickSearch
{
	GtkWidget			*hbox_search;
	GtkWidget			*search_type;
	GtkWidget			*search_type_opt;
	GtkWidget			*search_string_entry;
	GtkWidget			*search_condition_expression;
	GtkWidget			*search_description;
	GtkWidget			*clear_search;

	gboolean			 active;
	gchar				*search_string;
	MatcherList			*matcher_list;

	QuickSearchRequest		*request;
	QuickSearchExecuteCallback	 callback;
	gpointer			 callback_data;
	gboolean			 running;
	gboolean			 has_focus;
	gboolean			 matching;
	gboolean			 deferred_free;
	FolderItem			*root_folder_item;
	gboolean			 is_fast;
	gboolean			 in_typing;
	guint				 press_timeout_id;

	GList				*normal_search_strings;
	GList				*extended_search_strings;
	
	/* dynamic and autorun qs settings are exclusive*/
	GtkWidget 			 *dynamic_menuitem;
	GtkWidget 			 *autorun_menuitem;

	gboolean			gui;
};

static void quicksearch_set_running(QuickSearch *quicksearch, gboolean run);
static void quicksearch_set_matchstring(QuickSearch *quicksearch, const gchar *matchstring);
static void quicksearch_set_active(QuickSearch *quicksearch, gboolean active);
static void quicksearch_reset_folder_items(QuickSearch *quicksearch, FolderItem *folder_item);
static gchar *expand_search_string(const gchar *str);
static gchar *expand_tag_search_string(const gchar *str);

static gboolean quicksearch_from_gui(QuickSearch *quicksearch)
{
	return quicksearch->gui;
}

gboolean quicksearch_is_fast(QuickSearch *quicksearch)
{
	return quicksearch->is_fast;
}

void quicksearch_set_recursive(QuickSearch *quicksearch, gboolean recursive)
{
	quicksearch->request->recursive = recursive;
}

static void quicksearch_set_type(QuickSearch *quicksearch, gint type)
{
	gint index;
	quicksearch->request->type = type;
	if (quicksearch->gui == FALSE)
		return;
	index = menu_find_option_menu_index(GTK_CMOPTION_MENU(quicksearch->search_type_opt), 
					GINT_TO_POINTER(type),
					NULL);
	gtk_cmoption_menu_set_history(GTK_CMOPTION_MENU(quicksearch->search_type_opt), index);	
}

static gchar *quicksearch_get_text(QuickSearch * quicksearch)
{
	gchar *search_string = gtk_editable_get_chars(GTK_EDITABLE(gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry)))), 0, -1);

	g_strstrip(search_string);
	return search_string;
}

static void quicksearch_set_popdown_strings(QuickSearch *quicksearch)
{
	GtkWidget *search_string_entry = quicksearch->search_string_entry;

	combobox_unset_popdown_strings(GTK_COMBO_BOX(search_string_entry));

	if (prefs_common.summary_quicksearch_type == QUICK_SEARCH_EXTENDED)
		combobox_set_popdown_strings(GTK_COMBO_BOX(search_string_entry),
			quicksearch->extended_search_strings);	
	else
		combobox_set_popdown_strings(GTK_COMBO_BOX(search_string_entry),
			quicksearch->normal_search_strings);
}

static void prepare_matcher(QuickSearch *quicksearch)
{
	/* param search_string is "matchstring" */
	const gchar *search_string;
	QuickSearchType quicksearch_type;

	if (quicksearch == NULL)
		return;

	/* When called from the GUI, reset type and matchstring */
	if (quicksearch_from_gui(quicksearch)) {
		gchar *s = quicksearch_get_text(quicksearch);
		quicksearch_set_matchstring(quicksearch, s);
		g_free(s);
		quicksearch->request->type = prefs_common.summary_quicksearch_type;
	}
	quicksearch_type = quicksearch->request->type;
	search_string = quicksearch->request->matchstring;

	if (search_string == NULL || search_string[0] == '\0') {
		quicksearch_set_active(quicksearch, FALSE);
	}

	if (quicksearch->matcher_list != NULL) {
		if (quicksearch->matching) {
			quicksearch->deferred_free = TRUE;
			return;
		}
		quicksearch->deferred_free = FALSE;
		matcherlist_free(quicksearch->matcher_list);
		quicksearch->matcher_list = NULL;
	}

	if (search_string == NULL || search_string[0] == '\0') {
		return;
	}
	if (quicksearch_type == QUICK_SEARCH_EXTENDED) {
		char *newstr = NULL;

		newstr = expand_search_string(search_string);
		if (newstr && newstr[0] != '\0') {
			quicksearch->matcher_list = matcher_parser_get_cond(newstr, &quicksearch->is_fast);
			g_free(newstr);
		} else {
			quicksearch->matcher_list = NULL;
			quicksearch_set_active(quicksearch, FALSE);
			return;
		}
	} else if (quicksearch_type == QUICK_SEARCH_TAG) {
		char *newstr = expand_tag_search_string(search_string);
		quicksearch->matcher_list = matcher_parser_get_cond(newstr, &quicksearch->is_fast);
		g_free(newstr);
	} else if (quicksearch_type == QUICK_SEARCH_MIXED) {
		char *newstr = expand_tag_search_string(search_string);
		quicksearch->matcher_list = matcher_parser_get_cond(newstr, &quicksearch->is_fast);
		g_free(newstr);
		g_free(quicksearch->search_string);
		quicksearch->search_string = g_utf8_casefold(search_string, -1);
	} else {
		quicksearch->is_fast = TRUE;
		g_free(quicksearch->search_string);
		quicksearch->search_string = g_utf8_casefold(search_string, -1);
	}
	quicksearch_set_active(quicksearch, TRUE);
}

static void update_extended_buttons (QuickSearch *quicksearch)
{
	GtkWidget *expr_btn = quicksearch->search_condition_expression;
	GtkWidget *ext_btn = quicksearch->search_description;

	cm_return_if_fail(expr_btn != NULL);
	cm_return_if_fail(ext_btn != NULL);

	if (prefs_common.summary_quicksearch_type == QUICK_SEARCH_EXTENDED) {
		gtk_widget_show(expr_btn);
		gtk_widget_show(ext_btn);
	} else {
		gtk_widget_hide(expr_btn);
		gtk_widget_hide(ext_btn);
	}
}

static gboolean searchbar_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
			      	  QuickSearch *qs)
{
	qs->has_focus = TRUE;
	return FALSE;
}

static gboolean searchbar_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
			      	  QuickSearch *qs)
{
	qs->has_focus = FALSE;
	qs->in_typing = FALSE;
	return FALSE;
}

gboolean quicksearch_has_focus(QuickSearch *quicksearch)
{
	return quicksearch->has_focus;
}

static void searchbar_run(QuickSearch *quicksearch, gboolean run_only_if_fast)
{
	gchar *search_string = quicksearch_get_text(quicksearch);
	quicksearch_set_matchstring(quicksearch, search_string);
	prepare_matcher(quicksearch);

	/* add to history, for extended search add only correct matching rules */
	if (!quicksearch->in_typing && search_string && strlen(search_string) != 0) {
		switch (prefs_common.summary_quicksearch_type) {
			case QUICK_SEARCH_EXTENDED:
				if (quicksearch->matcher_list) {
					quicksearch->extended_search_strings =
						add_history(quicksearch->extended_search_strings,
								g_strdup(search_string));
					prefs_common.summary_quicksearch_history =
						add_history(prefs_common.summary_quicksearch_history,
								g_strdup(search_string));
				}
				break;
			default:
				quicksearch->normal_search_strings =
					add_history(quicksearch->normal_search_strings,
							g_strdup(search_string));		
				prefs_common.summary_quicksearch_history =
					add_history(prefs_common.summary_quicksearch_history,
							g_strdup(search_string));
				break;
		}

		quicksearch_set_popdown_strings(quicksearch);

	}

	if (run_only_if_fast && !quicksearch->is_fast) {
		g_free(search_string);
		return;
	}
	if (quicksearch->matcher_list == NULL && 
	    prefs_common.summary_quicksearch_type == QUICK_SEARCH_EXTENDED &&
	    search_string && strlen(search_string) != 0) {
	    	g_free(search_string);
		return;
	}
	quicksearch_set_running(quicksearch, TRUE);
	if (quicksearch->callback != NULL)
		quicksearch->callback(quicksearch, quicksearch->callback_data);
	quicksearch_set_running(quicksearch, FALSE);
	g_free(search_string);
}

static int searchbar_changed_timeout(void *data)
{
	QuickSearch *qs = (QuickSearch *)data;
	if (qs && prefs_common.summary_quicksearch_dynamic) {
		qs->in_typing = TRUE;
		searchbar_run(qs, TRUE);
	}
	return FALSE;
}

static void searchbar_changed_cb(GtkWidget *widget, QuickSearch *qs)
{
	if (!qs->has_focus && prefs_common.summary_quicksearch_autorun) {
		gtk_widget_grab_focus(qs->search_string_entry);
		searchbar_run(qs, TRUE);
		return;
	}

	if (prefs_common.summary_quicksearch_dynamic) {
		if (qs->press_timeout_id != -1) {
			g_source_remove(qs->press_timeout_id);
		}
		qs->press_timeout_id = g_timeout_add(500,
				searchbar_changed_timeout, qs);
	}

	if (!qs->has_focus)
		gtk_widget_grab_focus(qs->search_string_entry);
}

static gboolean searchbar_pressed(GtkWidget *widget, GdkEventKey *event,
			      	  QuickSearch *quicksearch)
{
	if (event && (event->keyval == GDK_Escape)) {
		gchar *str;

		quicksearch->in_typing = FALSE;

		str = quicksearch_get_text(quicksearch);
		cm_return_val_if_fail(str != NULL, TRUE);

		/* If the string entry is empty -> hide quicksearch bar. If not -> empty it */
		if (!*str) {
			summaryview_activate_quicksearch(
				mainwindow_get_mainwindow()->summaryview, 
				FALSE);
		} else {
			quicksearch_set(quicksearch, prefs_common.summary_quicksearch_type, "");
			gtk_widget_grab_focus(
					mainwindow_get_mainwindow()->summaryview->ctree);
		}
		g_free(str);
		return TRUE;
	}

	if (event != NULL && (event->keyval == GDK_Return || event->keyval == GDK_KP_Enter)) {
		if (quicksearch->press_timeout_id != -1) {
			g_source_remove(quicksearch->press_timeout_id);
			quicksearch->press_timeout_id = -1;
		}
		quicksearch->in_typing = FALSE;
		/* add expression to history list and exec quicksearch */
		searchbar_run(quicksearch, FALSE);

		g_signal_stop_emission_by_name(G_OBJECT(widget), "key_press_event");
		return TRUE;
	}

	if (event && (event->keyval == GDK_Down || event->keyval == GDK_Up)) {
		combobox_set_value_from_arrow_key(
				GTK_COMBO_BOX(quicksearch->search_string_entry),
				event->keyval);
		return TRUE;
	}

	return FALSE;
}

static gboolean searchtype_changed(GtkMenuItem *widget, gpointer data)
{
	QuickSearch *quicksearch = (QuickSearch *)data;
	gchar *search_string = quicksearch_get_text(quicksearch);
	quicksearch_set_matchstring(quicksearch, search_string);

	prefs_common.summary_quicksearch_type = GPOINTER_TO_INT(g_object_get_data(
				   G_OBJECT(GTK_MENU_ITEM(gtk_menu_get_active(
				   GTK_MENU(quicksearch->search_type)))), MENU_VAL_ID));

	/* Show extended search description button, only when Extended is selected */
	update_extended_buttons(quicksearch);
	quicksearch_set_popdown_strings(quicksearch);

	if (!search_string || *(search_string) == 0) {
		g_free(search_string);
		return TRUE;
	}

	prepare_matcher(quicksearch);

	quicksearch_set_running(quicksearch, TRUE);
	if (quicksearch->callback != NULL)
		quicksearch->callback(quicksearch, quicksearch->callback_data);
	quicksearch_set_running(quicksearch, FALSE);
	g_free(search_string);
	return TRUE;
}

static gboolean searchtype_recursive_changed(GtkMenuItem *widget, gpointer data)
{
	QuickSearch *quicksearch = (QuickSearch *)data;
	gboolean checked = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
	gchar *search_string = quicksearch_get_text(quicksearch);
	/* not needed to quicksearch_set_matchstring(search_string);
	   wait for prepare_matcher() */

	prefs_common.summary_quicksearch_recurse = checked;
	quicksearch_set_recursive(quicksearch, checked);

	/* reselect the search type */
	quicksearch_set_type(quicksearch, prefs_common.summary_quicksearch_type);

	if (!search_string || *(search_string) == 0) {
		g_free(search_string);
		return TRUE;
	}

	prepare_matcher(quicksearch);

	quicksearch_set_running(quicksearch, TRUE);
	if (quicksearch->callback != NULL)
		quicksearch->callback(quicksearch, quicksearch->callback_data);
	quicksearch_set_running(quicksearch, FALSE);
	g_free(search_string);
	return TRUE;
}

static gboolean searchtype_sticky_changed(GtkMenuItem *widget, gpointer data)
{
	QuickSearch *quicksearch = (QuickSearch *)data;
	gboolean checked = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));

	prefs_common.summary_quicksearch_sticky = checked;

	/* reselect the search type */
	quicksearch_set_type(quicksearch, prefs_common.summary_quicksearch_type);

	return TRUE;
}

static gboolean searchtype_dynamic_changed(GtkMenuItem *widget, gpointer data)
{
	QuickSearch *quicksearch = (QuickSearch *)data;
	gboolean checked = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));

	prefs_common.summary_quicksearch_dynamic = checked;
	if (checked)
		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(quicksearch->autorun_menuitem),
				FALSE);

	/* reselect the search type */
	quicksearch_set_type(quicksearch, prefs_common.summary_quicksearch_type);

	return TRUE;
}

static gboolean searchtype_autorun_changed(GtkMenuItem *widget, gpointer data)
{
	QuickSearch *quicksearch = (QuickSearch *)data;
	gboolean checked = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));

	prefs_common.summary_quicksearch_autorun = checked;
	if (checked)
		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(quicksearch->dynamic_menuitem),
				FALSE);

	/* reselect the search type */
	quicksearch_set_type(quicksearch, prefs_common.summary_quicksearch_type);

	return TRUE;
}

/*
 * Strings describing how to use Extended Search
 *
 * When adding new lines, remember to put 2 strings for each line
 */
static gchar *search_descr_strings[] = {
	"a",	 N_("all messages"),
	"ag #",  N_("messages whose age is greater than #"),
	"al #",  N_("messages whose age is less than #"),
	"b S",	 N_("messages which contain S in the message body"),
	"B S",	 N_("messages which contain S in the whole message"),
	"c S",	 N_("messages carbon-copied to S"),
	"C S",	 N_("message is either to: or cc: to S"),
	"D",	 N_("deleted messages"), /** how I can filter deleted messages **/
	"e S",	 N_("messages which contain S in the Sender field"),
	"E S",	 N_("true if execute \"S\" succeeds"),
	"f S",	 N_("messages originating from user S"),
	"F",	 N_("forwarded messages"),
	"h S",	 N_("messages which contain header S"),
	"i S",	 N_("messages which contain S in Message-ID header"),
	"I S",	 N_("messages which contain S in In-Reply-To header"),
	"k #",	 N_("messages which are marked with color #"),
	"L",	 N_("locked messages"),
	"n S",	 N_("messages which are in newsgroup S"),
	"N",	 N_("new messages"),
	"O",	 N_("old messages"),
	"p",	 N_("incomplete messages (not entirely downloaded)"),
	"r",	 N_("messages which have been replied to"),
	"R",	 N_("read messages"),
	"s S",	 N_("messages which contain S in subject"),
	"se #",  N_("messages whose score is equal to #"),
	"sg #",  N_("messages whose score is greater than #"),
	"sl #",  N_("messages whose score is lower than #"),
	"Se #",  N_("messages whose size is equal to #"),
	"Sg #",  N_("messages whose size is greater than #"),
	"Ss #",  N_("messages whose size is smaller than #"),
	"t S",	 N_("messages which have been sent to S"),
	"tg S",  N_("messages which tags contain S"),
	"tagged",N_("messages which have tag(s)"),
	"T",	 N_("marked messages"),
	"U",	 N_("unread messages"),
	"x S",	 N_("messages which contain S in References header"),
	"X \"cmd args\"", N_("messages returning 0 when passed to command - %F is message file"),
	"y S",	 N_("messages which contain S in X-Label header"),
	"",	 "" ,
	"&amp;",	 N_("logical AND operator"),
	"|",	 N_("logical OR operator"),
	"! or ~",	N_("logical NOT operator"),
	"%",	 N_("case sensitive search"),
	"",	 "" ,
	" ",	 N_("all filtering expressions are allowed"),
	NULL,	 NULL
};

static DescriptionWindow search_descr = {
	NULL,
	NULL,
	2,
	N_("Extended Search"),
	N_("Extended Search allows the user to define criteria that messages must "
           "have in order to match and be displayed in the message list.\n"
	   "The following symbols can be used:"),
	search_descr_strings
};

static void search_description_cb(GtkWidget *widget)
{
	description_window_create(&search_descr);
};

static gboolean clear_search_cb(GtkMenuItem *widget, gpointer data)
{
	QuickSearch *quicksearch = (QuickSearch *)data;

	if (!quicksearch->active)
		return TRUE;

	quicksearch_set(quicksearch, prefs_common.summary_quicksearch_type, "");

	return TRUE;
};

static void search_condition_expr_done(MatcherList * matchers)
{
	gchar *str;

	cm_return_if_fail(
			mainwindow_get_mainwindow()->summaryview->quicksearch != NULL);

	if (matchers == NULL)
		return;

	str = matcherlist_to_string(matchers);

	if (str != NULL) {
		quicksearch_set(mainwindow_get_mainwindow()->summaryview->quicksearch,
				prefs_common.summary_quicksearch_type, str);
		g_free(str);

		/* add expression to history list and exec quicksearch */
		searchbar_run(mainwindow_get_mainwindow()->summaryview->quicksearch, FALSE);
	}
}

static gboolean search_condition_expr(GtkMenuItem *widget, gpointer data)
{
	gchar * cond_str;
	MatcherList * matchers = NULL;
	
	cm_return_val_if_fail(
			mainwindow_get_mainwindow()->summaryview->quicksearch != NULL,
			FALSE);

	/* re-use the current quicksearch value if it's a condition expression,
	   otherwise ignore it silently */
	cond_str = quicksearch_get_text(mainwindow_get_mainwindow()->summaryview->quicksearch);

	if (*cond_str != '\0') {
		matchers = matcher_parser_get_cond((gchar*)cond_str, NULL);
	}

	prefs_matcher_open(matchers, search_condition_expr_done);

	if (matchers != NULL)
		matcherlist_free(matchers);

	g_free(cond_str);

	return TRUE;
};

static void quicksearch_set_button(GtkButton *button, const gchar *icon, const gchar *text)
{
	GList *children = gtk_container_get_children(GTK_CONTAINER(button));
	GList *cur;
	GtkWidget *box;
	gboolean icon_visible;

	g_object_get(gtk_settings_get_default(), 
					 "gtk-button-images", &icon_visible, 
					 NULL);

	for (cur = children; cur; cur = cur->next)
		gtk_container_remove(GTK_CONTAINER(button), GTK_WIDGET(cur->data));
	
	g_list_free(children);
	box = gtk_hbox_new(FALSE, 0);
	
	gtk_container_add(GTK_CONTAINER(button), box);
	if (icon_visible || !text || !*text)
		gtk_box_pack_start(GTK_BOX(box), gtk_image_new_from_stock(icon, 
			GTK_ICON_SIZE_BUTTON), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), gtk_label_new_with_mnemonic(text), FALSE, FALSE, 0);
	gtk_widget_show_all(box);
}

/*
 * Builds a new QuickSearchRequest
 */
static QuickSearchRequest *quicksearchrequest_new(void)
{
	QuickSearchRequest *request;
	request = g_new0(QuickSearchRequest, 1);
	return request;
}

/*
 * Builds a new QuickSearch object independent from the GUI
 */
QuickSearch *quicksearch_new_nogui(void)
{
	QuickSearch *quicksearch;
	QuickSearchRequest *request;

	request = quicksearchrequest_new();
	quicksearch = g_new0(QuickSearch, 1);
	quicksearch->request = request;
	quicksearch->gui = FALSE;

	/* init. values initally found in quicksearch_new().
	   There's no need to init. all pointers to NULL since we use g_new0
	 */
	quicksearch->matcher_list = NULL;
	quicksearch->active = FALSE;
	quicksearch->running = FALSE;
	quicksearch->in_typing = FALSE;
	quicksearch->press_timeout_id = -1;
	quicksearch->normal_search_strings = NULL;
	quicksearch->extended_search_strings = NULL;

	return quicksearch;
}

QuickSearch *quicksearch_new()
{
	QuickSearch *quicksearch;

	GtkWidget *hbox_search;
	GtkWidget *search_type_opt;
	GtkWidget *search_type;
	GtkWidget *search_string_entry;
	GtkWidget *search_hbox;
	GtkWidget *search_description;
	GtkWidget *clear_search;
	GtkWidget *search_condition_expression;
	GtkWidget *menuitem;
	CLAWS_TIP_DECL();
	GtkWidget *vbox;

	quicksearch = quicksearch_new_nogui();
	quicksearch->gui = TRUE;

	/* quick search */
	hbox_search = gtk_hbox_new(FALSE, 0);

	search_type_opt = gtk_cmoption_menu_new();
	gtk_widget_show(search_type_opt);
	gtk_box_pack_start(GTK_BOX(hbox_search), search_type_opt, FALSE, FALSE, 0);

	search_type = gtk_menu_new();
	MENUITEM_ADD (search_type, menuitem,
			prefs_common_translated_header_name("Subject"), QUICK_SEARCH_SUBJECT);
	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_changed),
			 quicksearch);
	MENUITEM_ADD (search_type, menuitem,
			prefs_common_translated_header_name("From"), QUICK_SEARCH_FROM);
	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_changed),
			 quicksearch);
	MENUITEM_ADD (search_type, menuitem,
			prefs_common_translated_header_name("To"), QUICK_SEARCH_TO);
	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_changed),
			 quicksearch);
	MENUITEM_ADD (search_type, menuitem,
			prefs_common_translated_header_name("Tag"), QUICK_SEARCH_TAG);
	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_changed),
			 quicksearch);
	MENUITEM_ADD (search_type, menuitem,
			_("From/To/Subject/Tag"), QUICK_SEARCH_MIXED);
	g_signal_connect(G_OBJECT(menuitem), "activate",
	                 G_CALLBACK(searchtype_changed),
			 quicksearch);
	MENUITEM_ADD (search_type, menuitem, _("Extended"), QUICK_SEARCH_EXTENDED);
	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_changed),
			 quicksearch);

	gtk_menu_shell_append(GTK_MENU_SHELL(search_type), gtk_separator_menu_item_new());

	menuitem = gtk_check_menu_item_new_with_label(_("Recursive"));
	gtk_menu_shell_append(GTK_MENU_SHELL(search_type), menuitem);

	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem),
					prefs_common.summary_quicksearch_recurse);
	quicksearch_set_recursive(quicksearch, prefs_common.summary_quicksearch_recurse);
	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_recursive_changed),
			 quicksearch);

	menuitem = gtk_check_menu_item_new_with_label(_("Sticky"));
	gtk_menu_shell_append(GTK_MENU_SHELL(search_type), menuitem);

	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem),
					prefs_common.summary_quicksearch_sticky);

	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_sticky_changed),
			 quicksearch);

	menuitem = gtk_check_menu_item_new_with_label(_("Type-ahead"));
	gtk_menu_shell_append(GTK_MENU_SHELL(search_type), menuitem);

	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem),
					prefs_common.summary_quicksearch_dynamic);

	quicksearch->dynamic_menuitem = menuitem;

	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_dynamic_changed),
			 quicksearch);

	menuitem = gtk_check_menu_item_new_with_label(_("Run on select"));
	gtk_menu_shell_append(GTK_MENU_SHELL(search_type), menuitem);

	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem),
					prefs_common.summary_quicksearch_autorun);

	quicksearch->autorun_menuitem = menuitem;

	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(searchtype_autorun_changed),
			 quicksearch);

	gtk_cmoption_menu_set_menu(GTK_CMOPTION_MENU(search_type_opt), search_type);

	quicksearch->search_type_opt = search_type_opt;
	quicksearch_set_type(quicksearch, prefs_common.summary_quicksearch_type);

	gtk_widget_show(search_type);

	search_string_entry = gtk_combo_box_entry_new_text ();
	gtk_combo_box_set_active(GTK_COMBO_BOX(search_string_entry), -1);

	vbox = gtk_vbox_new(TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), search_string_entry, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_search), vbox, TRUE, TRUE, 4);

	gtk_widget_show(vbox);
	gtk_widget_show(search_string_entry);

	search_hbox = gtk_hbox_new(FALSE, 5);
	clear_search = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
	gtk_box_pack_start(GTK_BOX(search_hbox), clear_search,
			   FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(clear_search), "clicked",
			 G_CALLBACK(clear_search_cb), quicksearch);
	CLAWS_SET_TIP(clear_search,
			     _("Clear the current search"));
	gtk_widget_show(clear_search);

	search_condition_expression = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	gtk_box_pack_start(GTK_BOX(search_hbox), search_condition_expression,
			   FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT (search_condition_expression), "clicked",
			 G_CALLBACK(search_condition_expr),
			 quicksearch);
	CLAWS_SET_TIP(search_condition_expression,
			     _("Edit search criteria"));
	gtk_widget_show(search_condition_expression);

	search_description = gtk_button_new_from_stock(GTK_STOCK_INFO);
	gtk_box_pack_start(GTK_BOX(search_hbox), search_description,
			   FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(search_description), "clicked",
			 G_CALLBACK(search_description_cb), NULL);
	CLAWS_SET_TIP(search_description,
			     _("Information about extended symbols"));
	gtk_widget_show(search_description);

	gtk_box_pack_start(GTK_BOX(hbox_search), search_hbox, FALSE, FALSE, 2);
	gtk_widget_show(search_hbox);

	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((search_string_entry)))),
			   "key_press_event",
			   G_CALLBACK(searchbar_pressed),
			   quicksearch);

	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((search_string_entry)))),
			 "changed",
			 G_CALLBACK(searchbar_changed_cb),
			 quicksearch);

	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((search_string_entry)))),
			 "focus_in_event",
			 G_CALLBACK(searchbar_focus_evt_in),
			 quicksearch);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((search_string_entry)))),
			 "focus_out_event",
			 G_CALLBACK(searchbar_focus_evt_out),
			 quicksearch);

	quicksearch->hbox_search = hbox_search;
	quicksearch->search_type = search_type;
	quicksearch->search_string_entry = search_string_entry;
	quicksearch->search_condition_expression = search_condition_expression;
	quicksearch->search_description = search_description;
	quicksearch->matcher_list = NULL;
	quicksearch->active = FALSE;
	quicksearch->running = FALSE;
	quicksearch->clear_search = clear_search;
	quicksearch->in_typing = FALSE;
	quicksearch->press_timeout_id = -1;
	quicksearch->normal_search_strings = NULL;
	quicksearch->extended_search_strings = NULL;

	quicksearch_set_button(GTK_BUTTON(quicksearch->search_description), GTK_STOCK_INFO, _("_Information"));
	quicksearch_set_button(GTK_BUTTON(quicksearch->search_condition_expression), GTK_STOCK_EDIT, _("_Edit"));
	quicksearch_set_button(GTK_BUTTON(quicksearch->clear_search), GTK_STOCK_CLEAR, _("C_lear"));
	
	update_extended_buttons(quicksearch);

	return quicksearch;
}

void quicksearch_relayout(QuickSearch *quicksearch)
{
	switch (prefs_common.layout_mode) {
	case NORMAL_LAYOUT:
	case WIDE_LAYOUT:
	case WIDE_MSGLIST_LAYOUT:
		quicksearch_set_button(GTK_BUTTON(quicksearch->search_description), GTK_STOCK_INFO, _("_Information"));
		quicksearch_set_button(GTK_BUTTON(quicksearch->search_condition_expression), GTK_STOCK_EDIT, _("_Edit"));
		quicksearch_set_button(GTK_BUTTON(quicksearch->clear_search), GTK_STOCK_CLEAR, _("C_lear"));
		break;
	case SMALL_LAYOUT:
	case VERTICAL_LAYOUT:
		quicksearch_set_button(GTK_BUTTON(quicksearch->search_description), GTK_STOCK_INFO, "");
		quicksearch_set_button(GTK_BUTTON(quicksearch->search_condition_expression), GTK_STOCK_EDIT, "");
		quicksearch_set_button(GTK_BUTTON(quicksearch->clear_search), GTK_STOCK_CLEAR, "");
		break;
	}
}

GtkWidget *quicksearch_get_widget(QuickSearch *quicksearch)
{
	return quicksearch->hbox_search;
}

void quicksearch_show(QuickSearch *quicksearch)
{
	MainWindow *mainwin = mainwindow_get_mainwindow();
	GtkWidget *ctree = NULL;
	prepare_matcher(quicksearch);
	gtk_widget_show(quicksearch->hbox_search);
	update_extended_buttons(quicksearch);
	gtk_widget_grab_focus(
		GTK_WIDGET(gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry)))));

	GTK_EVENTS_FLUSH();

	if (!mainwin || !mainwin->summaryview) {
		return;
	}
	
	ctree = summary_get_main_widget(mainwin->summaryview);
	
	if (ctree && mainwin->summaryview->selected)
		gtk_cmctree_node_moveto(GTK_CMCTREE(ctree), 
				mainwin->summaryview->selected, 
				0, 0.5, 0);
}

void quicksearch_hide(QuickSearch *quicksearch)
{
	if (quicksearch_is_active(quicksearch)) {
		quicksearch_set(quicksearch, prefs_common.summary_quicksearch_type, "");
		quicksearch_set_active(quicksearch, FALSE);
	}
	gtk_widget_hide(quicksearch->hbox_search);
}

/*
 *\brief	Sets the matchstring.
 *
 *\param	quicksearch quicksearch to set
 *\param	matchstring the match string; it is duplicated, not stored
 */
static void quicksearch_set_matchstring(QuickSearch *quicksearch,
					const gchar *matchstring)
{
	g_free(quicksearch->request->matchstring);
	quicksearch->request->matchstring = g_strdup(matchstring);
}

void quicksearch_set(QuickSearch *quicksearch, QuickSearchType type,
		     const gchar *matchstring)
{
	quicksearch_set_type(quicksearch, type);

	if (!matchstring || !(*matchstring))
		quicksearch->in_typing = FALSE;

	quicksearch_set_matchstring(quicksearch, matchstring);

	if (!quicksearch_from_gui(quicksearch)) {
		prepare_matcher(quicksearch);
		/* no callback */
		return;
	}
		
	g_signal_handlers_block_by_func(G_OBJECT(gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry)))),
					G_CALLBACK(searchbar_changed_cb), quicksearch);
	gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry)))),
			   matchstring);
	g_signal_handlers_unblock_by_func(G_OBJECT(gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry)))),
					  G_CALLBACK(searchbar_changed_cb), quicksearch);

	prefs_common.summary_quicksearch_type = type;

	prepare_matcher(quicksearch);

	quicksearch_set_running(quicksearch, TRUE);
	if (quicksearch->callback != NULL)
		quicksearch->callback(quicksearch, quicksearch->callback_data);
	quicksearch_set_running(quicksearch, FALSE);
}

gboolean quicksearch_is_active(QuickSearch *quicksearch)
{
	return quicksearch->active && 
		(prefs_common.summary_quicksearch_type != QUICK_SEARCH_EXTENDED
		 || quicksearch->matcher_list != NULL);
}

static void quicksearch_set_active(QuickSearch *quicksearch, gboolean active)
{
	static GdkColor yellow;
	static GdkColor red;
	static GdkColor black;
	static gboolean colors_initialised = FALSE;
	gboolean error = FALSE;

	
	quicksearch->active = active;
	if (quicksearch->gui == FALSE)
		return;

	if (!colors_initialised) {
		gdk_color_parse("#f5f6be", &yellow);
		gdk_color_parse("#000000", &black);
		gdk_color_parse("#ff7070", &red);
		colors_initialised = gdk_colormap_alloc_color(
			gdk_colormap_get_system(), &yellow, FALSE, TRUE);
		colors_initialised &= gdk_colormap_alloc_color(
			gdk_colormap_get_system(), &black, FALSE, TRUE);
		colors_initialised &= gdk_colormap_alloc_color(
			gdk_colormap_get_system(), &red, FALSE, TRUE);
	}

	if (active && 
		(prefs_common.summary_quicksearch_type == QUICK_SEARCH_EXTENDED
		 && quicksearch->matcher_list == NULL))
		error = TRUE;

	if (active) {
		gtk_widget_set_sensitive(quicksearch->clear_search, TRUE);
		if (colors_initialised) {
			gtk_widget_modify_base(
				gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry))),
				GTK_STATE_NORMAL, error ? &red : &yellow);
			gtk_widget_modify_text(
				gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry))),
				GTK_STATE_NORMAL, &black);
		}
	} else {
		gtk_widget_set_sensitive(quicksearch->clear_search, FALSE);
		if (colors_initialised) {
			gtk_widget_modify_base(
				gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry))),
				GTK_STATE_NORMAL, NULL);
			gtk_widget_modify_text(
				gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry))),
				GTK_STATE_NORMAL, NULL);
		}
	}

	if (!active) {
		quicksearch_reset_cur_folder_item(quicksearch);
	}
}

void quicksearch_set_execute_callback(QuickSearch *quicksearch,
				      QuickSearchExecuteCallback callback,
				      gpointer data)
{
	quicksearch->callback = callback;
	quicksearch->callback_data = data;
}

gboolean quicksearch_match(QuickSearch *quicksearch, MsgInfo *msginfo)
{
	gchar *searched_header = NULL;
	gboolean result = FALSE;
	gchar *to = NULL, *from = NULL, *subject = NULL;
	QuickSearchType quicksearch_type;

	if (!quicksearch->active)
		return TRUE;

	quicksearch_type = quicksearch->request->type;

	switch (quicksearch_type) {
	case QUICK_SEARCH_SUBJECT:
		if (msginfo->subject)
			searched_header = g_utf8_casefold(msginfo->subject, -1);
		else
			return FALSE;
		break;
	case QUICK_SEARCH_FROM:
		if (msginfo->from)
			searched_header = g_utf8_casefold(msginfo->from, -1);
		else
			return FALSE;
		break;
	case QUICK_SEARCH_TO:
		if (msginfo->to)
			searched_header = g_utf8_casefold(msginfo->to, -1);
		else
			return FALSE;
		break;
	case QUICK_SEARCH_MIXED:
		if (msginfo->to)
			to = g_utf8_casefold(msginfo->to, -1);
		if (msginfo->from)
			from = g_utf8_casefold(msginfo->from, -1);
		if (msginfo->subject)
			subject = g_utf8_casefold(msginfo->subject, -1);
		break;
	case QUICK_SEARCH_EXTENDED:
		break;
	default:
		debug_print("unknown search type (%d)\n", quicksearch_type);
		break;
	}

	quicksearch->matching = TRUE;
	if (quicksearch_type != QUICK_SEARCH_EXTENDED &&
	    quicksearch_type != QUICK_SEARCH_MIXED &&
	    quicksearch_type != QUICK_SEARCH_TAG &&
	    quicksearch->search_string &&
	    searched_header && strstr(searched_header, quicksearch->search_string) != NULL)
		result = TRUE;
	else if (quicksearch_type == QUICK_SEARCH_MIXED &&
		quicksearch->search_string && (
		(to && strstr(to, quicksearch->search_string) != NULL) ||
		(from && strstr(from, quicksearch->search_string) != NULL) ||
		(subject && strstr(subject, quicksearch->search_string) != NULL) ||
		((quicksearch->matcher_list != NULL) &&
		 matcherlist_match(quicksearch->matcher_list, msginfo))  ))
		result = TRUE;
	else if ((quicksearch->matcher_list != NULL) &&
		 matcherlist_match(quicksearch->matcher_list, msginfo))
		result = TRUE;

	quicksearch->matching = FALSE;
	if (quicksearch_from_gui(quicksearch)==TRUE && quicksearch->deferred_free) {
		/* Ref. http://lists.claws-mail.org/pipermail/users/2010-August/003063.html
		   See also 2.0.0cvs140 ChangeLog entry
		   and comment in search_msgs_in_folder() */
		prepare_matcher(quicksearch);
	}

	g_free(to);
	g_free(from);
	g_free(subject);
	g_free(searched_header);

	return result;
}

/* allow Mutt-like patterns in quick search */
static gchar *expand_search_string(const gchar *search_string)
{
	int i = 0;
	gchar term_char, save_char;
	gchar *cmd_start, *cmd_end;
	GString *matcherstr;
	gchar *returnstr = NULL;
	gchar *copy_str;
	gboolean casesens, dontmatch;
	/* list of allowed pattern abbreviations */
	struct {
		gchar		*abbreviated;	/* abbreviation */
		gchar		*command;	/* actual matcher command */
		gint		numparams;	/* number of params for cmd */
		gboolean	qualifier;	/* do we append regexpcase */
		gboolean	quotes;		/* do we need quotes */
	}
	cmds[] = {
		{ "a",	"all",				0,	FALSE,	FALSE },
		{ "ag",	"age_greater",			1,	FALSE,	FALSE },
		{ "al",	"age_lower",			1,	FALSE,	FALSE },
		{ "b",	"body_part",			1,	TRUE,	TRUE  },
		{ "B",	"message",			1,	TRUE,	TRUE  },
		{ "c",	"cc",				1,	TRUE,	TRUE  },
		{ "C",	"to_or_cc",			1,	TRUE,	TRUE  },
		{ "D",	"deleted",			0,	FALSE,	FALSE },
		{ "e",	"header \"Sender\"",		1,	TRUE,	TRUE  },
		{ "E",	"execute",			1,	FALSE,	TRUE  },
		{ "f",	"from",				1,	TRUE,	TRUE  },
		{ "F",	"forwarded",			0,	FALSE,	FALSE },
		{ "h",	"headers_part",			1,	TRUE,	TRUE  },
		{ "i",	"header \"Message-ID\"",	1,	TRUE,	TRUE  },
		{ "I",	"inreplyto",			1,	TRUE,	TRUE  },
		{ "k",	"colorlabel",			1,	FALSE,	FALSE },
		{ "L",	"locked",			0,	FALSE,	FALSE },
		{ "n",	"newsgroups",			1,	TRUE,	TRUE  },
		{ "N",	"new",				0,	FALSE,	FALSE },
		{ "O",	"~new",				0,	FALSE,	FALSE },
		{ "r",	"replied",			0,	FALSE,	FALSE },
		{ "R",	"~unread",			0,	FALSE,	FALSE },
		{ "s",	"subject",			1,	TRUE,	TRUE  },
		{ "se",	"score_equal",			1,	FALSE,	FALSE },
		{ "sg",	"score_greater",		1,	FALSE,	FALSE },
		{ "sl",	"score_lower",			1,	FALSE,	FALSE },
		{ "Se",	"size_equal",			1,	FALSE,	FALSE },
		{ "Sg",	"size_greater",			1,	FALSE,	FALSE },
		{ "Ss",	"size_smaller",			1,	FALSE,	FALSE },
		{ "t",	"to",				1,	TRUE,	TRUE  },
		{ "tg", "tag",				1,	TRUE,	TRUE  },
		{ "T",	"marked",			0,	FALSE,	FALSE },
		{ "U",	"unread",			0,	FALSE,	FALSE },
		{ "x",	"header \"References\"",	1,	TRUE,	TRUE  },
		{ "X",  "test",				1,	FALSE,  FALSE },
		{ "y",	"header \"X-Label\"",		1,	TRUE,	TRUE  },
		{ "&",	"&",				0,	FALSE,	FALSE },
		{ "|",	"|",				0,	FALSE,	FALSE },
		{ "p",	"partial",			0,	FALSE, 	FALSE },
		{ NULL,	NULL,				0,	FALSE,	FALSE }
	};

	if (search_string == NULL)
		return NULL;

	copy_str = g_strdup(search_string);

	matcherstr = g_string_sized_new(16);
	cmd_start = copy_str;
	while (cmd_start && *cmd_start) {
		/* skip all white spaces */
		while (*cmd_start && isspace((guchar)*cmd_start))
			cmd_start++;
		cmd_end = cmd_start;

		/* extract a command */
		while (*cmd_end && !isspace((guchar)*cmd_end))
			cmd_end++;

		/* save character */
		save_char = *cmd_end;
		*cmd_end = '\0';

		dontmatch = FALSE;
		casesens = FALSE;

		/* ~ and ! mean logical NOT */
		if (*cmd_start == '~' || *cmd_start == '!')
		{
			dontmatch = TRUE;
			cmd_start++;
		}
		/* % means case sensitive match */
		if (*cmd_start == '%')
		{
			casesens = TRUE;
			cmd_start++;
		}

		/* find matching abbreviation */
		for (i = 0; cmds[i].command; i++) {
			if (!strcmp(cmd_start, cmds[i].abbreviated)) {
				/* restore character */
				*cmd_end = save_char;

				/* copy command */
				if (matcherstr->len > 0) {
					g_string_append(matcherstr, " ");
				}
				if (dontmatch)
					g_string_append(matcherstr, "~");
				g_string_append(matcherstr, cmds[i].command);
				g_string_append(matcherstr, " ");

				/* stop if no params required */
				if (cmds[i].numparams == 0)
					break;

				/* extract a parameter, allow quotes */
				while (*cmd_end && isspace((guchar)*cmd_end))
					cmd_end++;

				cmd_start = cmd_end;
				if (*cmd_start == '"') {
					term_char = '"';
					cmd_end++;
				}
				else
					term_char = ' ';

				/* extract actual parameter */
				while ((*cmd_end) && (*cmd_end != term_char))
					cmd_end++;

				if (*cmd_end == '"')
					cmd_end++;

				save_char = *cmd_end;
				*cmd_end = '\0';

				if (cmds[i].qualifier) {
					if (casesens)
						g_string_append(matcherstr, "regexp ");
					else
						g_string_append(matcherstr, "regexpcase ");
				}

				/* do we need to add quotes ? */
				if (cmds[i].quotes && term_char != '"')
					g_string_append(matcherstr, "\"");

				/* copy actual parameter */
				g_string_append(matcherstr, cmd_start);

				/* do we need to add quotes ? */
				if (cmds[i].quotes && term_char != '"')
					g_string_append(matcherstr, "\"");

				/* restore original character */
				*cmd_end = save_char;

				break;
			}
		}

		if (*cmd_end)
			cmd_end++;
		cmd_start = cmd_end;
	}

	g_free(copy_str);

	/* return search string if no match is found to allow
	   all available filtering expressions in quicksearch */
	if (matcherstr->len > 0) returnstr = matcherstr->str;
	else returnstr = g_strdup(search_string);
	g_string_free(matcherstr, FALSE);
	return returnstr;
}

static gchar *expand_tag_search_string(const gchar *search_string)
{
	gchar *newstr = NULL;
	gchar **words = search_string ? g_strsplit(search_string, " ", -1):NULL;
	gint i = 0;
	while (words && words[i] && *words[i]) {
		g_strstrip(words[i]);
		if (!newstr) {
			newstr = g_strdup_printf("tag regexpcase \"%s\"", words[i]);
		} else {
			gint o_len = strlen(newstr);
			gint s_len = 18; /* strlen("|tag regexpcase \"\"") */
			gint n_len = s_len + strlen(words[i]);
			newstr = g_realloc(newstr,o_len+n_len+1);
			strcpy(newstr+o_len, "|tag regexpcase \"");
			strcpy(newstr+o_len+(s_len-1), words[i]);
			strcpy(newstr+o_len+(n_len-1), "\"");
		}
		i++;
	}
	g_strfreev(words);
	return newstr;
}

static void quicksearch_set_running(QuickSearch *quicksearch, gboolean run)
{
	quicksearch->running = run;
}

gboolean quicksearch_is_running(QuickSearch *quicksearch)
{
	return quicksearch->running;
}

void quicksearch_pass_key(QuickSearch *quicksearch, guint val, GdkModifierType mod)
{
	GtkEntry *entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN((quicksearch->search_string_entry))));
	glong curpos = gtk_editable_get_position(GTK_EDITABLE(entry));
	guint32 c;
	char *str = g_strdup(gtk_entry_get_text(entry));
	char *begin = str;
	char *end = NULL;
	char *new = NULL;
	char key[7] = "";
	guint char_len = 0;

	if (gtk_editable_get_selection_bounds(GTK_EDITABLE(entry), NULL, NULL)) {
		/* remove selection */
		gtk_editable_delete_selection(GTK_EDITABLE(entry));
		curpos = gtk_editable_get_position(GTK_EDITABLE(entry));
		/* refresh string */
		g_free(str);
		str = g_strdup(gtk_entry_get_text(entry));
		begin = str;
	}

	if (!(c = gdk_keyval_to_unicode(val))) {
		g_free(str);
		return;
	}
	char_len = g_unichar_to_utf8(c, key);
	if (char_len < 0)
		return;
	key[char_len] = '\0';
	if (curpos < g_utf8_strlen(str, -1)) {
		gchar *stop = g_utf8_offset_to_pointer(begin, curpos);
		end = g_strdup(g_utf8_offset_to_pointer(str, curpos));
		*stop = '\0';
		new = g_strdup_printf("%s%s%s", begin, key, end);
		gtk_entry_set_text(entry, new);
		g_free(end);
	} else {
		new = g_strdup_printf("%s%s", begin, key);
		gtk_entry_set_text(entry, new);
	}
	g_free(str);
	g_free(new);
	gtk_editable_set_position(GTK_EDITABLE(entry), curpos+1);

}

static gboolean quicksearch_match_subfolder(QuickSearch *quicksearch,
				 FolderItem *src)
{
	GSList *msglist = NULL;
	GSList *cur;
	gboolean result = FALSE;
	gint num = 0, total = 0;
	gint interval = quicksearch_is_fast(quicksearch) ? 5000:100;

	statusbar_print_all(_("Searching in %s... \n"),
		src->path ? src->path : "(null)");
		
	msglist = folder_item_get_msg_list(src);
	total = src->total_msgs;
	folder_item_update_freeze();
	for (cur = msglist; cur != NULL; cur = cur->next) {
		MsgInfo *msg = (MsgInfo *)cur->data;
		statusbar_progress_all(num++,total, interval);
		if (quicksearch_match(quicksearch, msg)) {
			result = TRUE;
			break;
		}
		if (num % interval == 0)
			GTK_EVENTS_FLUSH();
		if (!quicksearch_is_active(quicksearch))
			break;
	}
	folder_item_update_thaw();
	statusbar_progress_all(0,0,0);
	statusbar_pop_all();

	procmsg_msg_list_free(msglist);
	return result;
}

gboolean quicksearch_is_in_subfolder(QuickSearch *quicksearch, FolderItem *cur)
{
	if (quicksearch->root_folder_item == NULL)
		return FALSE;
	
	while (cur) {
		if (cur == quicksearch->root_folder_item) {
			return TRUE;
		}
		cur = folder_item_parent(cur);
	}
	return FALSE;
}

void quicksearch_search_subfolders(QuickSearch *quicksearch,
				   FolderView *folderview,
				   FolderItem *folder_item)
{
	FolderItem *cur = NULL;
	GNode *node = folder_item->node->children;

	if (!prefs_common.summary_quicksearch_recurse
	||  quicksearch->in_typing == TRUE)
		return;

	for (; node != NULL; node = node->next) {
		cur = FOLDER_ITEM(node->data);
		if (quicksearch_match_subfolder(quicksearch, cur)) {
			folderview_update_search_icon(cur, TRUE);
		} else {
			folderview_update_search_icon(cur, FALSE);
		}
		if (cur->node->children)
			quicksearch_search_subfolders(quicksearch,
						      folderview,
						      cur);
	}
	quicksearch->root_folder_item = folder_item;
	if (!quicksearch_is_active(quicksearch))
		quicksearch_reset_cur_folder_item(quicksearch);
}

static void quicksearch_reset_folder_items(QuickSearch *quicksearch,
				    FolderItem *folder_item)
{
	FolderItem *cur = NULL;
	GNode *node = (folder_item && folder_item->node) ?
			folder_item->node->children : NULL;

	for (; node != NULL; node = node->next) {
		cur = FOLDER_ITEM(node->data);
		folderview_update_search_icon(cur, FALSE);
		if (cur->node->children)
			quicksearch_reset_folder_items(quicksearch,
						       cur);
	}
}

void quicksearch_reset_cur_folder_item(QuickSearch *quicksearch)
{
	if (quicksearch->root_folder_item)
		quicksearch_reset_folder_items(quicksearch,
					       quicksearch->root_folder_item);

	quicksearch->root_folder_item = NULL;
}

gboolean quicksearch_is_in_typing(QuickSearch *quicksearch)
{
	return quicksearch->in_typing;
}

void quicksearch_set_search_strings(QuickSearch *quicksearch)
{
	GList *strings = prefs_common.summary_quicksearch_history;
	gchar *newstr = NULL;
	MatcherList *matcher_list = NULL;

	if (!strings)
		return;

	matcher_parser_disable_warnings(TRUE);
	
	do {
		newstr = expand_search_string((gchar *) strings->data);
		if (newstr && newstr[0] != '\0') {
			if (!strchr(newstr, ' ')) {
				quicksearch->normal_search_strings =
					g_list_append(
						quicksearch->normal_search_strings,
						g_strdup(strings->data));
				g_free(newstr);
				continue;
			}
			
			matcher_list = matcher_parser_get_cond(newstr, FALSE);
			g_free(newstr);
			
			if (matcher_list) {
				quicksearch->extended_search_strings =
					g_list_prepend(
						quicksearch->extended_search_strings,
						g_strdup(strings->data));
				matcherlist_free(matcher_list);
			} else
				quicksearch->normal_search_strings =
					g_list_prepend(
						quicksearch->normal_search_strings,
						g_strdup(strings->data));
		}
	
	} while ((strings = g_list_next(strings)) != NULL);

	matcher_parser_disable_warnings(FALSE);	

	quicksearch->normal_search_strings = g_list_reverse(quicksearch->normal_search_strings);
	quicksearch->extended_search_strings = g_list_reverse(quicksearch->extended_search_strings);

	quicksearch_set_popdown_strings(quicksearch);
}

/*
 * Searches in the supplied folderItem the messages (MessageInfo) matching a
 * QuickSearchType + search string (ex.: QUICK_SEARCH_FROM and "foo@bar.com").
 *
 * Found messages are appended to the array 'messages' and their ref.counts
 * are incremented by 1 --so they need to be released (procmsg_msginfo_free())
 * before the array 'messages' is freed.
 */
void search_msgs_in_folder(GSList **messages, QuickSearch* quicksearch,
			   FolderItem* folderItem)
{
	/* from quicksearch_match_subfolder */
	GSList *msglist = NULL;
	GSList *cur;

	/* The list is built w/ MsgInfo items whose ref.counts are incremented,
	   but they are decremented when the list is freed by
	   procmsg_msg_list_free(): we'll  ask for a new ref., below
	*/
	msglist = folder_item_get_msg_list(folderItem);

	for (cur = msglist; cur != NULL; cur = cur->next) {
		MsgInfo *msg = (MsgInfo *)cur->data;
		if (quicksearch_match(quicksearch, msg)) {
			/*debug_print("found: %s from:%s\n",procmsg_get_message_file_path(msg),msg->from);*/
			*messages = g_slist_prepend(*messages, procmsg_msginfo_new_ref(msg));
		}
		/* See 2.0.0cvs140 ChangeLog entry for details
		   see also comments in quicksearch_match() */
		if (quicksearch_from_gui(quicksearch)==TRUE
		    && !quicksearch_is_active(quicksearch))
			break;
	}
	procmsg_msg_list_free(msglist);
}

/*
 * Searches within the folderItem and its sub-folders (if recursive is TRUE)
 * the messages matching the search request.
 *
 * NB: search within a Folder can be done this way:
 *         search_msg_in_folders(messages, quicksearch, searchType,
 *                               FOLDER_ITEM(folder->node->data), TRUE);
 */
void search_msgs_in_folders(GSList **messages, QuickSearch* quicksearch,
			    FolderItem* folderItem)
{
	FolderItem *cur = NULL;

	search_msgs_in_folder(messages, quicksearch, folderItem);
	if (quicksearch->request->recursive == FALSE)
		return;

	GNode *node = folderItem->node->children;
	for (; node != NULL; node = node->next) {
		cur = FOLDER_ITEM(node->data);
		debug_print("in: %s\n",cur->path);
		if (cur->node->children)
			search_msgs_in_folders(messages, quicksearch, cur);
		else
			search_msgs_in_folder(messages, quicksearch, cur);
	}
	*messages = g_slist_reverse(*messages);
}

 /*
  * Returns the QuickSearchType associated to the supplied string.
  */
QuickSearchType quicksearch_type(const gchar* type)
{
	QuickSearchType searchType = QUICK_SEARCH_EXTENDED;
	if (!type)
		return searchType;
	switch(toupper(*type)) {
	case 'S':
		searchType = QUICK_SEARCH_SUBJECT;
	break;
	case 'F':
		searchType = QUICK_SEARCH_FROM;
	break;
	case 'T':
		searchType = QUICK_SEARCH_TO;
	break;
	case 'E':
		searchType = QUICK_SEARCH_EXTENDED;
	break;
	case 'M':
		searchType = QUICK_SEARCH_MIXED;
	break;
	case 'G':
		searchType = QUICK_SEARCH_TAG;
	break;
	}
	return searchType;
}
