/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
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
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "prefs_gtk.h"
#include "prefs_summary_open.h"
#include "prefs_common.h"
#include "manage_window.h"
#include "alertpanel.h"
#include "displayheader.h"
#include "utils.h"
#include "gtkutils.h"

enum {
	PREFS_SUMMARY_OPEN_HEADER,
	PREFS_SUMMARY_OPEN_DATA,
	N_PREFS_SUMMARY_OPEN_COLUMNS
};

static struct SummaryOpen {
	GtkWidget *window;

	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;

	GtkWidget *possible_actions_list_view;
	GtkWidget *actions_list_view;
} summaryopen;

/* widget creating functions */
static void prefs_summary_open_create	(void);

static void prefs_summary_open_set_dialog	(void);
static void prefs_summary_open_set_list	(void);

/* callback functions */
static void prefs_summary_open_register_cb	(GtkButton	*btn,
						 gpointer	 hidden_data);
static void prefs_summary_open_delete_cb	(GtkButton	*btn,
						 gpointer	 list_view_data);
static void prefs_summary_open_up		(void);
static void prefs_summary_open_down		(void);

static gboolean prefs_summary_open_key_pressed	(GtkWidget	*widget,
							 GdkEventKey	*event,
							 gpointer	 data);
static void prefs_summary_open_ok		(void);
static void prefs_summary_open_cancel		(void);
static gint prefs_summary_open_deleted	(GtkWidget	*widget,
						 GdkEventAny	*event,
						 gpointer	 data);


static GtkListStore *prefs_summary_open_create_store	(void);
static void prefs_summary_open_insert_action		(GtkListStore *store,
							 gchar *name,
							 gint act);
static GtkWidget *prefs_summary_open_list_view_create	(const gchar *name);
static void prefs_summary_open_create_list_view_columns	(GtkWidget *list_view, 
							 const gchar *name);
static void actions_list_model_row_changed		(GtkTreeModel *model, 
							 GtkTreePath *path, 
							 GtkTreeIter *iter, 
							 GtkTreeView *list_view);
							 
static void drag_begin	(GtkTreeView *list_view,
			 GdkDragContext *context,
			 gpointer data);

static void drag_end	(GtkTreeView *list_view,
			 GdkDragContext *context,
			 gpointer data);

static EntryAction saved_summary_select_prio[SUMMARY_OPEN_ACTIONS-1];

static gchar *action_name[SUMMARY_OPEN_ACTIONS] = 
{	  ("UNSET (!)"),
	 N_("first marked email"),
	 N_("first new email"),
	 N_("first unread email"),
	 N_("last opened email"),
	 N_("last email in the list"),
	 N_("none"),
	 N_("first email in the list")
};

void prefs_summary_open_open(void)
{
	int i;
	if (!summaryopen.window) {
		prefs_summary_open_create();
	}

	manage_window_set_transient(GTK_WINDOW(summaryopen.window));
	gtk_widget_grab_focus(summaryopen.ok_btn);

	prefs_summary_open_set_dialog();
	
	for (i = 0; i < SUMMARY_OPEN_ACTIONS-1; i++)
		saved_summary_select_prio[i] = prefs_common.summary_select_prio[i];

	gtk_widget_show(summaryopen.window);
	gtk_window_set_modal(GTK_WINDOW(summaryopen.window), TRUE);
}

static void prefs_summary_open_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *btn_hbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *confirm_area;

	GtkWidget *vbox1;

	GtkWidget *hbox1;

	GtkWidget *btn_vbox;
	GtkWidget *reg_btn;
	GtkWidget *del_btn;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	GtkWidget *list_view_hbox;
	GtkWidget *list_view_hbox1;
	GtkWidget *list_view_hbox2;
	GtkWidget *list_view_scrolledwin;
	GtkWidget *possible_actions_list_view;
	GtkWidget *actions_list_view;
	
	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_summary_open");
	gtk_container_set_border_width (GTK_CONTAINER (window), 8);
	gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW (window), TRUE);

	vbox = gtk_vbox_new (FALSE, 6);
	gtk_widget_show (vbox);
	gtk_container_add (GTK_CONTAINER (window), vbox);

	btn_hbox = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (btn_hbox);
	gtk_box_pack_end (GTK_BOX (vbox), btn_hbox, FALSE, FALSE, 0);

	gtkut_stock_button_set_create(&confirm_area, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);
	gtk_widget_show (confirm_area);
	gtk_box_pack_end (GTK_BOX(btn_hbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default (ok_btn);

	gtk_window_set_title (GTK_WINDOW(window),
			      _(" Selection when entering a folder"));
	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	g_signal_connect (G_OBJECT(window), "delete_event",
			  G_CALLBACK(prefs_summary_open_deleted),
			  NULL);
	g_signal_connect (G_OBJECT(window), "key_press_event",
			  G_CALLBACK(prefs_summary_open_key_pressed),
			  NULL);
	g_signal_connect (G_OBJECT(ok_btn), "clicked",
			  G_CALLBACK(prefs_summary_open_ok),
			  NULL);
	g_signal_connect (G_OBJECT(cancel_btn), "clicked",
			  G_CALLBACK(prefs_summary_open_cancel),
			  NULL);

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_box_pack_start (GTK_BOX (vbox), vbox1, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), 2);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, TRUE, 0);

	/* display headers list */

	list_view_hbox = gtk_hbox_new (FALSE, 10);
	gtk_widget_show (list_view_hbox);
	gtk_box_pack_start (GTK_BOX (vbox1), list_view_hbox, TRUE, TRUE, 0);

	list_view_hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (list_view_hbox1);
	gtk_box_pack_start (GTK_BOX (list_view_hbox), list_view_hbox1, TRUE, TRUE, 0);

	list_view_scrolledwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_set_size_request (list_view_scrolledwin, 200, 210);
	gtk_widget_show (list_view_scrolledwin);
	gtk_box_pack_start (GTK_BOX (list_view_hbox1), list_view_scrolledwin,
			    TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (list_view_scrolledwin),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(list_view_scrolledwin),
					    GTK_SHADOW_IN);

	possible_actions_list_view = prefs_summary_open_list_view_create
				(_("Possible selections"));
	gtk_widget_show (possible_actions_list_view);
	gtk_container_add(GTK_CONTAINER(list_view_scrolledwin), possible_actions_list_view);

	btn_vbox = gtk_vbox_new (FALSE, 8);
	gtk_widget_show (btn_vbox);
	gtk_box_pack_start (GTK_BOX (list_view_hbox1), btn_vbox, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock (GTK_STOCK_ADD);
	gtk_widget_show (reg_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), reg_btn, FALSE, TRUE, 0);
	g_signal_connect (G_OBJECT (reg_btn), "clicked",
			  G_CALLBACK (prefs_summary_open_register_cb),
			  possible_actions_list_view);
	del_btn = gtk_button_new_from_stock (GTK_STOCK_REMOVE);
	gtk_widget_show (del_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), del_btn, FALSE, TRUE, 0);

	/* actions list */

	list_view_hbox2 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (list_view_hbox2);
	gtk_box_pack_start (GTK_BOX (list_view_hbox), list_view_hbox2, TRUE, TRUE, 0);

	list_view_scrolledwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_set_size_request (list_view_scrolledwin, 200, 230);
	gtk_widget_show (list_view_scrolledwin);
	gtk_box_pack_start (GTK_BOX (list_view_hbox2), list_view_scrolledwin,
			    TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (list_view_scrolledwin),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(list_view_scrolledwin),
					    GTK_SHADOW_IN);

	actions_list_view = prefs_summary_open_list_view_create
					(_("Selection on folder opening"));
	g_signal_connect (G_OBJECT (del_btn), "clicked",
			  G_CALLBACK (prefs_summary_open_delete_cb),
			  actions_list_view);
	gtk_widget_show (actions_list_view);
	gtk_container_add (GTK_CONTAINER (list_view_scrolledwin),
			   actions_list_view);

	gtk_tree_view_set_reorderable(GTK_TREE_VIEW(actions_list_view), TRUE);
	g_signal_connect(G_OBJECT(actions_list_view), "drag_begin", 			 
			 G_CALLBACK(drag_begin),
			 actions_list_view);
			 
	g_signal_connect(G_OBJECT(actions_list_view), "drag_end", 			 
			 G_CALLBACK(drag_end),
			 actions_list_view);
	
	btn_vbox = gtk_vbox_new (FALSE, 8);
	gtk_widget_show (btn_vbox);
	gtk_box_pack_start (GTK_BOX (list_view_hbox2), btn_vbox, FALSE, FALSE, 0);

	up_btn = gtk_button_new_from_stock (GTK_STOCK_GO_UP);
	gtk_widget_show (up_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), up_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (up_btn), "clicked",
			  G_CALLBACK (prefs_summary_open_up), NULL);

	down_btn = gtk_button_new_from_stock (GTK_STOCK_GO_DOWN);
	gtk_widget_show (down_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), down_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (down_btn), "clicked",
			  G_CALLBACK (prefs_summary_open_down), NULL);
	

	gtk_widget_show_all(window);

	summaryopen.window        = window;
	summaryopen.ok_btn        = ok_btn;
	summaryopen.cancel_btn    = cancel_btn;

	summaryopen.possible_actions_list_view        = possible_actions_list_view;
	summaryopen.actions_list_view = actions_list_view;
}

/* do it SUMMARY_OPEN_ACTIONS-1 times */
#define SET_PRIO(p_one,p_two,p_three,p_four) {				\
	prefs_common.summary_select_prio[0] = p_one;			\
	prefs_common.summary_select_prio[1] = p_two;			\
	prefs_common.summary_select_prio[2] = p_three;			\
	prefs_common.summary_select_prio[3] = p_four;			\
	prefs_common.summary_select_prio[4] = ACTION_UNSET;		\
	prefs_common.summary_select_prio[5] = ACTION_UNSET;		\
	prefs_common.summary_select_prio[6] = ACTION_UNSET;		\
}							

void prefs_summary_open_set_defaults(void)
{
 	switch (prefs_common.select_on_entry) {
 		case SELECTONENTRY_MNU:
			SET_PRIO(ACTION_MARKED, ACTION_NEW, ACTION_UNREAD, ACTION_LAST_LIST); break;
 		case SELECTONENTRY_MUN:
			SET_PRIO(ACTION_MARKED, ACTION_UNREAD, ACTION_NEW, ACTION_LAST_LIST); break;
 		case SELECTONENTRY_NMU:
			SET_PRIO(ACTION_NEW, ACTION_MARKED, ACTION_UNREAD, ACTION_LAST_LIST); break;
 		case SELECTONENTRY_NUM:
			SET_PRIO(ACTION_NEW, ACTION_UNREAD, ACTION_MARKED, ACTION_LAST_LIST); break;
 		case SELECTONENTRY_UNM:
			SET_PRIO(ACTION_UNREAD, ACTION_NEW, ACTION_MARKED, ACTION_LAST_LIST); break;
 		case SELECTONENTRY_UMN:
			SET_PRIO(ACTION_UNREAD, ACTION_MARKED, ACTION_NEW, ACTION_LAST_LIST); break;
		case SELECTONENTRY_LAST:
			SET_PRIO(ACTION_LAST_OPENED, ACTION_LAST_LIST, ACTION_UNSET, ACTION_UNSET); break;
		case SELECTONENTRY_NOTHING:
			SET_PRIO(ACTION_LAST_LIST, ACTION_UNSET, ACTION_UNSET, ACTION_UNSET); break;
		default:
			break;
	}
}
static void prefs_summary_open_set_dialog(void)
{
	GtkTreeView *possible_list_view = GTK_TREE_VIEW(summaryopen.possible_actions_list_view);
	GtkTreeView *actions_list_view = GTK_TREE_VIEW(summaryopen.actions_list_view);
	GtkTreeModel *model_poss, *model_act;
	int i;
	gboolean set = FALSE;
	gboolean used[SUMMARY_OPEN_ACTIONS-1] = {FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE};

	model_poss = gtk_tree_view_get_model(possible_list_view);
	model_act = gtk_tree_view_get_model(actions_list_view);

	gtk_list_store_clear(GTK_LIST_STORE(model_poss));
	gtk_list_store_clear(GTK_LIST_STORE(model_act));

fill:
	for (i = 0; i < SUMMARY_OPEN_ACTIONS-1; i++) {
		EntryAction act = prefs_common.summary_select_prio[i];

		if (act == ACTION_UNSET) 
			continue;
		
		set = TRUE;
		used[act-1] = TRUE;
		prefs_summary_open_insert_action(GTK_LIST_STORE
					(model_act), action_name[act], act);	
	}
	if (!set) {
		/* backward compat */
		prefs_summary_open_set_defaults();
		goto fill;
	}

	for (i = 1; i < SUMMARY_OPEN_ACTIONS; i++) {
		if (!used[i-1]) {
			prefs_summary_open_insert_action(GTK_LIST_STORE
					(model_poss), action_name[i], i);	
		}
	}
}

static void prefs_summary_open_set_list(void)
{
	gint row = 0;
	gpointer data;
	GtkTreeModel *model;
	GtkTreeIter iter;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(summaryopen.actions_list_view));
	while (gtk_tree_model_iter_nth_child(model, &iter, NULL, row)) {
		gtk_tree_model_get(model, &iter, PREFS_SUMMARY_OPEN_DATA, &data, -1);
		prefs_common.summary_select_prio[row] = GPOINTER_TO_INT(data);
		row++;				
	}
	for (; row < SUMMARY_OPEN_ACTIONS-1; row++) {
		prefs_common.summary_select_prio[row] = ACTION_UNSET;
	}
}

static void prefs_summary_open_delete_cb(GtkButton *btn, gpointer list_view_data)
{
	GtkTreeView *list_view = GTK_TREE_VIEW(list_view_data);
	gpointer data;
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(list_view));
	GtkTreeSelection *selection = gtk_tree_view_get_selection(list_view);
	GtkTreeIter iter;
	GtkTreeView *possible_list_view = GTK_TREE_VIEW(summaryopen.possible_actions_list_view);
	GtkTreeModel *model, *model_poss;
	gint i;

	model = gtk_tree_view_get_model(list_view);
	model_poss = gtk_tree_view_get_model(possible_list_view);

	if (!gtk_tree_selection_get_selected(selection, NULL, &iter))
		return;

	gtk_tree_model_get(model, &iter, PREFS_SUMMARY_OPEN_DATA, &data, -1);
	if (!data) 
		return;

	gtk_list_store_remove(store, &iter);

	i = GPOINTER_TO_INT(data);
	prefs_summary_open_insert_action(GTK_LIST_STORE
					(model_poss), action_name[i], i);	
	
	prefs_summary_open_set_list();
}

static void prefs_summary_open_register_cb(GtkButton *btn, gpointer list_view_data)
{
	GtkTreeView *possible_list_view = GTK_TREE_VIEW(list_view_data);
	GtkTreeView *list_view = GTK_TREE_VIEW(summaryopen.actions_list_view);
	gpointer data;
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(possible_list_view));
	GtkTreeSelection *selection = gtk_tree_view_get_selection(possible_list_view);
	GtkTreeIter iter;
	GtkTreeModel *model, *model_poss;
	gint i;

	model = gtk_tree_view_get_model(list_view);
	model_poss = gtk_tree_view_get_model(possible_list_view);

	if (!gtk_tree_selection_get_selected(selection, NULL, &iter))
		return;

	gtk_tree_model_get(model_poss, &iter, PREFS_SUMMARY_OPEN_DATA, &data, -1);
	if (!data) 
		return;

	gtk_list_store_remove(store, &iter);

	i = GPOINTER_TO_INT(data);
	prefs_summary_open_insert_action(GTK_LIST_STORE
					(model), action_name[i], i);	
	
	prefs_summary_open_set_list();
}

static void prefs_summary_open_up(void)
{
	GtkTreePath *prev, *sel, *try;
	GtkTreeIter isel;
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter iprev;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(summaryopen.actions_list_view)),
		 &model,	
		 &isel))
		return;
	store = (GtkListStore *)model;
	sel = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &isel);
	if (!sel)
		return;
	
	/* no move if we're at row 0... */
	try = gtk_tree_path_copy(sel);
	if (!gtk_tree_path_prev(try)) {
		gtk_tree_path_free(try);
		gtk_tree_path_free(sel);
		return;
	}

	prev = try;
	gtk_tree_model_get_iter(GTK_TREE_MODEL(store),
				&iprev, prev);
	gtk_list_store_swap(store, &iprev, &isel);

	gtk_tree_path_free(sel);
	gtk_tree_path_free(prev);
	prefs_summary_open_set_list();
}

static void prefs_summary_open_down(void)
{
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter next, sel;
	GtkTreePath *try;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(summaryopen.actions_list_view)),
		 &model,
		 &sel))
		return;
	store = (GtkListStore *)model;
	try = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &sel);
	if (!try) 
		return;
	
	next = sel;
	if (gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &next))
		gtk_list_store_swap(store, &next, &sel);
		
	gtk_tree_path_free(try);
	prefs_summary_open_set_list();
}

static gboolean prefs_summary_open_key_pressed(GtkWidget *widget,
					     GdkEventKey *event,
					     gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape)
		prefs_summary_open_cancel();
	return FALSE;
}

static void prefs_summary_open_ok(void)
{
	int i;

	/* force an empty list to be at least ACTION_NONE */
	if (prefs_common.summary_select_prio[0] == ACTION_UNSET)
		prefs_common.summary_select_prio[0] = ACTION_NOTHING;

	for (i = 0; i < SUMMARY_OPEN_ACTIONS-1; i++)
		saved_summary_select_prio[i] = prefs_common.summary_select_prio[i];

	gtk_widget_hide(summaryopen.window);
	gtk_window_set_modal(GTK_WINDOW(summaryopen.window), FALSE);
}

static void prefs_summary_open_cancel(void)
{
	int i;
	for (i = 0; i < SUMMARY_OPEN_ACTIONS-1; i++)
		prefs_common.summary_select_prio[i] = saved_summary_select_prio[i];

	gtk_widget_hide(summaryopen.window);
}

static gint prefs_summary_open_deleted(GtkWidget *widget, GdkEventAny *event,
					 gpointer data)
{
	prefs_summary_open_cancel();
	return TRUE;
}

static GtkListStore *prefs_summary_open_create_store(void)
{
	return gtk_list_store_new(N_PREFS_SUMMARY_OPEN_COLUMNS,
				  G_TYPE_STRING,
				  G_TYPE_POINTER,
				  -1);
}

static void prefs_summary_open_insert_action(GtkListStore *store,
					       gchar *name,
					       gint dp)
{
	GtkTreeIter iter;

	/* add new */
	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter,
			   PREFS_SUMMARY_OPEN_HEADER,
			   gettext(name),
			   PREFS_SUMMARY_OPEN_DATA, GINT_TO_POINTER(dp),
			   -1);
}

static GtkWidget *prefs_summary_open_list_view_create(const gchar *name)
{
	GtkWidget *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(prefs_summary_open_create_store());
	list_view = gtk_tree_view_new_with_model(model);
	g_object_unref(G_OBJECT(model));
	
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(list_view),
				     prefs_common.use_stripes_everywhere);
	
	selector = gtk_tree_view_get_selection(GTK_TREE_VIEW(list_view));
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);

	prefs_summary_open_create_list_view_columns(GTK_WIDGET(list_view), name);

	return list_view;
}

static void prefs_summary_open_create_list_view_columns(GtkWidget *list_view, 
						     const gchar *name)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(name, renderer, "text", PREFS_SUMMARY_OPEN_HEADER, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
}

/*!
 *\brief	Called as a result of a drag & drop
 */
static void actions_list_model_row_changed(GtkTreeModel *model, 
					   GtkTreePath *path, 
					   GtkTreeIter *iter, 
					   GtkTreeView *list_view)
{
}

/*!
 *\brief	Called as a result of a gtk_list_store_swap()
 */
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
			 G_CALLBACK(actions_list_model_row_changed),
			 list_view);
}

static void drag_end(GtkTreeView *list_view,
		    GdkDragContext *context,
		    gpointer data)
{
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	g_signal_handlers_disconnect_by_func(G_OBJECT(model),
					     G_CALLBACK(actions_list_model_row_changed),
					     list_view);
	prefs_summary_open_set_list();
}

