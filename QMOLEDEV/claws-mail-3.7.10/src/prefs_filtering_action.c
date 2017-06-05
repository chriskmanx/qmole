/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2011 the Claws Mail team
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
#include "prefs_filtering_action.h"
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
#include "addr_compl.h"
#include "tags.h"
#include "matcher_parser.h"
#include "colorlabel.h"
#include "combobox.h"

enum {
	PFA_ACTION,
	PFA_VALID_ACTION,
	N_PFA_COLUMNS
};


static void prefs_filtering_action_create(void);
static void prefs_filtering_action_delete_cb(void);
static void prefs_filtering_action_substitute_cb(void);
static void prefs_filtering_action_register_cb(void);
static void prefs_filtering_action_reset_dialog(void);
static gboolean prefs_filtering_action_key_pressed(GtkWidget *widget,
    GdkEventKey *event, gpointer data);
static void prefs_filtering_action_cancel(void);
static void prefs_filtering_action_ok(void);
static gint prefs_filtering_action_deleted(GtkWidget *widget,
    GdkEventAny *event, gpointer data);
static void prefs_filtering_action_type_selection_changed(GtkWidget *widget,
    gpointer user_data);
static void prefs_filtering_action_select_dest(void);
static void prefs_filtering_action_select_addressbook(void);
static void prefs_filtering_action_up(void);
static void prefs_filtering_action_down(void);
static void prefs_filtering_action_set_dialog(GSList *action_list);
static GSList *prefs_filtering_action_get_list(void);

static GtkListStore* prefs_filtering_action_create_data_store	(void);
static void prefs_filtering_action_list_view_insert_action	(GtkWidget   *list_view,
								 GtkTreeIter *row,
								 const gchar *action,
								 gboolean     is_valid);
static GtkWidget *prefs_filtering_action_list_view_create	(void);
static void prefs_filtering_action_create_list_view_columns	(GtkTreeView *list_view);
static gboolean prefs_filtering_actions_selected		(GtkTreeSelection *selector,
								 GtkTreeModel *model, 
								 GtkTreePath *path,
								 gboolean currently_selected,
								 gpointer data);

static void prefs_filtering_action_exec_info			(GtkWidget *widget,
								 GtkWidget *parent);

/*!
 *\brief	UI data for matcher dialog
 */
static struct FilteringAction_ {
	GtkWidget *window;

	GtkWidget *ok_btn;

	GtkWidget *action_list_view;
	GtkWidget *label1;
	GtkWidget *label2;
	GtkWidget *label3;
	GtkWidget *action_combo;
	GtkWidget *account_combo;
	GtkWidget *dest_entry;
	GtkWidget *dest_btn;
	GtkWidget *exec_btn;
	GtkWidget *color_optmenu;
	GtkWidget *header_combo;
	GtkWidget *header_entry;
	GtkWidget *addressbook_btn;
	GtkWidget *score_entry;
	GtkWidget *tags_combo;

	gint current_action;
} filtering_action;


typedef enum Action_ {
	ACTION_MOVE,
	ACTION_COPY,
	ACTION_DELETE,
	ACTION_MARK,
	ACTION_UNMARK,
	ACTION_LOCK,
	ACTION_UNLOCK,
	ACTION_MARK_AS_READ,
	ACTION_MARK_AS_UNREAD,
	ACTION_MARK_AS_SPAM,
	ACTION_MARK_AS_HAM,
	ACTION_FORWARD,
	ACTION_FORWARD_AS_ATTACHMENT,
	ACTION_REDIRECT,
	ACTION_EXECUTE,
	ACTION_COLOR,
	ACTION_CHANGE_SCORE,
	ACTION_SET_SCORE,
	ACTION_SET_TAG,
	ACTION_UNSET_TAG,
	ACTION_CLEAR_TAGS,
	ACTION_HIDE,
	ACTION_IGNORE,
	ACTION_WATCH,
	ACTION_ADD_TO_ADDRESSBOOK,
	ACTION_STOP,
	/* add other action constants */
} Action;

static struct {
	gchar *menu;
	gchar *text;
	Action action;
} action_menu [] = {
	{ NULL, 		N_("Move"),			ACTION_MOVE }, 	
	{ NULL, 		N_("Copy"),			ACTION_COPY },
	{ NULL, 		N_("Delete"),			ACTION_DELETE },
	{ NULL, 		N_("Hide"),		        ACTION_HIDE },
	{ N_("Message flags"), 	N_("Mark"),			ACTION_MARK },
	{ N_("Message flags"), 	N_("Unmark"),			ACTION_UNMARK },
	{ N_("Message flags"), 	N_("Lock"),			ACTION_LOCK },
	{ N_("Message flags"), 	N_("Unlock"),			ACTION_UNLOCK },
	{ N_("Message flags"), 	N_("Mark as read"),		ACTION_MARK_AS_READ },
	{ N_("Message flags"), 	N_("Mark as unread"),		ACTION_MARK_AS_UNREAD },
	{ N_("Message flags"), 	N_("Mark as spam"),		ACTION_MARK_AS_SPAM },
	{ N_("Message flags"), 	N_("Mark as ham"),		ACTION_MARK_AS_HAM },
	{ NULL, 		N_("Execute"),			ACTION_EXECUTE },
	{ NULL, 		N_("Color label"),		ACTION_COLOR },
	{ N_("Resend"), 	N_("Forward"),			ACTION_FORWARD },
	{ N_("Resend"), 	N_("Forward as attachment"),	ACTION_FORWARD_AS_ATTACHMENT },
	{ N_("Resend"), 	N_("Redirect"),			ACTION_REDIRECT },
	{ N_("Score"), 		N_("Change score"),		ACTION_CHANGE_SCORE },
	{ N_("Score"), 		N_("Set score"),		ACTION_SET_SCORE },
	{ N_("Tags"), 		N_("Apply tag"),		ACTION_SET_TAG },
	{ N_("Tags"), 		N_("Unset tag"),		ACTION_UNSET_TAG },
	{ N_("Tags"), 		N_("Clear tags"),		ACTION_CLEAR_TAGS },
	{ N_("Threads"), 	N_("Ignore thread"),	        ACTION_IGNORE },
	{ N_("Threads"), 	N_("Watch thread"),	        ACTION_WATCH },
	{ NULL, 		N_("Add to address book"),	ACTION_ADD_TO_ADDRESSBOOK },
	{ NULL, 		N_("Stop filter"),		ACTION_STOP },
	{ NULL,			NULL,				0},
};

enum {
	ACTION_COMBO_TEXT,
	ACTION_COMBO_DATA,
	ACTION_COMBO_SENS,
	N_ACTION_COMBO
};

/*!
 *\brief	Hooks
 */
static PrefsFilteringActionSignal *filtering_action_callback;

static GtkTreeModel *prefs_filtering_action_create_model(void)
{
	GtkTreeIter iter, iter2;
	GtkTreeStore *store;
	gchar *curr_menu = NULL;
	gint i;
	
	store = gtk_tree_store_new(N_ACTION_COMBO, G_TYPE_STRING, G_TYPE_INT,
				   G_TYPE_BOOLEAN);
	
	for (i = 0; action_menu[i].menu || action_menu[i].text; i++)
	{
		if (action_menu[i].menu) {
			if (!curr_menu || strcmp(action_menu[i].menu, curr_menu)) {
				gtk_tree_store_append(store, &iter, NULL);
				gtk_tree_store_set(store, &iter,
						   ACTION_COMBO_TEXT,
						   gettext(action_menu[i].menu),
						   ACTION_COMBO_SENS, TRUE,
						   -1);
				curr_menu = action_menu[i].menu;
			} 
			
			gtk_tree_store_append(store, &iter2, &iter);
			gtk_tree_store_set(store, &iter2,
					   ACTION_COMBO_TEXT, gettext(action_menu[i].text),
					   ACTION_COMBO_DATA, action_menu[i].action,
					   ACTION_COMBO_SENS, TRUE,
					   -1);
		} else {
			curr_menu = NULL;
			
			gtk_tree_store_append(store, &iter, NULL);
			gtk_tree_store_set(store, &iter,
					   ACTION_COMBO_TEXT, gettext(action_menu[i].text),
					   ACTION_COMBO_DATA, action_menu[i].action,
					   ACTION_COMBO_SENS, TRUE,
					   -1); 
		}   
	}
	
	return GTK_TREE_MODEL(store);
} 

static void cell_is_sensitive(GtkCellLayout   *cell_layout,
			      GtkCellRenderer *cell,
			      GtkTreeModel    *tree_model,
			      GtkTreeIter     *iter,
			      gpointer         data)
{
	if(gtk_tree_model_iter_has_child (tree_model, iter)) {
		g_object_set (cell, "sensitive", FALSE, NULL);
	}
}

/*!
 *\brief	Opens the filtering action dialog with a list of actions
 *
 *\param	matchers List of conditions
 *\param	cb Callback
 *
 */
void prefs_filtering_action_open(GSList *action_list,
    PrefsFilteringActionSignal *cb)
{
	inc_lock();

	if (!filtering_action.window) {
		prefs_filtering_action_create();
	} else {
		/* update color label menu */
		gtk_cmoption_menu_set_menu(GTK_CMOPTION_MENU(filtering_action.color_optmenu),
				colorlabel_create_color_menu());
	}

	manage_window_set_transient(GTK_WINDOW(filtering_action.window));
	gtk_widget_grab_focus(filtering_action.ok_btn);

	filtering_action_callback = cb;

	prefs_filtering_action_set_dialog(action_list);

	gtk_widget_show(filtering_action.window);
}

/*!
 *\brief	Save Gtk object size to prefs dataset
 */
static void prefs_filtering_action_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.filteringactionwin_width = allocation->width;
	prefs_common.filteringactionwin_height = allocation->height;
}

#define LABELS_WIDTH		80
#define SECOND_ROW_WIDTH	250

static void prefs_filtering_action_check_widget_width(GtkWidget *widget)
{
	GtkRequisition req;
	
	gtk_widget_size_request(widget, &req);
	if(req.width > SECOND_ROW_WIDTH)
		gtk_widget_set_size_request(widget, SECOND_ROW_WIDTH, -1);
}

/*!
 *\brief	Create the matcher dialog
 */
static void prefs_filtering_action_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *confirm_area;

	GtkWidget *vbox1;
	GtkWidget *frame;
	GtkWidget *table;
	GtkWidget *hbox1;

	GtkWidget *label1;
	GtkWidget *label2;
	GtkWidget *label3;
	GtkWidget *action_combo;
	GtkWidget *account_combo;
	GtkWidget *header_combo;
	GtkWidget *header_entry;
	GtkWidget *addressbook_btn;
	GtkWidget *dest_entry;
	GtkWidget *dest_btn;
	GtkWidget *score_entry;
	GtkWidget *tags_combo;

	GtkWidget *reg_hbox;
	GtkWidget *btn_hbox;
	GtkWidget *arrow;
	GtkWidget *reg_btn;
	GtkWidget *subst_btn;
	GtkWidget *del_btn;

	GtkWidget *action_hbox;
	GtkWidget *action_scrolledwin;
	GtkWidget *action_list_view;

	GtkWidget *btn_vbox;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	GtkWidget *exec_btn;

	GtkWidget *color_optmenu;

	static GdkGeometry geometry;

        GList * accounts;
	GSList *tmp, *tags;

	GtkSizeGroup *size_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
	GtkSizeGroup *size_action = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
	GtkTreeModel *model;
	GtkCellRenderer *renderer;

	debug_print("Creating matcher configuration window...\n");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_filtering_action");
	gtk_container_set_border_width(GTK_CONTAINER(window), VBOX_BORDER);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window), TRUE);
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);

	vbox = gtk_vbox_new(FALSE, 6);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	gtkut_stock_button_set_create(&confirm_area,
				      &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_btn);

	gtk_window_set_title(GTK_WINDOW(window),
			     _("Action configuration"));
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(prefs_filtering_action_deleted), NULL);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(prefs_filtering_action_size_allocate_cb), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(prefs_filtering_action_key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(prefs_filtering_action_ok), NULL);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(prefs_filtering_action_cancel), NULL);

	vbox1 = gtk_vbox_new(FALSE, VSPACING);
	gtk_box_pack_start(GTK_BOX(vbox), vbox1, TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER (vbox1), 2);

	frame = gtk_frame_new(_("Rule"));
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.01, 0.5);
	gtk_box_pack_start (GTK_BOX (vbox1), frame, FALSE, FALSE, 0);	
	gtk_widget_set_size_request(frame, -1, 110);
	
	table = gtk_table_new(3, 3, FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(table), 2);
	gtk_table_set_row_spacings (GTK_TABLE (table), VSPACING_NARROW_2);
	gtk_table_set_col_spacings (GTK_TABLE (table), HSPACING_NARROW);
        gtk_container_add(GTK_CONTAINER(frame), table);
        
        /* first row labels */

	label1 = gtk_label_new (_("Action"));
	gtk_widget_set_size_request(label1, LABELS_WIDTH, -1);
	gtk_size_group_add_widget(size_group, label1);
	gtk_misc_set_alignment (GTK_MISC (label1), 1, 0.5);
	gtk_table_attach(GTK_TABLE(table), label1, 0, 1, 0, 1, 
			GTK_FILL, GTK_SHRINK, 0, 0);

	label2 = gtk_label_new ("");
	gtk_size_group_add_widget(size_group, label2);
	gtk_misc_set_alignment (GTK_MISC (label2), 1, 0.5);
	gtk_table_attach(GTK_TABLE(table), label2, 0, 1, 1, 2, 
			GTK_FILL, GTK_SHRINK, 0, 0);

	label3 = gtk_label_new ("");
	gtk_size_group_add_widget(size_group, label3);
	gtk_misc_set_alignment (GTK_MISC (label3), 1, 0.5);
	gtk_table_attach(GTK_TABLE(table), label3, 0, 1, 2, 3, 
			GTK_FILL, GTK_SHRINK, 0, 0);

	/* action combo */
	
	hbox1 = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox1, 1, 2, 0, 1, 
			GTK_FILL, GTK_SHRINK, 0, 0);
			
	model = prefs_filtering_action_create_model();
	action_combo = gtk_combo_box_new_with_model(model);
	gtk_size_group_add_widget(size_action, action_combo);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(action_combo), renderer, TRUE);
        gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(action_combo), renderer,
				       "text", ACTION_COMBO_TEXT,
				       "sensitive", ACTION_COMBO_SENS,
				       NULL);
	gtk_cell_layout_set_cell_data_func(GTK_CELL_LAYOUT(action_combo), renderer,
					   cell_is_sensitive, NULL, NULL);

	combobox_select_by_data(GTK_COMBO_BOX(action_combo), ACTION_MOVE);
	g_signal_connect(G_OBJECT(action_combo), "changed",
			 G_CALLBACK(prefs_filtering_action_type_selection_changed),
			 NULL);
	gtk_box_pack_start(GTK_BOX(hbox1), action_combo, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox1), gtk_label_new(""), TRUE, TRUE, 0);

	/* accounts */

	hbox1 = gtk_hbox_new (FALSE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox1, 1, 2, 1, 2, 
			 GTK_FILL | GTK_EXPAND, GTK_SHRINK, 0, 0);

	account_combo = gtk_combo_box_new_text ();
	gtk_size_group_add_widget(size_action, account_combo);

	for (accounts = account_get_list() ; accounts != NULL;
	     accounts = accounts->next) {
		PrefsAccount *ac = (PrefsAccount *)accounts->data;
		gchar *name = g_strdup(ac->account_name);
		gtk_combo_box_append_text(GTK_COMBO_BOX(account_combo), (gpointer) name);
		g_free(name);
	}

	prefs_filtering_action_check_widget_width(account_combo);
	gtk_combo_box_set_active(GTK_COMBO_BOX(account_combo), 0);
	gtk_box_pack_start (GTK_BOX (hbox1), account_combo,
			    FALSE, FALSE, 0);

	/* header */

	header_combo = combobox_text_new(TRUE, "From", "To", "Cc", "Reply-To", "Sender", NULL);
	gtk_size_group_add_widget(size_action, header_combo);
	gtk_box_pack_start (GTK_BOX (hbox1), header_combo,
			    FALSE, FALSE, 0);
	header_entry = gtk_bin_get_child(GTK_BIN((header_combo)));

	/* destination */

	hbox1 = gtk_hbox_new (FALSE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox1, 1, 2, 2, 3, 
			GTK_FILL | GTK_EXPAND, GTK_SHRINK, 0, 0);

	dest_entry = gtk_entry_new ();
	gtk_box_pack_start (GTK_BOX (hbox1), dest_entry, TRUE, TRUE, 0);
	
	score_entry = gtk_spin_button_new_with_range(-1000, 1000, 1);
	gtk_box_pack_start(GTK_BOX(hbox1), score_entry, FALSE, FALSE, 0);
	
	color_optmenu = gtk_cmoption_menu_new();
	gtk_size_group_add_widget(size_action, color_optmenu);
	gtk_cmoption_menu_set_menu(GTK_CMOPTION_MENU(color_optmenu),
				 colorlabel_create_color_menu());
	prefs_filtering_action_check_widget_width(color_optmenu);
	gtk_box_pack_start(GTK_BOX(hbox1), color_optmenu, FALSE, FALSE, 0);

	tags_combo = gtk_combo_box_new_text ();
	gtk_size_group_add_widget(size_action, tags_combo);

	for (tmp = tags = tags_get_list() ; tmp != NULL;
	     tmp = tmp->next) {
		gchar *name = g_strdup(tags_get_tag(GPOINTER_TO_INT(tmp->data)));

		gtk_combo_box_append_text(GTK_COMBO_BOX(tags_combo), (gpointer) name);
		g_free(name);
	}

	prefs_filtering_action_check_widget_width(tags_combo);
	gtk_combo_box_set_active(GTK_COMBO_BOX(tags_combo), 0);
	gtk_box_pack_start (GTK_BOX (hbox1), tags_combo,
			    FALSE, FALSE, 0);

	hbox1 = gtk_hbox_new (FALSE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox1, 2, 3, 2, 3, 
			GTK_FILL, GTK_SHRINK, 0, 0);

	dest_btn = gtk_button_new_with_label (_("Select ..."));
	gtk_box_pack_start (GTK_BOX (hbox1), dest_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (dest_btn), "clicked",
			  G_CALLBACK(prefs_filtering_action_select_dest),
			  NULL);

	addressbook_btn = gtk_button_new_with_label (_("Select ..."));
	gtk_box_pack_start (GTK_BOX (hbox1), addressbook_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (addressbook_btn), "clicked",
			  G_CALLBACK(prefs_filtering_action_select_addressbook),
			  NULL);

	exec_btn = gtk_button_new_from_stock(GTK_STOCK_INFO);
	gtk_box_pack_start (GTK_BOX (hbox1), exec_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (exec_btn), "clicked",
			  G_CALLBACK(prefs_filtering_action_exec_info),
			  window);

	/* register / substitute / delete */

	reg_hbox = gtk_hbox_new(FALSE, 4);
	gtk_box_pack_start(GTK_BOX(vbox1), reg_hbox, FALSE, FALSE, 0);

	arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_box_pack_start(GTK_BOX(reg_hbox), arrow, FALSE, FALSE, 0);
	gtk_widget_set_size_request(arrow, -1, 16);

	btn_hbox = gtk_hbox_new(TRUE, 4);
	gtk_box_pack_start(GTK_BOX(reg_hbox), btn_hbox, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_box_pack_start(GTK_BOX(btn_hbox), reg_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(reg_btn), "clicked",
			 G_CALLBACK(prefs_filtering_action_register_cb), NULL);

	subst_btn = gtkut_get_replace_btn(_("Replace"));
	gtk_box_pack_start(GTK_BOX(btn_hbox), subst_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(subst_btn), "clicked",
			 G_CALLBACK(prefs_filtering_action_substitute_cb),
			 NULL);

	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_box_pack_start(GTK_BOX(btn_hbox), del_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(del_btn), "clicked",
			 G_CALLBACK(prefs_filtering_action_delete_cb), NULL);

	action_hbox = gtk_hbox_new(FALSE, 8);
	gtk_box_pack_start(GTK_BOX(vbox1), action_hbox, TRUE, TRUE, 0);

	action_scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(action_scrolledwin, -1, 150);
	gtk_box_pack_start(GTK_BOX(action_hbox), action_scrolledwin,
			   TRUE, TRUE, 0);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(action_scrolledwin),
					    GTK_SHADOW_ETCHED_IN);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(action_scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	action_list_view = prefs_filtering_action_list_view_create();
	gtk_container_add(GTK_CONTAINER(action_scrolledwin), action_list_view);

	btn_vbox = gtk_vbox_new(FALSE, 8);
	gtk_box_pack_start(GTK_BOX(action_hbox), btn_vbox, FALSE, FALSE, 0);

	up_btn = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_box_pack_start(GTK_BOX(btn_vbox), up_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(up_btn), "clicked",
			 G_CALLBACK(prefs_filtering_action_up), NULL);

	down_btn = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_box_pack_start(GTK_BOX(btn_vbox), down_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(down_btn), "clicked",
			 G_CALLBACK(prefs_filtering_action_down), NULL);

	if (!geometry.min_height) {
		geometry.min_width = 490;
		geometry.min_height = 348;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.filteringactionwin_width,
				    prefs_common.filteringactionwin_height);

	gtk_widget_show_all(window);

	filtering_action.window    = window;
	filtering_action.label1 = label1;
	filtering_action.label2 = label2;
	filtering_action.label3 = label3;
	filtering_action.action_combo = action_combo;
	filtering_action.account_combo = account_combo;
	filtering_action.tags_combo = tags_combo;
	filtering_action.dest_entry = dest_entry;
	filtering_action.dest_btn = dest_btn;
	filtering_action.exec_btn = exec_btn;
	filtering_action.color_optmenu = color_optmenu;
	filtering_action.score_entry = score_entry;
	filtering_action.header_combo = header_combo;
	filtering_action.header_entry = header_entry;
	filtering_action.addressbook_btn = addressbook_btn;
	filtering_action.ok_btn = ok_btn;
	filtering_action.action_list_view = action_list_view;
	
	prefs_filtering_action_type_selection_changed(NULL, NULL);
}

/*!
 *\brief	Set the contents of a row
 *
 *\param	row Index of row to set
 *\param	prop Condition to set
 *
 */
static void prefs_filtering_action_list_view_set_row(GtkTreeIter *row, 
						     FilteringAction *action)
{
        gchar *buf;

	if (row == NULL && action == NULL) {
		prefs_filtering_action_list_view_insert_action
			(filtering_action.action_list_view,
			 NULL, _("(New)"), FALSE);
		return;
	}			 

        buf = filteringaction_to_string(action);

	prefs_filtering_action_list_view_insert_action
			(filtering_action.action_list_view,
			 row, buf, TRUE);
	g_free(buf);
}

/*!
 *\brief	Initializes dialog with a set of conditions
 *
 *\param	matchers List of conditions
 */
static void prefs_filtering_action_set_dialog(GSList *action_list)
{
	GSList *cur;

	gtk_list_store_clear(GTK_LIST_STORE(gtk_tree_view_get_model
			(GTK_TREE_VIEW(filtering_action.action_list_view))));

	prefs_filtering_action_list_view_set_row(NULL, NULL);
	if (action_list != NULL) {
		for (cur = action_list; cur != NULL;
		     cur = g_slist_next(cur)) {
			FilteringAction *action;
			action = (FilteringAction *) cur->data;
			prefs_filtering_action_list_view_set_row(NULL, action);
		}
	}
	
        prefs_filtering_action_reset_dialog();
        
        combobox_set_sensitive(GTK_COMBO_BOX(filtering_action.action_combo), 9,
        		(tags_get_size() > 0) ? TRUE : FALSE);
}

/*!
 *\brief	Converts current actions in list box in
 *		an action list used by the filtering system.
 *
 *\return	GSList * List of actions.
 */
static GSList *prefs_filtering_action_get_list(void)
{
	gchar *action_str;
	gboolean is_valid;
	gint row = 1;
	GSList *action_list;
	GtkTreeView *list_view = GTK_TREE_VIEW(filtering_action.action_list_view);
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	GtkTreeIter iter;

	action_list = NULL;

	while (gtk_tree_model_iter_nth_child(model, &iter, NULL, row)) {

		gtk_tree_model_get(model, &iter, 
			           PFA_ACTION, &action_str,
				   PFA_VALID_ACTION, &is_valid,
				   -1);

		if (is_valid) {				   
                        GSList * tmp_action_list;
			tmp_action_list = matcher_parser_get_action_list(action_str);
			
			if (tmp_action_list == NULL) {
				g_free(action_str);
				break;
			}				

			action_list = g_slist_concat(action_list,
                            tmp_action_list);
		}

		g_free(action_str);
		action_str = NULL;
		row ++;
		
	}

	return action_list;
}

/*!
 *\brief	Returns account ID from the given list index
 *
 *\return	gint account ID
 */
static gint get_account_id_from_list_id(gint list_id)
{
	GList * accounts;

	for (accounts = account_get_list() ; accounts != NULL;
	     accounts = accounts->next) {
		PrefsAccount *ac = (PrefsAccount *)accounts->data;

		if (list_id == 0)
			return ac->account_id;
		list_id--;
	}
	return 0;
}

/*!
 *\brief	Returns list index from the given account ID
 *
 *\return	gint list index
 */
static gint get_list_id_from_account_id(gint account_id)
{
	GList * accounts;
	gint list_id = 0;

	for (accounts = account_get_list() ; accounts != NULL;
	     accounts = accounts->next) {
		PrefsAccount *ac = (PrefsAccount *)accounts->data;

		if (account_id == ac->account_id)
			return list_id;
		list_id++;
	}
	return 0;
}


/*!
 *\brief	Returns parser action ID from internal action ID
 *
 *\return	gint parser action ID
 */
static gint prefs_filtering_action_get_matching_from_action(Action action_id)
{
	switch (action_id) {
	case ACTION_MOVE:
		return MATCHACTION_MOVE;
	case ACTION_SET_TAG:
		return MATCHACTION_SET_TAG;
	case ACTION_UNSET_TAG:
		return MATCHACTION_UNSET_TAG;
	case ACTION_CLEAR_TAGS:
		return MATCHACTION_CLEAR_TAGS;
	case ACTION_COPY:
		return MATCHACTION_COPY;
	case ACTION_DELETE:
		return MATCHACTION_DELETE;
	case ACTION_MARK:
		return MATCHACTION_MARK;
	case ACTION_UNMARK:
		return MATCHACTION_UNMARK;
	case ACTION_LOCK:
		return MATCHACTION_LOCK;
	case ACTION_UNLOCK:
		return MATCHACTION_UNLOCK;
	case ACTION_MARK_AS_READ:
		return MATCHACTION_MARK_AS_READ;
	case ACTION_MARK_AS_UNREAD:
		return MATCHACTION_MARK_AS_UNREAD;
	case ACTION_MARK_AS_SPAM:
		return MATCHACTION_MARK_AS_SPAM;
	case ACTION_MARK_AS_HAM:
		return MATCHACTION_MARK_AS_HAM;
	case ACTION_FORWARD:
		return MATCHACTION_FORWARD;
	case ACTION_FORWARD_AS_ATTACHMENT:
		return MATCHACTION_FORWARD_AS_ATTACHMENT;
	case ACTION_REDIRECT:
		return MATCHACTION_REDIRECT;
	case ACTION_EXECUTE:
		return MATCHACTION_EXECUTE;
	case ACTION_COLOR:
		return MATCHACTION_COLOR;
	case ACTION_HIDE:
		return MATCHACTION_HIDE;
	case ACTION_IGNORE:
		return MATCHACTION_IGNORE;
	case ACTION_WATCH:
		return MATCHACTION_WATCH;
	case ACTION_STOP:
		return MATCHACTION_STOP;
	case ACTION_CHANGE_SCORE:
		return MATCHACTION_CHANGE_SCORE;
	case ACTION_SET_SCORE:
		return MATCHACTION_SET_SCORE;
	case ACTION_ADD_TO_ADDRESSBOOK:
		return MATCHACTION_ADD_TO_ADDRESSBOOK;
	default:
		return -1;
	}
}

/*!
 *\brief	Returns action from the content of the dialog
 *
 *\param	alert specifies whether alert dialog boxes should be shown
 *                or not.
 *
 *\return	FilteringAction * action entered in the dialog box.
 */
static FilteringAction * prefs_filtering_action_dialog_to_action(gboolean alert)
{
	Action action_id;
	gint action_type;
	gint list_id;
	gint account_id;
	gchar * destination = NULL;
	gint labelcolor = 0;
        FilteringAction * action;
        gint score;
	gchar * header = NULL;
        
	action_id = combobox_get_active_data(GTK_COMBO_BOX(filtering_action.action_combo));
	action_type = prefs_filtering_action_get_matching_from_action(action_id);
	list_id = gtk_combo_box_get_active(GTK_COMBO_BOX(filtering_action.account_combo));
	account_id = get_account_id_from_list_id(list_id);
        score = 0;
        destination = NULL;
        
	switch (action_id) {
	case ACTION_MOVE:
	case ACTION_COPY:
	case ACTION_EXECUTE:
		destination = gtk_editable_get_chars(GTK_EDITABLE(filtering_action.dest_entry), 0, -1);
		if (*destination == '\0') {
			if (alert)
                                alertpanel_error(action_id == ACTION_EXECUTE 
						 ? _("Command-line not set")
						 : _("Destination is not set."));
			g_free(destination);
			return NULL;
		}
		break;
	case ACTION_FORWARD:
	case ACTION_FORWARD_AS_ATTACHMENT:
	case ACTION_REDIRECT:
		destination = gtk_editable_get_chars(GTK_EDITABLE(filtering_action.dest_entry), 0, -1);
		if (*destination == '\0') {
			if (alert)
                                alertpanel_error(_("Recipient is not set."));
			g_free(destination);
			return NULL;
		}
		break;
	case ACTION_COLOR:
		labelcolor = colorlabel_get_color_menu_active_item(
			gtk_cmoption_menu_get_menu(GTK_CMOPTION_MENU(filtering_action.color_optmenu)));
		destination = NULL;	
		break;
        case ACTION_CHANGE_SCORE:
        case ACTION_SET_SCORE:
        	score = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(
						filtering_action.score_entry));
		if (!score && action_id == ACTION_CHANGE_SCORE) {
			if (alert)
                                alertpanel_error(_("Score is not set"));
			return NULL;
		}
                break;
	case ACTION_ADD_TO_ADDRESSBOOK:
		header = gtk_editable_get_chars(GTK_EDITABLE(filtering_action.header_entry), 0, -1);
		if (*header == '\0') {
			if (alert)
                                alertpanel_error(_("Header is not set."));
			g_free(header);
			return NULL;
		}
		destination = gtk_editable_get_chars(GTK_EDITABLE(filtering_action.dest_entry), 0, -1);
		if (*destination == '\0') {
			if (alert)
                                alertpanel_error(_("Target addressbook/folder is not set."));
			g_free(destination);
			return NULL;
		}
		break;
	case ACTION_SET_TAG:
	case ACTION_UNSET_TAG:
		destination = gtk_combo_box_get_active_text(GTK_COMBO_BOX(filtering_action.tags_combo));
		if (!destination || *destination == '\0') {
			if (alert)
                                alertpanel_error(_("Tag name is empty."));
			g_free(destination);
			return NULL;
		}
		break;
	case ACTION_STOP:
	case ACTION_HIDE:
	case ACTION_IGNORE:
	case ACTION_WATCH:
        case ACTION_DELETE:
        case ACTION_MARK:
        case ACTION_UNMARK:
        case ACTION_LOCK:
        case ACTION_UNLOCK:
        case ACTION_MARK_AS_READ:
        case ACTION_MARK_AS_UNREAD:
        case ACTION_MARK_AS_SPAM:
        case ACTION_MARK_AS_HAM:
        case ACTION_CLEAR_TAGS:
	default:
		break;
	}
	action = filteringaction_new(action_type, account_id,
            destination, labelcolor, score, header);
	
	g_free(destination);
	return action;
}

/*!
 *\brief	Signal handler for register button
 */
static void prefs_filtering_action_register_cb(void)
{
	FilteringAction *action;
	
	action = prefs_filtering_action_dialog_to_action(TRUE);
	if (action == NULL)
		return;

	prefs_filtering_action_list_view_set_row(NULL, action);

	filteringaction_free(action);
	/* GTK 1 NOTE:
	 * (presumably gtk_list_select_item(), called by 
	 * prefs_filtering_action_reset_dialog() activates 
	 * what seems to be a bug. this causes any other 
	 * list items to be unselectable)
	 * prefs_filtering_action_reset_dialog(); */
	gtk_combo_box_set_active(GTK_COMBO_BOX(filtering_action.account_combo), 0);
	gtk_combo_box_set_active(GTK_COMBO_BOX(filtering_action.tags_combo), 0);
	gtk_entry_set_text(GTK_ENTRY(filtering_action.dest_entry), "");
}

/*!
 *\brief	Signal handler for substitute button
 */
static void prefs_filtering_action_substitute_cb(void)
{
	GtkTreeView *list_view = GTK_TREE_VIEW
			(filtering_action.action_list_view);
	GtkTreeSelection *selection = gtk_tree_view_get_selection(list_view);
	GtkTreeModel *model;
	gboolean is_valid;
	GtkTreeIter row;
	FilteringAction *action;

	if (!gtk_tree_selection_get_selected(selection, &model, &row))
		return;

	gtk_tree_model_get(model, &row, PFA_VALID_ACTION, &is_valid, -1);
	if (!is_valid)
		return;

	action = prefs_filtering_action_dialog_to_action(TRUE);
	if (action == NULL)
		return;

	prefs_filtering_action_list_view_set_row(&row, action);

	filteringaction_free(action);

	prefs_filtering_action_reset_dialog();
}

/*!
 *\brief	Signal handler for delete button
 */
static void prefs_filtering_action_delete_cb(void)
{
	GtkTreeView *list_view = GTK_TREE_VIEW
			(filtering_action.action_list_view);
	GtkTreeSelection *selection = gtk_tree_view_get_selection(list_view);
	GtkTreeModel *model;
	gboolean is_valid;
	GtkTreeIter row;

	if (!gtk_tree_selection_get_selected(selection, &model, &row))
		return;

	gtk_tree_model_get(model, &row, PFA_VALID_ACTION, &is_valid, -1);
	if (!is_valid)
		return;

	gtk_list_store_remove(GTK_LIST_STORE(model), &row);		

	prefs_filtering_action_reset_dialog();
}

/*!
 *\brief	Signal handler for 'move up' button
 */
static void prefs_filtering_action_up(void)
{
	GtkTreePath *prev, *sel, *try;
	GtkTreeIter isel;
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter iprev;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(filtering_action.action_list_view)),
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
static void prefs_filtering_action_down(void)
{
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter next, sel;
	GtkTreePath *try;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(filtering_action.action_list_view)),
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

/*!
 *\brief	Handle key press
 *
 *\param	widget Widget receiving key press
 *\param	event Key event
 *\param	data User data
 */
static gboolean prefs_filtering_action_key_pressed(GtkWidget *widget,
    GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_Escape) {
		prefs_filtering_action_cancel();
		return TRUE;		
	}
	return FALSE;
}

/*!
 *\brief	Cancel matcher dialog
 */
static void prefs_filtering_action_cancel(void)
{
	gtk_widget_destroy(filtering_action.window);
	filtering_action.window = NULL;
	inc_unlock();
}

/*!
 *\brief	Accept current matchers
 */
static void prefs_filtering_action_ok(void)
{
        GSList * action_list;
        GSList * cur;

	action_list = prefs_filtering_action_get_list();

        if (action_list == NULL) {
                alertpanel_error(_("No action was defined."));
                return;
        }

        if (filtering_action_callback != NULL)
                filtering_action_callback(action_list);
        for(cur = action_list ; cur != NULL ; cur = cur->next) {
                filteringaction_free(cur->data);
        }
        g_slist_free(action_list);

	gtk_widget_destroy(filtering_action.window);
	filtering_action.window = NULL;
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
static gint prefs_filtering_action_deleted(GtkWidget *widget,
    GdkEventAny *event, gpointer data)
{
	prefs_filtering_action_cancel();
	return TRUE;
}

/*
 * Strings describing exec format strings
 * 
 * When adding new lines, remember to put 2 strings for each line
 */
static gchar *exec_desc_strings[] = {
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
	NULL, NULL
};

static DescriptionWindow exec_desc_win = { 
	NULL,
        NULL, 
        2,
        N_("Filtering Action: 'Execute'"),
	N_("'Execute' allows you to send a message or message element "
	   "to an external program or script.\n"
	   "The following symbols can be used:"),
       exec_desc_strings
};

/*!
 *\brief	Show Execute action's info
 */
static void prefs_filtering_action_exec_info(GtkWidget *widget, GtkWidget *parent)
{
	exec_desc_win.parent = parent;
	description_window_create(&exec_desc_win);
}

static void prefs_filtering_action_select_dest(void)
{
	FolderItem *dest;
	gchar * path;

	dest = foldersel_folder_sel(NULL, FOLDER_SEL_COPY, NULL, FALSE);
	if (!dest) return;

	path = folder_item_get_identifier(dest);

	gtk_entry_set_text(GTK_ENTRY(filtering_action.dest_entry), path);
	g_free(path);
}

static void prefs_filtering_action_select_addressbook(void)
{
	const gchar *folderpath = NULL;
	gchar *new_path = NULL;

	folderpath = gtk_entry_get_text(GTK_ENTRY(filtering_action.dest_entry));
	new_path = addressbook_folder_selection(folderpath);
	if (new_path) {
		gtk_entry_set_text(GTK_ENTRY(filtering_action.dest_entry), new_path);
		g_free(new_path);
	} 
}

static void prefs_filtering_action_enable_widget(GtkWidget* widget, const gboolean enable)
{
	cm_return_if_fail(widget != NULL);

	if(enable == TRUE)
	{
		if(GTK_IS_COMBO_BOX(widget) || GTK_IS_COMBO_BOX_ENTRY(widget))
			gtk_combo_box_set_active(GTK_COMBO_BOX(widget), 0);
		else if(GTK_IS_SPIN_BUTTON(widget))
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget), 0);
		else if(GTK_IS_ENTRY(widget)) {
			gtk_entry_set_text(GTK_ENTRY(widget), "");
		} else if(GTK_IS_CMOPTION_MENU(widget))
			gtk_cmoption_menu_set_history(GTK_CMOPTION_MENU(widget), 0);
		
		gtk_widget_set_sensitive(widget, TRUE);
		gtk_widget_show(widget);
	} else {
		gtk_widget_set_sensitive(widget, FALSE);
		gtk_widget_hide(widget);
	}
}

#define ACTION_SEND(x)   (x == ACTION_FORWARD || x == ACTION_REDIRECT || \
			  x == ACTION_FORWARD_AS_ATTACHMENT)
#define ACTION_SCORE(x)  (x == ACTION_CHANGE_SCORE || x == ACTION_SET_SCORE)
#define ACTION_TAG(x)    (x == ACTION_SET_TAG || x == ACTION_UNSET_TAG)
#define ACTION_FILEOP(x) (x == ACTION_MOVE || x == ACTION_COPY)

static void prefs_filtering_action_type_selection_changed(GtkWidget *combo,
							  gpointer user_data)
{
	gint value;

	value = combobox_get_active_data(GTK_COMBO_BOX(filtering_action.action_combo));

	if (filtering_action.current_action != value) {
		if (ACTION_SEND(filtering_action.current_action)) {
			debug_print("unregistering address completion entry\n");
			address_completion_unregister_entry(GTK_ENTRY(filtering_action.dest_entry));
			address_completion_end(filtering_action.window);
		}
		if (ACTION_SEND(value)) {
			debug_print("registering address completion entry\n");
			address_completion_start(filtering_action.window);
			address_completion_register_entry(
					GTK_ENTRY(filtering_action.dest_entry),
					TRUE);
		}
		filtering_action.current_action = value;
	}

	prefs_filtering_action_enable_widget(filtering_action.label2,
		(ACTION_SEND(value) || value == ACTION_ADD_TO_ADDRESSBOOK));

	prefs_filtering_action_enable_widget(filtering_action.label3,
			(ACTION_SEND(value)    || ACTION_TAG(value) || 
			 ACTION_SCORE(value)   || ACTION_FILEOP(value) ||
			 value == ACTION_COLOR || value == ACTION_EXECUTE ||
			 value == ACTION_ADD_TO_ADDRESSBOOK));

	prefs_filtering_action_enable_widget(filtering_action.account_combo, 
						ACTION_SEND(value));

	prefs_filtering_action_enable_widget(filtering_action.tags_combo,
						ACTION_TAG(value));

	prefs_filtering_action_enable_widget(filtering_action.dest_entry,
			(ACTION_FILEOP(value) || value == ACTION_EXECUTE ||
			 ACTION_SEND(value)   || value == ACTION_ADD_TO_ADDRESSBOOK));
			 
	prefs_filtering_action_enable_widget(filtering_action.dest_btn,
						ACTION_FILEOP(value));

	prefs_filtering_action_enable_widget(filtering_action.exec_btn,
						(value == ACTION_EXECUTE));

	prefs_filtering_action_enable_widget(filtering_action.color_optmenu,
						(value == ACTION_COLOR));

	prefs_filtering_action_enable_widget(filtering_action.header_combo,
					(value == ACTION_ADD_TO_ADDRESSBOOK));	

	prefs_filtering_action_enable_widget(filtering_action.addressbook_btn,
					(value == ACTION_ADD_TO_ADDRESSBOOK));

	prefs_filtering_action_enable_widget(filtering_action.score_entry,
						ACTION_SCORE(value));
		
	switch(value) {
	case ACTION_FORWARD:
	case ACTION_FORWARD_AS_ATTACHMENT:
	case ACTION_REDIRECT:
		gtk_label_set_text(GTK_LABEL(filtering_action.label2), _("Account"));	
		gtk_label_set_text(GTK_LABEL(filtering_action.label3), _("Recipient"));
		break;
	case ACTION_ADD_TO_ADDRESSBOOK:
		gtk_label_set_text(GTK_LABEL(filtering_action.label2), _("Header Name"));
		gtk_label_set_text(GTK_LABEL(filtering_action.label3), _("Book/Folder"));
		break;
	case ACTION_COPY:
	case ACTION_MOVE:
		gtk_label_set_text(GTK_LABEL(filtering_action.label3), _("Destination"));
		break;
	case ACTION_COLOR:
		gtk_label_set_text(GTK_LABEL(filtering_action.label3), _("Color"));
		break;
	case ACTION_EXECUTE:
		gtk_label_set_text(GTK_LABEL(filtering_action.label3), _("Execute"));
		break;
	case ACTION_SET_SCORE:
	case ACTION_CHANGE_SCORE:
		gtk_label_set_text(GTK_LABEL(filtering_action.label3), _("Score"));
		break;
	case ACTION_SET_TAG:
	case ACTION_UNSET_TAG:
		gtk_label_set_text(GTK_LABEL(filtering_action.label3), _("Tag"));
		break;
	}
}

static void prefs_filtering_action_reset_dialog(void)
{
	combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo), ACTION_MOVE);
	gtk_combo_box_set_active(GTK_COMBO_BOX(filtering_action.account_combo), 0);
	gtk_entry_set_text(GTK_ENTRY(filtering_action.dest_entry), "");
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(filtering_action.score_entry), 0);
}

static GtkListStore* prefs_filtering_action_create_data_store(void)
{
	return gtk_list_store_new(N_PFA_COLUMNS,
				  G_TYPE_STRING,
				  G_TYPE_BOOLEAN,
				  -1);
}

static void prefs_filtering_action_list_view_insert_action(GtkWidget   *list_view,
							   GtkTreeIter *row,
							   const gchar *action,
							   gboolean	is_valid)
{
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model
					(GTK_TREE_VIEW(list_view)));
	GtkTreeIter iter;
	
	
	/* see if row exists, if not append */
	if (row == NULL)
		gtk_list_store_append(store, &iter);
	else
		iter = *row;

	gtk_list_store_set(store, &iter,
			   PFA_ACTION, action,
			   PFA_VALID_ACTION, is_valid,
			   -1);
}

static GtkWidget *prefs_filtering_action_list_view_create(void)
{
	GtkTreeView *list_view;
	GtkTreeModel *model;
	GtkTreeSelection *selector;

	model = GTK_TREE_MODEL(prefs_filtering_action_create_data_store());
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(model));
	g_object_unref(model);	
	
	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	gtk_tree_view_set_reorderable(list_view, TRUE);

	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);
	gtk_tree_selection_set_select_function
		(selector, prefs_filtering_actions_selected, NULL, NULL);
	
	/* create the columns */
	prefs_filtering_action_create_list_view_columns(list_view);

	return GTK_WIDGET(list_view);
}

static void prefs_filtering_action_create_list_view_columns(GtkTreeView *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Current action list"),
		 renderer,
		 "text", PFA_ACTION,
		 NULL);
	gtk_tree_view_append_column(list_view, column);		
}

static gboolean prefs_filtering_actions_selected
			(GtkTreeSelection *selector,
			 GtkTreeModel *model, 
			 GtkTreePath *path,
			 gboolean currently_selected,
			 gpointer data)
{
	gchar *action_str;
	FilteringAction *action;
        GSList * action_list;
	gint list_id;
	GtkTreeIter iter;
	gboolean is_valid;
	GtkWidget *menu;

	if (currently_selected)
		return TRUE;

	if (!gtk_tree_model_get_iter(model, &iter, path))
		return TRUE;

	gtk_tree_model_get(model, &iter, 
			   PFA_VALID_ACTION,  &is_valid,
			   -1);

	if (!is_valid) {
		prefs_filtering_action_reset_dialog();
		return TRUE;
	}

	gtk_tree_model_get(model, &iter, 
			   PFA_ACTION, &action_str,
			   -1);

	action_list = matcher_parser_get_action_list(action_str);
	g_free(action_str);

	if (action_list == NULL)
		return TRUE;

        action = action_list->data;
        g_slist_free(action_list);

	switch(action->type) {
	case MATCHACTION_MOVE:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_MOVE);
		break;
	case MATCHACTION_COPY:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_COPY);
		break;
	case MATCHACTION_DELETE:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_DELETE);
		break;
	case MATCHACTION_MARK:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_MARK);
		break;
	case MATCHACTION_UNMARK:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_UNMARK);
		break;
	case MATCHACTION_LOCK:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_LOCK);
		break;
	case MATCHACTION_UNLOCK:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_UNLOCK);
		break;
	case MATCHACTION_MARK_AS_READ:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_MARK_AS_READ);
		break;
	case MATCHACTION_MARK_AS_UNREAD:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_MARK_AS_UNREAD);
		break;
	case MATCHACTION_MARK_AS_SPAM:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_MARK_AS_SPAM);
		break;
	case MATCHACTION_MARK_AS_HAM:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_MARK_AS_HAM);
		break;
	case MATCHACTION_FORWARD:
		list_id = get_list_id_from_account_id(action->account_id);
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_FORWARD);
		gtk_combo_box_set_active(GTK_COMBO_BOX(filtering_action.account_combo),
				     list_id);
		break;
	case MATCHACTION_FORWARD_AS_ATTACHMENT:
		list_id = get_list_id_from_account_id(action->account_id);
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_FORWARD_AS_ATTACHMENT);
		gtk_combo_box_set_active(GTK_COMBO_BOX(filtering_action.account_combo),
				     list_id);
		break;
	case MATCHACTION_REDIRECT:
		list_id = get_list_id_from_account_id(action->account_id);
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_REDIRECT);
		gtk_combo_box_set_active(GTK_COMBO_BOX(filtering_action.account_combo),
				     list_id);
		break;
	case MATCHACTION_EXECUTE:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_EXECUTE);
		break;
	case MATCHACTION_COLOR:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_COLOR);
		gtk_cmoption_menu_set_history(GTK_CMOPTION_MENU(filtering_action.color_optmenu),
					    action->labelcolor + 1);
		menu = gtk_cmoption_menu_get_menu(GTK_CMOPTION_MENU(
						filtering_action.color_optmenu));
		g_signal_emit_by_name(G_OBJECT(menu), "selection-done", menu);
		break;
	case MATCHACTION_CHANGE_SCORE:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_CHANGE_SCORE);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(filtering_action.score_entry),
					  action->score);
		break;
	case MATCHACTION_SET_SCORE:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_SET_SCORE);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(filtering_action.score_entry),
					  action->score);
		break;
	case MATCHACTION_STOP:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_STOP);
		break;
	case MATCHACTION_HIDE:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_HIDE);
		break;
	case MATCHACTION_IGNORE:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_IGNORE);
		break;
	case MATCHACTION_WATCH:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_WATCH);
		break;
	case MATCHACTION_ADD_TO_ADDRESSBOOK:
		if (action->header)
			gtk_entry_set_text(GTK_ENTRY(filtering_action.header_entry), action->header);
		else
			gtk_entry_set_text(GTK_ENTRY(filtering_action.header_entry), "");
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_ADD_TO_ADDRESSBOOK);
		break;
	case MATCHACTION_SET_TAG:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_SET_TAG);
		combobox_select_by_text(GTK_COMBO_BOX(filtering_action.tags_combo),
					action->destination);
		break;
	case MATCHACTION_UNSET_TAG:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_UNSET_TAG);
		combobox_select_by_text(GTK_COMBO_BOX(filtering_action.tags_combo),
					action->destination);
		break;
	case MATCHACTION_CLEAR_TAGS:
		combobox_select_by_data(GTK_COMBO_BOX(filtering_action.action_combo),
				     ACTION_CLEAR_TAGS);
		break;
	default:
		g_warning("unhandled case !\n");
	}
	if (action->destination)
		gtk_entry_set_text(GTK_ENTRY(filtering_action.dest_entry), action->destination);
	else
		gtk_entry_set_text(GTK_ENTRY(filtering_action.dest_entry), "");

	filteringaction_free(action); /* XXX: memleak */
	return TRUE;
}
