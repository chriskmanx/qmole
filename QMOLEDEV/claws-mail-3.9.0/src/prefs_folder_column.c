/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & the Claws Mail team
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

#include "prefs_gtk.h"
#include "prefs_common.h"
#include "prefs_folder_column.h"
#include "manage_window.h"
#include "folderview.h"
#include "mainwindow.h"
#include "inc.h"
#include "gtkutils.h"
#include "utils.h"

static void prefs_folder_column_set_config(FolderColumnState *state);

enum {
	SUMCOL_NAME,
	SUMCOL_TYPE,
	N_SUMCOL_COLUMNS
};

#define TARGET_INFO_SUMCOL  (0xFEEDBABE)

static const GtkTargetEntry row_targets[] = {
	{ "PREFS_SUM_COL_MODEL_ROW", GTK_TARGET_SAME_APP, TARGET_INFO_SUMCOL }
};


static struct _FolderColumnDialog
{
	GtkWidget *window;

	GtkWidget *stock_list_view;
	GtkWidget *shown_list_view;

	GtkWidget *add_btn;
	GtkWidget *remove_btn;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	GtkWidget *default_btn;

	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;

	gboolean finished;
} folder_col;

static const gchar *const col_name[N_FOLDER_COLS] = {
	N_("Folder"),		/* F_COL_FOLDER  */
	N_("New"),		/* F_COL_NEW     */
	N_("Unread"),		/* F_COL_UNREAD  */
	N_("Total"),		/* F_COL_TOTAL   */
};

static FolderColumnState default_state[N_FOLDER_COLS] = {
	{ F_COL_FOLDER   , TRUE  },
	{ F_COL_NEW      , TRUE  },
	{ F_COL_UNREAD   , TRUE  },
	{ F_COL_TOTAL    , TRUE  },
};

static void prefs_folder_column_create	(void);

static void prefs_folder_column_set_dialog	(FolderColumnState *state);
static void prefs_folder_column_set_view	(void);

/* callback functions */
static void prefs_folder_column_add	(void);
static void prefs_folder_column_remove	(void);

static void prefs_folder_column_up	(void);
static void prefs_folder_column_down	(void);

static void prefs_folder_column_set_to_default	(void);

static void prefs_folder_column_ok	(void);
static void prefs_folder_column_cancel	(void);

static gint prefs_folder_column_delete_event	(GtkWidget	*widget,
						 GdkEventAny	*event,
						 gpointer	 data);
static gboolean prefs_folder_column_key_pressed(GtkWidget	*widget,
						 GdkEventKey	*event,
						 gpointer	 data);

static GtkListStore *prefs_folder_column_create_store	(void);

static void prefs_folder_column_insert_column	(GtkListStore *store,
						 gint row,
						 const gchar *name,
						 FolderColumnType type);
					       
static FolderColumnType prefs_folder_column_get_column	(GtkWidget *list, 
								 gint row);

static GtkWidget *prefs_folder_column_list_view_create	(const gchar *name);

static void prefs_filtering_create_list_view_columns	(GtkWidget *list_view, 
							 const gchar *name);

static void drag_data_get	(GtkTreeView *tree_view, 
				 GdkDragContext *context, 
				 GtkSelectionData *data, 
				 guint info, 
				 guint time, 
				 GtkTreeModel *model);
			  
static void drag_data_received	(GtkTreeView *tree_view, 
				 GdkDragContext *context,
				 gint x, gint y, 
				 GtkSelectionData *data,
				 guint info, 
				 guint time, 
				 GtkTreeModel *model);

static void prefs_folder_column_shown_set_btn_sensitivity(void);
static void prefs_folder_column_shown_set_active(const gboolean active);
static void prefs_folder_column_stock_set_active(const gboolean active);
static void prefs_folder_column_shown_sel_changed(void);
static void prefs_folder_column_stock_sel_changed(void);


void prefs_folder_column_open(void)
{
	inc_lock();

	if (!folder_col.window)
		prefs_folder_column_create();

	manage_window_set_transient(GTK_WINDOW(folder_col.window));
	gtk_widget_grab_focus(folder_col.ok_btn);

	prefs_folder_column_set_dialog(NULL);

	gtk_widget_show(folder_col.window);
	gtk_window_set_modal(GTK_WINDOW(folder_col.window), TRUE);

	folder_col.finished = FALSE;
	while (folder_col.finished == FALSE)
		gtk_main_iteration();

	gtk_widget_hide(folder_col.window);
	gtk_window_set_modal(GTK_WINDOW(folder_col.window), FALSE);

	inc_unlock();
}

static void prefs_folder_column_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;

	GtkWidget *label_hbox;
	GtkWidget *label;

	GtkWidget *vbox1;

	GtkWidget *hbox1;
	GtkWidget *clist_hbox;
	GtkWidget *scrolledwin;
	GtkWidget *stock_list_view;
	GtkWidget *shown_list_view;

	GtkWidget *btn_vbox;
	GtkWidget *add_btn;
	GtkWidget *remove_btn;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	GtkWidget *btn_hbox;
	GtkWidget *default_btn;
	GtkWidget *confirm_area;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;

	debug_print("Creating folder column setting window...\n");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_folder_column");
	gtk_container_set_border_width(GTK_CONTAINER(window), 8);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	gtk_window_set_title(GTK_WINDOW(window),
			     _("Folder list columns configuration"));
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(prefs_folder_column_delete_event),
			 NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(prefs_folder_column_key_pressed),
			 NULL);

	vbox = gtk_vbox_new(FALSE, 6);
	gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	label_hbox = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(label_hbox);
	gtk_box_pack_start(GTK_BOX(vbox), label_hbox, FALSE, FALSE, 4);

	label = gtk_label_new
		(_("Select columns to be displayed in the folder list. You can modify\n"
		   "the order by using the Up / Down buttons or by dragging the items."));
	gtk_widget_show(label);
	gtk_box_pack_start(GTK_BOX(label_hbox), label, FALSE, FALSE, 4);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);

	vbox1 = gtk_vbox_new(FALSE, VSPACING);
	gtk_widget_show(vbox1);
	gtk_box_pack_start(GTK_BOX(vbox), vbox1, TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(vbox1), 2);

	hbox1 = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox1);
	gtk_box_pack_start(GTK_BOX(vbox1), hbox1, FALSE, TRUE, 0);

	clist_hbox = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(clist_hbox);
	gtk_box_pack_start(GTK_BOX(hbox1), clist_hbox, TRUE, TRUE, 0);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(scrolledwin, 180, 210);
	gtk_widget_show(scrolledwin);
	gtk_box_pack_start(GTK_BOX(clist_hbox), scrolledwin, TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

				       
	stock_list_view = prefs_folder_column_list_view_create
				(_("Hidden columns"));
	g_signal_connect(G_OBJECT(stock_list_view), "cursor-changed",
			   G_CALLBACK(prefs_folder_column_stock_sel_changed),
			   NULL);
	gtk_widget_show(stock_list_view);
	gtk_container_add(GTK_CONTAINER(scrolledwin), stock_list_view);

	/* add/remove button */
	btn_vbox = gtk_vbox_new(FALSE, 8);
	gtk_widget_show(btn_vbox);
	gtk_box_pack_start(GTK_BOX(hbox1), btn_vbox, FALSE, FALSE, 0);

	add_btn = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_widget_show(add_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), add_btn, FALSE, TRUE, 0);

	g_signal_connect(G_OBJECT(add_btn), "clicked",
			 G_CALLBACK(prefs_folder_column_add), NULL);

	clist_hbox = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(clist_hbox);
	gtk_box_pack_start(GTK_BOX(hbox1), clist_hbox, TRUE, TRUE, 0);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(scrolledwin, 180, 210);
	gtk_widget_show(scrolledwin);
	gtk_box_pack_start(GTK_BOX(clist_hbox), scrolledwin, TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	shown_list_view = prefs_folder_column_list_view_create
				(_("Displayed columns"));
	g_signal_connect(G_OBJECT(shown_list_view), "cursor-changed",
			   G_CALLBACK(prefs_folder_column_shown_sel_changed),
			   NULL);
	gtk_widget_show(shown_list_view);
	gtk_container_add(GTK_CONTAINER(scrolledwin), shown_list_view);

	/* up/down button */
	btn_vbox = gtk_vbox_new(FALSE, 8);
	gtk_widget_show(btn_vbox);
	gtk_box_pack_start(GTK_BOX(hbox1), btn_vbox, FALSE, FALSE, 0);

	remove_btn = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	gtk_widget_show(remove_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), remove_btn, FALSE, TRUE, 0);

	up_btn = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_widget_show(up_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), up_btn, FALSE, TRUE, 0);

	down_btn = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_widget_show(down_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), down_btn, FALSE, TRUE, 0);

	g_signal_connect(G_OBJECT(remove_btn), "clicked",
			 G_CALLBACK(prefs_folder_column_remove), NULL);
	g_signal_connect(G_OBJECT(up_btn), "clicked",
			 G_CALLBACK(prefs_folder_column_up), NULL);
	g_signal_connect(G_OBJECT(down_btn), "clicked",
			 G_CALLBACK(prefs_folder_column_down), NULL);

	btn_hbox = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(btn_hbox);
	gtk_box_pack_end(GTK_BOX(vbox), btn_hbox, FALSE, FALSE, 0);

	btn_vbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_show(btn_vbox);
	gtk_box_pack_start(GTK_BOX(btn_hbox), btn_vbox, FALSE, FALSE, 0);

	default_btn = gtk_button_new_with_label(_(" Use default "));
	gtk_widget_show(default_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), default_btn, TRUE, FALSE, 0);
	g_signal_connect(G_OBJECT(default_btn), "clicked",
			 G_CALLBACK(prefs_folder_column_set_to_default),
			 NULL);

	gtkut_stock_button_set_create(&confirm_area,
				      &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);
	gtk_widget_show(confirm_area);
	gtk_box_pack_end(GTK_BOX(btn_hbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_btn);

	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(prefs_folder_column_ok), NULL);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(prefs_folder_column_cancel), NULL);

	folder_col.window      = window;
	folder_col.add_btn     = add_btn;
	folder_col.remove_btn  = remove_btn;
	folder_col.up_btn      = up_btn;
	folder_col.down_btn    = down_btn;
	folder_col.ok_btn      = ok_btn;
	folder_col.cancel_btn  = cancel_btn;
	folder_col.stock_list_view = stock_list_view;
	folder_col.shown_list_view = shown_list_view;
	
	prefs_folder_column_shown_set_active(FALSE);
	prefs_folder_column_stock_set_active(FALSE);
}

FolderColumnState *prefs_folder_column_get_config(void)
{
	static FolderColumnState state[N_FOLDER_COLS];
	FolderColumnType type;
	gint pos;

	for (pos = 0; pos < N_FOLDER_COLS; pos++)
		state[pos].type = -1;

	for (type = 0; type < N_FOLDER_COLS; type++) {
		pos = prefs_common.folder_col_pos[type];
		if (pos < 0 || pos >= N_FOLDER_COLS ||
		    state[pos].type != -1) {
			g_warning("Wrong column position\n");
			prefs_folder_column_set_config(default_state);
			return default_state;
		}

		state[pos].type = type;
		state[pos].visible = prefs_common.folder_col_visible[type];
	}

	return state;
}

static void prefs_folder_column_set_config(FolderColumnState *state)
{
	FolderColumnType type;
	gint pos;

	for (pos = 0; pos < N_FOLDER_COLS; pos++) {
		type = state[pos].type;
		prefs_common.folder_col_visible[type] = state[pos].visible;
		prefs_common.folder_col_pos[type] = pos;
	}
}

static void prefs_folder_column_set_dialog(FolderColumnState *state)
{
	GtkListStore *stock_store, *shown_store;
	gint pos;
	FolderColumnType type;
	gchar *name;

	stock_store = GTK_LIST_STORE(gtk_tree_view_get_model
			(GTK_TREE_VIEW(folder_col.stock_list_view)));
	shown_store = GTK_LIST_STORE(gtk_tree_view_get_model
			(GTK_TREE_VIEW(folder_col.shown_list_view)));

	gtk_list_store_clear(stock_store);
	gtk_list_store_clear(shown_store);

	if (!state)
		state = prefs_folder_column_get_config();

	for (pos = 0; pos < N_FOLDER_COLS; pos++) {
		type = state[pos].type;
		name = gettext(col_name[type]);

		if (state[pos].visible)
			prefs_folder_column_insert_column(shown_store,
							   -1, name,
							   type);
		else
			prefs_folder_column_insert_column(stock_store,
							    -1, name,
							    type);
	}
}

static void prefs_folder_column_set_view(void)
{
	gint stock_n_rows, shown_n_rows;
	FolderColumnState state[N_FOLDER_COLS];
	FolderColumnType type;
	gint row, pos = 0;

	stock_n_rows = gtk_tree_model_iter_n_children
		(gtk_tree_view_get_model(GTK_TREE_VIEW
			(folder_col.stock_list_view)), NULL);
	shown_n_rows = gtk_tree_model_iter_n_children
		(gtk_tree_view_get_model(GTK_TREE_VIEW
			(folder_col.shown_list_view)), NULL);

	cm_return_if_fail
		(stock_n_rows + shown_n_rows == N_FOLDER_COLS);

	for (row = 0; row < stock_n_rows; row++) {
		type = prefs_folder_column_get_column
			(folder_col.stock_list_view, row);
		state[row].type = type;
		state[row].visible = FALSE;
	}

	pos = row;
	for (row = 0; row < shown_n_rows; row++) {
		type = prefs_folder_column_get_column
			(folder_col.shown_list_view, row);
		state[pos + row].type = type;
		state[pos + row].visible = TRUE;
	}

	prefs_folder_column_set_config(state);
	main_window_set_folder_column();
}

static void prefs_folder_column_add(void)
{
	GtkListStore *stock_store, *shown_store;
	GtkTreeIter stock_sel, shown_sel, shown_add;
	gboolean shown_sel_valid;
	gchar *name;
	FolderColumnType type;
	
	stock_store = GTK_LIST_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(folder_col.stock_list_view)));
	shown_store = GTK_LIST_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(folder_col.shown_list_view)));
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(folder_col.stock_list_view)),
		 NULL,
		 &stock_sel))
		return;

	shown_sel_valid = gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(folder_col.shown_list_view)),
		 NULL,
		 &shown_sel);
			 
	gtk_tree_model_get(GTK_TREE_MODEL(stock_store), &stock_sel,
			   SUMCOL_TYPE, &type,
			   -1);
			
	gtk_list_store_remove(stock_store, &stock_sel);

	gtk_list_store_insert_after(shown_store, &shown_add, 
				    shown_sel_valid ? &shown_sel : NULL);

	name = gettext(col_name[type]);				    
				    
	gtk_list_store_set(shown_store, &shown_add,
			   SUMCOL_NAME, name,
			   SUMCOL_TYPE, type,
			   -1);
	
	gtk_tree_selection_select_iter(gtk_tree_view_get_selection
		(GTK_TREE_VIEW(folder_col.shown_list_view)),
		 &shown_add);
	prefs_folder_column_shown_set_active(TRUE);
	prefs_folder_column_stock_set_active(FALSE);
}

static void prefs_folder_column_remove(void)
{
	GtkListStore *stock_store, *shown_store;
	GtkTreeIter shown_sel, stock_sel, stock_add;
	gboolean stock_sel_valid;
	gchar *name;
	FolderColumnType type;
	
	stock_store = GTK_LIST_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(folder_col.stock_list_view)));
	shown_store = GTK_LIST_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(folder_col.shown_list_view)));
		
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(folder_col.shown_list_view)),
		 NULL,
		 &shown_sel))
		return;

	stock_sel_valid = gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(folder_col.stock_list_view)),
		 NULL,
		 &stock_sel);
	
	gtk_tree_model_get(GTK_TREE_MODEL(shown_store), &shown_sel,
			   SUMCOL_TYPE, &type,
			   -1);
			
	gtk_list_store_remove(shown_store, &shown_sel);

	gtk_list_store_insert_after(stock_store, &stock_add, 
				    stock_sel_valid ? &stock_sel : NULL);

	name = gettext(col_name[type]);				    
				    
	gtk_list_store_set(stock_store, &stock_add,
			   SUMCOL_NAME, name,
			   SUMCOL_TYPE, type,
			   -1);
	
	gtk_tree_selection_select_iter(gtk_tree_view_get_selection
		(GTK_TREE_VIEW(folder_col.stock_list_view)),
		&stock_add);
	prefs_folder_column_shown_set_active(FALSE);
	prefs_folder_column_stock_set_active(TRUE);
}

static void prefs_folder_column_up(void)
{
	GtkTreePath *prev, *sel;
	GtkTreeIter isel;
	GtkListStore *shown_store;
	GtkTreeIter iprev;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(folder_col.shown_list_view)),
		 NULL,
		 &isel))
		return;

	shown_store = GTK_LIST_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(folder_col.shown_list_view)));

	sel = gtk_tree_model_get_path(GTK_TREE_MODEL(shown_store), 
				      &isel);
	if (!sel)
		return;

	prev = gtk_tree_path_copy(sel);		
	if (!gtk_tree_path_prev(prev)) {
		gtk_tree_path_free(prev);
		gtk_tree_path_free(sel);
		return;
	}

	gtk_tree_model_get_iter(GTK_TREE_MODEL(shown_store),
				&iprev, prev);
	gtk_tree_path_free(sel);
	gtk_tree_path_free(prev);

	gtk_list_store_swap(shown_store, &iprev, &isel);
	prefs_folder_column_shown_set_btn_sensitivity();
}

static void prefs_folder_column_down(void)
{
	GtkListStore *shown_store;
	GtkTreeIter next, sel;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(folder_col.shown_list_view)),
		 NULL,
		 &sel))
		return;

	shown_store = GTK_LIST_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(folder_col.shown_list_view)));

	next = sel;
	if (!gtk_tree_model_iter_next(GTK_TREE_MODEL(shown_store), &next)) 
		return;

	gtk_list_store_swap(shown_store, &next, &sel);
	prefs_folder_column_shown_set_btn_sensitivity();	
}

static void prefs_folder_column_set_to_default(void)
{
	prefs_folder_column_set_dialog(default_state);
	prefs_folder_column_shown_set_active(FALSE);
	prefs_folder_column_stock_set_active(FALSE);
}

static void prefs_folder_column_ok(void)
{
	if (!folder_col.finished) {
		folder_col.finished = TRUE;
		prefs_folder_column_set_view();
	}
}

static void prefs_folder_column_cancel(void)
{
	folder_col.finished = TRUE;
}

static gint prefs_folder_column_delete_event(GtkWidget *widget,
					      GdkEventAny *event,
					      gpointer data)
{
	folder_col.finished = TRUE;
	return TRUE;
}

static gboolean prefs_folder_column_key_pressed(GtkWidget *widget,
						 GdkEventKey *event,
						 gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape)
		folder_col.finished = TRUE;
	return FALSE;
}

static GtkListStore *prefs_folder_column_create_store(void)
{
	return gtk_list_store_new(N_SUMCOL_COLUMNS,
				  G_TYPE_STRING,
				  G_TYPE_INT,
				  -1);
}

static void prefs_folder_column_insert_column(GtkListStore *store,
					       gint row,
					       const gchar *name,
					       FolderColumnType type)
{
	GtkTreeIter iter;

	if (row >= 0) {
		if (!gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(store),
						   &iter, NULL, row))
			row = -1;						   
	}
	if (row < 0) {
		/* add new */
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
				   SUMCOL_NAME, name,
				   SUMCOL_TYPE, type,
				   -1);
		return;
	} else {
		/* change existing */
		gtk_list_store_set(store, &iter, 
				   SUMCOL_NAME, name,
				   SUMCOL_TYPE, type,
				   -1);
	}
}

/*!
 *\brief	Return the columnn type for a row
 */
static FolderColumnType prefs_folder_column_get_column(GtkWidget *list, gint row)
{	
	GtkTreeView *list_view = GTK_TREE_VIEW(list);
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	GtkTreeIter iter;
	FolderColumnType result;

	if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, row))
		return -1;
	
	gtk_tree_model_get(model, &iter, 
			   SUMCOL_TYPE, &result,
			   -1);
	
	return result;
}

static GtkWidget *prefs_folder_column_list_view_create(const gchar *name)
{
	GtkWidget *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(prefs_folder_column_create_store());
	list_view = gtk_tree_view_new_with_model(model);
	g_object_unref(G_OBJECT(model));
	
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(list_view),
				     prefs_common.use_stripes_everywhere);
	
	selector = gtk_tree_view_get_selection(GTK_TREE_VIEW(list_view));
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);

	prefs_filtering_create_list_view_columns(GTK_WIDGET(list_view), name);

	gtk_tree_view_enable_model_drag_source(GTK_TREE_VIEW(list_view),
					       GDK_BUTTON1_MASK,
					       row_targets,
					       G_N_ELEMENTS(row_targets), 
					       GDK_ACTION_MOVE);
					    
	gtk_tree_view_enable_model_drag_dest(GTK_TREE_VIEW(list_view), 
					     row_targets, 
					     G_N_ELEMENTS(row_targets), 
					     GDK_ACTION_MOVE);
	    	
	g_signal_connect(G_OBJECT(list_view), "drag_data_get",
			 G_CALLBACK(drag_data_get),
			 model);

	g_signal_connect(G_OBJECT(list_view), "drag_data_received",
			 G_CALLBACK(drag_data_received),
			 model);

	return list_view;
}

static void prefs_filtering_create_list_view_columns(GtkWidget *list_view, 
						     const gchar *name)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(name, renderer, "text", SUMCOL_NAME, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
}

static void drag_data_get(GtkTreeView *tree_view, GdkDragContext *context, 
			  GtkSelectionData *data, guint info, 
			  guint time, GtkTreeModel *model)
{
	GtkTreeIter iter;
	FolderColumnType type;
	GtkTreeModel *source_model;

	if (info != TARGET_INFO_SUMCOL) 
		return;

	if (!gtk_tree_selection_get_selected
			(gtk_tree_view_get_selection(tree_view),
			 &source_model, &iter)) 
		return;			 
	
	gtk_tree_model_get(source_model, &iter, 
			   SUMCOL_TYPE, &type,
			   -1);

	/* send the type */
	gtk_selection_data_set(data, gtk_selection_data_get_target(data), 8,
		(gchar *) &type, sizeof type);
}

static void drag_data_received(GtkTreeView *tree_view, GdkDragContext *context,
			       gint x, gint y, GtkSelectionData *data,
			       guint info, guint time, GtkTreeModel *model)
{
	GtkWidget *source;
	GtkTreePath *dst = NULL, *sel = NULL;
	GtkTreeIter isel, idst;
	GtkTreeViewDropPosition pos;
	FolderColumnType type;
	GtkTreeModel *sel_model;
	gchar *name;
	
	source = gtk_drag_get_source_widget(context);
	
	if (source == GTK_WIDGET(tree_view)) {
	
		/*
		 * Same widget: re-order
		 */
		 
		gtk_tree_selection_get_selected(gtk_tree_view_get_selection(tree_view),
					   NULL, &isel);
		sel = gtk_tree_model_get_path(model, &isel);
		gtk_tree_view_get_dest_row_at_pos(tree_view, x, y,
						  &dst, &pos);

		/* NOTE: dst is invalid if selection beyond last row, in that
		 * case move beyond last one (XXX_move_before(..., NULL)) */						  

		if (dst) 						  
			gtk_tree_model_get_iter(model, &idst, dst);
		else 
			gtk_list_store_move_before(GTK_LIST_STORE(model),
						   &isel,
						   NULL);

		/* we do not drag if no valid dst and sel, and when
		 * dst and sel are the same (moving after or before
		 * itself doesn't change order...) */
		if ((dst && sel) && gtk_tree_path_compare(sel, dst) != 0) {
			if (pos == GTK_TREE_VIEW_DROP_BEFORE
			||  pos == GTK_TREE_VIEW_DROP_INTO_OR_BEFORE)
				gtk_list_store_move_before(GTK_LIST_STORE(model),
							   &isel,
							   &idst);
			else
				gtk_list_store_move_after(GTK_LIST_STORE(model),
							  &isel,
							  &idst);
			
		} 
		gtk_tree_path_free(dst);					  
		gtk_tree_path_free(sel);
		gtk_drag_finish(context, TRUE, FALSE, time);
		
	} else if (source == folder_col.stock_list_view 
	||	   source == folder_col.shown_list_view) {
	
		/*
		 * Other widget: change and update
		 */

		
		/* get source information and remove */
		gtk_tree_selection_get_selected(gtk_tree_view_get_selection(
						GTK_TREE_VIEW(source)),
						&sel_model, &isel);
		type = *((gint *) gtk_selection_data_get_data(data));
		name = gettext(col_name[type]);
		gtk_list_store_remove(GTK_LIST_STORE(sel_model), &isel);

		/* get insertion position */
		gtk_tree_view_get_dest_row_at_pos(tree_view, x, y, &dst, &pos);

		/* NOTE: dst is invalid if insertion point beyond last row, 
		 * just append to list in that case (XXX_store_append()) */

		if (dst) {
			gtk_tree_model_get_iter(model, &idst, dst);

			if (pos == GTK_TREE_VIEW_DROP_BEFORE
			||  pos == GTK_TREE_VIEW_DROP_INTO_OR_BEFORE)
				gtk_list_store_insert_before(GTK_LIST_STORE(model),
							     &isel,
							     &idst);
			else
				gtk_list_store_insert_after(GTK_LIST_STORE(model),
							    &isel,
							    &idst);
		} else
			gtk_list_store_append(GTK_LIST_STORE(model),
					      &isel);
		
		gtk_list_store_set(GTK_LIST_STORE(model), &isel,
				   SUMCOL_NAME, name,
				   SUMCOL_TYPE, type, -1);
		gtk_tree_path_free(dst);
		gtk_drag_finish(context, TRUE, FALSE, time);
	}

	prefs_folder_column_shown_set_active(FALSE);
	prefs_folder_column_stock_set_active(FALSE);
	
	/* XXXX: should we call gtk_drag_finish() for other code paths? */
}

static void prefs_folder_column_shown_set_btn_sensitivity(void)
{
	GtkTreeModel *model = GTK_TREE_MODEL(gtk_tree_view_get_model(
		GTK_TREE_VIEW(folder_col.shown_list_view)));
	GtkTreeSelection *selection = gtk_tree_view_get_selection(
		GTK_TREE_VIEW(folder_col.shown_list_view));
	GtkTreeIter iter;
	GtkTreePath *path;
	
	if(!gtk_tree_selection_get_selected(selection, NULL, &iter)) {
		gtk_widget_set_sensitive(folder_col.remove_btn, FALSE);
		gtk_widget_set_sensitive(folder_col.up_btn, FALSE);
		gtk_widget_set_sensitive(folder_col.down_btn, FALSE);
		return;
	}
	
	path = gtk_tree_model_get_path(model, &iter);

	gtk_widget_set_sensitive(folder_col.up_btn, gtk_tree_path_prev(path));
	gtk_widget_set_sensitive(folder_col.down_btn,
				 gtk_tree_model_iter_next(model, &iter));
	gtk_tree_path_free(path);
}

static void prefs_folder_column_shown_set_active(const gboolean active)
{
	GtkTreeSelection *selection = NULL;
	
	gtk_widget_set_sensitive(folder_col.remove_btn, active);
	
	if(active == FALSE) {
		selection = gtk_tree_view_get_selection(
			GTK_TREE_VIEW(folder_col.shown_list_view));
		gtk_tree_selection_unselect_all(selection);
		
		gtk_widget_set_sensitive(folder_col.up_btn, FALSE);
		gtk_widget_set_sensitive(folder_col.down_btn, FALSE);
	} else {
		prefs_folder_column_shown_set_btn_sensitivity();
	}
}

static void prefs_folder_column_stock_set_active(const gboolean active)
{
	GtkTreeSelection *selection = NULL;
	
	gtk_widget_set_sensitive(folder_col.add_btn, active);
	
	if(active == FALSE) {
		selection = gtk_tree_view_get_selection(
			GTK_TREE_VIEW(folder_col.stock_list_view));
		gtk_tree_selection_unselect_all(selection);
	}
}

static void prefs_folder_column_stock_sel_changed(void)
{
	GtkTreeSelection *selection = gtk_tree_view_get_selection(
		GTK_TREE_VIEW(folder_col.stock_list_view));
	prefs_folder_column_stock_set_active(
		(selection != NULL) ? TRUE : FALSE);
	prefs_folder_column_shown_set_active(
		(selection != NULL) ? FALSE : TRUE);
}

static void prefs_folder_column_shown_sel_changed(void)
{
	GtkTreeSelection *selection = gtk_tree_view_get_selection(
		GTK_TREE_VIEW(folder_col.shown_list_view));
	prefs_folder_column_shown_set_active(
		(selection != NULL) ? TRUE : FALSE);
	prefs_folder_column_stock_set_active(
		(selection != NULL) ? FALSE : TRUE);
}
