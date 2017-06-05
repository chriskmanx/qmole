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

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "progressdialog.h"
#include "gtkutils.h"
#include "utils.h"
#include "prefs_common.h"

enum {
	PROGRESS_IMAGE,
	PROGRESS_ACCOUNT,
	PROGRESS_STATE,
	N_PROGRESS_COLUMNS
};

static gint progress_dialog_insert_account(ProgressDialog *progress,
					   gint	 	   row,
					   const gchar	  *account,
					   const gchar	  *status,
					   GdkPixbuf	  *image);

static void progress_dialog_hide_btn_cb(GtkWidget *widget, gpointer data);
static void progress_dialog_delete_event_btn_cb(GtkWidget *widget, gpointer data);

ProgressDialog *progress_dialog_create(void)
{
	ProgressDialog *progress;
	GtkWidget *dialog;
	GtkWidget *hbox;
	GtkWidget *vbox;
	GtkWidget *label;
	GtkWidget *hide_btn;
	GtkWidget *showlog_btn;
	GtkWidget *cancel_btn;
	GtkWidget *progressbar;
	GtkWidget *scrolledwin;
	GtkWidget *treeview;
	GtkListStore *store;
	GtkTreeSelection *selection;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	debug_print("Creating progress dialog...\n");
	progress = g_new0(ProgressDialog, 1);

	dialog = gtk_dialog_new();
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 8);
	gtk_window_set_resizable(GTK_WINDOW(dialog), TRUE);
	gtk_widget_realize(dialog);

	gtk_container_set_border_width
		(GTK_CONTAINER(gtk_dialog_get_action_area(GTK_DIALOG(dialog))), 0);
	vbox = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	gtk_box_set_spacing(GTK_BOX(vbox), 8);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox,
			   FALSE, FALSE, 8);
	gtk_widget_show(hbox);

	label = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 8);
	gtk_widget_show(label);

	hide_btn = gtk_dialog_add_button(GTK_DIALOG(dialog),
					   _("_Hide"),
					   GTK_RESPONSE_NONE);
	showlog_btn = gtk_dialog_add_button(GTK_DIALOG(dialog),
					   _("_View log"),
					   GTK_RESPONSE_NONE);
	cancel_btn = gtk_dialog_add_button(GTK_DIALOG(dialog),
					   GTK_STOCK_CANCEL,
					   GTK_RESPONSE_NONE);
	gtk_widget_grab_default(cancel_btn);
	gtk_widget_grab_focus(cancel_btn);

	progressbar = gtk_progress_bar_new();
	gtk_box_pack_start(GTK_BOX(vbox), progressbar,
			   FALSE, FALSE, 0);
	gtk_widget_show(progressbar);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(scrolledwin);
	gtk_box_pack_start(GTK_BOX(vbox), scrolledwin,
			   TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	store = gtk_list_store_new(N_PROGRESS_COLUMNS, GDK_TYPE_PIXBUF, G_TYPE_STRING,
				   G_TYPE_STRING, G_TYPE_POINTER);

	treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	g_object_unref(G_OBJECT(store));
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview), TRUE);
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(treeview),
				     prefs_common.use_stripes_everywhere);
	gtk_widget_show(treeview);
	gtk_container_add(GTK_CONTAINER(scrolledwin), treeview);
	gtk_widget_set_size_request(treeview, -1, 120);

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_BROWSE);

	renderer = gtk_cell_renderer_pixbuf_new();
	g_object_set(renderer, "xalign", 0.5, NULL);
	column = gtk_tree_view_column_new_with_attributes
		(NULL, renderer, "pixbuf", PROGRESS_IMAGE, NULL);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_FIXED);
	gtk_tree_view_column_set_fixed_width(column, 20);
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Account"), renderer, "text", PROGRESS_ACCOUNT, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_FIXED);
	gtk_tree_view_column_set_fixed_width(column, 160);
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Status"), renderer, "text", PROGRESS_STATE, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_FIXED);
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	/* the WM close and hide buttons are handled internally and not published to caller */
	g_signal_connect(G_OBJECT(dialog), "delete_event",
				G_CALLBACK(progress_dialog_delete_event_btn_cb), NULL);
	g_signal_connect(G_OBJECT(hide_btn), "clicked",
				G_CALLBACK(progress_dialog_hide_btn_cb), dialog);

	progress->window      = dialog;
	progress->label       = label;
	progress->showlog_btn  = showlog_btn;
	progress->cancel_btn  = cancel_btn;
	progress->progressbar = progressbar;
	progress->treeview    = treeview;
	progress->store       = store;

	return progress;
}

void progress_dialog_set_label(ProgressDialog *progress, gchar *str)
{
	gtk_label_set_text(GTK_LABEL(progress->label), str);
}

void progress_dialog_get_fraction(ProgressDialog *progress)
{
	gtk_progress_bar_get_fraction(GTK_PROGRESS_BAR(progress->progressbar));
}

void progress_dialog_set_fraction(ProgressDialog *progress,
				  gfloat percentage)
{
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress->progressbar),
				      percentage);
}

void progress_dialog_destroy(ProgressDialog *progress)
{
	if (progress) {
		gtk_widget_destroy(progress->window);
		g_free(progress);
	}
}

/*!
 *\return	gint Row where status was set
 */
gint progress_dialog_list_set_status(ProgressDialog *progress,
				     gint	     row,
				     const gchar    *status)
{
	return progress_dialog_insert_account(progress, row, NULL,
					      status, NULL);
}

/*!
 *\return	gint Row where data were set 
 */
gint progress_dialog_list_set(ProgressDialog	*progress,
			      gint		 row,
			      GdkPixbuf		*image,
			      const gchar	*account_name,
			      const gchar	*status)
{
	return progress_dialog_insert_account(progress, row, account_name,
					      status, image);
}

void progress_dialog_scroll_to_row(ProgressDialog *progress, gint row)
{
	GtkTreeModel *model = GTK_TREE_MODEL(progress->store);
	GtkTreeIter iter;
	GtkTreePath *path;

	if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, row))
		return;

	path = gtk_tree_model_get_path(model, &iter);
	gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(progress->treeview),
				     path, NULL, FALSE, 0.0, 0.0);
	gtk_tree_path_free(path);
}

static gint progress_dialog_insert_account(ProgressDialog *progress,
					   gint	 	   row,
					   const gchar 	  *account,
					   const gchar 	  *status,
					   GdkPixbuf	  *image)
{
	GtkTreeIter iter;
	GtkListStore *store = progress->store;
	gint result = -1;					
	
	if (account == NULL && status == NULL && image == NULL)
		return -1;

	/* see if row exists, if not append */
	if (row < 0 || !gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(store), 
						      &iter, NULL, row)) {
		result = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(store),
							NULL);
		gtk_list_store_append(store, &iter);
	} else
		result = row;

	/*
	 * XXX: uhm, when does the iter invalidate? sure not while
	 * just setting a row's column i hope?
	 */
	
	if (account)
		gtk_list_store_set(store, &iter, 
				   PROGRESS_ACCOUNT, account,
				   -1);
	if (status)
		gtk_list_store_set(store, &iter,
				   PROGRESS_STATE, status,
				   -1);
	if (image) 
		gtk_list_store_set(store, &iter,
				   PROGRESS_IMAGE, image,
				   -1);

	return result;
}

static void progress_dialog_hide_btn_cb(GtkWidget *widget, gpointer data)
{
		gtk_widget_hide(GTK_WIDGET(data));
}

static void progress_dialog_delete_event_btn_cb(GtkWidget *widget, gpointer data)
{
		gtk_widget_hide(widget);
}
