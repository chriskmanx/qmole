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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>

#include "main.h"
#include "utils.h"
#include "gtkutils.h"
#include "stock_pixmap.h"
#include "foldersel.h"
#include "alertpanel.h"
#include "manage_window.h"
#include "folderview.h"
#include "inputdialog.h"
#include "folder.h"
#include "prefs_common.h"

enum {
	FOLDERSEL_FOLDERNAME,
	FOLDERSEL_FOLDERITEM,
	FOLDERSEL_PIXBUF,
	FOLDERSEL_PIXBUF_OPEN,
	FOLDERSEL_FOREGROUND,
	FOLDERSEL_BOLD,
	N_FOLDERSEL_COLUMNS
};

typedef struct _FolderItemSearch	FolderItemSearch;

struct _FolderItemSearch
{
	FolderItem *item;
	GtkTreePath *path;
	GtkTreeIter iter;
};

static GdkPixbuf *folder_pixbuf = NULL;
static GdkPixbuf *folderopen_pixbuf = NULL;
static GdkPixbuf *foldernoselect_pixbuf = NULL;

static GtkWidget *window;
static GtkWidget *treeview;
static GtkWidget *entry;
static GtkWidget *ok_button;
static GtkWidget *cancel_button;
static GtkWidget *new_button;
static gboolean   root_selectable;

static FolderItem *folder_item;
static FolderItem *selected_item;

static GtkTreeStore *tree_store;

static gboolean cancelled;
static gboolean finished;

static void foldersel_create		(void);
static void foldersel_init		(void);

static void foldersel_append_item	(GtkTreeStore	*store,
					 FolderItem	*item,
					 GtkTreeIter	*iter,
					 GtkTreeIter	*parent);

static void foldersel_set_tree		(Folder			*cur_folder,
					 FolderSelectionType	 type);

static gboolean foldersel_selected	(GtkTreeSelection *selection,
					 GtkTreeModel	  *model,
					 GtkTreePath	  *path,
					 gboolean	   currently_selected,
					 gpointer	   data);

static void foldersel_ok		(GtkButton	*button,
					 gpointer	 data);
static void foldersel_cancel		(GtkButton	*button,
					 gpointer	 data);
static void foldersel_new_folder	(GtkButton	*button,
					 gpointer	 data);

static void foldersel_entry_activated	(GtkEntry	*entry,
					 gpointer	 data);

static void foldersel_tree_activated	(GtkTreeView		*treeview,
					 GtkTreePath		*path,
					 GtkTreeViewColumn	*column,
					 gpointer		 data);

static gint delete_event		(GtkWidget	*widget,
					 GdkEventAny	*event,
					 gpointer	 data);
static gboolean key_pressed		(GtkWidget	*widget,
					 GdkEventKey	*event,
					 gpointer	 data);

static gint foldersel_folder_name_compare	(GtkTreeModel	*model,
						 GtkTreeIter	*a,
						 GtkTreeIter	*b,
						 gpointer	 context);

static gboolean tree_view_folder_item_func	(GtkTreeModel	  *model,
						 GtkTreePath	  *path,
						 GtkTreeIter	  *iter,
						 FolderItemSearch *data);

FolderItem *foldersel_folder_sel(Folder *cur_folder, FolderSelectionType type,
				 const gchar *default_folder, gboolean can_sel_mailbox)
{
	selected_item = NULL;
	root_selectable = can_sel_mailbox;

	if (!window) {
		foldersel_create();
		foldersel_init();
	}

	foldersel_set_tree(cur_folder, type);

	/* select current */
	if (folder_item) {
		FolderItemSearch fis;

		fis.item = folder_item;
		fis.path = NULL;

		/* find matching model entry */
		gtk_tree_model_foreach
			(GTK_TREE_MODEL(tree_store),
			 (GtkTreeModelForeachFunc)tree_view_folder_item_func,
			 &fis);

		if (fis.path) {
			GtkTreeSelection *selection;

			selection = gtk_tree_view_get_selection
				(GTK_TREE_VIEW(treeview));
			gtk_tree_selection_select_iter(selection, &fis.iter);
			gtk_tree_view_set_cursor(GTK_TREE_VIEW(treeview),
						 fis.path, NULL, FALSE);
			gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(treeview),
						     fis.path,
						     NULL, TRUE, 0.5, 0.0);
			gtk_tree_path_free(fis.path);
		} else
			gtk_tree_view_scroll_to_point
				(GTK_TREE_VIEW(treeview), 0, 0);
	} else
		gtk_tree_view_scroll_to_point(GTK_TREE_VIEW(treeview), 0, 0);

	gtk_widget_grab_focus(ok_button);
	gtk_widget_grab_focus(treeview);

	gtk_widget_show(window);
	gtk_window_set_modal(GTK_WINDOW(window), TRUE);
	manage_window_set_transient(GTK_WINDOW(window));

	cancelled = finished = FALSE;

	while (finished == FALSE)
		gtk_main_iteration();

	gtk_widget_hide(window);
	gtk_window_set_modal(GTK_WINDOW(window), FALSE);
	gtk_entry_set_text(GTK_ENTRY(entry), "");
	gtk_tree_store_clear(tree_store);

	if (!cancelled &&
	    selected_item && (selected_item->path || root_selectable)) {
		folder_item = selected_item;
		return folder_item;
	} else
		return NULL;
}

static void foldersel_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.folderselwin_width = allocation->width;
	prefs_common.folderselwin_height = allocation->height;
}

static void foldersel_create(void)
{
	GtkWidget *vbox;
	GtkWidget *scrolledwin;
	GtkWidget *confirm_area;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;
	GtkTreeSelection *selection;
	static GdkGeometry geometry;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "foldersel");
	gtk_window_set_title(GTK_WINDOW(window), _("Select folder"));
	gtk_container_set_border_width(GTK_CONTAINER(window), 4);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);

	gtk_widget_realize(window);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(delete_event), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(foldersel_size_allocate_cb), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);

	vbox = gtk_vbox_new(FALSE, 4);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);
	gtk_box_pack_start(GTK_BOX(vbox), scrolledwin, TRUE, TRUE, 0);

	tree_store = gtk_tree_store_new(N_FOLDERSEL_COLUMNS,
					G_TYPE_STRING,
					G_TYPE_POINTER,
					GDK_TYPE_PIXBUF,
					GDK_TYPE_PIXBUF,
					GDK_TYPE_COLOR,
					G_TYPE_INT);
	gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(tree_store),
					FOLDERSEL_FOLDERNAME,
					foldersel_folder_name_compare,
					NULL, NULL);

	treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(tree_store));
	g_object_unref(G_OBJECT(tree_store));
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview), FALSE);
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(treeview),
	                             prefs_common.use_stripes_everywhere);
	gtk_tree_view_set_enable_tree_lines(GTK_TREE_VIEW(treeview), FALSE);
	gtk_tree_view_set_search_column(GTK_TREE_VIEW(treeview),
					FOLDERSEL_FOLDERNAME);

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_BROWSE);
	gtk_tree_selection_set_select_function(selection, foldersel_selected,
					       NULL, NULL);

	g_signal_connect(G_OBJECT(treeview), "row-activated",
			 G_CALLBACK(foldersel_tree_activated), NULL);
	gtk_container_add(GTK_CONTAINER(scrolledwin), treeview);

	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_spacing(column, 2);
	renderer = gtk_cell_renderer_pixbuf_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	gtk_tree_view_column_set_attributes
		(column, renderer,
		 "pixbuf", FOLDERSEL_PIXBUF,
		 "pixbuf-expander-open", FOLDERSEL_PIXBUF_OPEN,
		 "pixbuf-expander-closed", FOLDERSEL_PIXBUF,
		 NULL);

	/* create text renderer */
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes
		(column, renderer,
		 "text", FOLDERSEL_FOLDERNAME,
		 "foreground-gdk", FOLDERSEL_FOREGROUND,
		 "weight", FOLDERSEL_BOLD,
		 NULL);
	g_object_set(G_OBJECT(renderer), "weight", PANGO_WEIGHT_BOLD, NULL);
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), entry, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(entry), "activate",
			 G_CALLBACK(foldersel_entry_activated), NULL);

	gtkut_stock_button_set_create(&confirm_area,
				      &new_button,    GTK_STOCK_NEW,
				      &cancel_button, GTK_STOCK_CANCEL,
				      &ok_button,     GTK_STOCK_OK);

	gtk_box_pack_end(GTK_BOX(vbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_button);

	g_signal_connect(G_OBJECT(ok_button), "clicked",
			 G_CALLBACK(foldersel_ok), NULL);
	g_signal_connect(G_OBJECT(cancel_button), "clicked",
			 G_CALLBACK(foldersel_cancel), NULL);
	g_signal_connect(G_OBJECT(new_button), "clicked",
			 G_CALLBACK(foldersel_new_folder), NULL);

	if (!geometry.min_height) {
		geometry.min_width = 300;
		geometry.min_height = 360;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.folderselwin_width,
				    prefs_common.folderselwin_height);

	gtk_widget_show_all(vbox);
}

static void foldersel_init(void)
{
	stock_pixbuf_gdk(treeview, STOCK_PIXMAP_DIR_CLOSE,
			 &folder_pixbuf);
	stock_pixbuf_gdk(treeview, STOCK_PIXMAP_DIR_OPEN,
			 &folderopen_pixbuf);
	stock_pixbuf_gdk(treeview, STOCK_PIXMAP_DIR_NOSELECT,
			 &foldernoselect_pixbuf);
}

void foldersel_reflect_prefs_pixmap_theme(void)
{
	if (folder_pixbuf)
		g_object_unref(folder_pixbuf);
	if (folderopen_pixbuf)
		g_object_unref(folderopen_pixbuf);
	if (foldernoselect_pixbuf)
		g_object_unref(foldernoselect_pixbuf);
	foldersel_init();
}

static void foldersel_append_item(GtkTreeStore *store, FolderItem *item,
				  GtkTreeIter *iter, GtkTreeIter *parent)
{
	gchar *name, *tmpname;
	GdkPixbuf *pixbuf, *pixbuf_open;
	gboolean use_color;
	PangoWeight weight = PANGO_WEIGHT_NORMAL;
	GdkColor *foreground = NULL;
	static GdkColor color_noselect = {0, COLOR_DIM, COLOR_DIM, COLOR_DIM};
	static GdkColor color_new;

	gtkut_convert_int_to_gdk_color(prefs_common.color_new, &color_new);

        name = tmpname = folder_item_get_name(item);

	if (item->stype != F_NORMAL && FOLDER_IS_LOCAL(item->folder)) {
		switch (item->stype) {
		case F_INBOX:
			if (!strcmp2(item->name, INBOX_DIR))
				name = _("Inbox");
			break;
		case F_OUTBOX:
			if (!strcmp2(item->name, OUTBOX_DIR))
				name = _("Sent");
			break;
		case F_QUEUE:
			if (!strcmp2(item->name, QUEUE_DIR))
				name = _("Queue");
			break;
		case F_TRASH:
			if (!strcmp2(item->name, TRASH_DIR))
				name = _("Trash");
			break;
		case F_DRAFT:
			if (!strcmp2(item->name, DRAFT_DIR))
				name = _("Drafts");
			break;
		default:
			break;
		}
	}

	if (folder_has_parent_of_type(item, F_QUEUE) && item->total_msgs > 0) {
		name = g_strdup_printf("%s (%d)", name, item->total_msgs);
	} else if (item->unread_msgs > 0) {
		name = g_strdup_printf("%s (%d)", name, item->unread_msgs);
	} else
		name = g_strdup(name);

	pixbuf = item->no_select ? foldernoselect_pixbuf : folder_pixbuf;
	pixbuf_open =
		item->no_select ? foldernoselect_pixbuf : folderopen_pixbuf;

	if (folder_has_parent_of_type(item, F_DRAFT) ||
	    folder_has_parent_of_type(item, F_OUTBOX) ||
	    folder_has_parent_of_type(item, F_TRASH)) {
		use_color = FALSE;
	} else if (folder_has_parent_of_type(item, F_QUEUE)) {
		use_color = (item->total_msgs > 0);
		if (item->total_msgs > 0)
			weight = PANGO_WEIGHT_BOLD;
	} else {
		if (item->unread_msgs > 0)
			weight = PANGO_WEIGHT_BOLD;
		use_color = (item->new_msgs > 0);
	}

	if (item->no_select)
		foreground = &color_noselect;
	else if (use_color)
		foreground = &color_new;

	/* insert this node */
	gtk_tree_store_append(store, iter, parent);
	gtk_tree_store_set(store, iter,
			   FOLDERSEL_FOLDERNAME, name,
			   FOLDERSEL_FOLDERITEM, item,
			   FOLDERSEL_PIXBUF, pixbuf,
			   FOLDERSEL_PIXBUF_OPEN, pixbuf_open,
			   FOLDERSEL_FOREGROUND, foreground,
			   FOLDERSEL_BOLD, weight,
			   -1);
        
        g_free(tmpname);
}

static void foldersel_insert_gnode_in_store(GtkTreeStore *store, GNode *node,
					    GtkTreeIter *parent)
{
	FolderItem *item;
	GtkTreeIter child;
	GNode *iter;

	cm_return_if_fail(node != NULL);
	cm_return_if_fail(node->data != NULL);
	cm_return_if_fail(store != NULL);

	item = FOLDER_ITEM(node->data);
	foldersel_append_item(store, item, &child, parent);

	/* insert its children (this node as parent) */
	for (iter = node->children; iter != NULL; iter = iter->next)
		foldersel_insert_gnode_in_store(store, iter, &child);
}

static void foldersel_set_tree(Folder *cur_folder, FolderSelectionType type)
{
	Folder *folder;
	GList *list;

	for (list = folder_get_list(); list != NULL; list = list->next) {
		folder = FOLDER(list->data);
		cm_return_if_fail(folder != NULL);

		if (type != FOLDER_SEL_ALL) {
			if (FOLDER_TYPE(folder) == F_NEWS)
				continue;
		}

		foldersel_insert_gnode_in_store(tree_store, folder->node, NULL);
	}

	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(tree_store),
					     FOLDERSEL_FOLDERNAME,
					     GTK_SORT_ASCENDING);

	gtk_tree_view_expand_all(GTK_TREE_VIEW(treeview));
}

#include "localfolder.h"
static gboolean foldersel_selected(GtkTreeSelection *selection,
				   GtkTreeModel *model, GtkTreePath *path,
				   gboolean currently_selected, gpointer data)
{
	GtkTreeIter iter;
	FolderItem *item = NULL;

	if (currently_selected)
		return TRUE;

	if (!gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path))
		return TRUE;

	gtk_tree_model_get(GTK_TREE_MODEL(tree_store), &iter,
			   FOLDERSEL_FOLDERITEM, &item, -1);

	selected_item = item;
	if (selected_item && selected_item->path) {
		gchar *id;
		id = folder_item_get_identifier(selected_item);
		gtk_entry_set_text(GTK_ENTRY(entry), id);
		g_free(id);
	} else
	if (root_selectable && selected_item && selected_item->folder &&
			(FOLDER_TYPE(selected_item->folder) == F_MH ||
			 FOLDER_TYPE(selected_item->folder) == F_MBOX ||
			 FOLDER_TYPE(selected_item->folder) == F_IMAP)) {
		gchar *id = folder_get_identifier(selected_item->folder);
		gtk_entry_set_text(GTK_ENTRY(entry), id);
		g_free(id);
	} else
		gtk_entry_set_text(GTK_ENTRY(entry), "");

	return TRUE;
}

static void foldersel_ok(GtkButton *button, gpointer data)
{
	finished = TRUE;
}

static void foldersel_cancel(GtkButton *button, gpointer data)
{
	cancelled = TRUE;
	finished = TRUE;
}

static void foldersel_new_folder(GtkButton *button, gpointer data)
{
	FolderItem *new_item;
	gchar *new_folder;
	gchar *disp_name;
	gchar *p;
	GtkTreeIter selected, new_child;
	GtkTreePath *selected_p, *new_child_p;
	GtkTreeStore *store;
	GtkTreeModel *model;
	GtkTreeSelection *selection;

	if (!selected_item || FOLDER_TYPE(selected_item->folder) == F_NEWS)
		return;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	if (!gtk_tree_selection_get_selected(selection, &model, &selected))
		return;
	store = GTK_TREE_STORE(model);

	new_folder = input_dialog(_("New folder"),
				  _("Input the name of new folder:"),
				  _("NewFolder"));
	if (!new_folder) return;
	AUTORELEASE_STR(new_folder, {g_free(new_folder); return;});

	p = strchr(new_folder, G_DIR_SEPARATOR);
	if ((p && FOLDER_TYPE(selected_item->folder) != F_IMAP) ||
	    (p && FOLDER_TYPE(selected_item->folder) == F_IMAP &&
	     *(p + 1) != '\0')) {
		alertpanel_error(_("'%c' can't be included in folder name."),
				G_DIR_SEPARATOR);
		return;
	}

	disp_name = trim_string(new_folder, 32);
	AUTORELEASE_STR(disp_name, {g_free(disp_name); return;});

	/* find whether the directory already exists */
	if (folder_find_child_item_by_name(selected_item, new_folder)) {
		alertpanel_error(_("The folder '%s' already exists."),
				 disp_name);
		return;
	}

	new_item = folder_create_folder(selected_item, new_folder);
	if (!new_item) {
		alertpanel_error(_("Can't create the folder '%s'."), disp_name);
		return;
	}

	/* add new child */
	foldersel_append_item(store, new_item, &new_child, &selected);
        
	selected_p = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &selected);
	new_child_p = gtk_tree_model_get_path(GTK_TREE_MODEL(store),
					      &new_child);

	gtk_tree_view_expand_row(GTK_TREE_VIEW(treeview), selected_p, FALSE);
	gtk_tree_selection_select_iter(selection, &new_child);
	gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(treeview), new_child_p,
				     NULL, TRUE, 0.5, 0.0);
	gtk_tree_path_free(new_child_p);
	gtk_tree_path_free(selected_p);

	folder_write_list();
}

static void foldersel_entry_activated(GtkEntry *entry, gpointer data)
{
	gtk_button_clicked(GTK_BUTTON(ok_button));
}

static void foldersel_tree_activated(GtkTreeView *treeview, GtkTreePath *path,
				     GtkTreeViewColumn *column, gpointer data)
{
	gtk_button_clicked(GTK_BUTTON(ok_button));
}

static gint delete_event(GtkWidget *widget, GdkEventAny *event, gpointer data)
{
	foldersel_cancel(NULL, NULL);
	return TRUE;
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape)
		foldersel_cancel(NULL, NULL);
	return FALSE;
}

static gint foldersel_folder_name_compare(GtkTreeModel *model, GtkTreeIter *a,
					  GtkTreeIter *b, gpointer context)
{
	gchar *str_a = NULL, *str_b = NULL;
	gint val = 0;
	FolderItem *item_a = NULL, *item_b = NULL;
	GtkTreeIter parent;

	gtk_tree_model_get(model, a, FOLDERSEL_FOLDERITEM, &item_a, -1);
	gtk_tree_model_get(model, b, FOLDERSEL_FOLDERITEM, &item_b, -1);

	/* no sort for root folder */
	if (!gtk_tree_model_iter_parent(GTK_TREE_MODEL(model), &parent, a))
		return 0;

	/* if both a and b are special folders, sort them according to
	 * their types (which is in-order). Note that this assumes that
	 * there are no multiple folders of a special type. */
	if (item_a->stype != F_NORMAL && item_b->stype != F_NORMAL)
		return item_a->stype - item_b->stype;

	/* if b is normal folder, and a is not, b is smaller (ends up
	 * lower in the list) */
	if (item_a->stype != F_NORMAL && item_b->stype == F_NORMAL)
		return item_b->stype - item_a->stype;

	/* if b is special folder, and a is not, b is larger (ends up
	 * higher in the list) */
	if (item_a->stype == F_NORMAL && item_b->stype != F_NORMAL)
		return item_b->stype - item_a->stype;

	/* XXX g_utf8_collate_key() comparisons may speed things
	 * up when having large lists of folders */
	gtk_tree_model_get(model, a, FOLDERSEL_FOLDERNAME, &str_a, -1);
	gtk_tree_model_get(model, b, FOLDERSEL_FOLDERNAME, &str_b, -1);

	/* otherwise just compare the folder names */
	val = g_utf8_collate(str_a, str_b);

	g_free(str_a);
	g_free(str_b);

	return val;
}

static gboolean tree_view_folder_item_func(GtkTreeModel *model,
					   GtkTreePath *path,
					   GtkTreeIter *iter,
					   FolderItemSearch *data)
{
	FolderItem *item = NULL;

	gtk_tree_model_get(model, iter, FOLDERSEL_FOLDERITEM, &item, -1);

	if (data->item == item) {
		data->path = gtk_tree_path_copy(path);
		data->iter = *iter;
		return TRUE;
	}

	return FALSE;
}
