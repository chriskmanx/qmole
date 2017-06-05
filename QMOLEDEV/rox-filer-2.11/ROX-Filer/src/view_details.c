/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* view_details.c - display a list of files in a TreeView */

#include "config.h"

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "global.h"

#include "view_iface.h"
#include "view_details.h"
#include "dir.h"
#include "diritem.h"
#include "support.h"
#include "type.h"
#include "filer.h"
#include "display.h"
#include "pixmaps.h"
#include "dnd.h"
#include "bind.h"
#include "gui_support.h"
#include "menu.h"
#include "options.h"
#include "cell_icon.h"

/* These are the column numbers in the ListStore */
#define COL_LEAF 0
#define COL_TYPE 1
#define COL_PERM 2
#define COL_OWNER 3
#define COL_GROUP 4
#define COL_SIZE 5
#define COL_MTIME 6
#define COL_ITEM 7
#define COL_COLOUR 8
#define COL_WEIGHT 9
#define COL_VIEW_ITEM 10
#define N_COLUMNS 11

static gpointer parent_class = NULL;

struct _ViewDetailsClass {
	GtkTreeViewClass parent;
};

/* Static prototypes */
static void view_details_finialize(GObject *object);
static void view_details_class_init(gpointer gclass, gpointer data);
static void view_details_init(GTypeInstance *object, gpointer gclass);

static void view_details_iface_init(gpointer giface, gpointer iface_data);

static void view_details_sort(ViewIface *view);
static void view_details_style_changed(ViewIface *view, int flags);
static void view_details_add_items(ViewIface *view, GPtrArray *items);
static void view_details_update_items(ViewIface *view, GPtrArray *items);
static void view_details_delete_if(ViewIface *view,
			  gboolean (*test)(gpointer item, gpointer data),
			  gpointer data);
static void view_details_clear(ViewIface *view);
static void view_details_select_all(ViewIface *view);
static void view_details_clear_selection(ViewIface *view);
static int view_details_count_items(ViewIface *view);
static int view_details_count_selected(ViewIface *view);
static void view_details_show_cursor(ViewIface *view);
static void view_details_get_iter(ViewIface *view,
				     ViewIter *iter, IterFlags flags);
static void view_details_get_iter_at_point(ViewIface *view, ViewIter *iter,
					   GdkWindow *src, int x, int y);
static void view_details_cursor_to_iter(ViewIface *view, ViewIter *iter);
static void view_details_set_selected(ViewIface *view,
					 ViewIter *iter,
					 gboolean selected);
static gboolean view_details_get_selected(ViewIface *view, ViewIter *iter);
static void view_details_select_only(ViewIface *view, ViewIter *iter);
static void view_details_set_frozen(ViewIface *view, gboolean frozen);
static void view_details_wink_item(ViewIface *view, ViewIter *iter);
static void view_details_autosize(ViewIface *view);
static gboolean view_details_cursor_visible(ViewIface *view);
static void view_details_set_base(ViewIface *view, ViewIter *iter);
static void view_details_start_lasso_box(ViewIface *view,
				     	 GdkEventButton *event);
static void view_details_extend_tip(ViewIface *view,
				    ViewIter *iter, GString *tip);
static gboolean view_details_auto_scroll_callback(ViewIface *view);

static DirItem *iter_peek(ViewIter *iter);
static DirItem *iter_prev(ViewIter *iter);
static DirItem *iter_next(ViewIter *iter);
static void make_iter(ViewDetails *view_details, ViewIter *iter,
		      IterFlags flags);
static void make_item_iter(ViewDetails *view_details, ViewIter *iter, int i);
static void view_details_tree_model_init(GtkTreeModelIface *iface);
static gboolean details_get_sort_column_id(GtkTreeSortable *sortable,
					   gint            *sort_column_id,
					   GtkSortType     *order);
static void details_set_sort_column_id(GtkTreeSortable     *sortable,
				       gint                sort_column_id,
				       GtkSortType         order);
static void details_set_sort_func(GtkTreeSortable          *sortable,
			          gint                    sort_column_id,
			          GtkTreeIterCompareFunc  func,
			          gpointer                data,
			          GtkDestroyNotify        destroy);
static void details_set_default_sort_func(GtkTreeSortable        *sortable,
				          GtkTreeIterCompareFunc  func,
				          gpointer                data,
				          GtkDestroyNotify        destroy);
static gboolean details_has_default_sort_func(GtkTreeSortable *sortable);
static void view_details_sortable_init(GtkTreeSortableIface *iface);
static void set_selected(ViewDetails *view_details, int i, gboolean selected);
static gboolean get_selected(ViewDetails *view_details, int i);
static void free_view_item(ViewItem *view_item);
static void details_update_header_visibility(ViewDetails *view_details);
static void set_lasso(ViewDetails *view_details, int x, int y);
static void cancel_wink(ViewDetails *view_details);


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

GtkWidget *view_details_new(FilerWindow *filer_window)
{
	ViewDetails *view_details;

	view_details = g_object_new(view_details_get_type(), NULL);
	view_details->filer_window = filer_window;

	gtk_range_set_adjustment(GTK_RANGE(filer_window->scrollbar),
		gtk_tree_view_get_vadjustment(GTK_TREE_VIEW(view_details)));

	if (filer_window->sort_type != -1)
		view_details_sort((ViewIface *) view_details);

	details_update_header_visibility(view_details);

	return GTK_WIDGET(view_details);
}

GType view_details_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (ViewDetailsClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			view_details_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(ViewDetails),
			0,			/* n_preallocs */
			view_details_init
		};
		static const GInterfaceInfo view_iface_info = {
			view_details_iface_init,
			NULL, NULL
		};
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) view_details_tree_model_init,
			NULL, NULL
		};
		static const GInterfaceInfo sortable_info = {
			(GInterfaceInitFunc) view_details_sortable_init,
			NULL, NULL
		};


		type = g_type_register_static(gtk_tree_view_get_type(),
						"ViewDetails", &info, 0);

		g_type_add_interface_static(type, VIEW_TYPE_IFACE,
				&view_iface_info);
		g_type_add_interface_static(type, GTK_TYPE_TREE_MODEL,
				&tree_model_info);
		g_type_add_interface_static(type, GTK_TYPE_TREE_SORTABLE,
				&sortable_info);
	}

	return type;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* Update the visibility of the list headers */
static void details_update_header_visibility(ViewDetails *view_details)
{
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(view_details),
					  o_display_show_headers.int_value);
}

/* Fulfill the GtkTreeModel requirements */
static guint details_get_flags(GtkTreeModel *tree_model)
{
	return GTK_TREE_MODEL_LIST_ONLY;
}

static gint details_get_n_columns(GtkTreeModel *tree_model)
{
	return N_COLUMNS;
}

static GType details_get_column_type(GtkTreeModel *tree_model, gint index)
{
	g_return_val_if_fail(index < N_COLUMNS && index >= 0, G_TYPE_INVALID);

	if (index == COL_COLOUR)
		return GDK_TYPE_COLOR;
	else if (index == COL_ITEM || index == COL_VIEW_ITEM)
		return G_TYPE_POINTER;
	else if (index == COL_WEIGHT)
		return G_TYPE_INT;
	return G_TYPE_STRING;
}

static gboolean details_get_iter(GtkTreeModel *tree_model,
				 GtkTreeIter  *iter,
				 GtkTreePath  *path)
{
	ViewDetails *view_details = (ViewDetails *) tree_model;
	gint i;

	g_return_val_if_fail(gtk_tree_path_get_depth (path) > 0, FALSE);

	i = gtk_tree_path_get_indices(path)[0];

	if (i >= view_details->items->len)
		return FALSE;

	iter->user_data = GINT_TO_POINTER(i);

	return TRUE;
}

static GtkTreePath *details_get_path(GtkTreeModel *tree_model,
				     GtkTreeIter  *iter)
{
	GtkTreePath *retval;

	retval = gtk_tree_path_new();
	gtk_tree_path_append_index(retval, GPOINTER_TO_INT(iter->user_data));

	return retval;
}

static void details_get_value(GtkTreeModel *tree_model,
			      GtkTreeIter  *iter,
			      gint         column,
			      GValue       *value)
{
	ViewDetails *view_details = (ViewDetails *) tree_model;
	gint i;
	GPtrArray *items = view_details->items;
	ViewItem *view_item;
	DirItem *item;
	mode_t m;

	g_return_if_fail(column >= 0 && column < N_COLUMNS);

	i = GPOINTER_TO_INT(iter->user_data);
	g_return_if_fail(i >= 0 && i < items->len);
	view_item = (ViewItem *) items->pdata[i];
	item = view_item->item;

	if (column == COL_LEAF)
	{
		g_value_init(value, G_TYPE_STRING);
		g_value_set_string(value,
			view_item->utf8_name ? view_item->utf8_name
					     : item->leafname);
		return;
	}
	else if (column == COL_VIEW_ITEM)
	{
		g_value_init(value, G_TYPE_POINTER);
		g_value_set_pointer(value, view_item);
		return;
	}
	else if (column == COL_ITEM)
	{
		g_value_init(value, G_TYPE_POINTER);
		g_value_set_pointer(value, item);
		return;
	}

	if (item->base_type == TYPE_UNKNOWN)
	{
		GType type;
		type = details_get_column_type(tree_model, column);
		g_value_init(value, type);
		if (type == G_TYPE_STRING)
			g_value_set_string(value, "");
		else if (type == GDK_TYPE_COLOR)
			g_value_set_boxed(value, NULL);
		else if (type == G_TYPE_INT)
			g_value_set_int(value, PANGO_WEIGHT_NORMAL);
		else
			g_value_set_object(value, NULL);
			     
		return;
	}
	m = item->mode;

	switch (column)
	{
		case COL_LEAF:
			g_value_init(value, G_TYPE_STRING);
			g_value_set_string(value, item->leafname);
			break;
		case COL_COLOUR:
			g_value_init(value, GDK_TYPE_COLOR);
			if (view_item->utf8_name)
			{
				GdkColor red;
				red.red = 0xffff;
				red.green = 0;
				red.blue = 0;
				g_value_set_boxed(value, &red);
			}
			else
				g_value_set_boxed(value,
						  type_get_colour(item, NULL));
			break;
		case COL_OWNER:
			g_value_init(value, G_TYPE_STRING);
			g_value_set_string(value, user_name(item->uid));
			break;
		case COL_GROUP:
			g_value_init(value, G_TYPE_STRING);
			g_value_set_string(value, group_name(item->gid));
			break;
		case COL_MTIME:
		{
			gchar *time;
			time = pretty_time(&item->mtime);
			g_value_init(value, G_TYPE_STRING);
			g_value_set_string(value, time);
			g_free(time);
			break;
		}
		case COL_PERM:
			g_value_init(value, G_TYPE_STRING);
			g_value_set_string(value, pretty_permissions(m));
			break;
		case COL_SIZE:
			g_value_init(value, G_TYPE_STRING);
			if (item->base_type != TYPE_DIRECTORY)
				g_value_set_string(value,
						   format_size(item->size));
			break;
		case COL_TYPE:
			g_value_init(value, G_TYPE_STRING);
			if(o_display_show_full_type.int_value)
				g_value_set_string(value, 
						   item->flags & ITEM_FLAG_APPDIR? "Application" :
						   mime_type_comment(item->mime_type));
			else
				g_value_set_string(value, 
						   item->flags & ITEM_FLAG_APPDIR? "App" :
						   S_ISDIR(m) ? "Dir" :
						   S_ISCHR(m) ? "Char" :
						   S_ISBLK(m) ? "Blck" :
						   S_ISLNK(m) ? "Link" :
						   S_ISSOCK(m) ? "Sock" :
						   S_ISFIFO(m) ? "Pipe" :
						   S_ISDOOR(m) ? "Door" :
						   "File");
			
			break;
		case COL_WEIGHT:
			g_value_init(value, G_TYPE_INT);
			if (item->flags & ITEM_FLAG_RECENT)
				g_value_set_int(value, PANGO_WEIGHT_BOLD);
			else
				g_value_set_int(value, PANGO_WEIGHT_NORMAL);
			break;
		default:
			g_value_init(value, G_TYPE_STRING);
			g_value_set_string(value, "Hello");
			break;
	}
}

static gboolean details_iter_next(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) tree_model;
	int i;

	i = GPOINTER_TO_INT(iter->user_data) + 1;
	iter->user_data = GINT_TO_POINTER(i);

	return i < view_details->items->len;
}

static gboolean details_iter_children(GtkTreeModel *tree_model,
				      GtkTreeIter  *iter,
				      GtkTreeIter  *parent)
{
	ViewDetails *view_details = (ViewDetails *) tree_model;

	/* this is a list, nodes have no children */
	if (parent)
		return FALSE;

	/* but if parent == NULL we return the list itself as children of the
	 * "root"
	 */

	if (view_details->items->len)
	{
		iter->user_data = GINT_TO_POINTER(0);
		return TRUE;
	}
	else
		return FALSE;
}

static gboolean details_iter_has_child(GtkTreeModel *tree_model,
				       GtkTreeIter  *iter)
{
	return FALSE;
}

static gint details_iter_n_children(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) tree_model;

	if (iter == NULL)
		return view_details->items->len;

	return 0;
}

static gboolean details_iter_nth_child(GtkTreeModel *tree_model,
				       GtkTreeIter  *iter,
				       GtkTreeIter  *parent,
				       gint          n)
{
	ViewDetails *view_details = (ViewDetails *) tree_model;

	if (parent)
		return FALSE;

	if (n >= 0 && n < view_details->items->len)
	{
		iter->user_data = GINT_TO_POINTER(n);
		return TRUE;
	}
	else
		return FALSE;
}

static gboolean details_iter_parent(GtkTreeModel *tree_model,
				    GtkTreeIter  *iter,
				    GtkTreeIter  *child)
{
	return FALSE;
}

/* A ViewDetails is both a GtkTreeView and a GtkTreeModel.
 * The following functions implement the model interface...
 */

static void view_details_tree_model_init(GtkTreeModelIface *iface)
{
	iface->get_flags = details_get_flags;
	iface->get_n_columns = details_get_n_columns;
	iface->get_column_type = details_get_column_type;
	iface->get_iter = details_get_iter;
	iface->get_path = details_get_path;
	iface->get_value = details_get_value;
	iface->iter_next = details_iter_next;
	iface->iter_children = details_iter_children;
	iface->iter_has_child = details_iter_has_child;
	iface->iter_n_children = details_iter_n_children;
	iface->iter_nth_child = details_iter_nth_child;
	iface->iter_parent = details_iter_parent;
}

static void view_details_sortable_init(GtkTreeSortableIface *iface)
{
	iface->get_sort_column_id = details_get_sort_column_id;
	iface->set_sort_column_id = details_set_sort_column_id;
	iface->set_sort_func = details_set_sort_func;
	iface->set_default_sort_func = details_set_default_sort_func;
	iface->has_default_sort_func = details_has_default_sort_func;
}

static gboolean details_get_sort_column_id(GtkTreeSortable *sortable,
					   gint            *sort_column_id,
					   GtkSortType     *order)
{
	ViewDetails *view_details = (ViewDetails *) sortable;
	FilerWindow *filer_window = view_details->filer_window;
	int col;

	if (!filer_window)
		return FALSE;	/* Not yet initialised */

	switch (filer_window->sort_type)
	{
		case SORT_NAME: col = COL_LEAF; break;
		case SORT_TYPE: col = COL_TYPE; break;
		case SORT_DATE: col = COL_MTIME; break;
		case SORT_SIZE: col = COL_SIZE; break;
		case SORT_OWNER: col = COL_OWNER; break;
		case SORT_GROUP: col = COL_GROUP; break;
		default:
			g_warning("details_get_sort_column_id(): error!");
			return FALSE;
	}
	if (sort_column_id)
		*sort_column_id = col;
	if (order)
		*order = filer_window->sort_order;
	return TRUE;
}

static void details_set_sort_column_id(GtkTreeSortable     *sortable,
				       gint                sort_column_id,
				       GtkSortType         order)
{
	ViewDetails *view_details = (ViewDetails *) sortable;
	FilerWindow *filer_window = view_details->filer_window;

	if (!filer_window)
		return;		/* Not yet initialised */

	switch (sort_column_id)
	{
		case COL_LEAF:
			display_set_sort_type(filer_window, SORT_NAME, order);
			break;
		case COL_SIZE:
			display_set_sort_type(filer_window, SORT_SIZE, order);
			break;
		case COL_MTIME:
			display_set_sort_type(filer_window, SORT_DATE, order);
			break;
		case COL_TYPE:
			display_set_sort_type(filer_window, SORT_TYPE, order);
			break;
		case COL_OWNER:
			display_set_sort_type(filer_window, SORT_OWNER, order);
			break;
		case COL_GROUP:
			display_set_sort_type(filer_window, SORT_GROUP, order);
			break;
		default:
			g_assert_not_reached();
	}
}

static void details_set_sort_func(GtkTreeSortable          *sortable,
			          gint                    sort_column_id,
			          GtkTreeIterCompareFunc  func,
			          gpointer                data,
			          GtkDestroyNotify        destroy)
{
	g_assert_not_reached();
}

static void details_set_default_sort_func(GtkTreeSortable        *sortable,
				          GtkTreeIterCompareFunc  func,
				          gpointer                data,
				          GtkDestroyNotify        destroy)
{
	g_assert_not_reached();
}

static gboolean details_has_default_sort_func(GtkTreeSortable *sortable)
{
	return FALSE;
}


/* End of model implementation */

static gboolean is_selected(ViewDetails *view_details, int i)
{
	ViewIter iter;
	iter.i = i;
	return view_details_get_selected((ViewIface *) view_details, &iter);
}

static gboolean view_details_scroll(GtkWidget *widget, GdkEventScroll *event)
{
	GtkTreeView *tree = (GtkTreeView *) widget;
	GtkTreePath *path = NULL;

	if (!gtk_tree_view_get_path_at_pos(tree, 0, 1, &path, NULL, NULL, NULL))
		return TRUE;	/* Empty? */

	if (event->direction == GDK_SCROLL_UP)
		gtk_tree_path_prev(path);
	else if (event->direction == GDK_SCROLL_DOWN)
		gtk_tree_path_next(path);
	else
		goto out;

	gtk_tree_view_scroll_to_cell(tree, path, NULL, TRUE, 0, 0);
out:
	gtk_tree_path_free(path);
	return TRUE;
}

static gint view_details_key_press(GtkWidget *widget, GdkEventKey *event)
{
	if (event->keyval == GDK_Up || event->keyval == GDK_Down ||
	    event->keyval == GDK_Prior || event->keyval == GDK_Next ||
	    event->keyval == GDK_Home || event->keyval == GDK_End) {
		/* Work around a strange GTK bug that prevents you from moving the cursor
		 * if nothing is selected.
		 */
		if (event->state & GDK_CONTROL_MASK)
		{
			return GTK_WIDGET_CLASS(parent_class)->key_press_event(widget, event);
		}
		else
		{
			/* GTK hard-codes the test for CTRL, and won't move the cursor
			 * if it isn't set.
			 */
			event->state |= GDK_CONTROL_MASK;
			gtk_propagate_event(widget, (GdkEvent *) event);
			return TRUE;
		}
	}
	return FALSE;
}

static gboolean view_details_button_press(GtkWidget *widget,
					  GdkEventButton *bev)
{
	FilerWindow *filer_window = ((ViewDetails *) widget)->filer_window;
	GtkTreeView *tree = (GtkTreeView *) widget;

	if (bev->window != gtk_tree_view_get_bin_window(tree))
		return GTK_WIDGET_CLASS(parent_class)->button_press_event(
								widget, bev);

	if (dnd_motion_press(widget, bev))
		filer_perform_action(filer_window, bev);

	return TRUE;
}

static int get_lasso_index(ViewDetails *view_details, int y)
{
	GtkTreeViewColumn *column = NULL;
	GtkTreePath *path = NULL;
	GtkTreeView *tree = (GtkTreeView *) view_details;
	gint cell_y;
	int i;

	if (y < 0)
		y = 0;	/* gtk_tree_view_get_path_at_pos can't handle negatives */

	if (gtk_tree_view_get_path_at_pos(tree, 4, y, &path,
				&column, NULL, &cell_y))
	{
		GdkRectangle rect;
		i = gtk_tree_path_get_indices(path)[0];
		gtk_tree_view_get_cell_area(tree, path, column, &rect);
		gtk_tree_path_free(path);

		if (2 * cell_y > rect.height)
			i += 1;
	}
	else
		i = view_details->items->len;

	return i;
}

static gboolean select_lasso_cb(ViewIter *iter, gpointer data)
{
	int start = ((int *) data)[0];
	int end = ((int *) data)[1];
	GdkFunction fn = ((int *) data)[2];
	ViewDetails *view_details = (ViewDetails *) iter->view;
	GtkTreeIter  titer;

	titer.user_data = GINT_TO_POINTER(iter->i);

	if (iter->i < start || iter->i >= end)
		return gtk_tree_selection_iter_is_selected(
				view_details->selection, &titer);

	if (fn == GDK_SET)
		return TRUE;

	return !gtk_tree_selection_iter_is_selected(view_details->selection,
						    &titer);
}

static void select_lasso(ViewDetails *view_details, GdkFunction fn)
{
	GtkAdjustment	*adj;
	int range[3];

	adj = gtk_tree_view_get_vadjustment((GtkTreeView *) view_details);

	range[0] = view_details->lasso_start_index;
	range[1] = get_lasso_index(view_details,
			      view_details->drag_box_y[1] - adj->value);
	range[2] = fn;

	if (range[0] == range[1])
		return;
	if (range[0] > range[1])
	{
		int tmp = range[0];
		range[0] = range[1];
		range[1] = tmp;
	}

	view_select_if((ViewIface *) view_details, select_lasso_cb, &range);
}

static gboolean view_details_button_release(GtkWidget *widget,
					    GdkEventButton *bev)
{
	ViewDetails *view_details = (ViewDetails *) widget;
	FilerWindow *filer_window = view_details->filer_window;
	GtkTreeView *tree = (GtkTreeView *) widget;

	if (bev->window != gtk_tree_view_get_bin_window(tree))
		return GTK_WIDGET_CLASS(parent_class)->button_release_event(
								widget, bev);

	if (!dnd_motion_release(bev))
		filer_perform_action(filer_window, bev);

	if (motion_buttons_pressed == 0 && view_details->lasso_box)
	{
		select_lasso(view_details,
				bev->button == 1 ? GDK_SET : GDK_INVERT);
		filer_set_autoscroll(filer_window, FALSE);
		set_lasso(view_details, 
			  view_details->drag_box_x[0],
			  view_details->drag_box_y[0]);
		view_details->lasso_box = FALSE;
	}

	return TRUE;
}

static gint view_details_motion_notify(GtkWidget *widget, GdkEventMotion *event)
{
	ViewDetails *view_details = (ViewDetails *) widget;
	GtkTreeView *tree = (GtkTreeView *) widget;

	if (event->window != gtk_tree_view_get_bin_window(tree))
		return GTK_WIDGET_CLASS(parent_class)->motion_notify_event(
								widget, event);

	if (view_details->lasso_box)
	{
		GtkAdjustment	*adj;
		adj = gtk_tree_view_get_vadjustment(tree);
		set_lasso(view_details, event->x, event->y + adj->value);
		return TRUE;
	}

	return filer_motion_notify(view_details->filer_window, event);
}

static gboolean view_details_expose(GtkWidget *widget, GdkEventExpose *event)
{
	GtkTreeView *tree = (GtkTreeView *) widget;
	GtkTreePath *path = NULL;
	GdkRectangle focus_rectangle;
	ViewDetails *view_details = (ViewDetails *) widget;
	gboolean    had_cursor;

	had_cursor = (GTK_WIDGET_FLAGS(widget) & GTK_HAS_FOCUS) != 0;
	
	if (view_details->filer_window->selection_state == GTK_STATE_SELECTED)
		GTK_WIDGET_SET_FLAGS(widget, GTK_HAS_FOCUS);
	else
		GTK_WIDGET_UNSET_FLAGS(widget, GTK_HAS_FOCUS);
	GTK_WIDGET_CLASS(parent_class)->expose_event(widget, event);
	if (had_cursor)
		GTK_WIDGET_SET_FLAGS(widget, GTK_HAS_FOCUS);

	if (event->window != gtk_tree_view_get_bin_window(tree))
		return FALSE;	/* Not the main area */

	if (view_details->lasso_box)
	{
		int x, y, width, height;
		GtkAdjustment *adj;

		adj = gtk_tree_view_get_vadjustment(tree);
		x = MIN(view_details->drag_box_x[0],
				view_details->drag_box_x[1]);
		y = MIN(view_details->drag_box_y[0],
				view_details->drag_box_y[1]);
		width = abs(view_details->drag_box_x[1] -
				view_details->drag_box_x[0]);
		height = abs(view_details->drag_box_y[1] -
				view_details->drag_box_y[0]);
		y -= adj->value;

		if (width && height)
			gdk_draw_rectangle(event->window,
				widget->style->fg_gc[GTK_STATE_NORMAL],
				FALSE, x, y, width - 1, height - 1);
	}

	if (view_details->wink_item != -1 && view_details->wink_step & 1)
	{
		GtkTreePath *wink_path;
		GdkRectangle wink_area;

		wink_path = gtk_tree_path_new();
		gtk_tree_path_append_index(wink_path, view_details->wink_item);
		gtk_tree_view_get_background_area(tree, wink_path,
					NULL, &wink_area);
		gtk_tree_path_free(wink_path);

		if (wink_area.height)
		{
			/* (visible) */
			wink_area.width = widget->allocation.width;
			gdk_draw_rectangle(event->window,
				widget->style->fg_gc[GTK_STATE_NORMAL],
					FALSE,
					wink_area.x + 1,
					wink_area.y + 1,
					wink_area.width - 3,
					wink_area.height - 3);
		}
	}

	gtk_tree_view_get_cursor(tree, &path, NULL);
	if (!path)
		return FALSE;	/* No cursor */
	gtk_tree_view_get_background_area(tree, path, NULL, &focus_rectangle);
	gtk_tree_path_free(path);

	if (!focus_rectangle.height)
		return FALSE;	/* Off screen */

	focus_rectangle.width = widget->allocation.width;

	gtk_paint_focus(widget->style,
			event->window,
			GTK_STATE_NORMAL,
			NULL,
			widget,
			"treeview",
			focus_rectangle.x,
			focus_rectangle.y,
			focus_rectangle.width,
			focus_rectangle.height);

	return FALSE;
}

static void view_details_size_request(GtkWidget *widget,
				      GtkRequisition *requisition)
{
	ViewDetails *view_details = (ViewDetails *) widget;

	(*GTK_WIDGET_CLASS(parent_class)->size_request)(widget, requisition);

	view_details->desired_size = *requisition;
	
	requisition->height = 50;
	requisition->width = 50;
}

static void view_details_drag_data_received(GtkWidget *widget,
		GdkDragContext *drag_context,
		gint x, gint y, GtkSelectionData *data, guint info, guint time)
{
	/* Just here to override annoying default handler */
}

static void view_details_destroy(GtkObject *obj)
{
	ViewDetails *view_details = VIEW_DETAILS(obj);

	view_details->filer_window = NULL;
	cancel_wink(view_details);
}

static void view_details_finialize(GObject *object)
{
	ViewDetails *view_details = (ViewDetails *) object;

	g_ptr_array_free(view_details->items, TRUE);
	view_details->items = NULL;

	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void view_details_class_init(gpointer gclass, gpointer data)
{
	GObjectClass *object = (GObjectClass *) gclass;
	GtkWidgetClass *widget = (GtkWidgetClass *) gclass;

	parent_class = g_type_class_peek_parent(gclass);

	object->finalize = view_details_finialize;
	GTK_OBJECT_CLASS(object)->destroy = view_details_destroy;

	widget->scroll_event = view_details_scroll;
	widget->key_press_event = view_details_key_press;
	widget->button_press_event = view_details_button_press;
	widget->button_release_event = view_details_button_release;
	widget->motion_notify_event = view_details_motion_notify;
	widget->expose_event = view_details_expose;
	widget->size_request = view_details_size_request;
	widget->drag_data_received = view_details_drag_data_received;

	/*
	 * Add the ViewDetails::mono-font style property.
	 * To use this add something like
	 *
	 * style "details" {
	 *   ViewDetails::mono-font = "Courier 8"
	 * }
	 * class "ViewDetails" style "details"
	 *
	 * to your ~/.gtkrc-2.0
	 */
	gtk_widget_class_install_style_property(widget,
		       g_param_spec_string("mono-font",
					   _("Mono font"),
					   _("Font for displaying mono-spaced text"),
					   "monospace",
					   G_PARAM_READABLE));
								    
}

static gboolean block_focus(GtkWidget *button, GtkDirectionType dir,
			    ViewDetails *view_details)
{
	GTK_WIDGET_UNSET_FLAGS(button, GTK_CAN_FOCUS);
	return FALSE;
}

static gboolean test_can_change_selection(GtkTreeSelection *sel,
                                          GtkTreeModel *model,
                                          GtkTreePath *path,
                                          gboolean path_currently_selected,
                                          gpointer data)
{
	ViewDetails *view_details;

	view_details = VIEW_DETAILS(gtk_tree_selection_get_tree_view(sel));
	
	return view_details->can_change_selection != 0;
}

static void selection_changed(GtkTreeSelection *selection,
			      gpointer user_data)
{
	ViewDetails *view_details = VIEW_DETAILS(user_data);

	filer_selection_changed(view_details->filer_window,
			gtk_get_current_event_time());
}

/*
 * Set the font used for a treeview column to that given for the
 * "mono-font" style property of a widget.  This has to be done _after_ the
 * widget has been realized, because that is when the style information is
 * set up.  This is done by connecting this function to run after the
 * "realize" signal.
 */
static void set_column_mono_font(GtkWidget *widget, GObject *object)
{
	const gchar *font_name;

	gtk_widget_style_get(widget, "mono-font", &font_name, NULL);
	g_object_set(object, "font", font_name, NULL);
}

#define ADD_TEXT_COLUMN(name, model_column) \
	cell = gtk_cell_renderer_text_new();	\
	column = gtk_tree_view_column_new_with_attributes(name, cell, \
					    "text", model_column,	\
					    "foreground-gdk", COL_COLOUR, \
					    "weight", COL_WEIGHT, 	\
					    NULL);			\
	gtk_tree_view_append_column(treeview, column);			\
	g_signal_connect(column->button, "grab-focus",			\
			G_CALLBACK(block_focus), view_details);

static void view_details_init(GTypeInstance *object, gpointer gclass)
{
	GtkTreeView *treeview = (GtkTreeView *) object;
	GtkTreeViewColumn *column;
	GtkCellRenderer *cell;
	GtkTreeSortable *sortable_list;
	ViewDetails *view_details = (ViewDetails *) object;

	view_details->items = g_ptr_array_new();
	view_details->cursor_base = -1;
	view_details->wink_item = -1;
	view_details->desired_size.width = -1;
	view_details->desired_size.height = -1;
	view_details->can_change_selection = 0;
	view_details->lasso_box = FALSE;

	view_details->selection = gtk_tree_view_get_selection(treeview);
	gtk_tree_selection_set_mode(view_details->selection,
				GTK_SELECTION_MULTIPLE);
	gtk_tree_selection_set_select_function(view_details->selection,
			test_can_change_selection, view_details, NULL);

	/* Sorting */
	view_details->sort_fn = NULL;
	sortable_list = GTK_TREE_SORTABLE(object);

	gtk_tree_view_set_model(treeview, GTK_TREE_MODEL(view_details));
	/* Do this after set_model, because that can generate this
	 * signal...
	 */
	g_signal_connect(view_details->selection, "changed",
			G_CALLBACK(selection_changed), view_details);

	/* Icon */
	cell = cell_icon_new(view_details);
	column = gtk_tree_view_column_new_with_attributes(NULL, cell,
					    "item", COL_VIEW_ITEM,
					    NULL);
	gtk_tree_view_append_column(treeview, column);

	ADD_TEXT_COLUMN(_("_Name"), COL_LEAF);
	gtk_tree_view_column_set_sort_column_id(column, COL_LEAF);
	gtk_tree_view_column_set_resizable(column, TRUE);
	ADD_TEXT_COLUMN(_("_Type"), COL_TYPE);
	gtk_tree_view_column_set_sort_column_id(column, COL_TYPE);
	gtk_tree_view_column_set_resizable(column, TRUE);
	ADD_TEXT_COLUMN(_("_Permissions"), COL_PERM);
	g_object_set(G_OBJECT(cell), "font", "monospace", NULL);
	g_signal_connect_after(object, "realize",
			       G_CALLBACK(set_column_mono_font),
			       G_OBJECT(cell));
	ADD_TEXT_COLUMN(_("_Owner"), COL_OWNER);
	gtk_tree_view_column_set_sort_column_id(column, COL_OWNER);
	ADD_TEXT_COLUMN(_("_Group"), COL_GROUP);
	gtk_tree_view_column_set_sort_column_id(column, COL_GROUP);
	ADD_TEXT_COLUMN(_("_Size"), COL_SIZE);
	g_object_set(G_OBJECT(cell), "xalign", 1.0, "font", "monospace", NULL);
	g_signal_connect_after(object, "realize",
			       G_CALLBACK(set_column_mono_font),
			       G_OBJECT(cell));
	gtk_tree_view_column_set_sort_column_id(column, COL_SIZE);
	ADD_TEXT_COLUMN(_("Last _Modified"), COL_MTIME);
	gtk_tree_view_column_set_sort_column_id(column, COL_MTIME);
}

/* Create the handers for the View interface */
static void view_details_iface_init(gpointer giface, gpointer iface_data)
{
	ViewIfaceClass *iface = giface;

	g_assert(G_TYPE_FROM_INTERFACE(iface) == VIEW_TYPE_IFACE);

	/* override stuff */
	iface->sort = view_details_sort;
	iface->style_changed = view_details_style_changed;
	iface->add_items = view_details_add_items;
	iface->update_items = view_details_update_items;
	iface->delete_if = view_details_delete_if;
	iface->clear = view_details_clear;
	iface->select_all = view_details_select_all;
	iface->clear_selection = view_details_clear_selection;
	iface->count_items = view_details_count_items;
	iface->count_selected = view_details_count_selected;
	iface->show_cursor = view_details_show_cursor;
	iface->get_iter = view_details_get_iter;
	iface->get_iter_at_point = view_details_get_iter_at_point;
	iface->cursor_to_iter = view_details_cursor_to_iter;
	iface->set_selected = view_details_set_selected;
	iface->get_selected = view_details_get_selected;
	iface->set_frozen = view_details_set_frozen;
	iface->select_only = view_details_select_only;
	iface->wink_item = view_details_wink_item;
	iface->autosize = view_details_autosize;
	iface->cursor_visible = view_details_cursor_visible;
	iface->set_base = view_details_set_base;
	iface->start_lasso_box = view_details_start_lasso_box;
	iface->extend_tip = view_details_extend_tip;
	iface->auto_scroll_callback = view_details_auto_scroll_callback;
}

/* Implementations of the View interface. See view_iface.c for comments. */

static void view_details_style_changed(ViewIface *view, int flags)
{
	ViewDetails *view_details = (ViewDetails *) view;
	GtkTreeModel *model = (GtkTreeModel *) view;
	GtkTreePath *path;
	ViewItem    **items = (ViewItem **) view_details->items->pdata;
	int i;
	int n = view_details->items->len;

	path = gtk_tree_path_new();
	gtk_tree_path_append_index(path, 0);

	for (i = 0; i < n; i++)
	{
		GtkTreeIter iter;
		ViewItem    *item = items[i];

		iter.user_data = GINT_TO_POINTER(i);
		if (item->image)
		{
			g_object_unref(G_OBJECT(item->image));
			item->image = NULL;
		}
		gtk_tree_model_row_changed(model, path, &iter);
		gtk_tree_path_next(path);
	}

	gtk_tree_path_free(path);

	gtk_tree_view_columns_autosize((GtkTreeView *) view);

	if (flags & VIEW_UPDATE_HEADERS)
		details_update_header_visibility(view_details);
}

static gint wrap_sort(gconstpointer a, gconstpointer b,
		      ViewDetails *view_details)
{
	ViewItem *ia = *(ViewItem **) a;
	ViewItem *ib = *(ViewItem **) b;

	if (view_details->filer_window->sort_order == GTK_SORT_ASCENDING)
		return view_details->sort_fn(ia->item, ib->item);
	else
		return -view_details->sort_fn(ia->item, ib->item);
}

static void resort(ViewDetails *view_details)
{
	ViewItem **items = (ViewItem **) view_details->items->pdata;
	gint i, len = view_details->items->len;
	guint *new_order;
	GtkTreePath *path;
	int wink_item = view_details->wink_item;

	if (!len)
		return;

	for (i = len - 1; i >= 0; i--)
		items[i]->old_pos = i;

	switch (view_details->filer_window->sort_type)
	{
		case SORT_NAME: view_details->sort_fn = sort_by_name; break;
		case SORT_TYPE: view_details->sort_fn = sort_by_type; break;
		case SORT_DATE: view_details->sort_fn = sort_by_date; break;
		case SORT_SIZE: view_details->sort_fn = sort_by_size; break;
		case SORT_OWNER: view_details->sort_fn = sort_by_owner; break;
		case SORT_GROUP: view_details->sort_fn = sort_by_group; break;
		default:
			g_assert_not_reached();
	}
	
	g_ptr_array_sort_with_data(view_details->items,
				   (GCompareDataFunc) wrap_sort,
				   view_details);

	new_order = g_new(guint, len);
	for (i = len - 1; i >= 0; i--)
	{
		new_order[i] = items[i]->old_pos;
		if (wink_item == items[i]->old_pos)
			wink_item = i;
	}

	view_details->wink_item = wink_item;

	path = gtk_tree_path_new();
	gtk_tree_model_rows_reordered((GtkTreeModel *) view_details,
					path, NULL, new_order);
	gtk_tree_path_free(path);
	g_free(new_order);
}

static void view_details_sort(ViewIface *view)
{
	resort((ViewDetails *) view);
	gtk_tree_sortable_sort_column_changed((GtkTreeSortable *) view);
}

static void view_details_add_items(ViewIface *view, GPtrArray *new_items)
{
	ViewDetails *view_details = (ViewDetails *) view;
	FilerWindow *filer_window = view_details->filer_window;
	GPtrArray *items = view_details->items;
	GtkTreeIter iter;
	int i;
	GtkTreePath *path;
	GtkTreeModel *model = (GtkTreeModel *) view;

	iter.user_data = GINT_TO_POINTER(items->len);
	path = details_get_path(model, &iter);

	for (i = 0; i < new_items->len; i++)
	{
		DirItem *item = (DirItem *) new_items->pdata[i];
		char	*leafname = item->leafname;
		ViewItem *vitem;

		if(!filer_match_filter(filer_window, item))
			continue;
		if (leafname[0] == '.')
		{
			if (leafname[1] == '\0')
				continue; /* Never show '.' */

			if (leafname[1] == '.' &&
					leafname[2] == '\0')
				continue; /* Never show '..' */
		}

		vitem = g_new(ViewItem, 1);
		vitem->item = item;
		vitem->image = NULL;
		if (!g_utf8_validate(leafname, -1, NULL))
			vitem->utf8_name = to_utf8(leafname);
		else
			vitem->utf8_name = NULL;
		
		g_ptr_array_add(items, vitem);

		iter.user_data = GINT_TO_POINTER(items->len - 1);
		gtk_tree_model_row_inserted(model, path, &iter);
		gtk_tree_path_next(path);
	}

	gtk_tree_path_free(path);

	resort(view_details);
}

/* Find an item in the sorted array.
 * Returns the item number, or -1 if not found.
 */
static int details_find_item(ViewDetails *view_details, DirItem *item)
{
	ViewItem **items, tmp, *tmpp;
	int	lower, upper;

	g_return_val_if_fail(view_details != NULL, -1);
	g_return_val_if_fail(item != NULL, -1);

	tmp.item = item;
	tmpp = &tmp;

	items = (ViewItem **) view_details->items->pdata;

	/* If item is here, then: lower <= i < upper */
	lower = 0;
	upper = view_details->items->len;

	while (lower < upper)
	{
		int	i, cmp;

		i = (lower + upper) >> 1;

		cmp = wrap_sort(&items[i], &tmpp, view_details);
		if (cmp == 0)
			return i;

		if (cmp > 0)
			upper = i;
		else
			lower = i + 1;
	}

	return -1;
}

static void view_details_update_items(ViewIface *view, GPtrArray *items)
{
	ViewDetails	*view_details = (ViewDetails *) view;
	FilerWindow	*filer_window = view_details->filer_window;
	int		i;
	GtkTreeModel	*model = (GtkTreeModel *) view_details;

	g_return_if_fail(items->len > 0);
	
	/* The item data has already been modified, so this gives the
	 * final sort order...
	 */
	resort(view_details);

	for (i = 0; i < items->len; i++)
	{
		DirItem *item = (DirItem *) items->pdata[i];
		const gchar *leafname = item->leafname;
		int j;

		if (!filer_match_filter(filer_window, item))
			continue;

		j = details_find_item(view_details, item);

		if (j < 0)
			g_warning("Failed to find '%s'\n", leafname);
		else
		{
			GtkTreePath *path;
			GtkTreeIter iter;
			ViewItem *view_item = view_details->items->pdata[j];
			if (view_item->image)
			{
				g_object_unref(G_OBJECT(view_item->image));
				view_item->image = NULL;
			}
			path = gtk_tree_path_new();
			gtk_tree_path_append_index(path, j);
			iter.user_data = GINT_TO_POINTER(j);
			gtk_tree_model_row_changed(model, path, &iter);
		}
	}
}

static void view_details_delete_if(ViewIface *view,
			  gboolean (*test)(gpointer item, gpointer data),
			  gpointer data)
{
	GtkTreePath *path;
	ViewDetails *view_details = (ViewDetails *) view;
	int	    i = 0;
	GPtrArray   *items = view_details->items;
	GtkTreeModel *model = (GtkTreeModel *) view;

	path = gtk_tree_path_new();

	gtk_tree_path_append_index(path, i);

	while (i < items->len)
	{
		ViewItem *item = items->pdata[i];

		if (test(item->item, data))
		{
			free_view_item(items->pdata[i]);
			g_ptr_array_remove_index(items, i);
			gtk_tree_model_row_deleted(model, path);
		}
		else
		{
			i++;
			gtk_tree_path_next(path);
		}
	}

	gtk_tree_path_free(path);
}

static void view_details_clear(ViewIface *view)
{
	GtkTreePath *path;
	GPtrArray *items = ((ViewDetails *) view)->items;
	GtkTreeModel *model = (GtkTreeModel *) view;

	path = gtk_tree_path_new();
	gtk_tree_path_append_index(path, items->len);

	while (gtk_tree_path_prev(path))
		gtk_tree_model_row_deleted(model, path);

	g_ptr_array_set_size(items, 0);
	gtk_tree_path_free(path);
}

static void view_details_select_all(ViewIface *view)
{
	ViewDetails *view_details = (ViewDetails *) view;

	view_details->can_change_selection++;
	gtk_tree_selection_select_all(view_details->selection);
	view_details->can_change_selection--;
}

static void view_details_clear_selection(ViewIface *view)
{
	ViewDetails *view_details = (ViewDetails *) view;

	view_details->can_change_selection++;
	gtk_tree_selection_unselect_all(view_details->selection);
	view_details->can_change_selection--;
}

static int view_details_count_items(ViewIface *view)
{
	ViewDetails *view_details = (ViewDetails *) view;

	return view_details->items->len;
}

#if GTK_MINOR_VERSION < 2
static void view_details_count_inc(GtkTreeModel *model, GtkTreePath *path,
				   GtkTreeIter *iter, gpointer data)
{
	int *count = (int *) data;
	(*count) += 1;
}
#endif

static int view_details_count_selected(ViewIface *view)
{
	ViewDetails *view_details = (ViewDetails *) view;

#if GTK_MINOR_VERSION >= 2
	return gtk_tree_selection_count_selected_rows(view_details->selection);
#else
	int count = 0;
	
	gtk_tree_selection_selected_foreach(view_details->selection,
					    view_details_count_inc, &count);
	return count;
#endif
}

static void view_details_show_cursor(ViewIface *view)
{
}

static void view_details_get_iter(ViewIface *view,
				  ViewIter *iter, IterFlags flags)
{
	make_iter((ViewDetails *) view, iter, flags);
}

static void view_details_get_iter_at_point(ViewIface *view, ViewIter *iter,
					   GdkWindow *src, int x, int y)
{
	ViewDetails *view_details = (ViewDetails *) view;
	GtkTreeModel *model;
	GtkTreeView *tree = (GtkTreeView *) view;
	GtkTreePath *path = NULL;
	int i = -1;
	gint cell_y;

	model = gtk_tree_view_get_model(tree);

	if (gtk_tree_view_get_path_at_pos(tree, x, y + 4, &path, NULL,
					  NULL, &cell_y))
	{
		g_return_if_fail(path != NULL);

		if (cell_y > 8)
			i = gtk_tree_path_get_indices(path)[0];
		gtk_tree_path_free(path);
	}

	make_item_iter(view_details, iter, i);
}

static void view_details_cursor_to_iter(ViewIface *view, ViewIter *iter)
{
	GtkTreePath *path;
	ViewDetails *view_details = (ViewDetails *) view;

	path = gtk_tree_path_new();

	if (iter)
		gtk_tree_path_append_index(path, iter->i);
	else
	{
		/* Using depth zero or index -1 gives an error, but this
		 * is OK!
		 */
		gtk_tree_path_append_index(path, view_details->items->len);
	}

	gtk_tree_view_set_cursor((GtkTreeView *) view, path, NULL, FALSE);
	gtk_tree_path_free(path);
}

static void set_selected(ViewDetails *view_details, int i, gboolean selected)
{
	GtkTreeIter iter;

	iter.user_data = GINT_TO_POINTER(i);
	view_details->can_change_selection++;
	if (selected)
		gtk_tree_selection_select_iter(view_details->selection, &iter);
	else
		gtk_tree_selection_unselect_iter(view_details->selection,
						&iter);
	view_details->can_change_selection--;
}

static void view_details_set_selected(ViewIface *view,
					 ViewIter *iter,
					 gboolean selected)
{
	set_selected((ViewDetails *) view, iter->i, selected);
}

static gboolean get_selected(ViewDetails *view_details, int i)
{
	GtkTreeIter iter;

	iter.user_data = GINT_TO_POINTER(i);

	return gtk_tree_selection_iter_is_selected(view_details->selection,
					&iter);
}

static gboolean view_details_get_selected(ViewIface *view, ViewIter *iter)
{
	return get_selected((ViewDetails *) view, iter->i);
}

static void view_details_select_only(ViewIface *view, ViewIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) view;
	GtkTreePath *path;

	path = gtk_tree_path_new();
	gtk_tree_path_append_index(path, iter->i);
	view_details->can_change_selection++;
	gtk_tree_selection_unselect_all(view_details->selection);
	gtk_tree_selection_select_range(view_details->selection, path, path);
	view_details->can_change_selection--;
	gtk_tree_path_free(path);
}

static void view_details_set_frozen(ViewIface *view, gboolean frozen)
{
}

static void redraw_wink_area(ViewDetails *view_details)
{
	GtkTreePath *wink_path;
	GdkRectangle wink_area;
	GtkTreeView *tree = (GtkTreeView *) view_details;

	g_return_if_fail(view_details->wink_item >= 0);

	wink_path = gtk_tree_path_new();
	gtk_tree_path_append_index(wink_path, view_details->wink_item);
	gtk_tree_view_get_background_area(tree, wink_path, NULL, &wink_area);
	gtk_tree_path_free(wink_path);

	if (wink_area.height)
	{
		GdkWindow *window;
		window = gtk_tree_view_get_bin_window(tree);

		wink_area.width = GTK_WIDGET(tree)->allocation.width;
		gdk_window_invalidate_rect(window, &wink_area, FALSE);
	}
}

static void cancel_wink(ViewDetails *view_details)
{
	if (view_details->wink_item == -1)
		return;

	if (view_details->filer_window)
		redraw_wink_area(view_details);

	view_details->wink_item = -1;
	g_source_remove(view_details->wink_timeout);
}

static gboolean wink_timeout(ViewDetails *view_details)
{
	view_details->wink_step--;
	if (view_details->wink_step < 1)
	{
		cancel_wink(view_details);
		return FALSE;
	}

	redraw_wink_area(view_details);

	return TRUE;
}

static void view_details_wink_item(ViewIface *view, ViewIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) view;
	GtkTreePath *path;

	cancel_wink(view_details);
	if (!iter)
		return;

	view_details->wink_item = iter->i;
	view_details->wink_timeout = g_timeout_add(70,
			(GSourceFunc) wink_timeout, view_details);
	view_details->wink_step = 7;
	redraw_wink_area(view_details);

	path = gtk_tree_path_new();
	gtk_tree_path_append_index(path, iter->i);
	gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(view),
			path, NULL, FALSE, 0, 0);
	gtk_tree_path_free(path);
}

static void view_details_autosize(ViewIface *view)
{
	ViewDetails *view_details = (ViewDetails *) view;
	FilerWindow *filer_window = view_details->filer_window;
	int max_width = (o_filer_size_limit.int_value * monitor_width) / 100;
	int max_height = (o_filer_size_limit.int_value * monitor_height) / 100;
	int h;
	GtkRequisition req;

	gtk_widget_queue_resize(GTK_WIDGET(view));
	gtk_widget_size_request(GTK_WIDGET(view), &req);

	h = MAX(view_details->desired_size.height, SMALL_HEIGHT);

	filer_window_set_size(filer_window,
			MIN(view_details->desired_size.width, max_width),
			MIN(h, max_height));
}

static gboolean view_details_cursor_visible(ViewIface *view)
{
	GtkTreePath *path = NULL;

	gtk_tree_view_get_cursor((GtkTreeView *) view, &path, NULL);

	if (path)
		gtk_tree_path_free(path);

	return path != NULL;
}

static void view_details_set_base(ViewIface *view, ViewIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) view;

	view_details->cursor_base = iter->i;
}

/* Change the dynamic corner of the lasso box and trigger a redraw */
static void set_lasso(ViewDetails *view_details, int x, int y)
{
	GdkWindow   *window;
	GdkRectangle area;
	int minx, miny, maxx, maxy;

	if (x == view_details->drag_box_x[1] &&
	    y == view_details->drag_box_y[1])
		return;

	/* Get old region */
	minx = MIN(view_details->drag_box_x[0], view_details->drag_box_x[1]);
	miny = MIN(view_details->drag_box_y[0], view_details->drag_box_y[1]);
	maxx = MAX(view_details->drag_box_x[0], view_details->drag_box_x[1]);
	maxy = MAX(view_details->drag_box_y[0], view_details->drag_box_y[1]);

	/* Enlarge to cover new point */
	minx = MIN(minx, x);
	miny = MIN(miny, y);
	maxx = MAX(maxx, x);
	maxy = MAX(maxy, y);

	area.x = minx;
	area.y = miny;
	area.width = maxx - minx;
	area.height = maxy - miny;

	view_details->drag_box_x[1] = x;
	view_details->drag_box_y[1] = y;

	window = gtk_tree_view_get_bin_window((GtkTreeView *) view_details);
	if (area.width && area.height)
	{
		GtkAdjustment *adj;

		adj = gtk_tree_view_get_vadjustment((GtkTreeView *)
							view_details);
		area.y -= adj->value;
		gdk_window_invalidate_rect(window, &area, FALSE);
	}
}

static void view_details_start_lasso_box(ViewIface *view, GdkEventButton *event)
{
	ViewDetails *view_details = (ViewDetails *) view;
	GtkAdjustment *adj;

	adj = gtk_tree_view_get_vadjustment((GtkTreeView *) view_details);

	view_details->lasso_start_index = get_lasso_index(view_details,
							  event->y);

	filer_set_autoscroll(view_details->filer_window, TRUE);

	view_details->drag_box_x[0] = view_details->drag_box_x[1] = event->x;
	view_details->drag_box_y[0] = view_details->drag_box_y[1] = event->y +
								adj->value;
	view_details->lasso_box = TRUE;
}

static void view_details_extend_tip(ViewIface *view,
				    ViewIter *iter, GString *tip)
{
}

static DirItem *iter_init(ViewIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) iter->view;
	int i = -1;
	int n = view_details->items->len;
	int flags = iter->flags;

	iter->peek = iter_peek;

	if (iter->n_remaining == 0)
		return NULL;

	if (flags & VIEW_ITER_FROM_CURSOR)
	{
		GtkTreePath *path;
		gtk_tree_view_get_cursor((GtkTreeView *) view_details,
					 &path, NULL);
		if (!path)
			return NULL;	/* No cursor */
		i = gtk_tree_path_get_indices(path)[0];
		gtk_tree_path_free(path);
	}
	else if (flags & VIEW_ITER_FROM_BASE)
		i = view_details->cursor_base;
	
	if (i < 0 || i >= n)
	{
		/* Either a normal iteration, or an iteration from an
		 * invalid starting point.
		 */
		if (flags & VIEW_ITER_BACKWARDS)
			i = n - 1;
		else
			i = 0;
	}

	if (i < 0 || i >= n)
		return NULL;	/* No items at all! */

	iter->next = flags & VIEW_ITER_BACKWARDS ? iter_prev : iter_next;
	iter->n_remaining--;
	iter->i = i;

	if (flags & VIEW_ITER_SELECTED && !is_selected(view_details, i))
		return iter->next(iter);
	return iter->peek(iter);
}

static DirItem *iter_prev(ViewIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) iter->view;
	int n = view_details->items->len;
	int i = iter->i;

	g_return_val_if_fail(iter->n_remaining >= 0, NULL);

	/* i is the last item returned (always valid) */

	g_return_val_if_fail(i >= 0 && i < n, NULL);

	while (iter->n_remaining)
	{
		i--;
		iter->n_remaining--;

		if (i == -1)
			i = n - 1;

		g_return_val_if_fail(i >= 0 && i < n, NULL);

		if (iter->flags & VIEW_ITER_SELECTED &&
		    !is_selected(view_details, i))
			continue;

		iter->i = i;
		return ((ViewItem *) view_details->items->pdata[i])->item;
	}
	
	iter->i = -1;
	return NULL;
}

static DirItem *iter_next(ViewIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) iter->view;
	int n = view_details->items->len;
	int i = iter->i;

	g_return_val_if_fail(iter->n_remaining >= 0, NULL);

	/* i is the last item returned (always valid) */

	g_return_val_if_fail(i >= 0 && i < n, NULL);
	
	while (iter->n_remaining)
	{
		i++;
		iter->n_remaining--;

		if (i == n)
			i = 0;

		g_return_val_if_fail(i >= 0 && i < n, NULL);

		if (iter->flags & VIEW_ITER_SELECTED &&
		    !is_selected(view_details, i))
			continue;

		iter->i = i;
		return ((ViewItem *) view_details->items->pdata[i])->item;
	}
	
	iter->i = -1;
	return NULL;
}

static DirItem *iter_peek(ViewIter *iter)
{
	ViewDetails *view_details = (ViewDetails *) iter->view;
	int n = view_details->items->len;
	int i = iter->i;

	if (i == -1)
		return NULL;
	
	g_return_val_if_fail(i >= 0 && i < n, NULL);

	return ((ViewItem *) view_details->items->pdata[i])->item;
}

/* Set the iterator to return 'i' on the next peek().
 * If i is -1, returns NULL on next peek().
 */
static void make_item_iter(ViewDetails *view_details, ViewIter *iter, int i)
{
	make_iter(view_details, iter, 0);

	g_return_if_fail(i >= -1 && i < (int) view_details->items->len);

	iter->i = i;
	iter->next = iter_next;
	iter->peek = iter_peek;
	iter->n_remaining = 0;
}

static void make_iter(ViewDetails *view_details, ViewIter *iter,
		      IterFlags flags)
{
	iter->view = (ViewIface *) view_details;
	iter->next = iter_init;
	iter->peek = NULL;
	iter->i = -1;

	iter->flags = flags;

	if (flags & VIEW_ITER_ONE_ONLY)
	{
		iter->n_remaining = 1;
		iter->next(iter);
	}
	else
		iter->n_remaining = view_details->items->len;
}

static void free_view_item(ViewItem *view_item)
{
	if (view_item->image)
		g_object_unref(G_OBJECT(view_item->image));
	g_free(view_item->utf8_name);
	g_free(view_item);
}

static gboolean view_details_auto_scroll_callback(ViewIface *view)
{
	GtkTreeView	*tree = (GtkTreeView *) view;
	ViewDetails	*view_details = (ViewDetails *) view;
	FilerWindow	*filer_window = view_details->filer_window;
	GtkRange	*scrollbar = (GtkRange *) filer_window->scrollbar;
	GtkAdjustment	*adj;
	GdkWindow	*window;
	gint		x, y, w, h;
	GdkModifierType	mask;
	int		diff = 0;

	window = gtk_tree_view_get_bin_window(tree);

	gdk_window_get_pointer(window, &x, &y, &mask);
	gdk_drawable_get_size(window, &w, NULL);

	adj = gtk_range_get_adjustment(scrollbar);
	h = adj->page_size;

	if ((x < 0 || x > w || y < 0 || y > h) && !view_details->lasso_box)
		return FALSE;		/* Out of window - stop */

	if (y < AUTOSCROLL_STEP)
		diff = y - AUTOSCROLL_STEP;
	else if (y > h - AUTOSCROLL_STEP)
		diff = AUTOSCROLL_STEP + y - h;

	if (diff)
	{
		int	old = adj->value;
		int	value = old + diff;

		value = CLAMP(value, 0, adj->upper - adj->page_size);
		gtk_adjustment_set_value(adj, value);

		if (adj->value != old)
			dnd_spring_abort();
	}

	return TRUE;
}
