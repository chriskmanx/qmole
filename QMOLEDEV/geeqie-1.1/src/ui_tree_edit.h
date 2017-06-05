/*
 * (SLIK) SimpLIstic sKin functions
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef UI_TREE_EDIT_H
#define UI_TREE_EDIT_H


typedef struct _TreeEditData TreeEditData;
struct _TreeEditData
{
	GtkWidget *window;
	GtkWidget *entry;

	gchar *old_name;
	gchar *new_name;

	gint (*edit_func)(TreeEditData *ted, const gchar *oldname, const gchar *newname, gpointer data);
	gpointer edit_data;

	GtkTreeView *tree;
	GtkTreePath *path;
	GtkTreeViewColumn *column;
	GtkCellRenderer *cell;
};


/*
 * edit_func: return TRUE if rename successful, FALSE on failure.
 */
gboolean tree_edit_by_path(GtkTreeView *tree, GtkTreePath *tpath, gint column, const gchar *text,
		           gboolean (*edit_func)(TreeEditData *, const gchar *, const gchar *, gpointer), gpointer data);


/* returns location of cell in screen coordinates */
gboolean tree_view_get_cell_origin(GtkTreeView *widget, GtkTreePath *tpath, gint column, gboolean text_cell_only,
			           gint *x, gint *y, gint *width, gint *height);
/* similar to above, but limits the returned area to that of the tree window */
void tree_view_get_cell_clamped(GtkTreeView *widget, GtkTreePath *tpath, gint column, gboolean text_cell_only,
			       gint *x, gint *y, gint *width, gint *height);

/* return 0 = row visible, -1 = row is above, 1 = row is below visible region
 * if fully_visible is TRUE, the bahavior changes to return -1/1 if _any_ part of the cell is out of view */
gint tree_view_row_get_visibility(GtkTreeView *widget, GtkTreeIter *iter, gboolean fully_visible);

/* scrolls to make row visible, if necessary
 * return is same as above (before the scroll)
 */
gint tree_view_row_make_visible(GtkTreeView *widget, GtkTreeIter *iter, gboolean center);

/* if iter is location of cursor, moves cursor to nearest row */
gboolean tree_view_move_cursor_away(GtkTreeView *widget, GtkTreeIter *iter, gboolean only_selected);

/* utility to return row position of given GtkTreePath
 */
gint tree_path_to_row(GtkTreePath *tpath);


/* shifts a GdkColor values lighter or darker
 * val is percent from 1 to 100, or -1 for default (usually 10%)
 * direction is -1 darker, 0 auto, 1 lighter
 */
void shift_color(GdkColor *src, gshort val, gint direction);

/*
 * Shifts a style's color for given state
 * Useful for alternating dark/light rows in lists.
 *
 * shift_value is 1 to 100, representing the percent of the shift.
 */
void style_shift_color(GtkStyle *style, GtkStateType type, gshort shift_value, gint direction);

/*
 * The standard shift percent for alternating list row colors
 */
#define STYLE_SHIFT_STANDARD 10

/*
 * auto scroll, set scroll_speed or region_size to -1 to their respective the defaults
 * notify_func will be called before a scroll, return FALSE to turn off autoscroll
 */
gint widget_auto_scroll_start(GtkWidget *widget, GtkAdjustment *v_adj, gint scroll_speed, gint region_size,
			      gint (*notify_func)(GtkWidget *widget, gint x, gint y, gpointer data), gpointer notify_data);
void widget_auto_scroll_stop(GtkWidget *widget);


/*
 * Various g_list utils, do not really fit anywhere, so they are here.
 */
GList *uig_list_insert_link(GList *list, GList *link, gpointer data);
GList *uig_list_insert_list(GList *parent, GList *insert_link, GList *list);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
