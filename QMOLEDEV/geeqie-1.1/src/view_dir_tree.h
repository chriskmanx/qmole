/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef VIEW_DIR_TREE_H
#define VIEW_DIR_TREE_H

typedef struct _NodeData NodeData;
struct _NodeData
{
	FileData *fd;
	gboolean expanded;
	time_t last_update;
	gint version;
};

ViewDir *vdtree_new(ViewDir *vd, FileData *dir_fd);

gboolean vdtree_set_fd(ViewDir *vd, FileData *dir_fd);
void vdtree_refresh(ViewDir *vd);

const gchar *vdtree_row_get_path(ViewDir *vd, gint row);
gboolean vdtree_find_row(ViewDir *vd, FileData *fd, GtkTreeIter *iter, GtkTreeIter *parent);
gboolean vdtree_populate_path_by_iter(ViewDir *vd, GtkTreeIter *iter, gboolean force, FileData *target_fd);

FileData *vdtree_populate_path(ViewDir *vd, FileData *target_fd, gboolean expand, gboolean force);
void vdtree_rename_by_data(ViewDir *vd, FileData *fd);

gboolean vdtree_press_key_cb(GtkWidget *widget, GdkEventKey *event, gpointer data);
gboolean vdtree_press_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data);

void vdtree_destroy_cb(GtkWidget *widget, gpointer data);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
