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

#ifndef VIEW_DIR_LIST_H
#define VIEW_DIR_LIST_H


ViewDir *vdlist_new(ViewDir *vd, FileData *dir_fd);

gboolean vdlist_set_fd(ViewDir *vd, FileData *dir_fd);
void vdlist_refresh(ViewDir *vd);

const gchar *vdlist_row_get_path(ViewDir *vd, gint row);
gboolean vdlist_find_row(ViewDir *vd, FileData *fd, GtkTreeIter *iter);

void vdlist_rename_by_row(ViewDir *vd, FileData *fd);
FileData *vdlist_row_by_path(ViewDir *vd, const gchar *path, gint *row);

gboolean vdlist_press_key_cb(GtkWidget *widget, GdkEventKey *event, gpointer data);
gboolean vdlist_press_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data);

void vdlist_destroy_cb(GtkWidget *widget, gpointer data);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
