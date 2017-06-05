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

#ifndef VIEW_FILE_LIST_H
#define VIEW_FILE_LIST_H


#include "filedata.h"

gboolean vflist_press_key_cb(GtkWidget *widget, GdkEventKey *event, gpointer data);
gboolean vflist_press_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data);
gboolean vflist_release_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data);

void vflist_dnd_init(ViewFile *vf);

void vflist_destroy_cb(GtkWidget *widget, gpointer data);
ViewFile *vflist_new(ViewFile *vf, FileData *dir_fd);

gboolean vflist_set_fd(ViewFile *vf, FileData *dir_fd);
gboolean vflist_refresh(ViewFile *vf);

void vflist_thumb_set(ViewFile *vf, gboolean enable);
void vflist_marks_set(ViewFile *vf, gboolean enable);
void vflist_sort_set(ViewFile *vf, SortType type, gboolean ascend);

GList *vflist_selection_get_one(ViewFile *vf, FileData *fd);
GList *vflist_pop_menu_file_list(ViewFile *vf);
void vflist_pop_menu_view_cb(GtkWidget *widget, gpointer data);
void vflist_pop_menu_rename_cb(GtkWidget *widget, gpointer data);
void vflist_pop_menu_refresh_cb(GtkWidget *widget, gpointer data);
void vflist_popup_destroy_cb(GtkWidget *widget, gpointer data);
void vflist_pop_menu_thumbs_cb(GtkWidget *widget, gpointer data);

FileData *vflist_index_get_data(ViewFile *vf, gint row);
gint vflist_index_by_fd(ViewFile *vf, FileData *fd);
guint vflist_count(ViewFile *vf, gint64 *bytes);
GList *vflist_get_list(ViewFile *vf);

gboolean vflist_index_is_selected(ViewFile *vf, gint row);
guint vflist_selection_count(ViewFile *vf, gint64 *bytes);
GList *vflist_selection_get_list(ViewFile *vf);
GList *vflist_selection_get_list_by_index(ViewFile *vf);

void vflist_select_all(ViewFile *vf);
void vflist_select_none(ViewFile *vf);
void vflist_select_invert(ViewFile *vf);
void vflist_select_by_fd(ViewFile *vf, FileData *fd);

void vflist_mark_to_selection(ViewFile *vf, gint mark, MarkToSelectionMode mode);
void vflist_selection_to_mark(ViewFile *vf, gint mark, SelectionToMarkMode mode);

void vflist_color_set(ViewFile *vf, FileData *fd, gboolean color_set);

void vflist_thumb_progress_count(GList *list, gint *count, gint *done);
void vflist_set_thumb_fd(ViewFile *vf, FileData *fd);
FileData *vflist_thumb_next_fd(ViewFile *vf);
void vflist_thumb_reset_all(ViewFile *vf);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
