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

#ifndef VIEW_FILE_ICON_H
#define VIEW_FILE_ICON_H

gboolean vficon_press_key_cb(GtkWidget *widget, GdkEventKey *event, gpointer data);
gboolean vficon_press_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data);
gboolean vficon_release_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data);

void vficon_dnd_init(ViewFile *vf);

void vficon_destroy_cb(GtkWidget *widget, gpointer data);
ViewFile *vficon_new(ViewFile *vf, FileData *dir_fd);

gboolean vficon_set_fd(ViewFile *vf, FileData *dir_fd);
gboolean vficon_refresh(ViewFile *vf);

void vficon_sort_set(ViewFile *vf, SortType type, gboolean ascend);

void vficon_marks_set(ViewFile *vf, gboolean enable);

GList *vficon_selection_get_one(ViewFile *vf, FileData *fd);
GList *vficon_pop_menu_file_list(ViewFile *vf);
void vficon_pop_menu_view_cb(GtkWidget *widget, gpointer data);
void vficon_pop_menu_rename_cb(GtkWidget *widget, gpointer data);
void vficon_pop_menu_refresh_cb(GtkWidget *widget, gpointer data);
void vficon_popup_destroy_cb(GtkWidget *widget, gpointer data);
void vficon_pop_menu_show_names_cb(GtkWidget *widget, gpointer data);

FileData *vficon_index_get_data(ViewFile *vf, gint row);
gint vficon_index_by_fd(ViewFile *vf, FileData *in_fd);
guint vficon_count(ViewFile *vf, gint64 *bytes);
GList *vficon_get_list(ViewFile *vf);

gboolean vficon_index_is_selected(ViewFile *vf, gint row);
guint vficon_selection_count(ViewFile *vf, gint64 *bytes);
GList *vficon_selection_get_list(ViewFile *vf);
GList *vficon_selection_get_list_by_index(ViewFile *vf);

void vficon_select_all(ViewFile *vf);
void vficon_select_none(ViewFile *vf);
void vficon_select_invert(ViewFile *vf);
void vficon_select_by_fd(ViewFile *vf, FileData *fd);

void vficon_mark_to_selection(ViewFile *vf, gint mark, MarkToSelectionMode mode);
void vficon_selection_to_mark(ViewFile *vf, gint mark, SelectionToMarkMode mode);


void vficon_thumb_progress_count(GList *list, gint *count, gint *done);
void vficon_set_thumb_fd(ViewFile *vf, FileData *fd);
FileData *vficon_thumb_next_fd(ViewFile *vf);
void vficon_thumb_reset_all(ViewFile *vf);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
