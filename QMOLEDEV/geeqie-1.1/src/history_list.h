/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis, Vladimir Nadvornik, Laurent Monin
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef HISTORY_LIST_H
#define HISTORY_LIST_H

/* history lists */

gboolean history_list_load(const gchar *path);
gboolean history_list_save(const gchar *path);

void history_list_free_key(const gchar *key);

void history_list_add_to_key(const gchar *key, const gchar *path, gint max);

void history_list_item_change(const gchar *key, const gchar *oldpath, const gchar *newpath);
void history_list_item_move(const gchar *key, const gchar *path, gint direction);
void history_list_item_remove(const gchar *key, const gchar *path);

const gchar *history_list_find_last_path_by_key(const gchar *key);

/* the returned GList is internal, don't free it */
GList *history_list_get_by_key(const gchar *key);


#endif /* HISTORY_LIST_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
