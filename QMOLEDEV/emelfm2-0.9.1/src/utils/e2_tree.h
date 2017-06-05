/* $Id: e2_tree.h 2743 2013-09-19 22:29:00Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelfm2.
emelfm2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_TREE_H__
#define __E2_TREE_H__

#include "emelfm2.h"

gboolean e2_tree_find_iter_from_str (GtkTreeModel *model, gint column,
	const gchar *search, GtkTreeIter *iter, gboolean with_children);
gboolean e2_tree_find_iter_from_str_simple (GtkTreeModel *model, gint column,
	const gchar *search, GtkTreeIter *iter, gboolean with_children);
gboolean e2_tree_find_iter_from_str_same (GtkTreeModel *model, gint column,
	const gchar *search, GtkTreeIter *iter);
gboolean e2_tree_get_lowest_iter_for_str (GtkTreeModel *model, gint column,
	GtkTreeIter *iter, const gchar *search);
//gboolean e2_tree_find_lowest_iter_for_str (GtkTreeModel *model, gint column,
//	GtkTreeIter *iter, const gchar *search);
//gchar *e2_tree_get_last_string (GtkTreeModel *model, gint column);
//guint e2_tree_store_count (GtkTreeModel *model);
GtkTreeRowReference *e2_tree_iter_to_ref (GtkTreeStore *store, GtkTreeIter *iter) G_GNUC_MALLOC;
gboolean e2_tree_ref_to_iter (GtkTreeStore *store, GtkTreeRowReference *ref,
	GtkTreeIter *iter);
void e2_tree_expand_all_cb (GtkMenuItem *widget, GtkTreeView *treeview);
void e2_tree_collapse_all_cb (GtkMenuItem *widget, GtkTreeView *treeview);
gboolean e2_tree_iter_previous (GtkTreeModel *model, GtkTreeIter *iter);
GList *e2_tree_copy (GtkTreeView *treeview) G_GNUC_MALLOC;
void e2_tree_paste (GList *rowscopied, GtkTreeView *treeview);
void e2_tree_delete (GtkTreeView *treeview);
//gchar *e2_tree_row_to_string (GtkTreeModel *model, GtkTreeIter *iter,
//	gint columns, gint level) G_GNUC_MALLOC;
//#ifdef STORECOPY
void e2_tree_store_copy (GtkTreeModel *model, gboolean treetype, gpointer *newstore);
//#endif

#endif //ndef __E2_TREE_H__
