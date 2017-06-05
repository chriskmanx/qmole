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


#ifndef COLLECT_TABLE_H
#define COLLECT_TABLE_H

#include "collect.h"

void collection_table_select_all(CollectTable *ct);
void collection_table_unselect_all(CollectTable *ct);

void collection_table_add_filelist(CollectTable *ct, GList *list);

void collection_table_file_update(CollectTable *ct, CollectInfo *ci);
void collection_table_file_add(CollectTable *ct, CollectInfo *ci);
void collection_table_file_insert(CollectTable *ct, CollectInfo *ci);
void collection_table_file_remove(CollectTable *ct, CollectInfo *ci);
void collection_table_refresh(CollectTable *ct);

CollectTable *collection_table_new(CollectionData *cd);

void collection_table_set_labels(CollectTable *ct, GtkWidget *status, GtkWidget *extra);

CollectInfo *collection_table_get_focus_info(CollectTable *ct);
GList *collection_table_selection_get_list(CollectTable *ct);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
