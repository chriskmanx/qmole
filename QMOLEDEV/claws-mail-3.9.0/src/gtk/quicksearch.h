/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Colin Leroy <colin@colino.net> 
 * and the Claws Mail team
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

#ifndef QUICKSEARCH_H
#define QUICKSEARCH_H 1

#include "advsearch.h"

typedef struct _QuickSearch QuickSearch;
typedef void (*QuickSearchExecuteCallback) (QuickSearch *quicksearch, gpointer data);

#include "procmsg.h"

QuickSearch *quicksearch_new();
GtkWidget *quicksearch_get_widget(QuickSearch *quicksearch);
void quicksearch_show(QuickSearch *quicksearch);
void quicksearch_hide(QuickSearch *quicksearch);
void quicksearch_set(QuickSearch *quicksearch, AdvancedSearchType type, const gchar *matchstring);
void quicksearch_set_recursive(QuickSearch *quicksearch, gboolean recursive);
gboolean quicksearch_has_sat_predicate(QuickSearch *quicksearch);
void quicksearch_set_execute_callback(QuickSearch *quicksearch,
				      QuickSearchExecuteCallback callback,
				      gpointer data);
void quicksearch_set_on_progress_cb(QuickSearch* search,
		gboolean (*cb)(gpointer data, guint at, guint matched, guint total), gpointer data);

gboolean quicksearch_run_on_folder(QuickSearch* quicksearch, FolderItem *folderItem, MsgInfoList **result);

gboolean quicksearch_is_running(QuickSearch *quicksearch);
gboolean quicksearch_has_focus(QuickSearch *quicksearch);
void quicksearch_pass_key(QuickSearch *quicksearch, guint val, GdkModifierType mod);
gboolean quicksearch_is_fast(QuickSearch *quicksearch);
gboolean quicksearch_is_in_typing(QuickSearch *quicksearch);
void quicksearch_relayout(QuickSearch *quicksearch);
void quicksearch_set_search_strings(QuickSearch *quicksearch);
#endif /* QUICKSEARCH_H */
