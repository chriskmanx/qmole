/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2012 the Claws Mail team
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

#ifndef ADVSEARCH_H
#define ADVSEARCH_H 1

#include "proctypes.h"
#include "folder.h"

// temporary
#include "matcher.h"

typedef enum
{
	ADVANCED_SEARCH_SUBJECT,
	ADVANCED_SEARCH_FROM,
	ADVANCED_SEARCH_TO,
	ADVANCED_SEARCH_EXTENDED,
	ADVANCED_SEARCH_MIXED,
	ADVANCED_SEARCH_TAG,
} AdvancedSearchType;


typedef struct _AdvancedSearch AdvancedSearch;

void advsearch_set_on_progress_cb(AdvancedSearch* search,
		gboolean (*cb)(gpointer data, guint at, guint matched, guint total), gpointer data);
void advsearch_set_on_error_cb(AdvancedSearch* search, void (*cb)(gpointer data), gpointer data);

AdvancedSearch *advsearch_new();
void advsearch_free(AdvancedSearch *search);

void advsearch_set(AdvancedSearch *search, AdvancedSearchType type, const gchar *matchstring);
gboolean advsearch_is_fast(AdvancedSearch *search);
gboolean advsearch_has_proper_predicate(AdvancedSearch *search);

gboolean advsearch_search_msgs_in_folders(AdvancedSearch* search, MsgInfoList **messages,
				          FolderItem* folderItem, gboolean recursive);

void advsearch_abort(AdvancedSearch *search);

gchar *advsearch_expand_search_string(const gchar *search_string);

#endif
