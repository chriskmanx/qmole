/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto
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

#ifndef __PREFS_FILTERING_H__
#define __PREFS_FILTERING_H__

#include <glib.h>

/*
void prefs_scoring_read_config	(void);
void prefs_scoring_write_config	(void);
*/

typedef enum
{
	FILTER_BY_NONE,
	FILTER_BY_AUTO,
	FILTER_BY_FROM,
	FILTER_BY_TO,
	FILTER_BY_SUBJECT
} PrefsFilterType;

#include "folder.h"

void prefs_filtering_open(GSList ** p_processing,
			  const gchar *title,
			  const gchar *help_url_anchor,
			  const gchar *header,
			  const gchar *key,
			  gboolean per_account_filtering);

void prefs_filtering_rename_path	(const gchar	*old_path,
				 	 const gchar	*new_path);
void prefs_filtering_delete_path	(const gchar	*path);

void prefs_filtering_rename_tag(const gchar *old_tag, const gchar *new_tag);

#endif /* __PREFS_FILTERING_H__ */
