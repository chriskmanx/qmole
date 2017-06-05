/* $Id: e2_list.h 2743 2013-09-19 22:29:00Z tpgww $

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

#ifndef __E2_LIST_H__
#define __E2_LIST_H__

#include "emelfm2.h"

gint e2_list_strcmp (const gchar *a, const gchar *b);
void e2_list_update_history (GList **history, const gchar *latest, guint *cur,
	guint max, gboolean doubl);
gchar **e2_list_to_strv (GList *list) G_GNUC_MALLOC;
GList *e2_list_copy_with_data (GList *list) G_GNUC_MALLOC;
void e2_list_free_with_data (GList **list);
//void e2_list_free_data_only (GList **list);
//GList *e2_list_find_data_string (GList *list, gchar *search_text);
//void e2_list_nth_break (GList **list, guint breaker, GList **part1, GList **part2);
//void e2_list_break (GList **list, GList *breaker, gboolean nullatend, GList **part1,
//	GList **part2);

#endif //ndef __E2_LIST_H__
