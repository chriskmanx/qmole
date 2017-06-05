/* $Id: e2_textiter.h 2318 2011-01-27 03:50:22Z tpgww $

 Copyright (C) 2003-2011 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_TEXT_ITER_H__
#define __E2_TEXT_ITER_H__

#ifndef USE_GTK3_0
# include <gtk/gtktextiter.h>
#endif

//this is a superset of GtkTextSearchFlags
typedef enum
{
	E2_SEARCH_VISIBLE_ONLY = 1,	//=GTK_TEXT_SEARCH_VISIBLE_ONLY
	E2_SEARCH_TEXT_ONLY    = 1 << 1,	//=GTK_TEXT_SEARCH_VISIBLE_ONLY
	E2_SEARCH_CASE_INSENSITIVE = 1 << 2,	//possible future support in gtk
	E2_SEARCH_REGEXP       = 1 << 3,	//possible future support in gtk
	E2_SEARCH_WHOLE_WORD   = 1 << 4
} E2TextSearchFlags;

gboolean e2_iter_forward_search (
			const GtkTextIter *iter,
			const gchar       *str,
			E2TextSearchFlags  flags,
			GtkTextIter       *match_start,
			GtkTextIter       *match_end,
			const GtkTextIter *limit);

gboolean e2_iter_backward_search (
			const GtkTextIter *iter,
			const gchar       *str,
			E2TextSearchFlags  flags,
			GtkTextIter       *match_start,
			GtkTextIter       *match_end,
			const GtkTextIter *limit);

#endif //ndef __E2_TEXT_ITER_H__
