/*
 * gtksourcecompletionwordsutils.h
 * This file is part of gtksourceview
 *
 * Copyright (C) 2009 - Jesse van den Kieboom
 *
 * gtksourceview is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * gtksourceview is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with gtksourceview; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include <gtk/gtk.h>

G_BEGIN_DECLS

typedef gboolean (*CharacterCheck)(gunichar ch, gpointer data);

gboolean	 gtk_source_completion_words_utils_forward_word_end 	(GtkTextIter    *iter,
									 CharacterCheck  valid,
									 gpointer        data);

gboolean	 gtk_source_completion_words_utils_backward_word_start 	(GtkTextIter    *iter,
									 CharacterCheck  valid,
									 CharacterCheck  valid_start,
									 gpointer        data);

G_END_DECLS
