/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletion-utils.h
 * This file is part of gtksourcecompletion
 *
 * Copyright (C) 2007 - 2009 Jesús Barbero Rodríguez <chuchiperriman@gmail.com>
 * Copyright (C) 2009 - Jesse van den Kieboom <jessevdk@gnome.org>
 *
 * gtksourceview is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * gtksourceview is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
 
#ifndef __GTK_SOURCE_COMPLETION_UTILS_H__
#define __GTK_SOURCE_COMPLETION_UTILS_H__

#include <gtksourceview/gtksourceview.h>

G_BEGIN_DECLS

gboolean	 gtk_source_completion_utils_is_separator		(gunichar         ch);

gchar		*gtk_source_completion_utils_get_word_iter		(GtkSourceBuffer *source_buffer, 
									 GtkTextIter     *current,
									 GtkTextIter     *start_word, 
									 GtkTextIter     *end_word);

gchar		*gtk_source_completion_utils_get_word			(GtkSourceBuffer *text_view);

void		 gtk_source_completion_utils_replace_word		(GtkSourceBuffer *source_buffer,
									 GtkTextIter     *iter,
									 const gchar     *text,
									 gint             len);

void		 gtk_source_completion_utils_replace_current_word	(GtkSourceBuffer *source_buffer, 
									 const gchar     *text,
									 gint             len);

void		 gtk_source_completion_utils_move_to_iter		(GtkWindow       *window,
									 GtkSourceView   *view,
									 GtkTextIter     *iter);

void		 gtk_source_completion_utils_move_to_cursor		(GtkWindow       *window,
									 GtkSourceView   *view);

G_END_DECLS

#endif /* __GTK_SOURCE_COMPLETION_ITEM_H__ */
