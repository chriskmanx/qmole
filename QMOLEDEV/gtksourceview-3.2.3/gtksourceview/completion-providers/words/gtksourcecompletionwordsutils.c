/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- *
 * gtksourcecompletionwordsutils.c
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2009 - Jesse van den Kieboom
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

#include "gtksourcecompletionwordsutils.h"

gboolean
gtk_source_completion_words_utils_backward_word_start (GtkTextIter    *iter,
                                                       CharacterCheck  valid,
                                                       CharacterCheck  valid_start,
                                                       gpointer        data)
{
	GtkTextIter prev = *iter;
	
	/* Go backward as long as there are word characters */
	while (TRUE)
	{
		/* Starting a line is good */
		if (gtk_text_iter_starts_line (&prev))
		{
			break;
		}
		
		gtk_text_iter_backward_char (&prev);
		
		/* Check if the previous character is a valid word character */
		if (!valid (gtk_text_iter_get_char (&prev), data))
		{
			break;
		}
		
		*iter = prev;
	}
	
	if (!valid (gtk_text_iter_get_char (iter), data))
	{
		return FALSE;
	}
	
	/* Go forward with while !valid_start */
	return valid_start (gtk_text_iter_get_char (iter), data);
}

gboolean
gtk_source_completion_words_utils_forward_word_end (GtkTextIter    *iter,
                                                    CharacterCheck  valid,
                                                    gpointer        data)
{
	/* Go backward as long as there are word characters */
	while (TRUE)
	{
		/* Ending a line is good */
		if (gtk_text_iter_ends_line (iter))
		{
			break;
		}
		
		/* Check if the next character is a valid word character */
		if (!valid (gtk_text_iter_get_char (iter), data))
		{
			break;
		}
		
		gtk_text_iter_forward_char (iter);
	}
	
	return TRUE;
}
