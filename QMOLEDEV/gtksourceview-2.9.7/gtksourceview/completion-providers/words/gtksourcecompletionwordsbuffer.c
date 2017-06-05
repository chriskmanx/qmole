/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletionwordsbuffer.c
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

#include "gtksourcecompletionwordsbuffer.h"
#include "gtksourcecompletionwordsutils.h"

#define GTK_SOURCE_COMPLETION_WORDS_BUFFER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER, GtkSourceCompletionWordsBufferPrivate))

#define REGION_FROM_LIST(list) ((ScanRegion *)list->data)

typedef struct
{
	guint start;
	guint end;
} ScanRegion;

enum
{
	EXT_INSERT_TEXT,
	EXT_INSERT_TEXT_AFTER,
	EXT_DELETE_RANGE,
	EXT_DELETE_RANGE_AFTER,
	NUM_EXT_SIGNALS
};

struct _GtkSourceCompletionWordsBufferPrivate
{
	GtkSourceCompletionWordsLibrary *library;
	GtkTextBuffer *buffer;
	
	GList *lines;
	gint current_insert_line;
	
	GList *scan_regions;
	guint idle_scan_id;
	
	guint ext_signal_handlers[NUM_EXT_SIGNALS];
	guint scan_batch_size;
	guint minimum_word_size;
	
	guint lock_handler_id;
	guint unlock_handler_id;
	
	GtkTextMark *mark;
};

G_DEFINE_TYPE (GtkSourceCompletionWordsBuffer, gtk_source_completion_words_buffer, G_TYPE_OBJECT)

static ScanRegion *
scan_region_new (gint start,
                 gint end)
{
	ScanRegion *region = (ScanRegion *)g_slice_new (ScanRegion);
	
	region->start = start;
	region->end = end;
	
	return region;
}

static void
scan_region_free (ScanRegion *region)
{
	g_slice_free (ScanRegion, region);
}

static void
remove_proposal (GtkSourceCompletionWordsProposal *proposal,
                 GtkSourceCompletionWordsBuffer   *buffer)
{
	gtk_source_completion_words_library_remove_word (buffer->priv->library,
	                                                 proposal);
}

static void
remove_line (GList                          *line,
             GtkSourceCompletionWordsBuffer *buffer)
{
	g_list_foreach (line, (GFunc)remove_proposal, buffer);
	g_list_free (line);
}

static void
gtk_source_completion_words_buffer_dispose (GObject *object)
{
	GtkSourceCompletionWordsBuffer *buffer =
			GTK_SOURCE_COMPLETION_WORDS_BUFFER (object);

	if (buffer->priv->mark)
	{
		gtk_text_buffer_delete_mark (gtk_text_mark_get_buffer (buffer->priv->mark),
		                             buffer->priv->mark);
		buffer->priv->mark = NULL;
	}

	if (buffer->priv->buffer)
	{
		gint i;
		
		for (i = 0; i < NUM_EXT_SIGNALS; ++i)
		{
			g_signal_handler_disconnect (buffer->priv->buffer,
			                             buffer->priv->ext_signal_handlers[i]);
		}
		
		g_object_unref (buffer->priv->buffer);
		buffer->priv->buffer = NULL;
	}
	
	if (buffer->priv->idle_scan_id)
	{
		g_source_remove (buffer->priv->idle_scan_id);
		buffer->priv->idle_scan_id = 0;
	}
	
	g_list_foreach (buffer->priv->scan_regions, (GFunc)scan_region_free, NULL);
	g_list_free (buffer->priv->scan_regions);
	
	buffer->priv->scan_regions = NULL;
	
	g_list_foreach (buffer->priv->lines, (GFunc)remove_line, buffer);
	g_list_free (buffer->priv->lines);

	buffer->priv->lines = NULL;

	if (buffer->priv->library)
	{
		g_signal_handler_disconnect (buffer->priv->library,
		                             buffer->priv->lock_handler_id);
		g_signal_handler_disconnect (buffer->priv->library,
		                             buffer->priv->unlock_handler_id);

		g_object_unref (buffer->priv->library);
		buffer->priv->library = NULL;
	}

	G_OBJECT_CLASS (gtk_source_completion_words_buffer_parent_class)->dispose (object);
}

static void
gtk_source_completion_words_buffer_class_init (GtkSourceCompletionWordsBufferClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->dispose = gtk_source_completion_words_buffer_dispose;

	g_type_class_add_private (object_class, sizeof(GtkSourceCompletionWordsBufferPrivate));
}

static void
gtk_source_completion_words_buffer_init (GtkSourceCompletionWordsBuffer *self)
{
	self->priv = GTK_SOURCE_COMPLETION_WORDS_BUFFER_GET_PRIVATE (self);
	
	self->priv->scan_batch_size = 20;
	self->priv->minimum_word_size = 3;
}

static void
on_insert_text_cb (GtkTextBuffer                  *textbuffer,
                   GtkTextIter                    *location,
                   const gchar                    *text,
                   gint                            len,
                   GtkSourceCompletionWordsBuffer *buffer)
{
	buffer->priv->current_insert_line = gtk_text_iter_get_line (location);
}

static gboolean
valid_word_char (gunichar ch, 
                 gpointer data)
{
	return g_unichar_isprint (ch) && (ch == '_' || g_unichar_isalnum (ch));
}

static gboolean
valid_start_char (gunichar ch)
{
	return !g_unichar_isdigit (ch);
}

static GList *
scan_line (GtkSourceCompletionWordsBuffer *buffer,
           gint                            line)
{
	GtkTextIter start;
	GtkTextIter end;
	GList *ret = NULL;
	
	gtk_text_buffer_get_iter_at_line (buffer->priv->buffer, &start, line);
	
	while (gtk_text_iter_get_line (&start) == line)
	{
		gchar *word;
		
		while (!gtk_text_iter_ends_line (&start) &&
		       !valid_word_char (gtk_text_iter_get_char (&start), NULL))
		{
			gtk_text_iter_forward_char (&start);
		}
		
		if (gtk_text_iter_ends_line (&start))
		{
			break;
		}

		end = start;
		
		if (!gtk_source_completion_words_utils_forward_word_end (&end,
		                                                         valid_word_char,
		                                                         NULL))
		{
			break;
		}
		
		if (valid_start_char (gtk_text_iter_get_char (&start)))
		{
			if (gtk_text_iter_get_offset (&end) - 
			    gtk_text_iter_get_offset (&start) >= buffer->priv->minimum_word_size)
			{
				GtkSourceCompletionWordsProposal *proposal;
			
				word = gtk_text_iter_get_text (&start, &end);
				proposal = gtk_source_completion_words_library_add_word (buffer->priv->library,
					                                                 word);
			
				if (proposal != NULL)
				{
					ret = g_list_prepend (ret, proposal);
					g_free (word);
				}
			}
		}
		
		start = end;
		
		if (!gtk_text_iter_forward_char (&start))
		{
			break;
		}
	}
	
	return g_list_reverse (ret);
}

static gboolean
idle_scan_regions (GtkSourceCompletionWordsBuffer *buffer)
{
	guint num = 0;
	GList *ptr = buffer->priv->lines;
	guint prev = 0;
	gboolean finished;
	
	/* Scan a few lines */
	while (buffer->priv->scan_regions)
	{
		ScanRegion *region = REGION_FROM_LIST (buffer->priv->scan_regions);

		gint span = (region->end - region->start) + 1;
		gint doscan = MIN(buffer->priv->scan_batch_size - num, span);
		gint i;
		
		ptr = g_list_nth (ptr, region->start - prev);
		
		for (i = 0; i < doscan; ++i)
		{
			/* First remove this line */
			remove_line ((GList *)ptr->data, buffer);
			
			/* Then scan it which adds words */
			ptr->data = scan_line (buffer, region->start + i);

			ptr = g_list_next (ptr);
		}
		
		prev = region->start + doscan;
		
		if (doscan == span)
		{
			/* Simply remove the region */
			scan_region_free (region);

			buffer->priv->scan_regions = 
				g_list_delete_link (buffer->priv->scan_regions,
				                    buffer->priv->scan_regions);
		}
		else
		{
			/* Update the region and break */
			region->start = region->start + doscan;
			break;
		}
		
		num += doscan;
	}

	finished = buffer->priv->scan_regions == NULL;
	
	if (finished)
	{
		buffer->priv->idle_scan_id = 0;
	}
	
	return !finished;
}

static void
remove_scan_regions (GtkSourceCompletionWordsBuffer *buffer,
                     gint                            start,
                     gint                            end)
{
	GList *item;
	gint span = end - start + 1;
	
	item = buffer->priv->scan_regions;

	while (item != NULL)
	{
		ScanRegion *region = REGION_FROM_LIST (item);
		
		if (region->start >= start)
		{
			if (region->end <= end)
			{
				/* Region fully within removed region */
				GList *remove = item;
				scan_region_free (region);

				item = g_list_next (item);
			
				/* Remove whole thing */
				buffer->priv->scan_regions = 
					g_list_delete_link (buffer->priv->scan_regions,
						            remove);
				continue;
			}
			else if (region->start <= end)
			{
				/* Top part of region in removed region */
				region->start = end + 1;
				region->end -= span;
			}
			else
			{
				/* Fully decrease */
				region->start -= span;
				region->end -= span;
			}
		}
		else if (region->end <= end && region->end > start)
		{
			/* Bottom part of region in removed region */
			region->end = start;
		}
		else if (region->end >= end)
		{
			region->end -= span;
		}

		item = g_list_next (item);
	}
}

static void
remove_range (GtkSourceCompletionWordsBuffer *buffer,
              gint                            start,
              gint                            end)
{
	if (start > end)
	{
		return;
	}
	
	remove_scan_regions (buffer, start, end);

	GList *line = g_list_nth (buffer->priv->lines, start);
	
	while (start <= end && line)
	{
		GList *cur = line;
		
		/* Remove proposals */
		remove_line ((GList *)line->data, buffer);
		line = g_list_next (cur);

		buffer->priv->lines = g_list_delete_link (buffer->priv->lines,
		                                          cur);
		++start;
	}
}

static void
add_scan_region (GtkSourceCompletionWordsBuffer *buffer,
                 gint                            start,
                 gint                            end)
{
	GList *item;
	GList *merge_start = NULL;
	GList *merge_end = NULL;
	GList *insert_after = NULL;

	gint line_count = gtk_text_buffer_get_line_count (buffer->priv->buffer);
	
	if (end >= line_count)
	{
		end = line_count - 1;
	}
	
	if (start > end)
	{
		return;
	}
	
	for (item = buffer->priv->scan_regions; item; item = g_list_next (item))
	{
		ScanRegion *region = (ScanRegion *)item->data;
		
		if (region->end < end)
		{
			insert_after = item;
		}

		/* Check if this region is overlapping, or directly adjacent to,
		   the new region */
		if (start <= region->end + 1 &&
		    end >= (gint)region->start - 1)
		{
			/* Merge ends at _least_ here, we keep updating
			   merge_end here until the region no longer qualifies
			   for merging */
			merge_end = item;
			
			if (!merge_start)
			{
				/* Merge starts here */
				merge_start = item;
			}
		}
		else if (merge_end != NULL)
		{
			/* Break early because following regions fall outside
			   of the new region and will not be merged */
			break;
		}
	}
	
	if (merge_start == NULL)
	{
		/* Simply prepend, there was no overlap */
		buffer->priv->scan_regions = 
			g_list_insert_before (buffer->priv->scan_regions,
			                      insert_after ? g_list_next (insert_after) : buffer->priv->scan_regions,
			                      scan_region_new (start, end));
	}
	else
	{
		ScanRegion *merged = REGION_FROM_LIST (merge_start);
		GList *item;

		/* 'merged' will be the merge of merge_start to merge_end,
		   including the new region. The regions next(merge_start) to
		   merge_end will be removed */
		merged->start = MIN(start, merged->start);
		merged->end = MAX(end, REGION_FROM_LIST (merge_end)->end);

		item = merge_start;
		
		while (item != merge_end)
		{
			item = g_list_next (item);
			scan_region_free (REGION_FROM_LIST (item));

			merge_start = g_list_delete_link (merge_start, item);
		}
	}
	
	if (buffer->priv->idle_scan_id == 0 && 
	    !gtk_source_completion_words_library_is_locked (buffer->priv->library))
	{
		buffer->priv->idle_scan_id = 
			g_idle_add_full (G_PRIORITY_LOW,
			                 (GSourceFunc)idle_scan_regions,
			                 buffer,
			                 NULL);
	}
}

static void
handle_text_inserted (GtkSourceCompletionWordsBuffer *buffer,
                      gint                            start,
                      gint                            end)
{
	gint pos = start;
	GList *ptr = NULL;
	GList *newlines = NULL;
	GList *last = NULL;
	
	while (pos < end)
	{
		newlines = g_list_prepend (newlines, NULL);

		if (last == NULL)
		{
			last = newlines;
		}

		++pos;
	}

	if (start > end)
	{
		ptr = g_list_nth (buffer->priv->lines,
		                  start + 1);
	}

	if (ptr != NULL)
	{
		if (ptr->prev)
		{
			ptr->prev->next = newlines;
			newlines->prev = ptr->prev;
		}

		newlines->next = ptr;
		ptr->prev = last;
	}
	else
	{
		buffer->priv->lines = g_list_concat (buffer->priv->lines,
		                                     newlines);
	}

	/* Invalidate new region */
	add_scan_region (buffer, 
	                 start,
	                 end);
}

static void
on_insert_text_after_cb (GtkTextBuffer                  *text_buffer,
                         GtkTextIter                    *location,
                         const gchar                    *text,
                         gint                            len,
                         GtkSourceCompletionWordsBuffer *buffer)
{
	handle_text_inserted (buffer,
	                      buffer->priv->current_insert_line,
	                      gtk_text_iter_get_line (location));
}

static void
on_delete_range_cb (GtkTextBuffer                  *text_buffer,
                    GtkTextIter                    *start,
                    GtkTextIter                    *end,
                    GtkSourceCompletionWordsBuffer *buffer)
{
	gint start_line = gtk_text_iter_get_line (start);
	gint end_line = gtk_text_iter_get_line (end);
	
	/* Simply remove everything from lines start + 1 to end */
	remove_range (buffer, start_line + 1, end_line);
}

static void
on_delete_range_after_cb (GtkTextBuffer                  *text_buffer,
                          GtkTextIter                    *start,
                          GtkTextIter                    *end,
                          GtkSourceCompletionWordsBuffer *buffer)
{
	gint start_line = gtk_text_iter_get_line (start);
	
	/* Add start line to scan regions */
	add_scan_region (buffer, start_line, start_line);
}

static void
connect_buffer (GtkSourceCompletionWordsBuffer *buffer)
{
	buffer->priv->ext_signal_handlers[EXT_INSERT_TEXT] =
		g_signal_connect (buffer->priv->buffer,
			          "insert-text",
			          G_CALLBACK (on_insert_text_cb),
			          buffer);

	buffer->priv->ext_signal_handlers[EXT_INSERT_TEXT_AFTER] =
		g_signal_connect_after (buffer->priv->buffer,
			                "insert-text",
			                G_CALLBACK (on_insert_text_after_cb),
			                buffer);

	buffer->priv->ext_signal_handlers[EXT_DELETE_RANGE] =
		g_signal_connect (buffer->priv->buffer,
			          "delete-range",
			          G_CALLBACK (on_delete_range_cb),
			          buffer);

	buffer->priv->ext_signal_handlers[EXT_DELETE_RANGE_AFTER] =
		g_signal_connect_after (buffer->priv->buffer,
			                "delete-range",
			                G_CALLBACK (on_delete_range_after_cb),
			                buffer);

	/* Start initial scan */
	handle_text_inserted (buffer,
	                      0,
	                      gtk_text_buffer_get_line_count (buffer->priv->buffer));
}

static void
on_library_lock (GtkSourceCompletionWordsBuffer *buffer)
{
	if (buffer->priv->idle_scan_id != 0)
	{
		g_source_remove (buffer->priv->idle_scan_id);
		buffer->priv->idle_scan_id = 0;
	}
}

static void
on_library_unlock (GtkSourceCompletionWordsBuffer *buffer)
{
	if (buffer->priv->idle_scan_id == 0 &&
	    buffer->priv->scan_regions != NULL)
	{
		buffer->priv->idle_scan_id = 
			g_idle_add_full (G_PRIORITY_LOW,
			                 (GSourceFunc)idle_scan_regions,
			                 buffer,
			                 NULL);
	}
}

GtkSourceCompletionWordsBuffer *
gtk_source_completion_words_buffer_new (GtkSourceCompletionWordsLibrary *library,
                                        GtkTextBuffer                   *buffer)
{
	GtkSourceCompletionWordsBuffer *ret;
	GtkTextIter iter;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_WORDS_LIBRARY (library), NULL);
	g_return_val_if_fail (GTK_IS_TEXT_BUFFER (buffer), NULL);
	
	ret = g_object_new (GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER, NULL);

	ret->priv->library = g_object_ref (library);
	ret->priv->buffer = g_object_ref (buffer);
	
	ret->priv->lock_handler_id =
		g_signal_connect_swapped (ret->priv->library,
			                  "lock",
			                  G_CALLBACK (on_library_lock),
			                  ret);

	ret->priv->unlock_handler_id =
		g_signal_connect_swapped (ret->priv->library,
			                  "unlock",
			                  G_CALLBACK (on_library_unlock),
			                  ret);
	
	gtk_text_buffer_get_start_iter (buffer, &iter);
	ret->priv->mark = gtk_text_buffer_create_mark (buffer, NULL, &iter, TRUE);
	
	connect_buffer (ret);

	return ret;
}

GtkTextBuffer *
gtk_source_completion_words_buffer_get_buffer (GtkSourceCompletionWordsBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_WORDS_BUFFER (buffer), NULL);
	
	return buffer->priv->buffer;
}

void
gtk_source_completion_words_buffer_set_scan_batch_size (GtkSourceCompletionWordsBuffer *buffer,
                                                        guint                           size)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_WORDS_BUFFER (buffer));
	g_return_if_fail (size != 0);

	buffer->priv->scan_batch_size = size;
}

void
gtk_source_completion_words_buffer_set_minimum_word_size (GtkSourceCompletionWordsBuffer *buffer,
                                                          guint                           size)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_WORDS_BUFFER (buffer));
	g_return_if_fail (size != 0);

	buffer->priv->minimum_word_size = size;
}

GtkTextMark *
gtk_source_completion_words_buffer_get_mark (GtkSourceCompletionWordsBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_WORDS_BUFFER (buffer), NULL);
	
	return buffer->priv->mark;
}
