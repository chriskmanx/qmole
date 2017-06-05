/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletionwordsbuffer.c
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

#include "gtksourcecompletionwordsbuffer.h"
#include "gtksourcecompletionwordsutils.h"

#include <glib.h>

#define GTK_SOURCE_COMPLETION_WORDS_BUFFER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_SOURCE_TYPE_COMPLETION_WORDS_BUFFER, GtkSourceCompletionWordsBufferPrivate))

/* Timeout in seconds */
#define INITIATE_SCAN_TIMEOUT 5

/* Timeout in milliseconds */
#define BATCH_SCAN_TIMEOUT 10

typedef struct
{
	GtkSourceCompletionWordsProposal *proposal;
	guint use_count;
} ProposalCache;

typedef struct
{
	GtkTextMark *start;
	GtkTextMark *end;
} ScanRegion;

enum
{
	EXT_INSERT_TEXT,
	EXT_DELETE_RANGE,
	NUM_EXT_SIGNALS
};

struct _GtkSourceCompletionWordsBufferPrivate
{
	GtkSourceCompletionWordsLibrary *library;
	GtkTextBuffer *buffer;

	GList *scan_regions;
	gulong batch_scan_id;
	gulong initiate_scan_id;

	guint ext_signal_handlers[NUM_EXT_SIGNALS];
	guint scan_batch_size;
	guint minimum_word_size;

	guint lock_handler_id;
	guint unlock_handler_id;

	GtkTextMark *mark;
	GHashTable *words;
};

G_DEFINE_TYPE (GtkSourceCompletionWordsBuffer, gtk_source_completion_words_buffer, G_TYPE_OBJECT)

static ProposalCache *
proposal_cache_new (GtkSourceCompletionWordsProposal *proposal)
{
	ProposalCache *cache = g_slice_new (ProposalCache);
	cache->proposal = g_object_ref (proposal);
	cache->use_count = 1;

	return cache;
}

static void
proposal_cache_free (ProposalCache *cache)
{
	g_object_unref (cache->proposal);
	g_slice_free (ProposalCache, cache);
}

static ScanRegion *
scan_region_new (GtkTextBuffer *buffer,
                 GtkTextIter   *start,
                 GtkTextIter   *end)
{
	ScanRegion *region = g_slice_new (ScanRegion);

	region->start = g_object_ref (gtk_text_buffer_create_mark (buffer,
	                                                           NULL,
	                                                           start,
	                                                           TRUE));

	region->end = g_object_ref (gtk_text_buffer_create_mark (buffer,
	                                                         NULL,
	                                                         end,
	                                                         FALSE));

	return region;
}

static void
scan_region_free (ScanRegion *region)
{
	GtkTextBuffer *buffer = gtk_text_mark_get_buffer (region->start);

	if (!gtk_text_mark_get_deleted (region->start))
	{
		gtk_text_buffer_delete_mark (buffer, region->start);
	}

	g_object_unref (region->start);

	if (!gtk_text_mark_get_deleted (region->end))
	{
		gtk_text_buffer_delete_mark (buffer, region->end);
	}

	g_object_unref (region->end);

	g_slice_free (ScanRegion, region);
}

static void
remove_proposal_cache (const gchar                    *key,
                       ProposalCache                  *cache,
                       GtkSourceCompletionWordsBuffer *buffer)
{
	guint i;

	for (i = 0; i < cache->use_count; ++i)
	{
		gtk_source_completion_words_library_remove_word (buffer->priv->library,
		                                                 cache->proposal);
	}
}

static void
remove_words (GtkSourceCompletionWordsBuffer *buffer)
{
	g_hash_table_foreach (buffer->priv->words,
	                      (GHFunc)remove_proposal_cache,
	                      buffer);

	g_hash_table_remove_all (buffer->priv->words);
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

	if (buffer->priv->words)
	{
		remove_words (buffer);

		g_hash_table_destroy (buffer->priv->words);
		buffer->priv->words = NULL;
	}

	g_list_foreach (buffer->priv->scan_regions, (GFunc)scan_region_free, NULL);
	g_list_free (buffer->priv->scan_regions);
	buffer->priv->scan_regions = NULL;

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

	if (buffer->priv->batch_scan_id)
	{
		g_source_remove (buffer->priv->batch_scan_id);
		buffer->priv->batch_scan_id = 0;
	}

	if (buffer->priv->initiate_scan_id)
	{
		g_source_remove (buffer->priv->initiate_scan_id);
		buffer->priv->initiate_scan_id = 0;
	}

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

	self->priv->words = g_hash_table_new_full (g_str_hash,
	                                           g_str_equal,
	                                           (GDestroyNotify)g_free,
	                                           (GDestroyNotify)proposal_cache_free);
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

static GSList *
scan_line (GtkSourceCompletionWordsBuffer *buffer,
           GtkTextIter                    *start)
{
	GSList *ret = NULL;
	GtkTextIter end;
	gint line = gtk_text_iter_get_line (start);

	while (line == gtk_text_iter_get_line (start))
	{
		gchar *word;

		while (!gtk_text_iter_ends_line (start) &&
		       !valid_word_char (gtk_text_iter_get_char (start), NULL))
		{
			gtk_text_iter_forward_char (start);
		}

		if (gtk_text_iter_ends_line (start))
		{
			break;
		}

		end = *start;

		if (!gtk_source_completion_words_utils_forward_word_end (&end,
		                                                         valid_word_char,
		                                                         NULL))
		{
			break;
		}

		if (valid_start_char (gtk_text_iter_get_char (start)))
		{
			if (gtk_text_iter_get_offset (&end) -
			    gtk_text_iter_get_offset (start) >= buffer->priv->minimum_word_size)
			{
				word = gtk_text_iter_get_text (start, &end);
				ret = g_slist_prepend (ret, word);
			}
		}

		*start = end;

		if (!gtk_text_iter_forward_char (start))
		{
			break;
		}
	}

	return ret;
}

static void
remove_word (GtkSourceCompletionWordsBuffer *buffer,
             const gchar                    *word)
{
	ProposalCache *cache;

	cache = g_hash_table_lookup (buffer->priv->words, word);

	if (!cache)
	{
		g_warning ("Could not find word to remove in buffer (%s), this should not happen!",
		           word);
		return;
	}

	gtk_source_completion_words_library_remove_word (buffer->priv->library,
	                                                 cache->proposal);

	--cache->use_count;

	if (cache->use_count == 0)
	{
		g_hash_table_remove (buffer->priv->words, word);
	}
}

static void
add_words (GtkSourceCompletionWordsBuffer *buffer,
           GSList                         *words)
{
	GSList *item;

	for (item = words; item; item = g_slist_next (item))
	{
		GtkSourceCompletionWordsProposal *proposal;
		ProposalCache *cache;

		proposal = gtk_source_completion_words_library_add_word (buffer->priv->library,
		                                                         item->data);

		cache = g_hash_table_lookup (buffer->priv->words,
		                             item->data);

		if (cache)
		{
			++cache->use_count;
			g_free (item->data);
		}
		else
		{
			/* Hash table takes over ownership of the word string */
			cache = proposal_cache_new (proposal);
			g_hash_table_insert (buffer->priv->words,
			                     item->data,
			                     cache);
		}
	}

	g_slist_free (words);
}

static gboolean
idle_scan_regions (GtkSourceCompletionWordsBuffer *buffer)
{
	gboolean finished;
	guint num = buffer->priv->scan_batch_size;

	/* Scan some regions */
	while (buffer->priv->scan_regions)
	{
		ScanRegion *region = buffer->priv->scan_regions->data;
		GtkTextIter start;
		GtkTextIter end;

		gtk_text_buffer_get_iter_at_mark (buffer->priv->buffer,
		                                  &start,
		                                  region->start);

		gtk_text_buffer_get_iter_at_mark (buffer->priv->buffer,
		                                  &end,
		                                  region->end);

		while (gtk_text_iter_compare (&start, &end) < 0 && num)
		{
			GSList *words;

			words = scan_line (buffer, &start);

			/* add_words also frees the list */
			add_words (buffer, words);

			--num;

			if (!gtk_text_iter_forward_line (&start))
			{
				num = 0;
				break;
			}
		}

		if (gtk_text_iter_compare (&start, &end) >= 0 || num != 0)
		{
			/* Done with region */
			scan_region_free (region);
			buffer->priv->scan_regions = g_list_delete_link (buffer->priv->scan_regions,
			                                                 buffer->priv->scan_regions);
		}
		else
		{
			gtk_text_buffer_move_mark (buffer->priv->buffer,
			                           region->start,
			                           &start);
			break;
		}
	}

	finished = buffer->priv->scan_regions == NULL;

	if (finished)
	{
		buffer->priv->batch_scan_id = 0;
	}

	return !finished;
}

static gboolean
initiate_scan (GtkSourceCompletionWordsBuffer *buffer)
{
	buffer->priv->initiate_scan_id = 0;

	/* Add the batch scanner */
	buffer->priv->batch_scan_id =
		g_timeout_add_full (G_PRIORITY_LOW,
		                    BATCH_SCAN_TIMEOUT,
		                    (GSourceFunc)idle_scan_regions,
		                    buffer,
		                    NULL);

	return FALSE;
}

static void
install_initiate_scan (GtkSourceCompletionWordsBuffer *buffer)
{
	if (buffer->priv->batch_scan_id == 0 &&
	    buffer->priv->initiate_scan_id == 0)
	{
		buffer->priv->initiate_scan_id =
			g_timeout_add_seconds_full (G_PRIORITY_LOW,
			                            INITIATE_SCAN_TIMEOUT,
			                            (GSourceFunc)initiate_scan,
			                            buffer,
			                            NULL);
	}
}

static void
add_scan_region (GtkSourceCompletionWordsBuffer *buffer,
                 GList                          *after,
                 GtkTextIter                    *start,
                 GtkTextIter                    *end,
                 gboolean                        remove_first)
{
	GList *next;
	ScanRegion *region;

	if (remove_first)
	{
		/* First remove all the words currently in the region */
		GtkTextIter cstart = *start;

		while (gtk_text_iter_compare (&cstart, end) < 0)
		{
			GSList *words = scan_line (buffer, &cstart);
			GSList *item;

			for (item = words; item; item = g_slist_next (item))
			{
				remove_word (buffer, item->data);
				g_free (item->data);
			}

			g_slist_free (words);

			if (!gtk_text_iter_forward_line (&cstart))
			{
				break;
			}
		}
	}

	/* Then just add the scan region */
	region = scan_region_new (buffer->priv->buffer,
	                          start,
	                          end);

	if (after == NULL)
	{
		buffer->priv->scan_regions = g_list_prepend (buffer->priv->scan_regions,
		                                             region);
	}

	next = g_list_next (after);

	if (next != NULL)
	{
		buffer->priv->scan_regions =
			g_list_insert_before (buffer->priv->scan_regions,
			                      next,
			                      region);
	}

	/* Add the initate scan timeout if it's not already running */
	install_initiate_scan (buffer);
}

static void
remove_and_rescan (GtkSourceCompletionWordsBuffer *buffer,
                   GtkTextIter                    *start,
                   GtkTextIter                    *end)
{
	GList *item;
	GtkTextIter startc = *start;
	GtkTextIter endc;

	if (end)
	{
		endc = *end;
	}
	else
	{
		endc = *start;
	}

	if (!gtk_text_iter_starts_line (&startc))
	{
		gtk_text_iter_set_line_offset (&startc, 0);
	}

	if (!gtk_text_iter_ends_line (&endc))
	{
		gtk_text_iter_forward_to_line_end (&endc);
	}

	for (item = buffer->priv->scan_regions; item; item = g_list_next (item))
	{
		ScanRegion *region = item->data;
		GtkTextIter region_start;
		GtkTextIter region_end;

		gtk_text_buffer_get_iter_at_mark (buffer->priv->buffer,
		                                  &region_start,
		                                  region->start);

		gtk_text_buffer_get_iter_at_mark (buffer->priv->buffer,
		                                  &region_end,
		                                  region->end);

		if (gtk_text_iter_compare (&endc, &region_start) < 0)
		{
			/* Region is before */
			add_scan_region (buffer,
			                 g_list_previous (item),
			                 &startc,
			                 &endc,
			                 TRUE);
			return;
		}
		else if (gtk_text_iter_compare (&startc, &region_start) < 0)
		{
			GtkTextIter last;
			last = region_start;
			gtk_text_iter_backward_line (&last);

			/* Add region from before */
			add_scan_region (buffer,
			                 g_list_previous (item),
			                 &startc,
			                 &last,
			                 TRUE);

			startc = region_end;
			gtk_text_iter_forward_line (&startc);
		}
		else if (gtk_text_iter_compare (&startc, &region_end) < 0)
		{
			startc = region_end;

			if (!gtk_text_iter_forward_line (&startc))
			{
				return;
			}
		}

		if (gtk_text_iter_compare (&startc, &endc) > 0)
		{
			return;
		}

		if (!item->next)
		{
			add_scan_region (buffer,
			                 NULL,
			                 &startc,
			                 &endc,
			                 TRUE);
			return;
		}
	}

	add_scan_region (buffer,
	                 NULL,
	                 &startc,
	                 &endc,
	                 TRUE);
}

static void
on_insert_text_cb (GtkTextBuffer                  *textbuffer,
                   GtkTextIter                    *location,
                   const gchar                    *text,
                   gint                            len,
                   GtkSourceCompletionWordsBuffer *buffer)
{
	remove_and_rescan (buffer, location, NULL);
}

static void
on_delete_range_cb (GtkTextBuffer                  *text_buffer,
                    GtkTextIter                    *start,
                    GtkTextIter                    *end,
                    GtkSourceCompletionWordsBuffer *buffer)
{
	GtkTextIter start_buf;
	GtkTextIter end_buf;

	gtk_text_buffer_get_bounds (text_buffer,
	                            &start_buf,
	                            &end_buf);

	/* Special case removing all the text */
	if (gtk_text_iter_equal (start, &start_buf) &&
	    gtk_text_iter_equal (end, &end_buf))
	{
		remove_words (buffer);
		g_list_foreach (buffer->priv->scan_regions, (GFunc)scan_region_free, NULL);
		g_list_free (buffer->priv->scan_regions);
		buffer->priv->scan_regions = NULL;

		add_scan_region (buffer, NULL, start, end, FALSE);
	}
	else
	{
		/* Remove all the words in the lines from start-to-end */
		remove_and_rescan (buffer, start, end);
	}
}

static void
connect_buffer (GtkSourceCompletionWordsBuffer *buffer)
{
	GtkTextIter start;
	GtkTextIter end;

	buffer->priv->ext_signal_handlers[EXT_INSERT_TEXT] =
		g_signal_connect (buffer->priv->buffer,
		                  "insert-text",
		                  G_CALLBACK (on_insert_text_cb),
		                  buffer);

	buffer->priv->ext_signal_handlers[EXT_DELETE_RANGE] =
		g_signal_connect (buffer->priv->buffer,
		                  "delete-range",
		                  G_CALLBACK (on_delete_range_cb),
		                  buffer);

	gtk_text_buffer_get_bounds (buffer->priv->buffer,
	                            &start,
	                            &end);

	add_scan_region (buffer,
	                 NULL,
	                 &start,
	                 &end,
	                 FALSE);
}

static void
on_library_lock (GtkSourceCompletionWordsBuffer *buffer)
{
	if (buffer->priv->batch_scan_id != 0)
	{
		g_source_remove (buffer->priv->batch_scan_id);
		buffer->priv->batch_scan_id = 0;
	}
	else if (buffer->priv->initiate_scan_id != 0)
	{
		g_source_remove (buffer->priv->initiate_scan_id);
		buffer->priv->initiate_scan_id = 0;
	}
}

static void
on_library_unlock (GtkSourceCompletionWordsBuffer *buffer)
{
	if (buffer->priv->scan_regions != NULL)
	{
		install_initiate_scan (buffer);
	}
}

GtkSourceCompletionWordsBuffer *
gtk_source_completion_words_buffer_new (GtkSourceCompletionWordsLibrary *library,
                                        GtkTextBuffer                   *buffer)
{
	GtkSourceCompletionWordsBuffer *ret;
	GtkTextIter iter;

	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library), NULL);
	g_return_val_if_fail (GTK_IS_TEXT_BUFFER (buffer), NULL);

	ret = g_object_new (GTK_SOURCE_TYPE_COMPLETION_WORDS_BUFFER, NULL);

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
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_BUFFER (buffer), NULL);

	return buffer->priv->buffer;
}

void
gtk_source_completion_words_buffer_set_scan_batch_size (GtkSourceCompletionWordsBuffer *buffer,
                                                        guint                           size)
{
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_BUFFER (buffer));
	g_return_if_fail (size != 0);

	buffer->priv->scan_batch_size = size;
}

void
gtk_source_completion_words_buffer_set_minimum_word_size (GtkSourceCompletionWordsBuffer *buffer,
                                                          guint                           size)
{
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_BUFFER (buffer));
	g_return_if_fail (size != 0);

	buffer->priv->minimum_word_size = size;
}

GtkTextMark *
gtk_source_completion_words_buffer_get_mark (GtkSourceCompletionWordsBuffer *buffer)
{
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_BUFFER (buffer), NULL);

	return buffer->priv->mark;
}
