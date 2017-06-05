/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletionwordslibrary.c
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

#include "gtksourcecompletionwordslibrary.h"

#include <string.h>

#define GTK_SOURCE_COMPLETION_WORDS_LIBRARY_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_SOURCE_TYPE_COMPLETION_WORDS_LIBRARY, GtkSourceCompletionWordsLibraryPrivate))

enum
{
	LOCK,
	UNLOCK,
	NUM_SIGNALS
};

struct _GtkSourceCompletionWordsLibraryPrivate
{
	GSequence *store;
	gboolean locked;
};

static guint signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (GtkSourceCompletionWordsLibrary, gtk_source_completion_words_library, G_TYPE_OBJECT)

static void
gtk_source_completion_words_library_finalize (GObject *object)
{
	G_OBJECT_CLASS (gtk_source_completion_words_library_parent_class)->finalize (object);
}

static void
gtk_source_completion_words_library_class_init (GtkSourceCompletionWordsLibraryClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	
	object_class->finalize = gtk_source_completion_words_library_finalize;

	signals[LOCK] =
		g_signal_new ("lock",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL, 
		              NULL,
		              g_cclosure_marshal_VOID__VOID, 
		              G_TYPE_NONE,
		              0);
	
	signals[UNLOCK] =
		g_signal_new ("unlock",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL, 
		              NULL,
		              g_cclosure_marshal_VOID__VOID, 
		              G_TYPE_NONE,
		              0);

	g_type_class_add_private (object_class, sizeof(GtkSourceCompletionWordsLibraryPrivate));
}

static void
gtk_source_completion_words_library_init (GtkSourceCompletionWordsLibrary *self)
{
	self->priv = GTK_SOURCE_COMPLETION_WORDS_LIBRARY_GET_PRIVATE (self);
	
	self->priv->store = g_sequence_new ((GDestroyNotify)g_object_unref);
}

GtkSourceCompletionWordsLibrary *
gtk_source_completion_words_library_new (void)
{
	return g_object_new (GTK_SOURCE_TYPE_COMPLETION_WORDS_LIBRARY, NULL);
}

static gint
compare_two_items (GtkSourceCompletionWordsProposal *a,
                   GtkSourceCompletionWordsProposal *b,
                   gpointer                          data)
{
	return strcmp (gtk_source_completion_words_proposal_get_word (a),
	               gtk_source_completion_words_proposal_get_word (b));
}

static gint
compare_items (GtkSourceCompletionWordsProposal *a,
               GtkSourceCompletionWordsProposal *b,
               const gchar                      *word)
{
	const gchar *m1 = 
		gtk_source_completion_words_proposal_get_word (a == NULL ? b : a);
	
	return strcmp (m1, word);
}

static gboolean
iter_match_prefix (GSequenceIter *iter,
                   const gchar   *word,
                   gint           len)
{
	GtkSourceCompletionWordsProposal *item;
	
	item = gtk_source_completion_words_library_get_proposal (iter);
	
	return strncmp (gtk_source_completion_words_proposal_get_word (item),
	                word, 
	                len != -1 ? len : strlen (word)) == 0;
}

GtkSourceCompletionWordsProposal *
gtk_source_completion_words_library_get_proposal (GSequenceIter *iter)
{
	if (iter == NULL)
	{
		return NULL;
	}
	
	return GTK_SOURCE_COMPLETION_WORDS_PROPOSAL (g_sequence_get (iter));
}

GSequenceIter *
gtk_source_completion_words_library_find_first (GtkSourceCompletionWordsLibrary *library,
                                                const gchar                     *word,
                                                gint                             len)
{
	GSequenceIter *iter;
	GSequenceIter *prev;
	
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library), NULL);
	g_return_val_if_fail (word != NULL, NULL);

	iter = g_sequence_search (library->priv->store,
	                          NULL,
	                          (GCompareDataFunc)compare_items,
	                          (gpointer)word);

	if (iter == NULL)
	{
		return NULL;
	}
	
	if (len == -1)
	{
		len = strlen (word);
	}
	
	/* Test if this position might be after the last match */
	if (!g_sequence_iter_is_begin (iter) && 
	    (g_sequence_iter_is_end (iter) || 
	     !iter_match_prefix (iter, word, len)))
	{
		iter = g_sequence_iter_prev (iter);
	
		/* Maybe there is actually nothing in the sequence */
		if (g_sequence_iter_is_end (iter) || 
		    !iter_match_prefix (iter, word, len))
		{
			return NULL;
		}
	}
	
	if (g_sequence_iter_is_end (iter))
	{
		return NULL;
	}
	
	/* Go back while it matches */
	while (iter &&
	       (prev = g_sequence_iter_prev (iter)) && 
	       iter_match_prefix (prev, word, len))
	{
		iter = prev;
		
		if (g_sequence_iter_is_begin (iter))
		{
			break;
		}
	}
	
	return iter;
}

GSequenceIter *
gtk_source_completion_words_library_find_next (GSequenceIter *iter,
                                               const gchar   *word,
                                               gint           len)
{
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (word != NULL, NULL);

	iter = g_sequence_iter_next (iter);

	if (!iter || g_sequence_iter_is_end (iter))
	{
		return NULL;
	}

	return iter_match_prefix (iter, word, len) ? iter : NULL;
}

GSequenceIter *
gtk_source_completion_words_library_find (GtkSourceCompletionWordsLibrary  *library,
					  GtkSourceCompletionWordsProposal *proposal)
{
	GSequenceIter *iter;
	GtkSourceCompletionWordsProposal *other;
	const gchar *word = gtk_source_completion_words_proposal_get_word (proposal);
	gint len = strlen (word);
	
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library), NULL);
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_PROPOSAL (proposal), NULL);
	
	iter = gtk_source_completion_words_library_find_first (library, word, len);
	
	if (!iter)
	{
		return NULL;
	}
	
	do
	{
		other = gtk_source_completion_words_library_get_proposal (iter);
		
		if (proposal == other)
		{
			return iter;
		}
		
		iter = g_sequence_iter_next (iter);
	} while (!g_sequence_iter_is_end (iter) &&
	         strcmp (gtk_source_completion_words_proposal_get_word (other),
	                 word) == 0);

	return NULL;
}

static void
on_proposal_unused (GtkSourceCompletionWordsProposal *proposal,
                    GtkSourceCompletionWordsLibrary  *library)
{
	GSequenceIter *iter = gtk_source_completion_words_library_find (library, 
	                                                                proposal);
	
	if (iter != NULL)
	{
		g_sequence_remove (iter);
	}
}

GtkSourceCompletionWordsProposal *
gtk_source_completion_words_library_add_word (GtkSourceCompletionWordsLibrary *library,
                                              const gchar                     *word)
{
	GtkSourceCompletionWordsProposal *proposal;
	GSequenceIter *iter;
	
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library), NULL);
	g_return_val_if_fail (word != NULL, NULL);
	
	/* Check if word already exists */
	iter = gtk_source_completion_words_library_find_first (library, word, -1);
	
	if (iter)
	{
		proposal = gtk_source_completion_words_library_get_proposal (iter);
		
		if (strcmp (gtk_source_completion_words_proposal_get_word (proposal),
		            word) == 0)
		{
			/* Already exists, increase the use count */
			gtk_source_completion_words_proposal_use (proposal);
			return proposal;
		}
	}
	
	if (library->priv->locked)
	{
		return NULL;
	}

	proposal = gtk_source_completion_words_proposal_new (word);
	
	g_signal_connect (proposal,
	                  "unused",
	                  G_CALLBACK (on_proposal_unused),
	                  library);
	
	/* Insert proposal into binary tree of words */
	g_sequence_insert_sorted (library->priv->store,
	                          proposal,
	                          (GCompareDataFunc)compare_two_items,
	                          NULL);

	return proposal;
}

void
gtk_source_completion_words_library_remove_word (GtkSourceCompletionWordsLibrary  *library,
                                                 GtkSourceCompletionWordsProposal *proposal)
{	
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library));
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_PROPOSAL (proposal));

	gtk_source_completion_words_proposal_unuse (proposal);
}

void
gtk_source_completion_words_library_lock (GtkSourceCompletionWordsLibrary *library)
{
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library));
	
	library->priv->locked = TRUE;
	g_signal_emit (library, signals[LOCK], 0);
}

void
gtk_source_completion_words_library_unlock (GtkSourceCompletionWordsLibrary *library)
{
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library));
	
	library->priv->locked = FALSE;
	g_signal_emit (library, signals[UNLOCK], 0);
}

gboolean
gtk_source_completion_words_library_is_locked (GtkSourceCompletionWordsLibrary *library)
{
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_WORDS_LIBRARY (library), TRUE);
	
	return library->priv->locked;
}
