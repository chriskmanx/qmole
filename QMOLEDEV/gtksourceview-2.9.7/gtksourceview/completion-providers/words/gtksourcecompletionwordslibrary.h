/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletionwordslibrary.h
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

#ifndef __GTK_SOURCE_COMPLETION_WORDS_LIBRARY_H__
#define __GTK_SOURCE_COMPLETION_WORDS_LIBRARY_H__

#include <glib-object.h>
#include "gtksourcecompletionwordsproposal.h"

G_BEGIN_DECLS

#define GTK_TYPE_SOURCE_COMPLETION_WORDS_LIBRARY			(gtk_source_completion_words_library_get_type ())
#define GTK_SOURCE_COMPLETION_WORDS_LIBRARY(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SOURCE_COMPLETION_PROVIDER_WORDS_LIBRARY, GtkSourceCompletionWordsLibrary))
#define GTK_SOURCE_COMPLETION_WORDS_LIBRARY_CONST(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SOURCE_COMPLETION_WORDS_LIBRARY, GtkSourceCompletionWordsLibrary const))
#define GTK_SOURCE_COMPLETION_WORDS_LIBRARY_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_SOURCE_COMPLETION_WORDS_LIBRARY, GtkSourceCompletionWordsLibraryClass))
#define GTK_IS_SOURCE_COMPLETION_WORDS_LIBRARY(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_SOURCE_COMPLETION_WORDS_LIBRARY))
#define GTK_IS_SOURCE_COMPLETION_WORDS_LIBRARY_CLASS(klass)		(G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SOURCE_COMPLETION_WORDS_LIBRARY))
#define GTK_SOURCE_COMPLETION_WORDS_LIBRARY_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_SOURCE_COMPLETION_WORDS_LIBRARY, GtkSourceCompletionWordsLibraryClass))

typedef struct _GtkSourceCompletionWordsLibrary			GtkSourceCompletionWordsLibrary;
typedef struct _GtkSourceCompletionWordsLibraryClass		GtkSourceCompletionWordsLibraryClass;
typedef struct _GtkSourceCompletionWordsLibraryPrivate		GtkSourceCompletionWordsLibraryPrivate;

struct _GtkSourceCompletionWordsLibrary {
	GObject parent;

	GtkSourceCompletionWordsLibraryPrivate *priv;
};

struct _GtkSourceCompletionWordsLibraryClass {
	GObjectClass parent_class;
};

GType gtk_source_completion_words_library_get_type (void) G_GNUC_CONST;

GtkSourceCompletionWordsLibrary *
		gtk_source_completion_words_library_new			(void);

/* Finding */
GSequenceIter	*gtk_source_completion_words_library_find 		(GtkSourceCompletionWordsLibrary  *library,
									 GtkSourceCompletionWordsProposal *proposal);
GSequenceIter	*gtk_source_completion_words_library_find_first		(GtkSourceCompletionWordsLibrary  *library,
									 const gchar                      *word,
									 gint                              len);

GSequenceIter	*gtk_source_completion_words_library_find_next		(GSequenceIter                    *iter,
									 const gchar                      *word,
									 gint                              len);

/* Getting */
GtkSourceCompletionWordsProposal *
		 gtk_source_completion_words_library_get_proposal 	(GSequenceIter                    *iter);

/* Adding/removing */
GtkSourceCompletionWordsProposal *
		 gtk_source_completion_words_library_add_word 		(GtkSourceCompletionWordsLibrary  *library,
                                              				 const gchar                      *word);
void		 gtk_source_completion_words_library_remove_word 	(GtkSourceCompletionWordsLibrary  *library,
                                                 			 GtkSourceCompletionWordsProposal *proposal);

gboolean	 gtk_source_completion_words_library_is_locked 		(GtkSourceCompletionWordsLibrary *library);
void		 gtk_source_completion_words_library_lock 		(GtkSourceCompletionWordsLibrary *library);
void		 gtk_source_completion_words_library_unlock 		(GtkSourceCompletionWordsLibrary *library);

G_END_DECLS

#endif /* __GTK_SOURCE_COMPLETION_WORDS_LIBRARY_H__ */
