/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletionwordsbuffer.h
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

#ifndef __GTK_SOURCE_COMPLETION_WORDS_BUFFER_H__
#define __GTK_SOURCE_COMPLETION_WORDS_BUFFER_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gtksourcecompletionwordslibrary.h"

G_BEGIN_DECLS

#define GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER			(gtk_source_completion_words_buffer_get_type ())
#define GTK_SOURCE_COMPLETION_WORDS_BUFFER(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER, GtkSourceCompletionWordsBuffer))
#define GTK_SOURCE_COMPLETION_WORDS_BUFFER_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER, GtkSourceCompletionWordsBuffer const))
#define GTK_SOURCE_COMPLETION_WORDS_BUFFER_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER, GtkSourceCompletionWordsBufferClass))
#define GTK_IS_SOURCE_COMPLETION_WORDS_BUFFER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER))
#define GTK_IS_SOURCE_COMPLETION_WORDS_BUFFER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER))
#define GTK_SOURCE_COMPLETION_WORDS_BUFFER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_SOURCE_COMPLETION_WORDS_BUFFER, GtkSourceCompletionWordsBufferClass))

typedef struct _GtkSourceCompletionWordsBuffer			GtkSourceCompletionWordsBuffer;
typedef struct _GtkSourceCompletionWordsBufferClass		GtkSourceCompletionWordsBufferClass;
typedef struct _GtkSourceCompletionWordsBufferPrivate		GtkSourceCompletionWordsBufferPrivate;

struct _GtkSourceCompletionWordsBuffer {
	GObject parent;
	
	GtkSourceCompletionWordsBufferPrivate *priv;
};

struct _GtkSourceCompletionWordsBufferClass {
	GObjectClass parent_class;
};

GType gtk_source_completion_words_buffer_get_type (void) G_GNUC_CONST;

GtkSourceCompletionWordsBuffer *
		 gtk_source_completion_words_buffer_new 	(GtkSourceCompletionWordsLibrary *library,
								 GtkTextBuffer                   *buffer);

GtkTextBuffer 	*gtk_source_completion_words_buffer_get_buffer	(GtkSourceCompletionWordsBuffer  *buffer);

void		 gtk_source_completion_words_buffer_set_scan_batch_size (GtkSourceCompletionWordsBuffer *buffer,
                                                                         guint                           size);

void		 gtk_source_completion_words_buffer_set_minimum_word_size (GtkSourceCompletionWordsBuffer *buffer,
                                                                           guint                           size);

GtkTextMark     *gtk_source_completion_words_buffer_get_mark	(GtkSourceCompletionWordsBuffer *buffer);

G_END_DECLS

#endif /* __GTK_SOURCE_COMPLETION_WORDS_BUFFER_H__ */
