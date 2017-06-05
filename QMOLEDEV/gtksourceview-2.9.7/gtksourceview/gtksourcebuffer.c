/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/*  gtksourcebuffer.c
 *
 *  Copyright (C) 1999,2000,2001,2002 by:
 *          Mikael Hermansson <tyan@linux.se>
 *          Chris Phelps <chicane@reninet.com>
 *          Jeroen Zwartepoorte <jeroen@xs4all.nl>
 *
 *  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
 *          Gustavo Giráldez <gustavo.giraldez@gmx.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <gtk/gtk.h>

#include "gtksourceview-i18n.h"
#include "gtksourcelanguage-private.h"
#include "gtksourcebuffer.h"
#include "gtksourceundomanager.h"
#include "gtksourceview-marshal.h"
#include "gtksourceiter.h"
#include "gtksourcestyleschememanager.h"
#include "gtksourcestyle-private.h"
#include "gtksourceundomanagerdefault.h"

/**
 * SECTION:buffer
 * @Short_description: Buffer object for #GtkSourceView
 * @Title: GtkSourceBuffer
 * @See_also: #GtkTextBuffer,#GtkSourceView
 *
 * The #GtkSourceBuffer object is the model for #GtkSourceView widgets.
 * It extends the #GtkTextBuffer object by adding features useful to display
 * and edit source code as syntax highlighting and bracket matching. It
 * also implements support for undo/redo operations.
 *
 * To create a #GtkSourceBuffer use gtk_source_buffer_new() or
 * gtk_source_buffer_new_with_language(). The second form is just a convenience
 * function which allows you to initially set a #GtkSourceLanguage.
 *
 * By default highlighting is enabled, but you can disable it with
 * gtk_source_buffer_set_highlight_syntax().
 */

/*
#define ENABLE_DEBUG
#define ENABLE_PROFILE
*/
#undef ENABLE_DEBUG
#undef ENABLE_PROFILE

#ifdef ENABLE_DEBUG
#define DEBUG(x) (x)
#else
#define DEBUG(x)
#endif

#ifdef ENABLE_PROFILE
#define PROFILE(x) (x)
#else
#define PROFILE(x)
#endif

#define MAX_CHARS_BEFORE_FINDING_A_MATCH    10000

#define TAG_CONTEXT_CLASS_NAME "GtkSourceViewTagContextClassName"

/* Signals */
enum {
	HIGHLIGHT_UPDATED,
	SOURCE_MARK_UPDATED,
	UNDO,
	REDO,
	LAST_SIGNAL
};

/* Properties */
enum {
	PROP_0,
	PROP_CAN_UNDO,
	PROP_CAN_REDO,
	PROP_HIGHLIGHT_SYNTAX,
	PROP_HIGHLIGHT_MATCHING_BRACKETS,
	PROP_MAX_UNDO_LEVELS,
	PROP_LANGUAGE,
	PROP_STYLE_SCHEME,
	PROP_UNDO_MANAGER
};

struct _GtkSourceBufferPrivate
{
	gint                   highlight_syntax:1;
	gint                   highlight_brackets:1;

	gint                   constructed:1;

	GtkTextTag            *bracket_match_tag;
	GtkTextMark           *bracket_mark;
	guint                  bracket_found:1;

	GArray                *source_marks;

	GtkSourceLanguage     *language;

	GtkSourceEngine       *highlight_engine;
	GtkSourceStyleScheme  *style_scheme;

	GtkSourceUndoManager  *undo_manager;
	gint                   max_undo_levels;
};

G_DEFINE_TYPE (GtkSourceBuffer, gtk_source_buffer, GTK_TYPE_TEXT_BUFFER)

static guint 	 buffer_signals[LAST_SIGNAL];

static GObject	*gtk_source_buffer_constructor		(GType                    type,
							 guint                    n_construct_properties,
							 GObjectConstructParam   *construct_param);
static void 	 gtk_source_buffer_finalize		(GObject                 *object);
static void 	 gtk_source_buffer_dispose		(GObject                 *object);
static void      gtk_source_buffer_set_property         (GObject                 *object,
							 guint                    prop_id,
							 const GValue            *value,
							 GParamSpec              *pspec);
static void      gtk_source_buffer_get_property         (GObject                 *object,
							 guint                    prop_id,
							 GValue                  *value,
							 GParamSpec              *pspec);
static void 	 gtk_source_buffer_can_undo_handler 	(GtkSourceUndoManager    *manager,
							 GtkSourceBuffer         *buffer);
static void 	 gtk_source_buffer_can_redo_handler	(GtkSourceUndoManager    *manager,
							 GtkSourceBuffer         *buffer);
static void 	 gtk_source_buffer_real_insert_text 	(GtkTextBuffer           *buffer,
							 GtkTextIter             *iter,
							 const gchar             *text,
							 gint                     len);
static void	 gtk_source_buffer_real_insert_pixbuf	(GtkTextBuffer           *buffer,
							 GtkTextIter             *pos,
							 GdkPixbuf               *pixbuf);
static void	 gtk_source_buffer_real_insert_anchor	(GtkTextBuffer           *buffer,
							 GtkTextIter             *pos,
							 GtkTextChildAnchor      *anchor);
static void 	 gtk_source_buffer_real_delete_range 	(GtkTextBuffer           *buffer,
							 GtkTextIter             *iter,
							 GtkTextIter             *end);
static void 	 gtk_source_buffer_real_mark_set	(GtkTextBuffer		 *buffer,
							 const GtkTextIter	 *location,
							 GtkTextMark		 *mark);
static void 	 gtk_source_buffer_real_mark_deleted	(GtkTextBuffer		 *buffer,
							 GtkTextMark		 *mark);
static gboolean	 gtk_source_buffer_find_bracket_match_with_limit (GtkSourceBuffer *buffer,
								  GtkTextIter     *orig,
								  gint             max_chars);

static void	 gtk_source_buffer_real_undo		(GtkSourceBuffer	 *buffer);
static void	 gtk_source_buffer_real_redo		(GtkSourceBuffer	 *buffer);

static void
gtk_source_buffer_class_init (GtkSourceBufferClass *klass)
{
	GObjectClass        *object_class;
	GtkTextBufferClass  *tb_class;
	GType                param_types[2];

	object_class 	= G_OBJECT_CLASS (klass);
	tb_class	= GTK_TEXT_BUFFER_CLASS (klass);

	object_class->constructor  = gtk_source_buffer_constructor;
	object_class->finalize	   = gtk_source_buffer_finalize;
	object_class->dispose	   = gtk_source_buffer_dispose;
	object_class->get_property = gtk_source_buffer_get_property;
	object_class->set_property = gtk_source_buffer_set_property;

	tb_class->delete_range        = gtk_source_buffer_real_delete_range;
	tb_class->insert_text 	      = gtk_source_buffer_real_insert_text;
	tb_class->insert_pixbuf       = gtk_source_buffer_real_insert_pixbuf;
	tb_class->insert_child_anchor = gtk_source_buffer_real_insert_anchor;

	tb_class->mark_set	= gtk_source_buffer_real_mark_set;
	tb_class->mark_deleted	= gtk_source_buffer_real_mark_deleted;

	klass->undo = gtk_source_buffer_real_undo;
	klass->redo = gtk_source_buffer_real_redo;

	/**
	 * GtkSourceBuffer:highlight-syntax:
	 *
	 * Whether to highlight syntax in the buffer.
	 */
	g_object_class_install_property (object_class,
					 PROP_HIGHLIGHT_SYNTAX,
					 g_param_spec_boolean ("highlight-syntax",
							       _("Highlight Syntax"),
							       _("Whether to highlight syntax "
								 "in the buffer"),
							       TRUE,
							       G_PARAM_READWRITE));

	/**
	 * GtkSourceBuffer:highlight-matching-brackets:
	 *
	 * Whether to highlight matching brackets in the buffer.
	 */
	g_object_class_install_property (object_class,
					 PROP_HIGHLIGHT_MATCHING_BRACKETS,
					 g_param_spec_boolean ("highlight-matching-brackets",
							       _("Highlight Matching Brackets"),
							       _("Whether to highlight matching brackets"),
							       TRUE,
							       G_PARAM_READWRITE));

	/**
	 * GtkSourceBuffer:max-undo-levels:
	 *
	 * Number of undo levels for the buffer. -1 means no limit. This property
	 * will only affect the default undo manager.
	 */
	g_object_class_install_property (object_class,
					 PROP_MAX_UNDO_LEVELS,
					 g_param_spec_int ("max-undo-levels",
							   _("Maximum Undo Levels"),
							   _("Number of undo levels for "
							     "the buffer"),
							   -1,
							   G_MAXINT,
							   1000,
							   G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_LANGUAGE,
					 g_param_spec_object ("language",
							      /* Translators: throughout gtksourceview "language" stands
							       * for "programming language", not "spoken language" */
							      _("Language"),
							      _("Language object to get "
								"highlighting patterns from"),
							      GTK_TYPE_SOURCE_LANGUAGE,
							      G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_CAN_UNDO,
					 g_param_spec_boolean ("can-undo",
							       _("Can undo"),
							       _("Whether Undo operation is possible"),
							       FALSE,
							       G_PARAM_READABLE));

	g_object_class_install_property (object_class,
					 PROP_CAN_REDO,
					 g_param_spec_boolean ("can-redo",
							       _("Can redo"),
							       _("Whether Redo operation is possible"),
							       FALSE,
							       G_PARAM_READABLE));

	/**
	 * GtkSourceBuffer:style-scheme:
	 *
	 * Style scheme. It contains styles for syntax highlighting, optionally
	 * foreground, background, cursor color, current line color, and matching
	 * brackets style.
	 */
	g_object_class_install_property (object_class,
					 PROP_STYLE_SCHEME,
					 g_param_spec_object ("style_scheme",
							      _("Style scheme"),
							      _("Style scheme"),
							      GTK_TYPE_SOURCE_STYLE_SCHEME,
							      G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
	                                 PROP_UNDO_MANAGER,
	                                 g_param_spec_object ("undo-manager",
	                                                      _("Undo manager"),
	                                                      _("The buffer undo manager"),
	                                                      GTK_TYPE_SOURCE_UNDO_MANAGER,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	param_types[0] = GTK_TYPE_TEXT_ITER | G_SIGNAL_TYPE_STATIC_SCOPE;
	param_types[1] = GTK_TYPE_TEXT_ITER | G_SIGNAL_TYPE_STATIC_SCOPE;

	buffer_signals[HIGHLIGHT_UPDATED] =
	    g_signal_newv ("highlight_updated",
			   G_OBJECT_CLASS_TYPE (object_class),
			   G_SIGNAL_RUN_LAST,
			   NULL,
			   NULL, NULL,
			   _gtksourceview_marshal_VOID__BOXED_BOXED,
			   G_TYPE_NONE,
			   2, param_types);
	/**
	 * GtkSourceBuffer::source-mark-updated
	 * @buffer: the buffer that received the signal
	 *
	 * The ::source_mark_updated signal is emitted each time
	 * a mark is added to, moved or removed from the @buffer.
	 **/
	buffer_signals[SOURCE_MARK_UPDATED] =
	    g_signal_new ("source_mark_updated",
			   G_OBJECT_CLASS_TYPE (object_class),
			   G_SIGNAL_RUN_LAST,
			   0,
			   NULL, NULL,
			   g_cclosure_marshal_VOID__OBJECT,
			   G_TYPE_NONE,
			   1, GTK_TYPE_TEXT_MARK);

	buffer_signals[UNDO] =
	    g_signal_new ("undo",
			  G_OBJECT_CLASS_TYPE (object_class),
			  G_SIGNAL_RUN_LAST,
			  G_STRUCT_OFFSET (GtkSourceBufferClass, undo),
			  NULL, NULL,
			  g_cclosure_marshal_VOID__VOID,
			  G_TYPE_NONE,
			  0);

	buffer_signals[REDO] =
	    g_signal_new ("redo",
			  G_OBJECT_CLASS_TYPE (object_class),
			  G_SIGNAL_RUN_LAST,
			  G_STRUCT_OFFSET (GtkSourceBufferClass, redo),
			  NULL, NULL,
			  g_cclosure_marshal_VOID__VOID,
			  G_TYPE_NONE,
			  0);

	g_type_class_add_private (object_class, sizeof(GtkSourceBufferPrivate));
}

static void
set_undo_manager (GtkSourceBuffer      *buffer,
                  GtkSourceUndoManager *manager)
{
	if (manager == buffer->priv->undo_manager)
	{
		return;
	}

	if (buffer->priv->undo_manager != NULL)
	{
		g_signal_handlers_disconnect_by_func (buffer->priv->undo_manager,
		                                      G_CALLBACK (gtk_source_buffer_can_undo_handler),
		                                      buffer);

		g_signal_handlers_disconnect_by_func (buffer->priv->undo_manager,
		                                      G_CALLBACK (gtk_source_buffer_can_redo_handler),
		                                      buffer);

		g_object_unref (buffer->priv->undo_manager);
		buffer->priv->undo_manager = NULL;
	}

	if (manager != NULL)
	{
		buffer->priv->undo_manager = g_object_ref (manager);

		g_signal_connect (buffer->priv->undo_manager,
		                  "can-undo-changed",
		                  G_CALLBACK (gtk_source_buffer_can_undo_handler),
		                  buffer);

		g_signal_connect (buffer->priv->undo_manager,
		                  "can-redo-changed",
		                  G_CALLBACK (gtk_source_buffer_can_redo_handler),
		                  buffer);

		/* Notify possible changes in the can-undo/redo state */
		g_object_notify (G_OBJECT (buffer), "can-undo");
		g_object_notify (G_OBJECT (buffer), "can-redo");
	}
}

static void
gtk_source_buffer_init (GtkSourceBuffer *buffer)
{
	GtkSourceBufferPrivate *priv;

	priv = G_TYPE_INSTANCE_GET_PRIVATE (buffer, GTK_TYPE_SOURCE_BUFFER,
					    GtkSourceBufferPrivate);

	buffer->priv = priv;

	priv->highlight_syntax = TRUE;
	priv->highlight_brackets = TRUE;
	priv->bracket_mark = NULL;
	priv->bracket_found = FALSE;

	priv->source_marks = g_array_new (FALSE, FALSE, sizeof (GtkSourceMark *));
	priv->style_scheme = _gtk_source_style_scheme_get_default ();

	if (priv->style_scheme != NULL)
		g_object_ref (priv->style_scheme);
}

static GObject *
gtk_source_buffer_constructor (GType                  type,
			       guint                  n_construct_properties,
			       GObjectConstructParam *construct_param)
{
	GObject *object;
	GtkSourceBuffer *buffer;

	object = G_OBJECT_CLASS(gtk_source_buffer_parent_class)->constructor (type,
									      n_construct_properties,
									      construct_param);

	/* we need to know that the tag-table was set */
	buffer = GTK_SOURCE_BUFFER (object);
	buffer->priv->constructed = TRUE;

	if (buffer->priv->undo_manager == NULL)
	{
		/* This will install the default undo manager */
		gtk_source_buffer_set_undo_manager (buffer, NULL);
	}

	return object;
}

static void
gtk_source_buffer_finalize (GObject *object)
{
	GtkSourceBuffer *buffer;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (object));

	buffer = GTK_SOURCE_BUFFER (object);
	g_return_if_fail (buffer->priv != NULL);

	if (buffer->priv->source_marks)
		g_array_free (buffer->priv->source_marks, TRUE);

	G_OBJECT_CLASS (gtk_source_buffer_parent_class)->finalize (object);
}

static void
gtk_source_buffer_dispose (GObject *object)
{
	GtkSourceBuffer *buffer;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (object));

	buffer = GTK_SOURCE_BUFFER (object);
	g_return_if_fail (buffer->priv != NULL);

	if (buffer->priv->undo_manager != NULL)
	{
		set_undo_manager (buffer, NULL);
	}

	if (buffer->priv->highlight_engine != NULL)
	{
		_gtk_source_engine_attach_buffer (buffer->priv->highlight_engine, NULL);
		g_object_unref (buffer->priv->highlight_engine);
		buffer->priv->highlight_engine = NULL;
	}

	if (buffer->priv->language != NULL)
	{
		g_object_unref (buffer->priv->language);
		buffer->priv->language = NULL;
	}

	if (buffer->priv->style_scheme != NULL)
	{
		g_object_unref (buffer->priv->style_scheme);
		buffer->priv->style_scheme = NULL;
	}

	G_OBJECT_CLASS (gtk_source_buffer_parent_class)->dispose (object);
}

static void
gtk_source_buffer_set_property (GObject      *object,
				guint         prop_id,
				const GValue *value,
				GParamSpec   *pspec)
{
	GtkSourceBuffer *source_buffer;

	g_return_if_fail (GTK_IS_SOURCE_BUFFER (object));

	source_buffer = GTK_SOURCE_BUFFER (object);

	switch (prop_id)
	{
		case PROP_HIGHLIGHT_SYNTAX:
			gtk_source_buffer_set_highlight_syntax (source_buffer,
							      g_value_get_boolean (value));
			break;

		case PROP_HIGHLIGHT_MATCHING_BRACKETS:
			gtk_source_buffer_set_highlight_matching_brackets (source_buffer,
								g_value_get_boolean (value));
			break;

		case PROP_MAX_UNDO_LEVELS:
			gtk_source_buffer_set_max_undo_levels (source_buffer,
							       g_value_get_int (value));
			break;

		case PROP_LANGUAGE:
			gtk_source_buffer_set_language (source_buffer,
							g_value_get_object (value));
			break;

		case PROP_STYLE_SCHEME:
			gtk_source_buffer_set_style_scheme (source_buffer,
							    g_value_get_object (value));
			break;

		case PROP_UNDO_MANAGER:
			gtk_source_buffer_set_undo_manager (source_buffer,
			                                    g_value_get_object (value));
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_buffer_get_property (GObject    *object,
				guint       prop_id,
				GValue     *value,
				GParamSpec *pspec)
{
	GtkSourceBuffer *source_buffer;

	g_return_if_fail (GTK_IS_SOURCE_BUFFER (object));

	source_buffer = GTK_SOURCE_BUFFER (object);

	switch (prop_id)
	{
		case PROP_HIGHLIGHT_SYNTAX:
			g_value_set_boolean (value,
					     source_buffer->priv->highlight_syntax);
			break;

		case PROP_HIGHLIGHT_MATCHING_BRACKETS:
			g_value_set_boolean (value,
					     source_buffer->priv->highlight_brackets);
			break;

		case PROP_MAX_UNDO_LEVELS:
			g_value_set_int (value,
					 source_buffer->priv->max_undo_levels);
			break;

		case PROP_LANGUAGE:
			g_value_set_object (value, source_buffer->priv->language);
			break;

		case PROP_STYLE_SCHEME:
			g_value_set_object (value, source_buffer->priv->style_scheme);
			break;

		case PROP_CAN_UNDO:
			g_value_set_boolean (value, gtk_source_buffer_can_undo (source_buffer));
			break;

		case PROP_CAN_REDO:
			g_value_set_boolean (value, gtk_source_buffer_can_redo (source_buffer));
			break;

		case PROP_UNDO_MANAGER:
			g_value_set_object (value, source_buffer->priv->undo_manager);
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

/**
 * gtk_source_buffer_new:
 * @table: a #GtkTextTagTable, or %NULL to create a new one.
 *
 * Creates a new source buffer.
 *
 * Return value: a new source buffer.
 **/
GtkSourceBuffer *
gtk_source_buffer_new (GtkTextTagTable *table)
{
	return g_object_new (GTK_TYPE_SOURCE_BUFFER,
			     "tag-table", table,
			     NULL);
}

/**
 * gtk_source_buffer_new_with_language:
 * @language: a #GtkSourceLanguage.
 *
 * Creates a new source buffer using the highlighting patterns in
 * @language.  This is equivalent to creating a new source buffer with
 * a new tag table and then calling gtk_source_buffer_set_language().
 *
 * Return value: a new source buffer which will highlight text
 * according to the highlighting patterns in @language.
 **/
GtkSourceBuffer *
gtk_source_buffer_new_with_language (GtkSourceLanguage *language)
{
	GtkSourceBuffer *buffer;

	g_return_val_if_fail (GTK_IS_SOURCE_LANGUAGE (language), NULL);

	buffer = gtk_source_buffer_new (NULL);

	gtk_source_buffer_set_language (buffer, language);

	return buffer;
}

static void
gtk_source_buffer_can_undo_handler (GtkSourceUndoManager *manager,
                                    GtkSourceBuffer      *buffer)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	g_object_notify (G_OBJECT (buffer), "can-undo");
}

static void
gtk_source_buffer_can_redo_handler (GtkSourceUndoManager *manager,
                                    GtkSourceBuffer      *buffer)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	g_object_notify (G_OBJECT (buffer), "can-redo");
}

static void
update_bracket_match_style (GtkSourceBuffer *buffer)
{
	if (buffer->priv->bracket_match_tag != NULL)
	{
		GtkSourceStyle *style = NULL;

		if (buffer->priv->style_scheme)
			style = _gtk_source_style_scheme_get_matching_brackets_style (buffer->priv->style_scheme);

		_gtk_source_style_apply (style, buffer->priv->bracket_match_tag);
	}
}

static GtkTextTag *
get_bracket_match_tag (GtkSourceBuffer *buffer)
{
	if (buffer->priv->bracket_match_tag == NULL)
	{
		buffer->priv->bracket_match_tag =
			gtk_text_buffer_create_tag (GTK_TEXT_BUFFER (buffer),
						    NULL,
						    NULL);
		update_bracket_match_style (buffer);
	}

	return buffer->priv->bracket_match_tag;
}

/*
 * This is private, just used by the compositor to not print bracket
 * matches. Note that unlike get_bracket_match_tag() it returns NULL
 * if the tag is not set.
 */
GtkTextTag *
_gtk_source_buffer_get_bracket_match_tag (GtkSourceBuffer *buffer)
{
	return buffer->priv->bracket_match_tag;
}

static void
gtk_source_buffer_move_cursor (GtkTextBuffer     *buffer,
			       const GtkTextIter *iter,
			       GtkTextMark       *mark)
{
	GtkTextIter iter1, iter2;

	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (mark != NULL);
	g_return_if_fail (gtk_text_iter_get_buffer (iter) == buffer);

	if (mark != gtk_text_buffer_get_insert (buffer))
		return;

	if (GTK_SOURCE_BUFFER (buffer)->priv->bracket_found)
	{
		gtk_text_buffer_get_iter_at_mark (buffer,
						  &iter1,
						  GTK_SOURCE_BUFFER (buffer)->priv->bracket_mark);
		iter2 = iter1;
		gtk_text_iter_forward_char (&iter2);
		gtk_text_buffer_remove_tag (buffer,
					    GTK_SOURCE_BUFFER (buffer)->priv->bracket_match_tag,
					    &iter1,
					    &iter2);
	}

	if (!GTK_SOURCE_BUFFER (buffer)->priv->highlight_brackets)
		return;

	iter1 = *iter;
	if (gtk_source_buffer_find_bracket_match_with_limit (GTK_SOURCE_BUFFER (buffer),
	                                                     &iter1,
	                                                     MAX_CHARS_BEFORE_FINDING_A_MATCH))
	{
		if (!GTK_SOURCE_BUFFER (buffer)->priv->bracket_mark)
			GTK_SOURCE_BUFFER (buffer)->priv->bracket_mark =
				gtk_text_buffer_create_mark (buffer,
							     NULL,
							     &iter1,
							     FALSE);
		else
			gtk_text_buffer_move_mark (buffer,
						   GTK_SOURCE_BUFFER (buffer)->priv->bracket_mark,
						   &iter1);

		iter2 = iter1;
		gtk_text_iter_forward_char (&iter2);
		gtk_text_buffer_apply_tag (buffer,
					   get_bracket_match_tag (GTK_SOURCE_BUFFER (buffer)),
					   &iter1,
					   &iter2);
		GTK_SOURCE_BUFFER (buffer)->priv->bracket_found = TRUE;
	}
	else
	{
		GTK_SOURCE_BUFFER (buffer)->priv->bracket_found = FALSE;
	}
}

static void
gtk_source_buffer_content_inserted (GtkTextBuffer *buffer,
				    gint           start_offset,
				    gint           end_offset)
{
	GtkTextMark *mark;
	GtkTextIter insert_iter;
	GtkSourceBuffer *source_buffer = GTK_SOURCE_BUFFER (buffer);

	mark = gtk_text_buffer_get_insert (buffer);
	gtk_text_buffer_get_iter_at_mark (buffer, &insert_iter, mark);
	gtk_source_buffer_move_cursor (buffer, &insert_iter, mark);

	if (source_buffer->priv->highlight_engine != NULL)
		_gtk_source_engine_text_inserted (source_buffer->priv->highlight_engine,
						  start_offset,
						  end_offset);
}

static void
gtk_source_buffer_real_insert_text (GtkTextBuffer *buffer,
				    GtkTextIter   *iter,
				    const gchar   *text,
				    gint           len)
{
	gint start_offset;

	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (text != NULL);
	g_return_if_fail (gtk_text_iter_get_buffer (iter) == buffer);

	start_offset = gtk_text_iter_get_offset (iter);

	/*
	 * iter is invalidated when
	 * insertion occurs (because the buffer contents change), but the
	 * default signal handler revalidates it to point to the end of the
	 * inserted text
	 */
	GTK_TEXT_BUFFER_CLASS (gtk_source_buffer_parent_class)->insert_text (buffer, iter, text, len);

	gtk_source_buffer_content_inserted (buffer,
					    start_offset,
					    gtk_text_iter_get_offset (iter));
}

/* insert_pixbuf and insert_child_anchor do nothing except notifying
 * the highlighting engine about the change, because engine's idea
 * of buffer char count must be correct at all times */
static void
gtk_source_buffer_real_insert_pixbuf (GtkTextBuffer *buffer,
				      GtkTextIter   *iter,
				      GdkPixbuf     *pixbuf)
{
	gint start_offset;

	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (gtk_text_iter_get_buffer (iter) == buffer);

	start_offset = gtk_text_iter_get_offset (iter);

	/*
	 * iter is invalidated when
	 * insertion occurs (because the buffer contents change), but the
	 * default signal handler revalidates it to point to the end of the
	 * inserted text
	 */
	GTK_TEXT_BUFFER_CLASS (gtk_source_buffer_parent_class)->insert_pixbuf (buffer, iter, pixbuf);

	gtk_source_buffer_content_inserted (buffer,
					    start_offset,
					    gtk_text_iter_get_offset (iter));
}

static void
gtk_source_buffer_real_insert_anchor (GtkTextBuffer      *buffer,
				      GtkTextIter        *iter,
				      GtkTextChildAnchor *anchor)
{
	gint start_offset;

	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (gtk_text_iter_get_buffer (iter) == buffer);

	start_offset = gtk_text_iter_get_offset (iter);

	/*
	 * iter is invalidated when
	 * insertion occurs (because the buffer contents change), but the
	 * default signal handler revalidates it to point to the end of the
	 * inserted text
	 */
	GTK_TEXT_BUFFER_CLASS (gtk_source_buffer_parent_class)->insert_child_anchor (buffer, iter, anchor);

	gtk_source_buffer_content_inserted (buffer,
					    start_offset,
					    gtk_text_iter_get_offset (iter));
}

static void
gtk_source_buffer_real_delete_range (GtkTextBuffer *buffer,
				     GtkTextIter   *start,
				     GtkTextIter   *end)
{
	gint offset, length;
	GtkTextMark *mark;
	GtkTextIter iter;
	GtkSourceBuffer *source_buffer = GTK_SOURCE_BUFFER (buffer);

	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (start != NULL);
	g_return_if_fail (end != NULL);
	g_return_if_fail (gtk_text_iter_get_buffer (start) == buffer);
	g_return_if_fail (gtk_text_iter_get_buffer (end) == buffer);

	gtk_text_iter_order (start, end);
	offset = gtk_text_iter_get_offset (start);
	length = gtk_text_iter_get_offset (end) - offset;

	GTK_TEXT_BUFFER_CLASS (gtk_source_buffer_parent_class)->delete_range (buffer, start, end);

	mark = gtk_text_buffer_get_insert (buffer);
	gtk_text_buffer_get_iter_at_mark (buffer, &iter, mark);
	gtk_source_buffer_move_cursor (buffer, &iter, mark);

	/* emit text deleted for engines */
	if (source_buffer->priv->highlight_engine != NULL)
		_gtk_source_engine_text_deleted (source_buffer->priv->highlight_engine,
						 offset, length);
}

/* This describes a mask of relevant context classes for highlighting matching
   brackets. Additional classes can be added below */
static const gchar *cclass_mask_definitions[] = {
	"comment",
	"string",
};

static gint
get_context_class_mask (GtkSourceBuffer *buffer,
                        GtkTextIter     *iter)
{
	gint i;
	gint ret = 0;

	for (i = 0; i < sizeof (cclass_mask_definitions) / sizeof (gchar *); ++i)
	{
		gboolean hasclass = gtk_source_buffer_iter_has_context_class (buffer,
		                                                              iter,
		                                                              cclass_mask_definitions[i]);

		ret |= hasclass << i;
	}

	return ret;
}

static gboolean
gtk_source_buffer_find_bracket_match_real (GtkSourceBuffer *buffer,
                                           GtkTextIter     *orig,
                                           gint             max_chars)
{
	GtkTextIter iter;

	gunichar base_char;
	gunichar search_char;
	gunichar cur_char;
	gint addition;
	gint char_cont;
	gint counter;

	gboolean found;

	gint cclass_mask;

	iter = *orig;

	cur_char = gtk_text_iter_get_char (&iter);

	base_char = search_char = cur_char;
	cclass_mask = get_context_class_mask (buffer, &iter);

	switch ((int) base_char) {
		case '{':
			addition = 1;
			search_char = '}';
			break;
		case '(':
			addition = 1;
			search_char = ')';
			break;
		case '[':
			addition = 1;
			search_char = ']';
			break;
		case '<':
			addition = 1;
			search_char = '>';
			break;
		case '}':
			addition = -1;
			search_char = '{';
			break;
		case ')':
			addition = -1;
			search_char = '(';
			break;
		case ']':
			addition = -1;
			search_char = '[';
			break;
		case '>':
			addition = -1;
			search_char = '<';
			break;
		default:
			addition = 0;
			break;
	}

	if (addition == 0)
		return FALSE;

	counter = 0;
	found = FALSE;
	char_cont = 0;

	do {
		gint current_mask;

		gtk_text_iter_forward_chars (&iter, addition);
		cur_char = gtk_text_iter_get_char (&iter);
		++char_cont;

		current_mask = get_context_class_mask (buffer, &iter);

		/* Check if we lost a class, which means we don't look any
		   further */
		if (current_mask < cclass_mask)
		{
			found = FALSE;
			break;
		}

		if ((cur_char == search_char || cur_char == base_char) &&
		    cclass_mask == current_mask)
		{
			if ((cur_char == search_char) && counter == 0) {
				found = TRUE;
				break;
			}
			if (cur_char == base_char)
				counter++;
			else
				counter--;
		}
	}
	while (!gtk_text_iter_is_end (&iter) && !gtk_text_iter_is_start (&iter) &&
		((char_cont < max_chars) || (max_chars < 0)));

	if (found)
		*orig = iter;

	return found;
}

/* Note that we take into account both the character following the cursor and the
 * one preceding it. If there are brackets on both sides the one following the
 * cursor takes precedence.
 */
static gboolean
gtk_source_buffer_find_bracket_match_with_limit (GtkSourceBuffer *buffer,
                                                 GtkTextIter     *orig,
                                                 gint             max_chars)
{
	GtkTextIter iter;

	if (gtk_source_buffer_find_bracket_match_real (buffer, orig, max_chars))
	{
		return TRUE;
	}

	iter = *orig;
	if (!gtk_text_iter_starts_line (&iter) &&
	    gtk_text_iter_backward_char (&iter))
	{
		if (gtk_source_buffer_find_bracket_match_real (buffer, &iter, max_chars))
		{
			*orig = iter;
			return TRUE;
		}
	}

	return FALSE;
}

#if 0
/**
 * gtk_source_iter_find_matching_bracket:
 * @iter: a #GtkTextIter.
 *
 * Tries to match the bracket character currently at @iter with its
 * opening/closing counterpart, and if found moves @iter to the position
 * where it was found.
 *
 * @iter must be a #GtkTextIter belonging to a #GtkSourceBuffer.
 *
 * Return value: %TRUE if the matching bracket was found and the @iter
 * iter moved.
 **/
gboolean
gtk_source_iter_find_matching_bracket (GtkTextIter *iter)
{
	g_return_val_if_fail (iter != NULL, FALSE);

	return gtk_source_buffer_find_bracket_match_with_limit (iter, -1);
}
#endif

/**
 * gtk_source_buffer_can_undo:
 * @buffer: a #GtkSourceBuffer.
 *
 * Determines whether a source buffer can undo the last action.
 *
 * Return value: %TRUE if it's possible to undo the last action.
 **/
gboolean
gtk_source_buffer_can_undo (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);

	return gtk_source_undo_manager_can_undo (buffer->priv->undo_manager);
}

/**
 * gtk_source_buffer_can_redo:
 * @buffer: a #GtkSourceBuffer.
 *
 * Determines whether a source buffer can redo the last action
 * (i.e. if the last operation was an undo).
 *
 * Return value: %TRUE if a redo is possible.
 **/
gboolean
gtk_source_buffer_can_redo (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);

	return gtk_source_undo_manager_can_redo (buffer->priv->undo_manager);
}

/**
 * gtk_source_buffer_undo:
 * @buffer: a #GtkSourceBuffer.
 *
 * Undoes the last user action which modified the buffer.  Use
 * gtk_source_buffer_can_undo() to check whether a call to this
 * function will have any effect.
 *
 * Actions are defined as groups of operations between a call to
 * gtk_text_buffer_begin_user_action() and
 * gtk_text_buffer_end_user_action(), or sequences of similar edits
 * (inserts or deletes) on the same line.
 **/
void
gtk_source_buffer_undo (GtkSourceBuffer *buffer)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	g_signal_emit (buffer, buffer_signals[UNDO], 0);
}

/**
 * gtk_source_buffer_redo:
 * @buffer: a #GtkSourceBuffer.
 *
 * Redoes the last undo operation.  Use gtk_source_buffer_can_redo()
 * to check whether a call to this function will have any effect.
 **/
void
gtk_source_buffer_redo (GtkSourceBuffer *buffer)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	g_signal_emit (buffer, buffer_signals[REDO], 0);
}

/**
 * gtk_source_buffer_get_max_undo_levels:
 * @buffer: a #GtkSourceBuffer.
 *
 * Determines the number of undo levels the buffer will track for
 * buffer edits.
 *
 * Return value: the maximum number of possible undo levels or
 *               -1 if no limit is set.
 **/
gint
gtk_source_buffer_get_max_undo_levels (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), 0);

	return buffer->priv->max_undo_levels;
}

/**
 * gtk_source_buffer_set_max_undo_levels:
 * @buffer: a #GtkSourceBuffer.
 * @max_undo_levels: the desired maximum number of undo levels.
 *
 * Sets the number of undo levels for user actions the buffer will
 * track.  If the number of user actions exceeds the limit set by this
 * function, older actions will be discarded.
 *
 * If @max_undo_levels is -1, no limit is set.
 *
 * A new action is started whenever the function
 * gtk_text_buffer_begin_user_action() is called.  In general, this
 * happens whenever the user presses any key which modifies the
 * buffer, but the undo manager will try to merge similar consecutive
 * actions, such as multiple character insertions into one action.
 * But, inserting a newline does start a new action.
 **/
void
gtk_source_buffer_set_max_undo_levels (GtkSourceBuffer *buffer,
				       gint             max_undo_levels)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	if (buffer->priv->max_undo_levels == max_undo_levels)
	{
		return;
	}

	buffer->priv->max_undo_levels = max_undo_levels;

	if (GTK_IS_SOURCE_UNDO_MANAGER_DEFAULT (buffer->priv->undo_manager))
	{
		gtk_source_undo_manager_default_set_max_undo_levels (GTK_SOURCE_UNDO_MANAGER_DEFAULT (buffer->priv->undo_manager),
		                                                     max_undo_levels);
	}

	g_object_notify (G_OBJECT (buffer), "max-undo-levels");
}

/**
 * gtk_source_buffer_begin_not_undoable_action:
 * @buffer: a #GtkSourceBuffer.
 *
 * Marks the beginning of a not undoable action on the buffer,
 * disabling the undo manager.  Typically you would call this function
 * before initially setting the contents of the buffer (e.g. when
 * loading a file in a text editor).
 *
 * You may nest gtk_source_buffer_begin_not_undoable_action() /
 * gtk_source_buffer_end_not_undoable_action() blocks.
 **/
void
gtk_source_buffer_begin_not_undoable_action (GtkSourceBuffer *buffer)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	gtk_source_undo_manager_begin_not_undoable_action (buffer->priv->undo_manager);
}

/**
 * gtk_source_buffer_end_not_undoable_action:
 * @buffer: a #GtkSourceBuffer.
 *
 * Marks the end of a not undoable action on the buffer.  When the
 * last not undoable block is closed through the call to this
 * function, the list of undo actions is cleared and the undo manager
 * is re-enabled.
 **/
void
gtk_source_buffer_end_not_undoable_action (GtkSourceBuffer *buffer)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	gtk_source_undo_manager_end_not_undoable_action (buffer->priv->undo_manager);
}

/**
 * gtk_source_buffer_get_highlight_matching_brackets:
 * @buffer: a #GtkSourceBuffer.
 *
 * Determines whether bracket match highlighting is activated for the
 * source buffer.
 *
 * Return value: %TRUE if the source buffer will highlight matching
 * brackets.
 **/
gboolean
gtk_source_buffer_get_highlight_matching_brackets (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);

	return (buffer->priv->highlight_brackets != FALSE);
}

/**
 * gtk_source_buffer_set_highlight_matching_brackets:
 * @buffer: a #GtkSourceBuffer.
 * @highlight: %TRUE if you want matching brackets highlighted.
 *
 * Controls the bracket match highlighting function in the buffer.  If
 * activated, when you position your cursor over a bracket character
 * (a parenthesis, a square bracket, etc.) the matching opening or
 * closing bracket character will be highlighted.  You can specify the
 * style with the gtk_source_buffer_set_bracket_match_style()
 * function.
 **/
void
gtk_source_buffer_set_highlight_matching_brackets (GtkSourceBuffer *buffer,
						   gboolean         highlight)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	highlight = (highlight != FALSE);

	if (highlight != buffer->priv->highlight_brackets)
	{
		GtkTextIter iter;
		GtkTextMark *mark;

		buffer->priv->highlight_brackets = highlight;

		/* try to see if there is already a bracket match at the
		 * current position, but only if the tag table is already set
		 * otherwise we have problems when calling this function
		 * on init (get_insert creates the tag table as a side effect */
		if (buffer->priv->constructed)
		{
			mark = gtk_text_buffer_get_insert (GTK_TEXT_BUFFER (buffer));
			gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer), &iter, mark);
			gtk_source_buffer_move_cursor (GTK_TEXT_BUFFER (buffer), &iter, mark);
		}

		g_object_notify (G_OBJECT (buffer), "highlight-matching-brackets");
	}
}

/**
 * gtk_source_buffer_get_highlight_syntax:
 * @buffer: a #GtkSourceBuffer.
 *
 * Determines whether syntax highlighting is activated in the source
 * buffer.
 *
 * Return value: %TRUE if syntax highlighting is enabled, %FALSE otherwise.
 **/
gboolean
gtk_source_buffer_get_highlight_syntax (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);

	return (buffer->priv->highlight_syntax != FALSE);
}

/**
 * gtk_source_buffer_set_highlight_syntax:
 * @buffer: a #GtkSourceBuffer.
 * @highlight: %TRUE to enable syntax highlighting, %FALSE to disable it.
 *
 * Controls whether syntax is highlighted in the buffer. If @highlight
 * is %TRUE, the text will be highlighted according to the syntax
 * patterns specified in the language set with
 * gtk_source_buffer_set_language(). If @highlight is %FALSE, syntax highlighting
 * is disabled and all the GtkTextTag objects that have been added by the
 * syntax highlighting engine are removed from the buffer.
 **/
void
gtk_source_buffer_set_highlight_syntax (GtkSourceBuffer *buffer,
					gboolean         highlight)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	highlight = (highlight != FALSE);

	if (buffer->priv->highlight_syntax != highlight)
	{
		buffer->priv->highlight_syntax = highlight;
		g_object_notify (G_OBJECT (buffer), "highlight-syntax");
	}
}

/**
 * gtk_source_buffer_set_language:
 * @buffer: a #GtkSourceBuffer.
 * @language: a #GtkSourceLanguage to set, or %NULL.
 *
 * Associate a #GtkSourceLanguage with the source buffer. If @language is
 * not-%NULL and syntax highlighting is enabled (see gtk_source_buffer_set_highlight_syntax()),
 * the syntax patterns defined in @language will be used to highlight the text
 * contained in the buffer. If @language is %NULL, the text contained in the
 * buffer is not highlighted.
 *
 * The buffer holds a reference to @language.
 **/
void
gtk_source_buffer_set_language (GtkSourceBuffer   *buffer,
				GtkSourceLanguage *language)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (GTK_IS_SOURCE_LANGUAGE (language) || language == NULL);

	if (buffer->priv->language == language)
		return;

	if (buffer->priv->highlight_engine != NULL)
	{
		/* disconnect the old engine */
		_gtk_source_engine_attach_buffer (buffer->priv->highlight_engine, NULL);
		g_object_unref (buffer->priv->highlight_engine);
		buffer->priv->highlight_engine = NULL;
	}

	if (buffer->priv->language != NULL)
		g_object_unref (buffer->priv->language);

	buffer->priv->language = language;

	if (language != NULL)
	{
		g_object_ref (language);

		/* get a new engine */
		buffer->priv->highlight_engine = _gtk_source_language_create_engine (language);

		if (buffer->priv->highlight_engine)
		{
			_gtk_source_engine_attach_buffer (buffer->priv->highlight_engine,
							  GTK_TEXT_BUFFER (buffer));

			if (buffer->priv->style_scheme)
				_gtk_source_engine_set_style_scheme (buffer->priv->highlight_engine,
								     buffer->priv->style_scheme);
		}
	}

	g_object_notify (G_OBJECT (buffer), "language");
}

/**
 * gtk_source_buffer_get_language:
 * @buffer: a #GtkSourceBuffer.
 *
 * Returns the #GtkSourceLanguage associated with the buffer,
 * see gtk_source_buffer_set_language().  The returned object should not be
 * unreferenced by the user.
 *
 * Return value: #GtkSourceLanguage associated with the buffer, or %NULL.
 **/
GtkSourceLanguage *
gtk_source_buffer_get_language (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);

	return buffer->priv->language;
}

/**
 * _gtk_source_buffer_update_highlight:
 * @buffer: a #GtkSourceBuffer.
 * @start: start of the area to highlight.
 * @end: end of the area to highlight.
 * @synchronous: whether the area should be highlighted synchronously.
 *
 * Asks the buffer to analyze and highlight given area.
 **/
void
_gtk_source_buffer_update_highlight (GtkSourceBuffer   *buffer,
				     const GtkTextIter *start,
				     const GtkTextIter *end,
				     gboolean           synchronous)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));

	if (buffer->priv->highlight_engine != NULL)
		_gtk_source_engine_update_highlight (buffer->priv->highlight_engine,
						     start,
						     end,
						     synchronous);
}

/**
 * gtk_source_buffer_ensure_highlight:
 * @buffer: a #GtkSourceBuffer.
 * @start: start of the area to highlight.
 * @end: end of the area to highlight.
 *
 * Forces buffer to analyze and highlight the given area synchronously.
 *
 * <note>
 *   <para>
 *     This is a potentially slow operation and should be used only
 *     when you need to make sure that some text not currently
 *     visible is highlighted, for instance before printing.
 *   </para>
 * </note>
 **/
void
gtk_source_buffer_ensure_highlight (GtkSourceBuffer   *buffer,
				    const GtkTextIter *start,
				    const GtkTextIter *end)
{
	_gtk_source_buffer_update_highlight (buffer,
					     start,
					     end,
					     TRUE);
}

/**
 * gtk_source_buffer_set_style_scheme:
 * @buffer: a #GtkSourceBuffer.
 * @scheme: style scheme.
 *
 * Sets style scheme used by the buffer. If @scheme is %NULL no
 * style scheme is used.
 **/
void
gtk_source_buffer_set_style_scheme (GtkSourceBuffer      *buffer,
				    GtkSourceStyleScheme *scheme)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (GTK_IS_SOURCE_STYLE_SCHEME (scheme) || scheme == NULL);

	if (buffer->priv->style_scheme == scheme)
		return;

	if (buffer->priv->style_scheme)
		g_object_unref (buffer->priv->style_scheme);

	buffer->priv->style_scheme = scheme ? g_object_ref (scheme) : NULL;
	update_bracket_match_style (buffer);

	if (buffer->priv->highlight_engine != NULL)
		_gtk_source_engine_set_style_scheme (buffer->priv->highlight_engine,
						     scheme);

	g_object_notify (G_OBJECT (buffer), "style-scheme");
}

/**
 * gtk_source_buffer_get_style_scheme:
 * @buffer: a #GtkSourceBuffer.
 *
 * Returns the #GtkSourceStyleScheme currently used in @buffer.
 *
 * Returns: the #GtkSourceStyleScheme set by
 * gtk_source_buffer_set_style_scheme(), or %NULL.
 **/
GtkSourceStyleScheme *
gtk_source_buffer_get_style_scheme (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);
	return buffer->priv->style_scheme;
}

/* Source Marks functionality */

/* At the moment this is pretty dumb (O(N)), if it is a performance
 * problem we should change data struct.
 * Since it's used from mark_set when the mark was moved, we cannot bsearch.
 * Returns TRUE if the mark was found and removed */
static gboolean
source_mark_remove (GtkSourceBuffer *buffer, GtkSourceMark *mark)
{
	guint i;

	for (i = 0; i < buffer->priv->source_marks->len; ++i)
	{
		GtkSourceMark *m;

		m = g_array_index (buffer->priv->source_marks, GtkSourceMark *, i);
		if (mark == m)
		{
			g_array_remove_index (buffer->priv->source_marks, i);
			g_object_unref (m);

			return TRUE;
		}
	}

	return FALSE;
}

/* Performs a binary search among the source marks in @buffer for the
 * position of the @iter.  Returns the nearest matching mark (its
 * index in the marks array) and optionally the value of the
 * comparision between the given iter and the iter at the returned mark.
 *
 * Return value: an index in the source marks array or -1 if the array is
 * empty.
 */
static gint
source_mark_bsearch (GtkSourceBuffer *buffer, GtkTextIter *iter, gint *last_cmp)
{
	GtkTextIter check_iter;
	GtkSourceMark **check, **p;
	GArray *marks = buffer->priv->source_marks;
	gint n_marks = marks->len;
	gint cmp, i;

	if (n_marks == 0)
		return -1;

	check = p = &g_array_index (marks, GtkSourceMark *, 0);
	p--;
	cmp = 0;
	while (n_marks)
	{
		i = (n_marks + 1) >> 1;
		check = p + i;
		gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
						  &check_iter,
						  GTK_TEXT_MARK (*check));
		cmp = gtk_text_iter_compare (iter, &check_iter);
		if (cmp > 0)
		{
			n_marks -= i;
			p = check;
		}
		else if (cmp < 0)
			n_marks = i - 1;
		else /* if (cmp == 0) */
			break;
	}

	i = check - &g_array_index (marks, GtkSourceMark *, 0);
	if (last_cmp)
		*last_cmp = cmp;

	return i;
}

static void
source_mark_insert (GtkSourceBuffer *buffer, GtkSourceMark *mark)
{
	GtkTextIter iter;
	gint idx, cmp;

	gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
					  &iter,
					  GTK_TEXT_MARK (mark));

	idx = source_mark_bsearch (buffer, &iter, &cmp);
	if (idx >= 0)
	{
		/* if the mark we found is at same iter or before
		 * put our mark after that */
		if (cmp >= 0)
			idx++;
	}
	else
	{
		idx = 0;
	}

	g_object_ref (mark);
	g_array_insert_val (buffer->priv->source_marks, idx, mark);
}

static void
gtk_source_buffer_real_mark_set	(GtkTextBuffer     *buffer,
				 const GtkTextIter *location,
				 GtkTextMark       *mark)
{
	if (GTK_IS_SOURCE_MARK (mark))
	{
		/* for now we simply remove and reinsert at
		 * the right place every time */
		source_mark_remove (GTK_SOURCE_BUFFER (buffer),
				    GTK_SOURCE_MARK (mark));
		source_mark_insert (GTK_SOURCE_BUFFER (buffer),
				    GTK_SOURCE_MARK (mark));

		g_signal_emit_by_name (buffer, "source_mark_updated", mark);
	}

	/* if the mark is the insert mark, update bracket matching */
	else if (mark == gtk_text_buffer_get_insert (buffer))
	{
		gtk_source_buffer_move_cursor (buffer, location, mark);
	}

	GTK_TEXT_BUFFER_CLASS (gtk_source_buffer_parent_class)->mark_set (buffer, location, mark);
}

static void
gtk_source_buffer_real_mark_deleted (GtkTextBuffer *buffer,
				     GtkTextMark *mark)
{
	if (GTK_IS_SOURCE_MARK (mark))
	{
		source_mark_remove (GTK_SOURCE_BUFFER (buffer),
				    GTK_SOURCE_MARK (mark));

		g_signal_emit_by_name (buffer, "source_mark_updated", mark);
	}

	if (GTK_TEXT_BUFFER_CLASS (gtk_source_buffer_parent_class)->mark_deleted != NULL)
		GTK_TEXT_BUFFER_CLASS (gtk_source_buffer_parent_class)->mark_deleted (buffer, mark);
}

static void
gtk_source_buffer_real_undo (GtkSourceBuffer *buffer)
{
	g_return_if_fail (gtk_source_undo_manager_can_undo (buffer->priv->undo_manager));

	gtk_source_undo_manager_undo (buffer->priv->undo_manager);
}

static void
gtk_source_buffer_real_redo (GtkSourceBuffer *buffer)
{
	g_return_if_fail (gtk_source_undo_manager_can_redo (buffer->priv->undo_manager));

	gtk_source_undo_manager_redo (buffer->priv->undo_manager);
}

/**
 * gtk_source_buffer_create_source_mark:
 * @buffer: a #GtkSourceBuffer.
 * @name: the name of the mark, or %NULL.
 * @category: a string defining the mark category.
 * @where: location to place the mark.
 *
 * Creates a source mark in the @buffer of category @category.  A source mark is
 * a #GtkTextMark but organised into categories. Depending on the category
 * a pixbuf can be specified that will be displayed along the line of the mark.
 *
 * Like a #GtkTextMark, a #GtkSourceMark can be anonymous if the
 * passed @name is %NULL.  Also, the buffer owns the marks so you
 * shouldn't unreference it.
 *
 * Marks always have left gravity and are moved to the beginning of
 * the line when the user deletes the line they were in.
 *
 * Typical uses for a source mark are bookmarks, breakpoints, current
 * executing instruction indication in a source file, etc..
 *
 * Return value: a new #GtkSourceMark, owned by the buffer.
 *
 * Since: 2.2
 **/
GtkSourceMark *
gtk_source_buffer_create_source_mark (GtkSourceBuffer   *buffer,
				      const gchar       *name,
				      const gchar       *category,
				      const GtkTextIter *where)
{
	GtkSourceMark *mark;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);
	g_return_val_if_fail (category != NULL, NULL);
	g_return_val_if_fail (where != NULL, NULL);

	mark = gtk_source_mark_new (name, category);
	gtk_text_buffer_add_mark (GTK_TEXT_BUFFER (buffer),
				  GTK_TEXT_MARK (mark),
				  where);

	return mark;
}

GtkSourceMark *
_gtk_source_buffer_source_mark_next (GtkSourceBuffer *buffer,
				     GtkSourceMark   *mark,
				     const gchar     *category)
{
	GtkTextIter iter;
	gint idx, cmp;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);

	/* TODO: we could speed this up by caching the current
	 * position in the mark and invalidating the cache when
	 * the marks array changes. For now we always lookup. */
	gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
					  &iter,
					  GTK_TEXT_MARK (mark));

	idx = source_mark_bsearch (buffer, &iter, &cmp);

	/* the array should already contain @mark */
	g_return_val_if_fail (idx >= 0, NULL);
	g_return_val_if_fail (cmp == 0, NULL);

	/* move up to our mark among the ones at this position */
	while (mark != g_array_index (buffer->priv->source_marks, GtkSourceMark *, idx))
	{
		++idx;
	}

	while ((guint) ++idx < buffer->priv->source_marks->len)
	{
		GtkSourceMark *ret;

		ret = g_array_index (buffer->priv->source_marks, GtkSourceMark *, idx);
		if (category == NULL ||
		    0 == strcmp (category, gtk_source_mark_get_category (ret)))
		{
			return ret;
		}
	}

	return NULL;
}

GtkSourceMark *
_gtk_source_buffer_source_mark_prev (GtkSourceBuffer *buffer,
				     GtkSourceMark   *mark,
				     const gchar     *category)
{
	GtkTextIter iter;
	gint idx, cmp;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);

	/* TODO: we could speed this up by caching the current
	 * position in the mark and invalidating the cache when
	 * the marks array changes. For now we always lookup. */
	gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
					  &iter,
					  GTK_TEXT_MARK (mark));

	idx = source_mark_bsearch (buffer, &iter, &cmp);

	/* the array should already contain @mark */
	g_return_val_if_fail (idx >= 0, NULL);
	g_return_val_if_fail (cmp == 0, NULL);

	/* move up to our mark among the ones at this position */
	while (mark != g_array_index (buffer->priv->source_marks, GtkSourceMark *, idx))
	{
		++idx;
	}

	while (--idx >= 0)
	{
		GtkSourceMark *ret;

		ret = g_array_index (buffer->priv->source_marks, GtkSourceMark *, idx);
		if (category == NULL ||
		    0 == strcmp (category, gtk_source_mark_get_category (ret)))
		{
			return ret;
		}
	}

	return NULL;
}

/**
 * gtk_source_buffer_forward_iter_to_source_mark:
 * @buffer: a #GtkSourceBuffer.
 * @iter: an iterator.
 * @category: category to search for or %NULL
 *
 * Moves @iter to the position of the next #GtkSourceMark of the given
 * @category. Returns #TRUE if @iter was moved. If @category is NULL, the
 * next source mark can be of any category.
 *
 * Returns: whether iter moved.
 *
 * Since: 2.2
 **/
gboolean
gtk_source_buffer_forward_iter_to_source_mark (GtkSourceBuffer *buffer,
					       GtkTextIter     *iter,
					       const gchar     *category)
{
	GtkTextIter i;
	gint idx, cmp;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);

	i = *iter;

	idx = source_mark_bsearch (buffer, &i, &cmp);
	if (idx < 0)
		return FALSE;

	if (cmp >= 0)
		++idx;

	while ((guint) idx < buffer->priv->source_marks->len)
	{
		GtkSourceMark *mark;

		mark = g_array_index (buffer->priv->source_marks, GtkSourceMark *, idx);
		if (category == NULL ||
		    0 == strcmp (category, gtk_source_mark_get_category (mark)))
		{
			/* update the iter */
			gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
							  &i, GTK_TEXT_MARK (mark));

			if (gtk_text_iter_compare (&i, iter) > 0 )
			{
				*iter = i;
				return TRUE;
			}
		}

		++idx;
	}

	return FALSE;
}

/**
 * gtk_source_buffer_backward_iter_to_source_mark:
 * @buffer: a #GtkSourceBuffer.
 * @iter: an iterator.
 * @category: category to search for or %NULL
 *
 * Moves @iter to the position of the previous #GtkSourceMark of the given
 * category. Returns #TRUE if @iter was moved. If @category is NULL, the
 * previous source mark can be of any category.
 *
 * Returns: whether iter moved.
 *
 * Since: 2.2
 **/
gboolean
gtk_source_buffer_backward_iter_to_source_mark (GtkSourceBuffer *buffer,
						GtkTextIter     *iter,
						const gchar     *category)
{
	GtkTextIter i;
	gint idx, cmp;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);

	i = *iter;

	idx = source_mark_bsearch (buffer, &i, &cmp);
	if (idx < 0)
		return FALSE;

	if (cmp <= 0)
		--idx;

	while (idx >= 0)
	{
		GtkSourceMark *mark;

		mark = g_array_index (buffer->priv->source_marks, GtkSourceMark *, idx);
		if (category == NULL ||
		    0 == strcmp (category, gtk_source_mark_get_category (mark)))
		{
			gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
							  &i, GTK_TEXT_MARK (mark));

			if (gtk_text_iter_compare (&i, iter) < 0 )
			{
				*iter = i;
				return TRUE;
			}
		}

		--idx;
	}

	return FALSE;
}

/**
 * gtk_source_buffer_get_source_marks_at_iter:
 * @buffer: a #GtkSourceBuffer.
 * @iter: an iterator.
 * @category: category to search for or %NULL
 *
 * Returns the list of marks of the given category at @iter. If @category
 * is %NULL it returns all marks at @iter.
 *
 * Returns: a newly allocated #GSList.
 *
 * Since: 2.2
 **/
GSList *
gtk_source_buffer_get_source_marks_at_iter (GtkSourceBuffer *buffer,
					    GtkTextIter     *iter,
					    const gchar     *category)
{
	GSList *marks, *l, *res;

	res = NULL;
	marks = gtk_text_iter_get_marks (iter);

	for (l = marks; l != NULL; l = l->next)
	{
		GtkSourceMark *mark;

		if (!GTK_IS_SOURCE_MARK (l->data))
			continue;

		mark = GTK_SOURCE_MARK (l->data);
		if (category == NULL ||
		    0 == strcmp (category, gtk_source_mark_get_category (mark)))
		{
			res = g_slist_prepend (res, l->data);
		}
	}

	g_slist_free (marks);

	return g_slist_reverse (res);
}

/**
 * gtk_source_buffer_get_source_marks_at_line:
 * @buffer: a #GtkSourceBuffer.
 * @line: a line number.
 * @category: category to search for or %NULL
 *
 * Returns the list of marks of the given category at @line.
 * If @category is NULL, all marks at @line are returned.
 *
 * Returns: a newly allocated #GSList.
 *
 * Since: 2.2
 **/
GSList *
gtk_source_buffer_get_source_marks_at_line (GtkSourceBuffer *buffer,
					    gint             line,
					    const gchar     *category)
{
	GtkTextIter iter;
	GSList *res;

 	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);

	gtk_text_buffer_get_iter_at_line (GTK_TEXT_BUFFER (buffer),
					  &iter, line);

	res = gtk_source_buffer_get_source_marks_at_iter (buffer,
							  &iter,
							  category);

	while (gtk_source_buffer_forward_iter_to_source_mark (buffer,
							      &iter,
							      category))
	{
		if (gtk_text_iter_get_line (&iter) == line)
		{
			GSList *l;

			l =  gtk_source_buffer_get_source_marks_at_iter (buffer,
									 &iter,
									 category);

			res = g_slist_concat (res, l);
		}
		else
		{
			break;
		}
	}

	return res;
}

/**
 * gtk_source_buffer_remove_source_marks:
 * @buffer: a #GtkSourceBuffer.
 * @start: a #GtkTextIter
 * @end: a #GtkTextIter
 * @category: category to search for or NULL
 *
 * Remove all marks of @category between @start and @end from the buffer.
 * If @category is NULL, all marks in the range will be removed.
 *
 * Since: 2.2
 **/
void
gtk_source_buffer_remove_source_marks (GtkSourceBuffer   *buffer,
				       const GtkTextIter *start,
				       const GtkTextIter *end,
				       const gchar       *category)
{
	GtkTextIter iter;
	GSList *list;
	GSList *l;

 	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
 	g_return_if_fail (start != NULL);
 	g_return_if_fail (end != NULL);

	iter = *start;

	list = gtk_source_buffer_get_source_marks_at_iter (buffer,
							   &iter,
							   category);

	while (gtk_source_buffer_forward_iter_to_source_mark (buffer,
							      &iter,
							      category))
	{
		if (gtk_text_iter_compare (&iter, end) <= 0)
		{
			l =  gtk_source_buffer_get_source_marks_at_iter (buffer,
									 &iter,
									 category);

			list = g_slist_concat (list, l);
		}
		else
		{
			break;
		}
	}

	for (l = list; l != NULL; l = l->next)
	{
		gtk_text_buffer_delete_mark (GTK_TEXT_BUFFER (buffer),
					     GTK_TEXT_MARK (l->data));
	}

	g_slist_free (list);
}


/**
 * gtk_source_buffer_iter_has_context_class:
 * @buffer: a #GtkSourceBuffer.
 * @iter: a #GtkTextIter
 * @context_class: class to search for
 *
 * Check if the class @context_klass is set on @iter.
 *
 * Since: 2.10
 **/
gboolean
gtk_source_buffer_iter_has_context_class (GtkSourceBuffer   *buffer,
                                          const GtkTextIter *iter,
                                          const gchar       *context_class)
{
	GtkTextTag *tag;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (context_class != NULL, FALSE);

	if (buffer->priv->highlight_engine == NULL)
	{
		return FALSE;
	}

	tag = _gtk_source_engine_get_context_class_tag (buffer->priv->highlight_engine,
							context_class);

	if (tag != NULL)
	{
		return gtk_text_iter_has_tag (iter, tag);
	}
	else
	{
		return FALSE;
	}
}

/**
 * gtk_source_buffer_get_context_classes_at_iter:
 * @buffer: a #GtkSourceBuffer.
 * @iter: a #GtkTextIter
 *
 * Get all defined context classes at @iter.
 *
 * Returns: a new %NULL terminated array of context class names. Use
 *          #g_strfreev to free the array if it is no longer needed.
 *
 * Since: 2.10
 **/
gchar **
gtk_source_buffer_get_context_classes_at_iter (GtkSourceBuffer   *buffer,
                                               const GtkTextIter *iter)
{
	GSList *tags;
	GSList *item;
	GPtrArray *ret;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);
	g_return_val_if_fail (iter != NULL, NULL);

	tags = gtk_text_iter_get_tags (iter);
	ret = g_ptr_array_new ();

	for (item = tags; item; item = g_slist_next (item))
	{
		gchar const *name = g_object_get_data (G_OBJECT (item->data),
		                                       TAG_CONTEXT_CLASS_NAME);

		if (name != NULL)
		{
			g_ptr_array_add (ret, g_strdup (name));
		}
	}

	g_ptr_array_add (ret, NULL);
	return (gchar **) g_ptr_array_free (ret, FALSE);
}

/**
 * gtk_source_buffer_iter_forward_to_context_class_toggle:
 * @buffer: a #GtkSourceBuffer.
 * @iter: a #GtkTextIter
 * @context_class: the context class
 *
 * Moves forward to the next toggle (on or off) of the context class. If no
 * matching context class toggles are found, returns %FALSE, otherwise %TRUE.
 * Does not return toggles located at @iter, only toggles after @iter. Sets
 * @iter to the location of the toggle, or to the end of the buffer if no
 * toggle is found.
 *
 * Returns: whether we found a context class toggle after @iter
 *
 * Since: 2.10
 **/
gboolean
gtk_source_buffer_iter_forward_to_context_class_toggle (GtkSourceBuffer *buffer,
                                                        GtkTextIter     *iter,
                                                        const gchar     *context_class)
{
	GtkTextTag *tag;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (context_class != NULL, FALSE);

	if (buffer->priv->highlight_engine == NULL)
	{
		return FALSE;
	}

	tag = _gtk_source_engine_get_context_class_tag (buffer->priv->highlight_engine,
							context_class);

	if (tag == NULL)
	{
		return FALSE;
	}
	else
	{
		return gtk_text_iter_forward_to_tag_toggle (iter, tag);
	}
}

/**
 * gtk_source_buffer_iter_backward_to_context_class_toggle:
 * @buffer: a #GtkSourceBuffer.
 * @iter: a #GtkTextIter
 * @context_class: the context class
 *
 * Moves backward to the next toggle (on or off) of the context class. If no
 * matching context class toggles are found, returns %FALSE, otherwise %TRUE.
 * Does not return toggles located at @iter, only toggles after @iter. Sets
 * @iter to the location of the toggle, or to the end of the buffer if no
 * toggle is found.
 *
 * Returns: whether we found a context class toggle before @iter
 *
 * Since: 2.10
 **/
gboolean
gtk_source_buffer_iter_backward_to_context_class_toggle (GtkSourceBuffer *buffer,
                                                         GtkTextIter     *iter,
                                                         const gchar     *context_class)
{
	GtkTextTag *tag;

	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (context_class != NULL, FALSE);

	if (buffer->priv->highlight_engine == NULL)
	{
		return FALSE;
	}

	tag = _gtk_source_engine_get_context_class_tag (buffer->priv->highlight_engine,
							context_class);

	if (tag == NULL)
	{
		return FALSE;
	}
	else
	{
		return gtk_text_iter_backward_to_tag_toggle (iter, tag);
	}
}

/**
 * gtk_source_buffer_set_undo_manager:
 * @buffer: A #GtkSourceBuffer
 * @manager: A #GtkSourceUndoManager
 *
 * Set the buffer undo manager. If @manager is %NULL the default undo manager
 * will be set.
 *
 **/
void
gtk_source_buffer_set_undo_manager (GtkSourceBuffer      *buffer,
                                    GtkSourceUndoManager *manager)
{
	g_return_if_fail (GTK_IS_SOURCE_BUFFER (buffer));
	g_return_if_fail (manager == NULL || GTK_IS_SOURCE_UNDO_MANAGER (manager));

	if (manager == NULL)
	{
		manager = g_object_new (GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT,
		                        "buffer", buffer,
		                        "max-undo-levels", buffer->priv->max_undo_levels,
		                        NULL);
	}
	else
	{
		g_object_ref (manager);
	}

	set_undo_manager (buffer, manager);
	g_object_unref (manager);

	g_object_notify (G_OBJECT (buffer), "undo-manager");
}

/**
 * gtk_source_buffer_get_undo_manager:
 * @buffer: A #GtkSourceBuffer
 *
 * Get the undo manager associated with the buffer.
 *
 * Returns: A #GtkSourceUndoManager
 *
 **/
GtkSourceUndoManager *
gtk_source_buffer_get_undo_manager (GtkSourceBuffer *buffer)
{
	g_return_val_if_fail (GTK_IS_SOURCE_BUFFER (buffer), NULL);

	return buffer->priv->undo_manager;
}
