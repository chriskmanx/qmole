/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourceengine.h - Abstract base class for highlighting engines
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2003 - Gustavo Giráldez
 *
 * GtkSourceView is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * GtkSourceView is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef __GTK_SOURCE_ENGINE_H__
#define __GTK_SOURCE_ENGINE_H__

#include <gtk/gtk.h>
#include <gtksourceview/gtksourcestylescheme.h>

G_BEGIN_DECLS

#define GTK_SOURCE_TYPE_ENGINE            (_gtk_source_engine_get_type ())
#define GTK_SOURCE_ENGINE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_SOURCE_TYPE_ENGINE, GtkSourceEngine))
#define GTK_SOURCE_ENGINE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_SOURCE_TYPE_ENGINE, GtkSourceEngineClass))
#define GTK_SOURCE_IS_ENGINE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_SOURCE_TYPE_ENGINE))
#define GTK_SOURCE_IS_ENGINE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_SOURCE_TYPE_ENGINE))
#define GTK_SOURCE_ENGINE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_SOURCE_TYPE_ENGINE, GtkSourceEngineClass))

typedef struct _GtkSourceEngine		GtkSourceEngine;
typedef struct _GtkSourceEngineClass	GtkSourceEngineClass;

struct _GtkSourceEngine
{
	GObject parent_instance;
};

struct _GtkSourceEngineClass
{
	GObjectClass parent_class;

	void     (* attach_buffer)    (GtkSourceEngine      *engine,
				       GtkTextBuffer        *buffer);

	void     (* text_inserted)    (GtkSourceEngine      *engine,
				       gint                  start_offset,
				       gint                  end_offset);
	void     (* text_deleted)     (GtkSourceEngine      *engine,
				       gint                  offset,
				       gint                  length);

	void     (* update_highlight) (GtkSourceEngine      *engine,
				       const GtkTextIter    *start,
				       const GtkTextIter    *end,
				       gboolean              synchronous);

	void     (* set_style_scheme) (GtkSourceEngine      *engine,
				       GtkSourceStyleScheme *scheme);

	GtkTextTag *(* get_context_class_tag)
				      (GtkSourceEngine      *engine,
				       const gchar          *context_class);
};

GType       _gtk_source_engine_get_type		(void) G_GNUC_CONST;

void        _gtk_source_engine_attach_buffer	(GtkSourceEngine      *engine,
						 GtkTextBuffer        *buffer);
void        _gtk_source_engine_text_inserted	(GtkSourceEngine      *engine,
						 gint                  start_offset,
						 gint                  end_offset);
void        _gtk_source_engine_text_deleted	(GtkSourceEngine      *engine,
						 gint                  offset,
						 gint                  length);
void        _gtk_source_engine_update_highlight	(GtkSourceEngine      *engine,
						 const GtkTextIter    *start,
						 const GtkTextIter    *end,
						 gboolean              synchronous);
void        _gtk_source_engine_set_style_scheme	(GtkSourceEngine      *engine,
						 GtkSourceStyleScheme *scheme);

GtkTextTag *_gtk_source_engine_get_context_class_tag
						 (GtkSourceEngine     *engine,
						  const gchar         *context_class);

G_END_DECLS

#endif /* __GTK_SOURCE_ENGINE_H__ */
