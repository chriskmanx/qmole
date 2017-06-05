/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 *
 *  gtksourceengine.h - Abstract base class for highlighting engines
 *
 *  Copyright (C) 2003 - Gustavo Giráldez
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

#ifndef __GTK_SOURCE_ENGINE_H__
#define __GTK_SOURCE_ENGINE_H__

#include <gtk/gtktextbuffer.h>
#include <gtksourceview/gtksourcestylescheme.h>

G_BEGIN_DECLS

#define GTK_TYPE_SOURCE_ENGINE            (_gtk_source_engine_get_type ())
#define GTK_SOURCE_ENGINE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SOURCE_ENGINE, GtkSourceEngine))
#define GTK_SOURCE_ENGINE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_SOURCE_ENGINE, GtkSourceEngineClass))
#define GTK_IS_SOURCE_ENGINE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_SOURCE_ENGINE))
#define GTK_IS_SOURCE_ENGINE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SOURCE_ENGINE))
#define GTK_SOURCE_ENGINE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_SOURCE_ENGINE, GtkSourceEngineClass))

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
