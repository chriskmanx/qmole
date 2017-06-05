/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 *
 *  gtksourceengine.c - Abstract base class for highlighting engines
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gtksourcebuffer.h"
#include "gtksourceengine.h"


G_DEFINE_TYPE (GtkSourceEngine, _gtk_source_engine, G_TYPE_OBJECT)


static void
_gtk_source_engine_class_init (GtkSourceEngineClass *klass)
{
	klass->attach_buffer = NULL;
}


static void
_gtk_source_engine_init (G_GNUC_UNUSED GtkSourceEngine *engine)
{
}

void
_gtk_source_engine_attach_buffer (GtkSourceEngine *engine,
				 GtkTextBuffer   *buffer)
{
	g_return_if_fail (GTK_IS_SOURCE_ENGINE (engine));
	g_return_if_fail (GTK_SOURCE_ENGINE_GET_CLASS (engine)->attach_buffer != NULL);

	GTK_SOURCE_ENGINE_GET_CLASS (engine)->attach_buffer (engine, buffer);
}

void
_gtk_source_engine_text_inserted (GtkSourceEngine *engine,
				  gint             start_offset,
				  gint             end_offset)
{
	g_return_if_fail (GTK_IS_SOURCE_ENGINE (engine));
	g_return_if_fail (GTK_SOURCE_ENGINE_GET_CLASS (engine)->text_inserted != NULL);

	GTK_SOURCE_ENGINE_GET_CLASS (engine)->text_inserted (engine,
							     start_offset,
							     end_offset);
}

void
_gtk_source_engine_text_deleted (GtkSourceEngine *engine,
				 gint             offset,
				 gint             length)
{
	g_return_if_fail (GTK_IS_SOURCE_ENGINE (engine));
	g_return_if_fail (GTK_SOURCE_ENGINE_GET_CLASS (engine)->text_deleted != NULL);

	GTK_SOURCE_ENGINE_GET_CLASS (engine)->text_deleted (engine,
							    offset,
							    length);
}

void
_gtk_source_engine_update_highlight (GtkSourceEngine   *engine,
				     const GtkTextIter *start,
				     const GtkTextIter *end,
				     gboolean           synchronous)
{
	g_return_if_fail (GTK_IS_SOURCE_ENGINE (engine));
	g_return_if_fail (start != NULL && end != NULL);
	g_return_if_fail (GTK_SOURCE_ENGINE_GET_CLASS (engine)->update_highlight != NULL);

	GTK_SOURCE_ENGINE_GET_CLASS (engine)->update_highlight (engine,
								start,
								end,
								synchronous);
}

void
_gtk_source_engine_set_style_scheme (GtkSourceEngine      *engine,
				     GtkSourceStyleScheme *scheme)
{
	g_return_if_fail (GTK_IS_SOURCE_ENGINE (engine));
	g_return_if_fail (GTK_IS_SOURCE_STYLE_SCHEME (scheme) || scheme == NULL);
	g_return_if_fail (GTK_SOURCE_ENGINE_GET_CLASS (engine)->set_style_scheme != NULL);

	GTK_SOURCE_ENGINE_GET_CLASS (engine)->set_style_scheme (engine, scheme);
}

GtkTextTag *
_gtk_source_engine_get_context_class_tag (GtkSourceEngine *engine,
					  const gchar     *context_class)
{
	g_return_val_if_fail (GTK_IS_SOURCE_ENGINE (engine), NULL);
	g_return_val_if_fail (context_class != NULL, NULL);

	return GTK_SOURCE_ENGINE_GET_CLASS (engine)->get_context_class_tag (engine,
									    context_class);
}
