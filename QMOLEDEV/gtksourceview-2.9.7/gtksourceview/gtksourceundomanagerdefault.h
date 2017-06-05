/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gtksourceundomanager_defaultdefault.h
 * This file is part of GtkSourceView
 *
 * Copyright (C) 1998, 1999 Alex Roberts, Evan Lawrence
 * Copyright (C) 2000, 2001 Chema Celorio, Paolo Maggi
 * Copyright (C) 2002, 2003 Paolo Maggi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef __GTK_SOURCE_UNDO_MANAGER_DEFAULT_H__
#define __GTK_SOURCE_UNDO_MANAGER_DEFAULT_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT		(gtk_source_undo_manager_default_get_type ())
#define GTK_SOURCE_UNDO_MANAGER_DEFAULT(obj)		(G_TYPE_CHECK_INSTANCE_CAST((obj), GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT, GtkSourceUndoManagerDefault))
#define GTK_SOURCE_UNDO_MANAGER_DEFAULT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST((klass), GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT, GtkSourceUndoManagerDefaultClass))
#define GTK_IS_SOURCE_UNDO_MANAGER_DEFAULT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT))
#define GTK_IS_SOURCE_UNDO_MANAGER_DEFAULT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT))
#define GTK_SOURCE_UNDO_MANAGER_DEFAULT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_SOURCE_UNDO_MANAGER_DEFAULT, GtkSourceUndoManagerDefaultClass))

typedef struct _GtkSourceUndoManagerDefault        	GtkSourceUndoManagerDefault;
typedef struct _GtkSourceUndoManagerDefaultClass 	GtkSourceUndoManagerDefaultClass;
typedef struct _GtkSourceUndoManagerDefaultPrivate 	GtkSourceUndoManagerDefaultPrivate;

struct _GtkSourceUndoManagerDefault
{
	GObject parent;

	GtkSourceUndoManagerDefaultPrivate *priv;
};

struct _GtkSourceUndoManagerDefaultClass
{
	GObjectClass parent_class;
};

GType gtk_source_undo_manager_default_get_type (void) G_GNUC_CONST;

void gtk_source_undo_manager_default_set_max_undo_levels (GtkSourceUndoManagerDefault *manager,
                                                          gint                         max_undo_levels);

G_END_DECLS

#endif /* __GTK_SOURCE_UNDO_MANAGER_DEFAULT_H__ */


