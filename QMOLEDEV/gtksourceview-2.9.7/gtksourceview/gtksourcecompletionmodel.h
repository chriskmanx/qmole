/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcecompletionmodel.h
 * This file is part of gtksourcecompletion
 *
 * Copyright (C) 2009 - Jesse van den Kieboom <jessevdk@gnome.org>
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

#ifndef __GTK_SOURCE_COMPLETION_MODEL_H__
#define __GTK_SOURCE_COMPLETION_MODEL_H__

#include <gtk/gtk.h>
#include <gtksourceview/gtksourcecompletionprovider.h>
#include <gtksourceview/gtksourcecompletionproposal.h>

G_BEGIN_DECLS

#define GTK_TYPE_SOURCE_COMPLETION_MODEL		(gtk_source_completion_model_get_type ())
#define GTK_SOURCE_COMPLETION_MODEL(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SOURCE_COMPLETION_MODEL, GtkSourceCompletionModel))
#define GTK_SOURCE_COMPLETION_MODEL_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SOURCE_COMPLETION_MODEL, GtkSourceCompletionModel const))
#define GTK_SOURCE_COMPLETION_MODEL_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_SOURCE_COMPLETION_MODEL, GtkSourceCompletionModelClass))
#define GTK_IS_SOURCE_COMPLETION_MODEL(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_SOURCE_COMPLETION_MODEL))
#define GTK_IS_SOURCE_COMPLETION_MODEL_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SOURCE_COMPLETION_MODEL))
#define GTK_SOURCE_COMPLETION_MODEL_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_SOURCE_COMPLETION_MODEL, GtkSourceCompletionModelClass))

typedef struct _GtkSourceCompletionModel	GtkSourceCompletionModel;
typedef struct _GtkSourceCompletionModelClass	GtkSourceCompletionModelClass;
typedef struct _GtkSourceCompletionModelPrivate	GtkSourceCompletionModelPrivate;

struct _GtkSourceCompletionModel {
	GObject parent;
	
	GtkSourceCompletionModelPrivate *priv;
};

struct _GtkSourceCompletionModelClass {
	GObjectClass parent_class;
	
	void (*providers_changed) 	(GtkSourceCompletionModel *model);
	void (*begin_delete)		(GtkSourceCompletionModel *model);
	void (*end_delete)		(GtkSourceCompletionModel *model);
};

enum
{
	GTK_SOURCE_COMPLETION_MODEL_COLUMN_LABEL,
	GTK_SOURCE_COMPLETION_MODEL_COLUMN_MARKUP,
	GTK_SOURCE_COMPLETION_MODEL_COLUMN_ICON,
	GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROPOSAL,
	GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROVIDER,
	GTK_SOURCE_COMPLETION_MODEL_N_COLUMNS
};

GType gtk_source_completion_model_get_type (void) G_GNUC_CONST;

GtkSourceCompletionModel *
		gtk_source_completion_model_new 	(void);

void		gtk_source_completion_model_begin	(GtkSourceCompletionModel           *model,
                                                         GList                              *providers);
void		gtk_source_completion_model_append 	(GtkSourceCompletionModel           *model,
							 GtkSourceCompletionProvider        *provider,
							 GList                              *proposals);
void		gtk_source_completion_model_end		(GtkSourceCompletionModel           *model,
							 GtkSourceCompletionProvider        *provider);
void		gtk_source_completion_model_cancel	(GtkSourceCompletionModel           *model);

gboolean	gtk_source_completion_model_is_empty 	(GtkSourceCompletionModel           *model,
                                                         gboolean                            invisible);

void            gtk_source_completion_model_set_visible_providers (GtkSourceCompletionModel *model,
                                                                   GList                    *providers);
GList          *gtk_source_completion_model_get_visible_providers (GtkSourceCompletionModel *model);


GList          *gtk_source_completion_model_get_providers (GtkSourceCompletionModel         *model);
guint		gtk_source_completion_model_n_proposals (GtkSourceCompletionModel           *model,
                                                         GtkSourceCompletionProvider        *provider);

void 		gtk_source_completion_model_clear 	(GtkSourceCompletionModel           *model);

void 		gtk_source_completion_model_set_show_headers (GtkSourceCompletionModel      *model,
							      gboolean                       show_headers);

gboolean	gtk_source_completion_model_iter_is_header (GtkSourceCompletionModel        *model,
                                                            GtkTreeIter                     *iter);

gboolean 	gtk_source_completion_model_iter_previous (GtkSourceCompletionModel         *model,
							   GtkTreeIter                      *iter);
							 
gboolean 	gtk_source_completion_model_iter_last 	(GtkSourceCompletionModel           *model,
							 GtkTreeIter                        *iter);

gboolean	gtk_source_completion_model_iter_equal	(GtkSourceCompletionModel           *model,
							 GtkTreeIter                        *iter1,
							 GtkTreeIter                        *iter2);

G_END_DECLS

#endif /* __GTK_SOURCE_COMPLETION_MODEL_H__ */
