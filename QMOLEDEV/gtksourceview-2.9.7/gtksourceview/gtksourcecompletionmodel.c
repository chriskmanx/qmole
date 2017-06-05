/*
 * gtksourcecompletionmodel.c
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

#include "gtksourcecompletionmodel.h"
#include "gtksourceview-i18n.h"
#include "gtksourcecompletionprovider.h"

#define GTK_SOURCE_COMPLETION_MODEL_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_TYPE_SOURCE_COMPLETION_MODEL, GtkSourceCompletionModelPrivate))

typedef struct
{
	GtkSourceCompletionModel *model;

	GtkSourceCompletionProvider *provider;
	GtkSourceCompletionProposal *proposal;

	gulong changed_id;
	gboolean mark;
	
	gboolean filtered;
} ProposalNode;

typedef struct
{
	GtkSourceCompletionProvider *provider;
	GHashTable *proposals;
	guint num_proposals;
	gboolean filtered;

	GList *first;
	GList *last;
	GList *ptr;
	gboolean first_batch;
} ProviderInfo;

struct _GtkSourceCompletionModelPrivate
{
	GType column_types[GTK_SOURCE_COMPLETION_MODEL_N_COLUMNS];
	GList *store;
	GList *last;
	
	GHashTable *providers_info;
	GList *providers;
	GList *visible_providers;
	
	guint num;
	gboolean show_headers;
	
	gboolean marking;
};

enum
{
	PROVIDERS_CHANGED,
	BEGIN_DELETE,
	END_DELETE,
	NUM_SIGNALS
};

static guint signals[NUM_SIGNALS] = {0,};

static void tree_model_iface_init (gpointer g_iface, gpointer iface_data);

G_DEFINE_TYPE_WITH_CODE (GtkSourceCompletionModel, 
                         gtk_source_completion_model, 
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_MODEL,
                                                tree_model_iface_init))


static gboolean
provider_is_visible (GtkSourceCompletionModel    *model,
                     GtkSourceCompletionProvider *provider)
{
	ProviderInfo *info = g_hash_table_lookup (model->priv->providers_info,
	                                          provider);

	if (info != NULL)
	{
		return !info->filtered;
	}
	else
	{
		return model->priv->visible_providers == NULL ||
		       g_list_index (model->priv->visible_providers, provider) != -1;
	}
}

/* Interface implementation */
static ProposalNode *
node_from_iter (GtkTreeIter *iter)
{
	return (ProposalNode *)(((GList *)iter->user_data)->data);
}

static GtkTreePath *
path_from_list (GtkSourceCompletionModel *model,
                GList                    *item)
{
	gint index = 0;
	GList *ptr;
	ProposalNode *node;
	
	ptr = model->priv->store;

	while (ptr && ptr != item)
	{
		node = (ProposalNode *)ptr->data;
	
		if (!node->filtered)
		{
			++index;
		}
	
		ptr = g_list_next (ptr);
	}
	
	if (ptr != item)
	{
		return NULL;
	}
	else
	{
		return gtk_tree_path_new_from_indices (index, -1);
	}
}

static GtkTreeModelFlags
tree_model_get_flags (GtkTreeModel *tree_model)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), 0);

	return 0;
}

static gint
tree_model_get_n_columns (GtkTreeModel *tree_model)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), 0);

	return GTK_SOURCE_COMPLETION_MODEL_N_COLUMNS;
}

static GType
tree_model_get_column_type (GtkTreeModel *tree_model,
			    gint          index)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), G_TYPE_INVALID);
	g_return_val_if_fail (index >= 0 && index < GTK_SOURCE_COMPLETION_MODEL_N_COLUMNS, G_TYPE_INVALID);

	return GTK_SOURCE_COMPLETION_MODEL (tree_model)->priv->column_types[index];
}

static gboolean
get_iter_from_index (GtkSourceCompletionModel *model,
                     GtkTreeIter              *iter,
                     gint                      index)
{
	GList *item;
	ProposalNode *node;

	if (index < 0 || index >= model->priv->num)
	{
		return FALSE;
	}

	item = model->priv->store;

	while (item != NULL && index >= 0)
	{
		node = (ProposalNode *)item->data;
	
		if (!node->filtered)
		{
			--index;
		}

		if (index != -1)
		{
			item = g_list_next (item);
		}
	}
	
	if (item != NULL)
	{
		iter->user_data = item;
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static gboolean
tree_model_get_iter (GtkTreeModel *tree_model,
		     GtkTreeIter  *iter, 
		     GtkTreePath  *path)
{
	GtkSourceCompletionModel *model;
	gint *indices;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (path != NULL, FALSE);
	
	model = GTK_SOURCE_COMPLETION_MODEL (tree_model);
	indices = gtk_tree_path_get_indices (path);
	
	return get_iter_from_index (model, iter, indices[0]);
}

static GtkTreePath *
tree_model_get_path (GtkTreeModel *tree_model,
		     GtkTreeIter  *iter)
{
	GtkSourceCompletionModel *model;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);

	model = GTK_SOURCE_COMPLETION_MODEL (tree_model);
	
	return path_from_list (model, (GList *)iter->user_data);
}

static void
tree_model_get_value (GtkTreeModel *tree_model,
		      GtkTreeIter  *iter, 
		      gint          column,
		      GValue       *value)
{
	ProposalNode *node;

	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (column >= 0 && column < GTK_SOURCE_COMPLETION_MODEL_N_COLUMNS);

	node = node_from_iter (iter);

	g_value_init (value, GTK_SOURCE_COMPLETION_MODEL (tree_model)->priv->column_types[column]);

	switch (column)
	{
		case GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROVIDER:
			g_value_set_object (value, node->provider);
			break;
		case GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROPOSAL:
			g_value_set_object (value, node->proposal);
			break;
		case GTK_SOURCE_COMPLETION_MODEL_COLUMN_LABEL:
			g_value_take_string (value, gtk_source_completion_proposal_get_label (node->proposal));
			break;
		case GTK_SOURCE_COMPLETION_MODEL_COLUMN_MARKUP:
			g_value_take_string (value, gtk_source_completion_proposal_get_markup (node->proposal));
			break;
		case GTK_SOURCE_COMPLETION_MODEL_COLUMN_ICON:
			if (node->proposal == NULL)
			{
				g_value_set_object (value, 
				                    (gpointer)gtk_source_completion_provider_get_icon (
				                    	node->provider));
			}
			else
			{
				g_value_set_object (value, 
				                    (gpointer)gtk_source_completion_proposal_get_icon (
				                    	node->proposal));
			}
			break;
	}
}

static gboolean
find_first_not_filtered (GtkSourceCompletionModel *model,
                         GList                    *item,
                         GtkTreeIter              *iter)
{
	ProposalNode *node;

	while (item)
	{
		node = (ProposalNode *)item->data;
	
		if (!node->filtered)
		{
			break;
		}
	
		item = g_list_next (item);
	}
	
	if (item != NULL)
	{
		iter->user_data = item;
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static gboolean
tree_model_iter_next (GtkTreeModel *tree_model,
		      GtkTreeIter  *iter)
{
	GList *item;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	
	item = g_list_next ((GList *)iter->user_data);
	
	return find_first_not_filtered (GTK_SOURCE_COMPLETION_MODEL (tree_model), 
	                                item, 
	                                iter);
}

static gboolean
tree_model_iter_children (GtkTreeModel *tree_model,
			  GtkTreeIter  *iter,
			  GtkTreeIter  *parent)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (parent == NULL || parent->user_data != NULL, FALSE);
	
	if (parent != NULL)
	{
		return FALSE;
	}
	else
	{
		GtkSourceCompletionModel *model = GTK_SOURCE_COMPLETION_MODEL (tree_model);
		return find_first_not_filtered (model,
		                                model->priv->store, iter);
	}
}

static gboolean
tree_model_iter_has_child (GtkTreeModel *tree_model,
			   GtkTreeIter  *iter)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	
	return FALSE;
}

static gint
tree_model_iter_n_children (GtkTreeModel *tree_model,
			    GtkTreeIter  *iter)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), 0);
	g_return_val_if_fail (iter == NULL || iter->user_data != NULL, 0);
	
	if (iter == NULL)
	{
		return GTK_SOURCE_COMPLETION_MODEL (tree_model)->priv->num;
	}
	else
	{
		return 0;
	}
}

static gboolean
tree_model_iter_nth_child (GtkTreeModel *tree_model,
			   GtkTreeIter  *iter,
			   GtkTreeIter  *parent, 
			   gint          n)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (parent == NULL || parent->user_data != NULL, FALSE);

	if (parent != NULL)
	{
		return FALSE;
	}
	else
	{
		return get_iter_from_index (GTK_SOURCE_COMPLETION_MODEL (tree_model), 
		                            iter, 
		                            n);
	}
}

static gboolean
tree_model_iter_parent (GtkTreeModel *tree_model,
			GtkTreeIter  *iter,
			GtkTreeIter  *child)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (child != NULL, FALSE);
	
	iter->user_data = NULL;
	return FALSE;
}

static void
tree_model_row_inserted (GtkTreeModel *tree_model,
			 GtkTreePath  *path,
			 GtkTreeIter  *iter)
{
}

static void
tree_model_row_deleted (GtkTreeModel *tree_model,
			 GtkTreePath  *path)
{
}

static void
tree_model_iface_init (gpointer g_iface, 
                       gpointer iface_data)
{
	GtkTreeModelIface *iface = (GtkTreeModelIface *)g_iface;

	iface->get_flags = tree_model_get_flags;
	iface->get_n_columns = tree_model_get_n_columns;
	iface->get_column_type = tree_model_get_column_type;
	iface->get_iter = tree_model_get_iter;
	iface->get_path = tree_model_get_path;
	iface->get_value = tree_model_get_value;
	iface->iter_next = tree_model_iter_next;
	iface->iter_children = tree_model_iter_children;
	iface->iter_has_child = tree_model_iter_has_child;
	iface->iter_n_children = tree_model_iter_n_children;
	iface->iter_nth_child = tree_model_iter_nth_child;
	iface->iter_parent = tree_model_iter_parent;
	
	iface->row_inserted = tree_model_row_inserted;
	iface->row_deleted = tree_model_row_deleted;
}

static void
proposal_node_free (ProposalNode *node)
{
	if (node->proposal != NULL)
	{
		if (node->changed_id != 0)
		{
			g_signal_handler_disconnect (node->proposal,
			                             node->changed_id);
		}
		
		g_object_unref (node->proposal);
	}		

	g_slice_free (ProposalNode, node);
}

static void
gtk_source_completion_model_dispose (GObject *object)
{
	GtkSourceCompletionModel *model = GTK_SOURCE_COMPLETION_MODEL (object);

	if (model->priv->providers_info != NULL)
	{
		g_hash_table_destroy (model->priv->providers_info);
		model->priv->providers_info = NULL;
	}
	
	g_list_foreach (model->priv->store, (GFunc)proposal_node_free, NULL);

	g_list_free (model->priv->store);
	model->priv->store = NULL;
	model->priv->last = NULL;
	
	g_list_free (model->priv->providers);
	model->priv->providers = NULL;
	
	G_OBJECT_CLASS (gtk_source_completion_model_parent_class)->dispose (object);
}

static void
gtk_source_completion_model_finalize (GObject *object)
{
	GtkSourceCompletionModel *model = GTK_SOURCE_COMPLETION_MODEL (object);

	g_list_free (model->priv->visible_providers);

	G_OBJECT_CLASS (gtk_source_completion_model_parent_class)->finalize (object);
}

static void
gtk_source_completion_model_class_init (GtkSourceCompletionModelClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	
	object_class->finalize = gtk_source_completion_model_finalize;
	object_class->dispose = gtk_source_completion_model_dispose;

	signals[PROVIDERS_CHANGED] =
		g_signal_new ("providers-changed",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		              G_STRUCT_OFFSET (GtkSourceCompletionModelClass, providers_changed),
		              NULL, 
		              NULL,
		              g_cclosure_marshal_VOID__VOID, 
		              G_TYPE_NONE,
		              0);

	signals[BEGIN_DELETE] =
		g_signal_new ("begin-delete",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		              G_STRUCT_OFFSET (GtkSourceCompletionModelClass, begin_delete),
		              NULL, 
		              NULL,
		              g_cclosure_marshal_VOID__VOID, 
		              G_TYPE_NONE,
		              0);

	signals[END_DELETE] =
		g_signal_new ("end-delete",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		              G_STRUCT_OFFSET (GtkSourceCompletionModelClass, end_delete),
		              NULL, 
		              NULL,
		              g_cclosure_marshal_VOID__VOID, 
		              G_TYPE_NONE,
		              0);

	g_type_class_add_private (object_class, sizeof(GtkSourceCompletionModelPrivate));
}

static void
provider_info_free (gpointer data)
{
	ProviderInfo *info = (ProviderInfo *)data;
	g_hash_table_destroy (info->proposals);

	g_slice_free (ProviderInfo, data);
}

static void
gtk_source_completion_model_init (GtkSourceCompletionModel *self)
{
	self->priv = GTK_SOURCE_COMPLETION_MODEL_GET_PRIVATE (self);
	
	self->priv->column_types[GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROVIDER] = G_TYPE_OBJECT;
	self->priv->column_types[GTK_SOURCE_COMPLETION_MODEL_COLUMN_PROPOSAL] = G_TYPE_OBJECT;
	self->priv->column_types[GTK_SOURCE_COMPLETION_MODEL_COLUMN_LABEL] = G_TYPE_STRING;
	self->priv->column_types[GTK_SOURCE_COMPLETION_MODEL_COLUMN_MARKUP] = G_TYPE_STRING;
	self->priv->column_types[GTK_SOURCE_COMPLETION_MODEL_COLUMN_ICON] = GDK_TYPE_PIXBUF;
	
	self->priv->providers_info = g_hash_table_new_full (g_direct_hash,
	                                                    g_direct_equal,
	                                                    g_object_unref,
	                                                    provider_info_free);
}

static void
num_inc (GtkSourceCompletionModel *model,
         ProviderInfo             *info,
         ProposalNode             *node)
{
	if (!node->filtered)
	{
		++model->priv->num;
	}

	if (node->proposal != NULL)
	{
		++info->num_proposals;
	}
}

static void
num_dec (GtkSourceCompletionModel *model,
         ProviderInfo             *info,
         ProposalNode             *node)
{
	if (!node->filtered)
	{
		--model->priv->num;
	}

	if (node->proposal != NULL && info->num_proposals > 0)
	{
		--info->num_proposals;
	}
}

/* Public */
GtkSourceCompletionModel*
gtk_source_completion_model_new (void)
{
	return g_object_new (GTK_TYPE_SOURCE_COMPLETION_MODEL, NULL);
}

static void
on_proposal_changed (GtkSourceCompletionProposal *proposal,
                     GList                       *item)
{
	GtkTreeIter iter;
	ProposalNode *node = (ProposalNode *)item->data;
	GtkTreePath *path;

	if (!node->filtered)
	{
		iter.user_data = node;
		path = path_from_list (node->model, item);

		gtk_tree_model_row_changed (GTK_TREE_MODEL (node->model),
		                            path,
		                            &iter);
		gtk_tree_path_free (path);
	}
}

void
gtk_source_completion_model_begin (GtkSourceCompletionModel *model,
                                   GList                    *providers)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model));

	if (providers != NULL)
	{
		model->priv->marking = !model->priv->marking;
		
		/* FIXME: maybe remove providers that are no longer selected now, but
		   since this is probably a atypical case, it might be a performance
		   hit (since providers have to be looked up in the GList). Anyway,
		   the providers are correctly removed by the marking process */
	}
	else
	{
		gtk_source_completion_model_clear (model);
	}
}

void
gtk_source_completion_model_cancel (GtkSourceCompletionModel *model)
{
	GList *item;
	
	/* If cancelled, mark all proposals correctly so that the fast marking
	   scheme still works */
	for (item = model->priv->store; item != NULL; item = g_list_next (item))
	{
		((ProposalNode *)item->data)->mark = model->priv->marking;
	}
}

static void
handle_row_inserted (GtkSourceCompletionModel  *model,
                     GList                     *item,
                     GtkTreePath              **path)
{
	GtkTreeIter iter;
	GtkTreePath *ppath = NULL;
	GtkTreeRowReference *ref = NULL;
	
	if (path != NULL)
	{
		ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), *path);
	}
	else
	{
		ppath = path_from_list (model, item);
	}
	
	iter.user_data = item;

	gtk_tree_model_row_inserted (GTK_TREE_MODEL (model),
	                             path ? *path : ppath,
	                             &iter);

	if (ref != NULL)
	{
		gtk_tree_path_free (*path);
		*path = gtk_tree_row_reference_get_path (ref);
		gtk_tree_row_reference_free (ref);
		
		gtk_tree_path_prev (*path);
	}
	else
	{
		gtk_tree_path_free (ppath);
	}
}

static void
insert_node (GtkSourceCompletionModel     *model,
             ProviderInfo                 *info,
             GList                        *position,
             GtkSourceCompletionProposal  *proposal,
             GtkTreePath                 **path)
{
	ProposalNode *node = g_slice_new (ProposalNode);
	GList *item;

	node->model = model;
	node->proposal = proposal ? g_object_ref (proposal) : NULL;
	node->provider = info->provider;
	node->changed_id = 0;
	node->mark = model->priv->marking;
	node->filtered = info->filtered || (!proposal && !model->priv->show_headers);
	
	if (position == NULL)
	{
		/* Append after last item */
		if (model->priv->store == NULL)
		{
			model->priv->store = model->priv->last = g_list_append (NULL,
			                                                        node);
		}
		else
		{
			model->priv->last = g_list_append (model->priv->last, node);
			model->priv->last = g_list_next (model->priv->last);
		}
		
		info->last = model->priv->last;
		
		if (info->first == NULL)
		{
			info->first = info->last;
		}
		
		item = model->priv->last;
	}
	else
	{
		/* Insert before item 'position' */
		model->priv->store = g_list_insert_before (model->priv->store,
		                                           position,
		                                           node);
		
		item = g_list_previous (position);
		
		if (!info->first || info->first == position)
		{
			info->first = item;
		}
		
		if (!info->last || info->last->next == item)
		{
			info->last = item;
		}
	}
	
	num_inc (model, info, node);
	
	if (proposal != NULL)
	{
		g_hash_table_insert (info->proposals, proposal, item);
	}

	if (!node->filtered)
	{
		handle_row_inserted (model, item, path);
	}
	
	if (proposal != NULL)
	{
		node->changed_id = g_signal_connect (node->proposal,
		                                     "changed",
		                                     G_CALLBACK (on_proposal_changed),
		                                     item);
	}
}

static void
handle_row_deleted (GtkSourceCompletionModel  *model,
                    GList                     *item,
                    GtkTreePath              **path)
{
	GtkTreePath *ppath = NULL;

	if (path == NULL)
	{
		ppath = path_from_list (model, item);
	}
	else
	{
		/* Create a copy here because row_deleted might modify it */
		ppath = gtk_tree_path_copy (*path);
	}
	
	gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), ppath);
	gtk_tree_path_free (ppath);
}

static void
remove_node (GtkSourceCompletionModel  *model,
             ProviderInfo              *info,
             GList                     *item,
             GtkTreePath              **path)
{
	ProposalNode *node = (ProposalNode *)item->data;
	GtkTreePath *ppath = NULL;

	if (item == info->first)
	{
		if (info->first != info->last)
		{
			info->first = g_list_next (info->first);
		}
		else
		{
			info->first = info->last = NULL;
		}
	}
	else if (item == info->last)
	{
		info->last = g_list_previous (info->last);
	}
	
	if (item == model->priv->last)
	{
		model->priv->last = g_list_previous (item);
	}
	
	num_dec (model, info, node);

	if (path == NULL)
	{
		ppath = path_from_list (model, item);
	}
	
	if (node->proposal != NULL)
	{
		g_hash_table_remove (info->proposals, node->proposal);
	}

	model->priv->store = g_list_delete_link (model->priv->store,
	                                         item);	
	
	handle_row_deleted (model, item, path ? path : &ppath);	
	
	if (ppath != NULL)
	{
		gtk_tree_path_free (ppath);
	}

	proposal_node_free (node);
}

static void
update_header_visibility_each (GtkSourceCompletionProvider *provider,
                               ProviderInfo                *info,
                               GtkSourceCompletionModel    *model)
{
	ProposalNode *header_node = info->first->data;

	/* Check if the header is already in the correct visibility state */
	if (info->filtered || model->priv->show_headers != header_node->filtered)
	{
		return;
	}

	if (model->priv->show_headers)
	{
		header_node->filtered = FALSE;
		handle_row_inserted (model, info->first, NULL);
	}
	else
	{
		header_node->filtered = TRUE;
		handle_row_deleted (model, info->first, NULL);
	}
}

static void
update_header_visibility (GtkSourceCompletionModel *model)
{
	g_hash_table_foreach (model->priv->providers_info,
	                      (GHFunc)update_header_visibility_each,
	                      model);
}

static void
update_provider_visibility_show_hide (GtkSourceCompletionModel *model,
                                      ProviderInfo             *info,
                                      gboolean                  show)
{
	GList *item;
	GtkTreePath *path = NULL;

	item = info->first;
	info->filtered = !show;

	while (item)
	{
		ProposalNode *node = (ProposalNode *)item->data;

		node->filtered = !show;

		if (path == NULL)
		{
			path = path_from_list (model, item);
		}

		if (show)
		{
			++model->priv->num;

			handle_row_inserted (model, item, &path);
			gtk_tree_path_next (path);
		}
		else
		{
			--model->priv->num;
			handle_row_deleted (model, item, &path);
		}

		if (item == info->last)
		{
			break;
		}

		item = g_list_next (item);
	}
	
	if (path != NULL)
	{
		gtk_tree_path_free (path);
	}
}

static void
update_provider_visibility_each (GtkSourceCompletionProvider *provider,
                                 ProviderInfo                *info,
                                 GtkSourceCompletionModel    *model)
{
	if (info->filtered == (model->priv->visible_providers != NULL && 
	                       g_list_index (model->priv->visible_providers, info->provider) == -1))
	{
		return;
	}

	update_provider_visibility_show_hide (model, info, info->filtered);
}

static void
update_provider_visibility (GtkSourceCompletionModel *model)
{
	g_hash_table_foreach (model->priv->providers_info,
	                      (GHFunc)update_provider_visibility_each,
	                      model);
}

static gboolean
remove_unmarked (GtkSourceCompletionModel    *model,
                 GtkSourceCompletionProvider *provider)
{
	GList *item;
	gboolean ret = TRUE;
	GtkTreePath *path = NULL;
	ProviderInfo *info = g_hash_table_lookup (model->priv->providers_info, 
	                                          provider);

	if (!info)
	{
		return FALSE;
	}
	
	g_signal_emit (model, signals[BEGIN_DELETE], 0);
	
	item = info->first;

	while (item)
	{
		ProposalNode *node = (ProposalNode *)item->data;
		
		if (node->provider != provider)
		{
			break;
		}
		
		if (path == NULL)
		{
			path = path_from_list (model, item);
		}
	
		if (node->proposal != NULL && node->mark != model->priv->marking)
		{
			GList *next = g_list_next (item);
			
			// Remove the node here
			remove_node (model, info, item, &path);
			item = next;
		}
		else
		{
			gtk_tree_path_next (path);
			item = g_list_next (item);
		}
	}
	
	if (path != NULL)
	{
		gtk_tree_path_free (path);
	}
	
	if (info->num_proposals == 0 && info->first != NULL && model->priv->show_headers)
	{
		remove_node (model, info, info->first, NULL);
	}
	
	if (info->num_proposals == 0)
	{
		g_hash_table_remove (model->priv->providers_info, provider);
		
		model->priv->providers = g_list_remove (model->priv->providers,
		                                        provider);

		model->priv->visible_providers = g_list_remove (model->priv->visible_providers,
		                                                provider);
		
		ret = FALSE;
	}
	
	g_signal_emit (model, signals[END_DELETE], 0);
	return ret;
}

static GList *
insert_provider (GtkSourceCompletionModel    *model,
                 GtkSourceCompletionProvider *provider)
{
	GList *providers = model->priv->providers;
	gint priority;
	GList *last = NULL;

	/* We do this manually to at the same time determine the position
	   at which the new provider is inserted */
	if (providers == NULL)
	{
		model->priv->providers = g_list_prepend (NULL, provider);
		return model->priv->providers;
	}

	priority = gtk_source_completion_provider_get_priority (provider);

	while (providers)
	{
		GtkSourceCompletionProvider *current = providers->data;
		gint current_prio = gtk_source_completion_provider_get_priority (current);

		if (priority >= current_prio)
		{
			/* Insert before */
			model->priv->providers = g_list_insert_before (model->priv->providers,
			                                               providers,
			                                               provider);

			return providers->prev;
		}

		last = providers;
		providers = g_list_next (providers);
	}

	/* Insert after */
	last = g_list_append (last, provider);
	return last->next;
}

static ProviderInfo *
add_provider_info (GtkSourceCompletionModel    *model,
                   GtkSourceCompletionProvider *provider)
{
	ProviderInfo *info;
	GList *pos;
	GList *before = NULL;

	info = g_slice_new0 (ProviderInfo);
	info->provider = provider;
	info->proposals = g_hash_table_new ((GHashFunc)gtk_source_completion_proposal_hash,
	                                    (GEqualFunc)gtk_source_completion_proposal_equal);

	info->filtered = !provider_is_visible (model, provider);
	info->first_batch = TRUE;

	g_hash_table_insert (model->priv->providers_info,
	                     g_object_ref (provider),
	                     info);

	/* Insert the provider sorted on the priority */
	pos = insert_provider (model, provider);

	/* Insert the header node */
	if (pos->next)
	{
		ProviderInfo *next = g_hash_table_lookup (model->priv->providers_info,
		                                          pos->next->data);

		if (next)
		{
			before = next->first;
		}
	}

	insert_node (model, info, before, NULL, NULL);
	return info;
}

void
gtk_source_completion_model_append (GtkSourceCompletionModel    *model,
                                    GtkSourceCompletionProvider *provider,
                                    GList                       *proposals)
{
	GList *item;
	ProviderInfo *info;
	GtkTreePath *path = NULL;
	gboolean is_new_provider = FALSE;
	
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model));
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider));

	if (proposals == NULL || !GTK_IS_SOURCE_COMPLETION_PROPOSAL (proposals->data))
	{
		return;
	}

	info = g_hash_table_lookup (model->priv->providers_info, provider);

	if (!info)
	{
		/* First batch for 'provider', add provider info */
		info = add_provider_info (model, provider);
		is_new_provider = TRUE;
	}
	
	if (info->first_batch)
	{
		info->ptr = info->first;
		
		if (info->ptr && !((ProposalNode *)info->ptr->data)->proposal)
		{
			info->ptr = g_list_next (info->ptr);
		}
	}
	
	info->first_batch = FALSE;
	
	for (item = proposals; item != NULL; item = g_list_next (item))
	{
		GtkSourceCompletionProposal *proposal;
		GList *nodeitem;
		
		if (!GTK_IS_SOURCE_COMPLETION_PROPOSAL (item->data))
		{
			continue;
		}
		
		proposal = GTK_SOURCE_COMPLETION_PROPOSAL (item->data);
		nodeitem = g_hash_table_lookup (info->proposals, proposal);

		if (nodeitem)
		{
			ProposalNode *node = (ProposalNode *)nodeitem->data;
			node->mark = model->priv->marking;
			
			/* Next items will be inserted after this one */
			info->ptr = g_list_next (nodeitem);
			
			if (path != NULL)
			{
				gtk_tree_path_free (path);
				path = NULL;
			}
		}
		else
		{
			GList *insert_before = info->ptr;

			if (path == NULL)
			{
				if (insert_before)
				{
					path = path_from_list (model, insert_before);
				}
				else
				{
					path = gtk_tree_path_new_from_indices (model->priv->num, -1);
				}
			}
			
			insert_node (model, info, insert_before, proposal, &path);
			gtk_tree_path_next (path);
		}
	}
	
	if (path != NULL)
	{
		gtk_tree_path_free (path);
	}
	
	if (is_new_provider)
	{
		g_signal_emit (model, signals[PROVIDERS_CHANGED], 0);
	}
}

void
gtk_source_completion_model_end (GtkSourceCompletionModel    *model,
                                 GtkSourceCompletionProvider *provider)
{
	/* Remove unmarked proposals, returns TRUE if there are any proposals
	 * left for 'provider'. If so, we add 'provider' to the list of 
	 * currently active providers
	 */
	if (!remove_unmarked (model, provider))
	{
		model->priv->providers = g_list_remove (model->priv->providers,
		                                        provider);

		g_signal_emit (model, signals[PROVIDERS_CHANGED], 0);
	}
	else
	{
		ProviderInfo *info = g_hash_table_lookup (model->priv->providers_info,
		                                          provider);
		info->first_batch = TRUE;
	}
}

void
gtk_source_completion_model_clear (GtkSourceCompletionModel *model)
{
	GtkTreePath *path;
	ProviderInfo *info = NULL;
	
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model));
	
	path = gtk_tree_path_new_first ();
	
	while (model->priv->store)
	{
		ProposalNode *node;

		node = (ProposalNode *)model->priv->store->data;

		model->priv->store = g_list_delete_link (model->priv->store, model->priv->store);
		
		if (model->priv->store == NULL)
		{
			model->priv->last = NULL;
		}
		
		if (info == NULL || info->provider != node->provider)
		{
			info = g_hash_table_lookup (model->priv->providers_info, node->provider);
		}

		num_dec (model, info, node);
		
		if (!node->filtered)
		{
			gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), path);
		}

		proposal_node_free (node);
	}

	gtk_tree_path_free (path);
	
	g_hash_table_remove_all (model->priv->providers_info);
	g_list_free (model->priv->providers);
	
	model->priv->providers = NULL;

	g_list_free (model->priv->visible_providers);
	model->priv->visible_providers = NULL;
	
	g_signal_emit (model, signals[PROVIDERS_CHANGED], 0);
}

static void
provider_has_proposals (GtkSourceCompletionProvider *provider,
                        ProviderInfo                *info,
                        gboolean                    *isempty)
{
	if (info->num_proposals != 0)
	{
		*isempty = FALSE;
	}
}

gboolean
gtk_source_completion_model_is_empty (GtkSourceCompletionModel *model,
                                      gboolean                  invisible)
{
	gboolean isempty = TRUE;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), FALSE);

	if (!invisible)
	{
		g_hash_table_foreach (model->priv->providers_info,
		                      (GHFunc)provider_has_proposals,
		                      &isempty);
	}
	else if (model->priv->num != 0)
	{
		isempty = FALSE;
	}

	return isempty;
}

guint
gtk_source_completion_model_n_proposals (GtkSourceCompletionModel    *model,
                                         GtkSourceCompletionProvider *provider)
{
	ProviderInfo *info;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), 0);
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_PROVIDER (provider), 0);
	
	info = g_hash_table_lookup (model->priv->providers_info, provider);
	
	if (info == NULL)
	{
		return 0;
	}
	else
	{
		return info->num_proposals;
	}
}

void 
gtk_source_completion_model_set_show_headers (GtkSourceCompletionModel *model,
                                              gboolean                  show_headers)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model));
	
	if (model->priv->show_headers != show_headers)
	{
		model->priv->show_headers = show_headers;
		update_header_visibility (model);
	}
}

gboolean
gtk_source_completion_model_iter_is_header (GtkSourceCompletionModel *model,
                                            GtkTreeIter              *iter)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);

	return node_from_iter (iter)->proposal == NULL;
}

gboolean
gtk_source_completion_model_iter_previous (GtkSourceCompletionModel *model,
                                           GtkTreeIter              *iter)
{
	GList *item;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	
	item = iter->user_data;
	
	do
	{
		item = g_list_previous (item);
	} while (item && ((ProposalNode *)item->data)->filtered);
	
	if (item != NULL)
	{
		iter->user_data = item;
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

gboolean
gtk_source_completion_model_iter_last (GtkSourceCompletionModel *model,
                                       GtkTreeIter              *iter)
{
	GList *item;
	
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	
	item = model->priv->last;
	iter->user_data = item;

	if (!((ProposalNode *)item->data)->filtered)
	{
		return TRUE;
	}
	else if (item != NULL)
	{
		return gtk_source_completion_model_iter_previous (model, iter);
	}
	else
	{
		return FALSE;
	}
}

GList *
gtk_source_completion_model_get_providers (GtkSourceCompletionModel *model)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), NULL);
	
	return model->priv->providers;
}

void 
gtk_source_completion_model_set_visible_providers (GtkSourceCompletionModel *model,
                                                   GList                    *providers)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model));
	
	g_list_free (model->priv->visible_providers);
	model->priv->visible_providers = g_list_copy (providers);
	
	update_provider_visibility (model);
}

GList *
gtk_source_completion_model_get_visible_providers (GtkSourceCompletionModel *model)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), NULL);
	
	return model->priv->visible_providers;
}

gboolean
gtk_source_completion_model_iter_equal (GtkSourceCompletionModel *model,
                                        GtkTreeIter              *iter1,
                                        GtkTreeIter              *iter2)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_MODEL (model), FALSE);
	
	return iter1->user_data == iter2->user_data;
}
