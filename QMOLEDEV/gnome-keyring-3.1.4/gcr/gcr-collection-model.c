/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the
 * Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "config.h"

#include "gcr-collection-model.h"

#include <gtk/gtk.h>

#include <string.h>
#include <unistd.h>

/**
 * SECTION:gcr-collection-model
 * @title: GcrCollectionModel
 * @short_description: A GtkTreeModel that represents a collection
 *
 * This is an implementation of #GtkTreeModel which represents the objects in
 * the a #GcrCollection. As objects are added or removed from the collection,
 * rows are added and removed from this model.
 *
 * The row values come from the properties of the objects in the collection. Use
 * gcr_collection_model_new() to create a new collection model. To have more
 * control over the values use a set of #GcrColumn structures to define the
 * columns. This can be done with gcr_collection_model_new_full() or
 * gcr_collection_model_set_columns().
 *
 * Each row can have a selected state, which is represented by a boolean column.
 * The selected state can be toggled with gcr_collection_model_toggle_selected()
 * or set with gcr_collection_model_set_selected_objects() and retrieved with
 * gcr_collection_model_get_selected_objects().
 *
 * To determine which object a row represents and vice versa, use the
 * gcr_collection_model_iter_for_object() or gcr_collection_model_object_for_iter()
 * functions.
 */

/**
 * GcrCollectionModel:
 * @parent: The parent object
 *
 * A #GtkTreeModel which contains a row for each object in a #GcrCollection.
 */

/**
 * GcrCollectionModelClass:
 * @parent_class: The parent class
 *
 * The class for #GcrCollectionModel.
 */

#define COLLECTION_MODEL_STAMP 0xAABBCCDD

enum {
	PROP_0,
	PROP_COLLECTION,
	PROP_COLUMNS
};

struct _GcrCollectionModelPrivate {
	GcrCollection *collection;
	GHashTable *selected;
	GSequence *objects;

	const GcrColumn *columns;
	guint n_columns;
};

/* Forward declarations */
static void gcr_collection_model_tree_model_init (GtkTreeModelIface *iface);

G_DEFINE_TYPE_WITH_CODE (GcrCollectionModel, gcr_collection_model, G_TYPE_OBJECT,
	G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_MODEL, gcr_collection_model_tree_model_init);
);

#define UNUSED_VALUE GINT_TO_POINTER (1)

static GHashTable*
selected_hash_table_new (void)
{
	return g_hash_table_new (g_direct_hash, g_direct_equal);
}

static gint
on_sequence_compare (gconstpointer a, gconstpointer b, gpointer user_data)
{
	return (GObject*)a - (GObject*)b;
}

static gint
index_for_iter (GcrCollectionModel *self, const GtkTreeIter *iter)
{
	GSequenceIter *seq;
	gint index;

	g_return_val_if_fail (iter, -1);
	g_return_val_if_fail (iter->stamp == COLLECTION_MODEL_STAMP, -1);

	seq = iter->user_data2;
	g_return_val_if_fail (g_sequence_iter_get_sequence (seq) ==
	                      self->pv->objects, -1);

	index = g_sequence_iter_get_position (seq);
	g_assert (index >= 0 && index < g_sequence_get_length (self->pv->objects));
	return index;
}

static gboolean
iter_for_seq (GcrCollectionModel *self, GSequenceIter *seq, GtkTreeIter *iter)
{
	GObject *object;

	object = g_sequence_get (seq);
	g_return_val_if_fail (G_IS_OBJECT (object), FALSE);

	memset (iter, 0, sizeof (*iter));
	iter->stamp = COLLECTION_MODEL_STAMP;
	iter->user_data = object;
	iter->user_data2 = seq;
	return TRUE;
}

static gboolean
iter_for_index (GcrCollectionModel *self, gint index, GtkTreeIter *iter)
{
	GSequenceIter *seq;

	if (index < 0 || index >= g_sequence_get_length (self->pv->objects))
		return FALSE;

	seq = g_sequence_get_iter_at_pos (self->pv->objects, index);
	return iter_for_seq (self, seq, iter);
}

static gint
index_for_object (GcrCollectionModel *self, GObject *object)
{
	GSequenceIter *seq;

	seq = g_sequence_lookup (self->pv->objects, object,
	                         on_sequence_compare, NULL);
	if (seq == NULL)
		return -1;

	return g_sequence_iter_get_position (seq);
}

static void
on_object_notify (GObject *object, GParamSpec *spec, GcrCollectionModel *self)
{
	GtkTreeIter iter;
	GtkTreePath *path;
	gboolean found = FALSE;
	guint i;

	g_return_if_fail (spec->name);

	for (i = 0; i < self->pv->n_columns - 1; ++i) {
		g_assert (self->pv->columns[i].property_name);
		if (g_str_equal (self->pv->columns[i].property_name, spec->name)) {
			found = TRUE;
			break;
		}
	}

	/* Tell the tree view that this row changed */
	if (found) {
		if (!gcr_collection_model_iter_for_object (self, object, &iter))
			g_return_if_reached ();
		path = gtk_tree_model_get_path (GTK_TREE_MODEL (self), &iter);
		g_return_if_fail (path);
		gtk_tree_model_row_changed (GTK_TREE_MODEL (self), path, &iter);
		gtk_tree_path_free (path);
	}
}

static void
on_object_gone (gpointer unused, GObject *was_object)
{
	g_warning ("object contained in GcrCollection and included in GcrCollectionModel "
	           "was destroyed before it was removed from the collection");
}

static void
on_collection_added (GcrCollection *collection, GObject *object, GcrCollectionModel *self)
{
	GSequenceIter *seq;
	GtkTreeIter iter;
	GtkTreePath *path;

	g_return_if_fail (GCR_COLLECTION_MODEL (self));
	g_return_if_fail (G_IS_OBJECT (object));

	g_assert (GCR_IS_COLLECTION_MODEL (self));
	g_assert (G_IS_OBJECT (object));

	seq = g_sequence_insert_sorted (self->pv->objects, object,
	                                on_sequence_compare, self);
	g_object_weak_ref (G_OBJECT (object), (GWeakNotify)on_object_gone, self);
	g_signal_connect (object, "notify", G_CALLBACK (on_object_notify), self);

	/* Fire signal for this added row */
	if (!iter_for_seq (self, seq, &iter))
		g_assert_not_reached ();

	path = gtk_tree_path_new_from_indices (g_sequence_iter_get_position (seq), -1);

	gtk_tree_model_row_inserted (GTK_TREE_MODEL (self), path, &iter);
	gtk_tree_path_free (path);
}

static void
disconnect_object (GcrCollectionModel *self, GObject *object)
{
	g_object_weak_unref (G_OBJECT (object), on_object_gone, self);
	g_signal_handlers_disconnect_by_func (object, on_object_notify, self);
}

static void
on_collection_removed (GcrCollection *collection, GObject *object,
                       GcrCollectionModel *self)
{
	GtkTreePath *path;
	GSequenceIter *seq;

	g_return_if_fail (GCR_COLLECTION_MODEL (self));
	g_return_if_fail (G_IS_OBJECT (object));

	seq = g_sequence_lookup (self->pv->objects, object, on_sequence_compare, NULL);
	g_return_if_fail (seq != NULL);

	path = gtk_tree_path_new_from_indices (g_sequence_iter_get_position (seq), -1);

	disconnect_object (self, object);

	g_hash_table_remove (self->pv->selected, object);
	g_sequence_remove (seq);

	/* Fire signal for this removed row */
	gtk_tree_model_row_deleted (GTK_TREE_MODEL (self), path);
	gtk_tree_path_free (path);
}

static void
populate_model (GcrCollectionModel *self)
{
	GList *objects, *l;
	objects = gcr_collection_get_objects (self->pv->collection);
	for (l = objects; l; l = g_list_next (l))
		on_collection_added (self->pv->collection, G_OBJECT (l->data), self);
	g_list_free (objects);
}

static void
free_owned_columns (gpointer data)
{
	GcrColumn *columns;
	g_assert (data);

	/* Only the property column is in use */
	for (columns = data; columns->property_name; ++columns)
		g_free ((gchar*)columns->property_name);
	g_free (data);
}

static GtkTreeModelFlags
gcr_collection_model_real_get_flags (GtkTreeModel *model)
{
	return GTK_TREE_MODEL_ITERS_PERSIST;
}

static gint
gcr_collection_model_real_get_n_columns (GtkTreeModel *model)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	return self->pv->n_columns;
}

static GType
gcr_collection_model_real_get_column_type (GtkTreeModel *model, gint column_id)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	g_return_val_if_fail (column_id >= 0 && column_id <= self->pv->n_columns, 0);

	/* The last is the selected column */
	if (column_id == self->pv->n_columns)
		return G_TYPE_BOOLEAN;

	return self->pv->columns[column_id].column_type;
}

static gboolean
gcr_collection_model_real_get_iter (GtkTreeModel *model, GtkTreeIter *iter, GtkTreePath *path)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	const gint *indices;
	gint count;

	count = gtk_tree_path_get_depth (path);
	if (count != 1)
		return FALSE;

	indices = gtk_tree_path_get_indices (path);
	return iter_for_index (self, indices[0], iter);
}

static GtkTreePath*
gcr_collection_model_real_get_path (GtkTreeModel *model, GtkTreeIter *iter)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	GtkTreePath *path;
	gint index;

	index = index_for_iter (self, iter);
	g_return_val_if_fail (index >= 0, NULL);

	path = gtk_tree_path_new ();
	gtk_tree_path_prepend_index (path, index);
	return path;
}

static void
gcr_collection_model_real_get_value (GtkTreeModel *model, GtkTreeIter *iter,
                                     gint column_id, GValue *value)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	GObject *object;
	GValue original;
	const GcrColumn *column;

	object = gcr_collection_model_object_for_iter (self, iter);
	g_return_if_fail (G_IS_OBJECT (object));
	g_return_if_fail (column_id >= 0 && column_id < self->pv->n_columns);

	/* The selected column? Last one */
	if (column_id == self->pv->n_columns - 1) {
		g_value_init (value, G_TYPE_BOOLEAN);
		g_value_set_boolean (value, gcr_collection_model_is_selected (self, iter));
		return;
	}

	/* Figure out which property */
	column = &self->pv->columns[column_id];
	g_assert (column->property_name);
	g_value_init (value, column->column_type);

	/* A transformer is specified, or mismatched types */
	if (column->transformer || column->column_type != column->property_type) {
		memset (&original, 0, sizeof (original));
		g_value_init (&original, column->property_type);
		g_object_get_property (object, column->property_name, &original);

		if (column->transformer) {
			(column->transformer) (&original, value);
		} else {
			g_warning ("%s property of %s class was of type %s instead of type %s"
			           " and cannot be converted due to lack of transformer",
			           column->property_name, G_OBJECT_TYPE_NAME (object),
			           g_type_name (column->property_type),
			           g_type_name (column->column_type));
		}

	/* Simple, no transformation necessary */
	} else {
		g_object_get_property (object, column->property_name, value);
	}
}

static gboolean
gcr_collection_model_real_iter_next (GtkTreeModel *model, GtkTreeIter *iter)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	gint index;

	index = index_for_iter (self, iter);
	g_return_val_if_fail (index >= 0, FALSE);

	return iter_for_index (self, index + 1, iter);
}

static gboolean
gcr_collection_model_real_iter_children (GtkTreeModel *model, GtkTreeIter *iter, GtkTreeIter *parent)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);

	if (parent != NULL)
		return FALSE;

	return iter_for_index (self, 0, iter);
}

static gboolean
gcr_collection_model_real_iter_has_child (GtkTreeModel *model, GtkTreeIter *iter)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	if (iter == NULL)
		return !g_sequence_iter_is_end (g_sequence_get_begin_iter (self->pv->objects));
	return FALSE;
}

static gint
gcr_collection_model_real_iter_n_children (GtkTreeModel *model, GtkTreeIter *iter)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	if (iter == NULL)
		return g_sequence_get_length (self->pv->objects);
	return 0;
}

static gboolean
gcr_collection_model_real_iter_nth_child (GtkTreeModel *model, GtkTreeIter *iter,
                                          GtkTreeIter *parent, gint n)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (model);
	if (parent != NULL)
		return FALSE;
	return iter_for_index (self, n, iter);
}

static gboolean
gcr_collection_model_real_iter_parent (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreeIter *child)
{
	return FALSE;
}

static void
gcr_collection_model_real_ref_node (GtkTreeModel *model, GtkTreeIter *iter)
{
	/* Nothing to do */
}

static void
gcr_collection_model_real_unref_node (GtkTreeModel *model, GtkTreeIter *iter)
{
	/* Nothing to do */
}

static void
gcr_collection_model_tree_model_init (GtkTreeModelIface *iface)
{
	iface->get_flags = gcr_collection_model_real_get_flags;
	iface->get_n_columns = gcr_collection_model_real_get_n_columns;
	iface->get_column_type = gcr_collection_model_real_get_column_type;
	iface->get_iter = gcr_collection_model_real_get_iter;
	iface->get_path = gcr_collection_model_real_get_path;
	iface->get_value = gcr_collection_model_real_get_value;
	iface->iter_next = gcr_collection_model_real_iter_next;
	iface->iter_children = gcr_collection_model_real_iter_children;
	iface->iter_has_child = gcr_collection_model_real_iter_has_child;
	iface->iter_n_children = gcr_collection_model_real_iter_n_children;
	iface->iter_nth_child = gcr_collection_model_real_iter_nth_child;
	iface->iter_parent = gcr_collection_model_real_iter_parent;
	iface->ref_node = gcr_collection_model_real_ref_node;
	iface->unref_node = gcr_collection_model_real_unref_node;
}

static void
gcr_collection_model_init (GcrCollectionModel *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_COLLECTION_MODEL, GcrCollectionModelPrivate);

	self->pv->objects = g_sequence_new (NULL);
	self->pv->selected = NULL;
	self->pv->columns = NULL;
	self->pv->n_columns = 0;
}

static void
gcr_collection_model_set_property (GObject *object, guint prop_id,
                                   const GValue *value, GParamSpec *pspec)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (object);
	GcrColumn *columns;

	switch (prop_id) {
	case PROP_COLLECTION:
		g_return_if_fail (self->pv->collection == NULL);
		self->pv->collection = g_value_dup_object (value);
		if (self->pv->collection) {
			g_signal_connect_after (self->pv->collection, "added", G_CALLBACK (on_collection_added), self);
			g_signal_connect_after (self->pv->collection, "removed", G_CALLBACK (on_collection_removed), self);
			populate_model (self);
		}
		break;

	case PROP_COLUMNS:
		columns = g_value_get_pointer (value);
		if (columns)
			gcr_collection_model_set_columns (self, columns);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
gcr_collection_model_get_property (GObject *object, guint prop_id,
                                   GValue *value, GParamSpec *pspec)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (object);

	switch (prop_id) {
	case PROP_COLLECTION:
		g_value_set_object (value, self->pv->collection);
		break;

	case PROP_COLUMNS:
		g_value_set_pointer (value, (gpointer)self->pv->columns);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
gcr_collection_model_dispose (GObject *object)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (object);
	GSequenceIter *seq;
	GSequenceIter *next;

	/* Disconnect from all rows */
	for (seq = g_sequence_get_begin_iter (self->pv->objects);
	     !g_sequence_iter_is_end (seq); seq = next) {
		next = g_sequence_iter_next (seq);
		disconnect_object (self, g_sequence_get (seq));
		g_sequence_remove (seq);
	}

	/* Disconnect from the collection */
	if (self->pv->collection) {
		g_signal_handlers_disconnect_by_func (self->pv->collection, on_collection_added, self);
		g_signal_handlers_disconnect_by_func (self->pv->collection, on_collection_removed, self);
		g_object_unref (self->pv->collection);
		self->pv->collection = NULL;
	}

	if (self->pv->selected)
		g_hash_table_remove_all (self->pv->selected);

	G_OBJECT_CLASS (gcr_collection_model_parent_class)->dispose (object);
}

static void
gcr_collection_model_finalize (GObject *object)
{
	GcrCollectionModel *self = GCR_COLLECTION_MODEL (object);

	g_assert (!self->pv->collection);

	g_assert (self->pv->objects);
	g_assert (g_sequence_get_length (self->pv->objects) == 0);
	g_sequence_free (self->pv->objects);
	self->pv->objects = NULL;

	if (self->pv->selected)
		g_hash_table_destroy (self->pv->selected);
	self->pv->selected = NULL;

	self->pv->columns = NULL;

	G_OBJECT_CLASS (gcr_collection_model_parent_class)->finalize (object);
}

static void
gcr_collection_model_class_init (GcrCollectionModelClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	gcr_collection_model_parent_class = g_type_class_peek_parent (klass);

	gobject_class->dispose = gcr_collection_model_dispose;
	gobject_class->finalize = gcr_collection_model_finalize;
	gobject_class->set_property = gcr_collection_model_set_property;
	gobject_class->get_property = gcr_collection_model_get_property;

	g_object_class_install_property (gobject_class, PROP_COLLECTION,
		g_param_spec_object ("collection", "Object Collection", "Collection to get objects from",
		                     GCR_TYPE_COLLECTION, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_COLUMNS,
		g_param_spec_pointer ("columns", "Columns", "Columns for the model",
		                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (klass, sizeof (GcrCollectionModelPrivate));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_collection_model_new:
 * @collection: The collection to represent
 * @...: The column names and types.
 *
 * Create a new #GcrCollectionModel. The variable argument list should contain
 * pairs of property names, and #GType values. The variable argument list should
 * be terminated with %NULL.
 *
 * Returns: A newly allocated model, which should be released with g_object_unref().
 */
GcrCollectionModel*
gcr_collection_model_new (GcrCollection *collection, ...)
{
	GcrColumn column;
	GcrCollectionModel *self;
	const gchar *arg;
	GArray *array;
	va_list va;

	/* With a null terminator */
	array = g_array_new (TRUE, TRUE, sizeof (GcrColumn));

	va_start (va, collection);
	while ((arg = va_arg (va, const gchar*)) != NULL) {
		memset (&column, 0, sizeof (column));
		column.property_name = g_strdup (arg);
		column.property_type = va_arg (va, GType);
		column.column_type = column.property_type;
		g_array_append_val (array, column);
	}
	va_end (va);

	self = gcr_collection_model_new_full (collection, (GcrColumn*)array->data);
	g_object_set_data_full (G_OBJECT (self), "gcr_collection_model_new",
	                        g_array_free (array, FALSE), free_owned_columns);
	return self;
}

/**
 * gcr_collection_model_new_full:
 * @collection: The collection to represent
 * @columns: The columns the model should contain
 *
 * Create a new #GcrCollectionModel.
 *
 * Returns: A newly allocated model, which should be released with g_object_unref().
 */
GcrCollectionModel*
gcr_collection_model_new_full (GcrCollection *collection, const GcrColumn *columns)
{
	GcrCollectionModel *self = g_object_new (GCR_TYPE_COLLECTION_MODEL, "collection", collection, NULL);
	gcr_collection_model_set_columns (self, columns);
	return self;
}

/**
 * gcr_collection_model_set_columns:
 * @self: The model
 * @columns: The columns the model should contain
 *
 * Set the columns that the model should contain. @columns is an array of
 * #GcrColumn structures, with the last one containing %NULL for all values.
 *
 * This function can only be called once, and only if the model was not created
 * without a set of columns. This function cannot be called after the model
 * has been added to a view.
 *
 * The columns are accessed as static data. They should continue to remain
 * in memory for longer than the GcrCollectionModel object.
 */
void
gcr_collection_model_set_columns (GcrCollectionModel *self, const GcrColumn *columns)
{
	const GcrColumn *col;
	guint n_columns;

	g_return_if_fail (GCR_IS_COLLECTION_MODEL (self));
	g_return_if_fail (columns);
	g_return_if_fail (self->pv->n_columns == 0);

	/* Count the number of columns, extra column for selected */
	for (col = columns, n_columns = 1; col->property_name; ++col)
		++n_columns;

	/* We expect the columns to stay around */
	self->pv->columns = columns;
	self->pv->n_columns = n_columns;
}

/**
 * gcr_collection_model_object_for_iter:
 * @self: The model
 * @iter: The row
 *
 * Get the object that is represented by the given row in the model.
 *
 * Returns: The object, owned by the model.
 */
GObject*
gcr_collection_model_object_for_iter (GcrCollectionModel *self, const GtkTreeIter *iter)
{
	g_return_val_if_fail (GCR_IS_COLLECTION_MODEL (self), NULL);
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (iter->stamp == COLLECTION_MODEL_STAMP, NULL);
	g_return_val_if_fail (G_IS_OBJECT (iter->user_data), NULL);

	return G_OBJECT (iter->user_data);
}

/**
 * gcr_collection_model_iter_for_object:
 * @self: The model
 * @object: The object
 * @iter: The row for the object
 *
 * Set @iter to the row for the given object. If the object is not in this
 * model, then %FALSE will be returned.
 *
 * Returns: %TRUE if the object was present.
 */
gboolean
gcr_collection_model_iter_for_object (GcrCollectionModel *self, GObject *object,
                                      GtkTreeIter *iter)
{
	gint index;

	g_return_val_if_fail (GCR_IS_COLLECTION_MODEL (self), FALSE);
	g_return_val_if_fail (G_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (iter, FALSE);

	index = index_for_object (self, object);
	if (index < 0)
		return FALSE;

	return iter_for_index (self, index, iter);
}

/**
 * gcr_collection_model_column_for_selected:
 * @self: The model
 *
 * Get the column identifier for the column that contains the values
 * of the selected state.
 *
 * Returns: The column identifier.
 */
gint
gcr_collection_model_column_for_selected (GcrCollectionModel *self)
{
	g_return_val_if_fail (GCR_IS_COLLECTION_MODEL (self), 0);
	g_assert (self->pv->n_columns > 0);
	return self->pv->n_columns - 1;
}

/**
 * gcr_collection_model_toggle_selected:
 * @self: The model
 * @iter: The row
 *
 * Toggle the selected state of a given row.
 */
void
gcr_collection_model_toggle_selected (GcrCollectionModel *self, GtkTreeIter *iter)
{
	GObject *object;

	g_return_if_fail (GCR_IS_COLLECTION_MODEL (self));

	object = gcr_collection_model_object_for_iter (self, iter);
	g_return_if_fail (G_IS_OBJECT (object));

	if (!self->pv->selected)
		self->pv->selected = selected_hash_table_new ();

	if (g_hash_table_lookup (self->pv->selected, object))
		g_hash_table_remove (self->pv->selected, object);
	else
		g_hash_table_insert (self->pv->selected, object, UNUSED_VALUE);
}

/**
 * gcr_collection_model_change_selected:
 * @self: The model
 * @iter: The row
 * @selected: Whether the row should be selected or not.
 *
 * Set whether a given row is toggled selected or not.
 */
void
gcr_collection_model_change_selected (GcrCollectionModel *self, GtkTreeIter *iter, gboolean selected)
{
	GtkTreePath *path;
	GObject *object;

	g_return_if_fail (GCR_IS_COLLECTION_MODEL (self));

	object = gcr_collection_model_object_for_iter (self, iter);
	g_return_if_fail (G_IS_OBJECT (object));

	if (!self->pv->selected)
		self->pv->selected = g_hash_table_new (g_direct_hash, g_direct_equal);

	if (selected)
		g_hash_table_insert (self->pv->selected, object, UNUSED_VALUE);
	else
		g_hash_table_remove (self->pv->selected, object);

	/* Tell the view that this row changed */
	path = gtk_tree_model_get_path (GTK_TREE_MODEL (self), iter);
	g_return_if_fail (path);
	gtk_tree_model_row_changed (GTK_TREE_MODEL (self), path, iter);
	gtk_tree_path_free (path);
}

/**
 * gcr_collection_model_is_selected:
 * @self: The model
 * @iter: The row
 *
 * Check whether a given row has been toggled as selected.
 *
 * Returns: Whether the row has been selected.
 */
gboolean
gcr_collection_model_is_selected (GcrCollectionModel *self, GtkTreeIter *iter)
{
	GObject *object;

	g_return_val_if_fail (GCR_IS_COLLECTION_MODEL (self), FALSE);

	object = gcr_collection_model_object_for_iter (self, iter);
	g_return_val_if_fail (G_IS_OBJECT (object), FALSE);

	if (!self->pv->selected)
		return FALSE;

	return g_hash_table_lookup (self->pv->selected, object) ? TRUE : FALSE;
}

GList*
gcr_collection_model_get_selected_objects (GcrCollectionModel *self)
{
	GHashTableIter iter;
	GList *result = NULL;
	gpointer key;

	g_return_val_if_fail (GCR_IS_COLLECTION_MODEL (self), NULL);

	if (!self->pv->selected)
		return NULL;

	g_hash_table_iter_init (&iter, self->pv->selected);
	while (g_hash_table_iter_next (&iter, &key, NULL))
		result = g_list_prepend (result, key);
	return result;
}

void
gcr_collection_model_set_selected_objects (GcrCollectionModel *self, GList *selected)
{
	GHashTable *newly_selected;
	GList *old_selection;
	GtkTreeIter iter;
	GList *l;

	old_selection = gcr_collection_model_get_selected_objects (self);
	newly_selected = selected_hash_table_new ();

	/* Select all the objects in selected which aren't already selected */
	for (l = selected; l; l = g_list_next (l)) {
		if (!self->pv->selected || !g_hash_table_lookup (self->pv->selected, l->data)) {
			if (!gcr_collection_model_iter_for_object (self, l->data, &iter))
				g_return_if_reached ();
			gcr_collection_model_change_selected (self, &iter, TRUE);
		}

		/* Note that we've seen this one */
		g_hash_table_insert (newly_selected, l->data, UNUSED_VALUE);
	}

	/* Unselect all the objects which aren't supposed to be selected */
	for (l = old_selection; l; l = g_list_next (l)) {
		if (!g_hash_table_lookup (newly_selected, l->data)) {
			if (!gcr_collection_model_iter_for_object (self, l->data, &iter))
				g_return_if_reached ();
			gcr_collection_model_change_selected (self, &iter, FALSE);
		}
	}

	g_list_free (old_selection);
	g_hash_table_destroy (newly_selected);
}
