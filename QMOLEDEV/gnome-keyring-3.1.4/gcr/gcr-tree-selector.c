/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gcr-collection-model.h"
#include "gcr-internal.h"
#include "gcr-tree-selector.h"

#include <glib/gi18n-lib.h>

#include <string.h>

/**
 * SECTION:gcr-tree-selector
 * @title: GcrTreeSelector
 * @short_description: A selector widget to select certificates or keys.
 *
 * The #GcrTreeSelector can be used to select certificates or keys. It allows
 * the user to select multiple objects from a tree.
 */

/**
 * GcrTreeSelector:
 * @parent: The parent object
 *
 * A tree selector widget.
 */

/**
 * GcrTreeSelectorClass:
 *
 * The class for #GcrTreeSelector.
 */

enum {
	PROP_0,
	PROP_COLLECTION,
	PROP_COLUMNS
};

struct _GcrTreeSelectorPrivate {
	GcrCollection *collection;
	const GcrColumn *columns;
	GtkTreeModel *sort;
	GcrCollectionModel *model;
};

G_DEFINE_TYPE (GcrTreeSelector, gcr_tree_selector, GTK_TYPE_TREE_VIEW);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
on_check_column_toggled (GtkCellRendererToggle *cell, gchar *path, GcrCollectionModel *model)
{
	GtkTreeIter iter;

	g_assert (path != NULL);

	if (gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (model), &iter, path))
		gcr_collection_model_toggle_selected (model, &iter);
}

typedef gint (*SortFunc) (GValue *, GValue *);

static gint
sort_string (GValue *val_a, GValue *val_b)
{
	const gchar *str_a = g_value_get_string (val_a);
	const gchar *str_b = g_value_get_string (val_b);

	if (str_a == str_b)
		return 0;
	else if (!str_a)
		return -1;
	else if (!str_b)
		return 1;
	else
		return g_utf8_collate (str_a, str_b);
}

static gint
sort_date (GValue *val_a, GValue *val_b)
{
	GDate *date_a = g_value_get_boxed (val_a);
	GDate *date_b = g_value_get_boxed (val_b);

	if (date_a == date_b)
		return 0;
	else if (!date_a)
		return -1;
	else if (!date_b)
		return 1;
	else
		return g_date_compare (date_a, date_b);
}

static inline SortFunc
sort_implementation_for_type (GType type)
{
	if (type == G_TYPE_STRING)
		return sort_string;
	else if (type == G_TYPE_DATE)
		return sort_date;
	else
		return NULL;
}

static gint
on_sort_column (GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b,
                gpointer user_data)
{
	GcrColumn *column = user_data;
	SortFunc func;
	GObject *object_a;
	GObject *object_b;
	GValue val_a;
	GValue val_b;
	gint ret;

	object_a = gcr_collection_model_object_for_iter (GCR_COLLECTION_MODEL (model), a);
	g_return_val_if_fail (G_IS_OBJECT (object_a), 0);
	object_b = gcr_collection_model_object_for_iter (GCR_COLLECTION_MODEL (model), b);
	g_return_val_if_fail (G_IS_OBJECT (object_b), 0);

	memset (&val_a, 0, sizeof (val_a));
	memset (&val_b, 0, sizeof (val_b));

	g_value_init (&val_a, column->property_type);
	g_value_init (&val_b, column->property_type);

	g_object_get_property (object_a, column->property_name, &val_a);
	g_object_get_property (object_b, column->property_name, &val_b);

	func = sort_implementation_for_type (column->property_type);
	g_return_val_if_fail (func, 0);

	ret = (func) (&val_a, &val_b);

	g_value_unset (&val_a);
	g_value_unset (&val_b);

	return ret;
}

static void
add_string_column (GcrTreeSelector *self, const GcrColumn *column, gint column_id)
{
	GtkCellRenderer *cell;
	GtkTreeViewColumn *col;
	const gchar *label;

	g_assert (column->column_type == G_TYPE_STRING);
	g_assert (!(column->flags & GCR_COLUMN_HIDDEN));

	cell = gtk_cell_renderer_text_new ();
	g_object_set (G_OBJECT (cell), "ellipsize", PANGO_ELLIPSIZE_END, NULL);
	label = column->label ? g_dpgettext2 (NULL, "column", column->label) : "";
	col = gtk_tree_view_column_new_with_attributes (label, cell, "text", column_id, NULL);
	gtk_tree_view_column_set_resizable (col, TRUE);
	if (column->flags & GCR_COLUMN_SORTABLE)
		gtk_tree_view_column_set_sort_column_id (col, column_id);
	gtk_tree_view_append_column (GTK_TREE_VIEW (self), col);
}

static void
add_icon_column (GcrTreeSelector *self, const GcrColumn *column, gint column_id)
{
	GtkCellRenderer *cell;
	GtkTreeViewColumn *col;
	const gchar *label;

	g_assert (column->column_type == G_TYPE_ICON);
	g_assert (!(column->flags & GCR_COLUMN_HIDDEN));

	cell = gtk_cell_renderer_pixbuf_new ();
	g_object_set (cell, "stock-size", GTK_ICON_SIZE_BUTTON, NULL);
	label = column->label ? g_dpgettext2 (NULL, "column", column->label) : "";
	col = gtk_tree_view_column_new_with_attributes (label, cell, "gicon", column_id, NULL);
	gtk_tree_view_column_set_resizable (col, TRUE);
	if (column->flags & GCR_COLUMN_SORTABLE)
		gtk_tree_view_column_set_sort_column_id (col, column_id);
	gtk_tree_view_append_column (GTK_TREE_VIEW (self), col);
}

static void
add_check_column (GcrTreeSelector *self, guint column_id)
{
	GtkCellRenderer *cell;
	GtkTreeViewColumn *col;

	cell = gtk_cell_renderer_toggle_new ();
	g_signal_connect (cell, "toggled", G_CALLBACK (on_check_column_toggled), self->pv->model);

	col = gtk_tree_view_column_new_with_attributes ("", cell, "active", column_id, NULL);
	gtk_tree_view_column_set_resizable (col, FALSE);
	gtk_tree_view_append_column (GTK_TREE_VIEW (self), col);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static GObject*
gcr_tree_selector_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GcrTreeSelector *self = GCR_TREE_SELECTOR (G_OBJECT_CLASS (gcr_tree_selector_parent_class)->constructor(type, n_props, props));
	const GcrColumn *column;
	GtkTreeSortable *sortable;
	guint i;

	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (self->pv->columns, NULL);

	self->pv->model = gcr_collection_model_new_full (self->pv->collection,
	                                                 self->pv->columns);

	self->pv->sort = gtk_tree_model_sort_new_with_model (GTK_TREE_MODEL (self->pv->model));
	sortable = GTK_TREE_SORTABLE (self->pv->sort);

	gtk_tree_view_set_model (GTK_TREE_VIEW (self), GTK_TREE_MODEL (self->pv->sort));

	/* First add the check mark column */
	add_check_column (self, gcr_collection_model_column_for_selected (self->pv->model));

	for (column = self->pv->columns, i = 0; column->property_name; ++column, ++i) {
		if (column->flags & GCR_COLUMN_HIDDEN)
			continue;

		if (column->column_type == G_TYPE_STRING)
			add_string_column (self, column, i);
		else if (column->column_type == G_TYPE_ICON)
			add_icon_column (self, column, i);
		else
			g_warning ("skipping unsupported column '%s' of type: %s",
			           column->property_name, g_type_name (column->column_type));

		/* Setup the column itself */
		if (column->flags & GCR_COLUMN_SORTABLE) {
			if (sort_implementation_for_type (column->property_type))
				gtk_tree_sortable_set_sort_func (sortable, i, on_sort_column,
				                                 (gpointer)column, NULL);
			else
				g_warning ("no sort implementation defined for type '%s' on column '%s'",
				           g_type_name (column->property_type), column->property_name);
		}
	}

	return G_OBJECT (self);
}

static void
gcr_tree_selector_init (GcrTreeSelector *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_TREE_SELECTOR, GcrTreeSelectorPrivate);
}

static void
gcr_tree_selector_dispose (GObject *obj)
{
	GcrTreeSelector *self = GCR_TREE_SELECTOR (obj);

	if (self->pv->model)
		g_object_unref (self->pv->model);
	self->pv->model = NULL;

	if (self->pv->collection)
		g_object_unref (self->pv->collection);
	self->pv->collection = NULL;

	if (self->pv->sort)
		g_object_unref (self->pv->sort);
	self->pv->sort = NULL;

	G_OBJECT_CLASS (gcr_tree_selector_parent_class)->dispose (obj);
}

static void
gcr_tree_selector_finalize (GObject *obj)
{
	GcrTreeSelector *self = GCR_TREE_SELECTOR (obj);

	g_assert (!self->pv->collection);
	g_assert (!self->pv->model);

	G_OBJECT_CLASS (gcr_tree_selector_parent_class)->finalize (obj);
}

static void
gcr_tree_selector_set_property (GObject *obj, guint prop_id, const GValue *value,
                                GParamSpec *pspec)
{
	GcrTreeSelector *self = GCR_TREE_SELECTOR (obj);
	switch (prop_id) {
	case PROP_COLLECTION:
		g_return_if_fail (!self->pv->collection);
		self->pv->collection = g_value_dup_object (value);
		g_return_if_fail (self->pv->collection);
		break;
	case PROP_COLUMNS:
		g_return_if_fail (!self->pv->columns);
		self->pv->columns = g_value_get_pointer (value);
		g_return_if_fail (self->pv->columns);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_tree_selector_get_property (GObject *obj, guint prop_id, GValue *value,
                                GParamSpec *pspec)
{
	GcrTreeSelector *self = GCR_TREE_SELECTOR (obj);

	switch (prop_id) {
	case PROP_COLLECTION:
		g_value_set_object (value, gcr_tree_selector_get_collection (self));
		break;
	case PROP_COLUMNS:
		g_value_set_pointer (value, (gpointer)gcr_tree_selector_get_columns (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_tree_selector_class_init (GcrTreeSelectorClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructor = gcr_tree_selector_constructor;
	gobject_class->dispose = gcr_tree_selector_dispose;
	gobject_class->finalize = gcr_tree_selector_finalize;
	gobject_class->set_property = gcr_tree_selector_set_property;
	gobject_class->get_property = gcr_tree_selector_get_property;

	g_type_class_add_private (gobject_class, sizeof (GcrTreeSelectorPrivate));

	/**
	 * GcrTreeSelector:collection:
	 *
	 * The collection which contains the objects to display in the selector.
	 */
	g_object_class_install_property (gobject_class, PROP_COLLECTION,
	           g_param_spec_object ("collection", "Collection", "Collection to select from",
	                                GCR_TYPE_COLLECTION, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GcrTreeSelector:columns:
	 *
	 * The columns to use to display the objects.
	 */
	g_object_class_install_property (gobject_class, PROP_COLUMNS,
	           g_param_spec_pointer ("columns", "Columns", "Columns to display in selector",
	                                 G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	_gcr_initialize ();
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_tree_selector_new:
 * @collection: The collection that contains the objects to display
 * @columns: The columns to use to display the objects
 *
 * Create a new #GcrTreeSelector.
 *
 * Returns: A newly allocated selector, which should be released with
 *     g_object_unref().
 */
GcrTreeSelector*
gcr_tree_selector_new (GcrCollection *collection, const GcrColumn *columns)
{
	return g_object_new (GCR_TYPE_TREE_SELECTOR,
	                     "collection", collection,
	                     "columns", columns,
	                     NULL);
}

/**
 * gcr_tree_selector_get_collection:
 * @self: The selector
 *
 * Get the collection that this selector is displaying objects from.
 *
 * Returns: The collection, owned by the selector.
 */
GcrCollection*
gcr_tree_selector_get_collection (GcrTreeSelector *self)
{
	g_return_val_if_fail (GCR_IS_TREE_SELECTOR (self), NULL);
	return self->pv->collection;
}

/**
 * gcr_tree_selector_get_columns:
 * @self: The selector
 *
 * Get the columns displayed in a selector in multiple mode.
 *
 * Returns: The columns, owned by the selector.
 */
const GcrColumn*
gcr_tree_selector_get_columns (GcrTreeSelector *self)
{
	g_return_val_if_fail (GCR_IS_TREE_SELECTOR (self), NULL);
	return self->pv->columns;
}

/**
 * gcr_tree_selector_get_selected:
 * @self: The selector
 *
 * Get a list of selected objects.
 *
 * Returns: The list of selected objects, to be released with g_list_free().
 */
GList*
gcr_tree_selector_get_selected (GcrTreeSelector *self)
{
	g_return_val_if_fail (GCR_IS_TREE_SELECTOR (self), NULL);
	return gcr_collection_model_get_selected_objects (self->pv->model);
}

/**
 * gcr_tree_selector_set_selected:
 * @self: The selector
 * @selected: The list of objects to select.
 *
 * Select certain objects in the selector.
 */
void
gcr_tree_selector_set_selected (GcrTreeSelector *self, GList *selected)
{
	g_return_if_fail (GCR_IS_TREE_SELECTOR (self));
	gcr_collection_model_set_selected_objects (self->pv->model, selected);
}
