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
#include "gcr-combo-selector.h"

#include <glib/gi18n-lib.h>

#include <string.h>

/**
 * SECTION:gcr-combo-selector
 * @title: GcrComboSelector
 * @short_description: A selector widget to select a single certificate or key.
 *
 * The #GcrComboSelector can be used to select a certificate or key. It allows
 * the user to select one object from the selector at a time.
 */

/**
 * GcrComboSelector:
 * @parent: Parent object
 *
 * A combo selector widget.
 */

/**
 * GcrComboSelectorClass:
 *
 * The class for #GcrComboSelector.
 */

enum {
	PROP_0,
	PROP_COLLECTION
};

struct _GcrComboSelectorPrivate {
	GcrCollection *collection;
	GcrCollectionModel *model;
};

G_DEFINE_TYPE (GcrComboSelector, gcr_combo_selector, GTK_TYPE_COMBO_BOX);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static GObject*
gcr_combo_selector_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GcrComboSelector *self = GCR_COMBO_SELECTOR (G_OBJECT_CLASS (gcr_combo_selector_parent_class)->constructor(type, n_props, props));
	GtkCellRenderer *cell;

	g_return_val_if_fail (self, NULL);

	self->pv->model = gcr_collection_model_new (self->pv->collection,
	                                            "icon", G_TYPE_ICON,
	                                            "markup", G_TYPE_STRING,
	                                            NULL);

	gtk_combo_box_set_model (GTK_COMBO_BOX (self), GTK_TREE_MODEL (self->pv->model));

	/* The icon */
	cell = gtk_cell_renderer_pixbuf_new ();
	g_object_set (cell, "stock-size", GTK_ICON_SIZE_DND, NULL);
	gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (self), cell, FALSE);
	gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (self), cell, "gicon", 0);

	/* The markup */
	cell = gtk_cell_renderer_text_new ();
	gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (self), cell, TRUE);
	gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (self), cell, "markup", 1);

	return G_OBJECT (self);
}

static void
gcr_combo_selector_init (GcrComboSelector *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_COMBO_SELECTOR, GcrComboSelectorPrivate);
}

static void
gcr_combo_selector_dispose (GObject *obj)
{
	GcrComboSelector *self = GCR_COMBO_SELECTOR (obj);

	if (self->pv->model)
		g_object_unref (self->pv->model);
	self->pv->model = NULL;

	if (self->pv->collection)
		g_object_unref (self->pv->collection);
	self->pv->collection = NULL;

	G_OBJECT_CLASS (gcr_combo_selector_parent_class)->dispose (obj);
}

static void
gcr_combo_selector_finalize (GObject *obj)
{
	GcrComboSelector *self = GCR_COMBO_SELECTOR (obj);

	g_assert (!self->pv->collection);
	g_assert (!self->pv->model);

	G_OBJECT_CLASS (gcr_combo_selector_parent_class)->finalize (obj);
}

static void
gcr_combo_selector_set_property (GObject *obj, guint prop_id, const GValue *value,
                                 GParamSpec *pspec)
{
	GcrComboSelector *self = GCR_COMBO_SELECTOR (obj);

	switch (prop_id) {
	case PROP_COLLECTION:
		g_return_if_fail (!self->pv->collection);
		self->pv->collection = g_value_dup_object (value);
		g_return_if_fail (self->pv->collection);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_combo_selector_get_property (GObject *obj, guint prop_id, GValue *value,
                                 GParamSpec *pspec)
{
	GcrComboSelector *self = GCR_COMBO_SELECTOR (obj);

	switch (prop_id) {
	case PROP_COLLECTION:
		g_value_set_object (value, gcr_combo_selector_get_collection (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_combo_selector_class_init (GcrComboSelectorClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructor = gcr_combo_selector_constructor;
	gobject_class->dispose = gcr_combo_selector_dispose;
	gobject_class->finalize = gcr_combo_selector_finalize;
	gobject_class->set_property = gcr_combo_selector_set_property;
	gobject_class->get_property = gcr_combo_selector_get_property;

	g_type_class_add_private (gobject_class, sizeof (GcrComboSelectorPrivate));

	/**
	 * GcrComboSelector:collection:
	 *
	 * The collection which contains the objects to display in the selector.
	 */
	g_object_class_install_property (gobject_class, PROP_COLLECTION,
	           g_param_spec_object ("collection", "Collection", "Collection to select from",
	                                GCR_TYPE_COLLECTION, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	_gcr_initialize ();
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_selector_new:
 * @collection: The collection that contains the objects to display
 *
 * Create a new #GcrTreeSelector.
 *
 * Returns: A newly allocated selector, which should be released with
 *     g_object_unref().
 */
GcrComboSelector*
gcr_combo_selector_new (GcrCollection *collection)
{
	return g_object_new (GCR_TYPE_COMBO_SELECTOR,
	                     "collection", collection,
	                     NULL);
}

/**
 * gcr_combo_selector_get_collection:
 * @self: The selector
 *
 * Get the collection that this selector is displaying objects from.
 *
 * Returns: The collection, owned by the selector.
 */
GcrCollection*
gcr_combo_selector_get_collection (GcrComboSelector *self)
{
	g_return_val_if_fail (GCR_IS_COMBO_SELECTOR (self), NULL);
	return self->pv->collection;
}

/**
 * gcr_combo_selector_get_selected:
 * @self: The selector
 *
 * Get the selected object in the selector, or %NULL if nothing selected.
 *
 * Returns: The selected object, owned by the selector, or %NULL.
 */
GObject*
gcr_combo_selector_get_selected (GcrComboSelector *self)
{
	GtkTreeIter iter;

	g_return_val_if_fail (GCR_IS_COMBO_SELECTOR (self), NULL);
	gtk_combo_box_get_active_iter (GTK_COMBO_BOX (self), &iter);

	return gcr_collection_model_object_for_iter (self->pv->model, &iter);
}

/**
 * gcr_combo_selector_set_selected:
 * @self: The selector
 * @selected: The object to select or %NULL.
 *
 * Set the currently selected object in the selector, or clear the selection
 * if selected is set to %NULL.
 */
void
gcr_combo_selector_set_selected (GcrComboSelector *self, GObject *selected)
{
	GtkTreeIter iter;

	g_return_if_fail (GCR_IS_COMBO_SELECTOR (self));

	if (selected) {
		if (!gcr_collection_model_iter_for_object (self->pv->model, selected, &iter))
			g_return_if_reached ();
		gtk_combo_box_set_active_iter (GTK_COMBO_BOX (self), &iter);
	} else {
		gtk_combo_box_set_active_iter (GTK_COMBO_BOX (self), NULL);
	}
}
