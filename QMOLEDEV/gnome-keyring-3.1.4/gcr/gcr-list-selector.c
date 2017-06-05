/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "gcr-collection-model.h"
#include "gcr-internal.h"
#include "gcr-list-selector.h"
#include "gcr-list-selector-private.h"
#include "gcr-live-search.h"

#include <glib/gi18n-lib.h>

#include <string.h>

/**
 * SECTION:gcr-list-selector
 * @title: GcrListSelector
 * @short_description: A selector widget to one or more certificates from a list.
 *
 * The #GcrListSelector can be used to select one or more certificates or keys.
 * Live search is available for quick filtering.
 */

/**
 * GcrListSelector:
 * @parent: Parent object
 *
 * A list selector widget.
 */

/**
 * GcrListSelectorClass:
 *
 * The class for #GcrListSelector.
 */

enum {
	PROP_0,
	PROP_COLLECTION
};

struct _GcrListSelectorPrivate {
	GcrCollection *collection;
	GcrCollectionModel *model;

	GtkTreeModelFilter *filter;
	GtkWidget *search_widget;
};

G_DEFINE_TYPE (GcrListSelector, gcr_list_selector, GTK_TYPE_TREE_VIEW);

static gboolean
object_is_visible (GcrListSelector *self, GObject *object)
{
	gchar *text;
	gboolean visible;

	if (g_object_class_find_property (G_OBJECT_GET_CLASS (object), "search-text"))
		g_object_get (object, "search-text", &text, NULL);
	else
		g_object_get (object, "label", &text, NULL);

	visible = _gcr_live_search_match (GCR_LIVE_SEARCH (self->pv->search_widget), text);
	g_free (text);

	return visible;
}

static gboolean
on_tree_filter_visible_func (GtkTreeModel *model, GtkTreeIter *iter,
                             gpointer user_data)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (user_data);
	GObject *object;

	if (self->pv->search_widget == NULL ||
	    !gtk_widget_get_visible (self->pv->search_widget))
		return TRUE;

	object = gcr_collection_model_object_for_iter (self->pv->model, iter);
	if (object != NULL)
		return object_is_visible (self, object);

	return FALSE;
}

static gboolean
on_tree_view_start_search (GtkTreeView *view, gpointer user_data)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (view);

	if (self->pv->search_widget == NULL)
		return FALSE;

	if (gtk_widget_get_visible (self->pv->search_widget))
		gtk_widget_grab_focus (self->pv->search_widget);
	else
		gtk_widget_show (self->pv->search_widget);

	return TRUE;
}

static void
on_search_widget_text_notify (GcrLiveSearch *search, GParamSpec *pspec,
                              gpointer user_data)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (user_data);
#if 0
	GtkTreeViewColumn *focus_column;
	GtkTreeModel *model;
	GtkTreeIter iter;
	GtkTreePath *path;
	gboolean set_cursor = FALSE;
#endif

	gtk_tree_model_filter_refilter (self->pv->filter);

#if 0
	/* Set cursor on the first object. */

	model = gtk_tree_view_get_model (GTK_TREE_VIEW (self));
	gtk_tree_view_get_cursor (GTK_TREE_VIEW (view), &path, &focus_column);

	if (path == NULL) {
		path = gtk_tree_path_new_from_string ("0");
		set_cursor = TRUE;
	}

	if (set_cursor) {
		/* FIXME: Workaround for GTK bug #621651, we have to make sure
		 * the path is valid. */
		if (gtk_tree_model_get_iter (model, &iter, path)) {
			gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), path,
				focus_column, FALSE);
		}
	}

	gtk_tree_path_free (path);
#endif
}

static void
on_search_widget_activate (GtkWidget *search, gpointer user_data)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (user_data);
	GtkTreePath *path;
	GtkTreeViewColumn *focus_column;

	gtk_tree_view_get_cursor (GTK_TREE_VIEW (self), &path, &focus_column);
	if (path != NULL) {
		gtk_tree_view_row_activated (GTK_TREE_VIEW (self), path, focus_column);
		gtk_tree_path_free (path);

		gtk_widget_hide (search);
	}
}

static gboolean
on_search_widget_key_navigation (GtkWidget *search, GdkEvent *event, gpointer user_data)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (user_data);
	GdkEvent *new_event;
	gboolean ret = FALSE;

	new_event = gdk_event_copy (event);
	gtk_widget_grab_focus (GTK_WIDGET (self));
	ret = gtk_widget_event (GTK_WIDGET (self), new_event);
	gtk_widget_grab_focus (search);

	gdk_event_free (new_event);

	return ret;
}

static void
on_check_column_toggled (GtkCellRendererToggle *cell, gchar *path, gpointer user_data)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (user_data);
	GtkTreeIter iter, model_iter;

	g_assert (path != NULL);

	if (gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (self->pv->filter), &iter, path)) {
		gtk_tree_model_filter_convert_iter_to_child_iter (self->pv->filter, &model_iter, &iter);
		gcr_collection_model_toggle_selected (self->pv->model, &model_iter);
	}
}

static void
gcr_list_selector_constructed (GObject *object)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (object);
	GtkCellRenderer *cell;
	GtkTreeViewColumn *column;
	guint column_id;

	G_OBJECT_CLASS (gcr_list_selector_parent_class)->constructed (object);

	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (self), FALSE);

	self->pv->model = gcr_collection_model_new (self->pv->collection,
	                                            "icon", G_TYPE_ICON,
	                                            "markup", G_TYPE_STRING,
	                                            NULL);

	self->pv->filter = GTK_TREE_MODEL_FILTER (gtk_tree_model_filter_new (
	                                          GTK_TREE_MODEL (self->pv->model), NULL));
	gtk_tree_model_filter_set_visible_func (self->pv->filter,
	                                        on_tree_filter_visible_func, self, NULL);

	gtk_tree_view_set_model (GTK_TREE_VIEW (self), GTK_TREE_MODEL (self->pv->filter));

	/* The check */

	cell = gtk_cell_renderer_toggle_new ();
	g_signal_connect (cell, "toggled", G_CALLBACK (on_check_column_toggled), self);

	column_id = gcr_collection_model_column_for_selected (self->pv->model);
	column = gtk_tree_view_column_new_with_attributes ("", cell, "active", column_id, NULL);
	gtk_tree_view_column_set_resizable (column, FALSE);
	gtk_tree_view_append_column (GTK_TREE_VIEW (self), column);

	column = gtk_tree_view_column_new ();

	/* The icon */
	cell = gtk_cell_renderer_pixbuf_new ();
	g_object_set (cell, "stock-size", GTK_ICON_SIZE_DND, NULL);
	gtk_tree_view_column_pack_start (column, cell, FALSE);
	gtk_tree_view_column_add_attribute (column, cell, "gicon", 0);

	/* The markup */
	cell = gtk_cell_renderer_text_new ();
	gtk_tree_view_column_pack_start (column, cell, TRUE);
	gtk_tree_view_column_add_attribute (column, cell, "markup", 1);

	gtk_tree_view_append_column (GTK_TREE_VIEW (self), column);
}

static void
gcr_list_selector_init (GcrListSelector *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_LIST_SELECTOR, GcrListSelectorPrivate);
}

static void
gcr_list_selector_dispose (GObject *obj)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (obj);

	if (self->pv->filter)
		g_object_unref (self->pv->filter);
	self->pv->filter = NULL;

	if (self->pv->model)
		g_object_unref (self->pv->model);
	self->pv->model = NULL;

	if (self->pv->collection)
		g_object_unref (self->pv->collection);
	self->pv->collection = NULL;

	_gcr_list_selector_set_live_search (self, NULL);

	G_OBJECT_CLASS (gcr_list_selector_parent_class)->dispose (obj);
}

static void
gcr_list_selector_finalize (GObject *obj)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (obj);

	g_assert (!self->pv->collection);
	g_assert (!self->pv->model);

	G_OBJECT_CLASS (gcr_list_selector_parent_class)->finalize (obj);
}

static void
gcr_list_selector_set_property (GObject *obj, guint prop_id, const GValue *value,
                                 GParamSpec *pspec)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (obj);

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
gcr_list_selector_get_property (GObject *obj, guint prop_id, GValue *value,
                                 GParamSpec *pspec)
{
	GcrListSelector *self = GCR_LIST_SELECTOR (obj);

	switch (prop_id) {
	case PROP_COLLECTION:
		g_value_set_object (value, gcr_list_selector_get_collection (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_list_selector_class_init (GcrListSelectorClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructed = gcr_list_selector_constructed;
	gobject_class->dispose = gcr_list_selector_dispose;
	gobject_class->finalize = gcr_list_selector_finalize;
	gobject_class->set_property = gcr_list_selector_set_property;
	gobject_class->get_property = gcr_list_selector_get_property;

	g_type_class_add_private (gobject_class, sizeof (GcrListSelectorPrivate));

	/**
	 * GcrListSelector:collection:
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
GcrListSelector*
gcr_list_selector_new (GcrCollection *collection)
{
	return g_object_new (GCR_TYPE_LIST_SELECTOR,
	                     "collection", collection,
	                     NULL);
}

/**
 * gcr_list_selector_get_collection:
 * @self: The selector
 *
 * Get the collection that this selector is displaying objects from.
 *
 * Returns: The collection, owned by the selector.
 */
GcrCollection*
gcr_list_selector_get_collection (GcrListSelector *self)
{
	g_return_val_if_fail (GCR_IS_LIST_SELECTOR (self), NULL);
	return self->pv->collection;
}

/**
 * gcr_list_selector_get_selected:
 * @self: The selector
 *
 * Get a list of selected objects.
 *
 * Returns: The list of selected objects, to be released with g_list_free().
 */
GList*
gcr_list_selector_get_selected (GcrListSelector *self)
{
	g_return_val_if_fail (GCR_IS_LIST_SELECTOR (self), NULL);
	return gcr_collection_model_get_selected_objects (self->pv->model);
}

/**
 * gcr_list_selector_set_selected:
 * @self: The selector
 * @selected: The list of objects to select.
 *
 * Select certain objects in the selector.
 */
void
gcr_list_selector_set_selected (GcrListSelector *self, GList *selected)
{
	g_return_if_fail (GCR_IS_LIST_SELECTOR (self));
	gcr_collection_model_set_selected_objects (self->pv->model, selected);
}


void
_gcr_list_selector_set_live_search (GcrListSelector *self, GcrLiveSearch *search)
{
	g_return_if_fail (GCR_IS_LIST_SELECTOR (self));

	/* remove old handlers if old search was not null */
	if (self->pv->search_widget != NULL) {
		g_signal_handlers_disconnect_by_func (self, on_tree_view_start_search, NULL);

		g_signal_handlers_disconnect_by_func (self->pv->search_widget,
			on_search_widget_text_notify, self);
		g_signal_handlers_disconnect_by_func (self->pv->search_widget,
			on_search_widget_activate, self);
		g_signal_handlers_disconnect_by_func (self->pv->search_widget,
			on_search_widget_key_navigation, self);
		g_object_unref (self->pv->search_widget);
		self->pv->search_widget = NULL;
	}

	/* connect handlers if new search is not null */
	if (search != NULL) {
		self->pv->search_widget = g_object_ref (search);

		g_signal_connect (self, "start-interactive-search",
		                  G_CALLBACK (on_tree_view_start_search), NULL);

		g_signal_connect (self->pv->search_widget, "notify::text",
			G_CALLBACK (on_search_widget_text_notify), self);
		g_signal_connect (self->pv->search_widget, "activate",
			G_CALLBACK (on_search_widget_activate), self);
		g_signal_connect (self->pv->search_widget, "key-navigation",
			G_CALLBACK (on_search_widget_key_navigation), self);
	}
}
