/*
 * Copyright (C) 2008 Stefan Walter
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

#include "gcr-display-scrolled.h"
#include "gcr-display-view.h"
#include "gcr-renderer.h"
#include "gcr-viewer.h"

/**
 * SECTION:gcr-viewer
 * @title: GcrViewer
 * @short_description: A viewer which can hold renderers
 *
 * A #GcrViewer is an abstract interface that represents a widget that can hold
 * various renderers and display their contents.
 *
 * The interaction between #GcrRenderer and #GcrViewer is not stable yet, and
 * so viewers cannot be implemented outside the Gcr library at this time.
 *
 * Use the gcr_viewer_new() and gcr_viewer_new_scrolled() to get default
 * implementations of viewers.
 */

/**
 * GcrViewer:
 *
 * An abstract viewer which displays renderers contents.
 */

/**
 * GcrViewerIface:
 * @parent: The parent interface
 * @add_renderer: Virtual method to add a renderer
 * @remove_renderer: Virtual method to remove a renderer
 * @count_renderers: Virtual method to count renderers
 * @get_renderer: Virtual method to get a renderer
 *
 * The interface for #GcrViewer
 */

static void
gcr_viewer_base_init (gpointer gobject_iface)
{
	static gboolean initialized = FALSE;
	if (!initialized) {

		initialized = TRUE;
	}
}

GType
gcr_viewer_get_type (void)
{
	static GType type = 0;
	if (!type) {
		static const GTypeInfo info = {
			sizeof (GcrViewerIface),
			gcr_viewer_base_init,  /* base init */
			NULL,                  /* base finalize */
		};
		type = g_type_register_static (G_TYPE_INTERFACE, "GcrViewerIface", &info, 0);
		g_type_interface_add_prerequisite (type, GTK_TYPE_WIDGET);
	}

	return type;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_viewer_new:
 *
 * Get an implementation of #GcrViewer that supports a view
 * of multiple renderers.
 *
 * Returns: A newly allocated #GcrViewer, which should be released with
 *     g_object_unref()
 */
GcrViewer*
gcr_viewer_new (void)
{
	return GCR_VIEWER (_gcr_display_view_new ());
}

/**
 * gcr_viewer_new_scrolled:
 *
 * Get an implementation of #GcrViewer that supports a scrolled view
 * of multiple renderers.
 *
 * Returns: A newly allocated #GcrViewer, which should be released with
 *     g_object_unref()
 */
GcrViewer*
gcr_viewer_new_scrolled (void)
{
	return GCR_VIEWER (_gcr_display_scrolled_new ());
}

/**
 * gcr_viewer_add_renderer:
 * @self: The viewer
 * @renderer: The renderer to add
 *
 * Add a renderer to this viewer.
 */
void
gcr_viewer_add_renderer (GcrViewer *self, GcrRenderer *renderer)
{
	g_return_if_fail (GCR_IS_VIEWER (self));
	g_return_if_fail (GCR_IS_RENDERER (renderer));
	g_return_if_fail (GCR_VIEWER_GET_INTERFACE (self)->add_renderer);
	GCR_VIEWER_GET_INTERFACE (self)->add_renderer (self, renderer);
}

/**
 * gcr_viewer_remove_renderer:
 * @self: The viewer
 * @renderer: The renderer to remove
 *
 * Remove a renderer from this viewer.
 */
void
gcr_viewer_remove_renderer (GcrViewer *self, GcrRenderer *renderer)
{
	g_return_if_fail (GCR_IS_VIEWER (self));
	g_return_if_fail (GCR_IS_RENDERER (renderer));
	g_return_if_fail (GCR_VIEWER_GET_INTERFACE (self)->remove_renderer);
	GCR_VIEWER_GET_INTERFACE (self)->remove_renderer (self, renderer);
}

/**
 * gcr_viewer_count_renderers:
 * @self: The viewer
 *
 * Get the number of renderers present in the viewer.
 *
 * Returns: The number of renderers.
 */
guint
gcr_viewer_count_renderers (GcrViewer *self)
{
	g_return_val_if_fail (GCR_IS_VIEWER (self), 0);
	g_return_val_if_fail (GCR_VIEWER_GET_INTERFACE (self)->count_renderers, 0);
	return GCR_VIEWER_GET_INTERFACE (self)->count_renderers (self);
}

/**
 * gcr_viewer_get_renderer:
 * @self: The viewer
 * @index_: The index of the renderer to get
 *
 * Get a pointer to the renderer at the given index. It is an error to request
 * an index that is out of bounds.
 *
 * Returns: The render, owned by the viewer.
 */
GcrRenderer*
gcr_viewer_get_renderer (GcrViewer *self, guint index_)
{
	g_return_val_if_fail (GCR_IS_VIEWER (self), NULL);
	g_return_val_if_fail (GCR_VIEWER_GET_INTERFACE (self)->get_renderer, NULL);
	return GCR_VIEWER_GET_INTERFACE (self)->get_renderer (self, index_);
}
