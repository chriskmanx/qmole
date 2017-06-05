/*
 * Copyright (C) 2011 Collabora Ltd.
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

#include "gcr-display-scrolled.h"
#include "gcr-viewer.h"

static void _gcr_display_scrolled_viewer_iface (GcrViewerIface *iface);

G_DEFINE_TYPE_WITH_CODE (GcrDisplayScrolled, _gcr_display_scrolled, GTK_TYPE_SCROLLED_WINDOW,
                         G_IMPLEMENT_INTERFACE (GCR_TYPE_VIEWER, _gcr_display_scrolled_viewer_iface));

struct _GcrDisplayScrolledPrivate {
	GcrViewer *internal;
};

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
_gcr_display_scrolled_init (GcrDisplayScrolled *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_DISPLAY_SCROLLED, GcrDisplayScrolledPrivate));
	self->pv->internal = gcr_viewer_new ();
}

static void
_gcr_display_scrolled_constructed (GObject *object)
{
	GcrDisplayScrolled *self = GCR_DISPLAY_SCROLLED (object);

	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (self), GTK_SHADOW_ETCHED_IN);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (self), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_container_add (GTK_CONTAINER (self), GTK_WIDGET (self->pv->internal));
	gtk_widget_show (GTK_WIDGET (self->pv->internal));
}

static void
_gcr_display_scrolled_get_preferred_height (GtkWidget *widget, gint *minimal_height,
                                            gint *natural_height)
{
	GcrDisplayScrolled *self = GCR_DISPLAY_SCROLLED (widget);
	gint minimal, natural;

	GTK_WIDGET_CLASS (_gcr_display_scrolled_parent_class)->get_preferred_height (widget,
	                                                                             minimal_height,
	                                                                             natural_height);

	minimal = 0;
	natural = 0;

	gtk_widget_get_preferred_height (GTK_WIDGET (self->pv->internal),
	                                 &minimal, &natural);

	/* This is messy, we add a extra for the etching height */
	*minimal_height = MAX (minimal + 3, *minimal_height);
	*natural_height = MAX (natural + 3, *natural_height);
}

static void
_gcr_display_scrolled_get_preferred_width (GtkWidget *widget, gint *minimal_width,
                                           gint *natural_width)
{
	GcrDisplayScrolled *self = GCR_DISPLAY_SCROLLED (widget);
	gint minimal, natural;

	GTK_WIDGET_CLASS (_gcr_display_scrolled_parent_class)->get_preferred_width (widget,
	                                                                            minimal_width,
	                                                                            natural_width);

	minimal = 0;
	natural = 0;

	gtk_widget_get_preferred_width (GTK_WIDGET (self->pv->internal),
	                                &minimal, &natural);

	/* This is messy, we add a extra for the scrollbar width, etching */
	*minimal_width = MAX (minimal + 32, *minimal_width);
	*natural_width = MAX (natural + 32, *natural_width);
}

static void
_gcr_display_scrolled_class_init (GcrDisplayScrolledClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

	widget_class->get_preferred_height = _gcr_display_scrolled_get_preferred_height;
	widget_class->get_preferred_width = _gcr_display_scrolled_get_preferred_width;

	object_class->constructed = _gcr_display_scrolled_constructed;

	g_type_class_add_private (klass, sizeof (GcrDisplayScrolledPrivate));
}

static void
_gcr_display_scrolled_real_add_renderer (GcrViewer *viewer, GcrRenderer *renderer)
{
	GcrDisplayScrolled *self = GCR_DISPLAY_SCROLLED (viewer);
	gcr_viewer_add_renderer (self->pv->internal, renderer);
}

static void
_gcr_display_scrolled_real_remove_renderer (GcrViewer *viewer, GcrRenderer *renderer)
{
	GcrDisplayScrolled *self = GCR_DISPLAY_SCROLLED (viewer);
	gcr_viewer_remove_renderer (self->pv->internal, renderer);
}

static guint
_gcr_display_scrolled_real_count_renderers (GcrViewer *viewer)
{
	GcrDisplayScrolled *self = GCR_DISPLAY_SCROLLED (viewer);
	return gcr_viewer_count_renderers (self->pv->internal);
}

static GcrRenderer*
_gcr_display_scrolled_real_get_renderer (GcrViewer *viewer, guint index_)
{
	GcrDisplayScrolled *self = GCR_DISPLAY_SCROLLED (viewer);
	return gcr_viewer_get_renderer (self->pv->internal, index_);
}

static void
_gcr_display_scrolled_viewer_iface (GcrViewerIface *iface)
{
	iface->add_renderer = _gcr_display_scrolled_real_add_renderer;
	iface->remove_renderer = _gcr_display_scrolled_real_remove_renderer;
	iface->count_renderers = _gcr_display_scrolled_real_count_renderers;
	iface->get_renderer = _gcr_display_scrolled_real_get_renderer;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GcrDisplayScrolled*
_gcr_display_scrolled_new (void)
{
	return g_object_new (GCR_TYPE_DISPLAY_SCROLLED, NULL);
}
