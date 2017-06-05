/*
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

#include "gcr-certificate.h"
#include "gcr-certificate-renderer.h"
#include "gcr-certificate-widget.h"
#include "gcr-renderer.h"
#include "gcr-viewer.h"

#include "gck/gck.h"

#include <gdk/gdk.h>
#include <glib/gi18n-lib.h>

/**
 * SECTION:gcr-certificate-widget
 * @title: GcrCertificateWidget
 * @short_description: Certificate widget and renderer
 *
 * A #GcrCertificateWidget can be used to display a certificate. The widget
 * is normally in a collapsed state showing only details, but can be expanded
 * by the user.
 *
 * Use gcr_certificate_widget_new() to create a new certificate widget. Only
 * one certificate can be displayed.  A #GcrCertificateWidget contains a
 * #GcrViewer internally and #GcrCertificateRenderer is used to render the
 * certificate to the viewer. To show more than one certificate in a view,
 * create the viewer and add renderers to it.
 */

/**
 * GcrCertificateWidget:
 *
 * A widget that displays a certificate.
 */

/**
 * GcrCertificateWidgetClass:
 *
 * The class for #GcrCertificateWidget
 */

enum {
	PROP_0,
	PROP_CERTIFICATE,
	PROP_ATTRIBUTES
};

struct _GcrCertificateWidget {
	GtkAlignment parent;

	/*< private >*/
	GcrCertificateWidgetPrivate *pv;
};

struct _GcrCertificateWidgetClass {
	/*< private >*/
	GtkAlignmentClass parent_class;
};

struct _GcrCertificateWidgetPrivate {
	GcrViewer *viewer;
	GcrCertificateRenderer *renderer;
};

G_DEFINE_TYPE (GcrCertificateWidget, gcr_certificate_widget, GTK_TYPE_ALIGNMENT);

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static GObject*
gcr_certificate_widget_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GObject *obj = G_OBJECT_CLASS (gcr_certificate_widget_parent_class)->constructor (type, n_props, props);
	GcrCertificateWidget *self = NULL;

	g_return_val_if_fail (obj, NULL);

	self = GCR_CERTIFICATE_WIDGET (obj);

	self->pv->viewer = gcr_viewer_new_scrolled ();
	gtk_container_add (GTK_CONTAINER (self), GTK_WIDGET (self->pv->viewer));
	gtk_widget_show (GTK_WIDGET (self->pv->viewer));

	gcr_viewer_add_renderer (self->pv->viewer, GCR_RENDERER (self->pv->renderer));
	return obj;
}

static void
gcr_certificate_widget_init (GcrCertificateWidget *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_CERTIFICATE_WIDGET, GcrCertificateWidgetPrivate));
	self->pv->renderer = gcr_certificate_renderer_new (NULL);
}

static void
gcr_certificate_widget_finalize (GObject *obj)
{
	GcrCertificateWidget *self = GCR_CERTIFICATE_WIDGET (obj);

	g_assert (self->pv->renderer);
	g_object_unref (self->pv->renderer);
	self->pv->renderer = NULL;

	g_assert (self->pv->viewer);
	self->pv->viewer = NULL;

	G_OBJECT_CLASS (gcr_certificate_widget_parent_class)->finalize (obj);
}

static void
gcr_certificate_widget_set_property (GObject *obj, guint prop_id, const GValue *value,
                                     GParamSpec *pspec)
{
	GcrCertificateWidget *self = GCR_CERTIFICATE_WIDGET (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		gcr_certificate_widget_set_certificate (self, g_value_get_object (value));
		break;
	case PROP_ATTRIBUTES:
		gcr_certificate_widget_set_attributes (self, g_value_get_boxed (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_certificate_widget_get_property (GObject *obj, guint prop_id, GValue *value,
                                     GParamSpec *pspec)
{
	GcrCertificateWidget *self = GCR_CERTIFICATE_WIDGET (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_value_set_object (value, gcr_certificate_widget_get_certificate (self));
		break;
	case PROP_ATTRIBUTES:
		g_value_set_boxed (value, gcr_certificate_widget_get_attributes (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_certificate_widget_class_init (GcrCertificateWidgetClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GckAttributes *registered;

	gcr_certificate_widget_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GcrCertificateWidgetPrivate));

	gobject_class->constructor = gcr_certificate_widget_constructor;
	gobject_class->finalize = gcr_certificate_widget_finalize;
	gobject_class->set_property = gcr_certificate_widget_set_property;
	gobject_class->get_property = gcr_certificate_widget_get_property;

	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object("certificate", "Certificate", "Certificate to display.",
	                               GCR_TYPE_CERTIFICATE, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_ATTRIBUTES,
	         g_param_spec_boxed ("attributes", "Attributes", "Attributes which contain the certificate",
	                             GCK_TYPE_ATTRIBUTES, G_PARAM_READWRITE));

	/* Register this as a renderer which can be loaded */
	registered = gck_attributes_new ();
	gck_attributes_add_ulong (registered, CKA_CLASS, CKO_CERTIFICATE);
	gcr_renderer_register (GCR_TYPE_CERTIFICATE_WIDGET, registered);
	gck_attributes_unref (registered);
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_certificate_widget_new:
 * @certificate: Certificate to display, or %NULL
 *
 * Create a new certificate widget which displays a given certificate.
 *
 * Returns: A newly allocated #GcrCertificateWidget, which should be freed
 *     with g_object_unref().
 */
GcrCertificateWidget*
gcr_certificate_widget_new (GcrCertificate *certificate)
{
	return g_object_new (GCR_TYPE_CERTIFICATE_WIDGET, "certificate", certificate, NULL);
}

/**
 * gcr_certificate_widget_get_certificate:
 * @self: The certificate widget
 *
 * Get the certificate displayed in the widget.
 *
 * Returns: The certificate.
 */
GcrCertificate*
gcr_certificate_widget_get_certificate (GcrCertificateWidget *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_WIDGET (self), NULL);
	return gcr_certificate_renderer_get_certificate (self->pv->renderer);
}

/**
 * gcr_certificate_widget_set_certificate:
 * @self: The certificate widget
 * @certificate: The certificate to display
 *
 * Set the certificate displayed in the widget
 */
void
gcr_certificate_widget_set_certificate (GcrCertificateWidget *self, GcrCertificate *certificate)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_WIDGET (self));
	gcr_certificate_renderer_set_certificate (self->pv->renderer, certificate);
}

/**
 * gcr_certificate_widget_get_attributes:
 * @self: The certificate widget
 *
 * Get the attributes displayed in the widget. The attributes should contain
 * a certificate.
 *
 * Returns: The attributes, owned by the widget.
 */
GckAttributes*
gcr_certificate_widget_get_attributes (GcrCertificateWidget *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_WIDGET (self), NULL);
	return gcr_certificate_renderer_get_attributes (self->pv->renderer);
}

/**
 * gcr_certificate_widget_set_attributes:
 * @self: The certificate widget
 * @attrs: The attributes to display
 *
 * Set the attributes displayed in the widget. The attributes should contain
 * a certificate.
 */
void
gcr_certificate_widget_set_attributes (GcrCertificateWidget *self, GckAttributes* attrs)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_WIDGET (self));
	gcr_certificate_renderer_set_attributes (self->pv->renderer, attrs);
}
