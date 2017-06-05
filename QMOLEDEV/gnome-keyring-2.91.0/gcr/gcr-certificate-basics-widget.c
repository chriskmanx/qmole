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

#include "gcr-certificate.h"
#include "gcr-certificate-basics-widget.h"

#include <glib/gi18n-lib.h>

enum {
	PROP_0,
	PROP_CERTIFICATE
};

struct _GcrCertificateBasicsWidgetPrivate {
	GcrCertificate *certificate;
	GtkBuilder *builder;
};

G_DEFINE_TYPE (GcrCertificateBasicsWidget, gcr_certificate_basics_widget, GTK_TYPE_ALIGNMENT);

/* -----------------------------------------------------------------------------
 * INTERNAL 
 */

static void
set_certificate_part_label (GtkBuilder *builder, const char *name, const gchar *value)
{
	GtkWidget *widget;
	gchar *markup;
	
	widget = GTK_WIDGET (gtk_builder_get_object (builder, name));
	g_return_if_fail (GTK_IS_LABEL (widget));
	if(value)
	{
		markup = g_markup_escape_text (value, -1);
		gtk_label_set_markup (GTK_LABEL (widget), markup);
		g_free (markup);
	}
	else
	{
		gtk_label_set_markup (GTK_LABEL (widget), _("<i>Not Part of Certificate</i>"));
	}
}

static void
set_certificate_part_date (GtkBuilder *builder, const char *name, const GDate *value)
{
	GtkWidget *widget;
	gchar *formatted;
	
	widget = GTK_WIDGET (gtk_builder_get_object (builder, name));
	g_return_if_fail (GTK_IS_LABEL (widget));
	if(value)
	{
		formatted = g_new (gchar, 11);
		g_date_strftime (formatted, 11, "%Y-%m-%d", value);
		gtk_label_set_text (GTK_LABEL (widget), formatted);
		g_free (formatted);
	}
	else
	{
		gtk_label_set_markup (GTK_LABEL (widget), _("<i>unknown</i>"));
	}
}

static void
refresh_display (GcrCertificateBasicsWidget *self)
{
	gchar *value;
	GDate *date;
	
	/* Issued To / Subject */
	
	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_subject_cn (self->pv->certificate);
	set_certificate_part_label (self->pv->builder, "issued-to-cn", value);
	g_free (value);
	
	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_subject_part (self->pv->certificate, "o");
	set_certificate_part_label (self->pv->builder, "issued-to-o", value);
	g_free (value);

	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_subject_part (self->pv->certificate, "ou");
	set_certificate_part_label (self->pv->builder, "issued-to-ou", value);
	g_free (value);

	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_serial_number_hex (self->pv->certificate);
	set_certificate_part_label (self->pv->builder, "issued-to-serial", value);
	g_free (value);
	
	
	/* Issued By / Issuer */
	
	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_issuer_cn (self->pv->certificate);
	set_certificate_part_label (self->pv->builder, "issued-by-cn", value);
	g_free (value);
	
	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_issuer_part (self->pv->certificate, "o");
	set_certificate_part_label (self->pv->builder, "issued-by-o", value);
	g_free (value);

	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_issuer_part (self->pv->certificate, "ou");
	set_certificate_part_label (self->pv->builder, "issued-by-ou", value);
	g_free (value);

	
	/* Expiry */
	
	date = NULL;
	if (self->pv->certificate)
		date = gcr_certificate_get_issued_date (self->pv->certificate);
	set_certificate_part_date (self->pv->builder, "validity-issued-on", date);
	if (date)
		g_date_free (date);
	
	date = NULL;
	if (self->pv->certificate)
		date = gcr_certificate_get_expiry_date (self->pv->certificate);
	set_certificate_part_date (self->pv->builder, "validity-expires-on", date);
	if (date)
		g_date_free (date);

	
	/* Fingerprints */
	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_fingerprint_hex (self->pv->certificate, G_CHECKSUM_SHA1);
	set_certificate_part_label (self->pv->builder, "fingerprints-sha1", value);
	g_free (value);
	
	value = NULL;
	if (self->pv->certificate)
		value = gcr_certificate_get_fingerprint_hex (self->pv->certificate, G_CHECKSUM_MD5);
	set_certificate_part_label (self->pv->builder, "fingerprints-md5", value);
	g_free (value);
}

/* -----------------------------------------------------------------------------
 * OBJECT 
 */


static GObject* 
gcr_certificate_basics_widget_constructor (GType type, guint n_props, GObjectConstructParam *props) 
{
	GObject *obj = G_OBJECT_CLASS (gcr_certificate_basics_widget_parent_class)->constructor (type, n_props, props);
	GcrCertificateBasicsWidget *self = NULL;
	GtkWidget *widget;
	
	if (obj) {
		self = GCR_CERTIFICATE_BASICS_WIDGET (obj);
		
		if (!gtk_builder_add_from_file (self->pv->builder, UIDIR "gcr-certificate-basics-widget.ui", NULL))
			g_return_val_if_reached (obj);
	
		widget = GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "certificate-basics-widget"));
		g_return_val_if_fail (GTK_IS_WIDGET (widget), obj);
		gtk_container_add (GTK_CONTAINER (self), widget);
		gtk_widget_show (widget);
	}
	
	return obj;
}

static void
gcr_certificate_basics_widget_init (GcrCertificateBasicsWidget *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_CERTIFICATE_BASICS_WIDGET, GcrCertificateBasicsWidgetPrivate));
	self->pv->builder = gtk_builder_new ();
}

static void
gcr_certificate_basics_widget_dispose (GObject *obj)
{
	GcrCertificateBasicsWidget *self = GCR_CERTIFICATE_BASICS_WIDGET (obj);
	
	if (self->pv->certificate)
		g_object_unref (self->pv->certificate);
	self->pv->certificate = NULL;
	
	G_OBJECT_CLASS (gcr_certificate_basics_widget_parent_class)->dispose (obj);
}

static void
gcr_certificate_basics_widget_finalize (GObject *obj)
{
	GcrCertificateBasicsWidget *self = GCR_CERTIFICATE_BASICS_WIDGET (obj);

	g_assert (!self->pv->certificate);
	
	G_OBJECT_CLASS (gcr_certificate_basics_widget_parent_class)->finalize (obj);
}

static void
gcr_certificate_basics_widget_set_property (GObject *obj, guint prop_id, const GValue *value, 
                                    GParamSpec *pspec)
{
	GcrCertificateBasicsWidget *self = GCR_CERTIFICATE_BASICS_WIDGET (obj);
	
	switch (prop_id) {
	case PROP_CERTIFICATE:
		gcr_certificate_basics_widget_set_certificate (self, g_value_get_object (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_certificate_basics_widget_get_property (GObject *obj, guint prop_id, GValue *value, 
                                    GParamSpec *pspec)
{
	GcrCertificateBasicsWidget *self = GCR_CERTIFICATE_BASICS_WIDGET (obj);
	
	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_value_set_object (value, self->pv->certificate);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_certificate_basics_widget_class_init (GcrCertificateBasicsWidgetClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    
	gcr_certificate_basics_widget_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GcrCertificateBasicsWidgetPrivate));

	gobject_class->constructor = gcr_certificate_basics_widget_constructor;
	gobject_class->dispose = gcr_certificate_basics_widget_dispose;
	gobject_class->finalize = gcr_certificate_basics_widget_finalize;
	gobject_class->set_property = gcr_certificate_basics_widget_set_property;
	gobject_class->get_property = gcr_certificate_basics_widget_get_property;
    
	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object("certificate", "Certificate", "Certificate to display.", 
	                               GCR_TYPE_CERTIFICATE, G_PARAM_READWRITE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC 
 */

GcrCertificateBasicsWidget*
gcr_certificate_basics_widget_new (GcrCertificate *certificate)
{
	return g_object_new (GCR_TYPE_CERTIFICATE_BASICS_WIDGET, "certificate", certificate, NULL);
}

GcrCertificate*
gcr_certificate_basics_widget_get_certificate (GcrCertificateBasicsWidget *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_BASICS_WIDGET (self), NULL);
	return self->pv->certificate;
}

void
gcr_certificate_basics_widget_set_certificate (GcrCertificateBasicsWidget *self, GcrCertificate *cert)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_BASICS_WIDGET (self));
	
	if (self->pv->certificate)
		g_object_unref (self->pv->certificate);
	self->pv->certificate = cert;
	if (self->pv->certificate)
		g_object_ref (self->pv->certificate);	
	
	refresh_display (self);
	g_object_notify (G_OBJECT (self), "certificate");
}
