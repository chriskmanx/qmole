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
#include "gcr-certificate-details-widget.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-dn.h"
#include "egg/egg-oid.h"
#include "egg/egg-hex.h"

#include <glib/gi18n-lib.h>

enum {
	PROP_0,
	PROP_CERTIFICATE
};

struct _GcrCertificateDetailsWidgetPrivate {
	GcrCertificate *certificate;
	GtkTextView *view;
	GtkTextBuffer *buffer;
	GtkTextTag *field_tag;
	gint field_width;
	guint key_size;
};

G_DEFINE_TYPE (GcrCertificateDetailsWidget, gcr_certificate_details_widget, GTK_TYPE_ALIGNMENT);

#define FIELD_MARGIN 17
#define COLUMN_MARGIN 6

/* -----------------------------------------------------------------------------
 * INTERNAL 
 */

static GtkTextTagTable*
create_tag_table (GcrCertificateDetailsWidget *self)
{
	GtkTextTagTable *tags;
	GtkTextTag *tag;

	g_assert (GCR_IS_CERTIFICATE_DETAILS_WIDGET (self));
	
	tags = gtk_text_tag_table_new ();
	
	tag = g_object_new (GTK_TYPE_TEXT_TAG,
	                    "name", "heading",
	                    "left-margin", 5, 
	                    "right-margin", 5,
	                    "pixels-above-lines", 9,
	                    "pixels-below-lines", 3,
	                    "weight", PANGO_WEIGHT_BOLD,
	                    NULL);
	
	gtk_text_tag_table_add (tags, tag);
	g_object_unref (tag);
	
	tag = g_object_new (GTK_TYPE_TEXT_TAG,
	                    "name", "monospace",
	                    "family", "monospace",
	                    NULL);
	
	gtk_text_tag_table_add (tags, tag);
	g_object_unref (tag);	

	g_assert (!self->pv->field_tag);
	self->pv->field_width = 0;
	self->pv->field_tag = g_object_new (GTK_TYPE_TEXT_TAG,
	                                    "name", "field",
	                                    "left-margin", self->pv->field_width + FIELD_MARGIN,
	                                    "indent", self->pv->field_width,
	                                    "right-margin", 5,
	                                    "pixels-below-lines", 3,
	                                    "wrap-mode", GTK_WRAP_WORD_CHAR,
	                                    NULL);
	gtk_text_tag_table_add (tags, self->pv->field_tag);
	
	return tags;
}

static void
append_field_and_value (GcrCertificateDetailsWidget *self, const gchar *field, 
                        const gchar *value, gboolean monospace)
{
	PangoRectangle extents;
	PangoTabArray *tabs;
	PangoLayout *layout;
	GtkTextIter iter;
	gchar *text;
	
	text = g_strdup_printf ("%s:", field);
	if (value == NULL)
		value = "";
	
	/* Measure the width of the field */
	layout = gtk_widget_create_pango_layout (GTK_WIDGET (self), text);
	pango_layout_get_extents (layout, NULL, &extents);
	pango_extents_to_pixels (&extents, NULL);
	g_object_unref (layout);
	
	/* Make the tab wide enough to accomodate */
	if (extents.width > self->pv->field_width) {
		self->pv->field_width = extents.width + COLUMN_MARGIN;
		tabs = pango_tab_array_new (1, TRUE);
		pango_tab_array_set_tab (tabs, 0, PANGO_TAB_LEFT, self->pv->field_width);
		g_object_set (self->pv->field_tag, 
		              "left-margin", FIELD_MARGIN,
		              "indent", 0 - self->pv->field_width,
		              "tabs", tabs,
		              NULL);
		pango_tab_array_free (tabs);
	}
	
	gtk_text_buffer_get_end_iter (self->pv->buffer, &iter);
	gtk_text_buffer_insert_with_tags_by_name (self->pv->buffer, &iter, text, -1, "field", NULL);
	gtk_text_buffer_insert (self->pv->buffer, &iter, "\t", 1);
	gtk_text_buffer_insert_with_tags_by_name (self->pv->buffer, &iter, value, -1, "field", 
	                                          monospace ? "monospace" : NULL, NULL);
	gtk_text_buffer_insert (self->pv->buffer, &iter, "\n", 1);
	
	g_free (text);
}

static void
append_heading (GcrCertificateDetailsWidget *self, const gchar *heading)
{
	GtkTextIter iter;
	
	gtk_text_buffer_get_end_iter (self->pv->buffer, &iter);
	gtk_text_buffer_insert_with_tags_by_name (self->pv->buffer, &iter, heading, -1, "heading", NULL);
	gtk_text_buffer_insert (self->pv->buffer, &iter, "\n", 1);
}

static void
append_fingerprint (GcrCertificateDetailsWidget *self, const guchar *data, 
                    gsize n_data, const gchar *name, GChecksumType type)
{
	GChecksum *checksum;
	guint8 *buffer;
	gsize n_buffer;
	gchar *display;
	
	checksum = g_checksum_new (type);
	g_return_if_fail (checksum);
	g_checksum_update (checksum, data, n_data);
	
	n_buffer = g_checksum_type_get_length (type);
	g_return_if_fail (n_buffer);
	buffer = g_malloc0 (n_buffer);
	
	g_checksum_get_digest (checksum, buffer, &n_buffer);
	g_checksum_free (checksum);
	
	display = egg_hex_encode_full (buffer, n_buffer, TRUE, ' ', 1);
	append_field_and_value (self, name, display, TRUE);
	g_free (display);
	
	g_free (buffer);		
}

static gboolean
append_extension (GcrCertificateDetailsWidget *self, GNode *asn,
                  const guchar *data, gsize n_data, gint index)
{
	GQuark oid;
	gchar *display;
	gsize n_value;
	const guchar *value;
	const gchar *text;
	gboolean critical;
	GNode *node;

	/* Make sure it is present */
	asn = egg_asn1x_node (asn, "tbsCertificate", "extensions", index, NULL);
	if (asn == NULL)
		return FALSE;

	/* Dig out the OID */
	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "extnID", NULL));
	g_return_val_if_fail (oid, FALSE);


	append_heading (self, _("Extension"));


	/* Extension type */
	text = egg_oid_get_description (oid);
	append_field_and_value (self, _("Identifier"), text, FALSE);


	/* Extension value */
	value = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "extnValue", NULL), &n_value);

	/* TODO: Parsing of extensions that we understand */
	display = egg_hex_encode_full (value, n_value, TRUE, ' ', 1);
	append_field_and_value (self, _("Value"), display, TRUE);
	g_free (display);


	/* Critical */
	node = egg_asn1x_node (asn, "critical", NULL);
	if (node != NULL) {
		if (egg_asn1x_get_boolean (node, &critical))
			append_field_and_value (self, _("Critical"), critical ? _("Yes") : _("No"), FALSE);
	}

	return TRUE;
}

static void
on_parsed_dn_part (guint index, GQuark oid, const guchar *value,
                   gsize n_value, gpointer user_data)
{
	GcrCertificateDetailsWidget *self = user_data;
	const gchar *attr;
	const gchar *desc;
	gchar *field;
	gchar *display;
	
	g_return_if_fail (GCR_IS_CERTIFICATE_DETAILS_WIDGET (self));
	
	attr = egg_oid_get_name (oid);
	desc = egg_oid_get_description (oid);
	
	/* Combine them into something sane */
	if (attr && desc) {
		if (strcmp (attr, desc) == 0)
			field = g_strdup (attr);
		else 
			field = g_strdup_printf ("%s (%s)", attr, desc);
	} else if (!attr && !desc) {
		field = g_strdup ("");
	} else if (attr) {
		field = g_strdup (attr);
	} else if (desc) {
		field = g_strdup (desc);
	} else {
		g_assert_not_reached ();
	}
	
	display = egg_dn_print_value (oid, value, n_value);
	if (display == NULL)
		display = g_strdup ("");
	
	append_field_and_value (self, field, display, FALSE);
	g_free (field);
	g_free (display);
}

static void
refresh_display (GcrCertificateDetailsWidget *self)
{
	GtkTextIter start, iter;
	const guchar *data, *value;
	gsize n_data, n_value;
	const gchar *text;
	gulong version;
	guint index, size, n_bits;
	gchar *display;
	guchar *bits;
	GNode *asn;
	GQuark oid;
	GDate date;

	gtk_text_buffer_get_start_iter (self->pv->buffer, &start);
	gtk_text_buffer_get_end_iter (self->pv->buffer, &iter);
	gtk_text_buffer_delete (self->pv->buffer, &start, &iter);
	
	if (!self->pv->certificate)
		return;
	
	data = gcr_certificate_get_der_data (self->pv->certificate, &n_data);
	g_return_if_fail (data);

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", data, n_data);
	g_return_if_fail (asn);

	/* The subject */
	append_heading (self, _("Subject Name"));
	egg_dn_parse (egg_asn1x_node (asn, "tbsCertificate", "subject", "rdnSequence", NULL), on_parsed_dn_part, self);

	/* The Issuer */
	append_heading (self, _("Issuer Name"));
	egg_dn_parse (egg_asn1x_node (asn, "tbsCertificate", "issuer", "rdnSequence", NULL), on_parsed_dn_part, self);

	/* The Issued Parameters */
	append_heading (self, _("Issued Certificate"));

	if (!egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "tbsCertificate", "version", NULL), &version))
		g_return_if_reached ();
	display = g_strdup_printf ("%lu", version + 1);
	append_field_and_value (self, _("Version"), display, FALSE);
	g_free (display);

	value = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "tbsCertificate", "serialNumber", NULL), &n_value);
	g_return_if_fail (value);
	display = egg_hex_encode_full (value, n_value, TRUE, ' ', 1);
	append_field_and_value (self, _("Serial Number"), display, TRUE);
	g_free (display);
	
	display = g_malloc0 (128);
	if (egg_asn1x_get_time_as_date (egg_asn1x_node (asn, "tbsCertificate", "validity", "notBefore", NULL), &date)) {
		if (!g_date_strftime (display, 128, "%Y-%m-%d", &date))
			g_return_if_reached ();
		append_field_and_value (self, _("Not Valid Before"), display, FALSE);
	}
	if (egg_asn1x_get_time_as_date (egg_asn1x_node (asn, "tbsCertificate", "validity", "notAfter", NULL), &date)) {
		if (!g_date_strftime (display, 128, "%Y-%m-%d", &date))
			g_return_if_reached ();
		append_field_and_value (self, _("Not Valid After"), display, FALSE);
	}
	g_free (display);
	
	/* Signature */
	append_heading (self, _("Signature"));

	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "signatureAlgorithm", "algorithm", NULL));
	text = egg_oid_get_description (oid);
	append_field_and_value (self, _("Signature Algorithm"), text, FALSE);

	value = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "signatureAlgorithm", "parameters", NULL), &n_value);
	if (value && n_value) {
		display = egg_hex_encode_full (value, n_value, TRUE, ' ', 1);
		append_field_and_value (self, _("Signature Parameters"), display, TRUE);
		g_free (display);
	}
	
	value = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "signature", NULL), &n_value);
	g_return_if_fail (value);
	display = egg_hex_encode_full (value, n_value, TRUE, ' ', 1);
	append_field_and_value (self, _("Signature"), display, TRUE);
	g_free (display);

	/* Public Key Info */
	append_heading (self, _("Public Key Info"));

	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "tbsCertificate", "subjectPublicKeyInfo", "algorithm", "algorithm", NULL));
	text = egg_oid_get_description (oid);
	append_field_and_value (self, _("Key Algorithm"), text, FALSE);

	value = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "tbsCertificate", "subjectPublicKeyInfo", "algorithm", "parameters", NULL), &n_value);
	if (value && n_value) {
		display = egg_hex_encode_full (value, n_value, TRUE, ' ', 1);
		append_field_and_value (self, _("Key Parameters"), display, TRUE);
		g_free (display);
	}

	size = gcr_certificate_get_key_size (self->pv->certificate);
	if (size > 0) {
		display = g_strdup_printf ("%u", size); 
		append_field_and_value (self, _("Key Size"), display, FALSE);
		g_free (display);
	}

	bits = egg_asn1x_get_bits_as_raw (egg_asn1x_node (asn, "tbsCertificate", "subjectPublicKeyInfo", "subjectPublicKey", NULL), NULL, &n_bits);
	g_return_if_fail (bits);
	display = egg_hex_encode_full (bits, n_bits / 8, TRUE, ' ', 1);
	append_field_and_value (self, _("Public Key"), display, TRUE);
	g_free (display);
	g_free (bits);

	/* Fingerprints */
	append_heading (self, _("Fingerprints"));
	
	append_fingerprint (self, data, n_data, "SHA1", G_CHECKSUM_SHA1);
	append_fingerprint (self, data, n_data, "MD5", G_CHECKSUM_MD5);
	
	/* Extensions */
	for (index = 1; TRUE; ++index) {
		if (!append_extension (self, asn, data, n_data, index))
			break;
	}

	egg_asn1x_destroy (asn);
}

/* -----------------------------------------------------------------------------
 * OBJECT 
 */

static GObject* 
gcr_certificate_details_widget_constructor (GType type, guint n_props, GObjectConstructParam *props) 
{
	GObject *obj = G_OBJECT_CLASS (gcr_certificate_details_widget_parent_class)->constructor (type, n_props, props);
	GcrCertificateDetailsWidget *self = NULL;
	GtkTextTagTable *tags;
	GtkWidget *widget; 
	GtkWidget *scroll;
	
	g_return_val_if_fail (obj, NULL);
	
	self = GCR_CERTIFICATE_DETAILS_WIDGET (obj);
	
	tags = create_tag_table (self);
	self->pv->buffer = gtk_text_buffer_new (tags);
	g_object_unref (tags);
	
	widget = gtk_text_view_new_with_buffer (self->pv->buffer);
	self->pv->view = GTK_TEXT_VIEW (widget);
	gtk_text_view_set_editable (self->pv->view, FALSE);
	
	scroll = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_container_add (GTK_CONTAINER (scroll), widget);
	
	gtk_container_add (GTK_CONTAINER (self), scroll);
	gtk_widget_show_all (scroll);
	
	return obj;
}

static void
gcr_certificate_details_widget_init (GcrCertificateDetailsWidget *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_CERTIFICATE_DETAILS_WIDGET, GcrCertificateDetailsWidgetPrivate));
}

static void
gcr_certificate_details_widget_dispose (GObject *obj)
{
	GcrCertificateDetailsWidget *self = GCR_CERTIFICATE_DETAILS_WIDGET (obj);
	
	if (self->pv->certificate)
		g_object_unref (self->pv->certificate);
	self->pv->certificate = NULL;
	
	G_OBJECT_CLASS (gcr_certificate_details_widget_parent_class)->dispose (obj);
}

static void
gcr_certificate_details_widget_finalize (GObject *obj)
{
	GcrCertificateDetailsWidget *self = GCR_CERTIFICATE_DETAILS_WIDGET (obj);

	g_assert (!self->pv->certificate);
	
	if (self->pv->buffer)
		g_object_unref (self->pv->buffer);
	self->pv->buffer = NULL;

	if (self->pv->field_tag)
		g_object_unref (self->pv->field_tag);
	self->pv->field_tag = NULL;
	
	G_OBJECT_CLASS (gcr_certificate_details_widget_parent_class)->finalize (obj);
}

static void
gcr_certificate_details_widget_set_property (GObject *obj, guint prop_id, const GValue *value, 
                                             GParamSpec *pspec)
{
	GcrCertificateDetailsWidget *self = GCR_CERTIFICATE_DETAILS_WIDGET (obj);
	
	switch (prop_id) {
	case PROP_CERTIFICATE:
		gcr_certificate_details_widget_set_certificate (self, g_value_get_object (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_certificate_details_widget_get_property (GObject *obj, guint prop_id, GValue *value, 
                                             GParamSpec *pspec)
{
	GcrCertificateDetailsWidget *self = GCR_CERTIFICATE_DETAILS_WIDGET (obj);
	
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
gcr_certificate_details_widget_class_init (GcrCertificateDetailsWidgetClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    
	gcr_certificate_details_widget_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GcrCertificateDetailsWidgetPrivate));

	gobject_class->constructor = gcr_certificate_details_widget_constructor;
	gobject_class->dispose = gcr_certificate_details_widget_dispose;
	gobject_class->finalize = gcr_certificate_details_widget_finalize;
	gobject_class->set_property = gcr_certificate_details_widget_set_property;
	gobject_class->get_property = gcr_certificate_details_widget_get_property;
    
	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object("certificate", "Certificate", "Certificate to display.", 
	                               GCR_TYPE_CERTIFICATE, G_PARAM_READWRITE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC 
 */

GcrCertificateDetailsWidget*
gcr_certificate_details_widget_new (GcrCertificate *certificate)
{
	return g_object_new (GCR_TYPE_CERTIFICATE_DETAILS_WIDGET, "certificate", certificate, NULL);
}

GcrCertificate*
gcr_certificate_details_widget_get_certificate (GcrCertificateDetailsWidget *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_DETAILS_WIDGET (self), NULL);
	return self->pv->certificate;
}

void
gcr_certificate_details_widget_set_certificate (GcrCertificateDetailsWidget *self, GcrCertificate *cert)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_DETAILS_WIDGET (self));
	
	if (self->pv->certificate)
		g_object_unref (self->pv->certificate);
	self->pv->certificate = cert;
	if (self->pv->certificate)
		g_object_ref (self->pv->certificate);	
	
	refresh_display (self);
	g_object_notify (G_OBJECT (self), "certificate");
}
