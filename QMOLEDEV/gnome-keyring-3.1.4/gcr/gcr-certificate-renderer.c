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
#include "gcr-certificate-exporter.h"
#include "gcr-certificate-extensions.h"
#include "gcr-certificate-renderer.h"
#include "gcr-display-view.h"
#include "gcr-fingerprint.h"
#include "gcr-icons.h"
#include "gcr-simple-certificate.h"
#include "gcr-renderer.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-dn.h"
#include "egg/egg-oid.h"
#include "egg/egg-hex.h"

#include "gck/gck.h"

#include <gdk/gdk.h>
#include <glib/gi18n-lib.h>

/**
 * GcrCertificateRenderer:
 *
 * An implementation of #GcrRenderer which renders certificates.
 */

/**
 * GcrCertificateRendererClass:
 * @parent_class: The parent class.
 *
 * The class for #GcrCertificateRenderer.
 */

enum {
	PROP_0,
	PROP_CERTIFICATE,
	PROP_LABEL,
	PROP_ATTRIBUTES
};

struct _GcrCertificateRendererPrivate {
	GcrCertificate *opt_cert;
	GckAttributes *opt_attrs;
	guint key_size;
	gchar *label;
};

static void gcr_renderer_iface_init (GcrRendererIface *iface);
static void gcr_renderer_certificate_iface_init (GcrCertificateIface *iface);

G_DEFINE_TYPE_WITH_CODE (GcrCertificateRenderer, gcr_certificate_renderer, G_TYPE_OBJECT,
	G_IMPLEMENT_INTERFACE (GCR_TYPE_RENDERER, gcr_renderer_iface_init);
	GCR_CERTIFICATE_MIXIN_IMPLEMENT_COMPARABLE ();
	G_IMPLEMENT_INTERFACE (GCR_TYPE_CERTIFICATE, gcr_renderer_certificate_iface_init);
);

static GQuark OID_BASIC_CONSTRAINTS = 0;
static GQuark OID_EXTENDED_KEY_USAGE = 0;
static GQuark OID_SUBJECT_KEY_IDENTIFIER = 0;
static GQuark OID_KEY_USAGE = 0;
static GQuark OID_SUBJECT_ALT_NAME = 0;

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static gchar*
calculate_label (GcrCertificateRenderer *self)
{
	gchar *label;

	if (self->pv->label)
		return g_strdup (self->pv->label);

	if (self->pv->opt_attrs) {
		if (gck_attributes_find_string (self->pv->opt_attrs, CKA_LABEL, &label))
			return label;
	}

	label = gcr_certificate_get_subject_cn (GCR_CERTIFICATE (self));
	if (label != NULL)
		return label;

	return g_strdup (_("Certificate"));
}

static gboolean
append_extension_basic_constraints (GcrCertificateRenderer *self, GcrDisplayView *view,
                                    gconstpointer data, gsize n_data)
{
	GcrRenderer *renderer = GCR_RENDERER (self);
	gboolean is_ca = FALSE;
	gint path_len = -1;
	gchar *number;

	if (!_gcr_certificate_extension_basic_constraints (data, n_data, &is_ca, &path_len))
		return FALSE;

	_gcr_display_view_append_heading (view, renderer, _("Basic Constraints"));

	_gcr_display_view_append_value (view, renderer, _("Certificate Authority"),
	                                is_ca ? _("Yes") : _("No"), FALSE);

	number = g_strdup_printf ("%d", path_len);
	_gcr_display_view_append_value (view, renderer, _("Max Path Length"),
	                                path_len < 0 ? _("Unlimited") : number, FALSE);
	g_free (number);

	return TRUE;
}

static gboolean
append_extension_extended_key_usage (GcrCertificateRenderer *self, GcrDisplayView *view,
                                     gconstpointer data, gsize n_data)
{
	GcrRenderer *renderer = GCR_RENDERER (self);
	GQuark *oids;
	GString *text;
	guint i;

	oids = _gcr_certificate_extension_extended_key_usage (data, n_data);
	if (oids == NULL)
		return FALSE;

	_gcr_display_view_append_heading (view, renderer, _("Extended Key Usage"));

	text = g_string_new ("");
	for (i = 0; oids[i] != 0; i++) {
		if (i > 0)
			g_string_append_unichar (text, GCR_DISPLAY_VIEW_LINE_BREAK);
		g_string_append (text, egg_oid_get_description (oids[i]));
	}

	g_free (oids);

	_gcr_display_view_append_value (view, renderer, _("Allowed Purposes"),
	                                text->str, FALSE);

	g_string_free (text, TRUE);

	return TRUE;
}

static gboolean
append_extension_subject_key_identifier (GcrCertificateRenderer *self, GcrDisplayView *view,
                                         gconstpointer data, gsize n_data)
{
	GcrRenderer *renderer = GCR_RENDERER (self);
	gpointer keyid;
	gsize n_keyid;

	keyid = _gcr_certificate_extension_subject_key_identifier (data, n_data, &n_keyid);
	if (keyid == NULL)
		return FALSE;

	_gcr_display_view_append_heading (view, renderer, _("Subject Key Identifier"));
	_gcr_display_view_append_hex (view, renderer, _("Key Identifier"), keyid, n_keyid);

	g_free (keyid);

	return TRUE;
}

static const struct {
	guint usage;
	const gchar *description;
} usage_descriptions[] = {
	{ GCR_KEY_USAGE_DIGITAL_SIGNATURE, N_("Digital signature") },
	{ GCR_KEY_USAGE_KEY_ENCIPHERMENT, N_("Key encipherment") },
	{ GCR_KEY_USAGE_DATA_ENCIPHERMENT, N_("Data encipherment") },
	{ GCR_KEY_USAGE_KEY_AGREEMENT, N_("Key agreement") },
	{ GCR_KEY_USAGE_KEY_CERT_SIGN, N_("Certificate signature") },
	{ GCR_KEY_USAGE_CRL_SIGN, N_("Revocation list signature") }
};

static gboolean
append_extension_key_usage (GcrCertificateRenderer *self, GcrDisplayView *view,
                            gconstpointer data, gsize n_data)
{
	GcrRenderer *renderer = GCR_RENDERER (self);
	gulong key_usage;
	GString *text;
	guint i;

	if (!_gcr_certificate_extension_key_usage (data, n_data, &key_usage))
		return FALSE;

	text = g_string_new ("");

	for (i = 0; i < G_N_ELEMENTS (usage_descriptions); i++) {
		if (key_usage & usage_descriptions[i].usage) {
			if (text->len > 0)
				g_string_append_unichar (text, GCR_DISPLAY_VIEW_LINE_BREAK);
			g_string_append (text, gettext (usage_descriptions[i].description));
		}
	}

	_gcr_display_view_append_heading (view, renderer, _("Key Usage"));
	_gcr_display_view_append_value (view, renderer, _("Usages"), text->str, FALSE);

	g_string_free (text, TRUE);

	return TRUE;
}

static gboolean
append_extension_subject_alt_name (GcrCertificateRenderer *self, GcrDisplayView *view,
                                   gconstpointer data, gsize n_data)
{
	GcrRenderer *renderer = GCR_RENDERER (self);
	GArray *general_names;
	GcrGeneralName *general;
	guint i;

	general_names = _gcr_certificate_extension_subject_alt_name (data, n_data);
	if (general_names == NULL)
		return FALSE;

	_gcr_display_view_append_heading (view, renderer, _("Subject Alternative Names"));

	for (i = 0; i < general_names->len; i++) {
		general = &g_array_index (general_names, GcrGeneralName, i);
		if (general->display == NULL)
			_gcr_display_view_append_hex (view, renderer, general->description,
			                              general->raw, general->n_raw);
		else
			_gcr_display_view_append_value (view, renderer, general->description,
			                                general->display, FALSE);
	}

	_gcr_general_names_free (general_names);

	return TRUE;
}


static gboolean
append_extension_hex (GcrCertificateRenderer *self, GcrDisplayView *view,
                      GQuark oid, gconstpointer data, gsize n_data)
{
	GcrRenderer *renderer = GCR_RENDERER (self);
	const gchar *text;

	_gcr_display_view_append_heading (view, renderer, _("Extension"));

	/* Extension type */
	text = egg_oid_get_description (oid);
	_gcr_display_view_append_value (view, renderer, _("Identifier"), text, FALSE);
	_gcr_display_view_append_hex (view, renderer, _("Value"), data, n_data);

	return TRUE;
}

static gboolean
append_extension (GcrCertificateRenderer *self, GcrDisplayView *view,
                  GNode *asn, const guchar *data, gsize n_data, gint index)
{
	GcrRenderer *renderer = GCR_RENDERER (self);
	GNode *node;
	GQuark oid;
	gsize n_value;
	const guchar *value;
	gboolean critical;
	gboolean ret = FALSE;

	/* Make sure it is present */
	node = egg_asn1x_node (asn, "tbsCertificate", "extensions", index, NULL);
	if (node == NULL)
		return FALSE;

	/* Dig out the OID */
	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (node, "extnID", NULL));
	g_return_val_if_fail (oid, FALSE);

	/* Extension value */
	value = egg_asn1x_get_raw_value (egg_asn1x_node (node, "extnValue", NULL), &n_value);

	/* The custom parsers */
	if (oid == OID_BASIC_CONSTRAINTS)
		ret = append_extension_basic_constraints (self, view, value, n_value);
	else if (oid == OID_EXTENDED_KEY_USAGE)
		ret = append_extension_extended_key_usage (self, view, value, n_value);
	else if (oid == OID_SUBJECT_KEY_IDENTIFIER)
		ret = append_extension_subject_key_identifier (self, view, value, n_value);
	else if (oid == OID_KEY_USAGE)
		ret = append_extension_key_usage (self, view, value, n_value);
	else if (oid == OID_SUBJECT_ALT_NAME)
		ret = append_extension_subject_alt_name (self, view, value, n_value);

	/* Otherwise the default raw display */
	if (ret == FALSE)
		ret = append_extension_hex (self, view, oid, value, n_value);

	/* Critical */
	if (ret == TRUE && egg_asn1x_get_boolean (egg_asn1x_node (node, "critical", NULL), &critical)) {
		_gcr_display_view_append_value (view, renderer, _("Critical"),
		                                critical ? _("Yes") : _("No"), FALSE);
	}

	return ret;
}

typedef struct _on_parsed_dn_args {
	GcrCertificateRenderer *renderer;
	GcrDisplayView *view;
} on_parsed_dn_args;

static void
on_parsed_dn_part (guint index, GQuark oid, const guchar *value,
                   gsize n_value, gpointer user_data)
{
	GcrCertificateRenderer *self = ((on_parsed_dn_args*)user_data)->renderer;
	GcrDisplayView *view = ((on_parsed_dn_args*)user_data)->view;
	const gchar *attr;
	const gchar *desc;
	gchar *field;
	gchar *display;

	g_return_if_fail (GCR_IS_CERTIFICATE_RENDERER (self));

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

	_gcr_display_view_append_value (view, GCR_RENDERER (self), field, display, FALSE);
	g_free (field);
	g_free (display);
}

static gboolean
on_delete_unref_dialog (GtkWidget *widget, GdkEvent *event, gpointer data)
{
	g_object_unref (widget);
	return FALSE;
}

static void
on_export_completed (GObject *source, GAsyncResult *result, gpointer user_data)
{
	GtkWindow *parent = GTK_WINDOW (user_data);
	GcrCertificateExporter *exporter = GCR_CERTIFICATE_EXPORTER (source);
	GError *error = NULL;
	GtkWidget *dialog;

	if (!_gcr_certificate_exporter_export_finish (exporter, result, &error)) {
		if (!g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED)) {
			dialog = gtk_message_dialog_new_with_markup (parent,
				  GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR,
				  GTK_BUTTONS_OK, "<big>%s</big>\n\n%s",
				  _("Couldn't export the certificate."),
				  error->message);
			gtk_widget_show (dialog);
			g_signal_connect (dialog, "delete-event",
					  G_CALLBACK (on_delete_unref_dialog), NULL);
		}
	}

	/* Matches ref in on_certificate_export */
	if (parent)
		g_object_unref (parent);
}

static void
on_certificate_export (GtkMenuItem *menuitem, gpointer user_data)
{
	GcrCertificateRenderer *self = GCR_CERTIFICATE_RENDERER (user_data);
	GcrCertificateExporter *exporter;
	gchar *label;
	GtkWidget *parent;

	label = calculate_label (self);

	parent = gtk_widget_get_toplevel (GTK_WIDGET (menuitem));
	if (parent && !GTK_IS_WINDOW (parent))
		parent = NULL;

	exporter = _gcr_certificate_exporter_new (GCR_CERTIFICATE (self), label,
	                                          GTK_WINDOW (parent));

	g_free (label);

	_gcr_certificate_exporter_export_async (exporter, NULL, on_export_completed,
	                                        parent ? g_object_ref (parent) : NULL);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gcr_certificate_renderer_init (GcrCertificateRenderer *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_CERTIFICATE_RENDERER, GcrCertificateRendererPrivate));
}

static void
gcr_certificate_renderer_dispose (GObject *obj)
{
	GcrCertificateRenderer *self = GCR_CERTIFICATE_RENDERER (obj);

	if (self->pv->opt_cert)
		g_object_unref (self->pv->opt_cert);
	self->pv->opt_cert = NULL;

	G_OBJECT_CLASS (gcr_certificate_renderer_parent_class)->dispose (obj);
}

static void
gcr_certificate_renderer_finalize (GObject *obj)
{
	GcrCertificateRenderer *self = GCR_CERTIFICATE_RENDERER (obj);

	g_assert (!self->pv->opt_cert);

	if (self->pv->opt_attrs)
		gck_attributes_unref (self->pv->opt_attrs);
	self->pv->opt_attrs = NULL;

	g_free (self->pv->label);
	self->pv->label = NULL;

	G_OBJECT_CLASS (gcr_certificate_renderer_parent_class)->finalize (obj);
}

static void
gcr_certificate_renderer_set_property (GObject *obj, guint prop_id, const GValue *value,
                                     GParamSpec *pspec)
{
	GcrCertificateRenderer *self = GCR_CERTIFICATE_RENDERER (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		gcr_certificate_renderer_set_certificate (self, g_value_get_object (value));
		break;
	case PROP_LABEL:
		g_free (self->pv->label);
		self->pv->label = g_value_dup_string (value);
		g_object_notify (obj, "label");
		gcr_renderer_emit_data_changed (GCR_RENDERER (self));
		break;
	case PROP_ATTRIBUTES:
		gcr_certificate_renderer_set_attributes (self, g_value_get_boxed (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_certificate_renderer_get_property (GObject *obj, guint prop_id, GValue *value,
                                     GParamSpec *pspec)
{
	GcrCertificateRenderer *self = GCR_CERTIFICATE_RENDERER (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_value_set_object (value, self->pv->opt_cert);
		break;
	case PROP_LABEL:
		g_value_take_string (value, calculate_label (self));
		break;
	case PROP_ATTRIBUTES:
		g_value_set_boxed (value, self->pv->opt_attrs);
		break;
	default:
		gcr_certificate_mixin_get_property (obj, prop_id, value, pspec);
		break;
	}
}

static void
gcr_certificate_renderer_class_init (GcrCertificateRendererClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GckAttributes *registered;

	OID_SUBJECT_KEY_IDENTIFIER = g_quark_from_static_string ("2.5.29.14");
	OID_BASIC_CONSTRAINTS = g_quark_from_static_string ("2.5.29.19");
	OID_EXTENDED_KEY_USAGE = g_quark_from_static_string ("2.5.29.37");
	OID_KEY_USAGE = g_quark_from_static_string ("2.5.29.15");
	OID_SUBJECT_ALT_NAME = g_quark_from_static_string ("2.5.29.17");

	gcr_certificate_renderer_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GcrCertificateRendererPrivate));

	gobject_class->dispose = gcr_certificate_renderer_dispose;
	gobject_class->finalize = gcr_certificate_renderer_finalize;
	gobject_class->set_property = gcr_certificate_renderer_set_property;
	gobject_class->get_property = gcr_certificate_renderer_get_property;

	/**
	 * GcrCertificateRenderer:certificate:
	 *
	 * The certificate to display. May be %NULL.
	 */
	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object ("certificate", "Certificate", "Certificate to display.",
	                                GCR_TYPE_CERTIFICATE, G_PARAM_READWRITE));

	/**
	 * GcrCertificateRenderer:attributes:
	 *
	 * The certificate attributes to display. One of the attributes must be
	 * a CKA_VALUE type attribute which contains a DER encoded certificate.
	 */
	g_object_class_install_property (gobject_class, PROP_ATTRIBUTES,
	           g_param_spec_boxed ("attributes", "Attributes", "Certificate pkcs11 attributes",
	                               GCK_TYPE_ATTRIBUTES, G_PARAM_READWRITE));

	/**
	 * GcrCertificateRenderer:label:
	 *
	 * The label to display.
	 */
	g_object_class_install_property (gobject_class, PROP_LABEL,
	           g_param_spec_string ("label", "Label", "Certificate Label",
	                                "", G_PARAM_READWRITE));

	_gcr_icons_register ();
	gcr_certificate_mixin_class_init (gobject_class);

	/* Register this as a renderer which can be loaded */
	registered = gck_attributes_new ();
	gck_attributes_add_ulong (registered, CKA_CLASS, CKO_CERTIFICATE);
	gcr_renderer_register (GCR_TYPE_CERTIFICATE_RENDERER, registered);
	gck_attributes_unref (registered);
}

static void
gcr_certificate_renderer_render (GcrRenderer *renderer, GcrViewer *viewer)
{
	GcrCertificateRenderer *self;
	gconstpointer data, value;
	gsize n_data, n_value, n_raw;
	GcrDisplayView *view;
	on_parsed_dn_args args;
	const gchar *text;
	GcrCertificate *cert;
	gpointer raw;
	gulong version;
	guint bits, index;
	gchar *display;
	GNode *asn;
	GQuark oid;
	GDate date;
	GIcon *icon;

	self = GCR_CERTIFICATE_RENDERER (renderer);

	if (GCR_IS_DISPLAY_VIEW (viewer)) {
		view = GCR_DISPLAY_VIEW (viewer);

	} else {
		g_warning ("GcrCertificateRenderer only works with internal specific "
		           "GcrViewer returned by gcr_viewer_new().");
		return;
	}

	_gcr_display_view_clear (view, renderer);
	cert = GCR_CERTIFICATE (self);

	data = gcr_certificate_get_der_data (cert, &n_data);
	if (!data)
		return;

	icon = gcr_certificate_get_icon (cert);
	_gcr_display_view_set_icon (view, GCR_RENDERER (self), icon);
	g_object_unref (icon);

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", data, n_data);
	g_return_if_fail (asn);

	display = calculate_label (self);
	_gcr_display_view_append_title (view, renderer, display);
	g_free (display);

	display = egg_dn_read_part (egg_asn1x_node (asn, "tbsCertificate", "subject", "rdnSequence", NULL), "CN");
	_gcr_display_view_append_content (view, renderer, _("Identity"), display);
	g_free (display);

	display = egg_dn_read_part (egg_asn1x_node (asn, "tbsCertificate", "issuer", "rdnSequence", NULL), "CN");
	_gcr_display_view_append_content (view, renderer, _("Verified by"), display);
	g_free (display);

	if (egg_asn1x_get_time_as_date (egg_asn1x_node (asn, "tbsCertificate", "validity", "notAfter", NULL), &date)) {
		display = g_malloc0 (128);
		if (!g_date_strftime (display, 128, "%x", &date))
			g_return_if_reached ();
		_gcr_display_view_append_content (view, renderer, _("Expires"), display);
		g_free (display);
	}

	_gcr_display_view_start_details (view, renderer);

	args.renderer = self;
	args.view = view;

	/* The subject */
	_gcr_display_view_append_heading (view, renderer, _("Subject Name"));
	egg_dn_parse (egg_asn1x_node (asn, "tbsCertificate", "subject", "rdnSequence", NULL), on_parsed_dn_part, &args);

	/* The Issuer */
	_gcr_display_view_append_heading (view, renderer, _("Issuer Name"));
	egg_dn_parse (egg_asn1x_node (asn, "tbsCertificate", "issuer", "rdnSequence", NULL), on_parsed_dn_part, &args);

	/* The Issued Parameters */
	_gcr_display_view_append_heading (view, renderer, _("Issued Certificate"));

	if (!egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "tbsCertificate", "version", NULL), &version))
		g_return_if_reached ();
	display = g_strdup_printf ("%lu", version + 1);
	_gcr_display_view_append_value (view, renderer, _("Version"), display, FALSE);
	g_free (display);

	raw = egg_asn1x_get_integer_as_raw (egg_asn1x_node (asn, "tbsCertificate", "serialNumber", NULL), NULL, &n_raw);
	g_return_if_fail (raw);
	_gcr_display_view_append_hex (view, renderer, _("Serial Number"), raw, n_raw);
	g_free (raw);

	display = g_malloc0 (128);
	if (egg_asn1x_get_time_as_date (egg_asn1x_node (asn, "tbsCertificate", "validity", "notBefore", NULL), &date)) {
		if (!g_date_strftime (display, 128, "%Y-%m-%d", &date))
			g_return_if_reached ();
		_gcr_display_view_append_value (view, renderer, _("Not Valid Before"), display, FALSE);
	}
	if (egg_asn1x_get_time_as_date (egg_asn1x_node (asn, "tbsCertificate", "validity", "notAfter", NULL), &date)) {
		if (!g_date_strftime (display, 128, "%Y-%m-%d", &date))
			g_return_if_reached ();
		_gcr_display_view_append_value (view, renderer, _("Not Valid After"), display, FALSE);
	}
	g_free (display);

	/* Fingerprints */
	_gcr_display_view_append_heading (view, renderer, _("Certificate Fingerprints"));

	_gcr_display_view_append_fingerprint (view, renderer, data, n_data, "SHA1", G_CHECKSUM_SHA1);
	_gcr_display_view_append_fingerprint (view, renderer, data, n_data, "MD5", G_CHECKSUM_MD5);

	/* Signature */
	_gcr_display_view_append_heading (view, renderer, _("Signature"));

	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "signatureAlgorithm", "algorithm", NULL));
	text = egg_oid_get_description (oid);
	_gcr_display_view_append_value (view, renderer, _("Signature Algorithm"), text, FALSE);

	value = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "signatureAlgorithm", "parameters", NULL), &n_value);
	if (value && n_value)
		_gcr_display_view_append_hex (view, renderer, _("Signature Parameters"), value, n_value);

	raw = egg_asn1x_get_bits_as_raw (egg_asn1x_node (asn, "signature", NULL), NULL, &bits);
	g_return_if_fail (raw);
	_gcr_display_view_append_hex (view, renderer, _("Signature"), raw, bits / 8);
	g_free (raw);

	/* Public Key Info */
	_gcr_display_view_append_heading (view, renderer, _("Public Key Info"));

	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "tbsCertificate", "subjectPublicKeyInfo",
	                                                  "algorithm", "algorithm", NULL));
	text = egg_oid_get_description (oid);
	_gcr_display_view_append_value (view, renderer, _("Key Algorithm"), text, FALSE);

	value = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "tbsCertificate", "subjectPublicKeyInfo",
	                                                   "algorithm", "parameters", NULL), &n_value);
	if (value && n_value)
		_gcr_display_view_append_hex (view, renderer, _("Key Parameters"), value, n_value);

	bits = gcr_certificate_get_key_size (cert);
	if (bits > 0) {
		display = g_strdup_printf ("%u", bits);
		_gcr_display_view_append_value (view, renderer, _("Key Size"), display, FALSE);
		g_free (display);
	}

	value = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "tbsCertificate",
	                                                   "subjectPublicKeyInfo", NULL), &n_value);
	raw = _gcr_fingerprint_from_subject_public_key_info (value, n_value, G_CHECKSUM_SHA1, &n_raw);
	_gcr_display_view_append_hex (view, renderer, _("Key SHA1 Fingerprint"), raw, n_raw);
	g_free (raw);

	raw = egg_asn1x_get_bits_as_raw (egg_asn1x_node (asn, "tbsCertificate", "subjectPublicKeyInfo",
	                                                 "subjectPublicKey", NULL), NULL, &bits);
	g_return_if_fail (raw);
	_gcr_display_view_append_hex (view, renderer, _("Public Key"), raw, bits / 8);
	g_free (raw);

	/* Extensions */
	for (index = 1; TRUE; ++index) {
		if (!append_extension (self, view, asn, data, n_data, index))
			break;
	}

	egg_asn1x_destroy (asn);
}

static void
gcr_certificate_renderer_populate_popup (GcrRenderer *self, GcrViewer *viewer,
                                         GtkMenu *menu)
{
	GtkWidget *item;

	item = gtk_separator_menu_item_new ();
	gtk_widget_show (item);
	gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), item);

	item = gtk_menu_item_new_with_label ("Export Certificate...");
	gtk_widget_show (item);
	g_signal_connect_data (item, "activate", G_CALLBACK (on_certificate_export),
	                       g_object_ref (self), (GClosureNotify)g_object_unref, 0);
	gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), item);
}

static void
gcr_renderer_iface_init (GcrRendererIface *iface)
{
	iface->populate_popup = gcr_certificate_renderer_populate_popup;
	iface->render_view = gcr_certificate_renderer_render;
}

static gconstpointer
gcr_certificate_renderer_get_der_data (GcrCertificate *cert, gsize *n_data)
{
	GcrCertificateRenderer *self = GCR_CERTIFICATE_RENDERER (cert);
	GckAttribute *attr;

	g_assert (n_data);

	if (self->pv->opt_cert)
		return gcr_certificate_get_der_data (self->pv->opt_cert, n_data);

	if (self->pv->opt_attrs) {
		attr = gck_attributes_find (self->pv->opt_attrs, CKA_VALUE);
		g_return_val_if_fail (attr, NULL);
		*n_data = attr->length;
		return attr->value;
	}

	return NULL;
}

static void
gcr_renderer_certificate_iface_init (GcrCertificateIface *iface)
{
	iface->get_der_data = gcr_certificate_renderer_get_der_data;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_certificate_renderer_new:
 * @certificate: The certificate to display
 *
 * Create a new certificate renderer to display the certificate.
 *
 * Returns: A newly allocated #GcrCertificateRenderer, which should be released
 *     with g_object_unref().
 */
GcrCertificateRenderer*
gcr_certificate_renderer_new (GcrCertificate *certificate)
{
	return g_object_new (GCR_TYPE_CERTIFICATE_RENDERER, "certificate", certificate, NULL);
}

/**
 * gcr_certificate_renderer_new_for_attributes:
 * @label: The label to display
 * @attrs: The attributes to display
 *
 * Create a new certificate renderer to display the label and attributes. One
 * of the attributes should be a CKA_VALUE type attribute containing a DER
 * encoded certificate.
 *
 * Returns: A newly allocated #GcrCertificateRenderer, which should be released
 *     with g_object_unref().
 */
GcrCertificateRenderer*
gcr_certificate_renderer_new_for_attributes (const gchar *label, struct _GckAttributes *attrs)
{
	return g_object_new (GCR_TYPE_CERTIFICATE_RENDERER, "label", label, "attributes", attrs, NULL);
}

/**
 * gcr_certificate_renderer_get_certificate:
 * @self: The renderer
 *
 * Get the certificate displayed in the renderer. If no certificate was
 * explicitly set, then the renderer will return itself since it acts as
 * a valid certificate.
 *
 * Returns: The certificate, owned by the renderer.
 */
GcrCertificate*
gcr_certificate_renderer_get_certificate (GcrCertificateRenderer *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_RENDERER (self), NULL);
	if (self->pv->opt_cert)
		return self->pv->opt_cert;
	return GCR_CERTIFICATE (self);
}

/**
 * gcr_certificate_renderer_set_certificate:
 * @self: The renderer
 * @certificate: The certificate to display
 *
 * Set a certificate to display in the renderer.
 */
void
gcr_certificate_renderer_set_certificate (GcrCertificateRenderer *self, GcrCertificate *certificate)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_RENDERER (self));

	if (self->pv->opt_cert)
		g_object_unref (self->pv->opt_cert);
	self->pv->opt_cert = certificate;
	if (self->pv->opt_cert)
		g_object_ref (self->pv->opt_cert);

	if (self->pv->opt_attrs) {
		gck_attributes_unref (self->pv->opt_attrs);
		self->pv->opt_attrs = NULL;
	}

	gcr_renderer_emit_data_changed (GCR_RENDERER (self));
	g_object_notify (G_OBJECT (self), "certificate");
}

/**
 * gcr_certificate_renderer_get_attributes:
 * @self: The renderer
 *
 * Get the PKCS\#11 attributes, if any, set for this renderer to display.
 *
 * Returns: The attributes, owned by the renderer.
 */
GckAttributes*
gcr_certificate_renderer_get_attributes (GcrCertificateRenderer *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_RENDERER (self), NULL);
	return self->pv->opt_attrs;
}

/**
 * gcr_certificate_renderer_set_attributes:
 * @self: The renderer
 * @attrs: Attributes to set
 *
 * Set the PKCS\#11 attributes for this renderer to display. One of the attributes
 * should be a CKA_VALUE type attribute containing a DER encoded certificate.
 */
void
gcr_certificate_renderer_set_attributes (GcrCertificateRenderer *self, GckAttributes *attrs)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_RENDERER (self));

	gck_attributes_unref (self->pv->opt_attrs);
	self->pv->opt_attrs = attrs;

	if (self->pv->opt_attrs)
		gck_attributes_ref (self->pv->opt_attrs);

	if (self->pv->opt_cert) {
		g_object_unref (self->pv->opt_cert);
		g_object_notify (G_OBJECT (self), "certificate");
		self->pv->opt_cert = NULL;
	}

	gcr_renderer_emit_data_changed (GCR_RENDERER (self));
	g_object_notify (G_OBJECT (self), "attributes");

}
