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

#include "gcr-certificate.h"
#include "gcr-certificate-exporter.h"

#include "egg/egg-openssl.h"

#include <glib/gi18n-lib.h>

enum {
	PROP_0,
	PROP_CERTIFICATE,
	PROP_LABEL,
	PROP_TRANSIENT_FOR
};

struct _GcrCertificateExporterPrivate {

	/* Setup stuff */
	GcrCertificate *certificate;
	gchar *label;
	GtkWindow *transient_for;

	/* Used during operation */
	GtkFileChooser *chooser_dialog;
	GFile *output_file;
	GByteArray *buffer;
	guint buffer_at;

	/* Async stuff */
	GAsyncReadyCallback callback;
	gpointer user_data;
	GCancellable *cancellable;
	GError *error;
	gboolean completed;
};

static const gchar *BAD_FILENAME_CHARS = "/\\<>|?*";

/* Forward declarations */
static void _gcr_certificate_exporter_async_result_init (GAsyncResultIface *iface);
static void write_to_outputstream (GcrCertificateExporter *self, GOutputStream *os);

G_DEFINE_TYPE_WITH_CODE (GcrCertificateExporter, _gcr_certificate_exporter, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (G_TYPE_ASYNC_RESULT, _gcr_certificate_exporter_async_result_init));

typedef void (*PrepareDataFunc) (GcrCertificateExporter *self);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
prepare_data_for_der (GcrCertificateExporter *self)
{
	gconstpointer data;
	gsize n_data;

	data = gcr_certificate_get_der_data (self->pv->certificate, &n_data);
	g_return_if_fail (data);

	self->pv->buffer = g_byte_array_new ();
	g_byte_array_append (self->pv->buffer, data, n_data);
}

static void
prepare_data_for_pem (GcrCertificateExporter *self)
{
	gconstpointer data;
	gpointer encoded;
	gsize n_data, n_encoded;

	data = gcr_certificate_get_der_data (self->pv->certificate, &n_data);
	g_return_if_fail (data);

	self->pv->buffer = g_byte_array_new ();

	encoded = egg_openssl_pem_write (data, n_data,
	                                 g_quark_from_static_string ("CERTIFICATE"),
	                                 NULL, &n_encoded);

	g_byte_array_append (self->pv->buffer, encoded, n_encoded);
	g_free (encoded);
}

static void
complete_async_result (GcrCertificateExporter *self)
{
	g_assert (self->pv->callback);
	g_assert (!self->pv->completed);

	if (self->pv->chooser_dialog)
		gtk_widget_hide (GTK_WIDGET (self->pv->chooser_dialog));

	self->pv->completed = TRUE;
	(self->pv->callback) (G_OBJECT (self), G_ASYNC_RESULT (self),
	                      self->pv->user_data);
}

static void
on_outputstream_write_ready (GObject *source, GAsyncResult *res, gpointer user_data)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (user_data);
	GOutputStream *os = G_OUTPUT_STREAM (source);
	gssize written;

	written = g_output_stream_write_finish (os, res, &self->pv->error);

	if (self->pv->error) {
		complete_async_result (self);
		return;
	}

	g_return_if_fail (written >= 0);
	g_return_if_fail (written <= self->pv->buffer->len - self->pv->buffer_at);
	self->pv->buffer_at += written;

	/* Write next bit, or finished */
	write_to_outputstream (self, os);
}

static void
on_outputstream_closed (GObject *source, GAsyncResult *res, gpointer user_data)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (user_data);
	g_output_stream_close_finish (G_OUTPUT_STREAM (source), res, &self->pv->error);
	complete_async_result (self);
}

static void
write_to_outputstream (GcrCertificateExporter *self, GOutputStream *os)
{
	gtk_widget_hide (GTK_WIDGET (self->pv->chooser_dialog));
	g_assert (GTK_IS_WIDGET (self->pv->chooser_dialog));

	/* Are we all done? */
	g_assert (self->pv->buffer_at <= self->pv->buffer->len);
	if (self->pv->buffer_at == self->pv->buffer->len) {
		g_output_stream_close_async (os, G_PRIORITY_DEFAULT,
		                             self->pv->cancellable,
		                             on_outputstream_closed, self);
		return;
	}

	g_output_stream_write_async (os, self->pv->buffer->data + self->pv->buffer_at,
	                             self->pv->buffer->len - self->pv->buffer_at,
	                             G_PRIORITY_DEFAULT, self->pv->cancellable,
	                             on_outputstream_write_ready, self);
}

static void
on_replace_file_ready (GObject *source, GAsyncResult *res, gpointer user_data)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (user_data);
	GFile *file = G_FILE (source);
	GFileOutputStream *os;

	os = g_file_replace_finish (file, res, &self->pv->error);

	if (self->pv->error) {
		complete_async_result (self);
		return;
	}

	write_to_outputstream (self, G_OUTPUT_STREAM (os));
}

static void
on_replace_dialog_response (GtkDialog *dialog, gint response_id, gpointer user_data)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (user_data);

	if (response_id == GTK_RESPONSE_ACCEPT) {
		g_file_replace_async (self->pv->output_file, NULL, FALSE, G_FILE_CREATE_NONE,
		                      G_PRIORITY_DEFAULT, self->pv->cancellable,
		                      on_replace_file_ready, self);
	}

	gtk_widget_destroy (GTK_WIDGET (dialog));
}

static void
on_cancel_replace_dialog (GCancellable *cancellable, gpointer user_data)
{
	gtk_widget_destroy (user_data);
}

static void
on_create_file_ready (GObject *source, GAsyncResult *res, gpointer user_data)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (user_data);
	GFileOutputStream *os;
	GtkWidget *dialog;

	os = g_file_create_finish (self->pv->output_file, res, &self->pv->error);

	/* Try again this time replacing the file */
	if (g_error_matches (self->pv->error, G_IO_ERROR, G_IO_ERROR_EXISTS)) {
		g_clear_error (&self->pv->error);

		dialog = gtk_message_dialog_new_with_markup (GTK_WINDOW (self->pv->chooser_dialog),
		     GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_QUESTION,
		     GTK_BUTTONS_NONE, _("<b>A file already exists with this name.</b>\n\nDo you want to replace it with a new file?"));
		gtk_dialog_add_buttons (GTK_DIALOG (dialog),
		                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		                        _("_Replace"), GTK_RESPONSE_ACCEPT, NULL);

		g_signal_connect (dialog, "response",
		                  G_CALLBACK (on_replace_dialog_response), self);
		if (self->pv->cancellable)
			g_cancellable_connect (self->pv->cancellable,
			                       G_CALLBACK (on_cancel_replace_dialog),
			                       g_object_ref (dialog), g_object_unref);
		gtk_widget_show (dialog);

		return;
	}

	if (self->pv->error) {
		complete_async_result (self);
		return;
	}

	write_to_outputstream (self, G_OUTPUT_STREAM (os));
}

static void
on_chooser_dialog_response (GtkDialog *dialog, gint response_id, gpointer user_data)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (user_data);
	GtkFileFilter *filter;
	PrepareDataFunc prepare_data;

	if (response_id != GTK_RESPONSE_ACCEPT) {
		g_set_error (&self->pv->error, G_IO_ERROR, G_IO_ERROR_CANCELLED,
		             _("The operation was cancelled."));
		complete_async_result (self);
		return;
	}

	if (self->pv->output_file)
		g_object_unref (self->pv->output_file);
	self->pv->output_file = gtk_file_chooser_get_file (self->pv->chooser_dialog);
	g_return_if_fail (self->pv->output_file);

	filter = gtk_file_chooser_get_filter (self->pv->chooser_dialog);
	prepare_data = g_object_get_data (G_OBJECT (filter), "prepare-data-func");
	g_assert (prepare_data);

	if (self->pv->buffer)
		g_byte_array_free (self->pv->buffer, TRUE);
	self->pv->buffer = NULL;
	self->pv->buffer_at = 0;

	/* Prepare the for writing out */
	(prepare_data) (self);

	/* Try to open the file */
	g_file_create_async (self->pv->output_file, G_FILE_CREATE_NONE, G_PRIORITY_DEFAULT,
	                     self->pv->cancellable, on_create_file_ready,
	                     self);
}

static void
on_cancel_chooser_dialog (GCancellable *cancellable, gpointer user_data)
{
	GtkDialog *dialog = GTK_DIALOG (user_data);
	gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
}

static void
exporter_display_chooser (GcrCertificateExporter *self)
{
	GtkFileFilter* filter;
	GtkWidget *dialog;
	gchar *filename;

	g_assert (!self->pv->chooser_dialog);

	dialog = gtk_file_chooser_dialog_new (_("Export certificate"),
	                     NULL, GTK_FILE_CHOOSER_ACTION_SAVE,
	                     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
	                     GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
	                     NULL);

	self->pv->chooser_dialog = g_object_ref_sink(dialog);
	gtk_dialog_set_default_response (GTK_DIALOG (dialog),
	                                 GTK_RESPONSE_ACCEPT);
	gtk_file_chooser_set_local_only (self->pv->chooser_dialog, FALSE);

	filter = gtk_file_filter_new ();
	gtk_file_filter_set_name (filter, _("Certificate files"));
	gtk_file_filter_add_mime_type (filter, "application/x-x509-ca-cert");
	gtk_file_filter_add_mime_type (filter, "application/x-x509-user-cert");
	gtk_file_filter_add_mime_type (filter, "application/pkix-cert");
	gtk_file_filter_add_pattern (filter, "*.cer");
	gtk_file_filter_add_pattern (filter, "*.crt");
	g_object_set_data (G_OBJECT (filter), "prepare-data-func", prepare_data_for_der);
	gtk_file_chooser_add_filter (self->pv->chooser_dialog, filter);
	gtk_file_chooser_set_filter (self->pv->chooser_dialog, filter);

	filter = gtk_file_filter_new ();
	gtk_file_filter_set_name (filter, _("PEM files"));
	gtk_file_filter_add_mime_type (filter, "text/plain");
	gtk_file_filter_add_pattern (filter, "*.pem");
	g_object_set_data (G_OBJECT (filter), "prepare-data-func", prepare_data_for_pem);
	gtk_file_chooser_add_filter (self->pv->chooser_dialog, filter);

	filename = g_strconcat (self->pv->label, ".crt", NULL);
	g_strdelimit (filename, BAD_FILENAME_CHARS, '_');
	gtk_file_chooser_set_current_name (self->pv->chooser_dialog, filename);
	g_free (filename);

	g_signal_connect (self->pv->chooser_dialog, "response",
	                  G_CALLBACK (on_chooser_dialog_response), self);
	if (self->pv->cancellable)
		g_cancellable_connect (self->pv->cancellable,
		                       G_CALLBACK (on_cancel_chooser_dialog), self, NULL);

	gtk_widget_show (GTK_WIDGET (self->pv->chooser_dialog));
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
_gcr_certificate_exporter_init (GcrCertificateExporter *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_CERTIFICATE_EXPORTER,
	                                         GcrCertificateExporterPrivate));
}

static void
_gcr_certificate_exporter_dispose (GObject *obj)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (obj);

	if (self->pv->certificate)
		g_object_unref (self->pv->certificate);
	self->pv->certificate = NULL;

	if (self->pv->cancellable)
		g_object_unref (self->pv->cancellable);
	self->pv->cancellable = NULL;

	G_OBJECT_CLASS (_gcr_certificate_exporter_parent_class)->dispose (obj);
}

static void
_gcr_certificate_exporter_finalize (GObject *obj)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (obj);

	g_free (self->pv->label);

	/*
	 * Should have been freed in _export_finish, which holds a ref to self
	 * so this should never be reached without being finished.
	 */
	g_assert (!self->pv->chooser_dialog);
	g_assert (!self->pv->output_file);
	g_assert (!self->pv->buffer);

	g_clear_error (&self->pv->error);

	G_OBJECT_CLASS (_gcr_certificate_exporter_parent_class)->finalize (obj);
}

static void
_gcr_certificate_exporter_set_property (GObject *obj, guint prop_id, const GValue *value,
                                        GParamSpec *pspec)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (obj);
	GcrCertificate *cert;

	switch (prop_id) {
	case PROP_CERTIFICATE:
		cert = g_value_dup_object (value);
		if (self->pv->certificate)
			g_object_unref (self->pv->certificate);
		self->pv->certificate = cert;
		g_object_notify (G_OBJECT (self), "certificate");
		break;
	case PROP_LABEL:
		g_free (self->pv->label);
		self->pv->label = g_value_dup_string (value);
		g_object_notify (obj, "label");
		break;
	case PROP_TRANSIENT_FOR:
		self->pv->transient_for = g_value_get_object (value);
		g_object_notify (obj, "transient-for");
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_certificate_exporter_get_property (GObject *obj, guint prop_id, GValue *value,
                                        GParamSpec *pspec)
{
	GcrCertificateExporter *self = GCR_CERTIFICATE_EXPORTER (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_value_set_object (value, self->pv->certificate);
		break;
	case PROP_LABEL:
		g_value_take_string (value, self->pv->label);
		break;
	case PROP_TRANSIENT_FOR:
		g_value_set_object (value, self->pv->transient_for);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_certificate_exporter_class_init (GcrCertificateExporterClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	_gcr_certificate_exporter_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GcrCertificateExporterPrivate));

	gobject_class->dispose = _gcr_certificate_exporter_dispose;
	gobject_class->finalize = _gcr_certificate_exporter_finalize;
	gobject_class->set_property = _gcr_certificate_exporter_set_property;
	gobject_class->get_property = _gcr_certificate_exporter_get_property;

	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object ("certificate", "Certificate", "Certificate to display.",
	                               GCR_TYPE_CERTIFICATE, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_LABEL,
	           g_param_spec_string ("label", "Label", "Label of certificate.",
	                                _("Certificate"), G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_TRANSIENT_FOR,
	           g_param_spec_object ("transient-for", "Transient For", "Transient for this Window",
	                                GTK_TYPE_WINDOW, G_PARAM_READWRITE));
}

static GObject*
_gcr_certificate_exporter_get_source_object (GAsyncResult *result)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_EXPORTER (result), NULL);
	return G_OBJECT (result);
}

static gpointer
_gcr_certificate_exporter_get_user_data (GAsyncResult *result)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_EXPORTER (result), NULL);
	return GCR_CERTIFICATE_EXPORTER (result)->pv->user_data;
}

static void
_gcr_certificate_exporter_async_result_init (GAsyncResultIface *iface)
{
	iface->get_source_object = _gcr_certificate_exporter_get_source_object;
	iface->get_user_data = _gcr_certificate_exporter_get_user_data;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GcrCertificateExporter*
_gcr_certificate_exporter_new (GcrCertificate *certificate, const gchar *label,
                               GtkWindow *transient_for)
{
	return g_object_new (GCR_TYPE_CERTIFICATE_EXPORTER,
	                     "certificate", certificate,
	                     "label", label,
	                     "transient-for", transient_for,
	                     NULL);
}

void
_gcr_certificate_exporter_export_async (GcrCertificateExporter *self,
                                        GCancellable *cancellable,
                                        GAsyncReadyCallback callback,
                                        gpointer user_data)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_EXPORTER (self));
	g_return_if_fail (callback);

	/* Must not have already started */
	g_return_if_fail (!self->pv->callback);
	g_return_if_fail (!self->pv->cancellable);

	self->pv->callback = callback;
	self->pv->user_data = user_data;
	if (cancellable)
		self->pv->cancellable = g_object_ref (cancellable);

	exporter_display_chooser (self);

	/* Matching in export_finish */
	g_object_ref (self);
}

gboolean
_gcr_certificate_exporter_export_finish (GcrCertificateExporter *self,
                                         GAsyncResult *result,
                                         GError **error)
{
	gboolean ret = TRUE;

	g_return_val_if_fail (G_ASYNC_RESULT (self) == result, FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);
	g_return_val_if_fail (self->pv->completed, FALSE);

	/* Cleanup all the operation stuff */
	self->pv->callback = NULL;

	if (self->pv->chooser_dialog)
		g_object_unref (self->pv->chooser_dialog);
	self->pv->chooser_dialog = NULL;

	if (self->pv->output_file)
		g_object_unref (self->pv->output_file);
	self->pv->output_file = NULL;

	if (self->pv->buffer)
		g_byte_array_free (self->pv->buffer, TRUE);
	self->pv->buffer = NULL;
	self->pv->buffer_at = 0;

	self->pv->completed = FALSE;

	if (self->pv->error) {
		g_propagate_error (error, self->pv->error);
		ret = FALSE;
	}

	/* Matches in export_async */
	g_object_unref (self);
	return ret;
}
