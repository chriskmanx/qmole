/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Collabora Ltd
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

#include "gcr-pkcs11-certificate.h"

#include <gck/gck.h>
#include <string.h>

#include "gcr-certificate.h"
#include "gcr-internal.h"
#include "gcr-library.h"

#include "pkcs11/pkcs11.h"

/**
 * SECTION:gcr-pkcs11-certificate
 * @title: GcrPkcs11Certificate
 * @short_description: A certificate loaded from PKCS\#11 storage
 *
 * A #GcrPkcs11Certificate is a certificate loaded from a PKCS\#11 storage.
 * It is also a valid #GckObject and can be used as such.
 *
 * Use gcr_pkcs11_certificate_lookup_issuer() to lookup the issuer of a given
 * certificate in the PKCS\#11 store.
 *
 * Various common PKCS\#11 certificate attributes are automatically loaded and
 * are available via gcr_pkcs11_certificate_get_attributes().
 */

/**
 * GcrPkcs11Certificate:
 *
 * A certificate loaded from PKCS\#11 storage.
 */

/**
 * GcrPkcs11CertificateClass:
 *
 * The class for #GcrPkcs11Certificate.
 */

enum {
	PROP_0,
	PROP_ATTRIBUTES
};

struct _GcrPkcs11CertificatePrivate {
	GckAttributes *attrs;
};

static void gcr_certificate_iface (GcrCertificateIface *iface);
G_DEFINE_TYPE_WITH_CODE (GcrPkcs11Certificate, gcr_pkcs11_certificate, GCK_TYPE_OBJECT,
	GCR_CERTIFICATE_MIXIN_IMPLEMENT_COMPARABLE ();
	G_IMPLEMENT_INTERFACE (GCR_TYPE_CERTIFICATE, gcr_certificate_iface);
);

/* ----------------------------------------------------------------------------
 * INTERAL
 */

static GckEnumerator*
prepare_lookup_certificate_issuer (GcrCertificate *cert)
{
	GckAttributes *search;
	GckEnumerator *en;
	gpointer data;
	gsize n_data;
	GList *modules;

	search = gck_attributes_new ();
	gck_attributes_add_ulong (search, CKA_CLASS, CKO_CERTIFICATE);
	gck_attributes_add_ulong (search, CKA_CERTIFICATE_TYPE, CKC_X_509);

	data = gcr_certificate_get_issuer_raw (cert, &n_data);
	gck_attributes_add_data (search, CKA_SUBJECT, data, n_data);
	g_free (data);

	modules = gcr_pkcs11_get_modules ();
	en = gck_modules_enumerate_objects (modules, search, 0);
	gck_attributes_unref (search);
	gck_list_unref_free (modules);

	return en;
}

static GcrCertificate*
perform_lookup_certificate (GckEnumerator *en, GCancellable *cancel, GError **error)
{
	GcrCertificate *cert;
	GckObject *object;
	GckAttributes *attrs;
	GckModule *module;
	GckSession *session;

	object = gck_enumerator_next (en, cancel, error);

	if (object == NULL)
		return NULL;

	/*
	 * Only the CKA_VALUE, CKA_CLASS and CKA_CERTIFICATE_TYPE
	 * is strictly necessary here, but we get more attrs.
	 */
	attrs = gck_object_get (object, cancel, error,
	                        CKA_VALUE, CKA_LABEL,
	                        CKA_ID, CKA_CLASS,
	                        CKA_CERTIFICATE_TYPE,
	                        CKA_ISSUER,
	                        CKA_SERIAL_NUMBER,
	                        GCK_INVALID);

	if (attrs == NULL) {
		g_object_unref (object);
		return NULL;
	}

	module = gck_object_get_module (object);
	session = gck_object_get_session (object);

	cert = g_object_new (GCR_TYPE_PKCS11_CERTIFICATE,
	                     "module", module,
	                     "handle", gck_object_get_handle (object),
	                     "session", session,
	                     "attributes", attrs,
	                     NULL);

	g_object_unref (module);
	g_object_unref (session);
	g_object_unref (object);

	gck_attributes_unref (attrs);

	return cert;
}

static void
thread_lookup_certificate (GSimpleAsyncResult *res, GObject *object, GCancellable *cancel)
{
	GError *error = NULL;
	GcrCertificate *cert;

	cert = perform_lookup_certificate (GCK_ENUMERATOR (object), cancel, &error);

	if (error != NULL) {
		g_assert (!cert);
		g_simple_async_result_set_from_error (res, error);
		g_clear_error (&error);

	} else if (cert != NULL) {
		g_simple_async_result_set_op_res_gpointer (res, cert, g_object_unref);

	} else {
		g_simple_async_result_set_op_res_gpointer (res, NULL, NULL);
	}
}

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static GObject*
gcr_pkcs11_certificate_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	gpointer obj = G_OBJECT_CLASS (gcr_pkcs11_certificate_parent_class)->constructor (type, n_props, props);
	GckAttributes *attrs;
	GckAttribute *attr;
	gulong value;

	attrs = gcr_pkcs11_certificate_get_attributes (obj);
	g_return_val_if_fail (attrs, NULL);

	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &value) ||
	    value != CKO_CERTIFICATE) {
		g_warning ("attributes don't contain a certificate with: %s",
		           "CKA_CLASS == CKO_CERTIFICATE");
		return NULL;
	}

	if (!gck_attributes_find_ulong (attrs, CKA_CERTIFICATE_TYPE, &value) ||
	    value != CKC_X_509) {
		g_warning ("attributes don't contain a certificate with: %s",
		           "CKA_CERTIFICATE_TYPE == CKC_X_509");
		return NULL;
	}

	attr = gck_attributes_find (attrs, CKA_VALUE);
	if (!attr || !attr->value || attr->length == 0 || attr->length == G_MAXULONG) {
		g_warning ("attributes don't contain a valid: CKA_VALUE");
		return NULL;
	}

	return obj;
}

static void
gcr_pkcs11_certificate_init (GcrPkcs11Certificate *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_PKCS11_CERTIFICATE, GcrPkcs11CertificatePrivate);
}

static void
gcr_pkcs11_certificate_set_property (GObject *obj, guint prop_id, const GValue *value,
                                     GParamSpec *pspec)
{
	GcrPkcs11Certificate *self = GCR_PKCS11_CERTIFICATE (obj);

	switch (prop_id) {
	case PROP_ATTRIBUTES:
		g_return_if_fail (self->pv->attrs == NULL);
		self->pv->attrs = g_value_dup_boxed (value);
		g_return_if_fail (self->pv->attrs != NULL);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_pkcs11_certificate_get_property (GObject *obj, guint prop_id, GValue *value,
                                     GParamSpec *pspec)
{
	GcrPkcs11Certificate *self = GCR_PKCS11_CERTIFICATE (obj);

	switch (prop_id) {
	case PROP_ATTRIBUTES:
		g_value_set_boxed (value, gcr_pkcs11_certificate_get_attributes (self));
		break;
	default:
		gcr_certificate_mixin_get_property (obj, prop_id, value, pspec);
		break;
	}
}

static void
gcr_pkcs11_certificate_finalize (GObject *obj)
{
	GcrPkcs11Certificate *self = GCR_PKCS11_CERTIFICATE (obj);

	gck_attributes_unref (self->pv->attrs);

	G_OBJECT_CLASS (gcr_pkcs11_certificate_parent_class)->finalize (obj);
}

static void
gcr_pkcs11_certificate_class_init (GcrPkcs11CertificateClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructor = gcr_pkcs11_certificate_constructor;
	gobject_class->get_property = gcr_pkcs11_certificate_get_property;
	gobject_class->set_property = gcr_pkcs11_certificate_set_property;
	gobject_class->finalize = gcr_pkcs11_certificate_finalize;

	/**
	 * GcrPkcs11Certificate:attributes:
	 *
	 * Automatically loaded attributes for this certificate.
	 */
	g_object_class_install_property (gobject_class, PROP_ATTRIBUTES,
	         g_param_spec_boxed ("attributes", "Attributes", "The data displayed in the renderer",
	                             GCK_TYPE_ATTRIBUTES, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (gobject_class, sizeof (GcrPkcs11CertificatePrivate));

	gcr_certificate_mixin_class_init (gobject_class);
	_gcr_initialize ();
}

static gconstpointer
gcr_pkcs11_certificate_get_der_data (GcrCertificate *cert, gsize *n_data)
{
	GcrPkcs11Certificate *self = GCR_PKCS11_CERTIFICATE (cert);
	GckAttribute *attr;

	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (self->pv->attrs, NULL);

	attr = gck_attributes_find (self->pv->attrs, CKA_VALUE);
	g_return_val_if_fail (attr && attr->length != 0 && attr->length != G_MAXULONG, NULL);
	*n_data = attr->length;
	return attr->value;
}

static void
gcr_certificate_iface (GcrCertificateIface *iface)
{
	iface->get_der_data = gcr_pkcs11_certificate_get_der_data;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_pkcs11_certificate_get_attributes:
 * @self: A #GcrPkcs11Certificate
 *
 * Access the automatically loaded attributes for this certificate.
 *
 * Returns: the certificate attributes
 */
GckAttributes*
gcr_pkcs11_certificate_get_attributes (GcrPkcs11Certificate *self)
{
	g_return_val_if_fail (GCR_IS_PKCS11_CERTIFICATE (self), NULL);
	return self->pv->attrs;
}

/**
 * gcr_pkcs11_certificate_lookup_issuer:
 * @certificate: a #GcrCertificate
 * @cancellable: a #GCancellable
 * @error: a #GError, or NULL
 *
 * Lookup a the issuer of a @certificate in the PKCS\#11 storage. The
 * lookup is done using the issuer DN of the certificate. No certificate chain
 * verification is done. Use a crypto library to make trust decisions.
 *
 * This call may block, see gcr_pkcs11_certificate_lookup_issuer() for the
 * non-blocking version.
 *
 * Will return %NULL if no issuer certificate is found. Use @error to determine
 * if an error occurred.
 *
 * Returns: a new #GcrPkcs11Certificate, or %NULL
 */
GcrCertificate*
gcr_pkcs11_certificate_lookup_issuer (GcrCertificate *certificate, GCancellable *cancellable,
                                      GError **error)
{
	GckEnumerator *en;
	GcrCertificate *issuer;

	g_return_val_if_fail (GCR_IS_CERTIFICATE (certificate), NULL);

	en = prepare_lookup_certificate_issuer (certificate);
	g_return_val_if_fail (en, FALSE);

	issuer = perform_lookup_certificate (en, cancellable, error);
	g_object_unref (en);

	return issuer;
}

/**
 * gcr_pkcs11_certificate_lookup_issuer_async:
 * @certificate: a #GcrCertificate
 * @cancellable: a #GCancellable
 * @callback: a #GAsyncReadyCallback to call when the operation completes
 * @user_data: the data to pass to callback function
 *
 * Lookup a the issuer of a @certificate in the PKCS\#11 storage. The
 * lookup is done using the issuer DN of the certificate. No certificate chain
 * verification is done. Use a crypto library to make trust decisions.
 *
 * When the operation is finished, callback will be called. You can then call
 * gcr_pkcs11_certificate_lookup_issuer_finish() to get the result of the
 * operation.
 */
void
gcr_pkcs11_certificate_lookup_issuer_async (GcrCertificate *certificate, GCancellable *cancellable,
                                            GAsyncReadyCallback callback, gpointer user_data)
{
	GSimpleAsyncResult *async;
	GckEnumerator *en;

	g_return_if_fail (GCR_IS_CERTIFICATE (certificate));

	en = prepare_lookup_certificate_issuer (certificate);
	g_return_if_fail (en);

	async = g_simple_async_result_new (G_OBJECT (en), callback, user_data,
	                                   gcr_pkcs11_certificate_lookup_issuer_async);

	g_simple_async_result_run_in_thread (async, thread_lookup_certificate,
	                                     G_PRIORITY_DEFAULT, cancellable);

	g_object_unref (async);
	g_object_unref (en);
}

/**
 * gcr_pkcs11_certificate_lookup_issuer_finish:
 * @result: the #GAsyncResult passed to the callback
 * @error: a #GError, or NULL
 *
 * Finishes an asynchronous operation started by
 * gcr_pkcs11_certificate_lookup_issuer_async().
 *
 * Will return %NULL if no issuer certificate is found. Use @error to determine
 * if an error occurred.
 *
 * Returns: a new #GcrPkcs11Certificate, or %NULL
 */
GcrCertificate*
gcr_pkcs11_certificate_lookup_issuer_finish (GAsyncResult *result, GError **error)
{
	GcrCertificate *cert;
	GObject *source;

	g_return_val_if_fail (G_IS_ASYNC_RESULT (result), NULL);
	source = g_async_result_get_source_object (result);
	g_return_val_if_fail (g_simple_async_result_is_valid (result, source,
	                      gcr_pkcs11_certificate_lookup_issuer_async), NULL);
	g_object_unref (source);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result), error))
		return NULL;

	cert = g_simple_async_result_get_op_res_gpointer (G_SIMPLE_ASYNC_RESULT (result));
	if (cert != NULL)
		g_object_ref (cert);

	return cert;
}
