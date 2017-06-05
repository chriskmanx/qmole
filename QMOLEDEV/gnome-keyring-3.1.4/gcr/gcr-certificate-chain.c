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

#include "gcr-certificate-chain.h"

#include "gcr-certificate.h"
#define DEBUG_FLAG GCR_DEBUG_CERTIFICATE_CHAIN
#include "gcr-debug.h"
#include "gcr-pkcs11-certificate.h"
#include "gcr-simple-certificate.h"
#include "gcr-trust.h"

#include "egg/egg-error.h"

/**
 * SECTION:gcr-certificate-chain
 * @title: GcrCertificateChain
 * @short_description: A certificate chain
 *
 * #GcrCertificateChain represents a chain of certificates, normally used to
 * validate the trust in a certificate. An X.509 certificate chain has one
 * endpoint certificate (the one for which trust is being verified) and then
 * in turn the certificate that issued each previous certificate in the chain.
 *
 * This functionality is for building of certificate chains not for validating
 * them. Use your favorite crypto library to validate trust in a certificate
 * chain once its built.
 *
 * The order of certificates in the chain should be first the endpoint
 * certificates and then the signing certificates.
 *
 * Create a new certificate chain with gcr_certificate_chain_new() and then
 * add the certificates with gcr_certificate_chain_add().
 *
 * You can then use gcr_certificate_chain_build() to build the remainder of
 * the chain. This will lookup missing certificates in PKCS\#11 modules and
 * also check that each certificate in the chain is the signer of the previous
 * one. If a trust anchor, pinned certificate, or self-signed certificate is
 * found, then the chain is considered built. Any extra certificates are
 * removed from the chain.
 *
 * Once the certificate chain has been built, you can access its status
 * through gcr_certificate_chain_get_status(). The status signifies whether
 * the chain is anchored on a trust root, self-signed, incomplete etc. See
 * #GcrCertificateChainStatus for information on the various statuses.
 *
 * It's important to understand that the building of a certificate chain is
 * merely the first step towards verifying trust in a certificate.
 */


/**
 * GCR_TYPE_CERTIFICATE_CHAIN_STATUS:
 *
 * The flags #GType for #GcrCertificateChainFlags.
 */

/**
 * GcrCertificateChain:
 *
 * A chain of certificates.
 */

/**
 * GcrCertificateChainClass:
 * @parent_class: The parent class
 *
 * The class for #GcrCertificateChain.
 */

enum {
	PROP_0,
	PROP_STATUS,
	PROP_LENGTH,
};

struct _GcrCertificateChainPrivate {
	GPtrArray *certificates;
	GcrCertificateChainStatus status;

	/* Used in build operation */
	gchar *purpose;
	gchar *peer;
	guint flags;
};

static GQuark Q_ORIGINAL_CERT = 0;
static GQuark Q_OPERATION_DATA = 0;

G_DEFINE_TYPE (GcrCertificateChain, gcr_certificate_chain, G_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
free_chain_private (gpointer data)
{
	GcrCertificateChainPrivate *pv = data;
	g_ptr_array_unref (pv->certificates);
	g_free (pv->purpose);
	g_free (pv->peer);
	g_slice_free (GcrCertificateChainPrivate, pv);
}

static GcrCertificateChainPrivate*
new_chain_private (void)
{
	GcrCertificateChainPrivate *pv = g_slice_new0 (GcrCertificateChainPrivate);
	pv->certificates = g_ptr_array_new_with_free_func (g_object_unref);
	return pv;
}

static GcrCertificateChainPrivate*
prep_chain_private (GcrCertificateChainPrivate *orig, const gchar *purpose,
                    const gchar *peer, guint flags)
{
	GcrCertificateChainPrivate *pv;
	GcrCertificate *certificate;
	guint i;

	g_assert (orig);
	g_assert (purpose);

	pv = new_chain_private ();
	for (i = 0; i < orig->certificates->len; ++i) {
		certificate = g_ptr_array_index (orig->certificates, i);
		g_ptr_array_add (pv->certificates, g_object_ref (certificate));
	}

	pv->status = orig->status;
	pv->purpose = g_strdup (purpose);
	pv->peer = g_strdup (peer);
	pv->flags = flags;
	return pv;
}

static GcrCertificateChainPrivate*
prep_chain_private_thread_safe (GcrCertificateChainPrivate *orig, const gchar *purpose,
                                const gchar *peer, guint flags)
{
	GcrCertificateChainPrivate *pv;
	GcrCertificate *certificate, *safe;
	gconstpointer der;
	gsize n_der;
	guint i;

	g_assert (orig);

	pv = prep_chain_private (orig, purpose, peer, flags);

	for (i = 0; i < pv->certificates->len; ++i) {
		certificate = g_ptr_array_index (pv->certificates, i);

		/* We regard these types as thread safe */
		if (GCR_IS_SIMPLE_CERTIFICATE (certificate) ||
		    GCR_IS_PKCS11_CERTIFICATE (certificate)) {
			safe = g_object_ref (certificate);

		/* Otherwise copy the certificate data */
		} else {
			der = gcr_certificate_get_der_data (certificate, &n_der);
			g_return_val_if_fail (der, NULL);
			safe = gcr_simple_certificate_new (der, n_der);

			_gcr_debug ("copying certificate so it's thread safe");

			/* Always set the original certificate onto the safe one */
			g_object_set_qdata_full (G_OBJECT (safe), Q_ORIGINAL_CERT,
			                         g_object_ref (certificate), g_object_unref);
		}

		g_ptr_array_index (pv->certificates, i) = safe;
		g_object_unref (certificate);
	}

	return pv;
}

static GcrCertificateChainPrivate*
cleanup_chain_private (GcrCertificateChainPrivate *pv)
{
	GcrCertificate *certificate, *orig;
	guint i;

	for (i = 0; i < pv->certificates->len; ++i) {
		certificate = g_ptr_array_index (pv->certificates, i);

		/* If there's an original certificate set, then replace it back */
		orig = g_object_get_qdata (G_OBJECT (certificate), Q_ORIGINAL_CERT);
		if (orig != NULL) {
			g_ptr_array_index (pv->certificates, i) = g_object_ref (orig);
			g_object_unref (certificate);
		}
	}

	return pv;
}

static gboolean
perform_build_chain (GcrCertificateChainPrivate *pv, GCancellable *cancellable,
                     GError **rerror)
{
	GError *error = NULL;
	GcrCertificate *certificate;
	gboolean lookups;
	gboolean ret;
	guint length;
	gchar *subject;

	g_assert (pv);
	g_assert (pv->certificates);

	pv->status = GCR_CERTIFICATE_CHAIN_UNKNOWN;
	lookups = !((pv->flags & GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS) == GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS);

	/* This chain is built */
	if (!pv->certificates->len) {
		_gcr_debug ("empty certificate chain");
		return TRUE;
	}

	/* First check for pinned certificates */
	certificate = g_ptr_array_index (pv->certificates, 0);
	if (_gcr_debugging) {
		subject = gcr_certificate_get_subject_dn (certificate);
		_gcr_debug ("first certificate: %s", subject);
		g_free (subject);
	}

	if (lookups && pv->peer) {
		ret = gcr_trust_is_certificate_pinned (certificate, pv->purpose,
		                                       pv->peer, cancellable, &error);
		if (!ret && error) {
			_gcr_debug ("failed to lookup pinned certificate: %s",
			            egg_error_message (error));
			g_propagate_error (rerror, error);
			return FALSE;
		}

		/*
		 * This is a pinned certificate and the rest of the chain
		 * is irrelevant, so truncate chain and consider built.
		 */
		if (ret) {
			_gcr_debug ("found pinned certificate for peer '%s', truncating chain",
			            pv->peer);

			g_ptr_array_set_size (pv->certificates, 1);
			pv->status = GCR_CERTIFICATE_CHAIN_PINNED;
			return TRUE;
		}
	}

	length = 1;

	/* The first certificate is always unconditionally in the chain */
	while (pv->status == GCR_CERTIFICATE_CHAIN_UNKNOWN) {

		/* Stop the chain if previous was self-signed */
		if (gcr_certificate_is_issuer (certificate, certificate)) {
			_gcr_debug ("found self-signed certificate");
			pv->status = GCR_CERTIFICATE_CHAIN_SELFSIGNED;
			break;
		}

		/* Try the next certificate in the chain */
		if (length < pv->certificates->len) {
			certificate = g_ptr_array_index (pv->certificates, length);
			if (_gcr_debugging) {
				subject = gcr_certificate_get_subject_dn (certificate);
				_gcr_debug ("next certificate: %s", subject);
				g_free (subject);
			}

		/* No more in chain, try to lookup */
		} else if (lookups) {
			certificate = gcr_pkcs11_certificate_lookup_issuer (certificate,
			                                                    cancellable, &error);
			if (error != NULL) {
				_gcr_debug ("failed to lookup issuer: %s", error->message);
				g_propagate_error (rerror, error);
				return FALSE;

			} else if (certificate) {
				g_ptr_array_add (pv->certificates, certificate);
				if (_gcr_debugging) {
					subject = gcr_certificate_get_subject_dn (certificate);
					_gcr_debug ("found issuer certificate: %s", subject);
					g_free (subject);
				}

			} else {
				_gcr_debug ("no issuer found");
			}

		/* No more in chain, and can't lookup */
		} else {
			_gcr_debug ("no more certificates available, and no lookups");
			certificate = NULL;
		}

		/* Stop the chain if nothing found */
		if (certificate == NULL) {
			_gcr_debug ("chain is incomplete");
			pv->status = GCR_CERTIFICATE_CHAIN_INCOMPLETE;
			break;
		}

		++length;

		/* See if this certificate is an anchor */
		if (lookups) {
			ret = gcr_trust_is_certificate_anchored (certificate, pv->purpose,
			                                         cancellable, &error);

			if (!ret && error) {
				_gcr_debug ("failed to lookup anchored certificate: %s",
				            egg_error_message (error));
				g_propagate_error (rerror, error);
				return FALSE;

			/* Stop the chain at the first anchor */
			} else if (ret) {
				_gcr_debug ("found anchored certificate");
				pv->status = GCR_CERTIFICATE_CHAIN_ANCHORED;
				break;
			}
		}
	}

	/* TODO: Need to check each certificate in the chain for distrusted */

	/* Truncate to the appropriate length */
	g_assert (length <= pv->certificates->len);
	g_ptr_array_set_size (pv->certificates, length);
	return TRUE;
}

static void
thread_build_chain (GSimpleAsyncResult *result, GObject *object,
                    GCancellable *cancellable)
{
	GcrCertificateChainPrivate *pv;
	GError *error = NULL;

	pv = g_object_get_qdata (G_OBJECT (result), Q_OPERATION_DATA);
	g_assert (pv);

	_gcr_debug ("building asynchronously in another thread");

	if (!perform_build_chain (pv, cancellable, &error)) {
		g_simple_async_result_set_from_error (result, error);
		g_clear_error (&error);
	}
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gcr_certificate_chain_init (GcrCertificateChain *self)
{
	self->pv = new_chain_private ();
}

static void
gcr_certificate_chain_dispose (GObject *obj)
{
	GcrCertificateChain *self = GCR_CERTIFICATE_CHAIN (obj);

	g_ptr_array_set_size (self->pv->certificates, 0);
	self->pv->status = GCR_CERTIFICATE_CHAIN_UNKNOWN;

	G_OBJECT_CLASS (gcr_certificate_chain_parent_class)->dispose (obj);
}

static void
gcr_certificate_chain_finalize (GObject *obj)
{
	GcrCertificateChain *self = GCR_CERTIFICATE_CHAIN (obj);

	free_chain_private (self->pv);
	self->pv = NULL;

	G_OBJECT_CLASS (gcr_certificate_chain_parent_class)->finalize (obj);
}

static void
gcr_certificate_chain_get_property (GObject *obj, guint prop_id, GValue *value,
                                    GParamSpec *pspec)
{
	GcrCertificateChain *self = GCR_CERTIFICATE_CHAIN (obj);

	switch (prop_id) {
	case PROP_STATUS:
		g_value_set_enum (value, gcr_certificate_chain_get_status (self));
		break;
	case PROP_LENGTH:
		g_value_set_uint (value, gcr_certificate_chain_get_length (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_certificate_chain_class_init (GcrCertificateChainClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gcr_certificate_chain_parent_class = g_type_class_peek_parent (klass);

	gobject_class->dispose = gcr_certificate_chain_dispose;
	gobject_class->finalize = gcr_certificate_chain_finalize;
	gobject_class->get_property = gcr_certificate_chain_get_property;

	/**
	 * GcrCertificateChain:status:
	 *
	 * The certificate chain status. See #GcrCertificateChainStatus
	 */
	g_object_class_install_property (gobject_class, PROP_STATUS,
	           g_param_spec_enum ("status", "Status", "Status of certificate chain",
	                              GCR_TYPE_CERTIFICATE_CHAIN_STATUS,
	                              GCR_CERTIFICATE_CHAIN_UNKNOWN, G_PARAM_READABLE));

	/**
	 * GcrCertificateChain:length:
	 *
	 * The length of the certificate chain.
	 */
	g_object_class_install_property (gobject_class, PROP_LENGTH,
	           g_param_spec_uint ("length", "Length", "Length of certificate chain",
	                              0, G_MAXUINT, 0, G_PARAM_READABLE));

	Q_ORIGINAL_CERT = g_quark_from_static_string ("gcr-certificate-chain-original-cert");
	Q_OPERATION_DATA = g_quark_from_static_string ("gcr-certificate-chain-operation-data");
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * GcrCertificateChainStatus:
 * @GCR_CERTIFICATE_CHAIN_UNKNOWN: The certificate chain's status is unknown.
 * When a chain is not yet built it has this status. If a chain is modified after
 * being built, it has this status.
 * @GCR_CERTIFICATE_CHAIN_INCOMPLETE: A full chain could not be loaded. The
 * chain does not end with a self-signed certificate, a trusted anchor, or a
 * pinned certificate.
 * @GCR_CERTIFICATE_CHAIN_SELFSIGNED: The chain ends with a self-signed
 * certificate. No trust anchor was found.
 * @GCR_CERTIFICATE_CHAIN_DISTRUSTED: The certificate chain contains a revoked
 * or otherwise explicitly distrusted certificate. The entire chain should
 * be distrusted.
 * @GCR_CERTIFICATE_CHAIN_ANCHORED: The chain ends with an anchored
 * certificate. The anchored certificate is not necessarily self-signed.
 * @GCR_CERTIFICATE_CHAIN_PINNED: The chain represents a pinned certificate. A
 * pinned certificate is an exception which trusts a given certificate
 * explicitly for a purpose and communication with a certain peer.
 *
 * The status of a built certificate chain. Will be set to
 * %GCR_CERTIFICATE_CHAIN_UNKNOWN for certificate chains that have not been
 * built.
 */

/**
 * GCR_TYPE_CERTIFICATE_CHAIN_STATUS:
 *
 * The enum #GType for #GcrCertificateChainStatus.
 */

/**
 * GcrCertificateChainFlags:
 * @GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS: If this flag is specified then no
 * lookups for anchors or pinned certificates are done, and the resulting chain
 * will be neither anchored or pinned. Additionally no missing certificate
 * authorities are looked up in PKCS\#11.
 *
 * Flags to be used with the gcr_certificate_chain_build() operation.
 */

/**
 * GCR_TYPE_CERTIFICATE_CHAIN_FLAGS:
 *
 * The flags #GType for #GcrCertificateChainFlags.
 */

/**
 * gcr_certificate_chain_new:
 *
 * Create a new #GcrCertificateChain.
 *
 * Returns: A newly allocated #GcrCertificateChain
 */
GcrCertificateChain*
gcr_certificate_chain_new (void)
{
	return g_object_new (GCR_TYPE_CERTIFICATE_CHAIN, NULL);
}

/**
 * gcr_certificate_chain_add:
 * @self: the #GcrCertificateChain
 * @certificate: a #GcrCertificate to add to the chain
 *
 * Add @certificate to the chain. The order of certificates in the chain are
 * important. The first certificate should be the endpoint certificate, and
 * then come the signers (certificate authorities) each in turn. If a root
 * certificate authority is present, it should come last.
 *
 * Adding a certificate an already built chain (see
 * gcr_certificate_chain_build()) resets the type of the certificate chain
 * to %GCR_CERTIFICATE_CHAIN_UNKNOWN
 */
void
gcr_certificate_chain_add (GcrCertificateChain *self, GcrCertificate *certificate)
{
	g_return_if_fail (GCR_IS_CERTIFICATE_CHAIN (self));
	g_return_if_fail (GCR_IS_CERTIFICATE (certificate));
	g_ptr_array_add (self->pv->certificates, g_object_ref (certificate));
	self->pv->status = GCR_CERTIFICATE_CHAIN_UNKNOWN;
	g_object_notify (G_OBJECT (self), "status");
	g_object_notify (G_OBJECT (self), "length");
}

/**
 * gcr_certificate_chain_get_status:
 * @self: the #GcrCertificateChain
 *
 * Get the status of a certificate chain. If the certificate chain has not
 * been built, then the status will be %GCR_CERTIFICATE_CHAIN_UNKNOWN.
 *
 * A status of %GCR_CERTIFICATE_CHAIN_ANCHORED does not mean that the
 * certificate chain has been verified, but merely that an anchor has been
 * found.
 *
 * Returns: the status of the certificate chain.
 */
GcrCertificateChainStatus
gcr_certificate_chain_get_status (GcrCertificateChain *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_CHAIN (self), GCR_CERTIFICATE_CHAIN_UNKNOWN);
	return self->pv->status;
}

/**
 * gcr_certificate_chain_get_anchor:
 * @self: the #GcrCertificateChain
 *
 * If the certificate chain has been built and is of status
 * %GCR_CERTIFICATE_CHAIN_ANCHORED, then this will return the anchor
 * certificate that was found. This is not necessarily a root certificate
 * authority. If an intermediate certificate authority in the chain was
 * found to be anchored, then that certificate will be returned.
 *
 * If an anchor is returned it does not mean that the certificate chain has
 * been verified, but merely that an anchor has been found.
 *
 * Returns: the anchor certificate, or NULL if not anchored.
 */
GcrCertificate*
gcr_certificate_chain_get_anchor (GcrCertificateChain *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_CHAIN (self), NULL);
	if (self->pv->status != GCR_CERTIFICATE_CHAIN_ANCHORED)
		return NULL;
	g_assert (self->pv->certificates->len > 0);
	return GCR_CERTIFICATE (g_ptr_array_index (self->pv->certificates,
	                                           self->pv->certificates->len - 1));
}

/**
 * gcr_certificate_chain_get_endpoint:
 * @self: the #GcrCertificateChain
 *
 * Get the endpoint certificate in the chain. This is always the first
 * certificate in the chain. The endpoint certificate cannot be anchored.
 *
 * Returns: the endpoint certificate, or NULL if the chain is empty.
 */
GcrCertificate*
gcr_certificate_chain_get_endpoint (GcrCertificateChain *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_CHAIN (self), NULL);
	if (!self->pv->certificates->len)
		return NULL;
	return GCR_CERTIFICATE (g_ptr_array_index (self->pv->certificates, 0));
}

/**
 * gcr_certificate_chain_get_length:
 * @self: the #GcrCertificateChain
 *
 * Get the length of the certificate chain.
 *
 * Returns: the length of the certificate chain
 */
guint
gcr_certificate_chain_get_length (GcrCertificateChain *self)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_CHAIN (self), 0);
	return self->pv->certificates->len;
}

/**
 * gcr_certificate_chain_get_certificate:
 * @self: the #GcrCertificateChain
 * @index: index of the certificate to get
 *
 * Get a certificate in the chain. It is an error to call this function
 * with an invalid index.
 *
 * Returns: the certificate
 */
GcrCertificate*
gcr_certificate_chain_get_certificate (GcrCertificateChain *self, guint index)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE_CHAIN (self), NULL);
	g_return_val_if_fail (index < self->pv->certificates->len, NULL);
	return GCR_CERTIFICATE (g_ptr_array_index (self->pv->certificates, index));
}

/**
 * gcr_certificate_chain_build:
 * @self: the #GcrCertificateChain
 * @purpose: the purpose the certificate chain will be used for
 * @peer: the peer the certificate chain will be used with, or NULL
 * @flags: chain completion flags
 * @cancellable: a #GCancellable or %NULL
 * @error: a #GError or %NULL
 *
 * Complete a certificate chain. Once a certificate chain has been built
 * its status can be examined.
 *
 * This operation will lookup missing certificates in PKCS\#11
 * modules and also that each certificate in the chain is the signer of the
 * previous one. If a trust anchor, pinned certificate, or self-signed certificate
 * is found, then the chain is considered built. Any extra certificates are
 * removed from the chain.
 *
 * It's important to understand that building of a certificate chain does not
 * constitute verifying that chain. This is merely the first step towards
 * trust verification.
 *
 * The @purpose is a string like %GCR_PURPOSE_CLIENT_AUTH and is the purpose
 * for which the certificate chain will be used. Trust anchors are looked up
 * for this purpose. This argument is required.
 *
 * The @peer is usually the host name of the peer whith which this certificate
 * chain is being used. It is used to look up pinned certificates that have
 * been stored for this peer. If %NULL then no pinned certificates will
 * be considered.
 *
 * If the %GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS flag is specified then no
 * lookups for anchors or pinned certificates are done, and the resulting chain
 * will be neither anchored or pinned. Additionally no missing certificate
 * authorities are looked up in PKCS\#11
 *
 * This call will block, see gcr_certificate_chain_build_async() for the
 * asynchronous version.
 *
 * Returns: whether the operation completed successfully
 */
gboolean
gcr_certificate_chain_build (GcrCertificateChain *self, const gchar *purpose,
                             const gchar *peer, guint flags,
                             GCancellable *cancellable, GError **error)
{
	GcrCertificateChainPrivate *pv;
	gboolean ret;

	g_return_val_if_fail (GCR_IS_CERTIFICATE_CHAIN (self), FALSE);
	g_return_val_if_fail (purpose, FALSE);

	pv = prep_chain_private (self->pv, purpose, peer, flags);

	ret = perform_build_chain (pv, cancellable, error);

	if (ret) {
		free_chain_private (self->pv);
		self->pv = cleanup_chain_private (pv);
		g_object_notify (G_OBJECT (self), "status");
		g_object_notify (G_OBJECT (self), "length");
	} else {
		free_chain_private (pv);
	}

	return ret;
}

/**
 * gcr_certificate_chain_build_async:
 * @self: the #GcrCertificateChain
 * @purpose: the purpose the certificate chain will be used for
 * @peer: the peer the certificate chain will be used with, or NULL
 * @flags: chain completion flags
 * @cancellable: a #GCancellable or %NULL
 * @callback: this will be called when the operation completes.
 * @user_data: data to pass to the callback
 *
 * Complete a certificate chain. Once a certificate chain has been built
 * its status can be examined.
 *
 * This will lookup missing certificates in PKCS\#11
 * modules and also that each certificate in the chain is the signer of the
 * previous one. If a trust anchor, pinned certificate, or self-signed certificate
 * is found, then the chain is considered built. Any extra certificates are
 * removed from the chain.
 *
 * It's important to understand that building of a certificate chain does not
 * constitute verifying that chain. This is merely the first step towards
 * trust verification.
 *
 * The @purpose is a string like %GCR_PURPOSE_CLIENT_AUTH and is the purpose
 * for which the certificate chain will be used. Trust anchors are looked up
 * for this purpose. This argument is required.
 *
 * The @peer is usually the host name of the peer whith which this certificate
 * chain is being used. It is used to look up pinned certificates that have
 * been stored for this peer. If %NULL then no pinned certificates will
 * be considered.
 *
 * If the %GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS flag is specified then no
 * lookups for anchors or pinned certificates are done, and the resulting chain
 * will be neither anchored or pinned. Additionally no missing certificate
 * authorities are looked up in PKCS\#11
 *
 * When the operation is finished, @callback will be called. You can then call
 * gcr_certificate_chain_build_finish() to get the result of the operation.
 */
void
gcr_certificate_chain_build_async (GcrCertificateChain *self, const gchar *purpose,
                                   const gchar *peer, guint flags,
                                   GCancellable *cancellable, GAsyncReadyCallback callback,
                                   gpointer user_data)
{
	GcrCertificateChainPrivate *pv;
	GSimpleAsyncResult *result;

	g_return_if_fail (GCR_IS_CERTIFICATE_CHAIN (self));

	g_return_if_fail (GCR_IS_CERTIFICATE_CHAIN (self));
	g_return_if_fail (purpose);

	pv = prep_chain_private_thread_safe (self->pv, purpose, peer, flags);

	result = g_simple_async_result_new (G_OBJECT (self), callback, user_data,
	                                    gcr_certificate_chain_build_async);
	g_object_set_qdata_full (G_OBJECT (result), Q_OPERATION_DATA, pv, free_chain_private);

	g_simple_async_result_run_in_thread (result, thread_build_chain,
	                                     G_PRIORITY_DEFAULT, cancellable);
	g_object_unref (result);
}

/**
 * gcr_certificate_chain_build_finish:
 * @self: the #GcrCertificateChain
 * @result: the #GAsyncResult passed to the callback
 * @error: a #GError, or NULL
 *
 * Finishes an asynchronous operation started by
 * gcr_certificate_chain_build_async().
 *
 * Returns: whether the operation succeeded
 */
gboolean
gcr_certificate_chain_build_finish (GcrCertificateChain *self, GAsyncResult *result,
                                    GError **error)
{
	GcrCertificateChainPrivate *pv;

	g_return_val_if_fail (GCR_IS_CERTIFICATE_CHAIN (self), FALSE);
	g_return_val_if_fail (G_IS_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (g_simple_async_result_is_valid (result, G_OBJECT (self),
	                      gcr_certificate_chain_build_async), FALSE);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result), error))
		return FALSE;

	pv = g_object_steal_qdata (G_OBJECT (result), Q_OPERATION_DATA);
	g_return_val_if_fail (pv, FALSE);

	free_chain_private (self->pv);
	self->pv = cleanup_chain_private (pv);

	g_object_notify (G_OBJECT (self), "status");
	g_object_notify (G_OBJECT (self), "length");
	return TRUE;
}
