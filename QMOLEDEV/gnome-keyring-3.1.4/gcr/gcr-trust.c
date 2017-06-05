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

#include "gcr.h"
#include "gcr-types.h"
#include "gcr-internal.h"
#include "gcr-library.h"
#include "gcr-trust.h"

#include <gck/gck.h>

#include "pkcs11/pkcs11n.h"
#include "pkcs11/pkcs11i.h"
#include "pkcs11/pkcs11x.h"

#include <glib/gi18n-lib.h>

/**
 * SECTION:gcr-trust
 * @title: Trust Storage and Lookups
 * @short_description: Store and lookup bits of information used for
 * verifying certificates.
 *
 * These functions provide access to stored information about which
 * certificates the system and user trusts as certificate authority trust
 * anchors, or overrides to the normal verification of certificates.
 *
 * Trust anchors are used to verify the certificate authority in a certificate
 * chain. Trust anchors are always valid for a given purpose. The most common
 * purpose is the #GCR_PURPOSE_SERVER_AUTH and is used for a client application
 * to verify that the certificate at the server side of a TLS connection is
 * authorized to act as such. To check if a certificate is a trust anchor use
 * gcr_trust_is_certificate_anchored().
 *
 * Pinned certificates are used when a user overrides the default trust
 * decision for a given certificate. They're often used with self-signed
 * certificates. Pinned certificates are always only valid for a single peer
 * such as the remote host with which TLS is being performed. To lookup
 * pinned certificates use gcr_trust_is_certificate_pinned().
 *
 * After the user has requested to override the trust decision
 * about a given certificate then a pinned certificates can be added by using
 * the gcr_trust_add_pinned_certificate() function.
 *
 * These functions do not constitute a viable method for verifying certificates
 * used in TLS or other locations. Instead they support such verification
 * by providing some of the needed data for a trust decision.
 *
 * The storage is provided by pluggable PKCS\#11 modules.
 */

/**
 * GCR_PURPOSE_SERVER_AUTH:
 *
 * The purpose used to verify the server certificate in a TLS connection. This
 * is the most common purpose in use.
 */

/**
 * GCR_PURPOSE_CLIENT_AUTH:
 *
 * The purpose used to verify the client certificate in a TLS connection.
 */

/**
 * GCR_PURPOSE_CODE_SIGNING:
 *
 * The purpose used to verify certificate used for the signature on signed code.
 */

/**
 * GCR_PURPOSE_EMAIL:
 *
 * The purpose used to verify certificates that are used in email communication
 * such as S/MIME.
 */

/* ----------------------------------------------------------------------------------
 * HELPERS
 */

typedef struct _GcrTrustOperation {
	GckEnumerator *en;
	GckAttributes *attrs;
	gboolean found;
} GcrTrustOperation;

static void
trust_operation_free (gpointer data)
{
	GcrTrustOperation *op = data;
	g_assert (data);

	/* No reference held */
	g_assert (GCK_IS_ENUMERATOR (op->en));
	op->en = NULL;

	g_assert (op->attrs);
	gck_attributes_unref (op->attrs);
	op->attrs = NULL;

	g_slice_free (GcrTrustOperation, op);
}

static void
trust_operation_init (GckEnumerator *en, GckAttributes *attrs)
{
	GcrTrustOperation *op;

	g_assert (GCK_IS_ENUMERATOR (en));
	g_assert (!g_object_get_data (G_OBJECT (en), "trust-operation"));
	g_assert (attrs);

	op = g_slice_new0 (GcrTrustOperation);
	op->attrs = gck_attributes_ref (attrs);

	/* No reference held, GckEnumerator owns */
	op->en = en;
	g_object_set_data_full (G_OBJECT (en), "trust-operation", op, trust_operation_free);
}

static GcrTrustOperation*
trust_operation_get (GckEnumerator *en)
{
	GcrTrustOperation *op = g_object_get_data (G_OBJECT (en), "trust-operation");
	g_assert (op);
	g_assert (op->en == en);
	return op;
}

static GckAttributes*
prepare_trust_attrs (GcrCertificate *certificate, CK_X_ASSERTION_TYPE type)
{
	GckAttributes *attrs;
	gconstpointer data;
	gsize n_data;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_X_TRUST_ASSERTION);
	gck_attributes_add_ulong (attrs, CKA_X_ASSERTION_TYPE, type);

	data = gcr_certificate_get_der_data (certificate, &n_data);
	g_return_val_if_fail (data, NULL);
	gck_attributes_add_data (attrs, CKA_X_CERTIFICATE_VALUE, data, n_data);

	return attrs;
}

/* ----------------------------------------------------------------------------------
 * GET PINNED CERTIFICATE
 */

static GckEnumerator*
prepare_is_certificate_pinned (GcrCertificate *certificate, const gchar *purpose, const gchar *peer)
{
	GckAttributes *attrs;
	GckEnumerator *en;
	GList *slots;

	attrs = prepare_trust_attrs (certificate, CKT_X_PINNED_CERTIFICATE);
	g_return_val_if_fail (attrs, NULL);

	gck_attributes_add_string (attrs, CKA_X_PURPOSE, purpose);
	gck_attributes_add_string (attrs, CKA_X_PEER, peer);

	slots = gcr_pkcs11_get_trust_lookup_slots ();
	en = gck_slots_enumerate_objects (slots, attrs, 0);
	trust_operation_init (en, attrs);
	gck_attributes_unref (attrs);
	gck_list_unref_free (slots);

	return en;
}

static gboolean
perform_is_certificate_pinned (GckEnumerator *en, GCancellable *cancellable, GError **error)
{
	GcrTrustOperation *op;
	GckObject *object;

	op = trust_operation_get (en);

	g_assert (op != NULL);
	g_assert (op->found == FALSE);

	object = gck_enumerator_next (en, cancellable, error);
	op->found = (object != NULL);

	if (object)
		g_object_unref (object);

	return op->found;
}

/**
 * gcr_trust_is_certificate_pinned:
 * @certificate: a #GcrCertificate to check
 * @purpose: the purpose string
 * @peer: the peer for this pinned
 * @cancellable: a #GCancellable
 * @error: a #GError, or NULL
 *
 * Check if @certificate is pinned for @purpose to communicate with @peer.
 * A pinned certificate overrides all other certificate verification.
 *
 * This call may block, see gcr_trust_is_certificate_pinned_async() for the
 * non-blocking version.
 *
 * In the case of an error, %FALSE is also returned. Check @error to detect
 * if an error occurred.
 *
 * Returns: %TRUE if the certificate is pinned for the host and purpose
 */
gboolean
gcr_trust_is_certificate_pinned (GcrCertificate *certificate, const gchar *purpose,
                                 const gchar *peer, GCancellable *cancellable, GError **error)
{
	GckEnumerator *en;
	gboolean ret;

	g_return_val_if_fail (GCR_IS_CERTIFICATE (certificate), FALSE);
	g_return_val_if_fail (purpose, FALSE);
	g_return_val_if_fail (peer, FALSE);

	_gcr_initialize ();

	en = prepare_is_certificate_pinned (certificate, purpose, peer);
	g_return_val_if_fail (en, FALSE);

	ret = perform_is_certificate_pinned (en, cancellable, error);

	g_object_unref (en);

	return ret;
}

static void
thread_is_certificate_pinned (GSimpleAsyncResult *result, GObject *object, GCancellable *cancel)
{
	GError *error = NULL;

	perform_is_certificate_pinned (GCK_ENUMERATOR (object), cancel, &error);

	if (error != NULL) {
		g_simple_async_result_set_from_error (result, error);
		g_clear_error (&error);
	}
}

/**
 * gcr_trust_is_certificate_pinned_async:
 * @certificate: a #GcrCertificate to check
 * @purpose: the purpose string
 * @peer: the peer for this pinned
 * @cancellable: a #GCancellable
 * @callback: a #GAsyncReadyCallback to call when the operation completes
 * @user_data: the data to pass to callback function
 *
 * Check if @certificate is pinned for @purpose to communicate with @peer. A
 * pinned certificate overrides all other certificate verification.
 *
 * When the operation is finished, callback will be called. You can then call
 * gcr_trust_is_certificate_pinned_finish() to get the result of the
 * operation.
 */
void
gcr_trust_is_certificate_pinned_async (GcrCertificate *certificate, const gchar *purpose,
                                       const gchar *peer, GCancellable *cancellable,
                                       GAsyncReadyCallback callback, gpointer user_data)
{
	GSimpleAsyncResult *async;
	GckEnumerator *en;

	g_return_if_fail (GCR_CERTIFICATE (certificate));
	g_return_if_fail (purpose);
	g_return_if_fail (peer);

	_gcr_initialize ();

	en = prepare_is_certificate_pinned (certificate, purpose, peer);
	g_return_if_fail (en);

	async = g_simple_async_result_new (G_OBJECT (en), callback, user_data,
	                                   gcr_trust_is_certificate_pinned_async);

	g_simple_async_result_run_in_thread (async, thread_is_certificate_pinned,
	                                     G_PRIORITY_DEFAULT, cancellable);

	g_object_unref (async);
	g_object_unref (en);
}

/**
 * gcr_trust_is_certificate_pinned_finish:
 * @result: the #GAsyncResult passed to the callback
 * @error: a #GError, or NULL
 *
 * Finishes an asynchronous operation started by
 * gcr_trust_is_certificate_pinned_async().
 *
 * In the case of an error, %FALSE is also returned. Check @error to detect
 * if an error occurred.
 *
 * Returns: %TRUE if the certificate is pinned.
 */
gboolean
gcr_trust_is_certificate_pinned_finish (GAsyncResult *result, GError **error)
{
	GcrTrustOperation *op;
	GObject *object;
	gboolean found;

	g_return_val_if_fail (G_IS_SIMPLE_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	_gcr_initialize ();

	object = g_async_result_get_source_object (result);
	g_return_val_if_fail (g_simple_async_result_is_valid (result, object,
	                      gcr_trust_is_certificate_pinned_async), FALSE);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result), error))
		return FALSE;

	op = trust_operation_get (GCK_ENUMERATOR (object));
	found = op->found;
	g_object_unref (object);
	return found;
}

/* ----------------------------------------------------------------------------------
 * ADD PINNED CERTIFICATE
 */

static GckEnumerator*
prepare_add_pinned_certificate (GcrCertificate *certificate, const gchar *purpose, const gchar *peer)
{
	GckAttributes *attrs;
	GckEnumerator *en;
	GList *slots;

	attrs = prepare_trust_attrs (certificate, CKT_X_PINNED_CERTIFICATE);
	g_return_val_if_fail (attrs, NULL);

	gck_attributes_add_string (attrs, CKA_X_PURPOSE, purpose);
	gck_attributes_add_string (attrs, CKA_X_PEER, peer);
	gck_attributes_add_boolean (attrs, CKA_TOKEN, TRUE);

	slots = gcr_pkcs11_get_trust_lookup_slots ();
	en = gck_slots_enumerate_objects (slots, attrs, CKF_RW_SESSION);
	trust_operation_init (en, attrs);
	gck_attributes_unref (attrs);
	gck_list_unref_free (slots);

	return en;
}

static gboolean
perform_add_pinned_certificate (GckEnumerator *en, GCancellable *cancellable, GError **error)
{
	GcrTrustOperation *op;
	GckAttributes *attrs;
	gboolean ret = FALSE;
	GError *lerr = NULL;
	GckObject *object;
	GckSession *session;
	GckSlot *slot;

	op = trust_operation_get (en);
	g_assert (op != NULL);

	/* We need an error below */
	if (error && !*error)
		*error = lerr;

	object = gck_enumerator_next (en, cancellable, error);
	if (*error)
		return FALSE;

	/* It already exists */
	if (object) {
		g_object_unref (object);
		return TRUE;
	}

	attrs = gck_attributes_new ();
	gck_attributes_add_all (attrs, op->attrs);

	/* TODO: Add relevant label */

	/* Find an appropriate token */
	slot = gcr_pkcs11_get_trust_store_slot ();
	if (slot == NULL) {
		g_set_error (error, GCK_ERROR, CKR_FUNCTION_FAILED,
		             /* Translators: A pinned certificate is an exception which
		                trusts a given certificate explicitly for a purpose and
		                communication with a certain peer. */
		             _("Couldn't find a place to store the pinned certificate"));
		ret = FALSE;
	} else {
		session = gck_slot_open_session (slot, CKF_RW_SESSION, NULL, error);
		if (session != NULL) {
			object = gck_session_create_object (session, attrs, cancellable, error);
			if (object != NULL) {
				g_object_unref (object);
				ret = TRUE;
			}

			g_object_unref (session);
		}

		g_object_unref (slot);
	}

	gck_attributes_unref (attrs);

	/* Our own local error pointer */
	g_clear_error (&lerr);

	return ret;
}

/**
 * gcr_trust_add_pinned_certificate:
 * @certificate: a #GcrCertificate
 * @purpose: the purpose string
 * @peer: the peer for this pinned certificate
 * @cancellable: a #GCancellable
 * @error: a #GError, or NULL
 *
 * Add a pinned @certificate for connections to @peer for @purpose. A pinned
 * certificate overrides all other certificate verification and should be
 * used with care.
 *
 * If the same pinned certificate already exists, then this operation
 * does not add another, and succeeds without error.
 *
 * This call may block, see gcr_trust_add_pinned_certificate_async() for the
 * non-blocking version.
 *
 * Returns: %TRUE if the pinned certificate is recorded successfully
 */
gboolean
gcr_trust_add_pinned_certificate (GcrCertificate *certificate, const gchar *purpose, const gchar *peer,
                                  GCancellable *cancellable, GError **error)
{
	GckEnumerator *en;
	gboolean ret;

	g_return_val_if_fail (GCR_IS_CERTIFICATE (certificate), FALSE);
	g_return_val_if_fail (purpose, FALSE);
	g_return_val_if_fail (peer, FALSE);

	_gcr_initialize ();

	en = prepare_add_pinned_certificate (certificate, purpose, peer);
	g_return_val_if_fail (en, FALSE);

	ret = perform_add_pinned_certificate (en, cancellable, error);

	g_object_unref (en);

	return ret;
}

static void
thread_add_pinned_certificate (GSimpleAsyncResult *result, GObject *object, GCancellable *cancel)
{
	GError *error = NULL;

	perform_add_pinned_certificate (GCK_ENUMERATOR (object), cancel, &error);

	if (error != NULL) {
		g_simple_async_result_set_from_error (result, error);
		g_clear_error (&error);
	}
}

/**
 * gcr_trust_add_pinned_certificate_async:
 * @certificate: a #GcrCertificate
 * @purpose: the purpose string
 * @peer: the peer for this pinned certificate
 * @cancellable: a #GCancellable
 * @callback: a #GAsyncReadyCallback to call when the operation completes
 * @user_data: the data to pass to callback function
 *
 * Add a pinned certificate for communication with @peer for @purpose. A pinned
 * certificate overrides all other certificate verification and should be used
 * with care.
 *
 * If the same pinned certificate already exists, then this operation
 * does not add another, and succeeds without error.
 *
 * When the operation is finished, callback will be called. You can then call
 * gcr_trust_add_pinned_certificate_finish() to get the result of the
 * operation.
 */
void
gcr_trust_add_pinned_certificate_async (GcrCertificate *certificate, const gchar *purpose,
                                        const gchar *peer, GCancellable *cancellable,
                                        GAsyncReadyCallback callback, gpointer user_data)
{
	GSimpleAsyncResult *async;
	GckEnumerator *en;

	g_return_if_fail (GCR_IS_CERTIFICATE (certificate));
	g_return_if_fail (purpose);
	g_return_if_fail (peer);

	_gcr_initialize ();

	en = prepare_add_pinned_certificate (certificate, purpose, peer);
	g_return_if_fail (en);

	async = g_simple_async_result_new (G_OBJECT (en), callback, user_data,
	                                   gcr_trust_add_pinned_certificate_async);

	g_simple_async_result_run_in_thread (async, thread_add_pinned_certificate,
	                                     G_PRIORITY_DEFAULT, cancellable);

	g_object_unref (async);
	g_object_unref (en);
}

/**
 * gcr_trust_add_pinned_certificate_finish:
 * @result: the #GAsyncResult passed to the callback
 * @error: a #GError, or NULL
 *
 * Finishes an asynchronous operation started by
 * gcr_trust_add_pinned_certificate_async().
 *
 * Returns: %TRUE if the pinned certificate is recorded successfully
 */
gboolean
gcr_trust_add_pinned_certificate_finish (GAsyncResult *result, GError **error)
{
	GObject *object;

	g_return_val_if_fail (G_IS_SIMPLE_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	_gcr_initialize ();

	object = g_async_result_get_source_object (result);
	g_return_val_if_fail (g_simple_async_result_is_valid (result, object,
	                      gcr_trust_add_pinned_certificate_async), FALSE);
	g_object_unref (object);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result), error))
		return FALSE;

	return TRUE;
}

/* -----------------------------------------------------------------------
 * REMOVE PINNED CERTIFICATE
 */

static GckEnumerator*
prepare_remove_pinned_certificate (GcrCertificate *certificate, const gchar *purpose,
                                   const gchar *peer)
{
	GckAttributes *attrs;
	GckEnumerator *en;
	GList *slots;

	attrs = prepare_trust_attrs (certificate, CKT_X_PINNED_CERTIFICATE);
	g_return_val_if_fail (attrs, NULL);

	gck_attributes_add_string (attrs, CKA_X_PURPOSE, purpose);
	gck_attributes_add_string (attrs, CKA_X_PEER, peer);

	slots = gcr_pkcs11_get_trust_lookup_slots ();
	en = gck_slots_enumerate_objects (slots, attrs, CKF_RW_SESSION);
	trust_operation_init (en, attrs);
	gck_attributes_unref (attrs);
	gck_list_unref_free (slots);

	return en;
}

static gboolean
perform_remove_pinned_certificate (GckEnumerator *en, GCancellable *cancellable, GError **error)
{
	GcrTrustOperation *op;
	GList *objects, *l;
	GError *lerr = NULL;

	op = trust_operation_get (en);
	g_assert (op != NULL);

	/* We need an error below */
	if (error && !*error)
		*error = lerr;

	objects = gck_enumerator_next_n (en, -1, cancellable, error);
	if (*error)
		return FALSE;

	for (l = objects; l; l = g_list_next (l)) {
		if (!gck_object_destroy (l->data, cancellable, error)) {

			/* In case there's a race condition */
			if (g_error_matches (*error, GCK_ERROR, CKR_OBJECT_HANDLE_INVALID)) {
				g_clear_error (error);
				continue;
			}

			gck_list_unref_free (objects);
			return FALSE;
		}
	}

	gck_list_unref_free (objects);
	return TRUE;
}

/**
 * gcr_trust_remove_pinned_certificate:
 * @certificate: a #GcrCertificate
 * @purpose: the purpose string
 * @peer: the peer for this pinned certificate
 * @cancellable: a #GCancellable
 * @error: a #GError, or NULL
 *
 * Remove a pinned certificate for communication with @peer for @purpose.
 *
 * If the same pinned certificate does not exist, or was already removed,
 * then this operation succeeds without error.
 *
 * This call may block, see gcr_trust_remove_pinned_certificate_async() for the
 * non-blocking version.
 *
 * Returns: %TRUE if the pinned certificate no longer exists
 */
gboolean
gcr_trust_remove_pinned_certificate (GcrCertificate *certificate, const gchar *purpose, const gchar *peer,
                                     GCancellable *cancellable, GError **error)
{
	GckEnumerator *en;
	gboolean ret;

	g_return_val_if_fail (GCR_IS_CERTIFICATE (certificate), FALSE);
	g_return_val_if_fail (purpose, FALSE);
	g_return_val_if_fail (peer, FALSE);

	_gcr_initialize ();

	en = prepare_remove_pinned_certificate (certificate, purpose, peer);
	g_return_val_if_fail (en, FALSE);

	ret = perform_remove_pinned_certificate (en, cancellable, error);

	g_object_unref (en);

	return ret;
}

static void
thread_remove_pinned_certificate (GSimpleAsyncResult *result, GObject *object, GCancellable *cancel)
{
	GError *error = NULL;

	perform_remove_pinned_certificate (GCK_ENUMERATOR (object), cancel, &error);

	if (error != NULL) {
		g_simple_async_result_set_from_error (result, error);
		g_clear_error (&error);
	}
}

/**
 * gcr_trust_remove_pinned_certificate_async:
 * @certificate: a #GcrCertificate
 * @purpose: the purpose string
 * @peer: the peer for this pinned certificate
 * @cancellable: a #GCancellable
 * @callback: a #GAsyncReadyCallback to call when the operation completes
 * @user_data: the data to pass to callback function
 *
 * Remove a pinned certificate for communication with @peer for @purpose.
 *
 * If the same pinned certificate does not exist, or was already removed,
 * then this operation succeeds without error.
 *
 * When the operation is finished, callback will be called. You can then call
 * gcr_trust_remove_pinned_certificate_finish() to get the result of the
 * operation.
 */
void
gcr_trust_remove_pinned_certificate_async (GcrCertificate *certificate, const gchar *purpose,
                                           const gchar *peer, GCancellable *cancellable,
                                           GAsyncReadyCallback callback, gpointer user_data)
{
	GSimpleAsyncResult *async;
	GckEnumerator *en;

	g_return_if_fail (GCR_IS_CERTIFICATE (certificate));
	g_return_if_fail (purpose);
	g_return_if_fail (peer);

	_gcr_initialize ();

	en = prepare_remove_pinned_certificate (certificate, purpose, peer);
	g_return_if_fail (en);

	async = g_simple_async_result_new (G_OBJECT (en), callback, user_data,
	                                   gcr_trust_remove_pinned_certificate_async);

	g_simple_async_result_run_in_thread (async, thread_remove_pinned_certificate,
	                                     G_PRIORITY_DEFAULT, cancellable);

	g_object_unref (async);
	g_object_unref (en);
}

/**
 * gcr_trust_remove_pinned_certificate_finish:
 * @result: the #GAsyncResult passed to the callback
 * @error: a #GError, or NULL
 *
 * Finishes an asynchronous operation started by
 * gcr_trust_remove_pinned_certificate_async().
 *
 * Returns: %TRUE if the pinned certificate no longer exists
 */
gboolean
gcr_trust_remove_pinned_certificate_finish (GAsyncResult *result, GError **error)
{
	GObject *object;

	g_return_val_if_fail (G_IS_SIMPLE_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	_gcr_initialize ();

	object = g_async_result_get_source_object (result);
	g_return_val_if_fail (g_simple_async_result_is_valid (result, object,
	                      gcr_trust_remove_pinned_certificate_async), FALSE);
	g_object_unref (object);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result), error))
		return FALSE;

	return TRUE;
}

/* ----------------------------------------------------------------------------------
 * CERTIFICATE ROOT
 */

static GckEnumerator*
prepare_is_certificate_anchored (GcrCertificate *certificate, const gchar *purpose)
{
	GckAttributes *attrs;
	GckEnumerator *en;
	GList *slots;

	attrs = prepare_trust_attrs (certificate, CKT_X_ANCHORED_CERTIFICATE);
	g_return_val_if_fail (attrs, NULL);

	gck_attributes_add_string (attrs, CKA_X_PURPOSE, purpose);

	slots = gcr_pkcs11_get_trust_lookup_slots ();
	en = gck_slots_enumerate_objects (slots, attrs, 0);
	trust_operation_init (en, attrs);
	gck_attributes_unref (attrs);
	gck_list_unref_free (slots);

	return en;
}

static gboolean
perform_is_certificate_anchored (GckEnumerator *en, GCancellable *cancellable, GError **error)
{
	GcrTrustOperation *op;
	GckObject *object;

	op = trust_operation_get (en);
	g_assert (op != NULL);

	object = gck_enumerator_next (en, cancellable, error);
	if (object != NULL) {
		op->found = TRUE;
		g_object_unref (object);
	} else {
		op->found = FALSE;
	}

	return op->found;
}

/**
 * gcr_trust_is_certificate_anchored:
 * @certificate: a #GcrCertificate to check
 * @purpose: the purpose string
 * @cancellable: a #GCancellable
 * @error: a #GError, or NULL
 *
 * Check if the @certificate is a trust anchor for the given @purpose. A trust
 * anchor is used to verify the signatures on other certificates when verifying
 * a certificate chain. Also known as a trusted certificate authority.
 *
 * This call may block, see gcr_trust_is_certificate_anchored_async() for the
 * non-blocking version.
 *
 * In the case of an error, %FALSE is also returned. Check @error to detect
 * if an error occurred.
 *
 * Returns: %TRUE if the certificate is a trust anchor
 */
gboolean
gcr_trust_is_certificate_anchored (GcrCertificate *certificate, const gchar *purpose,
                                   GCancellable *cancellable, GError **error)
{
	GckEnumerator *en;
	gboolean ret;

	g_return_val_if_fail (GCR_IS_CERTIFICATE (certificate), FALSE);
	g_return_val_if_fail (purpose, FALSE);

	_gcr_initialize ();

	en = prepare_is_certificate_anchored (certificate, purpose);
	g_return_val_if_fail (en, FALSE);

	ret = perform_is_certificate_anchored (en, cancellable, error);

	g_object_unref (en);

	return ret;
}

static void
thread_is_certificate_anchored (GSimpleAsyncResult *result, GObject *object, GCancellable *cancel)
{
	GError *error = NULL;

	perform_is_certificate_anchored (GCK_ENUMERATOR (object), cancel, &error);

	if (error != NULL) {
		g_simple_async_result_set_from_error (result, error);
		g_clear_error (&error);
	}
}

/**
 * gcr_trust_is_certificate_anchored_async:
 * @certificate: a #GcrCertificate to check
 * @purpose: the purpose string
 * @cancellable: a #GCancellable
 * @callback: a #GAsyncReadyCallback to call when the operation completes
 * @user_data: the data to pass to callback function
 *
 * Check if the @certificate is a trust anchor for the given @purpose. A trust
 * anchor is used to verify the signatures on other certificates when verifying
 * a certificate chain. Also known as a trusted certificate authority.
 *
 * When the operation is finished, callback will be called. You can then call
 * gcr_trust_is_certificate_anchored_finish() to get the result of the operation.
 */
void
gcr_trust_is_certificate_anchored_async (GcrCertificate *certificate, const gchar *purpose,
                                         GCancellable *cancellable, GAsyncReadyCallback callback,
                                         gpointer user_data)
{
	GSimpleAsyncResult *async;
	GckEnumerator *en;

	g_return_if_fail (GCR_IS_CERTIFICATE (certificate));
	g_return_if_fail (purpose);

	_gcr_initialize ();

	en = prepare_is_certificate_anchored (certificate, purpose);
	g_return_if_fail (en);

	async = g_simple_async_result_new (G_OBJECT (en), callback, user_data,
	                                   gcr_trust_is_certificate_anchored_async);

	g_simple_async_result_run_in_thread (async, thread_is_certificate_anchored,
	                                     G_PRIORITY_DEFAULT, cancellable);

	g_object_unref (async);
	g_object_unref (en);
}

/**
 * gcr_trust_is_certificate_anchored_finish:
 * @result: the #GAsyncResult passed to the callback
 * @error: a #GError, or NULL
 *
 * Finishes an asynchronous operation started by
 * gcr_trust_is_certificate_anchored_async().
 *
 * In the case of an error, %FALSE is also returned. Check @error to detect
 * if an error occurred.
 *
 * Returns: %TRUE if the certificate is a trust anchor
 */
gboolean
gcr_trust_is_certificate_anchored_finish (GAsyncResult *result, GError **error)
{
	GcrTrustOperation *op;
	GObject *object;
	gboolean found;

	g_return_val_if_fail (G_IS_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	_gcr_initialize ();

	object = g_async_result_get_source_object (result);
	g_return_val_if_fail (g_simple_async_result_is_valid (result, object,
	                      gcr_trust_is_certificate_anchored_async), FALSE);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result), error))
		return FALSE;

	op = trust_operation_get (GCK_ENUMERATOR (object));
	found = op->found;
	g_object_unref (object);
	return found;
}
