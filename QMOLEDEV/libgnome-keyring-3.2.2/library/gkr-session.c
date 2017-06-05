/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-session.c - negotiate session and encode secrets

   Copyright (C) 2009 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gkr-misc.h"
#include "gkr-session.h"
#include "gnome-keyring-private.h"

#include <gcrypt.h>

#include "egg/egg-dh.h"
#include "egg/egg-hkdf.h"
#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

struct _GkrSession {
	gint refs;
	gchar *path;
	gpointer key;
	gsize n_key;
};

G_LOCK_DEFINE_STATIC (session_globals);
static GkrSession *the_session;

static guchar*
pkcs7_pad_string_in_secure_memory (const gchar *string, gsize *n_padded)
{
	gsize length, n_pad;
	guchar *padded;

	length = strlen (string);

	if (!g_utf8_validate (string, length, NULL))
		return NULL;

	/* Pad the secret */
	*n_padded = ((length + 16) / 16) * 16;
	g_assert (length < *n_padded);
	g_assert (*n_padded > 0);
	n_pad = *n_padded - length;
	g_assert (n_pad > 0 && n_pad <= 16);
	padded = egg_secure_alloc (*n_padded);
	memcpy (padded, string, length);
	memset (padded + length, n_pad, n_pad);
	return padded;
}

static gchar*
pkcs7_unpad_string_in_place (guchar *padded, gsize n_padded)
{
	gsize n_pad, i;

	if (n_padded == 0)
		return NULL;

	n_pad = padded[n_padded - 1];

	/* Validate the padding */
	if (n_pad == 0 || n_pad > 16)
		return NULL;
	if (n_pad > n_padded)
		return NULL;
	for (i = n_padded - n_pad; i < n_padded; ++i) {
		if (padded[i] != n_pad)
			return NULL;
	}

	/* Null terminate */
	padded[n_padded - n_pad] = 0;
	if (!g_utf8_validate ((gchar*)padded, -1, NULL))
		return FALSE;

	return (gchar*)padded;
}

static GkrSession*
session_new (void)
{
	GkrSession *session = g_slice_new0 (GkrSession);
	session->refs = 1;
	return session;
}

GkrSession*
gkr_session_ref (GkrSession *session)
{
	g_assert (session);
	g_atomic_int_inc (&session->refs);
	return session;
}

void
gkr_session_unref (gpointer data)
{
	GkrSession *session = data;

	if (!session)
		return;

	if (!g_atomic_int_dec_and_test (&session->refs))
		return;

	g_free (session->path);
	egg_secure_free (session->key);
	g_slice_free (GkrSession, session);
}

const gchar*
gkr_session_get_path (GkrSession *session)
{
	g_assert (session);
	return session->path;
}

static gboolean
decode_open_session_plain (DBusMessage *message, const char **path)
{
	DBusMessageIter iter, variant;
	char *signature;
	gboolean equal;

	g_assert (message);
	g_assert (path);

	/* Parse the incomming message */
	if (!dbus_message_has_signature (message, "vo"))
		return FALSE;
	if (!dbus_message_iter_init (message, &iter))
		g_return_val_if_reached (FALSE);
	dbus_message_iter_recurse (&iter, &variant);

	signature = dbus_message_iter_get_signature (&variant);
	equal = g_str_equal (signature, "s");
	dbus_free (signature);
	if (!equal)
		return FALSE;

	if (!dbus_message_iter_next (&iter))
		g_return_val_if_reached (FALSE);
	dbus_message_iter_get_basic (&iter, path);

	return TRUE;
}

static void
on_open_session_plain (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	GkrSession *session;
	const char *path;

	if (gkr_operation_handle_errors (op, reply))
		return;

	/* Parse the result from OpenSession */
	if (!decode_open_session_plain (reply, &path)) {
		g_message ("received an invalid response to Service.OpenSession()");
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		return;
	}

	session = session_new ();
	session->path = g_strdup (path);
	session->key = NULL;
	session->n_key = 0;

	G_LOCK (session_globals);
	{
		if (the_session)
			gkr_session_unref (the_session);
		the_session = gkr_session_ref (session);
	}
	G_UNLOCK (session_globals);

	gkr_callback_invoke_op_session (gkr_operation_pop (op), session);
	gkr_session_unref (session);
}

static void
session_negotiate_plain (GkrOperation *op)
{
	DBusMessageIter iter, variant;
	const char *algorithm = "plain";
	const char *empty = "";
	DBusMessage *req;

	g_assert (op);

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, "OpenSession");
	dbus_message_iter_init_append (req, &iter);
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &algorithm);
	dbus_message_iter_open_container (&iter, DBUS_TYPE_VARIANT, "s", &variant);
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_STRING, &empty);
	dbus_message_iter_close_container (&iter, &variant);

	gkr_operation_push (op, on_open_session_plain, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static gboolean
decode_open_session_aes (DBusMessage *message, gcry_mpi_t *peer, const char **path)
{
	DBusMessageIter iter, variant, array;
	char *signature;
	gcry_error_t gcry;
	guchar *buffer;
	gboolean equal;
	int n_buffer;

	g_assert (message);
	g_assert (peer);
	g_assert (path);

	/* Parse the incomming message */
	if (!dbus_message_has_signature (message, "vo"))
		return FALSE;
	if (!dbus_message_iter_init (message, &iter))
		g_return_val_if_reached (FALSE);
	dbus_message_iter_recurse (&iter, &variant);

	signature = dbus_message_iter_get_signature (&variant);
	equal = g_str_equal (signature, "ay");
	dbus_free (signature);
	if (!equal)
		return FALSE;

	dbus_message_iter_recurse (&variant, &array);
	dbus_message_iter_get_fixed_array (&array, &buffer, &n_buffer);
	if (!dbus_message_iter_next (&iter))
		g_return_val_if_reached (FALSE);
	dbus_message_iter_get_basic (&iter, path);

	gcry = gcry_mpi_scan (peer, GCRYMPI_FMT_USG, buffer, n_buffer, NULL);
	return (gcry == 0);
}

static void
on_open_session_aes (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	gcry_mpi_t priv, prime, peer;
	GkrSession *session;
	const char *path;
	gpointer ikm;
	gsize n_ikm;

	g_assert (op);
	g_assert (user_data);

	/* If AES is not supported then try plain method */
	if (dbus_message_is_error (reply, DBUS_ERROR_NOT_SUPPORTED)) {
		session_negotiate_plain (op);
		return;
	}

	/* Handle any other errors */
	if (gkr_operation_handle_errors (op, reply))
		return;

	/* Parse the result from OpenSession */
	if (!decode_open_session_aes (reply, &peer, &path)) {
		g_message ("received an invalid response to Service.OpenSession()");
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		return;
	}

	if (!egg_dh_default_params ("ietf-ike-grp-modp-1024", &prime, NULL))
		g_return_if_reached ();

	/* Generate the actual secret */
	priv = user_data;
	ikm = egg_dh_gen_secret (peer, priv, prime, &n_ikm);

	gcry_mpi_release (peer);
	gcry_mpi_release (prime);

	if (ikm == NULL) {
		g_message ("couldn't negotiate a valid session key");
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		return;
	}

	session = session_new ();
	session->path = g_strdup (path);
	session->n_key = 16;

	/* Now digest this into our aes key */
	session->key = egg_secure_alloc (session->n_key);
	if (!egg_hkdf_perform ("sha256", ikm, n_ikm, NULL, 0, NULL, 0,
	                       session->key, session->n_key))
		g_return_if_reached ();
	egg_secure_free (ikm);

	G_LOCK (session_globals);
	{
		if (the_session)
			gkr_session_unref (the_session);
		the_session = gkr_session_ref (session);
	}
	G_UNLOCK (session_globals);

	gkr_callback_invoke_op_session (gkr_operation_pop (op), session);
	gkr_session_unref (session);
}

static void
session_negotiate_aes (GkrOperation *op)
{
	const char *algorithm = "dh-ietf1024-sha256-aes128-cbc-pkcs7";
	DBusMessageIter iter, variant, array;
	gcry_mpi_t prime, base, pub, priv;
	gboolean ret;
	guchar *buffer;
	gsize n_buffer;
	gcry_error_t gcry;
	DBusMessage *req;

	g_assert (op);

	egg_libgcrypt_initialize ();

	prime = base = pub = priv = NULL;
	ret = egg_dh_default_params ("ietf-ike-grp-modp-1024", &prime, &base) &&
	      egg_dh_gen_pair (prime, base, 0, &pub, &priv);

	gcry_mpi_release (prime);
	gcry_mpi_release (base);

	if (ret == TRUE) {
		req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
		                                    SERVICE_INTERFACE, "OpenSession");

		dbus_message_iter_init_append (req, &iter);
		dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &algorithm);
		dbus_message_iter_open_container (&iter, DBUS_TYPE_VARIANT, "ay", &variant);
		dbus_message_iter_open_container (&variant, DBUS_TYPE_ARRAY, "y", &array);

		gcry = gcry_mpi_aprint (GCRYMPI_FMT_USG, &buffer, &n_buffer, pub);
		g_return_if_fail (gcry == 0);
		dbus_message_iter_append_fixed_array (&array, DBUS_TYPE_BYTE, &buffer, n_buffer);
		gcry_free (buffer);

		dbus_message_iter_close_container (&variant, &array);
		dbus_message_iter_close_container (&iter, &variant);

		gkr_operation_push (op, on_open_session_aes, GKR_CALLBACK_OP_MSG,
		                    priv, (GDestroyNotify)gcry_mpi_release);
		priv = NULL;

		gkr_operation_request (op, req);
		dbus_message_unref (req);
	}

	gcry_mpi_release (pub);
	gcry_mpi_release (priv);

	if (ret == FALSE)
		gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_IO_ERROR);
}

void
gkr_session_negotiate (GkrOperation *op)
{
	GkrSession *session = NULL;

	G_LOCK(session_globals);
	{
		if (the_session)
			session = gkr_session_ref (the_session);
	}
	G_UNLOCK(session_globals);

	/* If we have a session just call through to the next step. */
	if (session) {
		gkr_callback_invoke_op_session (gkr_operation_pop (op), session);
		gkr_session_unref (session);
		return;
	}

	/* Try to start a new AES session */
	session_negotiate_aes (op);
}

void
gkr_session_clear (void)
{
	G_LOCK (session_globals);
	{
		if (the_session) {
			gkr_session_unref (the_session);
			the_session = NULL;
		}
	}
	G_UNLOCK (session_globals);
}

static gboolean
session_encode_secret (DBusMessageIter *iter, const gchar *path, gconstpointer parameter,
                       gsize n_parameter, gconstpointer secret, gsize n_secret)
{
	DBusMessageIter struc, array;
	const gchar *content_type = "text/plain; charset=utf8";

	/* Write out the result message */
	dbus_message_iter_open_container (iter, DBUS_TYPE_STRUCT, NULL, &struc);
	dbus_message_iter_append_basic (&struc, DBUS_TYPE_OBJECT_PATH, &path);
	dbus_message_iter_open_container (&struc, DBUS_TYPE_ARRAY, "y", &array);
	dbus_message_iter_append_fixed_array (&array, DBUS_TYPE_BYTE, &parameter, n_parameter);
	dbus_message_iter_close_container (&struc, &array);
	dbus_message_iter_open_container (&struc, DBUS_TYPE_ARRAY, "y", &array);
	dbus_message_iter_append_fixed_array (&array, DBUS_TYPE_BYTE, &secret, n_secret);
	dbus_message_iter_close_container (&struc, &array);
	dbus_message_iter_append_basic (&struc, DBUS_TYPE_STRING, &content_type);
	dbus_message_iter_close_container (iter, &struc);

	return TRUE;
}

static gboolean
session_encode_plain_secret (GkrSession *session, DBusMessageIter *iter, const gchar* secret)
{
	g_assert (session);
	g_assert (iter);
	g_assert (secret);

	return session_encode_secret (iter, session->path, "", 0, secret, strlen (secret));
}

static gboolean
session_encode_aes_secret (GkrSession *session, DBusMessageIter *iter, const gchar* secret)
{
	gcry_cipher_hd_t cih;
	guchar *padded;
	gsize n_padded, pos;
	gcry_error_t gcry;
	gpointer iv;

	g_assert (session);
	g_assert (iter);
	g_assert (secret);

	/* Create the cipher */
	gcry = gcry_cipher_open (&cih, GCRY_CIPHER_AES, GCRY_CIPHER_MODE_CBC, 0);
	if (gcry != 0) {
		g_warning ("couldn't create AES cipher: %s", gcry_strerror (gcry));
		return FALSE;
	}

	/* Perform the encoding here */
	padded = pkcs7_pad_string_in_secure_memory (secret, &n_padded);
	if (!padded) {
		g_warning ("couldn't encode secret for sending to service: invalid string");
		gcry_cipher_close (cih);
		return FALSE;
	}

	/* Setup the IV */
	iv = g_malloc0 (16);
	gcry_create_nonce (iv, 16);
	gcry = gcry_cipher_setiv (cih, iv, 16);
	g_return_val_if_fail (gcry == 0, FALSE);

	/* Setup the key */
	gcry = gcry_cipher_setkey (cih, session->key, session->n_key);
	g_return_val_if_fail (gcry == 0, FALSE);

	/* Perform the encryption */
	for (pos = 0; pos < n_padded; pos += 16) {
		gcry = gcry_cipher_encrypt (cih, (guchar*)padded + pos, 16, NULL, 0);
		g_return_val_if_fail (gcry == 0, FALSE);
	}

	gcry_cipher_close (cih);

	if (!session_encode_secret (iter, session->path, iv, 16, padded, n_padded))
		g_return_val_if_reached (FALSE);

	egg_secure_clear (padded, n_padded);
	egg_secure_free (padded);
	g_free (iv);

	return TRUE;
}

gboolean
gkr_session_encode_secret (GkrSession *session, DBusMessageIter *iter, const gchar* secret)
{
	g_assert (session);
	g_assert (iter);

	if (!secret)
		secret = "";

	if (session->key)
		return session_encode_aes_secret (session, iter, secret);
	else
		return session_encode_plain_secret (session, iter, secret);
}

static gboolean
session_decode_secret (DBusMessageIter *iter, const char **path, gconstpointer *parameter,
                       gsize *n_parameter, gconstpointer *secret, gsize *n_secret)
{
	DBusMessageIter struc, array;
	int n_elements;

	if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_STRUCT)
		return FALSE;
	dbus_message_iter_recurse (iter, &struc);

	if (dbus_message_iter_get_arg_type (&struc) != DBUS_TYPE_OBJECT_PATH)
		return FALSE;
	dbus_message_iter_get_basic (&struc, path);

	if (!dbus_message_iter_next (&struc) ||
	    dbus_message_iter_get_arg_type (&struc) != DBUS_TYPE_ARRAY ||
	    dbus_message_iter_get_element_type (&struc) != DBUS_TYPE_BYTE)
		return FALSE;
	dbus_message_iter_recurse (&struc, &array);
	dbus_message_iter_get_fixed_array (&array, parameter, &n_elements);
	*n_parameter = n_elements;

	if (!dbus_message_iter_next (&struc) ||
	    dbus_message_iter_get_arg_type (&struc) != DBUS_TYPE_ARRAY ||
	    dbus_message_iter_get_element_type (&struc) != DBUS_TYPE_BYTE)
		return FALSE;
	dbus_message_iter_recurse (&struc, &array);
	dbus_message_iter_get_fixed_array (&array, secret, &n_elements);
	*n_secret = n_elements;

	/*
	 * content_type: We have no use for the content-type, but check
	 * that it's there...
	 */
	if (!dbus_message_iter_next (&struc) ||
	    dbus_message_iter_get_arg_type (&struc) != DBUS_TYPE_STRING)
		return FALSE;

	return TRUE;
}

static gboolean
session_decode_plain_secret (GkrSession *session, DBusMessageIter *iter, gchar **result)
{
	gconstpointer parameter, secret;
	gsize n_parameter, n_secret;
	const char *path;

	g_assert (session);
	g_assert (iter);
	g_assert (result);

	if (!session_decode_secret (iter, &path, &parameter, &n_parameter, &secret, &n_secret))
		return FALSE;

	if (!g_str_equal (path, session->path)) {
		g_message ("received a secret encoded with wrong session");
		return FALSE;
	}

	if (n_parameter != 0) {
		g_message ("received a plain secret structure with invalid parameter");
		return FALSE;
	}

	if (!g_utf8_validate (secret, n_secret, NULL)) {
		g_message ("received a secret that was not utf8");
		return FALSE;
	}

	*result = egg_secure_alloc (n_secret + 1);
	memcpy (*result, secret, n_secret);
	return TRUE;
}

static gboolean
session_decode_aes_secret (GkrSession *session, DBusMessageIter *iter, gchar **result)
{
	gconstpointer parameter, secret;
	gsize n_parameter, n_secret, n_padded;
	const char *path;
	gcry_error_t gcry;
	gcry_cipher_hd_t cih;
	guchar *padded;
	gsize pos;

	g_assert (session);
	g_assert (iter);
	g_assert (result);

	if (!session_decode_secret (iter, &path, &parameter, &n_parameter, &secret, &n_secret))
		return FALSE;

	if (!g_str_equal (path, session->path)) {
		g_message ("received a secret encoded with wrong session");
		return FALSE;
	}

	if (n_parameter != 16) {
		g_message ("received an encrypted secret structure with invalid parameter");
		return FALSE;
	}

	if (n_secret == 0 || n_secret % 16 != 0) {
		g_message ("received an encrypted secret structure with bad secret length");
		return FALSE;
	}

	gcry = gcry_cipher_open (&cih, GCRY_CIPHER_AES, GCRY_CIPHER_MODE_CBC, 0);
	if (gcry != 0) {
		g_warning ("couldn't create AES cipher: %s", gcry_strerror (gcry));
		return FALSE;
	}

	gcry = gcry_cipher_setiv (cih, parameter, n_parameter);
	g_return_val_if_fail (gcry == 0, FALSE);

	gcry = gcry_cipher_setkey (cih, session->key, session->n_key);
	g_return_val_if_fail (gcry == 0, FALSE);

	/* Copy the memory buffer */
	n_padded = n_secret;
	padded = egg_secure_alloc (n_padded);
	memcpy (padded, secret, n_padded);

	/* Perform the decryption */
	for (pos = 0; pos < n_padded; pos += 16) {
		gcry = gcry_cipher_decrypt (cih, (guchar*)padded + pos, 16, NULL, 0);
		g_return_val_if_fail (gcry == 0, FALSE);
	}

	gcry_cipher_close (cih);

	/* Unpad the resulting value */
	*result = pkcs7_unpad_string_in_place (padded, n_padded);
	if (!*result) {
		egg_secure_clear (padded, n_padded);
		egg_secure_free (padded);
		g_message ("received an invalid, unencryptable, or non-utf8 secret");
		return FALSE;
	}

	return TRUE;
}

gboolean
gkr_session_decode_secret (GkrSession *session, DBusMessageIter *iter, gchar** secret)
{
	g_assert (session);
	g_assert (iter);
	g_assert (secret);

	if (session->key)
		return session_decode_aes_secret (session, iter, secret);
	else
		return session_decode_plain_secret (session, iter, secret);
}
