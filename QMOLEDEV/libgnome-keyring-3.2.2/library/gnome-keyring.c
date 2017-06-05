/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-keyring.c - library for talking to the keyring daemon.

   Copyright (C) 2003 Red Hat, Inc
   Copyright (C) 2007 Stefan Walter

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

   Author: Alexander Larsson <alexl@redhat.com>
   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gkr-callback.h"
#include "gkr-misc.h"
#include "gkr-operation.h"
#include "gkr-session.h"
#include "gnome-keyring.h"
#include "gnome-keyring-private.h"

#include "egg/egg-dbus.h"
#include "egg/egg-secure-memory.h"

#include <dbus/dbus.h>

#include <glib/gi18n-lib.h>

#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <stdarg.h>

typedef gboolean (*DecodeCallback) (DBusMessageIter *, gpointer);

typedef gboolean (*DecodeDictCallback) (const gchar *, DBusMessageIter *, gpointer);

typedef gboolean (*DecodePathCallback) (const char *, gpointer);

/**
 * SECTION:gnome-keyring-generic-callbacks
 * @title: Callbacks
 * @short_description: Different callbacks for retrieving async results
 */

static DBusMessage*
prepare_property_get (const gchar *path, const gchar *interface, const gchar *name)
{
	DBusMessage *req;

	g_assert (path);
	g_assert (name);

	if (!interface)
		interface = "";

	req = dbus_message_new_method_call (gkr_service_name (), path,
	                                    DBUS_INTERFACE_PROPERTIES, "Get");
	dbus_message_append_args (req, DBUS_TYPE_STRING, &interface,
	                          DBUS_TYPE_STRING, &name, DBUS_TYPE_INVALID);

	return req;
}

static DBusMessage*
prepare_property_getall (const gchar *path, const gchar *interface)
{
	DBusMessage *req;

	g_assert (path);

	if (!interface)
		interface = "";

	req = dbus_message_new_method_call (gkr_service_name (), path,
	                                    DBUS_INTERFACE_PROPERTIES, "GetAll");
	dbus_message_append_args (req, DBUS_TYPE_STRING, &interface, DBUS_TYPE_INVALID);

	return req;
}

static DBusMessage*
prepare_get_secret (GkrSession *session, const char *path)
{
	DBusMessage *req;
	const gchar *spath;

	g_assert (session);
	g_assert (path);

	req = dbus_message_new_method_call (gkr_service_name (), path,
	                                    ITEM_INTERFACE, "GetSecret");

	spath = gkr_session_get_path (session);
	dbus_message_append_args (req, DBUS_TYPE_OBJECT_PATH, &spath, DBUS_TYPE_INVALID);

	return req;
}

static DBusMessage*
prepare_get_secrets (GkrSession *session, char **paths, int n_paths)
{
	DBusMessage *req;
	const char *spath;

	g_assert (session);

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, "GetSecrets");

	spath = gkr_session_get_path (session);
	dbus_message_append_args (req, DBUS_TYPE_ARRAY, DBUS_TYPE_OBJECT_PATH, &paths, n_paths,
	                               DBUS_TYPE_OBJECT_PATH, &spath, DBUS_TYPE_INVALID);

	return req;
}

static DBusMessage*
prepare_xlock (const char *action, char **objects, int n_objects)
{
	DBusMessage *req;

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, action);

	dbus_message_append_args (req, DBUS_TYPE_ARRAY, DBUS_TYPE_OBJECT_PATH, &objects, n_objects,
	                          DBUS_TYPE_INVALID);

	return req;
}

static GnomeKeyringResult
decode_invalid_response (DBusMessage *reply)
{
	g_assert (reply);
	g_message ("call to daemon returned an invalid response: %s.%s()",
	           dbus_message_get_interface (reply),
	           dbus_message_get_member (reply));
	return GNOME_KEYRING_RESULT_IO_ERROR;
}

static GnomeKeyringResult
decode_property_variant_array (DBusMessage *reply, DecodeCallback callback,
                               gpointer user_data)
{
	DBusMessageIter iter, variant, array;
	int type;

	g_assert (reply);
	g_assert (callback);

	if (!dbus_message_has_signature (reply, "v"))
		return decode_invalid_response (reply);

	/* Iter to the variant */
	if (!dbus_message_iter_init (reply, &iter))
		g_return_val_if_reached (BROKEN);
	g_return_val_if_fail (dbus_message_iter_get_arg_type (&iter) == DBUS_TYPE_VARIANT, BROKEN);
	dbus_message_iter_recurse (&iter, &variant);

	/* Iter to the array */
	if (dbus_message_iter_get_arg_type (&variant) != DBUS_TYPE_ARRAY)
		return decode_invalid_response (reply);
	dbus_message_iter_recurse (&variant, &array);

	/* Each item in the array */
	for (;;) {
		type = dbus_message_iter_get_arg_type (&array);
		if (type == DBUS_TYPE_INVALID)
			break;
		if (!(callback) (&array, user_data))
			return decode_invalid_response (reply);

		dbus_message_iter_next (&array);
	}

	return GNOME_KEYRING_RESULT_OK;
}

static GnomeKeyringResult
decode_property_dict (DBusMessage *reply, DecodeDictCallback callback,
                      gpointer user_data)
{
	DBusMessageIter iter, variant, array, dict;
	const char *property;
	int type;

	g_assert (reply);
	g_assert (callback);

	if (!dbus_message_has_signature (reply, "a{sv}"))
		return decode_invalid_response (reply);

	/* Iter to the array */
	if (!dbus_message_iter_init (reply, &iter))
		g_return_val_if_reached (BROKEN);
	g_return_val_if_fail (dbus_message_iter_get_arg_type (&iter) == DBUS_TYPE_ARRAY, BROKEN);
	dbus_message_iter_recurse (&iter, &array);

	/* Each dict entry in the array */
	for (;;) {
		type = dbus_message_iter_get_arg_type (&array);
		if (type == DBUS_TYPE_INVALID)
			break;
		g_return_val_if_fail (type == DBUS_TYPE_DICT_ENTRY, BROKEN);

		dbus_message_iter_recurse (&array, &dict);

		/* The property type */
		g_return_val_if_fail (dbus_message_iter_get_arg_type (&dict) == DBUS_TYPE_STRING, BROKEN);
		dbus_message_iter_get_basic (&dict, &property);
		g_return_val_if_fail (property, BROKEN);

		/* The variant value */
		if (!dbus_message_iter_next (&dict))
			g_return_val_if_reached (BROKEN);
		g_return_val_if_fail (dbus_message_iter_get_arg_type (&dict) == DBUS_TYPE_VARIANT, BROKEN);
		dbus_message_iter_recurse (&dict, &variant);

		if (!(callback) (property, &variant, user_data))
			return decode_invalid_response (reply);

		dbus_message_iter_next (&array);
	}

	return GNOME_KEYRING_RESULT_OK;
}

static gboolean
decode_get_attributes_foreach (DBusMessageIter *iter, gpointer user_data)
{
	GHashTable *table = user_data;
	DBusMessageIter dict;
	const char *name;
	const char *value;

	if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_DICT_ENTRY)
		return FALSE;

	dbus_message_iter_recurse (iter, &dict);
	if (dbus_message_iter_get_arg_type (&dict) != DBUS_TYPE_STRING)
		return FALSE;
	dbus_message_iter_get_basic (&dict, &name);

	dbus_message_iter_next (&dict);
	if (dbus_message_iter_get_arg_type (&dict) != DBUS_TYPE_STRING)
		return FALSE;
	dbus_message_iter_get_basic (&dict, &value);

	/* These strings will last as long as the message, so no need to dup */
	g_return_val_if_fail (name && value, FALSE);
	g_hash_table_insert (table, (char*)name, (char*)value);
	return TRUE;
}

static GnomeKeyringResult
decode_get_attributes (DBusMessage *reply, GnomeKeyringAttributeList *attrs)
{
	GnomeKeyringResult res;
	GHashTableIter iter;
	GHashTable *table;
	const char *name;
	const char *value;
	guint32 number;
	gchar *check, *end;
	gboolean is_uint32;

	g_assert (reply);

	table = g_hash_table_new (g_str_hash, g_str_equal);
	res = decode_property_variant_array (reply, decode_get_attributes_foreach, table);
	if (res == GNOME_KEYRING_RESULT_OK) {
		g_hash_table_iter_init (&iter, table);
		while (g_hash_table_iter_next (&iter, (gpointer*)&name, (gpointer*)&value)) {
			g_assert (name && value);

			/* Hide these gnome-keyring internal attributes */
			if (g_str_has_prefix (name, "gkr:"))
				continue;

			/*
			 * Figure out the attribute type. In the secrets service
			 * all attributes have string values. The daemon will
			 * set a special compat attribute to indicate to us
			 * whether this was a uint32
			 */
			check = g_strdup_printf ("gkr:compat:uint32:%s", name);
			is_uint32 = g_hash_table_lookup (table, check) != NULL;
			g_free (check);

			if (is_uint32) {
				number = strtoul (value, &end, 10);
				if (end && end[0] == '\0')
					gnome_keyring_attribute_list_append_uint32 (attrs, name, number);
				else
					is_uint32 = FALSE;
			}

			if (!is_uint32)
				gnome_keyring_attribute_list_append_string (attrs, name, value);
		}
	}

	g_hash_table_destroy (table);
	return res;
}

static gboolean
decode_xlock_reply (DBusMessage *reply, const char **prompt,
                    DecodePathCallback callback, gpointer user_data)
{
	DBusMessageIter iter, array;
	const char *path;

	g_assert (reply);
	g_assert (prompt);
	g_assert (callback);

	if (!dbus_message_has_signature (reply, "aoo"))
		return FALSE;

	if (!dbus_message_iter_init (reply, &iter))
		g_return_val_if_reached (FALSE);
	dbus_message_iter_recurse (&iter, &array);
	if (!dbus_message_iter_next (&iter) ||
	    dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_OBJECT_PATH)
		g_return_val_if_reached (FALSE);
	dbus_message_iter_get_basic (&iter, prompt);
	g_return_val_if_fail (prompt, FALSE);

	while (dbus_message_iter_get_arg_type (&array) == DBUS_TYPE_OBJECT_PATH) {
		path = NULL;
		dbus_message_iter_get_basic (&array, &path);
		g_return_val_if_fail (path, FALSE);

		if (!(callback) (path, user_data))
			break;
		if (!dbus_message_iter_next (&array))
			break;
	}

	return TRUE;
}

static gboolean
decode_xlock_completed (DBusMessage *reply, gboolean *dismissed,
                        DecodePathCallback callback, gpointer user_data)
{
	DBusMessageIter variant, iter, array;
	dbus_bool_t bval;
	const char *path;
	char *signature;
	gboolean equal;

	g_assert (reply);
	g_assert (dismissed);
	g_assert (callback);

	if (!dbus_message_has_signature (reply, "bv"))
		return FALSE;

	if (!dbus_message_iter_init (reply, &iter))
		g_return_val_if_reached (FALSE);
	dbus_message_iter_get_basic (&iter, &bval);
	*dismissed = bval;

	/* Prompt was dismissed */
	if (bval == TRUE)
		return TRUE;

	/* Dig out the variant */
	if (!dbus_message_iter_next (&iter))
		g_return_val_if_reached (FALSE);
	dbus_message_iter_recurse (&iter, &variant);

	signature = dbus_message_iter_get_signature (&variant);
	equal = g_str_equal (signature, "ao");
	dbus_free (signature);
	if (!equal)
		return FALSE;

	g_return_val_if_fail (dbus_message_iter_get_arg_type (&variant) == DBUS_TYPE_ARRAY, FALSE);
	g_return_val_if_fail (dbus_message_iter_get_element_type (&variant) == DBUS_TYPE_OBJECT_PATH, FALSE);

	dbus_message_iter_recurse (&variant, &array);

	while (dbus_message_iter_get_arg_type (&array) == DBUS_TYPE_OBJECT_PATH) {
		path = NULL;
		dbus_message_iter_get_basic (&array, &path);
		g_return_val_if_fail (path, FALSE);

		if (!(callback) (path, user_data))
			break;
		if (!dbus_message_iter_next (&array))
			break;
	}

	return TRUE;
}

static void
encode_attribute_list (DBusMessageIter *iter, GnomeKeyringAttributeList *attrs)
{
	DBusMessageIter dict, array;
	GnomeKeyringAttribute *attr;
	const gchar *string;
	gchar *value;
	guint i;

	dbus_message_iter_open_container (iter, DBUS_TYPE_ARRAY, "{ss}", &array);

	for (i = 0; attrs && i < attrs->len; ++i) {
		attr = &gnome_keyring_attribute_list_index (attrs, i);
		dbus_message_iter_open_container (&array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);

		/* Add in the attribute type */
		string = attr->name ? attr->name : "";
		dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);

		/* String values */
		if (attr->type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
			string = attr->value.string ? attr->value.string : "";
			dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);

		/* Integer values */
		} else if (attr->type == GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32) {
			value = g_strdup_printf ("%u", attr->value.integer);
			dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &value);
			g_free (value);

		} else {
			g_warning ("received invalid attribute type");
			return;
		}

		dbus_message_iter_close_container (&array, &dict);

		/* Integer values get another compatibility marker */
		if (attr->type == GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32) {
			dbus_message_iter_open_container (&array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);

			value = g_strdup_printf ("gkr:compat:uint32:%s", attr->name);
			dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &value);
			g_free (value);
			string = "";
			dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);

			dbus_message_iter_close_container (&array, &dict);
		}
	}

	dbus_message_iter_close_container (iter, &array);
}

/**
 * SECTION:gnome-keyring-misc
 * @title: Miscellaneous Functions
 * @short_description: Miscellaneous functions.
 **/

/**
 * gnome_keyring_is_available:
 *
 * Check whether you can communicate with a gnome-keyring-daemon.
 *
 * Return value: %FALSE if you can't communicate with the daemon (so you
 * can't load and save passwords).
 **/
gboolean
gnome_keyring_is_available (void)
{
	GkrOperation *op;
	DBusMessage *req;

	gkr_init ();

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    DBUS_INTERFACE_PEER, "Ping");

	op = gkr_operation_new (gkr_callback_empty, GKR_CALLBACK_RES, NULL, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
	return gkr_operation_block_and_unref (op) == GNOME_KEYRING_RESULT_OK;
}

/**
 * gnome_keyring_cancel_request:
 * @request: The request returned from the asynchronous call function.
 *
 * Cancel an asynchronous request.
 *
 * If a callback was registered when making the asynchronous request, that callback
 * function will be called with a result of %GNOME_KEYRING_RESULT_CANCELLED
 **/
void
gnome_keyring_cancel_request (gpointer request)
{
	GkrOperation *op = request;

	gkr_init ();

	g_return_if_fail (request);
	gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_CANCELLED);
}

/**
 * SECTION:gnome-keyring-keyrings
 * @title: Keyrings
 * @short_description: Listing and managing keyrings
 *
 * <code>gnome-keyring-daemon</code> manages multiple keyrings. Each keyring can
 * store one or more items containing secrets.
 *
 * One of the keyrings is the default keyring, which can in many cases be used
 * by specifying %NULL for a keyring name.
 *
 * Each keyring can be in a locked or unlocked state. A password must be
 * specified, either by the user or the calling application, to unlock the
 * keyring.
 */

static GkrOperation*
set_default_keyring_start (const gchar *keyring, GnomeKeyringOperationDoneCallback callback,
                           gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	const char *string;
	GkrOperation *op;
	gchar *path;

	g_return_val_if_fail (keyring, NULL);
	g_return_val_if_fail (callback, NULL);

	path = gkr_encode_keyring_name (keyring);
	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, "SetAlias");

	string = "default";
	dbus_message_append_args (req, DBUS_TYPE_STRING, &string,
	                          DBUS_TYPE_OBJECT_PATH, &path, DBUS_TYPE_INVALID);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_set_keyring_hint (op);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
	g_free (path);

	return op;
}

/**
 * gnome_keyring_set_default_keyring:
 * @keyring: The keyring to make default
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Change the default keyring.
 *
 * For a synchronous version of this function see gnome_keyring_set_default_keyring_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_set_default_keyring (const gchar                             *keyring,
                                   GnomeKeyringOperationDoneCallback       callback,
                                   gpointer                                data,
                                   GDestroyNotify                          destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = set_default_keyring_start (keyring, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_set_default_keyring_sync:
 * @keyring: The keyring to make default
 *
 * Change the default keyring.
 *
 * For an asynchronous version of this function see gnome_keyring_set_default_keyring().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_set_default_keyring_sync (const char *keyring)
{
	GkrOperation *op;

	g_return_val_if_fail (keyring, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	op = set_default_keyring_start (keyring, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

static void
get_default_keyring_sync (GnomeKeyringResult res, const gchar *name, gpointer user_data)
{
	gchar **result = user_data;
	*result = (gchar*)name;
}

static void
get_default_keyring_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	GkrCallback *cb;
	const char *path;
	gchar *name;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_get_args (reply, NULL, DBUS_TYPE_OBJECT_PATH, &path,
	                            DBUS_TYPE_INVALID)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	if (g_str_equal (path, "/")) {
		name = NULL;
	} else {
		name = gkr_decode_keyring_name (path);
		if (name == NULL) {
			gkr_operation_complete (op, decode_invalid_response (reply));
			return;
		}
	}

	cb = gkr_operation_pop (op);
	gkr_callback_invoke_ok_string (cb, name);
	if (cb->callback != get_default_keyring_sync)
		g_free (name);
}

static GkrOperation*
get_default_keyring_start (GnomeKeyringOperationGetStringCallback callback,
                           gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	const char *string;
	GkrOperation *op;

	g_return_val_if_fail (callback, NULL);

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, "ReadAlias");

	string = "default";
	dbus_message_append_args (req, DBUS_TYPE_STRING, &string, DBUS_TYPE_INVALID);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_STRING, data, destroy_data);
	gkr_operation_push (op, get_default_keyring_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);

	return op;
}

/**
 * gnome_keyring_get_default_keyring:
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Get the default keyring name, which will be passed to the @callback. If no
 * default keyring exists, then %NULL will be passed to the @callback. The
 * string will be freed after @callback returns.
 *
 * For a synchronous version of this function see gnome_keyring_get_default_keyring_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_get_default_keyring (GnomeKeyringOperationGetStringCallback  callback,
                                   gpointer                                data,
                                   GDestroyNotify                          destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = get_default_keyring_start (callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_get_default_keyring_sync:
 * @keyring: Location for the default keyring name to be returned.
 *
 * Get the default keyring name.
 *
 * The string returned in @keyring must be freed with g_free().
 *
 * For an asynchronous version of this function see gnome_keyring_get_default_keyring().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_get_default_keyring_sync (char **keyring)
{
	GkrOperation *op;

	g_return_val_if_fail (keyring, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	op = get_default_keyring_start (get_default_keyring_sync, keyring, NULL);
	return gkr_operation_block_and_unref (op);
}

static void
list_keyring_names_sync (GnomeKeyringResult res, GList *names, gpointer user_data)
{
	GList **result = user_data;
	*result = names;
}

static gboolean
list_keyring_names_foreach (DBusMessageIter *iter, gpointer user_data)
{
	GList **names = user_data;
	const char *path;
	gchar *name;

	if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_OBJECT_PATH)
		return FALSE;

	/* The object path, gets converted into a name */
	dbus_message_iter_get_basic (iter, &path);
	name = gkr_decode_keyring_name (path);
	if (name != NULL)
		*names = g_list_prepend (*names, name);

	return TRUE;
}

static void
list_keyring_names_reply (GkrOperation *op, DBusMessage *reply,
                          gpointer user_data)
{
	GnomeKeyringResult res;
	GList *names = NULL;
	GkrCallback *cb;

	if (gkr_operation_handle_errors (op, reply))
		return;

	res = decode_property_variant_array (reply, list_keyring_names_foreach, &names);
	if (res == GNOME_KEYRING_RESULT_OK) {
		cb = gkr_operation_pop (op);
		gkr_callback_invoke_ok_list (cb, names);
		if (cb->callback == list_keyring_names_sync)
			names = NULL;
	} else {
		gkr_operation_complete (op, res);
	}

	gnome_keyring_string_list_free (names);
}

static GkrOperation*
list_keyring_names_start (GnomeKeyringOperationGetListCallback callback,
                          gpointer data, GDestroyNotify destroy_data)
{
	GkrOperation *op;
	DBusMessage *req;

	g_return_val_if_fail (callback, NULL);

	req = prepare_property_get (SERVICE_PATH, SERVICE_INTERFACE, "Collections");

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_LIST, data, destroy_data);
	gkr_operation_push (op, list_keyring_names_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
	return op;
}

/**
 * gnome_keyring_list_keyring_names:
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Get a list of keyring names.
 *
 * A %GList of null terminated strings will be passed to
 * the @callback. If no keyrings exist then an empty list will be passed to the
 * @callback. The list is freed after @callback returns.
 *
 * For a synchronous version of this function see gnome_keyring_list_keyring_names_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_list_keyring_names (GnomeKeyringOperationGetListCallback    callback,
                                  gpointer                                data,
                                  GDestroyNotify                          destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = list_keyring_names_start (callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_list_keyring_names_sync:
 * @keyrings: Location for a %GList of keyring names to be returned.
 *
 * Get a list of keyring names.
 *
 * The list returned in in @keyrings must be freed using
 * gnome_keyring_string_list_free().
 *
 * For an asynchronous version of this function see gnome_keyring_list_keyring_names().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_list_keyring_names_sync (GList **keyrings)
{
	GkrOperation *op;

	g_return_val_if_fail (keyrings, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	op = list_keyring_names_start (list_keyring_names_sync, keyrings, NULL);
	return gkr_operation_block_and_unref (op);
}

static GkrOperation*
lock_all_start (GnomeKeyringOperationDoneCallback callback,
                gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;

	g_return_val_if_fail (callback, NULL);

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, "LockService");

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_request (op, req);
	dbus_message_unref (req);

	return op;
}

/**
 * gnome_keyring_lock_all:
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Lock all the keyrings, so that their contents may not be accessed without
 * first unlocking them with a password.
 *
 * For a synchronous version of this function see gnome_keyring_lock_all_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_lock_all (GnomeKeyringOperationDoneCallback       callback,
                        gpointer                                data,
                        GDestroyNotify                          destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = lock_all_start (callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_lock_all_sync:
 *
 * Lock all the keyrings, so that their contents may not eb accessed without
 * first unlocking them with a password.
 *
 * For an asynchronous version of this function see gnome_keyring_lock_all().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_lock_all_sync (void)
{
	GkrOperation *op;

	gkr_init ();

	op = lock_all_start (gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

typedef struct _create_keyring_args {
	gchar *keyring_name;
	gchar *password;
} create_keyring_args;

static void
create_keyring_free (gpointer data)
{
	create_keyring_args *args = data;
	g_free (args->keyring_name);
	egg_secure_strfree (args->password);
	g_slice_free (create_keyring_args, args);
}

static void
create_keyring_encode_properties (DBusMessageIter *iter, const gchar *keyring_name)
{
	DBusMessageIter array, dict, variant;
	const gchar *label = COLLECTION_INTERFACE ".Label";

	dbus_message_iter_open_container (iter, DBUS_TYPE_ARRAY, "{sv}", &array);
	dbus_message_iter_open_container (&array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);
	dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &label);
	dbus_message_iter_open_container (&dict, DBUS_TYPE_VARIANT, "s", &variant);
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_STRING, &keyring_name);
	dbus_message_iter_close_container (&dict, &variant);
	dbus_message_iter_close_container (&array, &dict);
	dbus_message_iter_close_container (iter, &array);
}

static void
create_keyring_password_reply (GkrOperation *op, GkrSession *session, gpointer user_data)
{
	create_keyring_args *args = user_data;
	DBusMessageIter iter;
	DBusMessage *req;

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    "org.gnome.keyring.InternalUnsupportedGuiltRiddenInterface",
	                                    "CreateWithMasterPassword");

	dbus_message_iter_init_append (req, &iter);
	create_keyring_encode_properties (&iter, args->keyring_name);
	if (!gkr_session_encode_secret (session, &iter, args->password)) {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		dbus_message_unref (req);
		return;
	}

	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static void
create_keyring_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	const char *collection;
	const char *prompt;

	if (gkr_operation_handle_errors (op, reply))
		return;

	/* Parse the response */
	if (!dbus_message_get_args (reply, NULL, DBUS_TYPE_OBJECT_PATH, &collection,
	                            DBUS_TYPE_OBJECT_PATH, &prompt, DBUS_TYPE_INVALID)) {
		g_warning ("bad response to CreateCollection from service");
		gkr_callback_invoke_res (gkr_operation_pop (op), GNOME_KEYRING_RESULT_IO_ERROR);
		return;
	}

	/* No prompt, we're done */
	g_return_if_fail (prompt);
	if (g_str_equal (prompt, "/"))
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_OK);

	/* A prompt, display it, default handling for response */
	else
		gkr_operation_prompt (op, prompt);
}

static void
create_keyring_check_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	create_keyring_args *args = user_data;
	DBusMessageIter iter;
	DBusMessage *req;
	const gchar *alias = "";

	/* If no such object, then no such keyring exists and we're good to go. */
	if (!dbus_message_is_error (reply, ERROR_NO_SUCH_OBJECT)) {
		/* Success means 'already exists' */
		if (!gkr_operation_handle_errors (op, reply))
			gkr_operation_complete (op, GNOME_KEYRING_RESULT_ALREADY_EXISTS);
		return;
	}

	/* With a password requires a session, so get on that */
	if (args->password) {
		gkr_operation_push (op, create_keyring_password_reply, GKR_CALLBACK_OP_SESSION, args, NULL);
		gkr_session_negotiate (op);

	/* Otherwiswe just create the collection */
	} else {
		req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
		                                    SERVICE_INTERFACE, "CreateCollection");
		dbus_message_iter_init_append (req, &iter);
		create_keyring_encode_properties (&iter, args->keyring_name);
		dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &alias);
		gkr_operation_push (op, create_keyring_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
		gkr_operation_request (op, req);
		dbus_message_unref (req);
	}
}

static GkrOperation*
create_keyring_start (const char *keyring_name, const char *password,
                      GnomeKeyringOperationDoneCallback callback,
                      gpointer data, GDestroyNotify destroy_data)
{
	create_keyring_args *args;
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	g_return_val_if_fail (callback, NULL);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);

	args = g_slice_new0 (create_keyring_args);
	args->keyring_name = g_strdup (keyring_name);
	args->password = egg_secure_strdup (password);

	/*
	 * The secrets API has a significantly different model with creating of
	 * keyrings, where we never get an 'already exists' error. However this
	 * breaks certain strange uses of gnome_keyring_create ().
	 *
	 * So we simulate 'already exists' in a fairly good, but 'racy' manner.
	 */

	path = gkr_encode_keyring_name (keyring_name);
	req = prepare_property_get (path, COLLECTION_INTERFACE, "Label");
	gkr_operation_push (op, create_keyring_check_reply, GKR_CALLBACK_OP_MSG,
	                    args, create_keyring_free);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
	g_free (path);

	return op;
}

/**
 * gnome_keyring_create:
 * @keyring_name: The new keyring name. Must not be %NULL.
 * @password: The password for the new keyring. If %NULL user will be prompted.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Create a new keyring with the specified name. In most cases %NULL will be
 * passed as the @password, which will prompt the user to enter a password
 * of their choice.
 *
 * For a synchronous version of this function see gnome_keyring_create_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_create (const char                                  *keyring_name,
                      const char                                  *password,
                      GnomeKeyringOperationDoneCallback            callback,
                      gpointer                                     data,
                      GDestroyNotify                               destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = create_keyring_start (keyring_name, password, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_create_sync:
 * @keyring_name: The new keyring name. Must not be %NULL
 * @password: The password for the new keyring. If %NULL user will be prompted.
 *
 * Create a new keyring with the specified name. In most cases %NULL will be
 * passed in as the @password, which will prompt the user to enter a password
 * of their choice.
 *
 * For an asynchronous version of this function see gnome_keyring_create().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_create_sync (const char *keyring_name,
                           const char *password)
{
	GkrOperation *op;

	gkr_init ();

	op = create_keyring_start (keyring_name, password, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

typedef struct _xlock_check_args {
	const gchar *path;
	gboolean matched;
} xlock_check_args;

static gboolean
xlock_check_path (const char *path, gpointer user_data)
{
	xlock_check_args *args = user_data;
	g_assert (path);
	g_assert (args->path);
	args->matched = g_str_equal (path, args->path);
	return !args->matched;
}

static void
xlock_2_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	xlock_check_args args = { user_data, FALSE };
	gboolean dismissed;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!decode_xlock_completed (reply, &dismissed, xlock_check_path, &args)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	if (dismissed || !args.matched)
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_DENIED);
	else
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_OK);
}

static void
xlock_1_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	xlock_check_args args = { user_data, FALSE };
	const char *prompt;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!decode_xlock_reply (reply, &prompt, xlock_check_path, &args)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	if (args.matched) {
		gkr_callback_invoke_res (gkr_operation_pop (op), GNOME_KEYRING_RESULT_OK);
		return;
	}

	/* Is there a prompt needed? */
	if (!g_str_equal (prompt, "/")) {
		gkr_operation_push (op, xlock_2_reply, GKR_CALLBACK_OP_MSG, user_data, NULL);
		gkr_operation_prompt (op, prompt);

	/* No prompt, and no opportunity to */
	} else {
		gkr_callback_invoke_res (gkr_operation_pop (op), GNOME_KEYRING_RESULT_NO_SUCH_KEYRING);
	}
}

static GkrOperation*
xlock_async (const gchar *method, const gchar *keyring,
             GnomeKeyringOperationDoneCallback callback,
             gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	path = gkr_encode_keyring_name (keyring);
	req = prepare_xlock (method, &path, 1);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_push (op, xlock_1_reply, GKR_CALLBACK_OP_MSG, path, g_free);
	gkr_operation_request (op, req);

	dbus_message_unref (req);
	return op;
}

typedef struct _unlock_password_args {
	gchar *keyring_name;
	gchar *password;
} unlock_password_args;

static void
unlock_password_free (gpointer data)
{
	unlock_password_args *args = data;
	g_free (args->keyring_name);
	egg_secure_strfree (args->password);
	g_slice_free (unlock_password_args, args);
}

static void
unlock_password_reply (GkrOperation *op, GkrSession *session, gpointer user_data)
{
	unlock_password_args *args = user_data;
	DBusMessageIter iter;
	DBusMessage *req;
	gchar *path;

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    "org.gnome.keyring.InternalUnsupportedGuiltRiddenInterface",
	                                    "UnlockWithMasterPassword");

	dbus_message_iter_init_append (req, &iter);
	path = gkr_encode_keyring_name (args->keyring_name);
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_OBJECT_PATH, &path);
	g_free (path);
	if (!gkr_session_encode_secret (session, &iter, args->password)) {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		dbus_message_unref (req);
		return;
	}

	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static GkrOperation*
unlock_keyring_start (const char *keyring, const char *password,
                      GnomeKeyringOperationDoneCallback callback,
                      gpointer data, GDestroyNotify destroy_data)
{
	unlock_password_args *args;
	GkrOperation *op;

	g_return_val_if_fail (callback, NULL);

	/* Null password, standard operation */
	if (password == NULL)
		return xlock_async ("Unlock", keyring, callback, data, destroy_data);

	g_return_val_if_fail (callback, NULL);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);

	args = g_slice_new0 (unlock_password_args);
	args->keyring_name = g_strdup (keyring);
	args->password = egg_secure_strdup (password);
	gkr_operation_push (op, unlock_password_reply, GKR_CALLBACK_OP_SESSION,
	                    args, unlock_password_free);
	gkr_operation_set_keyring_hint (op);
	gkr_session_negotiate (op);

	return op;
}

/**
 * gnome_keyring_unlock:
 * @keyring: The name of the keyring to unlock, or %NULL for the default keyring.
 * @password: The password to unlock the keyring with, or %NULL to prompt the user.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Unlock a @keyring, so that its contents may be accessed. In most cases %NULL
 * will be passed as the @password, which will prompt the user to enter the
 * correct password.
 *
 * Most keyring operations involving items require that you first unlock the
 * keyring. One exception is gnome_keyring_find_items() and related functions.
 *
 * For a synchronous version of this function see gnome_keyring_unlock_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_unlock (const char                                  *keyring,
                      const char                                  *password,
                      GnomeKeyringOperationDoneCallback            callback,
                      gpointer                                     data,
                      GDestroyNotify                               destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = unlock_keyring_start (keyring, password, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_unlock_sync:
 * @keyring: The name of the keyring to unlock, or %NULL for the default keyring.
 * @password: The password to unlock the keyring with, or %NULL to prompt the user.
 *
 * Unlock a @keyring, so that its contents may be accessed. In most cases %NULL
 * will be passed in as the @password, which will prompt the user to enter the
 * correct password.
 *
 * Most keyring opretaions involving items require that yo ufirst unlock the
 * keyring. One exception is gnome_keyring_find_items_sync() and related functions.
 *
 * For an asynchronous version of this function see gnome_keyring_unlock().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_unlock_sync (const char *keyring,
                           const char *password)
{
	GkrOperation *op;

	gkr_init ();

	op = unlock_keyring_start (keyring, password, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

static GkrOperation*
lock_keyring_start (const char *keyring, GnomeKeyringOperationDoneCallback callback,
                    gpointer data, GDestroyNotify destroy_data)
{
	g_return_val_if_fail (callback, NULL);
	return xlock_async ("Lock", keyring, callback, data, destroy_data);
}

/**
 * gnome_keyring_lock:
 * @keyring: The name of the keyring to lock, or %NULL for the default keyring.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Lock a @keyring, so that its contents may not be accessed without first
 * supplying a password.
 *
 * Most keyring operations involving items require that you first unlock the
 * keyring. One exception is gnome_keyring_find_items() and related functions.
 *
 * For a synchronous version of this function see gnome_keyring_lock_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_lock (const char                                  *keyring,
                    GnomeKeyringOperationDoneCallback            callback,
                    gpointer                                     data,
                    GDestroyNotify                               destroy_data)
{
	GkrOperation* op;

	gkr_init ();

	op = lock_keyring_start (keyring, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_lock_sync:
 * @keyring: The name of the keyring to lock, or %NULL for the default keyring.
 *
 * Lock a @keyring, so that its contents may not be accessed without first
 * supplying a password.
 *
 * Most keyring opretaions involving items require that you first unlock the
 * keyring. One exception is gnome_keyring_find_items_sync() and related functions.
 *
 * For an asynchronous version of this function see gnome_keyring_lock().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_lock_sync (const char *keyring)
{
	GkrOperation *op;

	gkr_init ();

	op = lock_keyring_start (keyring, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

static GkrOperation*
delete_keyring_start (const char *keyring, GnomeKeyringOperationDoneCallback callback,
                      gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	g_return_val_if_fail (callback, NULL);

	path = gkr_encode_keyring_name (keyring);
	req = dbus_message_new_method_call (gkr_service_name (), path,
	                                    COLLECTION_INTERFACE, "Delete");

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
	g_free (path);

	return op;
}

/**
 * gnome_keyring_delete:
 * @keyring: The name of the keyring to delete. Cannot be %NULL.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Delete @keyring. Once a keyring is deleted there is no mechanism for
 * recovery of its contents.
 *
 * For a synchronous version of this function see gnome_keyring_delete_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_delete (const char                                  *keyring,
                      GnomeKeyringOperationDoneCallback            callback,
                      gpointer                                     data,
                      GDestroyNotify                               destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = delete_keyring_start (keyring, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_delete_sync:
 * @keyring: The name of the keyring to delete. Cannot be %NULL
 *
 * Delete @keyring. Once a keyring is deleted there is no mechanism for
 * recovery of its contents.
 *
 * For an asynchronous version of this function see gnome_keyring_delete().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_delete_sync (const char *keyring)
{
	GkrOperation *op;

	gkr_init ();

	op = delete_keyring_start (keyring, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

typedef struct _change_password_args {
	gchar *keyring_name;
	gchar *password;
	gchar *original;
} change_password_args;

static void
change_password_free (gpointer data)
{
	change_password_args *args = data;
	g_free (args->keyring_name);
	egg_secure_strfree (args->password);
	egg_secure_strfree (args->original);
	g_slice_free (change_password_args, args);
}

static void
change_password_reply (GkrOperation *op, GkrSession *session, gpointer user_data)
{
	change_password_args *args = user_data;
	DBusMessageIter iter;
	DBusMessage *req;
	gchar *path;

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    "org.gnome.keyring.InternalUnsupportedGuiltRiddenInterface",
	                                    "ChangeWithMasterPassword");

	dbus_message_iter_init_append (req, &iter);
	path = gkr_encode_keyring_name (args->keyring_name);
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_OBJECT_PATH, &path);
	g_free (path);
	if (!gkr_session_encode_secret (session, &iter, args->original) ||
	    !gkr_session_encode_secret (session, &iter, args->password)) {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		dbus_message_unref (req);
		return;
	}

	gkr_operation_request (op, req);
	dbus_message_unref (req);
}


static void
change_2_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	DBusMessageIter iter;
	dbus_bool_t dismissed;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_has_signature (reply, "bv")) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	if (!dbus_message_iter_init (reply, &iter))
		g_return_if_reached ();
	dbus_message_iter_get_basic (&iter, &dismissed);

	if (dismissed)
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_DENIED);
	else
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_OK);
}

static void
change_1_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	const char *prompt;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_get_args (reply, NULL, DBUS_TYPE_OBJECT_PATH, &prompt, DBUS_TYPE_INVALID)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Is there a prompt needed? */
	if (!g_str_equal (prompt, "/")) {
		gkr_operation_push (op, change_2_reply, GKR_CALLBACK_OP_MSG, user_data, NULL);
		gkr_operation_prompt (op, prompt);

	/* Somehow, no prompt was necessary */
	} else {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_OK);
	}
}

static GkrOperation*
change_password_start (const char *keyring, const char *original, const char *password,
                       GnomeKeyringOperationDoneCallback callback, gpointer data,
                       GDestroyNotify destroy_data)
{
	change_password_args *args;
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	g_return_val_if_fail (callback, NULL);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);

	/* With and without password are completely different */

	if (password || original) {
		args = g_slice_new0 (change_password_args);
		args->keyring_name = g_strdup (keyring);
		args->password = egg_secure_strdup (password);
		args->original = egg_secure_strdup (original);
		gkr_operation_push (op, change_password_reply, GKR_CALLBACK_OP_SESSION,
		                    args, change_password_free);
		gkr_session_negotiate (op);

	} else {
		req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
		                                    SERVICE_INTERFACE, "ChangeLock");
		path = gkr_encode_keyring_name (keyring);
		dbus_message_append_args (req, DBUS_TYPE_OBJECT_PATH, &path, DBUS_TYPE_INVALID);
		gkr_operation_push (op, change_1_reply, GKR_CALLBACK_OP_MSG, path, g_free);
		gkr_operation_request (op, req);
		dbus_message_unref (req);
	}

	return op;
}

/**
 * gnome_keyring_change_password:
 * @keyring: The name of the keyring to change the password for. Cannot be %NULL.
 * @original: The old keyring password, or %NULL to prompt the user for it.
 * @password: The new keyring password, or %NULL to prompt the user for it.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Change the password for a @keyring. In most cases you would specify %NULL for
 * both the @original and @password arguments and allow the user to type the
 * correct passwords.
 *
 * For a synchronous version of this function see gnome_keyring_change_password_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_change_password (const char                                  *keyring,
                               const char                                  *original,
                               const char                                  *password,
                               GnomeKeyringOperationDoneCallback            callback,
                               gpointer                                     data,
                               GDestroyNotify                               destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = change_password_start (keyring, original, password, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}


/**
 * gnome_keyring_change_password_sync:
 * @keyring: The name of the keyring to change the password for. Cannot be %NULL
 * @original: The old keyring password, or %NULL to prompt the user for it.
 * @password: The new keyring password, or %NULL to prompt the user for it.
 *
 * Change the password for @keyring. In most cases you would specify %NULL for
 * both the @original and @password arguments and allow the user to type the
 * correct passwords.
 *
 * For an asynchronous version of this function see gnome_keyring_change_password().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_change_password_sync (const char *keyring_name,
                                    const char *original, const char *password)
{
	GkrOperation *op;

	gkr_init ();

	op = change_password_start (keyring_name, original, password, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

static gboolean
get_keyring_info_foreach (const gchar *property, DBusMessageIter *iter, gpointer user_data)
{
	GnomeKeyringInfo *info = user_data;
	dbus_bool_t bval;
	dbus_int64_t i64val;

	if (g_str_equal (property, "Locked")) {
		if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_BOOLEAN)
			return FALSE;
		dbus_message_iter_get_basic (iter, &bval);
		info->is_locked = (bval == TRUE);

	} else if (g_str_equal (property, "Created")) {
		if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_INT64)
			return FALSE;
		dbus_message_iter_get_basic (iter, &i64val);
		info->ctime = (time_t)i64val;

	} else if (g_str_equal (property, "Modified")) {
		if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_INT64)
			return FALSE;
		dbus_message_iter_get_basic (iter, &i64val);
		info->ctime = (time_t)i64val;
	}

	return TRUE;
}

static void
get_keyring_info_sync (GnomeKeyringResult res, GnomeKeyringInfo *info, gpointer user_data)
{
	GnomeKeyringInfo **result = user_data;
	*result = info;
}

static void
get_keyring_info_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	GnomeKeyringResult res;
	GnomeKeyringInfo *info;
	GkrCallback *cb;

	if (gkr_operation_handle_errors (op, reply))
		return;

	info = g_new0 (GnomeKeyringInfo, 1);
	res = decode_property_dict (reply, get_keyring_info_foreach, info);
	if (res == GNOME_KEYRING_RESULT_OK) {
		cb = gkr_operation_pop (op);
		gkr_callback_invoke_ok_keyring_info (cb, info);
		if (cb->callback == get_keyring_info_sync)
			info = NULL;
	} else {
		gkr_operation_complete (op, res);
	}

	gnome_keyring_info_free (info);
}

static GkrOperation*
get_keyring_info_start (const char *keyring, GnomeKeyringOperationGetKeyringInfoCallback callback,
                        gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	g_return_val_if_fail (callback, NULL);

	path = gkr_encode_keyring_name (keyring);
	req = prepare_property_getall (path, COLLECTION_INTERFACE);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_KEYRING_INFO, data, destroy_data);
	gkr_operation_push (op, get_keyring_info_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
	g_free (path);

	return op;
}

/**
 * gnome_keyring_get_info:
 * @keyring: The name of the keyring, or %NULL for the default keyring.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Get information about the @keyring. The resulting #GnomeKeyringInfo structure
 * will be passed to @callback. The structure is freed after @callback returns.
 *
 * For a synchronous version of this function see gnome_keyring_get_info_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_get_info (const char                                  *keyring,
                        GnomeKeyringOperationGetKeyringInfoCallback  callback,
                        gpointer                                     data,
                        GDestroyNotify                               destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = get_keyring_info_start (keyring, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_get_info_sync:
 * @keyring: The name of the keyring, or %NULL for the default keyring.
 * @info: Location for the information about the keyring to be returned.
 *
 * Get information about @keyring.
 *
 * The #GnomeKeyringInfo structure returned in @info must be freed with
 * gnome_keyring_info_free().
 *
 * For an asynchronous version of this function see gnome_keyring_get_info().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_get_info_sync (const char        *keyring,
                             GnomeKeyringInfo **info)
{
	GkrOperation *op;

	g_return_val_if_fail (info, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	op = get_keyring_info_start (keyring, get_keyring_info_sync, info, NULL);
	return gkr_operation_block_and_unref (op);
}

static GkrOperation*
set_keyring_info_start (const char *keyring, GnomeKeyringInfo *info,
                        GnomeKeyringOperationDoneCallback callback,
                        gpointer data, GDestroyNotify destroy_data)
{
	GkrOperation *op;
	gchar *path;

	g_return_val_if_fail (info, NULL);
	g_return_val_if_fail (callback, NULL);

	path = gkr_encode_keyring_name (keyring);

	/*
	 * TODO: Currently nothing to do. lock_on_idle and lock_timeout are not
	 * implemented in the DBus API. They were never used by the old
	 * gnome-keyring-daemon either.
	 */

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_OK);

	g_free (path);
	return op;
}

/**
 * gnome_keyring_set_info:
 * @keyring: The name of the keyring, or %NULL for the default keyring.
 * @info: A structure containing flags and info for the keyring.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Set flags and info for the @keyring. The only fields in @info that are used
 * are lock_on_idle and lock_timeout.
 *
 * For a synchronous version of this function see gnome_keyring_set_info_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_set_info (const char                                  *keyring,
                        GnomeKeyringInfo                            *info,
                        GnomeKeyringOperationDoneCallback            callback,
                        gpointer                                     data,
                        GDestroyNotify                               destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = set_keyring_info_start (keyring, info, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_set_info_sync:
 * @keyring: The name of the keyring, or %NULL for the default keyring.
 * @info: A structure containing flags and info for the keyring.
 *
 * Set flags and info for @keyring. The only fields in @info that are used
 * are lock_on_idle and lock_timeout.
 *
 * For an asynchronous version of this function see gnome_keyring_set_info().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_set_info_sync (const char       *keyring,
                             GnomeKeyringInfo *info)
{
	gchar *path;

	g_return_val_if_fail (info, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	path = gkr_encode_keyring_name (keyring);

	/*
	 * TODO: Currently nothing to do. lock_on_idle and lock_timeout are not
	 * implemented in the DBus API. They were never used by the old
	 * gnome-keyring-daemon either.
	 */

	g_free (path);
	return GNOME_KEYRING_RESULT_OK;
}

static void
list_item_ids_sync (GnomeKeyringResult res, GList *ids, gpointer user_data)
{
	GList **result = user_data;
	*result = ids;
}

static gboolean
list_item_ids_foreach (DBusMessageIter *iter, gpointer data)
{
	GList **ids = data;
	const char *path;
	guint32 id;

	if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_OBJECT_PATH)
		return FALSE;

	/* The object path, gets converted into a name */
	dbus_message_iter_get_basic (iter, &path);
	if (gkr_decode_item_id (path, &id))
		*ids = g_list_prepend (*ids, GUINT_TO_POINTER (id));
	else
		g_message ("unsupported item. identifier is not an integer: %s", path);

	return TRUE;
}

static void
list_item_ids_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	GnomeKeyringResult res;
	GList *ids = NULL;
	GkrCallback *cb;

	if (gkr_operation_handle_errors (op, reply))
		return;

	res = decode_property_variant_array (reply, list_item_ids_foreach, &ids);
	if (res == GNOME_KEYRING_RESULT_OK) {
		cb = gkr_operation_pop (op);
		gkr_callback_invoke_ok_list (cb, ids);
		if (cb->callback == list_item_ids_sync)
			ids = NULL;
	} else {
		gkr_operation_complete (op, res);
	}

	g_list_free (ids);
}

static GkrOperation*
list_item_ids_start (const char *keyring, GnomeKeyringOperationGetListCallback callback,
                     gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	g_return_val_if_fail (callback, NULL);

	path = gkr_encode_keyring_name (keyring);
	req = prepare_property_get (path, COLLECTION_INTERFACE, "Items");

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_LIST, data, destroy_data);
	gkr_operation_push (op, list_item_ids_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_request (op, req);

	dbus_message_unref (req);
	g_free (path);

	return op;
}

/**
 * gnome_keyring_list_item_ids:
 * @keyring: The name of the keyring, or %NULL for the default keyring.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Get a list of all the ids for items in @keyring. These are passed in a %GList
 * to the @callback. Use GPOINTER_TO_UINT() on the list to access the integer ids.
 * The list is freed after @callback returns.
 *
 * All items that are not flagged as %GNOME_KEYRING_ITEM_APPLICATION_SECRET are
 * included in the list. This includes items that the calling application may not
 * (yet) have access to.
 *
 * For a synchronous version of this function see gnome_keyring_list_item_ids_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_list_item_ids (const char                                  *keyring,
                             GnomeKeyringOperationGetListCallback         callback,
                             gpointer                                     data,
                             GDestroyNotify                               destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = list_item_ids_start (keyring, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_list_item_ids_sync:
 * @keyring: The name of the keyring, or %NULL for the default keyring.
 * @ids: The location to store a %GList of item ids (ie: unsigned integers).
 *
 * Get a list of all the ids for items in @keyring.
 *
 * Use GPOINTER_TO_UINT() on the list to access the integer ids. The list
 * should be freed with g_list_free().
 *
 * For an asynchronous version of this function see gnome_keyring_list_item_ids().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_list_item_ids_sync (const char  *keyring,
                                  GList      **ids)
{
	GkrOperation *op;

	g_return_val_if_fail (ids, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	op = list_item_ids_start (keyring, list_item_ids_sync, ids, NULL);
	return gkr_operation_block_and_unref (op);
}

/**
 * SECTION:gnome-keyring-daemon
 * @title: Daemon Management Functions
 * @short_description: Functions used by session to run the Gnome Keyring Daemon.
 *
 * These functions are not used by most applications using Gnome Keyring.
 **/

/**
 * gnome_keyring_daemon_set_display_sync:
 * @display: Deprecated
 *
 * Deprecated. No longer supported, always fails.
 *
 * Return value: GNOME_KEYRING_RESULT_DENIED
 **/
GnomeKeyringResult
gnome_keyring_daemon_set_display_sync (const char *display)
{
	g_return_val_if_fail (display, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);
	return GNOME_KEYRING_RESULT_DENIED;
}

/**
 * gnome_keyring_daemon_prepare_environment_sync:
 *
 * Deprecated. No longer supported, call is ignored.
 *
 * Return value: GNOME_KEYRING_RESULT_OK
 **/
GnomeKeyringResult
gnome_keyring_daemon_prepare_environment_sync (void)
{
	return GNOME_KEYRING_RESULT_OK;
}

/**
 * SECTION:gnome-keyring-find
 * @title: Search Functionality
 * @short_description: Find Keyring Items
 *
 * A find operation searches through all keyrings for items that match the
 * attributes. The user may have been prompted to unlock necessary keyrings, and
 * user will have been prompted for access to the items if needed.
 *
 * A find operation may return multiple or zero results.
 **/

typedef struct _find_items_args {
	GList *found;
	GList *queued;
	GkrSession *session;
	GPtrArray *paths;
} find_items_args;

static void
find_items_free (gpointer data)
{
	find_items_args *args = data;
	guint i;

	gnome_keyring_found_list_free (args->queued);
	gnome_keyring_found_list_free (args->found);
	gkr_session_unref (args->session);
	for (i = 0; i < args->paths->len; ++i)
		g_free (g_ptr_array_index (args->paths, i));
	g_ptr_array_free (args->paths, TRUE);

	g_slice_free (find_items_args, args);
}

static void
find_items_sync (GnomeKeyringResult res, GList *found, gpointer user_data)
{
	GList **result = user_data;
	*result = found;
}

static gboolean
find_items_queue (const char *path, gpointer user_data)
{
	find_items_args *args = user_data;
	g_ptr_array_add (args->paths, g_strdup (path));
	return TRUE;
}

static gboolean
find_items_decode_secrets (DBusMessageIter *iter, find_items_args *args)
{
	DBusMessageIter array, dict;
	GnomeKeyringFound *found;
	const char *path;
	gchar *keyring;
	gchar *secret;
	guint32 item_id;
	int type;

	if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_ARRAY ||
	    dbus_message_iter_get_element_type (iter) != DBUS_TYPE_DICT_ENTRY)
		return FALSE;

	dbus_message_iter_recurse (iter, &array);

	for (;;) {
		type = dbus_message_iter_get_arg_type (&array);
		if (type == DBUS_TYPE_INVALID)
			break;
		else if (type != DBUS_TYPE_DICT_ENTRY)
			return FALSE;
		dbus_message_iter_recurse (&array, &dict);
		if (dbus_message_iter_get_arg_type (&dict) != DBUS_TYPE_OBJECT_PATH)
			return FALSE;

		/* The item path */
		dbus_message_iter_get_basic (&dict, &path);
		if (!dbus_message_iter_next (&dict))
			return FALSE;

		keyring = gkr_decode_keyring_item_id (path, &item_id);
		if (keyring == NULL)
			return FALSE;

		/* The secret */
		if (!gkr_session_decode_secret (args->session, &dict, &secret)) {
			g_free (keyring);
			return FALSE;
		}

		found = g_new0 (GnomeKeyringFound, 1);
		found->item_id = item_id;
		found->keyring = keyring;
		found->secret = secret;
		args->queued = g_list_prepend (args->queued, found);

		dbus_message_iter_next (&array);
	}

	return TRUE;
}

static void
find_items_6_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	GnomeKeyringFound *found;
	find_items_args *args = data;
	GnomeKeyringResult res;
	DBusMessage *req;
	gchar *path;
	GkrCallback *cb;

	if (reply != NULL) {

		/* At this point we have a response to GetProperty("Attributes") */

		if (gkr_operation_handle_errors (op, reply))
			return;

		found = args->queued->data;
		args->queued = g_list_remove (args->queued, args->queued->data);
		args->found = g_list_prepend (args->found, found);

		found->attributes = gnome_keyring_attribute_list_new ();
		res = decode_get_attributes (reply, found->attributes);
		if (res != GNOME_KEYRING_RESULT_OK) {
			gkr_operation_complete (op, res);
			return;
		}
	}

	/* Do we have any more items? */
	if (!args->queued) {
		if (args->found) {
			/* Back to the original order returned from daemon */
			args->found = g_list_reverse (args->found);

			cb = gkr_operation_pop (op);
			gkr_callback_invoke_ok_list (cb, args->found);
			if (cb->callback == find_items_sync)
				args->found = NULL;
		} else {
			gkr_operation_complete (op, GNOME_KEYRING_RESULT_NO_MATCH);
		}
		return;
	}

	/* Next item in the queue */
	found = args->queued->data;
	g_assert (found);

	/* Request the next set of attributes */
	path = gkr_encode_keyring_item_id (found->keyring, found->item_id);
	req = prepare_property_get (path, ITEM_INTERFACE, "Attributes");
	g_free (path);

	gkr_operation_push (op, find_items_6_reply, GKR_CALLBACK_OP_MSG, args, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static void
find_items_5_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	find_items_args *args = data;
	DBusMessageIter iter;

	/* At this point we got back all the secrets */

	if (gkr_operation_handle_errors (op, reply))
		return;

	/* Decode the unlocked secrets received */
	if (!dbus_message_iter_init (reply, &iter))
		g_return_if_reached ();
	if (!find_items_decode_secrets (&iter, args)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Start retrieving attributes */
	find_items_6_reply (op, NULL, args);
}

static void
find_items_4_reply (GkrOperation *op, GkrSession *session, gpointer data)
{
	find_items_args *args = data;
	DBusMessage *req;
	char **paths;
	int n_paths;

	/* At this point we have a session, and can get secrets */

	g_assert (!args->session);
	args->session = gkr_session_ref (session);

	paths = (char**)args->paths->pdata;
	n_paths = args->paths->len;

	/* Retrieve any unlocked secrets */
	req = prepare_get_secrets (session, paths, n_paths);

	gkr_operation_push (op, find_items_5_reply, GKR_CALLBACK_OP_MSG, args, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static void
find_items_3_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	find_items_args *args = data;
	gboolean dismissed;

	/* At this point Prompt has Completed, and should contain a list of unlocked items */

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!decode_xlock_completed (reply, &dismissed, find_items_queue, args)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Well we're going to be transferring secrets, so need a session */
	gkr_operation_push (op, find_items_4_reply, GKR_CALLBACK_OP_SESSION, args, NULL);
	gkr_session_negotiate (op);
}

static void
find_items_2_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	find_items_args *args = data;
	const char *prompt;
	char **unlocked;
	int n_unlocked, i;

	/* At this point Unlock has returned a list of unlocked items, plus prompt? */

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_get_args (reply, NULL, DBUS_TYPE_ARRAY, DBUS_TYPE_OBJECT_PATH, &unlocked, &n_unlocked,
	                            DBUS_TYPE_OBJECT_PATH, &prompt, DBUS_TYPE_INVALID)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* These are ready to retrieve */
	for (i = 0; i < n_unlocked; ++i)
		g_ptr_array_add (args->paths, g_strdup (unlocked[i]));

	/* Do we have a prompt to display? */
	if (prompt && !g_str_equal (prompt, "/")) {
		gkr_operation_push (op, find_items_3_reply, GKR_CALLBACK_OP_MSG, args, NULL);
		gkr_operation_prompt (op, prompt);

	/* Well we're going to be transferring secrets, so need a session */
	} else {
		gkr_operation_push (op, find_items_4_reply, GKR_CALLBACK_OP_SESSION, args, NULL);
		gkr_session_negotiate (op);
	}

	dbus_free_string_array (unlocked);
}

static void
find_items_1_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	find_items_args *args = data;
	char **unlocked, **locked;
	int n_unlocked, n_locked, i;
	DBusMessage *req;

	/* At this point SearchItems has returned two lists of locked/unlocked items */

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_get_args (reply, NULL,
	                            DBUS_TYPE_ARRAY, DBUS_TYPE_OBJECT_PATH, &unlocked, &n_unlocked,
	                            DBUS_TYPE_ARRAY, DBUS_TYPE_OBJECT_PATH, &locked, &n_locked,
	                            DBUS_TYPE_INVALID)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Did we find anything? */
	if (!n_unlocked && !n_locked) {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_NO_MATCH);
		dbus_free_string_array (locked);
		dbus_free_string_array (unlocked);
		return;
	}

	/* These are ready to retrieve */
	for (i = 0; i < n_unlocked; ++i)
		g_ptr_array_add (args->paths, g_strdup (unlocked[i]));

	/* Do we have any to unlock? */
	if (n_locked) {
		req = prepare_xlock ("Unlock", locked, n_locked);
		gkr_operation_push (op, find_items_2_reply, GKR_CALLBACK_OP_MSG, args, NULL);
		gkr_operation_request (op, req);

	/* Well we're going to be transferring secrets, so need a session */
	} else {
		gkr_operation_push (op, find_items_4_reply, GKR_CALLBACK_OP_SESSION, args, NULL);
		gkr_session_negotiate (op);
	}

	dbus_free_string_array (locked);
	dbus_free_string_array (unlocked);
}

static GkrOperation*
find_items_start (GnomeKeyringItemType type, GnomeKeyringAttributeList *attributes,
                  GnomeKeyringOperationGetListCallback callback,
                  gpointer data, GDestroyNotify destroy_data)
{
	DBusMessageIter iter;
	find_items_args *args;
	DBusMessage *req;
	GkrOperation *op;

	g_return_val_if_fail (attributes, NULL);
	g_return_val_if_fail (callback, NULL);

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, "SearchItems");

	/* Encode the attribute list */
	dbus_message_iter_init_append (req, &iter);
	encode_attribute_list (&iter, attributes);

	args = g_slice_new0 (find_items_args);
	args->paths = g_ptr_array_new ();

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_LIST, data, destroy_data);
	gkr_operation_push (op, find_items_1_reply, GKR_CALLBACK_OP_MSG, args, find_items_free);
	gkr_operation_request (op, req);

	dbus_message_unref (req);

	return op;
}

/**
 * gnome_keyring_find_items:
 * @type: The type of items to find.
 * @attributes: A list of attributes to search for. This cannot be an empty list.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Searches through all keyrings for items that match the @attributes. The matches
 * are for exact equality.
 *
 * A %GList of GnomeKeyringFound structures are passed to the @callback. The
 * list and structures are freed after the callback returns.
 *
 * The user may have been prompted to unlock necessary keyrings, and user will
 * have been prompted for access to the items if needed.
 *
 * For a synchronous version of this function see gnome_keyring_find_items_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_find_items  (GnomeKeyringItemType                  type,
                           GnomeKeyringAttributeList            *attributes,
                           GnomeKeyringOperationGetListCallback  callback,
                           gpointer                              data,
                           GDestroyNotify                        destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = find_items_start (type, attributes, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

static GnomeKeyringAttributeList *
make_attribute_list_va (va_list args)
{
	GnomeKeyringAttributeList *attributes;
	GnomeKeyringAttribute attribute;
	char *str;
	guint32 val;

	attributes = g_array_new (FALSE, FALSE, sizeof (GnomeKeyringAttribute));

	while ((attribute.name = va_arg (args, char *)) != NULL) {
		attribute.type = va_arg (args, GnomeKeyringAttributeType);

		switch (attribute.type) {
		case GNOME_KEYRING_ATTRIBUTE_TYPE_STRING:
			str = va_arg (args, char *);
			attribute.value.string = str;
			g_array_append_val (attributes, attribute);
			break;
		case GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32:
			val = va_arg (args, guint32);
			attribute.value.integer = val;
			g_array_append_val (attributes, attribute);
			break;
		default:
			g_array_free (attributes, TRUE);
			return NULL;
		}
	}
	return attributes;
}

/**
 * gnome_keyring_find_itemsv:
 * @type: The type of items to find.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 * @...: Attribute name, followed by the attribute type, and string or 32-bit unsigned int value. Terminated with NULL.
 *
 * Searches through all keyrings for items that match the specified attributes.
 * The matches are for exact equality.
 *
 * The variable argument list should contain a) The attribute name as a null
 * terminated string, followed by b) The attribute type, either
 * %GNOME_KEYRING_ATTRIBUTE_TYPE_STRING or %GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32
 * and then the c) attribute value, either a character string, or 32-bit
 * unsigned int. The list should be terminated with a NULL.
 *
 * A %GList of GnomeKeyringFound structures are passed to the @callback. The
 * list and structures are freed after the callback returns.
 *
 * The user may have been prompted to unlock necessary keyrings, and user will
 * have been prompted for access to the items if needed.
 *
 * For a synchronous version of this function see gnome_keyring_find_itemsv_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_find_itemsv (GnomeKeyringItemType                  type,
                           GnomeKeyringOperationGetListCallback  callback,
                           gpointer                              data,
                           GDestroyNotify                        destroy_data,
                           ...)
{
	GnomeKeyringAttributeList *attributes;
	va_list args;
	gpointer ret;

	gkr_init ();

	va_start (args, destroy_data);
	attributes = make_attribute_list_va (args);
	va_end (args);

	ret = gnome_keyring_find_items (type, attributes, callback, data, destroy_data);
	g_array_free (attributes, TRUE);
	return ret;
}

/**
 * gnome_keyring_find_items_sync:
 * @type: The type of items to find.
 * @attributes: A list of attributes to search for. This cannot be an empty list.
 * @found: The location to return a list of #GnomeKeyringFound pointers.
 *
 * Searches through all keyrings for items that match the @attributes and @type.
 * The matches are for exact equality.
 *
 * A %GList of GnomeKeyringFound structures is returned in @found. The list may
 * have zero items if nothing matched the criteria. The list should be freed
 * using gnome_keyring_found_list_free().
 *
 * The user may have been prompted to unlock necessary keyrings, and user will
 * have been prompted for access to the items if needed.
 *
 * For an asynchronous version of this function see gnome_keyring_find_items().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_find_items_sync (GnomeKeyringItemType        type,
                               GnomeKeyringAttributeList  *attributes,
                               GList                     **found)
{
	GkrOperation *op;

	gkr_init ();

	op = find_items_start (type, attributes, find_items_sync, found, NULL);
	return gkr_operation_block_and_unref (op);
}

/**
 * gnome_keyring_find_itemsv_sync:
 * @type: The type of items to find.
 * @found: The location to return a list of #GnomeKeyringFound pointers.
 * @...: Attribute name, followed by the attribute type, and string or 32-bit unsigned int value. Terminated with NULL.
 *
 * Searches through all keyrings for items that match the @attributes and @type.
 * The matches are for exact equality.
 *
 * The variable argument list should contain a) The attribute name as a null
 * terminated string, followed by b) The attribute type, either
 * %GNOME_KEYRING_ATTRIBUTE_TYPE_STRING or %GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32
 * and then the c) attribute value, either a character string, or 32-bit
 * unsigned int. The list should be terminated with a NULL.
 *
 * A %GList of GnomeKeyringFound structures is returned in @found. The list may
 * have zero items if nothing matched the criteria. The list should be freed
 * using gnome_keyring_found_list_free().
 *
 * The user may have been prompted to unlock necessary keyrings, and user will
 * have been prompted for access to the items if needed.
 *
 * For an asynchronous version of this function see gnome_keyring_find_items().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_find_itemsv_sync  (GnomeKeyringItemType        type,
                                 GList                     **found,
                                 ...)
{
	GnomeKeyringAttributeList *attributes;
	va_list args;
	GnomeKeyringResult ret;

	g_return_val_if_fail (found, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	va_start (args, found);
	attributes = make_attribute_list_va (args);
	va_end (args);

	ret = gnome_keyring_find_items_sync (type, attributes, found);
	g_array_free (attributes, TRUE);
	return ret;
}

/**
 * SECTION:gnome-keyring-items
 * @title: Keyring Items
 * @short_description: Keyring items each hold a secret and a number of attributes.
 *
 * A keyring contains multiple items. Each item has a secret, attributes and access
 * information associated with it.
 *
 * An item is identified by an unsigned integer unique to the keyring in which it
 * exists. An item's name is for displaying to the user. Each item has a single secret,
 * which is a null-terminated string. This secret is stored in non-pageable memory, and
 * encrypted on disk. All of this information is exposed via #GnomeKeyringItemInfo
 * pointers.
 *
 * Attributes allow various other pieces of information to be associated with an item.
 * These can also be used to search for relevant items. Attributes are accessed with
 * #GnomeKeyringAttribute structures and built into lists using #GnomeKeyringAttributeList.
 **/

typedef struct _item_create_args {
	DBusMessage *request;
	DBusMessageIter iter;
	gboolean is_default;
	gboolean update_if_exists;
	gchar *secret;
} item_create_args;

static void
item_create_free (gpointer data)
{
	item_create_args *args = data;
	dbus_message_unref (args->request);
	egg_secure_strfree (args->secret);
	g_slice_free (item_create_args, args);
}

static void
item_create_sync (GnomeKeyringResult res, guint32 item_id, gpointer data)
{
	guint32 *result = data;
	*result = item_id;
}

static const gchar *
item_type_to_string (GnomeKeyringItemType item_type)
{
	switch (item_type) {
	case GNOME_KEYRING_ITEM_GENERIC_SECRET:
		return "org.freedesktop.Secret.Generic";
	case GNOME_KEYRING_ITEM_NETWORK_PASSWORD:
		return "org.gnome.keyring.NetworkPassword";
	case GNOME_KEYRING_ITEM_NOTE:
		return "org.gnome.keyring.Note";
	case GNOME_KEYRING_ITEM_CHAINED_KEYRING_PASSWORD:
		return "org.gnome.keyring.ChainedKeyring";
	case GNOME_KEYRING_ITEM_ENCRYPTION_KEY_PASSWORD:
		return "org.gnome.keyring.EncryptionKey";
	case GNOME_KEYRING_ITEM_PK_STORAGE:
		return "org.gnome.keyring.PkStorage";
	default:
		return "org.freedesktop.Secret.Generic";
	}
}

static DBusMessage*
item_create_prepare (const gchar *path, GnomeKeyringItemType type, const gchar *label,
                     GnomeKeyringAttributeList *attrs, DBusMessageIter *iter)
{
	DBusMessageIter array, variant, dict;
	DBusMessage *req;
	const char *string;
	const gchar *type_string;

	req = dbus_message_new_method_call (gkr_service_name (), path,
	                                    COLLECTION_INTERFACE, "CreateItem");

	dbus_message_iter_init_append (req, iter);
	dbus_message_iter_open_container (iter, DBUS_TYPE_ARRAY, "{sv}", &array);

	/* Set the label */
	string = ITEM_INTERFACE ".Label";
	dbus_message_iter_open_container (&array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);
	dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);
	dbus_message_iter_open_container (&dict, DBUS_TYPE_VARIANT, "s", &variant);
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_STRING, &label);
	dbus_message_iter_close_container (&dict, &variant);
	dbus_message_iter_close_container (&array, &dict);

	/* Set the attributes */
	string = ITEM_INTERFACE ".Attributes";
	dbus_message_iter_open_container (&array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);
	dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);
	dbus_message_iter_open_container (&dict, DBUS_TYPE_VARIANT, "a{ss}", &variant);
	encode_attribute_list (&variant, attrs);
	dbus_message_iter_close_container (&dict, &variant);
	dbus_message_iter_close_container (&array, &dict);

	/* Set the item type */
	string = ITEM_INTERFACE ".Type";
	type_string = item_type_to_string (type);
	dbus_message_iter_open_container (&array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);
	dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);
	dbus_message_iter_open_container (&dict, DBUS_TYPE_VARIANT, "s", &variant);
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_STRING, &type_string);
	dbus_message_iter_close_container (&dict, &variant);
	dbus_message_iter_close_container (&array, &dict);

	dbus_message_iter_close_container (iter, &array);
	return req;
}

static void
item_create_3_created_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	/* Called after trying to create item */

	const char *path;
	const char *prompt;
	guint32 id;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_get_args (reply, NULL, DBUS_TYPE_OBJECT_PATH, &path,
	                            DBUS_TYPE_OBJECT_PATH, &prompt, DBUS_TYPE_INVALID)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	if (!gkr_decode_item_id (path, &id)) {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		return;
	}

	gkr_callback_invoke_ok_uint (gkr_operation_pop (op), id);
}

static void
item_create_2_session_reply (GkrOperation *op, GkrSession *session, gpointer data)
{
	/* Called after we have a session, start creating item */

	item_create_args *args = data;
	dbus_bool_t replace;

	if (!gkr_session_encode_secret (session, &args->iter, args->secret)) {
		gkr_operation_complete (op, BROKEN);
		g_return_if_reached ();
	}

	replace = args->update_if_exists;
	dbus_message_iter_append_basic (&args->iter, DBUS_TYPE_BOOLEAN, &replace);

	gkr_operation_push (op, item_create_3_created_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_set_keyring_hint (op);
	gkr_operation_request (op, args->request);
}

static void
item_create_2_session_request (GkrOperation *op, gpointer data)
{
	/* Called to get us a valid session */

	gkr_operation_push (op, item_create_2_session_reply, GKR_CALLBACK_OP_SESSION, data, NULL);
	gkr_session_negotiate (op);
}

static void
item_create_1_create_prompt_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	/* Called after prompting to create default collection for item */

	DBusMessageIter iter, variant;
	const char *path;
	char *signature;
	gboolean equal;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_has_signature (reply, "bv")) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Skip over dismissed, already parsed */
	if (!dbus_message_iter_init (reply, &iter) ||
	    !dbus_message_iter_next (&iter))
		g_return_if_reached ();

	/* Dig out the variant */
	dbus_message_iter_recurse (&iter, &variant);

	signature = dbus_message_iter_get_signature (&variant);
	equal = g_str_equal (signature, "o");
	dbus_free (signature);
	if (!equal) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	g_return_if_fail (dbus_message_iter_get_arg_type (&variant) == DBUS_TYPE_OBJECT_PATH);
	dbus_message_iter_get_basic (&variant, &path);

	/* Start the session */
	item_create_2_session_request (op, data);
}

static void
item_create_1_collection_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	/* Called after trying to create default collection to create item in */

	const char *collection;
	const char *prompt;

	if (gkr_operation_handle_errors (op, reply))
		return;

	/* Parse the response */
	if (!dbus_message_get_args (reply, NULL, DBUS_TYPE_OBJECT_PATH, &collection,
	                            DBUS_TYPE_OBJECT_PATH, &prompt, DBUS_TYPE_INVALID)) {
		g_warning ("bad response to CreateCollection from service");
		gkr_callback_invoke_res (gkr_operation_pop (op), GNOME_KEYRING_RESULT_IO_ERROR);
		return;
	}

	/* No prompt, set keyring as default */
	g_return_if_fail (prompt);
	if (g_str_equal (prompt, "/")) {
		item_create_2_session_request (op, data);

	/* A prompt, display it get the response */
	} else {
		gkr_operation_push (op, item_create_1_create_prompt_reply, GKR_CALLBACK_OP_MSG, data, NULL);
		gkr_operation_prompt (op, prompt);
	}
}

static gboolean
item_create_check_unlock (const char *path, gpointer user_data)
{
	gboolean *unlocked = user_data;
	*unlocked = TRUE;
	return FALSE;
}

static void
item_create_1_unlock_prompt_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	/* Called after unlocking the collection we're going to create item in */
	gboolean dismissed = FALSE;
	gboolean unlocked = FALSE;

	if (!decode_xlock_completed (reply, &dismissed, item_create_check_unlock, &unlocked)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	if (dismissed || !unlocked) {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_DENIED);
		return;
	}

	/* Now that its unlocked, we need a session to transfer the secret */
	item_create_2_session_request (op, data);
}

static void
item_create_1_unlock_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	/* Called after trying to unlock keyring we're going to create item in */

	item_create_args *args = data;
	DBusMessageIter iter;
	DBusMessage *req;
	gboolean unlocked = FALSE;
	const char *prompt;
	const char *alias = "default";

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!decode_xlock_reply (reply, &prompt, item_create_check_unlock, &unlocked)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Prompt to unlock the collection */
	if (!g_str_equal (prompt, "/")) {
		gkr_operation_push (op, item_create_1_unlock_prompt_reply, GKR_CALLBACK_OP_MSG, args, NULL);
		gkr_operation_prompt (op, prompt);

	/* No such keyring, no prompt, and not unlocked */
	} else if (!unlocked) {

		/* Caller asked for default keyring, and there is no such keyring. Create */
		if (args->is_default) {
			req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
							    SERVICE_INTERFACE, "CreateCollection");
			dbus_message_iter_init_append (req, &iter);
			/* TRANSLATORS: This is the name of an automatically created default keyring. */
			create_keyring_encode_properties (&iter, _("Default"));
			dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &alias);
			gkr_operation_push (op, item_create_1_collection_reply, GKR_CALLBACK_OP_MSG, args, NULL);
			gkr_operation_request (op, req);
			dbus_message_unref (req);

		/* No such keyring, error */
		} else {
			gkr_operation_complete (op, GNOME_KEYRING_RESULT_NO_SUCH_KEYRING);
		}

	/* Successfully unlocked, or not locked. We need a session to transfer the secret */
	} else {
		item_create_2_session_request (op, args);
	}
}

static GkrOperation*
item_create_start (const char *keyring, GnomeKeyringItemType type, const char *display_name,
                   GnomeKeyringAttributeList *attributes, const char *secret,
                   gboolean update_if_exists, GnomeKeyringOperationGetIntCallback callback,
                   gpointer data, GDestroyNotify destroy_data)
{
	item_create_args *args;
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	if (!display_name)
		display_name = "";

	args = g_slice_new0 (item_create_args);
	args->update_if_exists = update_if_exists;
	args->secret = egg_secure_strdup (secret);
	args->is_default = (keyring == NULL);

	path = gkr_encode_keyring_name (keyring);
	args->request = item_create_prepare (path, type, display_name, attributes, &args->iter);
	g_return_val_if_fail (args->request, NULL);

	/* First unlock the keyring */
	req = prepare_xlock ("Unlock", &path, 1);
	g_free (path);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_UINT, data, destroy_data);
	gkr_operation_push (op, item_create_1_unlock_reply, GKR_CALLBACK_OP_MSG, args, item_create_free);
	gkr_operation_set_keyring_hint (op);
	gkr_operation_request (op, req);
	dbus_message_unref (req);

	return op;
}

/**
 * gnome_keyring_item_create:
 * @keyring: The name of the keyring in which to create the item, or NULL for the default keyring.
 * @type: The item type.
 * @display_name: The name of the item. This will be displayed to the user where necessary.
 * @attributes: A (possibly empty) list of attributes to store with the item.
 * @secret: The password or secret of the item.
 * @update_if_exists: If true, then another item matching the type, and attributes
 *  will be updated instead of creating a new item.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Create a new item in a keyring.
 *
 * The @secret must be a null terminated string. It should be allocated using secure
 * memory whenever possible. See gnome_keyring_memory_strdup()
 *
 * The user may have been prompted to unlock necessary keyrings. If %NULL is
 * specified as the @keyring and no default keyring exists, the user will be
 * prompted to create a new keyring.
 *
 * When @update_if_exists is set to %TRUE, the user may be prompted for access
 * to the previously existing item.
 *
 * Whether a new item is created or not, id of the item will be passed to
 * the @callback.
 *
 * For a synchronous version of this function see gnome_keyring_item_create_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_item_create (const char                          *keyring,
                           GnomeKeyringItemType                 type,
                           const char                          *display_name,
                           GnomeKeyringAttributeList           *attributes,
                           const char                          *secret,
                           gboolean                             update_if_exists,
                           GnomeKeyringOperationGetIntCallback  callback,
                           gpointer                             data,
                           GDestroyNotify                       destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = item_create_start (keyring, type, display_name, attributes, secret,
	                        update_if_exists, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_create_sync:
 * @keyring: The name of the keyring in which to create the item, or NULL for the default keyring.
 * @type: The item type.
 * @display_name: The name of the item. This will be displayed to the user where necessary.
 * @attributes: A (possibly empty) list of attributes to store with the item.
 * @secret: The password or secret of the item.
 * @update_if_exists: If true, then another item matching the type, and attributes
 *  will be updated instead of creating a new item.
 * @item_id: return location for the id of the created/updated keyring item.
 *
 * Create a new item in a keyring.
 *
 * The @secret must be a null terminated string. It should be allocated using secure
 * memory whenever possible. See gnome_keyring_memory_strdup()
 *
 * The user may have been prompted to unlock necessary keyrings. If %NULL is
 * specified as the @keyring and no default keyring exists, the user will be
 * prompted to create a new keyring.
 *
 * When @update_if_exists is set to %TRUE, the user may be prompted for access
 * to the previously existing item.
 *
 * For an asynchronous version of this function see gnome_keyring_item_create().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 */
GnomeKeyringResult
gnome_keyring_item_create_sync (const char                                 *keyring,
                                GnomeKeyringItemType                        type,
                                const char                                 *display_name,
                                GnomeKeyringAttributeList                  *attributes,
                                const char                                 *secret,
                                gboolean                                    update_if_exists,
                                guint32                                    *item_id)
{
	GkrOperation *op;

	gkr_init ();

	op = item_create_start (keyring, type, display_name, attributes, secret,
	                        update_if_exists, item_create_sync, item_id, NULL);
	return gkr_operation_block_and_unref (op);
}

static GkrOperation*
item_delete_start (const char *keyring, guint32 id, GnomeKeyringOperationDoneCallback callback,
                   gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	path = gkr_encode_keyring_item_id (keyring, id);
	req = dbus_message_new_method_call (gkr_service_name (), path,
	                                    ITEM_INTERFACE, "Delete");

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_request (op, req);
	dbus_message_unref (req);

	return op;
}

/**
 * gnome_keyring_item_delete:
 * @keyring: The name of the keyring from which to delete the item, or NULL for the default keyring.
 * @id: The id of the item
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Delete an item in a keyring.
 *
 * The user may be prompted if the calling application doesn't have necessary
 * access to delete the item.
 *
 * For an asynchronous version of this function see gnome_keyring_delete().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_item_delete (const char                                 *keyring,
                           guint32                                     id,
                           GnomeKeyringOperationDoneCallback           callback,
                           gpointer                                    data,
                           GDestroyNotify                              destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = item_delete_start (keyring, id, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_delete_sync:
 * @keyring: The name of the keyring from which to delete the item, or NULL for the default keyring.
 * @id: The id of the item
 *
 * Delete an item in a keyring.
 *
 * The user may be prompted if the calling application doesn't have necessary
 * access to delete the item.
 *
 * For an asynchronous version of this function see gnome_keyring_item_delete().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_item_delete_sync (const char *keyring,
                                guint32     id)
{
	GkrOperation *op;

	gkr_init ();

	op = item_delete_start (keyring, id, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

/**
 * gnome_keyring_item_get_info:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Get information about an item and its secret.
 *
 * The user may be prompted if the calling application doesn't have necessary
 * access to read the item with its secret.
 *
 * A #GnomeKeyringItemInfo structure will be passed to the @callback. This structure
 * will be freed after @callback returns.
 *
 * For a synchronous version of this function see gnome_keyring_item_get_info_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_item_get_info (const char                                 *keyring,
                             guint32                                     id,
                             GnomeKeyringOperationGetItemInfoCallback    callback,
                             gpointer                                    data,
                             GDestroyNotify                              destroy_data)
{
	gkr_init ();

	return gnome_keyring_item_get_info_full (keyring, id, GNOME_KEYRING_ITEM_INFO_ALL,
	                                         callback, data, destroy_data);
}

/**
 * gnome_keyring_item_get_info_sync:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @info: The location to return a #GnomeKeyringItemInfo pointer.
 *
 * Get information about an item and its secret.
 *
 * The user may be prompted if the calling application doesn't have necessary
 * access to read the item with its secret.
 *
 * A #GnomeKeyringItemInfo structure will be returned in @info. This must be
 * freed using gnome_keyring_item_info_free().
 *
 * For an asynchronous version of this function see gnome_keyring_item_get_info().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_item_get_info_sync (const char            *keyring,
                                  guint32                id,
                                  GnomeKeyringItemInfo **info)
{
	gkr_init ();

	return gnome_keyring_item_get_info_full_sync (keyring, id, GNOME_KEYRING_ITEM_INFO_ALL, info);
}

static gboolean
item_get_info_foreach (const gchar *property, DBusMessageIter *iter, gpointer user_data)
{
	GnomeKeyringItemInfo *info = user_data;
	const char *sval;
	dbus_int64_t i64val;

	if (g_str_equal (property, "Label")) {
		if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_STRING)
			return FALSE;
		dbus_message_iter_get_basic (iter, &sval);
		info->display_name = g_strdup (sval);

	} else if (g_str_equal (property, "Created")) {
		if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_INT64)
			return FALSE;
		dbus_message_iter_get_basic (iter, &i64val);
		info->ctime = (time_t)i64val;

	} else if (g_str_equal (property, "Modified")) {
		if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_INT64)
			return FALSE;
		dbus_message_iter_get_basic (iter, &i64val);
		info->mtime = (time_t)i64val;

	} else if (g_str_equal (property, "Type")) {
		if (dbus_message_iter_get_arg_type (iter) != DBUS_TYPE_STRING)
			return FALSE;
		dbus_message_iter_get_basic (iter, &sval);
		g_return_val_if_fail (sval, FALSE);
		if (g_str_equal (sval, "org.freedesktop.Secret.Generic"))
			info->type = GNOME_KEYRING_ITEM_GENERIC_SECRET;
		else if (g_str_equal (sval, "org.gnome.keyring.NetworkPassword"))
			info->type = GNOME_KEYRING_ITEM_NETWORK_PASSWORD;
		else if (g_str_equal (sval, "org.gnome.keyring.Note"))
			info->type = GNOME_KEYRING_ITEM_NOTE;
		else if (g_str_equal (sval, "org.gnome.keyring.ChainedKeyring"))
			info->type = GNOME_KEYRING_ITEM_CHAINED_KEYRING_PASSWORD;
		else if (g_str_equal (sval, "org.gnome.keyring.EncryptionKey"))
			info->type = GNOME_KEYRING_ITEM_ENCRYPTION_KEY_PASSWORD;
		else if (g_str_equal (sval, "org.gnome.keyring.PkStorage"))
			info->type = GNOME_KEYRING_ITEM_PK_STORAGE;
		else
			info->type = GNOME_KEYRING_ITEM_GENERIC_SECRET;
	}

	return TRUE;
}

typedef struct _item_get_info_args {
	gchar *path;
	guint32 flags;
	GkrSession *session;
	GnomeKeyringItemInfo *info;
} item_get_info_args;

static void
item_get_info_free (gpointer data)
{
	item_get_info_args *args = data;
	g_assert (data);
	g_free (args->path);
	if (args->session)
		gkr_session_unref (args->session);
	gnome_keyring_item_info_free (args->info);
	g_slice_free (item_get_info_args, args);
}

static void
item_get_info_sync (GnomeKeyringResult res, GnomeKeyringItemInfo *info, gpointer user_data)
{
	GnomeKeyringItemInfo **result = user_data;
	*result = info;
}

static void
item_get_info_3_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	item_get_info_args *args = data;
	DBusMessageIter iter;
	GkrCallback *cb;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_iter_init (reply, &iter))
		g_return_if_reached ();

	g_assert (args->info);
	g_assert (args->session);
	g_assert (!args->info->secret);

	if (gkr_session_decode_secret (args->session, &iter, &args->info->secret)) {
		cb = gkr_operation_pop (op);
		gkr_callback_invoke_ok_item_info (cb, args->info);
		if (cb->callback == item_get_info_sync)
			args->info = NULL;
	} else {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
	}
}

static void
item_get_info_2_reply (GkrOperation *op, GkrSession *session, gpointer data)
{
	item_get_info_args *args = data;
	const char *path;
	DBusMessage *req;

	g_assert (!args->session);
	args->session = gkr_session_ref (session);

	req = dbus_message_new_method_call (gkr_service_name (), args->path, ITEM_INTERFACE, "GetSecret");
	path = gkr_session_get_path (session);
	dbus_message_append_args (req, DBUS_TYPE_OBJECT_PATH, &path, DBUS_TYPE_INVALID);

	gkr_operation_push (op, item_get_info_3_reply, GKR_CALLBACK_OP_MSG, args, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static void
item_get_info_1_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	item_get_info_args *args = data;
	GnomeKeyringResult res;
	GkrCallback *cb;

	if (gkr_operation_handle_errors (op, reply))
		return;

	g_assert (args->info);
	res = decode_property_dict (reply, item_get_info_foreach, args->info);
	if (res != GNOME_KEYRING_RESULT_OK) {
		gkr_operation_complete (op, res);
		return;
	}

	/* Need to request the secret as well? */
	if (args->flags & GNOME_KEYRING_ITEM_INFO_SECRET) {
		gkr_operation_push (op, item_get_info_2_reply, GKR_CALLBACK_OP_SESSION, args, NULL);
		gkr_session_negotiate (op);

	/* No secret needed, all done */
	} else {
		cb = gkr_operation_pop (op);
		gkr_callback_invoke_ok_item_info (cb, args->info);
		if (cb->callback == item_get_info_sync)
			args->info = NULL;
	}
}

static GkrOperation*
item_get_info_start (const char *keyring, guint32 id, guint32 flags,
                     GnomeKeyringOperationGetItemInfoCallback callback,
                     gpointer data, GDestroyNotify destroy_data)
{
	item_get_info_args *args;
	DBusMessage *req;
	GkrOperation *op;

	args = g_slice_new0 (item_get_info_args);
	args->info = g_new0 (GnomeKeyringItemInfo, 1);
	args->flags = flags;

	args->path = gkr_encode_keyring_item_id (keyring, id);
	req = prepare_property_getall (args->path, ITEM_INTERFACE);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_ITEM_INFO, data, destroy_data);
	gkr_operation_push (op, item_get_info_1_reply, GKR_CALLBACK_OP_MSG, args, item_get_info_free);
	gkr_operation_request (op, req);

	dbus_message_unref (req);
	return op;
}

/**
 * gnome_keyring_item_get_info_full:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @flags: The parts of the item to retrieve.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Get information about an item, optionally retrieving its secret.
 *
 * If @flags includes %GNOME_KEYRING_ITEM_INFO_SECRET then the user may be
 * prompted if the calling application doesn't have necessary access to read
 * the item with its secret.
 *
 * A #GnomeKeyringItemInfo pointer will be passed to the @callback. Certain fields
 * of this structure may be NULL or zero if they were not specified in @flags. This
 * structure will be freed after @callback returns.
 *
 * For a synchronous version of this function see gnome_keyring_item_get_info_full_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_item_get_info_full (const char                                 *keyring,
                                  guint32                                     id,
                                  guint32                                     flags,
                                  GnomeKeyringOperationGetItemInfoCallback    callback,
                                  gpointer                                    data,
                                  GDestroyNotify                              destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = item_get_info_start (keyring, id, flags, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_get_info_full_sync:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @flags: The parts of the item to retrieve.
 * @info: The location to return a #GnomeKeyringItemInfo pointer.
 *
 * Get information about an item, optionally retrieving its secret.
 *
 * If @flags includes %GNOME_KEYRING_ITEM_INFO_SECRET then the user may be
 * prompted if the calling application doesn't have necessary access to read
 * the item with its secret.
 *
 * A #GnomeKeyringItemInfo structure will be returned in @info. Certain fields
 * of this structure may be NULL or zero if they were not specified in @flags.
 * This must be freed using gnome_keyring_item_info_free().
 *
 * For an asynchronous version of this function see gnome_keyring_item_get_info_full().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_item_get_info_full_sync (const char              *keyring,
                                       guint32                  id,
                                       guint32                  flags,
                                       GnomeKeyringItemInfo   **info)
{
	GkrOperation *op;

	gkr_init ();

	op = item_get_info_start (keyring, id, flags, item_get_info_sync, info, NULL);
	return gkr_operation_block_and_unref (op);
}

typedef struct _item_set_info_args {
	gchar *path;
	GkrSession *session;
	GnomeKeyringItemInfo *info;
} item_set_info_args;

static void
item_set_info_free (gpointer data)
{
	item_set_info_args *args = data;
	g_assert (data);
	g_free (args->path);
	if (args->session)
		gkr_session_unref (args->session);
	gnome_keyring_item_info_free (args->info);
	g_slice_free (item_set_info_args, args);
}

static void
item_set_info_3_reply (GkrOperation *op, GkrSession *session, gpointer user_data)
{
	item_set_info_args *args = user_data;
	DBusMessageIter iter;
	DBusMessage *req;

	g_assert (args);
	g_assert (args->info);
	g_assert (args->info->secret);

	/* Sending a secret */
	req = dbus_message_new_method_call (gkr_service_name (), args->path,
	                                    ITEM_INTERFACE, "SetSecret");

	dbus_message_iter_init_append (req, &iter);
	if (!gkr_session_encode_secret (session, &iter, args->info->secret)) {
		dbus_message_unref (req);
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
		return;
	}

	/* Calls the final result handler directly */
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static void
item_set_info_2_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	item_set_info_args *args = user_data;

	if (gkr_operation_handle_errors (op, reply))
		return;

	/* Need a session to send a secret */
	if (args->info->secret) {
		gkr_operation_push (op, item_set_info_3_reply, GKR_CALLBACK_OP_SESSION, args, NULL);
		gkr_session_negotiate (op);

	/* No secret? all done */
	} else {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_OK);
	}
}

static void
item_set_info_1_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	item_set_info_args *args = user_data;
	DBusMessageIter iter, variant;
	DBusMessage *req;
	const char *string;

	if (gkr_operation_handle_errors (op, reply))
		return;

	/* Next set the type */
	req = dbus_message_new_method_call (gkr_service_name (), args->path,
	                                    DBUS_INTERFACE_PROPERTIES, "Set");

	dbus_message_iter_init_append (req, &iter);
	string = ITEM_INTERFACE;
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &string);
	string = "Type";
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &string);
	dbus_message_iter_open_container (&iter, DBUS_TYPE_VARIANT, "s", &variant);

	string = item_type_to_string (args->info->type);
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_STRING, &string);
	dbus_message_iter_close_container (&iter, &variant);

	gkr_operation_push (op, item_set_info_2_reply, GKR_CALLBACK_OP_MSG, args, NULL);
	gkr_operation_request (op, req);
}

static GkrOperation*
item_set_info_start (const char *keyring, guint32 id, GnomeKeyringItemInfo *info,
                     GnomeKeyringOperationDoneCallback callback,
                     gpointer data, GDestroyNotify destroy_data)
{
	item_set_info_args *args;
	DBusMessageIter iter, variant;
	DBusMessage *req;
	GkrOperation *op;
	const char *string;

	args = g_slice_new0 (item_set_info_args);
	args->info = gnome_keyring_item_info_copy (info);
	args->path = gkr_encode_keyring_item_id (keyring, id);

	/* First set the label */
	req = dbus_message_new_method_call (gkr_service_name (), args->path,
	                                    DBUS_INTERFACE_PROPERTIES, "Set");

	dbus_message_iter_init_append (req, &iter);
	string = ITEM_INTERFACE;
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &string);
	string = "Label";
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &string);
	dbus_message_iter_open_container (&iter, DBUS_TYPE_VARIANT, "s", &variant);
	string = args->info->display_name ? args->info->display_name : "";
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_STRING, &string);
	dbus_message_iter_close_container (&iter, &variant);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_push (op, item_set_info_1_reply, GKR_CALLBACK_OP_MSG, args, item_set_info_free);
	gkr_operation_request (op, req);
	dbus_message_unref (req);

	return op;
}

/**
 * gnome_keyring_item_set_info:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @info: The item info to save into the item.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Set information on an item, like its display name, secret etc...
 *
 * Only the fields in the @info pointer that are non-null or non-zero will be
 * set on the item.
 *
 * For a synchronous version of this function see gnome_keyring_item_set_info_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_item_set_info (const char                                 *keyring,
                             guint32                                     id,
                             GnomeKeyringItemInfo                       *info,
                             GnomeKeyringOperationDoneCallback           callback,
                             gpointer                                    data,
                             GDestroyNotify                              destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = item_set_info_start (keyring, id, info, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_set_info_sync:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @info: The item info to save into the item.
 *
 * Set information on an item, like its display name, secret etc...
 *
 * Only the fields in the @info pointer that are non-null or non-zero will be
 * set on the item.
 *
 * For an asynchronous version of this function see gnome_keyring_item_set_info().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_item_set_info_sync (const char           *keyring,
                                  guint32               id,
                                  GnomeKeyringItemInfo *info)
{
	GkrOperation *op;

	gkr_init ();

	op = item_set_info_start (keyring, id, info, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

static void
item_get_attributes_sync (GnomeKeyringResult res, GnomeKeyringAttributeList *attrs, gpointer user_data)
{
	GnomeKeyringAttributeList **result = user_data;
	*result = attrs;
}

static void
item_get_attributes_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	GnomeKeyringResult res;
	GnomeKeyringAttributeList *attrs;
	GkrCallback *cb;

	if (gkr_operation_handle_errors (op, reply))
		return;

	attrs = gnome_keyring_attribute_list_new ();
	res = decode_get_attributes (reply, attrs);
	if (res == GNOME_KEYRING_RESULT_OK) {
		cb = gkr_operation_pop (op);
		gkr_callback_invoke_ok_attributes (cb, attrs);
		if (cb->callback == item_get_attributes_sync)
			attrs = NULL;
	} else {
		gkr_operation_complete (op, res);
	}

	gnome_keyring_attribute_list_free (attrs);
}

static GkrOperation*
item_get_attributes_start (const char *keyring, guint32 id,
                           GnomeKeyringOperationGetAttributesCallback callback,
                           gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	path = gkr_encode_keyring_item_id (keyring, id);
	req = prepare_property_get (path, ITEM_INTERFACE, "Attributes");

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_ATTRIBUTES, data, destroy_data);
	gkr_operation_push (op, item_get_attributes_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
	g_free (path);

	return op;
}

/**
 * gnome_keyring_item_get_attributes:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Get all the attributes for an item.
 *
 * A #GnomeKeyringAttributeList will be passed to the @callback. This list will
 * be freed after @callback returns.
 *
 * For a synchronous version of this function see gnome_keyring_item_get_attributes_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_item_get_attributes (const char                                 *keyring,
                                   guint32                                     id,
                                   GnomeKeyringOperationGetAttributesCallback  callback,
                                   gpointer                                    data,
                                   GDestroyNotify                              destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = item_get_attributes_start (keyring, id, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_get_attributes_sync:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @attributes: The location to return a pointer to the attribute list.
 *
 * Get all attributes for an item.
 *
 * A #GnomeKeyringAttributeList will be returned in @attributes. This should be
 * freed using gnome_keyring_attribute_list_free().
 *
 * For an asynchronous version of this function see gnome_keyring_item_get_attributes().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_item_get_attributes_sync (const char                 *keyring,
                                        guint32                     id,
                                        GnomeKeyringAttributeList **attributes)
{
	GkrOperation *op;

	gkr_init ();

	op = item_get_attributes_start (keyring, id, item_get_attributes_sync, attributes, NULL);
	return gkr_operation_block_and_unref (op);
}

static DBusMessage*
item_set_attributes_prepare (const gchar *path, GnomeKeyringAttributeList *attrs)
{
	DBusMessageIter iter, variant;
	DBusMessage *req;
	const gchar *string;

	req = dbus_message_new_method_call (gkr_service_name (), path,
	                                    DBUS_INTERFACE_PROPERTIES, "Set");

	dbus_message_iter_init_append (req, &iter);
	string = ITEM_INTERFACE;
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &string);
	string = "Attributes";
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_STRING, &string);
	dbus_message_iter_open_container (&iter, DBUS_TYPE_VARIANT, "a{ss}", &variant);
	encode_attribute_list (&variant, attrs);
	dbus_message_iter_close_container (&iter, &variant);

	return req;
}

static GkrOperation*
item_set_attributes_start (const char *keyring, guint32 id, GnomeKeyringAttributeList *attributes,
                           GnomeKeyringOperationDoneCallback callback,
                           gpointer data, GDestroyNotify destroy_data)
{
	DBusMessage *req;
	GkrOperation *op;
	gchar *path;

	path = gkr_encode_keyring_item_id (keyring, id);

	/* First set the label */
	req = item_set_attributes_prepare (path, attributes);

	g_free (path);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_request (op, req);
	dbus_message_unref (req);

	return op;
}

/**
 * gnome_keyring_item_set_attributes:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @attributes: The full list of attributes to set on the item.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Set all the attributes for an item. This will replace any previous attributes
 * set on the item.
 *
 * For a synchronous version of this function see gnome_keyring_item_set_attributes_sync().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_item_set_attributes (const char                                 *keyring,
                                   guint32                                     id,
                                   GnomeKeyringAttributeList                  *attributes,
                                   GnomeKeyringOperationDoneCallback           callback,
                                   gpointer                                    data,
                                   GDestroyNotify                              destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = item_set_attributes_start (keyring, id, attributes, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_set_attributes_sync:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @attributes: The full list of attributes to set on the item.
 *
 * Set all the attributes for an item. This will replace any previous attributes
 * set on the item.
 *
 * For an asynchronous version of this function see gnome_keyring_item_set_attributes().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_item_set_attributes_sync (const char                *keyring,
                                        guint32                    id,
                                        GnomeKeyringAttributeList *attributes)
{
	GkrOperation *op;

	gkr_init ();

	op = item_set_attributes_start (keyring, id, attributes, gkr_callback_empty, NULL, NULL);
	return gkr_operation_block_and_unref (op);
}

static void
item_get_acl_reply (GnomeKeyringResult res, gpointer user_data)
{
	GkrCallback *cb = user_data;
	gkr_callback_invoke_ok_list (cb, NULL);
}

/**
 * gnome_keyring_item_get_acl:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 *
 * Deprecated: Never returns any ACL values.
 */
gpointer
gnome_keyring_item_get_acl (const char                                 *keyring,
                            guint32                                     id,
                            GnomeKeyringOperationGetListCallback        callback,
                            gpointer                                    data,
                            GDestroyNotify                              destroy_data)
{
	GkrOperation *op;
	GkrCallback *cb;

	gkr_init ();

	cb = gkr_callback_new (NULL, callback, GKR_CALLBACK_RES_LIST, data, destroy_data);
	op = gkr_operation_new (item_get_acl_reply, GKR_CALLBACK_RES, cb, gkr_callback_free);
	gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_OK);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_get_acl_sync:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @acl: The location to return a pointer to the access control list.
 *
 * Return value: Always %GNOME_KEYRING_RESULT_OK.
 *
 * Deprecated: Never returns any acls.
 **/
GnomeKeyringResult
gnome_keyring_item_get_acl_sync (const char  *keyring,
                                 guint32      id,
                                 GList      **acl)
{
	g_return_val_if_fail (acl, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);
	*acl = NULL;
	return GNOME_KEYRING_RESULT_OK;
}

/**
 * gnome_keyring_item_set_acl:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @acl: The access control list to set on the item.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 *
 * Deprecated: This function no longer has any effect.
 **/
gpointer
gnome_keyring_item_set_acl (const char                                 *keyring,
                            guint32                                     id,
                            GList                                      *acl,
                            GnomeKeyringOperationDoneCallback           callback,
                            gpointer                                    data,
                            GDestroyNotify                              destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_OK);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_set_acl_sync:
 * @keyring: The name of the keyring in which the item exists, or NULL for the default keyring.
 * @id: The id of the item
 * @acl: The access control list to set on the item.
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 *
 * Deprecated: This function no longer has any effect.
 **/
GnomeKeyringResult
gnome_keyring_item_set_acl_sync (const char *keyring,
                                 guint32     id,
                                 GList      *acl)
{
	return GNOME_KEYRING_RESULT_OK;
}

/**
 * gnome_keyring_item_grant_access_rights:
 * @keyring: The keyring name, or NULL for the default keyring.
 * @display_name: The display name for the application, as returned by g_get_application_name().
 * @full_path: The full filepath to the application.
 * @id: The id of the item to grant access to.
 * @rights: The type of rights to grant.
 * @callback: Callback which is called when the operation completes
 * @data: Data to be passed to callback
 * @destroy_data: Function to be called when data is no longer needed.
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 * Since: 2.20
 *
 * Deprecated: This function no longer has any effect.
 */
gpointer
gnome_keyring_item_grant_access_rights (const gchar *keyring,
                                        const gchar *display_name,
                                        const gchar *full_path,
                                        const guint32 id,
                                        const GnomeKeyringAccessType rights,
                                        GnomeKeyringOperationDoneCallback callback,
                                        gpointer data,
                                        GDestroyNotify destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);
	gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_OK);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_item_grant_access_rights_sync:
 * @keyring: The keyring name, or NULL for the default keyring.
 * @display_name: The display name for the application, as returned by g_get_application_name().
 * @full_path: The full filepath to the application.
 * @id: The id of the item to grant access to.
 * @rights: The type of rights to grant.
 *
 * Will grant the application access rights to the item, provided
 * callee has write access to said item.
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 *
 * Deprecated: This function no longer has any effect.
 */
GnomeKeyringResult
gnome_keyring_item_grant_access_rights_sync (const char                   *keyring,
                                             const char                   *display_name,
                                             const char                   *full_path,
                                             const guint32                id,
                                             const GnomeKeyringAccessType rights)
{
	return GNOME_KEYRING_RESULT_OK;
}

/* ------------------------------------------------------------------------------
 * NETWORK PASSWORD APIS
 */

/**
 * SECTION:gnome-keyring-network
 * @title: Network Passwords
 * @short_description: Saving of network passwords.
 *
 * Networks passwords are a simple way of saving passwords associated with a
 * certain user/server/protocol and other fields.
 **/

/**
 * gnome_keyring_network_password_free:
 * @data: A #GnomeKeyringNetworkPasswordData pointer.
 *
 * Free a network password data pointer. If %NULL is passed in,
 * nothing happens.
 **/
void
gnome_keyring_network_password_free (GnomeKeyringNetworkPasswordData *data)
{
	if (!data)
		return;

	g_free (data->keyring);
	g_free (data->protocol);
	g_free (data->server);
	g_free (data->object);
	g_free (data->authtype);
	g_free (data->user);
	g_free (data->domain);
	gnome_keyring_free_password (data->password);

	g_free (data);
}

/**
 * gnome_keyring_network_password_list_free:
 * @list: A list of #GnomeKeyringNetworkPasswordData pointers.
 *
 * Free a list of network password data.
 **/
void
gnome_keyring_network_password_list_free (GList *list)
{
	g_list_foreach (list, (GFunc)gnome_keyring_network_password_free, NULL);
	g_list_free (list);
}

static void
find_network_password_sync (GnomeKeyringResult res, GList *list, gpointer user_data)
{
	GList **result = user_data;
	*result = list;
}

static void
find_network_password_filter (GnomeKeyringResult res, GList *found_list, gpointer user_data)
{
	GkrCallback *cb = user_data;
	GnomeKeyringNetworkPasswordData *data;
	GnomeKeyringFound *found;
	GList *result, *l;
	int i;

	if (res != GNOME_KEYRING_RESULT_OK) {
		gkr_callback_invoke_res (cb, res);
		return;
	}

	result = NULL;
	for (l = found_list; l != NULL; l = l->next) {
		found = l->data;

		data = g_new0 (GnomeKeyringNetworkPasswordData, 1);

		result = g_list_prepend (result, data);

		data->keyring = g_strdup (found->keyring);
		data->item_id = found->item_id;
		data->password = found->secret;
		found->secret = NULL;

		for (i = 0; i < found->attributes->len; i++) {
			GnomeKeyringAttribute *attribute = &(g_array_index (found->attributes, GnomeKeyringAttribute, i));
			if (strcmp (attribute->name, "user") == 0 &&
			    attribute->type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
				data->user = g_strdup (attribute->value.string);
			} else if (strcmp (attribute->name, "domain") == 0 &&
				   attribute->type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
				data->domain = g_strdup (attribute->value.string);
			} else if (strcmp (attribute->name, "server") == 0 &&
				   attribute->type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
				data->server = g_strdup (attribute->value.string);
			} else if (strcmp (attribute->name, "object") == 0 &&
				   attribute->type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
				data->object = g_strdup (attribute->value.string);
			} else if (strcmp (attribute->name, "protocol") == 0 &&
				   attribute->type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
				data->protocol = g_strdup (attribute->value.string);
			} else if (strcmp (attribute->name, "authtype") == 0 &&
				   attribute->type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
				data->authtype = g_strdup (attribute->value.string);
			} else if (strcmp (attribute->name, "port") == 0 &&
				   attribute->type == GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32) {
				data->port = attribute->value.integer;
			}
		}
	}

	result = g_list_reverse (result);
	gkr_callback_invoke_ok_list (cb, result);
	if (cb->callback != find_network_password_sync)
		gnome_keyring_network_password_list_free (result);
}

static GnomeKeyringAttributeList *
make_attribute_list_for_network_password (const char                            *user,
                                          const char                            *domain,
                                          const char                            *server,
                                          const char                            *object,
                                          const char                            *protocol,
                                          const char                            *authtype,
                                          guint32                                port)
{
	GnomeKeyringAttributeList *attributes;

	attributes = g_array_new (FALSE, FALSE, sizeof (GnomeKeyringAttribute));

	if (user != NULL)
		gnome_keyring_attribute_list_append_string (attributes, "user", user);
	if (domain != NULL)
		gnome_keyring_attribute_list_append_string (attributes, "domain", domain);
	if (server != NULL)
		gnome_keyring_attribute_list_append_string (attributes, "server", server);
	if (object != NULL)
		gnome_keyring_attribute_list_append_string (attributes, "object", object);
	if (protocol != NULL)
		gnome_keyring_attribute_list_append_string (attributes, "protocol", protocol);
	if (authtype != NULL)
		gnome_keyring_attribute_list_append_string (attributes, "authtype", authtype);
	if (port != 0)
		gnome_keyring_attribute_list_append_uint32 (attributes, "port", port);
	return attributes;
}

static GkrOperation*
find_network_password_start (const char *user, const char *domain, const char *server,
                             const char *object, const char *protocol, const char *authtype,
                             guint32 port, GnomeKeyringOperationGetListCallback callback,
                             gpointer user_data, GDestroyNotify destroy_data)
{
	GnomeKeyringAttributeList *attributes;
	GkrOperation *op;
	GkrCallback *cb;

	attributes = make_attribute_list_for_network_password (user, domain, server, object,
	                                                       protocol, authtype, port);

	cb = gkr_callback_new (NULL, callback, GKR_CALLBACK_RES_LIST, user_data, destroy_data);
	op = find_items_start (GNOME_KEYRING_ITEM_NETWORK_PASSWORD, attributes,
	                       find_network_password_filter, cb, gkr_callback_free);
	gnome_keyring_attribute_list_free (attributes);

	return op;
}

/**
 * gnome_keyring_find_network_password:
 * @user: The user name or %NULL for any user.
 * @domain: The domain name %NULL for any domain.
 * @server: The server or %NULL for any server.
 * @object: The remote object or %NULL for any object.
 * @protocol: The network protorol or %NULL for any protocol.
 * @authtype: The authentication type or %NULL for any type.
 * @port: The network port or zero for any port.
 * @callback: Callback which is called when the operation completes
 * @data: Data to be passed to callback
 * @destroy_data: Function to be called when data is no longer needed.
 *
 * Find a previously stored network password. Searches all keyrings.
 *
 * A %GList of #GnomeKeyringNetworkPasswordData structures are passed to the
 * @callback. The list and structures are freed after the callback returns.
 *
 * The user may have been prompted to unlock necessary keyrings, and user will
 * have been prompted for access to the items if needed.
 *
 * Network passwords are items with the item type %GNOME_KEYRING_ITEM_NETWORK_PASSWORD
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_find_network_password      (const char                            *user,
                                          const char                            *domain,
                                          const char                            *server,
                                          const char                            *object,
                                          const char                            *protocol,
                                          const char                            *authtype,
                                          guint32                                port,
                                          GnomeKeyringOperationGetListCallback   callback,
                                          gpointer                               user_data,
                                          GDestroyNotify                         destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = find_network_password_start (user, domain, server, object, protocol,
	                                  authtype, port, callback, user_data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_find_network_password_sync:
 * @user: The user name or %NULL.
 * @domain: The domain name %NULL.
 * @server: The server or %NULL.
 * @object: The remote object or %NULL.
 * @protocol: The network protorol or %NULL.
 * @authtype: The authentication type or %NULL.
 * @port: The network port or zero.
 * @results: A location to return a %GList of #GnomeKeyringNetworkPasswordData pointers.
 *
 * Find a previously stored network password. Searches all keyrings.
 *
 * A %GList of #GnomeKeyringNetworkPasswordData structures are returned in the
 * @out_list argument. The list should be freed with gnome_keyring_network_password_list_free()
 *
 * The user may have been prompted to unlock necessary keyrings, and user will
 * have been prompted for access to the items if needed.
 *
 * Network passwords are items with the item type %GNOME_KEYRING_ITEM_NETWORK_PASSWORD
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_find_network_password_sync (const char                            *user,
                                          const char                            *domain,
                                          const char                            *server,
                                          const char                            *object,
                                          const char                            *protocol,
                                          const char                            *authtype,
                                          guint32                                port,
                                          GList                                **results)
{
	GkrOperation *op;

	gkr_init ();

	op = find_network_password_start (user, domain, server, object, protocol,
	                                  authtype, port, find_network_password_sync, results, NULL);
	return gkr_operation_block_and_unref (op);
}

static char *
set_network_password_display_name (const char *user,
                                   const char *server,
                                   const char *object,
                                   guint32  port)
{
	GString *s;
	char *name;

	if (server != NULL) {
		s = g_string_new (NULL);
		if (user != NULL) {
			g_string_append_printf (s, "%s@", user);
		}
		g_string_append (s, server);
		if (port != 0)
			g_string_append_printf (s, ":%d", port);
		if (object != NULL)
			g_string_append_printf (s, "/%s", object);
		name = g_string_free (s, FALSE);
	} else {
		name = g_strdup ("network password");
	}
	return name;
}

static void
set_network_password_sync (GnomeKeyringResult res, guint32 item_id, gpointer user_data)
{
	guint32 *result = user_data;
	*result = item_id;
}

static GkrOperation*
set_network_password_start (const char *keyring, const char *user, const char *domain,
                            const char *server, const char *object, const char *protocol,
                            const char *authtype, guint32 port, const char *password,
                            GnomeKeyringOperationGetIntCallback callback,
                            gpointer data, GDestroyNotify destroy_data)
{
	GnomeKeyringAttributeList *attributes;
	GkrOperation *op;
	char *name;

	name = set_network_password_display_name (user, server, object, port);

	attributes = make_attribute_list_for_network_password (user, domain, server, object,
	                                                       protocol, authtype, port);

	op = item_create_start (keyring, GNOME_KEYRING_ITEM_NETWORK_PASSWORD, name, attributes,
	                        password, TRUE, callback, data, destroy_data);

	gnome_keyring_attribute_list_free (attributes);
	g_free (name);

	return op;
}

/**
 * gnome_keyring_set_network_password:
 * @keyring: The keyring to store the password in, or %NULL for the default keyring.
 * @user: The user name or %NULL.
 * @domain: The domain name %NULL.
 * @server: The server or %NULL.
 * @object: The remote object or %NULL.
 * @protocol: The network protorol or %NULL.
 * @authtype: The authentication type or %NULL.
 * @port: The network port or zero.
 * @password: The password to store, must not be %NULL.
 * @callback: Callback which is called when the operation completes
 * @data: Data to be passed to callback
 * @destroy_data: Function to be called when data is no longer needed.
 *
 * Store a network password.
 *
 * If an item already exists for with this network info (ie: user, server etc...)
 * then it will be updated.
 *
 * Whether a new item is created or not, id of the item will be passed to
 * the @callback.
 *
 * Network passwords are items with the item type %GNOME_KEYRING_ITEM_NETWORK_PASSWORD
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 **/
gpointer
gnome_keyring_set_network_password      (const char                            *keyring,
                                         const char                            *user,
                                         const char                            *domain,
                                         const char                            *server,
                                         const char                            *object,
                                         const char                            *protocol,
                                         const char                            *authtype,
                                         guint32                                port,
                                         const char                            *password,
                                         GnomeKeyringOperationGetIntCallback    callback,
                                         gpointer                               data,
                                         GDestroyNotify                         destroy_data)
{
	GkrOperation *op;

	gkr_init ();

	op = set_network_password_start (keyring, user, domain, server, object, protocol,
	                                 authtype, port, password, callback, data, destroy_data);
	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_set_network_password_sync:
 * @keyring: The keyring to store the password in, or %NULL for the default keyring.
 * @user: The user name or %NULL.
 * @domain: The domain name %NULL.
 * @server: The server or %NULL.
 * @object: The remote object or %NULL.
 * @protocol: The network protorol or %NULL.
 * @authtype: The authentication type or %NULL.
 * @port: The network port or zero.
 * @password: The password to store, must not be %NULL.
 * @item_id: A location to store the resulting item's id.
 *
 * Store a network password.
 *
 * If an item already exists for with this network info (ie: user, server etc...)
 * then it will be updated.
 *
 * The created or updated item id will be returned in @item_id.
 *
 * Network passwords are items with the item type %GNOME_KEYRING_ITEM_NETWORK_PASSWORD
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 **/
GnomeKeyringResult
gnome_keyring_set_network_password_sync (const char                            *keyring,
                                         const char                            *user,
                                         const char                            *domain,
                                         const char                            *server,
                                         const char                            *object,
                                         const char                            *protocol,
                                         const char                            *authtype,
                                         guint32                                port,
                                         const char                            *password,
                                         guint32                               *item_id)
{
	GkrOperation *op;

	gkr_init ();

	op = set_network_password_start (keyring, user, domain, server, object, protocol,
	                                 authtype, port, password, set_network_password_sync, item_id, NULL);
	return gkr_operation_block_and_unref (op);
}

/* ------------------------------------------------------------------------------
 * SIMPLE PASSWORD APIS
 */

/**
 * SECTION:gnome-keyring-password
 * @title: Simple Password Storage
 * @short_description: Store and lookup passwords with a set of attributes.
 *
 * This is a simple API for storing passwords and retrieving passwords in the keyring.
 *
 * Each password is associated with a set of attributes. Attribute values can be either
 * strings or unsigned integers.
 *
 * The names and types of allowed attributes for a given password are defined with a
 * schema. Certain schemas are predefined such as %GNOME_KEYRING_NETWORK_PASSWORD.
 * Additional schemas can be defined via the %GnomeKeyringPasswordSchema structure.
 *
 * Each function accepts a variable list of attributes names and their values.
 * Include a %NULL to terminate the list of attributes.
 *
 * <example>
 * <title>Passing attributes to the functions</title>
 * <programlisting>
 *   res = gnome_keyring_delete_password_sync (GNOME_KEYRING_NETWORK_PASSWORD,
 *                                             "user", "me",        // A string attribute
 *                                             "server, "example.gnome.org",
 *                                             "port", "8080",      // An integer attribute
 *                                             NULL);
 * </programlisting></example>
 **/

/**
 * GnomeKeyringPasswordSchema:
 * @item_type: The item type for this schema.
 *
 * Describes a password schema. Often you'll want to use a predefined schema such
 * as %GNOME_KEYRING_NETWORK_PASSWORD.
 *
 * <para>
 * The last attribute name in a schema must be %NULL.
 *
 * <programlisting>
 *   GnomeKeyringPasswordSchema my_schema = {
 *       GNOME_KEYRING_ITEM_GENERIC_SECRET,
 *       {
 *            { "string-attr", GNOME_KEYRING_ATTRIBUTE_TYPE_STRING },
 *            { "uint-attr", GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32 },
 *            { NULL, 0 }
 *       }
 *   };
 * </programlisting>
 * </para>
 **/

static const GnomeKeyringPasswordSchema network_password_schema = {
	GNOME_KEYRING_ITEM_NETWORK_PASSWORD,
	{
		{  "user", GNOME_KEYRING_ATTRIBUTE_TYPE_STRING },
		{  "domain", GNOME_KEYRING_ATTRIBUTE_TYPE_STRING },
		{  "object", GNOME_KEYRING_ATTRIBUTE_TYPE_STRING },
		{  "protocol", GNOME_KEYRING_ATTRIBUTE_TYPE_STRING },
		{  "port", GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32 },
		{  "server", GNOME_KEYRING_ATTRIBUTE_TYPE_STRING },
		{  "NULL", 0 },
	}
};

/**
 * GNOME_KEYRING_NETWORK_PASSWORD:
 *
 * <para>
 * A predefined schema for network paswsords. It contains the following attributes:
 * </para>
 * <itemizedlist>
 * <listitem>user: A string for the user login.</listitem>
 * <listitem>server: The server being connected to.</listitem>
 * <listitem>protocol: The protocol used to access the server, such as 'http' or 'smb'</listitem>
 * <listitem>domain: A realm or domain, such as a Windows login domain.</listitem>
 * <listitem>port: The network port to used to connect to the server.</listitem>
 * </itemizedlist>
 **/

/* Declared in gnome-keyring.h */
const GnomeKeyringPasswordSchema *GNOME_KEYRING_NETWORK_PASSWORD = &network_password_schema;

/**
 * GNOME_KEYRING_DEFAULT:
 *
 * <para>
 * The default keyring.
 * </para>
 **/

/**
 * GNOME_KEYRING_SESSION:
 *
 * <para>
 * A keyring only stored in memory.
 * </para>
 **/

static GnomeKeyringAttributeList*
schema_attribute_list_va (const GnomeKeyringPasswordSchema *schema, va_list args)
{
	GnomeKeyringAttributeList *attributes;
	GnomeKeyringAttributeType type;
	GnomeKeyringAttribute attribute;
	gboolean type_found;
	char *str;
	guint32 i, val;

	attributes = g_array_new (FALSE, FALSE, sizeof (GnomeKeyringAttribute));

	while ((attribute.name = va_arg (args, char *)) != NULL) {

		type_found = FALSE;
		for (i = 0; i < G_N_ELEMENTS (schema->attributes); ++i) {
			if (!schema->attributes[i].name)
				break;
			if (strcmp (schema->attributes[i].name, attribute.name) == 0) {
				type_found = TRUE;
				type = schema->attributes[i].type;
				break;
			}
		}

		if (!type_found) {
			g_warning ("The password attribute '%s' was not found in the password schema.", attribute.name);
			g_array_free (attributes, TRUE);
			return NULL;
		}

		attribute.type = type;
		switch (type) {
		case GNOME_KEYRING_ATTRIBUTE_TYPE_STRING:
			str = va_arg (args, char *);
			attribute.value.string = str;
			g_array_append_val (attributes, attribute);
			break;
		case GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32:
			val = va_arg (args, guint32);
			attribute.value.integer = val;
			g_array_append_val (attributes, attribute);
			break;
		default:
			g_warning ("The password attribute '%s' has an invalid type in the password schema.", attribute.name);
			g_array_free (attributes, TRUE);
			return NULL;
		}
	}

	return attributes;
}

static void
store_password_filter (GnomeKeyringResult res, guint32 item_id, gpointer user_data)
{
	GkrCallback *cb = user_data;
	gkr_callback_invoke_res (cb, res);
}

/**
 * gnome_keyring_store_password:
 * @schema: The password schema.
 * @keyring: The keyring to store the password in. Specify %NULL for the default keyring.
 *           Use %GNOME_KEYRING_SESSION to store the password in memory only.
 * @display_name: A human readable description of what the password is for.
 * @password: The password to store.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 * @...: The variable argument list should contain pairs of a) The attribute name as a null
 *       terminated string, followed by b) attribute value, either a character string,
 *       or 32-bit unsigned int, as defined in the password @schema. The list of attribtues
 *       should be terminated with a %NULL.
 *
 * Store a password associated with a given set of attributes.
 *
 * Attributes which identify this password must be passed as additional
 * arguments. Attributes passed must be defined in the schema.
 *
 * If a password exists in the keyring that already has all the same arguments,
 * then the password will be updated.
 *
 * Another more complex way to create a keyring item is using gnome_keyring_item_create().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 * Since: 2.22
 **/
gpointer
gnome_keyring_store_password (const GnomeKeyringPasswordSchema* schema, const gchar *keyring,
                              const gchar *display_name, const gchar *password,
                              GnomeKeyringOperationDoneCallback callback,
                              gpointer data, GDestroyNotify destroy_data, ...)
{
	GnomeKeyringAttributeList *attributes;
	GkrOperation *op;
	GkrCallback *cb;
	va_list args;

	gkr_init ();

	va_start (args, destroy_data);
	attributes = schema_attribute_list_va (schema, args);
	va_end (args);

	cb = gkr_callback_new (NULL, callback, GKR_CALLBACK_RES, data, destroy_data);
	op = gnome_keyring_item_create (keyring, schema->item_type, display_name, attributes,
	                                password, TRUE, store_password_filter, cb, gkr_callback_free);

	g_array_free (attributes, TRUE);
	return op;
}

/**
 * gnome_keyring_store_password_sync:
 * @schema: The password schema.
 * @keyring: The keyring to store the password in. Specify %NULL for the default keyring.
 *           Use %GNOME_KEYRING_SESSION to store the password in memory only.
 * @display_name: A human readable description of what the password is for.
 * @password: The password to store.
 * @...: The variable argument list should contain pairs of a) The attribute name as a null
 *       terminated string, followed by b) attribute value, either a character string,
 *       or 32-bit unsigned int, as defined in the password @schema. The list of attribtues
 *       should be terminated with a %NULL.
 *
 * Store a password associated with a given set of attributes.
 *
 * Attributes which identify this password must be passed as additional
 * arguments. Attributes passed must be defined in the schema.
 *
 * This function may block for an unspecified period. If your application must
 * remain responsive to the user, then use gnome_keyring_store_password().
 *
 * If a password exists in the keyring that already has all the same arguments,
 * then the password will be updated.
 *
 * Another more complex way to create a keyring item is using
 * gnome_keyring_item_create_sync().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 * Since: 2.22
 **/
GnomeKeyringResult
gnome_keyring_store_password_sync (const GnomeKeyringPasswordSchema* schema, const gchar *keyring,
                                   const gchar *display_name, const gchar *password, ...)
{
	GnomeKeyringAttributeList *attributes;
	GnomeKeyringResult res;
	guint32 item_id;
	va_list args;

	g_return_val_if_fail (schema, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	va_start (args, password);
	attributes = schema_attribute_list_va (schema, args);
	va_end (args);

	if (!attributes || !attributes->len)
		return GNOME_KEYRING_RESULT_BAD_ARGUMENTS;

	res = gnome_keyring_item_create_sync (keyring, schema->item_type, display_name,
	                                      attributes, password, TRUE, &item_id);

	g_array_free (attributes, TRUE);
	return res;
}

static gboolean
find_unlocked_first (const char *path, gpointer user_data)
{
	const char **result = user_data;
	*result = path;
	return FALSE;
}

static void
find_unlocked_3_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	gboolean dismissed;
	const char *item = NULL;

	/* At this point Prompt has Completed, and should contain a list of unlocked items */

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!decode_xlock_completed (reply, &dismissed, find_unlocked_first, &item)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	gkr_callback_invoke_op_string (gkr_operation_pop (op), item);
}

static void
find_unlocked_2_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	const char *prompt;
	const char *item = NULL;

	/* At this point Unlock has returned a list of unlocked items, plus prompt? */

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!decode_xlock_reply (reply, &prompt, find_unlocked_first, &item)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Need to show prompt to find an unlocked item */
	if (!item && !g_str_equal (prompt, "/")) {
		gkr_operation_push (op, find_unlocked_3_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
		gkr_operation_prompt (op, prompt);
		return;
	}

	gkr_callback_invoke_op_string (gkr_operation_pop (op), item);
}

static void
find_unlocked_1_reply (GkrOperation *op, DBusMessage *reply, gpointer data)
{
	char **unlocked, **locked;
	int n_unlocked, n_locked;
	DBusMessage *req;

	/* At this point SearchItems has returned two lists of locked/unlocked items */

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_get_args (reply, NULL,
	                            DBUS_TYPE_ARRAY, DBUS_TYPE_OBJECT_PATH, &unlocked, &n_unlocked,
	                            DBUS_TYPE_ARRAY, DBUS_TYPE_OBJECT_PATH, &locked, &n_locked,
	                            DBUS_TYPE_INVALID)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	/* Do we have an unlocked item? */
	if (n_unlocked) {
		gkr_callback_invoke_op_string (gkr_operation_pop (op), unlocked[0]);

	/* Do we have any to unlock? */
	} else if (n_locked) {
		req = prepare_xlock ("Unlock", locked, n_locked);
		gkr_operation_push (op, find_unlocked_2_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
		gkr_operation_request (op, req);

	/* No passwords at all, complete */
	} else {
		gkr_callback_invoke_op_string (gkr_operation_pop (op), NULL);
	}

	dbus_free_string_array (locked);
	dbus_free_string_array (unlocked);
}

static void
find_unlocked (GkrOperation *op, GnomeKeyringAttributeList *attributes)
{
	DBusMessageIter iter;
	DBusMessage *req;

	req = dbus_message_new_method_call (gkr_service_name (), SERVICE_PATH,
	                                    SERVICE_INTERFACE, "SearchItems");

	/* Encode the attribute list */
	dbus_message_iter_init_append (req, &iter);
	encode_attribute_list (&iter, attributes);

	gkr_operation_push (op, find_unlocked_1_reply, GKR_CALLBACK_OP_MSG, NULL, NULL);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static void
find_password_sync (GnomeKeyringResult res, const gchar *secret, gpointer user_data)
{
	gchar **result = user_data;
	*result = (gchar*)secret;
}

static void
find_password_3_reply (GkrOperation *op, DBusMessage *reply, gpointer user_data)
{
	GkrSession *session = user_data;
	DBusMessageIter iter;
	GkrCallback *cb;
	gchar *secret;

	if (gkr_operation_handle_errors (op, reply))
		return;

	if (!dbus_message_iter_init (reply, &iter) ||
	    !gkr_session_decode_secret (session, &iter, &secret)) {
		gkr_operation_complete (op, decode_invalid_response (reply));
		return;
	}

	cb = gkr_operation_pop (op);
	gkr_callback_invoke_ok_string (cb, secret);
	if (cb->callback != find_password_sync)
		egg_secure_strfree (secret);
}

static void
find_password_2_reply (GkrOperation *op, GkrSession *session, gpointer user_data)
{
	gchar *path = user_data;
	DBusMessage *req;

	req = prepare_get_secret (session, path);

	gkr_operation_push (op, find_password_3_reply, GKR_CALLBACK_OP_MSG,
	                    gkr_session_ref (session), gkr_session_unref);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}

static void
find_password_1_reply (GkrOperation *op, const char *path, gpointer user_data)
{
	GkrCallback *cb;

	/* All done, complete the operation here */
	if (path == NULL) {
		cb = gkr_operation_pop (op);
		gkr_callback_invoke_res (cb, GNOME_KEYRING_RESULT_NO_MATCH);

	/* We need a session to get the secret for this item */
	} else {
		gkr_operation_push (op, find_password_2_reply, GKR_CALLBACK_OP_SESSION,
		                    g_strdup (path), g_free);
		gkr_session_negotiate (op);
	}
}

static GkrOperation*
find_password_va_start (const GnomeKeyringPasswordSchema* schema, va_list va,
                        GnomeKeyringOperationGetStringCallback callback,
                        gpointer data, GDestroyNotify destroy_data)
{
	GnomeKeyringAttributeList *attributes;
	GkrOperation *op;

	g_assert (schema);
	g_assert (callback);

	attributes = schema_attribute_list_va (schema, va);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES_STRING, data, destroy_data);

	if (attributes == NULL || attributes->len == 0) {
		gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	} else {
		gkr_operation_push (op, find_password_1_reply, GKR_CALLBACK_OP_STRING, NULL, NULL);
		find_unlocked (op, attributes);
	}

	g_array_free (attributes, TRUE);
	return op;
}

/**
 * gnome_keyring_find_password:
 * @schema: The password schema.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 * @...: The variable argument list should contain pairs of a) The attribute name as a null
 *       terminated string, followed by b) attribute value, either a character string,
 *       or 32-bit unsigned int, as defined in the password @schema. The list of attribtues
 *       should be terminated with a %NULL.
 *
 * Find a password that matches a given set of attributes.
 *
 * Attributes which identify this password must be passed as additional
 * arguments. Attributes passed must be defined in the schema.
 *
 * The string that is passed to @callback is automatically freed when the
 * function returns.
 *
 * Another more complex way to find items in the keyrings is using
 * gnome_keyring_find_items().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 * Since: 2.22
 **/
gpointer
gnome_keyring_find_password (const GnomeKeyringPasswordSchema* schema,
                             GnomeKeyringOperationGetStringCallback callback,
                             gpointer data, GDestroyNotify destroy_data, ...)
{
	GkrOperation *op;
	va_list va;

	g_return_val_if_fail (schema, NULL);
	g_return_val_if_fail (callback, NULL);

	gkr_init ();

	va_start (va, destroy_data);
	op = find_password_va_start (schema, va, callback, data, destroy_data);
	va_end (va);

	return gkr_operation_pending_and_unref (op);
}

/**
 * GnomeKeyringFound:
 * @keyring: The keyring the item was found in.
 * @item_id: The identifier for the item.
 * @attributes: The item's attributes.
 * @secret: The item's secret.
 *
 * A found structure returned by a found operation. Use gnome_keyring_found_list_free()
 * to free a list of these structures.
 */

/**
 * gnome_keyring_find_password_sync:
 * @schema: The password schema.
 * @password: An address to store password that was found. The password must
 *            be freed with gnome_keyring_free_password().
 * @...: The variable argument list should contain pairs of a) The attribute name as a null
 *       terminated string, followed by b) attribute value, either a character string,
 *       or 32-bit unsigned int, as defined in the password @schema. The list of attribtues
 *       should be terminated with a %NULL.
 *
 * Find a password that matches a given set of attributes.
 *
 * Attributes which identify this password must be passed as additional
 * arguments. Attributes passed must be defined in the schema.
 *
 * This function may block for an unspecified period. If your application must
 * remain responsive to the user, then use gnome_keyring_find_password().
 *
 * Another more complex way to find items in the keyrings is using
 * gnome_keyring_find_items_sync().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 * Since: 2.22
 **/
GnomeKeyringResult
gnome_keyring_find_password_sync (const GnomeKeyringPasswordSchema* schema,
                                  gchar **password, ...)
{
	GkrOperation *op;
	va_list va;

	g_return_val_if_fail (schema, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);
	g_return_val_if_fail (password, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	va_start (va, password);
	op = find_password_va_start (schema, va, find_password_sync, password, NULL);
	va_end (va);

	return gkr_operation_block_and_unref (op);
}

static void
delete_password_reply (GkrOperation *op, const char *path, gpointer user_data)
{
	DBusMessage *req;

	if (path == NULL) {
		gkr_operation_complete (op, GNOME_KEYRING_RESULT_NO_MATCH);
	} else {
		req = dbus_message_new_method_call (gkr_service_name (), path,
		                                    ITEM_INTERFACE, "Delete");
		gkr_operation_request (op, req);
		dbus_message_unref (req);
	}
}

static GkrOperation*
delete_password_va_start (const GnomeKeyringPasswordSchema* schema, va_list va,
                          GnomeKeyringOperationDoneCallback callback,
                          gpointer data, GDestroyNotify destroy_data)
{
	GnomeKeyringAttributeList *attributes;
	GkrOperation *op;

	g_assert (schema);
	g_assert (callback);

	attributes = schema_attribute_list_va (schema, va);

	op = gkr_operation_new (callback, GKR_CALLBACK_RES, data, destroy_data);

	if (!attributes || !attributes->len) {
		gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	} else {
		gkr_operation_push (op, delete_password_reply, GKR_CALLBACK_OP_STRING, NULL, NULL);
		find_unlocked (op, attributes);
	}

	return op;
}

/**
 * gnome_keyring_delete_password:
 * @schema: The password schema.
 * @callback: A callback which will be called when the request completes or fails.
 * @data: A pointer to arbitrary data that will be passed to the @callback.
 * @destroy_data: A function to free @data when it's no longer needed.
 * @...: The variable argument list should contain pairs of a) The attribute name as a null
 *       terminated string, followed by b) attribute value, either a character string,
 *       or 32-bit unsigned int, as defined in the password @schema. The list of attribtues
 *       should be terminated with a %NULL.
 *
 * Delete a password that matches a given set of attributes.
 *
 * Attributes which identify this password must be passed as additional
 * arguments. Attributes passed must be defined in the schema.
 *
 * Another more complex way to find items in the keyrings is using
 * gnome_keyring_item_delete().
 *
 * Return value: The asychronous request, which can be passed to gnome_keyring_cancel_request().
 * Since: 2.22
 **/
gpointer
gnome_keyring_delete_password (const GnomeKeyringPasswordSchema* schema,
                               GnomeKeyringOperationDoneCallback callback,
                               gpointer data, GDestroyNotify destroy_data, ...)
{
	GkrOperation *op;
	va_list va;

	g_return_val_if_fail (schema, NULL);
	g_return_val_if_fail (callback, NULL);

	gkr_init ();

	va_start (va, destroy_data);
	op = delete_password_va_start (schema, va, callback, data, destroy_data);
	va_end (va);

	return gkr_operation_pending_and_unref (op);
}

/**
 * gnome_keyring_delete_password_sync:
 * @schema: The password schema.
 * @...: The variable argument list should contain pairs of a) The attribute name as a null
 *       terminated string, followed by b) attribute value, either a character string,
 *       or 32-bit unsigned int, as defined in the password @schema. The list of attribtues
 *       should be terminated with a %NULL.
 *
 * Delete a password that matches a given set of attributes.
 *
 * Attributes which identify this password must be passed as additional
 * arguments. Attributes passed must be defined in the schema.
 *
 * This function may block for an unspecified period. If your application must
 * remain responsive to the user, then use gnome_keyring_delete_password().
 *
 * Another more complex way to find items in the keyrings is using
 * gnome_keyring_item_delete_sync().
 *
 * Return value: %GNOME_KEYRING_RESULT_OK if the operation was succcessful or
 * an error result otherwise.
 * Since: 2.22
 **/
GnomeKeyringResult
gnome_keyring_delete_password_sync (const GnomeKeyringPasswordSchema* schema, ...)
{
	GkrOperation *op;
	va_list va;

	g_return_val_if_fail (schema, GNOME_KEYRING_RESULT_BAD_ARGUMENTS);

	gkr_init ();

	va_start (va, schema);
	op = delete_password_va_start (schema, va, gkr_callback_empty, NULL, NULL);
	va_end (va);

	return gkr_operation_block_and_unref (op);
}
