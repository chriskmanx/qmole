/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-uri.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2010, Stefan Walter

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

#include "gck.h"
#include "gck-private.h"
#include "gck-marshal.h"

#include <glib/gi18n-lib.h>

#include <string.h>

#include "egg/egg-hex.h"

/**
 * SECTION:gck-modules
 * @title: GckModule lists
 * @short_description: Dealing with lists of PKCS#11 modules.
 *
 * Xxxxx
 */

#define URI_PREFIX "pkcs11:"
#define N_URI_PREFIX 7

GQuark
gck_uri_get_error_quark (void)
{
	static GQuark domain = 0;
	static volatile gsize quark_inited = 0;

	if (g_once_init_enter (&quark_inited)) {
		domain = g_quark_from_static_string ("gck-uri-error");
		g_once_init_leave (&quark_inited, 1);
	}

	return domain;
}

static gint
parse_string_attribute (const gchar *name, const gchar *start, const gchar *end,
                        GckAttributes *attrs, GError **error)
{
	gchar *value;
	gint res = 0;

	g_assert (name);
	g_assert (start);
	g_assert (end);

	if (!g_str_equal (name, "object") && !g_str_equal (name, "objecttype"))
		return 0;

	value = g_uri_unescape_segment (start, end, "");
	if (value == NULL) {
		g_set_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING,
		             _("The URI has invalid syntax. The '%s' field encoding is invalid."), name);
		return -1;
	}

	if (g_str_equal (name, "object")) {
		gck_attributes_add_string (attrs, CKA_LABEL, value);
		res = 1;

	} else if (g_str_equal (name, "objecttype")) {

		res = 1;
		if (g_str_equal (value, "cert"))
			gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_CERTIFICATE);
		else if (g_str_equal (value, "public"))
			gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_PUBLIC_KEY);
		else if (g_str_equal (value, "private"))
			gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_PRIVATE_KEY);
		else if (g_str_equal (value, "secretkey"))
			gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_SECRET_KEY);
		else if (g_str_equal (value, "data"))
			gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
		else {
			g_message ("ignoring unsupported value for '%s'", value);
			res = 0;
		}
	} else {
		g_assert_not_reached ();
	}

	g_free (value);
	return res;
}

static gint
parse_binary_attribute (const gchar *name, const gchar *start, const gchar *end,
                        GckAttributes *attrs, GError **error)
{
	guchar *data;
	gsize n_data;

	g_assert (name);
	g_assert (start);
	g_assert (end);
	g_assert (attrs);

	if (!g_str_equal (name, "id"))
		return 0;

	/*
	 * TODO: This requires some work. We're not yet sure about the actual
	 * encoding that's supported here.
	 */

	g_assert (end >= start);
	data = egg_hex_decode_full (start, end - start, ':', 1, &n_data);
	if (data == NULL) {
		g_set_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING,
		             _("The URI has invalid syntax. The '%s' field encoding is invalid."), name);
		return -1;
	}

	gck_attributes_add_data (attrs, CKA_ID, data, n_data);
	g_free (data);
	return 1;
}

static gint
parse_token_attribute (const gchar *name, const gchar *start, const gchar *end,
                       GckTokenInfo *token, GError **error)
{
	gchar **value;
	gchar *string;

	g_assert (name);
	g_assert (start);
	g_assert (end);
	g_assert (token);

	if (g_str_equal (name, "model"))
		value = &(token->model);
	else if (g_str_equal (name, "manufacturer"))
		value = &(token->manufacturer_id);
	else if (g_str_equal (name, "serial"))
		value = &(token->serial_number);
	else if (g_str_equal (name, "token"))
		value = &(token->label);
	else
		return 0;

	string = g_uri_unescape_segment (start, end, "");
	if (string == NULL) {
		g_set_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING,
		             _("The URI has invalid syntax. The '%s' field encoding is invalid."), name);
		return -1;
	}

	g_free (*value);
	*value = string;

	return 1;
}

gboolean
gck_uri_parse (const gchar *uri, GckTokenInfo **token, GckAttributes **attrs, GError **error)
{
	GckAttributes *rattrs = NULL;
	GckTokenInfo *rtoken = NULL;
	const gchar *spos, *epos;
	gchar *key = NULL;
	gboolean ret = FALSE;
	gint res;

	g_return_val_if_fail (uri, FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	if (!g_str_has_prefix (uri, URI_PREFIX)) {
		g_set_error_literal (error, GCK_URI_ERROR, GCK_URI_BAD_PREFIX,
		                     _("The URI has does not have the 'pkcs11' scheme."));
		goto cleanup;
	}

	uri += N_URI_PREFIX;
	rattrs = gck_attributes_new ();
	rtoken = g_new0 (GckTokenInfo, 1);

	for (;;) {
		spos = strchr (uri, ';');
		if (spos == NULL) {
			spos = uri + strlen (uri);
			g_assert (*spos == '\0');
			if (spos == uri)
				break;
		}

		epos = strchr (uri, '=');
		if (epos == NULL || spos == uri || epos == uri || epos >= spos) {
			g_set_error_literal (error, GCK_URI_ERROR, GCK_URI_BAD_SYNTAX,
			                     "The URI has invalid syntax. It must consist of key=value pairs.");
			goto cleanup;
		}

		g_free (key);
		key = g_strndup (uri, epos - uri);
		epos++;

		res = parse_string_attribute (key, epos, spos, rattrs, error);
		if (res == 0)
			res = parse_binary_attribute (key, epos, spos, rattrs, error);
		if (res == 0)
			res = parse_token_attribute (key, epos, spos, rtoken, error);
		if (res < 0)
			goto cleanup;
		if (res == 0)
			g_message ("Ignoring unsupported field '%s'", key);

		if (*spos == '\0')
			break;
		uri = spos + 1;
	}

	ret = TRUE;

cleanup:
	if (ret && token) {
		*token = rtoken;
		rtoken = NULL;
	}
	if (ret && attrs) {
		*attrs = rattrs;
		rattrs = NULL;
	}

	gck_token_info_free (rtoken);
	if (rattrs)
		gck_attributes_unref (rattrs);

	g_free (key);
	return ret;
}

static void
build_string_attribute (const gchar *name, const gchar *value,
                        GString *result, gboolean *first)
{
	gchar *segment;

	g_assert (first);
	g_assert (result);
	g_assert (name);

	if (!value)
		return;
	if (!value[0])
		return;

	segment = g_uri_escape_string (value, "", FALSE);
	if (!*first)
		g_string_append_c (result, ';');
	*first = FALSE;

	g_string_append (result, name);
	g_string_append_c (result, '=');
	g_string_append (result, segment);
	g_free (segment);
}

static void
build_binary_attribute (const gchar *name, gconstpointer data, gsize n_data,
                        GString *result, gboolean *first)
{
	gchar *segment;

	g_assert (first);
	g_assert (result);
	g_assert (name);

	if (!n_data)
		return;
	g_assert (data);

	segment = egg_hex_encode_full (data, n_data, FALSE, ':', 1);
	if (!*first)
		g_string_append_c (result, ';');
	*first = FALSE;

	g_string_append (result, name);
	g_string_append_c (result, '=');
	g_string_append (result, segment);
	g_free (segment);
}

gchar*
gck_uri_build (GckTokenInfo *token, GckAttributes *attrs)
{
	GckAttribute *attr;
	GString *result;
	gchar *value;
	gulong klass;
	gboolean first = TRUE;

	result = g_string_new (URI_PREFIX);

	if (token) {
		build_string_attribute ("model", token->model, result, &first);
		build_string_attribute ("manufacturer", token->manufacturer_id, result, &first);
		build_string_attribute ("serial", token->serial_number, result, &first);
		build_string_attribute ("token", token->label, result, &first);
	}

	if (attrs) {
		if (gck_attributes_find_string (attrs, CKA_LABEL, &value)) {
			build_string_attribute ("object", value, result, &first);
			g_free (value);
		}
		if (gck_attributes_find_ulong (attrs, CKA_CLASS, &klass)) {
			if (klass == CKO_CERTIFICATE)
				build_string_attribute ("objecttype", "cert", result, &first);
			else if (klass == CKO_PUBLIC_KEY)
				build_string_attribute ("objecttype", "public", result, &first);
			else if (klass == CKO_PRIVATE_KEY)
				build_string_attribute ("objecttype", "private", result, &first);
			else if (klass == CKO_SECRET_KEY)
				build_string_attribute ("objecttype", "secretkey", result, &first);
			else if (klass == CKO_DATA)
				build_string_attribute ("objecttype", "data", result, &first);
		}
		attr = gck_attributes_find (attrs, CKA_ID);
		if (attr != NULL)
			build_binary_attribute ("id", attr->value, attr->length, result, &first);
	}

	return g_string_free (result, FALSE);
}
