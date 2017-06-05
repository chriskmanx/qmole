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

#include <p11-kit/uri.h>

#include <string.h>
#include <stdlib.h>

/**
 * SECTION:gck-uri
 * @title: PKCS11 URIs
 * @short_description: Parsing and building PKCS\#11 URIs.
 *
 * <ulink href='http://tools.ietf.org/html/draft-pechanec-pkcs11uri-03'>PKCS\#11 URIs</ulink>
 * are a standard for referring to PKCS\#11 modules, tokens, or objects. What the
 * PKCS\#11 URI refers to depends on the context in which it is used.
 *
 * A PKCS\#11 URI can always resolve to more than one object, token or module. A
 * PKCS\#11 URI that refers to a token, would (when used in a context that expects
 * objects) refer to all the token on that module.
 *
 * In most cases the parsing or building of URIs is handled elsewhere in the GCK
 * library. For example to enumerate objects that match a PKCS\#11 URI use the
 * gck_modules_enumerate_uri() function.
 *
 * To parse a PKCS\#11 URI use the gck_uri_parse() function passing in the type of
 * context in which you're using the URI. To build a URI use the gck_uri_build()
 * function.
 **/

/**
 * GckUriData:
 * @any_unrecognized: whether any parts of the PKCS\#11 URI were unsupported or unrecognized.
 * @module_info: information about the PKCS\#11 modules matching the URI.
 * @token_info: information about the PKCS\#11 tokens matching the URI.
 * @attributes: information about the PKCS\#11 objects matching the URI.
 *
 * Information about the contents of a PKCS\#11 URI. Various fields may be %NULL
 * depending on the context that the URI was parsed for.
 *
 * Since PKCS\#11 URIs represent a set which results from the intersections of
 * all of the URI parts, if @any_recognized is set to %TRUE then usually the URI
 * should be treated as not matching anything.
 */

/**
 * GckUriFlags:
 * @GCK_URI_FOR_MODULE: the URI will be used to match modules.
 * @GCK_URI_FOR_TOKEN: the URI will be used to match tokens.
 * @GCK_URI_FOR_OBJECT: the URI will be used to match objects.
 * @GCK_URI_FOR_ANY: parse all recognized components of the URI.
 * @GCK_URI_FOR_MODULE_WITH_VERSION: the URI will match specific version of modules.
 * @GCK_URI_FOR_OBJECT_ON_TOKEN: the URI will match objects on a specific token.
 * @GCK_URI_FOR_OBJECT_ON_TOKEN_AND_MODULE: the URI will match objects on a specific
 *      token inserted into a device with a specific module.
 *
 * Which parts of the PKCS\#11 URI will be parsed or formatted. These can be
 * combined.
 */

/**
 * GCK_URI_ERROR:
 *
 * Error domain for URI errors.
 */

#define URI_PREFIX "pkcs11:"
#define N_URI_PREFIX 7

struct _GckUri {
	gboolean any_unrecognized;
	GckModuleInfo *module_info;
	GckTokenInfo *token_info;
	GckAttributes *attributes;
};

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

/**
 * gck_uri_new:
 *
 * Allocate a new GckUriData structure. None of the fields
 * will be set.
 *
 * Returns: A newly allocated GckUriData, free with gck_uri_data_free().
 */
GckUriData*
gck_uri_data_new (void)
{
	return g_slice_new0 (GckUriData);
}

/**
 * gck_uri_parse:
 * @string: the URI to parse.
 * @flags: the context in which the URI will be used.
 * @error: a #GError, or %NULL.
 *
 * Parse a PKCS\#11 URI for use in a given context.
 *
 * The result will contain the fields that are relevant for
 * the given context. See #GckUriData  for more info.
 * Other fields will be set to %NULL.
 *
 * Return value: a newly allocated #GckUriData, which should be freed with
 * 	gck_uri_data_free().
 */
GckUriData*
gck_uri_parse (const gchar *string, GckUriFlags flags, GError **error)
{
	GckUriData *uri_data = NULL;
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG i, n_attrs;
	P11KitUri *p11_uri;
	gint res;

	g_return_val_if_fail (string, FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	p11_uri = p11_kit_uri_new ();
	if (!p11_uri)
		g_error ("failed to allocate P11KitUri");

	res = p11_kit_uri_parse (string, flags, p11_uri);
	if (res != P11_KIT_URI_OK) {
		p11_kit_uri_free (p11_uri);
		switch (res) {
		case P11_KIT_URI_NO_MEMORY:
			g_error ("failed to allocate memory in p11_kit_uri_parse()");
			break;
		case P11_KIT_URI_BAD_ENCODING:
			g_set_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING,
				     _("The URI has invalid encoding."));
			break;
		case P11_KIT_URI_BAD_SCHEME:
			g_set_error_literal (error, GCK_URI_ERROR, GCK_URI_BAD_SCHEME,
			                     _("The URI has does not have the 'pkcs11' scheme."));
			break;
		case P11_KIT_URI_BAD_SYNTAX:
			g_set_error_literal (error, GCK_URI_ERROR, GCK_URI_BAD_SYNTAX,
			                     _("The URI has bad syntax."));
			break;
		case P11_KIT_URI_BAD_VERSION:
			g_set_error_literal (error, GCK_URI_ERROR, GCK_URI_BAD_SYNTAX,
			                     _("The URI has a bad version number."));
			break;
		case P11_KIT_URI_NOT_FOUND:
			g_assert_not_reached ();
			break;
		};
		return NULL;
	}

	/* Convert it to a GckUri */
	uri_data = gck_uri_data_new ();
	if (flags & GCK_URI_FOR_MODULE_WITH_VERSION)
		uri_data->module_info = _gck_module_info_from_pkcs11 (p11_kit_uri_get_module_info (p11_uri));
	if (flags & GCK_URI_FOR_TOKEN)
		uri_data->token_info = _gck_token_info_from_pkcs11 (p11_kit_uri_get_token_info (p11_uri));
	if (flags & GCK_URI_FOR_OBJECT) {
		attrs = p11_kit_uri_get_attributes (p11_uri, &n_attrs);
		uri_data->attributes = gck_attributes_new ();
		for (i = 0; i < n_attrs; ++i)
			gck_attributes_add (uri_data->attributes, (GckAttribute*)&attrs[i]);
	}
	uri_data->any_unrecognized = p11_kit_uri_any_unrecognized (p11_uri);

	p11_kit_uri_free (p11_uri);
	return uri_data;
}

/**
 * gck_uri_build:
 * @uri_data: the info to build the URI from.
 * @flags: The context that the URI is for
 *
 * Build a PKCS\#11 URI. The various parts relevant to the flags
 * specified will be used to build the URI.
 *
 * Return value: a newly allocated string containing a PKCS\#11 URI.
 */
gchar*
gck_uri_build (GckUriData *uri_data, GckUriFlags flags)
{
	GckAttribute *attr;
	P11KitUri *p11_uri = 0;
	gchar *string;
	int res;
	guint i;

	g_return_val_if_fail (uri_data, NULL);

	p11_uri = p11_kit_uri_new ();

	if ((flags & GCK_URI_FOR_MODULE_WITH_VERSION) && uri_data->module_info)
		_gck_module_info_to_pkcs11 (uri_data->module_info,
		                            p11_kit_uri_get_module_info (p11_uri));
	if ((flags & GCK_URI_FOR_TOKEN) && uri_data->token_info)
		_gck_token_info_to_pkcs11 (uri_data->token_info,
		                           p11_kit_uri_get_token_info (p11_uri));
	if ((flags & GCK_URI_FOR_OBJECT) && uri_data->attributes) {
		for (i = 0; i < gck_attributes_count (uri_data->attributes); ++i) {
			attr = gck_attributes_at (uri_data->attributes, i);
			res = p11_kit_uri_set_attribute (p11_uri, (CK_ATTRIBUTE_PTR)attr);
			if (res == P11_KIT_URI_NO_MEMORY)
				g_error ("failed to allocate memory in p11_kit_uri_set_attribute()");
			else if (res != P11_KIT_URI_NOT_FOUND && res != P11_KIT_URI_OK)
				g_return_val_if_reached (NULL);
		}
	}

	res = p11_kit_uri_format (p11_uri, flags & GCK_URI_FOR_ANY, &string);
	if (res == P11_KIT_URI_NO_MEMORY)
		g_error ("failed to allocate memory in p11_kit_uri_format()");
	else if (res != P11_KIT_URI_OK)
		g_return_val_if_reached (NULL);

	p11_kit_uri_free (p11_uri);
	return string;
}

/**
 * gck_uri_data_free:
 * @uri_data: URI data to free.
 *
 * Free a #GckUriData.
 */
void
gck_uri_data_free (GckUriData *uri_data)
{
	if (uri_data) {
		if (uri_data->attributes)
			gck_attributes_unref (uri_data->attributes);
		if (uri_data->module_info)
			gck_module_info_free (uri_data->module_info);
		if (uri_data->token_info)
			gck_token_info_free (uri_data->token_info);
		g_slice_free (GckUriData, uri_data);
	}
}
