/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-uri.c: Test routines for PKCS#11 URIs

   Copyright (C) 2011, Collabora Ltd.

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

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include "gck/gck.h"
#include "gck/gck-private.h"
#include "gck/gck-test.h"

#include <glib.h>

#include <errno.h>
#include <string.h>

static void
test_parse (void)
{
	GError *error = NULL;
	GckUriData *uri_data;

	uri_data = gck_uri_parse ("pkcs11:", GCK_URI_FOR_MODULE, &error);
	g_assert (uri_data != NULL);
	g_assert_no_error (error);

	g_assert (uri_data->attributes == NULL);
	g_assert (uri_data->token_info == NULL);

	g_assert (uri_data->module_info != NULL);
	g_assert (uri_data->module_info->library_description == NULL);
	g_assert (uri_data->module_info->manufacturer_id == NULL);

	gck_uri_data_free (uri_data);
}

static void
test_parse_bad_scheme (void)
{
	GError *error = NULL;
	GckUriData *uri_data;

	uri_data = gck_uri_parse ("http:\\example.com\test", GCK_URI_FOR_ANY, &error);
	g_assert (uri_data == NULL);
	g_assert_error (error, GCK_URI_ERROR, GCK_URI_BAD_PREFIX);
	g_error_free (error);
}

static void
test_parse_with_label (void)
{
	GError *error = NULL;
	GckUriData *uri_data;
	gchar *value;

	uri_data = gck_uri_parse ("pkcs11:object=Test%20Label", GCK_URI_FOR_ANY, &error);
	g_assert (uri_data != NULL);
	g_assert (uri_data->attributes != NULL);

	if (!gck_attributes_find_string (uri_data->attributes, CKA_LABEL, &value))
		g_assert_not_reached ();

	g_assert_cmpstr (value, ==, "Test Label");
	g_free (value);

	gck_uri_data_free (uri_data);
}

static void
test_parse_with_label_and_klass (void)
{
	GError *error = NULL;
	GckUriData *uri_data;
	gchar *value;
	gulong klass;

	uri_data = gck_uri_parse ("pkcs11:object=Test%20Label;objecttype=cert", GCK_URI_FOR_ANY, &error);
	g_assert (uri_data);
	g_assert (uri_data->attributes);

	if (!gck_attributes_find_string (uri_data->attributes, CKA_LABEL, &value))
		g_assert_not_reached ();

	if (!gck_attributes_find_ulong (uri_data->attributes, CKA_CLASS, &klass))
		g_assert_not_reached ();

	g_assert_cmpstr (value, ==, "Test Label");
	g_assert (klass == CKO_CERTIFICATE);
	g_free (value);

	gck_uri_data_free (uri_data);
}

static void
test_parse_with_id (void)
{
	GError *error = NULL;
	GckAttribute *attr;
	GckUriData *uri_data;

	uri_data = gck_uri_parse ("pkcs11:id=%54%45%53%54%00", GCK_URI_FOR_OBJECT, &error);
	g_assert (uri_data != NULL);
	g_assert (uri_data->attributes != NULL);

	attr = gck_attributes_find (uri_data->attributes, CKA_ID);
	g_assert (attr);
	g_assert (attr->value);
	g_assert (attr->length == 5);
	g_assert (memcmp (attr->value, "TEST", 5) == 0);

	gck_uri_data_free (uri_data);
}

static void
test_parse_with_bad_string_encoding (void)
{
	GError *error = NULL;
	GckUriData *uri_data;

	uri_data = gck_uri_parse ("pkcs11:object=Test%", GCK_URI_FOR_OBJECT, &error);
	g_assert (uri_data == NULL);
	g_assert_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING);
	g_error_free (error);
}

static void
test_parse_with_bad_binary_encoding (void)
{
	GError *error = NULL;
	GckUriData *uri_data;
	uri_data = gck_uri_parse ("pkcs11:id=%%", GCK_URI_FOR_ANY, &error);
	g_assert (!uri_data);
	g_assert_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING);
	g_error_free (error);
}

static void
test_parse_with_token (void)
{
	GError *error = NULL;
	GckUriData *uri_data = NULL;

	uri_data = gck_uri_parse ("pkcs11:token=Token%20Label;serial=3333;model=Deluxe;manufacturer=Me",
	                          GCK_URI_FOR_TOKEN, &error);

	g_assert (uri_data);
	g_assert (uri_data->token_info);
	g_assert_cmpstr (uri_data->token_info->label, ==, "Token Label");
	g_assert_cmpstr (uri_data->token_info->serial_number, ==, "3333");
	g_assert_cmpstr (uri_data->token_info->model, ==, "Deluxe");
	g_assert_cmpstr (uri_data->token_info->manufacturer_id, ==, "Me");
	gck_uri_data_free (uri_data);
}

static void
test_parse_with_token_bad_encoding (void)
{
	GError *error = NULL;
	GckUriData *uri_data;

	uri_data = gck_uri_parse ("pkcs11:token=Token%", GCK_URI_FOR_TOKEN, &error);
	g_assert (!uri_data);
	g_assert_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING);
	g_error_free (error);
}

static void
test_parse_with_bad_syntax (void)
{
	GError *error = NULL;
	GckUriData *uri_data;

	uri_data = gck_uri_parse ("pkcs11:token", GCK_URI_FOR_ANY, &error);
	g_assert (uri_data == NULL);
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_SYNTAX));
	g_error_free (error);
}

static void
test_parse_with_library (void)
{
	GError *error = NULL;
	GckUriData *uri_data = NULL;

	uri_data = gck_uri_parse ("pkcs11:library-description=The%20Library;library-manufacturer=Me",
	                          GCK_URI_FOR_MODULE, &error);

	g_assert (uri_data);
	g_assert (uri_data->module_info);
	g_assert_cmpstr (uri_data->module_info->manufacturer_id, ==, "Me");
	g_assert_cmpstr (uri_data->module_info->library_description, ==, "The Library");
	gck_uri_data_free (uri_data);
}

static void
test_parse_with_library_bad_encoding (void)
{
	GError *error = NULL;
	GckUriData *uri_data;

	uri_data = gck_uri_parse ("pkcs11:library-description=Library%", GCK_URI_FOR_MODULE, &error);
	g_assert (!uri_data);
	g_assert_error (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING);
	g_error_free (error);
}

static void
test_build_empty (void)
{
	GckUriData uri_data;
	gchar *uri;

	memset (&uri_data, 0, sizeof (uri_data));
	uri = gck_uri_build (&uri_data, 0);
	g_assert_cmpstr (uri, ==, "pkcs11:");
	g_free (uri);
}

static void
test_build_with_token_info (void)
{
	gchar *uri = NULL;
	GckUriData uri_data;
	GckUriData *check;

	memset (&uri_data, 0, sizeof (uri_data));
	uri_data.token_info = g_new0 (GckTokenInfo, 1);
	uri_data.token_info->label = g_strdup ("The Label");
	uri_data.token_info->serial_number = g_strdup ("44444");
	uri_data.token_info->manufacturer_id = g_strdup ("Me");
	uri_data.token_info->model = g_strdup ("Deluxe");

	uri = gck_uri_build (&uri_data, GCK_URI_FOR_TOKEN);
	g_assert (uri);

	check = gck_uri_parse (uri, GCK_URI_FOR_TOKEN, NULL);
	g_assert (check);
	g_assert (check->token_info);

	g_assert (_gck_token_info_match (uri_data.token_info, check->token_info));

	gck_token_info_free (uri_data.token_info);
	gck_uri_data_free (check);

	g_assert (g_str_has_prefix (uri, "pkcs11:"));
	g_assert (strstr (uri, "token=The%20Label"));
	g_assert (strstr (uri, "serial=44444"));
	g_assert (strstr (uri, "manufacturer=Me"));
	g_assert (strstr (uri, "model=Deluxe"));

	g_free (uri);
}

static void
test_build_with_token_null_info (void)
{
	gchar *uri = NULL;
	GckUriData uri_data;

	memset (&uri_data, 0, sizeof (uri_data));
	uri_data.token_info = g_new0 (GckTokenInfo, 1);
	uri_data.token_info->label = g_strdup ("The Label");

	uri = gck_uri_build (&uri_data, GCK_URI_FOR_TOKEN);
	g_assert (uri);

	g_assert (g_str_has_prefix (uri, "pkcs11:"));
	g_assert (strstr (uri, "token=The%20Label"));
	g_assert (!strstr (uri, "serial="));

	gck_token_info_free (uri_data.token_info);
	g_free (uri);
}

static void
test_build_with_token_empty_info (void)
{
	gchar *uri = NULL;
	GckUriData uri_data;

	memset (&uri_data, 0, sizeof (uri_data));
	uri_data.token_info = g_new0 (GckTokenInfo, 1);
	uri_data.token_info->label = g_strdup ("The Label");
	uri_data.token_info->serial_number = g_strdup ("");

	uri = gck_uri_build (&uri_data, GCK_URI_FOR_TOKEN);
	g_assert (uri);

	g_assert (g_str_has_prefix (uri, "pkcs11:"));
	g_assert (strstr (uri, "token=The%20Label"));
	g_assert (strstr (uri, "serial="));

	gck_token_info_free (uri_data.token_info);
	g_free (uri);
}

static void
test_build_with_attributes (void)
{
	gchar *uri = NULL;
	GckUriData uri_data;
	GckUriData *check;
	gchar *string;
	gulong value;
	GckAttribute *attr;

	memset (&uri_data, 0, sizeof (uri_data));
	uri_data.attributes = gck_attributes_new ();
	gck_attributes_add_string (uri_data.attributes, CKA_LABEL, "The Label");
	gck_attributes_add_ulong (uri_data.attributes, CKA_CLASS, CKO_DATA);
	gck_attributes_add_data (uri_data.attributes, CKA_ID, "TEST", 5);

	uri = gck_uri_build (&uri_data, GCK_URI_FOR_OBJECT);
	g_assert (uri);

	gck_attributes_unref (uri_data.attributes);

	check = gck_uri_parse (uri, GCK_URI_FOR_ANY, NULL);
	g_assert (check);
	g_assert (check->attributes);

	if (!gck_attributes_find_string (check->attributes, CKA_LABEL, &string))
		g_assert_not_reached ();
	g_assert_cmpstr (string, ==, "The Label");

	if (!gck_attributes_find_ulong (check->attributes, CKA_CLASS, &value))
		g_assert_not_reached ();
	g_assert (value == CKO_DATA);

	attr = gck_attributes_find (check->attributes, CKA_ID);
	g_assert (attr);
	g_assert (attr->length == 5);
	g_assert (memcmp (attr->value, "TEST", 5) == 0);

	gck_uri_data_free (check);

	g_assert (g_str_has_prefix (uri, "pkcs11:"));
	g_assert (strstr (uri, "object=The%20Label"));
	g_assert (strstr (uri, "objecttype=data"));
	g_assert (strstr (uri, "id=TEST%00"));

	g_free (uri);
}

static void
test_parse_private_key (void)
{
	GckUriData *uri_data;
	GError *error = NULL;
	gulong klass;

	uri_data = gck_uri_parse ("pkcs11:objecttype=private", GCK_URI_FOR_OBJECT, &error);
	g_assert (uri_data);
	g_assert_no_error (error);

	g_assert (uri_data->attributes);
	if (!gck_attributes_find_ulong (uri_data->attributes, CKA_CLASS, &klass))
		g_assert_not_reached ();
	gck_assert_cmpulong (klass, ==, CKO_PRIVATE_KEY);

	gck_uri_data_free (uri_data);
}

static void
test_parse_secret_key (void)
{
	GckUriData *uri_data;
	GError *error = NULL;
	gulong klass;

	uri_data = gck_uri_parse ("pkcs11:objecttype=secretkey", GCK_URI_FOR_OBJECT, &error);
	g_assert (uri_data);
	g_assert_no_error (error);

	g_assert (uri_data->attributes);
	if (!gck_attributes_find_ulong (uri_data->attributes, CKA_CLASS, &klass))
		g_assert_not_reached ();
	gck_assert_cmpulong (klass, ==, CKO_SECRET_KEY);

	gck_uri_data_free (uri_data);
}


static void
test_parse_unknown_objecttype (void)
{
	GckUriData *uri_data;
	GError *error = NULL;
	gulong klass;

	uri_data = gck_uri_parse ("pkcs11:objecttype=unknown", GCK_URI_FOR_OBJECT, &error);
	g_assert (uri_data);
	g_assert_no_error (error);

	g_assert (uri_data->attributes);
	g_assert (uri_data->any_unrecognized == TRUE);
	if (gck_attributes_find_ulong (uri_data->attributes, CKA_CLASS, &klass))
		g_assert_not_reached ();

	gck_uri_data_free (uri_data);
}

static void
test_build_objecttype_cert (void)
{
	GckUriData *uri_data;
	gchar *uri;

	uri_data = gck_uri_data_new ();
	uri_data->attributes = gck_attributes_new ();
	gck_attributes_add_ulong (uri_data->attributes, CKA_CLASS, CKO_CERTIFICATE);

	uri = gck_uri_build (uri_data, GCK_URI_FOR_OBJECT);
	g_assert (uri);
	g_assert (strstr (uri, "objecttype=cert"));

	gck_uri_data_free (uri_data);
	g_free (uri);
}

static void
test_build_objecttype_private (void)
{
	GckUriData *uri_data;
	gchar *uri;

	uri_data = gck_uri_data_new ();
	uri_data->attributes = gck_attributes_new ();
	gck_attributes_add_ulong (uri_data->attributes, CKA_CLASS, CKO_PRIVATE_KEY);

	uri = gck_uri_build (uri_data, GCK_URI_FOR_OBJECT);
	g_assert (uri);
	g_assert (strstr (uri, "objecttype=private"));

	gck_uri_data_free (uri_data);
	g_free (uri);
}

static void
test_build_objecttype_public (void)
{
	GckUriData *uri_data;
	gchar *uri;

	uri_data = gck_uri_data_new ();
	uri_data->attributes = gck_attributes_new ();
	gck_attributes_add_ulong (uri_data->attributes, CKA_CLASS, CKO_PUBLIC_KEY);

	uri = gck_uri_build (uri_data, GCK_URI_FOR_OBJECT);
	g_assert (uri);
	g_assert (strstr (uri, "objecttype=public"));

	gck_uri_data_free (uri_data);
	g_free (uri);
}

static void
test_build_objecttype_secret (void)
{
	GckUriData *uri_data;
	gchar *uri;

	uri_data = gck_uri_data_new ();
	uri_data->attributes = gck_attributes_new ();
	gck_attributes_add_ulong (uri_data->attributes, CKA_CLASS, CKO_SECRET_KEY);

	uri = gck_uri_build (uri_data, GCK_URI_FOR_OBJECT);
	g_assert (uri);
	g_assert (strstr (uri, "objecttype=secretkey"));

	gck_uri_data_free (uri_data);
	g_free (uri);
}

static void
test_build_with_library (void)
{
	GckUriData *uri_data;
	gchar *uri;

	uri_data = gck_uri_data_new ();
	uri_data->module_info = g_new0 (GckModuleInfo, 1);
	uri_data->module_info->library_description = g_strdup ("The Description");

	uri = gck_uri_build (uri_data, GCK_URI_FOR_MODULE);
	g_assert (uri);
	g_assert (strstr (uri, "library-description=The%20Description"));

	gck_uri_data_free (uri_data);
	g_free (uri);
}


static void
null_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
                  const gchar *message, gpointer user_data)
{

}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	/* Suppress these messages in tests */
	g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_MESSAGE | G_LOG_LEVEL_INFO | G_LOG_LEVEL_DEBUG,
	                   null_log_handler, NULL);

	g_test_add_func ("/gck/uri/parse", test_parse);
	g_test_add_func ("/gck/uri/parse_bad_scheme", test_parse_bad_scheme);
	g_test_add_func ("/gck/uri/parse_with_label", test_parse_with_label);
	g_test_add_func ("/gck/uri/parse_with_label_and_klass", test_parse_with_label_and_klass);
	g_test_add_func ("/gck/uri/parse_with_id", test_parse_with_id);
	g_test_add_func ("/gck/uri/parse_with_bad_string_encoding", test_parse_with_bad_string_encoding);
	g_test_add_func ("/gck/uri/parse_with_bad_binary_encoding", test_parse_with_bad_binary_encoding);
	g_test_add_func ("/gck/uri/parse_with_token", test_parse_with_token);
	g_test_add_func ("/gck/uri/parse_with_token_bad_encoding", test_parse_with_token_bad_encoding);
	g_test_add_func ("/gck/uri/parse_with_bad_syntax", test_parse_with_bad_syntax);
	g_test_add_func ("/gck/uri/parse_with_library", test_parse_with_library);
	g_test_add_func ("/gck/uri/parse_with_library_bad_encoding", test_parse_with_library_bad_encoding);
	g_test_add_func ("/gck/uri/build_empty", test_build_empty);
	g_test_add_func ("/gck/uri/build_with_token_info", test_build_with_token_info);
	g_test_add_func ("/gck/uri/build_with_token_null_info", test_build_with_token_null_info);
	g_test_add_func ("/gck/uri/build_with_token_empty_info", test_build_with_token_empty_info);
	g_test_add_func ("/gck/uri/build_with_attributes", test_build_with_attributes);
	g_test_add_func ("/gck/uri/parse_private_key", test_parse_private_key);
	g_test_add_func ("/gck/uri/parse_secret_key", test_parse_secret_key);
	g_test_add_func ("/gck/uri/parse_unknown_objecttype", test_parse_unknown_objecttype);
	g_test_add_func ("/gck/uri/build_objecttype_cert", test_build_objecttype_cert);
	g_test_add_func ("/gck/uri/build_objecttype_private", test_build_objecttype_private);
	g_test_add_func ("/gck/uri/build_objecttype_public", test_build_objecttype_public);
	g_test_add_func ("/gck/uri/build_objecttype_secret", test_build_objecttype_secret);
	g_test_add_func ("/gck/uri/build_with_library", test_build_with_library);

	return g_test_run ();
}
