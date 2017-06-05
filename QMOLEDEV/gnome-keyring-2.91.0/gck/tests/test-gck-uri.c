
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gck-test.h"
#include "gck-private.h"

DEFINE_SETUP(uri)
{

}

DEFINE_TEARDOWN(uri)
{

}

DEFINE_TEST(uri_parse)
{
	GError *error = NULL;
	if (!gck_uri_parse ("pkcs11:", NULL, NULL, &error))
		g_assert_not_reached ();
}

DEFINE_TEST(uri_parse_bad_scheme)
{
	GError *error = NULL;
	if (gck_uri_parse ("http:\\example.com\test", NULL, NULL, &error))
		g_assert_not_reached ();
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_PREFIX));
	g_error_free (error);
}

DEFINE_TEST(uri_parse_with_label)
{
	GError *error = NULL;
	GckAttributes *attrs;
	gchar *value;

	if (!gck_uri_parse ("pkcs11:object=Test%20Label", NULL, &attrs, &error))
		g_assert_not_reached ();

	if (!gck_attributes_find_string (attrs, CKA_LABEL, &value))
		g_assert_not_reached ();

	g_assert_cmpstr (value, ==, "Test Label");
	g_free (value);
}
DEFINE_TEST(uri_parse_with_label_and_klass)
{
	GError *error = NULL;
	GckAttributes *attrs;
	gchar *value;
	gulong klass;

	if (!gck_uri_parse ("pkcs11:object=Test%20Label;objecttype=cert", NULL, &attrs, &error))
		g_assert_not_reached ();

	if (!gck_attributes_find_string (attrs, CKA_LABEL, &value))
		g_assert_not_reached ();

	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &klass))
		g_assert_not_reached ();

	g_assert_cmpstr (value, ==, "Test Label");
	g_assert (klass == CKO_CERTIFICATE);
	g_free (value);
}

DEFINE_TEST(uri_parse_with_id)
{
	GError *error = NULL;
	GckAttributes *attrs;
	GckAttribute *attr;

	if (!gck_uri_parse ("pkcs11:id=54:45:53:54", NULL, &attrs, &error))
		g_assert_not_reached ();

	attr = gck_attributes_find (attrs, CKA_ID);
	g_assert (attr);
	g_assert (attr->value);
	g_assert (attr->length == 4);
	g_assert (memcmp (attr->value, "TEST", 4) == 0);

	gck_attributes_unref (attrs);
}

DEFINE_TEST(uri_parse_with_bad_string_encoding)
{
	GError *error = NULL;
	if (gck_uri_parse ("pkcs11:object=Test%", NULL, NULL, &error))
		g_assert_not_reached ();
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING));
	g_error_free (error);
}

DEFINE_TEST(uri_parse_with_bad_binary_encoding)
{
	GError *error = NULL;
	if (gck_uri_parse ("pkcs11:id=xxxxx", NULL, NULL, &error))
		g_assert_not_reached ();
	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING));
	g_error_free (error);
}

DEFINE_TEST(uri_parse_with_token)
{
	GError *error = NULL;
	GckTokenInfo *token = NULL;

	if (!gck_uri_parse ("pkcs11:token=Token%20Label;serial=3333;model=Deluxe;manufacturer=Me",
	                    &token, NULL, &error))
		g_assert_not_reached ();

	g_assert (token);
	g_assert_cmpstr (token->label, ==, "Token Label");
	g_assert_cmpstr (token->serial_number, ==, "3333");
	g_assert_cmpstr (token->model, ==, "Deluxe");
	g_assert_cmpstr (token->manufacturer_id, ==, "Me");
	gck_token_info_free (token);
}

DEFINE_TEST(uri_parse_with_token_bad_encoding)
{
	GError *error = NULL;

	if (gck_uri_parse ("pkcs11:token=Token%", NULL, NULL, &error))
		g_assert_not_reached ();

	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_ENCODING));
	g_error_free (error);
}

DEFINE_TEST(uri_parse_with_bad_syntax)
{
	GError *error = NULL;

	if (gck_uri_parse ("pkcs11:token", NULL, NULL, &error))
		g_assert_not_reached ();

	g_assert (g_error_matches (error, GCK_URI_ERROR, GCK_URI_BAD_SYNTAX));
	g_error_free (error);
}

DEFINE_TEST(uri_build_empty)
{
	gchar *uri = NULL;

	uri = gck_uri_build (NULL, NULL);
	g_assert_cmpstr (uri, ==, "pkcs11:");
	g_free (uri);
}

DEFINE_TEST(uri_build_with_token_info)
{
	gchar *uri = NULL;
	GckTokenInfo *token;
	GckTokenInfo *check;

	token = g_new0 (GckTokenInfo, 1);
	token->label = g_strdup ("The Label");
	token->serial_number = g_strdup ("44444");
	token->manufacturer_id = g_strdup ("Me");
	token->model = g_strdup ("Deluxe");

	uri = gck_uri_build (token, NULL);
	g_assert (uri);

	if (!gck_uri_parse (uri, &check, NULL, NULL))
		g_assert_not_reached ();

	g_assert (_gck_token_info_match (token, check));

	gck_token_info_free (check);
	gck_token_info_free (token);

	g_assert (g_str_has_prefix (uri, "pkcs11:"));
	g_assert (strstr (uri, "token=The%20Label"));
	g_assert (strstr (uri, "serial=44444"));
	g_assert (strstr (uri, "manufacturer=Me"));
	g_assert (strstr (uri, "model=Deluxe"));

	g_free (uri);
}

DEFINE_TEST(uri_build_with_attributes)
{
	gchar *uri = NULL;
	GckAttributes *attrs;
	gchar *string;
	gulong value;
	GckAttribute *attr;

	attrs = gck_attributes_new ();
	gck_attributes_add_string (attrs, CKA_LABEL, "The Label");
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_data (attrs, CKA_ID, "TEST", 4);

	uri = gck_uri_build (NULL, attrs);
	g_assert (uri);

	gck_attributes_unref (attrs);

	if (!gck_uri_parse (uri, NULL, &attrs, NULL))
		g_assert_not_reached ();

	if (!gck_attributes_find_string (attrs, CKA_LABEL, &string))
		g_assert_not_reached ();
	g_assert_cmpstr (string, ==, "The Label");

	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &value))
		g_assert_not_reached ();
	g_assert (value == CKO_DATA);

	attr = gck_attributes_find (attrs, CKA_ID);
	g_assert (attr);
	g_assert (attr->length == 4);
	g_assert (memcmp (attr->value, "TEST", 4) == 0);

	gck_attributes_unref (attrs);

	g_assert (g_str_has_prefix (uri, "pkcs11:"));
	g_assert (strstr (uri, "object=The%20Label"));
	g_assert (strstr (uri, "objecttype=data"));
	g_assert (strstr (uri, "id=54:45:53:54"));

	g_free (uri);
}
