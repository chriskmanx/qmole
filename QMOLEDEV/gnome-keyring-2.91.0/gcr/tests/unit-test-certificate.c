
#include "config.h"
#include "test-suite.h"

#include "gcr-certificate.h"
#include "gcr-simple-certificate.h"

#include <glib.h>

#include <string.h>

static GcrCertificate *certificate = NULL;

DEFINE_SETUP(certificate)
{
	guchar *contents;
	gsize n_contents;
	
	contents = testing_data_read ("der-certificate.crt", &n_contents);
	certificate = gcr_simple_certificate_new (contents, n_contents);
	g_assert (certificate);
	g_free (contents);
}

DEFINE_TEARDOWN(certificate)
{
	if (certificate)
		g_object_unref (certificate);
	certificate = NULL;
}

DEFINE_TEST(issuer_cn)
{
	gchar *cn = gcr_certificate_get_issuer_cn (certificate);
	g_assert (cn);
	g_assert_cmpstr (cn, ==, "http://www.valicert.com/");
	g_free (cn);
}

DEFINE_TEST(issuer_dn)
{
	gchar *dn = gcr_certificate_get_issuer_dn (certificate);
	g_assert (dn);
	g_assert_cmpstr (dn, ==, "L=ValiCert Validation Network, O=ValiCert, Inc., OU=ValiCert Class 3 Policy Validation Authority, CN=http://www.valicert.com/, EMAIL=info@valicert.com");
	g_free (dn);
}

DEFINE_TEST(issuer_part)
{
	gchar *part = gcr_certificate_get_issuer_part (certificate, "l");
	g_assert (part);
	g_assert_cmpstr (part, ==, "ValiCert Validation Network");
	g_free (part);
}

DEFINE_TEST(subject_cn)
{
	gchar *cn = gcr_certificate_get_subject_cn (certificate);
	g_assert (cn);
	g_assert_cmpstr (cn, ==, "http://www.valicert.com/");
	g_free (cn);
}

DEFINE_TEST(subject_dn)
{
	gchar *dn = gcr_certificate_get_subject_dn (certificate);
	g_assert (dn);
	g_assert_cmpstr (dn, ==, "L=ValiCert Validation Network, O=ValiCert, Inc., OU=ValiCert Class 3 Policy Validation Authority, CN=http://www.valicert.com/, EMAIL=info@valicert.com");
	g_free (dn);
}

DEFINE_TEST(subject_part)
{
	gchar *part = gcr_certificate_get_subject_part (certificate, "OU");
	g_assert (part);
	g_assert_cmpstr (part, ==, "ValiCert Class 3 Policy Validation Authority");
	g_free (part);
}

DEFINE_TEST(issued_date)
{
	GDate *date = gcr_certificate_get_issued_date (certificate);
	g_assert (date);
	g_assert_cmpuint (g_date_get_year (date), ==, 1999);
	g_assert_cmpuint (g_date_get_month (date), ==, 6);
	g_assert_cmpuint (g_date_get_day (date), ==, 26);
	g_date_free (date);
}

DEFINE_TEST(expiry_date)
{
	GDate *date = gcr_certificate_get_expiry_date (certificate);
	g_assert (date);
	g_assert_cmpuint (g_date_get_year (date), ==, 2019);
	g_assert_cmpuint (g_date_get_month (date), ==, 6);
	g_assert_cmpuint (g_date_get_day (date), ==, 26);
	g_date_free (date);
}

DEFINE_TEST(serial_number)
{
	gsize n_serial;
	guchar *serial;
	gchar *hex;
	
	serial = gcr_certificate_get_serial_number (certificate, &n_serial);
	g_assert (serial);
	g_assert_cmpuint (n_serial, ==, 1);
	g_assert (memcmp (serial, "\1", n_serial) == 0);
	g_free (serial);

	hex = gcr_certificate_get_serial_number_hex (certificate);
	g_assert (hex);
	g_assert_cmpstr (hex, ==, "01");
	g_free (hex);
}

DEFINE_TEST(fingerprint)
{
	gsize n_print;
	guchar *print = gcr_certificate_get_fingerprint (certificate, G_CHECKSUM_MD5, &n_print);
	g_assert (print);
	g_assert_cmpuint (n_print, ==, g_checksum_type_get_length (G_CHECKSUM_MD5));
	g_assert (memcmp (print, "\xa2\x6f\x53\xb7\xee\x40\xdb\x4a\x68\xe7\xfa\x18\xd9\x10\x4b\x72", n_print) == 0);
	g_free (print);
}

DEFINE_TEST(fingerprint_hex)
{
	gchar *print = gcr_certificate_get_fingerprint_hex (certificate, G_CHECKSUM_MD5);
	g_assert (print);
	g_assert_cmpstr (print, ==, "A2 6F 53 B7 EE 40 DB 4A 68 E7 FA 18 D9 10 4B 72");
	g_free (print);
}

