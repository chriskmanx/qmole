
#include "egg/egg-asn1x.h"
#include "testing/testing.h"

#include <pwd.h>
#include <stdlib.h>
#include <unistd.h>

#include "egg/egg-asn1-defs.h"

#if 0
static void
build_personal_name (void)
{
	ASN1_TYPE asn1_pkix = NULL, asn;
	guchar buffer[10024];
	int res, len;

	res = asn1_array2tree (pkix_asn1_tab, &asn1_pkix, NULL);
	g_assert (res == ASN1_SUCCESS);

	res = asn1_create_element (asn1_pkix, "PKIX1.PersonalName", &asn);
	g_assert (res == ASN1_SUCCESS);

	asn1_write_value (asn, "surname", "Turanga", 7);
	asn1_write_value (asn, "given-name", "Leela", 5);
	asn1_write_value (asn, "initials", NULL, 0);
	asn1_write_value (asn, "generation-qualifier", "Alien", 5);

	len = sizeof (buffer);
	res = asn1_der_coding (asn, "", buffer, &len, NULL);
	g_assert (res == ASN1_SUCCESS);

	asn1_delete_structure (&asn);
	asn1_delete_structure (&asn1_pkix);

	if (!g_file_set_contents ("/tmp/personal-name.der", (gchar*)buffer, len, NULL))
		g_assert (FALSE);

}
#endif

static void
test_some_asn1_stuff (const ASN1_ARRAY_TYPE *defs, const gchar *file, const gchar *identifier)
{
	GNode *asn;
	gpointer data, encoded;
	gsize n_data, n_encoded;

	data = testing_data_read (file, &n_data);
	asn = egg_asn1x_create (defs, identifier);
	egg_asn1x_dump (asn);

	if (!egg_asn1x_decode (asn, data, n_data))
		g_warning ("decode of %s failed: %s", identifier, egg_asn1x_message (asn));

	encoded = egg_asn1x_encode (asn, NULL, &n_encoded);
	if (encoded == NULL)
		g_warning ("encode of %s failed: %s", identifier, egg_asn1x_message (asn));

	/* Decode the encoding */
	if (!egg_asn1x_decode (asn, encoded, n_encoded))
		g_warning ("decode of encoded %s failed: %s", identifier, egg_asn1x_message (asn));

	egg_asn1x_clear (asn);
	egg_asn1x_destroy (asn);
	g_free (encoded);
	g_free (data);
}

static int
run (void)
{

	/* Build up a personal name, which is a set */
#if 0
	build_personal_name ();
#endif


	test_some_asn1_stuff (pkix_asn1_tab, "test-certificate-1.der", "Certificate");
	test_some_asn1_stuff (pkix_asn1_tab, "test-pkcs8-1.der", "pkcs-8-PrivateKeyInfo");
	test_some_asn1_stuff (pk_asn1_tab, "test-rsakey-1.der", "RSAPrivateKey");
	test_some_asn1_stuff (pkix_asn1_tab, "test-personalname-1.der", "PersonalName");
	test_some_asn1_stuff (pkix_asn1_tab, "test-pkcs7-1.der", "pkcs-7-ContentInfo");
	test_some_asn1_stuff (pkix_asn1_tab, "test-pkcs7-2.der", "pkcs-7-ContentInfo");
	test_some_asn1_stuff (pkix_asn1_tab, "test-pkcs12-1.der", "pkcs-12-PFX");

	return 0;
}

#include "testing/testing.c"
