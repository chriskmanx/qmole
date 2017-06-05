/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-pkix-parser.c: Test PKIX parser

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

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "test-suite.h"

#include "gkm/gkm-crypto.h"
#include "gkm/gkm-data-asn1.h"
#include "gkm/gkm-data-der.h"
#include "gkm/gkm-sexp.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-openssl.h"
#include "egg/egg-secure-memory.h"

#include <glib.h>
#include <gcrypt.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static GNode *certificate = NULL;
static guchar *certificate_data = NULL;
static gsize n_certificate_data = 0;

static GNode *certificate2 = NULL;
static guchar *certificate2_data = NULL;
static gsize n_certificate2_data = 0;

const gchar *rsapub = "(public-key (rsa" \
" (n #00AE4B381CF43F7DC24CF90827325E2FB2EB57EDDE29562DF391C8942AA8E6423410E2D3FE26381F9DE0395E74BF2D17621AE46992C72CF895F6FA5FBE98054FBF#)" \
" (e #010001#)))";

const gchar *rsaprv = "(private-key (rsa" \
" (n #00B78758D55EBFFAB61D07D0DC49B5309A6F1DA2AE51C275DFC2370959BB81AC0C39093B1C618E396161A0DECEB8768D0FFB14F197B96C3DA14190EE0F20D51315#)" \
" (e #010001#)" \
" (d #108BCAC5FDD35812981E6EC5957D98E2AB76E4064C47B861D27C2CC322C50792313C852B4164A035B42D261F1A09F9FFE8F477F9F78FF2EABBDA6BA875C671D7#)" \
" (p #00C357F11B19A18C66573D25D1E466D9AB8BCDDCDFE0B2E80BD46712C4BEC18EB7#)" \
" (q #00F0843B90A60EF7034CA4BE80414ED9497CABCC685143B388013FF989CBB0E093#)" \
" (u #12F2555F52EB56329A991CF0404B51C68AC921AD370A797860F550415FF987BD#)))";

const gchar *dsapub = "(public-key (dsa" \
" (p #0090EC0B60735839C754EAF8F64BB03FC35398D69772BFAE540079DEA2D3A61FAFFB27630A038A01A3D0CD62A10745A574A27ECB462F4F0885B79C61BBE954A60A29668AD54BBA5C07A72FD8B1105249670B339DF2C59E64A47064EFCF0B7236C5C72CD55CEB32917430BEC9A003D4E484FBAA84D79571B38D6B5AC95BB73E3F7B#)" \
" (q #00FA214A1385C21BFEBAADAB240A2430C607D56271#)" \
" (g #2DE05751F5DAEE97F3D43C54595A3E94A080728F0C66C98AEBED5762F6AB155802D8359EAD1DE1EC36A459FBEEEA48E59B9E6A8CB4F5295936B3CC881A5D957C7339175E2CFFE0F30D3711E430DB6648C2EB474AA10A4A3297450531FF2C7C6951220C9D446B6B6B0F00262E1EBEB3CC861476AA518CC555C9ABF9E5F39023FC#)" \
" (y #54734451DB79D4EEDF0BBCEBD43BB6CBB7B8584603B957080075DD318EB5B0266D4B20DC5EFF376BDFC4EA2983B1F7F02A39ED4C619ED68712729FFF3B7C696ADD1B6D748F56A4B4BEC5C4385E528423A3B88AE65E6D5500F97839E7A486255982189C3B4FA8D94338C76F0E5CAFC9A30A1ED728BB9F2091D594E3250A09EA00#)))";

const gchar *dsaprv = "(private-key (dsa" \
"  (p #0090EC0B60735839C754EAF8F64BB03FC35398D69772BFAE540079DEA2D3A61FAFFB27630A038A01A3D0CD62A10745A574A27ECB462F4F0885B79C61BBE954A60A29668AD54BBA5C07A72FD8B1105249670B339DF2C59E64A47064EFCF0B7236C5C72CD55CEB32917430BEC9A003D4E484FBAA84D79571B38D6B5AC95BB73E3F7B#)" \
"  (q #00FA214A1385C21BFEBAADAB240A2430C607D56271#)" \
"  (g #2DE05751F5DAEE97F3D43C54595A3E94A080728F0C66C98AEBED5762F6AB155802D8359EAD1DE1EC36A459FBEEEA48E59B9E6A8CB4F5295936B3CC881A5D957C7339175E2CFFE0F30D3711E430DB6648C2EB474AA10A4A3297450531FF2C7C6951220C9D446B6B6B0F00262E1EBEB3CC861476AA518CC555C9ABF9E5F39023FC#)" \
"  (y #54734451DB79D4EEDF0BBCEBD43BB6CBB7B8584603B957080075DD318EB5B0266D4B20DC5EFF376BDFC4EA2983B1F7F02A39ED4C619ED68712729FFF3B7C696ADD1B6D748F56A4B4BEC5C4385E528423A3B88AE65E6D5500F97839E7A486255982189C3B4FA8D94338C76F0E5CAFC9A30A1ED728BB9F2091D594E3250A09EA00#)" \
"  (x #00876F84F709D51108DFB0CBFA1F1C569C09C413EC#)))";

static gboolean
compare_keys (gcry_sexp_t key, gcry_sexp_t sexp)
{
	guchar hash1[20], hash2[20];
	guchar *p;

	/* Now compare them */
	p = gcry_pk_get_keygrip (key, hash1);
	g_assert ("couldn't get key id for private key" && p == hash1);
	p = gcry_pk_get_keygrip (sexp, hash2);
	g_assert ("couldn't get key id for parsed private key" && p == hash2);

	return memcmp (hash1, hash2, 20) == 0;
}

static void
test_der_public (gcry_sexp_t key)
{
	guchar *data;
	gsize n_data;
	GkmDataResult ret;
	gcry_sexp_t sexp;

	/* Encode it */
	data = gkm_data_der_write_public_key (key, &n_data);
	g_assert ("couldn't encode public key" && data != NULL);
	g_assert ("encoding is empty" && n_data > 0);

	/* Now parse it */
	ret = gkm_data_der_read_public_key (data, n_data, &sexp);
	g_assert ("couldn't decode public key" && ret == GKM_DATA_SUCCESS);
	g_assert ("parsed key is empty" && sexp != NULL);

	/* Now compare them */
	g_assert ("key parsed differently" && compare_keys (key, sexp));
}

DEFINE_SETUP(preload)
{
	certificate_data = testing_data_read ("test-certificate-1.der", &n_certificate_data);
	certificate = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", certificate_data, n_certificate_data);
	g_assert (certificate);

	certificate2_data = testing_data_read ("test-certificate-2.der", &n_certificate2_data);
	certificate2 = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", certificate2_data, n_certificate2_data);
	g_assert (certificate2);
}

DEFINE_TEARDOWN(preload)
{
	egg_asn1x_destroy (certificate);
	g_free (certificate_data);
	certificate_data = NULL;

	egg_asn1x_destroy (certificate2);
	g_free (certificate2_data);
	certificate2_data = NULL;
}

DEFINE_TEST(der_rsa_public)
{
	gcry_sexp_t key;
	gcry_error_t gcry;

	gcry = gcry_sexp_sscan (&key, NULL, rsapub, strlen (rsapub));
	g_return_if_fail (gcry == 0);

	test_der_public (key);
}

DEFINE_TEST(der_dsa_public)
{
	gcry_sexp_t key;
	gcry_error_t gcry;

	gcry = gcry_sexp_sscan (&key, NULL, dsapub, strlen (dsapub));
	g_return_if_fail (gcry == 0);

	test_der_public (key);
}

static void
test_der_private (gcry_sexp_t key)
{
	guchar *data;
	gsize n_data;
	GkmDataResult ret;
	gcry_sexp_t sexp;

	/* Encode it */
	data = gkm_data_der_write_private_key (key, &n_data);
	g_assert ("couldn't encode private key" && data != NULL);
	g_assert ("encoding is empty" && n_data > 0);

	/* Now parse it */
	ret = gkm_data_der_read_private_key (data, n_data, &sexp);
	g_assert ("couldn't decode private key" && ret == GKM_DATA_SUCCESS);
	g_assert ("parsed key is empty" && sexp != NULL);

	/* Now compare them */
	g_assert ("key parsed differently" && compare_keys (key, sexp));

	egg_secure_free (data);
}

DEFINE_TEST(der_rsa_private)
{
	gcry_sexp_t key;
	gcry_error_t gcry;

	gcry = gcry_sexp_sscan (&key, NULL, rsaprv, strlen (rsaprv));
	g_return_if_fail (gcry == 0);

	test_der_private (key);
}

DEFINE_TEST(der_dsa_private)
{
	gcry_sexp_t key;
	gcry_error_t gcry;

	gcry = gcry_sexp_sscan (&key, NULL, dsaprv, strlen (dsaprv));
	g_return_if_fail (gcry == 0);

	test_der_private (key);
}

DEFINE_TEST(der_dsa_private_parts)
{
	guchar *params, *key;
	gsize n_params, n_key;
	gcry_sexp_t skey, pkey;
	gcry_error_t gcry;
	GkmDataResult result;

	gcry = gcry_sexp_sscan (&skey, NULL, dsaprv, strlen (dsaprv));
	g_return_if_fail (gcry == 0);

	/* Encode the the dsa key by parts */
	params = gkm_data_der_write_private_key_dsa_params (skey, &n_params);
	g_assert ("didn't encode dsa params" && params != NULL);
	key = gkm_data_der_write_private_key_dsa_part (skey, &n_key);
	g_assert ("didn't encode dsa key" && key != NULL);

	/* Parse the dsa key by parts */
	result = gkm_data_der_read_private_key_dsa_parts (key, n_key, params, n_params, &pkey);
	g_assert ("couldn't parse dsa parts" && result == GKM_DATA_SUCCESS);
	g_assert ("parsing dsa parts resulted in null key" && pkey != NULL);

	/* Now compare them */
	g_assert ("key parsed differently" && compare_keys (skey, pkey));

	egg_secure_free (params);
	egg_secure_free (key);
}

const gchar *certpub = "(public-key (rsa " \
	"(n #00C966D9F80744CFB98C2EF0A1EF13456C05DFDE2716513641116C6C3BEDFE107D129EE59B429AFE6031C366B7733A48AE4ED032379488B50DB6D9F3F244D9D58812DD764DF21AFC6F231E7AF1D898454E0710EF1642D043756D4ADEE2AAC931FF1F00707C66CF102508BAFAEE00E94603662711153BAA5BF298DD3642B2DA8875#) " \
	"(e #010001#) ) )";

DEFINE_TEST(read_public_key_info)
{
	const guchar *data;
	guchar hash[20];
	gsize n_data;
	GkmDataResult res;
	gcry_sexp_t sexp, match;
	gcry_error_t gcry;

	data = egg_asn1x_get_raw_element (egg_asn1x_node (certificate, "tbsCertificate", "subjectPublicKeyInfo", NULL), &n_data);
	g_assert (data);

	res = gkm_data_der_read_public_key_info (data, n_data, &sexp);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (sexp != NULL);

	if (!gcry_pk_get_keygrip (sexp, hash))
		g_assert_not_reached ();

	gcry = gcry_sexp_sscan (&match, NULL, certpub, strlen (certpub));
	g_assert (gcry == 0);

	g_assert (compare_keys (sexp, match));

	gcry_sexp_release (sexp);
	gcry_sexp_release (match);
}

DEFINE_TEST(read_certificate)
{
	GNode *asn = NULL;
	GkmDataResult res;

	res = gkm_data_der_read_certificate (certificate_data, n_certificate_data, &asn);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (asn != NULL);

	egg_asn1x_destroy (asn);
}

DEFINE_TEST(write_certificate)
{
	guchar *data;
	gsize n_data;

	data = gkm_data_der_write_certificate (certificate, &n_data);
	g_assert (data);
	g_assert (n_data == n_certificate_data);
	g_assert (memcmp (data, certificate_data, n_data) == 0);
	g_free (data);
}

static void
on_ca_certificate_public_key_info (GQuark type, const guchar *data, gsize n_data,
                                   GHashTable *headers, gpointer user_data)
{
	GNode *asn1 = NULL;
	GkmDataResult res;
	gpointer keydata;
	gsize n_keydata;
	gcry_sexp_t sexp;

	g_assert (g_quark_try_string ("CERTIFICATE") == type);

	/* Parse the ASN1 data */
	res = gkm_data_der_read_certificate (data, n_data, &asn1);
	g_assert (res == GKM_DATA_SUCCESS);

	/* Generate a raw public key from our certificate */
	keydata = egg_asn1x_encode (egg_asn1x_node (asn1, "tbsCertificate", "subjectPublicKeyInfo", NULL), NULL, &n_keydata);
	g_assert (keydata);

	/* Now create us a nice public key with that identifier */
	res = gkm_data_der_read_public_key_info (keydata, n_keydata, &sexp);
	g_assert (res == GKM_DATA_SUCCESS || res == GKM_DATA_UNRECOGNIZED);

	if (res == GKM_DATA_SUCCESS)
		gcry_sexp_release (sexp);
	g_free (keydata);
}

DEFINE_TEST(read_ca_certificates_public_key_info)
{
	gpointer data;
	gsize n_data;

	data = testing_data_read ("ca-certificates.crt", &n_data);
	egg_openssl_pem_parse (data, n_data, on_ca_certificate_public_key_info, NULL);
	g_free (data);
}

static const guchar*
find_extension (GNode *asn, const guchar *data, gsize n_data, const gchar *oid, gsize *n_extension)
{
	const guchar *value;
	GNode *node = NULL;
	gchar *exoid;
	guint index;
	int len;

	len = strlen (oid);

	for (index = 1; TRUE; ++index) {

		/* Make sure it is present */
		node = egg_asn1x_node (asn, "tbsCertificate", "extensions", index, "extnID", NULL);
		if (node == NULL)
			return NULL;

		exoid = egg_asn1x_get_oid_as_string (node);
		g_assert (exoid);

		if (strcmp (exoid, oid) == 0) {
			g_free (exoid);
			node = egg_asn1x_node (asn, "tbsCertificate", "extensions", index, "extnValue", NULL);
			value = egg_asn1x_get_raw_value (node, n_extension);
			g_assert (value);
			return value;
		}

		g_free (exoid);
	}

	g_assert_not_reached ();
}

DEFINE_TEST(read_basic_constraints)
{
	const guchar *extension;
	gsize n_extension;
	gboolean is_ca;
	gint path_len;
	GkmDataResult res;

	extension = egg_asn1x_get_raw_value (egg_asn1x_node (certificate, "tbsCertificate", "extensions", 1, "extnValue", NULL),
	                                       &n_extension);
	g_assert (extension);

	res = gkm_data_der_read_basic_constraints (extension, n_extension, &is_ca, &path_len);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (is_ca == TRUE);
	g_assert (path_len == -1);
}

DEFINE_TEST(read_key_usage)
{
	const guchar *extension;
	gsize n_extension;
	gulong key_usage;
	GkmDataResult res;

	extension = find_extension (certificate2, certificate2_data, n_certificate2_data, "2.5.29.15", &n_extension);
	g_assert (extension);

	res = gkm_data_der_read_key_usage (extension, n_extension, &key_usage);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert_cmpuint (key_usage, ==, 0x01);
}

DEFINE_TEST(read_enhanced_usage)
{
	const guchar *extension;
	gsize n_extension;
	GQuark *usages;
	GkmDataResult res;

	extension = find_extension (certificate2, certificate2_data, n_certificate2_data, "2.5.29.37", &n_extension);
	g_assert (extension);

	res = gkm_data_der_read_enhanced_usage (extension, n_extension, &usages);
	g_assert (res == GKM_DATA_SUCCESS);

	g_free (usages);
}

DEFINE_TEST(read_all_pkcs8)
{
	gcry_sexp_t sexp;
	GkmDataResult res;
	GDir *dir;
	const gchar *name;
	guchar *data;
	gsize n_data;

	dir = g_dir_open (testing_data_directory (), 0, NULL);
	g_assert (dir);

	for(;;) {
		name = g_dir_read_name (dir);
		if (!name)
			break;

		if (!g_pattern_match_simple ("der-pkcs8-*", name))
			continue;

		data = testing_data_read (name, &n_data);
		res = gkm_data_der_read_private_pkcs8 (data, n_data, "booo", 4, &sexp);
		g_assert (res == GKM_DATA_SUCCESS);

		g_assert (gkm_sexp_parse_key (sexp, NULL, NULL, NULL));
		gcry_sexp_release (sexp);
		g_free (data);
	}

	g_dir_close (dir);
}

DEFINE_TEST(read_pkcs8_bad_password)
{
	gcry_sexp_t sexp;
	GkmDataResult res;
	guchar *data;
	gsize n_data;

	data = testing_data_read ("der-pkcs8-encrypted-pkcs5.key", &n_data);
	res = gkm_data_der_read_private_pkcs8 (data, n_data, "wrong password", 4, &sexp);
	g_assert (res == GKM_DATA_LOCKED);

	g_free (data);
}

DEFINE_TEST(write_pkcs8_plain)
{
	gcry_sexp_t sexp, check;
	gcry_error_t gcry;
	GkmDataResult res;
	guchar *data;
	gsize n_data;

	/* RSA */

	gcry = gcry_sexp_sscan (&sexp, NULL, rsaprv, strlen (rsaprv));
	g_return_if_fail (gcry == 0);

	data = gkm_data_der_write_private_pkcs8_plain (sexp, &n_data);
	g_assert (data);
	g_assert (n_data);

	res = gkm_data_der_read_private_pkcs8_plain (data, n_data, &check);
	egg_secure_free (data);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (check);

	g_assert (compare_keys (sexp, check));
	gcry_sexp_release (sexp);
	gcry_sexp_release (check);


	/* DSA */

	gcry = gcry_sexp_sscan (&sexp, NULL, dsaprv, strlen (dsaprv));
	g_return_if_fail (gcry == 0);

	data = gkm_data_der_write_private_pkcs8_plain (sexp, &n_data);
	g_assert (data);
	g_assert (n_data);

	res = gkm_data_der_read_private_pkcs8_plain (data, n_data, &check);
	egg_secure_free (data);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (check);

	g_assert (compare_keys (sexp, check));
	gcry_sexp_release (sexp);
	gcry_sexp_release (check);
}


DEFINE_TEST(write_pkcs8_encrypted)
{
	gcry_sexp_t sexp, check;
	gcry_error_t gcry;
	GkmDataResult res;
	guchar *data;
	gsize n_data;

	/* RSA */

	gcry = gcry_sexp_sscan (&sexp, NULL, rsaprv, strlen (rsaprv));
	g_return_if_fail (gcry == 0);

	data = gkm_data_der_write_private_pkcs8_crypted (sexp, "testo", 5, &n_data);
	g_assert (data);
	g_assert (n_data);

	res = gkm_data_der_read_private_pkcs8_crypted (data, n_data, "testo", 5, &check);
	g_free (data);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (check);

	g_assert (compare_keys (sexp, check));
	gcry_sexp_release (sexp);
	gcry_sexp_release (check);


	/* DSA */

	gcry = gcry_sexp_sscan (&sexp, NULL, dsaprv, strlen (dsaprv));
	g_return_if_fail (gcry == 0);

	data = gkm_data_der_write_private_pkcs8_crypted (sexp, "testo", 5, &n_data);
	g_assert (data);
	g_assert (n_data);

	res = gkm_data_der_read_private_pkcs8_crypted (data, n_data, "testo", 5, &check);
	g_free (data);
	g_assert (res == GKM_DATA_SUCCESS);
	g_assert (check);

	g_assert (compare_keys (sexp, check));
	gcry_sexp_release (sexp);
	gcry_sexp_release (check);
}
