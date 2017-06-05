/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-data-der.c - parsing and serializing of common crypto DER structures

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

#include "gkm-data-asn1.h"
#include "gkm-data-der.h"
#include "gkm-data-types.h"
#include "gkm-sexp.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-secure-memory.h"
#include "egg/egg-symkey.h"

#include <glib.h>
#include <gcrypt.h>

/* -----------------------------------------------------------------------------
 * QUARKS
 */

static GQuark OID_PKIX1_RSA;
static GQuark OID_PKIX1_DSA;
static GQuark OID_PKCS12_PBE_3DES_SHA1;

static void
init_quarks (void)
{
	static volatile gsize quarks_inited = 0;

	if (g_once_init_enter (&quarks_inited)) {

		#define QUARK(name, value) \
			name = g_quark_from_static_string(value)

		QUARK (OID_PKIX1_RSA, "1.2.840.113549.1.1.1");
		QUARK (OID_PKIX1_DSA, "1.2.840.10040.4.1");
		QUARK (OID_PKCS12_PBE_3DES_SHA1, "1.2.840.113549.1.12.1.3");

		#undef QUARK

		g_once_init_leave (&quarks_inited, 1);
	}
}

/* -----------------------------------------------------------------------------
 * KEY PARSING
 */

#define SEXP_PUBLIC_RSA  \
	"(public-key"    \
	"  (rsa"         \
	"    (n %m)"     \
	"    (e %m)))"

GkmDataResult
gkm_data_der_read_public_key_rsa (const guchar *data, gsize n_data, gcry_sexp_t *s_key)
{
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	GNode *asn = NULL;
	gcry_mpi_t n, e;
	int res;

	n = e = NULL;

	asn = egg_asn1x_create_and_decode (pk_asn1_tab, "RSAPublicKey", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (!gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "modulus", NULL), &n) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "publicExponent", NULL), &e))
		goto done;

	res = gcry_sexp_build (s_key, NULL, SEXP_PUBLIC_RSA, n, e);
	if (res)
		goto done;

	g_assert (*s_key);
	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (n);
	gcry_mpi_release (e);

	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid RSA public key");

	return ret;
}

#define SEXP_PRIVATE_RSA  \
	"(private-key"   \
	"  (rsa"         \
	"    (n %m)"     \
	"    (e %m)"     \
	"    (d %m)"     \
	"    (p %m)"     \
	"    (q %m)"     \
	"    (u %m)))"

GkmDataResult
gkm_data_der_read_private_key_rsa (const guchar *data, gsize n_data, gcry_sexp_t *s_key)
{
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	gcry_mpi_t n, e, d, p, q, u;
	gcry_mpi_t tmp;
	gulong version;
	GNode *asn = NULL;
	int res;

	n = e = d = p = q = u = NULL;

	asn = egg_asn1x_create_and_decode (pk_asn1_tab, "RSAPrivateKey", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (!egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "version", NULL), &version))
		goto done;

	/* We only support simple version */
	if (version != 0) {
		ret = GKM_DATA_UNRECOGNIZED;
		g_message ("unsupported version of RSA key: %lu", version);
		goto done;
	}

	if (!gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "modulus", NULL), &n) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "publicExponent", NULL), &e) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "privateExponent", NULL), &d) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "prime1", NULL), &p) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "prime2", NULL), &q) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "coefficient", NULL), &u))
		goto done;

	/* Fix up the incoming key so gcrypt likes it */
	if (gcry_mpi_cmp (p, q) > 0) {
		/* P shall be smaller then Q!  Swap primes.  iqmp becomes u.  */
		tmp = p;
		p = q;
		q = tmp;
	} else {
		/* U needs to be recomputed.  */
		gcry_mpi_invm (u, p, q);
	}

	res = gcry_sexp_build (s_key, NULL, SEXP_PRIVATE_RSA, n, e, d, p, q, u);
	if (res)
		goto done;

	g_assert (*s_key);
	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (n);
	gcry_mpi_release (e);
	gcry_mpi_release (d);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (u);

	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid RSA key");

	return ret;
}

#define SEXP_PUBLIC_DSA  \
	"(public-key"   \
	"  (dsa"         \
	"    (p %m)"     \
	"    (q %m)"     \
	"    (g %m)"     \
	"    (y %m)))"

GkmDataResult
gkm_data_der_read_public_key_dsa (const guchar *data, gsize n_data, gcry_sexp_t *s_key)
{
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	GNode *asn = NULL;
	gcry_mpi_t p, q, g, y;
	int res;

	p = q = g = y = NULL;

	asn = egg_asn1x_create_and_decode (pk_asn1_tab, "DSAPublicKey", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (!gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "p", NULL), &p) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "q", NULL), &q) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "g", NULL), &g) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "Y", NULL), &y))
		goto done;

	res = gcry_sexp_build (s_key, NULL, SEXP_PUBLIC_DSA, p, q, g, y);
	if (res)
		goto done;

	g_assert (*s_key);
	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);

	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid public DSA key");

	return ret;
}

GkmDataResult
gkm_data_der_read_public_key_dsa_parts (const guchar *keydata, gsize n_keydata,
                                        const guchar *params, gsize n_params,
                                        gcry_sexp_t *s_key)
{
	gcry_mpi_t p, q, g, y;
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	GNode *asn_params = NULL;
	GNode *asn_key = NULL;
	int res;

	p = q = g = y = NULL;

	asn_params = egg_asn1x_create_and_decode (pk_asn1_tab, "DSAParameters", params, n_params);
	asn_key = egg_asn1x_create_and_decode (pk_asn1_tab, "DSAPublicPart", keydata, n_keydata);
	if (!asn_params || !asn_key)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (!gkm_data_asn1_read_mpi (egg_asn1x_node (asn_params, "p", NULL), &p) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn_params, "q", NULL), &q) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn_params, "g", NULL), &g))
		goto done;

	if (!gkm_data_asn1_read_mpi (asn_key, &y))
		goto done;

	res = gcry_sexp_build (s_key, NULL, SEXP_PUBLIC_DSA, p, q, g, y);
	if (res)
		goto done;

	g_assert (*s_key);
	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn_key);
	egg_asn1x_destroy (asn_params);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);

	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid DSA key");

	return ret;
}

#define SEXP_PRIVATE_DSA  \
	"(private-key"   \
	"  (dsa"         \
	"    (p %m)"     \
	"    (q %m)"     \
	"    (g %m)"     \
	"    (y %m)"     \
	"    (x %m)))"

GkmDataResult
gkm_data_der_read_private_key_dsa (const guchar *data, gsize n_data, gcry_sexp_t *s_key)
{
	gcry_mpi_t p, q, g, y, x;
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	int res;
	GNode *asn = NULL;

	p = q = g = y = x = NULL;

	asn = egg_asn1x_create_and_decode (pk_asn1_tab, "DSAPrivateKey", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (!gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "p", NULL), &p) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "q", NULL), &q) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "g", NULL), &g) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "Y", NULL), &y) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn, "priv", NULL), &x))
		goto done;

	res = gcry_sexp_build (s_key, NULL, SEXP_PRIVATE_DSA, p, q, g, y, x);
	if (res)
		goto done;

	g_assert (*s_key);
	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);
	gcry_mpi_release (x);

	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid DSA key");

	return ret;
}

GkmDataResult
gkm_data_der_read_private_key_dsa_parts (const guchar *keydata, gsize n_keydata,
                                         const guchar *params, gsize n_params,
                                         gcry_sexp_t *s_key)
{
	gcry_mpi_t p, q, g, y, x;
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	int res;
	GNode *asn_params = NULL;
	GNode *asn_key = NULL;

	p = q = g = y = x = NULL;

	asn_params = egg_asn1x_create_and_decode (pk_asn1_tab, "DSAParameters", params, n_params);
	asn_key = egg_asn1x_create_and_decode (pk_asn1_tab, "DSAPrivatePart", keydata, n_keydata);
	if (!asn_params || !asn_key)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (!gkm_data_asn1_read_mpi (egg_asn1x_node (asn_params, "p", NULL), &p) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn_params, "q", NULL), &q) ||
	    !gkm_data_asn1_read_mpi (egg_asn1x_node (asn_params, "g", NULL), &g))
		goto done;

	if (!gkm_data_asn1_read_mpi (asn_key, &x))
		goto done;

	/* Now we calculate y */
	y = gcry_mpi_snew (1024);
	gcry_mpi_powm (y, g, x, p);

	res = gcry_sexp_build (s_key, NULL, SEXP_PRIVATE_DSA, p, q, g, y, x);
	if (res)
		goto done;

	g_assert (*s_key);
	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn_key);
	egg_asn1x_destroy (asn_params);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);
	gcry_mpi_release (x);

	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid DSA key");

	return ret;
}

GkmDataResult
gkm_data_der_read_public_key (const guchar *data, gsize n_data, gcry_sexp_t *s_key)
{
	GkmDataResult res;

	res = gkm_data_der_read_public_key_rsa (data, n_data, s_key);
	if (res == GKM_DATA_UNRECOGNIZED)
		res = gkm_data_der_read_public_key_dsa (data, n_data, s_key);

	return res;
}

GkmDataResult
gkm_data_der_read_public_key_info (const guchar* data, gsize n_data, gcry_sexp_t* s_key)
{
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	GQuark oid;
	GNode *asn = NULL;
	gsize n_params;
	guint n_bits;
	const guchar *params;
	guchar *key = NULL;

	init_quarks ();

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "SubjectPublicKeyInfo", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	/* Figure out the algorithm */
	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "algorithm", "algorithm", NULL));
	if (!oid)
		goto done;

	/* A bit string so we cannot process in place */
	key = egg_asn1x_get_bits_as_raw (egg_asn1x_node (asn, "subjectPublicKey", NULL), NULL, &n_bits);
	if (!key)
		goto done;

	/* An RSA key is simple */
	if (oid == OID_PKIX1_RSA) {
		ret = gkm_data_der_read_public_key_rsa (key, n_bits / 8, s_key);

	/* A DSA key paramaters are stored separately */
	} else if (oid == OID_PKIX1_DSA) {
		params = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "algorithm", "parameters", NULL), &n_params);
		if (!params)
			goto done;
		ret = gkm_data_der_read_public_key_dsa_parts (key, n_bits / 8, params, n_params, s_key);

	} else {
		g_message ("unsupported key algorithm in certificate: %s", g_quark_to_string (oid));
		ret = GKM_DATA_UNRECOGNIZED;
		goto done;
	}

done:
	egg_asn1x_destroy (asn);
	g_free (key);

	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid subject public-key info");

	return ret;
}

GkmDataResult
gkm_data_der_read_private_key (const guchar *data, gsize n_data, gcry_sexp_t *s_key)
{
	GkmDataResult res;

	res = gkm_data_der_read_private_key_rsa (data, n_data, s_key);
	if (res == GKM_DATA_UNRECOGNIZED)
		res = gkm_data_der_read_private_key_dsa (data, n_data, s_key);

	return res;
}

GkmDataResult
gkm_data_der_read_private_pkcs8_plain (const guchar *data, gsize n_data, gcry_sexp_t *s_key)
{
	GNode *asn = NULL;
	GkmDataResult ret;
	int algorithm;
	GQuark key_algo;
	const guchar *keydata;
	gsize n_keydata;
	const guchar *params;
	gsize n_params;

	ret = GKM_DATA_UNRECOGNIZED;

	init_quarks ();

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "pkcs-8-PrivateKeyInfo", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;
	algorithm = 0;

	key_algo = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "privateKeyAlgorithm", "algorithm", NULL));
	if (!key_algo)
		goto done;
	else if (key_algo == OID_PKIX1_RSA)
		algorithm = GCRY_PK_RSA;
	else if (key_algo == OID_PKIX1_DSA)
		algorithm = GCRY_PK_DSA;

	if (!algorithm) {
		ret = GKM_DATA_UNRECOGNIZED;
		goto done;
	}

	keydata = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "privateKey", NULL), &n_keydata);
	if (!keydata)
		goto done;

	params = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "privateKeyAlgorithm", "parameters", NULL),
	                                    &n_params);

	ret = GKM_DATA_SUCCESS;

done:
	if (ret == GKM_DATA_SUCCESS) {
		switch (algorithm) {
		case GCRY_PK_RSA:
			ret = gkm_data_der_read_private_key_rsa (keydata, n_keydata, s_key);
			break;
		case GCRY_PK_DSA:
			/* Try the normal one block format */
			ret = gkm_data_der_read_private_key_dsa (keydata, n_keydata, s_key);

			/* Otherwise try the two part format that everyone seems to like */
			if (ret == GKM_DATA_UNRECOGNIZED && params && n_params)
				ret = gkm_data_der_read_private_key_dsa_parts (keydata, n_keydata,
				                                               params, n_params, s_key);
			break;
		default:
			g_message ("invalid or unsupported key type in PKCS#8 key");
			ret = GKM_DATA_UNRECOGNIZED;
			break;
		};

	} else if (ret == GKM_DATA_FAILURE) {
		g_message ("invalid PKCS#8 key");
	}

	egg_asn1x_destroy (asn);
	return ret;
}

GkmDataResult
gkm_data_der_read_private_pkcs8_crypted (const guchar *data, gsize n_data, const gchar *password,
                                         gsize n_password, gcry_sexp_t *s_key)
{
	GNode *asn = NULL;
	gcry_cipher_hd_t cih = NULL;
	gcry_error_t gcry;
	GkmDataResult ret, r;
	GQuark scheme;
	guchar *crypted = NULL;
	const guchar *params;
	gsize n_crypted, n_params;
	gint l;

	init_quarks ();

	ret = GKM_DATA_UNRECOGNIZED;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "pkcs-8-EncryptedPrivateKeyInfo", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	/* Figure out the type of encryption */
	scheme = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "encryptionAlgorithm", "algorithm", NULL));
	if (!scheme)
		goto done;

	params = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "encryptionAlgorithm", "parameters", NULL), &n_params);
	if (!params)
		goto done;

	/*
	 * Parse the encryption stuff into a cipher.
	 */
	r = egg_symkey_read_cipher (scheme, password, n_password, params, n_params, &cih);
	if (r == GKM_DATA_UNRECOGNIZED) {
		ret = GKM_DATA_FAILURE;
		goto done;
	} else if (r != GKM_DATA_SUCCESS) {
		ret = r;
		goto done;
	}

	crypted = egg_asn1x_get_string_as_raw (egg_asn1x_node (asn, "encryptedData", NULL),
	                                       egg_secure_realloc, &n_crypted);
	if (!crypted)
		goto done;

	gcry = gcry_cipher_decrypt (cih, crypted, n_crypted, NULL, 0);
	gcry_cipher_close (cih);
	cih = NULL;

	if (gcry != 0) {
		g_warning ("couldn't decrypt pkcs8 data: %s", gcry_strerror (gcry));
		goto done;
	}

	/* Unpad the DER data */
	l = egg_asn1x_element_length (crypted, n_crypted);
	if (l <= 0 || l > n_crypted) {
		ret = GKM_DATA_LOCKED;
		goto done;
	}
	n_crypted = l;

	/* Try to parse the resulting key */
	ret = gkm_data_der_read_private_pkcs8_plain (crypted, n_crypted, s_key);
	egg_secure_free (crypted);
	crypted = NULL;

	/* If unrecognized we assume bad password */
	if (ret == GKM_DATA_UNRECOGNIZED)
		ret = GKM_DATA_LOCKED;

done:
	if (cih)
		gcry_cipher_close (cih);
	egg_asn1x_destroy (asn);
	egg_secure_free (crypted);

	return ret;
}

GkmDataResult
gkm_data_der_read_private_pkcs8 (const guchar *data, gsize n_data, const gchar *password,
                                 gsize n_password, gcry_sexp_t *s_key)
{
	GkmDataResult res;

	res = gkm_data_der_read_private_pkcs8_crypted (data, n_data, password, n_password, s_key);
	if (res == GKM_DATA_UNRECOGNIZED)
		res = gkm_data_der_read_private_pkcs8_plain (data, n_data, s_key);

	return res;
}

guchar*
gkm_data_der_write_public_key_rsa (gcry_sexp_t s_key, gsize *len)
{
	GNode *asn = NULL;
	gcry_mpi_t n, e;
	guchar *result = NULL;

	n = e = NULL;

	asn = egg_asn1x_create (pk_asn1_tab, "RSAPublicKey");
	g_return_val_if_fail (asn, NULL);

	if (!gkm_sexp_extract_mpi (s_key, &n, "rsa", "n", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &e, "rsa", "e", NULL))
		goto done;

	if (!gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "modulus", NULL), n) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "publicExponent", NULL), e))
		goto done;

	result = egg_asn1x_encode (asn, NULL, len);

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (n);
	gcry_mpi_release (e);

	return result;
}

guchar*
gkm_data_der_write_private_key_rsa (gcry_sexp_t s_key, gsize *n_key)
{
	GNode *asn = NULL;
	gcry_mpi_t n, e, d, p, q, u, e1, e2, tmp;
	guchar *result = NULL;

	n = e = d = p = q = u = e1 = e2 = tmp = NULL;

	asn = egg_asn1x_create (pk_asn1_tab, "RSAPrivateKey");
	g_return_val_if_fail (asn, NULL);

	if (!gkm_sexp_extract_mpi (s_key, &n, "rsa", "n", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &e, "rsa", "e", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &d, "rsa", "d", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &p, "rsa", "p", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &q, "rsa", "q", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &u, "rsa", "u", NULL))
		goto done;

	if (!gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "modulus", NULL), n) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "publicExponent", NULL), e) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "privateExponent", NULL), d) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "prime1", NULL), p) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "prime2", NULL), q) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "coefficient", NULL), u))
		goto done;

	/* Calculate e1 and e2 */
	tmp = gcry_mpi_snew (1024);
	gcry_mpi_sub_ui (tmp, p, 1);
	e1 = gcry_mpi_snew (1024);
	gcry_mpi_mod (e1, d, tmp);
	gcry_mpi_sub_ui (tmp, q, 1);
	e2 = gcry_mpi_snew (1024);
	gcry_mpi_mod (e2, d, tmp);

	/* Write out calculated */
	if (!gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "exponent1", NULL), e1) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "exponent2", NULL), e2))
		goto done;

	/* Write out the version */
	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn, "version", NULL), 0))
		goto done;

	result = egg_asn1x_encode (asn, egg_secure_realloc, n_key);

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (n);
	gcry_mpi_release (e);
	gcry_mpi_release (d);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (u);

	gcry_mpi_release (tmp);
	gcry_mpi_release (e1);
	gcry_mpi_release (e2);

	return result;
}

guchar*
gkm_data_der_write_public_key_dsa (gcry_sexp_t s_key, gsize *len)
{
	GNode *asn = NULL;
	gcry_mpi_t p, q, g, y;
	guchar *result = NULL;

	p = q = g = y = NULL;

	asn = egg_asn1x_create (pk_asn1_tab, "DSAPublicKey");
	g_return_val_if_fail (asn, NULL);

	if (!gkm_sexp_extract_mpi (s_key, &p, "dsa", "p", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &q, "dsa", "q", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &g, "dsa", "g", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &y, "dsa", "y", NULL))
		goto done;

	if (!gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "p", NULL), p) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "q", NULL), q) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "g", NULL), g) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "Y", NULL), y))
		goto done;

	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn, "version", NULL), 0))
		goto done;

	result = egg_asn1x_encode (asn, NULL, len);

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);

	return result;
}

guchar*
gkm_data_der_write_private_key_dsa_part (gcry_sexp_t skey, gsize *n_key)
{
	GNode *asn = NULL;
	gcry_mpi_t x;
	guchar *result = NULL;

	x = NULL;

	asn = egg_asn1x_create (pk_asn1_tab, "DSAPrivatePart");
	g_return_val_if_fail (asn, NULL);

	if (!gkm_sexp_extract_mpi (skey, &x, "dsa", "x", NULL))
		goto done;

	if (!gkm_data_asn1_write_mpi (asn, x))
		goto done;

	result = egg_asn1x_encode (asn, egg_secure_realloc, n_key);

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (x);

	return result;
}

guchar*
gkm_data_der_write_private_key_dsa_params (gcry_sexp_t skey, gsize *n_params)
{
	GNode *asn = NULL;
	gcry_mpi_t p, q, g;
	guchar *result = NULL;

	p = q = g = NULL;

	asn = egg_asn1x_create (pk_asn1_tab, "DSAParameters");
	g_return_val_if_fail (asn, NULL);

	if (!gkm_sexp_extract_mpi (skey, &p, "dsa", "p", NULL) ||
	    !gkm_sexp_extract_mpi (skey, &q, "dsa", "q", NULL) ||
	    !gkm_sexp_extract_mpi (skey, &g, "dsa", "g", NULL))
		goto done;

	if (!gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "p", NULL), p) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "q", NULL), q) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "g", NULL), g))
		goto done;

	result = egg_asn1x_encode (asn, egg_secure_realloc, n_params);

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);

	return result;
}

guchar*
gkm_data_der_write_private_key_dsa (gcry_sexp_t s_key, gsize *len)
{
	GNode *asn = NULL;
	gcry_mpi_t p, q, g, y, x;
	guchar *result = NULL;

	p = q = g = y = x = NULL;

	asn = egg_asn1x_create (pk_asn1_tab, "DSAPrivateKey");
	g_return_val_if_fail (asn, NULL);

	if (!gkm_sexp_extract_mpi (s_key, &p, "dsa", "p", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &q, "dsa", "q", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &g, "dsa", "g", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &y, "dsa", "y", NULL) ||
	    !gkm_sexp_extract_mpi (s_key, &x, "dsa", "x", NULL))
		goto done;

	if (!gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "p", NULL), p) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "q", NULL), q) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "g", NULL), g) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "Y", NULL), y) ||
	    !gkm_data_asn1_write_mpi (egg_asn1x_node (asn, "priv", NULL), x))
		goto done;

	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn, "version", NULL), 0))
		goto done;

	result = egg_asn1x_encode (asn, egg_secure_realloc, len);

done:
	egg_asn1x_destroy (asn);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);
	gcry_mpi_release (x);

	return result;
}

guchar*
gkm_data_der_write_public_key (gcry_sexp_t s_key, gsize *len)
{
	gboolean is_priv;
	int algorithm;

	g_return_val_if_fail (s_key != NULL, NULL);

	if (!gkm_sexp_parse_key (s_key, &algorithm, &is_priv, NULL))
		g_return_val_if_reached (NULL);

	g_return_val_if_fail (!is_priv, NULL);

	switch (algorithm) {
	case GCRY_PK_RSA:
		return gkm_data_der_write_public_key_rsa (s_key, len);
	case GCRY_PK_DSA:
		return gkm_data_der_write_public_key_dsa (s_key, len);
	default:
		g_return_val_if_reached (NULL);
	}
}

guchar*
gkm_data_der_write_private_key (gcry_sexp_t s_key, gsize *len)
{
	gboolean is_priv;
	int algorithm;

	g_return_val_if_fail (s_key != NULL, NULL);

	if (!gkm_sexp_parse_key (s_key, &algorithm, &is_priv, NULL))
		g_return_val_if_reached (NULL);

	g_return_val_if_fail (is_priv, NULL);

	switch (algorithm) {
	case GCRY_PK_RSA:
		return gkm_data_der_write_private_key_rsa (s_key, len);
	case GCRY_PK_DSA:
		return gkm_data_der_write_private_key_dsa (s_key, len);
	default:
		g_return_val_if_reached (NULL);
	}
}

static gcry_cipher_hd_t
prepare_and_encode_pkcs8_cipher (GNode *asn, const gchar *password,
                                 gsize n_password, gsize *n_block)
{
	GNode *asn1_params = NULL;
	gcry_cipher_hd_t cih;
	guchar salt[8];
	gcry_error_t gcry;
	guchar *key, *iv, *portion;
	gsize n_key, n_portion;
	int iterations;

	init_quarks ();

	/* Make sure the encryption algorithm works */
	g_return_val_if_fail (gcry_cipher_algo_info (OID_PKCS12_PBE_3DES_SHA1,
	                                             GCRYCTL_TEST_ALGO, NULL, 0), NULL);

	/* The encryption algorithm */
	if(!egg_asn1x_set_oid_as_quark (egg_asn1x_node (asn, "encryptionAlgorithm", "algorithm", NULL),
	                                OID_PKCS12_PBE_3DES_SHA1))
		g_return_val_if_reached (NULL);

	/* Randomize some input for the password based secret */
	iterations = 1000 + (int) (1000.0 * rand () / (RAND_MAX + 1.0));
	gcry_create_nonce (salt, sizeof (salt));

	/* Allocate space for the key and iv */
	n_key = gcry_cipher_get_algo_keylen (GCRY_CIPHER_3DES);
	*n_block = gcry_cipher_get_algo_blklen (GCRY_MD_SHA1);
	g_return_val_if_fail (n_key && *n_block, NULL);

	if (!egg_symkey_generate_pkcs12 (GCRY_CIPHER_3DES, GCRY_MD_SHA1,
	                                        password, n_password, salt,
	                                        sizeof (salt), iterations, &key, &iv))
		g_return_val_if_reached (NULL);

	/* Now write out the parameters */
	asn1_params = egg_asn1x_create (pkix_asn1_tab, "pkcs-12-PbeParams");
	g_return_val_if_fail (asn1_params, NULL);
	if (!egg_asn1x_set_string_as_raw (egg_asn1x_node (asn1_params, "salt", NULL), salt, sizeof (salt), NULL))
		g_return_val_if_reached (NULL);
	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn1_params, "iterations", NULL), iterations))
		g_return_val_if_reached (NULL);
	portion = egg_asn1x_encode (asn1_params, NULL, &n_portion);
	g_return_val_if_fail (portion, NULL);

	if (!egg_asn1x_set_raw_element (egg_asn1x_node (asn, "encryptionAlgorithm", "parameters", NULL),
	                                portion, n_portion, g_free))
		g_return_val_if_reached (NULL);

	/* Now make a cipher that matches what we wrote out */
	gcry = gcry_cipher_open (&cih, GCRY_CIPHER_3DES, GCRY_CIPHER_MODE_CBC, 0);
	g_return_val_if_fail (gcry == 0, NULL);
	g_return_val_if_fail (cih, NULL);

	gcry_cipher_setiv (cih, iv, *n_block);
	gcry_cipher_setkey (cih, key, n_key);

	g_free (iv);
	egg_secure_free (key);
	egg_asn1x_destroy (asn1_params);

	return cih;
}

guchar*
gkm_data_der_write_private_pkcs8_plain (gcry_sexp_t skey, gsize *n_data)
{
	GNode *asn = NULL;
	int algorithm;
	gboolean is_priv;
	GQuark oid;
	guchar *params, *key, *data;
	gsize n_params, n_key;

	init_quarks ();

	/* Parse and check that the key is for real */
	if (!gkm_sexp_parse_key (skey, &algorithm, &is_priv, NULL))
		g_return_val_if_reached (NULL);
	g_return_val_if_fail (is_priv == TRUE, NULL);

	asn = egg_asn1x_create (pkix_asn1_tab, "pkcs-8-PrivateKeyInfo");
	g_return_val_if_fail (asn, NULL);

	/* Write out the version */
	if (!egg_asn1x_set_integer_as_ulong (egg_asn1x_node (asn, "version", NULL), 0))
		g_return_val_if_reached (NULL);

	/* Per algorithm differences */
	switch (algorithm)
	{
	/* RSA gets encoded in a standard simple way */
	case GCRY_PK_RSA:
		oid = OID_PKIX1_RSA;
		params = NULL;
		n_params = 0;
		key = gkm_data_der_write_private_key_rsa (skey, &n_key);
		break;

	/* DSA gets incoded with the params seperate */
	case GCRY_PK_DSA:
		oid = OID_PKIX1_DSA;
		key = gkm_data_der_write_private_key_dsa_part (skey, &n_key);
		params = gkm_data_der_write_private_key_dsa_params (skey, &n_params);
		break;

	default:
		g_warning ("trying to serialize unsupported private key algorithm: %d", algorithm);
		return NULL;
	};

	/* Write out the algorithm */
	if (!egg_asn1x_set_oid_as_quark (egg_asn1x_node (asn, "privateKeyAlgorithm", "algorithm", NULL), oid))
		g_return_val_if_reached (NULL);

	/* Write out the parameters */
	if (params) {
		if (!egg_asn1x_set_raw_element (egg_asn1x_node (asn, "privateKeyAlgorithm", "parameters", NULL),
		                                params, n_params, egg_secure_free))
			g_return_val_if_reached (NULL);
	}

	/* Write out the key portion */
	if (!egg_asn1x_set_string_as_raw (egg_asn1x_node (asn, "privateKey", NULL),
	                                  key, n_key, egg_secure_free))
		g_return_val_if_reached (NULL);

	data = egg_asn1x_encode (asn, egg_secure_realloc, n_data);
	g_return_val_if_fail (data, NULL);

	egg_asn1x_destroy (asn);
	return data;
}

guchar*
gkm_data_der_write_private_pkcs8_crypted (gcry_sexp_t skey, const gchar *password,
                                          gsize n_password, gsize *n_data)
{
	gcry_error_t gcry;
	gcry_cipher_hd_t cih;
	GNode *asn = NULL;
	guchar *key, *data;
	gsize n_key, block = 0;

	/* Encode the key in normal pkcs8 fashion */
	key = gkm_data_der_write_private_pkcs8_plain (skey, &n_key);
	if (key == NULL)
		return NULL;

	asn = egg_asn1x_create (pkix_asn1_tab, "pkcs-8-EncryptedPrivateKeyInfo");
	g_return_val_if_fail (asn, NULL);

	/* Create a and write out a cipher used for encryption */
	cih = prepare_and_encode_pkcs8_cipher (asn, password, n_password, &block);
	g_return_val_if_fail (cih, NULL);

	/* Pad the block of data */
	if(block > 1) {
		gsize pad;
		guchar *padded;

		pad = block - (n_key % block);
		if (pad == 0)
			pad = block;
		padded = egg_secure_realloc (key, n_key + pad);
		memset (padded + n_key, pad, pad);
		key = padded;
		n_key += pad;
	}

	gcry = gcry_cipher_encrypt (cih, key, n_key, NULL, 0);
	g_return_val_if_fail (gcry == 0, NULL);

	gcry_cipher_close (cih);

	if (!egg_asn1x_set_string_as_raw (egg_asn1x_node (asn, "encryptedData", NULL),
	                                  key, n_key, egg_secure_free))
		g_return_val_if_reached (NULL);

	data = egg_asn1x_encode (asn, NULL, n_data);
	g_return_val_if_fail (data, NULL);

	egg_asn1x_destroy (asn);
	return data;
}

/* -----------------------------------------------------------------------------
 * CERTIFICATES
 */

GkmDataResult
gkm_data_der_read_certificate (const guchar *data, gsize n_data, GNode **asn1)
{
	*asn1 = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", data, n_data);
	if (!*asn1)
		return GKM_DATA_UNRECOGNIZED;

	return GKM_DATA_SUCCESS;
}

GkmDataResult
gkm_data_der_read_basic_constraints (const guchar *data, gsize n_data,
                                     gboolean *is_ca, gint *path_len)
{
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	GNode *asn = NULL;
	GNode *node;
	gulong value;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "BasicConstraints", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (path_len) {
		node = egg_asn1x_node (asn, "pathLenConstraint", NULL);
		if (!egg_asn1x_have (node))
			*path_len = -1;
		else if (!egg_asn1x_get_integer_as_ulong (node, &value))
			goto done;
		else
			*path_len = value;
	}

	if (is_ca) {
		node = egg_asn1x_node (asn, "cA", NULL);
		if (!egg_asn1x_have (node))
			*is_ca = FALSE;
		else if (!egg_asn1x_get_boolean (node, is_ca))
			goto done;
	}

	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn);
	if (ret == GKM_DATA_FAILURE)
		g_message ("invalid basic constraints");

	return ret;
}

GkmDataResult
gkm_data_der_read_key_usage (const guchar *data, gsize n_data, gulong *key_usage)
{
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	GNode *asn = NULL;
	guint n_bits;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "KeyUsage", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	if (!egg_asn1x_get_bits_as_ulong (asn, key_usage, &n_bits))
		goto done;

	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn);
	return ret;
}

GkmDataResult
gkm_data_der_read_enhanced_usage (const guchar *data, gsize n_data, GQuark **usage_oids)
{
	GkmDataResult ret = GKM_DATA_UNRECOGNIZED;
	GNode *asn = NULL;
	GNode *node;
	GArray *array;
	GQuark oid;
	int i;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "ExtKeyUsageSyntax", data, n_data);
	if (!asn)
		goto done;

	ret = GKM_DATA_FAILURE;

	array = g_array_new (TRUE, TRUE, sizeof (GQuark));
	for (i = 0; TRUE; ++i) {
		node = egg_asn1x_node (asn, i + 1, NULL);
		if (node == NULL)
			break;

		oid = egg_asn1x_get_oid_as_quark (node);
		g_array_append_val (array, oid);
	}

	*usage_oids = (GQuark*)g_array_free (array, FALSE);
	ret = GKM_DATA_SUCCESS;

done:
	egg_asn1x_destroy (asn);
	return ret;
}


guchar*
gkm_data_der_write_certificate (GNode *asn1, gsize *n_data)
{
	g_return_val_if_fail (asn1, NULL);
	g_return_val_if_fail (n_data, NULL);

	return egg_asn1x_encode (asn1, NULL, n_data);
}
