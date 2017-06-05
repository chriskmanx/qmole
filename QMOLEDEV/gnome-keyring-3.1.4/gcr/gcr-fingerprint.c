/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "gcr-fingerprint.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"

#include <glib.h>
#include <gcrypt.h>

static GQuark OID_PKIX1_RSA = 0;
static GQuark OID_PKIX1_DSA = 0;

static void
init_quarks (void)
{
	static volatile gsize quarks_inited = 0;

	if (g_once_init_enter (&quarks_inited)) {

		#define QUARK(name, value) \
			name = g_quark_from_static_string(value)

		QUARK (OID_PKIX1_RSA, "1.2.840.113549.1.1.1");
		QUARK (OID_PKIX1_DSA, "1.2.840.10040.4.1");

		#undef QUARK

		g_once_init_leave (&quarks_inited, 1);
	}
}

gpointer
_gcr_fingerprint_from_subject_public_key_info (gconstpointer key_info, gsize n_key_info,
                                               GChecksumType checksum_type,
                                               gsize *n_fingerprint)
{
	GChecksum *check;
	guint8 *fingerprint;

	g_return_val_if_fail (key_info, NULL);
	g_return_val_if_fail (n_key_info, NULL);
	g_return_val_if_fail (n_fingerprint, NULL);

	check = g_checksum_new (checksum_type);
	g_return_val_if_fail (check, NULL);

	g_checksum_update (check, key_info, n_key_info);

	*n_fingerprint = g_checksum_type_get_length (checksum_type);
	fingerprint = g_malloc (*n_fingerprint);
	g_checksum_get_digest (check, fingerprint, n_fingerprint);

	g_checksum_free (check);
	return fingerprint;
}

static gboolean
rsa_subject_public_key_from_attributes (GckAttributes *attrs, GNode *info_asn)
{
	GckAttribute *attr;
	GNode *key_asn;
	GNode *params_asn;
	gpointer key, params;
	gsize n_key, n_params;

	init_quarks ();

	key_asn = egg_asn1x_create (pk_asn1_tab, "RSAPublicKey");
	g_return_val_if_fail (key_asn, FALSE);

	params_asn = egg_asn1x_create (pk_asn1_tab, "RSAParameters");
	g_return_val_if_fail (params_asn, FALSE);

	attr = gck_attributes_find (attrs, CKA_MODULUS);
	g_return_val_if_fail (attr, FALSE);
	egg_asn1x_set_integer_as_raw (egg_asn1x_node (key_asn, "modulus", NULL),
	                              attr->value, attr->length, NULL);

	attr = gck_attributes_find (attrs, CKA_PUBLIC_EXPONENT);
	g_return_val_if_fail (attr, FALSE);
	egg_asn1x_set_integer_as_raw (egg_asn1x_node (key_asn, "publicExponent", NULL),
	                              attr->value, attr->length, NULL);

	key = egg_asn1x_encode (key_asn, g_realloc, &n_key);
	egg_asn1x_destroy (key_asn);

	egg_asn1x_set_null (params_asn);

	params = egg_asn1x_encode (params_asn, g_realloc, &n_params);
	egg_asn1x_destroy (params_asn);

	egg_asn1x_set_bits_as_raw (egg_asn1x_node (info_asn, "subjectPublicKey", NULL),
	                           key, n_key * 8, g_free);

	egg_asn1x_set_oid_as_quark (egg_asn1x_node (info_asn, "algorithm", "algorithm", NULL), OID_PKIX1_RSA);
	egg_asn1x_set_raw_element (egg_asn1x_node (info_asn, "algorithm", "parameters", NULL),
	                           params, n_params, g_free);

	return TRUE;
}

static gboolean
dsa_subject_public_key_from_private (GNode *key_asn, GckAttribute *ap,
                                     GckAttribute *aq, GckAttribute *ag, GckAttribute *ax)
{
	gcry_mpi_t mp, mq, mg, mx, my;
	size_t n_buffer;
	gcry_error_t gcry;
	unsigned char *buffer;

	gcry = gcry_mpi_scan (&mp, GCRYMPI_FMT_USG, ap->value, ap->length, NULL);
	g_return_val_if_fail (gcry == 0, FALSE);

	gcry = gcry_mpi_scan (&mq, GCRYMPI_FMT_USG, aq->value, aq->length, NULL);
	g_return_val_if_fail (gcry == 0, FALSE);

	gcry = gcry_mpi_scan (&mg, GCRYMPI_FMT_USG, ag->value, ag->length, NULL);
	g_return_val_if_fail (gcry == 0, FALSE);

	gcry = gcry_mpi_scan (&mx, GCRYMPI_FMT_USG, ax->value, ax->length, NULL);
	g_return_val_if_fail (gcry == 0, FALSE);

	/* Calculate the public part from the private */
	my = gcry_mpi_snew (gcry_mpi_get_nbits (mx));
	g_return_val_if_fail (my, FALSE);
	gcry_mpi_powm (my, mg, mx, mp);

	gcry = gcry_mpi_aprint (GCRYMPI_FMT_USG, &buffer, &n_buffer, my);
	g_return_val_if_fail (gcry == 0, FALSE);
	egg_asn1x_set_integer_as_raw (key_asn, buffer, n_buffer, gcry_free);

	gcry_mpi_release (mp);
	gcry_mpi_release (mq);
	gcry_mpi_release (mg);
	gcry_mpi_release (mx);
	gcry_mpi_release (my);

	return TRUE;
}

static gboolean
dsa_subject_public_key_from_attributes (GckAttributes *attrs, GNode *info_asn)
{
	GckAttribute *value, *g, *q, *p;
	GNode *key_asn, *params_asn;
	gpointer key, params;
	gsize n_key, n_params;
	gulong klass;

	init_quarks ();

	key_asn = egg_asn1x_create (pk_asn1_tab, "DSAPublicPart");
	g_return_val_if_fail (key_asn, FALSE);

	params_asn = egg_asn1x_create (pk_asn1_tab, "DSAParameters");
	g_return_val_if_fail (params_asn, FALSE);

	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &klass))
		klass = CKO_PUBLIC_KEY;

	p = gck_attributes_find (attrs, CKA_PRIME);
	g_return_val_if_fail (p, FALSE);
	egg_asn1x_set_integer_as_raw (egg_asn1x_node (params_asn, "p", NULL), p->value, p->length, NULL);

	q = gck_attributes_find (attrs, CKA_SUBPRIME);
	g_return_val_if_fail (q, FALSE);
	egg_asn1x_set_integer_as_raw (egg_asn1x_node (params_asn, "q", NULL), q->value, q->length, NULL);

	g = gck_attributes_find (attrs, CKA_BASE);
	g_return_val_if_fail (g, FALSE);
	egg_asn1x_set_integer_as_raw (egg_asn1x_node (params_asn, "g", NULL), g->value, g->length, NULL);

	value = gck_attributes_find (attrs, CKA_VALUE);
	g_return_val_if_fail (value, FALSE);

	/* Are these attributes for a public or private key? */
	if (klass == CKO_PRIVATE_KEY) {

		/* We need to calculate the public from the private key */
		if (!dsa_subject_public_key_from_private (key_asn, p, q, g, value))
			g_return_val_if_reached (FALSE);

	} else if (klass == CKO_PUBLIC_KEY) {
		egg_asn1x_set_integer_as_raw (key_asn, value->value, value->length, NULL);
	}

	key = egg_asn1x_encode (key_asn, g_realloc, &n_key);
	egg_asn1x_destroy (key_asn);

	params = egg_asn1x_encode (params_asn, g_realloc, &n_params);
	egg_asn1x_destroy (params_asn);

	egg_asn1x_set_bits_as_raw (egg_asn1x_node (info_asn, "subjectPublicKey", NULL),
	                           key, n_key * 8, g_free);
	egg_asn1x_set_raw_element (egg_asn1x_node (info_asn, "algorithm", "parameters", NULL),
	                           params, n_params, g_free);

	egg_asn1x_set_oid_as_quark (egg_asn1x_node (info_asn, "algorithm", "algorithm", NULL), OID_PKIX1_DSA);

	return TRUE;
}

gpointer
_gcr_fingerprint_from_attributes (GckAttributes *attrs, GChecksumType checksum_type,
                                  gsize *n_fingerprint)
{
	gpointer fingerprint = NULL;
	gboolean ret = FALSE;
	GNode *info_asn;
	gpointer info;
	gulong key_type;
	gsize n_info;

	g_return_val_if_fail (attrs, FALSE);
	g_return_val_if_fail (n_fingerprint, FALSE);

	if (!gck_attributes_find_ulong (attrs, CKA_KEY_TYPE, &key_type))
		g_return_val_if_reached (FALSE);

	info_asn = egg_asn1x_create (pkix_asn1_tab, "SubjectPublicKeyInfo");
	g_return_val_if_fail (info_asn, FALSE);

	if (key_type == CKK_RSA)
		ret = rsa_subject_public_key_from_attributes (attrs, info_asn);

	else if (key_type == CKK_DSA)
		ret = dsa_subject_public_key_from_attributes (attrs, info_asn);

	else
		g_return_val_if_reached (FALSE);

	if (ret) {
		info = egg_asn1x_encode (info_asn, g_realloc, &n_info);
		fingerprint = _gcr_fingerprint_from_subject_public_key_info (info, n_info,
		                                                             checksum_type,
		                                                             n_fingerprint);
		g_free (info);
	}

	egg_asn1x_destroy (info_asn);
	return fingerprint;
}
