/* 
 * gnome-keyring
 * 
 * Copyright (C) 2008 Stefan Walter
 * 
 * This program is free software; you can redistribute it and/or modify 
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *  
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  
 */

#include "config.h"

#include "gcr-internal.h"
#include "gcr-certificate.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-dn.h"
#include "egg/egg-hex.h"

#include <string.h>

/**
 * SECTION:gcr-certificate
 * @title: GcrCertificate
 * @short_description: Represents a certificate.
 * 
 * This is an interface that represents an X509 certificate. Objects can 
 * implement this interface to make a certificate usable with the GCR
 * library. 
 * 
 * You can use #GcrSimpleCertificate to simply load a certificate.
 */

/* 
 * The DER data in this structure is owned by the derived class. 
 * It is only valid for the duration of the current call stack
 * after we call gcr_certificate_get_der_data(). We shouldn't 
 * save it anywhere else.
 * 
 * We keep the pointer around and compare it so that if the derived
 * class returns exactly the same pointer and size, then we can
 * keep from parsing things over again. 
 */

typedef struct _GcrCertificateInfo {
	const guchar *der;
	gsize n_der;
	GNode *asn1;
	guint key_size;
} GcrCertificateInfo;

/* -----------------------------------------------------------------------------
 * INTERNAL 
 */

static GQuark CERTIFICATE_INFO = 0;
static GQuark OID_RSA_KEY = 0;
static GQuark OID_DSA_KEY = 0;

static void
certificate_info_free (gpointer data)
{
	GcrCertificateInfo *info = data;
	if (info) {
		g_assert (info->asn1);
		egg_asn1x_destroy (info->asn1);
		g_free (info);
	}
}

static GcrCertificateInfo*
certificate_info_load (GcrCertificate *cert)
{
	GcrCertificateInfo *info;
	GNode *asn1;
	const guchar *der;
	gsize n_der;
	
	g_assert (GCR_IS_CERTIFICATE (cert));
	
	der = gcr_certificate_get_der_data (cert, &n_der);
	g_return_val_if_fail (der, NULL);

	info = g_object_get_qdata (G_OBJECT (cert), CERTIFICATE_INFO);
	if (info != NULL) {
		if (n_der == info->n_der && der == info->der)
			return info;
	}
	
	/* Cache is invalid or non existent */
	asn1 = egg_asn1x_create_and_decode (pkix_asn1_tab, "Certificate", der, n_der);
	if (asn1 == NULL) {
		g_warning ("a derived class provided an invalid or unparseable X509 DER certificate data.");
		return NULL;
	}
	
	info = g_new0 (GcrCertificateInfo, 1);
	info->der = der;
	info->n_der = n_der;
	info->asn1 = asn1;

	g_object_set_qdata_full (G_OBJECT (cert), CERTIFICATE_INFO, info, certificate_info_free);
	return info;
}

static guint
calculate_rsa_key_size (const guchar *data, gsize n_data)
{
	GNode *asn;
	gsize n_content;

	asn = egg_asn1x_create_and_decode (pk_asn1_tab, "RSAPublicKey", data, n_data);
	g_return_val_if_fail (asn, 0);

	if (!egg_asn1x_get_raw_value (egg_asn1x_node (asn, "modulus", NULL), &n_content))
		g_return_val_if_reached (0);

	egg_asn1x_destroy (asn);

	/* Removes the complement */
	return (n_content / 2) * 2 * 8;
}

static guint
calculate_dsa_params_size (const guchar *data, gsize n_data)
{
	GNode *asn;
	gsize n_content;

	asn = egg_asn1x_create_and_decode (pk_asn1_tab, "DSAParameters", data, n_data);
	g_return_val_if_fail (asn, 0);

	if (!egg_asn1x_get_raw_value (egg_asn1x_node (asn, "p", NULL), &n_content))
		g_return_val_if_reached (0);

	egg_asn1x_destroy (asn);

	/* Removes the complement */
	return (n_content / 2) * 2 * 8;
}

static guint
calculate_key_size (GcrCertificateInfo *info)
{
	GNode *asn;
	const guchar *data, *params;
	gsize n_data, n_params;
	guint key_size = 0, n_bits;
	guchar *key;
	GQuark oid;

	data = egg_asn1x_get_raw_element (egg_asn1x_node (info->asn1, "tbsCertificate", "subjectPublicKeyInfo", NULL), &n_data);
	g_return_val_if_fail (data != NULL, 0);

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "SubjectPublicKeyInfo", data, n_data);
	g_return_val_if_fail (asn, 0);

	/* Figure out the algorithm */
	oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "algorithm", "algorithm", NULL));
	g_return_val_if_fail (oid, 0);

	/* RSA keys are stored in the main subjectPublicKey field */
	if (oid == OID_RSA_KEY) {

		/* A bit string so we cannot process in place */
		key = egg_asn1x_get_bits_as_raw (egg_asn1x_node (asn, "subjectPublicKey", NULL), NULL, &n_bits);
		g_return_val_if_fail (key, 0);
		key_size = calculate_rsa_key_size (key, n_bits / 8);

	/* The DSA key size is discovered by the prime in params */
	} else if (oid == OID_DSA_KEY) {
		params = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "algorithm", "parameters", NULL), &n_params);
		key_size = calculate_dsa_params_size (params, n_params);

	} else {
		g_message ("unsupported key algorithm in certificate: %s", g_quark_to_string (oid));
	}

	egg_asn1x_destroy (asn);
	g_free (key);

	return key_size;
}

static GChecksum*
digest_certificate (GcrCertificate *self, GChecksumType type)
{
	GChecksum *digest;
	const guchar *der;
	gsize n_der;
	
	g_assert (GCR_IS_CERTIFICATE (self));

	der = gcr_certificate_get_der_data (self, &n_der);
	g_return_val_if_fail (der, NULL);
	
	digest = g_checksum_new (type);
	g_return_val_if_fail (digest, NULL);
	
	g_checksum_update (digest, der, n_der);
	return digest;
}

/* ---------------------------------------------------------------------------------
 * INTERFACE
 */

static void
gcr_certificate_base_init (gpointer g_class)
{
	static volatile gsize initialized = 0;

	if (g_once_init_enter (&initialized)) {
		CERTIFICATE_INFO = g_quark_from_static_string ("_gcr_certificate_certificate_info");
		OID_RSA_KEY = g_quark_from_static_string ("1.2.840.113549.1.1.1");
		OID_DSA_KEY = g_quark_from_static_string ("1.2.840.10040.4.1");

		/* Add properties and signals to the interface */

		g_once_init_leave (&initialized, 1);
	}
}

GType
gcr_certificate_get_type (void)
{
	static GType type = 0;
	if (!type) {
		static const GTypeInfo info = {
			sizeof (GcrCertificateIface),
			gcr_certificate_base_init,               /* base init */
			NULL,             /* base finalize */
			NULL,             /* class_init */
			NULL,             /* class finalize */
			NULL,             /* class data */
			0,
			0,                /* n_preallocs */
			NULL,             /* instance init */
		};
		type = g_type_register_static (G_TYPE_INTERFACE, "GcrCertificateIface", &info, 0);
		g_type_interface_add_prerequisite (type, G_TYPE_OBJECT);
	}
	
	return type;
}


/* -----------------------------------------------------------------------------
 * PUBLIC 
 */

/**
 * gcr_certificate_get_der_data:
 * @self: a #GcrCertificate
 * @n_data: a pointer to a location to store the size of the resulting DER data.
 * 
 * Gets the raw DER data for an X509 certificate.
 * 
 * Returns: raw DER data of the X509 certificate.
 **/
const guchar*
gcr_certificate_get_der_data (GcrCertificate *self, gsize *n_data)
{
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	g_return_val_if_fail (GCR_CERTIFICATE_GET_INTERFACE (self)->get_der_data, NULL);
	return GCR_CERTIFICATE_GET_INTERFACE (self)->get_der_data (self, n_data);
}

/**
 * gcr_certificate_get_issuer_cn:
 * @self: a #GcrCertificate
 * 
 * Get the common name of the issuer of this certificate. 
 * 
 * The string returned should be freed by the caller when no longer
 * required.
 * 
 * Returns: The allocated issuer CN, or NULL if no issuer CN present.
 */
gchar*
gcr_certificate_get_issuer_cn (GcrCertificate *self)
{
	return gcr_certificate_get_issuer_part (self, "cn");
}

/**
 * gcr_certificate_get_issuer_part:
 * @self: a #GcrCertificate
 * @part: a DN type string or OID.
 * 
 * Get a part of the DN of the issuer of this certificate. 
 * 
 * Examples of a @part might be the 'OU' (organizational unit)
 * or the 'CN' (common name). Only the value of that part 
 * of the DN is returned.
 * 
 * The string returned should be freed by the caller when no longer
 * required.
 * 
 * Returns: The allocated part of the issuer DN, or NULL if no such part is present.
 */
gchar*
gcr_certificate_get_issuer_part (GcrCertificate *self, const char *part)
{
	GcrCertificateInfo *info;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	info = certificate_info_load (self);
	g_return_val_if_fail (info, NULL);

	return egg_dn_read_part (egg_asn1x_node (info->asn1, "tbsCertificate", "issuer", "rdnSequence", NULL), part);
}

/**
 * gcr_certificate_get_issuer_dn:
 * @self: a #GcrCertificate
 * 
 * Get the full issuer DN of the certificate as a (mostly) 
 * readable string. 
 * 
 * The string returned should be freed by the caller when no longer
 * required.
 * 
 * Returns: The allocated issuer DN of the certificate.
 */
gchar*
gcr_certificate_get_issuer_dn (GcrCertificate *self)
{
	GcrCertificateInfo *info;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	info = certificate_info_load (self);
	g_return_val_if_fail (info, NULL);

	return egg_dn_read (egg_asn1x_node (info->asn1, "tbsCertificate", "issuer", "rdnSequence", NULL));
}

/**
 * gcr_certificate_get_subject_cn:
 * @self: a #GcrCertificate
 * 
 * Get the common name of the subject of this certificate. 
 * 
 * The string returned should be freed by the caller when no longer
 * required.
 * 
 * Returns: The allocated subject CN, or NULL if no subject CN present.
 */
gchar* 
gcr_certificate_get_subject_cn (GcrCertificate *self)
{
	return gcr_certificate_get_subject_part (self, "cn");
}

/**
 * gcr_certificate_get_subject_part:
 * @self: a #GcrCertificate
 * @part: a DN type string or OID.
 * 
 * Get a part of the DN of the subject of this certificate. 
 * 
 * Examples of a @part might be the 'OU' (organizational unit)
 * or the 'CN' (common name). Only the value of that part 
 * of the DN is returned.
 * 
 * The string returned should be freed by the caller when no longer
 * required.
 * 
 * Returns: The allocated part of the subject DN, or NULL if no such part is present.
 */
gchar* 
gcr_certificate_get_subject_part (GcrCertificate *self, const char *part)
{
	GcrCertificateInfo *info;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	info = certificate_info_load (self);
	g_return_val_if_fail (info, NULL);

	return egg_dn_read_part (egg_asn1x_node (info->asn1, "tbsCertificate", "subject", "rdnSequence", NULL), part);
}

/**
 * gcr_certificate_get_subject_dn:
 * @self: a #GcrCertificate
 * 
 * Get the full subject DN of the certificate as a (mostly) 
 * readable string. 
 * 
 * The string returned should be freed by the caller when no longer
 * required.
 * 
 * Returns: The allocated subject DN of the certificate.
 */
gchar* 
gcr_certificate_get_subject_dn (GcrCertificate *self)
{
	GcrCertificateInfo *info;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	info = certificate_info_load (self);
	g_return_val_if_fail (info, NULL);

	return egg_dn_read (egg_asn1x_node (info->asn1, "tbsCertificate", "issuer", "rdnSequence", NULL));
}

/**
 * gcr_certificate_get_issued_date:
 * @self: a #GcrCertificate
 * 
 * Get the issued date of this certificate.
 * 
 * The #GDate returned should be freed by the caller using 
 * g_date_free() when no longer required.
 * 
 * Returns: An allocated issued date of this certificate.
 */
GDate* 
gcr_certificate_get_issued_date (GcrCertificate *self)
{
	GcrCertificateInfo *info;
	GDate *date;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	info = certificate_info_load (self);
	g_return_val_if_fail (info, NULL);
	
	date = g_date_new ();
	if (!egg_asn1x_get_time_as_date (egg_asn1x_node (info->asn1, "tbsCertificate", "validity", "notBefore", NULL), date)) {
		g_date_free (date);
		return NULL;
	}
	
	return date;
}

/**
 * gcr_certificate_get_expiry_date:
 * @self: a #GcrCertificate
 * 
 * Get the expiry date of this certificate.
 * 
 * The #GDate returned should be freed by the caller using 
 * g_date_free() when no longer required.
 * 
 * Returns: An allocated expiry date of this certificate.
 */
GDate* 
gcr_certificate_get_expiry_date (GcrCertificate *self)
{
	GcrCertificateInfo *info;
	GDate *date;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	info = certificate_info_load (self);
	g_return_val_if_fail (info, NULL);
	
	date = g_date_new ();
	if (!egg_asn1x_get_time_as_date (egg_asn1x_node (info->asn1, "tbsCertificate", "validity", "notAfter", NULL), date)) {
		g_date_free (date);
		return NULL;
	}
	
	return date;
}

/**
 * gcr_certificate_get_key_size:
 * @self: a #GcrCertificate
 * 
 * Get the key size in bits of the public key represented 
 * by this certificate. 
 * 
 * Returns: The key size of the certificate.
 */
guint
gcr_certificate_get_key_size (GcrCertificate *self)
{
	GcrCertificateInfo *info;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), 0);

	info = certificate_info_load (self);
	g_return_val_if_fail (info, 0);
	
	if (!info->key_size)
		info->key_size = calculate_key_size (info);
	
	return info->key_size;
}

/**
 * gcr_certificate_get_fingerprint:
 * @self: a #GcrCertificate
 * @type: the type of algorithm for the fingerprint.
 * @n_length: The length of the resulting fingerprint.
 * 
 * Calculate the fingerprint for this certificate.
 * 
 * You can pass G_CHECKSUM_SHA1 or G_CHECKSUM_MD5 as the @type
 * parameter.
 * 
 * The caller should free the returned data using g_free() when
 * it is no longer required.
 * 
 * Returns: the raw binary fingerprint.  
 **/
guchar*
gcr_certificate_get_fingerprint (GcrCertificate *self, GChecksumType type, gsize *n_length)
{
	GChecksum *sum;
	guchar *digest;
	gssize length;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	g_return_val_if_fail (n_length, NULL);
	
	sum = digest_certificate (self, type);
	g_return_val_if_fail (sum, NULL);
	length = g_checksum_type_get_length (type);
	g_return_val_if_fail (length > 0, NULL);
	digest = g_malloc (length);
	*n_length = length;
	g_checksum_get_digest (sum, digest, n_length);
	g_checksum_free (sum);
	
	return digest;
}

/**
 * gcr_certificate_get_fingerprint_hex:
 * @self: a #GcrCertificate
 * @type: the type of algorithm for the fingerprint.
 * 
 * Calculate the fingerprint for this certificate, and return it 
 * as a hex string.
 * 
 * You can pass G_CHECKSUM_SHA1 or G_CHECKSUM_MD5 as the @type
 * parameter.
 * 
 * The caller should free the returned data using g_free() when
 * it is no longer required.
 * 
 * Returns: an allocated hex string which contains the fingerprint.  
 */
gchar*
gcr_certificate_get_fingerprint_hex (GcrCertificate *self, GChecksumType type)
{
	GChecksum *sum;
	guchar *digest;
	gsize n_digest;
	gssize length;
	gchar *hex;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	sum = digest_certificate (self, type);
	g_return_val_if_fail (sum, NULL);
	length = g_checksum_type_get_length (type);
	g_return_val_if_fail (length > 0, NULL);
	digest = g_malloc (length);
	n_digest = length;
	g_checksum_get_digest (sum, digest, &n_digest);
	hex = egg_hex_encode_full (digest, n_digest, TRUE, ' ', 1);
	g_checksum_free (sum);
	g_free (digest);
	return hex;
}

/**
 * gcr_certificate_get_serial_number:
 * @self: a #GcrCertificate
 * @n_length: the length of the returned data.
 * 
 * Get the raw binary serial number of the certificate.
 * 
 * The caller should free the returned data using g_free() when
 * it is no longer required.
 * 
 * Returns: the raw binary serial number.
 */
guchar*
gcr_certificate_get_serial_number (GcrCertificate *self, gsize *n_length)
{
	GcrCertificateInfo *info;
	gconstpointer serial;

	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	g_return_val_if_fail (n_length, NULL);
	
	info = certificate_info_load (self);
	g_return_val_if_fail (info, NULL);

	serial = egg_asn1x_get_raw_value (egg_asn1x_node (info->asn1, "tbsCertificate", "serialNumber", NULL), n_length);
	return g_memdup (serial, *n_length);
}

/**
 * gcr_certificate_get_serial_number_hex:
 * @self: a #GcrCertificate
 * 
 * Get the serial number of the certificate as a hex string.
 * 
 * The caller should free the returned data using g_free() when
 * it is no longer required.
 * 
 * Returns: an allocated string containing the serial number as hex.
 */
gchar*
gcr_certificate_get_serial_number_hex (GcrCertificate *self)
{
	guchar *serial;
	gsize n_serial;
	gchar *hex;
	
	g_return_val_if_fail (GCR_IS_CERTIFICATE (self), NULL);
	
	serial = gcr_certificate_get_serial_number (self, &n_serial);
	if (serial == NULL)
		return NULL;
	
	hex = egg_hex_encode (serial, n_serial);
	g_free (serial);
	return hex;
}
