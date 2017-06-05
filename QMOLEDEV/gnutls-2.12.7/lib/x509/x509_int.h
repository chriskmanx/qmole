/*
 * Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010 Free Software
 * Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

#ifndef X509_H
#define X509_H

#include <gnutls/x509.h>
#include <gnutls/abstract.h>

#include <libtasn1.h>

/* Remove these when we require libtasn1 v1.6 or later. */
#ifndef ASN1_MAX_NAME_SIZE
#define ASN1_MAX_NAME_SIZE MAX_NAME_SIZE
#endif
#ifndef ASN1_MAX_ERROR_DESCRIPTION_SIZE
#define ASN1_MAX_ERROR_DESCRIPTION_SIZE MAX_ERROR_DESCRIPTION_SIZE
#endif

#define MAX_CRQ_EXTENSIONS_SIZE 8*1024
#define MAX_OID_SIZE 128

#define HASH_OID_SHA1 "1.3.14.3.2.26"
#define HASH_OID_MD5 "1.2.840.113549.2.5"
#define HASH_OID_MD2 "1.2.840.113549.2.2"
#define HASH_OID_RMD160 "1.3.36.3.2.1"
#define HASH_OID_SHA256 "2.16.840.1.101.3.4.2.1"
#define HASH_OID_SHA384 "2.16.840.1.101.3.4.2.2"
#define HASH_OID_SHA512 "2.16.840.1.101.3.4.2.3"

typedef struct gnutls_x509_crl_int
{
  ASN1_TYPE crl;
  int use_extensions;
} gnutls_x509_crl_int;

typedef struct gnutls_x509_crt_int
{
  ASN1_TYPE cert;
  int use_extensions;
} gnutls_x509_crt_int;

typedef struct gnutls_x509_crq_int
{
  ASN1_TYPE crq;
} gnutls_x509_crq_int;

typedef struct gnutls_pkcs7_int
{
  ASN1_TYPE pkcs7;
} gnutls_pkcs7_int;

#define MAX_PRIV_PARAMS_SIZE GNUTLS_MAX_PK_PARAMS       /* ok for RSA and DSA */

/* parameters should not be larger than this limit */
#define DSA_PRIVATE_PARAMS 5
#define DSA_PUBLIC_PARAMS 4
#define RSA_PRIVATE_PARAMS 8
#define RSA_PUBLIC_PARAMS 2

#if MAX_PRIV_PARAMS_SIZE - RSA_PRIVATE_PARAMS < 0
#error INCREASE MAX_PRIV_PARAMS
#endif

#if MAX_PRIV_PARAMS_SIZE - DSA_PRIVATE_PARAMS < 0
#error INCREASE MAX_PRIV_PARAMS
#endif

typedef struct gnutls_x509_privkey_int
{
  /* the size of params depends on the public
   * key algorithm
   */
  bigint_t params[MAX_PRIV_PARAMS_SIZE];

  /*
   * RSA: [0] is modulus
   *      [1] is public exponent
   *      [2] is private exponent
   *      [3] is prime1 (p)
   *      [4] is prime2 (q)
   *      [5] is coefficient (u == inverse of p mod q)
   *          note that other packages used inverse of q mod p,
   *          so we need to perform conversions on import/export
   *          using fixup.
   *      The following two are also not always available thus fixup
   *      will generate them.
   *      [6] e1 == d mod (p-1)
   *      [7] e2 == d mod (q-1)
   * DSA: [0] is p
   *      [1] is q
   *      [2] is g
   *      [3] is y (public key)
   *      [4] is x (private key)
   */
  int params_size;              /* holds the number of params */

  gnutls_pk_algorithm_t pk_algorithm;

  /* The crippled keys will not use the ASN1_TYPE key.  The encoding
   * will only be performed at the export phase, to optimize copying
   * etc. Cannot be used with the exported API (used internally only).
   */
  int crippled;

  ASN1_TYPE key;
} gnutls_x509_privkey_int;

int _gnutls_x509_crt_cpy (gnutls_x509_crt_t dest, gnutls_x509_crt_t src);


int _gnutls_x509_compare_raw_dn (const gnutls_datum_t * dn1,
                                 const gnutls_datum_t * dn2);


int _gnutls_x509_crl_cpy (gnutls_x509_crl_t dest, gnutls_x509_crl_t src);
int _gnutls_x509_crl_get_raw_issuer_dn (gnutls_x509_crl_t crl,
                                        gnutls_datum_t * dn);

/* sign.c */
int _gnutls_x509_get_tbs (ASN1_TYPE cert, const char *tbs_name,
                          gnutls_datum_t * tbs);
int _gnutls_x509_pkix_sign (ASN1_TYPE src, const char *src_name,
                            gnutls_digest_algorithm_t,
                            gnutls_x509_crt_t issuer,
                            gnutls_privkey_t issuer_key);

/* dn.c */
#define OID_X520_COUNTRY_NAME		"2.5.4.6"
#define OID_X520_ORGANIZATION_NAME	"2.5.4.10"
#define OID_X520_ORGANIZATIONAL_UNIT_NAME "2.5.4.11"
#define OID_X520_COMMON_NAME		"2.5.4.3"
#define OID_X520_LOCALITY_NAME		"2.5.4.7"
#define OID_X520_STATE_OR_PROVINCE_NAME	"2.5.4.8"
#define OID_LDAP_DC			"0.9.2342.19200300.100.1.25"
#define OID_LDAP_UID			"0.9.2342.19200300.100.1.1"
#define OID_PKCS9_EMAIL			"1.2.840.113549.1.9.1"

int _gnutls_x509_parse_dn (ASN1_TYPE asn1_struct,
                           const char *asn1_rdn_name, char *buf,
                           size_t * sizeof_buf);

int _gnutls_x509_parse_dn_oid (ASN1_TYPE asn1_struct,
                               const char *asn1_rdn_name, const char *oid,
                               int indx, unsigned int raw_flag, void *buf,
                               size_t * sizeof_buf);

int _gnutls_x509_set_dn_oid (ASN1_TYPE asn1_struct,
                             const char *asn1_rdn_name, const char *oid,
                             int raw_flag, const char *name, int sizeof_name);

int _gnutls_x509_get_dn_oid (ASN1_TYPE asn1_struct,
                             const char *asn1_rdn_name,
                             int indx, void *_oid, size_t * sizeof_oid);

int _gnutls_parse_general_name (ASN1_TYPE src, const char *src_name,
                                int seq, void *name, size_t * name_size,
                                unsigned int *ret_type, int othername_oid);

/* dsa.c */


/* verify.c */
int gnutls_x509_crt_is_issuer (gnutls_x509_crt_t cert,
                               gnutls_x509_crt_t issuer);

int
_gnutls_x509_verify_algorithm (gnutls_mac_algorithm_t * hash,
                               const gnutls_datum_t * signature,
                               gnutls_pk_algorithm_t pk,
                               bigint_t * issuer_params,
                               unsigned int issuer_params_size);

int _gnutls_x509_verify_signature (const gnutls_datum_t * tbs,
                                   const gnutls_datum_t * hash,
                                   const gnutls_datum_t * signature,
                                   gnutls_x509_crt_t issuer);
int _gnutls_x509_privkey_verify_signature (const gnutls_datum_t * tbs,
                                           const gnutls_datum_t * signature,
                                           gnutls_x509_privkey_t issuer);

/* privkey.h */
ASN1_TYPE _gnutls_privkey_decode_pkcs1_rsa_key (const gnutls_datum_t *
                                                raw_key,
                                                gnutls_x509_privkey_t pkey);
int _gnutls_asn1_encode_dsa (ASN1_TYPE * c2, bigint_t * params);

/* extensions.c */
int _gnutls_x509_crl_get_extension (gnutls_x509_crl_t crl,
                                    const char *extension_id, int indx,
                                    gnutls_datum_t * ret,
                                    unsigned int *_critical);

int _gnutls_x509_crl_get_extension_oid (gnutls_x509_crl_t crl,
                                        int indx, void *oid,
                                        size_t * sizeof_oid);

int _gnutls_x509_crl_set_extension (gnutls_x509_crl_t crl,
                                    const char *ext_id,
                                    const gnutls_datum_t * ext_data,
                                    unsigned int critical);

int _gnutls_x509_crt_get_extension (gnutls_x509_crt_t cert,
                                    const char *extension_id, int indx,
                                    gnutls_datum_t * ret,
                                    unsigned int *critical);
int _gnutls_x509_crt_get_extension_oid (gnutls_x509_crt_t cert,
                                        int indx, void *ret,
                                        size_t * ret_size);
int _gnutls_x509_ext_extract_keyUsage (uint16_t * keyUsage,
                                       opaque * extnValue, int extnValueLen);
int _gnutls_x509_ext_extract_basicConstraints (int *CA,
                                               int *pathLenConstraint,
                                               opaque * extnValue,
                                               int extnValueLen);
int _gnutls_x509_crt_set_extension (gnutls_x509_crt_t cert,
                                    const char *extension_id,
                                    const gnutls_datum_t * ext_data,
                                    unsigned int critical);

int
_gnutls_x509_ext_extract_number (opaque * number,
                                 size_t * nr_size,
                                 opaque * extnValue, int extnValueLen);
int
_gnutls_x509_ext_gen_number (const opaque * nuber, size_t nr_size,
                             gnutls_datum_t * der_ext);


int _gnutls_x509_ext_gen_basicConstraints (int CA, int pathLenConstraint,
                                           gnutls_datum_t * der_ext);
int _gnutls_x509_ext_gen_keyUsage (uint16_t usage, gnutls_datum_t * der_ext);
int _gnutls_x509_ext_gen_subject_alt_name (gnutls_x509_subject_alt_name_t
                                           type, const void *data,
                                           unsigned int data_size,
                                           gnutls_datum_t * prev_der_ext,
                                           gnutls_datum_t * der_ext);
int _gnutls_x509_ext_gen_crl_dist_points (gnutls_x509_subject_alt_name_t type,
                                          const void *data,
                                          unsigned int data_size,
                                          unsigned int reason_flags,
                                          gnutls_datum_t * der_ext);
int _gnutls_x509_ext_gen_key_id (const void *id, size_t id_size,
                                 gnutls_datum_t * der_data);
int _gnutls_x509_ext_gen_auth_key_id (const void *id, size_t id_size,
                                      gnutls_datum_t * der_data);
int _gnutls_x509_ext_extract_proxyCertInfo (int *pathLenConstraint,
                                            char **policyLanguage,
                                            char **policy,
                                            size_t * sizeof_policy,
                                            opaque * extnValue,
                                            int extnValueLen);
int _gnutls_x509_ext_gen_proxyCertInfo (int pathLenConstraint,
                                        const char *policyLanguage,
                                        const char *policy,
                                        size_t sizeof_policy,
                                        gnutls_datum_t * der_ext);

/* mpi.c */
int _gnutls_x509_crq_get_mpis (gnutls_x509_crq_t cert,
                               bigint_t * params, int *params_size);

int _gnutls_x509_crt_get_mpis (gnutls_x509_crt_t cert,
                               bigint_t * params, int *params_size);
int _gnutls_x509_read_rsa_params (opaque * der, int dersize,
                                  bigint_t * params);
int _gnutls_x509_read_dsa_pubkey (opaque * der, int dersize,
                                  bigint_t * params);
int _gnutls_x509_read_dsa_params (opaque * der, int dersize,
                                  bigint_t * params);

int _gnutls_x509_write_rsa_params (bigint_t * params, int params_size,
                                   gnutls_datum_t * der);
int _gnutls_x509_write_dsa_params (bigint_t * params, int params_size,
                                   gnutls_datum_t * der);
int _gnutls_x509_write_dsa_public_key (bigint_t * params, int params_size,
                                       gnutls_datum_t * der);

int _gnutls_x509_read_uint (ASN1_TYPE node, const char *value,
                            unsigned int *ret);

int _gnutls_x509_read_der_int (opaque * der, int dersize, bigint_t * out);

int _gnutls_x509_read_int (ASN1_TYPE node, const char *value,
                           bigint_t * ret_mpi);
int _gnutls_x509_write_int (ASN1_TYPE node, const char *value, bigint_t mpi,
                            int lz);
int _gnutls_x509_write_uint32 (ASN1_TYPE node, const char *value,
                               uint32_t num);

int _gnutls_x509_write_sig_params (ASN1_TYPE dst, const char *dst_name,
                                   gnutls_pk_algorithm_t pk_algorithm,
                                   gnutls_digest_algorithm_t);

/* pkcs12.h */
#include <gnutls/pkcs12.h>

typedef struct gnutls_pkcs12_int
{
  ASN1_TYPE pkcs12;
} gnutls_pkcs12_int;

#define MAX_BAG_ELEMENTS 32

struct bag_element
{
  gnutls_datum_t data;
  gnutls_pkcs12_bag_type_t type;
  gnutls_datum_t local_key_id;
  char *friendly_name;
};

typedef struct gnutls_pkcs12_bag_int
{
  struct bag_element element[MAX_BAG_ELEMENTS];
  int bag_elements;
} gnutls_pkcs12_bag_int;

#define BAG_PKCS8_KEY "1.2.840.113549.1.12.10.1.1"
#define BAG_PKCS8_ENCRYPTED_KEY "1.2.840.113549.1.12.10.1.2"
#define BAG_CERTIFICATE "1.2.840.113549.1.12.10.1.3"
#define BAG_CRL "1.2.840.113549.1.12.10.1.4"
#define BAG_SECRET "1.2.840.113549.1.12.10.1.5"

/* PKCS #7
 */
#define DATA_OID "1.2.840.113549.1.7.1"
#define ENC_DATA_OID "1.2.840.113549.1.7.6"

/* Bag attributes
 */
#define FRIENDLY_NAME_OID "1.2.840.113549.1.9.20"
#define KEY_ID_OID "1.2.840.113549.1.9.21"

int
_gnutls_pkcs12_string_to_key (unsigned int id, const opaque * salt,
                              unsigned int salt_size, unsigned int iter,
                              const char *pw, unsigned int req_keylen,
                              opaque * keybuf);

int _gnutls_pkcs7_decrypt_data (const gnutls_datum_t * data,
                                const char *password, gnutls_datum_t * dec);

typedef enum schema_id
{
  PBES2_GENERIC,                /* when the algorithm is unknown, temporal use when reading only */
  PBES2_3DES,                   /* the stuff in PKCS #5 */
  PBES2_AES_128,
  PBES2_AES_192,
  PBES2_AES_256,
  PKCS12_3DES_SHA1,             /* the stuff in PKCS #12 */
  PKCS12_ARCFOUR_SHA1,
  PKCS12_RC2_40_SHA1
} schema_id;

int _gnutls_pkcs_flags_to_schema (unsigned int flags);
int _gnutls_pkcs7_encrypt_data (schema_id schema,
                                const gnutls_datum_t * data,
                                const char *password, gnutls_datum_t * enc);
int _pkcs12_decode_safe_contents (const gnutls_datum_t * content,
                                  gnutls_pkcs12_bag_t bag);

int
_pkcs12_encode_safe_contents (gnutls_pkcs12_bag_t bag, ASN1_TYPE * content,
                              int *enc);

int _pkcs12_decode_crt_bag (gnutls_pkcs12_bag_type_t type,
                            const gnutls_datum_t * in, gnutls_datum_t * out);
int _pkcs12_encode_crt_bag (gnutls_pkcs12_bag_type_t type,
                            const gnutls_datum_t * raw, gnutls_datum_t * out);

/* crq */
int _gnutls_x509_crq_set_extension (gnutls_x509_crq_t crq,
                                    const char *ext_id,
                                    const gnutls_datum_t * ext_data,
                                    unsigned int critical);

#endif
