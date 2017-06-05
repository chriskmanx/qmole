/*
 * Copyright (C) 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
 * 2010 Free Software Foundation, Inc.
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

#include "gnutls_int.h"
#include "gnutls_algorithms.h"
#include "gnutls_errors.h"
#include "gnutls_cert.h"
#include <x509/common.h>


typedef struct
{
  const char *name;
  gnutls_sec_param_t sec_param;
  int bits;                     /* security level */
  int pk_bits;                  /* DH, RSA, SRP */
  int dsa_bits;                 /* bits for DSA. Handled differently since
                                 * choice of key size in DSA is political.
                                 */
  int subgroup_bits;            /* subgroup bits */
  int ecc_bits;                 /* bits for ECC keys */
} gnutls_sec_params_entry;

static const gnutls_sec_params_entry sec_params[] = {
  {"Weak", GNUTLS_SEC_PARAM_WEAK, 64, 816, 1024, 128, 128},
  {"Low", GNUTLS_SEC_PARAM_LOW, 80, 1248, 2048, 160, 160},
  {"Normal", GNUTLS_SEC_PARAM_NORMAL, 112, 2432, 3072, 224, 224},
  {"High", GNUTLS_SEC_PARAM_HIGH, 128, 3248, 3072, 256, 256},
  {"Ultra", GNUTLS_SEC_PARAM_ULTRA, 256, 15424, 3072, 512, 512},
  {NULL, 0, 0, 0, 0, 0}
};

#define GNUTLS_SEC_PARAM_LOOP(b) \
	{ const gnutls_sec_params_entry *p; \
                for(p = sec_params; p->name != NULL; p++) { b ; } }


/* Cred type mappings to KX algorithms 
 * FIXME: The mappings are not 1-1. Some KX such as SRP_RSA require
 * more than one credentials type.
 */
typedef struct
{
  gnutls_kx_algorithm_t algorithm;
  gnutls_credentials_type_t client_type;
  gnutls_credentials_type_t server_type;        /* The type of credentials a server
                                                 * needs to set */
} gnutls_cred_map;

static const gnutls_cred_map cred_mappings[] = {
  {GNUTLS_KX_ANON_DH, GNUTLS_CRD_ANON, GNUTLS_CRD_ANON},
  {GNUTLS_KX_RSA, GNUTLS_CRD_CERTIFICATE, GNUTLS_CRD_CERTIFICATE},
  {GNUTLS_KX_RSA_EXPORT, GNUTLS_CRD_CERTIFICATE, GNUTLS_CRD_CERTIFICATE},
  {GNUTLS_KX_DHE_DSS, GNUTLS_CRD_CERTIFICATE, GNUTLS_CRD_CERTIFICATE},
  {GNUTLS_KX_DHE_RSA, GNUTLS_CRD_CERTIFICATE, GNUTLS_CRD_CERTIFICATE},
  {GNUTLS_KX_PSK, GNUTLS_CRD_PSK, GNUTLS_CRD_PSK},
  {GNUTLS_KX_DHE_PSK, GNUTLS_CRD_PSK, GNUTLS_CRD_PSK},
  {GNUTLS_KX_SRP, GNUTLS_CRD_SRP, GNUTLS_CRD_SRP},
  {GNUTLS_KX_SRP_RSA, GNUTLS_CRD_SRP, GNUTLS_CRD_CERTIFICATE},
  {GNUTLS_KX_SRP_DSS, GNUTLS_CRD_SRP, GNUTLS_CRD_CERTIFICATE},
  {0, 0, 0}
};

#define GNUTLS_KX_MAP_LOOP(b) \
        const gnutls_cred_map *p; \
                for(p = cred_mappings; p->algorithm != 0; p++) { b ; }

#define GNUTLS_KX_MAP_ALG_LOOP_SERVER(a) \
                        GNUTLS_KX_MAP_LOOP( if(p->server_type == type) { a; break; })

/* KX mappings to PK algorithms */
typedef struct
{
  gnutls_kx_algorithm_t kx_algorithm;
  gnutls_pk_algorithm_t pk_algorithm;
  enum encipher_type encipher_type;     /* CIPHER_ENCRYPT if this algorithm is to be used
                                         * for encryption, CIPHER_SIGN if signature only,
                                         * CIPHER_IGN if this does not apply at all.
                                         *
                                         * This is useful to certificate cipher suites, which check
                                         * against the certificate key usage bits.
                                         */
} gnutls_pk_map;

/* This table maps the Key exchange algorithms to
 * the certificate algorithms. Eg. if we have
 * RSA algorithm in the certificate then we can
 * use GNUTLS_KX_RSA or GNUTLS_KX_DHE_RSA.
 */
static const gnutls_pk_map pk_mappings[] = {
  {GNUTLS_KX_RSA, GNUTLS_PK_RSA, CIPHER_ENCRYPT},
  {GNUTLS_KX_RSA_EXPORT, GNUTLS_PK_RSA, CIPHER_SIGN},
  {GNUTLS_KX_DHE_RSA, GNUTLS_PK_RSA, CIPHER_SIGN},
  {GNUTLS_KX_SRP_RSA, GNUTLS_PK_RSA, CIPHER_SIGN},
  {GNUTLS_KX_DHE_DSS, GNUTLS_PK_DSA, CIPHER_SIGN},
  {GNUTLS_KX_SRP_DSS, GNUTLS_PK_DSA, CIPHER_SIGN},
  {0, 0, 0}
};

#define GNUTLS_PK_MAP_LOOP(b) \
        const gnutls_pk_map *p; \
                for(p = pk_mappings; p->kx_algorithm != 0; p++) { b }

#define GNUTLS_PK_MAP_ALG_LOOP(a) \
                        GNUTLS_PK_MAP_LOOP( if(p->kx_algorithm == kx_algorithm) { a; break; })



/* TLS Versions */

typedef struct
{
  const char *name;
  gnutls_protocol_t id;         /* gnutls internal version number */
  int major;                    /* defined by the protocol */
  int minor;                    /* defined by the protocol */
  int supported;                /* 0 not supported, > 0 is supported */
} gnutls_version_entry;

static const gnutls_version_entry sup_versions[] = {
  {"SSL3.0", GNUTLS_SSL3, 3, 0, 1},
  {"TLS1.0", GNUTLS_TLS1, 3, 1, 1},
  {"TLS1.1", GNUTLS_TLS1_1, 3, 2, 1},
  {"TLS1.2", GNUTLS_TLS1_2, 3, 3, 1},
  {0, 0, 0, 0, 0}
};

/* Keep the contents of this struct the same as the previous one. */
static const gnutls_protocol_t supported_protocols[] = {
  GNUTLS_SSL3,
  GNUTLS_TLS1,
  GNUTLS_TLS1_1,
  GNUTLS_TLS1_2,
  0
};

#define GNUTLS_VERSION_LOOP(b) \
        const gnutls_version_entry *p; \
                for(p = sup_versions; p->name != NULL; p++) { b ; }

#define GNUTLS_VERSION_ALG_LOOP(a) \
	GNUTLS_VERSION_LOOP( if(p->id == version) { a; break; })

struct gnutls_cipher_entry
{
  const char *name;
  gnutls_cipher_algorithm_t id;
  uint16_t blocksize;
  uint16_t keysize;
  cipher_type_t block;
  uint16_t iv;
  int export_flag;              /* 0 non export */
};
typedef struct gnutls_cipher_entry gnutls_cipher_entry;

/* Note that all algorithms are in CBC or STREAM modes. 
 * Do not add any algorithms in other modes (avoid modified algorithms).
 * View first: "The order of encryption and authentication for
 * protecting communications" by Hugo Krawczyk - CRYPTO 2001
 *
 * Make sure to updated MAX_CIPHER_BLOCK_SIZE and MAX_CIPHER_KEY_SIZE as well.
 */
static const gnutls_cipher_entry algorithms[] = {
  {"AES-256-CBC", GNUTLS_CIPHER_AES_256_CBC, 16, 32, CIPHER_BLOCK, 16, 0},
  {"AES-192-CBC", GNUTLS_CIPHER_AES_192_CBC, 16, 24, CIPHER_BLOCK, 16, 0},
  {"AES-128-CBC", GNUTLS_CIPHER_AES_128_CBC, 16, 16, CIPHER_BLOCK, 16, 0},
  {"3DES-CBC", GNUTLS_CIPHER_3DES_CBC, 8, 24, CIPHER_BLOCK, 8, 0},
  {"DES-CBC", GNUTLS_CIPHER_DES_CBC, 8, 8, CIPHER_BLOCK, 8, 0},
  {"ARCFOUR-128", GNUTLS_CIPHER_ARCFOUR_128, 1, 16, CIPHER_STREAM, 0, 0},
  {"ARCFOUR-40", GNUTLS_CIPHER_ARCFOUR_40, 1, 5, CIPHER_STREAM, 0, 1},
  {"RC2-40", GNUTLS_CIPHER_RC2_40_CBC, 8, 5, CIPHER_BLOCK, 8, 1},
#ifdef	ENABLE_CAMELLIA
  {"CAMELLIA-256-CBC", GNUTLS_CIPHER_CAMELLIA_256_CBC, 16, 32, CIPHER_BLOCK,
   16, 0},
  {"CAMELLIA-128-CBC", GNUTLS_CIPHER_CAMELLIA_128_CBC, 16, 16, CIPHER_BLOCK,
   16, 0},
#endif

#ifdef ENABLE_OPENPGP
  {"IDEA-PGP-CFB", GNUTLS_CIPHER_IDEA_PGP_CFB, 8, 16, CIPHER_BLOCK, 8, 0},
  {"3DES-PGP-CFB", GNUTLS_CIPHER_3DES_PGP_CFB, 8, 24, CIPHER_BLOCK, 8, 0},
  {"CAST5-PGP-CFB", GNUTLS_CIPHER_CAST5_PGP_CFB, 8, 16, CIPHER_BLOCK, 8, 0},
  {"BLOWFISH-PGP-CFB", GNUTLS_CIPHER_BLOWFISH_PGP_CFB, 8,
   16 /*actually unlimited */ , CIPHER_BLOCK, 8, 0},
  {"SAFER-SK128-PGP-CFB", GNUTLS_CIPHER_SAFER_SK128_PGP_CFB, 8, 16,
   CIPHER_BLOCK, 8, 0},
  {"AES-128-PGP-CFB", GNUTLS_CIPHER_AES128_PGP_CFB, 16, 16, CIPHER_BLOCK, 16,
   0},
  {"AES-192-PGP-CFB", GNUTLS_CIPHER_AES192_PGP_CFB, 16, 24, CIPHER_BLOCK, 16,
   0},
  {"AES-256-PGP-CFB", GNUTLS_CIPHER_AES256_PGP_CFB, 16, 32, CIPHER_BLOCK, 16,
   0},
  {"TWOFISH-PGP-CFB", GNUTLS_CIPHER_TWOFISH_PGP_CFB, 16, 16, CIPHER_BLOCK, 16,
   0},
#endif
  {"NULL", GNUTLS_CIPHER_NULL, 1, 0, CIPHER_STREAM, 0, 0},
  {0, 0, 0, 0, 0, 0, 0}
};

/* Keep the contents of this struct the same as the previous one. */
static const gnutls_cipher_algorithm_t supported_ciphers[] = {
  GNUTLS_CIPHER_AES_256_CBC,
  GNUTLS_CIPHER_AES_128_CBC,
  GNUTLS_CIPHER_3DES_CBC,
  GNUTLS_CIPHER_DES_CBC,
  GNUTLS_CIPHER_ARCFOUR_128,
  GNUTLS_CIPHER_ARCFOUR_40,
  GNUTLS_CIPHER_RC2_40_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_256_CBC,
  GNUTLS_CIPHER_CAMELLIA_128_CBC,
#endif
  GNUTLS_CIPHER_NULL,
  0
};

#define GNUTLS_LOOP(b) \
        const gnutls_cipher_entry *p; \
                for(p = algorithms; p->name != NULL; p++) { b ; }

#define GNUTLS_ALG_LOOP(a) \
                        GNUTLS_LOOP( if(p->id == algorithm) { a; break; } )


struct gnutls_hash_entry
{
  const char *name;
  const char *oid;
  gnutls_mac_algorithm_t id;
  size_t key_size;              /* in case of mac */
};
typedef struct gnutls_hash_entry gnutls_hash_entry;

static const gnutls_hash_entry hash_algorithms[] = {
  {"SHA1", HASH_OID_SHA1, GNUTLS_MAC_SHA1, 20},
  {"MD5", HASH_OID_MD5, GNUTLS_MAC_MD5, 16},
  {"SHA256", HASH_OID_SHA256, GNUTLS_MAC_SHA256, 32},
  {"SHA384", HASH_OID_SHA384, GNUTLS_MAC_SHA384, 48},
  {"SHA512", HASH_OID_SHA512, GNUTLS_MAC_SHA512, 64},
  {"MD2", HASH_OID_MD2, GNUTLS_MAC_MD2, 0},     /* not used as MAC */
  {"RIPEMD160", HASH_OID_RMD160, GNUTLS_MAC_RMD160, 20},
  {"MAC-NULL", NULL, GNUTLS_MAC_NULL, 0},
  {0, 0, 0, 0}
};

/* Keep the contents of this struct the same as the previous one. */
static const gnutls_mac_algorithm_t supported_macs[] = {
  GNUTLS_MAC_SHA1,
  GNUTLS_MAC_MD5,
  GNUTLS_MAC_SHA256,
  GNUTLS_MAC_SHA384,
  GNUTLS_MAC_SHA512,
  GNUTLS_MAC_MD2,
  GNUTLS_MAC_RMD160,
  GNUTLS_MAC_NULL,
  0
};

#define GNUTLS_HASH_LOOP(b) \
        const gnutls_hash_entry *p; \
                for(p = hash_algorithms; p->name != NULL; p++) { b ; }

#define GNUTLS_HASH_ALG_LOOP(a) \
                        GNUTLS_HASH_LOOP( if(p->id == algorithm) { a; break; } )

/* Key Exchange Section */


extern mod_auth_st rsa_auth_struct;
extern mod_auth_st rsa_export_auth_struct;
extern mod_auth_st dhe_rsa_auth_struct;
extern mod_auth_st dhe_dss_auth_struct;
extern mod_auth_st anon_auth_struct;
extern mod_auth_st srp_auth_struct;
extern mod_auth_st psk_auth_struct;
extern mod_auth_st dhe_psk_auth_struct;
extern mod_auth_st srp_rsa_auth_struct;
extern mod_auth_st srp_dss_auth_struct;

struct gnutls_kx_algo_entry
{
  const char *name;
  gnutls_kx_algorithm_t algorithm;
  mod_auth_st *auth_struct;
  int needs_dh_params;
  int needs_rsa_params;
};
typedef struct gnutls_kx_algo_entry gnutls_kx_algo_entry;

static const gnutls_kx_algo_entry _gnutls_kx_algorithms[] = {
#ifdef ENABLE_ANON
  {"ANON-DH", GNUTLS_KX_ANON_DH, &anon_auth_struct, 1, 0},
#endif
  {"RSA", GNUTLS_KX_RSA, &rsa_auth_struct, 0, 0},
  {"RSA-EXPORT", GNUTLS_KX_RSA_EXPORT, &rsa_export_auth_struct, 0,
   1 /* needs RSA params */ },
  {"DHE-RSA", GNUTLS_KX_DHE_RSA, &dhe_rsa_auth_struct, 1, 0},
  {"DHE-DSS", GNUTLS_KX_DHE_DSS, &dhe_dss_auth_struct, 1, 0},

#ifdef ENABLE_SRP
  {"SRP-DSS", GNUTLS_KX_SRP_DSS, &srp_dss_auth_struct, 0, 0},
  {"SRP-RSA", GNUTLS_KX_SRP_RSA, &srp_rsa_auth_struct, 0, 0},
  {"SRP", GNUTLS_KX_SRP, &srp_auth_struct, 0, 0},
#endif
#ifdef ENABLE_PSK
  {"PSK", GNUTLS_KX_PSK, &psk_auth_struct, 0, 0},
  {"DHE-PSK", GNUTLS_KX_DHE_PSK, &dhe_psk_auth_struct,
   1 /* needs DHE params */ , 0},
#endif
  {0, 0, 0, 0, 0}
};

/* Keep the contents of this struct the same as the previous one. */
static const gnutls_kx_algorithm_t supported_kxs[] = {
#ifdef ENABLE_ANON
  GNUTLS_KX_ANON_DH,
#endif
  GNUTLS_KX_RSA,
  GNUTLS_KX_RSA_EXPORT,
  GNUTLS_KX_DHE_RSA,
  GNUTLS_KX_DHE_DSS,
#ifdef ENABLE_SRP
  GNUTLS_KX_SRP_DSS,
  GNUTLS_KX_SRP_RSA,
  GNUTLS_KX_SRP,
#endif
#ifdef ENABLE_PSK
  GNUTLS_KX_PSK,
  GNUTLS_KX_DHE_PSK,
#endif
  0
};

#define GNUTLS_KX_LOOP(b) \
        const gnutls_kx_algo_entry *p; \
                for(p = _gnutls_kx_algorithms; p->name != NULL; p++) { b ; }

#define GNUTLS_KX_ALG_LOOP(a) \
                        GNUTLS_KX_LOOP( if(p->algorithm == algorithm) { a; break; } )



/* Cipher SUITES */
#define GNUTLS_CIPHER_SUITE_ENTRY( name, block_algorithm, kx_algorithm, mac_algorithm, min_version, max_version ) \
	{ #name, {name}, block_algorithm, kx_algorithm, mac_algorithm, min_version, max_version }

typedef struct
{
  const char *name;
  cipher_suite_st id;
  gnutls_cipher_algorithm_t block_algorithm;
  gnutls_kx_algorithm_t kx_algorithm;
  gnutls_mac_algorithm_t mac_algorithm;
  gnutls_protocol_t min_version;        /* this cipher suite is supported
                                         * from 'version' and above;
                                         */
  gnutls_protocol_t max_version;        /* this cipher suite is not supported after that */
} gnutls_cipher_suite_entry;

/* RSA with NULL cipher and MD5 MAC
 * for test purposes.
 */
#define GNUTLS_RSA_NULL_MD5 { 0x00, 0x01 }
#define GNUTLS_RSA_NULL_SHA1 { 0x00, 0x02 }
#define GNUTLS_RSA_NULL_SHA256 { 0x00, 0x3B }

/* ANONymous cipher suites.
 */

#define GNUTLS_ANON_DH_3DES_EDE_CBC_SHA1 { 0x00, 0x1B }
#define GNUTLS_ANON_DH_ARCFOUR_MD5 { 0x00, 0x18 }

 /* rfc3268: */
#define GNUTLS_ANON_DH_AES_128_CBC_SHA1 { 0x00, 0x34 }
#define GNUTLS_ANON_DH_AES_256_CBC_SHA1 { 0x00, 0x3A }

/* rfc4132 */
#ifdef	ENABLE_CAMELLIA
#define GNUTLS_ANON_DH_CAMELLIA_128_CBC_SHA1 { 0x00,0x46 }
#define GNUTLS_ANON_DH_CAMELLIA_256_CBC_SHA1 { 0x00,0x89 }
#endif

#define GNUTLS_ANON_DH_AES_128_CBC_SHA256 { 0x00, 0x6C }
#define GNUTLS_ANON_DH_AES_256_CBC_SHA256 { 0x00, 0x6D }

/* PSK (not in TLS 1.0)
 * draft-ietf-tls-psk:
 */
#define GNUTLS_PSK_SHA_ARCFOUR_SHA1 { 0x00, 0x8A }
#define GNUTLS_PSK_SHA_3DES_EDE_CBC_SHA1 { 0x00, 0x8B }
#define GNUTLS_PSK_SHA_AES_128_CBC_SHA1 { 0x00, 0x8C }
#define GNUTLS_PSK_SHA_AES_256_CBC_SHA1 { 0x00, 0x8D }

#define GNUTLS_DHE_PSK_SHA_ARCFOUR_SHA1 { 0x00, 0x8E }
#define GNUTLS_DHE_PSK_SHA_3DES_EDE_CBC_SHA1 { 0x00, 0x8F }
#define GNUTLS_DHE_PSK_SHA_AES_128_CBC_SHA1 { 0x00, 0x90 }
#define GNUTLS_DHE_PSK_SHA_AES_256_CBC_SHA1 { 0x00, 0x91 }


/* SRP (rfc5054)
 */
#define GNUTLS_SRP_SHA_3DES_EDE_CBC_SHA1 { 0xC0, 0x1A }
#define GNUTLS_SRP_SHA_RSA_3DES_EDE_CBC_SHA1 { 0xC0, 0x1B }
#define GNUTLS_SRP_SHA_DSS_3DES_EDE_CBC_SHA1 { 0xC0, 0x1C }

#define GNUTLS_SRP_SHA_AES_128_CBC_SHA1 { 0xC0, 0x1D }
#define GNUTLS_SRP_SHA_RSA_AES_128_CBC_SHA1 { 0xC0, 0x1E }
#define GNUTLS_SRP_SHA_DSS_AES_128_CBC_SHA1 { 0xC0, 0x1F }

#define GNUTLS_SRP_SHA_AES_256_CBC_SHA1 { 0xC0, 0x20 }
#define GNUTLS_SRP_SHA_RSA_AES_256_CBC_SHA1 { 0xC0, 0x21 }
#define GNUTLS_SRP_SHA_DSS_AES_256_CBC_SHA1 { 0xC0, 0x22 }

/* RSA
 */
#define GNUTLS_RSA_ARCFOUR_SHA1 { 0x00, 0x05 }
#define GNUTLS_RSA_ARCFOUR_MD5 { 0x00, 0x04 }
#define GNUTLS_RSA_3DES_EDE_CBC_SHA1 { 0x00, 0x0A }

#define GNUTLS_RSA_EXPORT_ARCFOUR_40_MD5 { 0x00, 0x03 }

/* rfc3268:
 */
#define GNUTLS_RSA_AES_128_CBC_SHA1 { 0x00, 0x2F }
#define GNUTLS_RSA_AES_256_CBC_SHA1 { 0x00, 0x35 }

/* rfc4132 */
#ifdef	ENABLE_CAMELLIA
#define GNUTLS_RSA_CAMELLIA_128_CBC_SHA1 { 0x00,0x41 }
#define GNUTLS_RSA_CAMELLIA_256_CBC_SHA1 { 0x00,0x84 }
#endif

#define GNUTLS_RSA_AES_128_CBC_SHA256 { 0x00, 0x3C }
#define GNUTLS_RSA_AES_256_CBC_SHA256 { 0x00, 0x3D }

/* DHE DSS
 */

#define GNUTLS_DHE_DSS_3DES_EDE_CBC_SHA1 { 0x00, 0x13 }


/* draft-ietf-tls-56-bit-ciphersuites-01:
 */
#define GNUTLS_DHE_DSS_ARCFOUR_SHA1 { 0x00, 0x66 }


/* rfc3268:
 */
#define GNUTLS_DHE_DSS_AES_256_CBC_SHA1 { 0x00, 0x38 }
#define GNUTLS_DHE_DSS_AES_128_CBC_SHA1 { 0x00, 0x32 }

/* rfc4132 */
#ifdef	ENABLE_CAMELLIA
#define GNUTLS_DHE_DSS_CAMELLIA_128_CBC_SHA1 { 0x00,0x44 }
#define GNUTLS_DHE_DSS_CAMELLIA_256_CBC_SHA1 { 0x00,0x87 }
#endif

#define GNUTLS_DHE_DSS_AES_128_CBC_SHA256 { 0x00, 0x40 }
#define GNUTLS_DHE_DSS_AES_256_CBC_SHA256 { 0x00, 0x6A }

/* DHE RSA
 */
#define GNUTLS_DHE_RSA_3DES_EDE_CBC_SHA1 { 0x00, 0x16 }

/* rfc3268:
 */
#define GNUTLS_DHE_RSA_AES_128_CBC_SHA1 { 0x00, 0x33 }
#define GNUTLS_DHE_RSA_AES_256_CBC_SHA1 { 0x00, 0x39 }

/* rfc4132 */
#ifdef	ENABLE_CAMELLIA
#define GNUTLS_DHE_RSA_CAMELLIA_128_CBC_SHA1 { 0x00,0x45 }
#define GNUTLS_DHE_RSA_CAMELLIA_256_CBC_SHA1 { 0x00,0x88 }
#endif

#define GNUTLS_DHE_RSA_AES_128_CBC_SHA256 { 0x00, 0x67 }
#define GNUTLS_DHE_RSA_AES_256_CBC_SHA256 { 0x00, 0x6B }

/* Safe renegotiation */

#define CIPHER_SUITES_COUNT sizeof(cs_algorithms)/sizeof(gnutls_cipher_suite_entry)-1

static const gnutls_cipher_suite_entry cs_algorithms[] = {
  /* ANON_DH */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_ARCFOUR_MD5,
                             GNUTLS_CIPHER_ARCFOUR_128,
                             GNUTLS_KX_ANON_DH, GNUTLS_MAC_MD5,
                             GNUTLS_SSL3, GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_ANON_DH,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_ANON_DH,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_ANON_DH,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_CAMELLIA_128_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_128_CBC,
                             GNUTLS_KX_ANON_DH,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_CAMELLIA_256_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_256_CBC,
                             GNUTLS_KX_ANON_DH,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
#endif
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_AES_128_CBC_SHA256,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_ANON_DH,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_ANON_DH_AES_256_CBC_SHA256,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_ANON_DH,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),

  /* PSK */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_PSK_SHA_ARCFOUR_SHA1,
                             GNUTLS_CIPHER_ARCFOUR, GNUTLS_KX_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_PSK_SHA_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_PSK_SHA_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_PSK_SHA_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  /* DHE-PSK */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_PSK_SHA_ARCFOUR_SHA1,
                             GNUTLS_CIPHER_ARCFOUR, GNUTLS_KX_DHE_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_PSK_SHA_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_DHE_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_PSK_SHA_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_DHE_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_PSK_SHA_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_DHE_PSK,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  /* SRP */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_SRP,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_SRP,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_SRP,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_DSS_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_SRP_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_RSA_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_SRP_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_DSS_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_SRP_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_RSA_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_SRP_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_DSS_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_SRP_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_SRP_SHA_RSA_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_SRP_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),

  /* DHE_DSS */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_ARCFOUR_SHA1,
                             GNUTLS_CIPHER_ARCFOUR_128, GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_CAMELLIA_128_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_128_CBC,
                             GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_CAMELLIA_256_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_256_CBC,
                             GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
#endif
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_AES_128_CBC_SHA256,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_DSS_AES_256_CBC_SHA256,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_DHE_DSS,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),
  /* DHE_RSA */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_RSA_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC, GNUTLS_KX_DHE_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_RSA_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_DHE_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_RSA_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_DHE_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_RSA_CAMELLIA_128_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_128_CBC,
                             GNUTLS_KX_DHE_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_RSA_CAMELLIA_256_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_256_CBC,
                             GNUTLS_KX_DHE_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
#endif
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_RSA_AES_128_CBC_SHA256,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_DHE_RSA,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_DHE_RSA_AES_256_CBC_SHA256,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_DHE_RSA,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),
  /* RSA-NULL */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_NULL_MD5,
                             GNUTLS_CIPHER_NULL,
                             GNUTLS_KX_RSA, GNUTLS_MAC_MD5, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_NULL_SHA1,
                             GNUTLS_CIPHER_NULL,
                             GNUTLS_KX_RSA, GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_NULL_SHA256,
                             GNUTLS_CIPHER_NULL,
                             GNUTLS_KX_RSA, GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),

  /* RSA-EXPORT */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_EXPORT_ARCFOUR_40_MD5,
                             GNUTLS_CIPHER_ARCFOUR_40,
                             GNUTLS_KX_RSA_EXPORT, GNUTLS_MAC_MD5,
                             GNUTLS_SSL3, GNUTLS_TLS1_0),

  /* RSA */
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_ARCFOUR_SHA1,
                             GNUTLS_CIPHER_ARCFOUR_128,
                             GNUTLS_KX_RSA, GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_ARCFOUR_MD5,
                             GNUTLS_CIPHER_ARCFOUR_128,
                             GNUTLS_KX_RSA, GNUTLS_MAC_MD5, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_3DES_EDE_CBC_SHA1,
                             GNUTLS_CIPHER_3DES_CBC,
                             GNUTLS_KX_RSA, GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_AES_128_CBC_SHA1,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_AES_256_CBC_SHA1,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_SSL3,
                             GNUTLS_VERSION_MAX),
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_CAMELLIA_128_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_128_CBC, GNUTLS_KX_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_CAMELLIA_256_CBC_SHA1,
                             GNUTLS_CIPHER_CAMELLIA_256_CBC, GNUTLS_KX_RSA,
                             GNUTLS_MAC_SHA1, GNUTLS_TLS1,
                             GNUTLS_VERSION_MAX),
#endif
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_AES_128_CBC_SHA256,
                             GNUTLS_CIPHER_AES_128_CBC, GNUTLS_KX_RSA,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),
  GNUTLS_CIPHER_SUITE_ENTRY (GNUTLS_RSA_AES_256_CBC_SHA256,
                             GNUTLS_CIPHER_AES_256_CBC, GNUTLS_KX_RSA,
                             GNUTLS_MAC_SHA256, GNUTLS_TLS1_2,
                             GNUTLS_VERSION_MAX),
  {0, {{0, 0}}, 0, 0, 0, 0, 0}
};

#define GNUTLS_CIPHER_SUITE_LOOP(b) \
        const gnutls_cipher_suite_entry *p; \
                for(p = cs_algorithms; p->name != NULL; p++) { b ; }

#define GNUTLS_CIPHER_SUITE_ALG_LOOP(a) \
                        GNUTLS_CIPHER_SUITE_LOOP( if( (p->id.suite[0] == suite->suite[0]) && (p->id.suite[1] == suite->suite[1])) { a; break; } )



/* Generic Functions */

int
_gnutls_mac_priority (gnutls_session_t session,
                      gnutls_mac_algorithm_t algorithm)
{                               /* actually returns the priority */
  unsigned int i;
  for (i = 0; i < session->internals.priorities.mac.algorithms; i++)
    {
      if (session->internals.priorities.mac.priority[i] == algorithm)
        return i;
    }
  return -1;
}

/**
 * gnutls_mac_get_name:
 * @algorithm: is a MAC algorithm
 *
 * Convert a #gnutls_mac_algorithm_t value to a string.
 *
 * Returns: a string that contains the name of the specified MAC
 *   algorithm, or %NULL.
 **/
const char *
gnutls_mac_get_name (gnutls_mac_algorithm_t algorithm)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_HASH_ALG_LOOP (ret = p->name);

  return ret;
}

/**
 * gnutls_mac_get_id:
 * @name: is a MAC algorithm name
 *
 * Convert a string to a #gnutls_mac_algorithm_t value.  The names are
 * compared in a case insensitive way.
 *
 * Returns: a #gnutls_mac_algorithm_t id of the specified MAC
 *   algorithm string, or %GNUTLS_MAC_UNKNOWN on failures.
 **/
gnutls_mac_algorithm_t
gnutls_mac_get_id (const char *name)
{
  gnutls_mac_algorithm_t ret = GNUTLS_MAC_UNKNOWN;

  GNUTLS_HASH_LOOP (if (strcasecmp (p->name, name) == 0) ret = p->id);

  return ret;
}

/**
 * gnutls_mac_get_key_size:
 * @algorithm: is an encryption algorithm
 *
 * Get size of MAC key.
 *
 * Returns: length (in bytes) of the given MAC key size, or 0 if the
 *   given MAC algorithm is invalid.
 **/
size_t
gnutls_mac_get_key_size (gnutls_mac_algorithm_t algorithm)
{
  size_t ret = 0;

  /* avoid prefix */
  GNUTLS_HASH_ALG_LOOP (ret = p->key_size);

  return ret;
}

/**
 * gnutls_mac_list:
 *
 * Get a list of hash algorithms for use as MACs.  Note that not
 * necessarily all MACs are supported in TLS cipher suites.  For
 * example, MD2 is not supported as a cipher suite, but is supported
 * for other purposes (e.g., X.509 signature verification or similar).
 *
 * Returns: Return a zero-terminated list of #gnutls_mac_algorithm_t
 *   integers indicating the available MACs.
 **/
const gnutls_mac_algorithm_t *
gnutls_mac_list (void)
{
  return supported_macs;
}

const char *
_gnutls_x509_mac_to_oid (gnutls_mac_algorithm_t algorithm)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_HASH_ALG_LOOP (ret = p->oid);

  return ret;
}

gnutls_mac_algorithm_t
_gnutls_x509_oid2mac_algorithm (const char *oid)
{
  gnutls_mac_algorithm_t ret = 0;

  GNUTLS_HASH_LOOP (if (p->oid && strcmp (oid, p->oid) == 0)
                    {
                    ret = p->id; break;}
  );

  if (ret == 0)
    return GNUTLS_MAC_UNKNOWN;
  return ret;
}


int
_gnutls_mac_is_ok (gnutls_mac_algorithm_t algorithm)
{
  ssize_t ret = -1;
  GNUTLS_HASH_ALG_LOOP (ret = p->id);
  if (ret >= 0)
    ret = 0;
  else
    ret = 1;
  return ret;
}

/* CIPHER functions */

/**
 * gnutls_cipher_get_block_size:
 * @algorithm: is an encryption algorithm
 *
 * Get block size for encryption algorithm.
 *
 * Returns: block size for encryption algorithm.
 *
 * Since: 2.10.0
 **/
int
gnutls_cipher_get_block_size (gnutls_cipher_algorithm_t algorithm)
{
  size_t ret = 0;
  GNUTLS_ALG_LOOP (ret = p->blocksize);
  return ret;

}

 /* returns the priority */
int
_gnutls_cipher_priority (gnutls_session_t session,
                         gnutls_cipher_algorithm_t algorithm)
{
  unsigned int i;
  for (i = 0; i < session->internals.priorities.cipher.algorithms; i++)
    {
      if (session->internals.priorities.cipher.priority[i] == algorithm)
        return i;
    }
  return -1;
}


int
_gnutls_cipher_is_block (gnutls_cipher_algorithm_t algorithm)
{
  size_t ret = 0;

  GNUTLS_ALG_LOOP (ret = p->block);
  return ret;

}

/**
 * gnutls_cipher_get_key_size:
 * @algorithm: is an encryption algorithm
 *
 * Get key size for cipher.
 *
 * Returns: length (in bytes) of the given cipher's key size, or 0 if
 *   the given cipher is invalid.
 **/
size_t
gnutls_cipher_get_key_size (gnutls_cipher_algorithm_t algorithm)
{                               /* In bytes */
  size_t ret = 0;
  GNUTLS_ALG_LOOP (ret = p->keysize);
  return ret;

}

int
_gnutls_cipher_get_iv_size (gnutls_cipher_algorithm_t algorithm)
{                               /* In bytes */
  size_t ret = 0;
  GNUTLS_ALG_LOOP (ret = p->iv);
  return ret;

}

int
_gnutls_cipher_get_export_flag (gnutls_cipher_algorithm_t algorithm)
{                               /* In bytes */
  size_t ret = 0;
  GNUTLS_ALG_LOOP (ret = p->export_flag);
  return ret;

}

/**
 * gnutls_cipher_get_name:
 * @algorithm: is an encryption algorithm
 *
 * Convert a #gnutls_cipher_algorithm_t type to a string.
 *
 * Returns: a pointer to a string that contains the name of the
 *   specified cipher, or %NULL.
 **/
const char *
gnutls_cipher_get_name (gnutls_cipher_algorithm_t algorithm)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_ALG_LOOP (ret = p->name);

  return ret;
}

/**
 * gnutls_cipher_get_id:
 * @name: is a MAC algorithm name
 *
 * The names are compared in a case insensitive way.
 *
 * Returns: return a #gnutls_cipher_algorithm_t value corresponding to
 *   the specified cipher, or %GNUTLS_CIPHER_UNKNOWN on error.
 **/
gnutls_cipher_algorithm_t
gnutls_cipher_get_id (const char *name)
{
  gnutls_cipher_algorithm_t ret = GNUTLS_CIPHER_UNKNOWN;

  GNUTLS_LOOP (if (strcasecmp (p->name, name) == 0) ret = p->id);

  return ret;
}

/**
 * gnutls_cipher_list:
 *
 * Get a list of supported cipher algorithms.  Note that not
 * necessarily all ciphers are supported as TLS cipher suites.  For
 * example, DES is not supported as a cipher suite, but is supported
 * for other purposes (e.g., PKCS#8 or similar).
 *
 * Returns: a zero-terminated list of #gnutls_cipher_algorithm_t
 *   integers indicating the available ciphers.
 *
 **/
const gnutls_cipher_algorithm_t *
gnutls_cipher_list (void)
{
  return supported_ciphers;
}

int
_gnutls_cipher_is_ok (gnutls_cipher_algorithm_t algorithm)
{
  ssize_t ret = -1;
  GNUTLS_ALG_LOOP (ret = p->id);
  if (ret >= 0)
    ret = 0;
  else
    ret = 1;
  return ret;
}

/* Key EXCHANGE functions */
mod_auth_st *
_gnutls_kx_auth_struct (gnutls_kx_algorithm_t algorithm)
{
  mod_auth_st *ret = NULL;
  GNUTLS_KX_ALG_LOOP (ret = p->auth_struct);
  return ret;

}


int
_gnutls_kx_priority (gnutls_session_t session,
                     gnutls_kx_algorithm_t algorithm)
{
  unsigned int i;
  for (i = 0; i < session->internals.priorities.kx.algorithms; i++)
    {
      if (session->internals.priorities.kx.priority[i] == algorithm)
        return i;
    }
  return -1;
}

/**
 * gnutls_kx_get_name:
 * @algorithm: is a key exchange algorithm
 *
 * Convert a #gnutls_kx_algorithm_t value to a string.
 *
 * Returns: a pointer to a string that contains the name of the
 *   specified key exchange algorithm, or %NULL.
 **/
const char *
gnutls_kx_get_name (gnutls_kx_algorithm_t algorithm)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_KX_ALG_LOOP (ret = p->name);

  return ret;
}

/**
 * gnutls_kx_get_id:
 * @name: is a KX name
 *
 * Convert a string to a #gnutls_kx_algorithm_t value.  The names are
 * compared in a case insensitive way.
 *
 * Returns: an id of the specified KX algorithm, or %GNUTLS_KX_UNKNOWN
 *   on error.
 **/
gnutls_kx_algorithm_t
gnutls_kx_get_id (const char *name)
{
  gnutls_cipher_algorithm_t ret = GNUTLS_KX_UNKNOWN;

  GNUTLS_KX_LOOP (if (strcasecmp (p->name, name) == 0) ret = p->algorithm);

  return ret;
}

/**
 * gnutls_kx_list:
 *
 * Get a list of supported key exchange algorithms.
 *
 * Returns: a zero-terminated list of #gnutls_kx_algorithm_t integers
 * indicating the available key exchange algorithms.
 **/
const gnutls_kx_algorithm_t *
gnutls_kx_list (void)
{
  return supported_kxs;
}

int
_gnutls_kx_is_ok (gnutls_kx_algorithm_t algorithm)
{
  ssize_t ret = -1;
  GNUTLS_KX_ALG_LOOP (ret = p->algorithm);
  if (ret >= 0)
    ret = 0;
  else
    ret = 1;
  return ret;
}

int
_gnutls_kx_needs_rsa_params (gnutls_kx_algorithm_t algorithm)
{
  ssize_t ret = 0;
  GNUTLS_KX_ALG_LOOP (ret = p->needs_rsa_params);
  return ret;
}

int
_gnutls_kx_needs_dh_params (gnutls_kx_algorithm_t algorithm)
{
  ssize_t ret = 0;
  GNUTLS_KX_ALG_LOOP (ret = p->needs_dh_params);
  return ret;
}


/* Version */
int
_gnutls_version_priority (gnutls_session_t session, gnutls_protocol_t version)
{                               /* actually returns the priority */
  unsigned int i;

  for (i = 0; i < session->internals.priorities.protocol.algorithms; i++)
    {
      if (session->internals.priorities.protocol.priority[i] == version)
        return i;
    }
  return -1;
}

gnutls_protocol_t
_gnutls_version_lowest (gnutls_session_t session)
{                               /* returns the lowest version supported */
  unsigned int i, min = 0xff;

  for (i = 0; i < session->internals.priorities.protocol.algorithms; i++)
    {
      if (session->internals.priorities.protocol.priority[i] < min)
        min = session->internals.priorities.protocol.priority[i];
    }

  if (min == 0xff)
    return GNUTLS_VERSION_UNKNOWN;      /* unknown version */

  return min;
}

gnutls_protocol_t
_gnutls_version_max (gnutls_session_t session)
{                               /* returns the maximum version supported */
  unsigned int i, max = 0x00;

  for (i = 0; i < session->internals.priorities.protocol.algorithms; i++)
    {
      if (session->internals.priorities.protocol.priority[i] > max)
        max = session->internals.priorities.protocol.priority[i];
    }

  if (max == 0x00)
    return GNUTLS_VERSION_UNKNOWN;      /* unknown version */

  return max;
}


/**
 * gnutls_protocol_get_name:
 * @version: is a (gnutls) version number
 *
 * Convert a #gnutls_protocol_t value to a string.
 *
 * Returns: a string that contains the name of the specified TLS
 *   version (e.g., "TLS1.0"), or %NULL.
 **/
const char *
gnutls_protocol_get_name (gnutls_protocol_t version)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_VERSION_ALG_LOOP (ret = p->name);
  return ret;
}

/**
 * gnutls_protocol_get_id:
 * @name: is a protocol name
 *
 * The names are compared in a case insensitive way.
 *
 * Returns: an id of the specified protocol, or
 * %GNUTLS_VERSION_UNKNOWN on error.
 **/
gnutls_protocol_t
gnutls_protocol_get_id (const char *name)
{
  gnutls_protocol_t ret = GNUTLS_VERSION_UNKNOWN;

  GNUTLS_VERSION_LOOP (if (strcasecmp (p->name, name) == 0) ret = p->id);

  return ret;
}

/**
 * gnutls_protocol_list:
 *
 * Get a list of supported protocols, e.g. SSL 3.0, TLS 1.0 etc.
 *
 * Returns: a zero-terminated list of #gnutls_protocol_t integers
 * indicating the available protocols.
 *
 **/
const gnutls_protocol_t *
gnutls_protocol_list (void)
{
  return supported_protocols;
}

int
_gnutls_version_get_minor (gnutls_protocol_t version)
{
  int ret = -1;

  GNUTLS_VERSION_ALG_LOOP (ret = p->minor);
  return ret;
}

gnutls_protocol_t
_gnutls_version_get (int major, int minor)
{
  int ret = -1;

  GNUTLS_VERSION_LOOP (if ((p->major == major) && (p->minor == minor))
                       ret = p->id);
  return ret;
}

int
_gnutls_version_get_major (gnutls_protocol_t version)
{
  int ret = -1;

  GNUTLS_VERSION_ALG_LOOP (ret = p->major);
  return ret;
}

/* Version Functions */

int
_gnutls_version_is_supported (gnutls_session_t session,
                              const gnutls_protocol_t version)
{
  int ret = 0;

  GNUTLS_VERSION_ALG_LOOP (ret = p->supported);
  if (ret == 0)
    return 0;

  if (_gnutls_version_priority (session, version) < 0)
    return 0;                   /* disabled by the user */
  else
    return 1;
}


/* This function determines if the version specified has a
   cipher-suite selected PRF hash function instead of the old
   hardcoded MD5+SHA1. */
int
_gnutls_version_has_selectable_prf (gnutls_protocol_t version)
{
  return version == GNUTLS_TLS1_2;
}

/* This function determines if the version specified has selectable
   signature/hash functions for certificate authentification. */
int
_gnutls_version_has_selectable_sighash (gnutls_protocol_t version)
{
  return version == GNUTLS_TLS1_2;
}

/* This function determines if the version specified has support for
   TLS extensions. */
int
_gnutls_version_has_extensions (gnutls_protocol_t version)
{
  switch (version)
    {
    case GNUTLS_TLS1_0:
    case GNUTLS_TLS1_1:
    case GNUTLS_TLS1_2:
      return 1;
    default:
      return 0;
    }
}

/* This function determines if the version specified has explicit IVs
   (for CBC attack prevention). */
int
_gnutls_version_has_explicit_iv (gnutls_protocol_t version)
{
  switch (version)
    {
    case GNUTLS_TLS1_1:
    case GNUTLS_TLS1_2:
      return 1;
    default:
      return 0;
    }
}

/* This function determines if the version specified can have
   non-minimal padding. */
int
_gnutls_version_has_variable_padding (gnutls_protocol_t version)
{
  switch (version)
    {
    case GNUTLS_TLS1_0:
    case GNUTLS_TLS1_1:
    case GNUTLS_TLS1_2:
      return 1;
    default:
      return 0;
    }
}

/* Type to KX mappings */
gnutls_kx_algorithm_t
_gnutls_map_kx_get_kx (gnutls_credentials_type_t type, int server)
{
  gnutls_kx_algorithm_t ret = -1;

  if (server)
    {
      GNUTLS_KX_MAP_ALG_LOOP_SERVER (ret = p->algorithm);
    }
  else
    {
      GNUTLS_KX_MAP_ALG_LOOP_SERVER (ret = p->algorithm);
    }
  return ret;
}

gnutls_credentials_type_t
_gnutls_map_kx_get_cred (gnutls_kx_algorithm_t algorithm, int server)
{
  gnutls_credentials_type_t ret = -1;
  if (server)
    {
      GNUTLS_KX_MAP_LOOP (if (p->algorithm == algorithm) ret =
                          p->server_type);
    }
  else
    {
      GNUTLS_KX_MAP_LOOP (if (p->algorithm == algorithm) ret =
                          p->client_type);
    }

  return ret;
}


/* Cipher Suite's functions */
gnutls_cipher_algorithm_t
_gnutls_cipher_suite_get_cipher_algo (const cipher_suite_st * suite)
{
  int ret = 0;
  GNUTLS_CIPHER_SUITE_ALG_LOOP (ret = p->block_algorithm);
  return ret;
}

gnutls_protocol_t
_gnutls_cipher_suite_is_version_supported (const cipher_suite_st * suite,
                                           gnutls_protocol_t version)
{
  int ret = 0;
  GNUTLS_CIPHER_SUITE_ALG_LOOP ((version >= p->min_version
                                 && version <= p->max_version) ? (ret =
                                                                  1) : (ret =
                                                                        0));
  return ret;
}

gnutls_kx_algorithm_t
_gnutls_cipher_suite_get_kx_algo (const cipher_suite_st * suite)
{
  int ret = 0;

  GNUTLS_CIPHER_SUITE_ALG_LOOP (ret = p->kx_algorithm);
  return ret;

}

gnutls_mac_algorithm_t
_gnutls_cipher_suite_get_mac_algo (const cipher_suite_st * suite)
{                               /* In bytes */
  int ret = 0;
  GNUTLS_CIPHER_SUITE_ALG_LOOP (ret = p->mac_algorithm);
  return ret;

}

const char *
_gnutls_cipher_suite_get_name (cipher_suite_st * suite)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_CIPHER_SUITE_ALG_LOOP (ret = p->name + sizeof ("GNUTLS_") - 1);

  return ret;
}

/**
 * gnutls_cipher_suite_get_name:
 * @kx_algorithm: is a Key exchange algorithm
 * @cipher_algorithm: is a cipher algorithm
 * @mac_algorithm: is a MAC algorithm
 *
 * Note that the full cipher suite name must be prepended by TLS or
 * SSL depending of the protocol in use.
 *
 * Returns: a string that contains the name of a TLS cipher suite,
 * specified by the given algorithms, or %NULL.
 **/
const char *
gnutls_cipher_suite_get_name (gnutls_kx_algorithm_t kx_algorithm,
                              gnutls_cipher_algorithm_t cipher_algorithm,
                              gnutls_mac_algorithm_t mac_algorithm)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_CIPHER_SUITE_LOOP (if (kx_algorithm == p->kx_algorithm &&
                                cipher_algorithm == p->block_algorithm &&
                                mac_algorithm == p->mac_algorithm)
                            ret = p->name + sizeof ("GNUTLS_") - 1);

  return ret;
}

/**
 * gnutls_cipher_suite_info:
 * @idx: index of cipher suite to get information about, starts on 0.
 * @cs_id: output buffer with room for 2 bytes, indicating cipher suite value
 * @kx: output variable indicating key exchange algorithm, or %NULL.
 * @cipher: output variable indicating cipher, or %NULL.
 * @mac: output variable indicating MAC algorithm, or %NULL.
 * @version: output variable indicating TLS protocol version, or %NULL.
 *
 * Get information about supported cipher suites.  Use the function
 * iteratively to get information about all supported cipher suites.
 * Call with idx=0 to get information about first cipher suite, then
 * idx=1 and so on until the function returns NULL.
 *
 * Returns: the name of @idx cipher suite, and set the information
 * about the cipher suite in the output variables.  If @idx is out of
 * bounds, %NULL is returned.
 **/
const char *
gnutls_cipher_suite_info (size_t idx,
                          char *cs_id,
                          gnutls_kx_algorithm_t * kx,
                          gnutls_cipher_algorithm_t * cipher,
                          gnutls_mac_algorithm_t * mac,
                          gnutls_protocol_t * min_version)
{
  if (idx >= CIPHER_SUITES_COUNT)
    return NULL;

  if (cs_id)
    memcpy (cs_id, cs_algorithms[idx].id.suite, 2);
  if (kx)
    *kx = cs_algorithms[idx].kx_algorithm;
  if (cipher)
    *cipher = cs_algorithms[idx].block_algorithm;
  if (mac)
    *mac = cs_algorithms[idx].mac_algorithm;
  if (min_version)
    *min_version = cs_algorithms[idx].min_version;

  return cs_algorithms[idx].name + sizeof ("GNU") - 1;
}


static inline int
_gnutls_cipher_suite_is_ok (cipher_suite_st * suite)
{
  size_t ret;
  const char *name = NULL;

  GNUTLS_CIPHER_SUITE_ALG_LOOP (name = p->name);
  if (name != NULL)
    ret = 0;
  else
    ret = 1;
  return ret;

}

#define SWAP(x, y) memcpy(tmp,x,size); \
		   memcpy(x,y,size); \
		   memcpy(y,tmp,size);

#define MAX_ELEM_SIZE 4
static inline int
_gnutls_partition (gnutls_session_t session, void *_base,
                   size_t nmemb, size_t size,
                   int (*compar) (gnutls_session_t,
                                  const void *, const void *))
{
  uint8_t *base = _base;
  uint8_t tmp[MAX_ELEM_SIZE];
  uint8_t ptmp[MAX_ELEM_SIZE];
  unsigned int pivot;
  unsigned int i, j;
  unsigned int full;

  i = pivot = 0;
  j = full = (nmemb - 1) * size;

  memcpy (ptmp, &base[0], size);        /* set pivot item */

  while (i < j)
    {
      while ((compar (session, &base[i], ptmp) <= 0) && (i < full))
        {
          i += size;
        }
      while ((compar (session, &base[j], ptmp) >= 0) && (j > 0))
        j -= size;

      if (i < j)
        {
          SWAP (&base[j], &base[i]);
        }
    }

  if (j > pivot)
    {
      SWAP (&base[pivot], &base[j]);
      pivot = j;
    }
  else if (i < pivot)
    {
      SWAP (&base[pivot], &base[i]);
      pivot = i;
    }
  return pivot / size;
}

static void
_gnutls_qsort (gnutls_session_t session, void *_base, size_t nmemb,
               size_t size, int (*compar) (gnutls_session_t, const void *,
                                           const void *))
{
  unsigned int pivot;
  char *base = _base;
  size_t snmemb = nmemb;

#ifdef DEBUG
  if (size > MAX_ELEM_SIZE)
    {
      gnutls_assert ();
      _gnutls_debug_log ("QSORT BUG\n");
      exit (1);
    }
#endif

  if (snmemb <= 1)
    return;
  pivot = _gnutls_partition (session, _base, nmemb, size, compar);

  _gnutls_qsort (session, base, pivot < nmemb ? pivot + 1 : pivot, size,
                 compar);
  _gnutls_qsort (session, &base[(pivot + 1) * size], nmemb - pivot - 1,
                 size, compar);
}


/* a compare function for KX algorithms (using priorities). 
 * For use with qsort 
 */
static int
_gnutls_compare_algo (gnutls_session_t session, const void *i_A1,
                      const void *i_A2)
{
  gnutls_kx_algorithm_t kA1 =
    _gnutls_cipher_suite_get_kx_algo ((const cipher_suite_st *) i_A1);
  gnutls_kx_algorithm_t kA2 =
    _gnutls_cipher_suite_get_kx_algo ((const cipher_suite_st *) i_A2);
  gnutls_cipher_algorithm_t cA1 =
    _gnutls_cipher_suite_get_cipher_algo ((const cipher_suite_st *) i_A1);
  gnutls_cipher_algorithm_t cA2 =
    _gnutls_cipher_suite_get_cipher_algo ((const cipher_suite_st *) i_A2);
  gnutls_mac_algorithm_t mA1 =
    _gnutls_cipher_suite_get_mac_algo ((const cipher_suite_st *) i_A1);
  gnutls_mac_algorithm_t mA2 =
    _gnutls_cipher_suite_get_mac_algo ((const cipher_suite_st *) i_A2);

  int p1 = (_gnutls_kx_priority (session, kA1) + 1) * 64;
  int p2 = (_gnutls_kx_priority (session, kA2) + 1) * 64;
  p1 += (_gnutls_cipher_priority (session, cA1) + 1) * 8;
  p2 += (_gnutls_cipher_priority (session, cA2) + 1) * 8;
  p1 += _gnutls_mac_priority (session, mA1);
  p2 += _gnutls_mac_priority (session, mA2);

  if (p1 > p2)
    {
      return 1;
    }
  else
    {
      if (p1 == p2)
        {
          return 0;
        }
      return -1;
    }
}

#ifdef SORT_DEBUG
static void
_gnutls_bsort (gnutls_session_t session, void *_base, size_t nmemb,
               size_t size, int (*compar) (gnutls_session_t, const void *,
                                           const void *))
{
  unsigned int i, j;
  int full = nmemb * size;
  char *base = _base;
  char tmp[MAX_ELEM_SIZE];

  for (i = 0; i < full; i += size)
    {
      for (j = 0; j < full; j += size)
        {
          if (compar (session, &base[i], &base[j]) < 0)
            {
              SWAP (&base[j], &base[i]);
            }
        }
    }

}
#endif

int
_gnutls_supported_ciphersuites_sorted (gnutls_session_t session,
                                       cipher_suite_st ** ciphers)
{

#ifdef SORT_DEBUG
  unsigned int i;
#endif
  int count;

  count = _gnutls_supported_ciphersuites (session, ciphers);
  if (count <= 0)
    {
      gnutls_assert ();
      return count;
    }
#ifdef SORT_DEBUG
  _gnutls_debug_log ("Unsorted: \n");
  for (i = 0; i < count; i++)
    _gnutls_debug_log ("\t%d: %s\n", i,
                       _gnutls_cipher_suite_get_name ((*ciphers)[i]));
#endif

  _gnutls_qsort (session, *ciphers, count,
                 sizeof (cipher_suite_st), _gnutls_compare_algo);

#ifdef SORT_DEBUG
  _gnutls_debug_log ("Sorted: \n");
  for (i = 0; i < count; i++)
    _gnutls_debug_log ("\t%d: %s\n", i,
                       _gnutls_cipher_suite_get_name ((*ciphers)[i]));
#endif

  return count;
}

int
_gnutls_supported_ciphersuites (gnutls_session_t session,
                                cipher_suite_st ** _ciphers)
{

  unsigned int i, ret_count, j;
  unsigned int count = CIPHER_SUITES_COUNT;
  cipher_suite_st *tmp_ciphers;
  cipher_suite_st *ciphers;
  gnutls_protocol_t version;

  if (count == 0)
    {
      return 0;
    }

  tmp_ciphers = gnutls_malloc (count * sizeof (cipher_suite_st));
  if (tmp_ciphers == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  ciphers = gnutls_malloc (count * sizeof (cipher_suite_st));
  if (ciphers == NULL)
    {
      gnutls_free (tmp_ciphers);
      return GNUTLS_E_MEMORY_ERROR;
    }

  version = gnutls_protocol_get_version (session);

  for (i = 0; i < count; i++)
    {
      memcpy (&tmp_ciphers[i], &cs_algorithms[i].id,
              sizeof (cipher_suite_st));
    }

  for (i = j = 0; i < count; i++)
    {
      /* remove private cipher suites, if requested.
       */
      if (tmp_ciphers[i].suite[0] == 0xFF &&
          session->internals.enable_private == 0)
        continue;

      /* remove cipher suites which do not support the
       * protocol version used.
       */
      if (_gnutls_cipher_suite_is_version_supported (&tmp_ciphers[i], version)
          == 0)
        continue;

      if (_gnutls_kx_priority
          (session, _gnutls_cipher_suite_get_kx_algo (&tmp_ciphers[i])) < 0)
        continue;
      if (_gnutls_mac_priority
          (session, _gnutls_cipher_suite_get_mac_algo (&tmp_ciphers[i])) < 0)
        continue;
      if (_gnutls_cipher_priority
          (session,
           _gnutls_cipher_suite_get_cipher_algo (&tmp_ciphers[i])) < 0)
        continue;

      memcpy (&ciphers[j], &tmp_ciphers[i], sizeof (cipher_suite_st));
      j++;
    }

  ret_count = j;

#if 0                           /* expensive */
  if (ret_count > 0 && ret_count != count)
    {
      ciphers =
        gnutls_realloc_fast (ciphers, ret_count * sizeof (cipher_suite_st));
    }
  else
    {
      if (ret_count != count)
        {
          gnutls_free (ciphers);
          ciphers = NULL;
        }
    }
#endif

  gnutls_free (tmp_ciphers);

  /* This function can no longer return 0 cipher suites.
   * It returns an error code instead.
   */
  if (ret_count == 0)
    {
      gnutls_assert ();
      gnutls_free (ciphers);
      return GNUTLS_E_NO_CIPHER_SUITES;
    }
  *_ciphers = ciphers;
  return ret_count;
}

/**
 * gnutls_certificate_type_get_name:
 * @type: is a certificate type
 *
 * Convert a #gnutls_certificate_type_t type to a string.
 *
 * Returns: a string that contains the name of the specified
 *   certificate type, or %NULL in case of unknown types.
 **/
const char *
gnutls_certificate_type_get_name (gnutls_certificate_type_t type)
{
  const char *ret = NULL;

  if (type == GNUTLS_CRT_X509)
    ret = "X.509";
  if (type == GNUTLS_CRT_OPENPGP)
    ret = "OPENPGP";

  return ret;
}

/**
 * gnutls_certificate_type_get_id:
 * @name: is a certificate type name
 *
 * The names are compared in a case insensitive way.
 *
 * Returns: a #gnutls_certificate_type_t for the specified in a
 *   string certificate type, or %GNUTLS_CRT_UNKNOWN on error.
 **/
gnutls_certificate_type_t
gnutls_certificate_type_get_id (const char *name)
{
  gnutls_certificate_type_t ret = GNUTLS_CRT_UNKNOWN;

  if (strcasecmp (name, "X.509") == 0 || strcasecmp (name, "X509") == 0)
    return GNUTLS_CRT_X509;
  if (strcasecmp (name, "OPENPGP") == 0)
    return GNUTLS_CRT_OPENPGP;

  return ret;
}

static const gnutls_certificate_type_t supported_certificate_types[] = {
  GNUTLS_CRT_X509,
  GNUTLS_CRT_OPENPGP,
  0
};

/**
 * gnutls_certificate_type_list:
 *
 * Get a list of certificate types.  Note that to be able to use
 * OpenPGP certificates, you must link to libgnutls-extra and call
 * gnutls_global_init_extra().
 *
 * Returns: a zero-terminated list of #gnutls_certificate_type_t
 *   integers indicating the available certificate types.
 **/
const gnutls_certificate_type_t *
gnutls_certificate_type_list (void)
{
  return supported_certificate_types;
}

/* returns the gnutls_pk_algorithm_t which is compatible with
 * the given gnutls_kx_algorithm_t.
 */
gnutls_pk_algorithm_t
_gnutls_map_pk_get_pk (gnutls_kx_algorithm_t kx_algorithm)
{
  gnutls_pk_algorithm_t ret = -1;

  GNUTLS_PK_MAP_ALG_LOOP (ret = p->pk_algorithm) return ret;
}

/* Returns the encipher type for the given key exchange algorithm.
 * That one of CIPHER_ENCRYPT, CIPHER_SIGN, CIPHER_IGN.
 *
 * ex. GNUTLS_KX_RSA requires a certificate able to encrypt... so returns CIPHER_ENCRYPT.
 */
enum encipher_type
_gnutls_kx_encipher_type (gnutls_kx_algorithm_t kx_algorithm)
{
  int ret = CIPHER_IGN;
  GNUTLS_PK_MAP_ALG_LOOP (ret = p->encipher_type) return ret;

}

/* signature algorithms;
 */
struct gnutls_sign_entry
{
  const char *name;
  const char *oid;
  gnutls_sign_algorithm_t id;
  gnutls_pk_algorithm_t pk;
  gnutls_mac_algorithm_t mac;
  /* See RFC 5246 HashAlgorithm and SignatureAlgorithm
     for values to use in aid struct. */
  const sign_algorithm_st aid;
};
typedef struct gnutls_sign_entry gnutls_sign_entry;

#define TLS_SIGN_AID_UNKNOWN {255, 255}
static const sign_algorithm_st unknown_tls_aid = TLS_SIGN_AID_UNKNOWN;

static const gnutls_sign_entry sign_algorithms[] = {
  {"RSA-SHA1", SIG_RSA_SHA1_OID, GNUTLS_SIGN_RSA_SHA1, GNUTLS_PK_RSA,
   GNUTLS_MAC_SHA1, {2, 1}},
  {"RSA-SHA224", SIG_RSA_SHA224_OID, GNUTLS_SIGN_RSA_SHA224, GNUTLS_PK_RSA,
   GNUTLS_MAC_SHA224, {3, 1}},
  {"RSA-SHA256", SIG_RSA_SHA256_OID, GNUTLS_SIGN_RSA_SHA256, GNUTLS_PK_RSA,
   GNUTLS_MAC_SHA256, {4, 1}},
  {"RSA-SHA384", SIG_RSA_SHA384_OID, GNUTLS_SIGN_RSA_SHA384, GNUTLS_PK_RSA,
   GNUTLS_MAC_SHA384, {5, 1}},
  {"RSA-SHA512", SIG_RSA_SHA512_OID, GNUTLS_SIGN_RSA_SHA512, GNUTLS_PK_RSA,
   GNUTLS_MAC_SHA512, {6, 1}},
  {"RSA-RMD160", SIG_RSA_RMD160_OID, GNUTLS_SIGN_RSA_RMD160, GNUTLS_PK_RSA,
   GNUTLS_MAC_RMD160, TLS_SIGN_AID_UNKNOWN},
  {"DSA-SHA1", SIG_DSA_SHA1_OID, GNUTLS_SIGN_DSA_SHA1, GNUTLS_PK_DSA,
   GNUTLS_MAC_SHA1, {2, 2}},
  {"DSA-SHA224", SIG_DSA_SHA224_OID, GNUTLS_SIGN_DSA_SHA224, GNUTLS_PK_DSA,
   GNUTLS_MAC_SHA224, {3, 2}},
  {"DSA-SHA256", SIG_DSA_SHA256_OID, GNUTLS_SIGN_DSA_SHA256, GNUTLS_PK_DSA,
   GNUTLS_MAC_SHA256, {4, 2}},
  {"RSA-MD5", SIG_RSA_MD5_OID, GNUTLS_SIGN_RSA_MD5, GNUTLS_PK_RSA,
   GNUTLS_MAC_MD5, {1, 1}},
  {"RSA-MD2", SIG_RSA_MD2_OID, GNUTLS_SIGN_RSA_MD2, GNUTLS_PK_RSA,
   GNUTLS_MAC_MD2, TLS_SIGN_AID_UNKNOWN},
  {"GOST R 34.10-2001", SIG_GOST_R3410_2001_OID, 0, 0, 0,
   TLS_SIGN_AID_UNKNOWN},
  {"GOST R 34.10-94", SIG_GOST_R3410_94_OID, 0, 0, 0, TLS_SIGN_AID_UNKNOWN},
  {0, 0, 0, 0, 0, TLS_SIGN_AID_UNKNOWN}
};

/* Keep the contents of this struct the same as the previous one. */
static const gnutls_sign_algorithm_t supported_sign[] = {
  GNUTLS_SIGN_RSA_SHA1,
  GNUTLS_SIGN_RSA_SHA224,
  GNUTLS_SIGN_RSA_SHA256,
  GNUTLS_SIGN_RSA_SHA384,
  GNUTLS_SIGN_RSA_SHA512,
  GNUTLS_SIGN_RSA_RMD160,
  GNUTLS_SIGN_DSA_SHA1,
  GNUTLS_SIGN_DSA_SHA224,
  GNUTLS_SIGN_DSA_SHA256,
  GNUTLS_SIGN_RSA_MD5,
  GNUTLS_SIGN_RSA_MD2,
  0
};

#define GNUTLS_SIGN_LOOP(b) \
  do {								       \
    const gnutls_sign_entry *p;					       \
    for(p = sign_algorithms; p->name != NULL; p++) { b ; }	       \
  } while (0)

#define GNUTLS_SIGN_ALG_LOOP(a) \
  GNUTLS_SIGN_LOOP( if(p->id && p->id == sign) { a; break; } )

/**
 * gnutls_sign_algorithm_get_name:
 * @sign: is a sign algorithm
 *
 * Convert a #gnutls_sign_algorithm_t value to a string.
 *
 * Returns: a string that contains the name of the specified sign
 *   algorithm, or %NULL.
 **/
const char *
gnutls_sign_algorithm_get_name (gnutls_sign_algorithm_t sign)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_SIGN_ALG_LOOP (ret = p->name);

  return ret;
}

/**
 * gnutls_sign_list:
 *
 * Get a list of supported public key signature algorithms.
 *
 * Returns: a zero-terminated list of #gnutls_sign_algorithm_t
 *   integers indicating the available ciphers.
 *
 **/
const gnutls_sign_algorithm_t *
gnutls_sign_list (void)
{
  return supported_sign;
}

/**
 * gnutls_sign_get_id:
 * @name: is a MAC algorithm name
 *
 * The names are compared in a case insensitive way.
 *
 * Returns: return a #gnutls_sign_algorithm_t value corresponding to
 *   the specified cipher, or %GNUTLS_SIGN_UNKNOWN on error.
 **/
gnutls_sign_algorithm_t
gnutls_sign_get_id (const char *name)
{
  gnutls_sign_algorithm_t ret = GNUTLS_SIGN_UNKNOWN;

  GNUTLS_SIGN_LOOP (if (strcasecmp (p->name, name) == 0) ret = p->id);

  return ret;

}

/**
 * gnutls_sign_get_name:
 * @algorithm: is a public key signature algorithm
 *
 * Convert a #gnutls_sign_algorithm_t value to a string.
 *
 * Returns: a pointer to a string that contains the name of the
 *   specified public key signature algorithm, or %NULL.
 *
 * Since: 2.6.0
 **/
const char *
gnutls_sign_get_name (gnutls_sign_algorithm_t algorithm)
{
  const char *ret = "SIGN_UNKNOWN";

  GNUTLS_SIGN_LOOP (if (p->id == algorithm) ret = p->name);

  return ret;
}

gnutls_sign_algorithm_t
_gnutls_x509_oid2sign_algorithm (const char *oid)
{
  gnutls_sign_algorithm_t ret = 0;

  GNUTLS_SIGN_LOOP (if (p->oid && strcmp (oid, p->oid) == 0)
                    {
                    ret = p->id; break;}
  );

  if (ret == 0)
    {
      _gnutls_x509_log ("Unknown SIGN OID: '%s'\n", oid);
      return GNUTLS_SIGN_UNKNOWN;
    }
  return ret;
}

gnutls_sign_algorithm_t
_gnutls_x509_pk_to_sign (gnutls_pk_algorithm_t pk, gnutls_mac_algorithm_t mac)
{
  gnutls_sign_algorithm_t ret = 0;

  GNUTLS_SIGN_LOOP (if (pk == p->pk && mac == p->mac)
                    {
                    ret = p->id; break;}
  );

  if (ret == 0)
    return GNUTLS_SIGN_UNKNOWN;
  return ret;
}

const char *
_gnutls_x509_sign_to_oid (gnutls_pk_algorithm_t pk,
                          gnutls_mac_algorithm_t mac)
{
  gnutls_sign_algorithm_t sign;
  const char *ret = NULL;

  sign = _gnutls_x509_pk_to_sign (pk, mac);
  if (sign == GNUTLS_SIGN_UNKNOWN)
    return NULL;

  GNUTLS_SIGN_ALG_LOOP (ret = p->oid);
  return ret;
}

gnutls_mac_algorithm_t
_gnutls_sign_get_hash_algorithm (gnutls_sign_algorithm_t sign)
{
  gnutls_mac_algorithm_t ret = GNUTLS_DIG_UNKNOWN;

  GNUTLS_SIGN_ALG_LOOP (ret = p->mac);

  return ret;
}

gnutls_pk_algorithm_t
_gnutls_sign_get_pk_algorithm (gnutls_sign_algorithm_t sign)
{
  gnutls_pk_algorithm_t ret = GNUTLS_PK_UNKNOWN;

  GNUTLS_SIGN_ALG_LOOP (ret = p->pk);

  return ret;
}

gnutls_sign_algorithm_t
_gnutls_tls_aid_to_sign (const sign_algorithm_st * aid)
{
  gnutls_sign_algorithm_t ret = GNUTLS_SIGN_UNKNOWN;

  if (memcmp(aid, &unknown_tls_aid, sizeof(*aid))==0)
    return ret;

  GNUTLS_SIGN_LOOP (if (p->aid.hash_algorithm == aid->hash_algorithm
                        && p->aid.sign_algorithm == aid->sign_algorithm)
                    {
                      ret = p->id; break;
                    }
  );


  return ret;
}

/* Returns NULL if a valid AID is not found
 */
const sign_algorithm_st*
_gnutls_sign_to_tls_aid (gnutls_sign_algorithm_t sign)
{
  const sign_algorithm_st * ret = NULL;

  GNUTLS_SIGN_ALG_LOOP (ret = &p->aid);

  if (ret != NULL && memcmp(ret, &unknown_tls_aid, sizeof(*ret))==0)
    return NULL;

  return ret;
}



/* pk algorithms;
 */
struct gnutls_pk_entry
{
  const char *name;
  const char *oid;
  gnutls_pk_algorithm_t id;
};
typedef struct gnutls_pk_entry gnutls_pk_entry;

static const gnutls_pk_entry pk_algorithms[] = {
  /* having duplicate entries is ok, as long as the one
   * we want to return OID from is first */
  {"UNKNOWN", NULL, GNUTLS_PK_UNKNOWN},
  {"RSA", PK_PKIX1_RSA_OID, GNUTLS_PK_RSA},
  {"RSA (X.509)", PK_X509_RSA_OID, GNUTLS_PK_RSA},      /* some certificates use this OID for RSA */
  {"RSA (MD5)", SIG_RSA_MD5_OID, GNUTLS_PK_RSA},        /* some other broken certificates set RSA with MD5 as an indicator of RSA */
  {"RSA (SHA1)", SIG_RSA_SHA1_OID, GNUTLS_PK_RSA},      /* some other broken certificates set RSA with SHA1 as an indicator of RSA */
  {"DSA", PK_DSA_OID, GNUTLS_PK_DSA},
  {"GOST R 34.10-2001", PK_GOST_R3410_2001_OID, GNUTLS_PK_UNKNOWN},
  {"GOST R 34.10-94", PK_GOST_R3410_94_OID, GNUTLS_PK_UNKNOWN},
  {0, 0, 0}
};

/**
 * gnutls_pk_algorithm_get_name:
 * @algorithm: is a pk algorithm
 *
 * Convert a #gnutls_pk_algorithm_t value to a string.
 *
 * Returns: a string that contains the name of the specified public
 *   key algorithm, or %NULL.
 **/
const char *
gnutls_pk_algorithm_get_name (gnutls_pk_algorithm_t algorithm)
{
  const char *ret = NULL;
  const gnutls_pk_entry *p;

  for (p = pk_algorithms; p->name != NULL; p++)
    if (p->id == algorithm)
      {
        ret = p->name;
        break;
      }

  return ret;
}

/**
 * gnutls_pk_list:
 *
 * Get a list of supported public key algorithms.
 *
 * Returns: a zero-terminated list of #gnutls_pk_algorithm_t integers
 *   indicating the available ciphers.
 *
 * Since: 2.6.0
 **/
const gnutls_pk_algorithm_t *
gnutls_pk_list (void)
{
  static const gnutls_pk_algorithm_t supported_pks[] = {
    GNUTLS_PK_RSA,
    GNUTLS_PK_DSA,
    /* GNUTLS_PK_DH is not returned because it is not
     * a real public key algorithm. I.e. cannot be used
     * as a public key algorithm of a certificate.
     */
    0
  };

  return supported_pks;
}

/**
 * gnutls_pk_get_id:
 * @name: is a string containing a public key algorithm name.
 *
 * Convert a string to a #gnutls_pk_algorithm_t value.  The names are
 * compared in a case insensitive way.  For example,
 * gnutls_pk_get_id("RSA") will return %GNUTLS_PK_RSA.
 *
 * Returns: a #gnutls_pk_algorithm_t id of the specified public key
 *   algorithm string, or %GNUTLS_PK_UNKNOWN on failures.
 *
 * Since: 2.6.0
 **/
gnutls_pk_algorithm_t
gnutls_pk_get_id (const char *name)
{
  gnutls_pk_algorithm_t ret = GNUTLS_PK_UNKNOWN;
  const gnutls_pk_entry *p;

  for (p = pk_algorithms; p->name != NULL; p++)
    if (name && strcmp (p->name, name) == 0)
      {
        ret = p->id;
        break;
      }

  return ret;
}

/**
 * gnutls_pk_get_name:
 * @algorithm: is a public key algorithm
 *
 * Convert a #gnutls_pk_algorithm_t value to a string.
 *
 * Returns: a pointer to a string that contains the name of the
 *   specified public key algorithm, or %NULL.
 *
 * Since: 2.6.0
 **/
const char *
gnutls_pk_get_name (gnutls_pk_algorithm_t algorithm)
{
  const char *ret = "Unknown";
  const gnutls_pk_entry *p;

  for (p = pk_algorithms; p->name != NULL; p++)
    if (algorithm == p->id)
      {
        ret = p->name;
        break;
      }

  return ret;
}

gnutls_pk_algorithm_t
_gnutls_x509_oid2pk_algorithm (const char *oid)
{
  gnutls_pk_algorithm_t ret = GNUTLS_PK_UNKNOWN;
  const gnutls_pk_entry *p;

  for (p = pk_algorithms; p->name != NULL; p++)
    if (p->oid && strcmp (p->oid, oid) == 0)
      {
        ret = p->id;
        break;
      }

  return ret;
}

const char *
_gnutls_x509_pk_to_oid (gnutls_pk_algorithm_t algorithm)
{
  const char *ret = NULL;
  const gnutls_pk_entry *p;

  for (p = pk_algorithms; p->name != NULL; p++)
    if (p->id == algorithm)
      {
        ret = p->oid;
        break;
      }

  return ret;
}

/**
 * gnutls_sec_param_to_pk_bits:
 * @algo: is a public key algorithm
 * @param: is a security parameter
 *
 * When generating private and public key pairs a difficult question
 * is which size of "bits" the modulus will be in RSA and the group size
 * in DSA. The easy answer is 1024, which is also wrong. This function
 * will convert a human understandable security parameter to an
 * appropriate size for the specific algorithm.
 *
 * Returns: The number of bits, or zero.
 *
 **/
unsigned int
gnutls_sec_param_to_pk_bits (gnutls_pk_algorithm_t algo,
                             gnutls_sec_param_t param)
{
  unsigned int ret = 0;

  /* handle DSA differently */
  if (algo == GNUTLS_PK_DSA)
    {
      GNUTLS_SEC_PARAM_LOOP (if (p->sec_param == param)
                             {
                             ret = p->dsa_bits; break;}
      );
      return ret;
    }

  GNUTLS_SEC_PARAM_LOOP (if (p->sec_param == param)
                         {
                         ret = p->pk_bits; break;}
  );

  return ret;
}

/* Returns the corresponding size for subgroup bits (q),
 * given the group bits (p).
 */
unsigned int
_gnutls_pk_bits_to_subgroup_bits (unsigned int pk_bits)
{
  unsigned int ret = 0;

  GNUTLS_SEC_PARAM_LOOP (if (p->pk_bits >= pk_bits)
                         {
                         ret = p->subgroup_bits; break;}
  );

  return ret;
}

/**
 * gnutls_sec_param_get_name:
 * @param: is a security parameter
 *
 * Convert a #gnutls_sec_param_t value to a string.
 *
 * Returns: a pointer to a string that contains the name of the
 *   specified public key algorithm, or %NULL.
 *
 **/
const char *
gnutls_sec_param_get_name (gnutls_sec_param_t param)
{
  const char *ret = "Unknown";

  GNUTLS_SEC_PARAM_LOOP (if (p->sec_param == param)
                         {
                         ret = p->name; break;}
  );

  return ret;
}

/**
 * gnutls_pk_bits_to_sec_param:
 * @algo: is a public key algorithm
 * @bits: is the number of bits
 *
 * This is the inverse of gnutls_sec_param_to_pk_bits(). Given an algorithm
 * and the number of bits, it will return the security parameter. This is
 * a rough indication.
 *
 * Returns: The security parameter.
 *
 **/
gnutls_sec_param_t
gnutls_pk_bits_to_sec_param (gnutls_pk_algorithm_t algo, unsigned int bits)
{
  gnutls_sec_param_t ret = GNUTLS_SEC_PARAM_WEAK;

  GNUTLS_SEC_PARAM_LOOP (if (p->pk_bits > bits)
                         {
                         break;}
                         ret = p->sec_param;);

  return ret;
}
