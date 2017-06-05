#ifndef CERTTOOL_COMMON_H
#define CERTTOOL_COMMON_H

#include <gnutls/x509.h>
#include <stdio.h>

enum
{
  ACTION_SELF_SIGNED,
  ACTION_GENERATE_PRIVKEY,
  ACTION_CERT_INFO,
  ACTION_GENERATE_REQUEST,
  ACTION_GENERATE_CERTIFICATE,
  ACTION_VERIFY_CHAIN,
  ACTION_PRIVKEY_INFO,
  ACTION_UPDATE_CERTIFICATE,
  ACTION_TO_PKCS12,
  ACTION_PKCS12_INFO,
  ACTION_GENERATE_DH,
  ACTION_GET_DH,
  ACTION_CRL_INFO,
  ACTION_P7_INFO,
  ACTION_GENERATE_CRL,
  ACTION_VERIFY_CRL,
  ACTION_SMIME_TO_P7,
  ACTION_GENERATE_PROXY,
  ACTION_GENERATE_PKCS8,
  ACTION_PGP_INFO,
  ACTION_PGP_PRIVKEY_INFO,
  ACTION_RING_INFO,
  ACTION_REQUEST,
  ACTION_PUBKEY_INFO,
  ACTION_CERT_PUBKEY,
};

#define TYPE_CRT 1
#define TYPE_CRQ 2

void certtool_version (void);

#include <gnutls/x509.h>
#include <gnutls/abstract.h>

typedef struct common_info
{
  const char *secret_key;
  const char *privkey;
  const char *pubkey;
  int pkcs8;
  int incert_format;
  const char *cert;

  const char *request;
  const char *ca;
  const char *ca_privkey;
} common_info_st;

gnutls_pubkey_t load_public_key_or_import(int mand, gnutls_privkey_t privkey, common_info_st * info);
gnutls_privkey_t load_private_key (int mand, common_info_st * info);
gnutls_x509_privkey_t load_x509_private_key (int mand, common_info_st * info);
gnutls_x509_crq_t load_request (common_info_st * info);
gnutls_privkey_t load_ca_private_key (common_info_st * info);
gnutls_x509_crt_t load_ca_cert (common_info_st * info);
gnutls_x509_crt_t load_cert (int mand, common_info_st * info);
gnutls_datum *load_secret_key (int mand, common_info_st * info);
gnutls_pubkey_t load_pubkey (int mand, common_info_st * info);
gnutls_x509_crt_t *load_cert_list (int mand, size_t * size,
                                   common_info_st * info);

/* returns the bits specified in cmd */
int get_bits (gnutls_pk_algorithm_t);

/* prime.c */
int generate_prime (int how);

FILE *safe_open_rw (const char *file, int privkey_op);

extern unsigned char buffer[];
extern const int buffer_size;


#endif
