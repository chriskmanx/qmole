#include <config.h>

#ifdef ENABLE_OPENPGP

#ifndef GNUTLS_OPENPGP_LOCAL_H
#define GNUTLS_OPENPGP_LOCAL_H

#include <auth_cert.h>
#include <opencdk.h>
#include <gnutls/abstract.h>

/* OpenCDK compatible */
typedef enum
{
  KEY_ATTR_NONE = 0,
  KEY_ATTR_SHORT_KEYID = 3,
  KEY_ATTR_KEYID = 4,
  KEY_ATTR_FPR = 5
} key_attr_t;

int gnutls_openpgp_count_key_names (const gnutls_datum_t * cert);

int gnutls_openpgp_get_key (gnutls_datum_t * key,
                            gnutls_openpgp_keyring_t keyring,
                            key_attr_t by, opaque * pattern);

/* internal */
int _gnutls_openpgp_raw_crt_to_gcert (gnutls_cert * cert,
                                      const gnutls_datum_t * raw,
                                      const gnutls_openpgp_keyid_t);

int
_gnutls_openpgp_raw_privkey_to_gkey (gnutls_privkey_t * pkey,
                                     const gnutls_datum_t * raw_key);

int
_gnutls_openpgp_request_key (gnutls_session_t,
                             gnutls_datum_t * ret,
                             const gnutls_certificate_credentials_t cred,
                             opaque * key_fpr, int key_fpr_size);

int _gnutls_openpgp_verify_key (const gnutls_certificate_credentials_t,
                                const gnutls_datum_t * cert_list,
                                int cert_list_length, unsigned int *status);
int _gnutls_openpgp_fingerprint (const gnutls_datum_t * cert,
                                 unsigned char *fpr, size_t * fprlen);
time_t _gnutls_openpgp_get_raw_key_creation_time (const gnutls_datum_t *
                                                  cert);
time_t _gnutls_openpgp_get_raw_key_expiration_time (const gnutls_datum_t *
                                                    cert);

int
_gnutls_openpgp_privkey_sign_hash (gnutls_openpgp_privkey_t key,
                                   const gnutls_datum_t * hash,
                                   gnutls_datum_t * signature);


int
_gnutls_openpgp_privkey_decrypt_data (gnutls_openpgp_privkey_t key,
                                     unsigned int flags,
                                     const gnutls_datum_t * ciphertext,
                                     gnutls_datum_t * plaintext);

#endif /*GNUTLS_OPENPGP_LOCAL_H */

#endif /*ENABLE_OPENPGP */
