#ifndef OPENPGP_LOCAL_H
#define OPENPGP_LOCAL_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_OPENPGP

#include <opencdk.h>
#include <gnutls/openpgp.h>

#define KEYID_IMPORT(dst, src) { \
	dst[0] = _gnutls_read_uint32( src); \
	dst[1] = _gnutls_read_uint32( src+4); }

/* Internal context to store the OpenPGP key. */
typedef struct gnutls_openpgp_crt_int
{
  cdk_kbnode_t knode;
  uint8_t preferred_keyid[GNUTLS_OPENPGP_KEYID_SIZE];
  int preferred_set;
} gnutls_openpgp_crt_int;

/* Internal context to store the private OpenPGP key. */
typedef struct gnutls_openpgp_privkey_int
{
  cdk_kbnode_t knode;
  gnutls_openpgp_keyid_t preferred_keyid;
  int preferred_set;
} gnutls_openpgp_privkey_int;


typedef struct gnutls_openpgp_keyring_int
{
  cdk_keydb_hd_t db;
} gnutls_openpgp_keyring_int;

int _gnutls_map_cdk_rc (int rc);

int _gnutls_openpgp_export (cdk_kbnode_t node,
                            gnutls_openpgp_crt_fmt_t format,
                            void *output_data, size_t * output_data_size,
                            int private);

int _gnutls_openpgp_crt_to_gcert (gnutls_cert * gcert,
                                  gnutls_openpgp_crt_t cert);

cdk_packet_t _gnutls_get_valid_subkey (cdk_kbnode_t knode, int key_type);

unsigned int _gnutls_get_pgp_key_usage (unsigned int pgp_usage);

int
_gnutls_openpgp_crt_get_mpis (gnutls_openpgp_crt_t cert, uint32_t keyid[2],
                              bigint_t * params, int *params_size);

int
_gnutls_openpgp_privkey_get_mpis (gnutls_openpgp_privkey_t pkey,
                                  uint32_t keyid[2], bigint_t * params,
                                  int *params_size);

cdk_packet_t _gnutls_openpgp_find_key (cdk_kbnode_t knode, uint32_t keyid[2],
                                       unsigned int priv);

int _gnutls_read_pgp_mpi (cdk_packet_t pkt, unsigned int priv, size_t idx,
                          bigint_t * m);

int _gnutls_openpgp_find_subkey_idx (cdk_kbnode_t knode, uint32_t keyid[2],
                                     unsigned int priv);

int _gnutls_openpgp_get_algo (int cdk_algo);

#endif /* ENABLE_OPENPGP */

#endif /* OPENPGP_LOCAL_H */
