/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
 * 2009, 2010 Free Software Foundation, Inc.
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

#ifndef GNUTLS_INT_H
#define GNUTLS_INT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>

#ifdef NO_SSIZE_T
#define HAVE_SSIZE_T
typedef int ssize_t;
#endif

#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <time.h>

/* some systems had problems with long long int, thus,
 * it is not used.
 */
typedef struct
{
  unsigned char i[8];
} uint64;

#include <gnutls/gnutls.h>
#include <system.h>

/*
 * They are not needed any more. You can simply enable
 * the gnutls_log callback to get error descriptions.

#define BUFFERS_DEBUG
#define WRITE_DEBUG
#define READ_DEBUG
#define HANDSHAKE_DEBUG // Prints some information on handshake 
#define COMPRESSION_DEBUG
#define DEBUG
*/

/* The size of a handshake message should not
 * be larger than this value.
 */
#define MAX_HANDSHAKE_PACKET_SIZE 48*1024

#define TLS_MAX_SESSION_ID_SIZE 32

/* The maximum digest size of hash algorithms. 
 */
#define MAX_HASH_SIZE 64
#define MAX_CIPHER_BLOCK_SIZE 16
#define MAX_CIPHER_KEY_SIZE 32

#define MAX_USERNAME_SIZE 128
#define MAX_SERVER_NAME_SIZE 128

#define SESSION_TICKET_KEY_NAME_SIZE 16
#define SESSION_TICKET_KEY_SIZE 16
#define SESSION_TICKET_IV_SIZE 16
#define SESSION_TICKET_MAC_SECRET_SIZE 32

/* we can receive up to MAX_EXT_TYPES extensions.
 */
#define MAX_EXT_TYPES 32

/* The initial size of the receive
 * buffer size. This will grow if larger
 * packets are received.
 */
#define INITIAL_RECV_BUFFER_SIZE 256

/* the default for TCP */
#define DEFAULT_LOWAT 0

/* expire time for resuming sessions */
#define DEFAULT_EXPIRE_TIME 3600

/* the maximum size of encrypted packets */
#define DEFAULT_MAX_RECORD_SIZE 16384
#define RECORD_HEADER_SIZE 5
#define MAX_RECORD_SEND_SIZE (size_t)session->security_parameters.max_record_send_size
#define MAX_RECORD_RECV_SIZE (size_t)session->security_parameters.max_record_recv_size
#define MAX_PAD_SIZE 255
#define EXTRA_COMP_SIZE 2048
#define MAX_RECORD_OVERHEAD (MAX_CIPHER_BLOCK_SIZE/*iv*/+MAX_PAD_SIZE+EXTRA_COMP_SIZE)
#define MAX_RECV_SIZE (MAX_RECORD_OVERHEAD+MAX_RECORD_RECV_SIZE+RECORD_HEADER_SIZE)

#define HANDSHAKE_HEADER_SIZE 4

/* defaults for verification functions
 */
#define DEFAULT_VERIFY_DEPTH 32
#define DEFAULT_VERIFY_BITS 16*1024

#include <gnutls_mem.h>

#define MEMSUB(x,y) ((ssize_t)((ptrdiff_t)x-(ptrdiff_t)y))

#define DECR_LEN(len, x) do { len-=x; if (len<0) {gnutls_assert(); return GNUTLS_E_UNEXPECTED_PACKET_LENGTH;} } while (0)
#define DECR_LENGTH_RET(len, x, RET) do { len-=x; if (len<0) {gnutls_assert(); return RET;} } while (0)
#define DECR_LENGTH_COM(len, x, COM) do { len-=x; if (len<0) {gnutls_assert(); COM;} } while (0)

#define HASH2MAC(x) ((gnutls_mac_algorithm_t)x)

#define GNUTLS_POINTER_TO_INT(_) ((int) GNUTLS_POINTER_TO_INT_CAST (_))
#define GNUTLS_INT_TO_POINTER(_) ((void*) GNUTLS_POINTER_TO_INT_CAST (_))

typedef unsigned char opaque;
typedef struct
{
  opaque pint[3];
} uint24;

#include <gnutls_mpi.h>

typedef enum change_cipher_spec_t
{ GNUTLS_TYPE_CHANGE_CIPHER_SPEC = 1
} change_cipher_spec_t;

typedef enum handshake_state_t
{ STATE0 = 0, STATE1, STATE2,
  STATE3, STATE4, STATE5,
  STATE6, STATE7, STATE8, STATE9, STATE20 = 20, STATE21, STATE22,
  STATE30 = 30, STATE31, STATE40 = 40, STATE41, STATE50 = 50,
  STATE60 = 60, STATE61, STATE62, STATE70, STATE71
} handshake_state_t;

#include <gnutls_str.h>

/* This is the maximum number of algorithms (ciphers or macs etc).
 * keep it synced with GNUTLS_MAX_ALGORITHM_NUM in gnutls.h
 */
#define MAX_ALGOS 16

#define MAX_CIPHERSUITES 256

typedef enum extensions_t
{
  GNUTLS_EXTENSION_SERVER_NAME = 0,
  GNUTLS_EXTENSION_MAX_RECORD_SIZE = 1,
  GNUTLS_EXTENSION_CERT_TYPE = 9,
  GNUTLS_EXTENSION_SRP = 12,
  GNUTLS_EXTENSION_SIGNATURE_ALGORITHMS = 13,
  GNUTLS_EXTENSION_SESSION_TICKET = 35,
  GNUTLS_EXTENSION_INNER_APPLICATION = 37703,
  GNUTLS_EXTENSION_SAFE_RENEGOTIATION = 65281   /* aka: 0xff01 */
} extensions_t;

typedef enum
{ CIPHER_STREAM, CIPHER_BLOCK } cipher_type_t;

#define RESUME_TRUE 0
#define RESUME_FALSE -1

/* Record Protocol */
typedef enum content_type_t
{
  GNUTLS_CHANGE_CIPHER_SPEC = 20, GNUTLS_ALERT,
  GNUTLS_HANDSHAKE, GNUTLS_APPLICATION_DATA,
  GNUTLS_INNER_APPLICATION = 24
} content_type_t;

#define GNUTLS_PK_ANY (gnutls_pk_algorithm_t)-1
#define GNUTLS_PK_NONE (gnutls_pk_algorithm_t)-2

/* Message buffers (mbuffers) structures */

typedef struct mbuffer_st
{
  struct mbuffer_st *next;

  gnutls_datum_t msg;
  /* msg->size - mark = number of bytes left to process in this
     message. Mark should only be non-zero when this buffer is the
     head of the queue. */
  size_t mark;
  unsigned int user_mark;       /* only used during fill in */
  size_t maximum_size;
} mbuffer_st;

typedef struct mbuffer_head_st
{
  mbuffer_st *head;
  mbuffer_st **tail;

  unsigned int length;
  size_t byte_length;
} mbuffer_head_st;

typedef enum
{
  HANDSHAKE_MAC_TYPE_10 = 1,    /* TLS 1.0 style */
  HANDSHAKE_MAC_TYPE_12         /* TLS 1.2 style */
} handshake_mac_type_t;

/* Store & Retrieve functions defines: 
 */

typedef struct auth_cred_st
{
  gnutls_credentials_type_t algorithm;

  /* the type of credentials depends on algorithm 
   */
  void *credentials;
  struct auth_cred_st *next;
} auth_cred_st;

struct gnutls_key_st
{
  /* For DH KX */
  gnutls_datum_t key;
  bigint_t KEY;
  bigint_t client_Y;
  bigint_t client_g;
  bigint_t client_p;
  bigint_t dh_secret;
  /* for SRP */
  bigint_t A;
  bigint_t B;
  bigint_t u;
  bigint_t b;
  bigint_t a;
  bigint_t x;
  /* RSA: e, m
   */
  bigint_t rsa[2];

  /* this is used to hold the peers authentication data 
   */
  /* auth_info_t structures SHOULD NOT contain malloced 
   * elements. Check gnutls_session_pack.c, and gnutls_auth.c.
   * Rememember that this should be calloced!
   */
  void *auth_info;
  gnutls_credentials_type_t auth_info_type;
  int auth_info_size;           /* needed in order to store to db for restoring 
                                 */
  uint8_t crypt_algo;

  auth_cred_st *cred;           /* used to specify keys/certificates etc */

  int certificate_requested;
  /* some ciphersuites use this
   * to provide client authentication.
   * 1 if client auth was requested
   * by the peer, 0 otherwise
   *** In case of a server this
   * holds 1 if we should wait
   * for a client certificate verify
   */
};
typedef struct gnutls_key_st *gnutls_key_st;


struct record_state_st;
typedef struct record_state_st record_state_st;

struct record_parameters_st;
typedef struct record_parameters_st record_parameters_st;

/* STATE (cont) */

#include <gnutls_hash_int.h>
#include <gnutls_cipher_int.h>
#include <gnutls_compress.h>
#include <gnutls_cert.h>

typedef struct
{
  uint8_t suite[2];
} cipher_suite_st;

typedef struct
{
  uint8_t hash_algorithm;
  uint8_t sign_algorithm;       /* pk algorithm actually */
} sign_algorithm_st;

/* This structure holds parameters got from TLS extension
 * mechanism. (some extensions may hold parameters in auth_info_t
 * structures also - see SRP).
 */

#define MAX_SIGNATURE_ALGORITHMS 16

#define MAX_VERIFY_DATA_SIZE 36 /* in SSL 3.0, 12 in TLS 1.0 */

/* auth_info_t structures now MAY contain malloced 
 * elements.
 */

/* This structure and auth_info_t, are stored in the resume database,
 * and are restored, in case of resume.
 * Holds all the required parameters to resume the current 
 * session.
 */

/* if you add anything in Security_Parameters struct, then
 * also modify CPY_COMMON in gnutls_constate.c. 
 */

/* Note that the security parameters structure is set up after the
 * handshake has finished. The only value you may depend on while
 * the handshake is in progress is the cipher suite value.
 */
typedef struct
{
  gnutls_connection_end_t entity;
  gnutls_kx_algorithm_t kx_algorithm;
  handshake_mac_type_t handshake_mac_handle_type;       /* one of HANDSHAKE_TYPE_10 and HANDSHAKE_TYPE_12 */

  /* The epoch used to read and write */
  uint16_t epoch_read;
  uint16_t epoch_write;

  /* The epoch that the next handshake will initialize. */
  uint16_t epoch_next;

  /* The epoch at index 0 of record_parameters. */
  uint16_t epoch_min;

  /* this is the ciphersuite we are going to use 
   * moved here from internals in order to be restored
   * on resume;
   */
  cipher_suite_st current_cipher_suite;
  opaque master_secret[GNUTLS_MASTER_SIZE];
  opaque client_random[GNUTLS_RANDOM_SIZE];
  opaque server_random[GNUTLS_RANDOM_SIZE];
  opaque session_id[TLS_MAX_SESSION_ID_SIZE];
  uint8_t session_id_size;
  time_t timestamp;

  /* The send size is the one requested by the programmer.
   * The recv size is the one negotiated with the peer.
   */
  uint16_t max_record_send_size;
  uint16_t max_record_recv_size;
  /* holds the negotiated certificate type */
  gnutls_certificate_type_t cert_type;
  gnutls_protocol_t version;    /* moved here */

  /* FIXME: The following are not saved in the session storage
   * for session resumption.
   */

  /* Used by extensions that enable supplemental data: Which ones
   * do that? Do they belong in security parameters?
   */
  int do_recv_supplemental, do_send_supplemental;
} security_parameters_st;

struct record_state_st
{
  gnutls_datum_t mac_secret;
  gnutls_datum_t IV;
  gnutls_datum_t key;
  cipher_hd_st cipher_state;
  comp_hd_t compression_state;
  uint64 sequence_number;
};

/* These are used to resolve relative epochs. These values are just
   outside the 16 bit range to prevent off-by-one errors. An absolute
   epoch may be referred to by its numeric id in the range
   0x0000-0xffff. */
#define EPOCH_READ_CURRENT  70000
#define EPOCH_WRITE_CURRENT 70001
#define EPOCH_NEXT          70002

struct record_parameters_st
{
  uint16_t epoch;
  int initialized;

  gnutls_cipher_algorithm_t cipher_algorithm;
  gnutls_mac_algorithm_t mac_algorithm;
  gnutls_compression_method_t compression_algorithm;

  record_state_st read;
  record_state_st write;
};

typedef struct
{
  unsigned int priority[MAX_ALGOS];
  unsigned int algorithms;
} priority_st;

typedef enum
{
  SR_DISABLED,
  SR_UNSAFE,
  SR_PARTIAL,
  SR_SAFE
} safe_renegotiation_t;

/* For the external api */
struct gnutls_priority_st
{
  priority_st cipher;
  priority_st mac;
  priority_st kx;
  priority_st compression;
  priority_st protocol;
  priority_st cert_type;
  priority_st sign_algo;

  /* to disable record padding */
  int no_padding:1;
  int allow_large_records:1;
  safe_renegotiation_t sr;
  int ssl3_record_version:1;
  int additional_verify_flags;
};


/* DH and RSA parameters types.
 */
typedef struct gnutls_dh_params_int
{
  /* [0] is the prime, [1] is the generator.
   */
  bigint_t params[2];
} dh_params_st;

typedef struct
{
  gnutls_dh_params_t dh_params;
  int free_dh_params;
  gnutls_rsa_params_t rsa_params;
  int free_rsa_params;
} internal_params_st;



typedef struct
{
  opaque header[HANDSHAKE_HEADER_SIZE];
  /* this holds the number of bytes in the handshake_header[] */
  size_t header_size;
  /* this holds the length of the handshake packet */
  size_t packet_length;
  gnutls_handshake_description_t recv_type;
} handshake_header_buffer_st;

typedef union
{
  void *ptr;
  uint32_t num;
} extension_priv_data_t;

typedef struct
{
  gnutls_buffer_st application_data_buffer;     /* holds data to be delivered to application layer */
  gnutls_buffer_st handshake_hash_buffer;       /* used to keep the last received handshake 
                                                 * message */
  union
  {
    struct
    {
      digest_hd_st sha;         /* hash of the handshake messages */
      digest_hd_st md5;         /* hash of the handshake messages */
    } tls10;
    struct
    {
      digest_hd_st sha1;        /* hash of the handshake messages for TLS 1.2+ */
      digest_hd_st sha256;      /* hash of the handshake messages for TLS 1.2+ */
    } tls12;
  } handshake_mac_handle;
  int handshake_mac_handle_init;        /* 1 when the previous union and type were initialized */

  gnutls_buffer_st handshake_data_buffer;       /* this is a buffer that holds the current handshake message */
  gnutls_buffer_st ia_data_buffer;      /* holds inner application data (TLS/IA) */
  int resumable:1;              /* TRUE or FALSE - if we can resume that session */
  handshake_state_t handshake_state;    /* holds
                                         * a number which indicates where
                                         * the handshake procedure has been
                                         * interrupted. If it is 0 then
                                         * no interruption has happened.
                                         */

  int invalid_connection:1;     /* true or FALSE - if this session is valid */

  int may_not_read:1;           /* if it's 0 then we can read/write, otherwise it's forbiden to read/write
                                 */
  int may_not_write:1;
  int read_eof:1;               /* non-zero if we have received a closure alert. */

  int last_alert;               /* last alert received */

  /* The last handshake messages sent or received.
   */
  int last_handshake_in;
  int last_handshake_out;

  /* this is the compression method we are going to use */
  gnutls_compression_method_t compression_method;

  /* priorities */
  struct gnutls_priority_st priorities;

  /* resumed session */
  int resumed:1;                /* RESUME_TRUE or FALSE - if we are resuming a session */
  security_parameters_st resumed_security_parameters;
  gnutls_compression_method_t resumed_compression_method;

  /* sockets internals */
  int lowat;

  /* These buffers are used in the handshake
   * protocol only. freed using _gnutls_handshake_io_buffer_clear();
   */
  mbuffer_head_st handshake_send_buffer;
  gnutls_handshake_description_t handshake_send_buffer_htype;
  content_type_t handshake_recv_buffer_type;
  gnutls_handshake_description_t handshake_recv_buffer_htype;
  gnutls_buffer_st handshake_recv_buffer;

  /* this buffer holds a record packet -mostly used for
   * non blocking IO.
   */
  mbuffer_head_st record_recv_buffer;
  mbuffer_head_st record_send_buffer;   /* holds cached data
                                         * for the gnutls_io_write_buffered()
                                         * function.
                                         */
  size_t record_send_buffer_user_size;  /* holds the
                                         * size of the user specified data to
                                         * send.
                                         */


  /* 0 if no peeked data was kept, 1 otherwise.
   */
  int have_peeked_data:1;

  int expire_time;              /* after expire_time seconds this session will expire */
  struct mod_auth_st_int *auth_struct;  /* used in handshake packets and KX algorithms */
  int v2_hello;                 /* 0 if the client hello is v3+.
                                 * non-zero if we got a v2 hello.
                                 */
  /* keeps the headers of the handshake packet 
   */
  handshake_header_buffer_st handshake_header_buffer;

  /* this is the highest version available
   * to the peer. (advertized version).
   * This is obtained by the Handshake Client Hello 
   * message. (some implementations read the Record version)
   */
  uint8_t adv_version_major;
  uint8_t adv_version_minor;

  /* if this is non zero a certificate request message
   * will be sent to the client. - only if the ciphersuite
   * supports it.
   */
  int send_cert_req;

  /* bits to use for DHE and DHA 
   * use _gnutls_dh_get_prime_bits() and gnutls_dh_set_prime_bits() 
   * to access it.
   */
  uint16_t dh_prime_bits;

  size_t max_handshake_data_buffer_size;

  /* PUSH & PULL functions.
   */
  gnutls_pull_func pull_func;
  gnutls_push_func push_func;
  gnutls_vec_push_func vec_push_func;
  gnutls_errno_func errno_func;
  /* Holds the first argument of PUSH and PULL
   * functions;
   */
  gnutls_transport_ptr_t transport_recv_ptr;
  gnutls_transport_ptr_t transport_send_ptr;

  /* STORE & RETRIEVE functions. Only used if other
   * backend than gdbm is used.
   */
  gnutls_db_store_func db_store_func;
  gnutls_db_retr_func db_retrieve_func;
  gnutls_db_remove_func db_remove_func;
  void *db_ptr;

  /* post client hello callback (server side only)
   */
  gnutls_handshake_post_client_hello_func user_hello_func;

  /* holds the selected certificate and key.
   * use _gnutls_selected_certs_deinit() and _gnutls_selected_certs_set()
   * to change them.
   */
  gnutls_cert *selected_cert_list;
  int selected_cert_list_length;
  struct gnutls_privkey_st *selected_key;
  int selected_need_free:1;

  /* holds the extensions we sent to the peer
   * (in case of a client)
   */
  uint16_t extensions_sent[MAX_EXT_TYPES];
  uint16_t extensions_sent_size;

  /* is 0 if we are to send the whole PGP key, or non zero
   * if the fingerprint is to be sent.
   */
  int pgp_fingerprint;

  /* This holds the default version that our first
   * record packet will have. */
  opaque default_record_version[2];

  void *user_ptr;

  int enable_private;           /* non zero to
                                 * enable cipher suites
                                 * which have 0xFF status.
                                 */

  /* Holds 0 if the last called function was interrupted while
   * receiving, and non zero otherwise.
   */
  int direction;

  /* This callback will be used (if set) to receive an
   * openpgp key. (if the peer sends a fingerprint)
   */
  gnutls_openpgp_recv_key_func openpgp_recv_key_func;

  /* If non zero the server will not advertize the CA's he
   * trusts (do not send an RDN sequence).
   */
  int ignore_rdn_sequence;

  /* This is used to set an arbitary version in the RSA
   * PMS secret. Can be used by clients to test whether the
   * server checks that version. (** only used in gnutls-cli-debug)
   */
  opaque rsa_pms_version[2];

  /* Here we cache the DH or RSA parameters got from the
   * credentials structure, or from a callback. That is to
   * minimize external calls.
   */
  internal_params_st params;

  /* This buffer is used by the record recv functions,
   * as a temporary store buffer.
   */
  gnutls_datum_t recv_buffer;

  /* To avoid using global variables, and especially on Windows where
   * the application may use a different errno variable than GnuTLS,
   * it is possible to use gnutls_transport_set_errno to set a
   * session-specific errno variable in the user-replaceable push/pull
   * functions.  This value is used by the send/recv functions.  (The
   * strange name of this variable is because 'errno' is typically
   * #define'd.)
   */
  int errnum;

  /* Function used to perform public-key signing operation during
     handshake.  Used by gnutls_sig.c:_gnutls_tls_sign(), see also
     gnutls_sign_callback_set(). */
  gnutls_sign_func sign_func;
  void *sign_func_userdata;

  /* Callback to extract TLS Finished message. */
  gnutls_finished_callback_func finished_func;

  /* minimum bits to allow for SRP
   * use gnutls_srp_set_prime_bits() to adjust it.
   */
  uint16_t srp_prime_bits;

  int initial_negotiation_completed:1;

  struct
  {
    uint16_t type;
    extension_priv_data_t priv;
    int set:1;
  } extension_int_data[MAX_EXT_TYPES];

  struct
  {
    uint16_t type;
    extension_priv_data_t priv;
    int set:1;
  } resumed_extension_int_data[MAX_EXT_TYPES];

  unsigned int cb_tls_unique_len;
  unsigned char cb_tls_unique[MAX_VERIFY_DATA_SIZE];

  /* If you add anything here, check _gnutls_handshake_internal_state_clear().
   */
} internals_st;

/* Maximum number of epochs we keep around. */
#define MAX_EPOCH_INDEX 16

struct gnutls_session_int
{
  security_parameters_st security_parameters;
  record_parameters_st *record_parameters[MAX_EPOCH_INDEX];
  internals_st internals;
  gnutls_key_st key;
};



/* functions 
 */
void _gnutls_set_current_version (gnutls_session_t session,
                                  gnutls_protocol_t version);
void _gnutls_free_auth_info (gnutls_session_t session);

/* These two macros return the advertized TLS version of
 * the peer.
 */
#define _gnutls_get_adv_version_major( session) \
	session->internals.adv_version_major

#define _gnutls_get_adv_version_minor( session) \
	session->internals.adv_version_minor

#define set_adv_version( session, major, minor) \
	session->internals.adv_version_major = major; \
	session->internals.adv_version_minor = minor

void _gnutls_set_adv_version (gnutls_session_t, gnutls_protocol_t);
gnutls_protocol_t _gnutls_get_adv_version (gnutls_session_t);

int _gnutls_is_secure_mem_null (const void *);

#endif /* GNUTLS_INT_H */
