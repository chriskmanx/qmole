////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _SSH_H_INCLUDED
#define _SSH_H_INCLUDED

#include <string.h>

#include "puttymem.h"
#include "network.h"
#include "int64.h"
#include "sshrsa.h"
#include "sshsession.h"

class CSshSession;

typedef struct socket_function_table **Socket;

#ifndef FALSE
#define FALSE 0
#define TRUE (!FALSE)
#endif

struct ssh_channel;

void sshfwd_close(CSshSession &session, struct ssh_channel *c);
int sshfwd_write(CSshSession &session, struct ssh_channel *c, char *, int);
void sshfwd_unthrottle(CSshSession &session, struct ssh_channel *c, int bufsize);

/*
 * Useful thing.
 */
#ifndef lenof
#define lenof(x) ( (sizeof((x))) / (sizeof(*(x))))
#endif

#define SSH_CIPHER_IDEA		1
#define SSH_CIPHER_DES		2
#define SSH_CIPHER_3DES		3
#define SSH_CIPHER_BLOWFISH	6

#ifdef MSCRYPTOAPI
#define APIEXTRA 8
#else
#define APIEXTRA 0
#endif

struct dss_key {
    Bignum p, q, g, y, x;
};


typedef unsigned int word32;
typedef unsigned int uint32;

unsigned long crc32(const void *s, size_t len);
unsigned long crc32_update(unsigned long crc_input, const void *s, size_t len);

/* SSH CRC compensation attack detector */
int detect_attack(CSshSession &session, unsigned char *buf, uint32 len, unsigned char *IV);


struct ssh_cipher {
    void (*sesskey) (CSshSession &session, unsigned char *key);	/* for ssh 1 */
    void (*encrypt) (CSshSession &session, unsigned char *blk, int len);
    void (*decrypt) (CSshSession &session, unsigned char *blk, int len);
    int blksize;
};

struct ssh2_cipher {
    void (*setcsiv)  (CSshSession &session, unsigned char *key);	/* for ssh 2 */
    void (*setcskey) (CSshSession &session, unsigned char *key);	/* for ssh 2 */
    void (*setsciv)  (CSshSession &session, unsigned char *key);	/* for ssh 2 */
    void (*setsckey) (CSshSession &session, unsigned char *key);	/* for ssh 2 */
    void (*encrypt)  (CSshSession &session, unsigned char *blk, int len);
    void (*decrypt)  (CSshSession &session, unsigned char *blk, int len);
    char *name;
    int blksize;
    int keylen;
};

struct ssh2_ciphers {
    int nciphers;
    const struct ssh2_cipher *const *list;
};

struct ssh_mac {
    void (*setcskey) (CSshSession &session, unsigned char *key);
    void (*setsckey) (CSshSession &session, unsigned char *key);
    void (*generate) (CSshSession &session, unsigned char *blk, int len, unsigned long seq);
    int  (*verify)   (CSshSession &session, unsigned char *blk, int len, unsigned long seq);
    char *name;
    int len;
};

struct ssh_kex {
    /*
     * Plugging in another KEX algorithm requires structural chaos,
     * so it's hard to abstract them into nice little structures
     * like this. Hence, for the moment, this is just a
     * placeholder. I claim justification in the fact that OpenSSH
     * does this too :-)
     */
    char *name;
};

struct ssh_signkey {
    void *(*newkey) (char *data, int len);
    void (*freekey) (void *key);
    char *(*fmtkey) (void *key);
    unsigned char *(*public_blob) (void *key, int *len);
    unsigned char *(*private_blob) (void *key, int *len);
    void *(*createkey) (unsigned char *pub_blob, int pub_len,
			unsigned char *priv_blob, int priv_len);
    void *(*openssh_createkey) (unsigned char **blob, int *len);
    int (*openssh_fmtkey) (void *key, unsigned char *blob, int len);
    char *(*fingerprint) (void *key);
    int (*verifysig) (void *key, char *sig, int siglen,
		      char *data, int datalen);
    unsigned char *(*sign) (void *key, char *data, int datalen,
			    int *siglen);
    char *name;
    char *keytype;		       /* for host key cache */
};

struct ssh_compress {
    char *name;
    void (*compress_init) (CSshSession &session);
    int (*compress) (unsigned char *block, int len,
		     unsigned char **outblock, int *outlen);
    void (*decompress_init) (CSshSession &session);
    int (*decompress) (unsigned char *block, int len,
		       unsigned char **outblock, int *outlen);
    int (*disable_compression) (void);
};

struct ssh2_userkey {
    const struct ssh_signkey *alg;     /* the key algorithm */
    void *data;			       /* the key data */
    char *comment;		       /* the key comment */
};

//function tables
extern const struct ssh_cipher ssh_3des;
extern const struct ssh_cipher ssh_des;
extern const struct ssh_cipher ssh_blowfish_ssh1;
extern const struct ssh2_ciphers ssh2_3des;
extern const struct ssh2_ciphers ssh2_des;
extern const struct ssh2_ciphers ssh2_aes;
extern const struct ssh2_ciphers ssh2_blowfish;
extern const struct ssh_kex ssh_diffiehellman;
extern const struct ssh_kex ssh_diffiehellman_gex;
extern const struct ssh_signkey ssh_dss;
extern const struct ssh_signkey ssh_rsa;
extern const struct ssh_mac ssh_md5;
extern const struct ssh_mac ssh_sha1;
extern const struct ssh_mac ssh_sha1_buggy;

/*
 * PuTTY version number formatted as an SSH version string. 
 */
extern char sshver[];

/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which scp.c can reach
 * into the SSH code and find out which one it got.
 */
extern int ssh_fallback_cmd;

void logevent(CSshSession &session, char *);
void *new_sock_channel(CSshSession &session, Socket s); // allocates and register a new channel for port forwarding
void ssh_send_port_open(CSshSession &session, void *channel, char *hostname, int port, char *org);

void base64_encode_atom(unsigned char *data, int n, char *out);

/* ssh2_load_userkey can return this as an error */
extern struct ssh2_userkey ssh2_wrong_passphrase;
#define SSH2_WRONG_PASSPHRASE (&ssh2_wrong_passphrase)

int ssh2_userkey_encrypted(char *filename, char **comment);
struct ssh2_userkey *ssh2_load_userkey(char *filename, char *passphrase);
char *ssh2_userkey_loadpub(char *filename, char **algorithm,
			   int *pub_blob_len);
int ssh2_save_userkey(char *filename, struct ssh2_userkey *key,
		      char *passphrase);

enum {
    SSH_KEYTYPE_UNOPENABLE,
    SSH_KEYTYPE_UNKNOWN,
    SSH_KEYTYPE_SSH1, SSH_KEYTYPE_SSH2,
    SSH_KEYTYPE_OPENSSH, SSH_KEYTYPE_SSHCOM
};
int key_type(char *filename);
char *key_type_to_str(int type);

int import_possible(int type);
int import_target_type(int type);
int import_encrypted(char *filename, int type, char **comment);
int import_ssh1(char *filename, int type, struct RSAKey *key,char *passphrase);
struct ssh2_userkey *import_ssh2(char *filename, int type, char *passphrase);
int export_ssh1(char *filename, int type, struct RSAKey *key,char *passphrase);
int export_ssh2(char *filename, int type,
                struct ssh2_userkey *key, char *passphrase);

void des3_decrypt_pubkey(unsigned char *key, unsigned char *blk, int len);
void des3_encrypt_pubkey(unsigned char *key, unsigned char *blk, int len);
void des3_decrypt_pubkey_ossh(unsigned char *key, unsigned char *iv,
			      unsigned char *blk, int len);
void des3_encrypt_pubkey_ossh(unsigned char *key, unsigned char *iv,
			      unsigned char *blk, int len);
void aes256_encrypt_pubkey(unsigned char *key, unsigned char *blk,
			   int len);
void aes256_decrypt_pubkey(unsigned char *key, unsigned char *blk,
			   int len);

/*
 * For progress updates in the key generation utility.
 */
#define PROGFN_INITIALISE 1
#define PROGFN_LIN_PHASE 2
#define PROGFN_EXP_PHASE 3
#define PROGFN_PHASE_EXTENT 4
#define PROGFN_READY 5
#define PROGFN_PROGRESS 6
typedef void (*progfn_t) (void *param, int action, int phase, int progress);

int rsa_generate(struct RSAKey *key, int bits, progfn_t pfn,
		 void *pfnparam);
int dsa_generate(struct dss_key *key, int bits, progfn_t pfn,
		 void *pfnparam);
Bignum primegen(int bits, int modulus, int residue, Bignum factor,
		int phase, progfn_t pfn, void *pfnparam);



/*
 * SSH1 agent messages.
 */
#define SSH1_AGENTC_REQUEST_RSA_IDENTITIES    1
#define SSH1_AGENT_RSA_IDENTITIES_ANSWER      2
#define SSH1_AGENTC_RSA_CHALLENGE             3
#define SSH1_AGENT_RSA_RESPONSE               4
#define SSH1_AGENTC_ADD_RSA_IDENTITY          7
#define SSH1_AGENTC_REMOVE_RSA_IDENTITY       8
#define SSH1_AGENTC_REMOVE_ALL_RSA_IDENTITIES 9	/* openssh private? */

/*
 * Messages common to SSH1 and OpenSSH's SSH2.
 */
#define SSH_AGENT_FAILURE                    5
#define SSH_AGENT_SUCCESS                    6

/*
 * OpenSSH's SSH2 agent messages.
 */
#define SSH2_AGENTC_REQUEST_IDENTITIES          11
#define SSH2_AGENT_IDENTITIES_ANSWER            12
#define SSH2_AGENTC_SIGN_REQUEST                13
#define SSH2_AGENT_SIGN_RESPONSE                14
#define SSH2_AGENTC_ADD_IDENTITY                17
#define SSH2_AGENTC_REMOVE_IDENTITY             18
#define SSH2_AGENTC_REMOVE_ALL_IDENTITIES       19

/*
 * Need this to warn about support for the original SSH2 keyfile
 * format.
 */
void old_keyfile_warning(void);

#endif // _SSH_H_INCLUDED

