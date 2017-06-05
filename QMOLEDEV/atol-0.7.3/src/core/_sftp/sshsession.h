////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_SSHSESSION_H__992A7E2A_A4DD_4789_BE67_B2880FB0FAF0__INCLUDED_)
#define AFX_SSHSESSION_H__992A7E2A_A4DD_4789_BE67_B2880FB0FAF0__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifdef _WIN32
 #define _WINSOCKAPI_   // Prevent inclusion of winsock.h in windows.h
 #include <winsock2.h>
 #include <windows.h>
#endif

#include "ssh.h"
#include "network.h"
#include "tree234.h"
#include "backend.h"
#include "sshbn.h"

#include "sshaes.h"
#include "sshblowf.h"
#include "sshdes.h"
#include "sshsha.h"
#include "sshmd5.h"

#ifdef _WIN32
 #define INT64 __int64
#else
 #define INT64 long long
#endif
#ifndef SOCKET
 #define SOCKET int
#endif

#define  CIPHER_MAX			5		       /* no. ciphers (inc warn) */

//int ssh_state;
#define	SSH_STATE_PREPACKET		0
#define	SSH_STATE_BEFORE_SIZE	1
#define	SSH_STATE_INTERMED		2
#define	SSH_STATE_SESSION		3
#define	SSH_STATE_CLOSED		4

struct Packet {
    long length;
    int type;
    unsigned char *data;
    unsigned char *body;
    long savedpos;
    long maxlen;
};

struct rdpkt1_state_tag {
	long len, pad, biglen, to_read;
	unsigned long realcrc, gotcrc;
	unsigned char *p;
	int i;
	int chunk;
};

struct rdpkt2_state_tag {
	long len, pad, payload, packetlen, maclen;
	int i;
	int cipherblk;
	unsigned long incoming_sequence;
};

class CSshSession
{
public:
	CSshSession();
	virtual ~CSshSession();

	//ssh.c
	Socket s;
	unsigned char session_key[32];
	int ssh1_compressing;
	int ssh1_remote_protoflags;
	int ssh1_local_protoflags;
	int ssh_agentfwd_enabled;
	int ssh_X11_fwd_enabled;
	int ssh_remote_bugs;
	const struct ssh_cipher *cipher;
	const struct ssh2_cipher *cscipher;
	const struct ssh2_cipher *sccipher;
	const struct ssh_mac *csmac;
	const struct ssh_mac *scmac;
	const struct ssh_compress *cscomp;
	const struct ssh_compress *sccomp;
	const struct ssh_kex *kex;
	const struct ssh_signkey *hostkey;
	unsigned char ssh2_session_id[20];
	int (*ssh_get_line) (const char *prompt, char *str, int maxlen,
				 int is_pw);
	SHA_State exhash, exhashbase;
	char *savedhost;
	int savedport;
	int ssh_send_ok;
	int ssh_echoing, ssh_editing;

	tree234 *ssh_channels;		       /* indexed by local id */
	struct ssh_channel *mainchan;	   /* primary session channel */
	int ssh_exitcode;

	tree234 *ssh_rportfwds;
	tree234 *sktree;

	int ssh_state;

	int size_needed;
	int eof_needed;

	struct Packet pktin;
	struct Packet pktout;
	unsigned char *deferred_send_data;
	int deferred_len, deferred_size;

	/*
	 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
	 * that fails. This variable is the means by which scp.c can reach
	 * into the SSH code and find out which one it got.
	 */
	int ssh_fallback_cmd;

	int ssh_version;
	int ssh1_throttle_count;
	int ssh_overall_bufsize;
	int ssh_throttled_all;
	int ssh1_stdout_throttling;
	void (*ssh_protocol) (unsigned char *in, int inlen, int ispkt);
	int (*s_rdpkt) (CSshSession &session, unsigned char **data, int *datalen, int &crLine);

	struct rdpkt1_state_tag rdpkt1_state;
	struct rdpkt2_state_tag rdpkt2_state;

	int ssh_pkt_ctx;

	Backend *back;

	//, int crLine
	//replacing static data from crBegin macro (one for each function using it)
	int n_s_rdpkt;
	int n_do_ssh_init;
	int n_ssh_gotdata;
	int n_do_ssh1_login;
	int n_ssh_protocol;		//one for both protocols
	int n_do_ssh2_transport;
	int n_do_ssh2_authconn;

	//SFTP data
	SOCKET sftp_ssh_socket;
    unsigned long outgoing_sequence;
	char *pwd, *homedir;		//sftp client state

	//from function do_ssh2_transport
	Bignum p, g, e, f, K;
	int len, nbits, pbits, warn;
    char *str;
    int kex_init_value, kex_reply_value;
    const struct ssh_mac **maclist;
    int nmacs;
    const struct ssh2_cipher *cscipher_tobe;
    const struct ssh2_cipher *sccipher_tobe;
    const struct ssh_mac *csmac_tobe;
    const struct ssh_mac *scmac_tobe;
    const struct ssh_compress *cscomp_tobe;
    const struct ssh_compress *sccomp_tobe;
    char *hostkeydata, *sigdata; //, *keystr, *fingerprint;
    int hostkeylen, siglen;
    void *hkey;		       /* actual host key */
    unsigned char exchange_hash[20];
    unsigned char keyspace[40];
    int n_preferred_ciphers;
    const struct ssh2_ciphers *preferred_ciphers[CIPHER_MAX];
    const struct ssh_compress *preferred_comp;
    int cipherstr_started;
    int first_kex;

	//ciphers
	AESContext csctx, scctx;
	BlowfishContext ectx, dctx;
	DESContext cskeys[3], sckeys[3];

	//MAC's
	SHA_State sha1_cs_mac_s1, sha1_cs_mac_s2;
	SHA_State sha1_sc_mac_s1, sha1_sc_mac_s2;
	MD5Context md5_cs_mac_s1, md5_cs_mac_s2;
	MD5Context md5_sc_mac_s1, md5_sc_mac_s2;

	//download/upload progress
	int (* m_pfnProgress)(INT64, INT64, unsigned long);
	unsigned long m_data;

	//tracing support
	void (*m_dbgFn)(const char *, unsigned long);	//debugging callback 
	unsigned long m_dwDbgData;						//callback data

	//password callback
	int (*m_fnLogin)(unsigned long);	//callback 
	unsigned long m_fnLoginData;						//callback data
	char m_password[200];	//TOFIX temp

	int (*m_fnKeyWarning)(const char *, int, unsigned long);
	unsigned long m_fnKeyData;

	int (*m_fnError)(const char *, unsigned long);
	unsigned long m_fnErrData;
};

#endif // !defined(AFX_SSHSESSION_H__992A7E2A_A4DD_4789_BE67_B2880FB0FAF0__INCLUDED_)
