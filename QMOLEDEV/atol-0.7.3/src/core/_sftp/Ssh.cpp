////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include "SshSession.h"

#include "putty.h"
#include "tree234.h"
#include "ssh.h"
#include "sshsha.h"
#include "sshmd5.h"
#include "sshzlib.h"
#include "sshdh.h"
#include "sshrand.h"
#include "x11fwd.h"
#include "macros.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define logevent(sess, s) { logevent(sess, s); \
                      if ((flags & FLAG_STDERR) && (flags & FLAG_VERBOSE)) \
                      { fprintf(stderr, "%s\n", s); fflush(stderr); } }

/* logevent, only printf-formatted. */
void logeventf(CSshSession &session, char *fmt, ...)
{
    va_list ap;
    char stuff[200];

    va_start(ap, fmt);
    vsprintf(stuff, fmt, ap);
    va_end(ap);
    logevent(session, stuff);
}

#define bombout(msg) ( session.ssh_state = SSH_STATE_CLOSED, \
                          (session.s ? sk_close(session, session.s), session.s = NULL : 0), \
                          logeventf msg , connection_fatal msg )

#define SSH1_MSG_DISCONNECT                       1	/* 0x1 */
#define SSH1_SMSG_PUBLIC_KEY                      2	/* 0x2 */
#define SSH1_CMSG_SESSION_KEY                     3	/* 0x3 */
#define SSH1_CMSG_USER                            4	/* 0x4 */
#define SSH1_CMSG_AUTH_RSA                        6	/* 0x6 */
#define SSH1_SMSG_AUTH_RSA_CHALLENGE              7	/* 0x7 */
#define SSH1_CMSG_AUTH_RSA_RESPONSE               8	/* 0x8 */
#define SSH1_CMSG_AUTH_PASSWORD                   9	/* 0x9 */
#define SSH1_CMSG_REQUEST_PTY                     10	/* 0xa */
#define SSH1_CMSG_WINDOW_SIZE                     11	/* 0xb */
#define SSH1_CMSG_EXEC_SHELL                      12	/* 0xc */
#define SSH1_CMSG_EXEC_CMD                        13	/* 0xd */
#define SSH1_SMSG_SUCCESS                         14	/* 0xe */
#define SSH1_SMSG_FAILURE                         15	/* 0xf */
#define SSH1_CMSG_STDIN_DATA                      16	/* 0x10 */
#define SSH1_SMSG_STDOUT_DATA                     17	/* 0x11 */
#define SSH1_SMSG_STDERR_DATA                     18	/* 0x12 */
#define SSH1_CMSG_EOF                             19	/* 0x13 */
#define SSH1_SMSG_EXIT_STATUS                     20	/* 0x14 */
#define SSH1_MSG_CHANNEL_OPEN_CONFIRMATION        21	/* 0x15 */
#define SSH1_MSG_CHANNEL_OPEN_FAILURE             22	/* 0x16 */
#define SSH1_MSG_CHANNEL_DATA                     23	/* 0x17 */
#define SSH1_MSG_CHANNEL_CLOSE                    24	/* 0x18 */
#define SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION       25	/* 0x19 */
#define SSH1_SMSG_X11_OPEN                        27	/* 0x1b */
#define SSH1_CMSG_PORT_FORWARD_REQUEST            28	/* 0x1c */
#define SSH1_MSG_PORT_OPEN                        29	/* 0x1d */
#define SSH1_CMSG_AGENT_REQUEST_FORWARDING        30	/* 0x1e */
#define SSH1_SMSG_AGENT_OPEN                      31	/* 0x1f */
#define SSH1_MSG_IGNORE                           32	/* 0x20 */
#define SSH1_CMSG_EXIT_CONFIRMATION               33	/* 0x21 */
#define SSH1_CMSG_X11_REQUEST_FORWARDING          34	/* 0x22 */
#define SSH1_CMSG_AUTH_RHOSTS_RSA                 35	/* 0x23 */
#define SSH1_MSG_DEBUG                            36	/* 0x24 */
#define SSH1_CMSG_REQUEST_COMPRESSION             37	/* 0x25 */
#define SSH1_CMSG_AUTH_TIS                        39	/* 0x27 */
#define SSH1_SMSG_AUTH_TIS_CHALLENGE              40	/* 0x28 */
#define SSH1_CMSG_AUTH_TIS_RESPONSE               41	/* 0x29 */
#define SSH1_CMSG_AUTH_CCARD                      70	/* 0x46 */
#define SSH1_SMSG_AUTH_CCARD_CHALLENGE            71	/* 0x47 */
#define SSH1_CMSG_AUTH_CCARD_RESPONSE             72	/* 0x48 */

#define SSH1_AUTH_TIS                             5	/* 0x5 */
#define SSH1_AUTH_CCARD                           16	/* 0x10 */

#define SSH1_PROTOFLAG_SCREEN_NUMBER              1	/* 0x1 */
/* Mask for protoflags we will echo back to server if seen */
#define SSH1_PROTOFLAGS_SUPPORTED                 0	/* 0x1 */

#define SSH2_MSG_DISCONNECT                       1	/* 0x1 */
#define SSH2_MSG_IGNORE                           2	/* 0x2 */
#define SSH2_MSG_UNIMPLEMENTED                    3	/* 0x3 */
#define SSH2_MSG_DEBUG                            4	/* 0x4 */
#define SSH2_MSG_SERVICE_REQUEST                  5	/* 0x5 */
#define SSH2_MSG_SERVICE_ACCEPT                   6	/* 0x6 */
#define SSH2_MSG_KEXINIT                          20	/* 0x14 */
#define SSH2_MSG_NEWKEYS                          21	/* 0x15 */
#define SSH2_MSG_KEXDH_INIT                       30	/* 0x1e */
#define SSH2_MSG_KEXDH_REPLY                      31	/* 0x1f */
#define SSH2_MSG_KEX_DH_GEX_REQUEST               30	/* 0x1e */
#define SSH2_MSG_KEX_DH_GEX_GROUP                 31	/* 0x1f */
#define SSH2_MSG_KEX_DH_GEX_INIT                  32	/* 0x20 */
#define SSH2_MSG_KEX_DH_GEX_REPLY                 33	/* 0x21 */
#define SSH2_MSG_USERAUTH_REQUEST                 50	/* 0x32 */
#define SSH2_MSG_USERAUTH_FAILURE                 51	/* 0x33 */
#define SSH2_MSG_USERAUTH_SUCCESS                 52	/* 0x34 */
#define SSH2_MSG_USERAUTH_BANNER                  53	/* 0x35 */
#define SSH2_MSG_USERAUTH_PK_OK                   60	/* 0x3c */
#define SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ        60	/* 0x3c */
#define SSH2_MSG_USERAUTH_INFO_REQUEST            60	/* 0x3c */
#define SSH2_MSG_USERAUTH_INFO_RESPONSE           61	/* 0x3d */
#define SSH2_MSG_GLOBAL_REQUEST                   80	/* 0x50 */
#define SSH2_MSG_REQUEST_SUCCESS                  81	/* 0x51 */
#define SSH2_MSG_REQUEST_FAILURE                  82	/* 0x52 */
#define SSH2_MSG_CHANNEL_OPEN                     90	/* 0x5a */
#define SSH2_MSG_CHANNEL_OPEN_CONFIRMATION        91	/* 0x5b */
#define SSH2_MSG_CHANNEL_OPEN_FAILURE             92	/* 0x5c */
#define SSH2_MSG_CHANNEL_WINDOW_ADJUST            93	/* 0x5d */
#define SSH2_MSG_CHANNEL_DATA                     94	/* 0x5e */
#define SSH2_MSG_CHANNEL_EXTENDED_DATA            95	/* 0x5f */
#define SSH2_MSG_CHANNEL_EOF                      96	/* 0x60 */
#define SSH2_MSG_CHANNEL_CLOSE                    97	/* 0x61 */
#define SSH2_MSG_CHANNEL_REQUEST                  98	/* 0x62 */
#define SSH2_MSG_CHANNEL_SUCCESS                  99	/* 0x63 */
#define SSH2_MSG_CHANNEL_FAILURE                  100	/* 0x64 */

/*
 * Packet type contexts, so that ssh2_pkt_type can correctly decode
 * the ambiguous type numbers back into the correct type strings.
 */
#define SSH2_PKTCTX_DHGROUP1         0x0001
#define SSH2_PKTCTX_DHGEX            0x0002
#define SSH2_PKTCTX_PUBLICKEY        0x0010
#define SSH2_PKTCTX_PASSWORD         0x0020
#define SSH2_PKTCTX_KBDINTER         0x0040
#define SSH2_PKTCTX_AUTH_MASK        0x00F0

#define SSH2_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT 1	/* 0x1 */
#define SSH2_DISCONNECT_PROTOCOL_ERROR            2	/* 0x2 */
#define SSH2_DISCONNECT_KEY_EXCHANGE_FAILED       3	/* 0x3 */
#define SSH2_DISCONNECT_HOST_AUTHENTICATION_FAILED 4	/* 0x4 */
#define SSH2_DISCONNECT_MAC_ERROR                 5	/* 0x5 */
#define SSH2_DISCONNECT_COMPRESSION_ERROR         6	/* 0x6 */
#define SSH2_DISCONNECT_SERVICE_NOT_AVAILABLE     7	/* 0x7 */
#define SSH2_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED 8	/* 0x8 */
#define SSH2_DISCONNECT_HOST_KEY_NOT_VERIFIABLE   9	/* 0x9 */
#define SSH2_DISCONNECT_CONNECTION_LOST           10	/* 0xa */
#define SSH2_DISCONNECT_BY_APPLICATION            11	/* 0xb */
#define SSH2_DISCONNECT_TOO_MANY_CONNECTIONS      12	/* 0xc */
#define SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER    13	/* 0xd */
#define SSH2_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE 14	/* 0xe */
#define SSH2_DISCONNECT_ILLEGAL_USER_NAME         15	/* 0xf */

static const char *const ssh2_disconnect_reasons[] = {
    NULL,
    "SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT",
    "SSH_DISCONNECT_PROTOCOL_ERROR",
    "SSH_DISCONNECT_KEY_EXCHANGE_FAILED",
    "SSH_DISCONNECT_HOST_AUTHENTICATION_FAILED",
    "SSH_DISCONNECT_MAC_ERROR",
    "SSH_DISCONNECT_COMPRESSION_ERROR",
    "SSH_DISCONNECT_SERVICE_NOT_AVAILABLE",
    "SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED",
    "SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE",
    "SSH_DISCONNECT_CONNECTION_LOST",
    "SSH_DISCONNECT_BY_APPLICATION",
    "SSH_DISCONNECT_TOO_MANY_CONNECTIONS",
    "SSH_DISCONNECT_AUTH_CANCELLED_BY_USER",
    "SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE",
    "SSH_DISCONNECT_ILLEGAL_USER_NAME",
};

#define SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED     1	/* 0x1 */
#define SSH2_OPEN_CONNECT_FAILED                  2	/* 0x2 */
#define SSH2_OPEN_UNKNOWN_CHANNEL_TYPE            3	/* 0x3 */
#define SSH2_OPEN_RESOURCE_SHORTAGE               4	/* 0x4 */

#define SSH2_EXTENDED_DATA_STDERR                 1	/* 0x1 */

/*
 * Various remote-bug flags.
 */
#define BUG_CHOKES_ON_SSH1_IGNORE                 1
#define BUG_SSH2_HMAC                             2
#define BUG_NEEDS_SSH1_PLAIN_PASSWORD        	  4
#define BUG_CHOKES_ON_RSA	        	  8
#define BUG_SSH2_RSA_PADDING	        	 16

static int ssh_pkt_ctx = 0;

#define translate(x) if (type == x) return #x
#define translatec(x,ctx) if (type == x && (ssh_pkt_ctx & ctx)) return #x
char *ssh1_pkt_type(int type)
{
    translate(SSH1_MSG_DISCONNECT);
    translate(SSH1_SMSG_PUBLIC_KEY);
    translate(SSH1_CMSG_SESSION_KEY);
    translate(SSH1_CMSG_USER);
    translate(SSH1_CMSG_AUTH_RSA);
    translate(SSH1_SMSG_AUTH_RSA_CHALLENGE);
    translate(SSH1_CMSG_AUTH_RSA_RESPONSE);
    translate(SSH1_CMSG_AUTH_PASSWORD);
    translate(SSH1_CMSG_REQUEST_PTY);
    translate(SSH1_CMSG_WINDOW_SIZE);
    translate(SSH1_CMSG_EXEC_SHELL);
    translate(SSH1_CMSG_EXEC_CMD);
    translate(SSH1_SMSG_SUCCESS);
    translate(SSH1_SMSG_FAILURE);
    translate(SSH1_CMSG_STDIN_DATA);
    translate(SSH1_SMSG_STDOUT_DATA);
    translate(SSH1_SMSG_STDERR_DATA);
    translate(SSH1_CMSG_EOF);
    translate(SSH1_SMSG_EXIT_STATUS);
    translate(SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
    translate(SSH1_MSG_CHANNEL_OPEN_FAILURE);
    translate(SSH1_MSG_CHANNEL_DATA);
    translate(SSH1_MSG_CHANNEL_CLOSE);
    translate(SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION);
    translate(SSH1_SMSG_X11_OPEN);
    translate(SSH1_CMSG_PORT_FORWARD_REQUEST);
    translate(SSH1_MSG_PORT_OPEN);
    translate(SSH1_CMSG_AGENT_REQUEST_FORWARDING);
    translate(SSH1_SMSG_AGENT_OPEN);
    translate(SSH1_MSG_IGNORE);
    translate(SSH1_CMSG_EXIT_CONFIRMATION);
    translate(SSH1_CMSG_X11_REQUEST_FORWARDING);
    translate(SSH1_CMSG_AUTH_RHOSTS_RSA);
    translate(SSH1_MSG_DEBUG);
    translate(SSH1_CMSG_REQUEST_COMPRESSION);
    translate(SSH1_CMSG_AUTH_TIS);
    translate(SSH1_SMSG_AUTH_TIS_CHALLENGE);
    translate(SSH1_CMSG_AUTH_TIS_RESPONSE);
    translate(SSH1_CMSG_AUTH_CCARD);
    translate(SSH1_SMSG_AUTH_CCARD_CHALLENGE);
    translate(SSH1_CMSG_AUTH_CCARD_RESPONSE);
    return "unknown";
}
char *ssh2_pkt_type(int type)
{
    translate(SSH2_MSG_DISCONNECT);
    translate(SSH2_MSG_IGNORE);
    translate(SSH2_MSG_UNIMPLEMENTED);
    translate(SSH2_MSG_DEBUG);
    translate(SSH2_MSG_SERVICE_REQUEST);
    translate(SSH2_MSG_SERVICE_ACCEPT);
    translate(SSH2_MSG_KEXINIT);
    translate(SSH2_MSG_NEWKEYS);
    translatec(SSH2_MSG_KEXDH_INIT, SSH2_PKTCTX_DHGROUP1);
    translatec(SSH2_MSG_KEXDH_REPLY, SSH2_PKTCTX_DHGROUP1);
    translatec(SSH2_MSG_KEX_DH_GEX_REQUEST, SSH2_PKTCTX_DHGEX);
    translatec(SSH2_MSG_KEX_DH_GEX_GROUP, SSH2_PKTCTX_DHGEX);
    translatec(SSH2_MSG_KEX_DH_GEX_INIT, SSH2_PKTCTX_DHGEX);
    translatec(SSH2_MSG_KEX_DH_GEX_REPLY, SSH2_PKTCTX_DHGEX);
    translate(SSH2_MSG_USERAUTH_REQUEST);
    translate(SSH2_MSG_USERAUTH_FAILURE);
    translate(SSH2_MSG_USERAUTH_SUCCESS);
    translate(SSH2_MSG_USERAUTH_BANNER);
    translatec(SSH2_MSG_USERAUTH_PK_OK, SSH2_PKTCTX_PUBLICKEY);
    translatec(SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ, SSH2_PKTCTX_PASSWORD);
    translatec(SSH2_MSG_USERAUTH_INFO_REQUEST, SSH2_PKTCTX_KBDINTER);
    translatec(SSH2_MSG_USERAUTH_INFO_RESPONSE, SSH2_PKTCTX_KBDINTER);
    translate(SSH2_MSG_GLOBAL_REQUEST);
    translate(SSH2_MSG_REQUEST_SUCCESS);
    translate(SSH2_MSG_REQUEST_FAILURE);
    translate(SSH2_MSG_CHANNEL_OPEN);
    translate(SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
    translate(SSH2_MSG_CHANNEL_OPEN_FAILURE);
    translate(SSH2_MSG_CHANNEL_WINDOW_ADJUST);
    translate(SSH2_MSG_CHANNEL_DATA);
    translate(SSH2_MSG_CHANNEL_EXTENDED_DATA);
    translate(SSH2_MSG_CHANNEL_EOF);
    translate(SSH2_MSG_CHANNEL_CLOSE);
    translate(SSH2_MSG_CHANNEL_REQUEST);
    translate(SSH2_MSG_CHANNEL_SUCCESS);
    translate(SSH2_MSG_CHANNEL_FAILURE);
    return "unknown";
}
#undef translate
#undef translatec

enum { PKT_END, PKT_INT, PKT_CHAR, PKT_DATA, PKT_STR, PKT_BIGNUM };

/*
 * Coroutine mechanics for the sillier bits of the code. If these
 * macros look impenetrable to you, you might find it helpful to
 * read
 * 
 *   http://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
 * 
 * which explains the theory behind these macros.
 * 
 * In particular, if you are getting `case expression not constant'
 * errors when building with MS Visual Studio, this is because MS's
 * Edit and Continue debugging feature causes their compiler to
 * violate ANSI C. To disable Edit and Continue debugging:
 * 
 *  - right-click ssh.c in the FileView
 *  - click Settings
 *  - select the C/C++ tab and the General category
 *  - under `Debug info:', select anything _other_ than `Program
 *    Database for Edit and Continue'.
 */
#define crBegin1	//static int crLine = 0;
#define crBegin2	switch(crLine) { case 0:;
#define crBegin		crBegin1; crBegin2;
#define crFinish(z)	} crLine = 0; return (z)
#define crFinishV	} crLine = 0; return
#define crReturn(z)	\
	do {\
		crLine=__LINE__; return (z); case __LINE__:;\
	} while (0)
#define crReturnV \
	do {\
	    crLine=__LINE__; return; case __LINE__:;\
	} while (0)
#define crStop(z)	do{ crLine = 0; return (z); }while(0)
#define crStopV		do{ crLine = 0; return; }while(0)
#define crWaitUntil(c)	do { crReturn(0); } while (!(c))
#define crWaitUntilV(c)	do { crReturnV; } while (!(c))

char *pfd_newconnect(CSshSession &session, Socket * s, char *hostname, int port, void *c);
char *pfd_addforward(CSshSession &session, char *desthost, int destport, int port);
void pfd_close(CSshSession &session, Socket s);
int  pfd_send(CSshSession &session, Socket s, char *data, int len);
void pfd_confirm(CSshSession &session, Socket s);
void pfd_unthrottle(CSshSession &session, Socket s);
void pfd_override_throttle(CSshSession &session, Socket s, int enable);

static void ssh2_pkt_init(CSshSession &session, int pkt_type);
static void ssh2_pkt_addbool(CSshSession &session, unsigned char value);
static void ssh2_pkt_adduint32(CSshSession &session, unsigned long value);
static void ssh2_pkt_addstring_start(CSshSession &session);
static void ssh2_pkt_addstring_str(CSshSession &session, char *data);
static void ssh2_pkt_addstring_data(CSshSession &session, char *data, int len);
static void ssh2_pkt_addstring(CSshSession &session, char *data);
static char *ssh2_mpint_fmt(Bignum b, int *len);
static void ssh2_pkt_addmp(CSshSession &session, Bignum b);
static int ssh2_pkt_construct(CSshSession &session);
static void ssh2_pkt_send(CSshSession &session);

/*
 * Buffer management constants. There are several of these for
 * various different purposes:
 * 
 *  - SSH1_BUFFER_LIMIT is the amount of backlog that must build up
 *    on a local data stream before we throttle the whole SSH
 *    connection (in SSH1 only). Throttling the whole connection is
 *    pretty drastic so we set this high in the hope it won't
 *    happen very often.
 * 
 *  - SSH_MAX_BACKLOG is the amount of backlog that must build up
 *    on the SSH connection itself before we defensively throttle
 *    _all_ local data streams. This is pretty drastic too (though
 *    thankfully unlikely in SSH2 since the window mechanism should
 *    ensure that the server never has any need to throttle its end
 *    of the connection), so we set this high as well.
 * 
 *  - OUR_V2_WINSIZE is the maximum window size we present on SSH2
 *    channels.
 */

#define SSH1_BUFFER_LIMIT 32768
#define SSH_MAX_BACKLOG 32768
#define OUR_V2_WINSIZE 16384

const static struct ssh_kex *kex_algs[] = {
    &ssh_diffiehellman_gex,
    &ssh_diffiehellman
};

const static struct ssh_signkey *hostkey_algs[] = { &ssh_rsa, &ssh_dss };

static void nullmac_key(CSshSession &session, unsigned char *key)
{
}
static void nullmac_generate(CSshSession &session, unsigned char *blk, int len,
			     unsigned long seq)
{
}
static int nullmac_verify(CSshSession &session, unsigned char *blk, int len, unsigned long seq)
{
    return 1;
}
const static struct ssh_mac ssh_mac_none = {
    nullmac_key, nullmac_key, nullmac_generate, nullmac_verify, "none", 0
};
const static struct ssh_mac *macs[] = {
    &ssh_sha1, &ssh_md5, &ssh_mac_none
};
const static struct ssh_mac *buggymacs[] = {
    &ssh_sha1_buggy, &ssh_md5, &ssh_mac_none
};

static void ssh_comp_none_init(CSshSession &session)
{
}
static int ssh_comp_none_block(unsigned char *block, int len,
			       unsigned char **outblock, int *outlen)
{
    return 0;
}
static int ssh_comp_none_disable()
{
    return 0;
}

static void ssh_comp_none1(CSshSession &session)
{    
}

const static struct ssh_compress ssh_comp_none = {
    "none",
    ssh_comp_none_init,
	ssh_comp_none_block,
    ssh_comp_none1,
	ssh_comp_none_block,
    ssh_comp_none_disable
};
extern /*const*/ struct ssh_compress ssh_zlib;
const static struct ssh_compress *compressions[] = {
    &ssh_zlib, &ssh_comp_none
};

enum {				       /* channel types */
    CHAN_MAINSESSION,
    CHAN_X11,
    CHAN_AGENT,
    CHAN_SOCKDATA,
    CHAN_SOCKDATA_DORMANT	       /* one the remote hasn't confirmed */
};

/*
 * 2-3-4 tree storing channels.
 */
struct ssh_channel {
    unsigned remoteid, localid;
    int type;
    int closes;
    union {
	struct ssh1_data_channel {
	    int throttling;
	} v1;
	struct ssh2_data_channel {
	    bufchain outbuffer;
	    unsigned remwindow, remmaxpkt;
	    unsigned locwindow;
	} v2;
    } v;
    union {
	struct ssh_agent_channel {
	    unsigned char *message;
	    unsigned char msglen[4];
	    int lensofar, totallen;
	} a;
	struct ssh_x11_channel {
	    Socket s;
	} x11;
	struct ssh_pfd_channel {
	    Socket s;
	} pfd;
    } u;
};

/*
 * 2-3-4 tree storing remote->local port forwardings. SSH 1 and SSH
 * 2 use this structure in different ways, reflecting SSH 2's
 * altogether saner approach to port forwarding.
 * 
 * In SSH 1, you arrange a remote forwarding by sending the server
 * the remote port number, and the local destination host:port.
 * When a connection comes in, the server sends you back that
 * host:port pair, and you connect to it. This is a ready-made
 * security hole if you're not on the ball: a malicious server
 * could send you back _any_ host:port pair, so if you trustingly
 * connect to the address it gives you then you've just opened the
 * entire inside of your corporate network just by connecting
 * through it to a dodgy SSH server. Hence, we must store a list of
 * host:port pairs we _are_ trying to forward to, and reject a
 * connection request from the server if it's not in the list.
 * 
 * In SSH 2, each side of the connection minds its own business and
 * doesn't send unnecessary information to the other. You arrange a
 * remote forwarding by sending the server just the remote port
 * number. When a connection comes in, the server tells you which
 * of its ports was connected to; and _you_ have to remember what
 * local host:port pair went with that port number.
 * 
 * Hence: in SSH 1 this structure stores host:port pairs we intend
 * to allow connections to, and is indexed by those host:port
 * pairs. In SSH 2 it stores a mapping from source port to
 * destination host:port pair, and is indexed by source port.
 */
struct ssh_rportfwd {
    unsigned sport, dport;
    char dhost[256];
};


/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which scp.c can reach
 * into the SSH code and find out which one it got.
 */
int ssh_fallback_cmd = 0;

static int ssh_version;
static int ssh1_throttle_count;
static int ssh_overall_bufsize;
static int ssh_throttled_all;
static int ssh1_stdout_throttling;
static void (*ssh_protocol) (CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine);
static void ssh1_protocol(CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine);
static void ssh2_protocol(CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine);
static void ssh_size(CSshSession &session);
static void ssh_special(CSshSession &session, Telnet_Special);
static int ssh2_try_send(CSshSession &session, struct ssh_channel *c);
static void ssh2_add_channel_data(struct ssh_channel *c, char *buf,
				  int len);
static void ssh_throttle_all(CSshSession &session, int enable, int bufsize);
static void ssh2_set_window(CSshSession &session, struct ssh_channel *c, unsigned newwin);
static int ssh_sendbuffer(CSshSession &session);

static int ssh_channelcmp(void *av, void *bv)
{
    struct ssh_channel *a = (struct ssh_channel *) av;
    struct ssh_channel *b = (struct ssh_channel *) bv;
    if (a->localid < b->localid)
	return -1;
    if (a->localid > b->localid)
	return +1;
    return 0;
}
static int ssh_channelfind(void *av, void *bv)
{
    unsigned *a = (unsigned *) av;
    struct ssh_channel *b = (struct ssh_channel *) bv;
    if (*a < b->localid)
	return -1;
    if (*a > b->localid)
	return +1;
    return 0;
}

static int ssh_rportcmp_ssh1(void *av, void *bv)
{
    struct ssh_rportfwd *a = (struct ssh_rportfwd *) av;
    struct ssh_rportfwd *b = (struct ssh_rportfwd *) bv;
    int i;
    if ( (i = strcmp(a->dhost, b->dhost)) != 0)
	return i < 0 ? -1 : +1;
    if (a->dport > b->dport)
	return +1;
    if (a->dport < b->dport)
	return -1;
    return 0;
}

static int ssh_rportcmp_ssh2(void *av, void *bv)
{
    struct ssh_rportfwd *a = (struct ssh_rportfwd *) av;
    struct ssh_rportfwd *b = (struct ssh_rportfwd *) bv;

    if (a->sport > b->sport)
	return +1;
    if (a->sport < b->sport)
	return -1;
    return 0;
}

static int alloc_channel_id(CSshSession &session)
{
    const unsigned CHANNEL_NUMBER_OFFSET = 256;
    unsigned low, high, mid;
    int tsize;
    struct ssh_channel *c;

    /*
     * First-fit allocation of channel numbers: always pick the
     * lowest unused one. To do this, binary-search using the
     * counted B-tree to find the largest channel ID which is in a
     * contiguous sequence from the beginning. (Precisely
     * everything in that sequence must have ID equal to its tree
     * index plus CHANNEL_NUMBER_OFFSET.)
     */
    tsize = count234(session.ssh_channels);

    low = -1;
    high = tsize;
    while (high - low > 1) {
	mid = (high + low) / 2;
	c = (struct ssh_channel *)index234(session.ssh_channels, mid);
	if (c->localid == mid + CHANNEL_NUMBER_OFFSET)
	    low = mid;		       /* this one is fine */
	else
	    high = mid;		       /* this one is past it */
    }
    /*
     * Now low points to either -1, or the tree index of the
     * largest ID in the initial sequence.
     */
    {
	unsigned i = low + 1 + CHANNEL_NUMBER_OFFSET;
	assert(NULL == find234(session.ssh_channels, &i, ssh_channelfind));
    }
    return low + 1 + CHANNEL_NUMBER_OFFSET;
}

static void c_write(char *buf, int len)
{
    if ((flags & FLAG_STDERR)) {
		int i;
		for (i = 0; i < len; i++)
			if (buf[i] != '\r')
				fputc(buf[i], stderr);
		return;
    }
    from_backend(1, buf, len);
}

static void c_write_untrusted(char *buf, int len)
{
    int i;
    for (i = 0; i < len; i++) {
	if (buf[i] == '\n')
	    c_write("\r\n", 2);
	else if ((buf[i] & 0x60) || (buf[i] == '\r'))
	    c_write(buf + i, 1);
    }
}

static void c_write_str(char *buf)
{
    c_write(buf, strlen(buf));
}

/*
 * Collect incoming data in the incoming packet buffer.
 * Decipher and verify the packet when it is completely read.
 * Drop SSH1_MSG_DEBUG and SSH1_MSG_IGNORE packets.
 * Update the *data and *datalen variables.
 * Return the additional nr of bytes needed, or 0 when
 * a complete packet is available.
 */
static int ssh1_rdpkt(CSshSession &session, unsigned char **data, int *datalen, int &crLine)
{
    struct rdpkt1_state_tag *st = &(session.rdpkt1_state);

    crBegin;

  next_packet:

    session.pktin.type = 0;
    session.pktin.length = 0;

    for (st->i = st->len = 0; st->i < 4; st->i++) {
		while ((*datalen) == 0)
			crReturn(4 - st->i);
		st->len = (st->len << 8) + **data;
		(*data)++, (*datalen)--;
    }

    st->pad = 8 - (st->len % 8);
    st->biglen = st->len + st->pad;
    session.pktin.length = st->len - 5;

    if (session.pktin.maxlen < st->biglen) {
		session.pktin.maxlen = st->biglen;
		session.pktin.data = (session.pktin.data == NULL ? (unsigned char *)smalloc(st->biglen + APIEXTRA) :
				  (unsigned char *)srealloc(session.pktin.data, st->biglen + APIEXTRA));
		if (!session.pktin.data)
			fatalbox(session, "Out of memory");
    }

    st->to_read = st->biglen;
    st->p = session.pktin.data;
    while (st->to_read > 0) {
		st->chunk = st->to_read;
		while ((*datalen) == 0)
			crReturn(st->to_read);
		if (st->chunk > (*datalen))
			st->chunk = (*datalen);
		memcpy(st->p, *data, st->chunk);
		*data += st->chunk;
		*datalen -= st->chunk;
		st->p += st->chunk;
		st->to_read -= st->chunk;
    }

    if (session.cipher && detect_attack(session, session.pktin.data, st->biglen, NULL)) {
        bombout((session, "Network attack (CRC compensation) detected!"));
        crReturn(0);
    }

    if (session.cipher)
		session.cipher->decrypt(session, session.pktin.data, st->biglen);

    st->realcrc = crc32(session.pktin.data, st->biglen - 4);
    st->gotcrc = GET_32BIT(session.pktin.data + st->biglen - 4);
    if (st->gotcrc != st->realcrc) {
		bombout((session, "Incorrect CRC received on packet"));
		crReturn(0);
    }

    session.pktin.body = session.pktin.data + st->pad + 1;

    if (session.ssh1_compressing) {
		unsigned char *decompblk;
		int decomplen;
		zlib_decompress_block(session.pktin.body - 1, session.pktin.length + 1,
					  &decompblk, &decomplen);

		if (session.pktin.maxlen < st->pad + decomplen) {
			session.pktin.maxlen = st->pad + decomplen;
			session.pktin.data = (unsigned char *)srealloc(session.pktin.data, session.pktin.maxlen + APIEXTRA);
			session.pktin.body = session.pktin.data + st->pad + 1;
			if (!session.pktin.data)
				fatalbox(session, "Out of memory");
		}

		memcpy(session.pktin.body - 1, decompblk, decomplen);
		sfree(decompblk);
		session.pktin.length = decomplen - 1;
    }

    session.pktin.type = session.pktin.body[-1];

    log_packet(PKT_INCOMING, session.pktin.type, ssh1_pkt_type(session.pktin.type),
	       session.pktin.body, session.pktin.length);

    if (session.pktin.type == SSH1_SMSG_STDOUT_DATA ||
		session.pktin.type == SSH1_SMSG_STDERR_DATA ||
		session.pktin.type == SSH1_MSG_DEBUG ||
		session.pktin.type == SSH1_SMSG_AUTH_TIS_CHALLENGE ||
		session.pktin.type == SSH1_SMSG_AUTH_CCARD_CHALLENGE) 
	{
			long stringlen; 
			stringlen = GET_32BIT(session.pktin.body);
			if (stringlen + 4 != session.pktin.length) {
				bombout((session, "Received data packet with bogus string length"));
				crReturn(0);
			}
	}

    if (session.pktin.type == SSH1_MSG_DEBUG) {
		/* log debug message */
		char buf[80];
		int stringlen = GET_32BIT(session.pktin.body);
		strcpy(buf, "Remote: ");
		if (stringlen > 70)
			stringlen = 70;
		memcpy(buf + 8, session.pktin.body + 4, stringlen);
		buf[8 + stringlen] = '\0';
		logevent(session, buf);
		goto next_packet;
    } else if (session.pktin.type == SSH1_MSG_IGNORE) {
		/* do nothing */
		goto next_packet;
    }

    if (session.pktin.type == SSH1_MSG_DISCONNECT) {
		/* log reason code in disconnect message */
		char buf[256];
		unsigned msglen;
		unsigned nowlen;
		msglen = GET_32BIT(session.pktin.body);
		strcpy(buf, "Remote sent disconnect: ");
		nowlen = strlen(buf);
		if (msglen > sizeof(buf) - nowlen - 1)
			msglen = sizeof(buf) - nowlen - 1;
		memcpy(buf + nowlen, session.pktin.body + 4, msglen);
		buf[nowlen + msglen] = '\0';
		/* logevent(buf); (this is now done within the bombout macro) */
		bombout((session, "Server sent disconnect message:\n\"%s\"", buf+nowlen));
		crReturn(0);
    }

    crFinish(0);
}

static int ssh2_rdpkt(CSshSession &session, unsigned char **data, int *datalen, int &crLine)
{
    struct rdpkt2_state_tag *st = &(session.rdpkt2_state);

    crBegin;

  next_packet:
    session.pktin.type = 0;
    session.pktin.length = 0;
    if (session.sccipher)
		st->cipherblk = session.sccipher->blksize;
    else
		st->cipherblk = 8;
    if (st->cipherblk < 8)
		st->cipherblk = 8;

    if (session.pktin.maxlen < st->cipherblk) {
		session.pktin.maxlen = st->cipherblk;
		session.pktin.data =
			(session.pktin.data ==
			 NULL ? (unsigned char *)smalloc(st->cipherblk +
					APIEXTRA) : (unsigned char *)srealloc(session.pktin.data,
							 st->cipherblk +
							 APIEXTRA));
		if (!session.pktin.data)
			fatalbox(session, "Out of memory");
    }

    /*
     * Acquire and decrypt the first block of the packet. This will
     * contain the length and padding details.
     */
    for (st->i = st->len = 0; st->i < st->cipherblk; st->i++) {
		while ((*datalen) == 0)
			crReturn(st->cipherblk - st->i);
		session.pktin.data[st->i] = *(*data)++;
		(*datalen)--;
    }

    if (session.sccipher)
		session.sccipher->decrypt(session, session.pktin.data, st->cipherblk);

    /*
     * Now get the length and padding figures.
     */
    st->len = GET_32BIT(session.pktin.data);
    st->pad = session.pktin.data[4];

    /*
     * This enables us to deduce the payload length.
     */
    st->payload = st->len - st->pad - 1;

    session.pktin.length = st->payload + 5;

    /*
     * So now we can work out the total packet length.
     */
    st->packetlen = st->len + 4;
    st->maclen = session.scmac ? session.scmac->len : 0;

    /*
     * Adjust memory allocation if packet is too big.
     */
    if (session.pktin.maxlen < st->packetlen + st->maclen) {
		session.pktin.maxlen = st->packetlen + st->maclen;
		session.pktin.data =
			(session.pktin.data ==
			 NULL ? (unsigned char *)smalloc(session.pktin.maxlen + APIEXTRA) : 
					(unsigned char *)srealloc(session.pktin.data, session.pktin.maxlen+APIEXTRA));
		if (!session.pktin.data)
			fatalbox(session, "Out of memory");
    }

    /*
     * Read and decrypt the remainder of the packet.
     */
    for (st->i = st->cipherblk; st->i < st->packetlen + st->maclen;
	 st->i++) {
		while ((*datalen) == 0)
			crReturn(st->packetlen + st->maclen - st->i);
		session.pktin.data[st->i] = *(*data)++;
		(*datalen)--;
    }
    /* Decrypt everything _except_ the MAC. */
    if (session.sccipher)
		session.sccipher->decrypt(session, session.pktin.data + st->cipherblk,
			  st->packetlen - st->cipherblk);

    /*
     * Check the MAC.
     */
    if (session.scmac
	&& !session.scmac->verify(session, session.pktin.data, st->len + 4,
			  st->incoming_sequence)) {
		bombout((session, "Incorrect MAC received on packet"));
		crReturn(0);
    }
    st->incoming_sequence++;	       /* whether or not we MACed */

    /*
     * Decompress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (session.sccomp && session.sccomp->decompress(session.pktin.data + 5, 
			session.pktin.length - 5, &newpayload, &newlen)) 
	{
	    if (session.pktin.maxlen < newlen + 5) {
			session.pktin.maxlen = newlen + 5;
			session.pktin.data =
				(session.pktin.data ==
				 NULL ? (unsigned char *)smalloc(session.pktin.maxlen +APIEXTRA) : 
						(unsigned char *)srealloc(session.pktin.data,session.pktin.maxlen + APIEXTRA));
			if (!session.pktin.data)
				fatalbox(session, "Out of memory");
			}
			session.pktin.length = 5 + newlen;
			memcpy(session.pktin.data + 5, newpayload, newlen);
			sfree(newpayload);
	}
    }

    session.pktin.savedpos = 6;
    session.pktin.type = session.pktin.data[5];

    log_packet(PKT_INCOMING, session.pktin.type, ssh2_pkt_type(session.pktin.type),
	       session.pktin.data+6, session.pktin.length-6);

    switch (session.pktin.type) {
        /*
         * These packets we must handle instantly.
         */
      case SSH2_MSG_DISCONNECT:
        {
            /* log reason code in disconnect message */
            char buf[256];
            int reason;
            unsigned msglen;
            unsigned nowlen;

			reason = GET_32BIT(session.pktin.data + 6);
			msglen = GET_32BIT(session.pktin.data + 10);
            if (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) {
                sprintf(buf, "Received disconnect message (%s)",
                        ssh2_disconnect_reasons[reason]);
            } else {
                sprintf(buf, "Received disconnect message (unknown type %d)",
                        reason);
            }
            logevent(session, buf);
            strcpy(buf, "Disconnection message text: ");
            nowlen = strlen(buf);
            if (msglen > sizeof(buf) - nowlen - 1)
                msglen = sizeof(buf) - nowlen - 1;
            memcpy(buf + nowlen, session.pktin.data + 14, msglen);
            buf[nowlen + msglen] = '\0';
            logevent(session, buf);
            bombout((session, "Server sent disconnect message\ntype %d (%s):\n\"%s\"",
                     reason,
                     (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) ?
                     ssh2_disconnect_reasons[reason] : "unknown",
                     buf+nowlen));
            crReturn(0);
        }
        break;
      case SSH2_MSG_IGNORE:
	goto next_packet;
      case SSH2_MSG_DEBUG:
	{
	    /* log the debug message */
	    char buf[512];
	    /* int display = pktin.body[6]; */
	    uint32 stringlen = GET_32BIT(session.pktin.data+7);
	    int prefix;
	    strcpy(buf, "Remote debug message: ");
	    prefix = strlen(buf);
	    if (stringlen > sizeof(buf)-prefix-1)
			stringlen = sizeof(buf)-prefix-1;
	    memcpy(buf + prefix, session.pktin.data + 11, stringlen);
	    buf[prefix + stringlen] = '\0';
	    logevent(session, buf);
	}
        goto next_packet;              /* FIXME: print the debug message */

        /*
         * These packets we need do nothing about here.
         */
      case SSH2_MSG_UNIMPLEMENTED:
      case SSH2_MSG_SERVICE_REQUEST:
      case SSH2_MSG_SERVICE_ACCEPT:
      case SSH2_MSG_KEXINIT:
      case SSH2_MSG_NEWKEYS:
      case SSH2_MSG_KEXDH_INIT:
      case SSH2_MSG_KEXDH_REPLY:
      /* case SSH2_MSG_KEX_DH_GEX_REQUEST: duplicate case value */
      /* case SSH2_MSG_KEX_DH_GEX_GROUP: duplicate case value */
      case SSH2_MSG_KEX_DH_GEX_INIT:
      case SSH2_MSG_KEX_DH_GEX_REPLY:
      case SSH2_MSG_USERAUTH_REQUEST:
      case SSH2_MSG_USERAUTH_FAILURE:
      case SSH2_MSG_USERAUTH_SUCCESS:
      case SSH2_MSG_USERAUTH_BANNER:
      case SSH2_MSG_USERAUTH_PK_OK:
      /* case SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ: duplicate case value */
      /* case SSH2_MSG_USERAUTH_INFO_REQUEST: duplicate case value */
      case SSH2_MSG_USERAUTH_INFO_RESPONSE:
      case SSH2_MSG_GLOBAL_REQUEST:
      case SSH2_MSG_REQUEST_SUCCESS:
      case SSH2_MSG_REQUEST_FAILURE:
      case SSH2_MSG_CHANNEL_OPEN:
      case SSH2_MSG_CHANNEL_OPEN_CONFIRMATION:
      case SSH2_MSG_CHANNEL_OPEN_FAILURE:
      case SSH2_MSG_CHANNEL_WINDOW_ADJUST:
      case SSH2_MSG_CHANNEL_DATA:
      case SSH2_MSG_CHANNEL_EXTENDED_DATA:
      case SSH2_MSG_CHANNEL_EOF:
      case SSH2_MSG_CHANNEL_CLOSE:
      case SSH2_MSG_CHANNEL_REQUEST:
      case SSH2_MSG_CHANNEL_SUCCESS:
      case SSH2_MSG_CHANNEL_FAILURE:
        break;

        /*
         * For anything else we send SSH2_MSG_UNIMPLEMENTED.
         */
      default:
	ssh2_pkt_init(session, SSH2_MSG_UNIMPLEMENTED);
	ssh2_pkt_adduint32(session,st->incoming_sequence - 1);
	ssh2_pkt_send(session);
        break;
    }

    crFinish(0);
}

static void ssh1_pktout_size(CSshSession &session, int len)
{
    int pad, biglen;

    len += 5;			       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    session.pktout.length = len - 5;
    if (session.pktout.maxlen < biglen) {
	session.pktout.maxlen = biglen;
#ifdef MSCRYPTOAPI
	/* Allocate enough buffer space for extra block
	 * for MS CryptEncrypt() */
	session.pktout.data = (session.pktout.data == NULL ? smalloc(biglen + 12) :
		       srealloc(session.pktout.data, biglen + 12));
#else
	session.pktout.data = (session.pktout.data == NULL ? (unsigned char *)smalloc(biglen + 4) :
		       (unsigned char *)srealloc(session.pktout.data, biglen + 4));
#endif
	if (!session.pktout.data)
	    fatalbox(session, "Out of memory");
    }
    session.pktout.body = session.pktout.data + 4 + pad + 1;
}

static void s_wrpkt_start(CSshSession &session, int type, int len)
{
    ssh1_pktout_size(session, len);
    session.pktout.type = type;
}

static int s_wrpkt_prepare(CSshSession &session)
{
    int pad, len, biglen, i;
    unsigned long crc;

    session.pktout.body[-1] = session.pktout.type;

    log_packet(PKT_OUTGOING, session.pktout.type, ssh1_pkt_type(session.pktout.type),
	       session.pktout.body, session.pktout.length);

    if (session.ssh1_compressing) {
		unsigned char *compblk;
		int complen;
		zlib_compress_block(session.pktout.body - 1, session.pktout.length + 1,
					&compblk, &complen);
		ssh1_pktout_size(session, complen - 1);
		memcpy(session.pktout.body - 1, compblk, complen);
		sfree(compblk);
    }

    len = session.pktout.length + 5;	       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    for (i = 0; i < pad; i++)
		session.pktout.data[i + 4] = random_byte();
    crc = crc32(session.pktout.data + 4, biglen - 4);
    PUT_32BIT(session.pktout.data + biglen, crc);
    PUT_32BIT(session.pktout.data, len);

    if (session.cipher)
		session.cipher->encrypt(session, session.pktout.data + 4, biglen);

    return biglen + 4;
}

static void s_wrpkt(CSshSession &session)
{
    int len, backlog;
    len = s_wrpkt_prepare(session);
    backlog = sk_write(session, session.s, (char *)session.pktout.data, len);
    if (backlog > SSH_MAX_BACKLOG)
		ssh_throttle_all(session, 1, backlog);
}

static void s_wrpkt_defer(CSshSession &session)
{
    int len;
    len = s_wrpkt_prepare(session);
    if (session.deferred_len + len > session.deferred_size) {
		session.deferred_size = session.deferred_len + len + 128;
		session.deferred_send_data = 
			(unsigned char *)srealloc(session.deferred_send_data, session.deferred_size);
    }
    memcpy(session.deferred_send_data + session.deferred_len, session.pktout.data, len);
	session.deferred_len += len;
}

/*
 * Construct a packet with the specified contents.
 */
static void construct_packet(CSshSession &session, int pkttype, va_list ap1, va_list ap2)
{
    unsigned char *p, *argp, argchar;
    unsigned long argint;
    int pktlen, argtype, arglen;
    Bignum bn;

    pktlen = 0;
    while ((argtype = va_arg(ap1, int)) != PKT_END) {
	switch (argtype) {
	  case PKT_INT:
	    (void) va_arg(ap1, int);
	    pktlen += 4;
	    break;
	  case PKT_CHAR:
	    (void) va_arg(ap1, char);
	    pktlen++;
	    break;
	  case PKT_DATA:
	    (void) va_arg(ap1, unsigned char *);
	    arglen = va_arg(ap1, int);
	    pktlen += arglen;
	    break;
	  case PKT_STR:
	    argp = va_arg(ap1, unsigned char *);
	    arglen = strlen((const char *)argp);
	    pktlen += 4 + arglen;
	    break;
	  case PKT_BIGNUM:
	    bn = va_arg(ap1, Bignum);
	    pktlen += ssh1_bignum_length(bn);
	    break;
	  default:
	    assert(0);
	}
    }

    s_wrpkt_start(session, pkttype, pktlen);
    p = session.pktout.body;

    while ((argtype = va_arg(ap2, int)) != PKT_END) {
	switch (argtype) {
	  case PKT_INT:
	    argint = va_arg(ap2, int);
	    PUT_32BIT(p, argint);
	    p += 4;
	    break;
	  case PKT_CHAR:
	    argchar = va_arg(ap2, unsigned char);
	    *p = argchar;
	    p++;
	    break;
	  case PKT_DATA:
	    argp = va_arg(ap2, unsigned char *);
	    arglen = va_arg(ap2, int);
	    memcpy(p, argp, arglen);
	    p += arglen;
	    break;
	  case PKT_STR:
	    argp = va_arg(ap2, unsigned char *);
	    arglen = strlen((const char *)argp);
	    PUT_32BIT(p, arglen);
	    memcpy(p + 4, argp, arglen);
	    p += 4 + arglen;
	    break;
	  case PKT_BIGNUM:
	    bn = va_arg(ap2, Bignum);
	    p += ssh1_write_bignum(p, bn);
		freebn(bn);	//MIRO
	    break;
	}
    }
}

static void send_packet(CSshSession &session, int pkttype, ...)
{
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    construct_packet(session, pkttype, ap1, ap2);
    s_wrpkt(session);
}

static void defer_packet(CSshSession &session, int pkttype, ...)
{
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    construct_packet(session, pkttype, ap1, ap2);
    s_wrpkt_defer(session);
}

static int ssh_versioncmp(char *a, char *b)
{
    char *ae, *be;
    unsigned long av, bv;

    av = strtoul(a, &ae, 10);
    bv = strtoul(b, &be, 10);
    if (av != bv)
	return (av < bv ? -1 : +1);
    if (*ae == '.')
	ae++;
    if (*be == '.')
	be++;
    av = strtoul(ae, &ae, 10);
    bv = strtoul(be, &be, 10);
    if (av != bv)
	return (av < bv ? -1 : +1);
    return 0;
}


/*
 * Utility routines for putting an SSH-protocol `string' and
 * `uint32' into a SHA state.
 */
#include <stdio.h>
static void sha_string(SHA_State * s, void *str, int len)
{
    unsigned char lenblk[4];
    PUT_32BIT(lenblk, len);
    SHA_Bytes(s, lenblk, 4);
    SHA_Bytes(s, str, len);
}

static void sha_uint32(SHA_State * s, unsigned i)
{
    unsigned char intblk[4];
    PUT_32BIT(intblk, i);
    SHA_Bytes(s, intblk, 4);
}

/*
 * SSH2 packet construction functions.
 */
static void ssh2_pkt_ensure(CSshSession &session, int length)
{
    if (session.pktout.maxlen < length) {
		session.pktout.maxlen = length + 256;
		session.pktout.data =
			(session.pktout.data ==
			 NULL ? (unsigned char *)smalloc(session.pktout.maxlen +
					APIEXTRA) : (unsigned char *)srealloc(session.pktout.data,
							 session.pktout.maxlen +
							 APIEXTRA));
		if (!session.pktout.data)
			fatalbox(session, "Out of memory");
    }
}
static void ssh2_pkt_adddata(CSshSession &session, void *data, int len)
{
    session.pktout.length += len;
    ssh2_pkt_ensure(session, session.pktout.length);
    memcpy(session.pktout.data + session.pktout.length - len, data, len);
}
static void ssh2_pkt_addbyte(CSshSession &session, unsigned char byte)
{
    ssh2_pkt_adddata(session, &byte, 1);
}
static void ssh2_pkt_init(CSshSession &session, int pkt_type)
{
    session.pktout.length = 5;
    ssh2_pkt_addbyte(session, (unsigned char) pkt_type);
}
static void ssh2_pkt_addbool(CSshSession &session, unsigned char value)
{
    ssh2_pkt_adddata(session, &value, 1);
}
static void ssh2_pkt_adduint32(CSshSession &session, unsigned long value)
{
    unsigned char x[4];
    PUT_32BIT(x, value);
    ssh2_pkt_adddata(session, x, 4);
}
static void ssh2_pkt_addstring_start(CSshSession &session)
{
    ssh2_pkt_adduint32(session,0);
    session.pktout.savedpos = session.pktout.length;
}
static void ssh2_pkt_addstring_str(CSshSession &session, char *data)
{
    ssh2_pkt_adddata(session, data, strlen(data));
    PUT_32BIT(session.pktout.data + session.pktout.savedpos - 4,
	      session.pktout.length - session.pktout.savedpos);
}
static void ssh2_pkt_addstring_data(CSshSession &session, char *data, int len)
{
    ssh2_pkt_adddata(session, data, len);
    PUT_32BIT(session.pktout.data + session.pktout.savedpos - 4,
	      session.pktout.length - session.pktout.savedpos);
}
static void ssh2_pkt_addstring(CSshSession &session, char *data)
{
    ssh2_pkt_addstring_start(session);
    ssh2_pkt_addstring_str(session, data);
}
static char *ssh2_mpint_fmt(Bignum b, int *len)
{
    unsigned char *p;
    int i, n = (bignum_bitcount(b) + 7) / 8;
    p = (unsigned char *)smalloc(n + 1);
    if (!p)
		return (char *)p; //	fatalbox(session, "out of memory");//TOFIX
    p[0] = 0;
    for (i = 1; i <= n; i++)
	p[i] = bignum_byte(b, n - i);
    i = 0;
    while (i <= n && p[i] == 0 && (p[i + 1] & 0x80) == 0)
		i++;
    memmove(p, p + i, n + 1 - i);
    *len = n + 1 - i;
    return (char *)p;
}
static void ssh2_pkt_addmp(CSshSession &session, Bignum b)
{
    unsigned char *p;
    int len;
    p = (unsigned char *)ssh2_mpint_fmt(b, &len);
    ssh2_pkt_addstring_start(session);
    ssh2_pkt_addstring_data(session, (char *)p, len);
    sfree(p);
}

/*
 * Construct an SSH2 final-form packet: compress it, encrypt it,
 * put the MAC on it. Final packet, ready to be sent, is stored in
 * pktout.data. Total length is returned.
 */
static int ssh2_pkt_construct(CSshSession &session)
{
    int cipherblk, maclen, padding, i;

    log_packet(PKT_OUTGOING, session.pktout.data[5], ssh2_pkt_type(session.pktout.data[5]),
	       session.pktout.data + 6, session.pktout.length - 6);

    /*
     * Compress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (session.cscomp && session.cscomp->compress(session.pktout.data + 5, session.pktout.length - 5,
				       &newpayload, &newlen)) {
	    session.pktout.length = 5;
	    ssh2_pkt_adddata(session, newpayload, newlen);
	    sfree(newpayload);
	}
    }

    /*
     * Add padding. At least four bytes, and must also bring total
     * length (minus MAC) up to a multiple of the block size.
     */
    cipherblk = session.cscipher ? session.cscipher->blksize : 8;	/* block size */
    cipherblk = cipherblk < 8 ? 8 : cipherblk;	/* or 8 if blksize < 8 */
    padding = 4;
    padding +=
	(cipherblk - (session.pktout.length + padding) % cipherblk) % cipherblk;
    maclen = session.csmac ? session.csmac->len : 0;
    ssh2_pkt_ensure(session, session.pktout.length + padding + maclen);
    session.pktout.data[4] = padding;
    for (i = 0; i < padding; i++)
	session.pktout.data[session.pktout.length + i] = random_byte();
    PUT_32BIT(session.pktout.data, session.pktout.length + padding - 4);
    if (session.csmac)
		session.csmac->generate(session, session.pktout.data, session.pktout.length + padding,
			session.outgoing_sequence);
    session.outgoing_sequence++;	       /* whether or not we MACed */

    if (session.cscipher)
		session.cscipher->encrypt(session, session.pktout.data, session.pktout.length + padding);

    /* Ready-to-send packet starts at pktout.data. We return length. */
    return session.pktout.length + padding + maclen;
}

/*
 * Construct and send an SSH2 packet immediately.
 */
static void ssh2_pkt_send(CSshSession &session)
{
    int len;
    int backlog;
    len = ssh2_pkt_construct(session);
    backlog = sk_write(session, session.s, (char *)session.pktout.data, len);
    if (backlog > SSH_MAX_BACKLOG)
		ssh_throttle_all(session, 1, backlog);
}

/*
 * Construct an SSH2 packet and add it to a deferred data block.
 * Useful for sending multiple packets in a single sk_write() call,
 * to prevent a traffic-analysing listener from being able to work
 * out the length of any particular packet (such as the password
 * packet).
 * 
 * Note that because SSH2 sequence-numbers its packets, this can
 * NOT be used as an m4-style `defer' allowing packets to be
 * constructed in one order and sent in another.
 */
static void ssh2_pkt_defer(CSshSession &session)
{
    int len = ssh2_pkt_construct(session);
    if (session.deferred_len + len > session.deferred_size) {
		session.deferred_size = session.deferred_len + len + 128;
		session.deferred_send_data = 
			(unsigned char *)srealloc(session.deferred_send_data, session.deferred_size);
    }
    memcpy(session.deferred_send_data + session.deferred_len, session.pktout.data, len);
    session.deferred_len += len;
}

/*
 * Send the whole deferred data block constructed by
 * ssh2_pkt_defer() or SSH1's defer_packet().
 */
static void ssh_pkt_defersend(CSshSession &session)
{
    int backlog;
    backlog = sk_write(session, session.s, (char *)session.deferred_send_data, session.deferred_len);
    session.deferred_len = session.deferred_size = 0;
    sfree(session.deferred_send_data);
    session.deferred_send_data = NULL;
    if (backlog > SSH_MAX_BACKLOG)
		ssh_throttle_all(session, 1, backlog);
}

#if 0
void bndebug(char *string, Bignum b)
{
    unsigned char *p;
    int i, len;
    p = ssh2_mpint_fmt(b, &len);
    debug(("%s", string));
    for (i = 0; i < len; i++)
	debug((" %02x", p[i]));
    debug(("\n"));
    sfree(p);
}
#endif

static void sha_mpint(SHA_State * s, Bignum b)
{
    unsigned char *p;
    int len;
    p = (unsigned char *)ssh2_mpint_fmt(b, &len);
    sha_string(s, p, len);
    sfree(p);
}

/*
 * SSH2 packet decode functions.
 */
static unsigned long ssh2_pkt_getuint32(CSshSession &session)
{
    unsigned long value;
    if (session.pktin.length - session.pktin.savedpos < 4)
		return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = GET_32BIT(session.pktin.data + session.pktin.savedpos);
    session.pktin.savedpos += 4;
    return value;
}
static int ssh2_pkt_getbool(CSshSession &session)
{
    unsigned long value;
    if (session.pktin.length - session.pktin.savedpos < 1)
		return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = session.pktin.data[session.pktin.savedpos] != 0;
    session.pktin.savedpos++;
    return value;
}
static void ssh2_pkt_getstring(CSshSession &session, char **p, int *length)
{
    *p = NULL;
    if (session.pktin.length - session.pktin.savedpos < 4)
		return;
    *length = GET_32BIT(session.pktin.data + session.pktin.savedpos);
    session.pktin.savedpos += 4;
    if (session.pktin.length - session.pktin.savedpos < *length)
		return;
    *p = (char *)session.pktin.data + session.pktin.savedpos;
    session.pktin.savedpos += *length;
}
static Bignum ssh2_pkt_getmp(CSshSession &session)
{
    char *p;
    int length;
    Bignum b;

    ssh2_pkt_getstring(session, &p, &length);
    if (!p)
		return NULL;
    if (p[0] & 0x80) {
		bombout((session, "internal error: Can't handle negative mpints"));
		return NULL;
    }
    b = bignum_from_bytes((unsigned char *)p, length);
    return b;
}

/*
 * Helper function to add an SSH2 signature blob to a packet.
 * Expects to be shown the public key blob as well as the signature
 * blob. Normally works just like ssh2_pkt_addstring, but will
 * fiddle with the signature packet if necessary for
 * BUG_SSH2_RSA_PADDING.
 */
static void ssh2_add_sigblob(CSshSession &session, void *pkblob_v, int pkblob_len,
			     void *sigblob_v, int sigblob_len)
{
    unsigned char *pkblob = (unsigned char *)pkblob_v;
    unsigned char *sigblob = (unsigned char *)sigblob_v;

    /* dmemdump(pkblob, pkblob_len); */
    /* dmemdump(sigblob, sigblob_len); */

    /*
     * See if this is in fact an ssh-rsa signature and a buggy
     * server; otherwise we can just do this the easy way.
     */
    if ((session.ssh_remote_bugs & BUG_SSH2_RSA_PADDING) &&
		(GET_32BIT(pkblob) == 7 && !memcmp(pkblob+4, "ssh-rsa", 7))) {
	int pos, len, siglen;

	/*
	 * Find the byte length of the modulus.
	 */

	pos = 4+7;		       /* skip over "ssh-rsa" */
	pos += 4 + GET_32BIT(pkblob+pos);   /* skip over exponent */
	len = GET_32BIT(pkblob+pos);   /* find length of modulus */
	pos += 4;		       /* find modulus itself */
	while (len > 0 && pkblob[pos] == 0)
	    len--, pos++;
	/* debug(("modulus length is %d\n", len)); */

	/*
	 * Now find the signature integer.
	 */
	pos = 4+7;		       /* skip over "ssh-rsa" */
	siglen = GET_32BIT(sigblob+pos);
	/* debug(("signature length is %d\n", siglen)); */

	if (len != siglen) {
	    unsigned char newlen[4];
	    ssh2_pkt_addstring_start(session);
	    ssh2_pkt_addstring_data(session, (char *)sigblob, pos);
	    /* dmemdump(sigblob, pos); */
	    pos += 4;		       /* point to start of actual sig */
	    PUT_32BIT(newlen, len);
	    ssh2_pkt_addstring_data(session, (char *)newlen, 4);
	    /* dmemdump(newlen, 4); */
	    newlen[0] = 0;
	    while (len-- > siglen) {
		ssh2_pkt_addstring_data(session, (char *)newlen, 1);
		/* dmemdump(newlen, 1); */
	    }
	    ssh2_pkt_addstring_data(session, (char *)sigblob+pos, siglen);
	    /* dmemdump(sigblob+pos, siglen); */
	    return;
	}

	/* Otherwise fall through and do it the easy way. */
    }

    ssh2_pkt_addstring_start(session);
    ssh2_pkt_addstring_data(session, (char *)sigblob, sigblob_len);
}

/*
 * Examine the remote side's version string and compare it against
 * a list of known buggy implementations.
 */
static void ssh_detect_bugs(CSshSession &session, char *vstring)
{
    char *imp;			       /* pointer to implementation part */
    imp = vstring;
    imp += strcspn(imp, "-");
    if (*imp) imp++;
    imp += strcspn(imp, "-");
    if (*imp) imp++;

    session.ssh_remote_bugs = 0;

    if (!strcmp(imp, "1.2.18") || !strcmp(imp, "1.2.19") ||
		!strcmp(imp, "1.2.20") || !strcmp(imp, "1.2.21") ||
		!strcmp(imp, "1.2.22") || !strcmp(imp, "Cisco-1.25")) {
		
		/*
		 * These versions don't support SSH1_MSG_IGNORE, so we have
		 * to use a different defence against password length
		 * sniffing.
		 */
		session.ssh_remote_bugs |= BUG_CHOKES_ON_SSH1_IGNORE;
		logevent(session, "We believe remote version has SSH1 ignore bug");
    }

    if (!strcmp(imp, "Cisco-1.25")) {
		/*
		 * These versions need a plain password sent; they can't
		 * handle having a null and a random length of data after
		 * the password.
		 */
		session.ssh_remote_bugs |= BUG_NEEDS_SSH1_PLAIN_PASSWORD;
		logevent(session, "We believe remote version needs a plain SSH1 password");
    }

    if (!strcmp(imp, "Cisco-1.25")) {
		/*
		 * These versions apparently have no clue whatever about
		 * RSA authentication and will panic and die if they see
		 * an AUTH_RSA message.
		 */
		session.ssh_remote_bugs |= BUG_CHOKES_ON_RSA;
		logevent(session, "We believe remote version can't handle RSA authentication");
    }

    if (!strncmp(imp, "2.1.0", 5) || !strncmp(imp, "2.0.", 4) ||
		!strncmp(imp, "2.2.0", 5) || !strncmp(imp, "2.3.0", 5) ||
		!strncmp(imp, "2.1 ", 4)) {

		/*
		 * These versions have the HMAC bug.
		 */
		session.ssh_remote_bugs |= BUG_SSH2_HMAC;
		logevent(session, "We believe remote version has SSH2 HMAC bug");
    }

    if ((!strncmp(imp, "OpenSSH_2.", 10) && imp[10]>='5' && imp[10]<='9') ||
		(!strncmp(imp, "OpenSSH_3.", 10) && imp[10]>='0' && imp[10]<='2')) {

		/*
		 * These versions have the SSH2 RSA padding bug.
		 */
		session.ssh_remote_bugs |= BUG_SSH2_RSA_PADDING;
		logevent(session, "We believe remote version has SSH2 RSA padding bug");
    }
}

static int do_ssh_init(CSshSession &session, unsigned char c, int &crLine)
{
    static int vslen;
    static char version[10];
    static char *vstring;
    static int vstrsize;
    static char *vlog;
    static int i;

	static const int transS[] = { 1, 2, 2, 1 };
	static const int transH[] = { 0, 0, 3, 0 };
	static const int transminus[] = { 0, 0, 0, -1 };

    crBegin;

    /* Search for the string "SSH-" in the input. */
    i = 0;
    while (1) {
	if (c == 'S')
	    i = transS[i];
	else if (c == 'H')
	    i = transH[i];
	else if (c == '-')
	    i = transminus[i];
	else
	    i = 0;
	if (i < 0)
	    break;
	crReturn(1);		       /* get another character */
    }

    vstrsize = 16;
    vstring = (char *)smalloc(vstrsize);
    strcpy(vstring, "SSH-");
    vslen = 4;
    i = 0;
    while (1) {
		crReturn(1);		       /* get another char */
		if (vslen >= vstrsize - 1) {
			vstrsize += 16;
			vstring = (char *)srealloc(vstring, vstrsize);
		}
		vstring[vslen++] = c;
		if (i >= 0) {
			if (c == '-') {
			version[i] = '\0';
			i = -1;
			} else if (i < sizeof(version) - 1)
			version[i++] = c;
		} else if (c == '\n')
			break;
    }

    session.ssh_agentfwd_enabled = FALSE;
    session.rdpkt2_state.incoming_sequence = 0;

    vstring[vslen] = 0;
    vlog = (char *)smalloc(20 + vslen);
    vstring[strcspn (vstring, "\r\n")] = '\0'; /* remove end-of-line chars */
    sprintf(vlog, "Server version: %s", vstring);
    logevent(session, vlog);
    ssh_detect_bugs(session,vstring);
    sfree(vlog);

    /*
     * Server version "1.99" means we can choose whether we use v1
     * or v2 protocol. Choice is based on cfg.sshprot.
     */
    if (ssh_versioncmp(version, cfg.sshprot == 1 ? "2.0" : "1.99") >= 0) {
		/*
		 * This is a v2 server. Begin v2 protocol.
		 */
		char verstring[80], vlog[100];
		sprintf(verstring, "SSH-2.0-%s", sshver);
		SHA_Init(&session.exhashbase);
		/*
		 * Hash our version string and their version string.
		 */
		sha_string(&session.exhashbase, verstring, strlen(verstring));
		sha_string(&session.exhashbase, vstring, strcspn(vstring, "\r\n"));
		sprintf(vlog, "We claim version: %s", verstring);
		logevent(session, vlog);
		strcat(verstring, "\n");
		logevent(session, "Using SSH protocol version 2");
		sk_write(session, session.s, verstring, strlen(verstring));
		ssh_protocol = ssh2_protocol;
		ssh_version = 2;
		session.s_rdpkt = ssh2_rdpkt;
    } else {
		/*
		 * This is a v1 server. Begin v1 protocol.
		 */
		char verstring[80], vlog[100];
		sprintf(verstring, "SSH-%s-%s",
			(ssh_versioncmp(version, "1.5") <= 0 ? version : "1.5"),
			sshver);
		sprintf(vlog, "We claim version: %s", verstring);
		logevent(session, vlog);
		strcat(verstring, "\n");
		
		if (cfg.sshprot == 3) {
			bombout((session, "SSH protocol version 2 required by user but not provided by server"));
			crReturn(0);
		}

		logevent(session, "Using SSH protocol version 1");
		sk_write(session, session.s, verstring, strlen(verstring));
		ssh_protocol = ssh1_protocol;
		ssh_version = 1;
		session.s_rdpkt = ssh1_rdpkt;
    }
    session.ssh_state = SSH_STATE_BEFORE_SIZE;

    sfree(vstring);

    crFinish(0);
}

static void ssh_gotdata(CSshSession &session, unsigned char *data, int datalen, int &crLine)
{
    crBegin;

    /*
     * To begin with, feed the characters one by one to the
     * protocol initialisation / selection function do_ssh_init().
     * When that returns 0, we're done with the initial greeting
     * exchange and can move on to packet discipline.
     */
    while (1) {
	int ret;
	if (datalen == 0)
	    crReturnV;		       /* more data please */
	ret = do_ssh_init(session, *data, session.n_do_ssh_init);
	data++;
	datalen--;
	if (ret == 0)
	    break;
    }

    /*
     * We emerge from that loop when the initial negotiation is
     * over and we have selected an s_rdpkt function. Now pass
     * everything to s_rdpkt, and then pass the resulting packets
     * to the proper protocol handler.
     */
    if (datalen == 0)
		crReturnV;
		while (1) {
			while (datalen > 0) {
				if (session.s_rdpkt(session, &data, &datalen, session.n_s_rdpkt) == 0) {
					if (session.ssh_state == SSH_STATE_CLOSED) {
						return;
					}
					ssh_protocol(session, NULL, 0, 1, session.n_ssh_protocol);
					if (session.ssh_state == SSH_STATE_CLOSED) {
						return;
					}
				}
			}
		crReturnV;
		}
    crFinishV;
}

static int ssh_closing(CSshSession &session, Plug plug, char *error_msg, int error_code,
		       int calling_back)
{
    session.ssh_state = SSH_STATE_CLOSED;
    if (session.s) {
        sk_close(session, session.s);
        session.s = NULL;
    }
    if (error_msg) {
		/* A socket error has occurred. */
		logevent(session, error_msg);
		connection_fatal(session, error_msg);
    } else {
		/* Otherwise, the remote side closed the connection normally. */
    }
    return 0;
}

static int ssh_receive(CSshSession &session, Plug plug, int urgent, char *data, int len)
{
	//fix
	//ASSERT(NULL != session.s_rdpkt) 
    
	ssh_gotdata(session, (unsigned char *)data, len, session.n_ssh_gotdata);
    if (session.ssh_state == SSH_STATE_CLOSED) {
		if (session.s) {
			sk_close(session, session.s);
			session.s = NULL;
		}
		return 0;
    }
    return 1;
}

static void ssh_sent(CSshSession &session, Plug plug, int bufsize)
{
    /*
     * If the send backlog on the SSH socket itself clears, we
     * should unthrottle the whole world if it was throttled.
     */
    if (bufsize < SSH_MAX_BACKLOG)
		ssh_throttle_all(session, 0, bufsize);
}

/*
 * Connect to specified host and port.
 * Returns an error message, or NULL on success.
 * Also places the canonical host name into `realhost'. It must be
 * freed by the caller.
 */
static char *connect_to_host(CSshSession &session, char *host, int port, char **realhost, int nodelay)
{
    static struct plug_function_table fn_table = {
		ssh_closing,
		ssh_receive,
		ssh_sent,
		NULL
    }, *fn_table_ptr = &fn_table;

    SockAddr addr;
    char *err;
#ifdef FWHACK
    char *FWhost;
    int FWport;
#endif

    session.savedhost = (char *)smalloc(1 + strlen(host));
    if (!session.savedhost)
		fatalbox(session, "Out of memory");
    strcpy(session.savedhost, host);

    if (port < 0)
		port = 22;		       /* default ssh port */
    session.savedport = port;

#ifdef FWHACK
    FWhost = host;
    FWport = port;
    host = FWSTR;
    port = 23;
#endif

    /*
     * Try to find host.
     */
    {
		char buf[200];
		sprintf(buf, "Looking up host \"%.170s\"", host);
		logevent(session, buf);
    }
    addr = sk_namelookup(host, realhost);
    if ((err = sk_addr_error(addr)))
		return err;

#ifdef FWHACK
    *realhost = strdup(FWhost);
#endif

    /*
     * Open socket.
     */
    {
		char buf[200], addrbuf[100];
		sk_getaddr(addr, addrbuf, 100);
		sprintf(buf, "Connecting to %.100s port %d", addrbuf, port);
		logevent(session, buf);
    }
    session.s = new_connection(session, addr, *realhost, port, 0, 1, nodelay, &fn_table_ptr);
    if ((err = sk_socket_error(session, session.s))) {
		sfree(addr);	//MIRO
		session.s = NULL;
		return err;	
    }

	sfree(addr);	//MIRO

#ifdef FWHACK
    sk_write(s, "connect ", 8);
    sk_write(s, FWhost, strlen(FWhost));
    {
		char buf[20];
		sprintf(buf, " %d\n", FWport);
		sk_write(s, buf, strlen(buf));
    }
#endif

    return NULL;
}

/*
 * Throttle or unthrottle the SSH connection.
 */
static void ssh1_throttle(CSshSession &session, int adjust)
{
    int old_count = ssh1_throttle_count;
    ssh1_throttle_count += adjust;
    assert(ssh1_throttle_count >= 0);
    if (ssh1_throttle_count && !old_count) {
		sk_set_frozen(session, session.s, 1);
    } else if (!ssh1_throttle_count && old_count) {
		sk_set_frozen(session, session.s, 0);
    }
}

/*
 * Throttle or unthrottle _all_ local data streams (for when sends
 * on the SSH connection itself back up).
 */
static void ssh_throttle_all(CSshSession &session, int enable, int bufsize)
{
    int i;
    struct ssh_channel *c;

    if (enable == ssh_throttled_all)
	return;
    ssh_throttled_all = enable;
    ssh_overall_bufsize = bufsize;
    if (!session.ssh_channels)
	return;
    for (i = 0; NULL != (c = (struct ssh_channel *)index234(session.ssh_channels, i)); i++) {
	switch (c->type) {
	  case CHAN_MAINSESSION:
	    /*
	     * This is treated separately, outside the switch.
	     */
	    break;
	  case CHAN_X11:
	    x11_override_throttle(session, c->u.x11.s, enable);
	    break;
	  case CHAN_AGENT:
	    /* Agent channels require no buffer management. */
	    break;
	  case CHAN_SOCKDATA:
	    pfd_override_throttle(session, c->u.x11.s, enable);
	    break;
	}
    }
}

/*
 * Handle the key exchange and user authentication phases.
 */
static int do_ssh1_login(CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine)
{
    int i, j;
    static int len;
    static unsigned char *rsabuf, *keystr1, *keystr2;
    unsigned char cookie[8];
    struct RSAKey servkey, hostkey;
    struct MD5Context md5c;
    static unsigned long supported_ciphers_mask, supported_auths_mask;
    static int tried_publickey, tried_agent;
    static int tis_auth_refused, ccard_auth_refused;
    static unsigned char session_id[16];
    static int cipher_type;
    static char username[100];
    static void *publickey_blob;
    int publickey_bloblen;

    crBegin;

    if (!ispkt)
		crWaitUntil(ispkt);

    if (session.pktin.type != SSH1_SMSG_PUBLIC_KEY) {
		bombout((session, "Public key packet not received"));
		crReturn(0);
    }

    logevent(session, "Received public keys");

    memcpy(cookie, session.pktin.body, 8);

    i = makekey(session.pktin.body + 8, &servkey, &keystr1, 0);
    j = makekey(session.pktin.body + 8 + i, &hostkey, &keystr2, 0);

    /*
     * Log the host key fingerprint.
     */
    {
		char logmsg[80];
		logevent(session, "Host key fingerprint is:");
		strcpy(logmsg, "      ");
		hostkey.comment = NULL;
		rsa_fingerprint(logmsg + strlen(logmsg),
				sizeof(logmsg) - strlen(logmsg), &hostkey);
		logevent(session, logmsg);
    }

    session.ssh1_remote_protoflags = GET_32BIT(session.pktin.body + 8 + i + j);
    supported_ciphers_mask = GET_32BIT(session.pktin.body + 12 + i + j);
    supported_auths_mask = GET_32BIT(session.pktin.body + 16 + i + j);

    session.ssh1_local_protoflags =
	session.ssh1_remote_protoflags & SSH1_PROTOFLAGS_SUPPORTED;
    session.ssh1_local_protoflags |= SSH1_PROTOFLAG_SCREEN_NUMBER;

    MD5Init(&md5c);
    MD5Update(&md5c, keystr2, hostkey.bytes);
    MD5Update(&md5c, keystr1, servkey.bytes);
    MD5Update(&md5c, session.pktin.body, 8);
    MD5Final(session_id, &md5c);

    for (i = 0; i < 32; i++)
		session.session_key[i] = random_byte();

    len = (hostkey.bytes > servkey.bytes ? hostkey.bytes : servkey.bytes);

    rsabuf = (unsigned char *)smalloc(len);
    if (!rsabuf)
		fatalbox(session, "Out of memory");

    /*
     * Verify the host key.
     */
    {
	/*
	 * First format the key into a string.
	 */
	int len = rsastr_len(&hostkey);
	char fingerprint[100];
	char *keystr = (char *)smalloc(len);
	if (!keystr)
	    fatalbox(session, "Out of memory");
	rsastr_fmt(keystr, &hostkey);
	rsa_fingerprint(fingerprint, sizeof(fingerprint), &hostkey);
	verify_ssh_host_key(session, session.savedhost, session.savedport, "rsa", keystr,
			    fingerprint);
	sfree(keystr);
    }

    for (i = 0; i < 32; i++) {
		rsabuf[i] = session.session_key[i];
		if (i < 16)
			rsabuf[i] ^= session_id[i];
		}

		if (hostkey.bytes > servkey.bytes) {
			rsaencrypt(rsabuf, 32, &servkey);
			rsaencrypt(rsabuf, servkey.bytes, &hostkey);
		} else {
			rsaencrypt(rsabuf, 32, &hostkey);
			rsaencrypt(rsabuf, hostkey.bytes, &servkey);
		}

		logevent(session, "Encrypted session key");

		{
		int cipher_chosen, warn;
		char *cipher_string;

		cipher_chosen = 0; warn = 0;
		cipher_string = NULL;

		for (i = 0; !cipher_chosen && i < CIPHER_MAX; i++) {
			int next_cipher = cfg.ssh_cipherlist[i];
			if (next_cipher == CIPHER_WARN) {
			/* If/when we choose a cipher, warn about it */
			warn = 1;
			} else if (next_cipher == CIPHER_AES) {
			/* XXX Probably don't need to mention this. */
			logevent(session, "AES not supported in SSH1, skipping");
			} else {
			switch (next_cipher) {
			  case CIPHER_3DES:     cipher_type = SSH_CIPHER_3DES;
						cipher_string = "3DES"; break;
			  case CIPHER_BLOWFISH: cipher_type = SSH_CIPHER_BLOWFISH;
						cipher_string = "Blowfish"; break;
			  case CIPHER_DES:	cipher_type = SSH_CIPHER_DES;
						cipher_string = "single-DES"; break;
			}
			if (supported_ciphers_mask & (1 << cipher_type))
				cipher_chosen = 1;
			}
		}
		if (!cipher_chosen) {
			if ((supported_ciphers_mask & (1 << SSH_CIPHER_3DES)) == 0)
			bombout((session, "Server violates SSH 1 protocol by not "
				 "supporting 3DES encryption"));
			else
			/* shouldn't happen */
			bombout((session, "No supported ciphers found"));
			crReturn(0);
		}

		/* Warn about chosen cipher if necessary. */
		if (warn)
			askcipher(cipher_string, 0);
		}

		switch (cipher_type) {
		  case SSH_CIPHER_3DES:
			logevent(session, "Using 3DES encryption");
			break;
		  case SSH_CIPHER_DES:
			logevent(session, "Using single-DES encryption");
			break;
		  case SSH_CIPHER_BLOWFISH:
			logevent(session, "Using Blowfish encryption");
			break;
		}

		send_packet(session, SSH1_CMSG_SESSION_KEY,
			PKT_CHAR, cipher_type,
			PKT_DATA, cookie, 8,
			PKT_CHAR, (len * 8) >> 8, PKT_CHAR, (len * 8) & 0xFF,
			PKT_DATA, rsabuf, len,
			PKT_INT, session.ssh1_local_protoflags, PKT_END);

		logevent(session, "Trying to enable encryption...");

		sfree(rsabuf);

		session.cipher = cipher_type == SSH_CIPHER_BLOWFISH ? &ssh_blowfish_ssh1 :
		cipher_type == SSH_CIPHER_DES ? &ssh_des : &ssh_3des;
		session.cipher->sesskey(session, session.session_key);

		crWaitUntil(ispkt);

		if (session.pktin.type != SSH1_SMSG_SUCCESS) {
			bombout((session, "Encryption not successfully enabled"));
			crReturn(0);
		}

		logevent(session, "Successfully started encryption");

		//fflush(stdout);
		{
		static int pos;
		static char c;

		pos = 0;

		if ((flags & FLAG_INTERACTIVE) && !*cfg.username) {
			if (session.ssh_get_line) {
			if (!session.ssh_get_line("login as: ",
					  username, sizeof(username), FALSE)) {
				/*
				 * get_line failed to get a username.
				 * Terminate.
				 */
				logevent(session, "No username provided. Abandoning session.");
				session.ssh_state = SSH_STATE_CLOSED;
				crReturn(1);
			}
			} else {
			c_write_str("login as: ");
			session.ssh_send_ok = 1;
			while (pos >= 0) {
				crWaitUntil(!ispkt);
				while (inlen--)
				switch (c = *in++) {
				  case 10:
				  case 13:
					username[pos] = 0;
					pos = -1;
					break;
				  case 8:
				  case 127:
					if (pos > 0) {
					c_write_str("\b \b");
					pos--;
					}
					break;
				  case 21:
				  case 27:
					while (pos > 0) {
					c_write_str("\b \b");
					pos--;
					}
					break;
				  case 3:
				  case 4:
					cleanup_exit(0);
					break;
				  default:
					if (((c >= ' ' && c <= '~') ||
					 ((unsigned char) c >= 160))
					&& pos < sizeof(username)-1) {
					username[pos++] = c;
					c_write(&c, 1);
					}
					break;
				}
			}
			c_write_str("\r\n");
			username[strcspn(username, "\n\r")] = '\0';
			}
		} else {
			strncpy(username, cfg.username, 99);
			username[99] = '\0';
		}

		send_packet(session, SSH1_CMSG_USER, PKT_STR, username, PKT_END);
		{
			char userlog[22 + sizeof(username)];
			sprintf(userlog, "Sent username \"%s\"", username);
			logevent(session, userlog);
			if (flags & FLAG_INTERACTIVE &&
			(!((flags & FLAG_STDERR) && (flags & FLAG_VERBOSE)))) {
			strcat(userlog, "\r\n");
			c_write_str(userlog);
			}
		}
    }

    crWaitUntil(ispkt);

    if ((session.ssh_remote_bugs & BUG_CHOKES_ON_RSA)) {
		/* We must not attempt PK auth. Pretend we've already tried it. */
		tried_publickey = tried_agent = 1;
    } else {
		tried_publickey = tried_agent = 0;
    }
    tis_auth_refused = ccard_auth_refused = 0;
    /* Load the public half of cfg.keyfile so we notice if it's in Pageant */
    if (*cfg.keyfile) {
	if (!rsakey_pubblob(cfg.keyfile, &publickey_blob, &publickey_bloblen))
	    publickey_blob = NULL;
    } else
	publickey_blob = NULL;

    while (session.pktin.type == SSH1_SMSG_FAILURE) {
		static char password[100];
		static char prompt[200];
		static int pos;
		static char c;
		static int pwpkt_type;
		pwpkt_type = SSH1_CMSG_AUTH_PASSWORD;

	if (agent_exists() && !tried_agent) {
	    /*
	     * Attempt RSA authentication using Pageant.
	     */
	    static unsigned char request[5], *response, *p;
	    static int responselen;
	    static int i, nkeys;
	    static int authed;
	    void *r;

	    authed = FALSE;
	    tried_agent = 1;
	    logevent(session, "Pageant is running. Requesting keys.");

	    /* Request the keys held by the agent. */
	    PUT_32BIT(request, 1);
	    request[4] = SSH1_AGENTC_REQUEST_RSA_IDENTITIES;
	    agent_query(request, 5, &r, &responselen);
	    response = (unsigned char *) r;
	    if (response && responselen >= 5 &&
		response[4] == SSH1_AGENT_RSA_IDENTITIES_ANSWER) {
		p = response + 5;
		nkeys = GET_32BIT(p);
		p += 4;
		{
		    char buf[64];
		    sprintf(buf, "Pageant has %d SSH1 keys", nkeys);
		    logevent(session, buf);
		}
		for (i = 0; i < nkeys; i++) {
		    static struct RSAKey key;
		    static Bignum challenge;
		    static char *commentp;
		    static int commentlen;

		    {
				char buf[64];
				sprintf(buf, "Trying Pageant key #%d", i);
				logevent(session, buf);
		    }
		    if (publickey_blob &&
			!memcmp(p, publickey_blob, publickey_bloblen)) {
			logevent(session, "This key matches configured key file");
			tried_publickey = 1;
		    }
		    p += 4;
		    p += ssh1_read_bignum(p, &key.exponent);
		    p += ssh1_read_bignum(p, &key.modulus);
		    commentlen = GET_32BIT(p);
		    p += 4;
		    commentp = (char *)p;
		    p += commentlen;
		    send_packet(session, SSH1_CMSG_AUTH_RSA,
				PKT_BIGNUM, key.modulus, PKT_END);
		    crWaitUntil(ispkt);
		    if (session.pktin.type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
				logevent(session, "Key refused");
				continue;
		    }
		    logevent(session, "Received RSA challenge");
		    ssh1_read_bignum(session.pktin.body, &challenge);
		    {
			char *agentreq, *q, *ret;
			void *vret;
			int len, retlen;
			len = 1 + 4;   /* message type, bit count */
			len += ssh1_bignum_length(key.exponent);
			len += ssh1_bignum_length(key.modulus);
			len += ssh1_bignum_length(challenge);
			len += 16;     /* session id */
			len += 4;      /* response format */
			agentreq = (char *)smalloc(4 + len);
			PUT_32BIT(agentreq, len);
			q = agentreq + 4;
			*q++ = SSH1_AGENTC_RSA_CHALLENGE;
			PUT_32BIT(q, bignum_bitcount(key.modulus));
			q += 4;
			q += ssh1_write_bignum(q, key.exponent);
			q += ssh1_write_bignum(q, key.modulus);
			q += ssh1_write_bignum(q, challenge);
			memcpy(q, session_id, 16);
			q += 16;
			PUT_32BIT(q, 1);	/* response format */
			agent_query(agentreq, len + 4, &vret, &retlen);
			ret = (char *)vret;
			sfree(agentreq);
			if (ret) {
			    if (ret[4] == SSH1_AGENT_RSA_RESPONSE) {
				logevent(session, "Sending Pageant's response");
				send_packet(session, SSH1_CMSG_AUTH_RSA_RESPONSE,
					    PKT_DATA, ret + 5, 16,
					    PKT_END);
				sfree(ret);
				crWaitUntil(ispkt);
				if (session.pktin.type == SSH1_SMSG_SUCCESS) {
				    logevent(session, "Pageant's response accepted");
				    if (flags & FLAG_VERBOSE) {
					c_write_str
					    ("Authenticated using RSA key \"");
					c_write(commentp, commentlen);
					c_write_str("\" from agent\r\n");
				    }
				    authed = TRUE;
				} else
				    logevent(session, "Pageant's response not accepted");
			    } else {
					logevent(session, "Pageant failed to answer challenge");
					sfree(ret);
			    }
			} else {
			    logevent(session, "No reply received from Pageant");
			}
		    }
		    freebn(key.exponent);
		    freebn(key.modulus);
		    freebn(challenge);
		    if (authed)
			break;
		}
	    }
	    if (authed)
		break;
	}
	if (*cfg.keyfile && !tried_publickey)
	    pwpkt_type = SSH1_CMSG_AUTH_RSA;

	if (cfg.try_tis_auth &&
	    (supported_auths_mask & (1 << SSH1_AUTH_TIS)) &&
	    !tis_auth_refused) {
	    pwpkt_type = SSH1_CMSG_AUTH_TIS_RESPONSE;
	    logevent(session, "Requested TIS authentication");
	    send_packet(session, SSH1_CMSG_AUTH_TIS, PKT_END);
	    crWaitUntil(ispkt);
	    if (session.pktin.type != SSH1_SMSG_AUTH_TIS_CHALLENGE) {
			logevent(session, "TIS authentication declined");
			if (flags & FLAG_INTERACTIVE)
				c_write_str("TIS authentication refused.\r\n");
			tis_auth_refused = 1;
			continue;
	    } else {
			int challengelen = ((session.pktin.body[0] << 24) |
						(session.pktin.body[1] << 16) |
						(session.pktin.body[2] << 8) |
						(session.pktin.body[3]));
			logevent(session, "Received TIS challenge");
			if (challengelen > sizeof(prompt) - 1)
				challengelen = sizeof(prompt) - 1;	/* prevent overrun */
			memcpy(prompt, session.pktin.body + 4, challengelen);
			/* Prompt heuristic comes from OpenSSH */
			strncpy(prompt + challengelen,
					memchr(prompt, '\n', challengelen) ?
				"": "\r\nResponse: ",
				(sizeof prompt) - challengelen);
			prompt[(sizeof prompt) - 1] = '\0';
	    }
	}
	if (cfg.try_tis_auth &&
	    (supported_auths_mask & (1 << SSH1_AUTH_CCARD)) &&
	    !ccard_auth_refused) {
	    pwpkt_type = SSH1_CMSG_AUTH_CCARD_RESPONSE;
	    logevent(session, "Requested CryptoCard authentication");
	    send_packet(session, SSH1_CMSG_AUTH_CCARD, PKT_END);
	    crWaitUntil(ispkt);
	    if (session.pktin.type != SSH1_SMSG_AUTH_CCARD_CHALLENGE) {
			logevent(session, "CryptoCard authentication declined");
			c_write_str("CryptoCard authentication refused.\r\n");
			ccard_auth_refused = 1;
			continue;
	    } else {
			int challengelen = ((session.pktin.body[0] << 24) |
						(session.pktin.body[1] << 16) |
						(session.pktin.body[2] << 8) |
						(session.pktin.body[3]));
			logevent(session, "Received CryptoCard challenge");
			if (challengelen > sizeof(prompt) - 1)
				challengelen = sizeof(prompt) - 1;	/* prevent overrun */
			memcpy(prompt, session.pktin.body + 4, challengelen);
			strncpy(prompt + challengelen,
					memchr(prompt, '\n', challengelen) ?
				"" : "\r\nResponse: ",
				sizeof(prompt) - challengelen);
			prompt[sizeof(prompt) - 1] = '\0';
	    }
	}
	if (pwpkt_type == SSH1_CMSG_AUTH_PASSWORD) {
	    sprintf(prompt, "%.90s@%.90s's password: ",
		    username, session.savedhost);
	}
	if (pwpkt_type == SSH1_CMSG_AUTH_RSA) {
	    char *comment = NULL;
	    int type;
	    char msgbuf[256];
	    if (flags & FLAG_VERBOSE)
		c_write_str("Trying public key authentication.\r\n");
	    sprintf(msgbuf, "Trying public key \"%.200s\"", cfg.keyfile);
	    logevent(session, msgbuf);
	    type = key_type(cfg.keyfile);
	    if (type != SSH_KEYTYPE_SSH1) {
			sprintf(msgbuf, "Key is of wrong type (%s)", key_type_to_str(type));
		logevent(session, msgbuf);
		c_write_str(msgbuf);
		c_write_str("\r\n");
		tried_publickey = 1;
		continue;
	    }
	    if (!rsakey_encrypted(cfg.keyfile, &comment)) {
		if (flags & FLAG_VERBOSE)
		    c_write_str("No passphrase required.\r\n");
		goto tryauth;
	    }
	    sprintf(prompt, "Passphrase for key \"%.100s\": ", comment);
	    sfree(comment);
	}

	/*
	 * Show password prompt, having first obtained it via a TIS
	 * or CryptoCard exchange if we're doing TIS or CryptoCard
	 * authentication.
	 */
	if (session.ssh_get_line) {
	    if (!session.ssh_get_line(prompt, password, sizeof(password), TRUE)) {
			/*
			 * get_line failed to get a password (for example
			 * because one was supplied on the command line
			 * which has already failed to work). Terminate.
			 */
			send_packet(session, SSH1_MSG_DISCONNECT,
					PKT_STR, "No more passwords available to try",
					PKT_END);
			logevent(session, "Unable to authenticate");
			connection_fatal(session, "Unable to authenticate");
			session.ssh_state = SSH_STATE_CLOSED;
			crReturn(1);
	    }
	} else {
	    /* Prompt may have come from server. We've munged it a bit, so
	     * we know it to be zero-terminated at least once. */
	    c_write_untrusted(prompt, strlen(prompt));
	    pos = 0;
	    session.ssh_send_ok = 1;
	    while (pos >= 0) {
		crWaitUntil(!ispkt);
		while (inlen--)
		    switch (c = *in++) {
		      case 10:
		      case 13:
			password[pos] = 0;
			pos = -1;
			break;
		      case 8:
		      case 127:
			if (pos > 0)
			    pos--;
			break;
		      case 21:
		      case 27:
			pos = 0;
			break;
		      case 3:
		      case 4:
			cleanup_exit(0);
			break;
		      default:
			if (pos < sizeof(password)-1)
			    password[pos++] = c;
			break;
		    }
	    }
	    c_write_str("\r\n");
	}

      tryauth:
	if (pwpkt_type == SSH1_CMSG_AUTH_RSA) {
	    /*
	     * Try public key authentication with the specified
	     * key file.
	     */
	    static struct RSAKey pubkey;
	    static Bignum challenge, response;
	    static int i;
	    static unsigned char buffer[32];

	    tried_publickey = 1;
	    i = loadrsakey(cfg.keyfile, &pubkey, password);
	    if (i == 0) {
		c_write_str("Couldn't load private key from ");
		c_write_str(cfg.keyfile);
		c_write_str(".\r\n");
		continue;	       /* go and try password */
	    }
	    if (i == -1) {
		c_write_str("Wrong passphrase.\r\n");
		tried_publickey = 0;
		continue;	       /* try again */
	    }

	    /*
	     * Send a public key attempt.
	     */
	    send_packet(session, SSH1_CMSG_AUTH_RSA,
			PKT_BIGNUM, pubkey.modulus, PKT_END);

	    crWaitUntil(ispkt);
	    if (session.pktin.type == SSH1_SMSG_FAILURE) {
			c_write_str("Server refused our public key.\r\n");
			continue;	       /* go and try password */
	    }
	    if (session.pktin.type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
			bombout((session, "Bizarre response to offer of public key"));
			crReturn(0);
	    }
	    ssh1_read_bignum(session.pktin.body, &challenge);
	    response = rsadecrypt(challenge, &pubkey);
	    freebn(pubkey.private_exponent);	/* burn the evidence */

	    for (i = 0; i < 32; i++) {
		buffer[i] = bignum_byte(response, 31 - i);
	    }

	    MD5Init(&md5c);
	    MD5Update(&md5c, buffer, 32);
	    MD5Update(&md5c, session_id, 16);
	    MD5Final(buffer, &md5c);

	    send_packet(session, SSH1_CMSG_AUTH_RSA_RESPONSE,
			PKT_DATA, buffer, 16, PKT_END);

	    crWaitUntil(ispkt);
	    if (session.pktin.type == SSH1_SMSG_FAILURE) {
			if (flags & FLAG_VERBOSE)
				c_write_str("Failed to authenticate with our public key.\r\n");
			continue;	       /* go and try password */
	    } else if (session.pktin.type != SSH1_SMSG_SUCCESS) {
			bombout((session, "Bizarre response to RSA authentication response"));
			crReturn(0);
	    }

	    break;		       /* we're through! */
	} else {
	    if (pwpkt_type == SSH1_CMSG_AUTH_PASSWORD) {
		/*
		 * Defence against traffic analysis: we send a
		 * whole bunch of packets containing strings of
		 * different lengths. One of these strings is the
		 * password, in a SSH1_CMSG_AUTH_PASSWORD packet.
		 * The others are all random data in
		 * SSH1_MSG_IGNORE packets. This way a passive
		 * listener can't tell which is the password, and
		 * hence can't deduce the password length.
		 * 
		 * Anybody with a password length greater than 16
		 * bytes is going to have enough entropy in their
		 * password that a listener won't find it _that_
		 * much help to know how long it is. So what we'll
		 * do is:
		 * 
		 *  - if password length < 16, we send 15 packets
		 *    containing string lengths 1 through 15
		 * 
		 *  - otherwise, we let N be the nearest multiple
		 *    of 8 below the password length, and send 8
		 *    packets containing string lengths N through
		 *    N+7. This won't obscure the order of
		 *    magnitude of the password length, but it will
		 *    introduce a bit of extra uncertainty.
		 * 
		 * A few servers (the old 1.2.18 through 1.2.22)
		 * can't deal with SSH1_MSG_IGNORE. For these
		 * servers, we need an alternative defence. We make
		 * use of the fact that the password is interpreted
		 * as a C string: so we can append a NUL, then some
		 * random data.
		 * 
		 * One server (a Cisco one) can deal with neither
		 * SSH1_MSG_IGNORE _nor_ a padded password string.
		 * For this server we are left with no defences
		 * against password length sniffing.
		 */
		if (!(session.ssh_remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE)) {
		    /*
		     * The server can deal with SSH1_MSG_IGNORE, so
		     * we can use the primary defence.
		     */
		    int bottom, top, pwlen, i;
		    char *randomstr;

		    pwlen = strlen(password);
		    if (pwlen < 16) {
			bottom = 0;    /* zero length passwords are OK! :-) */
			top = 15;
		    } else {
			bottom = pwlen & ~7;
			top = bottom + 7;
		    }

		    assert(pwlen >= bottom && pwlen <= top);

		    randomstr = (char *)smalloc(top + 1);

		    for (i = bottom; i <= top; i++) {
			if (i == pwlen)
			    defer_packet(session, pwpkt_type, PKT_STR, password,
					 PKT_END);
			else {
			    for (j = 0; j < i; j++) {
				do {
				    randomstr[j] = random_byte();
				} while (randomstr[j] == '\0');
			    }
			    randomstr[i] = '\0';
			    defer_packet(session, SSH1_MSG_IGNORE,
					 PKT_STR, randomstr, PKT_END);
			}
		    }
		    logevent(session, "Sending password with camouflage packets");
		    ssh_pkt_defersend(session);
		} 
		else if (!(session.ssh_remote_bugs & BUG_NEEDS_SSH1_PLAIN_PASSWORD)) {
		    /*
		     * The server can't deal with SSH1_MSG_IGNORE
		     * but can deal with padded passwords, so we
		     * can use the secondary defence.
		     */
		    char string[64];
		    char *s;
		    int len;

		    len = strlen(password);
		    if (len < sizeof(string)) {
			s = string;
			strcpy(string, password);
			len++;	       /* cover the zero byte */
			while (len < sizeof(string)) {
			    string[len++] = (char) random_byte();
			}
		    } else {
			s = password;
		    }
		    logevent(session, "Sending length-padded password");
		    send_packet(session, pwpkt_type, PKT_INT, len,
				PKT_DATA, s, len, PKT_END);
		} else {
		    /*
		     * The server has _both_
		     * BUG_CHOKES_ON_SSH1_IGNORE and
		     * BUG_NEEDS_SSH1_PLAIN_PASSWORD. There is
		     * therefore nothing we can do.
		     */
		    int len;
		    len = strlen(password);
		    logevent(session, "Sending unpadded password");
		    send_packet(session, pwpkt_type, PKT_INT, len,
				PKT_DATA, password, len, PKT_END);
		}
	    } else {
			send_packet(session, pwpkt_type, PKT_STR, password, PKT_END);
	    }
	}
	logevent(session, "Sent password");
	memset(password, 0, strlen(password));
	crWaitUntil(ispkt);
	if (session.pktin.type == SSH1_SMSG_FAILURE) {
	    if (flags & FLAG_VERBOSE)
			c_write_str("Access denied\r\n");
	    logevent(session, "Authentication refused");
	} else if (session.pktin.type == SSH1_MSG_DISCONNECT) {
	    logevent(session, "Received disconnect request");
	    session.ssh_state = SSH_STATE_CLOSED;
	    crReturn(1);
	} else if (session.pktin.type != SSH1_SMSG_SUCCESS) {
	    bombout((session, "Strange packet received, type %d", session.pktin.type));
	    crReturn(0);
	}
    }

    logevent(session, "Authentication successful");

    crFinish(1);
}

void sshfwd_close(CSshSession &session, struct ssh_channel *c)
{
    if (c && !c->closes) {
	/*
	 * If the channel's remoteid is -1, we have sent
	 * CHANNEL_OPEN for this channel, but it hasn't even been
	 * acknowledged by the server. So we must set a close flag
	 * on it now, and then when the server acks the channel
	 * open, we can close it then.
	 */
	if (((int)c->remoteid) != -1) {
	    if (ssh_version == 1) {
			send_packet(session, SSH1_MSG_CHANNEL_CLOSE, PKT_INT, c->remoteid,
				    PKT_END);
	    } else {
			ssh2_pkt_init(session, SSH2_MSG_CHANNEL_CLOSE);
			ssh2_pkt_adduint32(session,c->remoteid);
			ssh2_pkt_send(session);
	    }
	}
	c->closes = 1;
	if (c->type == CHAN_X11) {
	    c->u.x11.s = NULL;
	    logevent(session, "Forwarded X11 connection terminated");
	} else if (c->type == CHAN_SOCKDATA ||
		   c->type == CHAN_SOCKDATA_DORMANT) {
	    c->u.pfd.s = NULL;
	    logevent(session, "Forwarded port closed");
	}
    }
}

int sshfwd_write(CSshSession &session, struct ssh_channel *c, char *buf, int len)
{
    if (ssh_version == 1) {
		send_packet(session, SSH1_MSG_CHANNEL_DATA,
				PKT_INT, c->remoteid,
				PKT_INT, len, PKT_DATA, buf, len, PKT_END);
		/*
		 * In SSH1 we can return 0 here - implying that forwarded
		 * connections are never individually throttled - because
		 * the only circumstance that can cause throttling will be
		 * the whole SSH connection backing up, in which case
		 * _everything_ will be throttled as a whole.
		 */
		return 0;
    } else {
		ssh2_add_channel_data(c, buf, len);
		return ssh2_try_send(session, c);
    }
}

void sshfwd_unthrottle(CSshSession &session, struct ssh_channel *c, int bufsize)
{
    if (ssh_version == 1) {
		if (c->v.v1.throttling && bufsize < SSH1_BUFFER_LIMIT) {
			c->v.v1.throttling = 0;
			ssh1_throttle(session,-1);
		}
    } else {
		ssh2_set_window(session, c, OUR_V2_WINSIZE - bufsize);
    }
}

static void ssh1_protocol(CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine)
{
    crBegin;

    random_init();

    while (!do_ssh1_login(session, in, inlen, ispkt, session.n_do_ssh1_login)) {
		crReturnV;
    }
    if (session.ssh_state == SSH_STATE_CLOSED)
		crReturnV;

    if (cfg.agentfwd && agent_exists()) {
	logevent(session, "Requesting agent forwarding");
	send_packet(session, SSH1_CMSG_AGENT_REQUEST_FORWARDING, PKT_END);
	do {
	    crReturnV;
	} while (!ispkt);
	if (session.pktin.type != SSH1_SMSG_SUCCESS
	    && session.pktin.type != SSH1_SMSG_FAILURE) {
	    bombout((session, "Protocol confusion"));
	    crReturnV;
	} else if (session.pktin.type == SSH1_SMSG_FAILURE) {
	    logevent(session, "Agent forwarding refused");
	} else {
	    logevent(session, "Agent forwarding enabled");
		session.ssh_agentfwd_enabled = TRUE;
	}
    }

    if (cfg.x11_forward) {
	char proto[20], data[64];
	logevent(session, "Requesting X11 forwarding");
	x11_invent_auth(proto, sizeof(proto), data, sizeof(data));
	if (session.ssh1_local_protoflags & SSH1_PROTOFLAG_SCREEN_NUMBER) {
	    send_packet(session, SSH1_CMSG_X11_REQUEST_FORWARDING,
			PKT_STR, proto, PKT_STR, data,
			PKT_INT, 0, PKT_END);
	} else {
	    send_packet(session, SSH1_CMSG_X11_REQUEST_FORWARDING,
			PKT_STR, proto, PKT_STR, data, PKT_END);
	}
	do {
	    crReturnV;
	} while (!ispkt);
	if (session.pktin.type != SSH1_SMSG_SUCCESS
	    && session.pktin.type != SSH1_SMSG_FAILURE) {
	    bombout((session, "Protocol confusion"));
	    crReturnV;
	} else if (session.pktin.type == SSH1_SMSG_FAILURE) {
	    logevent(session, "X11 forwarding refused");
	} else {
	    logevent(session, "X11 forwarding enabled");
	    session.ssh_X11_fwd_enabled = TRUE;
	}
    }

    {
	char type;
	static char *e;
	int n;
	int sport,dport,sserv,dserv;
	char sports[256], dports[256], host[256];
	char buf[1024];
	struct servent *se;

	session.ssh_rportfwds = newtree234(ssh_rportcmp_ssh1);

	/* Add port forwardings. */
	e = cfg.portfwd;
	while (*e) {
	    type = *e++;
	    n = 0;
	    while (*e && *e != '\t')
		sports[n++] = *e++;
	    sports[n] = 0;
	    if (*e == '\t')
		e++;
	    n = 0;
	    while (*e && *e != ':')
		host[n++] = *e++;
	    host[n] = 0;
	    if (*e == ':')
		e++;
	    n = 0;
	    while (*e)
		dports[n++] = *e++;
	    dports[n] = 0;
	    e++;
	    dport = atoi(dports);
	    dserv = 0;
	    if (dport == 0) {
		dserv = 1;
		se = getservbyname(dports, NULL);
		if (se != NULL) {
		    dport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for destination port \"%s\"",
			    dports);
		    logevent(session, buf);
		}
	    }
	    sport = atoi(sports);
	    sserv = 0;
	    if (sport == 0) {
		sserv = 1;
		se = getservbyname(sports, NULL);
		if (se != NULL) {
		    sport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for source port \"%s\"",
			    sports);
		    logevent(session, buf);
		}
	    }
	    if (sport && dport) {
		if (type == 'L') {
		    pfd_addforward(session, host, dport, sport);
		    sprintf(buf, "Local port %.*s%.*s%d%.*s forwarding to"
			    " %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
		    logevent(session, buf);
		} else {
		    struct ssh_rportfwd *pf;
		    pf = (struct ssh_rportfwd *)smalloc(sizeof(*pf));
		    strcpy(pf->dhost, host);
		    pf->dport = dport;
		    if (add234(session.ssh_rportfwds, pf) != pf) {
			sprintf(buf, 
				"Duplicate remote port forwarding to %s:%d",
				host, dport);
			logevent(session, buf);
			sfree(pf);
		    } else {
			sprintf(buf, "Requesting remote port %.*s%.*s%d%.*s"
				" forward to %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
			logevent(session, buf);
			send_packet(session, SSH1_CMSG_PORT_FORWARD_REQUEST,
				    PKT_INT, sport,
				    PKT_STR, host,
				    PKT_INT, dport,
				    PKT_END);
			do {
			    crReturnV;
			} while (!ispkt);
			if (session.pktin.type != SSH1_SMSG_SUCCESS
			    && session.pktin.type != SSH1_SMSG_FAILURE) {
			    bombout((session, "Protocol confusion"));
			    crReturnV;
			} else if (session.pktin.type == SSH1_SMSG_FAILURE) {
			    c_write_str("Server refused port forwarding\r\n");
			    session.ssh_editing = session.ssh_echoing = 1;
			}
			logevent(session, "Remote port forwarding enabled");
		    }
		}
	    }
	}
    }

    if (!cfg.nopty) {
		send_packet(session, SSH1_CMSG_REQUEST_PTY,
				PKT_STR, cfg.termtype,
				PKT_INT, rows, PKT_INT, cols,
				PKT_INT, 0, PKT_INT, 0, PKT_CHAR, 0, PKT_END);
		session.ssh_state = SSH_STATE_INTERMED;
		do {
			crReturnV;
		} while (!ispkt);
		if (session.pktin.type != SSH1_SMSG_SUCCESS
			&& session.pktin.type != SSH1_SMSG_FAILURE) {
			bombout((session, "Protocol confusion"));
			crReturnV;
		} else if (session.pktin.type == SSH1_SMSG_FAILURE) {
			c_write_str("Server refused to allocate pty\r\n");
			session.ssh_editing = session.ssh_echoing = 1;
		}
		logevent(session, "Allocated pty");
    } else {
		session.ssh_editing = session.ssh_echoing = 1;
    }

    if (cfg.compression) {
		send_packet(session, SSH1_CMSG_REQUEST_COMPRESSION, PKT_INT, 6, PKT_END);
		do {
			crReturnV;
		} while (!ispkt);
		if (session.pktin.type != SSH1_SMSG_SUCCESS
			&& session.pktin.type != SSH1_SMSG_FAILURE) {
			bombout((session, "Protocol confusion"));
			crReturnV;
		} else if (session.pktin.type == SSH1_SMSG_FAILURE) {
			c_write_str("Server refused to compress\r\n");
		}
		logevent(session, "Started compression");
		session.ssh1_compressing = TRUE;
		zlib_compress_init(session);
		zlib_decompress_init(session);
    }

    /*
     * Start the shell or command.
     * 
     * Special case: if the first-choice command is an SSH2
     * subsystem (hence not usable here) and the second choice
     * exists, we fall straight back to that.
     */
    {
		char *cmd = cfg.remote_cmd_ptr;
		
		if (cfg.ssh_subsys && cfg.remote_cmd_ptr2) {
			cmd = cfg.remote_cmd_ptr2;
			ssh_fallback_cmd = TRUE;
		}
		if (*cmd)
			send_packet(session, SSH1_CMSG_EXEC_CMD, PKT_STR, cmd, PKT_END);
		else
			send_packet(session, SSH1_CMSG_EXEC_SHELL, PKT_END);
		logevent(session, "Started session");
    }

    session.ssh_state = SSH_STATE_SESSION;
    if (session.size_needed)
		ssh_size(session);
    if (session.eof_needed)
		ssh_special(session, TS_EOF);

    ldisc_send(NULL, 0, 0);	       /* cause ldisc to notice changes */
    session.ssh_send_ok = 1;
    session.ssh_channels = newtree234(ssh_channelcmp);
    while (1) {
	crReturnV;
	if (ispkt) {
	    if (session.pktin.type == SSH1_SMSG_STDOUT_DATA ||
			session.pktin.type == SSH1_SMSG_STDERR_DATA) {
			long len = GET_32BIT(session.pktin.body);
			int bufsize =
				from_backend(session.pktin.type == SSH1_SMSG_STDERR_DATA,
					 (char *)session.pktin.body + 4, len);
			if (!ssh1_stdout_throttling && bufsize > SSH1_BUFFER_LIMIT) {
				ssh1_stdout_throttling = 1;
				ssh1_throttle(session, +1);
			}
	    } else if (session.pktin.type == SSH1_MSG_DISCONNECT) {
			session.ssh_state = SSH_STATE_CLOSED;
			logevent(session, "Received disconnect request");
			crReturnV;
	    } else if (session.pktin.type == SSH1_SMSG_X11_OPEN) {
		/* Remote side is trying to open a channel to talk to our
		 * X-Server. Give them back a local channel number. */
		struct ssh_channel *c;

		logevent(session, "Received X11 connect request");
		/* Refuse if X11 forwarding is disabled. */
		if (!session.ssh_X11_fwd_enabled) {
		    send_packet(session, SSH1_MSG_CHANNEL_OPEN_FAILURE,
				PKT_INT, GET_32BIT(session.pktin.body), PKT_END);
		    logevent(session, "Rejected X11 connect request");
		} else {
		    c = (struct ssh_channel *)smalloc(sizeof(struct ssh_channel));

		    if (x11_init(session, &c->u.x11.s, cfg.x11_display, c) != NULL) {
				logevent(session, "opening X11 forward connection failed");
				sfree(c);
				send_packet(session, SSH1_MSG_CHANNEL_OPEN_FAILURE,
						PKT_INT, GET_32BIT(session.pktin.body),
						PKT_END);
		    } else {
			logevent(session, "opening X11 forward connection succeeded");
			c->remoteid = GET_32BIT(session.pktin.body);
			c->localid = alloc_channel_id(session);
			c->closes = 0;
			c->v.v1.throttling = 0;
			c->type = CHAN_X11;	/* identify channel type */
			add234(session.ssh_channels, c);
			send_packet(session, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
				    PKT_INT, c->remoteid, PKT_INT,
				    c->localid, PKT_END);
			logevent(session, "Opened X11 forward channel");
		    }
		}
	    } else if (session.pktin.type == SSH1_SMSG_AGENT_OPEN) {
		/* Remote side is trying to open a channel to talk to our
		 * agent. Give them back a local channel number. */
		struct ssh_channel *c;

		/* Refuse if agent forwarding is disabled. */
		if (!session.ssh_agentfwd_enabled) {
		    send_packet(session, SSH1_MSG_CHANNEL_OPEN_FAILURE,
				PKT_INT, GET_32BIT(session.pktin.body), PKT_END);
		} else {
		    c = (struct ssh_channel *)smalloc(sizeof(struct ssh_channel));
		    c->remoteid = GET_32BIT(session.pktin.body);
		    c->localid = alloc_channel_id(session);
		    c->closes = 0;
		    c->v.v1.throttling = 0;
		    c->type = CHAN_AGENT;	/* identify channel type */
		    c->u.a.lensofar = 0;
		    add234(session.ssh_channels, c);
		    send_packet(session, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
				PKT_INT, c->remoteid, PKT_INT, c->localid,
				PKT_END);
		}
	    } else if (session.pktin.type == SSH1_MSG_PORT_OPEN) {
   		/* Remote side is trying to open a channel to talk to a
		 * forwarded port. Give them back a local channel number. */
		struct ssh_channel *c;
		struct ssh_rportfwd pf;
		int hostsize, port;
		char host[256], buf[1024];
		char *p, *h, *e;
		c = (struct ssh_channel *)smalloc(sizeof(struct ssh_channel));

		hostsize = GET_32BIT(session.pktin.body+4);
		for(h = host, p = (char *)session.pktin.body+8; hostsize != 0; hostsize--) {
		    if (h+1 < host+sizeof(host))
			*h++ = *p;
		    p++;
		}
		*h = 0;
		port = GET_32BIT(p);

		strcpy(pf.dhost, host);
		pf.dport = port;

		if (find234(session.ssh_rportfwds, &pf, NULL) == NULL) {
		    sprintf(buf, "Rejected remote port open request for %s:%d",
			    host, port);
		    logevent(session, buf);
            send_packet(session, SSH1_MSG_CHANNEL_OPEN_FAILURE,
                            PKT_INT, GET_32BIT(session.pktin.body), PKT_END);
		} else {
		    sprintf(buf, "Received remote port open request for %s:%d",
			    host, port);
		    logevent(session, buf);
		    e = pfd_newconnect(session, &c->u.pfd.s, host, port, c);
		    if (e != NULL) {
			char buf[256];
			sprintf(buf, "Port open failed: %s", e);
			logevent(session, buf);
			sfree(c);
			send_packet(session, SSH1_MSG_CHANNEL_OPEN_FAILURE,
				    PKT_INT, GET_32BIT(session.pktin.body),
				    PKT_END);
		    } else {
			c->remoteid = GET_32BIT(session.pktin.body);
			c->localid = alloc_channel_id(session);
			c->closes = 0;
			c->v.v1.throttling = 0;
			c->type = CHAN_SOCKDATA;	/* identify channel type */
			add234(session.ssh_channels, c);
			send_packet(session, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
				    PKT_INT, c->remoteid, PKT_INT,
				    c->localid, PKT_END);
			logevent(session, "Forwarded port opened successfully");
		    }
		}

	    } else if (session.pktin.type == SSH1_MSG_CHANNEL_OPEN_CONFIRMATION) {
			unsigned int remoteid = GET_32BIT(session.pktin.body);
			unsigned int localid = GET_32BIT(session.pktin.body+4);
			struct ssh_channel *c;

			c = (struct ssh_channel *)find234(session.ssh_channels, &remoteid, ssh_channelfind);
			if (c && c->type == CHAN_SOCKDATA_DORMANT) {
				c->remoteid = localid;
				c->type = CHAN_SOCKDATA;
				c->v.v1.throttling = 0;
				pfd_confirm(session, c->u.pfd.s);
			}

		if (c && c->closes) {
		    /*
		     * We have a pending close on this channel,
		     * which we decided on before the server acked
		     * the channel open. So now we know the
		     * remoteid, we can close it again.
		     */
		    send_packet(session, SSH1_MSG_CHANNEL_CLOSE, PKT_INT, c->remoteid,
				PKT_END);
		}

	    } else if (session.pktin.type == SSH1_MSG_CHANNEL_OPEN_FAILURE) {
		unsigned int remoteid = GET_32BIT(session.pktin.body);
		unsigned int localid = GET_32BIT(session.pktin.body+4);
		struct ssh_channel *c;

		c = (struct ssh_channel *)find234(session.ssh_channels, &remoteid, ssh_channelfind);
		if (c && c->type == CHAN_SOCKDATA_DORMANT) {
		    logevent(session, "Forwarded connection refused by server");
		    pfd_close(session, c->u.pfd.s);
		    del234(session.ssh_channels, c);
		    sfree(c);
		}

	    } else if (session.pktin.type == SSH1_MSG_CHANNEL_CLOSE ||
		       session.pktin.type == SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION) {
		/* Remote side closes a channel. */
		unsigned i = GET_32BIT(session.pktin.body);
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (c) {
		    int closetype;
		    closetype =
			(session.pktin.type == SSH1_MSG_CHANNEL_CLOSE ? 1 : 2);
		    if (!(c->closes & closetype))
			send_packet(session, session.pktin.type, PKT_INT, c->remoteid,
				    PKT_END);
		    if ((c->closes == 0) && (c->type == CHAN_X11)) {
			logevent(session, "Forwarded X11 connection terminated");
			assert(c->u.x11.s != NULL);
			x11_close(session, c->u.x11.s);
			c->u.x11.s = NULL;
		    }
		    if ((c->closes == 0) && (c->type == CHAN_SOCKDATA)) {
			logevent(session, "Forwarded port closed");
			assert(c->u.pfd.s != NULL);
			pfd_close(session, c->u.pfd.s);
			c->u.pfd.s = NULL;
		    }
		    c->closes |= closetype;
		    if (c->closes == 3) {
			del234(session.ssh_channels, c);
			sfree(c);
		    }
		}
	    } else if (session.pktin.type == SSH1_MSG_CHANNEL_DATA) {
		/* Data sent down one of our channels. */
		int i = GET_32BIT(session.pktin.body);
		int len = GET_32BIT(session.pktin.body + 4);
		unsigned char *p = session.pktin.body + 8;
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (c) {
		    int bufsize;
		    switch (c->type) {
		      case CHAN_X11:
			bufsize = x11_send(session, c->u.x11.s, (char *)p, len);
			break;
		      case CHAN_SOCKDATA:
			bufsize = pfd_send(session, c->u.pfd.s, (char *)p, len);
			break;
		      case CHAN_AGENT:
			/* Data for an agent message. Buffer it. */
			while (len > 0) {
			    if (c->u.a.lensofar < 4) {
				int l = min(4 - c->u.a.lensofar, len);
				memcpy(c->u.a.msglen + c->u.a.lensofar, p,
				       l);
				p += l;
				len -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == 4) {
				c->u.a.totallen =
				    4 + GET_32BIT(c->u.a.msglen);
				c->u.a.message = (unsigned char *)smalloc(c->u.a.totallen);
				memcpy(c->u.a.message, c->u.a.msglen, 4);
			    }
			    if (c->u.a.lensofar >= 4 && len > 0) {
				int l =
				    min(c->u.a.totallen - c->u.a.lensofar,
					len);
				memcpy(c->u.a.message + c->u.a.lensofar, p,
				       l);
				p += l;
				len -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == c->u.a.totallen) {
				void *reply, *sentreply;
				int replylen;
				agent_query(c->u.a.message,
					    c->u.a.totallen, &reply,
					    &replylen);
				if (reply)
				    sentreply = reply;
				else {
				    /* Fake SSH_AGENT_FAILURE. */
				    sentreply = "\0\0\0\1\5";
				    replylen = 5;
				}
				send_packet(session, SSH1_MSG_CHANNEL_DATA,
					    PKT_INT, c->remoteid,
					    PKT_INT, replylen,
					    PKT_DATA, sentreply, replylen,
					    PKT_END);
				if (reply)
				    sfree(reply);
				sfree(c->u.a.message);
				c->u.a.lensofar = 0;
			    }
			}
			bufsize = 0;   /* agent channels never back up */
			break;
		    }
		    if (!c->v.v1.throttling && bufsize > SSH1_BUFFER_LIMIT) {
				c->v.v1.throttling = 1;
				ssh1_throttle(session,+1);
		    }
		}
	    } else if (session.pktin.type == SSH1_SMSG_SUCCESS) {
		/* may be from EXEC_SHELL on some servers */
	    } else if (session.pktin.type == SSH1_SMSG_FAILURE) {
		/* may be from EXEC_SHELL on some servers
		 * if no pty is available or in other odd cases. Ignore */
	    } else if (session.pktin.type == SSH1_SMSG_EXIT_STATUS) {
		char buf[100];
		session.ssh_exitcode = GET_32BIT(session.pktin.body);
		sprintf(buf, "Server sent command exit status %d",
			session.ssh_exitcode);
		logevent(session, buf);
		send_packet(session, SSH1_CMSG_EXIT_CONFIRMATION, PKT_END);
                /*
                 * In case `helpful' firewalls or proxies tack
                 * extra human-readable text on the end of the
                 * session which we might mistake for another
                 * encrypted packet, we close the session once
                 * we've sent EXIT_CONFIRMATION.
                 */
                session.ssh_state = SSH_STATE_CLOSED;
                crReturnV;
	    } else {
			bombout((session, "Strange packet received: type %d", session.pktin.type));
			crReturnV;
	    }
	} else {
	    while (inlen > 0) {
		int len = min(inlen, 512);
		send_packet(session, SSH1_CMSG_STDIN_DATA,
			    PKT_INT, len, PKT_DATA, in, len, PKT_END);
		in += len;
		inlen -= len;
	    }
	}
    }

    crFinishV;
}

/*
 * Utility routine for decoding comma-separated strings in KEXINIT.
 */
static int in_commasep_string(char *needle, char *haystack, int haylen)
{
    int needlen = strlen(needle);
    while (1) {
	/*
	 * Is it at the start of the string?
	 */
	if (haylen >= needlen &&       /* haystack is long enough */
	    !memcmp(needle, haystack, needlen) &&	/* initial match */
	    (haylen == needlen || haystack[needlen] == ',')
	    /* either , or EOS follows */
	    )
	    return 1;
	/*
	 * If not, search for the next comma and resume after that.
	 * If no comma found, terminate.
	 */
	while (haylen > 0 && *haystack != ',')
	    haylen--, haystack++;
	if (haylen == 0)
	    return 0;
	haylen--, haystack++;	       /* skip over comma itself */
    }
}

/*
 * SSH2 key creation method.
 */
static void ssh2_mkkey(Bignum K, char *H, char *sessid, char chr,
		       char *keyspace)
{
    SHA_State s;
    /* First 20 bytes. */
    SHA_Init(&s);
    sha_mpint(&s, K);
    SHA_Bytes(&s, H, 20);
    SHA_Bytes(&s, &chr, 1);
    SHA_Bytes(&s, sessid, 20);
    SHA_Final(&s, (unsigned char *)keyspace);
    /* Next 20 bytes. */
    SHA_Init(&s);
    sha_mpint(&s, K);
    SHA_Bytes(&s, H, 20);
    SHA_Bytes(&s, keyspace, 20);
    SHA_Final(&s, (unsigned char *)keyspace + 20);
}

/*
 * Handle the SSH2 transport layer.
 */
static int do_ssh2_transport(CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine)
{
	int i, j;
	char *keystr, *fingerprint;

    crBegin;
    random_init();
    session.first_kex = 1;

    /*
     * Set up the preferred ciphers. (NULL => warn below here)
     */
    session.n_preferred_ciphers = 0;
    for (i = 0; i < CIPHER_MAX; i++) {
	switch (cfg.ssh_cipherlist[i]) {
	  case CIPHER_BLOWFISH:
	    session.preferred_ciphers[session.n_preferred_ciphers] = &ssh2_blowfish;
	    session.n_preferred_ciphers++;
	    break;
	  case CIPHER_DES:
	    if (cfg.ssh2_des_cbc) {
			session.preferred_ciphers[session.n_preferred_ciphers] = &ssh2_des;
			session.n_preferred_ciphers++;
	    }
	    break;
	  case CIPHER_3DES:
	    session.preferred_ciphers[session.n_preferred_ciphers] = &ssh2_3des;
	    session.n_preferred_ciphers++;
	    break;
	  case CIPHER_AES:
	    session.preferred_ciphers[session.n_preferred_ciphers] = &ssh2_aes;
	    session.n_preferred_ciphers++;
	    break;
	  case CIPHER_WARN:
	    /* Flag for later. Don't bother if it's the last in
	     * the list. */
	    if (i < CIPHER_MAX - 1) {
			session.preferred_ciphers[session.n_preferred_ciphers] = NULL;
			session.n_preferred_ciphers++;
	    }
	    break;
	}
    }

    /*
     * Set up preferred compression.
     */
    if (cfg.compression)
		session.preferred_comp = &ssh_zlib;
    else
		session.preferred_comp = &ssh_comp_none;

    /*
     * Be prepared to work around the buggy MAC problem.
     */
    if (cfg.buggymac || (session.ssh_remote_bugs & BUG_SSH2_HMAC))
		session.maclist = buggymacs, session.nmacs = lenof(buggymacs);
    else
		session.maclist = macs, session.nmacs = lenof(macs);

  begin_key_exchange:
    /*
     * Construct and send our key exchange packet.
     */
    ssh2_pkt_init(session, SSH2_MSG_KEXINIT);
    for (i = 0; i < 16; i++)
	ssh2_pkt_addbyte(session, (unsigned char) random_byte());
    /* List key exchange algorithms. */
    ssh2_pkt_addstring_start(session);
    for (i = 0; i < lenof(kex_algs); i++) {
	ssh2_pkt_addstring_str(session, kex_algs[i]->name);
	if (i < lenof(kex_algs) - 1)
	    ssh2_pkt_addstring_str(session, ",");
    }
    /* List server host key algorithms. */
    ssh2_pkt_addstring_start(session);
    for (i = 0; i < lenof(hostkey_algs); i++) {
	ssh2_pkt_addstring_str(session, hostkey_algs[i]->name);
	if (i < lenof(hostkey_algs) - 1)
	    ssh2_pkt_addstring_str(session, ",");
    }
    /* List client->server encryption algorithms. */
    ssh2_pkt_addstring_start(session);
    session.cipherstr_started = 0;
    for (i = 0; i < session.n_preferred_ciphers; i++) {
		const struct ssh2_ciphers *c = session.preferred_ciphers[i];
		if (!c) continue;	       /* warning flag */
		for (j = 0; j < c->nciphers; j++) {
			if (session.cipherstr_started)
				ssh2_pkt_addstring_str(session, ",");
			ssh2_pkt_addstring_str(session, c->list[j]->name);
			session.cipherstr_started = 1;
		}
    }
    /* List server->client encryption algorithms. */
    ssh2_pkt_addstring_start(session);
    session.cipherstr_started = 0;
    for (i = 0; i < session.n_preferred_ciphers; i++) {
		const struct ssh2_ciphers *c = session.preferred_ciphers[i];
		if (!c) continue; /* warning flag */
		for (j = 0; j < c->nciphers; j++) {
			if (session.cipherstr_started)
				ssh2_pkt_addstring_str(session, ",");
			ssh2_pkt_addstring_str(session, c->list[j]->name);
			session.cipherstr_started = 1;
		}
    }
    /* List client->server MAC algorithms. */
    ssh2_pkt_addstring_start(session);
    for (i = 0; i < session.nmacs; i++) {
		ssh2_pkt_addstring_str(session, session.maclist[i]->name);
		if (i < session.nmacs - 1)
			ssh2_pkt_addstring_str(session, ",");
    }
    /* List server->client MAC algorithms. */
    ssh2_pkt_addstring_start(session);
    for (i = 0; i < session.nmacs; i++) {
		ssh2_pkt_addstring_str(session, session.maclist[i]->name);
		if (i < session.nmacs - 1)
			ssh2_pkt_addstring_str(session, ",");
    }
    /* List client->server compression algorithms. */
    ssh2_pkt_addstring_start(session);
    for (i = 0; i < lenof(compressions) + 1; i++) {
		const struct ssh_compress *c =
			i == 0 ? session.preferred_comp : compressions[i - 1];
		ssh2_pkt_addstring_str(session, c->name);
		if (i < lenof(compressions))
			ssh2_pkt_addstring_str(session, ",");
    }
    /* List server->client compression algorithms. */
    ssh2_pkt_addstring_start(session);
    for (i = 0; i < lenof(compressions) + 1; i++) {
		const struct ssh_compress *c =
			i == 0 ? session.preferred_comp : compressions[i - 1];
		ssh2_pkt_addstring_str(session, c->name);
		if (i < lenof(compressions))
			ssh2_pkt_addstring_str(session, ",");
    }
    /* List client->server languages. Empty list. */
    ssh2_pkt_addstring_start(session);
    /* List server->client languages. Empty list. */
    ssh2_pkt_addstring_start(session);
    /* First KEX packet does _not_ follow, because we're not that brave. */
    ssh2_pkt_addbool(session, FALSE);
    /* Reserved. */
    ssh2_pkt_adduint32(session,0);

    session.exhash = session.exhashbase;
    sha_string(&session.exhash, session.pktout.data + 5, session.pktout.length - 5);

    ssh2_pkt_send(session);

    if (!ispkt)
	crWaitUntil(ispkt);
    sha_string(&session.exhash, session.pktin.data + 5, session.pktin.length - 5);

    /*
     * Now examine the other side's KEXINIT to see what we're up
     * to.
     */
    if (session.pktin.type != SSH2_MSG_KEXINIT) {
		bombout((session, "expected key exchange packet from server"));
		crReturn(0);
    }
    session.kex = NULL;
    session.hostkey = NULL;
    session.cscipher_tobe = NULL;
    session.sccipher_tobe = NULL;
    session.csmac_tobe = NULL;
    session.scmac_tobe = NULL;
    session.cscomp_tobe = NULL;
    session.sccomp_tobe = NULL;
    session.pktin.savedpos += 16;	       /* skip garbage cookie */
    ssh2_pkt_getstring(session, &session.str, &session.len);    /* key exchange algorithms */
    for (i = 0; i < lenof(kex_algs); i++) {
		if (in_commasep_string(kex_algs[i]->name, session.str, session.len)) {
			session.kex = kex_algs[i];
			break;
		}
    }
    ssh2_pkt_getstring(session, &session.str, &session.len);    /* host key algorithms */
    for (i = 0; i < lenof(hostkey_algs); i++) {
	if (in_commasep_string(hostkey_algs[i]->name, session.str, session.len)) {
	    session.hostkey = hostkey_algs[i];
	    break;
	}
    }
    ssh2_pkt_getstring(session, &session.str, &session.len);    /* client->server cipher */
    session.warn = 0;
    for (i = 0; i < session.n_preferred_ciphers; i++) {
		const struct ssh2_ciphers *c = session.preferred_ciphers[i];
		if (!c) {
			session.warn = 1;
		} else {
			for (j = 0; j < c->nciphers; j++) {
				if (in_commasep_string(c->list[j]->name, session.str, session.len)) {
					session.cscipher_tobe = c->list[j];
					break;
				}
			}
		}
		if (session.cscipher_tobe) {
			if (session.warn)
				askcipher(session.cscipher_tobe->name, 1);
			break;
		}
    }
    if (!session.cscipher_tobe) {
		bombout((session, "Couldn't agree a client-to-server cipher (available: %s)", session.str));
		crReturn(0);
    }

    ssh2_pkt_getstring(session, &session.str, &session.len);    /* server->client cipher */
    session.warn = 0;
    for (i = 0; i < session.n_preferred_ciphers; i++) {
		const struct ssh2_ciphers *c = session.preferred_ciphers[i];
		if (!c) {
			session.warn = 1;
		} else {
			for (j = 0; j < c->nciphers; j++) {
				if (in_commasep_string(c->list[j]->name, session.str, session.len)) {
					session.sccipher_tobe = c->list[j];
					break;
				}
			}
		}
		if (session.sccipher_tobe) {
			if (session.warn)
				askcipher(session.sccipher_tobe->name, 2);
			break;
		}
    }
    if (!session.sccipher_tobe) {
		bombout((session, "Couldn't agree a server-to-client cipher (available: %s)", session.str));
		crReturn(0);
    }

    ssh2_pkt_getstring(session, &session.str, &session.len);    /* client->server mac */
    for (i = 0; i < session.nmacs; i++) {
		if (in_commasep_string(session.maclist[i]->name, session.str, session.len)) {
			session.csmac_tobe = session.maclist[i];
			break;
		}
    }
    ssh2_pkt_getstring(session, &session.str, &session.len);    /* server->client mac */
    for (i = 0; i < session.nmacs; i++) {
		if (in_commasep_string(session.maclist[i]->name, session.str, session.len)) {
			session.scmac_tobe = session.maclist[i];
			break;
		}
    }
    ssh2_pkt_getstring(session, &session.str, &session.len);    /* client->server compression */
    for (i = 0; i < lenof(compressions) + 1; i++) {
		const struct ssh_compress *c =
			i == 0 ? session.preferred_comp : compressions[i - 1];
		if (in_commasep_string(c->name, session.str, session.len)) {
			session.cscomp_tobe = c;
			break;
		}
    }
    ssh2_pkt_getstring(session, &session.str, &session.len);    /* server->client compression */
    for (i = 0; i < lenof(compressions) + 1; i++) {
		const struct ssh_compress *c =
			i == 0 ? session.preferred_comp : compressions[i - 1];
		if (in_commasep_string(c->name, session.str, session.len)) {
			session.sccomp_tobe = c;
			break;
		}
    }

    /*
     * Work out the number of bits of key we will need from the key
     * exchange. We start with the maximum key length of either
     * cipher...
     */
    {
		int csbits, scbits;

		csbits = session.cscipher_tobe->keylen;
		scbits = session.sccipher_tobe->keylen;
		session.nbits = (csbits > scbits ? csbits : scbits);
    }

    /* The keys only have 160-bit entropy, since they're based on
     * a SHA-1 hash. So cap the key size at 160 bits. */
    if (session.nbits > 160)
		session.nbits = 160;

    /*
     * If we're doing Diffie-Hellman group exchange, start by
     * requesting a group.
     */
    if (session.kex == &ssh_diffiehellman_gex) {
		logevent(session, "Doing Diffie-Hellman group exchange");
		ssh_pkt_ctx |= SSH2_PKTCTX_DHGEX;
		/*
		 * Work out how big a DH group we will need to allow that
		 * much data.
		 */
		session.pbits = 512 << ((session.nbits - 1) / 64);
		ssh2_pkt_init(session, SSH2_MSG_KEX_DH_GEX_REQUEST);
		ssh2_pkt_adduint32(session, session.pbits);
		ssh2_pkt_send(session);

		crWaitUntil(ispkt);
		if (session.pktin.type != SSH2_MSG_KEX_DH_GEX_GROUP) {
			bombout((session, "expected key exchange group packet from server"));
			crReturn(0);
		}
		session.p = ssh2_pkt_getmp(session);
		session.g = ssh2_pkt_getmp(session);
		dh_setup_group(session.p, session.g);
		session.kex_init_value = SSH2_MSG_KEX_DH_GEX_INIT;
		session.kex_reply_value = SSH2_MSG_KEX_DH_GEX_REPLY;
	} else {
		ssh_pkt_ctx |= SSH2_PKTCTX_DHGROUP1;
		dh_setup_group1();
		session.kex_init_value = SSH2_MSG_KEXDH_INIT;
		session.kex_reply_value = SSH2_MSG_KEXDH_REPLY;
    }

    logevent(session, "Doing Diffie-Hellman key exchange");
    /*
     * Now generate and send e for Diffie-Hellman.
     */
    session.e = dh_create_e(session.nbits * 2);
    ssh2_pkt_init(session, session.kex_init_value);
    ssh2_pkt_addmp(session, session.e);
    ssh2_pkt_send(session);

    crWaitUntil(ispkt);
    if (session.pktin.type != session.kex_reply_value) {
		bombout((session, "expected key exchange reply packet from server"));
		crReturn(0);
    }
    ssh2_pkt_getstring(session, &session.hostkeydata, &session.hostkeylen);
    session.f = ssh2_pkt_getmp(session);
    ssh2_pkt_getstring(session, &session.sigdata, &session.siglen);

    session.K = dh_find_K(session.f);

    sha_string(&session.exhash, session.hostkeydata, session.hostkeylen);
    if (session.kex == &ssh_diffiehellman_gex) {
		sha_uint32(&session.exhash, session.pbits);
		sha_mpint(&session.exhash, session.p);
		sha_mpint(&session.exhash, session.g);
    }
    sha_mpint(&session.exhash, session.e);
    sha_mpint(&session.exhash, session.f);
    sha_mpint(&session.exhash, session.K);
    SHA_Final(&session.exhash, session.exchange_hash);

    dh_cleanup();

#if 0
    debug(("Exchange hash is:\n"));
    dmemdump(exchange_hash, 20);
#endif

    session.hkey = session.hostkey->newkey(session.hostkeydata, session.hostkeylen);
    if (!session.hkey ||
		!session.hostkey->verifysig(session.hkey, session.sigdata, session.siglen, (char *)session.exchange_hash, 20)) {
		bombout((session, "Server's host key did not match the signature supplied"));
		crReturn(0);
    }

    /*
     * Authenticate remote host: verify host key. (We've already
     * checked the signature of the exchange hash.)
     */
    keystr = session.hostkey->fmtkey(session.hkey);
    fingerprint = session.hostkey->fingerprint(session.hkey);
    verify_ssh_host_key(session, session.savedhost, session.savedport, session.hostkey->keytype,
			keystr, fingerprint);
    if (session.first_kex) {		       /* don't bother logging this in rekeys */
		logevent(session, "Host key fingerprint is:");
		logevent(session, fingerprint);
    }
    sfree(fingerprint);
    sfree(keystr);
    session.hostkey->freekey(session.hkey);

    /*
     * Send SSH2_MSG_NEWKEYS.
     */
    ssh2_pkt_init(session, SSH2_MSG_NEWKEYS);
    ssh2_pkt_send(session);

    /*
     * Expect SSH2_MSG_NEWKEYS from server.
     */
    crWaitUntil(ispkt);
    if (session.pktin.type != SSH2_MSG_NEWKEYS) {
		bombout((session, "expected new-keys packet from server"));
		crReturn(0);
    }

    /*
     * Create and initialise session keys.
     */
    session.cscipher = session.cscipher_tobe;
    session.sccipher = session.sccipher_tobe;
    session.csmac = session.csmac_tobe;
    session.scmac = session.scmac_tobe;
    session.cscomp = session.cscomp_tobe;
    session.sccomp = session.sccomp_tobe;
    session.cscomp->compress_init(session);
    session.sccomp->decompress_init(session);

	logeventf(session, "Negotiated client->server encryption: %s",	  session.cscipher->name);
	logeventf(session, "Negotiated server->client encryption: %s",	  session.sccipher->name);
	logeventf(session, "Negotiated client->server MAC algorithm: %s", session.csmac->name);
	logeventf(session, "Negotiated server->client MAC algorithm: %s", session.scmac->name);
	logeventf(session, "Negotiated client->server compression: %s",   session.cscomp->name);
	logeventf(session, "Negotiated server->client compression: %s",   session.sccomp->name);
	
    /*
     * Set IVs after keys. Here we use the exchange hash from the
     * _first_ key exchange.
     */
    if (session.first_kex)
		memcpy(session.ssh2_session_id, session.exchange_hash, sizeof(session.exchange_hash));
    ssh2_mkkey(session.K, (char *)session.exchange_hash, (char *)session.ssh2_session_id, 'C', (char *)session.keyspace);
    session.cscipher->setcskey(session, session.keyspace);
    ssh2_mkkey(session.K, (char *)session.exchange_hash, (char *)session.ssh2_session_id, 'D', (char *)session.keyspace);
    session.sccipher->setsckey(session, session.keyspace);
    ssh2_mkkey(session.K, (char *)session.exchange_hash, (char *)session.ssh2_session_id, 'A', (char *)session.keyspace);
    session.cscipher->setcsiv(session, session.keyspace);
    ssh2_mkkey(session.K, (char *)session.exchange_hash, (char *)session.ssh2_session_id, 'B', (char *)session.keyspace);
    session.sccipher->setsciv(session, session.keyspace);
    ssh2_mkkey(session.K, (char *)session.exchange_hash, (char *)session.ssh2_session_id, 'E', (char *)session.keyspace);
    session.csmac->setcskey(session, session.keyspace);
    ssh2_mkkey(session.K, (char *)session.exchange_hash, (char *)session.ssh2_session_id, 'F', (char *)session.keyspace);
    session.scmac->setsckey(session, session.keyspace);

    /*
     * If this is the first key exchange phase, we must pass the
     * SSH2_MSG_NEWKEYS packet to the next layer, not because it
     * wants to see it but because it will need time to initialise
     * itself before it sees an actual packet. In subsequent key
     * exchange phases, we don't pass SSH2_MSG_NEWKEYS on, because
     * it would only confuse the layer above.
     */
    if (!session.first_kex) {
		crReturn(0);
    }
    session.first_kex = 0;

    /*
     * Now we're encrypting. Begin returning 1 to the protocol main
     * function so that other things can run on top of the
     * transport. If we ever see a KEXINIT, we must go back to the
     * start.
     */
    while (!(ispkt && session.pktin.type == SSH2_MSG_KEXINIT)) {
		crReturn(1);
    }
    logevent(session, "Server initiated key re-exchange");
    goto begin_key_exchange;

    crFinish(1);
}

/*
 * Add data to an SSH2 channel output buffer.
 */
static void ssh2_add_channel_data(struct ssh_channel *c, char *buf,
				  int len)
{
    bufchain_add(&c->v.v2.outbuffer, buf, len);
}

/*
 * Attempt to send data on an SSH2 channel.
 */
static int ssh2_try_send(CSshSession &session, struct ssh_channel *c)
{
    while (c->v.v2.remwindow > 0 && bufchain_size(&c->v.v2.outbuffer) > 0) {
	int len;
	void *data;
	bufchain_prefix(&c->v.v2.outbuffer, &data, &len);
	if ((unsigned)len > c->v.v2.remwindow)
	    len = c->v.v2.remwindow;
	if ((unsigned)len > c->v.v2.remmaxpkt)
	    len = c->v.v2.remmaxpkt;
	ssh2_pkt_init(session, SSH2_MSG_CHANNEL_DATA);
	ssh2_pkt_adduint32(session,c->remoteid);
	ssh2_pkt_addstring_start(session);
	ssh2_pkt_addstring_data(session, (char *)data, len);
	ssh2_pkt_send(session);
	bufchain_consume(&c->v.v2.outbuffer, len);
	c->v.v2.remwindow -= len;
    }

    /*
     * After having sent as much data as we can, return the amount
     * still buffered.
     */
    return bufchain_size(&c->v.v2.outbuffer);
}

/*
 * Potentially enlarge the window on an SSH2 channel.
 */
static void ssh2_set_window(CSshSession &session, struct ssh_channel *c, unsigned newwin)
{
    /*
     * Never send WINDOW_ADJUST for a channel that the remote side
     * already thinks it's closed; there's no point, since it won't
     * be sending any more data anyway.
     */
    if (c->closes != 0)
		return;

    if (newwin > c->v.v2.locwindow) {
		ssh2_pkt_init(session, SSH2_MSG_CHANNEL_WINDOW_ADJUST);
		ssh2_pkt_adduint32(session,c->remoteid);
		ssh2_pkt_adduint32(session,newwin - c->v.v2.locwindow);
		ssh2_pkt_send(session);
		c->v.v2.locwindow = newwin;
    }
}

/*
 * Handle the SSH2 userauth and connection layers.
 */
static void do_ssh2_authconn(CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine)
{
    //static enum {
	//AUTH_INVALID, AUTH_PUBLICKEY_AGENT, AUTH_PUBLICKEY_FILE,
	//AUTH_PASSWORD,
    //    AUTH_KEYBOARD_INTERACTIVE
    //} method;
#define AUTH_INVALID				0
#define AUTH_PUBLICKEY_AGENT		1
#define AUTH_PUBLICKEY_FILE			2
#define AUTH_PASSWORD				3
#define AUTH_KEYBOARD_INTERACTIVE	4
	int method;

//    static enum {
//	AUTH_TYPE_NONE,
//	AUTH_TYPE_PUBLICKEY,
//	AUTH_TYPE_PUBLICKEY_OFFER_LOUD,
//	AUTH_TYPE_PUBLICKEY_OFFER_QUIET,
//	AUTH_TYPE_PASSWORD,
//	AUTH_TYPE_KEYBOARD_INTERACTIVE,
//	AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET
//	  } type;

#define AUTH_TYPE_NONE							0
#define AUTH_TYPE_PUBLICKEY						1
#define AUTH_TYPE_PUBLICKEY_OFFER_LOUD			2
#define AUTH_TYPE_PUBLICKEY_OFFER_QUIET			3
#define AUTH_TYPE_PASSWORD						4
#define AUTH_TYPE_KEYBOARD_INTERACTIVE			5
#define AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET	6
	int type = AUTH_TYPE_NONE;

    static int gotit, need_pw, can_pubkey, can_passwd, can_keyb_inter;
    static int tried_pubkey_config, tried_agent, tried_keyb_inter;
    static int kbd_inter_running;
    static int we_are_in;
    static int num_prompts, echo;
    static char username[100];
    static int got_username;
    static char pwprompt[200];
    static char password[100];
    static void *publickey_blob;
    static int publickey_bloblen;

    crBegin;

    /*
     * Request userauth protocol, and await a response to it.
     */
    ssh2_pkt_init(session, SSH2_MSG_SERVICE_REQUEST);
    ssh2_pkt_addstring(session, "ssh-userauth");
    ssh2_pkt_send(session);
    crWaitUntilV(ispkt);
    if (session.pktin.type != SSH2_MSG_SERVICE_ACCEPT) {
		bombout((session, "Server refused user authentication protocol"));
		crReturnV;
    }

    /*
     * We repeat this whole loop, including the username prompt,
     * until we manage a successful authentication. If the user
     * types the wrong _password_, they are sent back to the
     * beginning to try another username. (If they specify a
     * username in the config, they are never asked, even if they
     * do give a wrong password.)
     * 
     * I think this best serves the needs of
     * 
     *  - the people who have no configuration, no keys, and just
     *    want to try repeated (username,password) pairs until they
     *    type both correctly
     * 
     *  - people who have keys and configuration but occasionally
     *    need to fall back to passwords
     * 
     *  - people with a key held in Pageant, who might not have
     *    logged in to a particular machine before; so they want to
     *    type a username, and then _either_ their key will be
     *    accepted, _or_ they will type a password. If they mistype
     *    the username they will want to be able to get back and
     *    retype it!
     */
    username[0] = '\0';
    got_username = FALSE;
    do {
	static int pos;
	static char c;

	/*
	 * Get a username.
	 */
	pos = 0;
	if (got_username && !cfg.change_username) {
	    /*
	     * We got a username last time round this loop, and
	     * with change_username turned off we don't try to get
	     * it again.
	     */
	} else if ((flags & FLAG_INTERACTIVE) && !*cfg.username) {
	    if (session.ssh_get_line) {
		if (!session.ssh_get_line("login as: ",
				  username, sizeof(username), FALSE)) {
		    /*
		     * get_line failed to get a username.
		     * Terminate.
		     */
		    logevent(session, "No username provided. Abandoning session.");
		    session.ssh_state = SSH_STATE_CLOSED;
		    crReturnV;
		}
	    } else {
		c_write_str("login as: ");
		session.ssh_send_ok = 1;
		while (pos >= 0) {
		    crWaitUntilV(!ispkt);
		    while (inlen--)
			switch (c = *in++) {
			  case 10:
			  case 13:
			    username[pos] = 0;
			    pos = -1;
			    break;
			  case 8:
			  case 127:
			    if (pos > 0) {
				c_write_str("\b \b");
				pos--;
			    }
			    break;
			  case 21:
			  case 27:
			    while (pos > 0) {
				c_write_str("\b \b");
				pos--;
			    }
			    break;
			  case 3:
			  case 4:
			    cleanup_exit(0);
			    break;
			  default:
			    if (((c >= ' ' && c <= '~') ||
				 ((unsigned char) c >= 160))
				&& pos < sizeof(username)-1) {
				username[pos++] = c;
				c_write(&c, 1);
			    }
			    break;
			}
		}
	    }
	    c_write_str("\r\n");
	    username[strcspn(username, "\n\r")] = '\0';
	} else {
	    char stuff[200];
	    strncpy(username, cfg.username, 99);
	    username[99] = '\0';
	    if ((flags & FLAG_VERBOSE) || (flags & FLAG_INTERACTIVE)) {
		sprintf(stuff, "Using username \"%s\".\r\n", username);
		c_write_str(stuff);
	    }
	}
	got_username = TRUE;

	/*
	 * Send an authentication request using method "none": (a)
	 * just in case it succeeds, and (b) so that we know what
	 * authentication methods we can usefully try next.
	 */
	ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;

	ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
	ssh2_pkt_addstring(session, username);
	ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
	ssh2_pkt_addstring(session, "none");    /* method */
	ssh2_pkt_send(session);
	type = AUTH_TYPE_NONE;
	gotit = FALSE;
	we_are_in = FALSE;

	tried_pubkey_config = FALSE;
	tried_agent = FALSE;
	tried_keyb_inter = FALSE;
	kbd_inter_running = FALSE;
	/* Load the pub half of cfg.keyfile so we notice if it's in Pageant */
	if (*cfg.keyfile) {
	    int keytype;
	    logeventf(session, "Reading private key file \"%.150s\"", cfg.keyfile);
	    keytype = key_type(cfg.keyfile);
	    if (keytype == SSH_KEYTYPE_SSH2)
		publickey_blob = ssh2_userkey_loadpub(cfg.keyfile, NULL,
						      &publickey_bloblen);
	    else {
		char msgbuf[256];
		logeventf(session, "Unable to use this key file (%s)",
			key_type_to_str(keytype));
		sprintf(msgbuf, "Unable to use key file \"%.150s\" (%s)\r\n",
			cfg.keyfile, key_type_to_str(keytype));
		c_write_str(msgbuf);
		publickey_blob = NULL;
	    }
	} else
	    publickey_blob = NULL;

	while (1) {
	    /*
	     * Wait for the result of the last authentication request.
	     */
	    if (!gotit)
		crWaitUntilV(ispkt);
	    while (session.pktin.type == SSH2_MSG_USERAUTH_BANNER) {
		char *banner;
		int size;
		/*
		 * Don't show the banner if we're operating in
		 * non-verbose non-interactive mode. (It's probably
		 * a script, which means nobody will read the
		 * banner _anyway_, and moreover the printing of
		 * the banner will screw up processing on the
		 * output of (say) plink.)
		 */
		if (flags & (FLAG_VERBOSE | FLAG_INTERACTIVE)) {
		    ssh2_pkt_getstring(session,&banner, &size);
		    if (banner)
			c_write_untrusted(banner, size);
		}
		crWaitUntilV(ispkt);
	    }
	    if (session.pktin.type == SSH2_MSG_USERAUTH_SUCCESS) {
			logevent(session, "Access granted");
			we_are_in = TRUE;
			break;
	    }

	    if (kbd_inter_running &&
			session.pktin.type == SSH2_MSG_USERAUTH_INFO_REQUEST) 
		{
		/*
		 * This is a further prompt in keyboard-interactive
		 * authentication. Do nothing.
		 */
	    } else if (session.pktin.type != SSH2_MSG_USERAUTH_FAILURE) {
			bombout((session, "Strange packet received during authentication: type %d",
				 session.pktin.type));
			crReturnV;
	    }

	    gotit = FALSE;

	    /*
	     * OK, we're now sitting on a USERAUTH_FAILURE message, so
	     * we can look at the string in it and know what we can
	     * helpfully try next.
	     */
	    if (session.pktin.type == SSH2_MSG_USERAUTH_FAILURE) {
		char *methods;
		int methlen;
		ssh2_pkt_getstring(session,&methods, &methlen);
		kbd_inter_running = FALSE;
		if (!ssh2_pkt_getbool(session)) {
		    /*
		     * We have received an unequivocal Access
		     * Denied. This can translate to a variety of
		     * messages:
		     * 
		     *  - if we'd just tried "none" authentication,
		     *    it's not worth printing anything at all
		     * 
		     *  - if we'd just tried a public key _offer_,
		     *    the message should be "Server refused our
		     *    key" (or no message at all if the key
		     *    came from Pageant)
		     * 
		     *  - if we'd just tried anything else, the
		     *    message really should be "Access denied".
		     * 
		     * Additionally, if we'd just tried password
		     * authentication, we should break out of this
		     * whole loop so as to go back to the username
		     * prompt.
		     */
		    if (type == AUTH_TYPE_NONE) {
				/* do nothing */
		    } else if (type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD ||
			       type == AUTH_TYPE_PUBLICKEY_OFFER_QUIET) 
			{
				if (type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD)
				    c_write_str("Server refused our key\r\n");
				logevent(session, "Server refused public key");
		    } else if (type == AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET) {
				/* server declined keyboard-interactive; ignore */
		    } else {
				c_write_str("Access denied\r\n");
				logevent(session, "Access denied");
				if (type == AUTH_TYPE_PASSWORD) {
					we_are_in = FALSE;
					break;
				}
		    }
		} else {
		    c_write_str("Further authentication required\r\n");
		    logevent(session, "Further authentication required");
		}

		can_pubkey =
		    in_commasep_string("publickey", methods, methlen);
		can_passwd =
		    in_commasep_string("password", methods, methlen);
		can_keyb_inter = cfg.try_ki_auth &&
		    in_commasep_string("keyboard-interactive", methods, methlen);
	    }

	    method = 0;
	    ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;

	    if (!method && can_pubkey && agent_exists() && !tried_agent) {
		/*
		 * Attempt public-key authentication using Pageant.
		 */
		static unsigned char request[5], *response, *p;
		static int responselen;
		static int i, nkeys;
		static int authed;
		void *r;

		authed = FALSE;
		ssh_pkt_ctx |= SSH2_PKTCTX_PUBLICKEY;

		tried_agent = TRUE;

		logevent(session, "Pageant is running. Requesting keys.");

		/* Request the keys held by the agent. */
		PUT_32BIT(request, 1);
		request[4] = SSH2_AGENTC_REQUEST_IDENTITIES;
		agent_query(request, 5, &r, &responselen);
		response = (unsigned char *) r;
		if (response && responselen >= 5 &&
		    response[4] == SSH2_AGENT_IDENTITIES_ANSWER) {
		    p = response + 5;
		    nkeys = GET_32BIT(p);
		    p += 4;
		    {
			char buf[64];
			sprintf(buf, "Pageant has %d SSH2 keys", nkeys);
			logevent(session, buf);
		    }
		    for (i = 0; i < nkeys; i++) {
			static char *pkblob, *alg, *commentp;
			static int pklen, alglen, commentlen;
			static int siglen, retlen, len;
			static char *q, *agentreq, *ret;
			void *vret;

			{
			    char buf[64];
			    sprintf(buf, "Trying Pageant key #%d", i);
			    logevent(session, buf);
			}
			pklen = GET_32BIT(p);
			p += 4;
			if (publickey_blob &&
			    pklen == publickey_bloblen &&
			    !memcmp(p, publickey_blob, publickey_bloblen)) {
			    logevent(session, "This key matches configured key file");
			    tried_pubkey_config = 1;
			}
			pkblob = (char *)p;
			p += pklen;
			alglen = GET_32BIT(pkblob);
			alg = pkblob + 4;
			commentlen = GET_32BIT(p);
			p += 4;
			commentp = (char *)p;
			p += commentlen;
			ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(session, username);
			ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(session, "publickey");	/* method */
			ssh2_pkt_addbool(session, FALSE);	/* no signature included */
			ssh2_pkt_addstring_start(session);
			ssh2_pkt_addstring_data(session, alg, alglen);
			ssh2_pkt_addstring_start(session);
			ssh2_pkt_addstring_data(session, pkblob, pklen);
			ssh2_pkt_send(session);

			crWaitUntilV(ispkt);
			if (session.pktin.type != SSH2_MSG_USERAUTH_PK_OK) {
			    logevent(session, "Key refused");
			    continue;
			}

			if (flags & FLAG_VERBOSE) {
			    c_write_str
				("Authenticating with public key \"");
			    c_write(commentp, commentlen);
			    c_write_str("\" from agent\r\n");
			}

			/*
			 * Server is willing to accept the key.
			 * Construct a SIGN_REQUEST.
			 */
			ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(session, username);
			ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(session, "publickey");	/* method */
			ssh2_pkt_addbool(session, TRUE);
			ssh2_pkt_addstring_start(session);
			ssh2_pkt_addstring_data(session, alg, alglen);
			ssh2_pkt_addstring_start(session);
			ssh2_pkt_addstring_data(session, pkblob, pklen);

			siglen = session.pktout.length - 5 + 4 + 20;
			len = 1;       /* message type */
			len += 4 + pklen;	/* key blob */
			len += 4 + siglen;	/* data to sign */
			len += 4;      /* flags */
			agentreq = (char *)smalloc(4 + len);
			PUT_32BIT(agentreq, len);
			q = agentreq + 4;
			*q++ = SSH2_AGENTC_SIGN_REQUEST;
			PUT_32BIT(q, pklen);
			q += 4;
			memcpy(q, pkblob, pklen);
			q += pklen;
			PUT_32BIT(q, siglen);
			q += 4;
			/* Now the data to be signed... */
			PUT_32BIT(q, 20);
			q += 4;
			memcpy(q, session.ssh2_session_id, 20);
			q += 20;
			memcpy(q, session.pktout.data + 5, session.pktout.length - 5);
			q += session.pktout.length - 5;
			/* And finally the (zero) flags word. */
			PUT_32BIT(q, 0);
			agent_query(agentreq, len + 4, &vret, &retlen);
			ret = (char *)vret;
			sfree(agentreq);
			if (ret) {
			    if (ret[4] == SSH2_AGENT_SIGN_RESPONSE) {
				logevent(session, "Sending Pageant's response");
				ssh2_add_sigblob(session, pkblob, pklen,
						 ret + 9, GET_32BIT(ret + 5));
				ssh2_pkt_send(session);
				authed = TRUE;
				break;
			    } else {
				logevent(session, "Pageant failed to answer challenge");
				sfree(ret);
			    }
			}
		    }
		    if (authed)
				continue;
		}
	    }

	    if (!method && can_pubkey && publickey_blob	&& !tried_pubkey_config) 
		{
			unsigned char *pub_blob;
			char *algorithm, *comment;
			int pub_blob_len;

			tried_pubkey_config = TRUE;

			ssh_pkt_ctx |= SSH2_PKTCTX_PUBLICKEY;

			/*
			 * Try the public key supplied in the configuration.
			 *
			 * First, offer the public blob to see if the server is
			 * willing to accept it.
			 */
			pub_blob = (unsigned char *)ssh2_userkey_loadpub(cfg.keyfile, &algorithm,
							&pub_blob_len);
			if (pub_blob) {
				ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
				ssh2_pkt_addstring(session, username);
				ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
				ssh2_pkt_addstring(session, "publickey");	/* method */
				ssh2_pkt_addbool(session, FALSE);	/* no signature included */
				ssh2_pkt_addstring(session, algorithm);
				ssh2_pkt_addstring_start(session);
				ssh2_pkt_addstring_data(session, (char *)pub_blob, pub_blob_len);
				ssh2_pkt_send(session);
				logevent(session, "Offered public key");	/* FIXME */

				crWaitUntilV(ispkt);
				if (session.pktin.type != SSH2_MSG_USERAUTH_PK_OK) {
					gotit = TRUE;
					type = AUTH_TYPE_PUBLICKEY_OFFER_LOUD;
					continue;      /* key refused; give up on it */
				}

				logevent(session, "Offer of public key accepted");
				/*
				 * Actually attempt a serious authentication using
				 * the key.
				 */
				if (ssh2_userkey_encrypted(cfg.keyfile, &comment)) {
					sprintf(pwprompt, "Passphrase for key \"%.100s\": ", comment);
					need_pw = TRUE;
				} else {
					need_pw = FALSE;
				}
				c_write_str("Authenticating with public key \"");
				c_write_str(comment);
				c_write_str("\"\r\n");
				method = AUTH_PUBLICKEY_FILE;
			}
	    }

	    if (!method && can_keyb_inter && !tried_keyb_inter) {
			method = AUTH_KEYBOARD_INTERACTIVE;
			type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
			tried_keyb_inter = TRUE;

			ssh_pkt_ctx |= SSH2_PKTCTX_KBDINTER;

			ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(session, username);
			ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(session, "keyboard-interactive");	/* method */
			ssh2_pkt_addstring(session, ""); /* lang */
			ssh2_pkt_addstring(session, "");
			ssh2_pkt_send(session);

			crWaitUntilV(ispkt);
			if (session.pktin.type != SSH2_MSG_USERAUTH_INFO_REQUEST) {
				if (session.pktin.type == SSH2_MSG_USERAUTH_FAILURE)
					gotit = TRUE;
				logevent(session, "Keyboard-interactive authentication refused");
				type = AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET;
				continue;
			}

			kbd_inter_running = TRUE;
	    }

	    if (kbd_inter_running) {
			method = AUTH_KEYBOARD_INTERACTIVE;
			type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
			tried_keyb_inter = TRUE;

			ssh_pkt_ctx |= SSH2_PKTCTX_KBDINTER;

			/* We've got packet with that "interactive" info
			   dump banners, and set its prompt as ours */
			{
				char *name, *inst, *lang, *prompt;
				int name_len, inst_len, lang_len, prompt_len;
				ssh2_pkt_getstring(session, &name, &name_len);
				ssh2_pkt_getstring(session, &inst, &inst_len);
				ssh2_pkt_getstring(session, &lang, &lang_len);
				if (name_len > 0)
				c_write_untrusted(name, name_len);
				if (inst_len > 0)
				c_write_untrusted(inst, inst_len);
				num_prompts = ssh2_pkt_getuint32(session);

				ssh2_pkt_getstring(session, &prompt, &prompt_len);
				strncpy(pwprompt, prompt, sizeof(pwprompt));
				pwprompt[prompt_len < sizeof(pwprompt) ?
					 prompt_len : sizeof(pwprompt)-1] = '\0';
				need_pw = TRUE;

				echo = ssh2_pkt_getbool(session);
			}
	    }

	    if (!method && can_passwd) {
			method = AUTH_PASSWORD;
			ssh_pkt_ctx |= SSH2_PKTCTX_PASSWORD;
			sprintf(pwprompt, "%.90s@%.90s's password: ", username,
				session.savedhost);
			need_pw = TRUE;
	    }

	    if (need_pw) {
		if (session.ssh_get_line) {
		    if (!session.ssh_get_line(pwprompt, password,
				      sizeof(password), TRUE)) {
			/*
			 * get_line failed to get a password (for
			 * example because one was supplied on the
			 * command line which has already failed to
			 * work). Terminate.
			 */
			ssh2_pkt_init(session, SSH2_MSG_DISCONNECT);
			ssh2_pkt_adduint32(session,SSH2_DISCONNECT_BY_APPLICATION);
			ssh2_pkt_addstring(session,"No more passwords available to try");
			ssh2_pkt_addstring(session, "en");	/* language tag */
			ssh2_pkt_send(session);
			logevent(session, "Unable to authenticate");
			connection_fatal(session, "Unable to authenticate");
			session.ssh_state = SSH_STATE_CLOSED;
			crReturnV;
		    }
		} else {
		    static int pos;
		    static char c;

		    pos = 0;
		    c_write_untrusted(pwprompt, strlen(pwprompt));
		    session.ssh_send_ok = 1;

		    pos = 0;
		    while (pos >= 0) {
			crWaitUntilV(!ispkt);
			while (inlen--)
			    switch (c = *in++) {
			      case 10:
			      case 13:
				password[pos] = 0;
				pos = -1;
				break;
			      case 8:
			      case 127:
				if (pos > 0)
				    pos--;
				break;
			      case 21:
			      case 27:
				pos = 0;
				break;
			      case 3:
			      case 4:
				cleanup_exit(0);
				break;
			      default:
				if (pos < sizeof(password)-1)
				    password[pos++] = c;
				break;
			    }
		    }
		    c_write_str("\r\n");
		}
	    }

	    if (method == AUTH_PUBLICKEY_FILE) {
			/*
			 * We have our passphrase. Now try the actual authentication.
			 */
			struct ssh2_userkey *key;
			key = ssh2_load_userkey(cfg.keyfile, password);
			if (key == SSH2_WRONG_PASSPHRASE || key == NULL) {
				if (key == SSH2_WRONG_PASSPHRASE) {
					c_write_str("Wrong passphrase\r\n");
					tried_pubkey_config = FALSE;
				} else {
					c_write_str("Unable to load private key\r\n");
					tried_pubkey_config = TRUE;
				}
				/* Send a spurious AUTH_NONE to return to the top. */
				ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
				ssh2_pkt_addstring(session, username);
				ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
				ssh2_pkt_addstring(session, "none");	/* method */
				ssh2_pkt_send(session);
				type = AUTH_TYPE_NONE;
			} else {
				unsigned char *pkblob, *sigblob, *sigdata;
				int pkblob_len, sigblob_len, sigdata_len;

				/*
				 * We have loaded the private key and the server
				 * has announced that it's willing to accept it.
				 * Hallelujah. Generate a signature and send it.
				 */
				ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
				ssh2_pkt_addstring(session, username);
				ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
				ssh2_pkt_addstring(session, "publickey");	/* method */
				ssh2_pkt_addbool(session, TRUE);
				ssh2_pkt_addstring(session, key->alg->name);
				pkblob = key->alg->public_blob(key->data, &pkblob_len);
				ssh2_pkt_addstring_start(session);
				ssh2_pkt_addstring_data(session, (char *)pkblob, pkblob_len);

				/*
				 * The data to be signed is:
				 *
				 *   string  session-id
				 *
				 * followed by everything so far placed in the
				 * outgoing packet.
				 */
				sigdata_len = session.pktout.length - 5 + 4 + 20;
				sigdata = (unsigned char *)smalloc(sigdata_len);
				PUT_32BIT(sigdata, 20);
				memcpy(sigdata + 4, session.ssh2_session_id, 20);
				memcpy(sigdata + 24, session.pktout.data + 5,
				   session.pktout.length - 5);
				sigblob = key->alg->sign(key->data, (char *)sigdata,
							 sigdata_len, &sigblob_len);
				ssh2_add_sigblob(session,pkblob, pkblob_len,
						 sigblob, sigblob_len);
				sfree(pkblob);
				sfree(sigblob);
				sfree(sigdata);

				ssh2_pkt_send(session);
				type = AUTH_TYPE_PUBLICKEY;
			}
	    } else if (method == AUTH_PASSWORD) {

			//ask for new password
			//TOFIX CSshSession function?
			if(strlen(session.m_password)<1){
				if(session.m_fnLogin){
					if(!session.m_fnLogin(session.m_fnLoginData))
					{
						//TOFIX password canceled, exit, 
						c_write_str
							("Ssh login canceled by user (password not entered)!\r\n");
						logevent(session, "Ssh login canceled by user (password not entered)! Disconnecting.");
						ssh2_pkt_init(session, SSH2_MSG_DISCONNECT);
						ssh2_pkt_adduint32(session,SSH2_DISCONNECT_BY_APPLICATION);
						ssh2_pkt_addstring
							(session,"Ssh login canceled by user.");
						ssh2_pkt_addstring(session, "en");	/* language tag */
						ssh2_pkt_send(session);
						session.ssh_state = SSH_STATE_CLOSED;
						crReturnV;
					}
				}
			}

			/*
			 * We send the password packet lumped tightly together with
			 * an SSH_MSG_IGNORE packet. The IGNORE packet contains a
			 * string long enough to make the total length of the two
			 * packets constant. This should ensure that a passive
			 * listener doing traffic analyis can't work out the length
			 * of the password.
			 *
			 * For this to work, we need an assumption about the
			 * maximum length of the password packet. I think 256 is
			 * pretty conservative. Anyone using a password longer than
			 * that probably doesn't have much to worry about from
			 * people who find out how long their password is!
			 */
			ssh2_pkt_init(session, SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(session, username);
			ssh2_pkt_addstring(session, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(session, "password");
			ssh2_pkt_addbool(session, FALSE);
			ssh2_pkt_addstring(session, session.m_password);//ssh2_pkt_addstring(session, password);
			ssh2_pkt_defer(session);

			//clear old password
			session.m_password[0] = '\0'; 

			/*
			 * We'll include a string that's an exact multiple of the
			 * cipher block size. If the cipher is NULL for some
			 * reason, we don't do this trick at all because we gain
			 * nothing by it.
			 */
			if (session.cscipher) {
				int stringlen, i;

				stringlen = (256 - session.deferred_len);
				stringlen += session.cscipher->blksize - 1;
				stringlen -= (stringlen % session.cscipher->blksize);
				if (session.cscomp) {
					/*
					 * Temporarily disable actual compression,
					 * so we can guarantee to get this string
					 * exactly the length we want it. The
					 * compression-disabling routine should
					 * return an integer indicating how many
					 * bytes we should adjust our string length
					 * by.
					 */
					stringlen -= session.cscomp->disable_compression();
				}
				ssh2_pkt_init(session, SSH2_MSG_IGNORE);
				ssh2_pkt_addstring_start(session);
				for (i = 0; i < stringlen; i++) {
					char c = (char) random_byte();
					ssh2_pkt_addstring_data(session, &c, 1);
				}
				ssh2_pkt_defer(session);
			}
			ssh_pkt_defersend(session);
			logevent(session, "Sent password");
			type = AUTH_TYPE_PASSWORD;
	    } else if (method == AUTH_KEYBOARD_INTERACTIVE) {
            ssh2_pkt_init(session, SSH2_MSG_USERAUTH_INFO_RESPONSE);
            ssh2_pkt_adduint32(session,num_prompts);
            ssh2_pkt_addstring(session, password);//TOFIX?
            memset(password, 0, sizeof(password));
            ssh2_pkt_send(session);
			type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
	    } else {
			c_write_str
				("No supported authentication methods left to try!\r\n");
			logevent(session, "No supported authentications offered. Disconnecting");
			ssh2_pkt_init(session, SSH2_MSG_DISCONNECT);
			ssh2_pkt_adduint32(session,SSH2_DISCONNECT_BY_APPLICATION);
			ssh2_pkt_addstring
				(session,"No supported authentication methods available");
			ssh2_pkt_addstring(session, "en");	/* language tag */
			ssh2_pkt_send(session);
			session.ssh_state = SSH_STATE_CLOSED;
			crReturnV;
	    }
	}
    } while (!we_are_in);

    /*
     * Now we're authenticated for the connection protocol. The
     * connection protocol will automatically have started at this
     * point; there's no need to send SERVICE_REQUEST.
     */

    /*
     * So now create a channel with a session in it.
     */
    session.ssh_channels = newtree234(ssh_channelcmp);
    session.mainchan = (struct ssh_channel *)smalloc(sizeof(struct ssh_channel));
    session.mainchan->localid = alloc_channel_id(session);
    ssh2_pkt_init(session, SSH2_MSG_CHANNEL_OPEN);
    ssh2_pkt_addstring(session, "session");
    ssh2_pkt_adduint32(session,session.mainchan->localid);
    session.mainchan->v.v2.locwindow = OUR_V2_WINSIZE;
    ssh2_pkt_adduint32(session,session.mainchan->v.v2.locwindow);      /* our window size */
    ssh2_pkt_adduint32(session,0x4000UL);      /* our max pkt size */
    ssh2_pkt_send(session);
    crWaitUntilV(ispkt);
    if (session.pktin.type != SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
		bombout((session, "Server refused to open a session"));
		crReturnV;
	/* FIXME: error data comes back in FAILURE packet */
    }
    if (ssh2_pkt_getuint32(session) != session.mainchan->localid) {
		bombout((session, "Server's channel confirmation cited wrong channel"));
		crReturnV;
    }
    session.mainchan->remoteid = ssh2_pkt_getuint32(session);
    session.mainchan->type = CHAN_MAINSESSION;
    session.mainchan->closes = 0;
    session.mainchan->v.v2.remwindow = ssh2_pkt_getuint32(session);
    session.mainchan->v.v2.remmaxpkt = ssh2_pkt_getuint32(session);
    bufchain_init(&session.mainchan->v.v2.outbuffer);
    add234(session.ssh_channels, session.mainchan);
    logevent(session, "Opened channel for session");

    /*
     * Potentially enable X11 forwarding.
     */
    if (cfg.x11_forward) {
	char proto[20], data[64];
	logevent(session, "Requesting X11 forwarding");
	x11_invent_auth(proto, sizeof(proto), data, sizeof(data));
	ssh2_pkt_init(session, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(session,session.mainchan->remoteid);
	ssh2_pkt_addstring(session, "x11-req");
	ssh2_pkt_addbool(session, 1);	       /* want reply */
	ssh2_pkt_addbool(session, 0);	       /* many connections */
	ssh2_pkt_addstring(session, proto);
	ssh2_pkt_addstring(session, data);
	ssh2_pkt_adduint32(session,0);	       /* screen number */
	ssh2_pkt_send(session);

	do {
	    crWaitUntilV(ispkt);
	    if (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(session);
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(session);
	    }
	} while (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (session.pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (session.pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
			bombout((session, "Unexpected response to X11 forwarding request:"
				 " packet type %d", session.pktin.type));
			crReturnV;
	    }
	    logevent(session, "X11 forwarding refused");
	} else {
	    logevent(session, "X11 forwarding enabled");
	    session.ssh_X11_fwd_enabled = TRUE;
	}
    }

    /*
     * Enable port forwardings.
     */
    {
	static char *e;		       /* preserve across crReturn */
	char type;
	int n;
	int sport,dport,sserv,dserv;
	char sports[256], dports[256], host[256];
	char buf[1024];
	struct servent *se;

	session.ssh_rportfwds = newtree234(ssh_rportcmp_ssh2);

	/* Add port forwardings. */
	e = cfg.portfwd;
	while (*e) {
	    type = *e++;
	    n = 0;
	    while (*e && *e != '\t')
			sports[n++] = *e++;
	    sports[n] = 0;
	    if (*e == '\t')
		e++;
	    n = 0;
	    while (*e && *e != ':')
			host[n++] = *e++;
	    host[n] = 0;
	    if (*e == ':')
		e++;
	    n = 0;
	    while (*e)
			dports[n++] = *e++;
	    dports[n] = 0;
	    e++;
	    dport = atoi(dports);
	    dserv = 0;
	    if (dport == 0) {
		dserv = 1;
		se = getservbyname(dports, NULL);
		if (se != NULL) {
		    dport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for destination port \"%s\"",
			    dports);
		    logevent(session, buf);
		}
	    }
	    sport = atoi(sports);
	    sserv = 0;
	    if (sport == 0) {
		sserv = 1;
		se = getservbyname(sports, NULL);
		if (se != NULL) {
		    sport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for source port \"%s\"",
			    sports);
		    logevent(session, buf);
		}
	    }
	    if (sport && dport) {
		if (type == 'L') {
		    pfd_addforward(session, host, dport, sport);
		    sprintf(buf, "Local port %.*s%.*s%d%.*s forwarding to"
			    " %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
		    logevent(session, buf);
		} else {
		    struct ssh_rportfwd *pf;
		    pf = (struct ssh_rportfwd *)smalloc(sizeof(*pf));
		    strcpy(pf->dhost, host);
		    pf->dport = dport;
		    pf->sport = sport;
		    if (add234(session.ssh_rportfwds, pf) != pf) {
			sprintf(buf, 
				"Duplicate remote port forwarding to %s:%d",
				host, dport);
			logevent(session, buf);
			sfree(pf);
		    } else {
			sprintf(buf, "Requesting remote port %.*s%.*s%d%.*s"
				" forward to %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
			logevent(session, buf);
			ssh2_pkt_init(session, SSH2_MSG_GLOBAL_REQUEST);
			ssh2_pkt_addstring(session, "tcpip-forward");
			ssh2_pkt_addbool(session, 1);/* want reply */
			if (cfg.rport_acceptall)
			    ssh2_pkt_addstring(session, "0.0.0.0");
			else
			    ssh2_pkt_addstring(session, "127.0.0.1");
			ssh2_pkt_adduint32(session,sport);
			ssh2_pkt_send(session);

			do {
			    crWaitUntilV(ispkt);
			    if (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
				unsigned i = ssh2_pkt_getuint32(session);
				struct ssh_channel *c;
				c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
				if (!c)
				    continue;/* nonexistent channel */
				c->v.v2.remwindow += ssh2_pkt_getuint32(session);
			    }
			} while (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

			if (session.pktin.type != SSH2_MSG_REQUEST_SUCCESS) {
			    if (session.pktin.type != SSH2_MSG_REQUEST_FAILURE) {
					bombout((session, "Unexpected response to port "
					 "forwarding request: packet type %d",
					 session.pktin.type));
					crReturnV;
			    }
			    logevent(session, "Server refused this port forwarding");
			} else {
			    logevent(session, "Remote port forwarding enabled");
			}
		    }
		}
	    }
	}
    }

    /*
     * Potentially enable agent forwarding.
     */
    if (cfg.agentfwd && agent_exists()) {
	logevent(session, "Requesting OpenSSH-style agent forwarding");
	ssh2_pkt_init(session, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(session,session.mainchan->remoteid);
	ssh2_pkt_addstring(session, "auth-agent-req@openssh.com");
	ssh2_pkt_addbool(session, 1);	       /* want reply */
	ssh2_pkt_send(session);

	do {
	    crWaitUntilV(ispkt);
	    if (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(session);
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(session);
	    }
	} while (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (session.pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (session.pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout((session, "Unexpected response to agent forwarding request:"
			 " packet type %d", session.pktin.type));
		crReturnV;
	    }
	    logevent(session, "Agent forwarding refused");
	} else {
	    logevent(session, "Agent forwarding enabled");
	    session.ssh_agentfwd_enabled = TRUE;
	}
    }

    /*
     * Now allocate a pty for the session.
     */
    if (!cfg.nopty) {
	ssh2_pkt_init(session, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(session,session.mainchan->remoteid);	/* recipient channel */
	ssh2_pkt_addstring(session, "pty-req");
	ssh2_pkt_addbool(session, 1);	       /* want reply */
	ssh2_pkt_addstring(session, cfg.termtype);
	ssh2_pkt_adduint32(session,cols);
	ssh2_pkt_adduint32(session,rows);
	ssh2_pkt_adduint32(session,0);	       /* pixel width */
	ssh2_pkt_adduint32(session,0);	       /* pixel height */
	ssh2_pkt_addstring_start(session);
	ssh2_pkt_addstring_data(session, "\0", 1);	/* TTY_OP_END, no special options */
	ssh2_pkt_send(session);
	session.ssh_state = SSH_STATE_INTERMED;

	do {
	    crWaitUntilV(ispkt);
	    if (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(session);
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(session);
	    }
	} while (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (session.pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (session.pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout((session, "Unexpected response to pty request:"
			 " packet type %d", session.pktin.type));
		crReturnV;
	    }
	    c_write_str("Server refused to allocate pty\r\n");
	    session.ssh_editing = session.ssh_echoing = 1;
	} else {
	    logevent(session, "Allocated pty");
	}
    } else {
		session.ssh_editing = session.ssh_echoing = 1;
    }

    /*
     * Start a shell or a remote command. We may have to attempt
     * this twice if the config data has provided a second choice
     * of command.
     */
    while (1) {
	int subsys;
	char *cmd;

	if (ssh_fallback_cmd) {
	    subsys = cfg.ssh_subsys2;
	    cmd = cfg.remote_cmd_ptr2;
	} else {
	    subsys = cfg.ssh_subsys;
	    cmd = cfg.remote_cmd_ptr;
	}

	ssh2_pkt_init(session, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(session,session.mainchan->remoteid);	/* recipient channel */
	if (subsys) {
	    ssh2_pkt_addstring(session, "subsystem");
	    ssh2_pkt_addbool(session, 1);	       /* want reply */
	    ssh2_pkt_addstring(session, cmd);
	} else if (*cmd) {
	    ssh2_pkt_addstring(session, "exec");
	    ssh2_pkt_addbool(session, 1);	       /* want reply */
	    ssh2_pkt_addstring(session, cmd);
	} else {
	    ssh2_pkt_addstring(session, "shell");
	    ssh2_pkt_addbool(session, 1);	       /* want reply */
	}
	ssh2_pkt_send(session);
	do {
	    crWaitUntilV(ispkt);
	    if (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(session);
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(session);
	    }
	} while (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);
	if (session.pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (session.pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout((session, "Unexpected response to shell/command request:"
			 " packet type %d", session.pktin.type));
		crReturnV;
	    }
	    /*
	     * We failed to start the command. If this is the
	     * fallback command, we really are finished; if it's
	     * not, and if the fallback command exists, try falling
	     * back to it before complaining.
	     */
	    if (!ssh_fallback_cmd && cfg.remote_cmd_ptr2 != NULL) {
		logevent(session, "Primary command failed; attempting fallback");
		ssh_fallback_cmd = TRUE;
		continue;
	    }
	    bombout((session, "Server refused to start a shell/command"));
	    crReturnV;
	} else {
	    logevent(session, "Started a shell/command");
	}
	break;
    }

    session.ssh_state = SSH_STATE_SESSION;
    if (session.size_needed)
		ssh_size(session);
    if (session.eof_needed)
		ssh_special(session, TS_EOF);

    /*
     * Transfer data!
     */
    ldisc_send(NULL, 0, 0);	       /* cause ldisc to notice changes */
    session.ssh_send_ok = 1;
    while (1) {
	static int try_send;
	crReturnV;
	try_send = FALSE;
	if (ispkt) {
	    if (session.pktin.type == SSH2_MSG_CHANNEL_DATA ||
			session.pktin.type == SSH2_MSG_CHANNEL_EXTENDED_DATA) {
		char *data;
		int length;
		unsigned i = ssh2_pkt_getuint32(session);
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (session.pktin.type == SSH2_MSG_CHANNEL_EXTENDED_DATA &&
		    ssh2_pkt_getuint32(session) != SSH2_EXTENDED_DATA_STDERR)
		    continue;	       /* extended but not stderr */
		ssh2_pkt_getstring(session, &data, &length);
		if (data) {
		    int bufsize;
		    c->v.v2.locwindow -= length;
		    switch (c->type) {
		      case CHAN_MAINSESSION:
			bufsize =
			    from_backend(session.pktin.type ==
					 SSH2_MSG_CHANNEL_EXTENDED_DATA,
					 data, length);
			break;
		      case CHAN_X11:
			bufsize = x11_send(session, c->u.x11.s, data, length);
			break;
		      case CHAN_SOCKDATA:
			bufsize = pfd_send(session, c->u.pfd.s, data, length);
			break;
		      case CHAN_AGENT:
			while (length > 0) {
			    if (c->u.a.lensofar < 4) {
				int l = min(4 - c->u.a.lensofar, length);
				memcpy(c->u.a.msglen + c->u.a.lensofar,
				       data, l);
				data += l;
				length -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == 4) {
				c->u.a.totallen =
				    4 + GET_32BIT(c->u.a.msglen);
				c->u.a.message = (unsigned char *)smalloc(c->u.a.totallen);
				memcpy(c->u.a.message, c->u.a.msglen, 4);
			    }
			    if (c->u.a.lensofar >= 4 && length > 0) {
				int l =
				    min(c->u.a.totallen - c->u.a.lensofar,
					length);
				memcpy(c->u.a.message + c->u.a.lensofar,
				       data, l);
				data += l;
				length -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == c->u.a.totallen) {
				void *reply, *sentreply;
				int replylen;
				agent_query(c->u.a.message,
					    c->u.a.totallen, &reply,
					    &replylen);
				if (reply)
				    sentreply = reply;
				else {
				    /* Fake SSH_AGENT_FAILURE. */
				    sentreply = "\0\0\0\1\5";
				    replylen = 5;
				}
				ssh2_add_channel_data(c, (char *)sentreply,
						      replylen);
				try_send = TRUE;
				if (reply)
				    sfree(reply);
				sfree(c->u.a.message);
				c->u.a.lensofar = 0;
			    }
			}
			bufsize = 0;
			break;
		    }
		    /*
		     * If we are not buffering too much data,
		     * enlarge the window again at the remote side.
		     */
		    if (bufsize < OUR_V2_WINSIZE)
				ssh2_set_window(session, c, OUR_V2_WINSIZE - bufsize);
		}
	    } else if (session.pktin.type == SSH2_MSG_DISCONNECT) {
			session.ssh_state = SSH_STATE_CLOSED;
			logevent(session, "Received disconnect message");
			crReturnV;
			} else if (session.pktin.type == SSH2_MSG_CHANNEL_EOF) {
			unsigned i = ssh2_pkt_getuint32(session);
			struct ssh_channel *c;

			c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
			if (!c)
				continue;	       /* nonexistent channel */

			if (c->type == CHAN_X11) {
				/*
				 * Remote EOF on an X11 channel means we should
				 * wrap up and close the channel ourselves.
				 */
				x11_close(session, c->u.x11.s);
				sshfwd_close(session, c);
			} else if (c->type == CHAN_AGENT) {
				sshfwd_close(session, c);
			} else if (c->type == CHAN_SOCKDATA) {
				pfd_close(session, c->u.pfd.s);
				sshfwd_close(session, c);
			}
	    } else if (session.pktin.type == SSH2_MSG_CHANNEL_CLOSE) {
			unsigned i;
			struct ssh_channel *c;

			i = ssh2_pkt_getuint32(session);
			c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
			if (!c)
				continue;	       /* nonexistent channel */
			/* Do pre-close processing on the channel. */
			switch (c->type) {
			  case CHAN_MAINSESSION:
				break;	       /* nothing to see here, move along */
			  case CHAN_X11:
				if (c->u.x11.s != NULL)
				x11_close(session, c->u.x11.s);
				sshfwd_close(session, c);
				break;
			  case CHAN_AGENT:
				sshfwd_close(session, c);
				break;
			  case CHAN_SOCKDATA:
				if (c->u.pfd.s != NULL)
				pfd_close(session, c->u.pfd.s);
				sshfwd_close(session, c);
				break;
			}
			if (c->closes == 0) {
				ssh2_pkt_init(session, SSH2_MSG_CHANNEL_CLOSE);
				ssh2_pkt_adduint32(session,c->remoteid);
				ssh2_pkt_send(session);
			}
			del234(session.ssh_channels, c);
			bufchain_clear(&c->v.v2.outbuffer);
			sfree(c);

			/*
			 * See if that was the last channel left open.
			 */
			if (count234(session.ssh_channels) == 0) {
#if 0
                /*
                 * We used to send SSH_MSG_DISCONNECT here,
                 * because I'd believed that _every_ conforming
                 * SSH2 connection had to end with a disconnect
                 * being sent by at least one side; apparently
                 * I was wrong and it's perfectly OK to
                 * unceremoniously slam the connection shut
                 * when you're done, and indeed OpenSSH feels
                 * this is more polite than sending a
                 * DISCONNECT. So now we don't.
                 */
				logevent(session, "All channels closed. Disconnecting");
				ssh2_pkt_init(session, SSH2_MSG_DISCONNECT);
				ssh2_pkt_adduint32(session,SSH2_DISCONNECT_BY_APPLICATION);
				ssh2_pkt_addstring(session, "All open channels closed");
				ssh2_pkt_addstring(session, "en");	/* language tag */
				ssh2_pkt_send();
#endif
				session.ssh_state = SSH_STATE_CLOSED;
				crReturnV;
			}
			continue;	       /* remote sends close; ignore (FIXME) */
			} else if (session.pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
			unsigned i = ssh2_pkt_getuint32(session);
			struct ssh_channel *c;
			c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
			if (!c)
				continue;	       /* nonexistent channel */
			c->v.v2.remwindow += ssh2_pkt_getuint32(session);
			try_send = TRUE;
			} else if (session.pktin.type == SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
			unsigned i = ssh2_pkt_getuint32(session);
			struct ssh_channel *c;
			c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
			if (!c)
				continue;	       /* nonexistent channel */
			if (c->type != CHAN_SOCKDATA_DORMANT)
				continue;	       /* dunno why they're confirming this */
			c->remoteid = ssh2_pkt_getuint32(session);
			c->type = CHAN_SOCKDATA;
			c->v.v2.remwindow = ssh2_pkt_getuint32(session);
			c->v.v2.remmaxpkt = ssh2_pkt_getuint32(session);
			if (c->u.pfd.s)
				pfd_confirm(session, c->u.pfd.s);
			if (c->closes) {
				/*
				 * We have a pending close on this channel,
				 * which we decided on before the server acked
				 * the channel open. So now we know the
				 * remoteid, we can close it again.
				 */
				ssh2_pkt_init(session, SSH2_MSG_CHANNEL_CLOSE);
				ssh2_pkt_adduint32(session,c->remoteid);
				ssh2_pkt_send(session);
			}
	    } else if (session.pktin.type == SSH2_MSG_CHANNEL_OPEN_FAILURE) {
		unsigned i = ssh2_pkt_getuint32(session);
		struct ssh_channel *c;
		c = (struct ssh_channel *)find234(session.ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (c->type != CHAN_SOCKDATA_DORMANT)
		    continue;	       /* dunno why they're failing this */

		logevent(session, "Forwarded connection refused by server");

		pfd_close(session, c->u.pfd.s);
		del234(session.ssh_channels, c);
		sfree(c);
	    } else if (session.pktin.type == SSH2_MSG_CHANNEL_REQUEST) {
 		unsigned localid;
		char *type;
		int typelen, want_reply;
		struct ssh_channel *c;

		localid = ssh2_pkt_getuint32(session);
		ssh2_pkt_getstring(session, &type, &typelen);
		want_reply = ssh2_pkt_getbool(session);

		/*
		 * First, check that the channel exists. Otherwise,
		 * we can instantly disconnect with a rude message.
		 */
		c = (struct ssh_channel *)find234(session.ssh_channels, &localid, ssh_channelfind);
		if (!c) {
		    char buf[80];
		    sprintf(buf, "Received channel request for nonexistent"
			    " channel %d", localid);
		    logevent(session, buf);
		    ssh2_pkt_init(session, SSH2_MSG_DISCONNECT);
		    ssh2_pkt_adduint32(session,SSH2_DISCONNECT_BY_APPLICATION);
		    ssh2_pkt_addstring(session, buf);
		    ssh2_pkt_addstring(session, "en");	/* language tag */
		    ssh2_pkt_send(session);
		    connection_fatal(session, "%s", buf);
		    session.ssh_state = SSH_STATE_CLOSED;
		    crReturnV;
		}

		/*
		 * Having got the channel number, we now look at
		 * the request type string to see if it's something
		 * we recognise.
		 */
		if (typelen == 11 && !memcmp(type, "exit-status", 11) &&
		    c == session.mainchan) {
		    /* We recognise "exit-status" on the primary channel. */
		    char buf[100];
		    session.ssh_exitcode = ssh2_pkt_getuint32(session);
		    sprintf(buf, "Server sent command exit status %d",
			    session.ssh_exitcode);
		    logevent(session, buf);
		    if (want_reply) {
			ssh2_pkt_init(session, SSH2_MSG_CHANNEL_SUCCESS);
			ssh2_pkt_adduint32(session,c->remoteid);
			ssh2_pkt_send(session);
		    }
		} else {
		    /*
		     * This is a channel request we don't know
		     * about, so we now either ignore the request
		     * or respond with CHANNEL_FAILURE, depending
		     * on want_reply.
		     */
		    if (want_reply) {
			ssh2_pkt_init(session, SSH2_MSG_CHANNEL_FAILURE);
			ssh2_pkt_adduint32(session,c->remoteid);
			ssh2_pkt_send(session);
		    }
		}
	    } else if (session.pktin.type == SSH2_MSG_GLOBAL_REQUEST) {
		char *type;
		int typelen, want_reply;

		ssh2_pkt_getstring(session, &type, &typelen);
		want_reply = ssh2_pkt_getbool(session);

                /*
                 * We currently don't support any global requests
                 * at all, so we either ignore the request or
                 * respond with REQUEST_FAILURE, depending on
                 * want_reply.
                 */
                if (want_reply) {
                    ssh2_pkt_init(session, SSH2_MSG_REQUEST_FAILURE);
                    ssh2_pkt_send(session);
		}
	    } else if (session.pktin.type == SSH2_MSG_CHANNEL_OPEN) {
		char *type;
		int typelen;
		char *error = NULL;
		struct ssh_channel *c;
		unsigned remid, winsize, pktsize;
		ssh2_pkt_getstring(session, &type, &typelen);
		c = (struct ssh_channel *)smalloc(sizeof(struct ssh_channel));

		remid = ssh2_pkt_getuint32(session);
		winsize = ssh2_pkt_getuint32(session);
		pktsize = ssh2_pkt_getuint32(session);

		if (typelen == 3 && !memcmp(type, "x11", 3)) {
		    if (!session.ssh_X11_fwd_enabled)
			error = "X11 forwarding is not enabled";
		    else if (x11_init(session, &c->u.x11.s, cfg.x11_display, c) !=
			     NULL) {
			error = "Unable to open an X11 connection";
		    } else {
			c->type = CHAN_X11;
		    }
		} else if (typelen == 15 &&
			   !memcmp(type, "forwarded-tcpip", 15)) {
		    struct ssh_rportfwd pf, *realpf;
		    char *dummy;
		    int dummylen;
		    ssh2_pkt_getstring(session, &dummy, &dummylen);/* skip address */
		    pf.sport = ssh2_pkt_getuint32(session);
		    realpf = (struct ssh_rportfwd *)find234(session.ssh_rportfwds, &pf, NULL);
		    if (realpf == NULL) {
			error = "Remote port is not recognised";
		    } else {
			char *e = pfd_newconnect(session, &c->u.pfd.s, realpf->dhost,
						 realpf->dport, c);
			char buf[1024];
			sprintf(buf, "Received remote port open request for %s:%d",
				realpf->dhost, realpf->dport);
			logevent(session, buf);
			if (e != NULL) {
			    sprintf(buf, "Port open failed: %s", e);
			    logevent(session, buf);
			    error = "Port open failed";
			} else {
			    logevent(session, "Forwarded port opened successfully");
			    c->type = CHAN_SOCKDATA;
			}
		    }
		} else if (typelen == 22 &&
			   !memcmp(type, "auth-agent@openssh.com", 3)) {
		    if (!session.ssh_agentfwd_enabled)
				error = "Agent forwarding is not enabled";
		    else {
				c->type = CHAN_AGENT;	/* identify channel type */
				c->u.a.lensofar = 0;
		    }
		} else {
		    error = "Unsupported channel type requested";
		}

		c->remoteid = remid;
		if (error) {
		    ssh2_pkt_init(session, SSH2_MSG_CHANNEL_OPEN_FAILURE);
		    ssh2_pkt_adduint32(session,c->remoteid);
		    ssh2_pkt_adduint32(session,SSH2_OPEN_CONNECT_FAILED);
		    ssh2_pkt_addstring(session, error);
		    ssh2_pkt_addstring(session, "en");	/* language tag */
		    ssh2_pkt_send(session);
		    sfree(c);
		} else {
		    c->localid = alloc_channel_id(session);
		    c->closes = 0;
		    c->v.v2.locwindow = OUR_V2_WINSIZE;
		    c->v.v2.remwindow = winsize;
		    c->v.v2.remmaxpkt = pktsize;
		    bufchain_init(&c->v.v2.outbuffer);
		    add234(session.ssh_channels, c);
		    ssh2_pkt_init(session, SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
		    ssh2_pkt_adduint32(session,c->remoteid);
		    ssh2_pkt_adduint32(session,c->localid);
		    ssh2_pkt_adduint32(session,c->v.v2.locwindow);
		    ssh2_pkt_adduint32(session,0x4000UL);	/* our max pkt size */
		    ssh2_pkt_send(session);
		}
	    } else {
		bombout((session, "Strange packet received: type %d", session.pktin.type));
		crReturnV;
	    }
	} else {
	    /*
	     * We have spare data. Add it to the channel buffer.
	     */
	    ssh2_add_channel_data(session.mainchan, (char *)in, inlen);
	    try_send = TRUE;
	}
	if (try_send) {
	    int i;
	    struct ssh_channel *c;
	    /*
	     * Try to send data on all channels if we can.
	     */
	    for (i = 0; NULL != (c = (struct ssh_channel *)index234(session.ssh_channels, i)); i++) {
		int bufsize = ssh2_try_send(session, c);
		if (bufsize == 0) {
		    switch (c->type) {
		      case CHAN_MAINSESSION:
			/* stdin need not receive an unthrottle
			 * notification since it will be polled */
			break;
		      case CHAN_X11:
			x11_unthrottle(session, c->u.x11.s);
			break;
		      case CHAN_AGENT:
			/* agent sockets are request/response and need no
			 * buffer management */
			break;
		      case CHAN_SOCKDATA:
			pfd_unthrottle(session, c->u.pfd.s);
			break;
		    }
		}
	    }
	}
    }

    crFinishV;
}

/*
 * Handle the top-level SSH2 protocol.
 */
static void ssh2_protocol(CSshSession &session, unsigned char *in, int inlen, int ispkt, int &crLine)
{
    if (do_ssh2_transport(session, in, inlen, ispkt, session.n_do_ssh2_transport) == 0)
		return;
    do_ssh2_authconn(session, in, inlen, ispkt, session.n_do_ssh2_authconn);
}

/*
 * Called to set up the connection.
 *
 * Returns an error message, or NULL on success.
 */
static char *ssh_init(CSshSession &session, char *host, int port, char **realhost, int nodelay)
{
    char *p;

#ifdef MSCRYPTOAPI
    if (crypto_startup() == 0)
	return "Microsoft high encryption pack not installed!";
#endif

    session.ssh_send_ok = 0;
    session.ssh_editing = 0;
    session.ssh_echoing = 0;
    ssh1_throttle_count = 0;
    ssh_overall_bufsize = 0;
    ssh_fallback_cmd = 0;

    p = connect_to_host(session, host, port, realhost, nodelay);
    if (p != NULL)
		return p;

    return NULL;
}

/*
 * Called to send data down the Telnet connection.
 */
static int ssh_send(CSshSession &session, char *buf, int len)
{
    if (session.s == NULL || ssh_protocol == NULL)
		return 0;

    ssh_protocol(session, (unsigned char *)buf, len, 0, session.n_ssh_protocol);

    return ssh_sendbuffer(session);
}

/*
 * Called to query the current amount of buffered stdin data.
 */
static int ssh_sendbuffer(CSshSession &session)
{
    int override_value;

    if (session.s == NULL || ssh_protocol == NULL)
		return 0;

    /*
     * If the SSH socket itself has backed up, add the total backup
     * size on that to any individual buffer on the stdin channel.
     */
    override_value = 0;
    if (ssh_throttled_all)
		override_value = ssh_overall_bufsize;

    if (ssh_version == 1) {
	return override_value;
    } else if (ssh_version == 2) {
	if (!session.mainchan || session.mainchan->closes > 0)
	    return override_value;
	else
	    return override_value + bufchain_size(&session.mainchan->v.v2.outbuffer);
    }

    return 0;
}

/*
 * Called to set the size of the window from SSH's POV.
 */
static void ssh_size(CSshSession &session)
{
    switch (session.ssh_state) {
      case SSH_STATE_BEFORE_SIZE:
      case SSH_STATE_PREPACKET:
      case SSH_STATE_CLOSED:
	break;			       /* do nothing */
      case SSH_STATE_INTERMED:
	session.size_needed = TRUE;	       /* buffer for later */
	break;
      case SSH_STATE_SESSION:
	if (!cfg.nopty) {
	    if (ssh_version == 1) {
		send_packet(session, SSH1_CMSG_WINDOW_SIZE,
			    PKT_INT, rows, PKT_INT, cols,
			    PKT_INT, 0, PKT_INT, 0, PKT_END);
	    } else {
		ssh2_pkt_init(session, SSH2_MSG_CHANNEL_REQUEST);
		ssh2_pkt_adduint32(session,session.mainchan->remoteid);
		ssh2_pkt_addstring(session, "window-change");
		ssh2_pkt_addbool(session, 0);
		ssh2_pkt_adduint32(session,cols);
		ssh2_pkt_adduint32(session,rows);
		ssh2_pkt_adduint32(session,0);
		ssh2_pkt_adduint32(session,0);
		ssh2_pkt_send(session);
	    }
	}
	break;
    }
}

/*
 * Send Telnet special codes. TS_EOF is useful for `plink', so you
 * can send an EOF and collect resulting output (e.g. `plink
 * hostname sort').
 */
static void ssh_special(CSshSession &session, Telnet_Special code)
{
    if (code == TS_EOF) {
	if (session.ssh_state != SSH_STATE_SESSION) {
	    /*
	     * Buffer the EOF in case we are pre-SESSION, so we can
	     * send it as soon as we reach SESSION.
	     */
	    if (code == TS_EOF)
			session.eof_needed = TRUE;
	    return;
	}
	if (ssh_version == 1) {
	    send_packet(session, SSH1_CMSG_EOF, PKT_END);
	} else {
	    ssh2_pkt_init(session, SSH2_MSG_CHANNEL_EOF);
	    ssh2_pkt_adduint32(session,session.mainchan->remoteid);
	    ssh2_pkt_send(session);
	}
	logevent(session, "Sent EOF message");
    } else if (code == TS_PING) {
	if (session.ssh_state == SSH_STATE_CLOSED
	    || session.ssh_state == SSH_STATE_PREPACKET) return;
	if (ssh_version == 1) {
	    send_packet(session, SSH1_MSG_IGNORE, PKT_STR, "", PKT_END);
	} else {
	    ssh2_pkt_init(session, SSH2_MSG_IGNORE);
	    ssh2_pkt_addstring_start(session);
	    ssh2_pkt_send(session);
	}
    } else {
	/* do nothing */
    }
}

void *new_sock_channel(CSshSession &session, Socket s)
{
    struct ssh_channel *c;
    c = (struct ssh_channel *)smalloc(sizeof(struct ssh_channel));

    if (c) {
	c->remoteid = -1;	       /* to be set when open confirmed */
	c->localid = alloc_channel_id(session);
	c->closes = 0;
	c->type = CHAN_SOCKDATA_DORMANT;/* identify channel type */
	c->u.pfd.s = s;
	bufchain_init(&c->v.v2.outbuffer);
	add234(session.ssh_channels, c);
    }
    return c;
}

/*
 * This is called when stdout/stderr (the entity to which
 * from_backend sends data) manages to clear some backlog.
 */
void ssh_unthrottle(CSshSession &session, int bufsize)
{
    if (ssh_version == 1) {
	if (ssh1_stdout_throttling && bufsize < SSH1_BUFFER_LIMIT) {
	    ssh1_stdout_throttling = 0;
	    ssh1_throttle(session, -1);
	}
    } else {
	if (session.mainchan && session.mainchan->closes == 0)
	    ssh2_set_window(session, session.mainchan, OUR_V2_WINSIZE - bufsize);
    }
}

void ssh_send_port_open(CSshSession &session, void *channel, char *hostname, int port, char *org)
{
    struct ssh_channel *c = (struct ssh_channel *)channel;
    char buf[1024];

    sprintf(buf, "Opening forwarded connection to %.512s:%d", hostname, port);
    logevent(session, buf);

    if (ssh_version == 1) {
	send_packet(session, SSH1_MSG_PORT_OPEN,
		    PKT_INT, c->localid,
		    PKT_STR, hostname,
		    PKT_INT, port,
		    //PKT_STR, <org:orgport>,
		    PKT_END);
    } else {
	ssh2_pkt_init(session, SSH2_MSG_CHANNEL_OPEN);
	ssh2_pkt_addstring(session, "direct-tcpip");
	ssh2_pkt_adduint32(session,c->localid);
	c->v.v2.locwindow = OUR_V2_WINSIZE;
	ssh2_pkt_adduint32(session,c->v.v2.locwindow);/* our window size */
	ssh2_pkt_adduint32(session,0x4000UL);      /* our max pkt size */
	ssh2_pkt_addstring(session, hostname);
	ssh2_pkt_adduint32(session,port);
	/*
	 * We make up values for the originator data; partly it's
	 * too much hassle to keep track, and partly I'm not
	 * convinced the server should be told details like that
	 * about my local network configuration.
	 */
	ssh2_pkt_addstring(session, "client-side-connection");
	ssh2_pkt_adduint32(session,0);
	ssh2_pkt_send(session);
    }
}


static Socket ssh_socket(CSshSession &session)
{
    return session.s;
}

static int ssh_sendok(CSshSession &session)
{
    return session.ssh_send_ok;
}

static int ssh_ldisc(CSshSession &session, int option)
{
    if (option == LD_ECHO)
		return session.ssh_echoing;
    if (option == LD_EDIT)
		return session.ssh_editing;
    return FALSE;
}

static int ssh_return_exitcode(CSshSession &session)
{
    return session.ssh_exitcode;
}

Backend ssh_backend = {
    ssh_init,
    ssh_send,
    ssh_sendbuffer,
    ssh_size,
    ssh_special,
    ssh_socket,
    ssh_return_exitcode,
    ssh_sendok,
    ssh_ldisc,
    ssh_unthrottle,
    22
};
