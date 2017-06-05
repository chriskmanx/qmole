////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "SshSession.h"

static struct Packet pktNull = { 0, 0, NULL, NULL, 0 };

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSshSession::CSshSession() 
{
	s = NULL;
	cipher = NULL;
	cscipher = NULL;
	sccipher = NULL;
	csmac = NULL;
	scmac = NULL;
	cscomp = NULL;
	sccomp = NULL;
	kex = NULL;
	hostkey = NULL;
	ssh_get_line = NULL;
	ssh_exitcode = -1;
	ssh_state = SSH_STATE_PREPACKET;
	size_needed = FALSE;
	eof_needed = FALSE;
	deferred_send_data = NULL;
	deferred_len = 0;
	deferred_size = 0;
	pktin  = pktNull;
	pktout = pktNull;
	ssh_fallback_cmd = 0;
	ssh_pkt_ctx = 0;
	ssh_protocol = NULL;
	s_rdpkt = NULL; //TOFIX?

	ssh_channels = NULL;   /* indexed by local id */
	mainchan	 = NULL;   /* primary session channel */
	ssh_exitcode = -1;
	ssh_rportfwds = NULL;
	sktree = NULL;

	p = NULL;
	g = NULL;
	e = NULL;
	f = NULL;
	K = NULL;


	SHA_Init(&sha1_cs_mac_s1);
	SHA_Init(&sha1_cs_mac_s2);
	SHA_Init(&sha1_sc_mac_s1);
	SHA_Init(&sha1_sc_mac_s2);
	MD5Init(&md5_cs_mac_s1);
	MD5Init(&md5_cs_mac_s2);
	MD5Init(&md5_sc_mac_s1);
	MD5Init(&md5_sc_mac_s2);

	pwd = NULL;
	homedir = NULL;
	outgoing_sequence = 0;

    str	= NULL;
    maclist = NULL;
    cscipher_tobe = NULL;
    sccipher_tobe = NULL;
    csmac_tobe = NULL;
    scmac_tobe = NULL;
    cscomp_tobe = NULL;
    sccomp_tobe = NULL;
    hostkeydata	= NULL;
	sigdata	= NULL;
    hkey	= NULL;		       /* actual host key */
    preferred_comp = NULL;;

	n_s_rdpkt = 0;
	n_do_ssh_init = 0;
	n_ssh_gotdata = 0;
	n_do_ssh1_login = 0;
	n_ssh_protocol = 0;		//one for both protocols
	n_do_ssh2_transport = 0;
	n_do_ssh2_authconn = 0;

	m_pfnProgress = NULL;
	m_data	= 0;
	m_dbgFn = NULL;
	m_dwDbgData	= 0;
	m_fnLogin = NULL;
	m_fnLoginData	= 0;
	m_fnKeyWarning = NULL;
	m_fnKeyData = 0;
	m_password[0] = '\0';

	back = NULL;
	savedhost = NULL;
}

CSshSession::~CSshSession()
{
	n_s_rdpkt = 0;
	n_do_ssh_init = 0;
	n_ssh_gotdata = 0;
	n_do_ssh1_login = 0;
	n_ssh_protocol = 0;		//one for both protocols
	n_do_ssh2_transport = 0;
	n_do_ssh2_authconn = 0;

//	if(ssh_channels)
//		sfree(ssh_channels);
//	if(ssh_rportfwds)
//		sfree(ssh_rportfwds);
	if(pktin.data)
		sfree(pktin.data);
	if(pktout.data)
		sfree(pktout.data);
	if(pwd)
		sfree(pwd);
	if(homedir)
		sfree(homedir);
	if(savedhost)
		sfree(savedhost);
//	if(p)
//		freebn(p);
//	if(g)
//		freebn(g);
	if(e)
		freebn(e);
	if(f)
		freebn(f);
	if(K)
		freebn(K);

	if(sktree){
		freetree234(sktree);
		sktree = NULL;
	}
	if(ssh_rportfwds){
		freetree234(ssh_rportfwds);
		ssh_rportfwds = NULL;
	}
	if(ssh_channels){
		freetree234(ssh_channels);
		ssh_channels = NULL;
	}
}
