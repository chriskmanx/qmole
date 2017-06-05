////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef CONNECTIONINFO_H
#define CONNECTIONINFO_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "String.h"

class CProxyInfo
{
public:
    CProxyInfo(){};
    CProxyInfo(const CProxyInfo &a){ operator=(a); };

    void operator =(const CProxyInfo &a)
    {
        if(this == &a)
            return;

        //TOFIX what data to keep?
    }
};

#define PROT_NONE	0
#define PROT_FTP	1
#define PROT_SFTP	2	//ssh2
//#define PROT_SCP	3	//ssh1
//#define PROT_FTP_SSH	4 //ftp over ssh

class CConnectionInfo
{
public:
    CConnectionInfo(){ m_uPort = 21; m_dwProtocol = PROT_NONE; };
    CConnectionInfo(const CConnectionInfo &a){ operator=(a); };

    void operator =(const CConnectionInfo &a)
    {
        if(this == &a)
            return;

		m_dwProtocol	= a.m_dwProtocol;
        m_strTitle	    = a.m_strTitle;
        m_strHost		= a.m_strHost;
        m_strUser		= a.m_strUser;
        m_strPassword   = a.m_strPassword;
        m_strRemoteDir  = a.m_strRemoteDir;
        m_strLocalDir   = a.m_strLocalDir;
        m_uPort         = a.m_uPort;
        m_bUseFirewall  = a.m_bUseFirewall;
        m_proxy         = a.m_proxy;
		m_strDesc		= a.m_strDesc;
    }

    //used to sort list by name
    bool operator <(const CConnectionInfo &a)
    {
        return (-1 == strcmp(m_strTitle, a.m_strTitle));
    }

    //used for search by name
    bool operator ==(const CConnectionInfo &a){ return (m_strTitle == a.m_strTitle); }

public:
	int	       m_dwProtocol;	//FTP, SFTP, ...
	String     m_strTitle;		//descriptive name for connection
	String     m_strHost;		//host name or address string
	String     m_strUser;		//user name
	String     m_strPassword;   //password
	unsigned short m_uPort;     //port number (default FTP port is 21)
	String     m_strRemoteDir;  //remote directory to set after connecting
	String     m_strLocalDir;   //local directory to set after connecting
	//String   m_strSendCmds;   //after logon sent these commands
	bool       m_bUseFirewall;  //use proxy settings
	CProxyInfo m_proxy;         //proxy settings
	String     m_strDesc;       //site description
};

#endif // CONNECTIONINFO_H
