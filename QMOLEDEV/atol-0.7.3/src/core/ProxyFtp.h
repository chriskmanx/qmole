////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef PROXYFTP_H
#define PROXYFTP_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifdef _WIN32
 #pragma warning(disable: 4786)
#endif 

#include "ProxyLayer.h"
#include "String.h"
#include "_ftp/xSocket.h"
#include <vector>

class CProxyFtp : public CProxyLayer  
{
public:
	CProxyFtp();
	virtual ~CProxyFtp();

	virtual SOCKET Connect(const char *szHost = NULL, unsigned short nPort = 21);
	virtual bool   ForcePassive(){ return true; };
	virtual bool   Login(SOCKET hSock);
	virtual bool   NeedLogin(){ return false; };	//next layer must log itself ?

	virtual int  GetProxyType(){ return PROXY_FTP; }

	int m_nFtpProxyType;	//TOFIX SetProxyType

protected:
	bool Login(SOCKET hSock, tProxyLoginData &data, std::string acct, int logontype);
	bool SendCmd(xSocket &socket, const char *szCmd);
	int  ReadReply(xSocket &socket);
	int  GetReplyDigit();
	bool ParseBuffer(int &nStart);

	char	m_szBuffer[512];
	std::vector<std::string> m_list;
};

#endif // PROXYFTP_H
