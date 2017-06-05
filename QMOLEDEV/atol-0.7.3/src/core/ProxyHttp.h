////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef PROXYHTTP_H
#define PROXYHTTP_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "ProxyLayer.h"
#include "String.h"

class CProxyHttp : public CProxyLayer  
{
public:
	CProxyHttp();
	virtual ~CProxyHttp();

	virtual SOCKET Connect(const char *szHost = NULL, unsigned short nPort = 21);
	virtual bool   ForcePassive(){ return true; };
	virtual bool   Login(SOCKET hSock);
	virtual int    GetProxyType(){ return PROXY_HTTP; }

#define PROXY_SOCKS	3

protected:
	std::string	m_strPeerHost;
	int		m_nPeerPort;
};

#endif // PROXYHTTP_H
