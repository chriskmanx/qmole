////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef PROXYSOCKS_H_
#define PROXYSOCKS_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "ProxyLayer.h"
#include "String.h"

//TOFIX Socks5 UDP support missing
//TOFIX Bind option (reuse authentication code)
//TOFIX Identd support!
//TOFIX Accept code!!!
//TOFIX error handling
//interface for bind (return listening socket data)

//socks versions (types) supported
#define SOCKS_V5	1
#define SOCKS_V4A	2
#define SOCKS_V4	3

//socks operation codes
#define SOCKS_OP_CONNECT	1	//all SOCKS versions
#define SOCKS_OP_BIND		2	//all SOCKS versions 
#define SOCKS_OP_UDP_ASSOC	3	//SOCKS v5 only

#define SOCKS5_AUTH_NONE		0
#define SOCKS5_AUTH_PASSWORD	2

#define SOCKS5_ADDR_IP4			1
#define SOCKS5_ADDR_NAME		3
#define SOCKS5_ADDR_IP6			4

class CProxySocks : public CProxyLayer
{
public:
	CProxySocks();
	virtual ~CProxySocks();

	// CProxyLayer interface
	virtual SOCKET Connect(const char *szHost = NULL, unsigned short nPort = 21);
	virtual bool   Login(SOCKET hSock);
	virtual SOCKET Listen(String &strIP, unsigned short &uPort);
	virtual bool   Accept(SOCKET hSock, String &strIP, unsigned short &uPort);

	//TOFIX Socks should work witout PASV, but all the proxies I tested did not suport that 
	virtual bool   ForcePassive(){ return true; };
	virtual int	   GetProxyType(){ return PROXY_SOCKS; }

	//
	int		GetProxyVer() { return m_nSocksVer; }
	void	SetProxyVer(int version){ m_nSocksVer = version; };

	static bool ResolveToIP(const char *szHost, unsigned long &uIP);

protected:
	bool	Authenticate(SOCKET hSock);
	bool	Authenticate4(SOCKET hSock);
	bool	Authenticate5(SOCKET hSock);

	bool	Op_Connect(SOCKET hSock, const char *szHost, int nPort);
	bool	Op_Bind(SOCKET hSock, String &strIP, unsigned short &uPort);
	bool	Op_Accept(SOCKET hSock, String &strIP, unsigned short &uPort);

	bool	Op_Connect4(SOCKET hSock, const char *szHost, int nPort);
	bool	Op_Connect5(SOCKET hSock, const char *szHost, int nPort);
	bool	Op_Bind4(SOCKET hSock, String &strIP, unsigned short &uPort);
	bool	Op_Bind5(SOCKET hSock, String &strIP, unsigned short &uPort);

	bool	ProxyReply(SOCKET hSock);
	void	ReportSocksError(unsigned char nCode);
	void	ExtractAddress(String &strIP, unsigned short &uPort);

protected:
	int		m_nSocksVer;	//socks protocol version (see defines above)
	char	m_szBuffer[256];
	int		m_nBufferPos;
	char	m_szOutBuffer[256];
};

#endif // PROXYSOCKS_H_
