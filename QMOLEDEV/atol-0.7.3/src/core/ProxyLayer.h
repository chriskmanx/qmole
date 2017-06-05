////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef PROXYLAYER_H_
#define PROXYLAYER_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifdef _WIN32
 #include <winsock2.h>
 #include <windows.h>
#endif

#ifndef SOCKET
 #define SOCKET int
#endif
#ifndef INVALID_SOCKET
 #define INVALID_SOCKET -1
#endif
#ifndef SOCKET_ERROR
 #define SOCKET_ERROR   -1
#endif

#include <string>

//TOFIX list all proxy types?, use this ids in the list?
#define LAYER_PROXY_SOCKS	1
#define LAYER_PROXY_FTP		2

//trace type info
#define	 TT_COMMAND	1
#define	 TT_REPLY	2
#define  TT_INFO	3
#define  TT_ERROR	4	//TOFIX reply can be error?

//parameters: trace text, trace type, user data
typedef void (* TRACE_FN)(const char *, int, unsigned long);

typedef struct {
	//data on proxy
	std::string strProxyHost;
	std::string strProxyUser;
	std::string strProxyPass;
	unsigned int nProxyPort;
	bool nProxyPassive;

	//data on connection that is to be made through proxy
	std::string strPeerHost;
	std::string strPeerUser;
	std::string strPeerPass;
	unsigned int nPeerPort;
} tProxyLoginData;

#define PROXY_FTP	1
#define PROXY_HTTP	2
#define PROXY_SOCKS	3

//TOFIX listen and accept must both return some result on success ((ip,port)/(name,port))
class CProxyLayer  
{
public:
	CProxyLayer();
	virtual ~CProxyLayer();
	
	//tracing support
	void SetTraceCallback(TRACE_FN pfTrace, unsigned long dwData);
	void Trace(int nType, const char *fmt, ...);
		
	//layer management
	CProxyLayer *GetNextLayer() const { return m_pNextLayer; }
	virtual bool AddNextLayer(CProxyLayer *pLayer);
	virtual int  GetProxyType(){ return 0; }

	//
	void SetData(tProxyLoginData &data){ m_data = data; }

	//socket operations/events to handle
	virtual SOCKET Connect(const char *szHost = NULL, unsigned short nPort = 21) = 0;
	virtual bool Login(SOCKET hSock){ return false; };
	virtual SOCKET Listen(std::string &strIP, unsigned short &uPort){ return INVALID_SOCKET; };
	virtual bool Accept(SOCKET hSock, std::string &strIP, unsigned short &uPort){ return false; };

	virtual bool CanListen(){ return false; };
	virtual bool CanAccept(){ return false; };
	virtual bool ForcePassive(){ return m_data.nProxyPassive; };
	virtual bool NeedLogin(){ return true; };	//next layer must log itself ?

	std::string	GetIP()		{ return m_strIP; }
	unsigned short	GetPort()	{ return m_nPort; }

	tProxyLoginData  m_data;

protected:
	CProxyLayer *m_pNextLayer;	//show next layer in chain

	//proxy socket data - result of Listen/Accept
	std::string	m_strIP;
	unsigned short	m_nPort;

private:
	//trace callback
	TRACE_FN	m_pfnTrace;
	unsigned long		m_dwTraceData;
};

#endif // 
