////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: SSL socket operations (using OpenSSL library)
////////////////////////////////////////////////////////////////////////////

#ifndef XSSLSOCKET_H__
#define XSSLSOCKET_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "xSocket.h"
#include "xSSLContext.h"

//TOFIX rename to xSSLSession ?
//TOFIX copy constructor
//TOFIX function to check client certificate
//TOFIX install cert password callback
//TOFIX load trusted CAs list
//TOFIX dynamic certificate (anonymous DH)

typedef struct {
	char subject[256];
	char issuer[256];
} t_CertInfo;

class xSSLSocket : public xSocket  
{
public:
	xSSLSocket();
	~xSSLSocket();

	static void InitSSLLib();
	static void CleanupSSLLib();
	static std::string GetSSLError(int nError);

	SSL	*GetSession(){	return m_ssl; }

	bool Open(unsigned short nLocalPort = 0);
	void Attach(SOCKET hSock);
	void Attach(xSocket &sock) { Attach(sock.Detach()); }
	void Close();

	bool GetPeerCertificate(t_CertInfo &info);

	bool Connect(xSSLContext &context, const char *szHost, unsigned short nRemotePort);
	bool SSLHandshake(xSSLContext &context, bool bAccept = false);
	int	 Read(char *szBuffer, int nLen);
	int  Write(const char *szBuffer, int nLen);
	virtual int Read(char *buffer, int len, int nTimeout);
	virtual int Write(const char *buffer, int len, int nTimeout);

protected:
	SSL	*m_ssl;
};

#endif // XSSLSOCKET_H__
