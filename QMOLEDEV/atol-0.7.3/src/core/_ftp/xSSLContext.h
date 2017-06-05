////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: wrapper for SSL context (using OpenSSL library)
////////////////////////////////////////////////////////////////////////////

#ifndef XSSLCONTEXT_H__
#define XSSLCONTEXT_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <string>
#include <openssl/ssl.h>

#include "../FtpInfo.h"

class xSSLContext  
{
public:
	xSSLContext();
	virtual ~xSSLContext();

	bool IsValid()			{ return (NULL != m_ctx); }
	int  GetMethod()		{ return m_nMethod; }
	SSL_CTX	*GetHandle()	{ return m_ctx; }

	bool Create(CFtpInfo::FtpEncryptionType nMethod = CFtpInfo::ENCRYPT_TLSv1);
	void Destroy();

	bool LoadKey(const char *szCertFile, const char *szPrivKeyFile, const char *szPass, int nType = SSL_FILETYPE_PEM);
	bool GenerateTmpKey();

public:
	std::string m_strPass;	//pass to decrypt private key

protected:
	int m_nMethod;
	SSL_CTX	*m_ctx;
};

#endif // XSSLCONTEXT_H__
