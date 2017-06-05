////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: wrapper for SSL context (using OpenSSL library)
////////////////////////////////////////////////////////////////////////////

#include "xSSLContext.h"

static int password_callback(char *buf,int num, int rwflag, void *userdata);

xSSLContext::xSSLContext()
{
	m_ctx = NULL;
	m_nMethod = CFtpInfo::ENCRYPT_NONE;
}

xSSLContext::~xSSLContext()
{
	Destroy();	// just in case
}

bool xSSLContext::Create(CFtpInfo::FtpEncryptionType nMethod)
{
	Destroy();	// just in case

	//create SSL context using selected protocol type
	SSL_METHOD *meth = NULL;

	switch(nMethod){
		case CFtpInfo::ENCRYPT_SSLv3:
			meth = SSLv3_method();
			break;
		case CFtpInfo::ENCRYPT_SSLv2:
			meth = SSLv2_method();
			break;
		case CFtpInfo::ENCRYPT_TLSv1:
			meth = TLSv1_method();
 			break;
		default:
			return false;
	}
	
	if(meth)
	{
		m_ctx = SSL_CTX_new (meth);
		if(m_ctx)
			SSL_CTX_set_options(m_ctx, SSL_OP_ALL);	// use all known bug workarounds
	}

	return IsValid();
}

void xSSLContext::Destroy()
{
	if(IsValid())
	{
		SSL_CTX_free(m_ctx);
		m_ctx = NULL;
	}
}

bool xSSLContext::LoadKey(const char *szCertFile, const char *szPrivKeyFile, const char *szPass, int nType)
{
	if(szPass && strlen(szPass)){
		m_strPass = szPass;	//store password
		SSL_CTX_set_default_passwd_cb(m_ctx, password_callback);
		SSL_CTX_set_default_passwd_cb_userdata(m_ctx, this);
	}

	if(szCertFile && strlen(szCertFile))
		if (SSL_CTX_use_certificate_file(m_ctx, szCertFile, nType) <= 0)
			return false;

	if(szPrivKeyFile && strlen(szPrivKeyFile))
		if (SSL_CTX_use_PrivateKey_file(m_ctx, szPrivKeyFile, nType) <= 0)
			return false;

	if( szCertFile && strlen(szCertFile) &&	szPrivKeyFile && strlen(szPrivKeyFile))
		if (!SSL_CTX_check_private_key(m_ctx))
			return false;	//Private key does not match the certificate public key

	return true;
}

bool xSSLContext::GenerateTmpKey()
{
	SSL_CTX_set_cipher_list(m_ctx, "ADH");
/*
	RSA *rsa;

	rsa=RSA_generate_key(512,RSA_F4,NULL,NULL);

	if (!SSL_CTX_set_tmp_rsa(m_ctx,rsa))
		return false;	//berr_exit("Couldn't set RSA key");

	RSA_free(rsa);
*/
	return true;
 } 

// WARNING: password code might not be thread-safe
static int password_callback(char *buf,int num, int rwflag, void *userdata)
{
	xSSLContext *pCtx = (xSSLContext *)userdata;
	if(pCtx)
	{
		int nLen = strlen(pCtx->m_strPass.c_str());
		if(num < nLen+1)
		  return (0);

		strcpy(buf, pCtx->m_strPass.c_str());
		return nLen;
	}

	return 0;
}
