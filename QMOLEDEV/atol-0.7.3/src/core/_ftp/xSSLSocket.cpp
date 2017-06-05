////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: SSL socket operations (using OpenSSL library)
////////////////////////////////////////////////////////////////////////////

#include "xSSLSocket.h"

#ifdef _WIN32
 #include <windows.h>
 #pragma comment(lib, "libeay32")	//automatical linking
 #pragma comment(lib, "ssleay32")	//automatical linking
#endif

//additional SSL headers
#include <openssl/crypto.h>
#include <openssl/x509.h>
#include <openssl/pem.h>
#include <openssl/err.h>
#include <openssl/rand.h>

xSSLSocket::xSSLSocket()
{
	m_ssl = NULL;
}

xSSLSocket::~xSSLSocket()
{
	Close();	//just in case
}

void xSSLSocket::InitSSLLib()
{
	//xSocket::InitSocketLib();

	SSL_library_init();
	SSLeay_add_ssl_algorithms();
	SSL_load_error_strings();
	ERR_load_crypto_strings();
	ERR_load_SSL_strings();
	//~ RAND_screen();	//initialize random number generator
}

void xSSLSocket::CleanupSSLLib()
{
	//~ xSocket::CleanupSocketLib();
}

std::string xSSLSocket::GetSSLError(int nError)
{
	char szError[1024];
    ERR_error_string(nError, szError);
	return std::string(szError);
}

bool xSSLSocket::Open(unsigned short nLocalPort)
{
	//create and bind socket to the local interface and port
	if(!xSocket::Open(nLocalPort))
		return false;

	Attach(m_hSocket);
	return true;
}

void xSSLSocket::Attach(SOCKET hSock)
{
	xSocket::Attach(hSock);
}

bool xSSLSocket::Connect(xSSLContext &context, const char *szHost, unsigned short nRemotePort)
{
	//connect socket
	if(!xSocket::Connect(szHost, nRemotePort))
		return false;

	return SSLHandshake(context);
}

bool xSSLSocket::SSLHandshake(xSSLContext &context, bool bAccept)
{
	//create SSL session and attach socket to it
	m_ssl = SSL_new (context.GetHandle());
	if(!m_ssl){
		xSocket::Close();
		return false;
	}
	SSL_set_fd (m_ssl, m_hSocket);
	SSL_clear(m_ssl);

	//~ printf("Check1: %s\n", ERR_error_string(ERR_get_error(), NULL));

	//SSL handshake
	int err, nRet;

	while(1){
		nRet = (bAccept)? SSL_accept(m_ssl) : SSL_connect(m_ssl);
		//~ printf("Check2: %s\n", ERR_error_string(ERR_get_error(), NULL));
		if(nRet <= 0)
		{
			err = SSL_get_error(m_ssl, nRet);
			switch(err){
				case SSL_ERROR_NONE:
					break;
				default:
					return false;
					break;
			}
		}
		else
			break;
	}

	return true;
}

void xSSLSocket::Close()
{
	if(m_ssl)  // send SSL/TLS close_notify
	{
		int r = SSL_shutdown(m_ssl);
		if(!r){
		  // If we called SSL_shutdown() first then we always get return value of '0'. In
		  // this case, try again, but first send a TCP FIN to trigger the other side's close_notify
		  //~ shutdown(m_hSocket,1);
		  r = SSL_shutdown(m_ssl);
		}
    }

	xSocket::Close();	

	if(m_ssl)
	{ 
		SSL_free (m_ssl);
		m_ssl = NULL;
		printf("> SSL connection closed\n");
	}
}

int xSSLSocket::Read(char *buffer, int len, int nTimeout)
{
	return Read(buffer, len);
}

int xSSLSocket::Read(char *szBuffer, int nLen)
{
	int nRead = -1;
	int r = SSL_read (m_ssl, szBuffer, nLen);
	switch(SSL_get_error(m_ssl,r)){
		case SSL_ERROR_NONE:
			nRead=r;
			break;
		case SSL_ERROR_ZERO_RETURN:
			Close();	//connection closed, cleanup
			break;
		default:
			//berr_exit("SSL read problem");
			break;
	}

	return nRead;
}

int xSSLSocket::Write(const char *buffer, int len, int nTimeout) {
	return Write(buffer, len);
}

int xSSLSocket::Write(const char *szBuffer, int nLen)
{
	// keep writing until we've written everything
	int nOffset = 0;
	int nToSend = nLen;
	int nRet = 0;

	while(nToSend > 0)
	{
		xSocket::WaitUntilWritable();

		ERR_clear_error();
 		nRet = SSL_write(m_ssl, szBuffer + nOffset, nToSend);
		if(nRet <= 0)
		{
			int err = SSL_get_error(m_ssl, nRet);

			switch(err){
			  case SSL_ERROR_NONE:
				  nToSend -= nRet;
				  nOffset += nRet;
				  break;

			 case SSL_ERROR_SSL:
				  printf("SSL_write(): SSL_ERROR_SSL (%s)\n", ERR_error_string(ERR_get_error(), NULL));
				  return -1;
				  break;

			 case SSL_ERROR_WANT_READ:
				  printf("SSL_write(): SSL_ERROR_WANT_READ");
				  return -1;
				  break;

			 case SSL_ERROR_WANT_WRITE:
				  // Resend the same buffer (openssl knows what to do). Give up if
				  // we reach SSL_RETRY_LIMIT.
				  //if (++iWRetries >= SSL_RETRY_LIMIT)
				  {
					printf("SSL_write(): SSL_ERROR_WANT_WRITE");
					return -1;
				  }
				  break;

			  case SSL_ERROR_WANT_X509_LOOKUP:
				  printf("SSL_write(): SSL_ERROR_WANT_X509_LOOKUP");
				  return -1;
				  break;

			  case SSL_ERROR_ZERO_RETURN:
				  printf("SSL_write(): SSL_ERROR_ZERO_RETURN: The SSL connection has been closed.");
				  return -1;
				  break;

			  default:
				  printf("SSL error: %s", GetSSLError(err).c_str());
				  return -1;
			}
		}
		else
			break;
	} 

	//~ printf("Sent %d of %d", nOffset+nRet, nLen);
	return nOffset+nRet;
}

bool xSSLSocket::GetPeerCertificate(t_CertInfo &info)
{
	//get peer certificate (note: beware of dynamic allocation) - opt */
	X509* pPeerCert = SSL_get_peer_certificate (m_ssl);

	if (NULL != pPeerCert) 
	{
		char *str = X509_NAME_oneline (X509_get_subject_name (pPeerCert), 0, 0);
		if(str){
			strncpy(info.subject, str, sizeof(info.subject)-1);
			info.subject[sizeof(info.subject)-1] = '\0';
			free (str);
		}

		str = X509_NAME_oneline (X509_get_issuer_name (pPeerCert), 0, 0);
		if(str){
			strncpy(info.issuer, str, sizeof(info.issuer)-1);
			info.issuer[sizeof(info.issuer)-1] = '\0';
			free (str);
		}

		//TOFIX get additional data (adjust structure)

		X509_free (pPeerCert);
		return true;
	}

	return false;
}

