////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: xSocket class handles TCP/IP socket operations
////////////////////////////////////////////////////////////////////////////

#include "xSocket.h"
#include "../debug.h"

#ifdef _WIN32
 #include <winsock2.h>
#else
 #include <unistd.h> //close
 #include <netdb.h>
 #include <sys/types.h>
 #include <sys/socket.h>
 #include <linux/net.h>

 #ifndef SOCKET
  //#define SOCKET int
  #define INVALID_SOCKET -1
  #define SOCKET_ERROR -1 
 #endif
#endif

#ifdef _WIN32
 #pragma comment(lib, "ws2_32")	//automatically link this library (WSAConnect)
#else
 #include <errno.h>
  #include <netinet/in.h>
  #include <arpa/inet.h>
  #include <sys/ioctl.h>
#endif

xSocket::xSocket()
{
	m_hSocket = INVALID_SOCKET;
}

xSocket::~xSocket()
{
	Close();	//just in case
}

bool xSocket::IsValid()
{ 
	return (m_hSocket != INVALID_SOCKET); 
}

bool xSocket::Open(int nLocalPort)
{
	//ASSERT(!IsValid());

	if(!IsValid())		//only if not already created
	{
		m_hSocket = socket(PF_INET, SOCK_STREAM, 0);	//create socket
		
		if(IsValid())
		{
			struct sockaddr_in sockAddr;
			memset(&sockAddr,0,sizeof(sockAddr));
			sockAddr.sin_family			= AF_INET;
			sockAddr.sin_addr.s_addr	= htonl(INADDR_ANY);
			sockAddr.sin_port			= htons(nLocalPort);

			if (0 != bind(m_hSocket, (struct sockaddr *)&sockAddr, sizeof(sockAddr)))
			{
				Close();
				//TRACE("ERROR: Error binding socket.\n");
				return false;
			}

			return true;
		}
	}

	return false;
}

void xSocket::Close()
{
	if(IsValid())
	{
	#ifdef _WIN32
		//TRACE("xSocket: closing %d\n", m_hSocket);
		closesocket(m_hSocket);
	#else
		close(m_hSocket);
	#endif

		m_hSocket = INVALID_SOCKET;
	}
}

bool xSocket::Connect(const char *szHost, int nRemotePort, int nTimeoutSec)
{
	//ASSERT(IsValid());
	//ASSERT(NULL != szHost);
	//ASSERT(nRemotePort > 0);

	TRACE("xSocket::Connect(Host=%s, Port=%d)", szHost, nRemotePort);
	if(!IsValid() || NULL == szHost || nRemotePort < 1)
		return false;

	struct sockaddr_in sockAddr;
	memset(&sockAddr,0,sizeof(sockAddr));
	sockAddr.sin_family	= AF_INET;
	sockAddr.sin_port	= htons(nRemotePort);

	//NOTE: if address is NOT in numerical (dotted string) form
	//		we must find out address for given host name
	unsigned long lAddress = inet_addr(szHost);	
	
	if(INADDR_NONE == lAddress)
	{
		struct hostent *pHost = gethostbyname(szHost);
		if(NULL != pHost)
		{
			memcpy(&lAddress, pHost->h_addr_list[0], sizeof(unsigned long));
		}
		else
		{
			//TRACE("ERROR: Couldn't resolve server address (unknown host)\n");
			return false;
		}
	}

	sockAddr.sin_addr.s_addr	= lAddress;
	//TRACE("Server address: %d\n", lAddress);

	if(nTimeoutSec > 0)	//connect with timeout
	{
		//convert socket to non-blocking mode
		#ifdef _WIN32
			unsigned long lNonBlock = 1;
			ioctlsocket(m_hSocket, FIONBIO, &lNonBlock);
		#else
			//setsockopt(m_hSocket, )
			unsigned long lNonBlock = 1;
			ioctl(m_hSocket, FIONBIO, &lNonBlock);
		#endif

		if(0 != connect(m_hSocket, (struct sockaddr *)&sockAddr, sizeof(sockAddr)))
		{
		#ifdef _WIN32
			if(WSAEWOULDBLOCK != WSAGetLastError())
				return false;	//error
		#endif

			//wait for result
			fd_set writefds, exceptfds;
			FD_ZERO(&writefds);
			FD_SET(m_hSocket, &writefds);
			FD_ZERO(&exceptfds);
			FD_SET(m_hSocket, &exceptfds);

			struct timeval tout;
			tout.tv_sec  = nTimeoutSec;	//seconds
			tout.tv_usec = 0;			//microseconds
			
			int nEvents = select(m_hSocket+1, NULL, &writefds, &exceptfds, &tout);
			if(0 == nEvents)
			{
				Close();
				return false;	//timeout
			}
			else if(FD_ISSET(m_hSocket, &exceptfds))
			{
				Close();
				return false;	//connection failed
			}
		}

		//convert socket back to blocking mode
		#ifdef _WIN32
			lNonBlock = 0;
			ioctlsocket(m_hSocket, FIONBIO, &lNonBlock);
		#else
			lNonBlock = 0;
			ioctl(m_hSocket, FIONBIO, &lNonBlock);
		#endif

	}
	else
	{
		if(0 != connect(m_hSocket, (struct sockaddr*)&sockAddr, sizeof(sockAddr)))
		{
			//TRACE("ERROR: Error (%d) while connecting to host\n", GetError());
			return false;
		}
	}

	return true;
}

void xSocket::Disconnect()
{
	if(IsValid())
	{
	#ifdef _WIN32
		shutdown(m_hSocket, SD_BOTH);	//both read & write
	#else
		shutdown(m_hSocket, SHUT_RDWR);	//both read & write
	#endif

		Close();
	}
}

//is there anything to read from the socket
bool xSocket::IsReadable()
{
	return WaitUntilReadable(0);
}

//can we write to the socket
bool xSocket::IsWritable()
{
	return WaitUntilWritable(0);
}

//get local IP address in form of dotted string
std::string xSocket::GetLocalIP()
{
	struct sockaddr_in addr;
	std::string strIP;

	#ifdef _WIN32
		int size = sizeof(struct sockaddr);
	#else
		socklen_t size = sizeof(struct sockaddr);
	#endif

	if( 0 == getsockname(m_hSocket, (struct sockaddr *)&addr, &size) )
		strIP = inet_ntoa(addr.sin_addr);

	return strIP;
}

//get remote IP address in form of dotted string
std::string xSocket::GetRemoteIP()
{
	struct sockaddr_in addr;
	std::string strIP;

	#ifdef _WIN32
		int size = sizeof(struct sockaddr);
	#else
		socklen_t size = sizeof(struct sockaddr);
	#endif

	if( 0 == getpeername(m_hSocket, (struct sockaddr *)&addr, &size) )
		strIP = inet_ntoa(addr.sin_addr);

	return strIP;
}

//turn socket into listening mode
bool xSocket::Listen(int backlog)
{
	if(IsValid())
		if(0 == listen(m_hSocket, backlog))
			return true;

	return false;
}

void xSocket::Attach(int s)
{
	Close();
	m_hSocket = s;
}

int xSocket::Detach()
{
	int hSock = m_hSocket;
	m_hSocket = INVALID_SOCKET;
	return hSock;
}

int xSocket::Accept(int nTimeoutSec)
{
	if(nTimeoutSec > 0)	//accept with timeout
	{
		//convert socket to non-blocking mode
		#ifdef _WIN32
			unsigned long lNonBlock = 1;
			ioctlsocket(m_hSocket, FIONBIO, &lNonBlock);
		#else
			unsigned long lNonBlock = 1;
			ioctl(m_hSocket, FIONBIO, &lNonBlock);
		#endif


		int nSocket = INVALID_SOCKET;
		struct sockaddr_in sockAddr;

		//TOFIX: add also abort event checking
		//loop until some connection accepted or timeout expires
		while(nTimeoutSec > 0)
		{
			nSocket = accept(m_hSocket, (struct sockaddr *)&sockAddr, NULL);
			if(INVALID_SOCKET == nSocket){
			#ifdef _WIN32
				if(WSAEWOULDBLOCK == WSAGetLastError())
					Sleep(1000);	//sleep for 1 sec, then try again
				else
					break;
			#endif		
			}
			else
				break;					
			nTimeoutSec --;
		}

		//convert socket back to blocking mode
		#ifdef _WIN32
			lNonBlock = 0;
			ioctlsocket(m_hSocket, FIONBIO, &lNonBlock);
		#else
			lNonBlock = 0;
			ioctl(m_hSocket, FIONBIO, &lNonBlock);
		#endif

		return nSocket;
	}
	else	//blocking accept
	{
		struct sockaddr_in sockAddr;
		return accept(m_hSocket, (struct sockaddr *)&sockAddr, NULL);
	}
}

//returns number of bytes received
int xSocket::Read(char *buffer, int len)
{
	if(IsReadable())	//we don't want to block here
	{
		int nRes = recv(m_hSocket, buffer, len, 0);
		if(SOCKET_ERROR != nRes)
			return nRes;
	}
	
	return -1;	//error
}

// waits for socket to become writable to send the buffer
int xSocket::Read(char *buffer, int len, int nTimeout)
{
	if(WaitUntilReadable(nTimeout))
		return Read(buffer, len);

	return -1;	// error
}

//returns number of bytes send
int xSocket::Write(const char *pch, int nLen)
{
	if(IsWritable())
	{
		int nTotalSent = 0;
		int nSent;

		//send inside loop (until error or until done)
		do{
			nSent = send(m_hSocket, pch, nLen - nTotalSent, 0);
			if(SOCKET_ERROR == nSent || 0 == nSent){
				//TRACE("xSocket::Write FAILED(last = %d, total =%d %d)! Error = %d\n", nSent, nTotalSent, nLen, GetLastError());
				break;
			}

			pch			+= nSent;	// increase data pointer
			nTotalSent	+= nSent;	// increase sent counter
		} 
		while(nTotalSent < nLen);

		return nTotalSent;
	}
	//else
	//	TRACE("xSocket::Write FAILED(socket not writable)!\n");
	
	return -1;	//error
}

// waits for socket to become writable to send the buffer
int xSocket::Write(const char *pch, int nLen, int nTimeout)
{
	int nTotalSent = 0;
	int nSent;

	//send inside loop (until error or until done)
	do{
		if(!WaitUntilWritable(nTimeout)){
			//TRACE("xSocket::Write FAILED(socket not writable)!\n");
			break;
		}

		nSent = send(m_hSocket, pch, nLen - nTotalSent, 0);
		if(SOCKET_ERROR == nSent || 0 == nSent){
			//TRACE("xSocket::Write FAILED(last = %d, total =%d %d)! Error = %d\n", nSent, nTotalSent, nLen, GetLastError());
			break;
		}
			
		pch			+= nSent;	// increase data pointer
		nTotalSent	+= nSent;	// increase sent counter
	} 
	while(nTotalSent < nLen);

	return nTotalSent;
}

//NOTE: timeout < 0 -> blocks forever until socket ready for writing (default)
//      timeout = 0 -> just checking current state
//		timeout > 0 -> wait until given timeout
//
//wait max until timeout for socket to become writable
bool xSocket::WaitUntilWritable(int nTimeout)
{
	if(IsValid())
	{
		fd_set writefds;
		FD_ZERO(&writefds);
		FD_SET(m_hSocket, &writefds);
		
		struct timeval tout;
		struct timeval *pTime = &tout;

		if(nTimeout >= 0)	// valid timeout specified
		{
			tout.tv_sec  = nTimeout / 1000;				//extract seconds
			tout.tv_usec = (nTimeout % 1000) * 1000;	//extract microseconds
		}
		else	//no timeout - block until something
			pTime = NULL;

		int nEvents = select(m_hSocket+1, NULL, &writefds, NULL, pTime);
		return (nEvents > 0);
	}
	else
		return false;
}

//NOTE: timeout < 0 -> blocks forever until something comes for reading (default)
//      timeout = 0 -> just checking current state
//		timeout > 0 -> wait until given timeout
//
//wait max until timeout for socket to become readable
bool xSocket::WaitUntilReadable(int nTimeout)
{
	if(IsValid())
	{
		fd_set readfds;
		FD_ZERO(&readfds);
		FD_SET(m_hSocket, &readfds);

		struct timeval tout;
		struct timeval *pTime = &tout;

		if(nTimeout >= 0)	// valid timeout specified
		{
			tout.tv_sec  = nTimeout / 1000;			//extract seconds
			tout.tv_usec = (nTimeout % 1000) * 1000;	//extract microseconds
		}
		else	//no timeout - block until something
			pTime = NULL;
		
		int nEvents = select(m_hSocket+1, &readfds, NULL, NULL, pTime);
		//ASSERT(SOCKET_ERROR != nEvents);

//		int nErr;
//		if(SOCKET_ERROR == nEvents)
//			nErr = WSAGetLastError();

		return (nEvents > 0);
	}
	else
		return false;
}

void xSocket::FlushIncomingData()
{
	if(IsValid())
	{
		char buffer[256];
		int	 nRet;

		do{
			nRet = recv(m_hSocket, buffer, sizeof(buffer), 0);
		} while(nRet != SOCKET_ERROR && nRet > 0);
	}
}

int xSocket::GetError()
{
	int nError = 0;

	#ifdef _WIN32
		int nLen = sizeof(int);
	#else
		socklen_t  nLen = sizeof(int);
	#endif

	//try to return per-socket error
	if(0 == getsockopt(m_hSocket, SOL_SOCKET, SO_ERROR, (char *)&nError, &nLen))
	{
		return nError;
	}
	else
	{
		//return global socket error
		#ifdef _WIN32
			return WSAGetLastError(); 
		#else
			return errno;
		#endif
	}
}

std::string xSocket::GetErrorText(int nError)
{
	std::string strErr;

	#ifdef _WIN32
		strErr = WinGetErrorText(nError);	
	#else
		strErr = strerror(nError); 
	#endif

	return strErr;
}

#ifdef _WIN32
std::string xSocket::WinGetErrorText(int nError)
{
	std::string	strRet;

	switch(nError)
	{
	case WSANOTINITIALISED:
		strRet = "A successful WSAStartup must occur before using this function.";
		//strRet = "Mrena biblioteka funkcija nije inicijalizirana.";
		break;
	case WSAENETDOWN :
		strRet = "The network subsystem has failed.";
		//strRet = "Greka u mrenom podsustavu.";
		break;
	case WSAEADDRINUSE :
		strRet = "The specified address is already in use.";
		//strRet = "Zadana adresa je ve�u upotrebi.";
		break;
	case WSAEINTR:
		strRet = "The (blocking) call was canceled through WSACancelBlockingCall.";
		//strRet = "Odustajanje od postavljenog zahtjeva";
		break;
	case WSAEINPROGRESS :
		strRet = "A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function.";
		//strRet = "Blokiraju� poziv je u tijeku";
		break;
	case WSAEALREADY :
		strRet = "A nonblocking connect call is in progress on the specified socket.";
		//strRet = "Na zadanoj vezi u tijeku je spajanje poziva";
		break;
	case WSAEADDRNOTAVAIL :
		strRet = "The specified address is not available from the local machine.";
		//strRet = "Zadana adresa nije dostupna sa lokalnog stroja";
		break;
	case WSAEAFNOSUPPORT :
		strRet = "Addresses in the specified family cannot be used with this socket.";
		//strRet = "Zadana obitelj adresa nije dostupna sa preko zadane veze";
		break; 
	case WSAECONNREFUSED :
		strRet = "The attempt to connect was forcefully rejected.";
		//strRet = "Pokuaj spajanja odbijen";
		break;
	case WSAEFAULT :
		strRet = "Incorrect address";
		//strRet = "Neispravna adresa";
		break;
	case WSAEINVAL :
		strRet = "The parameter is a listening socket.";
		//strRet = "Parametar je veza koja oslukuje";
		break;
	case WSAEISCONN :
		strRet = "The socket is already connected (connection-oriented sockets only).";
		//strRet = "Zadana veza je ve�spojena";
		break;
	case WSAENETUNREACH :
		strRet = "The network cannot be reached from this host at this time.";
		//strRet = "Mrea je trenutno nedostupna";
		break;
	case WSAENOBUFS :
		strRet = "No buffer space is available. The socket cannot be connected.";
		//strRet = "Premalen buffer. Veza se ne moe uspostaviti.";
		break;
	case WSAENOTSOCK :
		strRet = "The descriptor is not a socket.";
		//strRet = "Neispravna veza";
		break;
	case WSAETIMEDOUT :
		strRet = "Attempt to connect timed out without establishing a connection.";
		//strRet = "Isteklo je vrijeme pokuaja uspostave veze";
		break;
	case WSAEWOULDBLOCK :
		strRet = "The socket is marked as nonblocking and the connection cannot be completed immediately.";
		//strRet = "Uspostava veze u tijeku";
		break;
	case WSAEACCES :
		strRet = "Attempt to connect datagram socket to broadcast address failed.";
		//strRet = "Neuspjeh pri uspostavi BROADCAST veze";
		break;
	case WSAHOST_NOT_FOUND:
		strRet = "Host not found.";
		//strRet = "Server nepoznat.";
		break;
	case WSAECONNRESET:
		strRet = "Connection reset by peer.";
		//strRet = "Druga strana je prekinula vezu";
		break;
	default:
		{
			strRet = "Error #";
			char szNumBuffer[8];
			//strRet  = "Nedefinirana greka [";
			strRet += itoa(nError, szNumBuffer, 10);
			//strRet += "]";
		}
	}

	return strRet;
}
#endif

//whois
std::string xSocket::ResolveIPToHost(const char *szIP)
{
	std::string strHost;
	
	//Task 1:	Given IP Address i.e. "111.111.111.111",
	//			Return Network byte ordered address (ulIP)	
	unsigned long nIP = inet_addr(szIP);

	if(INADDR_NONE != nIP)
	{
		struct sockaddr_in addr;
		addr.sin_family				= AF_INET;
		#ifdef _WIN32
			addr.sin_addr.S_un.S_addr	= nIP;
		#else
			addr.sin_addr.s_addr = (in_addr_t)nIP;
		#endif

		struct hostent *hHost = gethostbyaddr((char*)&addr.sin_addr, 4, PF_INET);
		if(hHost != NULL)
		{
			strHost = hHost->h_name;
		}
	}
	//else
	//	TRACE("ERROR: Invalid numeric address\n");

	return strHost;
}

// host name to "195.77.24.1" like string
std::string xSocket::ResolveHostToIP(const char *szHost)
{
	std::string strIP;
	
	struct hostent *pHostent = gethostbyname(szHost);
	if (NULL != pHostent)
	{
		unsigned long lAddress;
		memcpy(&lAddress, pHostent->h_addr_list[0], sizeof(unsigned long));

		struct in_addr inAddr;
		inAddr.s_addr = lAddress;

		strIP = inet_ntoa(inAddr);
	}

	return strIP;
}

//TOFIX some static variable to prevent multiple calls
bool xSocket::InitSockets()
{
#ifdef _WIN32
    //initiates use of socket DLL by a process
    WSADATA WsaData;
    if(0 != WSAStartup(0x0101,&WsaData))
    {
        //TRACE("ERROR: Winsock init failed.\n");
        return false;
    }
#endif

    return true;
}

void xSocket::CleanupSockets()
{
#ifdef _WIN32
    WSACleanup();
#endif
}

