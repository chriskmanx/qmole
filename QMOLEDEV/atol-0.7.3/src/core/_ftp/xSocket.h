////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: xSocket class handles TCP/IP socket operations
////////////////////////////////////////////////////////////////////////////

#ifndef XSOCKET_H__
#define XSOCKET_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <string>

#ifdef _WIN32
 #define _WINSOCKAPI_   // Prevent inclusion of winsock.h in windows.h
 #include <winsock2.h>
 #include <windows.h>
#endif

#ifndef SOCKET
 #define SOCKET int 
#endif
#ifndef INVALID_SOCKET
 #define INVALID_SOCKET (int)-1
#endif
#ifndef SOCKET_ERROR
 #define SOCKET_ERROR (int)-1
#endif

//TOFIX save local,remote port into variable (if loc was 0 find it)
class xSocket
{
public:
	xSocket();
	virtual ~xSocket();

	inline bool operator == (const xSocket &that)
	{ return m_hSocket == that.m_hSocket; }

	inline operator int ()	//cast operator
	{ return m_hSocket; }	

	inline int GetHandle()
	{ return m_hSocket; }
	
	bool IsValid();

	void   Attach(int /*SOCKET*/ s);
	int/*SOCKET*/ Detach();

	bool Open(int nLocalPort = 0);
	void Close();

	bool Connect(const char *szHost, int nRemotePort, int nTimeoutSec = 0);
	void Disconnect();

	bool Listen(int backlog = 5);
	int	 Accept(int nTimeoutSec = 0);

	bool WaitUntilWritable(int nTimeout = -1);
	bool WaitUntilReadable(int nTimeout = -1);

	virtual int Read(char *buffer, int len);
	virtual int Read(char *buffer, int len, int nTimeout);
	virtual int Write(const char *buffer, int len);
	virtual int Write(const char *buffer, int len, int nTimeout);

	void FlushIncomingData();

	//
	//info functions
	//
		
	bool IsReadable();
	bool IsWritable();

	std::string GetLocalIP();
	std::string GetRemoteIP();

	int	GetError();
	std::string	GetErrorText(int nError);

	static std::string ResolveIPToHost(const char *szIP);
	static std::string ResolveHostToIP(const char *szHost);

	static bool InitSockets();
	static void CleanupSockets();

	int m_hSocket;

protected:
#ifdef _WIN32
	std::string	WinGetErrorText(int nError);
#endif
};

#endif // XSOCKET_H__
