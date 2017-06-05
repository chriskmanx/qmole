////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "ProxyHttp.h"
#include "_util/Base64.h"
#include "_ftp/xSocket.h"

#ifndef _WIN32
 #include <sys/types.h>
 #include <sys/socket.h>
 #include <unistd.h>
#endif 

CProxyHttp::CProxyHttp()
{
}

CProxyHttp::~CProxyHttp()
{
}

SOCKET CProxyHttp::Connect(const char *szHost, unsigned short nPort)
{
	//connect to proxy
	xSocket socket;
	socket.Open();

	if(szHost){
		Trace(TT_INFO, "Connecting to HTTP proxy (%s:%d)\r\n", szHost, nPort);

		if(socket.Connect(szHost, nPort))
			Trace(TT_INFO, "Connected to proxy!\r\n");
		else
			Trace(TT_INFO, "Failed to connect to proxy!\r\n");
	}
	else{
		Trace(TT_INFO, "Connecting to HTTP proxy (%s:%d)\r\n", m_data.strProxyHost.c_str(), m_data.nProxyPort);

		if(socket.Connect(m_data.strProxyHost.c_str(), m_data.nProxyPort))
			Trace(TT_INFO, "Connected to proxy!\r\n");
		else
			Trace(TT_INFO, "Failed to connect to proxy!\r\n");
	}

	return socket.Detach();
}

bool CProxyHttp::Login(SOCKET hSock)
{
	//see if we need logon
	bool bUseLogon = false; 
	if( m_data.strProxyUser.size() > 0 && 
		m_data.strProxyPass.size() > 0)
			bUseLogon = true; 

	//send connect request
	String strMsg;
	if (!bUseLogon)
	{
		strMsg.Printf("GET http://%s:%d/ HTTP/1.0\r\n\r\n",
						m_data.strPeerHost.c_str(), m_data.nPeerPort);
	}
	else
	{
		strMsg.Printf("GET ftp://%s:%d/ HTTP/1.1\r\nHost: %s:%d\r\nUser-Agent: Atol\r\n",
						m_data.strPeerHost.c_str(), m_data.nPeerPort,
						m_data.strPeerHost.c_str(), m_data.nPeerPort);
		
		String userpass = m_data.strProxyUser.c_str();
		userpass += ":";
		userpass += m_data.strProxyPass.c_str();
			
		CBase64 base64coding;
		std::string base64str = base64coding.Encode(userpass.c_str(), userpass.size());
		if(base64str.size() == 0)
			return false;

		strMsg += "Authorization: Basic ";
		strMsg += base64str.c_str();
		strMsg += "\r\n\r\n";
		strMsg += "Proxy-Authorization: Basic ";
		strMsg += base64str.c_str();
		strMsg += "\r\n\r\n";
	}

	xSocket sock;
	sock.Attach(hSock);
	if(!sock.WaitUntilWritable(3000)){
		//TRACE("HTTP proxy: socket not writable\n");
		sock.Detach();
		return false;
	}
	sock.Detach();

	int res= send(hSock, strMsg.c_str(), strMsg.size(), 0);
	if (res==SOCKET_ERROR || res<(int)strMsg.size()){
		//TRACE("HTTP proxy: failed to send data\n");
		return false;
	}

#ifdef _WIN32
	Sleep(1000);
#else
	usleep(1000);  //1sec
#endif
	return true;
/*
	//read reply
	std::string m_strBuffer;

	sock.Attach(hSock);

	char buffer[1256]="";
	buffer[1]=0;

	for(;;)
	{
		if(!sock.WaitUntilReadable(10000)){
			sock.Detach();
			return true;
		}

		int numread= recv(hSock, buffer, sizeof(buffer), 0);
		if (numread==SOCKET_ERROR){
			sock.Detach();
			return false;
		}

		if (0 == numread)
			return true;

//		Trace(TT_INFO, "HTTP proxy response (%d bytes):\n%s\n", numread, buffer);

		//Response begins with HTTP/
		m_strBuffer+=buffer;
		std::string start="HTTP/";
		start=start.Left(m_strBuffer.size());
		if (start!=m_strBuffer.Left(start.size())){
			sock.Detach();
			return false;
		}
		int pos=m_strBuffer.Find("\r\n");
		if (pos!=-1)
		{
			int pos2=m_strBuffer.Find(" ");
			if (pos2==-1 || m_strBuffer[pos2+1]!='2' || pos2>pos){
				sock.Detach();
				return false;
			}
		}
		if (m_strBuffer.Right(4)=="\r\n\r\n") //End of the HTTP header
		{
			//Reset();
			//ClearBuffer();
			sock.Detach();
			return true;
		}
	}
*/
	return true;
}

