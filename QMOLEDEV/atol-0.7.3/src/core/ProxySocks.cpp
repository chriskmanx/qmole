////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "ProxySocks.h"
#include "_ftp/xSocket.h"
#ifdef _WIN32
#else
 #include <unistd.h>
 #include <sys/types.h>
 #include <sys/socket.h>
 #include <linux/net.h>
 #include <netinet/in.h>
 #include <arpa/inet.h>
 #include <sys/ioctl.h>
 #include <netdb.h>
#endif

#define  UC(b)	(((int)b)&0xff)

CProxySocks::CProxySocks()
{
}

CProxySocks::~CProxySocks()
{
}

SOCKET CProxySocks::Connect(const char *szHost, unsigned short nPort)
{
	//connect to proxy
	xSocket socket;
	socket.Open();

	if(szHost){
		Trace(TT_INFO, "Connecting to SOCKS proxy (%s:%d) ...\r\n", szHost, nPort);

		if(socket.Connect(szHost, nPort))
			Trace(TT_INFO, "Connected to proxy!\r\n");
		else
			Trace(TT_INFO, "Failed to connect to proxy!\r\n");
	}
	else{
		Trace(TT_INFO, "Connecting to SOCKS proxy (%s:%d)\r\n", m_data.strProxyHost.c_str(), m_data.nProxyPort);

		if(socket.Connect(m_data.strProxyHost.c_str(), m_data.nProxyPort))
			Trace(TT_INFO, "Connected to proxy!\r\n");
		else
			Trace(TT_INFO, "Failed to connect to proxy!\r\n");
	}

	return socket.Detach();
}

bool CProxySocks::Login(SOCKET hSock)
{ 
	return Op_Connect(hSock, m_data.strPeerHost.c_str(), m_data.nPeerPort);
}

SOCKET CProxySocks::Listen(String &strIP, unsigned short &uPort)
{ 
	//connect to proxy
	xSocket socket;
	socket.Open();
	if(!socket.Connect(m_data.strProxyHost.c_str(), m_data.nProxyPort))
		return INVALID_SOCKET;

	if(!Op_Bind(socket.GetHandle(), strIP, uPort))
		socket.Close();
	return socket.Detach();
}

bool CProxySocks::Accept(SOCKET hSock, String &strIP, unsigned short &uPort)
{ 
	return Op_Accept(hSock, strIP, uPort); 
}

//request from proxy to make connection to remote site 
bool CProxySocks::Op_Connect(SOCKET hSock, const char *szHost, int nPort)
{
	//ASSERT(szHost != NULL);
	//ASSERT(nPort > 0);

	switch(m_nSocksVer){
		case SOCKS_V4:
		case SOCKS_V4A:
			return Op_Connect4(hSock, szHost, nPort);
			break;

		case SOCKS_V5:
			return Op_Connect5(hSock, szHost, nPort);
			break;

		default:
			;//ASSERT(false);
	}

	return false;
}

//SOCKS4/SOCKS4A proxy
bool CProxySocks::Op_Connect4(SOCKET hSock, const char *szHost, int nPort)
{
	if(!Authenticate4(hSock))
		return false;

	//ASSERT(NULL != szHost && '\0' != *szHost);

	//Send request
	memset(m_szOutBuffer,0,9+strlen(szHost)+1);
	int len=9;
	m_szOutBuffer[0]= 4;								// protocol version
	m_szOutBuffer[1]= SOCKS_OP_CONNECT;				// SOCKS request code

	unsigned short uPort = htons(nPort);
	memcpy(&m_szOutBuffer[2],&uPort,2);				// copy target port

	//try to resolve host name to IP
	unsigned long nPeerIP = 0;
	ResolveToIP(szHost, nPeerIP);
	
	//copy target address
	if (!nPeerIP)
	{
		//failed to resolve host name
		if(SOCKS_V4A != m_nSocksVer)
			return false;

		//if we are speaking to SOCKS4A we can use hostname instead of IP
		//(server will try to resolve it)

		//set the IP to 0.0.0.x (x is nonzero)
		m_szOutBuffer[4]=0;
		m_szOutBuffer[5]=0;
		m_szOutBuffer[6]=0;
		m_szOutBuffer[7]=1;
		
		//Add host as URL
		strcpy(&m_szOutBuffer[9],szHost);
		len += strlen(szHost)+1;
	}
	else
		memcpy(&m_szOutBuffer[4],&nPeerIP,4);		//copy target IP

	//send socks packet
	int res = send(hSock, m_szOutBuffer, len, 0); //Send command
	if (res==SOCKET_ERROR || res<len){
		//TRACE("ProxySocks: Error sending data\n");
		return false;
	}

	//wait for reply
	ProxyReply(hSock);

	if(0 == m_szBuffer[0] && 90 == m_szBuffer[1])
		return true;

	ReportSocksError(m_szBuffer[1]);
	return false;
}

//our SOCKS5 code supports two logon types: 
// - No logon
// - cleartext username/password (if set) logon
bool CProxySocks::Op_Connect5(SOCKET hSock, const char *szHost, int nPort)
{
	if(!Authenticate5(hSock))
		return false;
	
	//TRACE("Socks5 authentication sucess!\n");

	//we are now authenticated, send connection request
	//max possible length needed
	//TOFIX for IP6 support use max(16, 1+strlen(szHost)) instead of 1+strlen(szHost)
	int nLenNeeded = 6 + 1/*host name len*/ + strlen(szHost) + 1 /*one more for safety*/;

	//prepare connect request
	memset(m_szOutBuffer,0,nLenNeeded);
	m_szOutBuffer[0]= 5;					// SOCKS protocol version
	m_szOutBuffer[1]= SOCKS_OP_CONNECT;		// operation
	m_szOutBuffer[2]= 0;					// reserved (must be 0x00)

	//try to resolve host name to IP
	unsigned long nPeerIP = 0;
	ResolveToIP(szHost, nPeerIP);
	int len = 0;
	
	//copy target address
	if (nPeerIP)
	{
		m_szOutBuffer[3]= SOCKS5_ADDR_IP4;		// address type field
		memcpy(&m_szOutBuffer[4],&nPeerIP,4);	// address data
		len = 8;
	}
	else
	{
		m_szOutBuffer[3]= SOCKS5_ADDR_NAME;		// address type field
		m_szOutBuffer[4]= strlen(szHost);		// address data - name length
		strcpy(&m_szOutBuffer[5], szHost);		// address data - name
		len = 5 + strlen(szHost);
	}

	//
	unsigned short uPort = htons(nPort);
	memcpy(&m_szOutBuffer[len],&uPort,sizeof(unsigned short));		// target port
	len += 2;

	//send request
	xSocket sock;
	sock.Attach(hSock);
	sock.WaitUntilWritable(10000);
	int res=send(hSock, m_szOutBuffer, len, 0);
	sock.Detach();

	if (res==SOCKET_ERROR || res<len){
		//TRACE("ProxySocks: Error sending data\n");
		return false;
	}

	//wait for reply
	ProxyReply(hSock);
	if(m_nBufferPos>=2)
	{
		if(0 != m_szBuffer[1]){
			ReportSocksError(m_szBuffer[1]);
			return false;
		}
		
		return true;
	}

	Trace(TT_ERROR, "Failed to login to Socks proxy\r\n");
	return false;
}

bool CProxySocks::ResolveToIP(const char *szHost, unsigned long &uIP)
{
	uIP = 0;

	unsigned long uTemp = inet_addr(szHost);

	if(INADDR_NONE == uTemp)
	{
		struct hostent *pHost = gethostbyname(szHost);
		if(pHost)
		{
			memcpy(&uIP, pHost->h_addr_list[0], sizeof(unsigned long));
			return true;
		}
	}
	else
	{
		uIP = uTemp;
		return true;
	}

	return false;
}

bool CProxySocks::ProxyReply(SOCKET hSock)
{
	xSocket sock;
	sock.Attach(hSock);

	if(!sock.WaitUntilReadable(3000)){
		sock.Detach();
		return false;
	}

	m_nBufferPos = recv(hSock, m_szBuffer, sizeof(m_szBuffer), 0);

	sock.Detach();
	return true;
}

bool CProxySocks::Op_Bind(SOCKET hSock, String &strIP, unsigned short &uPort)
{
	switch(m_nSocksVer){
		case SOCKS_V4:
		case SOCKS_V4A:
			return Op_Bind4(hSock, strIP, uPort);

		case SOCKS_V5:
			return Op_Bind5(hSock, strIP, uPort);

		default:
			; //ASSERT(false);
	}

	return false;
}

bool CProxySocks::Op_Bind4(SOCKET hSock, String &strIP, unsigned short &uPort)
{
	if(!Authenticate4(hSock))
		return false;

	//Send request
	memset(m_szOutBuffer,0,9+1);
	int len=9;
	m_szOutBuffer[0]= 4;								// protocol version
	m_szOutBuffer[1]= SOCKS_OP_BIND;					// SOCKS request code
	//BIND request fills dummy target port and address data	
	m_szOutBuffer[2]=0;
	m_szOutBuffer[3]=0;
	m_szOutBuffer[4]=0;
	m_szOutBuffer[5]=0;
	m_szOutBuffer[6]=0;
	m_szOutBuffer[7]=0;

	//send socks packet
	int res = send(hSock, m_szOutBuffer, len, 0); //Send m_szOutBuffer
	if (res==SOCKET_ERROR || res<len){
		//TRACE("ProxySocks: Error sending data\n");
		return false;
	}

	//wait for reply
	ProxyReply(hSock);

	if(0 == m_szBuffer[0] && 90 == m_szBuffer[1])
	{
		//bind packet contains host and port of the listening socket
		ExtractAddress(strIP, uPort);
		return true;
	}

	ReportSocksError(m_szBuffer[1]);
	return false;
}

//our SOCKS5 code supports two logon types: 
// - No logon
// - cleartext username/password (if set) logon
bool CProxySocks::Op_Bind5(SOCKET hSock, String &strIP, unsigned short &uPort)
{
	if(!Authenticate5(hSock))
		return false;

	//we are now authenticated, send connection request

	//max possible length needed
	//TOFIX for IP6 support use max(16, 1+strlen(szHost)) instead of 1+strlen(szHost)
	int nLenNeeded = 6 + 1/*host name len*/ + 4 /*+ strlen(szHost)*/ + 1 /*one more for safety*/;

	//prepare connect request
	memset(m_szOutBuffer,0,nLenNeeded);
	m_szOutBuffer[0]= 5;						// SOCKS protocol version
	m_szOutBuffer[1]= SOCKS_OP_BIND;			// operation
	m_szOutBuffer[2]= 0;						// reserved (must be 0x00)

	//dummy address and port
	m_szOutBuffer[3]= SOCKS5_ADDR_IP4;			// address type field
	m_szOutBuffer[4]= 0;
	m_szOutBuffer[5]= 0;
	m_szOutBuffer[6]= 0;
	m_szOutBuffer[7]= 0;
	m_szOutBuffer[8]= 0;
	m_szOutBuffer[9]= 0;

	int len = 10;

	//send request
	int res=send(hSock, m_szOutBuffer, len, 0);
	if (res==SOCKET_ERROR || res<len){
		//TRACE("ProxySocks: Error sending data\n");
		return false;
	}

	//wait for reply
	ProxyReply(hSock);
	if(m_nBufferPos>=2)
	{
		if(0 != m_szBuffer[1]){
			ReportSocksError(m_szBuffer[1]);
			return false;
		}

		//bind packet contains host and port of the listening socket
		ExtractAddress(strIP, uPort);
		return true;
	}

	ReportSocksError(m_szBuffer[1]);
	return false;
}

//after the successfull BIND command we may wait for incoming connection
//proxy will send us a packet indicating that someone connected of that listen timeout expired
bool CProxySocks::Op_Accept(SOCKET hSock, String &strIP, unsigned short &uPort)
{
	//same code for both SOCKS families
	ProxyReply(hSock);	//wait for packet

	if(m_nBufferPos>=8)
	{
		if(0 != m_szBuffer[1]){
			ReportSocksError(m_szBuffer[1]);
			return false;
		}

		//bind result is host name and port of the listening socket
		ExtractAddress(strIP, uPort);
		return true;
	}

	return false;
}

void CProxySocks::ReportSocksError(unsigned char nCode)
{
	/*
	SOCKS4:
	90: request granted
	91: request rejected or failed
	92: request rejected becasue SOCKS server cannot connect to
	    identd on the client
	93: request rejected because the client program and identd
	    report different user-ids

	SOCKS5:
	o  X'00' succeeded
	o  X'01' general SOCKS server failure
	o  X'02' connection not allowed by ruleset
	o  X'03' Network unreachable
	o  X'04' Host unreachable
	o  X'05' Connection refused
	o  X'06' TTL expired
	o  X'07' Command not supported
	o  X'08' Address type not supported
	o  X'09' to X'FF' unassigned
	*/

	std::string strErr;

	if(SOCKS_V5 == m_nSocksVer){
		switch (nCode){
			case 0x01:
				strErr = "(SOCKS server failure)";
				break;
			case 0x02:
				strErr = "(connection not allowed by ruleset)";
				break;
			case 0x03:
				strErr = "(Network unreachable)";
				break;
			case 0x04:
				strErr = "(Host unreachable)";
				break;
			case 0x05:
				strErr = "(Connection refused)";
				break;
			case 0x06:
				strErr = "(TTL expired)";
				break;
			case 0x07:
				strErr = "(Command not supported)";
				break;
			case 0x08:
				strErr = "(Address type not supported)";
				break;
			default:
				strErr = "(Unknown error)";
		}
	}
	else
	{
		switch (nCode){
			case 90:
				strErr = "(success)";
				break;
			case 91:
				strErr = "(request rejected or failed)";
				break;
			case 92:
				strErr = "(failed to connect to client identd)";
				break;
			case 93:
				strErr = "(identd error)";
				break;
			default:
				strErr = "(Unknown error)";
		}
	}

	Trace(TT_ERROR, "Socks proxy error! %s\r\n", strErr.c_str());
}

//bind packet contains host and port of the listening socket
void CProxySocks::ExtractAddress(String &strIP, unsigned short &uPort)
{
/* TOFIX
       * If the server have assigned same host as it was contacted on
       * it might return an address of all zeros
       
      if(reply.host.equals("0.0.0.0")){
         localIP = proxy.proxyIP;
         localHost = localIP.getHostName();
      }else{
         localHost = reply.host;
         localIP = reply.ip;
      }
*/
	if(SOCKS_V5 == m_nSocksVer)
	{
		//SOCKS5 packet;
		//+----+-----+-------+------+----------+----------+
		//|VER | REP |  RSV  | ATYP | BND.ADDR | BND.PORT |
		//+----+-----+-------+------+----------+----------+
		//| 1  |  1  | X'00' |  1   | Variable |    2     |
		//+----+-----+-------+------+----------+----------+

		//ASSERT(m_nBufferPos>=10);

		unsigned char nAddrType = m_szBuffer[3];
		
		switch(nAddrType){
			case SOCKS5_ADDR_IP4:
				strIP.Printf("%d.%d.%d.%d",
						UC(m_szBuffer[4]), 
						UC(m_szBuffer[5]), 
						UC(m_szBuffer[6]), 
						UC(m_szBuffer[7]));
				uPort = UC(m_szBuffer[8]) * 256 + UC(m_szBuffer[9]);
				break;
			case SOCKS5_ADDR_NAME:
				{
					int nLen = (unsigned char)m_szBuffer[4];

					//extract host name
					char szHost[256] ="";
					memcpy(szHost, &(m_szBuffer[5]), nLen);
					szHost[nLen] = '\0';

					unsigned long uIP;
					unsigned char *pBytes = (unsigned char *)&uIP;
					ResolveToIP(szHost, uIP);
					strIP.Printf("%d.%d.%d.%d",
						UC(pBytes[0]), 
						UC(pBytes[1]), 
						UC(pBytes[2]), 
						UC(pBytes[3]));
					uPort = UC(m_szBuffer[8]) * 256 + UC(m_szBuffer[9]);
				}
				break;
			case SOCKS5_ADDR_IP6:
			default:
				;//ASSERT(false);	//not supported or invalid type
		}
	}
	else
	{
		//SOCKS4 packet;
		//+----+----+----+----+----+----+----+----+
		//| VN | CD | DSTPORT |      DSTIP        |
		//+----+----+----+----+----+----+----+----+
		//| 1  | 1  |    2    |         4         |
		//+----+----+----+----+----+----+----+----+

		//ASSERT(m_nBufferPos>=8);

		strIP.Printf("%d.%d.%d.%d",
			UC(m_szBuffer[4]), 
			UC(m_szBuffer[5]), 
			UC(m_szBuffer[6]), 
			UC(m_szBuffer[7]));
		uPort = UC(m_szBuffer[2]) * 256 + UC(m_szBuffer[3]);
	}
}

bool CProxySocks::Authenticate(SOCKET hSock)
{
	if(SOCKS_V5 == m_nSocksVer)
		return Authenticate5(hSock);
	else
		return Authenticate4(hSock);
}

bool CProxySocks::Authenticate4(SOCKET hSock)
{
	//no need for separate code see Op_Bind4 or Op_Connect4
	return true;
}

bool CProxySocks::Authenticate5(SOCKET hSock)
{
	//see if we need logon
	bool bUseLogon = false; 
	if( m_data.strProxyUser.size()>0 && 
		m_data.strProxyPass.size()>0)
			bUseLogon = true; 

	//send initialization request
	memset(m_szOutBuffer,0,10);
	m_szOutBuffer[0]=5;	//SOCKS protocol version number
	m_szOutBuffer[1]=bUseLogon?2:1; //Number of logon types our client supports
	m_szOutBuffer[2]=bUseLogon? SOCKS5_AUTH_PASSWORD : SOCKS5_AUTH_NONE; //2=user/pass, 0=no logon

	int len=bUseLogon?4:3; //length of request
	int res=send(hSock, (char *)m_szOutBuffer, len, 0);
	if (res==SOCKET_ERROR || res<len){
		//TRACE("ProxySocks: Error sending data\n");
		return false;
	}

	//wait for reply
	ProxyReply(hSock);

	if(m_nBufferPos>=2)
	{
		int nAuthMethod = m_szBuffer[1];

		if(nAuthMethod == SOCKS5_AUTH_PASSWORD)
		{
			Trace(TT_INFO, "Entering SOCKS5 password authentication\r\n");

			//subnegotiation
			int nLenNeeded = 3 + strlen(m_data.strProxyUser.c_str()) + strlen(m_data.strProxyPass.c_str()) + 1 /*one more for safety*/;

			//Send request
			memset(m_szOutBuffer,0,nLenNeeded);
			m_szOutBuffer[0]= 1;								// subnegotiation version
			m_szOutBuffer[1]= strlen(m_data.strProxyUser.c_str());		// user field length
			strcpy(&m_szOutBuffer[2],m_data.strProxyUser.c_str());
			int len = 2 + strlen(m_data.strProxyUser.c_str());
			m_szOutBuffer[len] = strlen(m_data.strProxyPass.c_str());
			strcpy(&m_szOutBuffer[len+1],m_data.strProxyPass.c_str());

			int res=send(hSock, m_szOutBuffer, nLenNeeded-1, 0);
			if (res==SOCKET_ERROR || res<len){
				//TRACE("ProxySocks: Error sending data\n");
				return false;
			}
	
			//wait for reply
			ProxyReply(hSock);
			if(m_nBufferPos>=2)
			{
				if(0 != m_szBuffer[1]){
					ReportSocksError(m_szBuffer[1]);
					return false;
				}
			}
			else{
				Trace(TT_ERROR, "Failed to login to Socks proxy\r\n");
				return false;
			}
	
			Trace(TT_INFO, "Authentication done\r\n");
		}
		else if(nAuthMethod == SOCKS5_AUTH_NONE)
		{
			//OK, do nothing here
			Trace(TT_INFO, "No SOCKS5 authentication needed\r\n");
		}
		else{
			Trace(TT_INFO, "Invalid SOCKS5 authentication type\r\n");
			return false;
		}
	}
	else
		return false;

	return true;
}
