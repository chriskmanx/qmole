////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "ProxyFtp.h"
#include "_ftp/xSocket.h"

CProxyFtp::CProxyFtp()
{
	m_nFtpProxyType = 0;
}

CProxyFtp::~CProxyFtp()
{
}

bool CProxyFtp::Login(SOCKET hSock, tProxyLoginData &data, std::string acct, int logontype) 
{
	const int LO = -2, ER = -1;
	const int NUMLOGIN = 9; // currently supports 9 different login sequences

	static const int logonseq[NUMLOGIN][20] = {
		// this array stores all of the logon sequences for the various firewalls
		// in blocks of 3 nums. 
		//	1st num is command to send,
		//	2nd num is next point in logon sequence array if 200 series response is rec'd from server as the result of the command, 
		//	3rd num is next point in logon sequence if 300 series rec'd
		{0,LO,3, 1,LO,6, 2,LO,ER},							// no firewall
		{3,6,3, 4,6,ER, 5,ER,9, 0,LO,12, 1,LO,15, 2,LO,ER}, // SITE hostname
		{3,6,3, 4,6,ER, 6,LO,9, 1,LO,12, 2,LO,ER},			// USER after logon
		{7,3,3, 0,LO,6, 1,LO,9, 2,LO,ER},					// proxy OPEN
		{3,6,3, 4,6,ER, 0,LO,9, 1,LO,12, 2,LO,ER},			// Transparent
		{6,LO,3, 1,LO,6, 2,LO,ER},							// USER with no logon
		{8,6,3, 4,6,ER, 0,LO,9, 1,LO,12, 2,LO,ER},			// USER fireID@remotehost
		{9,ER,3, 1,LO,6, 2,LO,ER},							// USER remoteID@remotehost fireID
		{10,LO,3, 11,LO,6, 2,LO,ER}							// USER remoteID@fireID@remotehost
	};

	if(logontype<0||logontype>=NUMLOGIN)
		return false; // illegal connect code

	xSocket socket;
	socket.Attach(hSock);

	int nFtpCode;
	int port,logonpoint=0;
	String buf,temp;

	// are we connecting directly to the host (logon type 0) or via a firewall? (logon type>0)
	if(!logontype) {
		temp = data.strPeerHost.c_str();
		port = data.nPeerPort;
	}
	else {
		temp = data.strProxyHost.c_str();
		port = data.nProxyPort;
	}

	String hostname = data.strPeerHost.c_str();
	if(data.nPeerPort!=21)
		hostname.Printf(":%d",data.nPeerPort); // add port to hostname (only if port is not 21)

	Trace(TT_INFO, "Connected to %s.\r\n", hostname.c_str());
	
	//read initial proxy text
	int nRead = ReadReply(socket);
	if(nRead <=0){
		//TRACE("Ftp proxy: No initial response.\n");
		socket.Detach();
		return false; //no response
	}

	// get 1st digit of the return code (indicates primary result)
	nFtpCode = GetReplyDigit();

	// go through appropriate logon procedure
	while(1) {
		switch(logonseq[logontype][logonpoint]) {
		case 0:
			temp.Printf("USER %s\r\n", data.strPeerUser.c_str());
			break;
		case 1:
			temp.Printf("PASS %s\r\n", data.strPeerPass.c_str());
			break;
		case 2:
			temp.Printf("ACCT %s\r\n", acct.c_str());
			break;
		case 3:
			temp.Printf("USER %s\r\n", data.strProxyUser.c_str());
			break;
		case 4:
			temp.Printf("PASS %s\r\n",data.strProxyPass.c_str());
			break;
		case 5:
			temp.Printf("SITE %s\r\n", hostname.c_str());
			break;
		case 6:
			temp.Printf("USER %s@%s:%d\r\n", data.strPeerUser.c_str(), hostname.c_str(), data.nPeerPort);
			break;
		case 7:
			temp.Printf("OPEN %s\r\n", hostname.c_str());
			break;
		case 8:
			temp.Printf("USER %s@%s\r\n", data.strProxyUser.c_str(), hostname.c_str());
			break;
		case 9:
			temp.Printf("USER %s@%s %s\r\n", data.strPeerUser.c_str(), hostname.c_str(), data.strProxyUser.c_str());
			break;
		case 10:
			temp.Printf("USER %s@%s@%s\r\n", data.strPeerUser.c_str(), data.strProxyUser.c_str(), hostname.c_str());
			break;
		case 11:
			temp.Printf("PASS %s@%s\r\n", data.strPeerPass.c_str(), data.strProxyPass.c_str());
			break;
		}
	
		// send command, get response
		if(!SendCmd(socket, temp.c_str())){
			socket.Detach();
			return false;
		}

		//read command reply string
		nRead = ReadReply(socket);
		if(nRead <= 0){
			//TRACE("Ftp proxy: No command response.\n");
			socket.Detach();
			return false;
		}

		// get 1st digit of the return code (indicates primary result)
		nFtpCode = GetReplyDigit(); 
	
		// only these responses are valid
		if(nFtpCode!=2 && nFtpCode!=3){
			socket.Detach();
			return false;
		}

		logonpoint = logonseq[logontype][logonpoint+nFtpCode-1]; //get next command from array
		switch(logonpoint) {
		case ER: // ER means something has gone wrong
			//m_retmsg.LoadString(IDS_FTPMSG1);
			//TRACE("logging error\n");
			socket.Detach();
			return false;
		case LO: // LO means we're fully logged on
			socket.Detach();
			return true;
		}
	}
}

int CProxyFtp::ReadReply(xSocket &socket)
{
	// wait for data to become available
	if(!socket.WaitUntilReadable(5000)) //wait max 5 seconds
		return -1; //no response

	int nRes, nRead = 0, nPos = 0;
	m_szBuffer[0] = '\0';
	m_list.clear();

	while(true){
		//read from socket into free part of buffer (slow a little due to Windows NT servers)
		nRes = socket.Read(m_szBuffer+nPos, sizeof(m_szBuffer)-nPos-1);
		if(nRes <= 0) break;

		//TRACE("ProxyFtp: Read %d characters.\n", nRes);

		nRead += nRes;				//read counter
		nPos  += nRes;				//buffer position
		m_szBuffer[nPos] = '\0';	//terminate string so we can search inside it	

		//parse buffer
		ParseBuffer(nPos);

		//wait for more data
		if(!socket.WaitUntilReadable(100))
			break;
	}

	//trace reply
	for(unsigned int i=0; i<m_list.size(); i++)
		Trace(TT_REPLY, m_list[i].c_str());

	return nRead;
}

bool CProxyFtp::SendCmd(xSocket &socket, const char *szCmd)
{
	//log content sent over control socket
	//hide password in the log string
	if(0 == strncmp(szCmd, "PASS", strlen("PASS")))
		Trace(TT_COMMAND, "PASS (hidden)\r\n");	
	else
		Trace(TT_COMMAND, szCmd);
	
	int nRes = socket.Write(szCmd, strlen(szCmd));
	if(nRes <= 0)
	{
		Trace(TT_ERROR, "Failed to send proxy command. Connection terminated.");
		socket.Close();
		return false;
	}

	return true;
}

bool CProxyFtp::Login(SOCKET hSock)
{
	return Login(hSock, m_data, ""/*TOfIX acct*/, m_nFtpProxyType); 
}

SOCKET CProxyFtp::Connect(const char *szHost, unsigned short nPort)
{
	//connect to proxy
	xSocket socket;
	socket.Open();

	if(szHost){
		Trace(TT_INFO, "Connecting to FTP proxy (%s:%d)\r\n", szHost, nPort);

		if(socket.Connect(szHost, nPort))
			Trace(TT_INFO, "Connected to proxy!\r\n");
		else
			Trace(TT_INFO, "Failed to connect to proxy!\r\n");
	
	}
	else{
		Trace(TT_INFO, "Connecting to FTP proxy (%s:%d)\r\n", m_data.strProxyHost.c_str(), m_data.nProxyPort);

		if(socket.Connect(m_data.strProxyHost.c_str(), m_data.nProxyPort))
			Trace(TT_INFO, "Connected to proxy!\r\n");
		else
			Trace(TT_INFO, "Failed to connect to proxy!\r\n");
	}

	return socket.Detach();
}

int CProxyFtp::GetReplyDigit()
{
	int nDigit = -1;
	int nLastLine = m_list.size()-1;
	if(nLastLine>=0)
		nDigit = m_list[nLastLine].at(0) - 48;

	m_list.clear();
	return nDigit;
}

bool CProxyFtp::ParseBuffer(int &nStart)
{
	String strLine, strNumCode;
	int  nLines = 0;

	//extract text lines from buffer
	//(there might be more then one line in the buffer after each read)
	while(true)
	{
		//copy the buffer into string
		strLine = m_szBuffer;

		//search for end of the line in the buffer
		//NOTE: this covers both "\n" and "\r\n" line termination
		int nPos = strLine.Find("\n");

		if(nPos != -1)	//we found the end of the line
		{
			nPos += 1;	//point behind end of line
			
			//eat found line from the buffer
			memmove(m_szBuffer, m_szBuffer+nPos, sizeof(m_szBuffer)-nPos);
			
			//store line of text into result list
			strLine = strLine.substr(0, nPos).c_str();

			m_list.push_back(strLine.c_str());
			nLines ++;

			//update pointers
			nStart -= nPos;
		}
		else
			break;	//leave internal while loop (go reading again)
	}

	return true;
}

