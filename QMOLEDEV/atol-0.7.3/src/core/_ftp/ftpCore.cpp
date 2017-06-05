////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "ftpCore.h"

#include <stdarg.h>
#include <sys/stat.h>	//stat
#ifdef _WIN32
 #include <io.h>		//access
 #include <direct.h>	//_getcwd
 #define stat _stat
#else
 #include <unistd.h>	//ftruncate
 #include <sys/types.h>	//ftruncate
 #include <sys/socket.h>
 #include <linux/in.h>
 #include <linux/net.h>
 #include <ctype.h>
#endif
//#include "../NameInputDlg.h"
#include "../debug.h"

#define  UC(b)				(((int)b)&0xff)
#define  bzero(addr, len)	memset(addr, 0, len)

//telnet protocol special codes
static const char IAC	= (char)255;	/* interpret as command: */
static const char IP	= (char)244;	/* interrupt process--permanently */
static const char SYNCH	= (char)242;	/* for telfunc calls */

//ftp protocol commands
#define  CMD_ABORT	"ABOR"		// abort current operation
#define  CMD_QUIT	"QUIT"		// close ftp session
#define  CMD_USER	"USER %s"	// send user name
#define  CMD_PASS	"PASS %s"	// send password
#define  CMD_ACCT	"ACCT %s"	// send account information
#define  CMD_PORT	"PORT %d,%d,%d,%d,%d,%d"	//send IP/port info of the listening socket
#define  CMD_TYPE_I	"TYPE I"	// set binary type
#define  CMD_TYPE_A	"TYPE A"	// set ASCII type
#define  CMD_RETR	"RETR %s"	// download (retreive)
#define  CMD_STOR	"STOR %s"	// upload (store)
#define  CMD_CWD	"CWD %s"	// set current working directory
#define  CMD_LIST	"LIST"		// list current working directory
#define  CMD_NLST	"NLST"		// list current working directory (file names only)
#define  CMD_HELP	"HELP"		// give list of all supprted commands
#define  CMD_HELP1	"HELP %s"	// explain specific command
#define  CMD_PWD	"PWD"		// get current working directory
#define  CMD_SYST	"SYST"		// get remote system description
#define  CMD_SIZE	"SIZE %s"	// get file size
#define  CMD_DELE	"DELE %s"	// delete given file
#define  CMD_MKD	"MKD %s"	// make directory
#define  CMD_RMD	"RMD %s"	// remove empty directory
#define  CMD_RNFR	"RNFR %s"	// rename from
#define  CMD_RNTO	"RNTO %s"	// rename to
#define  CMD_NOOP	"NOOP"		// no action - does nothing (TOFIX to implement)
#define  CMD_PASV	"PASV"		// passive mode - server listens for data connection
#define  CMD_REST	"REST %d"	// restart transfer from given file pos
//#define  CMD_LIST1	"LIST %s"	// list given directory name
//#define  CMD_CDUP		"CDUP"		// equal to "CD .." in DOS (TOFIX to implement)
//MODE, ,

//FTPS commands
#define  CMD_TLS	"AUTH TLS"	// Start TLSv1 connection
#define  CMD_SSL	"AUTH SSL"	// Start SSL connection
#define  CMD_PROT_P	"PROT P"	// Setup secure protocol
#define  CMD_PROT_C	"PROT C"	// Setup secure protocol
 

#define SOCKET_READ_TOUT	30000	// mseconds -> 30 sec
#define SOCKET_WRITE_TOUT	20000	// mseconds -> 20 sec

//////////////////////////////////////////////////////////////////////
//Construction/Destruction
//////////////////////////////////////////////////////////////////////

CFtpClient::CFtpClient()
{
	m_bAbort		= false;
	m_bConnected	= false;
	m_bLogged		= false;
	m_bSystQueried	= false;
	m_bPassiveMode  = false;

	m_szBuffer[0]	= '\0';

	m_eTransferType = ftpTtype_BINARY; //default
//	m_EventFree.SetEvent(); //TOFIX

	m_pProgressInfo = NULL;
	m_pProxy		= NULL;
	m_pContext		= NULL;
	m_pfnTrace		= NULL;
	m_dwTraceData	= 0;

	//listing options
	m_nEncryptType = CFtpInfo::ENCRYPT_NONE;
	m_nEncryptData = false;
	m_nListLongFormat = false;
	m_nListShowHidden = false;
	m_nListResolveLinks = false;
	m_nListCompleteTime = false;
	m_pSockListen = new xSocket();
	m_pSockData = NULL;	// They can be not just plain sockets
	m_pSockCtrl = NULL;
}

CFtpClient::~CFtpClient()
{
	DoClose();	//Justin Case :)

	DestroySockets();
	if(m_pContext)
		delete m_pContext;

	//TOFIX free entire proxy chain
	if(m_pProxy)
		delete m_pProxy;
}

void CFtpClient::DestroySockets() 
{
	if(m_pSockListen) {
		m_pSockListen->Close();
		delete m_pSockListen;
		m_pSockListen = NULL;
	}
	if(m_pSockData) {
		m_pSockData->Close();
		delete m_pSockData;
		m_pSockData = NULL;
	}
	if(m_pSockCtrl) {
		m_pSockCtrl->Close();
		delete m_pSockCtrl;
		m_pSockCtrl = NULL;
	}
}

//accepts data connection between server and client
int CFtpClient::AcceptConnection()
{
	int sockfd = m_pSockListen->Accept(10); //10 sec timeout
	m_pSockListen->Close();

	if (sockfd < 0) 
	{
        TRACE("ERROR: Accept failed\n");
		DoTrace(TT_ERROR, "Accept failed.\r\n");
		//AfxMessageBox("Error: Accept failed!");
		return INVALID_SOCKET;
    }

	DoTrace(TT_INFO, "Incoming connection accepted.\r\n");

	m_pSockData->Attach(sockfd);
	if(m_nEncryptData){
		((xSSLSocket *)m_pSockData)->SSLHandshake(*m_pContext, true);
	}

    return sockfd;
}

bool CFtpClient::SendFtpCmd(const char *szFormat, ... )
{
	//check socket
	if(!m_pSockCtrl->IsValid()){
		DoTrace(TT_ERROR, "Connection terminated.\n");
		LocalClose();
		return false;
	}

	//format command string
	char szBuffer[512];
	va_list valist;
	va_start(valist, szFormat);
	/*int nResult =*/ vsprintf(szBuffer, szFormat, valist);
	va_end(valist);

	//ASSERT(nResult > 0);

	//each FTP command is terminated with "\r\n"
	strcat(szBuffer, "\r\n");

	TRACE("Sending: [%s]\n", szBuffer);

	//log content sent over control socket
	//hide password in the log string
	if(0 == strncmp(szBuffer, "PASS", strlen("PASS")))
		DoTrace(TT_COMMAND, "PASS (hidden)\r\n");	
	else
		DoTrace(TT_COMMAND, szBuffer);
	
	int nRes = m_pSockCtrl->Write(szBuffer, strlen(szBuffer));
	if(nRes <= 0)
	{
		//TOFIX remove from core
		ReportError("Failed to send ftp command. Connection terminated.", false);
		LocalClose();
		//TOFIX callback function to refresh GUI
		return false;
	}

	return true;
}

int CFtpClient::ReadDataMsg(char *szBuffer, int len)
{
	int ret = m_pSockData->Read(szBuffer,len);
	if(ret <= 0)
		return 0;

	return ret;
}

int CFtpClient::SendDataMsg(const char *szBuffer, int len)
{
	return m_pSockData->Write(szBuffer,len, SOCKET_WRITE_TOUT);
}

int CFtpClient::GetListenSocket()
{
	char szIP[64];
	unsigned short nPort = 0;

	if(m_pProxy)
	{ 
		if(m_pProxy->ForcePassive()){
			//ASSERT(false);	//we should not come here in case of passive mode proxy
			return INVALID_SOCKET;
		}

		std::string strIP;
		unsigned short uPort;
		SOCKET hSock = m_pProxy->Listen(strIP, uPort);
		if(INVALID_SOCKET == hSock)
			return INVALID_SOCKET;

		//attach our listen socket to proxy listen socket
		m_pSockListen->Attach(hSock);
		
		strcpy(szIP, strIP.c_str());
		nPort = uPort;
	}
	else
	{
		m_pSockListen->Close();
		m_pSockListen->Open();

		// allow ftp server to connect
		// allow only one server
		if(!m_pSockListen->Listen(1)) 
		{
			TRACE("ERROR: listen\n");
			m_pSockListen->Close();
			return INVALID_SOCKET;
		}

		if(!ExtractPortInfo(szIP, nPort))
		{
			m_pSockListen->Close();
			return INVALID_SOCKET;
		}
	}

	DoTrace(TT_INFO, "Listening at %s:%d\r\n", szIP, nPort);

	if(!DoPort(szIP, nPort))
		return INVALID_SOCKET;

    return m_pSockListen->GetHandle();
}

//
// DownloadFile
// called to retrive a single file from remote host
//
bool CFtpClient::DownloadFile(const char *szSrcFile, const char *szDestFile, int nFromOffset)
{
	int  nTotal=0, nBytesRead=0;

	m_bAbort	 = false;
	m_bDownload  = true;

	// did we get a filename?
	if( !szSrcFile || ! (*szSrcFile)) {
	  TRACE("No file specified.\n");
	  return false;
	}

	int nStartPos = 0;

//TOFIX	m_EventFree.ResetEvent();

	//send to 
	if(!SetTransferType(m_eTransferType))
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	//get the size of remote file (to track progress)
	//TOFIX cache this - use the size from listing?
	int fsize = GetRemoteFileSize (szSrcFile);
	//ASSERT(fsize > 0);

	if( !InitDataConn() )
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	//TOFIX CFile64
	FILE *fp=NULL;

	if(nFromOffset > 0)
	{
		//TOFIX read 30 bytes at the end
		//TOFIX those overlapping bytes must be used to check if this is the same file
		if(!(fp=fopen(szDestFile, (m_eTransferType==ftpTtype_ASCII) ? "at" : "ab"))) {
			TRACE("ERROR: file open failed\n");
			//TOFIX m_EventFree.SetEvent();
			return false;
		}

		nStartPos = nFromOffset;

		//truncate existing file to the given offset
#ifdef _WIN32
		_chsize(fileno(fp), nStartPos);	//TOFIX err handling
#else
		ftruncate(fileno(fp), nStartPos); //TOFIX err handling
#endif
		nTotal	= nStartPos; 	

		//send "restart transfer" command
		if(!FtpRest(nStartPos)){
			fclose(fp); //TOFIX delete file
			return false;
		}
		//NOTE: next ftp command after REST must be either RETR or STOR
	}
	else
	{
		// open the file with current mode
		if(! (fp=fopen(szDestFile, (m_eTransferType==ftpTtype_ASCII) ? "wt" : "wb"))) {
			TRACE("ERROR: file open failed\n");
			//TOFIX m_EventFree.SetEvent();
			return false;
		}

		nStartPos = 0;
	}

	// send command to server and read response
	if(!SendFtpCmd(CMD_RETR, szSrcFile)) 
	{
		//TOFIX m_bConnected = false
		fclose(fp); //TOFIX delete file
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	if( !StartDataConn() )
	{
		//TOFIX m_bConnected = false ????????
		fclose(fp); //TOFIX delete file
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	ReadReplySingle(*m_pSockCtrl);	//read command reply
	if(!Handle_Retr())
	{
		//TOFIX m_bConnected = false
		fclose(fp); //TOFIX delete file
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	if(NULL != m_pProgressInfo)
	{
		String strSrc(m_strCwd.c_str());
		if(strSrc.at(strSrc.size()-1) != '/' && strSrc.at(strSrc.size()-1) != '\\')
			strSrc.insert(strSrc.size(), "/");
		strSrc += szSrcFile;

		//NOTE do this before initsingle
		m_pProgressInfo->InitCurrentFiles(strSrc.c_str(), szDestFile);
		m_pProgressInfo->InitCurrentProgress(nStartPos, fsize);
	}

	m_pSockData->WaitUntilReadable();	//infinite

	//TOFIX use: FtpReadData
	// now get file and store
	while( (nBytesRead = ReadDataMsg(m_szBuffer, sizeof(m_szBuffer))) > 0) 
	{
		fwrite(m_szBuffer, nBytesRead, 1, fp);
		nTotal += nBytesRead;

		if(NULL != m_pProgressInfo)
			m_pProgressInfo->SetPos(nTotal);

		// did we abort?
		if(m_bAbort) 
			break;

		m_pSockData->WaitUntilReadable(SOCKET_READ_TOUT);	//max 1 sec
	}

	fclose(fp);
	
	if(m_bAbort) 
	{
		//::DeleteFile(szDestFile);	//delete local file
	}
	else
	{
		m_pSockData->Close();
		//ASSERT(nTotal == fsize); //transfer broke
		ReadReply(m_ReplyList, *m_pSockCtrl);
	}

	//TOFIX m_EventFree.SetEvent();
	m_bAbort = false;
	return true;
}

// UploadFile
// called to transfer a file to the remote host using the current
// file transfer mode.  it's just like GetFile.
bool CFtpClient::UploadFile(const char *szSrcFile, const char *szDestFile, int nFromOffset)
{
	//TOFIX CFile64
	FILE *fp=NULL;
	int nTotal=0, nBytesRead=0;

	m_bAbort	 = false;	
	m_bDownload  = false;

	if( !szSrcFile || ! (*szSrcFile)) {
		TRACE("No file specified.\n");
		return false;
	}

	if(! (fp=fopen(szSrcFile,(m_eTransferType==ftpTtype_ASCII) ? "rt" : "rb"))) {
		TRACE("ERROR: file open\n");
		return false;
	}

	//TOFIX m_EventFree.ResetEvent();

	//refresh transfer type
	if(!SetTransferType(m_eTransferType))
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	int nStartPos = 0;

	//TOFIX use xFile::...
	//get file size (for progress)
	struct stat st;;
	stat(szSrcFile, &st);
	int fsize = st.st_size;
	//ASSERT(fsize > 0);

	//provjera da li na serveru postoji datoteka istog imena
	//uz upit da li �mo resumati puknuti upload
	int nRemoteSize = GetRemoteFileSize (szDestFile);
	if(nRemoteSize > 0)
	{
		if(!InitDataConn())
		{
			fclose(fp); //TOFIX delete file
			//TOFIX m_EventFree.SetEvent();
			return false;
		}

		if(nFromOffset > 0)
		{
			fseek(fp, nFromOffset /*nRemoteSize*/, SEEK_SET);
			nStartPos = nFromOffset /*nRemoteSize*/;
			nTotal	= nStartPos; 	
		}
		else
			nStartPos = 0;

		//send "restart transfer" command
		if(!FtpRest(nStartPos)){
			fclose(fp); //TOFIX delete file
			return false;
		}
	}
	else
	{
		if(!InitDataConn())
		{
			//TOFIX m_bConnected = false ??
			fclose(fp); //TOFIX delete file
			//TOFIX m_EventFree.SetEvent();
			return false;
		}
	}	

	// send command to server & read reply
	if(!SendFtpCmd(CMD_STOR, szDestFile))
	{
		//TOFIX m_bConnected = false ??
		fclose(fp); 
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	if(!StartDataConn())
	{
		//TOFIX m_bConnected = false ??

		fclose(fp); //TOFIX delete file
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	ReadReplySingle(*m_pSockCtrl);
	if(!Handle_Stor())
	{
		//TOFIX m_bConnected = false ??
		fclose(fp);
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	//ReadReply(m_ReplyList, *m_pSockCtrl);

	if(NULL != m_pProgressInfo){
		std::string strDest(m_strCwd.c_str());
		if(strDest.at(strDest.size()-1) != '/' && strDest.at(strDest.size()-1) != '\\')
			strDest.insert(strDest.end(), '/');
		strDest += szDestFile;

		//NOTE do this before initsingle
		m_pProgressInfo->InitCurrentFiles(szSrcFile, strDest.c_str());
		m_pProgressInfo->InitCurrentProgress(nStartPos, fsize);
	}

	//wait until stream is ready to accept data
	m_pSockData->WaitUntilWritable(-1);

	// now send file
	nBytesRead = fread(m_szBuffer, 1, sizeof(m_szBuffer), fp);
	while(nBytesRead > 0) 
	{
		//TOFIX: abort if not all bytes transfered!
		//make sure everything is being sent
		int nSent = SendDataMsg(m_szBuffer, nBytesRead);
		//ASSERT(nBytesRead == nSent);
		
		//error handling
		if(nBytesRead != nSent)
		{
			//close data connection
			m_pSockData->Close();
			fclose(fp);
			ReadReply(m_ReplyList, *m_pSockCtrl);
			return false;	
		}

		nTotal+=nBytesRead;
	
		if(NULL != m_pProgressInfo)
			m_pProgressInfo->SetPos(nTotal);

		// send an abort command to server if we aborted.
		if(m_bAbort)
			break;

		nBytesRead = fread(m_szBuffer, 1, sizeof(m_szBuffer), fp);
		m_pSockData->WaitUntilWritable(SOCKET_WRITE_TOUT);	//max 1 sec
	}

	//close data connection
	m_pSockData->Close();
	fclose(fp);
	ReadReply(m_ReplyList, *m_pSockCtrl);

	//TOFIX m_EventFree.SetEvent();
	return true;
}

// this function is called when the o/open command is issued.
// it connects to the requested host, and logs the user in
void CFtpClient::DoOpen(CFtpInfo &ftpInfo)
{
	// do not do anything if we are already connected.
	if(m_bConnected){
	   TRACE("ERROR: Already connected. Close connection first.\n");
	   //ASSERT(false);
	   return;
	}

	m_bLogged = false;

	TRACE("Connecting to %s\n", (const char *)ftpInfo.m_strHost);

	m_pSockCtrl = new xSocket();
	m_pSockCtrl->Open();

	if(m_pSockCtrl->IsValid())
	{
		if(m_pProxy){
			SOCKET hSock = m_pProxy->Connect();
			if(INVALID_SOCKET == hSock)
				return;

			m_pSockCtrl->Attach(hSock);			// attach to proxy socket

			if(!m_pProxy->Login(hSock)){
				DoTrace(TT_ERROR, "Failed to login.\r\n");
				m_pSockCtrl->Close();
				return;
			}

			m_bConnected = true;				// we are now connected
			if(m_pProxy->ForcePassive())
				m_bPassiveMode = true;			// proxy overrides connection setting

			if(m_pProxy->NeedLogin())
			{
				ReadReply(m_ReplyList, *m_pSockCtrl);		// get reply (welcome message) from server
				HandleConnect();

				if(DoLogin(ftpInfo))				// send username and password
				{
					SetTransferType(m_eTransferType);	// set binary mode by default
				}
				else
				{
					LocalClose();
					DoTrace(TT_ERROR, "Error: failed to log to ftp server!");
				}
			}
		}
		else if(m_pSockCtrl->Connect(ftpInfo.m_strHost, ftpInfo.m_uPort, FTP_CONNECT_TOUT))	//20 sec timeout
		{
			/*
			setsockopt(hSocket,SOL_SOCKET,SO_LINGER,0,0);
			setsockopt(hSocket,SOL_SOCKET,SO_REUSEADDR,0,0);
			setsockopt(hSocket,SOL_SOCKET,SO_KEEPALIVE,0,0);
			*/

			TRACE("Connected\nLocal IP = [%s], Remote IP = [%s]\n", m_pSockCtrl->GetLocalIP().c_str(), m_pSockCtrl->GetRemoteIP().c_str());

			//remember host
			m_strHost = ftpInfo.m_strHost;

			m_bConnected = true;									// we are now connected
			ReadReply(m_ReplyList, *m_pSockCtrl, FTP_CONNECT_TOUT);	// get reply (welcome message) from server
			if(!HandleConnect()){
				LocalClose();
				return;
			}

			if(ftpInfo.m_nEncryptType)
			{
				if(SendFtpCmd(m_nEncryptType == CFtpInfo::ENCRYPT_TLSv1 ? CMD_TLS : CMD_SSL))
				{
					ReadReply(m_ReplyList, *m_pSockCtrl);
					if(m_ReplyList.size()==1 &&
						(0 == strncmp(m_ReplyList[0].c_str(), "234 ", strlen("234 "))
						|| 0 == strncmp(m_ReplyList[0].c_str(), "334 ", strlen("334 ")))) {
						xSSLSocket::InitSSLLib();
						if(!m_pContext)
						{
							m_pContext = new xSSLContext();
							m_pContext->Create(m_nEncryptType); //TOFIX: Check if created
						}
						xSSLSocket *s = new xSSLSocket();
						s->Attach(*m_pSockCtrl);
						if(s->SSLHandshake(*m_pContext, false))
						{
							if(m_pSockCtrl)
							{
								delete m_pSockCtrl;
							}
							if(m_nEncryptData) {
								ASSERT(NULL == m_pSockData);
								m_pSockData = new xSSLSocket();
							}
							m_pSockCtrl = s;
							SendFtpCmd(m_nEncryptData ? CMD_PROT_P : CMD_PROT_C);
							ReadReply(m_ReplyList, *m_pSockCtrl);	//TOFIX: check reply
						} else {
							LocalClose();
							DoTrace(TT_ERROR, "Error: SSL handshake error.\r\n");
							return;
						}
					} else {
						LocalClose();
						DoTrace(TT_ERROR, "Error: Server doesn't support TLS.\r\n");
						return;
					}
				} else {
					LocalClose();
					DoTrace(TT_ERROR, "Error: failed to send command!\r\n");
					return;
				}
			} else {
				m_pSockData = new xSocket();
			}


			if(DoLogin(ftpInfo))		// send username and password
			{
				SetTransferType(m_eTransferType);	// set binary mode by default
			}
			else
			{
				LocalClose();
				DoTrace(TT_ERROR, "Error: failed to log to ftp server!\r\n");
			}
		}
		else
		{
			LocalClose();
			DoTrace(TT_ERROR, "Error: unknown or unreachable server !\r\n");
		}

		m_nEncryptType = ftpInfo.m_nEncryptType;
		m_nEncryptData = ftpInfo.m_nEncryptData;
		m_nListLongFormat = ftpInfo.m_nListLongFormat;
		m_nListShowHidden = ftpInfo.m_nListShowHidden;
		m_nListResolveLinks = ftpInfo.m_nListResolveLinks;
		m_nListCompleteTime = ftpInfo.m_nListCompleteTime;
		SetPassiveMode(ftpInfo.m_bPassiveMode);
	}
	else
	{
		LocalClose();
		DoTrace(TT_ERROR, "Error: failed to create socket!\r\n");
	}

	//FIX: it is important to know FTP system type prior to setting CWD
	//	   because some systems have nonstandard file systems (VMS has ":" as delimiter)
	if(!m_bSystQueried)
		DoSyst();
}

//TOFIX this will be main function, remove DoLogin?
//TOFIX input param should be some object instead of 6-5 params
//TOFIX support for GetPass dialog - use callback
// function to connect & log on to FTP server
// supports 9 firewalls

// this function logs the user into the remote host.
// prompts for username and password
bool CFtpClient::DoLogin(CFtpInfo &ftpInfo)
{
	if(m_bConnected)
	{
		if(!DoUser(ftpInfo.m_bAnonymous ? "anonymous" : ftpInfo.m_strUser, ftpInfo.m_strAccount))
			return false;

		//NOTE anonymous login doesn't always require password
		if(!m_bLogged && !DoPass(ftpInfo.m_bAnonymous ? "guest" : ftpInfo.m_strPassword, ftpInfo.m_strAccount)){
			return false;
		}

		//remember user data
		m_strUser = ftpInfo.m_strUser;
		m_strPass = ftpInfo.m_strPassword;

		return true;
	}
	else{
		TRACE("Not Connected.\n");
		//ASSERT(false);
	}

	return false;
}

// closes connection to the ftp server
void CFtpClient::DoClose()
{
   if( !m_bConnected  ) {
     TRACE("Not Connected.\n");
   }
   else {
	   //TOFIX WaitForSingleObject()
	   //TOFIX m_EventFree.ResetEvent();

	   if(!SendFtpCmd(CMD_QUIT))
	   {
			//TOFIX m_EventFree.SetEvent();
			return;
	   }
	   ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	   Handle_Quit();
	   LocalClose();
	   //TOFIX m_EventFree.SetEvent();
   }
}

// perform directory listing i.e: ls
bool CFtpClient::DoList()
{
	if( !m_bConnected ) {
		TRACE("Not Connected.\n");
		return false;
	}

	//TOFIX WaitForSingleObject(m_EventFree, 10000);
	//TOFIX m_EventFree.ResetEvent();

	//transfer je tipa ASCII; pazi da ne zaboravi koja je korisni�a postavka
	FtpTransferType eType = m_eTransferType; 
	if(!SetTransferType(ftpTtype_ASCII))
	{
		m_eTransferType = eType;		//restore type
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	m_eTransferType = eType;		//restore type

	if( !InitDataConn() )
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	//prepare LIST command
	std::string strCmd(CMD_LIST);

	if(	m_nListLongFormat ||
		m_nListShowHidden  ||
		m_nListResolveLinks  ||
		m_nListCompleteTime )
	{
		strCmd += " -";
		if(m_nListLongFormat)
			strCmd += "l";
		if(m_nListShowHidden)
			strCmd += "a";
		if(m_nListResolveLinks)
			strCmd += "L";
		if(m_nListCompleteTime)
			strCmd += "T";
	}

	// send command to server and get response
	if(!SendFtpCmd(strCmd.c_str()))
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	//FIX: some servers prefer to first start data connection, 
	//	   then to send LIST reply, others like just the oposite !!!	
	if(m_pSockCtrl->WaitUntilReadable(150))	//wait for X ms
	{
		//read single-line command reply
		ReadReplySingle(*m_pSockCtrl);	
		if(!Handle_List())
			return false;

		if(!StartDataConn()){
			//on error, switch to passive mode and repeat operation
			//(in case error due to wrong mode settings - firewall present, ...)
			if(!m_bPassiveMode){
				m_bPassiveMode = true;
				DoTrace(TT_INFO, "Switching to passive mode of work.\r\n");
				return DoList();  //repeat operation
			}

			//TOFIX m_EventFree.SetEvent();
			return false;
		}
	}
	else
	{
		if(!StartDataConn()){
			//on error, switch to passive mode and repeat operation
			//(in case error due to wrong mode settings - firewall present, ...)
			if(!m_bPassiveMode){
				m_bPassiveMode = true;
				DoTrace(TT_INFO, "Switching to passive mode of work.\r\n");
				return DoList();  //repeat operation
			}

			//TOFIX m_EventFree.SetEvent();
			return false;
		}

		//read single-line command reply
		ReadReplySingle(*m_pSockCtrl);	
		if(!Handle_List())
			return false;
	}

	if(m_pSockData->IsValid())
	{
		DoTrace(TT_INFO, "Reading data socket.\r\n");

		//read directory listing from data socket (as list of text lines)
		m_lstDirRaw.clear();
		ReadReply(m_lstDirRaw, *m_pSockData);

		DoTrace(TT_INFO, "Closing data socket.\r\n");
		m_pSockData->Close();

#ifdef _DEBUG
#ifdef _WIN32
		//measure FTP list parsing speed
		unsigned int uStart = GetTickCount();
#endif
#endif
		m_Listing.Clear();
		m_ListParser.ParseList(m_lstDirRaw, m_Listing);

#ifdef _DEBUG
#ifdef _WIN32
		TRACE("FTP: Parsed %d files in %d miliseconds\n", m_Listing.GetCountRaw(), GetTickCount()-uStart);
#endif
#endif

		DoTrace(TT_INFO, "List parsed.\r\n");
	}

#ifdef _DEBUG
#ifdef _WIN32
	//measure FTP reply wait
	unsigned int uStart = GetTickCount();
#endif
#endif

	//TOFIX does this need to be skipped in PASV mode ???
	// read response
	ReadReply(m_ReplyList, *m_pSockCtrl);	//TOFIX parse this

#ifdef _DEBUG
#ifdef _WIN32
	TRACE("FTP: Listing done reply %d miliseconds\n", GetTickCount()-uStart);
#endif
#endif

#ifdef _DEBUG
	//m_Listing.Dump();
#endif

	//TOFIX m_EventFree.SetEvent();
	return true;
}

// change to another directory on the remote system
void CFtpClient::DoCD(const char * szDir)
{
	if(m_bConnected)
	{
		//TOFIX m_EventFree.ResetEvent();

		const char *dir = szDir;

		// ignore leading whitespace
		while( *dir && (*dir == ' ' || *dir == '\t') ) 
		   dir++;

		// if dir is not specified, read it in
		if( ! (*dir) ) {
			TRACE("Remote directory name missing\n");
			return;
		}
   
		// send command to server and read response
		if(!SendFtpCmd(CMD_CWD, dir))
			return;
		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Cwd();	//TOFIX if(!)

		//TOFIX refresh szCwd here? (so far we do it in Pwd)
		//TOFIX m_EventFree.SetEvent();
	}
	else
		TRACE("Not Connected.\n");
}

// set file transfer mode to ascii or binary
bool CFtpClient::SetTransferType(FtpTransferType eTransferType)
{
	m_eTransferType = eTransferType;

	if (m_bConnected)
	{
		char chArg;

		switch (eTransferType)
		{
		case ftpTtype_ASCII:	chArg = 'A'; break;
		case ftpTtype_BINARY:	chArg = 'I'; break;
		case ftpTtype_EBCDIC:	chArg = 'E'; break;
		default:
			return false;
		}

		if(!DoType(chArg))
			return false;
	}

	return true;
}

bool CFtpClient::DoType(int chType)
{
	if(!SendFtpCmd("TYPE %c", chType))
		return false;

	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	Handle_Type();	//TOFIX if(!)
	return true;
}

// sends a help command to the server
void CFtpClient::DoRhelp(const char *szCmd)
{
	if(m_bConnected)
	{
		//TOFIX m_EventFree.ResetEvent();

		if(szCmd)
		{
			if(!SendFtpCmd(CMD_HELP1, szCmd))
				return;
		}
		else
		{
			if(!SendFtpCmd(CMD_HELP))
				return;
		}

		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Help();	//TOFIX if(!)

		//TOFIX m_EventFree.SetEvent();
	}
	else
		TRACE("Not Connected.\n");
}

// retrieves the current directory on the remote host
void CFtpClient::DoPWD()
{
	if(m_bConnected) 
	{
		//TOFIX m_EventFree.ResetEvent();

		if(!SendFtpCmd(CMD_PWD))
			return;
		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Pwd();	//TOFIX if(!)

		//TOFIX m_EventFree.SetEvent();
	}
	else
		TRACE("Not Connected.\n");
}

void CFtpClient::DoSyst()
{
	if(m_bConnected)
	{
		if(!SendFtpCmd(CMD_SYST))
			return;
		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Syst();	//TOFIX if(!)
	}
	else
		TRACE("Not Connected.\n");
}

const char *  CFtpClient::GetSystemType()
{
	if(m_bConnected) 
	{
		//TOFIX m_EventFree.ResetEvent();

		if(!m_bSystQueried)
			DoSyst();

		//TOFIX m_EventFree.SetEvent();

		return m_ListParser.GetSystemType();
	}
	else
	{
		TRACE("Not Connected.\n");
		return NULL;
	}
}

void CFtpClient::SetProgress(OpState *pInfo)
{
	m_pProgressInfo = pInfo;
}

int CFtpClient::GetRemoteFileSize(const char * szFile)
{
	if(m_bConnected)
	{
		//TOFIX m_EventFree.ResetEvent();

		if(!SendFtpCmd(CMD_SIZE, szFile))
		{
			//TOFIX m_EventFree.SetEvent();
			return -1;
		}

		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply

		int size;
		Handle_Size(size);

		//TOFIX m_EventFree.SetEvent();

		return size;
	}
	else
		TRACE("Not Connected.\n");

	return -1;
}	

bool CFtpClient::DeleteFile(const char * szFile)
{
	if(m_bConnected)
	{
		//TOFIX m_EventFree.ResetEvent();

		if(!SendFtpCmd(CMD_DELE, szFile))
		{
			//TOFIX m_EventFree.SetEvent();
			return false;
		}

		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Dele();	//TOFIX if(!)	

		//TOFIX m_EventFree.SetEvent();

		return true;
	}
	else
	{
		TRACE("Not Connected.\n");
		return false;
	}
}

bool CFtpClient::MakeDirectory(const char * szDir)
{
	if(m_bConnected)  
	{
		//TOFIX m_EventFree.ResetEvent();

		if(!SendFtpCmd(CMD_MKD, szDir))
		{
			//TOFIX m_EventFree.SetEvent();
			return false;
		}
		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Mkd();	//TOFIX if(!)

		//TOFIX m_EventFree.SetEvent();

		return true;
	}
	else
	{
		TRACE("Not Connected.\n");
		return false;
	}
}

bool CFtpClient::RemoveDirectory(const char * szDir)
{
	if(m_bConnected)
	{
		//TOFIX m_EventFree.ResetEvent();

		if(!SendFtpCmd(CMD_RMD, szDir))
		{
			//TOFIX m_EventFree.SetEvent();
			return false;
		}
		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Rmd();	//TOFIX if(!)

		//TOFIX m_EventFree.SetEvent();

		return true;
	}
	else
	{
		TRACE("Not Connected.\n");
		return false;
	}
}

//rename file or directory on remote end of FTP Vfs
int CFtpClient::DoRename(const char * szOldName, const char * szNewName)
{
	//check the parameters
	//ASSERT(NULL != szNewName && '\0' != szNewName);
	//ASSERT(NULL != szOldName && '\0' != szOldName);

	//TOFIX remove this ???
	if(NULL == szNewName || '\0' == szNewName)
	{
		TRACE("Invalid file name.\n");
		return -1;
	}

	if(m_bConnected)
	{
		//TOFIX m_EventFree.ResetEvent();

		if(!SendFtpCmd(CMD_RNFR, szOldName))
		{
			//TOFIX m_EventFree.SetEvent();
			return -1;
		}
		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Rnfr();	//TOFIX if(!)

		if(!SendFtpCmd(CMD_RNTO, szNewName))
		{
			//TOFIX m_EventFree.SetEvent();
			return -1;
		}
		ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
		Handle_Rnto();	//TOFIX if(!)

		//TOFIX m_EventFree.SetEvent();

		return 1;
	}
	else
	{
		TRACE("Not Connected.\n");
		return -1;
	}
}

bool CFtpClient::ExtractPortInfo(char *szIP, unsigned short &nPort)
{
	//ASSERT(NULL != szIP);

    *szIP = '\0';

	struct sockaddr_in  serv_addr, TempAddr;

	//TOFIX move functionality into xSocket class
	//take port out of listen socket
	#ifdef _WIN32
		int len = sizeof(serv_addr);
	#else
		socklen_t len = sizeof(serv_addr);
	#endif

    if(getsockname(*m_pSockListen, (struct sockaddr *)&serv_addr, &len)<0)
	{
		TRACE("ERROR: getsockname\n");
		m_pSockListen->Close();
		return false;
    }

	//take IP out of control socket
    len = sizeof(TempAddr);
    if(getsockname(*m_pSockCtrl, (struct sockaddr *)&TempAddr, &len)<0)
	{
		TRACE("ERROR: getsockname\n");
		m_pSockListen->Close();
		return false;
    }

    char *ipaddr	= (char *)&TempAddr.sin_addr;
    char *port		= (char *)&serv_addr.sin_port;

	nPort = UC(port[0]) * 256 + UC(port[1]);
    sprintf(szIP, "%d.%d.%d.%d", UC(ipaddr[0]), UC(ipaddr[1]), UC(ipaddr[2]), UC(ipaddr[3]));

	return true;
}

//TOFIX return number of bytes read
//TOFIX additional timeout parameter
//read from control socket into list of text lines
//ASSUMING:
//	1. szBuffer[1025] is large enough for one line of text (usualy under 80)
//	2. each line is terminated with "\r\n" or "\n"
int CFtpClient::ReadReply(CStrList &list, xSocket &sock, int nMsTimeout)
{
	list.clear();	//make sure result list is initially empty

	int	nStart	= 0;	//points to free part of buffer
	//int	nTotalRead = 0;

	//some servers don't give result on some commands ?
	if(!sock.WaitUntilReadable(nMsTimeout)) //wait max 20 seconds
		return 0; // 0 bytes read

	TRACE("CFtpClient::ReadReply: Going to read some data\n");

	//if(!sock.WaitUntilReadable(SOCKET_READ_TOUT)) //wait max 10 seconds
	//	return;

	bool bMultiline = false;
	//bool bEndline	= false;
	int  nSleep		= 0;

	while(true)
	{
		//read from socket into free part of buffer (slow a little due to Windows NT servers)
		int nRes = sock.Read(m_szBuffer+nStart, sizeof(m_szBuffer)-nStart-1);
		if(nRes < 0)
		{
			//TOFIX test socket on error u IsValid ugraditi provjeru 
			TRACE("Reply: Socket read error: [%s]\n", sock.GetErrorText(sock.GetError()).c_str());
			if(bMultiline)
			{
			#ifdef _WIN32
				Sleep(3);
			#else
				usleep(3000);  //3ms
			#endif
				nSleep += 3;

				if(nSleep>3000)	//prevent eternal looping
					break;
				
				continue;
			}
			else{
				sock.Close();
				break; 
			}
		}
		else if(nRes == 0)
		{
			TRACE("Reply: No more to read, socket closed\n");
			sock.Close();
			break;
		}
		else{
			TRACE("Read %d bytes\n", nRes);
			nSleep = 0;

			//increment pointer to free buffer
			nStart += nRes;
		}

		//terminate string so we can search inside it
		m_szBuffer[nStart] = '\0';	

		if(!ParseBuffer(list, bMultiline, nStart, sock))
			break;
		
		//FIX listing of big directories can end sooner than it should
		if(sock == *m_pSockData)
			sock.WaitUntilReadable(SOCKET_READ_TOUT);	//max 1 sec
	}

//TOFIX move to list.Dump?
#ifdef _DEBUG
	if(list.size() > 0)
	{
		TRACE("Start of line list\n");
		for(unsigned int i=0; i<list.size(); i++)
			TRACE("%d:\t%s", i, list[i].c_str());
		TRACE("End of list\n");
	}
	else
		TRACE("ReadReply(): WARNING Reply list is empty\n");
#endif

	if(sock == *m_pSockCtrl){
		for(unsigned int j=0; j<list.size(); j++)
			DoTrace(TT_REPLY, list[j].c_str());
	}

	return nStart; //return number of bytes read
}

int CFtpClient::GetReplyCode(CStrList &list)
{
	//TOFIX da li se gleda prva ili zadnja linija odgovora?
	//ASSERT(list.size() > 0);
	
	//Reply lines start with certain numerical value. Examples:
	//"150 Opening BINARY mode data connection for mysql.zip"
	//"230 User logged in, proceed."
	//"530 Not logged in."
	//"331 User name okay, need password."
	if(list.size() > 0)
	{
		std::string strReply = list[0];

		//ASSERT(strReply.size() > 3);
		if(strReply.size() > 3)
		{
			if( isdigit(strReply.at(0)) &&
				isdigit(strReply.at(1)) &&
				isdigit(strReply.at(2)))
			{
				std::string strNum = strReply.substr(0, 4);
				int nNum = atoi(strNum.c_str());

				return nNum;
			}
		}
	}

	return -1;
}

//handle result of the PORT command
bool CFtpClient::Handle_Port()
{
	//"200 PORT command successful"
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(200 == nCode)
		return true;

	ReportError("PORT");
	return false;
}

bool CFtpClient::Handle_Retr()
{
	//TOFIX (110)
	//"150 Preparing to transfer binary file"
	//"125 Data connection already open; Transfer starting."
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(125 == nCode || 150 == nCode || 226 == nCode || 250 == nCode)
		return true;

	ReportError("RETR");
	return false;
}

bool CFtpClient::Handle_Stor()
{
	//TOFIX (110)
  
	//"125 Data connection already open; transfer starting."
	//"150 Opening BINARY mode data connection for mysql.zip"
	//"250 Requested file action okay, completed."
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(125 == nCode || 150 == nCode || 226 == nCode || 250 == nCode)
		return true;

	ReportError("STOR");
	return false;
}

bool CFtpClient::Handle_User(const char * szAccount)
{
	//TOFIX poseban handler koji u slu�ju potrebe starta dijalog za unos lozinke
	//230 User logged in proceed
	//331 Password required for miro.
	//331 Anonymous access ok: give e-mail address as password
	//"332 Need account for login." -> zahtjeva ACCT naredbu
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(230 == nCode)
	{
		m_bLogged = true;
		return true;
	}
	else if(331 == nCode)
		return true;

	//if account needed
	if(332 == nCode)
		return DoAccount(szAccount);

	ReportError("USER");
	return false;
}

bool CFtpClient::Handle_Pass(const char * szAccount)
{
	//TOFIX poseban handler koji u slu�ju potrebe starta dijalog za unos lozinke
	//230 User anonymous logged-in
	//202 Command not implemented, superfluous at this site.
	//332 Need account for login." -> zahtjeva ACCT naredbu
	//221 Goodbye
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(230 == nCode || 202 == nCode)
		return true;
	//if account needed
	if(332 == nCode)
		return DoAccount(szAccount);

	//NOTE: recursive! give user more chances to enter right password
	if(DoPass(NULL, szAccount))
		return true;

	ReportError("PWD");
	return false;
}

bool CFtpClient::Handle_Acct()
{
	//230 User logged-in
    //202 Command not implemented, superfluous at this site.
    //"530 Not logged in.
    //500, 501, 503, 421
	//501 Syntax error in parameters or arguments.
    //503 Bad sequence of commands.
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(230 == nCode || 202 == nCode)
		return true;

	ReportError("ACCT");
	return false;
}

bool CFtpClient::Handle_Cwd()
{
	//250 CWD command successful
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(250 == nCode)
		return true;

	ReportError("CWD");
	return false;
}

bool CFtpClient::Handle_Quit()
{
	//221 Goodbye.
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(221 == nCode)
		return true;
	return false;
}

bool CFtpClient::Handle_List()
{
	//125 Data connection already open; transfer starting.
	//150 Opening BINARY mode data connection for /bin/ls
	//150 File status okay; about to open data connection.
	//226 Closing data connection. Requested file action successful
	//250 Requested file action okay, completed.
	//200 Command okay
	//TOFIX more ????
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(150 == nCode || 125 == nCode || 200 == nCode || 226 == nCode || 250 == nCode)
		return true;

	ReportError("LIST");
	return false;
}

bool CFtpClient::Handle_Type()
{
	//200 Type set to I.
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(200 == nCode)
		return true;

	ReportError("TYPE");
	return false;
}

bool CFtpClient::Handle_Help()
{
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(211 == nCode || 214 == nCode)
		return true;

	ReportError("HELP");
	return false;
}

bool CFtpClient::Handle_Syst()
{
	//"215 Windows 95 4.10"
	//"215 Windows_NT version 4.0"
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(215 == nCode)
	{
		//TOFIX izbaciti prefix 215 iz imena sistema
		//list parser object keeps string with system type
		std::string strSystem(m_ReplyList[0]);
		strSystem = strSystem.substr(strSystem.size() - 4, 4);

		m_ListParser.SetSystemType(strSystem.c_str());
		m_bSystQueried = true;

		return true;
	}

	ReportError("SYST");
	return false;
}

bool CFtpClient::Handle_Dele()
{
	//250 DELE command successful.
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(250 == nCode)
		return true;

	ReportError("DELE");
	return false;
}

bool CFtpClient::Handle_Mkd()
{
	//257 MKD command successful.
	//250 
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(257 == nCode || 250 == nCode)//TOFIX a to ako ima jo kodova (moda ispitiati raspon)?
		return true;

	ReportError("MKD");
	return false;
}

bool CFtpClient::Handle_Rmd()
{
	//250 RMD command successful.
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(250 == nCode)
		return true;

	ReportError("RMD");
	return false;
}

bool CFtpClient::Handle_Rnfr()
{
	//"350 Requested file action pending further information."
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(350 == nCode)
		return true;

	ReportError("RNFR");
	return false;
}

bool CFtpClient::Handle_Rnto()
{
	//"250 Requested file action okay, completed"
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(250 == nCode)
		return true;

	ReportError("RNTO");
	return false;
}

bool CFtpClient::Handle_Pwd()
{
	//257 "/home/miro" is current directory.
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(257 == nCode)
	{
		//extract directory from reply
		//ASSERT(m_ReplyList.size() > 0);

		String strLine = m_ReplyList[0].c_str();

		//TOFIX is this ok for all types of system ?
		int nPos = strLine.Find("\"");
		if(-1 != nPos)
		{
			strLine = strLine.substr(nPos+1, strLine.size() - nPos - 1).c_str();
			
			nPos = strLine.Find("\"");
			if(-1 != nPos)
				strLine = strLine.substr(0, nPos).c_str();

			m_strCwd = strLine;
			TRACE("Dir = %s\n", m_strCwd.c_str());
		}
		return true;
	}

	ReportError("PWD");
	return false;
}

bool CFtpClient::HandleConnect()
{
	//"220 HRCAK2.kron.hr FTP server (wu-2.4.2-academ-15 + security patches) ready."
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(220 == nCode)
		return true;

	ReportError("connect");
	return false;
}

bool CFtpClient::Handle_Rest()
{
	//"350 Restarting at 5295460."
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(350 == nCode)
		return true;

	//TOFIX s ovim rezultatom ipak radi (Koprivnica) "504 Reply marker must be 0."
	ReportError("REST");
	return false;
}

bool CFtpClient::Handle_Abor(CStrList &list)
{
	//"426 Transfer aborted. Data connection closed."
	int nCode = GetReplyCode(list);
	if(nCode < 0)
		return false;

	if(426 == nCode)
	{
		ReadReply(list, *m_pSockCtrl);	//read command reply
		nCode = GetReplyCode(list);
	}

	//226 Abort successful
	//225 ABOR command successful.
	if(225 == nCode || 226 == nCode)
		return true;

	return false;
}

bool CFtpClient::Handle_Pasv()
{
	//"227 Entering Passive Mode (195,77,24,81,128,67)"
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(227 == nCode)
	{
		//parsiraj odgovor i izvuci podatke o adresi na koju se moramo spojiti
		String strReply = m_ReplyList[0].c_str();
		
		int nPos = strReply.Find("(");
		if(-1 != nPos)
		{
			strReply = strReply.substr(nPos, strReply.size()-nPos).c_str();

			int nIP1, nIP2, nIP3, nIP4, nPort1, nPort2;
			/*int nRes =*/ sscanf(strReply.c_str(), "(%d,%d,%d,%d,%d,%d)\r\n", &nIP1, &nIP2, &nIP3, &nIP4, &nPort1, &nPort2);
			//ASSERT(6 == nRes);

			//store addres/port information
			m_strPassiveIP.Printf("%d.%d.%d.%d", nIP1, nIP2, nIP3, nIP4);
			m_nPassivePort = nPort1 * 256 + nPort2;

			return true;
		}
	}

	return false;
}

bool CFtpClient::Handle_Size(int &size)
{
	//"213 1427524"
	//"550 ASD.LOG: No such file or directory"
	int nCode = GetReplyCode(m_ReplyList);
	if(nCode < 0)
		return false;

	if(213 == nCode)
	{
		std::string strLine = m_ReplyList[0];
		strLine = strLine.substr(4, strLine.size()-4);
		size	= atoi(strLine.c_str());
		TRACE("Remote file size = %d\n", size);
		return true;
	}

	size = -1;

	if(550 != nCode)
		ReportError("SIZE");

	return false;
}

//NOTE: funkcija je namijenjena pozivu iz drugog threada
bool CFtpClient::DoAbort()
{
	m_bAbort = true;

	if(m_bDownload)
	{
		DoTrace(TT_INFO, "Sending abort command to abort current download.\r\n");
		
		//send data in urgent mode to notify operation abort
		static const char szTelnet[3] = { IAC, IP, IAC };
		if (send(*m_pSockCtrl, szTelnet, 3, MSG_OOB) != 3)
			return false;

		//send abort command (with SYNCH byte before it)
		if(!SendFtpCmd("%c%s", SYNCH, CMD_ABORT))
			return false;
		
		CStrList lstReply;
		ReadReply(lstReply, *m_pSockCtrl);	//read command reply
		Handle_Abor(lstReply);	//TOFIX if(!)
		
		m_pSockCtrl->Close();
	}
	else
	{
		//TOFIX sync using Mutexa
		//TOFIX WaitForSingleObject(m_EventFree, 60000); //TOFIX define
	}

	return true;
}

//TOFIX mijeam parsiranje s odlukom da li �tamo jo sa socketa
bool CFtpClient::ParseBuffer(CStrList &list, bool &bMultiline, int &nStart, xSocket &sock)
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
			list.push_back(strLine.c_str());

			if(0 == nLines)
			{
				strNumCode = strLine.substr(0, 3).c_str();

				//check for multiline response 
				if(sock == *m_pSockCtrl)
					if(strLine.size()>3 && '-' == strLine.at(3))
					{
						TRACE("Start of multiline reply: %s\n", strLine.c_str());
						bMultiline = true;
					}
					else{
						if(strlen(m_szBuffer) <= 2)
							return false;	//stop reading (not a multiline reply)
					}
			}
			else
			{
				// is it time to quit reading
				if(sock == *m_pSockCtrl)
					if(strLine.size()>3 && ' ' == strLine.at(3))
						if(strLine.substr(0, 3) == strNumCode)
						{
							TRACE("End of multiline reply: %s\n", strLine.c_str());
							return false;	//stop reading (End of multiline reply)
						}
			}

			nLines ++;

			//update pointers
			nStart -= nPos;
		}
		else{
			//not entire line loaded

			//peek if we can decide for multiline 
			if(sock == *m_pSockCtrl && strlen(m_szBuffer)>3){
				if('-' == m_szBuffer[3]){
					TRACE("Multiline response detected\n");
					bMultiline = true;
				}
			}

			break;	//leave internal while loop (go reading again)
		}
	}

	return true;
}

bool CFtpClient::InitDataConn()
{
	if(m_bConnected)
	{
		if(m_bPassiveMode)
		{
			if(!DoPasv())
				return false;
		}
		else
		{
			// obtain a listen socket
			if( GetListenSocket() < 0 )
			   return false;
		}
	}
	return true;
}

bool CFtpClient::StartDataConn()
{
	if(m_bPassiveMode)
	{
		//make data connection based on PORT data 
		if(m_pProxy){
			if(PROXY_SOCKS == m_pProxy->GetProxyType())
			{
				m_pProxy->m_data.strPeerHost = m_strPassiveIP;
				m_pProxy->m_data.nPeerPort	 = m_nPassivePort;

				//TOFIX sakrij ovu slozenost!!!
				SOCKET hSock = m_pProxy->Connect();	//connect to proxy
				if(INVALID_SOCKET == hSock)
					return false;

				if(!m_pProxy->Login(hSock))
					return false;

				//attach out data socket to proxy data socket
				m_pSockData->Attach(hSock);
			}
			else
			{
				//connect directly to the send PORT data 
				//(FTP proxy tweaks realy server reply and inserts data of its listening socket)
				SOCKET hSock = m_pProxy->Connect(m_strPassiveIP.c_str(), m_nPassivePort);
				if(INVALID_SOCKET == hSock)
					return false;

				//attach out data socket to proxy data socket
				m_pSockData->Attach(hSock);
			}
		}
		else
		{
			m_pSockData->Close();
			m_pSockData->Open();
			m_pSockData->Connect(m_strPassiveIP.c_str(), m_nPassivePort, FTP_CONNECT_TOUT);//20 sec timeout
			if(m_nEncryptData) {
				((xSSLSocket *)m_pSockData)->SSLHandshake(*m_pContext);
			}
		}
	}
	else
	{
		if(m_pProxy){
			//we were given IP/port where to connect
			std::string strIP;
			unsigned short uPort;
			if(!m_pProxy->Accept(m_pSockListen->GetHandle(), strIP, uPort)){
				m_pSockListen->Close();
				return false;
			}

			//listen socket will be used as data socket from now on
			m_pSockData->Attach(m_pSockListen->Detach());
		}
		else{
			// accept server connection
			if(AcceptConnection() <= 0)
				return false;
		}
	}

	return true;
}

String CFtpClient::FormatCmdError(const char * szDesc)
{
	//TOFIX set error string from outside?
	String strErr;
	for(unsigned int i=0; i<m_ReplyList.size(); i++)
		strErr += m_ReplyList[i].c_str();

	strErr.Replace("\r\n", "");

	//format and show error message
	String strMsg;
	strMsg.Printf("Command %s failed! (%s)\r\n", szDesc, strErr.c_str());
	return strMsg;
}

void CFtpClient::ReportError(const char * szDesc, bool bFormat /*= true*/)
{
	//TOFIX move this code out of class through callback function?
	if(bFormat)
		DoTrace(TT_ERROR, FormatCmdError(szDesc).c_str());
	else
		DoTrace(TT_ERROR, szDesc);
}

bool CFtpClient::DoPasv()
{
	if(!SendFtpCmd(CMD_PASV))
		return false;

	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	if(!Handle_Pasv())
		return false;

	m_bPassiveMode = true;

	return true;
}

bool CFtpClient::DoPort(const char *szIP, unsigned short nPort)
{
	char buffer[64];

	int nIP1, nIP2, nIP3, nIP4, nPort1, nPort2;
	/*int nRes =*/ sscanf(szIP, "%d.%d.%d.%d", &nIP1, &nIP2, &nIP3, &nIP4);
	//ASSERT(4 == nRes);

	nPort1 = nPort / 256;
	nPort2 = nPort % 256;

    sprintf(buffer, CMD_PORT, nIP1, nIP2, nIP3, nIP4, nPort1, nPort2);

    if(!SendFtpCmd(buffer))
		return false;
	
	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	if(!Handle_Port())
		return false;

	return true;
}

bool CFtpClient::DoRetr(const char *szFile)
{
	if(!SendFtpCmd(CMD_RETR, szFile))
		return false;

	ReadReply(m_ReplyList, *m_pSockCtrl);
	if(!Handle_Retr())
		return false;

	return true;
}

bool CFtpClient::DoStor(const char *szFile)
{
	if(!SendFtpCmd(CMD_STOR, szFile))
		return false;

	ReadReply(m_ReplyList, *m_pSockCtrl);
	if(!Handle_Stor())
		return false;

	return true;
}

bool CFtpClient::DoUser(const char *szUserName, const char *szAccount)
{
	char szUser[256] = "";

	// if user name was not provided ask it
	if(!szUserName || !(*szUserName))
	{
/*
		//TOFIX
		CNameInputDlg dlg;
		//TOFIX dlg.SetTitleString("User name");
		//TOFIX dlg.SetDescString("Please enter user name for this connection");
		if(wxOK == dlg.ShowModal()){
			strncpy(szUser, dlg.m_strValue, sizeof(szUser));//TOFIX provjera da li je dovoljno dug unos
			szUser[sizeof(szUser)-1] = '\0';
		}
		else
			return false;	//TOFIX close connection here
*/
	}
	else{
		strncpy(szUser, szUserName, sizeof(szUser));
		szUser[sizeof(szUser)-1] = '\0';
	}

	// send user name & password to server & get reply message
	if(!SendFtpCmd(CMD_USER, szUser))
		return false;

	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	if(!Handle_User(szAccount)){
		LocalClose();
		return false;
	}

	return true;
}

bool CFtpClient::DoPass(const char *szPassword, const char *szAccount)
{
	char szPwd[256] = "";

	//no password available
	if(NULL == szPassword || '\0' == *szPassword)
	{
		//ask user for password
		if(!GetPassword(szPwd, sizeof(szPwd)))
			return false;
	}
	else
	{
		strncpy(szPwd, szPassword, sizeof(szPwd));
		szPwd[sizeof(szPwd)-1] = '\0';
	}

	if(!SendFtpCmd(CMD_PASS, szPwd))
		return false;
	
	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	if(!Handle_Pass(szAccount)){
		LocalClose();
		return false;
	}

	m_bLogged = true;
	return true;
}

bool CFtpClient::DoAccount(const char *szAccount)
{
	char szAcc[256] = "";

	if(NULL == szAccount || '\0' == *szAccount)
	{
/*
		//TOFIX use callback function
		CNameInputDlg dlg;
		//TOFIX dlg.SetTitleString("Account name");
		//TOFIX dlg.SetDescString("Please enter account name for this connection");
		if(wxOK == dlg.ShowModal())
		{
			strncpy(szAcc, dlg.m_strValue, sizeof(szAcc));//TOFIX provjera da li je dovoljno dug unos
			szAcc[sizeof(szAcc)-1] = '\0';
		}
		else
			return false;	//TOFIX close connection here
*/
	}
	else{
		strncpy(szAcc, szAccount, sizeof(szAcc));
		szAcc[sizeof(szAcc)-1] = '\0';
	}

	if(!SendFtpCmd(CMD_ACCT, szAcc))
		return false;
	
	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	if(!Handle_Acct()){
		DoClose();	//TOFIX Close
		return false;
	}

	return true;
}

//TOFIX needed ? See UploadFile()
bool CFtpClient::FtpStore(const char * szDestFile)
{
	// send command to server & read reply
	if(!SendFtpCmd(CMD_STOR, szDestFile))
	{
		//TOFIX m_bConnected = false ??
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	ReadReply(m_ReplyList, *m_pSockCtrl);
	if(!Handle_Stor())
	{
		//TOFIX LocalClose(); / Doclose?
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	//TOFIX da li ovo ide ovdje	
	if( !StartDataConn() )
	{
		//TOFIX LocalClose(); / Doclose?

		//on error, switch to passive mode and repeat operation
		//(in case error due to wrong mode settings - firewall present, ...)
		if(!m_bPassiveMode){
			m_bPassiveMode = true;
			DoTrace(TT_INFO, "Switching to passive mode of work.\r\n");
			return FtpStore(szDestFile);  //repeat operation
		}

		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	return true;
}

//TOFIX needed ? See DownloadFile()
bool CFtpClient::FtpRetr(const char * szSrcFile)
{
	// send command to server and read response
	if(!SendFtpCmd(CMD_RETR, szSrcFile)) 
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	if(!Handle_Retr())
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	if( !StartDataConn() )
	{
		//TOFIX LocalClose(); / Doclose?

		//on error, switch to passive mode and repeat operation
		//(in case error due to wrong mode settings - firewall present, ...)
		if(!m_bPassiveMode){
			m_bPassiveMode = true;
			DoTrace(TT_INFO, "Switching to passive mode of work.\r\n");
			return FtpRetr(szSrcFile);  //repeat operation
		}

		//TOFIX m_EventFree.SetEvent();
		return false;
	}

	return true;
}

//send "restart transfer" command
bool CFtpClient::FtpRest(int nStartPos)
{
	if(!SendFtpCmd(CMD_REST, nStartPos))
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}
	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	Handle_Rest();	//TOFIX if(!)
	return true;
}

int CFtpClient::FtpReadData(char *szBuf, int nSize)
{
	m_pSockData->WaitUntilReadable(SOCKET_READ_TOUT);	//max 6 sec
	return ReadDataMsg(szBuf, nSize); 
}

bool CFtpClient::FtpSendData(const char *szBuf, int nSize)
{
	return (nSize == SendDataMsg(szBuf, nSize));
}

bool CFtpClient::FtpNoop()
{
	if(!SendFtpCmd(CMD_NOOP))
	{
		//TOFIX m_EventFree.SetEvent();
		return false;
	}
	ReadReply(m_ReplyList, *m_pSockCtrl);	//read command reply
	Handle_Rest();	//TOFIX if(!)
	return true;
}

void CFtpClient::LocalClose()
{
	DestroySockets(); // Cleanup, so it can be reused for reconnect
	m_bConnected	= false;
	m_bSystQueried	= false;
	m_bLogged		= false;
}

bool CFtpClient::FxpTransfer(const char * szSrc, const char * szDest, CFtpClient *pDest)
{
    //set pasive mode for destination
    if(pDest->DoPasv())
    {
		//tell destination to connect to source ftp server
        if(DoPort(pDest->m_strPassiveIP.c_str(), pDest->m_nPassivePort))
        {
			if(NULL != m_pProgressInfo){
				int fsize = GetRemoteFileSize (szSrc);
				//ASSERT(fsize > 0);

				int nStartPos = 0; //TOFIX resume FXP option
				
				//show full ftp path for FXP progress
				std::string strSrc("ftp://");
				strSrc += m_strHost.c_str();
				strSrc += "/";
				strSrc += m_strCwd.c_str();
				strSrc += "/";
				strSrc += szSrc;
				
				std::string strDest("ftp://");
				strDest += pDest->m_strHost.c_str();
				strDest += "/";
				strDest += pDest->m_strCwd.c_str();
				strDest += "/";
				strDest += szDest;

				//NOTE do this before initsingle
				m_pProgressInfo->InitCurrentFiles(strSrc.c_str(), strDest.c_str());
				m_pProgressInfo->InitCurrentProgress(nStartPos, fsize);
			}

			//transmit file data between servers directly
            if(!DoRetr(szSrc))
                return false;
            if(!pDest->DoStor(szDest))
                return false;
			
			//wait for operation end
			//(block until we get data on one of control sockets)
			fd_set fs;
			FD_ZERO(&fs);
			FD_SET(m_pSockCtrl->GetHandle(), &fs);
			FD_SET(pDest->m_pSockCtrl->GetHandle(), &fs);
			select(2, &fs, NULL, NULL, NULL);
			
            ReadReply(m_ReplyList, *m_pSockCtrl);
            if(!Handle_Retr())	//TOFIX Handle_RetrEnded()
            {
                pDest->ReadReply(pDest->m_ReplyList, *pDest->m_pSockCtrl);
                pDest->DoAbort();
                return false;
            }
			
            pDest->ReadReply(pDest->m_ReplyList, *pDest->m_pSockCtrl);
            if(!pDest->Handle_Stor())	//TOFIX Handle_StoreEnded()
            {
                return false;
            }
        }
        else
        {
            //TOFIX wxMessageBox("FXP transfer is probably not supported by one of the servers!");
            return false;
        }
    }

    return true;
}

void CFtpClient::SetTraceCallback(TRACE_FN pfTrace, unsigned long dwData)
{
	m_pfnTrace		= pfTrace;
	m_dwTraceData	= dwData;
}

void CFtpClient::GetTraceCallback(TRACE_FN &pfTrace, unsigned long &dwData)
{
	pfTrace	= m_pfnTrace;
	dwData	= m_dwTraceData;
}

void CFtpClient::DoTrace(int nType, const char *fmt, ...)
{
	if(NULL != m_pfnTrace)
	{
		//format message
		char szBuffer[512];

		va_list valist;
		va_start(valist, fmt);
		/*int nRes =*/ vsprintf(szBuffer, fmt, valist);
		va_end(valist);

		//send formated message through registered callback function
		m_pfnTrace(szBuffer, nType, m_dwTraceData);
	}
}

//read single reply line terminated by "\r\n"
int CFtpClient::ReadReplySingle(xSocket &sock)
{
	m_ReplyList.clear();	//make sure result list is initially empty

	int	nStart	= 0;		//points to free part of buffer
	//int	nTotalRead = 0;

	//some servers don't give result on some commands ?
	if(!sock.WaitUntilReadable(SOCKET_READ_TOUT)) //wait max 20 seconds
		return 0; // 0 bytes read

	while(true)
	{
		//read from socket into free part of buffer (slow a little due to Windows NT servers)
		int nRes = sock.Read(m_szBuffer+nStart, 1);
		if(nRes < 0)
		{
			//TOFIX test socket on error u IsValid ugraditi provjeru 
			TRACE("Reply: Socket read error: [%s]\n", sock.GetErrorText(sock.GetError()).c_str());
			sock.Close();
			break; 
		}
		else if(nRes == 0)
		{
			TRACE("Reply: No more to read\n");
			break;
		}

		//TRACE("Read: %c\n", m_szBuffer[nStart]);

		//increment pointer to free buffer
		nStart += nRes;

		//terminate string so we can search inside it
		m_szBuffer[nStart] = '\0';	

		if('\n' == m_szBuffer[nStart-1])
		{
			//last line char read
			m_ReplyList.push_back(m_szBuffer);
			break;
		}

		//FIX listing of big directories sometimes finishes to early
		if(sock == *m_pSockData)
			sock.WaitUntilReadable(SOCKET_READ_TOUT);	//max 1 sec
	}

//TOFIX move to list.Dump?
#ifdef _DEBUG
	if(m_ReplyList.size() > 0)
	{
		TRACE("Start of line list\n");
		for(unsigned int i=0; i<m_ReplyList.size(); i++)
			TRACE("%d:\t%s", i, m_ReplyList[i].c_str());
		TRACE("End of list\n");
	}
	else
		TRACE("ReadReply(): WARNING Reply list is empty\n");
#endif

	if(sock == *m_pSockCtrl){
		for(unsigned int j=0; j<m_ReplyList.size(); j++)
			DoTrace(TT_REPLY, m_ReplyList[j].c_str());
	}

	return nStart; //return number of bytes read
}

bool CFtpClient::GetPassword(char *szBuff, int nSize)
{
	//TOFIX use callback function
	//get password
/*
	CNameInputDlg dlg;
	//dlg.SetPasswordMode();
	//dlg.SetTitleString("Password");
	//dlg.SetDescString("Please enter password for this connection");
	if(wxOK == dlg.ShowModal()){
		strncpy(szBuff, dlg.m_strValue, nSize);
		szBuff[nSize-1] = '\0';
	}
*/
	return (strlen(szBuff) > 0);
	//return false;	//TOFIX close connection here
}

