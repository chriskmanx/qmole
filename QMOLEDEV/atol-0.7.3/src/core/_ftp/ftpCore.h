////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef FTPCORE_H__
#define FTPCORE_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../String.h"
#include "xSocket.h"
#include "xSSLSocket.h"
#include "FtpListParser.h"
#include "../VfsItem.h"
#include "../OpState.h"
#include "../ProxyLayer.h"
#include "../VfsListing.h"
#include "../FtpInfo.h"

#include <string>

#define FTP_CONNECT_TOUT	30000	// mseconds -> 30 sec
#define FTP_REPLY_TOUT		20000	// mseconds -> 20 sec

//TOFIX fix functions Do... not to parse input but ti have their own params?

class CFtpClient
{
public:
	CFtpClient();
	virtual ~CFtpClient();

	//TOFIX transfer_ASCII, ... 
	typedef enum {  ftpTtype_ASCII,
	                ftpTtype_BINARY,
	                ftpTtype_EBCDIC	} FtpTransferType;
	bool m_bAbort;

public:
	void SetProxy(CProxyLayer *pProxy){ m_pProxy = pProxy; }

	bool SetTransferType(FtpTransferType eTransferType);
	void LocalClose();

	void SetTraceCallback(TRACE_FN pfTrace, unsigned long dwData);
	void GetTraceCallback(TRACE_FN &pfTrace, unsigned long &dwData);
	void DoTrace(int nType, const char *fmt, ...);
	
	int GetReplyCode(CStrList &list);
	void ReportError(const char * szDesc, bool bFormat = true);
	String FormatCmdError(const char *szDesc);
	
	int ReadDataMsg(char *szBuffer, int len);
	int SendDataMsg(const char *szBuffer, int len);
	int GetRemoteFileSize (const char * szFile);

	bool SendFtpCmd(const char *szFormat, ... );
	bool UploadFile(const char *szSrcFile, const char *szDestFile, int nFromOffset);
	bool DownloadFile(const char *szSrcFile, const char *szDestFile, int nFromOffset);

	bool DeleteFile(const char * szFile);
	bool MakeDirectory(const char * szDir);
	bool RemoveDirectory(const char * szDir);

	int  DoRename(const char * szOldName, const char * szNewName);

	void DoOpen(CFtpInfo &ftpInfo);
	void Connect();
	bool DoLogin(CFtpInfo &ftpInfo);
	void DoClose();
	bool DoList();
	void DoCD(const char * szDir);
	void DoRhelp(const char *szCmd);
	void DoPWD();
	void DoSyst();
	bool DoAbort();
	bool DoType(int chType);
	bool DoPasv();
	bool DoPort(const char *szIP, unsigned short nPort);
	bool DoRetr(const char *szFile);
	bool DoStor(const char *szFile);
	bool DoUser(const char *szUserName, const char *szAccount);
	bool DoPass(const char *szPassword, const char *szAccount);
	bool DoAccount(const char *szAccount);

	void SetProgress(OpState *pInfo);

	const char *GetSystemType();

	inline bool		IsConnected(){ return m_bConnected; }
	inline const char *	GetDirectory(){ return m_strCwd.c_str(); }

	VfsListing m_Listing;

	inline void SetPassiveMode(bool bPassive){ m_bPassiveMode = bPassive; }

	String	m_strPassiveIP;
	unsigned short	m_nPassivePort;

	xSocket *m_pSockCtrl;
	xSocket *m_pSockData;
	xSocket *m_pSockListen;
	xSSLContext *m_pContext;

	int ReadReply(CStrList &list, xSocket &sock, int nMsTimeout = FTP_REPLY_TOUT);
	int ReadReplySingle(xSocket &sock);

	CStrList m_ReplyList;

	bool Handle_Retr();
	bool Handle_Stor();

	FtpTransferType m_eTransferType;

	bool InitDataConn();
	bool StartDataConn();

	CStrList m_lstDirRaw;

	//listing options
	CFtpInfo::FtpEncryptionType m_nEncryptType;
	bool	m_nEncryptData:1;
	bool	m_nListLongFormat:1;
	bool	m_nListShowHidden:1;
	bool	m_nListResolveLinks:1;
	bool	m_nListCompleteTime:1;

protected:
	//CEvent			m_EventFree;  //TOFIX
	CFtpListParser	m_ListParser;

	OpState		  *m_pProgressInfo;
	CProxyLayer	  *m_pProxy;			//one or more layered proxies	

	//trace callback
	TRACE_FN	m_pfnTrace;
	unsigned long	m_dwTraceData;

	bool	m_bSystQueried;
	bool	m_bConnected;	//TOFIX varijabla stanje	
	bool	m_bLogged;
	bool	m_bPassiveMode;
	bool    m_bBusy;		//TOFIX

	//TOFIX make sure buffer is not used from more than one thread at a time !!!
	char m_szBuffer[2048];		/* buffer used to read/write */

	std::string m_strUser;       // stores username
	std::string m_strPass;       // stores user password
	std::string m_strAccount;    // stores user password
	bool m_bAnonymous:1;         // anonymous access
	std::string m_strCwd;		// 
	std::string m_strHost;       // stores hostname
	unsigned short m_uPort;    // port

	bool m_bDownload;	//operation info (download / upload)

protected:
	bool GetPassword(char *szBuff, int nSize);
	bool ParseBuffer(CStrList &list, bool &bMultiline, int &nStart, xSocket &sock);

	int  GetListenSocket();
	int  AcceptConnection();
	bool ExtractPortInfo(char *szIP, unsigned short &nPort);

	bool HandleReply(const char * szCmd, const char * szValidRes);

	bool HandleConnect();
	bool Handle_Abor(CStrList &list);
	bool Handle_Size(int &size);

	//handle server reply for specific command
	bool Handle_Port();
	bool Handle_User(const char * szAccount);
	bool Handle_Pass(const char * szAccount);
	bool Handle_Acct();
	bool Handle_Pwd();
	bool Handle_Cwd();
	bool Handle_Quit();
	bool Handle_List();
	bool Handle_Type();
	bool Handle_Help();
	bool Handle_Syst();
	bool Handle_Dele();
	bool Handle_Mkd();
	bool Handle_Rmd();
	bool Handle_Rnfr();
	bool Handle_Rnto();
	bool Handle_Rest();
	bool Handle_Pasv();

	void DestroySockets();
	
public:
	//TOFIX this is start of better public interface ?
	bool FtpStore(const char * szDestFile);
	bool FtpRetr(const char * szSrcFile);
	bool FtpRest(int nStartPos);
	bool FtpNoop();
	bool FxpTransfer(const char * szSrc, const char * szDest, CFtpClient *pDest);

	int FtpReadData(char *szBuf, int nSize);
	bool FtpSendData(const char *szBuf, int nSize);
};

#endif // FTPCORE_H__
