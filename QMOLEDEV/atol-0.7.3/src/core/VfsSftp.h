////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef VFS_SFTP_H__
#define VFS_SFTP_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "VfsRemote.h"
#include "ProxyLayer.h"
#include "_sftp/sshsession.h"

//parameters: trace text, trace type, user data
typedef void (* FTP_TRACE)(const char *, int, unsigned long);

class Vfs_Sftp : public Vfs_Remote
{
public:
	Vfs_Sftp();
	virtual ~Vfs_Sftp();

	bool Open();
	bool Close();
	bool IsConnected();

	bool ExecuteCmd(const char *szCmdLine);
	bool ExecuteItem(const char *szFileName);

	bool IsRootDir();
	void FixPath(String &strPath);

	//browsing functions
	bool SetRootDir();
	bool UpDir();
	bool SetDir(const char *szPath);
	bool ListDir(VfsListing &list, bool &bAbort);

	bool Copy(VfsItem &item, const char *szNewName, Vfs *pVfsDest, INT64 nOffset);
	bool CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset = 0);
	bool CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset = 0);

	//action functions
	bool Rename(const char *szItem, const char *szNewItem);
	bool Delete(VfsItem &item, int &nOpSettings);
	bool MkDir(const char *szName);

	virtual void ShowCtxMenu(VfsSelection &items, int x, int y);
	void SetTraceCallback(FTP_TRACE pfTrace, unsigned long dwData);

	String GetPathTitle();

public:
#ifdef _WIN32
	CSshSession		m_objSession;
#endif	

	//TOFIX store as CFtpInfo class
	String	m_szHost;		// Hostname (keep last connection data)
	unsigned short	m_nPort;	// Port
	String	m_szUser;
	String	m_szPwd;
	String	m_strRemoteDir;

	//trace callback
	FTP_TRACE	m_pfnTrace;
	unsigned long	m_dwTraceData;
};

#endif // VFS_SFTP_H__
