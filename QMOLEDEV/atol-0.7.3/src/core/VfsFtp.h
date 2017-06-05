////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef VFSFTP_H__
#define VFSFTP_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "VfsRemote.h"
#include "_ftp/ftpCore.h"
#include "FtpInfo.h"

//parameters: trace text, trace type, user data
typedef void (* FTP_TRACE)(const char *, int, unsigned long);

//FTP transfer modes
#define FTP_AUTO_MODE	0
#define FTP_BIN_MODE	1
#define FTP_TEXT_MODE	2

class Vfs_Ftp : public Vfs_Remote
{
public:
	Vfs_Ftp();
	~Vfs_Ftp();

	//all but drag/drop supported
	//DWORD GetCaps(){ return VOP_COPY_SRC|VOP_COPY_DST|VOP_DELETE|VOP_RENAME|VOP_MKDIR; };

	bool Open();
	bool Close();
	bool IsConnected();

	void Abort();
	bool ExecuteCmd(const char *szCmdLine);
	bool ExecuteItem(const char *szFileName);

	void FixPath(String &strPath);
	bool IsRootDir();

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

	void FtpFileProperties();
	void ViewRawListing();

	String GetPathTitle();

//protected:
	CFtpClient  m_ftpCore;
	CFtpInfo    m_ftpInfo;
	int			m_nFtpMode;

	void RefreshTransferMode(const char *szItem);
	void SetTraceCallback(FTP_TRACE pfTrace, unsigned long dwData);

protected:
	bool Ftp2FtpCopy(const char *szName, const char *szNewName, Vfs *pDest);

	//trace callback
	FTP_TRACE		m_pfnTrace;
	unsigned long	m_dwTraceData;
};

#endif // VFSFTP_H__

