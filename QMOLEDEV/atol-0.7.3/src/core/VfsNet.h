////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Vfs_Net is a VFS implementation for LAN shares browsing
////////////////////////////////////////////////////////////////////////////

#ifndef VFS_NET_H__
#define VFS_NET_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "VfsLocal.h"
#ifdef _WIN32
 #include "_win/NetBrowser.h"
#endif

class Vfs_Net : public Vfs_Local
{
public:
	Vfs_Net();
	virtual ~Vfs_Net();

    // directory change
	virtual String GetPathTitle();    //for display purposes

    virtual bool IsRootDir();
    virtual bool SetRootDir();
    virtual bool SetDir(const char *szPath);
	bool UpDir();

	virtual bool ListDir(VfsListing &list, bool &bAbort);  //TOFIX start using abort
	virtual void ShowCtxMenu(VfsSelection &items, int x, int y);
	virtual bool Execute(const char *szItem, bool bLocalDir = true);

	virtual INT64 GetDriveFreeSpace();

protected:
#ifdef _WIN32
	NetBrowser   m_objBrowser;
#endif	
};

#endif // VFS_NET_H__
