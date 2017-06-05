////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Vfs_Local implements VFS for accessing local file system (HDD,FDD,...) 
////////////////////////////////////////////////////////////////////////////

#ifndef VFSLOCAL_H_
#define VFSLOCAL_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Vfs.h"
#ifdef _WIN32
 #include "_win\DirWatcher.h"
#endif

class Vfs_Local : public Vfs  
{
public:
	Vfs_Local();
	virtual ~Vfs_Local();
    
	virtual void FixPath(String &strPath);

	// mount / unmount new virtual file system
	virtual bool Open(); 
	virtual bool Close();

	virtual Vfs *Clone();

	// directory change
	virtual bool IsRootDir();
	virtual bool SetRootDir();
	virtual bool SetDir(const char *szPath);
	virtual bool UpDir();
    
	// operations
	virtual bool ListDir(VfsListing &list, bool &bAbort);  //TOFIX start using abort
	virtual bool MkDir(const char *szName);
	virtual bool Delete(VfsItem &item, int &nOpSettings);
	virtual bool Rename(const char *szItem, const char *szNewItem);
	virtual bool Copy(VfsItem &item, const char *szNewName, Vfs *pVfsDest, INT64 nOffset = 0);
	virtual bool Execute(const char *szItem, bool bLocalDir = true);
	virtual VfsItem GetLinkTarget(const VfsItem& item);

	virtual void ShowCtxMenu(VfsSelection &items, int x, int y);
	virtual void ShowProperties(VfsSelection &items);
	virtual void ClipboardCut(VfsSelection &items);
	virtual void ClipboardCopy(VfsSelection &items);
	virtual void ClipboardPaste(); 

	virtual bool CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset = 0);
	virtual bool CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset = 0);

	virtual INT64 GetDriveFreeSpace();
	virtual INT64 GetDriveSize();

	void RegisterChangeNotify(void *list);
	void StopNotifications();
	void StartNotifications();
	static void MakeItem(const std::string& strNativePath, VfsItem& item);

public:
	bool m_bRecycleBin;

#ifdef _WIN32
	CDirWatcher m_objWatcher;	//file change notification object
#endif
};

#endif // VFSLOCAL_H_

