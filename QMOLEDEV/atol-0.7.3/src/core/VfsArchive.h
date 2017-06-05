////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Vfs_Local implements VFS for accessing archive files
//       Specific archivers are implemented inside plugin libraries. 
////////////////////////////////////////////////////////////////////////////

#ifndef VFSARCHIVE_H_
#define VFSARCHIVE_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Vfs.h"
#include "ArchiverPlugin.h"

class Vfs_Archive  : public Vfs   
{
	friend class VfsManager;

public:
	Vfs_Archive();
	virtual ~Vfs_Archive();

	virtual void FixPath(String &strPath);

	// mount / unmount new virtual file system
	virtual bool Open(); 
	virtual bool Close();

	// directory change
	virtual String GetPathTitle();    //for display purposes
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

	virtual bool CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset = 0);
	virtual bool CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset = 0);

	virtual INT64 GetDriveFreeSpace();

	bool InitArchiver(const char *szFileName, bool canReadFile = false);
	String  GetArchivePath(){ return m_strArchiveFile; }

protected:
	bool CheckArchiverCaps(int dwCapsRequested);
	void BuildTree();

protected:
	ArchiverPlugin *m_pPlugin;
	String        m_strArchiveFile;
	String        m_strTitle;

	int     m_hArchive;
	bool    m_bReadOnly;
	bool    m_bReadFile;
	bool    m_bDeleteFile;    //after archive closes
	VfsSelection m_lstTree;
};

#endif // VFSARCHIVE_H_


