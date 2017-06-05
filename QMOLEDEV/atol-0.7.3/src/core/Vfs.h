////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This is an abstract class defining interface for Virtual File System (VFS)
//         VFS is an abstraction over file system allowing to browse/manipulate different
//         systems as if they are file systems 
//         (example: compressed archives, FTP/SFTP connection, CDROM catalogs, database access, ...)
////////////////////////////////////////////////////////////////////////////

#ifndef VFS_H__
#define VFS_H__

#include <limits.h>
#include "types.h"
#include "VfsListing.h"
#include "VfsSelection.h"
#include "OpState.h"
#include <gtk/gtk.h>
#include "pthread.h"

class Vfs
{
public:
	//define all (soon to be) supported VFS types
	enum {
		UNKNOWN,
		LOCAL,
		NET,    //Win32 net / SMB
		ARCHIVE,
		FTP,
		SFTP,
		SITEMAN
	};


	Vfs();
	virtual ~Vfs();

	// info
	int  GetType(){ return m_nType; }
	bool IsEqualLocation(Vfs *pOther);
    
	//convert path separators etc to match current Vfs
	virtual void FixPath(String &strPath) = 0;

	// mount / unmount new virtual file system
	virtual bool Open() = 0; 
	virtual bool Close() = 0;
	
	virtual Vfs *Clone(){ return NULL; };

	// directory change
	String GetDir(){ return m_strCurDir; };
	virtual String GetPathTitle(){ return GetDir(); };    //for display purposes
	virtual bool IsRootDir() = 0;
	virtual bool SetRootDir() = 0;
	virtual bool SetDir(const char *szPath) = 0;
	virtual bool UpDir() = 0;

	// operations
	virtual bool ListDir(VfsListing &list, bool &bAbort) = 0;
	virtual bool CachedListDir(VfsListing &list, bool &bAbort, bool bForceRefresh = false);
	virtual bool MkDir(const char *szName) = 0;
	virtual bool Delete(VfsItem &item, int &nOpSettings) = 0;
	virtual bool Rename(const char *szItem, const char *szNewItem) = 0;
	virtual bool Copy(VfsItem &item, const char *szNewName, Vfs *pVfsDest, INT64 nOffset) = 0;
	
	// Executes (opens) item.
	// szItem: Path to item.
	virtual bool Execute(const char *szItem, bool bLocalDir = true){ return false; }

	// Returns item that represents link target if item is a link or item itself otherwise.
	// Target item will contain full path, not only name.
	virtual VfsItem GetLinkTarget(const VfsItem& item){ return item; }

	virtual void ShowCtxMenu(VfsSelection &items, int x, int y){};
	virtual void ShowProperties(VfsSelection &items){}
	virtual void ClipboardCut(VfsSelection &items) {}
	virtual void ClipboardCopy(VfsSelection &items) {}
	virtual void ClipboardPaste() {} 

	virtual INT64 GetDriveFreeSpace(){ return INT_MAX; }; //TOFIX define INT64_MAX
	virtual INT64 GetDriveSize(){ return -1; };

	// utils
	void ExpandSelection(VfsSelection &sel, bool &bAbort);
	void ExpandTree(std::vector<VfsSelectionItem> &list, bool &bAbort);

	virtual bool CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset = 0) = 0;
	virtual bool CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset = 0) = 0;

	OpState *m_pProgress;

	virtual bool Lock();
	virtual bool Unlock();

protected:
	int    m_nType;        //VFS type indicator
	String m_strCurDir;    //current directory in UTF-8
	Mutex mutex;
};

#endif // VFS_H__
