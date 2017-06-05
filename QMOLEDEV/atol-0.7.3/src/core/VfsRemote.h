////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This is a common parent class for all remote Vfs objects (Vfs_Ftp and Vfs_Sftp)
////////////////////////////////////////////////////////////////////////////

#ifndef VFSREMOTE_H
#define VFSREMOTE_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Vfs.h"
#include <map>

class CCacheItem {
	String m_strDir;
	VfsListing m_list;
	
public:
	CCacheItem(String &dir,VfsListing &list) {
		m_strDir = dir;
		m_list = list;
	}
	String dir() {
		return m_strDir;
	}
	VfsListing list() {
		return m_list;
	}
};

typedef std::map<String, unsigned int> VfsCacheIndex;
typedef std::map<unsigned int, CCacheItem> VfsListingCache;

class Vfs_Remote : public Vfs 
{
	VfsListingCache m_cache;
	VfsCacheIndex m_cacheIndex;
	unsigned int m_nCacheCounter;

public:
	Vfs_Remote(unsigned int cachesize = 50);
	virtual ~Vfs_Remote();

	virtual bool Open() = 0;
	virtual bool Close() = 0;
	virtual bool IsConnected() = 0;
	virtual bool Lock();

	virtual bool CachedListDir(VfsListing &list, bool &bAbort, bool bForceRefresh);

	void *m_pWndTrace;
	unsigned int m_nMaxCacheSize; // 0 - means no cache
};

#endif // VFSREMOTE_H
