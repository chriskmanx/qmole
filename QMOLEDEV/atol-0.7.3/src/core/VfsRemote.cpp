////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This is a common parent class for all remote Vfs objects (Vfs_Ftp and Vfs_Sftp)
////////////////////////////////////////////////////////////////////////////

#include "VfsRemote.h"

Vfs_Remote::Vfs_Remote(unsigned int cachesize)
{
	m_pWndTrace = NULL;
	m_nMaxCacheSize = cachesize;
	m_nCacheCounter = 0;
}

Vfs_Remote::~Vfs_Remote()
{
}

bool Vfs_Remote::CachedListDir(VfsListing &list, bool &bAbort, bool bForceRefresh)
{
	ASSERT(mutex.locked);
	if(m_nMaxCacheSize == 0) {
		return ListDir(list, bAbort);
	} else {
		VfsCacheIndex::iterator idxitr;
		VfsListingCache::iterator cacheitr;
		bool r = false;
		idxitr = m_cacheIndex.find(m_strCurDir);
		bool cached = idxitr != m_cacheIndex.end();
		if(cached) {
			cacheitr = m_cache.find(idxitr->second);
			if(!bForceRefresh) {
				list = cacheitr->second.list();
				r = true;
			}
			m_cacheIndex.erase(idxitr);
			m_cache.erase(cacheitr);
		}
		if(bForceRefresh || !cached) {
			r = ListDir(list, bAbort);
		}
		m_cache.insert(m_cache.end(),VfsListingCache::value_type(m_nCacheCounter,CCacheItem(m_strCurDir, list)));
		m_cacheIndex.insert(m_cacheIndex.end(),VfsCacheIndex::value_type(m_strCurDir,m_nCacheCounter++));
		while(m_cache.size() > m_nMaxCacheSize) {
			cacheitr = m_cache.begin();
			m_cacheIndex.erase(m_cacheIndex.find(cacheitr->second.dir()));
			m_cache.erase(cacheitr);
		}
		return r;
	}
}

bool Vfs_Remote::Lock() 
{
	if(mutex.Lock()) 
	{
		if(!IsConnected()) 
		{
			Close();
			Open();
			/// Be aware that current path can change
			/// but it is set explicitly before every operation i think
			if(!IsConnected())
				return false;
		}
		return true;
	}
	return false;
}
