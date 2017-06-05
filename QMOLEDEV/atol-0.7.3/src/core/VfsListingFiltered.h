////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements filtered item list (filtered by name pattern, size, data, ...)
//////////////////////////////////////////////////////////////////////////// 

#ifndef VFSLISTINGFILTERED_H__
#define VFSLISTINGFILTERED_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "VfsListing.h"
#include "FilterDesc.h"

class VfsListingFiltered : public VfsListing  
{
public:
	VfsListingFiltered();
	virtual ~VfsListingFiltered();

	void operator = (const VfsListingFiltered &that)
	{
		VfsListing::operator = (that);
		SetFilter(that.m_objFilter);
		m_nCount        = that.m_nCount;
		m_lstFiltered   = that.m_lstFiltered;    //copy filter list
	}

	void operator = (const VfsListing &that)
	{
		VfsListing::operator = (that);
		FilterList();
	}

	//
	// standard accessors
	//
	inline VfsItem &GetAt (int nIdx){
		ASSERTMSG((unsigned int)nIdx < m_lstFiltered.size(), "VfsListingFiltered:GetAt error: access after the end of list (%d at %d)\n", nIdx, m_lstFiltered.size());
		int nRawIdx = m_lstFiltered[nIdx];
		return GetAtRaw(nRawIdx);
	}

	virtual void Clear();
	virtual int  Insert(const VfsItem &item);
	virtual void FilterList();
	int  GetCount()        const { return m_nCount; };

	FilterDesc &GetFilter(){ return m_objFilter; }
	void SetFilter(const FilterDesc &filter){ m_objFilter = filter; }

	int  FindItemRaw(const char *szName, int nStart = 0);

protected:
	FilterDesc    m_objFilter;                    //filter applied to this file list
	std::vector<unsigned int>  m_lstFiltered;    //item indexes of  that are displayed on the screen
	int m_nCount;
};

#endif // VFSLISTINGFILTERED_H__
