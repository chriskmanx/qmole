////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsListing is an collection of virtual file system items (VfsItem)
////////////////////////////////////////////////////////////////////////////

#ifndef VFSLISTING_H
#define VFSLISTING_H

#if _MSC_VER > 1000
  #pragma warning (disable : 4786)
#endif

#include "VfsItem.h"
#include "debug.h"
#include <vector>

typedef std::vector<VfsItem> VfsListingBase;

class VfsListing : protected VfsListingBase
{
public:
	VfsListing();
	virtual ~VfsListing();

	enum _SORT_COL{
		ST_NONE = 0,
		ST_NAME = 1,
		ST_EXT  = 2,
		ST_SIZE = 3,
		ST_DATE = 4,
		ST_ATTR = 5,
		ST_PATH = 6,
	};

	void operator = (const VfsListing &that);

	virtual int  GetCount()  const { return m_nCountRaw; };
	int  GetCountRaw() const { return m_nCountRaw; };
	
	// standard accessors
	virtual inline VfsItem &GetAt(int nIdx){ return GetAtRaw(nIdx); }
	inline VfsItem &GetAtRaw (int nIdx){ ASSERT((unsigned int)nIdx < size()); return operator [](nIdx); }

	// const (read only) accessors
	virtual inline const VfsItem &GetAt (int nIdx) const { return GetAtRaw(nIdx); }
	inline const VfsItem &GetAtRaw (int nIdx) const{ ASSERT((unsigned int)nIdx < size()); return operator [](nIdx); }

	void Sort();
	void SetSort(enum _SORT_COL nColumn, bool bAscending = true);
	int  GetSortCol(){ return m_nSortCol; };
	bool GetSortAsc(){ return m_bAscending; };

	String GetColumnText(int nIdx, enum _SORT_COL nColumn);

	virtual void Clear();
	virtual int  Insert(const VfsItem &item);
	int FindItem(const char *szName, int nStart = 0, bool bCaseSensitive = true);
	int FindPartial(const char *szPartial, int nStart = 0, bool bForward = true);
	int FindPattern(const char *szPattern, int nStart = 0, bool bForward = true);

	virtual void FilterList(){};

protected:
	//sort info
	enum _SORT_COL  m_nSortCol;
	bool m_bAscending;

	//filtered list info
	int m_nCountRaw;
};

#endif //VFSLISTING_H
