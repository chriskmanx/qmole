////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsListing implementation
////////////////////////////////////////////////////////////////////////////

#include "VfsListing.h"
#include "util.h"
#include <algorithm>    //sort

//TOFIX move to VfsItem with enum
//used to sort VfsListing class
class VfsComparator{
public:
	VfsListing::_SORT_COL m_nColumn;
	bool m_bAscending;

	bool operator()(const VfsItem &a, const VfsItem &b);
};

//TOFIX add option (as in WC) that dirs are sorted by name/asc no matter what!
bool VfsComparator::operator()(const VfsItem &a, const VfsItem &b)
{
	//directory is ALWAYS placed before file item (no matter which column is being sorted)
	if(a.IsDir() && !b.IsDir())
		return true;
	if(!a.IsDir() && b.IsDir())
		return false;

	// item ".." is ALWAYS on top of the list (no matter which column is being sorted)
	static const std::string strUp("..");
	if(a.GetName() == strUp)
		return true;
	if(b.GetName() == strUp)
		return false;

	int nCmpRes = 0;    //three-state comparison result

	switch(m_nColumn){
		case VfsListing::ST_NAME:    //sort by name
			nCmpRes = a.GetName().CmpNoCase(b.GetName());
			if(0 == nCmpRes)
				nCmpRes = a.GetName().Cmp(b.GetName());    //Unix rare cases
			break;
		case VfsListing::ST_EXT:     //sort by extension
			nCmpRes = a.GetExtTitle().CmpNoCase(b.GetExtTitle());
			break;
		case VfsListing::ST_PATH:     //sort by item path
			nCmpRes = a.GetPath().CmpNoCase(b.GetPath());
			break;
		case VfsListing::ST_SIZE:     //sort by item size
			if(a.m_nSize < b.m_nSize)
				nCmpRes = -1;
			else if(a.m_nSize > b.m_nSize)
				nCmpRes = 1;
			break;
		case VfsListing::ST_DATE:
			if(a.m_nLastModDate < b.m_nLastModDate)
				nCmpRes = -1;
			else if(a.m_nLastModDate > b.m_nLastModDate)
				nCmpRes = 1;
			break;
		case VfsListing::ST_ATTR:    //TOFIX no sense to support?
			nCmpRes = a.GetAttr().CmpNoCase(b.GetAttr());
			break;
		case VfsListing::ST_NONE:
		default:
			ASSERT(false);
			nCmpRes = 0;
	};

	//we must garantee strict file ordering
	//- if two files have same attributes (size, ...) we must compare by name
	//  (name is always unique within single directory)
	if(0 == nCmpRes && m_nColumn != VfsListing::ST_NAME)
	{
		nCmpRes = a.GetName().CmpNoCase(b.GetName());
		if(0 == nCmpRes)
			nCmpRes = a.GetName().Cmp(b.GetName());    //Unix rare cases
	}

	//revert result if the sort was not ascending
	if(!m_bAscending)
		nCmpRes = -nCmpRes;

	return (nCmpRes < 0);    //operator < (is a < b)
}

VfsListing::VfsListing()
{
	m_nCountRaw = 0;
	m_nSortCol  = ST_NONE;
	m_bAscending= true;
}

VfsListing::~VfsListing()
{
	Clear();
}

void VfsListing::operator =(const VfsListing &that)
{
	//ASSERT(this != &that);
	m_nCountRaw    = that.m_nCountRaw;
	m_nSortCol     = that.m_nSortCol;
	m_bAscending   = that.m_bAscending;
	
	VfsListingBase::operator =(that);    //copy item list
}

void VfsListing::Sort()
{
	SetSort(m_nSortCol, m_bAscending);
}

void VfsListing::SetSort(enum _SORT_COL nColumn, bool bAscending)
{
    m_nSortCol   = nColumn;
    m_bAscending = bAscending;    

    //using functor object instead of function to store sort preferences
    VfsComparator cmp;
    cmp.m_nColumn     = (ST_NONE == m_nSortCol) ? ST_NAME : m_nSortCol;
    cmp.m_bAscending = m_bAscending;
    std::sort(begin(), end(), cmp);

    //rebuild filter list
    FilterList();
}

int VfsListing::Insert(const VfsItem &item)
{
    //insert item in the list - no sorting
    push_back(item);
    m_nCountRaw ++;

    return 0; //TOFIX
}

int VfsListing::FindItem(const char *szName, int nStart, bool bCaseSensitive)
{
	if(nStart < 0)
		nStart = 0;

	int nCount = GetCount();
	//printf("Find %s from %d to %d\n", szName, nStart, nCount);
	for(int i=nStart; i<nCount; i++)
	{
		//printf("Testing %s\n", operator [](i).GetName().c_str());
		if( (bCaseSensitive && GetAt(i).GetName() == szName) ||
			(!bCaseSensitive && 0 == GetAt(i).GetName().CmpNoCase(szName)))
		{
			//printf("Result = %d\n", i);
			return i;   //return list index
		}
	}

	return -1;
}

//case insensitive, partial string search (forward/backward search)
int VfsListing::FindPartial(const char *szPartial, int nStart, bool bForward)
{
	int nCount = GetCount();
	int nLen   = strlen(szPartial);

	if(bForward)
	{
		if(nStart < 0)
			nStart = 0;
    
		for(int i=nStart; i<nCount; i++)
		{
			if(0 == GetAt(i).GetName().Left(nLen).CmpNoCase(szPartial))
				return i;   //return index (filtered list)
		}
	}
	else
	{
		if(nStart > 0)
		{
			for(int i=nStart; i>=0; i--)
			{
				if(0 == GetAt(i).GetName().Left(nLen).CmpNoCase(szPartial))
					return i;   //return index (filtered list)
			}
		}
	}

	return -1;
}

//case insensitive, wildcard search (forward/backward search)
int VfsListing::FindPattern(const char *szPattern, int nStart, bool bForward)
{
	int nCount = GetCount();

	if(bForward)
	{
		if(nStart < 0)
			nStart = 0;
    
		for(int i=nStart; i<nCount; i++)
		{
			if(fnmatch(szPattern, GetAt(i).GetName(), false, false))
				return i;   //return index (filtered list)
		}
	}
	else
	{
		if(nStart > 0)
		{
			for(int i=nStart; i>=0; i--)
			{
				if(fnmatch(szPattern, GetAt(i).GetName(), false, false))
					return i;   //return index (filtered list)
			}
		}
	}

	return -1;
}
 
void VfsListing::Clear()
{
	clear();
	m_nCountRaw = 0;
}

//TOFIX move to VfsItem
String VfsListing::GetColumnText(int nIdx, enum _SORT_COL nColumn)
{
	///TOFIX ASSERT(nIdx<GetCount)

	switch (nColumn){
		case ST_NAME:
			return GetAt(nIdx).GetTitle();
		case ST_EXT:
			return GetAt(nIdx).GetExtTitle();
		case ST_SIZE:
			return GetAt(nIdx).GetSize();
		case ST_DATE:
			return GetAt(nIdx).GetDate();
		case ST_ATTR:
			return GetAt(nIdx).GetAttr();
		case ST_PATH:
			return GetAt(nIdx).GetPath();
		case ST_NONE:
			;
	};

	return String();
}

