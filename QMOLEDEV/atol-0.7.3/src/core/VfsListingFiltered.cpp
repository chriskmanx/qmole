////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements filtered item list (filtered by name pattern, size, data, ...)
//////////////////////////////////////////////////////////////////////////// 

#include "VfsListingFiltered.h"

VfsListingFiltered::VfsListingFiltered()
{
	m_nCount = 0;
}

VfsListingFiltered::~VfsListingFiltered()
{
}

//rebuild filter list
void VfsListingFiltered::FilterList()
{
    m_nCount = 0;
    m_lstFiltered.clear();
    
    for(int i=0; i<m_nCountRaw; i++){
        if(m_objFilter.Match(operator[](i))){
            m_nCount ++;
            m_lstFiltered.push_back(i);
        }
    }
}

void VfsListingFiltered::Clear()
{
	VfsListing::Clear();

    m_lstFiltered.clear();
    m_nCount    = 0;
}

int VfsListingFiltered::Insert(const VfsItem &item)
{
	VfsListing::Insert(item);

    //if item matches filter add its index into the index list
//    if(m_objFilter.Match(item)){
        m_lstFiltered.push_back(m_nCountRaw-1);
        m_nCount ++;
//    }

    return 0; //TOFIX
}

int VfsListingFiltered::FindItemRaw(const char *szName, int nStart)
{
	if(nStart < 0)
		nStart = 0;

	int nCount = GetCountRaw();
	for(int i=nStart; i<nCount; i++)
	{
		//printf("Testing %s\n", operator [](i).GetName().c_str());
		if(GetAtRaw(i).GetName() == szName)
			return i;   //return list index
	}

	return -1;
}
