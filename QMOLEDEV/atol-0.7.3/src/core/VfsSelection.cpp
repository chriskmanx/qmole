////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsSelection class implementation
////////////////////////////////////////////////////////////////////////////

#include "VfsSelection.h"
#include <algorithm>
#include "debug.h"

VfsSelection::VfsSelection()
{
}

VfsSelection::~VfsSelection()
{
}

void VfsSelection::Clear()
{
    m_lstRootItems.clear();
}

int VfsSelection::GetTotalCount()
{
    int nSize  = m_lstRootItems.size();
    int uCount = nSize;

    for(int i=0; i<nSize; i++)
        uCount += GetTotalCountRecursive(&(m_lstRootItems[i]));

    return uCount;
}

int VfsSelection::GetTotalCountRecursive(VfsSelectionItem *pItem)
{
    int uCount = 0;

    if(NULL != pItem)
    {
        int nSize = pItem->m_lstSubItems.size();
        uCount = nSize;

        for(int i=0; i<nSize; i++)
            uCount += GetTotalCountRecursive(&(pItem->m_lstSubItems[i]));
    }

    return uCount;
}

INT64 VfsSelection::GetTotalSize()
{
    INT64 uSize = 0;

    int nCount = m_lstRootItems.size();
    for(int i=0; i<nCount; i++)
        uSize += GetTotalSizeRecursive(&(m_lstRootItems[i]));

    return uSize;
}

INT64 VfsSelection::GetTotalSizeRecursive(VfsSelectionItem *pItem)
{
    INT64 uSize = 0;

    if(NULL != pItem)
    {
        if(pItem->IsDir())
        {
            if(!pItem->IsDots())
            {
                int nCount = pItem->m_lstSubItems.size();
                for(int i=0; i<nCount; i++)
                    uSize += GetTotalSizeRecursive(&(pItem->m_lstSubItems[i]));
            }
        }
        else
            uSize = pItem->m_nSize;
    }

    return uSize;
}

//search hierarchicaly using path "dir1/dir2/file"
std::vector<VfsSelectionItem> *VfsSelection::Find(const char *szPath)
{
    String strDir = szPath;
    std::vector<VfsSelectionItem> *pChildList = &m_lstRootItems;

    int nPos;

    //TOFIX extract Drive first (search for ":\\" like in "C:\\")
    while(-1 < (nPos = strDir.find_first_of("\\/")))
    {
        String strName = strDir.Left(nPos);
        strDir = strDir.Right(strDir.Length()-nPos-1);

        if(0 == nPos)
            continue;

        VfsSelectionItem item;
        item.SetName(strName);

        std::vector<VfsSelectionItem>::iterator It = std::find(pChildList->begin(), pChildList->end(), item);
        if(It == pChildList->end())
            return NULL;

        if(strDir.IsEmpty())
            return &(It->m_lstSubItems);

        pChildList = &(It->m_lstSubItems);
    }

    //last in line
    if(!strDir.IsEmpty())
    {
        VfsSelectionItem item;
        item.SetName(strDir);

        std::vector<VfsSelectionItem>::iterator It = std::find(pChildList->begin(), pChildList->end(), item);
        if(It == pChildList->end())
            return NULL;

        return &(It->m_lstSubItems);
    }

    return pChildList;
}

VfsItem *VfsSelection::Insert(const char *szPath)
{
    //ensure path not prefixed with delimiters
    while( szPath != NULL && 
           ((*szPath) == '\\' || (*szPath) == '/' ))
    {
        szPath++;
    }

    String strDir = szPath;
    VfsSelectionItem *pParent = NULL;
    VfsSelectionItem *pEntry = NULL;
    int nPos;

    //TOFIX extract Drive first (search for ":\\" like in "C:\\")
    while(-1 < (nPos = strDir.find_first_of("\\/")))
    {
        String strName = strDir.Left(nPos);
        strDir = strDir.Right(strDir.Length()-nPos-1);
		ASSERT(strName.Length()>0);

        VfsSelectionItem entry;
        entry.m_nAttrib = ATTR_DIR;	//set this before SetName for ext calculations
        entry.m_nSize    = -1;
        entry.SetName(strName);

        pEntry = InsertUnder(entry, pParent);
        if(NULL == pEntry)
            return NULL;

        pParent = pEntry;
    }

    //last in line
    if(!strDir.IsEmpty())
    {
        VfsSelectionItem entry;
        entry.SetName(strDir);

        pEntry = InsertUnder(entry, pParent);
    }

    return pEntry;
}

//TOFIX insert as child?
VfsSelectionItem *VfsSelection::InsertUnder(VfsSelectionItem &item, VfsSelectionItem *pParent)
{
    //ASSERT(item.GetName().Length()>0); //sanity check

    std::vector<VfsSelectionItem> *pList = &m_lstRootItems;
    if(NULL != pParent)
        pList = &(pParent->m_lstSubItems);

    std::vector<VfsSelectionItem>::iterator It = std::find(pList->begin(), pList->end(), item);
    if(It == pList->end())
    {
        pList->push_back(item);

        //TOFIX move this out ?
        //NOTE: some "bad" archives have not directory flag set for some directories
        if(NULL != pParent && !pParent->IsDir()){
            pParent->m_nAttrib |= ATTR_DIR;
            pParent->m_nSize    = -1;
			pParent->CalcExt();	//recalc extension
        }

        return &((*pList)[pList->size()-1]);
    }
    else
        return &(*It);
}

void VfsSelection::operator = (const VfsListing &a)
{
    Clear();

    for(int i=0; i<a.GetCount(); i++)
        m_lstRootItems.push_back(a.GetAt(i));
}

#ifdef _DEBUG

void VfsSelection::Dump()
{
    //TOFIX dump whole tree recursively
    int nCount = m_lstRootItems.size();
	TRACE("VfsSelection::Dump %d root items!\n", nCount);
    for(int i=0; i<nCount; i++)
        TRACE("Name: %s, Size: %d, Attr: %u\n", m_lstRootItems[i].GetName().c_str(), m_lstRootItems[i].m_nSize, m_lstRootItems[i].m_nAttrib);
}

#endif

void VfsSelection::RemoveRootDirs()
{
    int nCount = m_lstRootItems.size();
    for(int i=nCount-1; i>=0; i--)
		if(m_lstRootItems[i].IsDir())
			m_lstRootItems.erase(m_lstRootItems.begin()+i);
}

void VfsSelection::RemoveRootFiles()
{
    int nCount = m_lstRootItems.size();
    for(int i=nCount-1; i>=0; i--)
		if(!m_lstRootItems[i].IsDir())
			m_lstRootItems.erase(m_lstRootItems.begin()+i);
}

