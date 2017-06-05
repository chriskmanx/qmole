////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to define file item tree
////////////////////////////////////////////////////////////////////////////

#include "EntryTree.h"

//used to sort CEntryTree
class EntryComparator{
public:
	bool operator()(const CEntry &a, const CEntry &b);
};

bool EntryComparator::operator()(const CEntry &a, const CEntry &b)
{
	//directory is ALWAYS placed after file item
	//needed when writing to file
	if(!a.bDir && b.bDir)
		return true;
	if(a.bDir && !b.bDir)
		return false;

	//sort by name
	int nCmpRes = strcmp(a.strFile.c_str(), b.strFile.c_str());
	//if(0 == nCmpRes)
	//	nCmpRes = a.strFile.Cmp(b.strFile);    //Unix rare cases

	return (nCmpRes < 0);    //operator < (is a < b)
}

CEntry::CEntry()
{
	m_pUpDir = NULL;
}

CEntry::~CEntry()
{
	m_lstEntries.clear();
}

void CEntry::Clear()
{
	m_pUpDir = NULL;
	m_lstEntries.clear();

	//clear tItem
	strFile.erase();
	strFullPath.erase();
	nSize = 0;
	tmModified = 0;
	bDir = false;
}

CEntryTree::CEntryTree()
{
	m_lstEntries.clear();
}

CEntryTree::~CEntryTree()
{
}

//TOFIX insert as child?
CEntry *CEntryTree::InsertUnder(CEntry &item, CEntry *pParent)
{
	std::vector<CEntry> *pList = &m_lstEntries;
	if(NULL != pParent)
		pList = &(pParent->m_lstEntries);

	std::vector<CEntry>::iterator It = std::find(pList->begin(), pList->end(), item);
	if(It == pList->end())
	{
		pList->push_back(item);
		return &((*pList)[pList->size()-1]);
	}
	else
		return &(*It);
}

CEntry *CEntryTree::Insert(const char *szFullPath)
{
	//ensure path not prefixed with delimiters
	while( szFullPath != NULL && 
	       ((*szFullPath) == '\\' || (*szFullPath) == '/' ))
	{
		szFullPath++;
	}

	std::string strDir = szFullPath;
	CEntry *pParent = NULL;
	CEntry *pEntry = NULL;
	int nPos;

	//TOFIX extract Drive first (search for ":\\" like in "C:\\")
	while(-1 != (nPos = strDir.find_first_of("\\/")))
	{
		std::string strName = strDir.substr(0, nPos);
		strDir = strDir.substr(nPos+1, strDir.size()-nPos-1);

		CEntry entry;
		entry.strFile = strName;
		entry.bDir = true;

		pEntry = InsertUnder(entry, pParent);
		if(NULL == pEntry)
			return NULL;

		pParent = pEntry;
	}

	//last in line
	if(strDir.size()>0)
	{
		CEntry entry;
		entry.strFile = strDir;
		entry.bDir    = true;

		pEntry = InsertUnder(entry, pParent);
	}

	return pEntry;
}

void CEntryTree::Clear()
{
	m_lstEntries.clear();
}

bool CEntryTree::IsEmpty()
{
	return m_lstEntries.empty();
}

std::vector<CEntry> *CEntryTree::FindList(const char *szFullPath)
{
	std::string strDir = szFullPath;
	std::vector<CEntry> *pChildList = &m_lstEntries;

	int nPos;

	//TOFIX remove starting / ?
	while(-1 != (nPos = strDir.find_first_of("\\/")))
	{
		std::string strName = strDir.substr(0, nPos);
		strDir = strDir.substr(nPos+1, strDir.size()-nPos-1);

		if(0 == nPos)
			continue;

		CEntry item;
		item.strFile = strName;
     
		std::vector<CEntry>::iterator It = std::find(pChildList->begin(), pChildList->end(), item);
		if(It == pChildList->end())
			return NULL;

		if(0 == strDir.size())
			return &(It->m_lstEntries);

		pChildList = &(It->m_lstEntries);
	}

	//last in line
	if(0 != strDir.size())
	{
		CEntry item;
		item.strFile = strDir;

		//dump the sublist
#ifdef _DEBUG
		for(int i=0; i<pChildList->size(); i++)
		{
			CEntry *pEntry = &(*pChildList)[i];
			std::string strName = pEntry->strFile;
			printf("%s\n", strName.c_str());
		}
#endif			

		std::vector<CEntry>::iterator It = std::find(pChildList->begin(), pChildList->end(), item);
		if(It == pChildList->end())
			return NULL;

		return &(It->m_lstEntries);
	}

	return pChildList;
}

CEntry *CEntryTree::FindItem(const char *szFullPath)
{
	std::string strDir = szFullPath;
	std::vector<CEntry> *pChildList = &m_lstEntries;

	int nPos;

	//ensure not prefixed with "\" or "/"
	if( strDir.size()>0 && 
	    (strDir.at(0) == '\\' || strDir.at(0) == '/'))
	{
		strDir = strDir.substr(1, strDir.size()-1);
	}

	//TOFIX extract Drive first (search for ":\\" like in "C:\\")
	while(-1 != (nPos = strDir.find_first_of("\\/")))
	{
		std::string strName = strDir.substr(0, nPos);
		strDir = strDir.substr(nPos+1, strDir.size()-nPos-1);

		if(0 == nPos)
			continue;

		CEntry item;
		item.strFile = strName;

		std::vector<CEntry>::iterator It = std::find(pChildList->begin(), pChildList->end(), item);
		if(It == pChildList->end())
			return NULL;

		if(0 == strDir.size())
			return &(*It);

		//found one part of the path, route to the next level
		pChildList = &(It->m_lstEntries);
	}

	//last in line
	if(strDir.size() > 0)
	{
		CEntry item;
		item.strFile = strDir;

		std::vector<CEntry>::iterator It = std::find(pChildList->begin(), pChildList->end(), item);
		if(It == pChildList->end())
			return NULL;

		return &(*It);
	}

	return NULL;
}

#ifdef _DEBUG
#include <windows.h>
void CEntryTree::Dump()
{
	std::vector<CEntry> *pChildList = &m_lstEntries;
	for(unsigned int i=0; i<m_lstEntries.size(); i++)
		DumpRecursive(&(m_lstEntries[i]), "");
}

void CEntryTree::DumpRecursive(CEntry *pEntry, const char *szPfx)
{
	if(NULL != pEntry)
	{
		std::string strTmp = szPfx;
		strTmp += (*pEntry).strFile;

		OutputDebugString(strTmp.c_str());
		OutputDebugString("\n");
		for(unsigned int i=0; i<pEntry->m_lstEntries.size(); i++)
		{
			strTmp += "/";
			DumpRecursive(&(pEntry->m_lstEntries[i]), strTmp.c_str());
		}
	}
}
#endif

unsigned int CEntryTree::GetCount()
{
	unsigned int uCount = 0;

	int nSize = m_lstEntries.size();
	for(int i=0; i<nSize; i++)
		uCount += CountRecursive(&(m_lstEntries[i]));

	uCount += nSize;
	return uCount;
}

unsigned int CEntryTree::CountRecursive(CEntry *pEntry)
{
	unsigned int uCount = 0;

	if(NULL != pEntry)
	{
		int nSize = pEntry->m_lstEntries.size();
		for(int i=0; i<nSize; i++)
			uCount += CountRecursive(&(pEntry->m_lstEntries[i]));

		uCount += nSize;
    }

    return uCount;
}

//TOFIX INT64
unsigned int CEntryTree::GetTotalSize()
{
	unsigned int uSize = 0;

	int nCount = m_lstEntries.size();
	for(int i=0; i<nCount; i++)
		uSize += GetTotalSizeRecursive(&(m_lstEntries[i]));

	return uSize;
}

//TOFIX INT64
unsigned int CEntryTree::GetTotalSizeRecursive(CEntry *pEntry)
{
	unsigned int uSize = 0;

	if(NULL != pEntry)
	{
		if(pEntry->bDir)
		{
			int nCount = pEntry->m_lstEntries.size();
			for(int i=0; i<nCount; i++)
				uSize += GetTotalSizeRecursive(&(pEntry->m_lstEntries[i]));
		}
		else
			uSize = pEntry->nSize;
	}

	return uSize;
}

CEntry *CEntryTree::GetEntryRecursive(int nIdx)
{
	m_nSearchCnt = -1;

	int nCount = m_lstEntries.size();
	for(int i=0; i<nCount; i++)
	{
		//test current entry
		m_nSearchCnt ++;
		if(m_nSearchCnt == nIdx)
			return &(m_lstEntries[i]);

		//if dir text its children
		CEntry *pEntry = _GetEntryRecursive(nIdx, &(m_lstEntries[i]));
		if(pEntry)
			return pEntry;	//entry found
	}

	return NULL;
}

//depth first recursion
CEntry *CEntryTree::_GetEntryRecursive(int nIdx, CEntry *pRoot)
{
	int nCount = pRoot->m_lstEntries.size();
	for(int i=0; i<nCount; i++)
	{
		//test current entry
		m_nSearchCnt ++;
		if(m_nSearchCnt == nIdx)
			return &(pRoot->m_lstEntries[i]);

		//if dir text its children
		CEntry *pEntry = _GetEntryRecursive(nIdx, &(pRoot->m_lstEntries[i]));
		if(pEntry)
			return pEntry;	//entry found
	}

	return NULL;
}

void CEntryTree::Reorder()
{
	ReorderLevel(&m_lstEntries);
}

void CEntryTree::ReorderLevel(std::vector<CEntry> *pList)
{
	// sort list
	EntryComparator cmp;
	std::sort(pList->begin(), pList->end(), cmp);

	// recurse into subdirectories
	int nCount = pList->size();
	for(int i=0; i<nCount; i++)
	{
		if( (*pList)[i].bDir )
			ReorderLevel( &((*pList)[i].m_lstEntries) );
	}
}
