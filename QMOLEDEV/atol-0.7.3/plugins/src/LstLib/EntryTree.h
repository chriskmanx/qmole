////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to define file item tree
////////////////////////////////////////////////////////////////////////////

#ifndef ENTITYTREE_H__
#define ENTITYTREE_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <string>
#include <vector>
#include <algorithm>

typedef struct {
	std::string strFile;
	std::string strFullPath;
	int  nSize;
	time_t tmModified;
	bool bDir;
} tItem;

class CEntry : public tItem 
{
public:
	CEntry();
	~CEntry();

	CEntry(const CEntry &other){
		operator =(other);
	}

	void operator =(const CEntry &other){
		//tItem::operator =(other);
		m_lstEntries = other.m_lstEntries;
		m_pUpDir = other.m_pUpDir;

		strFile = other.strFile;
		strFullPath = other.strFullPath;
		nSize = other.nSize;
		tmModified = other.tmModified;
		bDir = other.bDir;
	}

	void operator =(const tItem &other){
		//tItem::operator =(other);
		strFile = other.strFile;
		strFullPath = other.strFullPath;
		nSize = other.nSize;
		tmModified = other.tmModified;
		bDir = other.bDir;
	}
	void Clear();

	bool operator ==(const CEntry &other){ return (0 == strcmp(strFile.c_str(), other.strFile.c_str())); }

	CEntry *m_pUpDir;	//TOFIX use ?
	std::vector<CEntry> m_lstEntries;
};

//TOFIX ovo bi trebala biti jedna od vanijih klasa
// (keiranje FTP stabla direktorija, stabla unutar zip arhive i sl.)
class CEntryTree
{
public:
	CEntryTree();
	~CEntryTree();

	bool IsEmpty();
	void Clear();

	CEntry *Insert(const char *szFullPath);
	CEntry *InsertUnder(CEntry &item, CEntry *pParent);
	
	std::vector<CEntry> *FindList(const char *szFullPath);
	CEntry *FindItem(const char *szFullPath);
	CEntry *GetEntryRecursive(int nIdx);
		
	//TOFIX are those two below making one single CEntry
	std::string m_strRoot;
	std::vector<CEntry> m_lstEntries;

	unsigned int GetCount();
	unsigned int CountRecursive(CEntry *pEntry);

	unsigned int GetTotalSize();
	unsigned int GetTotalSizeRecursive(CEntry *pEntry);

	void Reorder();
	void ReorderLevel(std::vector<CEntry> *pList);

#ifdef _DEBUG
	void Dump();
protected:
	void DumpRecursive(CEntry *pEntry, const char *szPfx);
#endif

protected:	
	CEntry *_GetEntryRecursive(int nIdx, CEntry *pRoot);
	int m_nSearchCnt;
};

#endif // ENTITYTREE_H__

