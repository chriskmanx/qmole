////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements archive description object and the list of such objects
////////////////////////////////////////////////////////////////////////////

#ifndef ARCHIVEINFO_H__
#define ARCHIVEINFO_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "ArjUnpack.h"

#if _MSC_VER > 1000
 #pragma warning (disable : 4786)
#endif 

#include <map>
#include <string>

//
//	store single archive data 
//	(one for each archive opened by this plugin)
//
class CArchiveInfo  
{
public:
	CArchiveInfo();
	~CArchiveInfo();

	CArjUnpack	m_objArj;

	int m_dwID;
	void *m_hArchive;
	std::string m_strFile;

	bool	m_bGzListed;

	bool	m_bUnpacking;
	int		m_nProgressPos;
	std::string  m_strCurEntry;

};

typedef CArchiveInfo *AAA;
typedef std::map<int, AAA> tOpenArchiveMap;

//
//	list of opened archive descriptions
//	(each archive is mapped to its unique ID number)
//
class COpenArchivesList : protected tOpenArchiveMap
{
public:
	COpenArchivesList();
	~COpenArchivesList();

	void Clear();
	int Add(CArchiveInfo *pInfo);
	bool  Remove(int  dwID);
	void  RemoveAll();

	bool IdExists(int dwID);
	CArchiveInfo *Find(int  dwID);
};

#endif // ARCHIVEINFO_H__
