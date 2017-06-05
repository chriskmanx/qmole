// ArchiveInfo.h: interface for the CArchiveInfo class.
//
//////////////////////////////////////////////////////////////////////

#ifndef ARCHIVEINFO_H__
#define ARCHIVEINFO_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../plugin_defs.h"
#include <windows.h>
#include "unrar/unrar.h"

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

	int   m_dwID;
	RAROpenArchiveDataEx m_rar;
	HANDLE m_hArchive;
	std::string m_strFile;

	struct RARHeaderDataEx m_HeaderData;//enumeration entry
		
	tProcessDataProc m_pfnProgress;
	long	m_dwUserData;

	BOOL	m_bUnpacking;
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

	void  Clear();
	int   Add(CArchiveInfo *pInfo);
	bool  Remove(int dwID);
	void  RemoveAll();

	bool  IdExists(int dwID);
	CArchiveInfo *Find(int dwID);
};

#endif // ARCHIVEINFO_H__
