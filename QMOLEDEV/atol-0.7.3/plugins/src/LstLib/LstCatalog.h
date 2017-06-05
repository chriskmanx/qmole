////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements working with .lst "archives" (stores just file lists)
//		 This archive is compatible to TotalCommander .lst archive plugin
////////////////////////////////////////////////////////////////////////////

#ifndef LSTCATALOG_H__
#define LSTCATALOG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#if _MSC_VER > 1000
 #pragma warning (disable : 4786)
#endif 

#include <stdio.h>
#include <string>
#include <vector>
#include "EntryTree.h"

class CLstCatalog
{
public:
	CLstCatalog();
	virtual ~CLstCatalog();

	bool Open(const char *szFile, bool bRead = true);
	void Close();
	bool IsOpen(){ return (NULL != m_pArchive); };

	CEntryTree	m_lstFiles;

	void ReadList();
	void WriteList();
	bool Reorganize(const char *szRoot);

protected:
	FILE *m_pArchive;
};

#endif // LSTCATALOG_H__
