////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Contains file listing with its browse history list
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILELISTCONTROLLER_H__
#define FILELISTCONTROLLER_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Vfs.h"
#include "VfsListingFiltered.h"
#include "BrowseHistoryList.h"

class FileListController  
{
public:
	FileListController();
	virtual ~FileListController();

	bool GoRootDir();

	int OnGetItemImage(long item) {
		return m_lstItems.GetAt(item).m_nIconIdx;
	} 

public:
	Vfs *m_pVfs;
	VfsListingFiltered m_lstItems;
	BrowseHistoryList  m_lstHistory; 

	bool m_bSortAscending;	//TOFIX remove, cen be read from m_lstItems
	int  m_nSortColumn;		//TOFIX remove, cen be read from m_lstItems
};

#endif // FILELISTCONTROLLER_H__
