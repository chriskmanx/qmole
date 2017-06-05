////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Window for single file list panel (file list + path info + drive button menu)
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILELISTPANEL_H__
#define FILELISTPANEL_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "FileList.h"

class FileListPanel  
{
public:
	FileListPanel();
	virtual ~FileListPanel();

	void Create();
	bool IsActive();
	void ShowActive(bool bActive = true);
	void Activate();
	void DropDriveMenu();
	void DropBookmarkMenu();

public:
	GtkWidget* m_pWidget;
	GtkWidget* m_pEventBox;
	GtkWidget* m_pPathLabel;
	GtkWidget* m_pDriveButton;
	GtkWidget* m_pEntry;		// on-the-fly edit for editing path
	FileList   m_wndFileList;
	bool m_bMenuIgnoreMousePos;
	bool m_bActive;

	std::vector<std::string> m_lstPartitions;	//local drives as listed on menu
};

#endif // FILELISTPANEL_H__
