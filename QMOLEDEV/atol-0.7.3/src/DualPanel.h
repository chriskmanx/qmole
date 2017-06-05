////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class that stores and gives access to two main file panels
//////////////////////////////////////////////////////////////////////////// 

#ifndef DUALPANEL_H__
#define DUALPANEL_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "FileListPanel.h"

class DualPanel  
{
public:
	DualPanel();
	virtual ~DualPanel();

	void CreatePanels();
	void InitVfs();
	void SwapPanels();

	FileListPanel &GetLeftPanel(){ return m_wndPanelLeft; };
	FileListPanel &GetRightPanel(){ return m_wndPanelRight; };

	FileListPanel &GetActivePanel();
	FileListPanel &GetInactivePanel();
	FileListPanel &GetOtherPanel(const FileListPanel &first);
	FileListPanel &GetPanelFromList(const FileList &list);

	FileList &GetLeftFileList(){ return m_wndPanelLeft.m_wndFileList; };
	FileList &GetRightFileList(){ return m_wndPanelRight.m_wndFileList; };

	FileList &GetActiveFileList();
	FileList &GetInactiveFileList();

protected:
	FileListPanel	m_wndPanelLeft;
	FileListPanel	m_wndPanelRight;
};

#endif // DUALPANEL_H__
