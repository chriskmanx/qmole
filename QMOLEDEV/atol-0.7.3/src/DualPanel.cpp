////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class that stores and gives access to two main file panels
//////////////////////////////////////////////////////////////////////////// 

#include "DualPanel.h"
#include "core/VfsManager.h"

extern VfsManager g_VfsManager;

DualPanel::DualPanel()
{
}

DualPanel::~DualPanel()
{
}

FileListPanel &DualPanel::GetActivePanel()
{
	if(m_wndPanelLeft.IsActive())
		return m_wndPanelLeft;
	else
		return m_wndPanelRight;
}

FileListPanel &DualPanel::GetInactivePanel()
{
	if(m_wndPanelLeft.IsActive())
		return m_wndPanelRight;
	else
		return m_wndPanelLeft;
}

FileListPanel &DualPanel::GetOtherPanel(const FileListPanel &first)
{
	if(&first == &m_wndPanelLeft)
		return m_wndPanelRight;
	else
		return m_wndPanelLeft;
}

FileListPanel &DualPanel::GetPanelFromList(const FileList &list)
{
	if(&list == &(m_wndPanelLeft.m_wndFileList))
		return m_wndPanelLeft;
	else
		return m_wndPanelRight;
}

FileList &DualPanel::GetActiveFileList()
{
	return GetActivePanel().m_wndFileList;
}
	
FileList &DualPanel::GetInactiveFileList()
{
	return GetInactivePanel().m_wndFileList;
}

void DualPanel::CreatePanels()
{
	m_wndPanelLeft.Create();
	m_wndPanelRight.Create();

	//exchange pointers (TOFIX remove this?)
	m_wndPanelLeft.m_wndFileList.m_pOtherList = &m_wndPanelRight.m_wndFileList;
	m_wndPanelRight.m_wndFileList.m_pOtherList = &m_wndPanelLeft.m_wndFileList;
}

void DualPanel::InitVfs()
{
	g_VfsManager.InitList(&(m_wndPanelLeft.m_wndFileList.m_ctrl), NULL);
	g_VfsManager.InitList(&(m_wndPanelRight.m_wndFileList.m_ctrl), NULL);

	m_wndPanelLeft.m_wndFileList.InitVfs();
	m_wndPanelRight.m_wndFileList.InitVfs();
}

void DualPanel::SwapPanels()
{
	//swap lists (VFS's) for left/right panels
	//NOTE: we must exchange entire VFS stacks in the panels

	//exchange entire VFS stack
	g_VfsManager.SwapVfsStacks(&GetLeftPanel().m_wndFileList.m_ctrl, &GetRightPanel().m_wndFileList.m_ctrl);
    
	//TOFIX remember selections and restore them after exchange

	//exchange listing (no need for listing the Vfs)
	VfsListing lstTemp;
	lstTemp = GetLeftPanel().m_wndFileList.GetListing();
	GetLeftPanel().m_wndFileList.SetListing(GetRightPanel().m_wndFileList.GetListing());
	GetRightPanel().m_wndFileList.SetListing(lstTemp);

	//exchange history lists
	BrowseHistoryList tmpHistory = GetLeftPanel().m_wndFileList.m_ctrl.m_lstHistory;
	GetLeftPanel().m_wndFileList.m_ctrl.m_lstHistory = GetRightPanel().m_wndFileList.m_ctrl.m_lstHistory;
	GetRightPanel().m_wndFileList.m_ctrl.m_lstHistory = tmpHistory; 

	//exchange sort information
	bool bAsc = GetLeftPanel().m_wndFileList.m_ctrl.m_bSortAscending;
	GetLeftPanel().m_wndFileList.m_ctrl.m_bSortAscending = GetRightPanel().m_wndFileList.m_ctrl.m_bSortAscending;
	GetRightPanel().m_wndFileList.m_ctrl.m_bSortAscending = bAsc;
	int nCol =  GetLeftPanel().m_wndFileList.m_ctrl.m_nSortColumn;
	GetLeftPanel().m_wndFileList.m_ctrl.m_nSortColumn = GetRightPanel().m_wndFileList.m_ctrl.m_nSortColumn;
	GetRightPanel().m_wndFileList.m_ctrl.m_nSortColumn = nCol;
	GetLeftPanel().m_wndFileList.RefreshSortIndicator();
	GetRightPanel().m_wndFileList.RefreshSortIndicator();
}

