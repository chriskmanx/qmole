////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file list widget using GtkTreeView
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILELIST_H__
#define FILELIST_H__

#include <gtk/gtk.h>
#include "core/FileListController.h"
#include "core/OpManager.h"

class FileList
{
public:
	FileList();

	VfsListing &GetListing(){ return m_ctrl.m_lstItems; }
	void SetListing(VfsListing &list); 

	bool SetDirectory(const char *szDir, bool bAddToHistory = true, bool bSilent = false);
	void OnCalcDirSize();
	bool QuickSearchExit();

	void Create();
	void InitVfs();
	int  GetColumnIndex(GtkTreeViewColumn *column);
	void RefreshList();
	void RefilterList();
	void Redraw();

	void OnItemDblClick(int nIdx);
	bool GoUpDir();
	void SetSort(int nCol, bool bAsc);
	void SetSortIndicator(int nCol, bool bAsc);
	void RefreshSortIndicator();

	void GoHomeDir();

	//operations
	void PanelList(bool bRestoreSelection = false, bool bForceRefresh = false, bool bSetFocus = true);
	void Copy();
	void Rename();
	void Move();
	void MkDir();
	void Delete(bool bRecycleBin = false);
	void EditFile();
	void ContextMenu(GtkWidget *widget, int x, int y);

	void DropDriveMenu(bool bLeft);
	void GoIntoFile();

	//selection related
	bool IsItemSelected(int nIdx);
	void SelectAll();
	void SelectNone();
	void SelectInvert();
	void SelectAdd();
	void SelectRemove();
	void SetFilter();

	int  SelectFilesOnly(bool bUnselectOther = true);
	void SelectItem(int nIdx);
	void DeselectItem(int nIdx);

	void GetSelection(VfsSelection &sel, bool bIgnoreDots = false, bool bAddFocusOnEmpty = false);
	int  SetSelection(VfsSelection &sel);

	int  GetItemFocus();
	void SetItemFocus(int nIdx);
	bool IsItemVisible(int nIdx);
	void SetTopItem(int nIdx);
	int	 GetTopItem();

	void StartNotifications(Vfs *pVfsSrc, Vfs *pVfsDst = NULL);
	void StopNotifications(Vfs *pVfsSrc, Vfs *pVfsDst = NULL);

public:
	GtkWidget* m_pWidget;
	GtkWidget* m_pPathWidget;
	GtkWidget* m_pInfoWidget;
	GtkWidget* m_pScrollWidget;
	GtkTreeViewColumn *cols[5]; //TOFIX no hardcoding!
	FileListController m_ctrl;

	FileList*  m_pOtherList; //TOFIX move to central place
	int m_nEditedIdx;

	bool m_bQuickSearchMode;
	String m_strQuickPtrn;

	//cumulate file list change notifications, to reduce flickering
	int    m_nChangeCumulateTimer;
	UINT64 m_nLastChangeEventMs;
	UINT64 m_nLastRefreshEventMs;

	bool m_bDnDEnabled;
	bool m_bSkipSpaceInfo;

public:
	void UpdateInfoBar();
	void UpdatePathBar();
};

#endif // FILELIST_H__

