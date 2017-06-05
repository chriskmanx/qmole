////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define and start remote connections (FTP, SFTP)
//////////////////////////////////////////////////////////////////////////// 

#ifndef SITEMANAGERDLG_H__
#define SITEMANAGERDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"
#include "core/ConnectionInfoList.h"

class SiteManagerDlg : public Dialog  
{
public:
	SiteManagerDlg();
	virtual ~SiteManagerDlg();

	CNodeInfo m_objCurSite;

	void OnCtxMenu_New();
	void OnCtxMenu_Delete();
	void OnCtxMenu_Duplicate();
	void OnCtxMenu_MoveUp();
	void OnCtxMenu_MoveDown();
	void OnSiteTree_SelChange();
	void OnSiteTree_DblClick();
	void OnChk_Anonymous();
	void OnBtn_OK();
	void OnBtn_Close();

	void UpdateData(CNodeInfo &info, bool bToScreen = true);
	void EnableCtrls(bool bEnable = true);
	void ClearData();
	void BuildTree();

protected:
	int  m_nSelection;
	bool m_bIniLoaded;
	bool m_bSkipSave;
	CConnectionInfoList m_lstSites;

protected:
	virtual void Create();
	GtkWidget* create_dialog (GtkWidget* parent);
	int GetSelectedSite();
};

#endif // SITEMANAGERDLG_H__
