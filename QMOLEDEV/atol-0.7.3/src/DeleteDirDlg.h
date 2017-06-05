////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to ask permission to delete (non-empty toplevel) directories
//////////////////////////////////////////////////////////////////////////// 

#ifndef DELETEDIRDLG_H__
#define DELETEDIRDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

//TOFIX rename OpDeleteDirDlg
class DeleteDirDlg : public Dialog  
{
public:
	DeleteDirDlg();
	virtual ~DeleteDirDlg();

	void SetInfo(const char *szTxt);

	String m_strInfo;
	bool m_bHideDialog;

protected:
	virtual void Create();
	GtkWidget* create_dialog ();
};

#endif // DELETEDIRDLG_H__
