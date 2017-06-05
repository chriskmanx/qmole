////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to ask permission to delete (read-only) files
//////////////////////////////////////////////////////////////////////////// 

#ifndef DELETEFILEDLG_H__
#define DELETEFILEDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

//TOFIX rename OpDeleteFileDlg
class DeleteFileDlg : public Dialog  
{
public:
	DeleteFileDlg();
	virtual ~DeleteFileDlg();

	void SetInfo(const char *szTxt);

	String m_strInfo;
	bool m_bHideDialog; 

protected:
	virtual void Create();
	GtkWidget* create_dialog ();
};

#endif // DELETEFILEDLG_H__
