////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Starting dialog for file delete operation
//////////////////////////////////////////////////////////////////////////// 

#ifndef DELETESTARTDLG_H__
#define DELETESTARTDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"

//TOFIX rename OpDeleteStartDlg
class DeleteStartDlg : public Dialog  
{
public:
	DeleteStartDlg(bool bRecycle = false);
	virtual ~DeleteStartDlg();

	void SetList(const char *szList);

protected:
	GtkWidget* create_delete_start_dialog ();
	virtual void Create();
	bool m_bRecycle;
};

#endif // DELETESTARTDLG_H__
