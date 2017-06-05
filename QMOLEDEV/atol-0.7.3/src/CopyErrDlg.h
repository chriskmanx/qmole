////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to handle copy failure (inside file operation)
//////////////////////////////////////////////////////////////////////////// 

#ifndef COPYERRDLG_H__
#define COPYERRDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

class CopyErrDlg : public Dialog  
{
public:
	CopyErrDlg();
	virtual ~CopyErrDlg();

	void SetInfo(const char *szTxt);

	String m_strInfo;

protected:
	virtual void Create();

	GtkWidget* create_dialog ();
};

#endif // COPYERRDLG_H__
