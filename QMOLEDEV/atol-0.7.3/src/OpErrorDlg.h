////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to handle abstract file operation failure (Retry,Skip,Abort)
//////////////////////////////////////////////////////////////////////////// 

#ifndef OPERRDLG_H__
#define OPERRDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

class OpErrorDlg : public Dialog  
{
public:
	OpErrorDlg();
	virtual ~OpErrorDlg();

	void SetInfo(const char *szTxt);

	String m_strInfo;

protected:
	virtual void Create();
	GtkWidget* create_dialog ();
};

#endif // OPERRDLG_H__
