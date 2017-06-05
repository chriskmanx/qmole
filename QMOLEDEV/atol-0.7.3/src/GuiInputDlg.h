////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: General dialog for input of a single string (plain or as a password)
//////////////////////////////////////////////////////////////////////////// 

#ifndef GUIINPUTDLG_H__
#define GUIINPUTDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

class GuiInputDlg : public Dialog
{
public:
	GuiInputDlg(bool bPassword = false, bool bCreate = true);
	virtual ~GuiInputDlg();

	void SetLabel(const char *szText);
	void SetInput(const char *szText, bool bSelectAll = false);
	
	const char *GetInput();
	String m_strData;
	String m_strLabel;

protected:
	bool m_bPasswordMode;

protected:
	virtual void Create();
	GtkWidget* create_input_dialog();
};

#endif // GUIINPUTDLG_H__
