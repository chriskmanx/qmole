////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Window class for Options dialog
////////////////////////////////////////////////////////////////////////////

#ifndef OptionsDialog_H_
#define OptionsDialog_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include <string>

class OptionsDialog : public Dialog  
{
public:
	OptionsDialog();
	virtual ~OptionsDialog();

	virtual void Create();

	void OnDialogInit();
	void OnDialogOK();

protected:
	GtkWidget* create_options_dialog (GtkWidget* parent=NULL);
	std::string m_strLocale;
};

#endif // OptionsDialog_H_
