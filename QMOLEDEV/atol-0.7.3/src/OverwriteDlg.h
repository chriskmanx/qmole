////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to ask user a permission to overwrite files (inside file operation)
//////////////////////////////////////////////////////////////////////////// 

#ifndef OVERWRITEDLG_H__
#define OVERWRITEDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

//TOFIX rename OpOverwriteDlg
class OverwriteDlg : public Dialog  
{
public:
	OverwriteDlg();
	virtual ~OverwriteDlg();

	void SetInfo(const char *szTxt);

	String m_strInfo;

protected:
	virtual void Create();
	GtkWidget* create_overwrite_dialog ();
};

#endif // OVERWRITEDLG_H__
