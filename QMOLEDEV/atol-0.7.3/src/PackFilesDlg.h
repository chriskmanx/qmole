////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Start dialog for pack operation, defines archive type, name and additional properties
////////////////////////////////////////////////////////////////////////////

#ifndef PACKFILESDLG_H__
#define PACKFILESDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

class PackFilesDlg : public Dialog  
{
public:
	PackFilesDlg();
	virtual ~PackFilesDlg();

	virtual void Create();
	virtual void Destroy();
	
	void OnDialogOK();

protected:
	GtkWidget* create_pack_dialog ();

public:
	String m_strArchive;
	bool   m_bMultiSupport;
	bool   m_bMoveToArchive;
};

#endif // PACKFILESDLG_H__
