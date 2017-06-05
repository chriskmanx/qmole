////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to block panel while operation is in progress
//////////////////////////////////////////////////////////////////////////// 

#ifndef GUIBLOCKDLG_H__
#define GUIBLOCKDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"

#ifdef _WIN32
 #include "core/_win/Event.h"
#else
 #include "core/_unx/Event.h"
#endif

class GuiBlockDlg : public Dialog  
{
public:
	GuiBlockDlg();
	virtual ~GuiBlockDlg();

	virtual void Create();

	void SetTitle(const char *szTitle);
	void SetInfoText(const char *szText);

	bool m_bAbortRequest;
	Event *m_pEvBegin;
};

#endif // GUIBLOCKDLG_H__
