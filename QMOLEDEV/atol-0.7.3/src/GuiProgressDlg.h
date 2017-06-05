////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to display progress information for file operations
//////////////////////////////////////////////////////////////////////////// 

#ifndef GUIPROGRESSDLG_H__
#define GUIPROGRESSDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"

#ifdef _WIN32
 #include "core/_win/Event.h"
#else
 #include "core/_unx/Event.h"
#endif

//TOFIX rename OpProgressDlg
class GuiProgressDlg : public Dialog  
{
public:
	GuiProgressDlg();
	virtual ~GuiProgressDlg();

	void SetTitle(const char *szTitle);
	void SetFileProgress(double dPercentage);
	void SetTotalProgress(double dPercentage);

	void SetSourceInfo(const char *szText);
	void SetDestinationInfo(const char *szText);
	void SetStatsInfo(const char *szText);

	bool m_bAbortRequest;

	
	Event *m_pEvBegin;
	virtual void Create();
};

#endif // GUIPROGRESSDLG_H__
