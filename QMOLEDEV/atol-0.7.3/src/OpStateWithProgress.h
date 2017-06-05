////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: File operation progress object, specialized to use GUI progress dialog
//////////////////////////////////////////////////////////////////////////// 

#ifndef OPSTATEWITHPROGRESS_H__
#define OPSTATEWITHPROGRESS_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "GuiProgressDlg.h"
#include "core/OpState.h"

class OpStateWithProgress : public OpState  
{
public:
	OpStateWithProgress();
	virtual ~OpStateWithProgress();

	virtual void InitProgress();
	virtual void DestroyProgress();
	virtual void UpdateProgress();
	virtual void InitCurrentFiles(const char *szSrc, const char *szDest);

	void SetOperationName();

//protected:
	void KillTimer();

public:
	GuiProgressDlg *m_pDlg;
	int m_nTimer;
	int m_nUpdateCount;
};

#endif // OPSTATEWITHPROGRESS_H__
