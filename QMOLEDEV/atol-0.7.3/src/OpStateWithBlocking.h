////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: File operation progress object, specialized to use GUI block dialog (no progress info)
//////////////////////////////////////////////////////////////////////////// 

#ifndef OPSTATEWITHBLOCKING_H__
#define OPSTATEWITHBLOCKING_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "GuiBlockDlg.h"
#include "core/OpState.h"

class OpStateWithBlocking : public OpState  
{
public:
	OpStateWithBlocking();
	virtual ~OpStateWithBlocking();

	virtual void InitProgress();
	virtual void DestroyProgress();
	virtual void UpdateProgress();
	virtual void InitCurrentFiles(const char *szSrc, const char *szDest);

	void SetOperationName();

//protected:
	void KillTimer();

public:
	GuiBlockDlg *m_pDlg;
	int m_nTimer;
};

#endif // OPSTATEWITHBLOCKING_H__
