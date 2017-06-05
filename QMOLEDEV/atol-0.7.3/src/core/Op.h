////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This is an abstract file operation class (all operations inherit this class).
//         Each operation is executed within separate thread and usually has
//         a progress window attached to it.
////////////////////////////////////////////////////////////////////////////

#ifndef OP_H__
#define OP_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Vfs.h"
#include "VfsSelection.h"
#include "OpState.h"
#include "OpManager.h"
#include "pthread.h"
#include "opcodes.h"

//operation class

class Op
{
public:
	Op();
	virtual ~Op();

	void RunOperation();
	int  GetType(){ return m_nOpType; } 

public:
	OpManager *m_pManager; //operation manager
	OpState   *m_pStat;    //operation status

	bool m_bError;
	Vfs *m_pVfsSrc;
	Vfs *m_pVfsDst;
	VfsSelection m_objSrcItems;

protected:
	int m_nOpType;

protected:
	virtual void OpInit();
	virtual void OpCleanup();
	virtual bool OpExecute() = 0;
};

#endif // OP_H__

