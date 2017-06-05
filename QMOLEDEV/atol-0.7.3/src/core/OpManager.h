////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Operation mamanger object starts all file operations
////////////////////////////////////////////////////////////////////////////

#ifndef OPMANAGER_H__
#define OPMANAGER_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Vfs.h"
#include "VfsSelection.h"
#include <map>

class OpThread;
class Op;

class OpManager  
{
public:
	OpManager();
	virtual ~OpManager();

	void StartOperation(int nOp, Vfs *pVfsSrc, Vfs *pVfsDst, VfsSelection &sel);
	Op *NewOperation(int nOp);
	void OnOperationDone(OpThread *pOp);

	//TOFIX design mechanism for operation input and ooepration results 
	//(current mechanism assumes one operation at the time - shared data)

	//operation input params
	String       m_strParam1;
	unsigned int m_nParam1;

	//operation output params
	bool	m_bResult1;

protected:
	int	m_nTaskCounter;
};

#endif // OPMANAGER_H__

