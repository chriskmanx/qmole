////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Defines thread for a file operation 
//////////////////////////////////////////////////////////////////////////// 

#ifndef OPTHREAD_H__
#define OPTHREAD_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Thread.h"
#include "Op.h"

class OpThread : public Thread  
{
public:
	OpThread();
	virtual ~OpThread();

protected:
	virtual void MainMethod();

public://protected:
	Op   *m_pOp; //operation to be executed in a separate thread
	int  m_nID;
};

#endif // OPTHREAD_H__
