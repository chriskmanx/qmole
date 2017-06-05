////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Defines thread for a file operation
//////////////////////////////////////////////////////////////////////////// 

#include "OpThread.h"
#include "debug.h"

extern OpManager g_objOpManager;

OpThread::OpThread()
{
	m_pOp = NULL;
}

OpThread::~OpThread()
{
	if(m_pOp)
		delete m_pOp;
}

void OpThread::MainMethod()
{
	if(m_pOp){
		TRACE("OpThread::MainMethod wait for begin event!\n");
		if(!m_pOp->m_pStat->m_evOpBegin.IsSignaled())
			m_pOp->m_pStat->m_evOpBegin.Wait();	//wait until GUI ready
		TRACE("OpThread::MainMethod running operation!\n");
		m_pOp->RunOperation();
		TRACE("OpThread::MainMethod operation done!\n");
		g_objOpManager.OnOperationDone(this);
	}
}

