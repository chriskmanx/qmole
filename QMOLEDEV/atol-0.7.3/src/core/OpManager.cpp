////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Operation manager implementation
////////////////////////////////////////////////////////////////////////////

#include "../OpStateWithProgress.h" //TOFIX move this specific implementation from core?
#include "../OpStateWithBlocking.h" //TOFIX move this specific implementation from core?
#include "OpManager.h"
#include "Op.h"
#include "OpCopy.h"
#include "OpDelete.h"
#include "OpMove.h"
#include "OpThread.h"
#include "OpPack.h"
#include "OpUnpack.h"
#include "OpHash.h"
#include "OpSplit.h"
#include "OpMerge.h"
#include "OpEncrypt.h"
#include "OpDecrypt.h"
#include "OpConnect.h"
#include "debug.h"

OpManager::OpManager()
{
	m_nTaskCounter = 0;
}

OpManager::~OpManager()
{
}

//factory method for creating operation objects
Op *OpManager::NewOperation(int nOp)
{
	Op *pOp = NULL;

	//create operation object on heap  
	switch(nOp){
		case OP_COPY:
			pOp = new OpCopy;
			((OpCopy *)pOp)->m_strDstPattern = m_strParam1;
			break;
		case OP_DELETE:
			pOp = new OpDelete;
			((OpDelete *)pOp)->m_bRecycleBin = (bool)(m_nParam1>0);
			break;
		case OP_MOVE:
			pOp = new OpMove;
			((OpMove *)pOp)->m_strDstPattern = m_strParam1;
			break;
		case OP_PACK:
			pOp = new OpPack;
			((OpPack *)pOp)->m_strArchive = m_strParam1;
			((OpPack *)pOp)->m_bMove      = m_nParam1 != 0;
			break;
		case OP_UNPACK:
			pOp = new OpUnpack;
			break;
		case OP_HASH:
			pOp = new OpHash;
			((OpHash *)pOp)->m_nHashType = m_nParam1;
			break;
		case OP_SPLIT:
			pOp = new OpSplit;
			((OpSplit *)pOp)->m_nSplitSize = m_nParam1;
			break;
		case OP_MERGE:
			pOp = new OpMerge;
			((OpMerge *)pOp)->m_strOutFile = m_strParam1;
			break;
		case OP_ENCRYPT:
			pOp = new OpEncrypt;
			((OpEncrypt *)pOp)->m_strPassword = m_strParam1;
			((OpEncrypt *)pOp)->m_bDeleteOriginal = (m_nParam1>0);
			break;
		case OP_DECRYPT:
			pOp = new OpDecrypt;
			((OpDecrypt *)pOp)->m_strPassword = m_strParam1;
			((OpDecrypt *)pOp)->m_bDeleteOriginal = (m_nParam1>0);
			break;

		case OP_CONNECT:
			pOp = new OpConnect;
			break;
	}

	//init operation object
	if(NULL != pOp)
		if(OP_CONNECT == nOp)
			pOp->m_pStat = new OpStateWithBlocking;
		else
			pOp->m_pStat = new OpStateWithProgress;

	return pOp;
}

void OpManager::StartOperation(int nOp, Vfs *pVfsSrc, Vfs *pVfsDst, VfsSelection &sel)
{
	Op *pOp = NewOperation(nOp);

	//TOFIX store Op pointer in some container (for multiple paralel operations)

	//start the operation
	if(pOp)
	{
		m_nTaskCounter ++;

		//initialize
		pOp->m_pVfsSrc = pVfsSrc;
		pOp->m_pVfsDst = pVfsDst;
		pOp->m_objSrcItems = sel;
		pOp->m_pManager    = this;

		OpThread *pOpThread = new OpThread;
		pOpThread->m_pOp    = pOp;
		pOpThread->m_nID    = m_nTaskCounter;	

		ASSERT(!pOp->m_pStat->m_evOpTerminated.IsSignaled());

		pOp->m_pStat->m_pThread = pOpThread;

		//first start the thread
		pOpThread->Run();

		// next start the progress
		pOp->m_pStat->InitProgress();
	}
}

void OpManager::OnOperationDone(OpThread *pThread)
{
	//copy output data from operation object
	if(pThread->m_pOp->GetType() == OP_COPY)
		m_bResult1 = ((OpCopy *)pThread->m_pOp)->m_bLocalDestination;

	//do not delete thread here, but withing main gui thread context
	TRACE("Operation done, fire terminate event\n");
	pThread->m_pOp->m_pStat->m_pThread = pThread;
	pThread->m_pOp->m_pStat->m_evOpTerminated.Set();	//signal termination, needs cleanup
}

