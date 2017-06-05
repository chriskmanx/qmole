////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Operation class implementation
////////////////////////////////////////////////////////////////////////////

#include "Op.h"

Op::Op()
{
	m_pVfsSrc = NULL;
	m_pVfsDst = NULL;
	m_pManager = NULL;
	m_pStat = NULL;
	m_nOpType = 0;
}

Op::~Op()
{
	if(NULL != m_pStat)
		delete m_pStat;
}

void Op::OpInit()
{
	//connect VFS with operation progress object
	if(m_pVfsSrc)
	{
		m_pVfsSrc->m_pProgress = m_pStat;
		if(m_pVfsSrc->GetType() != Vfs::LOCAL) {
			//m_pVfsSrc->Lock();
		}
	}
	if(m_pVfsDst)
	{
		m_pVfsDst->m_pProgress = m_pStat;
		if(m_pVfsDst->GetType() != Vfs::LOCAL) {
			//m_pVfsDst->Lock();
		}
	}
}

void Op::OpCleanup()
{
	//detach VFS from progress object
	if(m_pVfsSrc)
	{
		m_pVfsSrc->m_pProgress = NULL;
		if(m_pVfsSrc->GetType() != Vfs::LOCAL) {
			//m_pVfsSrc->Unlock();
		}
	}
	if(m_pVfsDst)
	{
		m_pVfsDst->m_pProgress = NULL;
		if(m_pVfsDst->GetType() != Vfs::LOCAL) {
			//m_pVfsDst->Unlock();
		}
	}
}

void Op::RunOperation()
{
	OpInit();
	OpExecute();
	OpCleanup();
}

