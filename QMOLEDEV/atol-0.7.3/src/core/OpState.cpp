////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Operation state implementation
////////////////////////////////////////////////////////////////////////////

#include "OpState.h"
#include "debug.h"

OpState::OpState()
{
	Reset();
	m_pThread = NULL;
}

OpState::~OpState()
{
}

void OpState::Reset()
{
	m_bAbort = false;
    
	m_nCurBytesPos = 0;
	m_nCurBytesMax = 0;
	m_nTotBytesPos = 0;
	m_nTotBytesMax = 0;

	m_nProcessedFilesCount = 0;
	m_nProcessedDirsCount = 0;

	//TOFIX initialize all
	m_nLastOverall = 0;
	m_nBytesTransfered = 0;
	m_nBytesTransferedTotal = 0;
	m_nElapsedTotal = 0;
	m_nElapsedCur = 0;
}

void OpState::Abort()
{
	m_mtxDataAccess.Lock();
	m_bAbort = true;    		//signalize abort
	m_mtxDataAccess.Unlock();
}

void OpState::InitCurrentFiles(const char *szSrc, const char *szDest)
{
	m_mtxDataAccess.Lock();

	if(NULL != szSrc)
		m_strSrcFile = szSrc;
	else
		m_strSrcFile.Empty();

	if(NULL != szDest)
		m_strDstFile = szDest;
	else
		m_strDstFile.Empty();

	m_mtxDataAccess.Unlock();
}

void OpState::InitTotalProgress(INT64 nTotal)
{
	ASSERT(nTotal > 0);

	m_mtxDataAccess.Lock();

	m_nTotBytesMax = nTotal;

	//
	m_nBytesTransfered = 0;
	m_nBytesTransferedTotal = 0;
	m_nBytesTransferedLast = 0;
	m_nElapsedTotal = 0;
	m_nElapsedCur = 0;

	m_objWatch.Start();
	m_nOpStartTime = m_objWatch.GetStartTime();

	m_mtxDataAccess.Unlock();
}

void OpState::InitCurrentProgress(INT64 nStartPos, INT64 nTotal)
{
	m_mtxDataAccess.Lock();

	//
	m_nBytesTransferedLast = m_nBytesTransferedTotal;
	m_nBytesTransferedTotal = m_nBytesTransferedLast + nStartPos;
	m_nLastOverall += m_nCurBytesMax;
	m_nTotBytesPos = m_nLastOverall + nStartPos;

	//
	m_nCurBytesPos = nStartPos;
	m_nCurBytesMax = nTotal;

	m_nElapsedCur = 0;

	m_mtxDataAccess.Unlock();
}

void OpState::StepPos(INT64 nAmount)
{
	m_mtxDataAccess.Lock();

	ASSERT(nAmount > 0);
	m_nBytesTransferedTotal += nAmount;
	m_nBytesTransfered = nAmount;

	m_nCurBytesPos += nAmount;
	m_nTotBytesPos += nAmount;

	m_mtxDataAccess.Unlock();
}

void OpState::SetPos(INT64 nPos)
{
	m_mtxDataAccess.Lock();

	ASSERT(m_nBytesTransferedTotal <= m_nBytesTransferedLast + nPos);
	ASSERT(nPos >= m_nCurBytesPos);
	
	m_nBytesTransferedTotal = m_nBytesTransferedLast + nPos;
	m_nBytesTransfered = nPos - m_nCurBytesPos;

	m_nTotBytesPos += (nPos - m_nCurBytesPos);
	m_nCurBytesPos = nPos;
   
	m_mtxDataAccess.Unlock();
}

//recalc elapsed time, ...
void OpState::RecalcStatistics()
{
	//TOFIX elapsed time must be calculated without time of waiting for user's msg boxes
	UINT64 uDiffMs = m_objWatch.GetElapsedMiliSeconds();
	m_nElapsedTotal += uDiffMs;
	m_nElapsedCur	+= uDiffMs;
	
	m_objWatch.Start();
	
	//TOFIX calc m_nCurSpeed as average of few last measurements?
	m_nCurSpeed = 0;
	if(m_nBytesTransfered > 0 && uDiffMs > 0)
		m_nCurSpeed = 1000 * m_nBytesTransfered / uDiffMs;  //current speed bytes / sec
	
	m_nTotTimeLeftEst = 10000000; //MAX_INT; //TOFIX
	m_nAvgSpeedTot	= 0;
	
	if(m_nBytesTransferedTotal > 0)
	{
		if(m_nElapsedTotal > 0)
		{
			m_nAvgSpeedTot = 1000 * m_nBytesTransferedTotal / m_nElapsedTotal;
			if(m_nAvgSpeedTot > 0 && m_nTotBytesMax > 0)
				m_nTotTimeLeftEst = (m_nTotBytesMax - m_nBytesTransferedTotal) / m_nAvgSpeedTot;
		}
	}
	//ASSERT(m_nTotTimeLeftEst >= 0);
	
	m_nCurTimeLeftEst = 10000000; //MAX_INT; //TOFIX
	m_nAvgSpeedCur	= 0;
	
	if(m_nBytesTransfered > 0)
	{
		if(m_nElapsedCur > 0)
		{
			//TOFIX use something like m_nBytesTransferedTotal if file copy didnt start from 0
			m_nAvgSpeedCur = 1000 * m_nCurBytesPos / m_nElapsedCur;
			if(m_nAvgSpeedCur > 0 && m_nTotBytesMax > 0)
				m_nCurTimeLeftEst = (m_nCurBytesMax - m_nCurBytesPos) / m_nAvgSpeedCur;
		}
	}
	//ASSERT(m_nCurTimeLeftEst >= 0);
}

