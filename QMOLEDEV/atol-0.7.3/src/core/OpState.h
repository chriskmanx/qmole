////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: OpState object stores status + progress + statistics information
//       for a single file manager operation (COp)
////////////////////////////////////////////////////////////////////////////

#ifndef OPSTATE_H__
#define OPSTATE_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "types.h"
#include "String.h"
#include "pthread.h"
#include "StopWatch.h"

class OpState
{
public:
	OpState();
	virtual ~OpState();

	//override for different progress dialog
	virtual void InitProgress(){};
	virtual void UpdateProgress(){};
	virtual void DestroyProgress(){};
	virtual void InitCurrentFiles(const char *szSrc, const char *szDest);

	void Abort();
	bool IsAborted() const { return m_bAbort; }    

	void InitTotalProgress(INT64 nTotal);
	void InitCurrentProgress(INT64 nStartPos, INT64 nTotal);
    
	void StepPos(INT64 nAmount);
	void SetPos(INT64 nPos);

	void RecalcStatistics();
	void Reset();

public:
	//progress info (num. of bytes transfered)
	INT64 m_nCurBytesPos; //currently processed file
	INT64 m_nCurBytesMax; //currently processed file (file size)
	INT64 m_nTotBytesPos; //all files to be processed
	INT64 m_nTotBytesMax; //all files to be processed (total)

	//elapsed time / operation speed info
	UINT64  m_nOpStartTime;		//as returned by xTimer
	UINT64  m_nElapsedCur;		//elapsed ms time for current file
	UINT64  m_nElapsedTotal;	//total elapsed ms time for this operation

	UINT64  m_nCurTimeLeftEst;  //estimated seconds left to finish current file
	UINT64  m_nTotTimeLeftEst;  //estimated seconds left to finish total operation
	UINT64  m_nCurSpeed;        //current speed for current file transfer (bytes/sec)
	UINT64  m_nAvgSpeedCur;     //average speed for current file (bytes/sec)
	UINT64  m_nAvgSpeedTot;	    //average speed for total operation (multiple files, bytes/sec)

	UINT64  m_nLastOverall;	    //total bytes transfered so far in this op excluding current file
	UINT64  m_nBytesTransfered;
	UINT64  m_nBytesTransferedTotal; //used in case that we started from some offset (to calculate speed)
	UINT64  m_nBytesTransferedLast;  //total before this file started
	//UINT64  m_nStartingPos;        //initial OverallPos ? -> to calculate speed

	//TOFIX use in Op's
	int m_nProcessedFilesCount;
	int m_nProcessedDirsCount;

	String m_strSrcFile;
	String m_strDstFile;

	bool m_bAbort;

	StopWatch  m_objWatch;
	Event      m_evOpBegin;		// "proceed with operation" event (progress->thread)
	Event      m_evOpTerminated;	// "operation terminated" event (thread->progress)
	Mutex      m_mtxDataAccess;	// lock data access to prevent multithread coruption
	void      *m_pThread;  //thread pointer
};

#endif // OPSTATE_H__

