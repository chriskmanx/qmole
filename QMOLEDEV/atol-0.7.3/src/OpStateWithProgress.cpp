////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: File operation progress object, specialized to use GUI progress dialog
//////////////////////////////////////////////////////////////////////////// 

#include "OpStateWithProgress.h"
#include "support.h"
#include "core/util.h"
#include "core/debug.h"
#include "core/OpThread.h"

#include "ThreadSafeGui.h"
extern ThreadSafeQueue g_gui_pump; //used only for unsinkable dialogs
static int progress_timer(gpointer data);

#define PROGRESS_REFRESH_INTERVAL 100
#define STATISTICS_REFRESH_RATE 5

OpStateWithProgress::OpStateWithProgress()
{
	m_pDlg	 = NULL;
	m_nTimer = 0;
	m_nUpdateCount = 0;
}

OpStateWithProgress::~OpStateWithProgress()
{
}

void OpStateWithProgress::DestroyProgress()
{
	KillTimer();

	//destroy dialog
	if(NULL != m_pDlg)
		delete m_pDlg;
	m_pDlg = NULL;
}

void OpStateWithProgress::InitCurrentFiles(const char *szSrc, const char *szDest)
{
	OpState::InitCurrentFiles(szSrc, szDest);
}

void OpStateWithProgress::InitProgress()
{
	m_pDlg = new GuiProgressDlg;
	m_pDlg->m_pEvBegin = &m_evOpBegin;	//pass event that controls Op start
	m_pDlg->Create();

	SetOperationName();

	m_nTimer = g_timeout_add (PROGRESS_REFRESH_INTERVAL, progress_timer, this); 
	m_pDlg->ShowModal();
}

void OpStateWithProgress::SetOperationName()
{
	//set operation name
	OpThread* pThread = static_cast<OpThread*>(m_pThread);
	if(NULL == pThread)
		return;

	int nType = pThread->m_pOp->GetType();

	std::string strTitle;

	switch(nType){
		case OP_COPY:	strTitle = _("Copy operation"); break;
		case OP_MOVE:	strTitle = _("Move operation"); break;
		case OP_DELETE: strTitle = _("Delete operation"); break;
		case OP_MKDIR:	strTitle = _("MkDir operation"); break;
		case OP_RENAME: strTitle = _("Rename operation"); break;
		case OP_PACK:	strTitle = _("Pack operation"); break;
		case OP_UNPACK: strTitle = _("Unpack operation"); break;
		case OP_ENCRYPT:strTitle = _("Encrypt operation"); break;
		case OP_DECRYPT:strTitle = _("Decrypt operation"); break;
		case OP_SPLIT:	strTitle = _("Split operation"); break;
		case OP_MERGE:	strTitle = _("Merge operation"); break;
		case OP_HASH:	strTitle = _("Hash operation"); break;
		case OP_CONNECT:strTitle = _("Connect operation"); break;
	}
	
	m_pDlg->SetTitle(strTitle.c_str());

}

void OpStateWithProgress::KillTimer()
{
	//destroy timer
	if(m_nTimer > 0)
		g_source_remove (m_nTimer);
	m_nTimer = 0;
}

gboolean progress_timer(gpointer data)
{
	OpStateWithProgress *pState = (OpStateWithProgress *)data;
	pState->UpdateProgress();
	return TRUE;
}

//called within main thread using timer
void OpStateWithProgress::UpdateProgress()
{
	static int nCount = g_gui_pump.GetActionsExecutedCount();

	//operation cleanup code
	if(m_evOpTerminated.IsSignaled())
	{
		m_mtxDataAccess.Lock();

		TRACE("Operation terminated, cleaning up!\n");
		DestroyProgress();
		
		//delete the operation thread
		OpThread* pThread = static_cast<OpThread*>(m_pThread);
		Op *pOp = pThread->m_pOp;
		pThread->m_pOp = NULL;
		delete pThread; 

		m_mtxDataAccess.Unlock();
		delete pOp;	// delete operation at the end - fix mem corruption (deletes this object!)
		return;
	}

	if(IsAborted())
	{
		TRACE("Operation thread has abort flag: waiting for termination\n!");
		return;
	}

	//refresh progress state in GUI
	if(m_pDlg)
	{
		if(m_pDlg->m_bAbortRequest)
		{
			TRACE("Abort request: pass from GUI to the thread!\n");
			Abort(); //request operation thread to abort
			return;
		}

		//refresh file name (proper place is here - from the main thread)
		m_pDlg->SetSourceInfo(m_strSrcFile);
		m_pDlg->SetDestinationInfo(m_strDstFile);

		//TOFIX RecalcProgress
		double dCurProgress = 0.0;
		if(m_nCurBytesMax > 0)
			dCurProgress = ((double)m_nCurBytesPos)/m_nCurBytesMax;
		static double dTotProgress = 0.0;
		if(m_nTotBytesMax > 0)
			dTotProgress = ((double)m_nTotBytesPos)/m_nTotBytesMax;

		m_pDlg->SetFileProgress(dCurProgress);
		m_pDlg->SetTotalProgress(dTotProgress);

		if(!(m_nUpdateCount++ % STATISTICS_REFRESH_RATE)) // avoid too much flickering
		{
			RecalcStatistics();

			//TOFIX: some of these strings can be cached for some time (once per file op)
	  		String strTotPos, strTotMax, strCurPos, strCurMax;
			String strCurSpeed, strCurAvgSpeed, strTotAvgSpeed;

			strTotPos = FormatSizeUnits(m_nTotBytesPos);
			strTotMax = FormatSizeUnits(m_nTotBytesMax);
			strCurPos = FormatSizeUnits(m_nCurBytesPos);
			strCurMax = FormatSizeUnits(m_nCurBytesMax);

			//strCurSpeed = FormatSizeUnits(m_info.m_nCurSpeed);
			//strCurSpeed += "/s";
			strCurAvgSpeed = FormatSizeUnits(m_nAvgSpeedCur);
			strCurAvgSpeed += "/s";
			strTotAvgSpeed = FormatSizeUnits(m_nAvgSpeedTot);
			strTotAvgSpeed += "/s";

			//TOFIX FormatTimeSyst -> /10000000
			String strCurElapsedTotal   = FormatTime(m_nElapsedCur/1000); //milisecons to seconds
			String strCurEstimatedTotal = FormatTime(m_nCurTimeLeftEst + m_nElapsedCur/1000);
			String strTotElapsedTotal   = FormatTime(m_nElapsedTotal/1000); //milisecons to seconds
			String strTotEstimatedTotal = FormatTime(m_nTotTimeLeftEst + m_nElapsedTotal/1000);

			//TOFIX TRACE("Cur: %d/%d=%f\n", m_nCurBytesPos, m_nCurBytesMax, dCurProgress);
			//TOFIX TRACE("Tot: %d/%d=%f\n", m_nTotBytesPos, m_nTotBytesMax, dTotProgress);

			//TOFIX special string for delete op "Time: %s / %s", strTotElapsedTotal, strTotEstimatedTotal);
			String strStat;
			strStat.Printf(_("Current size: %s / %s\nCurrent time: %s / %s (%s)\nTotal size: %s / %s\nTotal time: %s / %s (%s)"),
				strCurPos.c_str(),
				strCurMax.c_str(),
				strCurElapsedTotal.c_str(),
				strCurEstimatedTotal.c_str(),
				strCurAvgSpeed.c_str(),
				strTotPos.c_str(), 
				strTotMax.c_str(),
				strTotElapsedTotal.c_str(),
				strTotEstimatedTotal.c_str(),
				strTotAvgSpeed.c_str());

			m_pDlg->SetStatsInfo(strStat.c_str());
		}

		//window might have gotten below others, put it back on top!
		if( nCount != g_gui_pump.GetActionsExecutedCount() &&	//some dialog poped in the meantime
		    g_gui_pump.Count() == 0)			//TOFIX all such dialogs closed?
		{
			gtk_window_present(GTK_WINDOW(m_pDlg->m_pDialog));
			nCount = g_gui_pump.GetActionsExecutedCount();
		}
	}
	else
	{
		TRACE("No GUI found, kill timer!\n");
		KillTimer();
	}
}
