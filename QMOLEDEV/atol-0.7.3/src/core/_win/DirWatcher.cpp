////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "DirWatcher.h"
#include "../debug.h"

CDirWatcher::CDirWatcher()
{
    m_hThread = NULL;
	m_hStopEvent = ::CreateEvent(NULL, TRUE, FALSE, NULL);

	m_pfnNotify	= NULL;
	m_nData = 0;
	m_szPath[0] = '\0';

	//set default monitor flags
	m_dwMonitorFlags = 
			FILE_NOTIFY_CHANGE_FILE_NAME	|
			FILE_NOTIFY_CHANGE_DIR_NAME		|
			FILE_NOTIFY_CHANGE_ATTRIBUTES	|
			FILE_NOTIFY_CHANGE_SIZE			|
			FILE_NOTIFY_CHANGE_LAST_WRITE;
}

CDirWatcher::~CDirWatcher()
{
	Stop();
	CloseHandle(m_hStopEvent);
}

void CDirWatcher::SetMonitorFlags(DWORD dwFlags)
{
	m_dwMonitorFlags = dwFlags;
}

void CDirWatcher::SetNotifyInfo(FN_NOTIFY pFn, long nData)
{
	//store parameters
	m_pfnNotify	= pFn;
	m_nData = nData;
}

BOOL CDirWatcher::Start(LPCTSTR pszPath)
{
	ASSERT(NULL != pszPath);
	ASSERT(NULL != m_pfnNotify);

	//store parameters
	lstrcpyn(m_szPath, pszPath, sizeof(m_szPath));

	//ensure previous thread stopped
	Stop();

	//VERIFY(::ResetEvent(m_hStopEvent));
	::ResetEvent(m_hStopEvent);

	//create thread
	DWORD dwThreadID = 0;
	m_hThread = CreateThread(NULL, 0, WatcherThread, this, CREATE_SUSPENDED, &dwThreadID);
	if(m_hThread != NULL)
	{
		SetThreadPriority(m_hThread, THREAD_PRIORITY_LOWEST);
		ResumeThread(m_hThread);
		return TRUE;
	}

	return FALSE;
}

void CDirWatcher::Stop()
{
	if(NULL == m_hThread)
		return;

	//signal termination request flag and wait for thread termination
	::SetEvent(m_hStopEvent);
    ::WaitForSingleObject(m_hThread, INFINITE);
	
	//cleanup
	::CloseHandle(m_hThread);
	m_hThread = NULL;
}

//
// Thread function for detecting file system changes
//
DWORD WINAPI CDirWatcher::WatcherThread(LPVOID pParam)
{
    CDirWatcher *pThis = (CDirWatcher *)pParam;
	ASSERT(NULL != pThis);

	// Get a handle to a file change notification object.
	// FALSE - do not monitor subtree, only this directory
    HANDLE hChange = ::FindFirstChangeNotification(pThis->m_szPath, FALSE, pThis->m_dwMonitorFlags);
	if( hChange == INVALID_HANDLE_VALUE )
		return 1;	      // Error, failed to create handle

    HANDLE aHandles[2];
	aHandles[0] = hChange;
    aHandles[1] = pThis->m_hStopEvent;

	// Sleep until a file change notification wakes this thread or
    // m_event becomes set indicating it's time for the thread to end.
	BOOL bContinue = TRUE;
    while (bContinue)
	{
		//wait for the first of two possible events (terminate request or file notification event)
        DWORD dwRes = ::WaitForMultipleObjects(2, aHandles, FALSE, INFINITE);

		if(WAIT_OBJECT_0 == dwRes)	//first handle event
		{
			//file change notification
			pThis->m_pfnNotify(pThis->m_nData);
			//::PostMessage(pThis->m_hWnd, pThis->m_dwMsg, 0, 0);

			::FindNextChangeNotification(hChange);
			TRACE("FileNotifyThread: Change notification for path: %s\n", pThis->m_szPath);
		}
		else if(WAIT_OBJECT_0 + 1 == dwRes) //second handle event
		{
			//terminate request		
            bContinue = FALSE;
			TRACE("FileNotifyThread: detected stop request\n");
		}
		else	//something else
		{
			bContinue = FALSE;
		}
    }

	if (hChange && hChange != INVALID_HANDLE_VALUE)
		::FindCloseChangeNotification(hChange);

	TRACE("FileNotifyThread: end\n");
    return 0;
}
