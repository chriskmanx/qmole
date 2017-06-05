////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_DIRWATCHER_H__D0FD3261_1FE3_11D6_8F25_00C1280194AC__INCLUDED_)
#define AFX_DIRWATCHER_H__D0FD3261_1FE3_11D6_8F25_00C1280194AC__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

//TOFIX could add NT version that use ReadDirectoryChangesW ?
//NOTE this watches single dir at the time
#include <windows.h>

typedef void (* FN_NOTIFY)(long);

class CDirWatcher  
{
public:
	CDirWatcher();
	virtual ~CDirWatcher();

	void SetMonitorFlags(DWORD dwFlags);
	void SetNotifyInfo(FN_NOTIFY pFn, long nData);

	BOOL Start(LPCTSTR pszPath);
	void Stop();

protected:
	static DWORD WINAPI WatcherThread(LPVOID pParam);

	DWORD	m_dwMonitorFlags;

	FN_NOTIFY m_pfnNotify;
	long	m_nData;
	TCHAR	m_szPath[MAX_PATH];

	HANDLE	m_hThread;
	HANDLE	m_hStopEvent;
};

#endif // !defined(AFX_DIRWATCHER_H__D0FD3261_1FE3_11D6_8F25_00C1280194AC__INCLUDED_)
