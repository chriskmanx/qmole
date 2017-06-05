////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define options for a file search operation
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILESEARCHDLG_H__
#define FILESEARCHDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "FileSearchThread.h"
#include "core/pthread.h"

class FileSearchDlg : public Dialog  
{
public:
	FileSearchDlg();
	virtual ~FileSearchDlg();

	void Create();
	void Destroy();

	void Start();
	void Stop();
	void OnSearchDone();

	void AddItem(VfsItem &item);
	void AddToList(VfsItem &item);

	String m_strInitialDir;
	String m_strInitialName;

	GtkListStore *m_store;
	VfsListing m_lstResults;

	Event m_evOpDone;
	Event m_evRefreshList;
	Mutex m_objListAccess;

	int   m_nTimer;
	bool  m_bSearching;
	int   m_nResultListSize;

protected:
	GtkWidget* create_file_search (); 
	void KillTimer();

protected:
	FileSearchThread *m_pObjThread;
};

#endif // FILESEARCHDLG_H__
