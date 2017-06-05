////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for thread-safe GUI messages using idle time processing
////////////////////////////////////////////////////////////////////////////

#ifndef THREADSAFEGUI_H__
#define THREADSAFEGUI_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <gtk/gtk.h>
#include <vector>

#include "core/pthread.h"	//portable thread stuff

class ThreadSafeAction
{
	friend class ThreadSafeQueue;
	friend gboolean on_gtk_idle (gpointer data);

public:
	ThreadSafeAction();
	virtual ~ThreadSafeAction();

	virtual void Run() = 0;

protected:
	Event m_ActionDone;
};

class ThreadSafeQueue
{
	friend gboolean on_gtk_idle (gpointer data);

public:
	ThreadSafeQueue();
	virtual ~ThreadSafeQueue();

	void Init();

	void Add(ThreadSafeAction *pAction);
	void Wait(ThreadSafeAction *pAction);
	void Remove(ThreadSafeAction *pAction);

	int  GetActionsExecutedCount(){ return m_nExecutedCount; };
	int  Count(){ return m_lstActions.size(); }

protected:
	std::vector<ThreadSafeAction *> m_lstActions;
	GMutex *m_LockEditing;
	int m_nExecutedCount;
};

#endif // THREADSAFEGUI_H__
