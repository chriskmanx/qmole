////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for thread-safe GUI messages using idle time processing
////////////////////////////////////////////////////////////////////////////

#include "ThreadSafeGui.h"
#ifndef _WIN32
#include <unistd.h>
#endif

gboolean on_gtk_idle (gpointer data);

ThreadSafeAction::ThreadSafeAction()
{
}

ThreadSafeAction::~ThreadSafeAction()
{
}

//
//

ThreadSafeQueue::ThreadSafeQueue()
{
	m_nExecutedCount = 0;
}

ThreadSafeQueue::~ThreadSafeQueue()
{
	//no more processing
	g_idle_remove_by_data(this);	//unregister idle method

	g_mutex_lock (m_LockEditing);
	
	//delete and remove all remaining actions
	int nCount = Count();
	for(int i=0; i<nCount; i++){
		m_lstActions[i]->m_ActionDone.Set();	//this will free any waiting thread
		delete m_lstActions[i];
	}
	m_lstActions.clear();

	g_mutex_unlock (m_LockEditing);
	
	g_mutex_free(m_LockEditing);	//free mutex
}

void ThreadSafeQueue::Init()
{
	m_LockEditing = g_mutex_new();	//allocate mutex
	g_idle_add(on_gtk_idle, this);	// register idle-time callback method
}

void ThreadSafeQueue::Add(ThreadSafeAction *pAction)
{
	g_mutex_lock (m_LockEditing);
	m_lstActions.push_back(pAction);	//push action in the queue
	g_mutex_unlock (m_LockEditing);
}

void ThreadSafeQueue::Wait(ThreadSafeAction *pAction)
{
	pAction->m_ActionDone.Wait();	//block waiting for action finish executing
}

void ThreadSafeQueue::Remove(ThreadSafeAction *pAction)
{
	g_mutex_lock (m_LockEditing);

	//find and remove from list
	int nCount = Count();
	for(int i=0; i<nCount; i++)
		if(pAction == m_lstActions[i])
			m_lstActions.erase(m_lstActions.begin() + i);

	g_mutex_unlock (m_LockEditing);

	//delete action object
	delete pAction;
}

gboolean on_gtk_idle (gpointer data)
{
	ThreadSafeQueue *pQueue = (ThreadSafeQueue *)data;

	g_mutex_lock (pQueue->m_LockEditing);

	int nCount = pQueue->Count();
	if(nCount > 0)
	{
		//grab and execute oldest waiting event
		ThreadSafeAction *pAction = pQueue->m_lstActions[0];

		gdk_threads_enter();
		pAction->Run();
		gdk_threads_leave();

		//remove action object from list
		pQueue->m_lstActions.erase(pQueue->m_lstActions.begin());
		pQueue->m_nExecutedCount ++;

		//mark action as done
		pAction->m_ActionDone.Set();
	}
	else
	{
		//nothing to do in idle:
		//sleep a little to reduce process CPU usage when idle
		//TOFIX util function msSleep()
		#ifdef _WIN32
			Sleep(1);	   //1ms	
		#else
			usleep(1000);  //1ms
		#endif
	}

	g_mutex_unlock (pQueue->m_LockEditing);
	
	return TRUE;
}

