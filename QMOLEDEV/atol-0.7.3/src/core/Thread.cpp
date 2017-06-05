////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements portable thread class
//////////////////////////////////////////////////////////////////////////// 

#include "Thread.h"

Thread::Thread()
{
#ifdef _WIN32
	m_hThread = NULL;
#else
	m_nThreadID = 0;
	//TOFIX m_hThread = NULL;	
#endif
	m_bDone = false;
}

Thread::~Thread()
{
#ifdef _WIN32
	if (m_hThread)
		CloseHandle(m_hThread);
#else
	if (m_hThread)
		pthread_detach(m_hThread);
#endif
}

bool Thread::Run()
{
#ifdef _WIN32
	m_hThread = (HANDLE)_beginthreadex(NULL, 0, ThreadMethod, this, 0, &m_nThreadID); 	
#else
	m_nThreadID = pthread_create(&m_hThread, NULL, ThreadMethod, (void*)this);
#endif
	return true;	//TOFIX
}

void Thread::Exit()
{
	// terminate the thread
#ifdef _WIN32
	_endthread();
#else
	pthread_exit(NULL);
#endif
}

void Thread::Wait()
{
	// block calling thread by waiting for this thread's termination
#ifdef _WIN32
	WaitForSingleObject(m_hThread, INFINITE);
#else
	pthread_join(m_hThread, NULL);
#endif
}

THR_RESULT STDCALL Thread::ThreadMethod(void *data)
{
	Thread *pThread = (Thread *)data;
	if(pThread){
		pThread->MainMethod();
		pThread->m_bDone = true;
	}

	return 0;
}

