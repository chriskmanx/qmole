////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements portable thread class
//////////////////////////////////////////////////////////////////////////// 

#ifndef THREAD_H__
#define THREAD_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifdef _WIN32
 #define _WINSOCKAPI_   // Prevent inclusion of winsock.h in windows.h
 #include <windows.h>
 #include <process.h>
#else
 #include <pthread.h> 
#endif

#ifdef _WIN32
 #define STDCALL __stdcall
 #define THR_HANDLE HANDLE
 #define THR_RESULT unsigned int
#else
 #define STDCALL
 #define THR_HANDLE pthread_t
 #define THR_RESULT void * 
#endif 

class Thread  
{
public:
	Thread();
	virtual ~Thread();

	bool Run();
	void Wait();	//join thread

	bool m_bDone;

protected:
	void Exit();

	virtual void MainMethod() = 0;
	static THR_RESULT STDCALL ThreadMethod(void *data);

protected:
	THR_HANDLE	 m_hThread;
	unsigned int m_nThreadID;
};

#endif // THREAD_H__
