////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for Semaphore object on Win32
////////////////////////////////////////////////////////////////////////////

#ifndef _SEMAPHORE_H__
#define _SEMAPHORE_H__

#include <windows.h>

class Semaphore 
{ 
private: 
	HANDLE semaphore; 

public: 
	Semaphore() { semaphore = CreateSemaphore(NULL, 0, 0x7ffffff, NULL); } 
	Semaphore(int available) { semaphore = CreateSemaphore(NULL, available, 0x7ffffff, NULL); }
	~Semaphore() { CloseHandle(semaphore); }
	
	void Wait() { WaitForSingleObject(semaphore, INFINITE); }
	void Post() { ReleaseSemaphore(semaphore, 1, NULL); }
	void Post(int how_many) { ReleaseSemaphore(semaphore, how_many, NULL); } 

	bool IsAvailable(){ return (WAIT_OBJECT_0 == WaitForSingleObject(semaphore, 0)); }
};

#endif
