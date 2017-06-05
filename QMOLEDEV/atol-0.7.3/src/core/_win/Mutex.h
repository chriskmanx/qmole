////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for Mutex object on Win32
////////////////////////////////////////////////////////////////////////////

#ifndef _MUTEX_H__
#define _MUTEX_H__

#include <windows.h>

class Mutex 
{ 
private: 
	CRITICAL_SECTION lock;

public: 
#ifdef _DEBUG
	bool locked;
#endif

	Mutex(void) { 
		InitializeCriticalSection(&lock); 
		#ifdef _DEBUG
			locked = false; 
		#endif
	}
	bool Lock(void) { 
		EnterCriticalSection(&lock); 
		#ifdef _DEBUG
			locked = true;
		#endif
			return true; 
	}
	bool Unlock(void) { 
		LeaveCriticalSection(&lock); 
	#ifdef _DEBUG
		locked = false; 
	#endif	
		return true; 
	} 
};

#endif
