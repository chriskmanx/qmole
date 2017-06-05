////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for Event object on Win32
////////////////////////////////////////////////////////////////////////////

#ifndef _EVENT_H__
#define _EVENT_H__

#include <windows.h>

class Event
{ 
private: 
	HANDLE event; 

public: 
	Event() { event = CreateEvent(NULL, TRUE, FALSE, NULL); } 
	~Event() { CloseHandle(event); }

	void Set()		{ SetEvent(event); }
	void Reset()	{ ResetEvent(event); }
	void Wait()		{ WaitForSingleObject(event, INFINITE); }

	bool IsSignaled(){ return (WAIT_OBJECT_0 == WaitForSingleObject(event, 0)); }
};

#endif
