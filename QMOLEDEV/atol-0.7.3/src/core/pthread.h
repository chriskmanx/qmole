////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Portable synchronization objects
////////////////////////////////////////////////////////////////////////////

#ifdef _WIN32
	#include "_win/Mutex.h"
	#include "_win/Event.h"
	#include "_win/Semaphore.h"
#else
	#include "_unx/Mutex.h"
	#include "_unx/Event.h"
	//#include "_unx/Semaphore.h"
#endif

