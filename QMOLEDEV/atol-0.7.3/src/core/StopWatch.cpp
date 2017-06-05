////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: measure time between two events
////////////////////////////////////////////////////////////////////////////

#include "StopWatch.h"

#include <stdlib.h>
#ifdef _WIN32
#else
 #include <sys/time.h>
#endif

StopWatch::StopWatch()
{
	Start();	//just in case
}

StopWatch::~StopWatch()
{
}

void StopWatch::GetCurTime(UINT64 &nResult)
{
	//time is stored 64-bit value representing the number of 100-nanosecond intervals

#ifdef _WIN32
	//The FILETIME structure is a 64-bit value representing the number of 100-nanosecond
	//intervals since January 1, 1601.
	//TOFIX use local time?
	//GetLocalTime?
	GetSystemTimeAsFileTime((LPFILETIME)&nResult);
#else
	timeval tv;
	gettimeofday(&tv, NULL);
	nResult = (tv.tv_sec * 1000000 + tv.tv_usec) * 10;
#endif
}

void StopWatch::Start()
{
	GetCurTime(m_nStartTime);
}

UINT64 StopWatch::GetElapsedSeconds()
{
	UINT64 nCurrentTime(0);
	GetCurTime(nCurrentTime);

	//100 nanoseconds count
	UINT64 nDifference = nCurrentTime - m_nStartTime;
	UINT64 nSeconds = nDifference / 10000000;
	return nSeconds;
}

UINT64 StopWatch::GetElapsedMiliSeconds()
{
	UINT64 nCurrentTime(0);
	GetCurTime(nCurrentTime);

	//100 nanoseconds count
	UINT64 nDifference = nCurrentTime - m_nStartTime;
	UINT64 nMs = nDifference / 10000;
	return nMs;
}

