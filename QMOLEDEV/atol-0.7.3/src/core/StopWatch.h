////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: measure time between two events
////////////////////////////////////////////////////////////////////////////

#ifndef STOPWATCH_H__
#define STOPWATCH_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "types.h"

class StopWatch
{
public:
	StopWatch();
	~StopWatch();

public:
	void Start();
	//TOFIX void Stop()?
	//TOFIX void SetStartTime()?
	
	UINT64	GetElapsedSeconds();
	UINT64	GetElapsedMiliSeconds();

	inline UINT64 GetStartTime(){ return m_nStartTime; }
	static void GetCurTime(UINT64 &nResult);
	
protected:
	UINT64 m_nStartTime;
};

#endif // STOPWATCH_H__
