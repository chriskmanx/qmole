///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSStopWatch.H>



#if defined(MS_HAS_TMS_STRUCT)
#include <unistd.h>     //sgi needs this for sysconf, whic is used by CLK_TCK
#include <sys/times.h>
#elif !defined(MS_NO_CPU_TIME)
#include <sys/resource.h>
#if !defined(MS_HAS_GETRUSAGE_DECLARATION)
extern "C" int getrusage(int,struct rusage *);
#endif
#else
#include <sys/time.h>
#endif


MSTimeStat& MSTimeStat::operator=(const MSTimeStat& a_)
{
  set(a_.user(),a_.sys(),a_.elapsed());
  return *this;
}

MSTimeStat operator-(const MSTimeStat& a_,const MSTimeStat& b_)
{ return MSTimeStat(a_.user()-b_.user(),a_.sys()-b_.sys(),a_.elapsed()-b_.elapsed()); }

ostream& operator<<(ostream& os_,const MSTimeStat& ts_)
{
  os_<<endl;
  os_<<"User:    "<<ts_.user()<<endl;
  os_<<"Sys:     "<<ts_.sys()<<endl;
  os_<<"Elapsed: "<<ts_.elapsed()<<endl;     
  return os_;
}

ostream& operator<<(ostream& os_,const MSStopWatch& time_)
{ return os_<<(time_.stopStat()-time_.startStat()); }

void MSStopWatch::time(MSTimeStat& time_)
{
  struct timeval tp;
  struct timezone tzp;
  gettimeofday(&tp,&tzp);
#if defined(MS_HAS_TMS_STRUCT)
  struct tms tmsBuff;
  times(&tmsBuff);
  time_.set(tmsBuff.tms_utime*1000/CLK_TCK,
	    tmsBuff.tms_stime*1000/CLK_TCK,
	    normalize(tp));
#elif !defined(MS_NO_CPU_TIME)
  struct rusage r;
  getrusage(RUSAGE_SELF,&r);
  time_.set(normalize(r.ru_utime),normalize(r.ru_stime),normalize(tp));
#else
  time_.set(0,0,normalize(tp));
#endif
}

