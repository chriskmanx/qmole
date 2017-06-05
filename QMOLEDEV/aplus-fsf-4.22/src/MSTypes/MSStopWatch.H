#ifndef MSStopWatchHEADER
#define MSStopWatchHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <sys/types.h>
#include <sys/time.h>

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


class MSTypesExport MSTimeStat
{
public:
  int _user;
  int _sys;
  int _elapsed;

  MSTimeStat(void) {_user=0;_sys=0;_elapsed=0;}
  MSTimeStat(const MSTimeStat& a_) {_user=a_.user();_sys=a_.sys();_elapsed=a_.elapsed();}
  MSTimeStat(int user_,int sys_,int elapsed_) {_user=user_;_sys=sys_;_elapsed=elapsed_;}
  ~MSTimeStat(void) {}     
  
  int user(void)    const { return _user; } 
  int sys(void)     const { return _sys; } 
  int elapsed(void) const { return _elapsed; } 

  void user(int user_)      { _user=user_; } 
  void sys(int sys_)        { _sys=sys_; } 
  void elapsed(int elapsed_){ _elapsed=elapsed_; } 
  void set(int user_,int sys_,int elapsed_) {_user=user_;_sys=sys_;_elapsed=elapsed_;} 

  MSTimeStat& operator=(const MSTimeStat&);
  friend MSTypesExport MSTimeStat operator-(const MSTimeStat&,const MSTimeStat&);
  friend MSTypesExport ostream& operator<<(ostream& os_,const MSTimeStat& ts_);
};

class MSTypesExport MSStopWatch
{
public:
  MSTimeStat _startStat;
  MSTimeStat _stopStat;

  MSStopWatch(void) {}
  ~MSStopWatch(void) {}      

  void time(MSTimeStat&);
  void start(void) { time(_startStat); }
  void stop(void)  { time(_stopStat); }

  const MSTimeStat& startStat(void)  const { return _startStat; }
  const MSTimeStat& stopStat(void)   const { return _stopStat; }  

  int normalize(struct timeval tv_) { return tv_.tv_sec*1000+10*(tv_.tv_usec/10000); }

  friend MSTypesExport ostream& operator<<(ostream& os_,const MSStopWatch& time_);
};

#endif









