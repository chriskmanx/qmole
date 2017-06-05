#ifndef MSTimerHEADER
#define MSTimerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSCallback.H>

class MSNodeItem;

class MSIPCExport MSTimer
{
public:
  enum TType {Regular,Absolute,Interval};

protected:
  static MSNodeItem *_pTimerList;
  struct timeval     _expire;
  struct timeval     _interval;
  TType              _type;
  MSNodeItem        *_pNode;
  MSCallback        *_pCallback;

  void init(TType type_,time_t sec_,long usec_,MSCallback *pCallback_);
  MSNodeItem *pNode(void);
 
public:
  MSTimer(TType type_,unsigned long msec_,MSCallback *pCallback_);
  MSTimer(TType type_,time_t sec_,long usec_,MSCallback *pCallback_);
  virtual ~MSTimer(void);

  static struct timeval *nextTimeVal(void);
  
  inline struct timeval *expire(void);
  inline struct timeval *interval(void);
  inline MSTimer::TType type(void) const;
  
  void callback(MSCallback *);
  inline MSCallback *callback(void);
  
  void reset(void);
  void stop(void);
  
  static MSBoolean processTimers(void);
  virtual void process(void);
};

inline MSNodeItem *MSTimer::pNode(void)
{return _pNode;}

inline struct timeval *MSTimer::expire(void)
{return &_expire;}

inline struct timeval *MSTimer::interval(void)
{return &_interval;}

inline MSTimer::TType MSTimer::type(void) const
{return _type;}

inline MSCallback *MSTimer::callback(void)
{return _pCallback;}

#endif






