///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSNodeList.H>
#include <MSIPC/MSTv.H>
#include <MSIPC/MSIntervalTimer.H>

MSIntervalTimer::MSIntervalTimer(unsigned long interval_,MSCallback *cb_) :
MSTimer(MSTimer::Interval,interval_,cb_) {}
MSIntervalTimer::~MSIntervalTimer(void) {}

void MSIntervalTimer::expirationInterval(unsigned long msec_)
{
  unsigned long msec=interval()->tv_sec*1000+interval()->tv_usec/1000;
  if (msec!=msec_)
   {
     interval()->tv_sec=msec_/1000;
     interval()->tv_usec=(msec_%1000)*1000;
     (void) tvnorm(interval());
     // Reset myself if I'm not currently stopped
     if (pNode()->next()!=pNode()&&pNode()->prev()!=pNode()) reset();
   }
}

unsigned long MSIntervalTimer::expirationInterval(void) const
{
  unsigned long msec=_interval.tv_sec*1000+_interval.tv_usec/1000;
  return msec;
}
