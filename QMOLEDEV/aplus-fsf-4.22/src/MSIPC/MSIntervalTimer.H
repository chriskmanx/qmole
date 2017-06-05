#ifndef MSIntervalTimerHEADER
#define MSIntervalTimerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSIPC/MSTimer.H>

class MSIPCExport MSIntervalTimer : public MSTimer
{
public:
  MSIntervalTimer(unsigned long msec_,MSCallback *cb_=0);
  ~MSIntervalTimer(void);

  void expirationInterval(unsigned long msec_);
  unsigned long expirationInterval(void) const;
};

#endif
