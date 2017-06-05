#ifndef MSRegularTimerHEADER
#define MSRegularTimerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSIPC/MSTimer.H>

class MSIPCExport MSRegularTimer : public MSTimer
{
public:
  MSRegularTimer(unsigned long msec_,MSCallback *cb_);
  MSRegularTimer(time_t sec_,long usec_,MSCallback *cb_);
  ~MSRegularTimer(void);
};

#endif
