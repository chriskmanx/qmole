///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSTv.H>
#include <MSIPC/MSAbsoluteTimer.H>

MSAbsoluteTimer::MSAbsoluteTimer(unsigned long interval_,MSCallback *cb_) :
MSTimer(MSTimer::Absolute,interval_,cb_) {}
MSAbsoluteTimer::MSAbsoluteTimer(time_t sec_,long usec_,MSCallback *cb_) :
MSTimer(MSTimer::Absolute,sec_,usec_,cb_) {}
MSAbsoluteTimer::~MSAbsoluteTimer(void) {}
