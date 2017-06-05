#ifndef MSDoubleClickHEADER
#define MSDoubleClickHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>
#include <X11/Xlib.h>

class MSGUIExport MSDoubleClick
{
protected:
   unsigned long  _lastEventTime;
   unsigned long lastEventTime(void) const { return _lastEventTime; }
public:
   MSDoubleClick(void) : _lastEventTime(0) {}
   void eventTime(unsigned long eventTime_) { _lastEventTime=eventTime_; }
   MSBoolean isDoubleClick(const XEvent *ev_);
};

#endif





