///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSDoubleClick.H>

extern unsigned long applicationDoubleClickInterval(void);

MSBoolean MSDoubleClick::isDoubleClick(const XEvent *ev_)
{
   if (ev_->type==ButtonPress||ev_->type==ButtonRelease)
   {
      if (ev_->xbutton.time-lastEventTime()<=applicationDoubleClickInterval())
      {
	 eventTime(0);
	 return MSTrue;
      }
      else
      {
	 eventTime(ev_->xbutton.time);
	 return MSFalse;
      }
   }
   else return MSFalse;
}

