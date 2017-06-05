///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCollapsible.H>

extern long dbg_tmstk;

AplusCollapsible::AplusCollapsible(MSWidget *xwin_) : MSCollapsibleLayout(xwin_)
{}

AplusCollapsible::~AplusCollapsible(void)
{}

void AplusCollapsible::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusCollapsible::receiveEvent(MSEvent &event_)
{
  if (event_.type()==MSIndexedEvent::symbol())
   {
     MSIndexedEvent &ev=(MSIndexedEvent&)event_;
     update(ev.index());
   }
  else if (event_.type()==MSNullEvent::symbol())
    update(MSIndexVector::nullVector());
  else if (event_.type() == AplusEvent::symbol())
   {
    if (dbg_tmstk) cout << "Received UpdateEvent in " << "AplusCollapsible"  << endl; 
    redraw();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
    if (dbg_tmstk) cout << "Received VerifyEvent in AplusCollapsible"  << endl;
     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}

const MSSymbol& AplusCollapsible::widgetType(void) const
{
  return symbol();
}

const MSSymbol& AplusCollapsible::symbol(void)
{
  static MSSymbol sym("AplusCollapsible");
  return sym;
}
