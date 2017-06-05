///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusLayout.H>

extern long dbg_tmstk;

AplusLayout::AplusLayout(MSWidget *xwin_) : MSLayout(xwin_)
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusLayout::~AplusLayout(void)
{}

void AplusLayout::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusLayout::receiveEvent(MSEvent &event_)
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
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusLayout"  << endl;
     redraw();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusLayout"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


const MSSymbol& AplusLayout::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusLayout::symbol(void)
{
  static MSSymbol sym("AplusLayout");
  return sym;
}
