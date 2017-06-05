///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusNotebook.H>

extern long dbg_tmstk;

AplusNotebook::AplusNotebook(MSWidget *xwin_) : MSNotebook(xwin_)
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusNotebook::~AplusNotebook(void)
{}

void AplusNotebook::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusNotebook::receiveEvent(MSEvent &event_)
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
    if (dbg_tmstk) cout << "Received UpdateEvent in AplusNotebook"  << endl; 
    redraw();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
    if (dbg_tmstk) cout << "Received VerifyEvent in AplusNotebook"  << endl;
     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


const MSSymbol& AplusNotebook::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusNotebook::symbol(void)
{
  static MSSymbol sym("AplusNotebook");
  return sym;
}
