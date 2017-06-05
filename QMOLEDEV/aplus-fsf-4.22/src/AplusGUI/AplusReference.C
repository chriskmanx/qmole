///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusReference.H>

extern long dbg_tmstk;

AplusReference::AplusReference(MSWidget *owner_) : MSWidgetCommon(owner_)
{ 
  visible(MSTrue);
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusReference::~AplusReference(void) {}

void AplusReference::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusReference::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusReference"  << endl;
     redraw();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusReference"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


const MSSymbol& AplusReference::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusReference::symbol(void)
{
  static MSSymbol sym("AplusReference");
  return sym;
}
