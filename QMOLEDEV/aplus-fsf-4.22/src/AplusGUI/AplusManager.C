///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusManager.H>

extern long dbg_tmstk;

AplusManager::AplusManager(MSWidget *owner_) : MSManager(owner_)
{ 
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusManager::~AplusManager(void) {}

void AplusManager::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusManager::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusManager"  << endl;
     redraw();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusManager"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


const MSSymbol& AplusManager::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusManager::symbol(void)
{
  static MSSymbol sym("AplusManager");
  return sym;
}
