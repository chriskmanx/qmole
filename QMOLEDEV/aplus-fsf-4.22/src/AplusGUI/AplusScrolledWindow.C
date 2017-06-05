///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusScrolledWindow.H>

extern long dbg_tmstk;

AplusScrolledWindow::AplusScrolledWindow(MSWidget *xwin_) : MSScrolledWindow(xwin_)
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusScrolledWindow::~AplusScrolledWindow(void)
{}

void AplusScrolledWindow::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusScrolledWindow::receiveEvent(MSEvent &event_)
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
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusScrolledWindow"  << endl;
     redraw();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusScrolledWindow"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


const MSSymbol& AplusScrolledWindow::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusScrolledWindow::symbol(void)
{
  static MSSymbol sym("AplusScrolledWindow");
  return sym;
}


