///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusRulePrintItem.H>

extern long dbg_tmstk;

AplusRulePrintItem::AplusRulePrintItem(MSWidget *owner_) : MSWidgetView(owner_), MSRulePrintItem()
{ 
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

// MSRulePrintItem does not currently have a copy constructor, so just emulate it here for now
AplusRulePrintItem::AplusRulePrintItem(const AplusRulePrintItem& rule_) : MSWidgetView(), MSRulePrintItem(rule_)
{
  ruleWidth(rule_.ruleWidth());
  fgGrayScale(rule_.fgGrayScale());
}
  

AplusRulePrintItem::~AplusRulePrintItem(void) {}

void AplusRulePrintItem::addSenderNotify(MSEventSender *sender_)
{
  INTERNAL_COUPLE(((AplusModel *)sender_));
}

void AplusRulePrintItem::receiveEvent(MSEvent &event_)
{
  if (event_.type()==AplusEvent::symbol())
   {
     if (dbg_tmstk) showError("Received UpdateEvent in AplusRulePrintItem",2); // print info message
   }
  if (event_.type()==AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) showError("Received VerifyEvent in AplusRulePrintItem",2); // print info message

     AplusVerifyEvent *ave = (AplusVerifyEvent *)&event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


MSBoolean AplusRulePrintItem::verifyData(V v_, ::A a_)
{
  return MSTrue;
}


const MSSymbol& AplusRulePrintItem::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusRulePrintItem::symbol(void)
{
  static MSSymbol sym("AplusRulePrintItem");
  return sym;
}
