///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusParagraph.H>

extern long dbg_tmstk;

AplusParagraph::AplusParagraph(MSWidget *owner_) : MSWidgetView(owner_), MSParagraph()
{ 
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusParagraph::~AplusParagraph(void) {}

void AplusParagraph::addSenderNotify(MSEventSender *sender_)
{
  INTERNAL_COUPLE(((AplusModel *)sender_));
}

void AplusParagraph::receiveEvent(MSEvent &event_)
{
  if (event_.type()==AplusEvent::symbol())
   {
     if (dbg_tmstk) showError("Received UpdateEvent in AplusParagraph",2); // print info message
   }
  if (event_.type()==AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) showError("Received VerifyEvent in AplusParagraph",2); // print info message

     AplusVerifyEvent *ave = (AplusVerifyEvent *)&event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


MSBoolean AplusParagraph::verifyData(V v_, ::A a_)
{
  if (a_==0)
    {
      return MSFalse;
    }
  else if (a_->t==Ct)		// if it's a string
    {
      return MSTrue;
    }
  else if (a_->t==Et)		// check if it's a string vector
    {
      ::A *p=(::A *)a_->p;
      for (int i=0; i<a_->n; ++i)
	{
	  if (!QA(p[i]) || p[i]->t!=Ct)	    // if any of the elements is not a string
	    {
	      return MSFalse;
	    }
	}

      return MSTrue;
    }
  else
    {
      return MSFalse;
    }
}


const MSSymbol& AplusParagraph::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusParagraph::symbol(void)
{
  static MSSymbol sym("AplusParagraph");
  return sym;
}
