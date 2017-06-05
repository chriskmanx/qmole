///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusPrintColumn.H>
#include <AplusGUI/AplusConvert.H>
#include <AplusGUI/AplusPrintTool.H>

extern long dbg_tmstk;

AplusPrintColumn::AplusPrintColumn(MSWidget *owner_) : MSWidgetView(owner_), MSPrintColumn(0)
{ 
  destroyable(MSFalse);
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusPrintColumn::~AplusPrintColumn(void) {}

void AplusPrintColumn::addSenderNotify(MSEventSender *sender_)
{
  INTERNAL_COUPLE(((AplusModel *)sender_));
}

void AplusPrintColumn::receiveEvent(MSEvent &event_)
{
  if (event_.type()==AplusEvent::symbol())
   {
     if (dbg_tmstk) showError("Received UpdateEvent in AplusPrintColumn",2); // print info message
   }
  if (event_.type()==AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) showError("Received VerifyEvent in AplusPrintColumn",2); // print info message

     AplusVerifyEvent *ave = (AplusVerifyEvent *)&event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


MSBoolean AplusPrintColumn::verifyData(V v_, ::A a_)
{
  if (a_==0)
    {
      return MSFalse;
    }
  else if (a_->t==Ct || a_->t==Et)		// a single paragraph item or a list of items
    {
      return MSTrue;
    }
  else
    {
      return MSFalse;
    }
}


const MSSymbol& AplusPrintColumn::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusPrintColumn::symbol(void)
{
  static MSSymbol sym("AplusPrintColumn");
  return sym;
}


// This function should somehow be combined with AplusPrintTool::constructBody(); they do same things
void AplusPrintColumn::constructColumn(void)
{
  ::A aItems=model()->a();

  if (verifyData(model()->aplusVar(),aItems)==MSFalse)
    {
      if (dbg_tmstk)  showError("Invalid `reportcolumn format",1); // print warning message
      return;
    }

  AplusPrintTool::constructPrintManager(this, aItems);

  if (aItems->t==Ct)	// it's a single text item
    {
      numColumns(1);
    }
  else	// it's a list or a vector of print items
    {
      if (numColumns()==0)	// if the number of columns hasn't been set by the user
	{
	  numColumns(aItems->n);
	}
    }
}  
