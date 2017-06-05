///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSTime.H>
#include <AplusGUI/AplusCommand.H>

AplusCommand::AplusCommand(MSWidget *owner_) : AplusEntryField(owner_)
{}

AplusCommand::~AplusCommand(void)
{}

MSBoolean AplusCommand::verifyData(V,A a_)
{ return (0!=a_ && QA(a_) && a_->t==Ct && a_->r<=1)?MSTrue:MSFalse; }

const char *AplusCommand::buffer(void)
{ return fieldEditor()->string(); }

void AplusCommand::buffer(A buf_)
{
  if (QA(buf_)&&buf_->t==Ct&&fieldEditor()->mapped()==MSTrue)
   {
     fieldEditor()->string((char *)buf_->p);
   }
}

int AplusCommand::cursorPosition(void)
{ return fieldEditor()->cursorPosition(); }

void AplusCommand::cursorPosition(int cp_)
{ fieldEditor()->cursorPosition(cp_); }


void AplusCommand::keyPress(const XEvent *e_,KeySym k_,unsigned int s_,const char * b_)
{
  if (fieldEditor()->mapped()==MSTrue) 
   {
     AplusEntryField::keyPress(e_, k_, s_, b_);
     activateCallback(MSSymbol("keypress"));
   }
  else
   {
     if (k_==XK_Return) reference();
     else if (sensitive()==MSTrue)
      {
	if (k_==XK_Insert)
	 {
	   MSString s;
	   const char *str = formatOutput(s);
	   if (str != 0)
	    {
	      fieldEditor()->string(str);
	    }
  	   else fieldEditor()->string(""); 
           mapEditor();
	 }
	else if (k_==XK_BackSpace)
	 {
	   clearEditor();
	   mapEditor();
	 }
	else if (strlen(b_)>0)
	 {
	   clearEditor();
	   AplusEntryField::keyPress(e_, k_, s_, b_);
	   if (fieldEditor()->length()>0) 
            {
              mapEditor();
	      activateCallback(MSSymbol("keypress"));
            }
	 }
      }
   }
}


const MSSymbol& AplusCommand::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusCommand::symbol(void)
{
  static MSSymbol sym("AplusCommand");
  return sym;
}
