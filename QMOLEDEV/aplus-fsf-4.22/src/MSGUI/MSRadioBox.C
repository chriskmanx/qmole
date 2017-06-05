///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSRadioBox.H>

MSRadioBox::MSRadioBox(MSWidget *owner_,const char *title_) : 
MSActionBox(owner_,title_) 
{ _activeButton=0; }
MSRadioBox::MSRadioBox(MSWidget *owner_,const MSStringVector& title_) : 
MSActionBox(owner_,title_) 
{ _activeButton=0; }
MSRadioBox::~MSRadioBox(void) {}

const MSSymbol& MSRadioBox::symbol(void)
{
  static MSSymbol sym ("MSRadioBox");
  return sym;
}

const MSSymbol& MSRadioBox::widgetType(void) const
{ return symbol(); }

void MSRadioBox::arm(MSRadioButton *radioButton_)
{
  disarm();
  _activeButton=radioButton_;
  if (activeButton()!=0) activeButton()->state(MSTrue);
}

void MSRadioBox::disarm(void)
{
  if (activeButton()!=0) activeButton()->state(MSFalse);
  _activeButton=0;
}

void MSRadioBox::firstMapNotify(void)
{
  MSNodeItem     *hp=childListHead(); 
  MSNodeItem     *np=hp;
  MSLayoutEntry  *entry;
  MSRadioButton  *radioButton;
  unsigned        count=0;
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     radioButton=(MSRadioButton *)entry->widget();
     if (radioButton->state()==MSTrue) 
      {
        if (count==0) _activeButton=radioButton;
        count++;
      }
     if (count>1) radioButton->state(MSFalse);
   }
  if (count==0&&(np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     radioButton=(MSRadioButton *)entry->widget();
     radioButton->state(MSTrue);
     _activeButton=radioButton;
   }
  MSActionBox::firstMapNotify();
}

void MSRadioBox::activeButton(MSRadioButton *radioButton_, MSBoolean callback_)
{
   radioButton_->arm(callback_);
}













