///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCheckBox.H>

MSCheckBox::MSCheckBox(MSWidget *owner_,const char *title_) :
MSActionBox(owner_,title_) {}

MSCheckBox::MSCheckBox(MSWidget *owner_,const MSStringVector& title_) : 
MSActionBox(owner_,title_) {}

MSCheckBox::~MSCheckBox(void) {}

void MSCheckBox::arm(MSCheckButton *checkButton_)    
{ activate(checkButton_); }
void MSCheckBox::disarm(MSCheckButton *checkButton_) 
{ activate(checkButton_); }

void MSCheckBox::integerState(const MSIntVector &aIntVector_)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     MSLayoutEntry *entry=(MSLayoutEntry *)np->data();
     MSCheckButton *button=(MSCheckButton *)entry->widget();
     if (aIntVector_.indexOf(button->integerTag())==aIntVector_.length())
	button->state(MSFalse);
     else
	button->state(MSTrue);
   }
}

void MSCheckBox::symbolicState(const MSSymbolVector &aSymbolVector_)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     MSLayoutEntry *entry=(MSLayoutEntry *)np->data();
     MSCheckButton *button=(MSCheckButton *)entry->widget();
     if (aSymbolVector_.indexOf(button->tag())==aSymbolVector_.length())
	button->state(MSFalse);
     else
	button->state(MSTrue);
   }
}

const MSSymbol& MSCheckBox::symbol(void)
{
  static MSSymbol sym ("MSCheckBox");
  return sym;
}

const MSSymbol& MSCheckBox::widgetType(void) const
{ return symbol(); }
