///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSSymbolCheckBox.H>

MSSymbolCheckBox::MSSymbolCheckBox(MSWidget *owner_,const char *title_)
: MSModelCheckBox(owner_,title_)
{ internalCouple(new MSSymbolVector); }


MSSymbolCheckBox::MSSymbolCheckBox(MSWidget *owner_,const MSStringVector& title_)
: MSModelCheckBox(owner_,title_)
{ internalCouple(new MSSymbolVector); }


MSSymbolCheckBox::MSSymbolCheckBox(MSWidget *owner_,MSSymbolVector &aSymbolVector_,const char *title_)
: MSModelCheckBox(owner_,title_)
{
   model(aSymbolVector_);
}

MSSymbolCheckBox::MSSymbolCheckBox(MSWidget *owner_,MSSymbolVector &aSymbolVector_,const MSStringVector& title_)
: MSModelCheckBox(owner_,title_)
{
   model(aSymbolVector_);
}

MSSymbolCheckBox::~MSSymbolCheckBox(void)
{}

void MSSymbolCheckBox::model(MSSymbolVector &aSymbolVector_)
{
   couple(&aSymbolVector_);
}

void MSSymbolCheckBox::setModel(void)
{
   if (MSView::model()!=0)
   {
      MSSymbolVector *myModel=(MSSymbolVector *)_model;
      *myModel=symbolicState();
   }
}

void MSSymbolCheckBox::drawCurrentState(void)
{
   if (MSView::model()!=0)
   {
      MSSymbolVector *pSymbolVector=(MSSymbolVector *)_model;
      symbolicState(*pSymbolVector);
   }
}

