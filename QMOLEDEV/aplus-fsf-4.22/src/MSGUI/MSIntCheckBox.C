///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIntCheckBox.H>

MSIntCheckBox::MSIntCheckBox(MSWidget *owner_,const char *title_)
: MSModelCheckBox(owner_,title_)
{ internalCouple(new MSIntVector); }

MSIntCheckBox::MSIntCheckBox(MSWidget *owner_,const MSStringVector& title_)
: MSModelCheckBox(owner_,title_)
{ internalCouple(new MSIntVector); }

MSIntCheckBox::MSIntCheckBox(MSWidget *owner_,MSIntVector &aIntVector_,const char *title_)
: MSModelCheckBox(owner_,title_)
{
   model(aIntVector_);
}

MSIntCheckBox::MSIntCheckBox(MSWidget *owner_,MSIntVector &aIntVector_,const MSStringVector& title_)
: MSModelCheckBox(owner_,title_)
{
   model(aIntVector_);
}

MSIntCheckBox::~MSIntCheckBox(void)
{}

void MSIntCheckBox::model(MSIntVector &aIntVector_)
{
   couple(&aIntVector_);
}

void MSIntCheckBox::setModel(void)
{
   if (MSView::model()!=0)
   {
      MSIntVector *myModel=(MSIntVector *)_model;
      *myModel=integerState();
   }
}

void MSIntCheckBox::drawCurrentState(void)
{
   if (MSView::model()!=0)
   {
      MSIntVector *pIntVector=(MSIntVector *)_model;
      integerState(*pIntVector);
   }
}

