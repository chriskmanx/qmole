///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSModelCheckBox.H>

MSModelCheckBox::MSModelCheckBox(MSWidget *owner_,const char *title_)
: MSCheckBox(owner_,title_)
{}

MSModelCheckBox::MSModelCheckBox(MSWidget *owner_,const MSStringVector& title_)
: MSCheckBox(owner_,title_)
{}

MSModelCheckBox::~MSModelCheckBox(void)
{}

void MSModelCheckBox::updateData(void)
{
   drawCurrentState();
}

void MSModelCheckBox::firstMapNotify(void)
{
   MSCheckBox::firstMapNotify();
   drawCurrentState();
}

void MSModelCheckBox::arm(MSCheckButton *checkButton_)
{
   setModel();
   MSCheckBox::arm(checkButton_);
}

void MSModelCheckBox::disarm(MSCheckButton *checkButton_)
{
   setModel();
   MSCheckBox::disarm(checkButton_);
}

void MSModelCheckBox::update(const MSIndexVector &)
{
   drawCurrentState();
}

