///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIntRadioBox.H>

MSIntRadioBox::MSIntRadioBox(MSWidget *owner_,const char *title_): 
MSRadioBox(owner_,title_)
{ internalCouple(new MSInt); }

MSIntRadioBox::MSIntRadioBox(MSWidget *owner_,const MSStringVector& title_):
MSRadioBox(owner_,title_)
{ internalCouple(new MSInt); }
 
MSIntRadioBox::MSIntRadioBox(MSWidget *owner_,MSInt &model_,const char *title_): 
MSRadioBox(owner_,title_)
{ model(model_); }

MSIntRadioBox::MSIntRadioBox(MSWidget *owner_,MSInt& model_,const MSStringVector& title_):
MSRadioBox(owner_,title_)
{ model(model_); }

MSIntRadioBox::~MSIntRadioBox(void)
{}

void MSIntRadioBox::model(MSInt& model_)
{ couple(&model_); }

void MSIntRadioBox::updateData(void)
{
  drawCurrentState();
}

void MSIntRadioBox::firstMapNotify(void)
{
  MSRadioBox::firstMapNotify();
  drawCurrentState();
}

void MSIntRadioBox::arm(MSRadioButton *radioButton_)
{
  MSRadioBox::arm(radioButton_);
  setModel();
}

void MSIntRadioBox::setModel(void)
{
  if (MSView::model()!=0)
   {
     MSInt *myModel=(MSInt *)_model;
     *myModel=activeButton()->integerTag();
   }
}

void MSIntRadioBox::drawCurrentState(void)
{
  if (MSView::model()!=0)
   {
     MSInt *value=(MSInt *)_model;
     MSRadioButton *radioButton=(MSRadioButton *)button((int)*value);
     if (radioButton==0)
      {
	if (activeButton()!=0) *value=activeButton()->integerTag();
      }
     else if (radioButton!=activeButton())
      {
	if (activeButton()!=0) activeButton()->state(MSFalse);
	_activeButton=radioButton;
	activeButton()->state(MSTrue);
      }
   }
}

void MSIntRadioBox::receiveEvent(MSEvent &)
{
  drawCurrentState();
}












