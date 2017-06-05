///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSBoolEntryField.H>

MSBoolEntryField::MSBoolEntryField(MSWidget *owner_,const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  internalCouple(new MSBool);
  init();
}

MSBoolEntryField::MSBoolEntryField(MSWidget *owner_,MSBool& model_,
				   const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  model(model_);
  init();
}

MSBoolEntryField::~MSBoolEntryField(void) {}

void MSBoolEntryField::init(void)
{ _format=MSFormat(MSBool::TrueAndFalse); }

void MSBoolEntryField::model(MSBool& model_)
{ couple(&model_); }

void MSBoolEntryField::model(const MSBool& model_)
{ constCouple(&model_); }

void MSBoolEntryField::updateData(void)
{
  if (MSView::model()==0) internalCouple(new MSBool());
  MSEntryFieldPlus::updateData();
}
MSBoolean MSBoolEntryField::validate(const char *pString_)
{
  if (MSView::model()!=0)
   {
     MSBool aBool;
     if (aBool.set(pString_)==MSError::MSSuccess)
      {
	getBool()=aBool;
	return MSTrue;
      }
   }
  return MSFalse;
}

const char *MSBoolEntryField::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) getBool().format(buffer_,format());
  return buffer_.string();
}

void MSBoolEntryField::increment(void)
{
  //cast is need for Visual C++, and doesn't hurt other compilers
  if((MSBoolean)getBool()==MSTrue) setBool(MSFalse);
  else setBool(MSTrue);
}

void MSBoolEntryField::decrement(void)
{
  if((MSBoolean)getBool()==MSFalse) setBool(MSTrue);
  else setBool(MSFalse);
}
