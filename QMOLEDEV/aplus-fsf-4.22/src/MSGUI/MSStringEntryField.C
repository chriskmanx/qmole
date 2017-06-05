///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSStringEntryField.H>

MSStringEntryField::MSStringEntryField(MSWidget *owner_,const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{ internalCouple(new MSString); }

MSStringEntryField::MSStringEntryField(MSWidget *owner_,MSString& model_,const char *label_,const MSSymbol& tag_):
MSEntryFieldPlus(owner_,label_,tag_) 
{ model(model_); }

MSStringEntryField::~MSStringEntryField(void) {}

void MSStringEntryField::model(MSString& model_)
{ couple(&model_); }

void MSStringEntryField::model(const MSString& model_)
{ constCouple(&model_); }

void MSStringEntryField::value(const char *value_) 
{ if (MSView::model()!=0) value()=value_; }

MSBoolean MSStringEntryField::validate(const char *pString_)
{ return (MSView::model()!=0)?((value().set(pString_)==MSError::MSSuccess)?MSTrue:MSFalse):MSTrue; }

const char *MSStringEntryField::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) buffer_=value();
  return buffer_.string();
}

void MSStringEntryField::updateData(void)
{
  if(MSView::model()==0) internalCouple(new MSString());
  MSEntryFieldPlus::updateData();
}

MSAttrValueList& MSStringEntryField::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("increment","",MSAttrValue::Callback);  
  avList_<<MSAttrValue("decrement","",MSAttrValue::Callback);  
  return MSEntryFieldPlus::get(avList_);
}

