///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSComboField.H>


MSComboField::MSComboField(MSWidget *owner_,const char *label_,const MSSymbol& tag_) :
MSEntryFieldCombo(owner_,label_,tag_) 
{ internalCouple(new MSString); init(); }
MSComboField::MSComboField(MSWidget *owner_,MSString& model_,const char *label_,const MSSymbol& tag_) :
MSEntryFieldCombo(owner_,label_,tag_) 
{ model(model_); init(); }

MSComboField::~MSComboField(void) 
{}

void MSComboField::model(MSString& model_)
{ couple(&model_); }
void MSComboField::value(const char *pString_) 
{ if (MSView::model()!=0) value()=pString_; }
MSBoolean MSComboField::validate(const char *pString_)
{ return (MSView::model()!=0)?((value().set(pString_)==MSError::MSSuccess)?MSTrue:MSFalse):MSTrue; }
const char *MSComboField::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) buffer_=value();
  return buffer_.string();
}

void MSComboField::init(void)
{ buttonState(ComboButton, MSTrue); }

const char *MSComboField::editString(void)
{ return fieldEditor()->string(); }

void MSComboField::comboArrowColor(const char * color_)
{ buttonColor(ComboButton, color_); }

void MSComboField::comboArrowColor(unsigned long color_)
{ buttonColor(ComboButton, color_); }

unsigned long MSComboField::comboArrowColor(void)
{ return buttonColor(ComboButton); }

void MSComboField::set(MSAttrValueList& avList_)
{
  MSEntryFieldCombo::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="comboArrowColor")
        comboArrowColor(avList_[i].value()),index<<i;     
   }
  avList_.remove(index);
}

MSAttrValueList& MSComboField::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("comboArrowColor", server()->colorName(comboArrowColor()),
                       MSAttrValue::Color);
  return MSEntryFieldCombo::get(avList_);
}
