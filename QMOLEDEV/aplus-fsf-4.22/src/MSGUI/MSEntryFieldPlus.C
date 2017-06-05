///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSEntryFieldPlus.H>

#include <MSGUI/MSArrow.H>

const MSSymbol MSEntryFieldPlus::buttonactivate = MSSymbol("buttonactivate");

// ##################################################################
// MSEntryFieldPlus
// ##################################################################
MSEntryFieldPlus::MSEntryFieldPlus(MSWidget *owner_,const MSSymbol& tag_):
MSEntryFieldCombo(owner_,tag_)
{ init(); }

MSEntryFieldPlus::MSEntryFieldPlus(MSWidget *owner_,const char *label_,const MSSymbol& tag_):
MSEntryFieldCombo(owner_,label_,tag_)
{ init(); }

void MSEntryFieldPlus::init(void)
{}

MSEntryFieldPlus::~MSEntryFieldPlus(void)
{}

void MSEntryFieldPlus::arrowButtons(MSBoolean arrowButtons_)
{  buttonState(UpDownArrows, arrowButtons_); }

MSBoolean MSEntryFieldPlus::arrowButtons(void) const
{ return buttonState(UpDownArrows); }

void MSEntryFieldPlus::textButtonActivate(void)
{ activateCallback(MSEntryFieldPlus::buttonactivate); }

void MSEntryFieldPlus::buttonLabel(const MSString& label_)
{ comboButtonLabel(label_); }

const MSString& MSEntryFieldPlus::buttonLabel(void) const
{ return comboButtonLabel(); }

void MSEntryFieldPlus::arrowColor(const char * color_)
{ buttonColor(UpDownArrows, color_); }

void MSEntryFieldPlus::arrowColor(unsigned long color_)
{ buttonColor(UpDownArrows, color_); }

unsigned long  MSEntryFieldPlus::arrowColor(void)
{ return buttonColor(UpDownArrows); }


void MSEntryFieldPlus::set(MSAttrValueList& avList_)
{
  MSEntryFieldCombo::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="arrowButtons")
        arrowButtons(avList_[i].value().asBoolean()),index<<i;     
     else if (avList_[i].attribute()=="buttonLabel")
        buttonLabel(avList_[i].value()),index<<i;     
     else if (avList_[i].attribute()=="arrowColor")
        arrowColor(avList_[i].value()),index<<i;     
   }
  avList_.remove(index);
}

MSAttrValueList& MSEntryFieldPlus::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("arrowButtons",
		       (arrowButtons()==MSTrue)?"MSTrue":"MSFalse",
		       MSStringVector("MSFalse\nMSTrue"));
  avList_<<MSAttrValue("arrowColor", server()->colorName(arrowColor()), MSAttrValue::Color);
  avList_<<MSAttrValue("buttonLabel", buttonLabel(), MSAttrValue::String);
  avList_<<MSAttrValue("buttonactivate", "", MSAttrValue::Callback);
  return MSEntryFieldCombo::get(avList_);
}

