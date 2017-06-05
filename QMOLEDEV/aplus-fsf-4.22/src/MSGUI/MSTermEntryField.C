///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTermEntryField.H>

MSTermEntryField::MSTermEntryField(MSWidget *owner_,
				   const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  internalCouple(new MSTerm());
  init();
}

MSTermEntryField::MSTermEntryField(MSWidget *owner_,MSTerm& model_,
				   const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  model(model_);
  init();
}

MSTermEntryField::~MSTermEntryField(void)
{}

void MSTermEntryField::init(void)
{
  _format=MSFormat(MSTerm::YearMonthDayPad);
  _incrementValue=MSTerm(0,0,1);
}

void MSTermEntryField::model(MSTerm& model_)
{ couple(&model_); }

void MSTermEntryField::model(const MSTerm& model_)
{ constCouple(&model_); }

void MSTermEntryField::updateData(void)
{
  if (MSView::model()==0) internalCouple(new MSTerm());
  MSEntryFieldPlus::updateData();
}

MSBoolean MSTermEntryField::validate(const char *pString_)
{
  if (MSView::model()!=0)
   {
     MSTerm aTerm;
     if (aTerm.set(pString_)==MSError::MSSuccess)
      {
	if (minimumValue().isSet()==MSTrue&&maximumValue().isSet()==MSTrue)
	 {
	   if (aTerm>=minimumValue()&&aTerm<=maximumValue())
	    {
	      value()=aTerm;
	      return MSTrue;
	    }
	 }
	else if (minimumValue().isSet()==MSTrue)
	 {
	   if (aTerm>=minimumValue())
	    {
	      value()=aTerm;
	      return MSTrue;
	    }
	 }
	else if (maximumValue().isSet()==MSTrue)
	 {
	   if (aTerm<=maximumValue())
	    {
	      value()=aTerm;
	      return MSTrue;
	    }
	 }
	else
	 {
	   value()=aTerm;
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

const char *MSTermEntryField::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) value().format(buffer_,format());
  return buffer_.string();
}

void MSTermEntryField::increment(void)
{
  if (MSView::model()!=0)
   {
     if (maximumValue().isSet()==MSTrue)
      {
	MSTerm aTerm=value();
	aTerm+=incrementValue();
	if (aTerm<=maximumValue())
	 {
	   value()=aTerm;
	   valueChange();
	 }
      }
     else
      {
	value()+=incrementValue();
	valueChange();
      }
   }
}

void MSTermEntryField::generateInputMask(void)
{
  MSString mask;
  switch(format().termFormat())
   {
   case MSTerm::YearsMonthsDaysPad:   mask = "--y---m---d";    break; 
   default:                    mask =""; break;
   }

  if(inputMaskCharacter()!='-' && mask != "") mask.change('-', inputMaskCharacter());
  fieldEditor()->inputMask(mask);
}

void MSTermEntryField::set(MSAttrValueList& avList_)
{
  MSEntryFieldPlus::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     const MSString& attr=avList_[i].attribute();
     const MSString& aValue=avList_[i].value();
     
     if (attr=="incrementValue")
      {
	MSTerm aTerm;
	if (aTerm.set(aValue)==MSError::MSSuccess)
	 {
	   incrementValue(aTerm),index<<i;
	 }
      }
     else if (attr=="minimumValue")
      {
	MSTerm aTerm;	   
	if (aTerm.set(aValue)==MSError::MSSuccess)
	 {
	   if (aValue=="") _minimumValue.unset();
	   else _minimumValue=aTerm;
	   index<<i;
	 }
      }
     else if (attr=="maximumValue")
      {
	MSTerm aTerm;
	if (aTerm.set(aValue)==MSError::MSSuccess)
	 {
	   if (aValue=="") _maximumValue.unset();
	   else _maximumValue=aTerm;
	   index<<i;
	 }
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSTermEntryField::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("incrementValue",_incrementValue.asString(),MSAttrValue::String);
  if (_minimumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("minimumValue",_minimumValue.asString(),MSAttrValue::String);
   }
  else avList_<<MSAttrValue("minimumValue","");
  if (_maximumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("maximumValue",_maximumValue.asString(),MSAttrValue::String);
   }
  else avList_<<MSAttrValue("maximumValue","");
  return MSEntryFieldPlus::get(avList_);
}


