///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTimeEntryField.H>

MSTimeEntryField::MSTimeEntryField(MSWidget *owner_,
				   const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  internalCouple(new MSTime(MSTime::now()));
  init();
}

MSTimeEntryField::MSTimeEntryField(MSWidget *owner_,MSTime& model_,
				   const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  model(model_);
  init();
}

MSTimeEntryField::~MSTimeEntryField(void)
{}

void MSTimeEntryField::init(void)
{
  _minimumValue.unset();
  _maximumValue.unset();  
  _format=MSFormat(MSTime::HoursMinutesSecondsSlash4Zone);
  _incrementValue=1;
  _timeZone=MSTime::Local;
}

void MSTimeEntryField::model(MSTime& model_)
{ couple(&model_); }

void MSTimeEntryField::model(const MSTime& model_)
{ constCouple(&model_); }

void MSTimeEntryField::updateData(void)
{
  if (MSView::model()==0) internalCouple(new MSTime(MSTime::now()));
  MSEntryFieldPlus::updateData();
}

const char *MSTimeEntryField::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) value().format(buffer_,format(),timeZone());
  return buffer_.string();
}

MSBoolean MSTimeEntryField::validate(const char *pString_)
{
  if (MSView::model()!=0)
   {
     MSTime aTime;
     if (aTime.set(pString_,timeZone())==MSError::MSSuccess)
      {
	if (minimumValue().isSet()==MSTrue&&maximumValue().isSet()==MSTrue)
	 {
	   if (aTime>=minimumValue()&&aTime<=maximumValue())
	    {
	      value()=aTime;
	      return MSTrue;
	    }
	 }
	else if (minimumValue().isSet()==MSTrue)
	 {
	   if (aTime>=minimumValue())
	    {
	      value()=aTime;
	      return MSTrue;
	    }
	 }
	else if (maximumValue().isSet()==MSTrue)
	 {
	   if (aTime<=maximumValue())
	    {
	      value()=aTime;
	      return MSTrue;
	    }
	 }
	else
	 {
	   value()=aTime;
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

void MSTimeEntryField::increment(void)
{
  if (MSView::model()!=0)
   {
     if (maximumValue().isSet()==MSTrue)
      {
	MSTime aTime=value();
	aTime+=incrementValue();
	if (aTime<=maximumValue())
	 {
	   value()=aTime;
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

void MSTimeEntryField::decrement(void)
{
  if (MSView::model()!=0)
   {
     if (minimumValue().isSet()==MSTrue)
      {
	MSTime aTime=value();
	aTime-=incrementValue();
	if (aTime>=minimumValue())
	 {
	   value()=aTime;
	   valueChange();
	 }
      }
     else
      {
	value()-=incrementValue();
	valueChange();
      }
   }
}

void MSTimeEntryField::generateInputMask(void)
{
  MSString mask;
  
  switch(format().timeFormat())
   {
   case MSTime::HoursMinutesSeconds:  mask="--:--:--"; break;
   case MSTime::MonthDayYear:         mask="--/--/----"; break;
   case MSTime::HoursMinutesSecondsSlash:  mask="--:--:-- --/--/--"; break;
   case MSTime::HoursMinutesSecondsSlash4: mask="--:--:-- --/--/----"; break;
   default: mask=""; break;
   }
  if(inputMaskCharacter()!='-' && mask!="") mask.change('-',inputMaskCharacter());
  fieldEditor()->inputMask(mask);  
}

void MSTimeEntryField::timeZone(MSTime::MSTimeZone timeZone_)
{
  if (_timeZone!=timeZone_)
   {
     _timeZone=timeZone_;
     drawFieldValue();
   }
}

void MSTimeEntryField::set(MSAttrValueList& avList_)
{
  MSEntryFieldPlus::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     const MSString& attr=avList_[i].attribute();
     const MSString& aValue=avList_[i].value();
     
     if (attr=="incrementValue")
      {
	incrementValue(aValue.asInt()),index<<i;
      }
     else if (attr=="minimumValue")
      {
	MSTime aTime;	   
	if (aTime.set(aValue)==MSError::MSSuccess)
	 {
	   if (aValue=="") _minimumValue.unset();
	   else _minimumValue=aTime;
	   index<<i;
	 }
      }
     else if (attr=="maximumValue")
      {
	MSTime aTime;
	if (aTime.set(aValue)==MSError::MSSuccess)
	 {
	   if (aValue=="") _maximumValue.unset();
	   else _maximumValue=aTime;
	   index<<i;
	 }
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSTimeEntryField::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("incrementValue",MSString(_incrementValue));
  if (_minimumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("minimumValue",_minimumValue.asString(),MSAttrValue::String);
   }
  else avList_<<MSAttrValue("minimumValue","",MSAttrValue::String);
  if (_maximumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("maximumValue",_maximumValue.asString(),MSAttrValue::String);
   }
  else avList_<<MSAttrValue("maximumValue","",MSAttrValue::String);
  return MSEntryFieldPlus::get(avList_);
}

