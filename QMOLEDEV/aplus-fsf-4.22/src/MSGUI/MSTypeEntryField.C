#ifndef MSTypeEntryFieldIMPLEMENTATION
#define MSTypeEntryFieldIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTypeEntryField.H>

template<class Type>
MSTypeEntryField<Type>::
MSTypeEntryField(MSWidget *owner_,const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  _minimumValue.unset();
  _maximumValue.unset();  
  internalCouple(new Type());
}

template<class Type>
MSTypeEntryField<Type>::
MSTypeEntryField(MSWidget *owner_,Type& model_,const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  _minimumValue.unset();
  _maximumValue.unset();  
  model(model_);
}

template<class Type>
MSTypeEntryField<Type>::~MSTypeEntryField(void)
{}

template<class Type>
void MSTypeEntryField<Type>::model(Type& model_)
{ couple(&model_); }

template<class Type>
void MSTypeEntryField<Type>::model(const Type& model_)
{ constCouple(&model_); }

template<class Type>
void MSTypeEntryField<Type>::updateData(void)
{
  if(MSView::model() == 0) internalCouple(new Type());
  MSEntryFieldPlus::updateData();
}

template<class Type>
const char *MSTypeEntryField<Type>::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) value().format(buffer_,format());
  return buffer_.string();
}

template<class Type>
MSBoolean MSTypeEntryField<Type>::validate(const char *pString_)
{
  if (MSView::model()!=0)
   {
     Type aType;
     if (aType.set(pString_)==MSError::MSSuccess)
      {
	if (minimumValue().isSet()==MSTrue&&maximumValue().isSet()==MSTrue)
	 {
	   if (aType>=minimumValue()&&aType<=maximumValue())
	    {
	      value()=aType;
	      return MSTrue;
	    }
	 }
	else if (minimumValue().isSet()==MSTrue)
	 {
	   if (aType>=minimumValue())
	    {
	      value()=aType;
	      return MSTrue;
	    }
	 }
	else if (maximumValue().isSet()==MSTrue)
	 {
	   if (aType<=maximumValue())
	    {
	      value()=aType;
	      return MSTrue;
	    }
	 }
	else
	 {
	   value()=aType;
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

template<class Type>
void MSTypeEntryField<Type>::increment(void)
{
  if (MSView::model()!=0)
   {
     if (maximumValue().isSet()==MSTrue)
      {
	Type aType=value();
	aType+=incrementValue();
	if (aType<=maximumValue())
	 {
	   value()=aType;
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

template<class Type>
void MSTypeEntryField<Type>::decrement(void)
{
  if (MSView::model()!=0)
   {
     if (minimumValue().isSet()==MSTrue)
      {
	Type aType=value();
	aType-=incrementValue();
	if (aType>=minimumValue())
	 {
	   value()=aType;
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

template<class Type>
void MSTypeEntryField<Type>::set(MSAttrValueList& avList_)
{
  MSEntryFieldPlus::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     const MSString& attr=avList_[i].attribute();
     const MSString& aValue=avList_[i].value();
     
     if (attr=="incrementValue")
      {
	Type aType;
	if (aType.set(aValue)==MSError::MSSuccess)
	 {
	   incrementValue(aType),index<<i;
	 }
      }
     else if (attr=="minimumValue")
      {
	Type aType;	   
        if (aValue.length()==0)
         {
           _minimumValue.unset();
         }
        else if (aType.set(aValue)==MSError::MSSuccess)
	 {
	   _minimumValue=aType;
	 }
        index<<i;
      }
     else if (attr=="maximumValue")
      {
	Type aType;
        if (aValue.length()==0)
         {
           _maximumValue.unset();
         }
        else if (aType.set(aValue)==MSError::MSSuccess)
	 {
	   _maximumValue=aType;
	 }
        index<<i;
      }
   }
  avList_.remove(index);
}

template<class Type>
MSAttrValueList& MSTypeEntryField<Type>::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("incrementValue",_incrementValue.asString());
  if (_minimumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("minimumValue",_minimumValue.asString());
   }
  else avList_<<MSAttrValue("minimumValue","");
  if (_maximumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("maximumValue",_maximumValue.asString());
   }
  else avList_<<MSAttrValue("maximumValue","");
  return MSEntryFieldPlus::get(avList_);
}

#endif





