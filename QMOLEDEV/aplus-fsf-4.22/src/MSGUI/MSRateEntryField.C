///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSRateEntryField.H>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSGUI/MSTypeEntryField.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeEntryField<MSRate>
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeEntryField<MSRate>)
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeEntryField<MSRate>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeEntryField<MSRate>;
#endif

#endif // MSTK_MANUAL_INSTANTIATION


MSRateEntryField::MSRateEntryField(MSWidget *owner_,
				   const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSRate>(owner_,label_,tag_) 
{
  init();
}

MSRateEntryField::MSRateEntryField(MSWidget *owner_,MSRate& model_,
				   const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSRate>(owner_,model_,label_,tag_) 
{
  init();
}

MSRateEntryField::~MSRateEntryField(void)
{}

void MSRateEntryField::init(void)
{
  _format=MSFormat(MSRate::Percent2);
  _incrementValue=0.01;
}

void MSRateEntryField::set(MSAttrValueList& avList_)
{
  MSTypeEntryField<MSRate>::set(avList_);
}

MSAttrValueList& MSRateEntryField::get(MSAttrValueList& avList_)
{
  //This is replacement to the get method in MSTypeEntryField as
  //code there is too generic and resulting code is incorrect

  MSString buf;
  MSFloat value;
  
  value=(double)_incrementValue;
  value.format(buf,MSFloat::Decimal4);
  avList_<<MSAttrValue("incrementValue",buf);

  if (_minimumValue.isSet()==MSTrue)
   {
     value=(double)_minimumValue;
     value.format(buf,MSFloat::Decimal4);
     avList_<<MSAttrValue("minimumValue",buf);
   }
  else avList_<<MSAttrValue("minimumValue","");
  if (_maximumValue.isSet()==MSTrue)
   {
     value=(double)_maximumValue;
     value.format(buf,MSFloat::Decimal4);
     avList_<<MSAttrValue("maximumValue",buf);
   }
  else avList_<<MSAttrValue("maximumValue","");

  return MSEntryFieldPlus::get(avList_);
}








