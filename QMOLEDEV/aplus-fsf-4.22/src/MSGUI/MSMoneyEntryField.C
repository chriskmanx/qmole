///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMoneyEntryField.H>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSGUI/MSTypeEntryField.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeEntryField<MSMoney>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeEntryField<MSMoney>
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeEntryField<MSMoney>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeEntryField<MSMoney>;
#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSMoneyEntryField::MSMoneyEntryField(MSWidget *owner_,
				     const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSMoney>(owner_,label_,tag_) 
{
  init();
}

MSMoneyEntryField::MSMoneyEntryField(MSWidget *owner_,MSMoney& model_,
				     const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSMoney>(owner_,model_,label_,tag_) 
{
  init();
}

MSMoneyEntryField::~MSMoneyEntryField(void)
{}

void MSMoneyEntryField::init(void)
{
  _format=MSFormat(MSMoney::CurrencyPrecision);
  _symbolType=MSMoney::LocalSymbol;
  _symbolLocation=MSMoney::DefaultLocation;
  _incrementValue=1.0;
  _clipMode=MSClipIndicator;
}

const char *MSMoneyEntryField::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) value().format(buffer_,format().moneyFormat(),symbolType(),symbolLocation());
  return buffer_.string();
}


void MSMoneyEntryField::symbolType(MSMoney::SymbolType symbolType_)
{
  if(_symbolType!=symbolType_)
   {
     _symbolType=symbolType_;
     drawFieldValue();
   }
}

void MSMoneyEntryField::symbolLocation(MSMoney::SymbolLocation symbolLocation_)
{
  if(_symbolLocation!=symbolLocation_)
   {
     _symbolLocation=symbolLocation_;
     drawFieldValue();
   }
}

void MSMoneyEntryField::set(MSAttrValueList& avList_)
{
  MSTypeEntryField<MSMoney>::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
  {
    if(avList_[i].attribute()=="symbolType")
     {
       const MSString& value=avList_[i].value();
       if(value=="MSMoney::ISOSymbol") symbolType(MSMoney::ISOSymbol);
       else if(value=="MSMoney::NoCurrencySymbol") symbolType(MSMoney::NoCurrencySymbol);
       else symbolType(MSMoney::LocalSymbol);
       index<<i;
     }
    else if(avList_[i].attribute()=="symbolLocation")
     {
       const MSString& value=avList_[i].value();
       if(value=="MSMoney::SymbolAtStart") symbolLocation(MSMoney::SymbolAtStart);
       else if(value=="MSMoney::SymbolAtEnd") symbolLocation(MSMoney::SymbolAtEnd);
       else symbolLocation(MSMoney::DefaultLocation);
       index<<i;
     }
  }
  avList_.remove(index);
}

MSAttrValueList& MSMoneyEntryField::get(MSAttrValueList& avList_)
{
  //This is replacement to the get method in MSTypeEntryField as
  //code there is too generic and resulting code is incorrect
  MSString buf;
  MSStringVector sv("MSMoney::ISOSymbol\nMSMoney::LocalSymbol\nMSMoney::NoCurrencySymbol");
  avList_<<MSAttrValue("symbolType",sv((int)symbolType()),sv);
  
  sv="MSMoney::SymbolAtStart\nMSMoney::SymbolAtEnd\nMSMoney::DefaultLocation";
  avList_<<MSAttrValue("symbolLocation",sv((int)symbolLocation()),sv);

  _incrementValue.format(buf,MSMoney::CurrencyPrecision);
  avList_<<MSAttrValue("incrementValue",buf);

  if (_minimumValue.isSet()==MSTrue)
   {
     _minimumValue.format(buf,MSMoney::CurrencyPrecision);
     avList_<<MSAttrValue("minimumValue",buf);
   }
  else avList_<<MSAttrValue("minimumValue","");
  if (_maximumValue.isSet()==MSTrue)
   {
     _maximumValue.format(buf,MSMoney::CurrencyPrecision);
     avList_<<MSAttrValue("maximumValue",buf);
   }
  else avList_<<MSAttrValue("maximumValue","");

  return MSEntryFieldPlus::get(avList_);
}




