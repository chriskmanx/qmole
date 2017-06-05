///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIntEntryField.H>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSGUI/MSTypeEntryField.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeEntryField<MSInt>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeEntryField<MSInt>
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeEntryField<MSInt>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeEntryField<MSInt>;
#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSIntEntryField::MSIntEntryField(MSWidget *owner_,
				 const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSInt>(owner_,label_,tag_) 
{
  init();
}

MSIntEntryField::MSIntEntryField(MSWidget *owner_,MSInt& model_,
				 const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSInt>(owner_,model_,label_,tag_) 
{
  init();
}

MSIntEntryField::~MSIntEntryField(void)
{}

void MSIntEntryField::init(void)
{
  _format=MSFormat(MSInt::WithoutCommas);
  _incrementValue=1;
  _clipMode=MSClipIndicator;
}

void MSIntEntryField::increment(void)
{
  if (MSView::model()!=0)
   {
     if (value() <= INT_MAX - incrementValue())   // prevent overflow
      {
       if (maximumValue().isSet()==MSTrue)
        {
          int anInt=value();
          anInt+=(int)incrementValue();
          if (anInt<=maximumValue())
           {
             value()=anInt;
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
}

void MSIntEntryField::decrement(void)
{
  if (MSView::model()!=0)
   {
     if (value() >= INT_MIN + incrementValue())   // prevent underflow
      {
       if (minimumValue().isSet()==MSTrue)
        {
          int anInt=value();
          anInt-=(int)incrementValue();
          if (anInt>=minimumValue())
           {
             value()=anInt;
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
}

