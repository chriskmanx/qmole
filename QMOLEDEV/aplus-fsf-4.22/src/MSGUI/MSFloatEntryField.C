///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFloatEntryField.H>

#include <float.h>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSGUI/MSTypeEntryField.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeEntryField<MSFloat>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeEntryField<MSFloat>
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeEntryField<MSFloat>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeEntryField<MSFloat>;
#endif

#endif //MSTK_MANUAL_INSTANTIATION

MSFloatEntryField::MSFloatEntryField(MSWidget *owner_,
				     const char *label_,const MSSymbol& tag_) : 
MSTypeEntryField<MSFloat>(owner_,label_,tag_) 
{
  init();
}

MSFloatEntryField::MSFloatEntryField(MSWidget *owner_,MSFloat& model_,
				     const char *label_,const MSSymbol& tag_) : 
MSTypeEntryField<MSFloat>(owner_,model_,label_,tag_) 
{
  init();
}

MSFloatEntryField::~MSFloatEntryField(void)
{}

void MSFloatEntryField::init(void)
{
  _format=MSFormat(MSFloat::Decimal2),
  _incrementValue=1.0;
  _clipMode=MSClipIndicator;
}


void MSFloatEntryField::increment(void)
{
  if (MSView::model()!=0)
   {
     if (value() <= DBL_MAX - incrementValue())   // prevent overflow
      {
       if (maximumValue().isSet()==MSTrue)
        {
          double anFloat=value();
          anFloat+=incrementValue();
          if (anFloat<=maximumValue())
           {
             value()=anFloat;
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

void MSFloatEntryField::decrement(void)
{
  if (MSView::model()!=0)
   {
     if (value() >= -DBL_MAX + incrementValue())   // prevent underflow
      {
       if (minimumValue().isSet()==MSTrue)
        {
          double anFloat=value();
          anFloat-=incrementValue();
          if (anFloat>=minimumValue())
           {
             value()=anFloat;
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

