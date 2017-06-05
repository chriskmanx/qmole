///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSUnsignedEntryField.H>
#include <MSTypes/MSInt.H>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSGUI/MSTypeEntryField.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeEntryField<MSUnsigned>
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeEntryField<MSUnsigned>)
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeEntryField<MSUnsigned>;
#endif


#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeEntryField<MSUnsigned>;
#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSUnsignedEntryField::MSUnsignedEntryField(MSWidget *owner_,
				 const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSUnsigned>(owner_,label_,tag_) 
{
  init();
}

MSUnsignedEntryField::MSUnsignedEntryField(MSWidget *owner_,MSUnsigned& model_,
				 const char *label_,const MSSymbol& tag_) :
MSTypeEntryField<MSUnsigned>(owner_,model_,label_,tag_) 
{
  init();
}

MSUnsignedEntryField::~MSUnsignedEntryField(void)
{}

void MSUnsignedEntryField::init(void)
{
  _format=MSFormat(MSInt::WithoutCommas);
  _incrementValue=1;
  _clipMode=MSClipIndicator;
  minimumValue( MSUnsigned(0) );
}

void MSUnsignedEntryField::increment(void)
{
  if (MSView::model()!=0)
   {
     if ((unsigned)value() <= UINT_MAX - (unsigned)incrementValue())  // prevent overflow
      {
       if (maximumValue().isSet()==MSTrue)
        {
          unsigned anUnsigned=value();
          anUnsigned+=(unsigned)incrementValue();
          if (anUnsigned<=maximumValue())
           {
             value()=anUnsigned;
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

void MSUnsignedEntryField::decrement(void)
{
  if (MSView::model()!=0)
   {
     if (value() >= incrementValue())   // prevent underflow
      {
       if (minimumValue().isSet()==MSTrue)
        {
          unsigned anUnsigned=value();
          anUnsigned-=(unsigned)incrementValue();
          if (anUnsigned>=minimumValue())
           {
             value()=anUnsigned;
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

