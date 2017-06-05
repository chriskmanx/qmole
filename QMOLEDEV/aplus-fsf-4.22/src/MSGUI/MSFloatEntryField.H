#ifndef MSFloatEntryFieldHEADER
#define MSFloatEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSFloat.H>
#include <MSGUI/MSTypeEntryField.H>

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSTypeEntryField<MSFloat>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSGUIExport MSTypeEntryField<MSFloat>;
#endif

class MSGUIExport MSFloatEntryField : public MSTypeEntryField<MSFloat>
{
protected:
  void init(void);

public:
  MSFloatEntryField(MSWidget *,const char *label_=0,
		    const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatEntryField(MSWidget *,MSFloat&,const char *label_=0,
		    const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSFloatEntryField(void);

  void increment(void);
  void decrement(void);

  MSFloatEntryField& operator=(double value_)         {value(value_); return *this;}
  operator MSFloat&()                                 {return *((MSFloat *)_model);} 
  operator double()                                   {return (double)value();}          
};

#endif






