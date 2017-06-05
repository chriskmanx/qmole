#ifndef MSUnsignedEntryFieldHEADER
#define MSUnsignedEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSUnsigned.H>
#include <MSGUI/MSTypeEntryField.H>

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSTypeEntryField<MSUnsigned>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSGUIExport MSTypeEntryField<MSUnsigned>;
#endif

class MSGUIExport MSUnsignedEntryField : public MSTypeEntryField<MSUnsigned>
{
protected:
  void init(void);

public:
  MSUnsignedEntryField(MSWidget *,const char *label_=0,
		  const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSUnsignedEntryField(MSWidget *,MSUnsigned&,const char *label_=0,
		  const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSUnsignedEntryField(void);

  void increment(void);
  void decrement(void);

  MSUnsignedEntryField& operator=(unsigned value_)          {value(value_); return *this;}
  MSUnsignedEntryField& operator=(const MSUnsigned& value_) {value(value_); return *this;}
  operator MSUnsigned&()                                    {return *((MSUnsigned *)_model);} 
  operator unsigned()                                       {return (unsigned)value();}        
};

#endif
