#ifndef MSIntEntryFieldHEADER
#define MSIntEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSInt.H>
#include <MSGUI/MSTypeEntryField.H>

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSTypeEntryField<MSInt>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSGUIExport MSTypeEntryField<MSInt>;
#endif

class MSGUIExport MSIntEntryField : public MSTypeEntryField<MSInt>
{
protected:
  void init(void);

public:
  MSIntEntryField(MSWidget *,const char *label_=0,
		  const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntEntryField(MSWidget *,MSInt&,const char *label_=0,
		  const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSIntEntryField(void);

  void increment(void);
  void decrement(void);

  MSIntEntryField& operator=(int value_)          {value(value_); return *this;}
  MSIntEntryField& operator=(const MSInt& value_) {value(value_); return *this;}
  operator MSInt&()                               {return *((MSInt *)_model);} 
  operator int()                                  {return (int)value();}        
};

#endif






