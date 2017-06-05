#ifndef MSRateEntryFieldHEADER
#define MSRateEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSRate.H>
#include <MSGUI/MSTypeEntryField.H>

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSTypeEntryField<MSRate>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSGUIExport MSTypeEntryField<MSRate>;
#endif

class MSGUIExport MSRateEntryField : public MSTypeEntryField<MSRate>
{
protected:
  void init(void);
public:
  MSRateEntryField(MSWidget *,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSRateEntryField(MSWidget *,MSRate&,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSRateEntryField(void);

  MSRateEntryField& operator=(const MSRate& value_) {value(value_); return *this;}
  operator MSRate&()                                {return value();} 

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

};

#endif






