#ifndef MSMoneyEntryFieldHEADER
#define MSMoneyEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSMoney.H>
#include <MSGUI/MSTypeEntryField.H>

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSTypeEntryField<MSMoney>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSGUIExport MSTypeEntryField<MSMoney>;
#endif

class MSGUIExport MSMoneyEntryField : public MSTypeEntryField<MSMoney>
{
protected:
  MSMoney::SymbolType     _symbolType;
  MSMoney::SymbolLocation _symbolLocation;
  virtual const char *formatOutput(MSString &);
  void init();
public:
  MSMoneyEntryField(MSWidget *,const char *label_=0,
		    const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSMoneyEntryField(MSWidget *,MSMoney&,const char *label_=0,
		    const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSMoneyEntryField(void);

  MSMoneyEntryField& operator=(const MSMoney& money_) {value(money_); return *this;}
  operator MSMoney&()                                 {return value();} 

  MSMoney::SymbolType symbolType(void) const { return _symbolType;}
  void symbolType(MSMoney::SymbolType);

  MSMoney::SymbolLocation symbolLocation(void) const { return _symbolLocation;}
  void symbolLocation(MSMoney::SymbolLocation);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
};

#endif






