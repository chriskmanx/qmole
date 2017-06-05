#ifndef MSEntryFieldPlusHEADER
#define MSEntryFieldPlusHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSEntryFieldCombo.H>
#include <MSTypes/MSSymbol.H>

class MSGUIExport MSEntryFieldPlus : public MSEntryFieldCombo
{
public:
  static const MSSymbol buttonactivate;

  MSEntryFieldPlus(MSWidget *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSEntryFieldPlus(MSWidget *,const char *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSEntryFieldPlus(void);

  void arrowButtons(MSBoolean);
  MSBoolean arrowButtons(void) const;

  void arrowColor(unsigned long);
  void arrowColor(const char *);
  unsigned long arrowColor(void);
  
  const MSString& buttonLabel(void) const;
  void buttonLabel(const MSString&);
  
  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
  
protected:
  void init(void);

  virtual void textButtonActivate(void);
};


#endif






