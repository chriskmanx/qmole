#ifndef MSBoolEntryFieldHEADER
#define MSBoolEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSEntryFieldPlus.H>
#include <MSTypes/MSBool.H>

class MSGUIExport MSBoolEntryField : public MSEntryFieldPlus
{
protected:
  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);
  virtual void increment(void);
  virtual void decrement(void);
  
  MSBool& getBool(void)            const {return *((MSBool *)_model);} 
  void setBool(const MSBool& bool_) const {if (MSView::model()!=0) getBool()=bool_;}

  void init(void);
  virtual void updateData(void);
public:
  MSBoolEntryField(MSWidget *,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSBoolEntryField(MSWidget *,MSBool&,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSBoolEntryField(void);

  void model(MSBool&);
  void model(const MSBool&);

  MSBool& viewModel(void)             { return *((MSBool*)_model); }
  const MSBool& viewModel(void) const { return *((MSBool*)_model); }

  MSBoolEntryField& operator=(const MSBool& bool_) {setBool(bool_); return *this;}
  operator MSBool&()                               {return getBool();}
};


#endif
