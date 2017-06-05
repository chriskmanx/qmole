#ifndef MSStringEntryFieldHEADER
#define MSStringEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSEntryFieldPlus.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSFormat.H>

class MSGUIExport MSStringEntryField : public MSEntryFieldPlus
{
protected:
  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);

  const char *svalue(void) const { return (MSView::model()!=0)?value().string():0; }
  MSString& value(void) const { return (MSString&)*_model; }
  void value(const char *);

  virtual void updateData(void);
  
public:
  MSStringEntryField(MSWidget *,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSStringEntryField(MSWidget *,MSString&,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSStringEntryField(void);

  void model(MSString&);
  void model(const MSString&);

  MSString& viewModel(void)             { return *((MSString*)_model); }
  const MSString& viewModel(void) const { return *((MSString*)_model); }

  MSStringEntryField& operator=(const char *value_) {value(value_); return *this;}
  operator MSString&()                              {return *((MSString *)_model);} 
  operator const char *()                           {return svalue();}           

  virtual MSAttrValueList& get(MSAttrValueList&);
  
};

#endif






