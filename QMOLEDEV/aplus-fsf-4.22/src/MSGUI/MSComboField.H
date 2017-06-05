#ifndef MSComboFieldHEADER
#define MSComboFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSEntryFieldCombo.H>

class MSGUIExport MSComboField : public MSEntryFieldCombo
{
public:
  MSComboField(MSWidget *,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSComboField(MSWidget *,MSString&,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSComboField(void);

  void model(MSString&);

  MSString& viewModel(void)             { return *((MSString*)_model); }
  const MSString& viewModel(void) const { return *((MSString*)_model); }

  MSComboField& operator=(const char *);
  operator MSString&();
  operator const char *();

  void comboArrowColor(unsigned long);
  void comboArrowColor(const char *);
  unsigned long comboArrowColor(void);
  
  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

protected:
  void init(void);
  
  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);

  const char *editString(void);
  MSString& value(void);
  const MSString& value(void) const;
  void value(const char *);
};

inline MSString& MSComboField::value(void)
{ return (MSString&)*_model; }
inline const MSString& MSComboField::value(void) const
{ return (MSString&)*_model; }

inline MSComboField& MSComboField::operator=(const char *value_) 
{ value(value_); return *this; }
inline MSComboField::operator MSString&()                      
{ return *((MSString *)_model); } 
inline MSComboField::operator const char *()                   
{ return (MSView::model()!=0)?(const char *)value():0; }

#endif



