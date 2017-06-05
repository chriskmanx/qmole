#ifndef MSTypeEntryFieldHEADER
#define MSTypeEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <MSGUI/MSEntryFieldPlus.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSTypeEntryField.C")
#endif

template<class Type>
class MSTypeEntryField : public MSEntryFieldPlus
{
protected:
  Type _incrementValue;
  Type _minimumValue;
  Type _maximumValue;

  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);
  virtual void increment(void);
  virtual void decrement(void);

  virtual void updateData(void);
  
  Type& value(void) const { return *((Type *)_model); } 
  void value(const Type& value_) const { if (MSView::model()!=0) value()=value_; }

public:
  MSTypeEntryField(MSWidget *,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSTypeEntryField(MSWidget *,Type&,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSTypeEntryField(void);

  void model(Type&);
  void model(const Type&);

  Type& viewModel(void)             { return *((Type*)_model); }
  const Type& viewModel(void) const { return *((Type*)_model); }

  const Type& incrementValue(void) const {return _incrementValue;}
  const Type& maximumValue(void)   const {return _maximumValue;}
  const Type& minimumValue(void)   const {return _minimumValue;}

  void incrementValue(const Type& incrementValue_) {_incrementValue=incrementValue_;}
  void maximumValue(const Type& maximumValue_)     {_maximumValue=maximumValue_;}
  void minimumValue(const Type& minimumValue_)     {_minimumValue=minimumValue_;}
  
  MSTypeEntryField<Type>& operator=(const Type& value_) {value(value_); return *this;}
  operator Type&()                                      {return value();}

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
};

#endif






