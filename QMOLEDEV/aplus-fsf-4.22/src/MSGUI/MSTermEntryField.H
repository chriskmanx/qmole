#ifndef MSTermEntryFieldHEADER
#define MSTermEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSTerm.H>
#include <MSGUI/MSEntryFieldPlus.H>

class MSGUIExport MSTermEntryField : public MSEntryFieldPlus
{
protected:
  MSTerm _incrementValue;
  MSTerm _minimumValue;
  MSTerm _maximumValue;

  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);
  virtual void increment(void);
  
  void init(void);
  
  MSTerm& value(void) const { return *((MSTerm *)_model); } 
  void value(const MSTerm& value_) const { if (MSView::model()!=0) value()=value_; }

  virtual void generateInputMask(void);

  virtual void updateData(void);
  
public:
  MSTermEntryField(MSWidget *,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSTermEntryField(MSWidget *,MSTerm&,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSTermEntryField(void);

  void model(MSTerm&);
  void model(const MSTerm&);

  MSTerm& viewModel(void)             { return *((MSTerm*)_model); }
  const MSTerm& viewModel(void) const { return *((MSTerm*)_model); }

  const MSTerm& incrementValue(void) const {return _incrementValue;}
  const MSTerm& maximumValue(void)   const {return _maximumValue;}
  const MSTerm& minimumValue(void)   const {return _minimumValue;}

  void incrementValue(const MSTerm& incrementValue_) {_incrementValue=incrementValue_;}
  void maximumValue(const MSTerm& maximumValue_)     {_maximumValue=maximumValue_;}
  void minimumValue(const MSTerm& minimumValue_)     {_minimumValue=minimumValue_;}
  
  MSTermEntryField& operator=(const MSTerm& value_) {value(value_); return *this;}
  operator MSTerm&()                                {return value();}

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
};

#endif






