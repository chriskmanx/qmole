#ifndef MSTimeEntryFieldHEADER
#define MSTimeEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSEntryFieldPlus.H>
#include <MSTypes/MSTime.H>

class MSGUIExport MSTimeEntryField : public MSEntryFieldPlus
{
private:
  long   _incrementValue;
  MSTime _minimumValue;
  MSTime _maximumValue;
  MSTime::MSTimeZone _timeZone;

protected:
  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);
  virtual void increment(void);
  virtual void decrement(void);

  MSTime& value(void) const { return *((MSTime *)_model); } 
  void value(const MSTime& value_) const { if (MSView::model()!=0) value()=value_; }

  void init(void);
  virtual void generateInputMask(void);

  virtual void updateData(void);
  
public:
  MSTimeEntryField(MSWidget *,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSTimeEntryField(MSWidget *,MSTime&,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSTimeEntryField(void);

  void model(MSTime&);
  void model(const MSTime&);

  MSTime& viewModel(void)             { return *((MSTime*)_model); }
  const MSTime& viewModel(void) const { return *((MSTime*)_model); }

  long incrementValue(void)          const {return _incrementValue;}
  const MSTime& maximumValue(void)   const {return _maximumValue;}
  const MSTime& minimumValue(void)   const {return _minimumValue;}
  
  void incrementValue(long incrementValue_)      {_incrementValue=incrementValue_;}
  void maximumValue(const MSTime& maximumValue_) {_maximumValue=maximumValue_;}
  void minimumValue(const MSTime& minimumValue_) {_minimumValue=minimumValue_;}

  MSTimeEntryField& operator=(const MSTime& value_) {value(value_); return *this;}
  operator MSTime&()                                {return value();}

  void timeZone(MSTime::MSTimeZone);
  MSTime::MSTimeZone timeZone(void) const {return _timeZone;}

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
};

#endif






