#ifndef MSFormatHEADER
#define MSFormatHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSBool.H>
#include <MSTypes/MSDate.H>
#include <MSTypes/MSFloat.H>
#include <MSTypes/MSInt.H>
#include <MSTypes/MSMoney.H>
#include <MSTypes/MSRate.H>
#include <MSTypes/MSTerm.H>
#include <MSTypes/MSTime.H>

template <class Type> class MSTypeVector;
typedef MSTypeVector<MSString> MSStringVector;

class MSHashTable;

class MSTypesExport MSFormat
{
public:

  enum MSFormatType {NoFormat=0,FBool,Date,Float,Int,Money,Rate,Term,Time};
  enum MSFormatModifier {NoModifier=0,UpperCaseK=1,LowerCaseK=2,
			 UpperCaseM=4,LowerCaseM=8,Parenthesis=16};

  MSFormat(void);
  MSFormat(const MSFormat&);
  MSFormat(const char *,const char *modifierString_="NoModifier");
  MSFormat(MSBool::MSBoolFormat,unsigned long formatModifier_=NoModifier);
  MSFormat(MSDate::MSDateFormat,unsigned long formatModifier_=NoModifier);
  MSFormat(MSFloat::MSFloatFormat,unsigned long formatModifier_=NoModifier);
  MSFormat(MSInt::MSIntFormat,unsigned long formatModifier_=NoModifier);  
  MSFormat(MSMoney::MSMoneyFormat,unsigned long formatModifier_=NoModifier);
  MSFormat(MSRate::MSRateFormat,unsigned long formatModifier_=NoModifier);
  MSFormat(MSTerm::MSTermFormat,unsigned long formatModifier_=NoModifier);
  MSFormat(MSTime::MSTimeFormat,unsigned long formatModifier_=NoModifier);
  ~MSFormat(void);

  MSFormat& operator=(const MSFormat&);

  MSBoolean operator==(const MSFormat&) const;
  MSBoolean operator!=(const MSFormat&) const;
  MSBoolean operator==(MSBool::MSBoolFormat) const;
  MSBoolean operator==(MSDate::MSDateFormat) const;
  MSBoolean operator==(MSFloat::MSFloatFormat) const;
  MSBoolean operator==(MSInt::MSIntFormat) const;  
  MSBoolean operator==(MSMoney::MSMoneyFormat) const;
  MSBoolean operator==(MSRate::MSRateFormat) const;
  MSBoolean operator==(MSTerm::MSTermFormat) const;
  MSBoolean operator==(MSTime::MSTimeFormat) const;
  
  void format(const MSFormat&);  
  void format(MSBool::MSBoolFormat);
  void format(MSDate::MSDateFormat);
  void format(MSFloat::MSFloatFormat);
  void format(MSInt::MSIntFormat);
  void format(MSMoney::MSMoneyFormat);
  void format(MSRate::MSRateFormat);
  void format(MSTerm::MSTermFormat);
  void format(MSTime::MSTimeFormat);
  void formatModifier(unsigned long);

  MSFormatType formatType(void) const;
  unsigned long formatModifier(void) const;
  MSString asString(void) const;
  MSString asModifierString(void) const;
  const MSStringVector& formats(void) const;
  
  MSBool::MSBoolFormat boolFormat(void) const;
  MSDate::MSDateFormat dateFormat(void) const;
  MSFloat::MSFloatFormat floatFormat(void) const;
  MSInt::MSIntFormat intFormat(void) const;  
  MSMoney::MSMoneyFormat moneyFormat(void) const;
  MSRate::MSRateFormat rateFormat(void) const;
  MSTerm::MSTermFormat termFormat(void) const;
  MSTime::MSTimeFormat timeFormat(void) const;

  static const MSStringVector& boolFormats(void);
  static const MSStringVector& dateFormats(void);
  static const MSStringVector& floatFormats(void);
  static const MSStringVector& intFormats(void);  
  static const MSStringVector& moneyFormats(void);
  static const MSStringVector& rateFormats(void);
  static const MSStringVector& termFormats(void);
  static const MSStringVector& timeFormats(void);
  static const MSStringVector& noFormats(void);
  static const MSStringVector& modifiers(void);

private:
  MSFormatType          _formatType;
  unsigned long         _formatModifier;
  union 
   {
     int                    _any;
     MSBool::MSBoolFormat   _bool;
     MSDate::MSDateFormat   _date;
     MSFloat::MSFloatFormat _float;
     MSInt::MSIntFormat     _int;     
     MSMoney::MSMoneyFormat _money;
     MSRate::MSRateFormat   _rate;
     MSTerm::MSTermFormat   _term;
     MSTime::MSTimeFormat   _time;
   } _format;

  static MSHashTable *initFormatHashTable(void);
  static MSHashTable *initEnumHashTable(void);
  static MSHashTable *initModifierHashTable(void);

  static const MSHashTable *formatHashTable(void);
  static const MSHashTable *enumHashTable(void);
  static const MSHashTable *modifierHashTable(void);
};

#ifndef MS_NO_INLINES
#ifndef MSFormatINLINES
#include <MSTypes/MSFormatInlines.C>
#endif
#endif

#endif
