///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSFormat.H>
#include <MSTypes/MSStringVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSFormatInlines.C>
#endif

MSFormat::MSFormat(void) :
_formatType(MSFormat::NoFormat),_formatModifier(MSFormat::NoModifier)
{ _format._any=0; }
MSFormat::MSFormat(MSBool::MSBoolFormat format_,unsigned long formatModifier_)
{
  format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(MSDate::MSDateFormat format_,unsigned long formatModifier_)
{ format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(MSFloat::MSFloatFormat format_,unsigned long formatModifier_)
{
  format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(MSInt::MSIntFormat format_,unsigned long formatModifier_)
{
  format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(MSMoney::MSMoneyFormat format_,unsigned long formatModifier_)
{
  format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(MSRate::MSRateFormat format_,unsigned long formatModifier_)
{
  format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(MSTerm::MSTermFormat format_,unsigned long formatModifier_)
{
  format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(MSTime::MSTimeFormat format_,unsigned long formatModifier_)
{
  format(format_);
  formatModifier(formatModifier_);
}
MSFormat::MSFormat(const MSFormat& aFormat_)
{ format(aFormat_); }

MSFormat::MSFormat(const char *formatString_,const char *modifierString_)
{
  _format._any=0;
  if (formatString_!=0)
   {
     MSFormat *pFormat=(MSFormat *)formatHashTable()->lookup(formatString_);
     if (pFormat!=0) format(*pFormat);
     else _formatType=NoFormat;
   }
  else _formatType=NoFormat;
  _formatModifier=NoModifier;
  if (modifierString_!=0)
   {
     MSString modifierString(modifierString_);
     modifierString.change("|","\n");
     MSStringVector modifierVector(modifierString);
     for (unsigned i=0;i<modifierVector.length();i++)
      {
	_formatModifier|=(unsigned long)modifierHashTable()->lookup(modifierVector(i));
      }
   }
}

MSFormat::~MSFormat(void)
{}

MSFormat& MSFormat::operator=(const MSFormat& aFormat_)
{ 
  _formatType=aFormat_._formatType,_formatModifier=aFormat_._formatModifier,_format=aFormat_._format;
  return *this;
}

void MSFormat::format(const MSFormat& aFormat_)
{ _formatType=aFormat_._formatType,_formatModifier=aFormat_._formatModifier,_format=aFormat_._format; }
void MSFormat::format(MSBool::MSBoolFormat format_)
{ _formatType=FBool,_format._bool=format_;}
void MSFormat::format(MSDate::MSDateFormat format_)
{ _formatType=Date,_format._date=format_;}
void MSFormat::format(MSFloat::MSFloatFormat format_)
{ _formatType=Float,_format._float=format_;}
void MSFormat::format(MSInt::MSIntFormat format_)
{ _formatType=Int,_format._int=format_;}
void MSFormat::format(MSMoney::MSMoneyFormat format_)
{ _formatType=Money,_format._money=format_;}
void MSFormat::format(MSRate::MSRateFormat format_)
{ _formatType=Rate,_format._rate=format_;}
void MSFormat::format(MSTerm::MSTermFormat format_)
{ _formatType=Term,_format._term=format_;}
void MSFormat::format(MSTime::MSTimeFormat format_)
{ _formatType=Time,_format._time=format_;}

void MSFormat::formatModifier(unsigned long formatModifier_)
{ _formatModifier=formatModifier_; }

MSString MSFormat::asString(void) const
{
  if (_formatType!=MSFormat::NoFormat)
   {
     const char *pString=(char *)enumHashTable()->lookup((unsigned long)_format._any);
     if (pString==0) return MSString("NoFormat");
     return MSString(pString);
   }
  return MSString("NoFormat");
}

MSString MSFormat::asModifierString(void) const
{
  MSString modifierString;
  if (formatModifier()&UpperCaseK)
   {
     if (modifierString.length()>0) modifierString<<"|";
     modifierString<<"UpperCaseK";
   }
  if (formatModifier()&LowerCaseK)
   {
     if (modifierString.length()>0) modifierString<<"|";
     modifierString<<"LowerCaseK";
   }
  if (formatModifier()&UpperCaseM)
   {
     if (modifierString.length()>0) modifierString<<"|";
     modifierString<<"UpperCaseM";
   }
  if (formatModifier()&LowerCaseM)
   {
     if (modifierString.length()>0) modifierString<<"|";
     modifierString<<"LowerCaseM";
   }
  if (formatModifier()&Parenthesis)
   {
     if (modifierString.length()>0) modifierString<<"|";
     modifierString<<"Parenthesis";
   }
  if (modifierString.length()==0) modifierString="NoModifier";
  return modifierString;
}

const MSStringVector& MSFormat::formats(void) const
{
  switch (_formatType)
   {
   case FBool:    return boolFormats();
   case Date:     return dateFormats();
   case Float:    return floatFormats();
   case Int:      return intFormats();
   case Money:    return moneyFormats();
   case Rate:     return rateFormats();
   case Term:     return termFormats();
   case Time:     return timeFormats();
   case NoFormat: 
   default:       return noFormats();
   }
}

MSHashTable *MSFormat::initFormatHashTable(void)
{
  MSHashTable *pHashTable=new MSHashTable(64);
  
  pHashTable->notFound((unsigned long)0x0);     

  // MSFloat
  pHashTable->add("Decimal0",new MSFormat(MSFloat::Decimal0));
  pHashTable->add("Decimal1",new MSFormat(MSFloat::Decimal1));
  pHashTable->add("Decimal2",new MSFormat(MSFloat::Decimal2));
  pHashTable->add("Decimal3",new MSFormat(MSFloat::Decimal3));
  pHashTable->add("Decimal4",new MSFormat(MSFloat::Decimal4));
  pHashTable->add("Decimal5",new MSFormat(MSFloat::Decimal5));
  pHashTable->add("Decimal6",new MSFormat(MSFloat::Decimal6));
  pHashTable->add("Decimal7",new MSFormat(MSFloat::Decimal7));
  pHashTable->add("Decimal8",new MSFormat(MSFloat::Decimal8));
  pHashTable->add("CommaDecimal0",new MSFormat(MSFloat::CommaDecimal0));
  pHashTable->add("CommaDecimal1",new MSFormat(MSFloat::CommaDecimal1));
  pHashTable->add("CommaDecimal2",new MSFormat(MSFloat::CommaDecimal2));
  pHashTable->add("CommaDecimal3",new MSFormat(MSFloat::CommaDecimal3));
  pHashTable->add("CommaDecimal4",new MSFormat(MSFloat::CommaDecimal4));
  pHashTable->add("CommaDecimal5",new MSFormat(MSFloat::CommaDecimal5));
  pHashTable->add("CommaDecimal6",new MSFormat(MSFloat::CommaDecimal6));
  pHashTable->add("CommaDecimal7",new MSFormat(MSFloat::CommaDecimal7));
  pHashTable->add("CommaDecimal8",new MSFormat(MSFloat::CommaDecimal8));
  pHashTable->add("MaximumPrecision",new MSFormat(MSFloat::MaximumPrecision));
  pHashTable->add("Default",new MSFormat(MSFloat::Default));

  // MSInt
  pHashTable->add("WithoutCommas",new MSFormat(MSInt::WithoutCommas));
  pHashTable->add("WithCommas",new MSFormat(MSInt::WithCommas));  

  // MSDate
  pHashTable->add("Slash",new MSFormat(MSDate::Slash)); 
  pHashTable->add("Slash4",new MSFormat(MSDate::Slash4));
  pHashTable->add("Terse",new MSFormat(MSDate::Terse));
  pHashTable->add("Terse4",new MSFormat(MSDate::Terse4));
  pHashTable->add("Long",new MSFormat(MSDate::Long));
  pHashTable->add("MonthYear",new MSFormat(MSDate::MonthYear));
  pHashTable->add("YearMonthDay",new MSFormat(MSDate::YearMonthDay));
  pHashTable->add("Year2MonthDay",new MSFormat(MSDate::Year2MonthDay));
  pHashTable->add("EuropeanDot",new MSFormat(MSDate::EuropeanDot));
  pHashTable->add("EuropeanDot4",new MSFormat(MSDate::EuropeanDot4));
  pHashTable->add("Database",new MSFormat(MSDate::Database));
  pHashTable->add("DataBase",new MSFormat(MSDate::DataBase)); // for backward compatibility only
  pHashTable->add("Julian",new MSFormat(MSDate::Julian));

  // MSTime
  pHashTable->add("HoursMinutesSeconds",new MSFormat(MSTime::HoursMinutesSeconds));
  pHashTable->add("MonthDayYear",new MSFormat(MSTime::MonthDayYear));
  pHashTable->add("HoursMinutesSecondsSlash",new MSFormat(MSTime::HoursMinutesSecondsSlash));
  pHashTable->add("HoursMinutesSecondsSlashZone",new MSFormat(MSTime::HoursMinutesSecondsSlashZone));
  pHashTable->add("HoursMinutesSecondsSlash4",new MSFormat(MSTime::HoursMinutesSecondsSlash4));
  pHashTable->add("HoursMinutesSecondsSlash4Zone",new MSFormat(MSTime::HoursMinutesSecondsSlash4Zone));
  pHashTable->add("CalendarTime",new MSFormat(MSTime::CalendarTime));

  // MSMoney
  pHashTable->add("CurrencyPrecision",new MSFormat(MSMoney::CurrencyPrecision));
  pHashTable->add("CommaCurrencyPrecision",new MSFormat(MSMoney::CommaCurrencyPrecision));
  pHashTable->add("Eights",new MSFormat(MSMoney::Eights));
  pHashTable->add("Sixteenths",new MSFormat(MSMoney::Sixteenths));
  pHashTable->add("ThirtySeconds",new MSFormat(MSMoney::ThirtySeconds));
  pHashTable->add("SixtyForths",new MSFormat(MSMoney::SixtyForths));
  pHashTable->add("OneTwentyEights",new MSFormat(MSMoney::OneTwentyEights));
  pHashTable->add("TwoFiftySixths",new MSFormat(MSMoney::TwoFiftySixths));
  pHashTable->add("Tenths",new MSFormat(MSMoney::Tenths));
  pHashTable->add("Hundredths",new MSFormat(MSMoney::Hundredths));
  pHashTable->add("Thousandths",new MSFormat(MSMoney::Thousandths));
  pHashTable->add("TenThousandths",new MSFormat(MSMoney::TenThousandths));
  pHashTable->add("HundredThousandths",new MSFormat(MSMoney::HundredThousandths));
  pHashTable->add("Millionths",new MSFormat(MSMoney::Millionths));
  pHashTable->add("TenMillionths",new MSFormat(MSMoney::TenMillionths));
  pHashTable->add("HundredMillionths",new MSFormat(MSMoney::HundredMillionths));

  // MSRate
  pHashTable->add("Percent0",new MSFormat(MSRate::Percent0));
  pHashTable->add("Percent1",new MSFormat(MSRate::Percent1));
  pHashTable->add("Percent2",new MSFormat(MSRate::Percent2));
  pHashTable->add("Percent3",new MSFormat(MSRate::Percent3));
  pHashTable->add("Percent4",new MSFormat(MSRate::Percent4));
  pHashTable->add("Percent5",new MSFormat(MSRate::Percent5));
  pHashTable->add("BasisPoint",new MSFormat(MSRate::BasisPoint));

  // MSTerm
  pHashTable->add("YearsMonthsDays",new MSFormat(MSTerm::YearMonthDay)); 
  pHashTable->add("YearsMonthsDaysPad",new MSFormat(MSTerm::YearMonthDayPad));
  pHashTable->add("YearsMonthsDaysNoPad",new MSFormat(MSTerm::YearMonthDayNoPad));

  // MSBool
  pHashTable->add("YesAndNo",new MSFormat(MSBool::YesAndNo));
  pHashTable->add("TrueAndFalse",new MSFormat(MSBool::TrueAndFalse));
  pHashTable->add("Binary",new MSFormat(MSBool::Binary));

  return pHashTable;
}

MSHashTable *MSFormat::initEnumHashTable(void)
{
  MSHashTable *pHashTable=new MSHashTable(64);
  
  pHashTable->notFound((unsigned long)0x0);     

  // MSFloat
  pHashTable->add((unsigned long)MSFloat::Decimal0,(void *)"Decimal0");
  pHashTable->add((unsigned long)MSFloat::Decimal1,(void *)"Decimal1");
  pHashTable->add((unsigned long)MSFloat::Decimal2,(void *)"Decimal2");
  pHashTable->add((unsigned long)MSFloat::Decimal3,(void *)"Decimal3");
  pHashTable->add((unsigned long)MSFloat::Decimal4,(void *)"Decimal4");
  pHashTable->add((unsigned long)MSFloat::Decimal5,(void *)"Decimal5");
  pHashTable->add((unsigned long)MSFloat::Decimal6,(void *)"Decimal6");
  pHashTable->add((unsigned long)MSFloat::Decimal7,(void *)"Decimal7");
  pHashTable->add((unsigned long)MSFloat::Decimal8,(void *)"Decimal8");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal0,(void *)"CommaDecimal0");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal1,(void *)"CommaDecimal1");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal2,(void *)"CommaDecimal2");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal3,(void *)"CommaDecimal3");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal4,(void *)"CommaDecimal4");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal5,(void *)"CommaDecimal5");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal6,(void *)"CommaDecimal6");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal7,(void *)"CommaDecimal7");
  pHashTable->add((unsigned long)MSFloat::CommaDecimal8,(void *)"CommaDecimal8");
  pHashTable->add((unsigned long)MSFloat::MaximumPrecision,(void *)"MaximumPrecision");
  pHashTable->add((unsigned long)MSFloat::Default,(void *)"Default");

  // MSInt
  pHashTable->add((unsigned long)MSInt::WithoutCommas,(void *)"WithoutCommas");
  pHashTable->add((unsigned long)MSInt::WithCommas,(void *)"WithCommas");  

  // MSDate
  pHashTable->add((unsigned long)MSDate::Slash,(void *)"Slash"); 
  pHashTable->add((unsigned long)MSDate::Slash4,(void *)"Slash4");
  pHashTable->add((unsigned long)MSDate::Terse,(void *)"Terse");
  pHashTable->add((unsigned long)MSDate::Terse4,(void *)"Terse4");
  pHashTable->add((unsigned long)MSDate::Long,(void *)"Long");
  pHashTable->add((unsigned long)MSDate::MonthYear,(void *)"MonthYear");
  pHashTable->add((unsigned long)MSDate::YearMonthDay,(void *)"YearMonthDay");
  pHashTable->add((unsigned long)MSDate::Year2MonthDay,(void *)"Year2MonthDay");
  pHashTable->add((unsigned long)MSDate::EuropeanDot,(void *)"EuropeanDot");
  pHashTable->add((unsigned long)MSDate::EuropeanDot4,(void *)"EuropeanDot4");
  pHashTable->add((unsigned long)MSDate::Database,(void *)"Database");
  pHashTable->add((unsigned long)MSDate::Julian,(void *)"Julian");

  // MSTime
  pHashTable->add((unsigned long)MSTime::HoursMinutesSeconds,(void *)"HoursMinutesSeconds");
  pHashTable->add((unsigned long)MSTime::MonthDayYear,(void *)"MonthDayYear");
  pHashTable->add((unsigned long)MSTime::HoursMinutesSecondsSlash,(void *)"HoursMinutesSecondsSlash");
  pHashTable->add((unsigned long)MSTime::HoursMinutesSecondsSlashZone,(void *)"HoursMinutesSecondsSlashZone");
  pHashTable->add((unsigned long)MSTime::HoursMinutesSecondsSlash4,(void *)"HoursMinutesSecondsSlash4");
  pHashTable->add((unsigned long)MSTime::HoursMinutesSecondsSlash4Zone,(void *)"HoursMinutesSecondsSlash4Zone");
  pHashTable->add((unsigned long)MSTime::CalendarTime,(void *)"CalendarTime");

  // MSMoney
  pHashTable->add((unsigned long)MSMoney::CurrencyPrecision,(void *)"CurrencyPrecision");
  pHashTable->add((unsigned long)MSMoney::CommaCurrencyPrecision,(void *)"CommaCurrencyPrecision");
  pHashTable->add((unsigned long)MSMoney::Eights,(void *)"Eights");
  pHashTable->add((unsigned long)MSMoney::Sixteenths,(void *)"Sixteenths");
  pHashTable->add((unsigned long)MSMoney::ThirtySeconds,(void *)"ThirtySeconds");
  pHashTable->add((unsigned long)MSMoney::SixtyForths,(void *)"SixtyForths");
  pHashTable->add((unsigned long)MSMoney::OneTwentyEights,(void *)"OneTwentyEights");
  pHashTable->add((unsigned long)MSMoney::TwoFiftySixths,(void *)"TwoFiftySixths");

  // MSRate
  pHashTable->add((unsigned long)MSRate::Percent0,(void *)"Percent0");
  pHashTable->add((unsigned long)MSRate::Percent1,(void *)"Percent1");
  pHashTable->add((unsigned long)MSRate::Percent2,(void *)"Percent2");
  pHashTable->add((unsigned long)MSRate::Percent3,(void *)"Percent3");
  pHashTable->add((unsigned long)MSRate::Percent4,(void *)"Percent4");
  pHashTable->add((unsigned long)MSRate::Percent5,(void *)"Percent5");
  pHashTable->add((unsigned long)MSRate::BasisPoint,(void *)"BasisPoint");

  // MSTerm
  pHashTable->add((unsigned long)MSTerm::YearMonthDay,(void *)"YearsMonthsDays"); 
  pHashTable->add((unsigned long)MSTerm::YearMonthDayPad,(void *)"YearsMonthsDaysPad");
  pHashTable->add((unsigned long)MSTerm::YearMonthDayNoPad,(void *)"YearsMonthsDaysNoPad");

  // MSBool
  pHashTable->add((unsigned long)MSBool::YesAndNo,(void *)"YesAndNo");
  pHashTable->add((unsigned long)MSBool::TrueAndFalse,(void *)"TrueAndFalse");
  pHashTable->add((unsigned long)MSBool::Binary,(void *)"Binary");

  return pHashTable;
}

MSHashTable *MSFormat::initModifierHashTable(void)
{
  MSHashTable *pHashTable=new MSHashTable(16);
  
  pHashTable->notFound((unsigned long)NoModifier);     

  pHashTable->add("NoModifier",(void *)NoModifier);
  pHashTable->add("UpperCaseK",(void *)UpperCaseK);
  pHashTable->add("LowerCaseK",(void *)LowerCaseK);
  pHashTable->add("UpperCaseM",(void *)UpperCaseM);
  pHashTable->add("LowerCaseM",(void *)LowerCaseM);
  pHashTable->add("Parenthesis",(void *)Parenthesis);

  return pHashTable;
}

MSBoolean MSFormat::operator==(const MSFormat& aFormat_) const
{
  return MSBoolean(_formatType==aFormat_._formatType&&_formatModifier==aFormat_._formatModifier
		   &&_format._any==aFormat_._format._any);
}

MSBoolean MSFormat::operator!=(const MSFormat& aFormat_) const
{
  if (_formatType!=aFormat_._formatType) return MSTrue;
  else if (_formatModifier!=aFormat_._formatModifier) return MSTrue;
  else if (_format._any!=aFormat_._format._any) return MSTrue;
  return MSFalse;
}

MSBoolean MSFormat::operator==(MSBool::MSBoolFormat format_) const
{ return MSBoolean(_formatType==MSFormat::FBool&&_format._bool==format_); }
MSBoolean MSFormat::operator==(MSDate::MSDateFormat format_) const
{ return MSBoolean(_formatType==MSFormat::Date&&_format._date==format_); }
MSBoolean MSFormat::operator==(MSFloat::MSFloatFormat format_) const
{ return MSBoolean(_formatType==MSFormat::Float&&_format._float==format_); }
MSBoolean MSFormat::operator==(MSInt::MSIntFormat format_) const
{ return MSBoolean(_formatType==MSFormat::Int&&_format._int==format_); }
MSBoolean MSFormat::operator==(MSMoney::MSMoneyFormat format_) const
{ return MSBoolean(_formatType==MSFormat::Money&&_format._money==format_); }
MSBoolean MSFormat::operator==(MSRate::MSRateFormat format_) const
{ return MSBoolean(_formatType==MSFormat::Rate&&_format._rate==format_); }
MSBoolean MSFormat::operator==(MSTerm::MSTermFormat format_) const
{ return MSBoolean(_formatType==MSFormat::Term&&_format._term==format_); }
MSBoolean MSFormat::operator==(MSTime::MSTimeFormat format_) const
{ return MSBoolean(_formatType==MSFormat::Time&&_format._time==format_); }


const MSStringVector& MSFormat::boolFormats(void)
{
  static MSStringVector formats ("YesAndNo\n"\
				 "TrueAndFalse\n"\
				 "Binary");
  return formats;
}

const MSStringVector& MSFormat::dateFormats(void)
{
  static MSStringVector formats ("Slash\n"\
				 "Slash4\n"\
				 "Terse\n"\
				 "Terse4\n"\
				 "Long\n"\
				 "MonthYear\n"\
				 "YearMonthDay\n"\
				 "Year2MonthDay\n"\
				 "EuropeanDot\n"\
				 "EuropeanDot4\n"\
				 "Database\n"\
				 "Julian");
  return formats;
}

const MSStringVector& MSFormat::floatFormats(void)
{
  static MSStringVector formats ("Decimal0\n"\
				 "Decimal1\n"\
				 "Decimal2\n"\
				 "Decimal3\n"\
				 "Decimal4\n"\
				 "Decimal5\n"\
				 "Decimal6\n"\
				 "Decimal7\n"\
				 "Decimal8\n"\
				 "CommaDecimal0\n"\
				 "CommaDecimal1\n"\
				 "CommaDecimal2\n"\
				 "CommaDecimal3\n"\
				 "CommaDecimal4\n"\
				 "CommaDecimal5\n"\
				 "CommaDecimal6\n"\
				 "CommaDecimal7\n"\
				 "CommaDecimal8\n"\
				 "MaximumPrecision\n"\
				 "Default");
  return formats;
}

const MSStringVector& MSFormat::intFormats(void)
{
  static MSStringVector formats ("WithoutCommas\n"\
				 "WithCommas");
  return formats;
}

const MSStringVector& MSFormat::moneyFormats(void)
{
  static MSStringVector formats ("CurrencyPrecision\n"\
				 "CommaCurrencyPrecision\n"\
				 "Eights\n"\
				 "Sixteenths\n"\
				 "ThirtySeconds\n"\
				 "SixtyForths\n"\
				 "OneTwentyEights\n"\
				 "TwoFiftySixths\n"\
				 "Tenths\n"\
				 "Hundredths\n"\
				 "Thousandths\n"\
				 "TenThousandths\n"\
				 "HundredThousandths\n"\
				 "Millionths\n"\
				 "TenMillionths\n"\
				 "HundredMillionths");
  return formats;
}

const MSStringVector& MSFormat::rateFormats(void)
{
  static MSStringVector formats ("Percent0\n"\
				 "Percent1\n"\
				 "Percent2\n"\
				 "Percent3\n"\
				 "Percent4\n"\
				 "Percent5\n"\
				 "BasisPoint");
  return formats;
}

const MSStringVector& MSFormat::termFormats(void)
{
  static MSStringVector formats( "YearsMonthsDays\n"\
				 "YearsMonthsDaysPad\n"\
				 "YearsMonthsDaysNoPad");
  return formats;
}

const MSStringVector& MSFormat::timeFormats(void)
{
  static MSStringVector formats ("HoursMinutesSeconds\n"\
				 "MonthDayYear\n"\
				 "HoursMinutesSecondsSlash\n"\
				 "HoursMinutesSecondsSlashZone\n"\
				 "HoursMinutesSecondsSlash4\n"\
				 "HoursMinutesSecondsSlash4Zone\n"\
				 "CalendarTime");
 return formats;
}

const MSStringVector& MSFormat::noFormats(void)
{
  static MSStringVector formats;
  return formats;
}  

const MSStringVector& MSFormat::modifiers(void)
{
  static MSStringVector formats ("NoModifier\n"\
				 "UpperCaseK\n"\
				 "LowerCaseK\n"\
				 "UpperCaseM\n"\
				 "LowerCaseM\n"\
				 "Parenthesis");
  return formats;
}

const MSHashTable *MSFormat::formatHashTable(void)
{
  static MSHashTable *hashTable = initFormatHashTable();
  return hashTable;
}

const MSHashTable *MSFormat::enumHashTable(void)
{
  static MSHashTable *hashTable = initEnumHashTable();
  return hashTable;
}

const MSHashTable *MSFormat::modifierHashTable(void)
{
  static MSHashTable *hashTable = initModifierHashTable();
  return hashTable;
}

