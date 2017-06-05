///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSTime.H>
#include <MSTypes/MSRate.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSMoney.H>
#include <MSTypes/MSFormat.H>
#include <MSTypes/MSFractionTables.H>
#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSTypes_MSF.H>
#include <MSTypes/MSMessageLog.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

MSMoney::Currency MSMoney::_defaultCurrency=MSMoney::USDollar;

#ifdef MS_NO_INLINES
#include <MSTypes/MSMoneyInlines.C>
#endif

MSString MSMoney::asString(void) const
{
  MSString s;
  format(s,MSMoney::CurrencyPrecision,MSMoney::LocalSymbol);
  return s;
}


MSString MSMoney::asDebugInfo(void) const
{
  MSString result("MSMoney(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_real=";
  result+=MSString(_real);
  result+=",_isSet=";
  result+=(isSet()==MSTrue)?"MSTrue":"MSFalse";
  result+=",_isValid=";
  result+=(isValid()==MSTrue)?"MSTrue":"MSFalse";
  result+=",_currency=";
  result+=MSString((int)_currency);
  result+=",_defaultCurrency=";
  result+=MSString((int)_defaultCurrency);  
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}
  
MSString MSMoney::className(void) const
{ return MSString("MSMoney"); }

const MSSymbol& MSMoney::type(void) const
{ return symbol(); }

MSModel *MSMoney::clone(void) const
{ return new MSMoney(*this); }

MSModel *MSMoney::create(void) const
{ return new MSMoney(); }

void MSMoney::assign(const MSModel& aModel_)
{ *this=(MSMoney&)aModel_; }

long MSMoney::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSMoney&)aModel_); }

const MSSymbol& MSMoney::symbol(void)   
{
  static MSSymbol sym ("MSMoney");
  return sym;
}

// currency information structure
//
struct CurrencyData
{
  MSMoney::Currency curr;
  const char *isoSym;
  const char *localSym;
  MSMoney::SymbolLocation symLocation;
  MSBoolean isSpaceSep;
  MSMoney::MSMoneyFormat decimalFormat;
  MSMoney::MSMoneyFormat commaDecimalFormat;
};

// table mapping currency enumeration types to ISO symbols and other currency info - relies on sequential
// ordering of the enumerations as the code just indexes into the table using the enumeration value as the index.
//
CurrencyData currencyTable[] =
{
  {MSMoney::DefaultCurrency,  "",    "",      MSMoney::DefaultLocation,  MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  // the most important and common currencies go first
  {MSMoney::USDollar,         "USD", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::CanadianDollar,   "CAD", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::BritishPound,     "GBP", "\x0a3", MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::JapaneseYen,      "JPY", "\x0a5", MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal0, MSMoney::CommaDecimal0},
  {MSMoney::DeutscheMark,     "DEM", "DM",    MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::FrenchFranc,      "FRF", "F",     MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::SwissFranc,       "CHF", "F",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::DutchGuilder,     "NLG", "FL",    MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::AustralianDollar, "AUD", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::ItalianLira,      "ITL", "L",     MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::SpanishPeseta,    "ESP", "Pts",   MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::Euro,"EUR", "EUR",MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},


  // the rest of the currencies in alphabetical order
  {MSMoney::ArgentinianPeso,  "ARS", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::AustrianSchilling,"ATS", "S",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::BelgianFranc,     "BEF", "F",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::BrazilianReal,    "BRL", "R$",    MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::BulgarianLev,     "BGL", "Lv",    MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::ChileanPeso,      "CLP", "N$",    MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::ChineseRenminbi,  "CNY", "Y",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::CzechKoruna,      "CSK", "CK",    MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::DanishKrone,      "DKK", "Kr",    MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::FinnishMarkka,    "FIM", "FM",    MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::GreekDrachma,     "GRD", "Dr",    MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::HongKongDollar,   "HKD", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::HungarianForint,  "HUF", "Ft",    MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::IcelandKrona,     "ISK", "Kr",    MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::IndianRupee,      "INR", "Rs",    MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::IrishPunt,        "IEP", "\x0a3", MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::IsraeliShekel,    "ILS", "NIS",   MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::MexicanPeso,      "MXN", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::NewZealandDollar, "NZD", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::NorwegianKrone,   "NOK", "Kr",    MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::PolishZloty,      "PLN", "Zl",    MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::PortugueseEscudo, "PTE", "Esc",   MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::RomanianLeu,      "ROL", "L",     MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::RussianRouble,    "RUR", "R",     MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::SingaporeDollar,  "SGD", "$",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::SlovakianKoruna,  "SKK", "Ks",    MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::SouthKoreanWon,   "KRW", "W",     MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::SwedishKrona,     "SEK", "kr",    MSMoney::SymbolAtEnd,   MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::TaiwanDollar,     "TWD", "NT$",   MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::TurkishLira,      "TRL", "TL",    MSMoney::SymbolAtStart, MSFalse, MSMoney::Decimal2, MSMoney::CommaDecimal2},
  {MSMoney::VenezuelanBolivar,"VEB", "Bs",    MSMoney::SymbolAtStart, MSTrue,  MSMoney::Decimal2, MSMoney::CommaDecimal2}
};


const char *MSMoney::isoCurrency(void) const
{
  // find the ISO symbol for the currency in use,ignore DefaultCurrency as
  // this is resolved by currency()
  //
  return currencyTable[currency()].isoSym;
}


MSMoney::Currency MSMoney::isoConvert(const char *piso_sym_) const
{
  CurrencyData *pCurrData = findCurrency(piso_sym_);
  return (pCurrData==0) ? defaultCurrency() : pCurrData->curr;
}


// creates string <MSMSF_SPACE>float price<NSMSF_SPACE>ISO Currency string

MSString MSMoney::asMSF(void) const
{ 
  MSString aString;
  if (isSet()==MSTrue)
   {
     aString+=MSFloat::asMSF(); 
     aString+=MSMSF_SPACE;
     aString+=isoCurrency();
   }
  return aString;
}


// parses string <MSMSF_SPACE>float price<NSMSF_SPACE>ISO Currency string

MSError::ErrorStatus MSMoney::setFromMSF(const char *pString_)
{ 
  int code;

// first parse out the price and set it
// then find the ISO unit of currency string and use it to set the currency 

  if ((pString_!=0))
   {
     unsigned found;
     unsigned size=strlen(pString_); ;
     unsigned i;

     char *buff=new char[size];
     if (buff!=0)
      {
	found=0;
	for (i=0; i<size; i++)
	 { 
	   if (pString_[i]!=MSMSF_SPACE)
	   buff[i]=pString_[i];
	   else
	    {
	      found=1;
	      buff[i]='\0';
	      break;
	    }
	 }

	if (found==1)
	 {
	   code=MSFloat::set(buff);
	   if (code==MSError::MSSuccess) _currency=isoConvert(&pString_[i+sizeof(MSMSF_US)]);
	 }
	else code=MSError::BadMSFString;

	delete [] buff;
      }
     else code=MSError::MSFailure;
   }
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

MSMoney::~MSMoney(void) {}
// need to handle currency specifier here
MSError::ErrorStatus MSMoney::set(const MSString *pString_) { return set(pString_->string()); }
MSError::ErrorStatus MSMoney::set(const MSString& aString_) { return set(aString_.string()); }
MSError::ErrorStatus MSMoney::set(double d)                 { return MSFloat::set(d); }
MSError::ErrorStatus MSMoney::set(const char *pString_)
{
  // All MSFloat formats for the number part are supported here, except for the exponential
  // format (e.g., 1.23E+4), which is not used with money anyway.
  //
  if (pString_!=0)
    {
      if (*pString_=='\0')
	{
	  _currency = DefaultCurrency;
	  return MSFloat::set(pString_);
	}

      MSString str(pString_);
      str.strip();	// remove leading and trailing whitespace
      unsigned int strLen = str.length();

      unsigned int currStart = str.indexOf(MSStringTest(iscurr));
      if (currStart<strLen)	// found the beginning of the currency symbol
	{
	  unsigned int currEnd = str.indexOfAnyBut(MSStringTest(iscurr),currStart); // find the number part
	  //
	  // We have found the beginning and the end of the currency part in the string.
	  // In principle, we don't care where in the string the currency part is located
	  // (at the beginning, at the end, or in the middle).  We could get the currency
	  // substring, look up the currency in the currency table, remove the substring
	  // from the original string and pass the rest to MSFloat::set().  However, 
	  // we can do this more efficiently if we determine where the currency part is
	  // in the string, as we do below.
	  //
	  if (currEnd<strLen)	// the currency part is in the beginning or the middle of the string
	    {
	      char c=str(currEnd);  // save the first character of the number part
	      str.set(currEnd,'\0');  // null-terminate the string after the end of the currency part

	      CurrencyData *pCurr=findCurrency(str.string()+currStart); // find the symbol in the currency table
	      if (pCurr==0)	// could not match the symbol part
		{
		  return MSError::BadMoney;
		}

	      _currency = pCurr->curr;   // set the currency

	      str.set(currEnd,c);	// restore the first character of the number part

	      if (currStart==0)	  // if the currency is in the beginning (e.g. "$5.00")
		{
		  return MSFloat::set(str.string()+currEnd); // set the number part; calls changed() internally
		}
	      else	// the currency is in the middle of the string (e.g. "-$5.00")
		{
		  str.remove(currStart,currEnd-currStart); // remove the currency part from the string
		  return MSFloat::set(str.string());  // set the number part; calls changed() internally
		}
	    }
	  else	// the currency part extends to the end of the string (e.g "5.00 USD")
	    {
	      CurrencyData *pCurr=findCurrency(str.string()+currStart); // find the symbol in the currency table
	      if (pCurr==0)	// could not match the symbol part
		{
		  return MSError::BadMoney;
		}

	      _currency = pCurr->curr;   // set the currency

	      str.set(currStart,'\0');  // null-terminate the number part; str's length is unchanged
	      return MSFloat::set(str.string());   // set the number part; calls changed() internally
	    }
	}
      else	// no currency specified in the string
	{
	  _currency = DefaultCurrency;
	  return MSFloat::set(str.string());
	}	  
    }

  return MSError::MSFailure;
}

static char *lookupFraction(FractionTable *fractionTable_,double fractionValue_)
{
  register int low,mid,high;
  
  // Do a binary search of the security array.
  // Complements of Kernighan and Ritchie.
  low=0;
  mid=0;
  high=fractionTable_->length-1;
  fractionValue_=MSUtil::abs(fractionValue_);
  while (low<=high)
   {
     mid=(low+high)/2;
     if (fractionValue_<fractionTable_->table[mid].value) high=mid-1;
     else if (fractionValue_>fractionTable_->table[mid].value) low=mid+1;
     else break; // found it
   }
  return fractionTable_->table[mid].fraction; 
}

void MSMoney::currency(Currency c_)    
{ 
  if (c_!=currency()) 
   {
    _currency=c_; 
    changed();
  }
}

const char *MSMoney::format(MSString *pString_) const
{ return format(*pString_,CurrencyPrecision,NoCurrencySymbol); }

const char *MSMoney::format(MSString& aString_) const
{ return format(aString_,CurrencyPrecision,NoCurrencySymbol); }

const char *MSMoney::format(MSString *pString_,MSMoneyFormat format_,MSBoolean justify_) const
{ return format(*pString_,format_,NoCurrencySymbol,DefaultLocation,justify_);}

const char *MSMoney::format(MSString& aString_,MSMoneyFormat format_,MSBoolean justify_) const
{ return format(aString_,format_,NoCurrencySymbol,DefaultLocation,justify_); }

const char *MSMoney::format(MSString *pString_,const MSFormat& aFormat_,MSBoolean justify_) const
{ return format(*pString_,aFormat_,justify_); }

const char *MSMoney::format(MSString& aString_,const MSFormat& aFormat_,MSBoolean justify_) const
{
  if (aFormat_.formatType()==MSFormat::Money) return format(aString_,aFormat_.moneyFormat(),justify_);
  else return format(aString_);
}

const char *MSMoney::format(MSString *pString_, MSMoneyFormat f_,
			    MSMoney::SymbolType symType_, MSMoney::SymbolLocation symLocation_,
			    MSBoolean justify_) const
{ return format(*pString_,f_,symType_,symLocation_,justify_); }

const char *MSMoney::format(MSString& aString_, MSMoneyFormat moneyFormat_,
			    MSMoney::SymbolType symType_, MSMoney::SymbolLocation symLocation_,
			    MSBoolean justify_) const
{
  char	 buf[512];
  char  *fract_str;
  int    mantisa;
  double fraction;
  int    fieldWidth=0;
  CurrencyData *currData = &currencyTable[currency()];
  
  if (moneyFormat_==CurrencyPrecision)
   {
     // determine appropriate precision for currency type and use that
     //
     moneyFormat_ = currData->decimalFormat;
   }
  else if (moneyFormat_==CommaCurrencyPrecision)
   {
     // determine appropriate precision for currency type and use that
     //
     moneyFormat_ = currData->commaDecimalFormat;
   }
  
  switch (moneyFormat_)
   {
   // "inherited" formats from MSFloat
   case Decimal0:
   case Decimal1:
   case Decimal2:
   case Decimal3:
   case Decimal4:
   case Decimal5:
   case Decimal6:
   case Decimal7:
   case Decimal8:
   case CommaDecimal0:
   case CommaDecimal1:
   case CommaDecimal2:
   case CommaDecimal3:
   case CommaDecimal4:
   case CommaDecimal5:
   case CommaDecimal6:
   case CommaDecimal7:
   case CommaDecimal8: MSFloat::format(aString_,(MSFloatFormat) moneyFormat_); break;
   case Eights:
     if (justify_) fieldWidth=3;
     mantisa=int(_real);
     fraction=_real-mantisa;
     fract_str=lookupFraction(&EightsTable, fraction);
     if (mantisa!=0) sprintf(buf,"%d %-*s",mantisa,fieldWidth,fract_str);
     else
      {
	if (fraction<0.0) sprintf(buf,"-%-*s",fieldWidth,fract_str);
	else sprintf(buf,"%-*s",fieldWidth,fract_str);
      }
     aString_=buf;
     break;
   case Sixteenths:
     if (justify_) fieldWidth=3;
     mantisa=int(_real);
     fraction=_real-mantisa;
     fract_str=lookupFraction(&SixteenthsTable, fraction);
     if (mantisa!=0) sprintf(buf,"%d %-*s",mantisa,fieldWidth,fract_str);
     else
      {
	if (fraction<0.0) sprintf(buf,"-%-*s",fieldWidth,fract_str);
	else sprintf(buf,"%-*s",fieldWidth,fract_str);
      }
     aString_=buf;
     break;
   case ThirtySeconds:
     if (justify_) fieldWidth=5;
     mantisa=int(_real);
     fraction=_real-mantisa;
     fract_str=lookupFraction(&ThirtySecondsTable, fraction);
     if (mantisa!=0) sprintf(buf,"%d %-*s",mantisa,fieldWidth,fract_str);
     else
      {
	if (fraction<0.0) sprintf(buf,"-%-*s",fieldWidth,fract_str);
	else sprintf(buf,"%-*s",fieldWidth,fract_str);
      }
     aString_=buf;
     break;
   case SixtyForths:
     if (justify_) fieldWidth=5;
     mantisa=int(_real);
     fraction=_real-mantisa;
     fract_str=lookupFraction(&SixtyForthsTable, fraction);
     if (mantisa!=0) sprintf(buf,"%d %-*s",mantisa,fieldWidth,fract_str);
     else
      {
	if (fraction<0.0) sprintf(buf,"-%-*s",fieldWidth,fract_str);
	else sprintf(buf,"%-*s",fieldWidth,fract_str);
      }
     aString_=buf;
     break;
   case OneTwentyEights:
     if (justify_) fieldWidth=7;
     mantisa=int(_real);
     fraction=_real-mantisa;
     fract_str=lookupFraction(&OneTwentyEightsTable, fraction);
     if (mantisa!=0) sprintf(buf,"%d %-*s",mantisa,fieldWidth,fract_str);
     else
      {
	if (fraction<0.0) sprintf(buf,"-%-*s",fieldWidth,fract_str);
	else sprintf(buf,"%-*s",fieldWidth,fract_str);
      }
     aString_=buf;
     break;
   case TwoFiftySixths:
     {
       mantisa=int(_real);
       fraction=_real-mantisa;
       // try to figure out if the original number was set as
       // a fraction or decimal by doing a fudge-factor comparison
       // with the number rounded to the nearest 1/256
       MSMoney rounded((fraction*256)/256);
       if (rounded==fraction)
	{
	  // multiple of 256
	  if (justify_) fieldWidth=7;
	  fract_str=lookupFraction(&TwoFiftySixthsTable,fraction);
	  if (mantisa!=0) sprintf(buf,"%d %-*s",mantisa,fieldWidth,(char *)fract_str);
	  else
	   {
	     if (fraction<0.0) sprintf(buf,"-%-*s",fieldWidth,fract_str);
	     else sprintf(buf,"%-*s",fieldWidth,fract_str);
	   }
	} 
       else 
	{
	  // use decimal format
	  if (justify_) fieldWidth=5;  // align decimal point with space
	  sprintf(buf,"%.2f%*s",_real,fieldWidth,"");
	}
       aString_=buf;
     }
     break;
     
   case CurrencyPrecision:
   case CommaCurrencyPrecision:
   default:
     MSError::error(MSError::MSFailure,"MSMoney::MSMoneyFormat","Invalid Format");
     return format(aString_);
   }
  
  if (symLocation_==DefaultLocation)
    {
      symLocation_ = currData->symLocation;
    }

  if (symType_!=NoCurrencySymbol)
    {
      char newbuf[512];
      const char *currSymbol = (symType_==ISOSymbol) ? currData->isoSym : currData->localSym;
      const char *sep = (symType_==ISOSymbol || currData->isSpaceSep==MSTrue) ? " " : "";

      if (symLocation_==SymbolAtStart)
	{
	  sprintf(newbuf,"%s%s%s",currSymbol,sep,aString_.string());
	}
      else
	{
	  sprintf(newbuf,"%s%s%s",aString_.string(),sep,currSymbol);
	}

      aString_=newbuf;
    }

  return (aString_.string());
}

//####################################################################################
// operators

MSMoney& MSMoney::operator=(const MSMoney& aMoney_)
{ 
  if (this!=&aMoney_)
   {
     _currency=aMoney_._currency;
     MSFloat::operator=(aMoney_);
   }
  return *this; 
}

MSMoney& MSMoney::operator=(const MSFloat& f_)
{ return MSFloat::operator=(f_),*this; }
MSMoney& MSMoney::operator=(const MSInt& i_)
{ return MSFloat::operator=((int)i_),*this; }
MSMoney& MSMoney::operator=(double d_)
{ return MSFloat::operator=(d_),*this; }
MSMoney& MSMoney::operator=(int i_)
{ return MSFloat::operator=(i_),*this; }

MSMoney MSMoney::operator+(const MSMoney& m_) const 
{
  if (currency()!=m_.currency())
   {
     MSMessageLog::errorMessage("MSMoney::operator+():  currencies don't match\n");
     return MSMoney(*this);
   }
  else
    {
      return MSMoney(*this,m_,Plus);
    }
}


MSMoney MSMoney::operator-(const MSMoney& m_) const 
{
  if (currency()!=m_.currency())
   {
     MSMessageLog::errorMessage("MSMoney::operator-():  currencies don't match\n");
     return MSMoney(*this);
   }
  else
    {
      return MSMoney(*this,m_,Minus);
    }
}

//#########################################################################################
// special protected constructors used for return value optimization
//#########################################################################################

MSMoney::MSMoney(const MSMoney& a_,const MSMoney& b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(a_._currency)   // assumes a_._currency==b_._currency
{
}


MSMoney::MSMoney(const MSMoney& a_,const MSFloat& b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(a_._currency)
{
}


MSMoney::MSMoney(const MSMoney& a_,const MSInt& b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(a_._currency)
{
}


MSMoney::MSMoney(const MSMoney& a_,double b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(a_._currency)
{
}


MSMoney::MSMoney(const MSMoney& a_,int b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(a_._currency)
{
}


MSMoney::MSMoney(const MSFloat& a_,const MSMoney& b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(b_._currency)
{
}


MSMoney::MSMoney(const MSInt& a_,const MSMoney& b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(b_._currency)
{
}

MSMoney::MSMoney(double a_,const MSMoney& b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(b_._currency)
{
}


MSMoney::MSMoney(int a_,const MSMoney& b_,MSFloat::FloatOperator operator_)
  : MSFloat(a_,b_,operator_), _currency(b_._currency)
{
}


MSMoney& MSMoney::operator+=(const MSMoney& aMoney_)
{
  if (aMoney_.currency()!=currency())
    {
      MSError::error(MSError::MSFailure,"MSMoney::operator+=","currencies don't match");
      setInvalid();
      return changed(),*this;
    }
  else 
    {
      MSFloat::operator+=(aMoney_);
      return *this;
    }
}


MSMoney& MSMoney::operator-=(const MSMoney& aMoney_)
{
  if (aMoney_.currency()!=currency())
    {
      MSError::error(MSError::MSFailure,"MSMoney::operator-=","currencies don't match");
      setInvalid();
      return changed(),*this;
    }
  else 
    {
      MSFloat::operator-=(aMoney_);
      return *this;
    }
}


ostream& operator<<(ostream& aStream_,const MSMoney& aMoney_)
{
  MSString aString;
  return aStream_<<aMoney_.format(aString,MSMoney::CurrencyPrecision,MSMoney::LocalSymbol);
}


void MSMoney::setDefaultCurrency(Currency c_)
{
  if (c_==DefaultCurrency) 
     MSError::error(MSError::MSFailure,"MSMoney::SetDefaultCurrency","Can't set default");
  else _defaultCurrency=c_;
}


CurrencyData *MSMoney::findCurrency(const char *sym_)
{
  static MSBoolean isInitialized=MSFalse;
  static MSHashTable *currSymTable;

  if (isInitialized==MSFalse)
    {
      isInitialized = MSTrue;
      unsigned int currTblSize = sizeof(currencyTable)/sizeof(CurrencyData);
      currSymTable = new MSHashTable(2*currTblSize); // ISO symbols and local symbols
      currSymTable->notFound(0x0);
      // add the ISO symbols to the table first
      for (unsigned int i=0; i<currTblSize; ++i)
	{
	  currSymTable->add(currencyTable[i].isoSym, (void *)&currencyTable[i]);
	  //
	  // Unlike ISO currency symbols, local currency symbols are not unique
	  // therefore, check if a symbol (e.g., "$") has already been added to the
	  // hash table; if not, add it.  Thus, if a local currency symbol is used
	  // in more than one country, the first one added here will be returned.
	  // This is why the most common/important currencies (USDollar, FrenchFranc, etc.)
	  // are laid down *first* in the currencyTable.
	  //
	  CurrencyData *pData = (CurrencyData *)currSymTable->lookup(currencyTable[i].localSym);
	  if (pData==(CurrencyData *)currSymTable->notFound()) // if it's not in the table yet
	    {
	      currSymTable->add(currencyTable[i].localSym, (void *)&currencyTable[i]);
	    }
	}
    }

  return (CurrencyData *)currSymTable->lookup(sym_);
}


int MSMoney::iscurr(int c)
{
  return (c=='$' || !(isdigit(c) || ispunct(c) || isspace(c)));
}
