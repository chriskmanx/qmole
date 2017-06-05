///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSRate.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSFormat.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef MS_NO_INLINES
#include <MSTypes/MSRateInlines.C>
#endif

MSString MSRate::asString(void) const
{ MSString s; return MSString(format(s,MSRate::Percent4)); }

MSString MSRate::asDebugInfo(void) const
{
  MSString result("MSRate(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_real=";
  result+=MSString(_real);
  result+=",_isSet=";
  result+=isSet()==MSTrue?"MSTrue":"MSFalse";
  result+=",_isValid=";
  result+=isValid()==MSTrue?"MSTrue":"MSFalse";
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return result;
}
  
MSString MSRate::className(void) const
{ return MSString("MSRate"); }

const MSSymbol& MSRate::type(void) const
{ return symbol(); }

MSModel *MSRate::clone(void) const
{ return new MSRate(*this); }

MSModel *MSRate::create(void) const
{ return new MSRate(); }

void MSRate::assign(const MSModel& aModel_)
{ *this=(MSRate&)aModel_; }

long MSRate::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSRate&)aModel_); }

const MSSymbol& MSRate::symbol(void)   
{
  static MSSymbol sym ("MSRate");
  return sym;
}

MSRate::~MSRate(void)
{}

MSError::ErrorStatus MSRate:: set(const char *pString_)
{
  // create copy in case we have to remove bp/%
  // MSFloat::set handles valid and set flags
  MSString copy(pString_);
  copy.stripTrailing();
  int len=copy.length();
  
  // check for percent
  if (copy.last()=='%')
//  if ((len>0)&&(pString_[len-1]=='%')) -- old style
   {
     copy.truncate(1);
     MSError::ErrorStatus status=MSFloat::internalSet(copy.string());
     if (status!=MSError::MSSuccess) return status;
     _real/=100.0;
     return changed(),MSError::MSSuccess;
   }

  // check for basis points
  if ((len>1)&&(((pString_[len-2]=='b')&&(pString_[len-1]=='p'))||
		((pString_[len-2]=='B')&&(pString_[len-1]=='P')))) 
   {
     MSString copy(pString_);
     copy.truncate(2);
     MSError::ErrorStatus status=MSFloat::internalSet(copy.string());
     if (status!=MSError::MSSuccess) return status;
     _real/=10000.0;
     return changed(),MSError::MSSuccess;
   }
  if (MSFloat::internalSet(pString_)!=MSError::MSSuccess) return MSError::BadRate;
  return changed(),MSError::MSSuccess;
}

MSError::ErrorStatus MSRate:: set(const MSString *pString_) { return set(pString_->string()); }
MSError::ErrorStatus MSRate:: set(const MSString& aString_) { return set(aString_.string()); }
MSError::ErrorStatus MSRate:: set(double d_)                { return MSFloat::set(d_); }

const char *MSRate:: format(MSString *pString_) const
{ return MSFloat::format(pString_); }
const char *MSRate:: format(MSString& aString_) const
{ return MSFloat::format(aString_); }

const char *MSRate::format(MSString& aString_,const MSFormat& aFormat_) const
{ return (aFormat_.formatType()==MSFormat::Rate)?format(aString_,aFormat_.rateFormat()):format(aString_); }
const char *MSRate::format(MSString *pString_,const MSFormat& aFormat_) const
{ return format(*pString_,aFormat_); }

const char *MSRate:: format(MSString *pString_,MSRateFormat format_) const
{ return format(*pString_,format_); }
const char *MSRate:: format(MSString& aString_,MSRateFormat format_) const
{
  if (isSet()==MSFalse) { aString_=""; return aString_.string(); }
  if (isValid()==MSFalse) 
   {
     aString_="?";
     MSError::error(MSError::MSFailure,"MSRate::MSRateFormat","Invalid Value");
     return aString_.string();
   }
  
  char buf[80];
  switch (format_)
   {
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
   case CommaDecimal8: return formatReal(aString_,MSFloatFormat(format_),MSFormat::NoModifier,_real);
   case Percent0: sprintf(buf,"%.0f%%",_real*100); break;
   case Percent1: sprintf(buf,"%.1f%%",_real*100); break;
   case Percent2: sprintf(buf,"%.2f%%",_real*100); break;
   case Percent3: sprintf(buf,"%.3f%%",_real*100); break;
   case Percent4: sprintf(buf,"%.4f%%",_real*100); break;
   case Percent5: sprintf(buf,"%.5f%%",_real*100); break;
   case BasisPoint: sprintf(buf,"%.0fbp",_real*10000); break;
   default:
     MSError::error(MSError::MSFailure,"MSRate::MSRateFormat","Invalid Value");
     return format(aString_);
   }

  aString_=buf;
  return aString_.string();
}

ostream& operator<<(ostream& aStream_,const MSRate& aRate_)
{ MSString aString; return aStream_<<aRate_.format(aString,MSRate::Percent4); }
