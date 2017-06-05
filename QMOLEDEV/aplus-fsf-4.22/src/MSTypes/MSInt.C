///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSInt.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSFormat.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <stdio.h>
#include <stdlib.h>

ostream& operator<<(ostream& aStream_,const MSInt& aInt_)   
{ return aStream_<<aInt_._int; }

istream& operator>>(istream& aStream_,MSInt& aInt_)
{
  int i;
  aStream_>>i;
  aInt_=i;
  return aStream_;
}

#ifdef MS_NO_INLINES
#include <MSTypes/MSIntInlines.C>
#endif

MSString MSInt::asString(void) const
{ return MSString(_int); }

MSString MSInt::asDebugInfo(void) const
{
  MSString result("MSInt(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_int=";
  result+=MSString(_int);
  result+=",_isSet=";
  result+=(isSet()==MSTrue)?"MSTrue":"MSFalse";
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}
  
MSString MSInt::className(void) const
{ return MSString("MSInt"); }

const MSSymbol& MSInt::type(void) const
{ return symbol(); }

MSModel *MSInt::clone(void) const
{ return new MSInt(*this); }

MSModel *MSInt::create(void) const
{ return new MSInt(); }

void MSInt::assign(const MSModel& aModel_)
{ *this=(MSInt&)aModel_; }

long MSInt::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSInt&)aModel_); }

MSString MSInt::asMSF(void) const
{
  if (isSet()==MSTrue) return MSString(_int);
  return MSString();
}

MSError::ErrorStatus MSInt::setFromMSF(const char *pString_)
{ 
  int code;
  if (pString_!=0) code=set(pString_);
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

const MSSymbol& MSInt::symbol(void)   
{
  static MSSymbol sym ("MSInt");
  return sym;
}

void MSInt::unset(void)
{
  if (isSet()==MSTrue)
   {
     _int=0,_isSet=MSFalse;
     changed();
   }
}

MSError::ErrorStatus MSInt::set(const MSString& aString_)
{
  MSError::ErrorStatus code=MSError::MSSuccess;
  _int=0,_isSet=MSTrue;

  char *pReturnVal=0;
  MSString aString=aString_;
  unsigned index=aString.indexOf(',');
  while (index<aString.length())
   {
     aString.remove(index,1);
     index=aString.indexOf(',',index);
   }
  if (aString.indexOf('.')<aString.length()) code=MSError::BadInt;
  else
   {
     unsigned slen=aString.length();
     if (slen==0) code=MSError::BadInt;
     else if (aString(0)=='-')
      {
	if (slen>MSInt::MaximumLength+1) code=MSError::IntTooBig;
      }
     else if (slen>MSInt::MaximumLength) code=MSError::IntTooBig;
     if (code==MSError::MSSuccess)
      {
	_int=strtol(aString.string(),&pReturnVal,10);
	if (*pReturnVal!=0) _int=0,code=MSError::BadInt;
      }
   }
  return changed(),code;
}

MSError::ErrorStatus MSInt::set(const char *pString_)
{ return set(MSString(pString_)); }
MSError::ErrorStatus MSInt::set(const MSString *pString_) 
{ return set(*pString_);}

MSError::ErrorStatus MSInt::set(int i_) 
{
  _int=i_;
  _isSet=MSTrue;
  return changed(),MSError::MSSuccess;
}

const char *MSInt::format(MSString *pString_) const
{ return format(*pString_,MSInt::WithoutCommas); }
const char *MSInt::format(MSString& aString_) const
{ return format(aString_,MSInt::WithoutCommas); }

const char *MSInt::format(MSString& aString_,const MSFormat& aFormat_) const
{
  return (aFormat_.formatType()==MSFormat::Int)?
  formatInt(aString_,aFormat_.intFormat(),aFormat_.formatModifier(),_int):format(aString_,MSInt::WithoutCommas);
}

const char *MSInt::format(MSString *pString_,const MSFormat& aFormat_) const
{ return format(*pString_,aFormat_); }

// MSUtil::comma is too inefficient, and one reason is becuase it does useful
// checking to avoid writing past the end of a string (another reason may
// be that it doesn't use "register" storage).  We need as efficient
// a function as possible, otherwise we will spend more time here than
// anywhere else. addCommas runs in about 1/100 of the time of MSUtil::comma.
// Plan on having enough space in outBuffer_. 
static void addCommas(register char *inBuffer_,register char *outBuffer_)
{
  // addCommas takes the number string in inBuffer_ and copies it to
  // outBuffer_,adding commas between the first character and the end
  // point according to American convention. 
  
  // ignore leading minus sign
  if (*inBuffer_=='-') *outBuffer_++=*inBuffer_++;
  
  register int places=strlen(inBuffer_); // number of digits to the left of the decimal point
  register char *cp=inBuffer_+places;    // pointer to the end
  register int remainder=((places-1)%3)+1; 

  // copy 1 to 3 digits before the first comma.
  while (remainder-->0) *outBuffer_++=*inBuffer_++;
  
  // copy a comma and 3 digits until we format to the end.
  while (inBuffer_<cp)
   {
     *outBuffer_++=',';
     *outBuffer_++=*inBuffer_++;
     *outBuffer_++=*inBuffer_++;
     *outBuffer_++=*inBuffer_++;
   }
  
  // copy over the decimal point and the rest of the digits,and
  // the trailing null.  We could use strcpy, but at this point,
  // there is likely to be fewer than 4 characters left, so it's
  // faster to do it ourselves.
  while ((*outBuffer_++=*inBuffer_++)!=0);
}

const char *MSInt::format(MSString& aString_,MSIntFormat aFormat_) const
{
  return formatInt(aString_,aFormat_,MSFormat::NoModifier,_int);
}

const char *MSInt::format(MSString *pString_,MSIntFormat aFormat_) const
{ return format(*pString_,aFormat_); }

const char *MSInt::formatInt(MSString &aString_,MSIntFormat aFormat_,unsigned long modifier_,int value_) const
{
  if (isSet()==MSTrue)
   {
     int value=value_;
     char modifierChar='\0';
     if (modifier_&MSFormat::UpperCaseK)
      {
	value=value_/1000;
	modifierChar='K';
      }
     else if (modifier_&MSFormat::LowerCaseK)
      {
	value=value_/1000;
	modifierChar='k';
      }
     else if (modifier_&MSFormat::UpperCaseM)
      {
	value=value_/1000000;
	modifierChar='M';
      }
     else if (modifier_&MSFormat::LowerCaseM)
      {
	value=value_/1000000;
	modifierChar='m';
      }
     char buf[32];
     sprintf(buf,"%d",value);
     if (aFormat_==MSInt::WithCommas&&strlen(buf)>3) 
      {
	char outBuffer[32];
	addCommas(buf,outBuffer);
	aString_=outBuffer;
      }
     else aString_=buf;
     if (modifierChar!='\0') aString_<<modifierChar;
     if (modifier_&MSFormat::Parenthesis)
      {
	if (value<0)
	 {
	   aString_.change("-","(");
	   aString_<<")";
	 }
      }
   }
  else aString_="";
  return aString_.string();
}

MSInt& MSInt::operator+=(const MSInt& i_)
{
  _int+=i_._int;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}

MSInt& MSInt::operator-=(const MSInt& i_)
{
  _int-=i_._int;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}

MSInt& MSInt::operator*=(const MSInt& i_)
{
  _int*=i_._int;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}

MSInt& MSInt::operator/=(const MSInt& i_)
{
  _int/=i_._int;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}

// friend opetators
int operator+(const MSInt& a_,int b_)
{ return a_._int+b_; }
int operator+(int a_,const MSInt& b_)
{ return a_+b_._int; }
int operator+(const MSInt& a_,const MSInt& b_)
{ return a_._int+b_._int; }

int operator-(const MSInt& a_,int b_)
{ return a_._int-b_; }
int operator-(int a_,const MSInt& b_)
{ return a_-b_._int; }
int operator-(const MSInt& a_,const MSInt& b_)
{ return a_._int-b_._int; }

int operator*(const MSInt& a_,int b_)
{ return a_._int*b_; }
int operator*(int a_,const MSInt& b_)
{ return a_*b_._int; }
int operator*(const MSInt& a_,const MSInt& b_)
{ return a_._int*b_._int; }

int operator/(const MSInt& a_,int b_)
{ return a_._int/b_; }
int operator/(int a_,const MSInt& b_)
{ return a_/b_._int; }
int operator/(const MSInt& a_,const MSInt& b_)
{ return a_._int/b_._int; }

int operator%(const MSInt& a_,int b_)
{ return a_._int%b_; }
int operator%(int a_,const MSInt& b_)
{ return a_%b_._int; }
int operator%(const MSInt& a_,const MSInt& b_)
{ return a_._int%b_._int; }

MSBoolean operator==(int a_,const MSInt& b_)
{ return MSBoolean(a_==b_._int); } 
MSBoolean operator!=(int a_,const MSInt& b_)
{ return MSBoolean(a_!=b_._int); } 
MSBoolean operator< (int a_,const MSInt& b_)
{ return MSBoolean(a_< b_._int); } 
MSBoolean operator<=(int a_,const MSInt& b_)
{ return MSBoolean(a_<=b_._int); } 
MSBoolean operator> (int a_,const MSInt& b_)
{ return MSBoolean(a_> b_._int); } 
MSBoolean operator>=(int a_,const MSInt& b_)
{ return MSBoolean(a_>=b_._int); }
