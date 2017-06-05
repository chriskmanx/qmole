///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSUnsigned.H>
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

ostream& operator<<(ostream& aStream_,const MSUnsigned& aUnsigned_)   
{ return aStream_<<aUnsigned_._unsigned; }

istream& operator>>(istream& aStream_,MSUnsigned& aUnsigned_)
{
  unsigned i;
  aStream_>>i;
  aUnsigned_=i;
  return aStream_;
}

#ifdef MS_NO_INLINES
#include <MSTypes/MSUnsignedInlines.C>
#endif

MSString MSUnsigned::asString(void) const
{ return MSString(_unsigned); }

MSString MSUnsigned::asDebugInfo(void) const
{
  MSString result("MSUnsigned(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_unsigned=";
  result+=MSString(_unsigned);
  result+=",_isSet=";
  result+=(isSet()==MSTrue)?"MSTrue":"MSFalse";
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}
  
MSString MSUnsigned::className(void) const
{ return MSString("MSUnsigned"); }

const MSSymbol& MSUnsigned::type(void) const
{ return symbol(); }

MSModel *MSUnsigned::clone(void) const
{ return new MSUnsigned(*this); }

MSModel *MSUnsigned::create(void) const
{ return new MSUnsigned(); }

void MSUnsigned::assign(const MSModel& aModel_)
{ *this=(MSUnsigned&)aModel_; }

long MSUnsigned::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSUnsigned&)aModel_); }

MSString MSUnsigned::asMSF(void) const
{
  if (isSet()==MSTrue) return MSString(_unsigned);
  return MSString();
}

MSError::ErrorStatus MSUnsigned::setFromMSF(const char *pString_)
{ 
  int code;
  if (pString_!=0) code=set(pString_);
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

const MSSymbol& MSUnsigned::symbol(void)   
{
  static MSSymbol sym ("MSUnsigned");
  return sym;
}

void MSUnsigned::unset(void)
{
  if (isSet()==MSTrue)
   {
     _unsigned=0,_isSet=MSFalse;
     changed();
   }
}

MSError::ErrorStatus MSUnsigned::set(const MSString& aString_)
{
  MSError::ErrorStatus code=MSError::MSSuccess;
  _unsigned=0,_isSet=MSTrue;

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
     else if (aString(0)=='-') code=MSError::BadInt;  // only positive values
     else if (slen>MSUnsigned::MaximumLength) code=MSError::IntTooBig;
     if (code==MSError::MSSuccess)
      {
	if (sscanf(aString.string(),"%u",&_unsigned) != 1)
	   _unsigned=0,code=MSError::BadInt;
	else
	if (_unsigned == 0)	// could be the result of overflow in scanf
	 {
	   for (int n=slen-1;n>=0;n--)
	     if (aString(n)>='1'&&aString(n)<='9')   // result > 0
	       _unsigned=0,code=MSError::BadInt;     // -> overflow
	 }
      }
   }
  return changed(),code;
}

MSError::ErrorStatus MSUnsigned::set(const char *pString_)
{ return set(MSString(pString_)); }
MSError::ErrorStatus MSUnsigned::set(const MSString *pString_) 
{ return set(*pString_);}

MSError::ErrorStatus MSUnsigned::set(unsigned i_) 
{
  _unsigned=i_;
  _isSet=MSTrue;
  return changed(),MSError::MSSuccess;
}

const char *MSUnsigned::format(MSString *pString_) const
{ return format(*pString_,MSInt::WithoutCommas); }
const char *MSUnsigned::format(MSString& aString_) const
{ return format(aString_,MSInt::WithoutCommas); }

const char *MSUnsigned::format(MSString& aString_,const MSFormat& aFormat_) const
{
  return (aFormat_.formatType()==MSFormat::Int)?
  formatUnsigned(aString_,aFormat_.intFormat(),aFormat_.formatModifier(),_unsigned):format(aString_,MSInt::WithoutCommas);
}

const char *MSUnsigned::format(MSString *pString_,const MSFormat& aFormat_) const
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

const char *MSUnsigned::format(MSString& aString_,MSInt::MSIntFormat aFormat_) const
{
  return formatUnsigned(aString_,aFormat_,MSFormat::NoModifier,_unsigned);
}

const char *MSUnsigned::format(MSString *pString_,MSInt::MSIntFormat aFormat_) const
{ return format(*pString_,aFormat_); }

const char *MSUnsigned::formatUnsigned(MSString &aString_,MSInt::MSIntFormat aFormat_,unsigned long modifier_,unsigned value_) const
{
  if (isSet()==MSTrue)
   {
     unsigned value=value_;
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
     sprintf(buf,"%u",value);
     if (aFormat_==MSInt::WithCommas&&strlen(buf)>3) 
      {
	char outBuffer[32];
	addCommas(buf,outBuffer);
	aString_=outBuffer;
      }
     else aString_=buf;
     if (modifierChar!='\0') aString_<<modifierChar;
   }
  else aString_="";
  return aString_.string();
}

MSUnsigned& MSUnsigned::operator+=(const MSUnsigned& i_)
{
  _unsigned+=i_._unsigned;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}

MSUnsigned& MSUnsigned::operator-=(const MSUnsigned& i_)
{
  _unsigned-=i_._unsigned;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}

MSUnsigned& MSUnsigned::operator*=(const MSUnsigned& i_)
{
  _unsigned*=i_._unsigned;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}

MSUnsigned& MSUnsigned::operator/=(const MSUnsigned& i_)
{
  _unsigned/=i_._unsigned;
  _isSet=MSBoolean(_isSet&&i_._isSet); // the result is set only if both numbers are set
  return changed(),*this;
}
