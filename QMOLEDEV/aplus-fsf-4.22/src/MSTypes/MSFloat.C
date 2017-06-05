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
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <MSTypes/MSFloat.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSFormat.H>

// MSRealMaximumLength specifies the maximum length (in characters) of the
// decimal part of a string representation of double.  We use this as a
// shortcut during parsing.  DBL_DIG defined in float.h specifies the maximum
// *total* number of digits in a double.  The longest decimal part is in the
// exponential format:  e.g., 1.79769313486232e+308.  Thus, we need to subtract
// 1 from DBL_DIG to exclude the digit *before* the decimal (there can be only
// one digit before the decimal point in the exponential format) and add 5 to
// include the exponent part, which is at most 5 characters long.  Thus, in
// total we need to add -1+5=4 to get MSRealMaximumLength.
//
const int MSRealMaximumLength=DBL_DIG+4;
const double MSFloatDelta=0.00000000000001; // for comparison of doubles

ostream& operator<<(ostream& aStream_,const MSFloat& aFloat_)
{ return aStream_<<aFloat_._real; }

istream& operator>>(istream& aStream_,MSFloat& aFloat_)
{
  double d;
  aStream_>>d;
  aFloat_=d;
  return aStream_;
}

#ifdef MS_NO_INLINES
#include <MSTypes/MSFloatInlines.C>
#endif

MSFloat::~MSFloat(void)
{}

MSString MSFloat::asString(void) const
{ return MSString(_real); }

MSString MSFloat::asDebugInfo(void) const
{
  MSString result("MSFloat(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_real=";
  result+=MSString(_real);
  result+=",_isSet=";
  result+=(isSet()==MSTrue)?"MSTrue":"MSFalse";
  result+=",_isValid=";
  result+=(isValid()==MSTrue)?"MSTrue":"MSFalse";
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}
  
MSString MSFloat::className(void) const
{ return MSString("MSFloat"); }

const MSSymbol& MSFloat::type(void) const
{ return symbol(); }

MSModel *MSFloat::clone(void) const
{ return new MSFloat(*this); }

MSModel *MSFloat::create(void) const
{ return new MSFloat(); }

void MSFloat::assign(const MSModel& aModel_)
{ *this=(MSFloat&)aModel_; }

long MSFloat::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSFloat&)aModel_); }

MSString MSFloat::asMSF(void) const
{ 
  MSString aString;
  if (isSet()==MSTrue) (void)formatReal(aString,MSFloat::MaximumPrecision,MSFormat::NoModifier,_real);
  return aString;
}

MSError::ErrorStatus MSFloat::setFromMSF(const char *pString_)
{ 
  int code;
  if (pString_!=0) code=set(pString_);
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

const MSSymbol& MSFloat::symbol(void)   
{
  static MSSymbol sym ("MSFloat");
  return sym;
}

MSError::ErrorStatus MSFloat::internalSet(const char *pString_)
{
  char	*np,buf[512];
  
  _real=0.0;
  _flags=MSFloat::Set; // clear the Valid bit, i.e. isValid==MSFalse
  
  if (*pString_=='\0')
   { _real=0.0; setToValid(); return (MSError::MSSuccess); }
  else if (strncmp(pString_,"NaN",3)==0) 
   { _real=0.0; return (MSError::MSSuccess); }
  else if ((np=strchr(pString_,'.'))&&(strchr(++np,'.')))
   { return (MSError::BadReal); } // Disallow more than one decimal point
  else if ((np=strchr(pString_,'.'))&&(strchr(++np,','))) 
   { return (MSError::BadReal); } // Disallow commas after the decimal point
  else if (*pString_==',') 
   { return (MSError::BadReal); } // Disallow leading commas
  
  // Make sure we don't save too many characters after the decimal
  char *decimal=strchr(pString_,'.');
  if ((decimal!=0)&&(strlen(decimal+1)>MSRealMaximumLength))
   { return (MSError::IntTooBig); }
  strcpy(buf,pString_);
  
  // Check for bad (alpha) characters -- the letter "e" is ok
  for (char *cp1=buf; *cp1!='\0'; cp1++) 
   {
     if (isalpha(*cp1))
     switch (*cp1) 
      {
      case 'e': case 'E': break;
      default: return (MSError::BadReal);
      }
   }
  
  // If there are commas,make sure they are correctly spaced
  if (strchr(buf,',')!=NULL) 
   {
     int i; char *cp2;
     // Find the end of the units part of the number
     if ((cp2=strchr(buf,'.'))==NULL) 
      { cp2=buf; cp2+=strlen(buf); } // no decimal point, go for the end of the string
     for (i=0; cp2>buf; cp2--,i++)
      { if (*cp2==','&&i%4!=0) return (MSError::BadReal); }
   }
  
  // Remove the commas and parse it
  MSUtil::remove(buf,',');
  
  // Looks OK after preliminary checks-now let's parse it
  int units,numerator,denominator;
  if (sscanf(buf,"%d %d/%d",&units,&numerator,&denominator)==3) 
   { 
     if (denominator>0)
       {
	 if (units<0)	// if number is negative, we need to subtract fractional part
	   {
	     _real=units-double(numerator)/double(denominator);
	   }
	 else	// if the number is non-negative, we need to add fractional part
	   {
	     _real=units+double(numerator)/double(denominator);
	   }
       }
     else
       {
	 return (MSError::BadReal);
       }
   } 
  else if (sscanf(buf,"%d/%d",&numerator,&denominator)==2) 
   { 
     if (denominator>0) _real=double(numerator)/double(denominator);
     else return (MSError::BadReal);
   } 
  else _real=atof(buf);
  
  if (isfinite(_real)>0) setToValid();
  return (MSError::MSSuccess);
}

void MSFloat::unset(void)
{
  if (isSet()==MSTrue)
   {
     _real=0.0;
     _flags=MSFloat::Valid; // clear the set bit
     changed();
   }
}

MSError::ErrorStatus MSFloat::set(const char *pString_) 
{ 
  MSError::ErrorStatus status=internalSet(pString_);
  return changed(),status;
}

MSFloat& MSFloat::operator=(const MSFloat& aFloat_)
{
  if (this!=&aFloat_)
   {
     _real=aFloat_._real;
     _flags=aFloat_._flags;
     changed();
   }
  return *this;
}

MSError::ErrorStatus MSFloat::set(double d)
{
  _real=d;
  _flags=MSFloat::Set; // clean the valid bit
  if (isfinite(_real)>0) setToValid();
  return changed(),(MSError::MSSuccess);
}

MSError::ErrorStatus MSFloat::set(const MSString *pString_)
{ return MSFloat::set(pString_->string()); }

MSError::ErrorStatus MSFloat::set(const MSString& aString_)
{ return MSFloat::set(aString_.string()); }

const char *MSFloat::format(MSString *pString_) const
{ return formatReal(*pString_,Decimal2,MSFormat::NoModifier,_real); }

const char *MSFloat::format(MSString& aString_) const
{ return format(&aString_); }

MSBoolean MSFloat::operator==(double d_) const
{
  d_-=_real;
  return MSBoolean(d_<MSFloatDelta&&-d_<MSFloatDelta);
}

MSFloat& MSFloat::operator+=(double r_)
{
  if (isSet()==MSTrue)
    {
      _real+=r_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator+=(int i_)
{
  if (isSet()==MSTrue)
    {
      _real+=i_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator-=(double r_)
{
  if (isSet()==MSTrue)
    {
      _real-=r_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator-=(int i_)
{
  if (isSet()==MSTrue)
    {
      _real-=i_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator*=(double r_)
{
  if (isSet()==MSTrue)
    {
      _real*=r_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator*=(int i_)
{
  if (isSet()==MSTrue)
    {
      _real*=i_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator/=(double r_)
{
  if (isSet()==MSTrue)
    {
      _real/=r_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator/=(int i_)
{
  if (isSet()==MSTrue)
    {
      _real/=i_;
      if (isfinite(_real)>0)
	{
	  setToValid();
	}
      else
	{
	  setToInValid();
	}
      changed();
    }

  return *this;
}


MSFloat& MSFloat::operator+=(const MSFloat& aFloat_)
{
  _flags &= aFloat_._flags;	// combine _flags with aFloat_._flags by ANDing them together;
				// the result will be valid and/or set only if *both* numbers are
				// valid and/or set, respectively
  _real+=aFloat_._real;
  if (isValid()==MSTrue)
    {
      if (isfinite(_real)<=0)
	{
	  setToInValid();
	}
    }
  //
  // We could have optimized this method for calls to changed() by providing a more
  // rigorous checking of the original state of both numbers.  For example, if "this" number
  // is *not* set originally, then we don't do anything to it and it stays unset; thus, we
  // don't need to call changed().  However, doing this proper testing of the valid and set
  // states requires additional bit extractions and comparisons.  Thus, we optimize for the
  // most common case - that is, when both numbers are set and valid, which probably accounts
  // for a good 99% of all situations - at the expense of some very rare cases, when "this"
  // number is either unset or invalid.
  //
  return changed(), *this;
}


MSFloat& MSFloat::operator+=(const MSInt& aInt_)
{
  // we are clearing the Valid bit and setting the Set bit to AND of Set state and 
  // aInt's Set state
  //
  _flags &= (aInt_.isSet()==MSTrue) ? Set : 0;
  _real+=int(aInt_);
  if (isfinite(_real)>0)
    {
      setToValid();
    }
      
  return changed(),*this;
}


MSFloat& MSFloat::operator*=(const MSFloat& aFloat_)
{
  _flags &= aFloat_._flags;	// combine _flags with aFloat_._flags by ANDing them together;
				// the result will be valid and/or set only if *both* numbers are
				// valid and/or set, respectively
  _real*=aFloat_._real;
  if (isValid()==MSTrue)
    {
      if (isfinite(_real)<=0)
	{
	  setToInValid();
	}
    }

  return changed(), *this;
}


MSFloat& MSFloat::operator*=(const MSInt& aInt_)
{
  _real*=int(aInt_);
  _flags &= (aInt_.isSet()==MSTrue) ? Set : 0;
  if (isfinite(_real)>0)
    {
      setToValid();
    }
      
  return changed(),*this;
}


MSFloat& MSFloat::operator-=(const MSFloat& aFloat_)
{
  _flags &= aFloat_._flags;	// combine _flags with aFloat_._flags by ANDing them together;
				// the result will be valid and/or set only if *both* numbers are
				// valid and/or set, respectively
  _real-=aFloat_._real;
  if (isValid()==MSTrue)
    {
      if (isfinite(_real)<=0)
	{
	  setToInValid();
	}
    }

  return changed(), *this;
}


MSFloat& MSFloat::operator-=(const MSInt& aInt_)
{
  _real-=int(aInt_);
  _flags &= (aInt_.isSet()==MSTrue) ? Set : 0;
  if (isfinite(_real)>0)
    {
      setToValid();
    }
      
  return changed(),*this;
}


MSFloat& MSFloat::operator/=(const MSFloat& aFloat_)
{
  _flags &= aFloat_._flags;	// combine _flags with aFloat_._flags by ANDing them together;
				// the result will be valid and/or set only if *both* numbers are
				// valid and/or set, respectively
  _real/=aFloat_._real;
  if (isValid()==MSTrue)
    {
      if (isfinite(_real)<=0)
	{
	  setToInValid();
	}
    }

  return changed(), *this;
}


MSFloat& MSFloat::operator/=(const MSInt& aInt_)
{
  _real/=int(aInt_);
  _flags &= (aInt_.isSet()==MSTrue) ? Set : 0;
  if (isfinite(_real)>0)
    {
      setToValid();
    }
      
  return changed(),*this;
}

// this code takes advantage of the return value optimization (RVO)
// discussed in section 12.1c of the ARM
MSFloat::MSFloat(const MSFloat& a_,const MSFloat& b_,MSFloat::FloatOperator operator_)
: _flags(a_._flags&b_._flags)
{
  if (isSet()==MSTrue)
    {
      switch (operator_)
	{
	case Plus:     _real=a_._real+b_._real; break;
	case Minus:    _real=a_._real-b_._real; break;
	case Multiply: _real=a_._real*b_._real; break;
	case Divide:   _real=a_._real/b_._real; break;
	}

      if (isValid()==MSTrue)	// if both a_ and b_ are valid, check if the result is valid
	{
	  if (isfinite(_real)<=0)
	    {
	      setToInValid();
	    }
	}
    }
  else
    {
      _real=0.0;
    }
}


MSFloat::MSFloat(const MSFloat& a_,const MSInt& b_,MSFloat::FloatOperator operator_)
{
  _flags = a_._flags & ((b_.isSet()==MSTrue) ? Set : 0); // set _flags to AND of the set states of a_ and b_
  
  if (isSet()==MSTrue)
    {
      switch (operator_)
	{
	case Plus:     _real=a_._real+(double)b_; break;
	case Minus:    _real=a_._real-(double)b_; break;
	case Multiply: _real=a_._real*(double)b_; break;
	case Divide:   _real=a_._real/(double)b_; break;
	}
      
      if (a_.isValid()==MSTrue)	// if a_ is valid (and we know that b_ is always valid), check if result is valid
	{
	  if (isfinite(_real)>0)
	    {
	      setToValid();
	    }
	}
    }
  else
    {
      _real=0.0;
    }
}


MSFloat::MSFloat(const MSInt& a_,const MSFloat& b_,MSFloat::FloatOperator operator_)
{
  _flags = ((a_.isSet()==MSTrue) ? Set : 0) & b_._flags; // set _flags to AND of the set states of a_ and b_

  if (isSet()==MSTrue)
    {
      switch (operator_)
	{
	case Plus:     _real=(double)a_+b_._real; break;
	case Minus:    _real=(double)a_-b_._real; break;
	case Multiply: _real=(double)a_*b_._real; break;
	case Divide:   _real=(double)a_/b_._real; break;
	}

      if (b_.isValid()==MSTrue)	// if b_ is valid (and we know that a_ is always valid), check if result is valid
	{
	  if (isfinite(_real)>0)
	    {
	      setToValid();
	    }
	}
    }
  else
    {
      _real=0.0;
    }
}

MSFloat::MSFloat(const MSFloat& a_,double b_,MSFloat::FloatOperator operator_)
: _flags(a_._flags)
{
  if (isSet()==MSTrue)
    {
      switch (operator_)
	{
	case Plus:     _real=a_._real+b_; break;
	case Minus:    _real=a_._real-b_; break;
	case Multiply: _real=a_._real*b_; break;
	case Divide:   _real=a_._real/b_; break;
	}

      if (isValid()==MSTrue)
	{
	  if (isfinite(_real)<=0)
	    {
	      setToInValid();
	    }
	}
    }
  else
    {
      _real=0.0;
    }
}

MSFloat::MSFloat(double a_,const MSFloat& b_,MSFloat::FloatOperator operator_)
: _flags(b_._flags)
{
  if (isSet()==MSTrue)
    {
      switch (operator_)
	{
	case Plus:     _real=a_+b_._real; break;
	case Minus:    _real=a_-b_._real; break;
	case Multiply: _real=a_*b_._real; break;
	case Divide:   _real=a_/b_._real; break;
	}

      if (isValid()==MSTrue)
	{
	  if (isfinite(_real)<=0)
	    {
	      setToInValid();
	    }
	}
    }
  else
    {
      _real=0.0;
    }
}

// these two functions are only provided to save the finite check-i.e.
// we could have casted the int to double and used the above constructors.
MSFloat::MSFloat(const MSFloat& a_,int b_,MSFloat::FloatOperator operator_)
: _flags(a_._flags)
{
  if (isSet()==MSTrue)
    {
      switch (operator_)
	{
	case Plus:     _real=a_._real+b_; break;
	case Minus:    _real=a_._real-b_; break;
	case Multiply: _real=a_._real*b_; break;
	case Divide:   _real=a_._real/b_; break;
	}

      if (isValid()==MSTrue)
	{
	  if (isfinite(_real)<=0)
	    {
	      setToInValid();
	    }
	}
    }
  else
    {
      _real=0.0;
    }
}


MSFloat::MSFloat(int a_,const MSFloat& b_,MSFloat::FloatOperator operator_)
: _flags(b_._flags)
{
  if (isSet()==MSTrue)
    {
      switch (operator_)
	{
	case Plus:     _real=a_+b_._real; break;
	case Minus:    _real=a_-b_._real; break;
	case Multiply: _real=a_*b_._real; break;
	case Divide:   _real=a_/b_._real; break;
	}

      if (isValid()==MSTrue)
	{
	  if (isfinite(_real)<=0)
	    {
	      setToInValid();
	    }
	}
    }
  else
    {
      _real=0.0;
    }
}

MSFloat operator+(const MSFloat& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Plus); }    
MSFloat operator-(const MSFloat& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Minus); }   
MSFloat operator*(const MSFloat& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Multiply);} 
MSFloat operator/(const MSFloat& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Divide); }  

MSFloat operator+(const MSFloat& a_,const MSInt& b_) { return MSFloat(a_,b_,MSFloat::Plus); }    
MSFloat operator-(const MSFloat& a_,const MSInt& b_) { return MSFloat(a_,b_,MSFloat::Minus); }   
MSFloat operator*(const MSFloat& a_,const MSInt& b_) { return MSFloat(a_,b_,MSFloat::Multiply);} 
MSFloat operator/(const MSFloat& a_,const MSInt& b_) { return MSFloat(a_,b_,MSFloat::Divide); }  

MSFloat operator+(const MSInt& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Plus); }    
MSFloat operator-(const MSInt& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Minus); }   
MSFloat operator*(const MSInt& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Multiply);} 
MSFloat operator/(const MSInt& a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Divide); }  

MSFloat operator+(const MSFloat& a_,double b_) { return MSFloat(a_,b_,MSFloat::Plus); }    
MSFloat operator-(const MSFloat& a_,double b_) { return MSFloat(a_,b_,MSFloat::Minus); }   
MSFloat operator*(const MSFloat& a_,double b_) { return MSFloat(a_,b_,MSFloat::Multiply);} 
MSFloat operator/(const MSFloat& a_,double b_) { return MSFloat(a_,b_,MSFloat::Divide); }  

MSFloat operator+(double a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Plus); }    
MSFloat operator-(double a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Minus); }   
MSFloat operator*(double a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Multiply);} 
MSFloat operator/(double a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Divide); }  

MSFloat operator+(const MSFloat& a_,int b_) { return MSFloat(a_,b_,MSFloat::Plus); }    
MSFloat operator-(const MSFloat& a_,int b_) { return MSFloat(a_,b_,MSFloat::Minus); }   
MSFloat operator*(const MSFloat& a_,int b_) { return MSFloat(a_,b_,MSFloat::Multiply);} 
MSFloat operator/(const MSFloat& a_,int b_) { return MSFloat(a_,b_,MSFloat::Divide); }  

MSFloat operator+(int a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Plus); }    
MSFloat operator-(int a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Minus); }   
MSFloat operator*(int a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Multiply);} 
MSFloat operator/(int a_,const MSFloat& b_) { return MSFloat(a_,b_,MSFloat::Divide); }

const char *MSFloat::format(MSString& aString_,const MSFormat& aFormat_) const
{
  return (aFormat_.formatType()==MSFormat::Float)?
  formatReal(aString_,aFormat_.floatFormat(),aFormat_.formatModifier(),_real):format(aString_);
}
const char *MSFloat::format(MSString *pString_,const MSFormat& aFormat_) const
{ return format(*pString_,aFormat_); }

const char *MSFloat::format(MSString& aString_,MSFloatFormat format_) const
{ return formatReal(aString_,format_,MSFormat::NoModifier,_real); }
const char *MSFloat::format(MSString *pString_,MSFloatFormat format_) const
{ return formatReal(*pString_,format_,MSFormat::NoModifier,_real); }

// MSUtil::comma is too inefficient, and one reason is becuase it does useful
// checking to avoid writing past the end of a string (another reason may
// be that it doesn't use "register" storage).  We need as efficient
// a function as possible, otherwise we will spend more time here than
// anywhere else. addCommas runs in about 1/100 of the time of MSUtil::comma.
// Plan on having enough space in outBuffer_. 
static void addCommas(register char *inBuffer_,register char *outBuffer_)
{
  // addCommas takes the number string in inBuffer_ and copies it to
  // outBuffer_,adding commas between the first character and the decimal
  // point according to American convention.  If inBuffer_ does not have
  // a '.', then no commas are added.  
  register char *cp;   // pointer to the decimal point
  register int places; // number of digits to the left of the decimal point
  
  // ignore leading minus sign
  if (*inBuffer_=='-')
  *outBuffer_++=*inBuffer_++;
  
  cp=strchr(inBuffer_,'.');
  // No commas need to be added, just do the copy.
  if ((cp==NULL)||((places=cp-inBuffer_)<=3)) strcpy(outBuffer_,inBuffer_);
  else
   {
     // copy 1 to 3 digits before the first comma.
     register int remainder=((places-1) % 3)+1; 
     while (remainder-->0)
     *outBuffer_++=*inBuffer_++;
     
     // copy a comma and 3 digits until we format to the decimal point.
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
}

const char *MSFloat::formatReal(MSString& aString_,MSFloatFormat format_,unsigned long modifier_,double value_) const
{
  if (isSet()==MSFalse) { aString_=""; return aString_.string(); }
  if (isValid()==MSFalse)
   {
     aString_="?";
     MSError::error(MSError::MSFailure,"MSFloat::MSFloatFormat","Invalid Value");
     return aString_.string();
   }
  // use MSFloat fudge factor to check for "close-to-zero"
  if (format_!=MaximumPrecision)
   {
     if (value_==MSFloat(0.0)) value_=0.0;
   }
  double value=value_;
  char modifierChar='\0';
  if (modifier_&MSFormat::UpperCaseK)
   {
     value=value_/1000.0;
     modifierChar='K';
   }
  else if (modifier_&MSFormat::LowerCaseK)
   {
     value=value_/1000.0;
     modifierChar='k';
   }
  else if (modifier_&MSFormat::UpperCaseM)
   {
     value=value_/1000000.0;
     modifierChar='M';
   }
  else if (modifier_&MSFormat::LowerCaseM)
   {
     value=value_/1000000.0;
     modifierChar='m';
   }
  char buf[512],newbuf[512];
  switch (format_) 
   {
   case Decimal0: sprintf(buf,"%.0f",value); aString_=buf; break;
   case Decimal1: sprintf(buf,"%.1f",value); aString_=buf; break;
   case Decimal2: sprintf(buf,"%.2f",value); aString_=buf; break;
   case Decimal3: sprintf(buf,"%.3f",value); aString_=buf; break;
   case Decimal4: sprintf(buf,"%.4f",value); aString_=buf; break;
   case Decimal5: sprintf(buf,"%.5f",value); aString_=buf; break;
   case Decimal6: sprintf(buf,"%.6f",value); aString_=buf; break;
   case Decimal7: sprintf(buf,"%.7f",value); aString_=buf; break;
   case Decimal8: sprintf(buf,"%.8f",value); aString_=buf; break;
   case CommaDecimal0:
     // trailing '.' required for addCommas
     sprintf(buf,"%.0f.",value); addCommas(buf,newbuf); aString_=newbuf;
     // remove trailing '.'
     aString_.truncate(1); break;
   case CommaDecimal1:   sprintf(buf,"%.1f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case CommaDecimal2:   sprintf(buf,"%.2f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case CommaDecimal3:   sprintf(buf,"%.3f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case CommaDecimal4:   sprintf(buf,"%.4f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case CommaDecimal5:   sprintf(buf,"%.5f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case CommaDecimal6:   sprintf(buf,"%.6f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case CommaDecimal7:   sprintf(buf,"%.7f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case CommaDecimal8:   sprintf(buf,"%.8f",value);         addCommas(buf,newbuf); aString_=newbuf; break;
   case MaximumPrecision: sprintf(buf,"%.*g",DBL_DIG,value);                        aString_=buf; break;
   case Default:         sprintf(buf,"%g",value);                                  aString_=buf; break;
   default:
    // to be replaced with an Exception
    MSError::error(MSError::MSFailure,"MSFloat::MSFloatFormat","Invalid Format");
    return format(aString_);
   }
  if (modifierChar!='\0') aString_<<modifierChar;
  if (modifier_&MSFormat::Parenthesis)
   {
     if (value<0.0&&aString_.length()>0)
      {
	if (aString_(0)=='-') aString_[0]='(';
	else aString_.insert("(");
	aString_<<")";
      }
   }
  return aString_.string();
}

