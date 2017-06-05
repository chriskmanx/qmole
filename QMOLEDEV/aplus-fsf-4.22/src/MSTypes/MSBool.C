///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSBool.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSFormat.H>
#include <MSTypes/MSMessageLog.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSBoolInlines.C>
#endif

ostream& operator<<(ostream& aStream_,const MSBool& aBool_)
{ return aStream_<<(aBool_._bool==MSTrue?"1":"0"); }

istream &operator>>(istream &aStream_,MSBool &aBool_)
{
  MSString aString;
  aStream_>>aString;
  if (aString.length()>0) aBool_.set(aString);
  return aStream_;
}

MSString MSBool::asString(void) const
{ 
  if (_bool==MSTrue) return MSString("1");
  else return MSString("0");
}

MSString MSBool::asDebugInfo(void) const
{
  MSString result("MSBool(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_bool=";
  result+=_bool==MSTrue?"MSTrue":"MSFalse";
  result+=",_isSet=";
  result+=_isSet==MSTrue?"MSTrue":"MSFalse";
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}
  
MSString MSBool::className(void) const
{ return MSString("MSBool"); }

const MSSymbol& MSBool::type(void) const
{ return symbol(); }

MSModel *MSBool::clone(void) const
{ return new MSBool(*this); }

MSModel *MSBool::create(void) const
{ return new MSBool(); }

void MSBool::assign(const MSModel& aModel_)
{ *this=(MSBool&)aModel_; }

long MSBool::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSBool&)aModel_); }

MSString MSBool::asMSF(void) const
{ 
  if (isSet()==MSTrue)
   {
    if(_bool==MSTrue) return MSString("T");
    else return MSString("F");
  }
  return MSString();
}

MSError::ErrorStatus MSBool::setFromMSF(const char *pString_)
{ 
  int code;
  if (pString_!=0)
  {
    if (*pString_=='T') code=set(1);
    else code=set(0);
  }
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

const MSSymbol& MSBool::symbol(void)    
{
  static MSSymbol sym ("MSBool");
  return sym;
}

void MSBool::unset(void)
{
  if (isSet()==MSTrue)
   {
     _bool=MSFalse;
     _isSet=MSFalse;
     changed();
   }
}

MSError::ErrorStatus MSBool::set(int i_)
{
  _bool=i_?MSTrue:MSFalse;
  _isSet=MSTrue;
  return changed(),MSError::MSSuccess;
}

MSError::ErrorStatus MSBool::set(const MSString& aString_)
{
  MSString aString=aString_;
  aString.strip();	// remove leading and trailing whitespace
  aString.lower();
  if(aString=="0"||aString=="no"||aString=="n"||aString=="false"||aString=="f")
   { _bool=MSFalse;_isSet=MSTrue; return changed(),MSError::MSSuccess; }
  if(aString=="1"||aString=="yes"||aString=="y"||aString=="true"||aString=="t")
   { _bool=MSTrue;_isSet=MSTrue; return changed(),MSError::MSSuccess; }
  _isSet=MSFalse; 
  return changed(),MSError::BadBool;
}

MSError::ErrorStatus MSBool::set(const char *pString_)
{   return set(MSString(pString_)); }

MSError::ErrorStatus MSBool::set(const MSString *pString_)
{ return set(*pString_); }

const char *MSBool::format(MSString *pString_) const
{ return format(*pString_); }
const char *MSBool::format(MSString& aString_) const
{
  aString_=_bool?"1":"0";
  return aString_.string();
}

const char *MSBool::format(MSString *pString_,MSBoolFormat format_) const
{ return format(*pString_,format_); }

const char *MSBool::format(MSString& aString_,MSBoolFormat format_) const
{
  switch (format_)
   {
   case YesAndNo:     aString_=(_bool==MSTrue)?"Yes":"No";     break;
   case TrueAndFalse: aString_=(_bool==MSTrue)?"True":"False"; break;
   case Binary:       aString_=(_bool==MSTrue)?"1":"0";        break;
   default:           MSMessageLog::warningMessage("MSBool: invalid value of format\n");
   }
  return aString_.string();
}

const char *MSBool::format(MSString& aString_,const MSFormat& aFormat_) const
{
  return (aFormat_.formatType()==MSFormat::FBool)?
  format(aString_,aFormat_.boolFormat()):format(aString_);
}

const char *MSBool::format(MSString *pString_,const MSFormat& aFormat_) const
{ return format(*pString_,aFormat_); } 

MSBoolean operator==(int i_,const MSBool& aBoolean_)
{ return MSBoolean(i_?aBoolean_._bool:!aBoolean_._bool); }
MSBoolean operator!=(int i_,const MSBool& aBoolean_)
{ return MSBoolean(i_?!aBoolean_._bool:aBoolean_._bool); }

#if !defined (MS_ENUM_COMPARE_BUG)
MSBoolean operator==(MSBoolean bool_,const MSBool& aBoolean_)
{ return MSBoolean(bool_==aBoolean_._bool); }
MSBoolean operator!=(MSBoolean bool_,const MSBool& aBoolean_)
{ return MSBoolean(bool_!=aBoolean_._bool); }
#endif





