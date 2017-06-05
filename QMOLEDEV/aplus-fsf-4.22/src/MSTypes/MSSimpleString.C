///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <string.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSSimpleString.H>
#include <MSTypes/MSMessageLog.H>

static void fastcopy(const char *src_,char *dst_,int count_)
{
  memcpy((char *)dst_,(const char *)src_,count_);
}

char MSSimpleString::_badData='\0';

// this originally printed "<empty>", if the string was null
ostream& operator<<(ostream& os_,const MSSimpleString& s_)
{ return os_<<((s_.string()!=0)?s_.string():"")<<endl; }

void MSSimpleString::indexError(unsigned i_,unsigned l_) const 
{ MSMessageLog::errorMessage("MSSimpleString Index Error: index %d\tlength %d\n",i_,l_); }

MSSimpleString::MSSimpleString(void) 
{ _string=0,_length=0; }
MSSimpleString::MSSimpleString(const MSSimpleString& aString) 
{ duplicate(aString.string()); }
MSSimpleString::MSSimpleString(const MSSimpleString& aString,const MSSimpleString& bString)
{
  _length=aString.length()+bString.length();
  _string=new char[length()+1];
  if (length()>0)
   {
     char *dp=_string;
     if (aString.length()>0) fastcopy(aString.string(),dp,aString.length());
     if (bString.length()>0) fastcopy(bString.string(),dp+aString.length(),bString.length());
   }
  _string[length()]='\0';
}

MSSimpleString::MSSimpleString(const MSSimpleString& aString,const char *pString)
{
  unsigned slen=(pString!=0)?strlen(pString):0;
  _length=aString.length()+slen;
  _string=new char[length()+1];
  if (length()>0)
   {
     char *dp=_string;
     if (aString.length()>0) fastcopy(aString.string(),dp,aString.length());
     if (slen>0) fastcopy(pString,dp+aString.length(),slen);
   }
  _string[length()]='\0';
}

MSSimpleString::MSSimpleString(const char *pString,const MSSimpleString& aString)
{
  unsigned slen=(pString!=0)?strlen(pString):0;
  _length=aString.length()+slen;
  _string=new char[length()+1];
  if (length()>0)
   {
     char *dp=_string;
     if (slen>0) fastcopy(pString,dp,slen);
     if (aString.length()>0) fastcopy(aString.string(),dp+slen,aString.length());
   }
  _string[length()]='\0';
}

MSSimpleString::MSSimpleString(const MSSimpleString& aString,char aChar)
{
  _length=1+aString.length();
  _string=new char[length()+1];
  if (aString.length()>0) fastcopy(aString.string(),_string,aString.length());
  _string[length()-1]=aChar;  
  _string[length()]='\0';
}

MSSimpleString::MSSimpleString(char aChar,const MSSimpleString& aString)
{
  _length=1+aString.length();
  _string=new char[length()+1];
  _string[0]=aChar;
  if (aString.length()>0) fastcopy(aString.string(),_string+1,aString.length());
  _string[length()]='\0';
}

MSSimpleString::MSSimpleString(const char *pString) 
{ duplicate(pString); }

MSSimpleString::~MSSimpleString(void) 
{ if (string()!=0)  delete [] _string; }

void MSSimpleString::duplicate(const char *pString)
{
  if (pString!=0)
   {
     _length=strlen(pString);
     _string=new char[length()+1];
     fastcopy(pString,_string,strlen(pString));
     _string[length()]='\0';
   }
  else 
   {
     _string=0,_length=0;
   }
}

MSSimpleString& MSSimpleString::catenate(const char *pString)
{
  if (pString!=0)
   { 
     unsigned  slen=strlen(pString);
     unsigned  len=length()+slen;
     char     *temp=new char[len+1];
     
     if (string()!=0) fastcopy(string(),temp,length());
     fastcopy(pString,temp+length(),slen);
     if (string()!=0) delete [] _string;
     _string=temp;
     _length=len;
   }
  return *this;
}

void MSSimpleString::string(const char *pString)
{
  if (pString!=0) 
   {
     char *s=_string;
     duplicate(pString);
     if (s!=0) delete [] s;
   }
  else
   {
     _length=0;
     if (string()!=0) 
      {
        delete [] _string;
	_string=0;
      }
   }
}

MSBoolean MSSimpleString::operator==(const MSSimpleString& aString) const
{
  if (length()!=aString.length()) return MSFalse;
  if (string()==0&&aString.string()==0) return MSTrue;
  if (string()!=0&&aString.string()!=0)
   { return (strcmp(string(),aString.string())==0)?MSTrue:MSFalse; }
  return MSFalse;
}

MSBoolean MSSimpleString::operator!=(const MSSimpleString& aString) const
{
  if (length()!=aString.length()) return MSTrue;
  if (string()==0&&aString.string()==0) return MSFalse;
  if (string()!=0&&aString.string()!=0)
   { return (strcmp(string(),aString.string())==0)?MSFalse:MSTrue; }
  return MSTrue;
}

MSSimpleString& MSSimpleString::operator=(const MSSimpleString& aString)
{
  if (this==&aString) return *this;
  return operator=(aString.string());
}

MSSimpleString& MSSimpleString::operator=(const char *pString)
{
  string(pString);
  return *this;
}

MSSimpleString& MSSimpleString::operator=(char aChar)
{
  char buf[2];
  buf[0]=aChar;
  buf[1]='\0';
  string(buf);
  return *this;
}

MSSimpleString& MSSimpleString::operator<<=(char aChar)
{
  char buf[2];
  buf[0]=aChar;
  buf[1]='\0';
  catenate(buf);
  return *this;
}

MSSimpleString& MSSimpleString::operator<<(const MSSimpleString &aString)
{
  return operator<<= (aString);
}

MSSimpleString& MSSimpleString::operator<<(const char *pString)
{
  return operator<<= (pString);
}

MSSimpleString& MSSimpleString::operator<<(char aChar)
{
  return operator<<= (aChar);
}


MSSimpleString operator+(const MSSimpleString& aString,const MSSimpleString& bString) 
{ return MSSimpleString(aString,bString); }
MSSimpleString operator+(const MSSimpleString& aString,const char *pString)   
{ return MSSimpleString(aString,pString); }
MSSimpleString operator+(const MSSimpleString& aString,char aChar)          
{ return MSSimpleString(aString,aChar); }
MSSimpleString operator+(const char *pString,const MSSimpleString& aString)   
{ return MSSimpleString(pString,aString); }
MSSimpleString operator+(char aChar,const MSSimpleString& aString)          
{ return MSSimpleString(aChar,aString); }

