///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <MSTypes/MSUtil.H>

int aplus_isspace(int c) { return isspace(c); }

// if a string contains any alphabetic characters return true
// otherwise, return false
MSBoolean MSUtil::hasAlpha(register const char *s_)
{
  if(*s_=='\0')  return MSFalse;
  for(;*s_!='\0';s_++) if(isalpha(*s_)) return MSTrue;
  return MSFalse;
}

void MSUtil::comma(const char *fromBuffer_,char *toBuffer_,int size_)
{
  int places,len,i,j;
  char *cp;
  
  if ((cp=strchr(fromBuffer_,'.'))!=NULL)
   {
     places=cp-fromBuffer_;
     for (i=0,j=0,len=strlen(fromBuffer_); j<=len&&i<(size_-1); j++,i++)
      {
	// move the digits from left to right
	if (places>0&&(places%3)==0&&j>0&&(fromBuffer_[j-1]!='-'))
	 {
	   toBuffer_[i]=',';
	   i++;
	   if (i>=(size_-1))
	    {
	      continue;
	    }
	 }
	toBuffer_[i]=fromBuffer_[j];
	places--;
      }
   }
  else
   {
     strncpy(toBuffer_,fromBuffer_,size_);
   }
}

// remove all instances of character c from string s
void MSUtil::remove(register char *s_,register int c_)
{
  register char	*t;
  for (t=s_;*t;t++)
   {
     if (*t!=c_) *s_++=*t;
   }
  *s_='\0';
  return;
}

// if the null-terminated string is not the null string and contains only
// space characters return true, otherwise return false
MSBoolean MSUtil::isSpace(register const char *s_)
{
  if(*s_=='\0') return MSFalse;
  for(;*s_;s_++) if(*s_!=' ') return MSFalse;
  return MSTrue;
}

// if the null-terminated string contains only digits return true
// else return false
MSBoolean MSUtil::isNumeric(register const char *s_)
{
  for(;*s_!='\0';s_++)
  if(!isascii(*s_)||!isdigit(*s_)) return MSFalse;
  return MSTrue;
}


