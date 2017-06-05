///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

extern "C"
{
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
}

#include <MSTypes/MSMBStringBuffer.H>

#ifndef MSStringEnumHEADER
#include <MSTypes/MSStringEnum.H>
#endif

#ifndef MSStringTestHEADER
#include <MSTypes/MSStringTest.H>
#endif

/*------------------------------------------------------------------------------
| createMSMBStringBuffer(void)                                                 |
| used by MSStringBuffer to create MSMSStringBuffer objects                    |
------------------------------------------------------------------------------*/
MSStringBuffer *createMSMBStringBuffer(void)
{ return new (0) MSMBStringBuffer(0); }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::className                                                  |
------------------------------------------------------------------------------*/
const char *MSMBStringBuffer::className(void) const
{ return "MSMBStringBuffer"; }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::MSMBStringBuffer                                           |
|                                                                              |
| Construct a buffer of the specified length.  The "data" member               |
| array must actually be of length 1 greater than the argument                 |
| value (this is achieved automatically via use of the overloaded              |
| operator new for class MSStringBuffer).                                      |
|                                                                              |
| The terminating (extra) byte is set to null.                                 |
|                                                                              |
| Notes:                                                                       |
|   1. This method is protected.  MSMBStringBufferss must be obtained by using |
|      MSMBStringBuffer::nullBuffer and subsequent newBuffer calls to existing |
|      MSMBStringBuffer objects.  The only non-heap instance of this class is  |
|      the static MSMBStringBuffer::nullBuffer object.                         |
------------------------------------------------------------------------------*/
MSMBStringBuffer::MSMBStringBuffer(unsigned len) : MSStringBuffer(len) 
{}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::~MSMBStringBuffer                                          |
|                                                                              |
| Empty dtor to prevent generation of static version.                          |
------------------------------------------------------------------------------*/
MSMBStringBuffer::~MSMBStringBuffer(void)
{}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::allocate                                                   |
|                                                                              |
|  Allocate a new MSStringBuffer of the same type as the receiver              |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::allocate(unsigned newLen) const
{ return new (newLen)MSMBStringBuffer(newLen); }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::rightJustify                                               |
|                                                                              |
| This function right justifies the receiver in a new buffer of the            |
| given length and returns the address of the new buffer.                      |
|                                                                              |
| The right justification algorithm is as follows:                             |
|                                                                              |
|   1. Calculate how much padding will be needed on the left.                  |
|   2. Calculate how many bytes of the current buffer will be                  |
|      transferred to the right-justified result.                              |
|   3. Determine if the first byte of the new buffer is the 2nd byte           |
|      of a DBCS character.  If so,replace it with the padCharacter.           |
|   4. return a new buffer comprised of the "prefix" pad bytes                 |
|      and the portion of the receiver calculated at step 2.                   |
|                                                                              |
| Notes:                                                                       |
|   1. This function (and likewise all the similar "editing" functions)        |
|      is usually called in the following context:                             |
|         MSString::xxxxx()                                                    |
|           {                                                                  |
|           ...                                                                |
|           MSString old(pBuffer);                                             |
|           pBuffer=old.pBuffer->rightJustify(...);                            |
|           ...                                                                |
|           }                                                                  |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::rightJustify(unsigned newLength,char padCharacter)
{
  MSStringBuffer *result=this;

  if (newLength!=length())
   {	
     unsigned prefix=(newLength>length())?newLength-length():0;
     unsigned fromReceiver=(length()<newLength)?length():newLength;

     result=newBuffer(0,prefix,
		      contents()+length()-fromReceiver,
		      fromReceiver,
		      0,0,
		      padCharacter);

     //-----------------------------------------------------------------------
     // Check to see if the first byte of the returned string is a DBCS2.
     // If so,then it is replaced with the padCharacter.
     //-----------------------------------------------------------------------
     if (fromReceiver<length())
      {
	for (unsigned i=0;i<fromReceiver;i++)
	 {	
	   if (charType(length()-fromReceiver+1+i)>MSStringEnum::DBCS1)  // should be MBCS1 
            {
	      result->contents()[i]=padCharacter;
	    }
	 }
      }	
   }
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::remove                                                     |
|                                                                              |
| If the buffer will remain unmodified (starting index is beyond               |
| the end or receiver is null),then the receiver is returned                   |
| unmodified.                                                                  |
|                                                                              |
| Else,if the number of characters to delete is zero,or passes                 |
| the end of the receiver,then it is defaulted to the rest of the              |
| buffer.  Then,check to see if the result of removing the characters          |
| will split a DBCS character on either end.  If so,replace that               |
| character with a SBCS space. Finally,the new buffer is allocated             |
| and filled in via a call to newBuffer().                                     |
|                                                                              |
| Notes:                                                                       |
|   1. See notes 1. and 2. under MSMBStringBuffer::rightJustify.               |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::remove(unsigned startPos,unsigned numChars)
{
  MSStringBuffer *result=this;

  if (startPos<=length()&&length()!=0)
  {
    //------------------------------------------------------------------------
    // Consider startPos 0 to be 1:
    //------------------------------------------------------------------------
    if (startPos==0) startPos++;

    //------------------------------------------------------------------------
    // Default numChars to rest of string:
    //------------------------------------------------------------------------
    if (numChars>length()-startPos) numChars=length()-startPos+1;

    //------------------------------------------------------------------------
    // Initialize from current contents before/after deleted chars:
    //------------------------------------------------------------------------
    result=newBuffer(contents(),startPos-1,
		     contents()+startPos+numChars-1,
		     length()-numChars-startPos+1,
		     0,0,
		     0);

    //------------------------------------------------------------------------
    // If the byte at (startPos-1) is the first byte of a DBCS character,
    // then that byte should be replaced with a SBCS space.
    // If the byte at (startPos+numChars) is the second byte of a DBCS
    // character,then that byte should be replaced with a SBCS space.
    //------------------------------------------------------------------------
    if (startPos<=length())
     {
       MSStringEnum::CharType typeOfFirstRemoved=charType(startPos);
       if (typeOfFirstRemoved>MSStringEnum::DBCS1)
	{
	  for (unsigned i=1;i<typeOfFirstRemoved&&i<startPos;i++)
	   {
	     result->contents()[startPos-1-i]=' ';
	   }
	}
     }

    for (unsigned i=0;startPos+numChars+i<=length();i++)
     {
       if (charType(startPos+numChars+i)>MSStringEnum::DBCS1) result->contents()[startPos-1+i]=' ';
     }
  }
  return result;
}

size_t MSMBStringBuffer::prevCharLength(unsigned pos) const
{
  if (pos==1) return 1;
  else
   {
     MSStringEnum::CharType ctype=charType(pos-1);
     if (ctype==MSStringEnum::SBCS) return 1;
     else return (size_t) ctype;
   }
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::isMBCS                                                     |
|                                                                              |
| isMBCS returns MSTrue if the string consists of only MBCS characters         |
------------------------------------------------------------------------------*/
MSBoolean MSMBStringBuffer::isMBCS() const
{
  unsigned    i;
  const char *p=contents();
  
  for (i=0;i<length();)
   {
     if (isSBC(p)) return MSFalse;
     p+=2;
     i+=2;
   }
  return MSTrue;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::isSBCS                                                     |
|                                                                              |
| isSBCS returns MSTrue if the string consists of only SBCS characters         |
------------------------------------------------------------------------------*/
MSBoolean MSMBStringBuffer::isSBCS() const
{
  unsigned    i;
  const char *p=contents();
  
  for (i=0;i<length();i++)
   {
     if (!isSBC(p)) return MSFalse;
     p++;
   }
  return MSTrue;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::isValidMBCS                                                |
|                                                                              |
| isValidMBCS checks to see if a null character (\0) appears at the second     |
| byte position of multi-byte characters.                                      |
------------------------------------------------------------------------------*/
MSBoolean MSMBStringBuffer::isValidMBCS() const
 {
   unsigned    i;
   const char *p=contents();
   
   for (i=0;i<length();i++)
    {
      unsigned nextChar=i+charLength(p);
      p++;
      for (;i<nextChar;i++,p++)
       {
	 if (*p=='\0') return MSFalse;
       }
    }
   return MSTrue;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::includesMBCS                                               |
|                                                                              |
| includesMBCS returns MSTrue if the string contains any MBCS characters       |
------------------------------------------------------------------------------*/
MSBoolean MSMBStringBuffer::includesMBCS() const
{ return MSBoolean(isSBCS()==MSFalse); }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::includesSBCS                                               |
|                                                                              |
| includesSBCS returns MSTrue if the string contains any SBCS characters       |
------------------------------------------------------------------------------*/
MSBoolean MSMBStringBuffer::includesSBCS() const
{ return MSBoolean(isMBCS()==MSFalse); }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::next                                                       |
|                                                                              |
| next returns a pointer to the next logical character in a string.            |
|                                                                              |
| Notes:                                                                       |
|   This function makes the assumption that the input character string         |
|   (pChars) is either pointing to a SBCS character or the first byte of a     |
|   DBCS character i.e. pChars is NOT pointing to the 2nd byte of a DBCS       |
|   character.                                                                 |
------------------------------------------------------------------------------*/
char* MSMBStringBuffer::next(const char *pChars)
{ return((char*)pChars+charLength(pChars)); }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::next                                                       |
------------------------------------------------------------------------------*/
const char* MSMBStringBuffer::next(const char *pChars) const
{ return((char*)pChars+charLength(pChars)); }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::charType                                                   |
|                                                                              |
| This function evaluates the byte at position index in a string and returns   |
| SBCS if the byte is a single byte character,DBCS1 if the byte is the first   |
| byte of a double byte character,and DBCS2 if the byte is the second byte     |
| of a double byte character.                                                  |
|                                                                              |
| Notes:                                                                       |
|   All invocations of charType from within MSMBStringBuffer functions ensure  |
|   that the index is valid (i.e. index>0 and index<=length()) before    |
|   calling charType.                                                          |
------------------------------------------------------------------------------*/
MSStringEnum::CharType MSMBStringBuffer::charType(unsigned index) const
{
  unsigned i=0;
  
  for (;;)
   { 
     unsigned next=i+charLength(&contents()[i]);
     if (next>=index)
      {
	if (next-i==1) return MSStringEnum::SBCS;
	else return MSStringEnum::CharType(index-i);
      }
     else i=next;
   }
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::isCharValid                                               |
|                                                                             |
| This function determines if a character,pChar (which can be either a SBCS   |
| or DBCS character) is in the string of valid characters,pValidChars (which  |
| can contain any combination of SBCS and DBCS characters).                   |
------------------------------------------------------------------------------*/
MSBoolean MSMBStringBuffer::isCharValid(unsigned pos,const char *pValidChars,unsigned numValidChars) const
{
  char const *c=&contents()[pos-1];
  size_t lc=charLength(c);
  
  while (numValidChars>=lc)
   { 
     size_t len=charLength(pValidChars);
     if (len==lc)
      {
	MSBoolean match=MSTrue;
	for (unsigned i=0;i<len;i++)
	 {
	   if (pValidChars [i]!=c [i])
	    {
	      match=MSFalse;
	      break;
	    }
	 }
	if (match) return MSTrue;
      }
     pValidChars+=len;
     numValidChars-=len;
   }
  return MSFalse;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::subString                                                  |
|                                                                              |
| Allocate a new buffer by calling the inherited version of this function.     |
|                                                                              |
| Then,if the substring split a DBCS character at either end,convert that      |
| character to the pad character.                                              |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::subString(unsigned startPos,unsigned len,char padCharacter) const
{
  MSStringBuffer *result;
  
  if (startPos==0) startPos=0;
  
  //-------------------------------------------------------------------------
  // Calculate how much can come from receiver.
  //-------------------------------------------------------------------------
  unsigned int fromReceiver=(startPos<=length())?length()-startPos+1:0;
  
  //-------------------------------------------------------------------------
  // Default length to rest of string.
  //-------------------------------------------------------------------------
  if (len<fromReceiver) fromReceiver=len;
  
  //-------------------------------------------------------------------------
  // Initialize what we can from the receiver and leave
  // room for the remainder.
  //-------------------------------------------------------------------------
  result=newBuffer(contents()+startPos-1,fromReceiver,
		   0,len-fromReceiver,
		   0,0,
		   padCharacter);
  
  //-------------------------------------------------------------------------
  // If the byte at startPos is the second byte of a DBCS character,
  // then that byte should be replaced with a padCharacter.
  // If the byte at (startPos+len-1) is the first byte of a DBCS
  // character,then that byte should be replaced with a padCharacter.
  //-------------------------------------------------------------------------
  for (unsigned i=0;startPos+i<=length();i++)
   {
     if (charType(startPos+i)>MSStringEnum::DBCS1) result->contents()[i]=padCharacter;
   }
  if (startPos+fromReceiver<=length())
   {
     MSStringEnum::CharType typeOfFirstAfterSubstring=charType(startPos+len);
     if (typeOfFirstAfterSubstring>MSStringEnum::DBCS1)
      {
	for (unsigned i=1;i<typeOfFirstAfterSubstring&&i<=len;i++) 
         {
	   result->contents()[len-i]=padCharacter;
	 }
      }
   }
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::startSearch                                                |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::startSearch(unsigned startPos,unsigned searchLen) const
{
  unsigned result=MSStringBuffer::startSearch(startPos,searchLen);
  while (charType(result)>MSStringEnum::DBCS1)
   {
     if (++result>length())
      {
	result=0;
	break;
      }
   }
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::startBackwardsSearch                                       |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::startBackwardsSearch(unsigned startPos,unsigned searchLen) const
{
  unsigned result=MSStringBuffer::startBackwardsSearch(startPos,searchLen);
  while (result>0&&charType(result)>MSStringEnum::DBCS1) result--;
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::indexOf                                                    |
|                                                                              |
| 1. First,check whether the search can succeed (and set the point at which    |
|    the search is to start) by calling startSearch().                         |
| 2. If the search cannot succeed,then return 0.                               |
| 3. Otherwise,compare the buffer contents starting at each index to the       |
|    search string until a match is found (in which case we return the index)  |
|    or the entire buffer is tested (in which case 0 is returned).             |
|                                                                              |
|                                                                              |
| Notes:                                                                       |
|   1. If the search string is a single character,then this function is        |
|      implemented via the equivalent invocation of indexOfAnyOf (which is     |
|      optimized for that case).  This saves the addtional logic of special    |
|      code to handle that case here,as well.                                  |
|   2. If the search string is null,then 0 is returned.                        |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::indexOf(const char *pSearchString,unsigned searchLen,unsigned startPos) const
{
  if (searchLen==1) return indexOfAnyOf(pSearchString,searchLen,startPos);
  
  startPos=startSearch(startPos,searchLen);
  if (startPos&&searchLen)
   {
     while (startPos<=length())
      {
	if (memcmp(contents()+startPos-1,pSearchString,searchLen)==0) return startPos;
	else startPos+=charLength(startPos);
      }
   }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::indexOf                                                    |
|                                                                              |
| Simply invokes the functionally equivalent indexOf function.                 |
|                                                                              |
| Notes:                                                                       |
|   1. If the search string is null,then 0 is returned.                        |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::indexOf(const MSStringTest &aTest,unsigned startPos) const
{ return MSStringBuffer::indexOf(aTest,startPos); }

/*------------------------------------------------------------------------------
| MSMBStringBuffer::indexOfAnyBut                                              |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the starting       |
|    point) by calling startSearch().                                          |
| 2. If it can't succeed,return 0.                                             |
| 3. If it can succeed,the starting point is converted to an array (0-based)   |
|    index.                                                                    |
| 4. Search for the first character not in the set of valid characters,based   |
|    on the number of valid characters:                                        |
|      0 -return 1;i.e.,the first character of the buffer                      |
|           isn't in the (empty) set of valid characters                       |
|      1 -compare each character of the buffer to the valid                    |
|           character until one doesn't match                                  |
|      >1-call memchr for each character of the buffer to see if the           |
|           character is in the set of valid characters;                       |
|           return the index of the first that isn't found                     |
|                                                                              |
| If all characters pass the test,return 0.                                    |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::indexOfAnyBut(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos)
    {
    switch(numValidChars)
      {
      case 0:
        return startPos;
      case 1:
        // No character can match single DBCS first-byte:
        if (! isSBC(pValidChars)) return startPos;
        // Examine each character of this string...
        while (startPos<=length())
	 {
	   // Compare this character.  Note that if current character
	   // is DBCS first-byte,this compare will fail.
	   if (contents()[startPos-1]!=*pValidChars) return startPos;
	   else startPos++;
	 }
        break;
      default:
        while (startPos<=length())
	 {
	   if (!isCharValid(startPos,pValidChars,numValidChars)) return startPos;
	   else startPos+=charLength(startPos);
	 }
      }
    }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::indexOfAnyBut                                              |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the starting       |
|    point) by calling startSearch().                                          |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSFalse result.                       |
|                                                                              |
| If all characters pass the test,return 0.                                    |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::indexOfAnyBut(const MSStringTest& aTest,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos)
   {
    while (startPos<=length())
     {
       // Note:  This is broken!  Must somehow pass both bytes of DBCS!
       if (!aTest.test(contents()[startPos-1])) return startPos;
       else startPos+=charLength(startPos);
     }
   }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::indexOfAnyOf                                               |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the starting       |
|    point) by calling startSearch().                                          |
| 2. If it can't succeed,return 0.                                             |
| 3. If it can succeed,the starting point is converted to an array (0-based)   |
|    index.                                                                    |
| 4. Search for the first character in the set of search characters,based      |
|    on the number of search characters:                                       |
|      0 -return 0;i.e.,no characters of the buffer are in the (empty)         |
|           set of search characters                                           |
|      1 -search for a matching character in the buffer using memchr           |
|      >1-call memchr for each character of the buffer to see if the           |
|           character is in the set of search characters;                      |
|           return the index of the first that is found                        |
|                                                                              |
|  If all characters fail the test,return 0.                                   |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::indexOfAnyOf(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos)
    {
    switch(numValidChars)
      {
      case 0:
        break;
      case 1:
        while (startPos<=length())
	 {
	   if (contents()[startPos-1]==*pValidChars) return startPos;
	   startPos+=charLength(startPos);
	 }
        break;
      default:
        while (startPos<=length())
	 {
	   if (isCharValid(startPos,pValidChars,numValidChars)) return startPos;
	   else startPos+=charLength(startPos);
	 }
      }
    }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::indexOfAnyOf                                               |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the starting       |
|    point) by calling startSearch().                                          |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSTrue result.                        |
|                                                                              |
| If all characters fail the test,return 0.                                    |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::indexOfAnyOf(const MSStringTest &aTest,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos)
   {
     while (startPos<=length())
      {
	if (aTest.test(contents()[startPos-1])) return startPos;
	else startPos++;
      }
   }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::lastIndexOf                                                |
|                                                                              |
|  1. Adjust starting position and see if the search can                       |
|     succeed by calling startBackwardsSearch().                               |
|  2. If a match isn't possible,return 0.                                      |
|  3. Otherwise,search (backward) according to length of search                |
|     string:                                                                  |
|       0 -return 0;                                                           |
|       1 -search for a matching character in the buffer by                    |
|            comparing one byte at a time                                      |
|       >1-call memcmp for each character of the buffer to see                 |
|            if the characters starting here match the search string.          |
|                                                                              |
| If no match is found,return 0.                                               |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::lastIndexOf(const char *pSearchString,unsigned searchLen,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,searchLen);
  if (startPos)
    {
    switch(searchLen)
      {
      case 0:
        break;
      case 1:
        while (startPos)
	 {
	   if (contents()[startPos-1]==*pSearchString) return startPos;
	   else startPos-=prevCharLength(startPos);
	 }
        break;
      default:
        while (startPos)
	 {
	   if (memcmp(contents()+startPos-1,pSearchString,searchLen)==0) return startPos;
	   else startPos-=prevCharLength(startPos);
	 }
      }
    }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::lastIndexOf                                                |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::lastIndexOf(const MSStringTest &aTest,unsigned startPos) const
{ return MSStringBuffer::lastIndexOf(aTest,startPos); }


/*------------------------------------------------------------------------------
| MSMBStringBuffer::lastIndexOfAnyBut                                          |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the                |
|    starting point) by calling startBackwardsSearch().                        |
| 2. If it can't succeed,return 0.                                             |
| 3. If it can succeed,the starting point is converted to an                   |
|    array (0-based) index.                                                    |
| 4. Search (going backwards) for the first character not in the               |
|    set of valid characters,based on the number of valid                      |
|    characters:                                                               |
|      0 -return the buffer length;i.e.,the last character of                  |
|           the buffer isn't in the (empty) set of valid characters            |
|      1 -compare each character of the buffer to the valid                    |
|           character until one doesn't match                                  |
|      >1-call memchr for each character of the buffer to see                  |
|           if the character is in the set of valid characters;                |
|           return the index of the first that isn't found                     |
|                                                                              |
| If all characters pass the test,return 0.                                    |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::lastIndexOfAnyBut(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  if (startPos)
   {
     switch(numValidChars)
      {
	case 0:
        return startPos;
	case 1:
        if (! isSBC(pValidChars)) return 0;
        while (startPos)
	 {
	   if (contents()[startPos-1]!=*pValidChars) return startPos;
	   else startPos-=prevCharLength(startPos);
	 }
        break;
	default:
        while (startPos)
	 {
	   if (!isCharValid(startPos,pValidChars,numValidChars)) return startPos;
	   else startPos-=prevCharLength(startPos);
	 }
      }
   }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::lastIndexOfAnyBut                                          |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the starting       |
|    point) by calling startBackwardsSearch().                                 |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSFalse result.                       |
|                                                                              |
| If all characters pass the test,return 0.                                    |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::lastIndexOfAnyBut(const MSStringTest &aTest,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  while (startPos)
   {
     if (!aTest.test(contents()[startPos-1])) return startPos;
     else startPos-=prevCharLength(startPos);
   }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::lastIndexOfAnyOf                                           |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the                |
|    starting point) by calling startBackwardsSearch().                        |
| 2. If it can't succeed,return 0.                                             |
| 3. Otherwise,search for the first character in the set of                    |
|    search characters,based on the number of search characters:               |
|      0 -return 0;i.e.,no characters of the buffer are in                     |
|           the (empty) set of search characters                               |
|      1 -search for a matching character in the buffer by                     |
|           comparing buffer characters one-by-one (going backwards).          |
|      >1-call memchr for each character of the buffer to see                  |
|           if the character is in the set of search characters                |
|           return the index of the first that is found                        |
|                                                                              |
| If all characters fail the test,return 0.                                    |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::lastIndexOfAnyOf(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  if (startPos)
   {
     switch(numValidChars)
      {
	case 0:
        break;
	case 1:
        while (startPos)
	 {
	   if (contents()[startPos-1]==*pValidChars) return startPos;
	   else startPos-=prevCharLength(startPos);
	 }
        break;
	default:
        while (startPos)
	 {
	   if (isCharValid(startPos,pValidChars,numValidChars)) return startPos;
	   else startPos-=prevCharLength(startPos);
	 }
      }
   }
  return 0;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::lastIndexOfAnyOf                                           |
|                                                                              |
| 1. Checking that the search can succeed (and establishing the starting       |
|    point) by calling startBackwardsSearch().                                 |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSTrue result.                        |
|                                                                              |
| If all characters fail the test,return 0.                                    |
------------------------------------------------------------------------------*/
unsigned MSMBStringBuffer::lastIndexOfAnyOf(const MSStringTest &aTest,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  while (startPos)
   {
     if (aTest.test(contents()[startPos-1])) return startPos;
     else startPos-=prevCharLength(startPos);
   }
  return 0;
}

/*==============================================================================
 Notes:
   1. Each of these functions returns a pointer to an MSMBStringBuffer object.
      The resulting buffer contains the edited result.  It may or not
      be a different buffer than the receiver,depending on the nature
      of the change and the use count of the receiver.
   2. It is presumed that the caller has properly adjusted the use
      count for the receiver prior to calling any of these functions
      (that is,the use count is accurate).
   3. It is presumed that the caller will attach the resulting buffer
      to an MSString so that the built-in use count in the returned
      buffer is accurate.
   4. Typical usage is as follows:
        ...
        // Preserve old contents.
        MSString old(aString.pBuffer);
        ...
        // Reset aString to point to (possibly) new edited buffer:
        aString.pBuffer=aString.pBuffer->xxxxxxx();
        ...
==============================================================================*/

/*------------------------------------------------------------------------------
| MSMBStringBuffer::center                                                     |
|                                                                              |
| If the requested length is equal to the length of the receiver,              |
| then the receiver is returned unmodified.  Otherwise,the                     |
| receiver's length is reset to the argument length and space is               |
| allocated for the centered buffer.  The new buffer is built                  |
| one of two ways,depending on whether the new length is larger                |
| or smaller than the original length of the receiver's buffer.                |
|                                                                              |
| In the former case,the amount of padding required before and                 |
| after the centered string is calculated.  Then,the new string                |
| is filled with pad characters,the old contents are copied,and                |
| then the remainder is filled with more pad characters.                       |
|                                                                              |
| In the latter case,a substring of the receiver (the center portion           |
| of the requested length) is copied to the result buffer.                     |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::center(unsigned newLength,char padCharacter)
{
  MSStringBuffer *result=this;
  
  if (newLength!=length())
   {
     //-----------------------------------------------------------------------
     // Initialize parameters to likely values:
     //-----------------------------------------------------------------------
     unsigned prefix=0,suffix=0,fromReceiver=length(),startPos=1;
     
     //-----------------------------------------------------------------------
     // Adjust initialization parameters:
     //-----------------------------------------------------------------------
     if (newLength>length())
      {
	prefix=(newLength-length())/2;
	suffix=newLength-length()-prefix;
      }
     else
      {
	fromReceiver=newLength;
	startPos=(length()-fromReceiver)/2+1;
	//---------------------------------------------------------------------
	// If the byte at startPos is the second byte of a DBCS character,then
	// the startPos is decremented so that the string will get cut from the
	// previous byte.
	//---------------------------------------------------------------------
	while (startPos>1&&charType(startPos)>MSStringEnum::DBCS1) startPos--;
      }
     
     //-----------------------------------------------------------------------
     // Allocate space and copy receiver to middle:
     //-----------------------------------------------------------------------
     result=newBuffer(0,prefix,
		      contents()+startPos-1,fromReceiver,
		      0,suffix,
		      padCharacter);
     
     //-----------------------------------------------------------------------
     // If the last byte of the new string is the first byte of a DBCS
     // character,then that byte gets set to the padCharacter.
     //-----------------------------------------------------------------------
     if (startPos+fromReceiver<=length())
      {
	MSStringEnum::CharType typeOfFirstAfterSubstring=charType(startPos+fromReceiver);
	unsigned len=result->length();
	if (typeOfFirstAfterSubstring>MSStringEnum::DBCS1)
	 {
	   for (unsigned i=1;i<typeOfFirstAfterSubstring&&i<=len;i++)
	    {
	      result->contents()[len-i]=padCharacter;
	    }
	 }
      }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::insert                                                     |
|                                                                              |
| This function accepts as arguments:                                          |
|  -pointer to string to be inserted                                           |
|  -length  of string to be inserted                                           |
|  -position at which to insert the string                                     |
|  -pad character.                                                             |
|                                                                              |
| It returns the receiver unmodified if both the string to be inserted is      |
| null and the index at which it is to be inserted is not off the end of       |
| the receiver buffer.                                                         |
|                                                                              |
| Otherwise,space is allocated for the combined buffers.  The insertion        |
| is accomplished in four steps:                                               |
|                                                                              |
|   1. The portion of the receiver prior to the index at which                 |
|      the new string is to be inserted is copied (may be null).               |
|   2. If the receiver isn't this long,then it is padded out                   |
|      to the insert index using the pad character.                            |
|   3. The to-be-inserted string is copied (may be null).                      |
|   4. The portion of the receiver following the index at which                |
|      the new string is to be inserted is copied (may be null).               |
|                                                                              |
| All this is accomplished with a single call to newBuffer().                  |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::insert(const char *pInsert,unsigned len,unsigned index,char padCharacter)
{
  MSStringBuffer *result=this;

  if (len!=0||index>length())
   {
     //-----------------------------------------------------------------------
     // If the index is somewhere in the existing string,then
     // check to see if the insertion point is between bytes of a DBCS
     // character.  If so,then move the insertion point back one.
     //-----------------------------------------------------------------------
     if (index<length())
      {
	while (index>0&&charType(index+1)>MSStringEnum::DBCS1) index--;
      }	
     
     unsigned len1=(length()<index)?length():index,len2,len3;
     const char *p1=contents(),*p2,*p3;
     
     //-----------------------------------------------------------------------
     // See if we need to pad the receiver:
     //-----------------------------------------------------------------------
     if (index>length())
      {
	//---------------------------------------------------------------------
	// Second initializer is pad characters:
	//---------------------------------------------------------------------
	len2=index-length(),
	p2=0,
	//---------------------------------------------------------------------
	// Third initializer is the string to be inserted:
	//---------------------------------------------------------------------
	len3=len,
	p3=pInsert;
      }
     else
      {
	//---------------------------------------------------------------------
	// Second initializer is the string to be inserted:
	//---------------------------------------------------------------------
	len2=len,
	p2=pInsert,
	//---------------------------------------------------------------------
	// Third initializer is the rest of the receiver:
	//---------------------------------------------------------------------
	len3=length()-index,
	p3=contents()+index;
      }
     
     //-----------------------------------------------------------------------
     // Return new buffer:
     //-----------------------------------------------------------------------
     result=newBuffer(p1,len1,p2,len2,p3,len3,padCharacter);
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::leftJustify                                                |
|                                                                              |
| If the new length is the same as the receiver's current length,              |
| then the receiver is returned unmodified.                                    |
|                                                                              |
| Otherwise,the new buffer is generated via an invocation of the               |
| newBuffer() function.  The new contents are built from the                   |
| receiver (either its full length or truncated at the new length              |
| if that is shorter) and a string of pad characters (of sufficient            |
| length to fill the result out to the specified length).                      |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::leftJustify(unsigned newLength,char padCharacter)
{
  MSStringBuffer *result=this;
  unsigned len2=0;

  if (newLength!=length())
   {
     if (newLength>length()) len2=newLength-length();
     
     result=newBuffer(contents(),(length()<newLength)?length():newLength,
		      0,len2,
		      0,0,
		      padCharacter);
     //------------------------------------------------------------------------
     // If the last byte of the resulting string is DBCS1,then replace it
     // with the padCharacter.
     //------------------------------------------------------------------------
     if (newLength<length())
      {
        MSStringEnum::CharType typeOfFirstAfterSubstring=charType(newLength+1);
        if (typeOfFirstAfterSubstring>MSStringEnum::DBCS1)
	 {
	   for (unsigned i=1;i<typeOfFirstAfterSubstring&&i<=newLength;i++)
	    {
	      result->contents()[newLength-i]=padCharacter;
	    }
	 }
      }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::lowerCase                                                  |
|                                                                              |
| Starting at the beginning of the buffer,scan for the next capital            |
| letter and if found,convert it to lower case.  Quit as soon as               |
| the search for a capital (via indexOfAnyOf()) fails.                         |
------------------------------------------------------------------------------*/
static const char caps[]="ABCDEFGHIJKLMNOPQRSTUVWXYZ";

MSStringBuffer *MSMBStringBuffer::lowerCase()
{
  MSStringBuffer *result=this;
  unsigned pos=indexOfAnyOf(caps,26,0);
  if (pos<length())
   {
     if (useCount()>1) result=newBuffer(contents(),length());
     else addRef();
     char *p=result->contents();
     while (pos<length())
      {
	if (charType(pos)==MSStringEnum::SBCS) p[pos]+='a'-'A';
	pos=indexOfAnyOf(caps,26,pos+1);
      }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::overlayWith                                                |
|                                                                              |
| This member function overlayWith() returns the receiver                      |
| unmodified if both the length of the overlay string is zero and              |
| the overlay occurs within the current receiver (not off the end).            |
|                                                                              |
| Otherwise,space is allocated for the combined buffers.  The                  |
| overlay is accomplished in four steps:                                       |
|                                                                              |
|   1. The portion of the receiver prior to the index at which                 |
|      the new string is to be overlayed is copied (may be null).              |
|   2. If the receiver isn't long enough,then it is padded out                 |
|      to this index using the pad character.                                  |
|   3. The overlay string is copied (may be null).                             |
|   4. The portion of the receiver following the portion of it                 |
|       which was overlaid is copied (may be null).                            |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::overlayWith(const char *pOverlay,unsigned len,
					      unsigned index,char padCharacter)
{
  MSStringBuffer *result=this;

  if (len!=0||index>length())
   {
     //------------------------------------------------------------------------
     // Treat index 0 like index 1:
     //------------------------------------------------------------------------
     if (index==0) index=1;
     
     unsigned len1=(length()<index-1)?length():index-1,len2,len3;
     const char *p1=contents(),*p2,*p3;
     
     //------------------------------------------------------------------------
     // Rest is pad+overlay or overlay+rest:
     //------------------------------------------------------------------------
     if (len1<index-1)
      {
	//----------------------------------------------------------------------
	// Second initializer is pad characters:
	//----------------------------------------------------------------------
	len2=(index-1)-len1;
	p2=0;
	//----------------------------------------------------------------------
	// Third initializer is the string to be overlayed:
	//----------------------------------------------------------------------
	len3=len;
	p3=pOverlay;
      }
     else
      {
	//----------------------------------------------------------------------
	// Second initializer is the string to be overlayed:
	//----------------------------------------------------------------------
	len2=len;
	p2=pOverlay;
	//----------------------------------------------------------------------
	// Third initializer is the rest of the receiver:
	//----------------------------------------------------------------------
	if (length()>=index+len)
	 {
	   len3=length()-index-len+1;
	   p3=contents()+index+len-1;
	 }
	else
	 {
	   len3=0;
	   p3=0;
	 }
      }
     
     //------------------------------------------------------------------------
     // Obtain new buffer:
     //------------------------------------------------------------------------
     result=newBuffer(p1,len1,p2,len2,p3,len3,padCharacter);
     
     //------------------------------------------------------------------------
     // If the byte at (index-1) is the first byte of a DBCS character,
     // then that byte should be replaced with padCharacter.
     // If the byte at (index+len) is the second byte of a DBCS
     // character,then that byte should be replaced with padCharacter.
     //------------------------------------------------------------------------
     if (index<=length())
      {
	MSStringEnum::CharType typeOfFirstOverlayed=charType(index);
	if (typeOfFirstOverlayed>MSStringEnum::DBCS1)
	 {
	   for (unsigned i=1;i<typeOfFirstOverlayed&&i<index;i++) 
            {
              result->contents()[index-1-i]=' ';
	    }
	 }
      }
     for (unsigned i=0;index+len+i<=length();i++)
      {
	if (charType(index+len+i)>MSStringEnum::DBCS1) result->contents()[index+len-1+i]=padCharacter;
      }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::reverse                                                    |
|                                                                              |
| If the string is not null,reverse its bytes.  Since the MSStringBuffer is    |
| not guaranteed to not have null characters,we can't use strrev()             |
| and instead roll our own.                                                    |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::reverse()
{
  MSStringBuffer *result=this;
  if (length()>1)
   {
     if (useCount()>1) result=newBuffer(contents(),length());
     else addRef();
     
     char *p1=result->contents();
     char *p2=result->contents()+result->length()-1;
     
     // Swap string end for end...
     while (p1<p2)
      { 
	size_t len1=charLength(p1);
	char* lastP1=p1+len1-1;
	if (lastP1>=p2) break;
	char c2[MB_LEN_MAX];
	size_t len2;
	char *p;
	unsigned i;
	for (len2=0,p=p2;len2<len1&&p>lastP1;len2++,p--) c2[len2]=*p;
	for (i=0;i<len1;i++) *(p2-i)=*(lastP1-i);
	for (i=0;i<len2;i++) *(p1+i)=c2 [i];
	p1+=len1;
	p2-=len1;
      }
     // Now reverse multi-byte chars in first half that got mangled...
     p1=result->contents();
     while (p2>p1)
      {
	char c2[MB_LEN_MAX];
	char *p;
	unsigned i;
	for (i=0,p=p2;i<MB_LEN_MAX&&p>=p1;i++,p--) c2[i]=*p;
	size_t len2=charLength(c2);
	if (len2>1)
	 {
	   for (i=0;i<len2;i++) *(p2-len2+1+i)=c2[i];
	   p2-=len2;
	 }
      }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::strip                                                      |
|                                                                              |
| Accepts a string of characters to be stripped. A "mode" indicator specifies  |
| whether leading,trailing,or,both leading and trailing characters meeting     |
| the criteria are to be stripped.                                             |
|                                                                              |
| This method is implemented with the basic algorithm:                         |
|    1. Find the first character that fails the criteria                       |
|    2. Find the last character that fails the criteria                        |
|    3. Allocate space for and copy the intervening characters                 |
|    4. Return the resulting buffer.                                           |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::strip(const char *pChars,unsigned len,MSStringEnum::StripMode mode)
{
  MSStringBuffer *result=this;
  
  if (length()!=0)
   {
     unsigned start=1,stop=length();
     if (mode!=MSStringEnum::Trailing) start=indexOfAnyBut(pChars,len,1);
     if (mode!=MSStringEnum::Leading&&start!=0)
      {
	stop=lastIndexOfAnyBut(pChars,len,length());
	// If last char to keep is MBCS,keep all bytes.
	stop+=charLength(stop)-1;
      }
     if (start!=1||stop!=length())
      { 
	if (start!=0) result=newBuffer(contents()+start-1,stop-start+1);
	else
	 {
	   result=null();
	   result->addRef();
	 }
      }
     else addRef();
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::strip                                                      |
|                                                                              |
| This method uses an MSStringTest object to determine which characters are to |
| be stripped. A "mode" indicator specifies whether leading,trailing,or,       |
| both leading and trailing characters meeting the criteria are to be          |
| stripped.                                                                    |
|                                                                              |
| 1. Find the first character that fails the criteria                          |
| 2. Find the last character that fails the criteria                           |
| 3. Allocate space for and copy the intervening characters                    |
| 4. Return the resulting buffer.                                              |
------------------------------------------------------------------------------*/
MSStringBuffer *MSMBStringBuffer::strip(const MSStringTest& aTest, MSStringEnum::StripMode mode)
{
  MSStringBuffer *result=this;
  
  if (length()!=0)
   {
     unsigned start=1,stop=length();
     if (mode!=MSStringEnum::Trailing) start=indexOfAnyBut(aTest,1);
     if (mode!=MSStringEnum::Leading&&start!=0)
      {
	stop=lastIndexOfAnyBut(aTest,length());
	// If last char to keep is DBCS,keep two bytes.
	stop+=charLength(stop)-1;
      }
     if (start!=1||stop!=length())
      { 
	if (start!=0) result=newBuffer(contents()+start-1,stop-start+1);
	else
	 {
	   result=null();
	   result->addRef();
	 }
      }
     else addRef();
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::translate                                                    |
|                                                                              |
| The translation is accomplished by:                                          |
|                                                                              |
|   1. Determine the number of logical characters in the input string          |
|      (pInputChars) that are to be translated.                                |
|   2. Set up a translation table that consists of an entry for each           |
|      logical character to be translated,and contains information             |
|      such as the input character,input character length,output character,    |
|      output character length,and difference between outlen and inlen.        |
|   3. Set up a translation occurrence linked list which consists of           |
|      one node per translation that is to occur.  Each node contains 1) the   |
|      position in self.data that contains a character to be translated,       |
|      and 2) an index into the transTable that contains the translation       |
|      info.  This is accomplished by finding all occurrences of each          |
|      inputCharacter (which is a logical character) in the string (via        |
|      indexOfAnyOf()).                                                        |
|   4. A new buffer is allocated (with a new length if required) and filled    |
|      in by processing each node in the translation occurrence linked list.   |
------------------------------------------------------------------------------*/
class transTable
{
public:
  char in[MB_LEN_MAX];                // Input character to be translated
  char out[MB_LEN_MAX];               // Output character
  char inLen;                         // Length of input character (1 for
                                      // single byte,2 for double byte)
  char outLen;                        // Length of output character (1 for
                                      // single byte,2 for double byte)
  int diff;                           // outlen-inlen
};

class Occurrence
{
public:
  Occurrence* pNext;                  // pointer to next occurrence
  unsigned int pos;                   // position in string of occurrence
  Occurrence(unsigned int i) : pNext(0),pos(i) {};
  ~Occurrence() { if (pNext!=0) delete pNext; };
};

class transOccurrence : public Occurrence
{
public:
  unsigned int n;                     // corresponding index in transTable
  transOccurrence(unsigned int i,unsigned int j);
  ~transOccurrence();
};

transOccurrence::transOccurrence(unsigned int i,unsigned int j) : 
Occurrence(i),n(j)
{}

transOccurrence::~transOccurrence()
{}

MSStringBuffer *MSMBStringBuffer::translate(const char *pInputChars,unsigned inputLen,
					    const char *pOutputChars,unsigned outputLen,
					    char padCharacter)
{
  MSStringBuffer *result=this;

  //--------------------------------------------------------------------------
  // Determine the number of characters (NOT bytes) to be translated.
  //--------------------------------------------------------------------------
  unsigned int numChars=0,n=0;
  while (n<inputLen)
   {
     n+=charLength(&pInputChars [n]);
     numChars++;
   }

  //--------------------------------------------------------------------------
  // Set up the translation table.  There is a separate translation table
  // entry for each input-output pair.  Each entry contains inputChar,
  // inputChar length,outputChar,outputChar length,and the amount of
  // difference in the two lengths.
  //--------------------------------------------------------------------------
  transTable* transTbl=new transTable[numChars];
  const char *pi=pInputChars;
  const char *po=pOutputChars;

  for (n=0;n<numChars;n++)
   {
     unsigned i;
     transTbl[n].inLen=(char) charLength(pi);
     for (i=0;i<transTbl[n].inLen;i++) transTbl[n].in[i]=*pi++;
     
     if (po>=pOutputChars+outputLen)
      {
	transTbl[n].outLen=1;
	transTbl[n].out[0]=padCharacter;
      }
     else
      {
	transTbl[n].outLen=(char) charLength(po);
	for (i=0;i<transTbl[n].outLen;i++) transTbl[n].out[i]=*po++;
      }
     transTbl[n].diff=transTbl[n].outLen-transTbl[n].inLen;
   }

  //--------------------------------------------------------------------------
  //                        Set up transOccurrence.
  //--------------------------------------------------------------------------
  // transOccurrence is a linked list consisting of one node per translation
  // that is to occur.  Each node contains 1) the position in self.data that
  // contains a character to be translated,and 2) an index into the transTable
  // that contains the translation info (inputChars,outputChars,lengths).
  //--------------------------------------------------------------------------
  pi=pInputChars;
  unsigned int pos=0;
  transOccurrence head=transOccurrence(0,0);
  transOccurrence *pTransOcc=&head;
  unsigned int count=0;
  int lengthChange=0;

  //--------------------------------------------------------------------------
  // Set up the transOccurrence linked list by finding all occurrences of
  // each inputCharacter (which is a logical character) in the string.  Also,
  // keep track of the amount of string length change that will occur so that
  // the new resulting string can be allocated at the correct length.
  //--------------------------------------------------------------------------
  while ((pos=indexOfAnyOf(pi,inputLen,pos+1))!=0)
   {
     size_t len=charLength(pos);
     for (n=0;n<numChars;n++)
      {
        if (!memcmp(&contents()[pos-1],&transTbl[n].in,len)) break;
      }
     pTransOcc=(transOccurrence*)(pTransOcc->pNext=new transOccurrence(pos-1,n));
     count++;
     lengthChange+=transTbl[n].diff;
   }

  //--------------------------------------------------------------------------
  // Allocate the new,translated string;this string will reflect any length
  // change calculated above.
  //--------------------------------------------------------------------------
  char *pOld=contents();
  result=newBuffer(contents(),length(),0,lengthChange,0,0,0);
  pTransOcc=(transOccurrence*)head.pNext;
  char* pSource=pOld;
  char* pDest=result->contents();
  unsigned int previousPos=0;

  //--------------------------------------------------------------------------
  // Copy the contents of the old string into the new string (result) until
  // we get to a position that requires a translation.  At that point,copy
  // the translation character into the new string,based on the
  // information in the transTable.  Continue this until all nodes in
  // transOccurrence have been processed (number of nodes==count).
  //--------------------------------------------------------------------------
  while (count--)
   {
     unsigned int len=pTransOcc->pos-previousPos;
     memcpy(pDest,pSource,len);
     pDest+=len;
     pSource+=len;
     memcpy(pDest,transTbl[pTransOcc->n].out,len=transTbl[pTransOcc->n].outLen);
     pDest+=len;
     pSource+=transTbl[pTransOcc->n].inLen;
     previousPos=pSource-pOld;
     pTransOcc=(transOccurrence*)pTransOcc->pNext;
   }

  //--------------------------------------------------------------------------
  // Finish filling in the new string (result) with the remaining string
  // contents.
  //--------------------------------------------------------------------------
  while ((*pDest++=*pSource++)!=0) { ;/* empty */ }

  delete [] transTbl;

  return result;
}

/*------------------------------------------------------------------------------
| MSMBStringBuffer::upperCase                                                  |
|                                                                              |
| Starting at the beginning of the string,scan for the next lower              |
| case letter and if found,convert it to upper case.  Quit as soon             |
| as the search for a lowercase (via indexOfAnyOf()) fails.                    |
------------------------------------------------------------------------------*/
static const char lowers[]="abcdefghijklmnopqrstuvwxyz";

MSStringBuffer *MSMBStringBuffer::upperCase()
{
  MSStringBuffer *result=this;
  unsigned pos=indexOfAnyOf(lowers,26,0);
  if (pos<length())
   {
     if (useCount()>1) result=newBuffer(contents(),length());
     else addRef();
     char *p=result->contents();
     while (pos<length())
      {
	if (charType(pos)==MSStringEnum::SBCS) p[pos]+='A'-'a';
	pos=indexOfAnyOf(lowers,26,pos+1);
      }
   }
  else addRef();
  return result;
}
