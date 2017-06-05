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
#include <stdlib.h>
}

#ifdef __cfront
#undef isalnum
#undef isalpha
#undef iscntrl
#undef isdigit
#undef isgraph
#undef isxdigit
#undef islower
#undef isprint
#undef ispunct
#undef isupper
#endif

#include <MSTypes/MSStringBuffer.H>

#ifndef MSStringHEADER
#include <MSTypes/MSString.H>
#endif

#ifndef MSStringTestHEADER
#include <MSTypes/MSStringTest.H>
#endif

#ifdef MS_NO_INLINES
#include <MSTypes/MSStringBufferInlines.C>
#endif

MSBaseStringBuffer::~MSBaseStringBuffer() {}

/*------------------------------------------------------------------------------
| MSStringBuffer::className                                                    |
------------------------------------------------------------------------------*/
const char *MSStringBuffer::className(void) const
{ return "MSStringBuffer"; }

/*------------------------------------------------------------------------------
| MSStringBuffer::asDebugInfo                                                  |
------------------------------------------------------------------------------*/
MSString MSStringBuffer::asDebugInfo(void) const
{
  MSString result(className());
  result+="(@";
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",refs=";
  result+=MSString(useCount());
  result+=",len=";
  result+=MSString(length());
  result+=",data=[";
  if (length()>23)
   {
     result+=MSString(contents(),10);
     result+="...";
     result+=MSString(contents()+length()-10,10);
   }
  else result+=contents();
  result+="])";
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::charType                                                     |
------------------------------------------------------------------------------*/
MSStringEnum::CharType MSStringBuffer::charType(unsigned) const
{ return MSStringEnum::SBCS; }

/*------------------------------------------------------------------------------
| MSStringBuffer::allocate                                                     |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::allocate(unsigned newLen) const
{ return new (newLen)MSStringBuffer(newLen); }

/*------------------------------------------------------------------------------
| MSStringBuffer::null                                                         |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::null(void) const
{ return MSStringBuffer::defaultBuffer(); }

/*------------------------------------------------------------------------------
| MSStringBuffer::defaultBuffer                                                |
|                                                                              |
| MSStringBuffer with "null" value.                                            |
|                                                                              |
| This buffer is the "default" buffer used to construct MSStrings.             |
|                                                                              |
| All "null" MSStrings will point to this instance.  Its "contents"            |
| are a single null character.  This ensures that using a null                 |
| MSString equates to using a null string(in more of the C sense).             |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::defaultBuffer(void)
{
  static MSStringBuffer *pNullBuffer = MSStringBuffer::initialize();
  return pNullBuffer;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::initialize                                                   |
|                                                                              |
| Build DBCS table and return pointer to appropriate(null) MSStringBuffer.     |
------------------------------------------------------------------------------*/
extern MSStringBuffer *createMSMBStringBuffer(void);

MSStringBuffer *MSStringBuffer::initialize(void)
{
  if (MB_CUR_MAX>1) return (MSStringBuffer *)createMSMBStringBuffer();
  return new(0) MSStringBuffer(0);
}

/*------------------------------------------------------------------------------
| MSStringBuffer::next                                                         |
------------------------------------------------------------------------------*/
char *MSStringBuffer::next(const char *prev)
{ return(char*)prev++; }

/*------------------------------------------------------------------------------
| MSStringBuffer::next                                                         |
------------------------------------------------------------------------------*/
const char *MSStringBuffer::next(const char *prev) const
{ return prev++; }

/*------------------------------------------------------------------------------
| MSStringBuffer::MSStringBuffer                                               |
|                                                                              |
| Construct a buffer of the specified length.  The "data" member               |
| array must actually be of length 1 greater than the argument                 |
| value(this is achieved automatically via use of the overloaded               |
| operator new for class MSStringBuffer).                                      |
|                                                                              |
| The terminating(extra) byte is set to null.                                  |
|                                                                              |
| Notes:                                                                       |
|   1. This method is protected.  MSStringBuffers must be obtained by using    |
|      MSStringBuffer::nullBuffer and subsequent newBuffer calls to existing   |
|      MSStringBuffer objects.  The only non-heap instance of this class is    |
|      the static MSStringBuffer::nullBuffer object.                           |
|   2. This method should be inline(in context of MSStringBuffer::newBuffer).  |
------------------------------------------------------------------------------*/
MSStringBuffer::MSStringBuffer(unsigned length):refs(1),len(length)
{ data[length]='\0'; }
MSStringBuffer::~MSStringBuffer() {}

/*------------------------------------------------------------------------------
| MSStringBuffer::newBuffer                                                    |
|                                                                              |
| Allocate a new MSStringBuffer of the same type as the receiver and           |
| initialize it with the provided data.                                        |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::newBuffer(const void *p1,unsigned len1,
                                          const void *p2,unsigned len2,
                                          const void *p3,unsigned len3,char padChar) const
{
  unsigned newLen=checkAddition(checkAddition(len1,len2),len3);
  MSStringBuffer *buffer;
  if (newLen)
   {
     buffer=allocate(newLen);
     char *p=buffer->contents();
     // Copy first portion(or pad).
     if (p1) memcpy(p,p1,len1);
     else    memset(p,padChar,len1);
     p+=len1;
     // Copy second portion(or pad).
     if (p2) memcpy(p,p2,len2);
     else    memset(p,padChar,len2);
     p+=len2;
     // Copy third portion(or pad).
     if (p3) memcpy(p,p3,len3);
     else    memset(p,padChar,len3);
   }
  else
   {
     buffer=null();
     buffer->addRef();
   }
  return buffer;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::operator new                                                 |
|                                                                              |
| Allocate an MSStringBuffer object with "data" array of requested size.       |
|                                                                              |
| Notes:                                                                       |
|   1. The argument size is the size of the string to be placed in             |
|      the buffer,*excluding* the terminating null.  The total amount          |
|      of space is thus the size of the MSStringBuffer header(use count and    |
|      length fields),plus the string length(as given by the argument),        |
|      plus 1 more byte(for the terminating null).  Since one byte of          |
|      the data array is declared in MSStringBuffer,the proper amount of       |
|      space is calculated by the expression used below.                       |
------------------------------------------------------------------------------*/
// See MSStringBufferInlines.C

/*------------------------------------------------------------------------------
| MSStringBuffer::operator delete                                              |
|                                                                              |
| Delete the storage using the inverse of the operator used to allocate it.    |
------------------------------------------------------------------------------*/

void MSStringBuffer::operator delete(void *p)
{ ::delete [](char*)p; }

/*------------------------------------------------------------------------------
| MSStringBuffer::addRef                                                       |
|                                                                              |
| Increment use count for this MSStringBuffer.                                 |
------------------------------------------------------------------------------*/
// See MSStringBufferInlines.C

/*------------------------------------------------------------------------------
| MSStringBuffer::removeRef                                                    |
|                                                                              |
| Decrement the use count of the receiver.                                     |
|                                                                              |
| If the resulting use count is now zero,release the MSStringBuffer by         |
| calling delete against "this."                                               |
|                                                                              |
| Notes:                                                                       |
|   1. This function appears dangerous.  However,a call to this function       |
|      is interpreted as giving up access to the associated buffer.  At        |
|      that point,only other references are valid(and if there are none,       |
|      then the receiver can be deleted).                                      |
------------------------------------------------------------------------------*/
// See MSStringBufferInlines.C

/*------------------------------------------------------------------------------
| MSStringBuffer::rightJustify                                                 |
|                                                                              |
| This function right justifies the receiver in a new buffer of the given      |
| length and returns the address of the new buffer.                            |
|                                                                              |
| The right justification algorithm is as follows:                             |
|                                                                              |
|   1. Calculate how much padding will be needed on the left.                  |
|   2. Calculate how many bytes of the current buffer will be                  |
|      transferred to the right-justified result.                              |
|   3. return a new buffer comprised of the "prefix" pad bytes                 |
|      and the portion of the receiver calculated at step 2.                   |
|                                                                              |
| Notes:                                                                       |
|   1. This function(and likewise all the similar "editing" functions)         |
|      is usually called in the following context:                             |
|         MSString::xxxxx()                                                    |
|           {                                                                  |
|           ...                                                                |
|           MSString old(self.pBuffer);                                        |
|           self.pBuffer=old.pBuffer->rightJustify(...);                       |
|           ...                                                                |
|           }                                                                  |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::rightJustify(unsigned newLength,char padCharacter)
{
  if (newLength!=length())
   {
     unsigned prefix=(newLength>length())?newLength-length():0;
     unsigned fromReceiver=(length()<newLength)?length():newLength;
     return newBuffer(0,prefix,
		      contents()+length()-fromReceiver,fromReceiver,
		      0,0,
		      padCharacter);
   }
  return this;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::remove                                                       |
|                                                                              |
| If the buffer will remain unmodified(starting index is beyond the end or     |
| receiver is null),then the receiver is returned unmodified.                  |
|                                                                              |
| Else,if the number of characters to delete is zero,or passes the end of      |
| the receiver,then it is defaulted to the rest of the buffer. Finally,the     |
| new buffer is allocated and filled in via a call to newBuffer().             |
|                                                                              |
| Notes:                                                                       |
|   1. See notes 1. and 2. under MSStringBuffer::rightJustify.                 |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::remove(unsigned startPos,unsigned numChars)
{
  if (startPos<length()&&length()!=0)
   {
     // Default numChars to rest of string:
     if (numChars>length()-startPos) numChars=length()-startPos;
     
     // Initialize from current contents before/after deleted chars:
     return newBuffer(contents(),startPos,
		      contents()+startPos+numChars,length()-numChars-startPos,
		      0,0,
		      0);
   }
  return this;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::overflow                                                     |
|                                                                              |
| Throw "overflow" exception.                                                  |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::overflow(void)
{
//  ITHROWLIBRARYERROR(ICMSStringHEADEROVERFLOW,IErrorInfo::invalidRequest,IException::recoverable);
  return 0;
}
/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the MSStringBuffer testing     *
*   functions.                                                                 *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSStringBuffer::isAlnum                                                      |
| MSStringBuffer::isAlpha                                                      |
| MSStringBuffer::isAscii                                                      |
| MSStringBuffer::isCntrl                                                      |
| MSStringBuffer::isDigits                                                     |
| MSStringBuffer::isGraphics                                                   |
| MSStringBuffer::isHexDigits                                                  |
| MSStringBuffer::isLowerCase                                                  |
| MSStringBuffer::isPrintable                                                  |
| MSStringBuffer::isPunctuation                                                |
| MSStringBuffer::isUpperCase                                                  |
| MSStringBuffer::isWhitespace                                                 |
|                                                                              |
| Each of these functions is implemented by constructing(implicitly)           |
| an MSStringTest with the corresponding standard library function.            |
| This MSStringTest is then applied to the buffer contents using the           |
| indexOfAnyBut member function(which returns the index of the first           |
| character that fails the test).  This "index" is then compared to            |
| achieve the desired result:                                                  |
|   0 (no characters failed the test) ->"MSTrue"                               |
|   >0(some character failed the test)->"MSFalse"                              |
|                                                                              |
| Notes:                                                                       |
|   1. This simplistic implementation fails NLS/DBCS criteria,the              |
|      determination of alpha/lowercase/etc. must be made on a per             |
|      code page basis.                                                        |
------------------------------------------------------------------------------*/
#if defined(__APPLE__)
int MS_ISALNUM(int c) { return isalnum(c); }
int MS_ISALPHA(int c) { return isalpha(c); }
int MS_ISCNTRL(int c) { return iscntrl(c); }
int MS_ISUPPER(int c) { return isupper(c); }
int MS_ISPUNCT(int c) { return ispunct(c); }
int MS_ISPRINT(int c) { return isprint(c); }
int MS_ISLOWER(int c) { return islower(c); }
int MS_ISXDIGIT(int c) { return isxdigit(c); }
int MS_ISGRAPH(int c) { return isgraph(c); }
int MS_ISDIGIT(int c) { return isdigit(c); }
#else
#define MS_ISALNUM isalnum
#define MS_ISALPHA isalpha
#define MS_ISCNTRL iscntrl
#define MS_ISUPPER isupper
#define MS_ISPUNCT ispunct
#define MS_ISPRINT isprint
#define MS_ISLOWER islower
#define MS_ISXDIGIT isxdigit
#define MS_ISGRAPH isgraph
#define MS_ISDIGIT isdigit
#endif

MSBoolean MSStringBuffer::isAlphanumeric() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISALNUM))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isAlphabetic                                                 |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isAlphabetic() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISALPHA))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isASCII                                                      |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isASCII() const
{ 
  unsigned i=length();
  const unsigned char *p=(unsigned char *)contents();
  while (i--) if (*p++>127) return MSFalse;
  return MSTrue;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::isControl                                                    |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isControl() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISCNTRL))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isDigits                                                     |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isDigits() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISDIGIT))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isGraphics                                                   |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isGraphics() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISGRAPH))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isHexDigits                                                  |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isHexDigits() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISXDIGIT))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isLowerCase                                                  |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isLowerCase() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISLOWER))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isPrintable                                                  |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isPrintable() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISPRINT))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isPunctuation                                                |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isPunctuation() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISPUNCT))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isUpperCase                                                  |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isUpperCase() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(MS_ISUPPER))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isWhiteSpace                                                 |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isWhiteSpace() const
{ return MSBoolean(indexOfAnyBut(MSStringTest(APLUS_ISPACE))==length()); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isMBCS                                                       |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isMBCS() const
{ return MSFalse; }

/*------------------------------------------------------------------------------
| MSStringBuffer::isDBCS                                                       |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isDBCS() const
{ return isMBCS(); }

/*------------------------------------------------------------------------------
| MSStringBuffer::isSBCS                                                      |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isSBCS() const
{ return MSTrue; }

/*------------------------------------------------------------------------------
| MSStringBuffer::includesMBCS                                                 |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::includesMBCS() const
{ return MSFalse; }

/*------------------------------------------------------------------------------
| MSStringBuffer::includesDBCS                                                 |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::includesDBCS() const
{ return includesMBCS(); }

/*------------------------------------------------------------------------------
| MSStringBuffer::includesSBCS                                                 |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::includesSBCS() const
{ return MSTrue; }

/*------------------------------------------------------------------------------
| MSStringBuffer::isValidMBCS                                                  |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isValidMBCS() const
{ return MSTrue; }

/*------------------------------------------------------------------------------
| MSStringBuffer::isValidDBCS                                                  |
------------------------------------------------------------------------------*/
MSBoolean MSStringBuffer::isValidDBCS() const
{ return isValidMBCS(); }

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the MSStringBuffer comparison  *
*   function "compare".                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSStringBuffer::compare                                                      |
|                                                                              |
| Initialize result to "less than".                                            |
|                                                                              |
| If neither string is null,we compare as many bytes as in the                 |
| shorter of the two,using memcmp.                                             |
|                                                                              |
| If these sections compare equal,then,if the lengths are the same,            |
| set the result to "equal."  Otherwise,set the result to either               |
| "less than" or "greater than," depending on which string is longer           |
|(the longer one is "greater").                                                |
|                                                                              |
| If these sections aren't equal,then simply set the result to                 |
| "less than" or "greater than" according to the result of the call            |
| to memcmp.                                                                   |
|                                                                              |
| If the receiver isn't null,but the argument is,then return                   |
| "greater than."  If both the receiver and argument are null,                 |
| return "equal."                                                              |
|                                                                              |
| Notes:                                                                       |
|   1. A quick check is made to see if the receiver's buffer and the           |
|      argument coincide(in which case "equal" is returned).                   |
------------------------------------------------------------------------------*/
MSStringBuffer::Comparison MSStringBuffer::compare(const void *pArg,unsigned argLen) const
{
  Comparison result=lessThan;
  if (contents()==pArg&&length()==argLen) result=equal;
  else if (length()>0)
   {
     if (argLen>0)
      {
	int rc=memcmp(contents(),pArg,(length()<argLen)?length():argLen);
	if (rc==0) result=(length()==argLen)?equal:(length()<argLen)?lessThan:greaterThan;
	else result=(rc<0)?lessThan:greaterThan;
      }
     else result=greaterThan;
   }
  else if (argLen==0) result=equal;
  return result;
}

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the MSStringBuffer accessing   *
*   function "subString".                                                      *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSStringBuffer::subString                                                    |
|                                                                              |
| If the starting position is zero,then start at beginning of string.          |
|                                                                              |
| Then,we calculate how much of the substring can come from the receiver.      |
| This is either the tail end starting at startPos,or 0(if startPos is         |
| off the end already).                                                        |
|                                                                              |
| Next,we reduce fromReceiver to the requested length(if smaller).             |
|                                                                              |
| Finally,we allocate a new buffer via a call to newBuffer(),initializing      |
| from the receiver(for length fromReceiver) and padding with the              |
| argument pad character if extra is required.                                 |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::subString(unsigned startPos_,unsigned len_,char padCharacter_) const
{
  // Calculate how much can come from receiver.
  unsigned int fromReceiver=(startPos_<length())?length()-startPos_:0;
  // Reduce if request length is shorter...
  if (len_<fromReceiver) fromReceiver=len_;
  // Initialize what we can from the receiver and leave
  // room for the remainder.
  return newBuffer(contents()+startPos_,fromReceiver,
                   0,len_-fromReceiver,
                   0,0,
                   padCharacter_);
}

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the MSStringBuffer searching   *
*   functions.                                                                 *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSStringBuffer::startSearch                                                  |
|                                                                              |
| This function returns the "adjusted" starting position for a                 |
| search ostensibly starting at position "startPos" for a search               |
| string of length "searchLen."  A return value of 0 indicates                 |
| that the search cannot succeed.                                              |
|                                                                              |
| If the starting position is zero,start the search at the                     |
| beginning of the buffer(at index 1).                                         |
|                                                                              |
| If the starting position plus the length of the search string is             |
| longer than the buffer,then the search cannot succeed,so return 0.           |
| The check for this condition is made carefully to avoid overflow             |
| situations.                                                                  |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::startSearch(unsigned startPos,unsigned searchLen) const
{ return (startPos>length()||searchLen>length()-startPos)?length():startPos; }

/*------------------------------------------------------------------------------
| MSStringBuffer::startBackwardsSearch                                         |
|                                                                              |
| This function returns the "adjusted" starting position for a                 |
| backward search ostensibly starting at position "startPos" for a             |
| search string of length "searchLen".  A return value of length() indicates   |
| that the search cannot succeed.                                              |
|                                                                              |
| If the length of the search string exceeds the length of the buffer,         |
| then the search cannot succeed,so return length().                           |
|                                                                              |
| If the starting position is zero,or,the specified starting position          |
| is beyond the end of the buffer,then start the search at the end             |
| of the buffer(index equal to buffer length).                                 |
|                                                                              |
| Finally,the starting point is moved back so that the search string           |
| starting at that point won't run off the end of the buffer.                  |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::startBackwardsSearch(unsigned startPos,unsigned searchLen) const
{
  if (searchLen<=length())
   {
     if (startPos>=length()) startPos=length()-1;
     if (startPos>length()-searchLen) startPos=length()-searchLen;
   }
  else startPos=length();
  return startPos;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::indexOf                                                      |
|                                                                              |
| 1. First,check whether the search can succeed(and set the                    |
|    point at which the search is to start) by calling startSearch().          |
| 2. If the search cannot succeed,then return length().                        |
| 3. Otherwise,compare the buffer contents starting at each index              |
|    to the search string until a match is found(in which case we              |
|    return the index) or the entire buffer is tested(in which case            |
|    0 is returned).                                                           |
|                                                                              |
| Notes:                                                                       |
|   1. If the search string is a single character,then this function is        |
|      implemented via the equivalent invocation of indexOfAnyOf(which is      |
|      optimized for that case).  This saves the additional logic of           |
|      special code to handle that case here,as well.                          |
|   2. If the search string is null,then length() is returned.                 |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::indexOf(const char *pSearchString,unsigned searchLen,unsigned startPos) const
{
  if (searchLen>0&&length()>0)
   {
     if (searchLen==1) return indexOfAnyOf(pSearchString,searchLen,startPos);
     startPos=startSearch(startPos,searchLen);
     if (startPos<length()&&searchLen)
      {
        const char *p=contents();
        int diffLength=length()-searchLen;
	while (startPos<=diffLength)
	 {
	   if (memcmp(p+startPos,pSearchString,searchLen)==0) return startPos;
	   else startPos++;
	 }
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::indexOf                                                      |
|                                                                              |
| Simply invokes the functionally equivalent indexOfAnyOf function.            |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::indexOf(const MSStringTest &aTest,unsigned startPos) const
{ return indexOfAnyOf(aTest,startPos); }

/*------------------------------------------------------------------------------
| MSStringBuffer::indexOfAnyBut                                                |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the starting        |
|    point) by calling startSearch().                                          |
| 2. If it can't succeed,return length().                                      |
| 3. If it can succeed,the starting point is converted to an                   |
|    array(0-based) index.                                                     |
| 4. Search for the first character not in the set of valid characters,based   |
|    on the number of valid characters:                                        |
|      0 -return 1; i.e.,the first character of the buffer                     |
|           isn't in the(empty) set of valid characters                        |
|      1 -compare each character of the buffer to the valid                    |
|           character until one doesn't match                                  |
|      >1-call memchr for each character of the buffer to see                  |
|           if the character is in the set of valid characters;                |
|           return the index of the first that isn't found                     |
|                                                                              |
| If all characters pass the test,return length().                             |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::indexOfAnyBut(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos<length())
   {
     switch(numValidChars)
      {
      case 0:
        return startPos;
      case 1:
        while (startPos<length())
	 {
	   if (contents()[startPos]!=*pValidChars) return startPos;
           else startPos++;
	 }
        break;
      default:
        while (startPos<length())
	 {
	   if (memchr(pValidChars,contents()[startPos],numValidChars)==0) return startPos;
           else startPos++;
	 }
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::indexOfAnyBut                                                |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the starting        |
|    point) by calling startSearch().                                          |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSFalse result.                       |
|                                                                              |
| If all characters pass the test,return length().                             |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::indexOfAnyBut(const MSStringTest &aTest,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos<length())
   {
     while (startPos<length())
      {
	if (!aTest.test(contents()[startPos])) return startPos;
        else startPos++;
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::indexOfAnyOf                                                 |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the                 |
|    starting point) by calling startSearch().                                 |
| 2. If it can't succeed,return length().                                      |
| 3. If it can succeed,the starting point is converted to an                   |
|    array(0-based) index.                                                     |
| 4. Search for the first character in the set of search                       |
|    characters,based on the number of search characters:                      |
|      0 -return length(); i.e.,no characters of the buffer are in             |
|           the(empty) set of search characters                                |
|      1 -search for a matching character in the buffer using                  |
|           memchr                                                             |
|      >1-call memchr for each character of the buffer to see                  |
|           if the character is in the set of search characters;               |
|           return the index of the first that is found                        |
|                                                                              |
| If all characters fail the test,return length().                             |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::indexOfAnyOf(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos<length())
   {
     switch(numValidChars)
      {
        const char *p;
        case 0:
          break;
        case 1:
          p=(const char *)memchr(contents()+startPos,*pValidChars,length()-startPos);
          if (p) return p-contents();
          break;
        default:
          while (startPos<length())
	   {
	     if (memchr(pValidChars,contents()[startPos],numValidChars)) return startPos;
             else startPos++;
	   }
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::indexOfAnyOf                                                 |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the starting        |
|    point) by calling startSearch().                                          |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSTrue result.                        |
|                                                                              |
| If all characters fail the test,return length().                             |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::indexOfAnyOf(const MSStringTest &aTest,unsigned startPos) const
{
  startPos=startSearch(startPos,1);
  if (startPos<length())
   {
     while (startPos<length())
      {
	if (aTest.test(contents()[startPos])) return startPos;
        else startPos++;
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::lastIndexOf                                                  |
|                                                                              |
| 1. Adjust starting position and see if the search can succeed by calling     |
|    startBackwardsSearch().                                                   |
| 2. If a match isn't possible,return length().                                |
| 3. Otherwise,search(backward) according to length of search string:          |
|      0 -return length();                                                     |
|      1 -search for a matching character in the buffer by                     |
|           comparing one byte at a time                                       |
|      >1-call memcmp for each character of the buffer to see                  |
|           if the characters starting here match the search                   |
|           string                                                             |
| If no match is found,return length().                                        |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::lastIndexOf(const char *pSearchString,unsigned searchLen,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,searchLen);
  if (startPos<length())
   {
     switch(searchLen)
      {
        case 0:
          break;
        case 1:
          while (startPos<length())
           { 
             if (contents()[startPos]==pSearchString[0]) return startPos;
             else startPos--;
	   }
          break;
        default:
          while (startPos<length())
           {
             if (memcmp(contents()+startPos,pSearchString,searchLen)==0) return startPos;
             else startPos--;
	   }
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::lastIndexOf                                                  |
|                                                                              |
|  Simply invoke the functionally equivalent lastIndexOfAnyOf.                 |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::lastIndexOf(const MSStringTest &aTest,unsigned startPos) const
{ return lastIndexOfAnyOf(aTest,startPos); }

/*------------------------------------------------------------------------------
| MSStringBuffer::lastIndexOfAnyBut                                            |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the                 |
|    starting point) by calling startBackwardsSearch().                        |
| 2. If it can't succeed,return length().                                      |
| 3. If it can succeed,the starting point is converted to an                   |
|    array(0-based) index.                                                     |
| 4. Search(going backwards) for the first character not in the                |
|    set of valid characters,based on the number of valid                      |
|    characters:                                                               |
|      0 -return the buffer length; i.e.,the last character of                 |
|           the buffer isn't in the(empty) set of valid characters             |
|      1 -compare each character of the buffer to the valid                    |
|           character until one doesn't match                                  |
|      >1-call memchr for each character of the buffer to see                  |
|           if the character is in the set of valid characters;                |
|           return the index of the first that isn't found                     |
|                                                                              |
|    If all characters pass the test,return length().                          |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::lastIndexOfAnyBut(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  if (startPos<length())
   {
     switch(numValidChars)
      {
        case 0:
          return startPos;
        case 1:
          while (startPos<length())
	   {
	     if (contents()[startPos]!=*pValidChars) return startPos;
	     else startPos--;
	   }
          break;
        default:
          while (startPos<length())
	   {
	     if (memchr(pValidChars,contents()[startPos],numValidChars)==0) return startPos;
	     else startPos--;
	   }
      }
   }
  return length();
}


/*------------------------------------------------------------------------------
| MSStringBuffer::lastIndexOfAnyBut                                            |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the starting        |
|    point) by calling startBackwardsSearch().                                 |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSFalse result.                       |
|                                                                              |
| If all characters pass the test,return length().                             |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::lastIndexOfAnyBut(const MSStringTest &aTest,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  if (startPos<length())
   {
     while (startPos<length())
      {
	if (!aTest.test(contents()[startPos])) return startPos;
	else startPos--;
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::lastIndexOfAnyOf                                             |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the                 |
|    starting point) by calling startBackwardsSearch().                        |
| 2. If it can't succeed,return length().                                      |
| 3. Otherwise,search for the first character in the set of                    |
|    search characters,based on the number of search characters:               |
|      0 -return length(); i.e.,no characters of the buffer are in             |
|           the(empty) set of search characters                                |
|      1 -search for a matching character in the buffer by                     |
|           comparing buffer characters one-by-one(going backwards).           |
|      >1-call memchr for each character of the buffer to see                  |
|           if the character is in the set of search characters;               |
|           return the index of the first that is found                        |
|                                                                              |
| If all characters fail the test,return length().                             |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::lastIndexOfAnyOf(const char *pValidChars,unsigned numValidChars,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  if (startPos<length())
   {
     switch(numValidChars)
      {
        case 0:
          break;
        case 1:
          while (startPos<length())
	   {
	     if (contents()[startPos]==*pValidChars) return startPos;
	     else startPos--;
	   }
          break;
        default:
          while (startPos<length())
	   {
	     if (memchr(pValidChars,contents()[startPos],numValidChars)) return startPos;
	     else startPos--;
	   }
      }
   }
  return length();
}

/*------------------------------------------------------------------------------
| MSStringBuffer::lastIndexOfAnyOf                                             |
|                                                                              |
| 1. Checking that the search can succeed(and establishing the starting        |
|    point) by calling startBackwardsSearch().                                 |
| 2. Invokes the MSStringTest test method against each character in turn,until |
|    the argument MSStringTest returns a MSTrue result.                        |
|                                                                              |
| If all characters fail the test,return length().                             |
------------------------------------------------------------------------------*/
unsigned MSStringBuffer::lastIndexOfAnyOf(const MSStringTest &aTest,unsigned startPos) const
{
  startPos=startBackwardsSearch(startPos,1);
  if (startPos<length())
   {
     while (startPos<length())
      {
	if (aTest.test(contents()[startPos])) return startPos;
	else startPos--;
      }
   }
  return length();
}
/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the editing functions of       *
*   the class MSStringBuffer.                                                  *
*******************************************************************************/

/*==============================================================================
  Notes:
    1. Each of these functions returns a pointer to an MSStringBuffer object.
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
         // Reset aString to point to(possibly) new edited buffer:
         aString.pBuffer=aString.pBuffer->xxxxxxx();
         ...
==============================================================================*/
/*------------------------------------------------------------------------------
| MSStringBuffer::center                                                       |
|                                                                              |
| If the requested length is equal to the length of the receiver,              |
| then the reciever is returned unmodified.  Otherwise,the                     |
| receiver's length is reset to the argument length and space is               |
| allocated for the centered buffer.  The new buffer is built                  |
| one of two ways,depending on whether the new length is larger                |
| or smaller than the original length of the reciever's buffer.                |
|                                                                              |
| In the former case,the amount of padding required before and                 |
| after the centered string is calculated.  Then,the new string                |
| is filled with pad characters,the old contents are copied,and                |
| then the remainder is filled with more pad characters.                       |
|                                                                              |
| In the latter case,a substring of the receiver(the center portion            |
| of the requested length) is copied to the result buffer.                     |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::center(unsigned newLength,char padCharacter)
{
  MSStringBuffer *result=this;
  if (newLength!=length())
   {
     // Initialize parameters to likely values:
     unsigned prefix=0,suffix=0,fromReceiver=length(),startPos=1;
     
     // Adjust initialization parameters:
     if (newLength>length())
      {
	prefix =(newLength-length())/2;
	suffix=newLength-length()-prefix;
      }
     else
      {
	fromReceiver=newLength;
	startPos=(length()-fromReceiver)/2+1;
      }
     // Allocate space and copy receiver to middle:
     result=newBuffer(0,prefix,
		      contents()+startPos-1,fromReceiver,
		      0,suffix,
		      padCharacter);
   }
  else addRef();
  return result;
} 

// Class to maintain a linked list of occurrences of patterns.
struct Occurrence 
{
  Occurrence   *pNext; // Pointer to next occurrence.
  unsigned int  pos; // Index of this occurrence.
  Occurrence(unsigned int i):pNext(0),pos(i) {}
  ~Occurrence() { if (pNext!=0) delete pNext; };
};

/*------------------------------------------------------------------------------
| MSStringBuffer::change                                                       |
|                                                                              |
| This function is implemented by:                                             |
|   1. Making a pass through the receiver,noting the number of occurrences     |
|      of the pattern and the position of each.                                |
|   2. Calculating the new length of the receiver,based on the relative        |
|      lengths of the pattern and replacement strings.                         |
|   3. Filling the new buffer,alternating pieces of the old buffer with the    |
|      new replacement string.                                                 |
|                                                                              |
| The information collected during the the first pass through the receiver is  |
| maintained in a linked list of Occurrence objects.  Each has a data member   |
| to record the position in the receiver of the pattern occurrence and a       |
| pointer to the Occurrence object for the next occurrence of the pattern.     |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::change(const char *pPattern,unsigned patternLen,
                                       const char *pReplacement,unsigned replacementLen,
                                       unsigned startPos,unsigned numChanges)
{
  MSStringBuffer *result=this;
  
  unsigned count=0; // Number of occurrences.
  
  // Initialize occurrence chain with a dummy entry at head.
  // pOccurrence is used to step through the chain; it points
  // to the last occurrence during search,current occurrence
  // during pattern replacement.  Note that when "head" is
  // destructed,it will delete all the other Occurrences.
  Occurrence  head       =Occurrence(0);
  Occurrence *pOccurrence=&head;
  
  // Search for occurrences till all(we need) are found:
  while (count<numChanges)
   {
     if ((startPos=indexOf(pPattern,patternLen,startPos))<length())
      {
	// Occurrence found...
	count++;
	pOccurrence=pOccurrence->pNext=new Occurrence(startPos);
	startPos+=patternLen;
      }
     else break;  // No more occurrences...
   }

  if (count!=0)
   { 
     // Occurrences were found,replace them...
     // Add dummy occurrence at end to ensure tail end of receiver
     // gets copied to destination:
     pOccurrence->pNext=new Occurrence(length()+1);
     // Start at first actual occurrence:
     pOccurrence=head.pNext;
     const char *pSource=contents()+pOccurrence->pos;
     
     // Allocate new buffer if size is changing:
     if (patternLen!=replacementLen)
      {
	unsigned newLen=length();
	if (patternLen>replacementLen) newLen-=checkMultiplication(patternLen-replacementLen,count);
	else newLen=checkAddition(newLen,checkMultiplication(replacementLen-patternLen,count));
	// Allocate new buffer and fill with old contents up to
	// first occurrence of the pattern:
	result=newBuffer(contents(),pSource-contents(),
			 0,newLen-(pSource-contents()),
			 0,0,
			 0);
      }
     else
      { // See if we need new buffer for receiver:
	if (useCount()==1) addRef(); // Hang on to this buffer.
	else result=newBuffer(contents(),length()); // Copy shared buffer.
      }
     
     char *pDest=result->contents()+(pOccurrence->pos);
     
     // For each occurrence,replace pattern and copy up to next occurrence:
     while (count--)
      {
	unsigned previousPos=pOccurrence->pos;
	pOccurrence=pOccurrence->pNext;
	memcpy(pDest,pReplacement,replacementLen);
	pDest  +=replacementLen;
	pSource+=patternLen;
	// Copy source string up to next occurrence:
	unsigned n=(pOccurrence->pos-previousPos)-patternLen;
	if (patternLen!=replacementLen) memcpy(pDest,pSource,n);
	pDest  +=n;
	pSource+=n;
      }
   }
  else addRef(); // No change,reuse receiver:
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::copy                                                         |
|                                                                              |
| If the number of copies is 1,or the receiver is a null buffer,               |
| then the receiver is returned unmodified.                                    |
|                                                                              |
| If the number of copies is zero,then the result is the null buffer.          |
|                                                                              |
| Otherwise,the actual number of copies to be made is calculated and           |
| space allocated.  The old receiver contents are copied to the new            |
| buffer.  Then,we loop,copying as much as we have(or need) from               |
| the beginning of the buffer to the end.                                      |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::copy(unsigned int numCopies)
{
  MSStringBuffer *result=this;
  if (length()!=0&&numCopies!=1)
   { // MSStringBuffer contents will change.
     if (numCopies)
      {
	// Initialize new buffer with first copy:
	result=newBuffer(contents(),length(),0,checkMultiplication(--numCopies,length()));
	// Make additional copies:
	// Where second copy goes.
	char *pDest=result->contents()+length(); 
	while (numCopies>0)
	 { // Duplicate as much as we can from what we've already got:
	   unsigned n =(pDest-result->contents()<numCopies*length())?pDest-result->contents():numCopies*length();
	   memcpy(pDest,result->contents(),n);
	   pDest+=n;
	   numCopies-=n/length();
	 }
      }
     else
      {
	result=null();
	result->addRef();
      }
   }
  else addRef(); // This buffer will be maintained:
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::insert                                                       |
|                                                                              |
| This function accepts as arguments:                                          |
|  -pointer to string to be inserted                                           |
|  -length  of string to be inserted                                           |
|  -position at which to insert the string                                     |
|  -pad character.                                                             |
|                                                                              |
|  It returns the receiver unmodified if both the string to be                 |
|  inserted is null and the index at which it is to be inserted is             |
|  not off the end of the receiver buffer.                                     |
|                                                                              |
|  Otherwise,space is allocated for the combined buffers.  The                 |
|  insertion is accomplished in four steps:                                    |
|                                                                              |
|    1. The portion of the receiver prior to the index at which                |
|       the new string is to be inserted is copied(may be null).               |
|    2. If the receiver isn't this long,then it is padded out                  |
|       to the insert index using the pad character.                           |
|    3. The to-be-inserted string is copied(may be null).                      |
|    4. The portion of the receiver following the index at which               |
|       the new string is to be inserted is copied(may be null).               |
|                                                                              |
|  All this is accomplished with a single call to newBuffer().                 |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::insert(const char *pInsert,unsigned len_,
                                       unsigned index,char padCharacter)
{
  MSStringBuffer *result=this;
  if (len_!=0||index>length())
   {
     unsigned len1=(length()<index)?length():index;
     unsigned len2,len3;
     const char *p1=contents(),*p2,*p3;
     // See if we need to pad the receiver:
     if (index>length())
      {
	// Second initializer is pad characters:
	// Third initializer is the string to be inserted:
	len2=index-length(),p2=0;
        len3=len_,p3=pInsert;
      }
     else
      {
	// Second initializer is the string to be inserted:
	// Third initializer is the rest of the receiver:
	len2=len_,p2=pInsert;
        len3=length()-index,p3=contents()+index;
      }
     // Return new buffer:
     result=newBuffer(p1,len1,
		      p2,len2,
		      p3,len3,
		      padCharacter);
   } 
  else addRef();
  return result;
} 

/*------------------------------------------------------------------------------
| MSStringBuffer::leftJustify                                                  |
|                                                                              |
| If the new length is the same as the receiver's current length,then the      |
| receiver is returned unmodified.                                             |
|                                                                              |
| Otherwise,the new buffer is generated via an invocation of the               |
| newBuffer() function.  The new contents are built from the receiver(either   |
| its full length or truncated at the new length if that is shorter) and a     |
| string of pad characters(of sufficient length to fill the result out to      |
| the specified length).                                                       |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::leftJustify(unsigned newLength,char padCharacter)
{
  MSStringBuffer *result=this;
  if (newLength!=length())
   {
     result=newBuffer(contents(),(length()<newLength)?length():newLength,
		      0,(newLength>length())?newLength-length():0,
		      0,0,
		      padCharacter);
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::lowerCase                                                    |
|                                                                              |
| Starting at the beginning of the buffer,scan for the next capital            |
| letter and if found,convert it to lower case.  Quit as soon as               |
| the search for a capital(via indexOfAnyOf()) fails.                          |
------------------------------------------------------------------------------*/
static const char caps[]="ABCDEFGHIJKLMNOPQRSTUVWXYZ";

MSStringBuffer *MSStringBuffer::lowerCase()
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
	p[pos]+='a'-'A';
	pos=indexOfAnyOf(caps,26,pos+1);
      }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::overlayWith                                                 |
|                                                                             |
| The member function overlayWith() returns the receiver                      |
| unmodified if both the length of the overlay string is zero and             |
| the overlay occurs within the current receiver(not off the end).            |
|                                                                             |
| Otherwise,space is allocated for the combined buffers.  The                 |
| overlay is accomplished in four steps:                                      |
|                                                                             |
|   1. The portion of the receiver prior to the index at which                |
|      the new string is to be overlayed is copied(may be null).              |
|   2. If the receiver isn't long enough,then it is padded out                |
|      to this index using the pad character.                                 |
|   3. The overlay string is copied(may be null).                             |
|   4. The portion of the receiver following the portion of it                |
|      which was overlaid is copied(may be null).                             |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::overlayWith(const char *pOverlay,unsigned len_,
                                            unsigned index,char padCharacter)
{
  MSStringBuffer *result=this;
  if (len_!=0||index>length())
   {
     unsigned  len1=(length()<index)?length():index,len2,len3;
     const char *p1=contents(),*p2,*p3;
     // Rest is pad+overlay or overlay+rest:
     if (len1<index)
      {
	// Second initializer is pad characters:
	len2=index-len1,p2=0;
	// Third initializer is the string to be overlayed:
	len3=len_,p3=pOverlay;
      }
     else
      { 
	// Second initializer is the string to be overlayed:
	len2=len_,p2=pOverlay;
	// Third initializer is the rest of the receiver:
	if (length()>=index+len_) len3=length()-index-len_,p3=contents()+index+len_;
	else len3=0,p3=0;
      }
     // Return new buffer:
     result=newBuffer(p1,len1,p2,len2,p3,len3,padCharacter);
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::reverse                                                      |
|                                                                              |
| If the string is not null,reverse its bytes.  Since the MSStringBuffer is    |
| not guaranteed to not have null characters,we can't use strrev()             |
| and instead roll our own.                                                    |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::reverse()
{
  MSStringBuffer *result=this;
  if (length()>1)
   {
     if (useCount()>1) result=newBuffer(contents(),length());
     else addRef();
     char* p1=result->contents();
     char* p2=result->contents()+result->length()-1;
     char c;
     while (p1<p2) { c=*p1; *p1++=*p2; *p2--=c; }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::strip                                                        |
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
MSStringBuffer *MSStringBuffer::strip(const char *pChars,unsigned len_,MSStringEnum::StripMode mode)
{
  MSStringBuffer *result=this;
  if (length()!=0)
   {
     unsigned start=0,stop=length()-1;
     unsigned newLength=length();
     switch (mode)
      {
      case MSStringEnum::Trailing:
	stop=lastIndexOfAnyBut(pChars,len_,length());
	if (stop<length()) newLength=stop+1;
	else if (stop==length()) newLength=0;
	break;
      case MSStringEnum::Leading:	
	start=indexOfAnyBut(pChars,len_,0);
        if (start<length()) newLength=length()-start;
	else if (start==length()) newLength=0;
        break;
      case MSStringEnum::Both:	
	start=indexOfAnyBut(pChars,len_,0);
	stop=lastIndexOfAnyBut(pChars,len_,length());
	if (start==stop&&start==length()) newLength=0;
	else
	 {
	   if (start==length()) start=0;	
	   if (stop==length()) stop=length()-1;
	   newLength=stop-start+1;
	 }
	break;
      }
     if (newLength!=length())
      { 
	if (newLength>0&&start<length()) result=newBuffer(contents()+start,newLength);
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
| MSStringBuffer::strip                                                        |
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
MSStringBuffer *MSStringBuffer::strip(const MSStringTest &aTest,MSStringEnum::StripMode mode)
{
  MSStringBuffer *result=this;
  if (length()!=0)
   {
     unsigned start=0,stop=length()-1;
     unsigned newLength=length();
     switch (mode)
      {
      case MSStringEnum::Trailing:
	stop=lastIndexOfAnyBut(aTest,length());
	if (stop<length()) newLength=stop+1;
	else if (stop==length()) newLength=0;
	break;
      case MSStringEnum::Leading:	
	start=indexOfAnyBut(aTest,0);
        if (start<length()) newLength=length()-start;
	else if (start==length()) newLength=0;	
        break;
      case MSStringEnum::Both:	
	start=indexOfAnyBut(aTest,0);
	stop=lastIndexOfAnyBut(aTest,length());
	if (start==stop&&start==length()) newLength=0;
	else
	 {
	   if (start==length()) start=0;	
	   if (stop==length()) stop=length()-1;
	   newLength=stop-start+1;
	 }
	break;
      }
     if (newLength!=length())
      { 
	if (newLength>0&&start<length()) result=newBuffer(contents()+start,newLength);
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
| The translation is accomplished by searching for the next occurrence of one  |
| of the input characters(via indexOfAnyOf()) and then converting it to the    |
| corresponding output character. The index into the character arrays is       |
| calculated via memchr(). If the output character array isn't this long,      |
| then it is effectively padded with the pad character.  The process stops     |
| when the call to indexOfAnyOf() fails.                                       |
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::translate(const char *pInputChars,unsigned inputLen,
					  const char *pOutputChars,unsigned outputLen,
                                          char padCharacter)
{
  MSStringBuffer *result=this;
  unsigned pos=indexOfAnyOf(pInputChars,inputLen,0);
  if (pos<length())
   {
     // Changes will occur,make sure string contents are private:
     if (useCount()>1) result=newBuffer(contents(),length());
     else addRef();
     char *p=result->contents();
     // Create copies of source/target characters to avoid
     // problems if either overlap the receiver:
     char *pIn  =(char*) memcpy(new char[inputLen],pInputChars,inputLen);
     char *pOut =(char*) memcpy(new char[inputLen],
                             pOutputChars,
			     (outputLen<inputLen)?outputLen:inputLen);
     if (outputLen<inputLen) memset(pOut+outputLen,padCharacter,inputLen-outputLen);
     // Translate all occurrences of input chars:
     while (pos<length())
      {
	unsigned i=(char*) memchr(pIn,p[pos],inputLen)-pIn;
	p[pos]=pOut[i];
	pos=indexOfAnyOf(pIn,inputLen,pos+1);
      }
     delete pIn;
     delete pOut;
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer::upperCase                                                    |
|                                                                              |
| Starting at the beginning of the string,scan for the next lower case letter  |
| and if found,convert it to upper case.  Quit as soon as the search for a     |
| lowercase(via indexOfAnyOf()) fails.                                         |
------------------------------------------------------------------------------*/
static const char lowers[]="abcdefghijklmnopqrstuvwxyz";

MSStringBuffer *MSStringBuffer::upperCase()
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
	p[pos]+='A'-'a';
	pos=indexOfAnyOf(lowers,26,pos+1);
      }
   }
  else addRef();
  return result;
}

/*------------------------------------------------------------------------------
| MSStringBuffer *MSStringBuffer::rotate(unsigned count)
| MSStringBuffer *MSStringBuffer::take(unsigned count)
| MSStringBuffer *MSStringBuffer::drop(unsigned count)
------------------------------------------------------------------------------*/
MSStringBuffer *MSStringBuffer::rotate(int count)
{
  MSStringBuffer *result=this;
  unsigned n=(count>=0)?count:-count; 
  if (n>0&&n!=length())
   {
     if (useCount()>1) result=newBuffer(contents(),length());
     else addRef();
     char *dp=result->contents();
     unsigned i=0,j=0;
     if (n>length()) n%=length();
     if (count<0) n=length()-n;
     if (n>0)
      {
        char *tp=new char[n];
	for (i=0;i<n;i++) tp[i]=dp[i];
        unsigned rotateLength=length()-n;
	for (i=0;i<rotateLength;i++) dp[i]=dp[i+n];
	for (i=rotateLength,j=0;j<n;i++,j++) dp[i]=tp[j];
        dp[length()]='\0';
        delete [] tp;
      }
   }
  else addRef();
  return result;
}

MSStringBuffer *MSStringBuffer::drop(int count)
{
  MSStringBuffer *result=this;
  unsigned an=(count>=0)?count:-count; 
  if (count!=0&&an<=length())
   {
     unsigned n=length()-an;
     result=newBuffer((count<0)?contents():0,n);
     char *dp=result->contents();
     const char *sp=contents();
     if (count>=0) for (unsigned i=0;i<n;i++) dp[i]=sp[i+count];
     dp[n]='\0';
   }
  else addRef();
  return result;
}

MSStringBuffer *MSStringBuffer::take(int count)
{
  MSStringBuffer *result=this;
  unsigned an=(count>=0)?count:-count; 
  if (an>0)
   {
     unsigned n=an;
     int i=0;
     if (an<=length()) result=newBuffer(contents(),an);
     else result=newBuffer(contents(),length(),0,an-length());
     char *dp=result->contents();
     const char *sp=contents();
     if (count<0)       
      {
        int k=int(length())-int(n);
        for (i=0;i<n;i++) dp[i]=(k+i>=0)?sp[k+i]:' ';
      }  
     else if (count>=0) for (i=length();i<n;i++) dp[i]=' ';
     dp[n]='\0';
   }
  else if (an==0) result=newBuffer(0,0,0,0,0,0);
  else addRef();
  return result;
}

